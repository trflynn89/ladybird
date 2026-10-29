#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

import argparse
import base64
import http.server
import json
import struct
import tempfile
import threading
import time
import zlib

from pathlib import Path

import webdriver_test as helpers


def test_site_zoom(webdriver_binary):
    requested = threading.Event()
    release_response = threading.Event()

    class Handler(http.server.BaseHTTPRequestHandler):
        def do_GET(self):
            if self.path == "/blocked":
                requested.set()
                if not release_response.wait(timeout=30):
                    self.send_error(500)
                    return
            if self.path == "/favicon.ico":
                self.send_error(404)
                return
            self.send_response(200)
            self.send_header("Content-Type", "text/html")
            self.send_header("Cache-Control", "no-store")
            self.end_headers()
            self.wfile.write(b"<!doctype html><title>Zoom test</title><body>Zoom test")

        def log_message(self, format, *args):
            pass

    with tempfile.TemporaryDirectory() as directory:
        config = Path(directory) / "config"
        config.mkdir()
        (config / "Settings.json").write_text(json.dumps({"content": {"zoomPerHost": {"127.0.0.1": 1.8}}}))
        server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
        threading.Thread(target=server.serve_forever, daemon=True).start()
        with helpers.webdriver_process(webdriver_binary, profile=directory) as (_, port):
            session = None
            try:
                helpers.wait_for_port(port)
                session = helpers.create_session(port)

                def command(path, body):
                    status, payload, raw = helpers.request(port, "POST", f"/session/{session}/{path}", body)
                    assert status == 200, raw
                    return payload["value"]

                def script(source, *args):
                    return command("execute/sync", {"script": source, "args": list(args)})

                def wait_for_page(url, pixel_ratio):
                    deadline = time.monotonic() + 30
                    state = None
                    while time.monotonic() < deadline:
                        status, payload, raw = helpers.request(
                            port,
                            "POST",
                            f"/session/{session}/execute/sync",
                            {"script": "return [location.href, document.readyState, devicePixelRatio]", "args": []},
                        )
                        if (
                            status == 500
                            and payload["value"]["message"] == "WebContent was replaced while executing the command"
                        ):
                            continue
                        assert status == 200, raw
                        state = payload["value"]
                        if state == [url, "complete", pixel_ratio]:
                            return
                        time.sleep(0.01)
                    raise AssertionError(f"Expected {url} at scale {pixel_ratio}, got {state}")

                # Measure the platform's unzoomed ratio rather than assuming a particular display density.
                baseline_ratio = script("return devicePixelRatio")
                source_url = f"http://127.0.0.1:{server.server_port}/source"
                target_url = f"http://localhost:{server.server_port}/blocked"
                command("url", {"url": source_url})
                wait_for_page(source_url, baseline_ratio * 1.8)

                # Hold the response until the old document has been inspected. No delay determines whether
                # navigation has started or whether it is allowed to commit.
                script(
                    "const a = document.createElement('a'); a.href = arguments[0]; document.body.append(a); a.click();",
                    target_url,
                )
                assert requested.wait(timeout=30), "Destination request did not arrive"
                state = script("return [location.href, devicePixelRatio]")
                assert state == [source_url, baseline_ratio * 1.8], (
                    f"Pending navigation changed the old page's zoom: {state}"
                )
                release_response.set()
                wait_for_page(target_url, baseline_ratio)

                command("back", {})
                wait_for_page(source_url, baseline_ratio * 1.8)

                # A canceled navigation must also leave the source document at its saved zoom.
                requested.clear()
                release_response.clear()
                script("location.href = arguments[0]", target_url)
                assert requested.wait(timeout=30), "Canceled destination request did not arrive"
                script("window.stop()")
                state = script("return [location.href, devicePixelRatio]")
                assert state == [source_url, baseline_ratio * 1.8], f"Canceled navigation changed zoom: {state}"
                release_response.set()
                print("PASS: site zoom follows the committed document")
            finally:
                release_response.set()
                if session is not None:
                    helpers.request(port, "DELETE", f"/session/{session}")
                server.shutdown()
                server.server_close()


def test_force_dark(webdriver_binary):
    class Handler(http.server.BaseHTTPRequestHandler):
        def do_GET(self):
            if self.path == "/favicon.ico":
                self.send_error(404)
                return
            self.send_response(200)
            self.send_header("Content-Type", "text/html")
            self.send_header("Cache-Control", "no-store")
            self.end_headers()
            self.wfile.write(b"<!doctype html><title>Force-dark test</title><body style='background: white'>")

        def log_message(self, format, *args):
            pass

    def png_pixel(png, x, y):
        assert png[:8] == b"\x89PNG\r\n\x1a\n" and png[12:16] == b"IHDR", "Screenshot is not a PNG"
        width, _, bit_depth, color_type, _, _, interlace = struct.unpack(">IIBBBBB", png[16:29])
        assert bit_depth == 8 and color_type in (2, 6) and interlace == 0, "Unsupported PNG format"
        channels = 4 if color_type == 6 else 3
        offset = 8
        data = b""
        while offset < len(png):
            (length,) = struct.unpack(">I", png[offset : offset + 4])
            if png[offset + 4 : offset + 8] == b"IDAT":
                data += png[offset + 8 : offset + 8 + length]
            offset += 12 + length
        raw = zlib.decompress(data)
        stride = width * channels
        previous = bytearray(stride)
        for row in range(y + 1):
            filter_type = raw[row * (stride + 1)]
            line = bytearray(raw[row * (stride + 1) + 1 : (row + 1) * (stride + 1)])
            for i in range(stride):
                left = line[i - channels] if i >= channels else 0
                up = previous[i]
                up_left = previous[i - channels] if i >= channels else 0
                if filter_type == 1:
                    line[i] = (line[i] + left) & 0xFF
                elif filter_type == 2:
                    line[i] = (line[i] + up) & 0xFF
                elif filter_type == 3:
                    line[i] = (line[i] + (left + up) // 2) & 0xFF
                elif filter_type == 4:
                    estimate = left + up - up_left
                    distances = (abs(estimate - left), abs(estimate - up), abs(estimate - up_left))
                    predictor = (left, up, up_left)[distances.index(min(distances))]
                    line[i] = (line[i] + predictor) & 0xFF
            previous = line
        return tuple(previous[x * channels : x * channels + 3])

    with tempfile.TemporaryDirectory() as directory:
        config = Path(directory) / "config"
        config.mkdir()
        (config / "Settings.json").write_text(json.dumps({"content": {"enableForceDark": True}}))
        server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
        threading.Thread(target=server.serve_forever, daemon=True).start()
        with helpers.webdriver_process(webdriver_binary, profile=directory) as (_, port):
            session = None
            try:
                helpers.wait_for_port(port)
                session = helpers.create_session(port)

                def command(method, path, body=None):
                    status, payload, raw = helpers.request(port, method, f"/session/{session}/{path}", body)
                    return status, payload, raw

                def wait_for_page(url):
                    deadline = time.monotonic() + 30
                    state = None
                    while time.monotonic() < deadline:
                        status, payload, raw = command(
                            "POST",
                            "execute/sync",
                            {"script": "return [location.href, document.readyState]", "args": []},
                        )
                        if (
                            status == 500
                            and payload["value"]["message"] == "WebContent was replaced while executing the command"
                        ):
                            continue
                        assert status == 200, raw
                        state = payload["value"]
                        if state == [url, "complete"]:
                            return
                        time.sleep(0.01)
                    raise AssertionError(f"Expected {url}, got {state}")

                def assert_force_dark(description):
                    status, payload, raw = command("GET", "screenshot")
                    assert status == 200, raw
                    color = png_pixel(base64.b64decode(payload["value"]), 10, 10)
                    assert max(color) < 128, f"{description} painted its white background as {color}"

                source_url = f"http://127.0.0.1:{server.server_port}/source"
                target_url = f"http://localhost:{server.server_port}/target"

                status, _, raw = command("POST", "url", {"url": source_url})
                assert status == 200, raw
                wait_for_page(source_url)
                assert_force_dark("The tab's first page")

                # A cross-site navigation moves the tab into a page of another process.
                status, _, raw = command("POST", "url", {"url": target_url})
                assert status == 200, raw
                wait_for_page(target_url)
                assert_force_dark("A cross-site navigation's page")

                status, _, raw = command("POST", "back", {})
                assert status == 200, raw
                wait_for_page(source_url)
                assert_force_dark("A cross-site traversal's page")
                print("PASS: force-dark follows the tab into every page")
            finally:
                if session is not None:
                    helpers.request(port, "DELETE", f"/session/{session}")
                server.shutdown()
                server.server_close()


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("webdriver_binary")
    args = parser.parse_args()
    test_site_zoom(args.webdriver_binary)
    test_force_dark(args.webdriver_binary)


if __name__ == "__main__":
    main()
