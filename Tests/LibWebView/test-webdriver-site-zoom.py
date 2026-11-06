#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

import http.server
import sys
import threading

import webdriver_test_utils as helpers

from test_utils import cleanup
from test_utils import running_http_server
from test_utils import temporary_profile

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


with temporary_profile(settings={"content": {"zoomPerHost": {"127.0.0.1": 1.8}}}) as directory:
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    with running_http_server(server):
        with helpers.webdriver_session(sys.argv[1], "--headless", "--profile-path", directory) as browser:
            try:
                command = browser.command
                script = browser.script

                def wait_for_page(url, pixel_ratio):
                    browser.wait_for_script(
                        "return [location.href, document.readyState, devicePixelRatio]",
                        expected=[url, "complete", pixel_ratio],
                    )

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
                cleanup(release_response.set)
