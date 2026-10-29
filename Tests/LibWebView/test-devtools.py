#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

import argparse
import http.server
import json
import tempfile
import threading

from pathlib import Path

from devtools_test import DevToolsClient
from devtools_test import launch
from devtools_test import stop
from devtools_test import wait_for_page

loads_lock = threading.Lock()
loads = {}


CATCH_UP_PAGE = b"""<!doctype html><title>catch-up</title>
<style>
    .container { container-type: inline-size; display: contents; }
    .item { width: 50cqw; }
</style>
<div class="container"><div class="item">item</div></div>
<script>window.loaded = true;</script>
"""
WORKER_PAGE = b"""<!doctype html><title>worker-console</title>
<script>
window.onerror = (message, filename, line, column) => {
    window.lastError = { message, filename, line, column };
};
</script>
"""
WORKER = b'\nthrow new Error("worker console sentinel");\n'


def load_count(path):
    with loads_lock:
        return loads.get(path, 0)


class Handler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        if self.path == "/favicon.ico":
            self.send_error(404)
            return
        with loads_lock:
            load = loads.get(self.path, 0) + 1
            loads[self.path] = load
        self.send_response(200)
        self.send_header("Content-Type", "text/javascript" if self.path == "/worker.js" else "text/html")
        self.send_header("Cache-Control", "no-store")
        self.end_headers()
        if self.path == "/catch-up":
            body = CATCH_UP_PAGE
        elif self.path == "/worker-console":
            body = WORKER_PAGE
        elif self.path == "/worker.js":
            body = WORKER
        else:
            body = f"<!doctype html><title>{self.path}</title><script>window.load = {load};</script>".encode()
        self.wfile.write(body)

    def log_message(self, format, *args):
        pass


def assert_page_survives_disconnects(devtools_port, path, load):
    # The closed client disconnected DevTools from the page that displays the tab, so that page must still hold the
    # document it loaded: The tab would have reloaded into another page had it crashed.
    client = DevToolsClient(devtools_port)
    try:
        client.watch_frame_targets()
        assert client.evaluate("window.load") == load
    finally:
        client.close()
    assert load_count(path) == load, f"{path} was loaded {load_count(path)} times"


def test_cross_site_navigation(ladybird_binary, profile, server):
    source_url = f"http://127.0.0.1:{server.server_port}/source"
    target_url = f"http://localhost:{server.server_port}/target"
    process, devtools_port, client = launch(ladybird_binary, profile, source_url)
    try:
        try:
            wait_for_page(client, source_url, "/source")
            client.watch_frame_targets()

            # A cross-site navigation moves the tab into a page of another process.
            client.evaluate(f"setTimeout(() => location.href = {json.dumps(target_url)}, 0)")
            wait_for_page(client, target_url, "/target")
        finally:
            client.close()

        assert_page_survives_disconnects(devtools_port, "/target", 1)
        assert process.poll() is None, "The browser exited"
        print("PASS: A DevTools client follows the tab into a cross-site navigation's page")
    finally:
        stop(process)


def test_crash_recovery(ladybird_binary, profile, server):
    url = f"http://127.0.0.1:{server.server_port}/crash"
    process, devtools_port, client = launch(ladybird_binary, profile, url)
    try:
        try:
            wait_for_page(client, url, "/crash")
            client.watch_frame_targets()
            assert client.evaluate("window.load") == 1

            # The headless browser restores the crashed tab into a page of a new process, and the client inspects
            # that page's document once it's active.
            panic = "internals.panicRenderStateForTesting()"
            client.send({"to": client.console, "type": "evaluateJSAsync", "text": panic})
            client.follow_frame_target()
            assert client.evaluate("window.load") == 2
        finally:
            client.close()

        assert_page_survives_disconnects(devtools_port, "/crash", 2)
        assert process.poll() is None, "The browser exited"
        print("PASS: A DevTools client follows the tab into the page that replaces a crashed one")
    finally:
        stop(process)


def test_catch_up_layout(ladybird_binary, profile, server):
    url = f"http://127.0.0.1:{server.server_port}/catch-up"
    process, devtools_port, client = launch(ladybird_binary, profile, url)
    try:
        try:
            wait_for_page(client, url, "catch-up")
            # The first client lays the document out once more, to collect the layout inspection data.
            client.watch_frame_targets()
            assert client.evaluate("window.loaded") is True
        finally:
            client.close()
        assert process.poll() is None, "The browser exited"
        print("PASS: A DevTools client's catch-up layout settles")
    finally:
        stop(process)


def test_worker_error_console(ladybird_binary, profile, server):
    origin = f"http://127.0.0.1:{server.server_port}"
    url = origin + "/worker-console"
    worker_url = origin + "/worker.js"
    process, _, client = launch(ladybird_binary, profile, url)
    try:
        wait_for_page(client, url, "worker-console")
        client.watch_frame_targets()
        errors = []
        receive = client.receive

        def receive_and_record_errors():
            packet = receive()
            if packet.get("type") == "resources-available-array":
                for resource_type, resources in packet.get("array", []):
                    if resource_type == "error-message":
                        errors.extend(resource["pageError"] for resource in resources)
            return packet

        # Keep error resources even if they arrive while evaluation is retried after a target change.
        client.receive = receive_and_record_errors
        client.evaluate("window.worker = new Worker('/worker.js'); undefined")
        if not errors:
            client.receive_matching(lambda packet: bool(errors))
        preview = errors[0]["exception"]["preview"]
        event = json.loads(client.evaluate("JSON.stringify(window.lastError)"))
        assert "worker console sentinel" in event["message"], event
        assert preview["message"] == event["message"], preview
        assert event["filename"] == worker_url, event
        assert event["line"] == 2 and event["column"] > 0, event
        expected_location = f"@{worker_url}:{event['line']}:{event['column']}\n"
        assert preview.get("stack") == expected_location, preview
        assert not errors[0]["isPromiseRejection"], errors[0]
        print("PASS: An unhandled worker exception keeps its message and source location in the console")
    finally:
        client.close()
        stop(process)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("ladybird_binary")
    ladybird_binary = parser.parse_args().ladybird_binary
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    threading.Thread(target=server.serve_forever, daemon=True).start()
    try:
        with tempfile.TemporaryDirectory(prefix="ladybird-devtools-") as directory:
            profile = Path(directory)
            test_cross_site_navigation(ladybird_binary, profile / "navigation", server)
            test_crash_recovery(ladybird_binary, profile / "crash", server)
            test_catch_up_layout(ladybird_binary, profile / "catch-up", server)
            test_worker_error_console(ladybird_binary, profile / "worker", server)
    finally:
        server.shutdown()
        server.server_close()


if __name__ == "__main__":
    main()
