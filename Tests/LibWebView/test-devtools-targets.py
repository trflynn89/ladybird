#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

import http.server
import json
import threading

from devtools_test_utils import DevToolsClient
from devtools_test_utils import launch
from devtools_test_utils import wait_for_page
from devtools_test_utils import wait_until
from test_utils import cleanup
from test_utils import run_test_cases
from test_utils import running_http_server
from test_utils import stop_process
from test_utils import temporary_profile

# This container generates no box. Each layout leaves its descendant container unit pending a style update.
CATCH_UP_PAGE = b"""<!doctype html><title>catch-up</title>
<style>
    .container { container-type: inline-size; display: contents; }
    .item { width: 50cqw; }
</style>
<div class="container"><div class="item">item</div></div>
<script>window.loaded = true;</script>
"""

loads_lock = threading.Lock()
loads = {}


def load_count(path):
    with loads_lock:
        return loads.get(path, 0)


class Handler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        if self.path == "/favicon.ico":
            self.send_error(404)
            return
        if self.path == "/catch-up":
            body = CATCH_UP_PAGE
        else:
            with loads_lock:
                load = loads.get(self.path, 0) + 1
                loads[self.path] = load
            body = f"<!doctype html><title>{self.path}</title><script>window.load = {load};</script>".encode()
        self.send_response(200)
        self.send_header("Content-Type", "text/html")
        self.send_header("Cache-Control", "no-store")
        self.end_headers()
        self.wfile.write(body)

    def log_message(self, format, *args):
        pass


def assert_page_survives_disconnects(devtools_port, path, load):
    # The closed client disconnected DevTools from the page that displays the tab, so that page must still hold the
    # document it loaded: The tab would have reloaded into another page had it crashed.
    client = DevToolsClient(devtools_port)
    try:
        client.watch_frame_targets()
        page_load = client.evaluate("window.load")
        assert page_load == load, f"window.load is {page_load!r}"
    finally:
        cleanup(client.close)
    assert load_count(path) == load, f"{path} was loaded {load_count(path)} times"


def test_cross_site_navigation(ladybird_binary, profile, server):
    source_url = f"http://127.0.0.1:{server.server_port}/source"
    target_url = f"http://localhost:{server.server_port}/target"
    process, devtools_port, client = launch(ladybird_binary, profile, source_url)
    try:
        try:
            wait_for_page(client, source_url, "/source")
            client.watch_frame_targets()

            # A cross-site navigation moves the tab into a page of another process. Don't wait for its result: The
            # navigation is what switches the target, so evaluate() would see the result lost, and run the navigation
            # a second time. But the source page's own load can still finish and switch the target before the request
            # arrives, so it needs submit() to hand it to a live console.
            navigate = f"setTimeout(() => location.href = {json.dumps(target_url)}, 0)"
            client.submit(navigate)
            wait_for_page(client, target_url, "/target")
        finally:
            cleanup(client.close)

        assert_page_survives_disconnects(devtools_port, "/target", 1)
        assert process.poll() is None, "The browser exited"
        print("PASS: A DevTools client follows the tab into a cross-site navigation's page")
    finally:
        cleanup(lambda: stop_process(process))


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
            # A target switch can remove the console before the crash request reaches it, and the server then drops
            # the request: submit() asks the new target's console again, until one accepts it.
            client.submit("internals.panicRenderStateForTesting()")
            # A target form for the crashed page itself can still arrive first.
            wait_until("the client inspects the restored page", lambda: client.evaluate("window.load") == 2)
        finally:
            cleanup(client.close)

        assert_page_survives_disconnects(devtools_port, "/crash", 2)
        assert process.poll() is None, "The browser exited"
        print("PASS: A DevTools client follows the tab into the page that replaces a crashed one")
    finally:
        cleanup(lambda: stop_process(process))


def test_tab_lifecycle(ladybird_binary):
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    with running_http_server(server):
        with temporary_profile(prefix="ladybird-devtools-") as directory:
            test_cross_site_navigation(ladybird_binary, directory / "navigation", server)
            test_crash_recovery(ladybird_binary, directory / "crash", server)


def test_catch_up_layout(ladybird_binary):
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    with running_http_server(server):
        url = f"http://127.0.0.1:{server.server_port}/catch-up"
        with temporary_profile(prefix="ladybird-devtools-") as profile:
            process, devtools_port, client = launch(ladybird_binary, profile, url)
            try:
                try:
                    wait_for_page(client, url, "catch-up")
                    # The first client lays the document out once more, to collect the layout inspection data.
                    client.watch_frame_targets()
                    assert client.evaluate("window.loaded") is True
                finally:
                    cleanup(client.close)
                assert process.poll() is None, "The browser exited"
                print("PASS: A DevTools client's catch-up layout settles")
            finally:
                cleanup(lambda: stop_process(process))


if __name__ == "__main__":
    run_test_cases([test_catch_up_layout, test_tab_lifecycle], "ladybird_binary")
