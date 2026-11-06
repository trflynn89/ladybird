#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

import http.server
import json

from typing import cast

import devtools_test_utils as devtools

from test_utils import cleanup
from test_utils import run_test_cases
from test_utils import running_http_server
from test_utils import stop_process
from test_utils import temporary_profile

USER_AGENT_PRESET = "Safari macOS Desktop"


class Handler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        if self.path == "/favicon.ico":
            self.send_error(404)
            return
        self.send_response(200)
        self.send_header("Content-Type", "text/html")
        self.send_header("Cache-Control", "no-store")
        self.end_headers()
        if self.path == "/child":
            # The child frame reports what its own process answers, and what it sends on the wire.
            body = """<!doctype html><title>/child</title><script>
                parent.postMessage(JSON.stringify({ userAgent: navigator.userAgent, languages: navigator.languages }), "*");
            </script>"""
        elif self.path == "/parent":
            server_port = cast(http.server.HTTPServer, self.server).server_port
            body = f"""<!doctype html><title>/parent</title><script>
                window.child = null;
                addEventListener("message", event => {{ window.child = JSON.parse(event.data); }});
            </script><iframe src="http://localhost:{server_port}/child"></iframe>"""
        else:
            body = f"<!doctype html><title>{self.path}</title>"
        self.wfile.write(body.encode())

    def log_message(self, format, *args):
        pass


def window_geometry(client):
    return json.loads(client.evaluate("JSON.stringify([outerWidth, outerHeight, innerWidth, innerHeight])"))


def test_user_agent_preset_reaches_child_frames(ladybird_binary):
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    with running_http_server(server), temporary_profile() as directory:
        profile = directory / "profile"
        parent_url = f"http://127.0.0.1:{server.server_port}/parent"
        process, _, client = devtools.launch(
            ladybird_binary, profile, parent_url, f"--user-agent-preset={USER_AGENT_PRESET}", expose_internals=False
        )
        try:
            devtools.wait_until("the tab shows the parent", lambda: client.tab().get("title") == "/parent")
            client.watch_frame_targets()

            parent = json.loads(
                client.evaluate("JSON.stringify({ userAgent: navigator.userAgent, languages: navigator.languages })")
            )
            assert "Safari" in parent["userAgent"] and "Ladybird" not in parent["userAgent"], parent

            # The child frame is cross-site, so another process hosts it: it has to answer with the same spoofed
            # User-Agent and the same languages as the page that frames it.
            devtools.wait_until("the child frame reports", lambda: client.evaluate("window.child !== null"))
            child = json.loads(client.evaluate("JSON.stringify(window.child)"))
            assert child == parent, (child, parent)

            print("PASS: A child frame in another process gets the tab's User-Agent preset and languages")
        finally:
            cleanup(client.close, lambda: stop_process(process))


def test_window_size_reaches_every_page(ladybird_binary):
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    with running_http_server(server), temporary_profile() as directory:
        profile = directory / "profile"
        source_url = f"http://127.0.0.1:{server.server_port}/source"
        target_url = f"http://localhost:{server.server_port}/target"
        process, _devtools_port, client = devtools.launch(ladybird_binary, profile, source_url)
        try:
            devtools.wait_for_page(client, source_url, "/source")
            client.watch_frame_targets()

            # A headless window is exactly its viewport, so the page's window size is its viewport size.
            outer_width, outer_height, inner_width, inner_height = window_geometry(client)
            assert outer_width > 0 and outer_height > 0, (outer_width, outer_height)
            assert (outer_width, outer_height) == (inner_width, inner_height), (
                outer_width,
                outer_height,
                inner_width,
                inner_height,
            )

            # A cross-site navigation moves the tab into a page of another process, which is told the window size the
            # tab's view last got, like every page the tab comes to be hosted by.
            client.evaluate(f"setTimeout(() => location.href = {json.dumps(target_url)}, 0)")
            devtools.wait_for_page(client, target_url, "/target")
            geometry = window_geometry(client)
            assert geometry == [outer_width, outer_height, inner_width, inner_height], geometry

            print("PASS: Every page the tab is hosted by reports the window size")
        finally:
            cleanup(client.close, lambda: stop_process(process))


if __name__ == "__main__":
    run_test_cases(
        [test_user_agent_preset_reaches_child_frames, test_window_size_reaches_every_page],
        "ladybird_binary",
    )
