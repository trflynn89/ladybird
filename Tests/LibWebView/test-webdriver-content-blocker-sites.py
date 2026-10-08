#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

import http.server
import json
import runpy
import subprocess
import sys
import tempfile
import threading

from pathlib import Path

helpers = runpy.run_path(str(Path(__file__).with_name("test-webdriver-delete-session.py")))
request = helpers["request"]


class Handler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        assert isinstance(self.server, http.server.HTTPServer)
        if self.path == "/redirect":
            self.send_response(302)
            self.send_header("Location", f"http://127.0.0.1:{self.server.server_port}/page")
            self.end_headers()
            return
        self.send_response(200)
        self.send_header("Cache-Control", "no-store")
        self.send_header("Content-Type", "text/javascript" if self.path == "/blocked-resource.js" else "text/html")
        self.end_headers()
        if self.path == "/blocked-resource.js":
            self.wfile.write(b"window.resourceAllowed = true;")
        else:
            self.wfile.write(b'<!doctype html><div class="advertisement">Advertisement</div>')
            self.wfile.write(b'<script src="/blocked-resource.js"></script>')
            if self.path == "/page":
                self.wfile.write(f'<iframe src="http://127.0.0.2:{self.server.server_port}/frame"></iframe>'.encode())
                self.wfile.write(b'<iframe srcdoc="<div class=advertisement>Embedded advertisement</div>"></iframe>')

    def log_message(self, format, *args):
        pass


with tempfile.TemporaryDirectory() as directory:
    profile = Path(directory)
    (profile / "config").mkdir()
    (profile / "config" / "Settings.json").write_text(
        json.dumps(
            {
                "contentBlockers": {
                    "enabled": True,
                    "disabledSites": ["LOCALHOST."],
                    "customFilters": "/blocked-resource.js\n##.advertisement",
                }
            }
        )
    )
    server = http.server.ThreadingHTTPServer(("0.0.0.0", 0), Handler)
    threading.Thread(target=server.serve_forever, daemon=True).start()
    try:
        # Restart the browser to verify that profile exceptions remain effective.
        for _ in range(2):
            port = helpers["unused_port"]()
            process = subprocess.Popen(
                [
                    sys.argv[1],
                    "--headless",
                    "--site-isolation",
                    "iframe",
                    "-l",
                    "127.0.0.1",
                    "-p",
                    str(port),
                    "--profile-path",
                    directory,
                ]
            )
            session = None
            try:
                helpers["wait_for_port"](port)
                session = helpers["create_session"](port)

                def command(path, body, port=port, session=session):
                    status, payload, raw = request(port, "POST", f"/session/{session}/{path}", body)
                    assert status == 200, raw
                    return payload["value"]

                exempt_tab = request(port, "GET", f"/session/{session}/window")[1]["value"]
                blocked_tab = command("window/new", {"type": "tab"})["handle"]
                for tab, hostname, enabled in [(exempt_tab, "localhost", False), (blocked_tab, "127.0.0.1", True)]:
                    command("window", {"handle": tab})
                    command("url", {"url": f"http://{hostname}:{server.server_port}/page"})
                    assert command(
                        "execute/sync",
                        {
                            "script": "return [!!window.resourceAllowed, getComputedStyle(document.querySelector('.advertisement')).display]",
                            "args": [],
                        },
                    ) == [not enabled, "none" if enabled else "block"]
                    for frame in range(2):
                        command("frame", {"id": frame})
                        assert command(
                            "execute/sync",
                            {
                                "script": "return getComputedStyle(document.querySelector('.advertisement')).display",
                                "args": [],
                            },
                        ) == ("none" if enabled else "block")
                        if frame == 0:
                            assert command(
                                "execute/sync", {"script": "return !!window.resourceAllowed", "args": []}
                            ) == (not enabled)
                        command("frame", {"id": None})
                # A destination reached from an exempt site must apply its own policy.
                command("window", {"handle": exempt_tab})
                command("url", {"url": f"http://localhost:{server.server_port}/redirect"})
                assert command("execute/sync", {"script": "return !!window.resourceAllowed", "args": []}) is False
            finally:
                if session:
                    request(port, "DELETE", f"/session/{session}")
                process.terminate()
                process.wait(timeout=10)
    finally:
        server.shutdown()
