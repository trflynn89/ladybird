#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

import http.server
import subprocess
import sys
import tempfile
import threading

from pathlib import Path


class FixtureServer(http.server.ThreadingHTTPServer):
    def __init__(self, address):
        self.slow_release = threading.Event()
        super().__init__(address, Handler)


class Handler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        assert isinstance(self.server, FixtureServer)
        if self.path == "/slow":
            self.server.slow_release.wait(timeout=4)
        elif self.path == "/disconnect":
            self.close_connection = True
            return
        self.send_response(200)
        self.send_header("Cache-Control", "max-age=3600" if self.path == "/cached-ad.js" else "no-store")
        self.send_header("Content-Type", "text/javascript" if self.path.endswith(".js") else "text/html")
        self.end_headers()
        if self.path == "/cached":
            self.wfile.write(b'<!doctype html><script src="/cached-ad.js"></script>')
            return
        if self.path == "/frames":
            self.wfile.write(
                b'<!doctype html><iframe src="/blocked-frame-one"></iframe><iframe src="/blocked-frame-two"></iframe>'
            )
            return
        if self.path.endswith(".js"):
            self.wfile.write(b"window.allowed = true;")
            return
        self.wfile.write(b'<!doctype html><div class="advertisement">Advertisement</div>')
        self.wfile.write(b'<script src="/blocked.js"></script>')
        self.wfile.write(b'<script src="/blocked-allowed.js"></script>')
        self.wfile.write(b'<link rel="dns-prefetch" href="/blocked.js"><link rel="preconnect" href="/blocked.js">')
        if self.path == "/page":
            self.wfile.write(f'<iframe src="http://127.0.0.2:{self.server.server_port}/frame"></iframe>'.encode())

    def log_message(self, format, *args):
        pass


with tempfile.TemporaryDirectory() as directory:
    rules = Path(directory) / "rules.txt"
    rules.write_text("/cached-ad.js\n/blocked\n/blocked.js\n@@/blocked-allowed.js\n##.advertisement")
    server = FixtureServer(("0.0.0.0", 0))
    threading.Thread(target=server.serve_forever, daemon=True).start()
    try:
        for extra_arguments in ([], ["--disable-content-blocker"]):
            subprocess.run(
                [
                    sys.argv[1],
                    "--temporary-profile",
                    "--site-isolation",
                    "iframe",
                    "--content-blocker-list",
                    str(rules),
                    *extra_arguments,
                    f"http://127.0.0.1:{server.server_port}/page",
                ],
                check=True,
                timeout=60,
            )
    finally:
        server.slow_release.set()
        server.shutdown()
