#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

import argparse
import http.server

import webdriver_test_utils as helpers

from test_utils import running_http_server
from test_utils import temporary_profile


def run_test(webdriver_binary):
    class Handler(http.server.BaseHTTPRequestHandler):
        def do_GET(self):
            self.send_response(200)
            self.send_header("Content-Type", "text/html")
            self.end_headers()
            self.wfile.write(b"<!DOCTYPE html><title>set permission</title>")

        def log_message(self, format, *args):
            pass

    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    with running_http_server(server):
        with temporary_profile(settings={"firstRunComplete": True}) as directory:
            with helpers.webdriver_session(webdriver_binary, "--headless", "--profile-path", directory) as browser:
                request = browser.request

                def query(name):
                    status, payload, raw = request(
                        "execute/async",
                        {
                            "script": """
                            const done = arguments[1];
                            navigator.permissions.query({ name: arguments[0] }).then(status => done(status.state));
                        """,
                            "args": [name],
                        },
                    )
                    assert status == 200, raw
                    return payload["value"]

                status, _, raw = request("url", {"url": f"http://127.0.0.1:{server.server_address[1]}/"})
                assert status == 200, raw

                for state in ("denied", "granted", "prompt"):
                    status, payload, raw = request(
                        "permissions", {"descriptor": {"name": "geolocation"}, "state": state}
                    )
                    assert status == 200, raw
                    assert payload["value"] is None, raw
                    assert query("geolocation") == state, (state, query("geolocation"))

                for body in (
                    {"descriptor": {"name": "camera"}, "state": "granted"},
                    {"descriptor": {"name": "geolocation"}, "state": "maybe"},
                    {"descriptor": "geolocation", "state": "granted"},
                    {"state": "granted"},
                ):
                    status, payload, raw = request("permissions", body)
                    assert status == 400, raw
                    assert payload["value"]["error"] == "invalid argument", raw

                print("PASS: Set Permission sets the current site's permission, and rejects invalid arguments")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("webdriver_binary")
    args = parser.parse_args()
    run_test(args.webdriver_binary)
