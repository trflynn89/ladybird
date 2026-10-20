#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

import argparse
import http.server
import runpy
import subprocess
import tempfile
import threading

from pathlib import Path

helpers = runpy.run_path(str(Path(__file__).with_name("test-webdriver-delete-session.py")))


class Handler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        self.send_response(200)
        self.send_header("Content-Type", "text/html")
        self.end_headers()
        self.wfile.write(b"""<!doctype html><title>Geolocation permissions</title>
            <script>
            window.addEventListener('message', async event => {
                const permission = await navigator.permissions.query({name: 'geolocation'});
                event.source.postMessage(permission.state, event.origin);
            });
            </script>""")

    def log_message(self, *_args):
        pass


def run_test(webdriver_binary):
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        with tempfile.TemporaryDirectory() as directory:
            port = helpers["unused_port"]()
            process = subprocess.Popen(
                [
                    webdriver_binary,
                    "--headless",
                    "--expose-experimental-interfaces",
                    "--expose-internals-object",
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

                def command(path, body=None, method="POST"):
                    status, payload, raw = helpers["request"](port, method, f"/session/{session}/{path}", body)
                    assert status == 200, raw
                    return payload["value"]

                def script(source, *args, asynchronous=False):
                    return command(
                        "execute/async" if asynchronous else "execute/sync", {"script": source, "args": list(args)}
                    )

                def settings_message(name, data=None, response="loadSettings"):
                    return script(
                        """
                        const [name, data, response, done] = arguments;
                        document.addEventListener('WebUIMessage', function listener(event) {
                            if (event.detail.name !== response) return;
                            document.removeEventListener('WebUIMessage', listener);
                            done(event.detail.data);
                        });
                        ladybird.sendMessage(name, data);
                        """,
                        name,
                        data,
                        response,
                        asynchronous=True,
                    )

                def open_settings():
                    command("url", {"url": "about:settings#geolocation"})
                    return settings_message("loadCurrentSettings")["geolocation"]

                origin = f"http://localhost:{server.server_port}"
                other_origin = f"http://127.0.0.1:{server.server_port}"
                assert open_settings() == {"policy": "block", "siteFilters": []}
                available = settings_message("loadFeatures", response="loadFeatures")["geolocation"]
                assert script("return document.querySelector('#geolocation-settings').disabled") == (not available)
                assert script("return document.querySelector('#site-settings-title').textContent") == "Geolocation"

                # The two permission types share a dialog, including when switching directly between deep links.
                script("location.hash = 'autoplay'")
                settings_message("loadCurrentSettings")
                assert script("return document.querySelector('#site-settings-title').textContent") == "Autoplay"
                script("location.hash = 'geolocation'")
                settings_message("loadCurrentSettings")
                assert script("return document.querySelector('#site-settings-title').textContent") == "Geolocation"

                changes = settings_message("addSiteSettingFilter", {"setting": "geolocation", "filter": origin})
                assert set(changes) == {"geolocation"}, changes
                assert script(
                    "return document.querySelector('#site-settings-list').textContent.includes(arguments[0])", origin
                )
                script("""
                    window.geolocationItem = document.querySelector('#site-settings-list .dialog-list-item');
                    document.querySelector('#site-settings-input').value = 'unfinished.example';
                """)
                changes = settings_message(
                    "setSiteSettingPolicy", {"setting": "autoplay", "policy": "block-audio-and-video"}
                )
                assert set(changes) == {"autoplay"}, changes
                assert script("""
                    return geolocationItem === document.querySelector('#site-settings-list .dialog-list-item')
                        && document.querySelector('#site-settings-input').value === 'unfinished.example';
                """), "An autoplay-only update replaced the open geolocation dialog"
                script("location.hash = 'autoplay'")
                # Read the DOM on a later turn, without requesting a full settings refresh.
                assert script("return document.querySelector('#site-settings-policy').value") == "block-audio-and-video"
                changes = settings_message("addSiteSettingFilter", {"setting": "geolocation", "filter": other_origin})
                assert set(changes) == {"geolocation"}, changes
                assert script("return document.querySelector('#site-settings-policy').value") == "block-audio-and-video"
                script("location.hash = 'geolocation'")
                assert script(
                    "return document.querySelector('#site-settings-list').textContent.includes(arguments[0])",
                    other_origin,
                )

                def permission(url):
                    command("url", {"url": url})
                    return script(
                        "const done = arguments[0]; navigator.permissions.query({name: 'geolocation'}).then(p => done(p.state));",
                        asynchronous=True,
                    )

                assert permission(origin) == ("granted" if available else "denied")
                assert permission(other_origin) == ("granted" if available else "denied")

                # An independently allowed origin embedded cross-origin must not inherit access.
                assert (
                    script(
                        """
                    const [url, done] = arguments;
                    window.addEventListener('message', event => done(event.data), {once: true});
                    const frame = document.createElement('iframe');
                    frame.src = url;
                    frame.onload = () => frame.contentWindow.postMessage('query', '*');
                    document.body.appendChild(frame);
                    """,
                        origin + "/frame",
                        asynchronous=True,
                    )
                    == "denied"
                )

                assert permission(origin) == ("granted" if available else "denied")
                if available:
                    # Populate a real Geolocation object's cache using emulation, without accessing the system provider.
                    script("internals.setGeolocationEmulatedPosition(1.25, 2.5, 10)")
                    assert (
                        script(
                            """
                        const done = arguments[0];
                        navigator.geolocation.getCurrentPosition(p => done(p.coords.latitude), error => done(error.code));
                        """,
                            asynchronous=True,
                        )
                        == 1.25
                    )
                page_handle = command("window", method="GET")
                settings_handle = command("window/new", {"type": "tab"})["handle"]
                command("window", {"handle": settings_handle})
                open_settings()
                settings_message("removeSiteSettingFilter", {"setting": "geolocation", "filter": origin})
                command("window", {"handle": page_handle})
                assert (
                    script(
                        """
                    const done = arguments[0];
                    navigator.geolocation.getCurrentPosition(() => done('unexpected position'), error => done(error.code), {maximumAge: Infinity});
                    """,
                        asynchronous=True,
                    )
                    == 1
                )
                assert permission(other_origin) == ("granted" if available else "denied")

                # A fresh browser process reads the same policy and exceptions.
                command("", method="DELETE")
                session = helpers["create_session"](port)
                assert open_settings() == {"policy": "block", "siteFilters": [other_origin]}
                settings_message("removeAllSiteSettingFilters", {"setting": "geolocation"})
                assert permission(other_origin) == "denied"
                print("PASS: geolocation settings persist, enforce origin permissions, and revoke access")
            finally:
                if session is not None:
                    helpers["request"](port, "DELETE", f"/session/{session}")
                process.terminate()
                try:
                    process.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    process.kill()
                    process.wait()
    finally:
        server.shutdown()
        server.server_close()
        thread.join()


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("webdriver_binary")
    run_test(parser.parse_args().webdriver_binary)
