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
        self.wfile.write(b"<!doctype html><title>Site settings</title>")

    def log_message(self, *_args):
        pass


def run_test(webdriver_binary):
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    threading.Thread(target=server.serve_forever, daemon=True).start()
    try:
        with tempfile.TemporaryDirectory() as directory:
            port = helpers["unused_port"]()
            process = subprocess.Popen(
                [
                    webdriver_binary,
                    "--headless",
                    "--expose-experimental-interfaces",
                    "--expose-internals-object",
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

                def command(path, body=None, method="POST"):
                    status, payload, raw = helpers["request"](port, method, f"/session/{session}/{path}", body)
                    assert status == 200, raw
                    return payload["value"]

                def script(source, *args, asynchronous=False):
                    return command(
                        "execute/async" if asynchronous else "execute/sync", {"script": source, "args": list(args)}
                    )

                def switch(handle):
                    command("window", {"handle": handle})

                def new_tab(url):
                    handle = command("window/new", {"type": "tab"})["handle"]
                    switch(handle)
                    command("url", {"url": url})
                    return handle

                settings_tab = command("window", method="GET")
                command("url", {"url": "about:settings#permissions"})
                script(
                    """
                    const done = arguments[0];
                    document.addEventListener('WebUIMessage', function listener(event) {
                        if (event.detail.name !== 'loadSettings') return;
                        document.removeEventListener('WebUIMessage', listener);
                        done();
                    });
                    ladybird.sendMessage('loadCurrentSettings');
                    """,
                    asynchronous=True,
                )

                available = script(
                    """
                    const done = arguments[0];
                    document.addEventListener('WebUIMessage', function listener(event) {
                        if (event.detail.name !== 'loadFeatures') return;
                        document.removeEventListener('WebUIMessage', listener);
                        done(event.detail.data.geolocation);
                    });
                    ladybird.sendMessage('loadFeatures');
                    """,
                    asynchronous=True,
                )

                def change(name, data):
                    switch(settings_tab)
                    script(
                        """
                        const [name, data, done] = arguments;
                        document.addEventListener('WebUIMessage', function listener(event) {
                            if (event.detail.name !== 'loadSettings') return;
                            document.removeEventListener('WebUIMessage', listener);
                            done();
                        });
                        ladybird.sendMessage(name, data);
                        """,
                        name,
                        data,
                        asynchronous=True,
                    )

                origin = f"http://localhost:{server.server_port}"
                page = new_tab(origin)

                def permission():
                    return script(
                        "const done = arguments[0]; navigator.permissions.query({name: 'geolocation'}).then(p => done(p.state));",
                        asynchronous=True,
                    )

                def autoplay_decision():
                    return script(
                        """
                        const done = arguments[0];
                        const video = document.createElement('video');
                        Promise.race([
                            video.play().then(() => 'playing', error => error.name),
                            new Promise(resolve => setTimeout(() => resolve('pending'), 0)),
                        ]).then(result => { video.pause(); done(result); });
                        """,
                        asynchronous=True,
                    )

                assert permission() == "denied"
                if available:
                    change("setSiteSettingPolicy", {"setting": "geolocation", "policy": "allow"})
                    switch(page)
                    assert permission() == "granted", "An existing process did not receive a permission update"

                later_page = new_tab(f"http://127.0.0.1:{server.server_port}")
                assert permission() == ("granted" if available else "denied"), (
                    "A new process did not receive its initial settings"
                )
                if available:
                    change("setSiteSettingPolicy", {"setting": "geolocation", "policy": "block"})
                    switch(later_page)
                    assert permission() == "denied"
                    switch(page)
                    assert permission() == "denied", "Revocation did not reach every existing process"

                # WebDriver forces autoplay on. With no source, an allowed play() stays pending without producing audio.
                switch(page)
                assert autoplay_decision() == "pending"
                script("internals.setAutoplayPolicy('block-audio-and-video')")
                assert autoplay_decision() == "NotAllowedError"
                change("setSiteSettingPolicy", {"setting": "autoplay", "policy": "block-audio-and-video"})
                switch(page)
                assert autoplay_decision() == "pending", (
                    "A browser update did not restore the command-line autoplay policy"
                )
                script("internals.setAutoplayPolicy('block-audio-and-video')")
                change("addSiteSettingFilter", {"setting": "autoplay", "filter": origin})
                switch(page)
                assert autoplay_decision() == "pending", "An autoplay allowlist update did not reach WebContent"
                print("PASS: browser site settings initialize processes and update permission and autoplay decisions")
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


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("webdriver_binary")
    run_test(parser.parse_args().webdriver_binary)
