#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

import argparse
import http.server
import json
import runpy
import subprocess
import tempfile
import threading
import time

from pathlib import Path

helpers = runpy.run_path(str(Path(__file__).with_name("test-webdriver-delete-session.py")))


def run_test(webdriver_binary):
    class Handler(http.server.BaseHTTPRequestHandler):
        def do_GET(self):
            self.send_response(200)
            self.send_header("Content-Type", "text/html")
            self.end_headers()
            self.wfile.write(b"<!DOCTYPE html><title>permissions settings</title>")

        def log_message(self, format, *args):
            pass

    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    threading.Thread(target=server.serve_forever, daemon=True).start()
    test_origin = f"http://127.0.0.1:{server.server_address[1]}"
    with tempfile.TemporaryDirectory() as directory:
        config = Path(directory) / "config"
        config.mkdir()
        (config / "Settings.json").write_text(json.dumps({"firstRunComplete": True}))
        permissions_file = config / "Permissions.json"
        permissions_file.write_text(
            json.dumps(
                {
                    "version": 1,
                    "decisions": [
                        {"origin": "https://example.com", "permission": "geolocation", "decision": "allow"},
                        {"origin": "https://talk.example.org", "permission": "microphone", "decision": "block"},
                    ],
                    "embargoes": [],
                }
            )
        )

        port = helpers["unused_port"]()
        process = subprocess.Popen(
            [webdriver_binary, "--headless", "-l", "127.0.0.1", "-p", str(port), "--profile-path", directory]
        )
        session = None
        try:
            helpers["wait_for_port"](port)
            session = helpers["create_session"](port)

            def command(path, body=None, method="POST"):
                status, payload, raw = helpers["request"](port, method, f"/session/{session}/{path}", body)
                assert status == 200, raw
                return payload["value"]

            def script(source, *args):
                return command("execute/sync", {"script": source, "args": list(args)})

            def rows(hash, permission):
                command("url", {"url": f"about:settings#{hash}"})
                return command(
                    "execute/async",
                    {
                        "script": """
                        const done = arguments[1];
                        document.addEventListener('WebUIMessage', function listener(event) {
                            if (event.detail.name !== 'loadSettings') return;
                            document.removeEventListener('WebUIMessage', listener);
                            setTimeout(() => done(Array.from(document.querySelectorAll('#site-settings-list .dialog-list-item'))
                                .map(row => [row.dataset.origin, row.querySelector('select').value])));
                        });
                        ladybird.sendMessage('loadCurrentSettings');
                    """,
                        "args": [permission],
                    },
                )

            def change(source, *args):
                return command(
                    "execute/async",
                    {
                        "script": """
                        const done = arguments[arguments.length - 1];
                        document.addEventListener('WebUIMessage', function listener(event) {
                            if (event.detail.name !== 'loadSettings') return;
                            document.removeEventListener('WebUIMessage', listener);
                            setTimeout(done);
                        });
                        """
                        + source,
                        "args": list(args),
                    },
                )

            def saved_decisions():
                deadline = time.time() + 5
                while True:
                    # The browser rewrites the file in place, so a read may overlap a write and see part of it.
                    try:
                        decisions = json.loads(permissions_file.read_text())["decisions"]
                    except (OSError, ValueError, KeyError):
                        decisions = None
                    if decisions is not None:
                        yield decisions
                    if time.time() > deadline:
                        return
                    time.sleep(0.1)

            def wait_for_saved(condition):
                for decisions in saved_decisions():
                    if condition(decisions):
                        return decisions
                raise AssertionError(f"Permissions.json never matched: {permissions_file.read_text()}")

            assert rows("location-permissions", "geolocation") == [["https://example.com", "allow"]]
            assert rows("microphone-permissions", "microphone") == [["https://talk.example.org", "block"]]

            rows("location-permissions", "geolocation")
            script("""
                const select = document.querySelector('#site-settings-list .dialog-list-item select');
                select.value = 'block';
                select.dispatchEvent(new Event('change'));
            """)
            wait_for_saved(
                lambda decisions: (
                    {"origin": "https://example.com", "permission": "geolocation", "decision": "block"} in decisions
                )
            )

            script("document.querySelector('#site-settings-list .dialog-list-item button').click();")
            wait_for_saved(lambda decisions: all(entry["origin"] != "https://example.com" for entry in decisions))

            for permission, hash in (("geolocation", "location-permissions"), ("microphone", "microphone-permissions")):
                rows(hash, permission)
                assert script("return document.querySelector('#site-settings-policy').value") == "ask"
                for decision in ("allow", "block"):
                    change(
                        """
                        document.querySelector('#site-settings-input').value = 'https://added.example/path';
                        document.querySelector('#site-settings-decision').value = arguments[0];
                        document.querySelector('#site-settings-add').click();
                    """,
                        decision,
                    )
                    wait_for_saved(
                        lambda decisions, permission=permission, decision=decision: (
                            {"origin": "https://added.example", "permission": permission, "decision": decision}
                            in decisions
                        )
                    )

                script("""
                    document.querySelector('#site-settings-input').value = 'not a URL';
                    document.querySelector('#site-settings-add').click();
                """)
                assert not script("return document.querySelector('#site-settings-input').validity.valid")

                change(
                    """
                    document.querySelector('#site-settings-input').value = arguments[0];
                    document.querySelector('#site-settings-decision').value = 'allow';
                    document.querySelector('#site-settings-add').click();
                """,
                    test_origin,
                )

                for policy in ("block", "ask"):
                    change(
                        """
                        const select = document.querySelector('#site-settings-policy');
                        select.value = arguments[0];
                        select.dispatchEvent(new Event('change'));
                    """,
                        policy,
                    )
                    saved = json.loads((config / "Settings.json").read_text())
                    assert saved[permission]["policy"] == policy, saved
                    command("url", {"url": test_origin})
                    state = command(
                        "execute/async",
                        {
                            "script": "navigator.permissions.query({name: arguments[0]}).then(status => arguments[1](status.state));",
                            "args": [permission],
                        },
                    )
                    assert state == ("denied" if policy == "block" else "granted"), (permission, policy, state)
                    rows(hash, permission)

                change("document.querySelector('#site-settings-remove-all').click();")
                wait_for_saved(
                    lambda decisions, permission=permission: all(
                        entry["permission"] != permission for entry in decisions
                    )
                )
                assert script("return document.querySelector('#site-settings-list').textContent") == "No sites added"

            # Switching hashes while the shared dialog is open updates its controls without closing it.
            script("location.hash = 'autoplay';")
            assert script("return document.querySelector('#site-settings').open")
            assert script("return document.querySelector('#site-settings-title').textContent") == "Autoplay"
            change("""
                document.querySelector('#site-settings-input').value = 'https://autoplay.example';
                document.querySelector('#site-settings-add').click();
            """)
            wait_for_saved(
                lambda decisions: (
                    {"origin": "https://autoplay.example", "permission": "autoplay", "decision": "allow"} in decisions
                )
            )
            for decision in ("allow-if-inaudible", "block"):
                change(
                    """
                    const select = document.querySelector('#site-settings-list .dialog-list-item select');
                    select.value = arguments[0];
                    select.dispatchEvent(new Event('change'));
                """,
                    decision,
                )
                wait_for_saved(
                    lambda decisions, decision=decision: (
                        {"origin": "https://autoplay.example", "permission": "autoplay", "decision": decision}
                        in decisions
                    )
                )
            for policy in ("allow", "block", "allow-if-inaudible"):
                change(
                    """
                    const select = document.querySelector('#site-settings-policy');
                    select.value = arguments[0];
                    select.dispatchEvent(new Event('change'));
                """,
                    policy,
                )
                assert json.loads((config / "Settings.json").read_text())["autoplay"]["policy"] == policy
            change("document.querySelector('#site-settings-remove-all').click();")

            print("PASS: shared site settings dialogs load, add, change, and remove permissions and policies")
        finally:
            if session is not None:
                helpers["request"](port, "DELETE", f"/session/{session}")
            server.shutdown()
            process.terminate()
            try:
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait()


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("webdriver_binary")
    args = parser.parse_args()
    run_test(args.webdriver_binary)
