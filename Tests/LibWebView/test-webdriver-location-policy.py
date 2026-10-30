#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

import argparse
import json
import runpy
import subprocess
import tempfile

from pathlib import Path

helpers = runpy.run_path(str(Path(__file__).with_name("test-webdriver-delete-session.py")))


def run_browser(webdriver_binary, settings, check):
    with tempfile.TemporaryDirectory() as directory:
        config = Path(directory) / "config"
        config.mkdir()
        settings_file = config / "Settings.json"
        settings_file.write_text(json.dumps(settings))
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

            def policy_state():
                command("url", {"url": "about:settings#location-permissions"})
                return command(
                    "execute/async",
                    {
                        "script": """
                        const done = arguments[0];
                        const select = document.querySelector('#site-settings-policy');
                        document.addEventListener('WebUIMessage', function listener(event) {
                            if (event.detail.name !== 'loadSettings') return;
                            document.removeEventListener('WebUIMessage', listener);
                            setTimeout(() => done(select.value));
                        });
                        // The page may have loaded its settings already, so ask for them again.
                        if (window.ladybird) ladybird.sendMessage('loadCurrentSettings');
                    """,
                        "args": [],
                    },
                )

            def toggle_policy():
                command(
                    "execute/async",
                    {
                        "script": """
                        const done = arguments[0];
                        document.addEventListener('WebUIMessage', function listener(event) {
                            if (event.detail.name !== 'loadSettings') return;
                            document.removeEventListener('WebUIMessage', listener);
                            done();
                        });
                        const select = document.querySelector('#site-settings-policy');
                        select.value = select.value === 'ask' ? 'block' : 'ask';
                        select.dispatchEvent(new Event('change'));
                    """,
                        "args": [],
                    },
                )

            check(policy_state, toggle_policy, settings_file)

            # The Permissions API is exposed to every site, not only under the experimental-interfaces flag.
            command("url", {"url": "about:blank"})
            exposed = command(
                "execute/sync",
                {
                    "script": "return [typeof navigator.permissions, typeof PermissionStatus, typeof Permissions];",
                    "args": [],
                },
            )
            assert exposed == ["object", "function", "function"], exposed
        finally:
            if session is not None:
                helpers["request"](port, "DELETE", f"/session/{session}")
            process.terminate()
            try:
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                process.kill()
                process.wait()


def run_test(webdriver_binary):
    def check_default_policy(policy_state, toggle_policy, settings_file):
        assert policy_state() == "ask", "the location policy should default to letting sites ask"
        toggle_policy()
        saved = json.loads(settings_file.read_text())
        assert saved["geolocation"]["policy"] == "block", saved
        toggle_policy()
        saved = json.loads(settings_file.read_text())
        assert saved["geolocation"]["policy"] == "ask", saved

    run_browser(webdriver_binary, {}, check_default_policy)
    run_browser(webdriver_binary, {"geolocation": {"policy": 42}}, check_default_policy)

    def check_blocking_profile(policy_state, toggle_policy, settings_file):
        assert policy_state() == "block", "a saved block should keep sites from asking"
        toggle_policy()
        assert json.loads(settings_file.read_text())["geolocation"]["policy"] == "ask"

    run_browser(webdriver_binary, {"geolocation": {"policy": "block"}}, check_blocking_profile)
    print("PASS: the location policy defaults to letting sites ask, and a saved block stays")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("webdriver_binary")
    args = parser.parse_args()
    run_test(args.webdriver_binary)
