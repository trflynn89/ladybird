#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

import argparse
import json

import webdriver_test_utils as helpers

from test_utils import temporary_profile


def run_browser(webdriver_binary, settings, check):
    with temporary_profile(settings=settings) as directory:
        config = directory / "config"
        settings_file = config / "Settings.json"
        with helpers.webdriver_session(webdriver_binary, "--headless", "--profile-path", directory) as browser:
            command = browser.command

            def switch_state():
                command("url", {"url": "about:settings"})
                return command(
                    "execute/async",
                    {
                        "script": """
                        const done = arguments[0];
                        const toggle = document.querySelector('#enable-notifications');
                        document.addEventListener('WebUIMessage', function listener(event) {
                            if (event.detail.name !== 'loadSettings') return;
                            document.removeEventListener('WebUIMessage', listener);
                            setTimeout(() => done(toggle.checked));
                        });
                        // The page may have loaded its settings already, so ask for them again.
                        if (window.ladybird) ladybird.sendMessage('loadCurrentSettings');
                    """,
                        "args": [],
                    },
                )

            def toggle_switch():
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
                        document.querySelector('#enable-notifications').click();
                    """,
                        "args": [],
                    },
                )

            check(switch_state, toggle_switch, settings_file)


def run_test(webdriver_binary):
    def check_fresh_profile(switch_state, toggle_switch, settings_file):
        assert switch_state(), "a profile that never chose a notifications policy should let sites ask"
        toggle_switch()
        saved = json.loads(settings_file.read_text())
        assert saved.get("notificationsPolicy") == "block", saved
        toggle_switch()
        saved = json.loads(settings_file.read_text())
        assert saved.get("notificationsPolicy") == "ask", saved

    run_browser(webdriver_binary, {"firstRunComplete": True}, check_fresh_profile)

    def check_blocking_profile(switch_state, toggle_switch, settings_file):
        assert not switch_state(), "a profile whose notifications policy is block should keep sites from asking"

    run_browser(webdriver_binary, {"firstRunComplete": True, "notificationsPolicy": "block"}, check_blocking_profile)
    print("PASS: the notifications policy defaults to letting sites ask, and a saved block stays")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("webdriver_binary")
    args = parser.parse_args()
    run_test(args.webdriver_binary)
