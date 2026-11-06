#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

import argparse
import json
import time

import webdriver_test_utils as helpers

from test_utils import temporary_profile


def run_test(webdriver_binary):
    with temporary_profile(
        settings={"firstRunComplete": True},
        permissions={
            "version": 1,
            "decisions": [
                {"origin": "https://example.com", "permission": "geolocation", "decision": "allow"},
                {"origin": "https://talk.example.org", "permission": "microphone", "decision": "block"},
                {"origin": "https://news.example.net", "permission": "notifications", "decision": "allow"},
            ],
            "embargoes": [],
        },
    ) as directory:
        config = directory / "config"
        permissions_file = config / "Permissions.json"

        with helpers.webdriver_session(webdriver_binary, "--headless", "--profile-path", directory) as browser:
            command = browser.command
            script = browser.script

            def rows(hash, permission):
                command("url", {"url": f"about:settings#{hash}"})
                return command(
                    "execute/async",
                    {
                        "script": """
                        const permission = arguments[0];
                        const done = arguments[1];
                        const dialog = document.querySelector(`#site-permissions-${permission}`);
                        const read = () => Array.from(dialog.querySelectorAll('.site-permission-row'))
                            .map(row => [row.dataset.origin, row.querySelector('select').value]);
                        const list = dialog.querySelector('.site-permissions-list');
                        if (read().length) return done(read());
                        const observer = new MutationObserver(() => {
                            if (!read().length) return;
                            observer.disconnect();
                            done(read());
                        });
                        observer.observe(list, { childList: true, subtree: true });
                    """,
                        "args": [permission],
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
            assert rows("notifications-permissions", "notifications") == [["https://news.example.net", "allow"]]

            rows("location-permissions", "geolocation")
            script("""
                const select = document.querySelector('#site-permissions-geolocation .site-permission-row select');
                select.value = 'block';
                select.dispatchEvent(new Event('change'));
            """)
            wait_for_saved(
                lambda decisions: (
                    {"origin": "https://example.com", "permission": "geolocation", "decision": "block"} in decisions
                )
            )

            script("document.querySelector('#site-permissions-geolocation .site-permission-row button').click();")
            wait_for_saved(lambda decisions: all(entry["origin"] != "https://example.com" for entry in decisions))

            print("PASS: about:settings lists, changes, and removes site permissions")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("webdriver_binary")
    args = parser.parse_args()
    run_test(args.webdriver_binary)
