#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

import json
import time

import webdriver_test_utils as helpers

from test_utils import run_test_cases
from test_utils import temporary_profile
from webui_test_utils import settings_exchange


def run_policy_browser(webdriver_binary, settings, selector, check, *, check_permissions_api=False):
    with temporary_profile(settings=settings) as directory:
        settings_file = directory / "config" / "Settings.json"
        with helpers.webdriver_session(webdriver_binary, "--headless", "--profile-path", directory) as browser:

            def switch_state():
                browser.command("url", {"url": "about:settings"})
                # Request a new snapshot in case the page has already loaded its settings.
                settings_exchange(browser, "if (window.ladybird) ladybird.sendMessage('loadCurrentSettings');")
                return browser.script(
                    "const toggle = document.querySelector(arguments[0]); return [toggle.checked, toggle.disabled];",
                    selector,
                )

            def toggle_switch():
                settings_exchange(browser, "document.querySelector(parameters[0]).click();", selector)

            check(switch_state, toggle_switch, settings_file)

            if check_permissions_api:
                # The Permissions API is exposed without the experimental-interfaces flag.
                browser.command("url", {"url": "about:blank"})
                exposed = browser.script(
                    "return [typeof navigator.permissions, typeof PermissionStatus, typeof Permissions];"
                )
                assert exposed == ["object", "function", "function"], exposed


def test_location_policy(webdriver_binary):
    # A profile saved before the location policy existed carries a geolocationEnabled of false that the user never
    # chose, since every save wrote it. Such a profile lets sites ask.
    def check_migrated_profile(switch_state, toggle_switch, settings_file):
        checked, disabled = switch_state()
        assert checked, "a profile with only the old geolocationEnabled key should let sites ask"
        if disabled:
            print("NOTE: no geolocation provider here, so the switch can't be toggled")
            return
        toggle_switch()
        saved = json.loads(settings_file.read_text())
        assert saved.get("locationPolicy") == "block", saved
        assert "geolocationEnabled" not in saved, saved
        toggle_switch()
        saved = json.loads(settings_file.read_text())
        assert saved.get("locationPolicy") == "ask", saved

    run_policy_browser(
        webdriver_binary,
        {"geolocationEnabled": False},
        "#enable-geolocation",
        check_migrated_profile,
        check_permissions_api=True,
    )

    def check_blocking_profile(switch_state, toggle_switch, settings_file):
        checked, _ = switch_state()
        assert not checked, "a profile whose location policy is block should keep sites from asking"

    run_policy_browser(
        webdriver_binary,
        {"locationPolicy": "block"},
        "#enable-geolocation",
        check_blocking_profile,
        check_permissions_api=True,
    )
    print("PASS: the location policy defaults to letting sites ask, and a saved block stays")


def test_notifications_policy(webdriver_binary):
    def check_fresh_profile(switch_state, toggle_switch, settings_file):
        assert switch_state()[0], "a profile that never chose a notifications policy should let sites ask"
        toggle_switch()
        saved = json.loads(settings_file.read_text())
        assert saved.get("notificationsPolicy") == "block", saved
        toggle_switch()
        saved = json.loads(settings_file.read_text())
        assert saved.get("notificationsPolicy") == "ask", saved

    run_policy_browser(webdriver_binary, {"firstRunComplete": True}, "#enable-notifications", check_fresh_profile)

    def check_blocking_profile(switch_state, toggle_switch, settings_file):
        assert not switch_state()[0], "a profile whose notifications policy is block should keep sites from asking"

    run_policy_browser(
        webdriver_binary,
        {"firstRunComplete": True, "notificationsPolicy": "block"},
        "#enable-notifications",
        check_blocking_profile,
    )
    print("PASS: the notifications policy defaults to letting sites ask, and a saved block stays")


def test_site_permissions(webdriver_binary):
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
    run_test_cases(
        [test_location_policy, test_notifications_policy, test_site_permissions],
        "webdriver_binary",
    )
