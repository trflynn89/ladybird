#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

import argparse
import http.server
import json
import os
import tempfile
import threading

from pathlib import Path

import webdriver_test as helpers


def test_settings_changes(webdriver_binary):
    release_download = threading.Event()

    class Handler(http.server.BaseHTTPRequestHandler):
        def do_GET(self):
            release_download.wait(30)
            self.send_response(200)
            self.end_headers()
            self.wfile.write(b"! Test filter list\n")

        def log_message(self, format, *args):
            pass

    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    threading.Thread(target=server.serve_forever, daemon=True).start()

    with tempfile.TemporaryDirectory() as directory:
        config = Path(directory) / "config"
        config.mkdir()
        (config / "Settings.json").write_text(json.dumps({"contentBlockers": {"enabled": True}}))
        with helpers.webdriver_process(webdriver_binary, profile=directory) as (_, port):
            session = None
            try:
                helpers.wait_for_port(port)
                session = helpers.create_session(port)

                def command(path, body):
                    status, payload, raw = helpers.request(port, "POST", f"/session/{session}/{path}", body)
                    assert status == 200, raw
                    return payload["value"]

                def script(source, *args):
                    return command("execute/sync", {"script": source, "args": list(args)})

                def change(name, data):
                    return command(
                        "execute/async",
                        {
                            "script": """
                            const [name, data, done] = arguments;
                            document.addEventListener('WebUIMessage', function listener(event) {
                                if (event.detail.name !== 'loadSettings') return;
                                document.removeEventListener('WebUIMessage', listener);
                                done(event.detail.data);
                            });
                            ladybird.sendMessage(name, data);
                        """,
                            "args": [name, data],
                        },
                    )

                command("url", {"url": "about:settings#advanced"})
                command(
                    "execute/async",
                    {
                        "script": """
                        const done = arguments[0];
                        function ready() { return document.querySelector('#config-list input[type=number]'); }
                        if (ready()) return done();
                        const observer = new MutationObserver(() => {
                            if (!ready()) return;
                            observer.disconnect();
                            done();
                        });
                        observer.observe(document.documentElement, {childList: true, subtree: true});
                    """,
                        "args": [],
                    },
                )
                script("""
                    window.draftInput = document.querySelector('#config-list input[type=number]');
                    draftInput.value = '999';
                    draftInput.focus();
                    document.querySelector('#dns-server').value = 'unfinished.example';
                """)

                changes = change("setAppearance", {"showBookmarksBar": "never"})
                assert set(changes) == {"appearance"}, changes
                assert changes["appearance"]["showBookmarksBar"] == "never"
                assert script("""
                    return draftInput === document.getElementById(draftInput.id)
                        && draftInput === document.activeElement && draftInput.value === '999'
                        && document.querySelector('#dns-server').value === 'unfinished.example';
                """), "An unrelated update replaced an unfinished edit"

                # No enabled lists means no download starts, and no loadSettings message is needed.
                assert command(
                    "execute/async",
                    {
                        "script": """
                        const done = arguments[0];
                        const button = document.querySelector('#update-content-blocker-lists');
                        button.click();
                        setTimeout(() => done(!button.disabled && button.textContent === 'Update enabled lists'), 100);
                    """,
                        "args": [],
                    },
                ), "An update with no eligible lists left the button waiting for a notification"

                variable = script("""
                    const input = [...document.querySelectorAll('#config-list input[type=number]')]
                        .find(input => input !== draftInput);
                    return {name: input.id, value: Number(input.value) + 1};
                """)
                changes = change("setConfigVariable", variable)
                assert set(changes) == {"configVariables", "configVariableDefinitions"}, changes
                assert script(
                    "return draftInput === document.getElementById(draftInput.id) && draftInput.value === '999';"
                )

                # A false boolean must update its own section without clearing any other controls.
                assert change("setGlobalPrivacyControl", True) == {"globalPrivacyControl": True}
                assert change("setGlobalPrivacyControl", False) == {"globalPrivacyControl": False}
                assert script("return !document.querySelector('#global-privacy-control-toggle').checked;")

                engine = {"name": "Test custom engine", "url": "https://example.com/search?q=%s"}
                changes = change("addCustomSearchEngine", engine)
                assert set(changes) == {"searchEngine"}, changes
                assert engine in changes["searchEngine"]["custom"]
                changes = change("removeCustomSearchEngine", engine)
                assert set(changes) == {"searchEngine"}, changes
                assert not changes["searchEngine"].get("custom")

                changes = change("addCustomContentBlockerSubscription", f"http://127.0.0.1:{server.server_port}/list")
                assert set(changes) == {
                    "contentBlockers",
                    "contentBlockerLists",
                    "contentBlockerListUpdateInProgress",
                }, changes
                assert changes["contentBlockerListUpdateInProgress"] is True
                assert script("return document.querySelector('#update-content-blocker-lists').disabled;")
                script("""
                    window.finalSettingsChanges = new Promise(resolve => {
                        document.addEventListener('WebUIMessage', function listener(event) {
                            if (event.detail.name !== 'loadSettings') return;
                            if (event.detail.data.contentBlockerListUpdateInProgress !== false) return;
                            document.removeEventListener('WebUIMessage', listener);
                            resolve(event.detail.data);
                        });
                    });
                """)
                release_download.set()
                final_changes = command(
                    "execute/async",
                    {"script": "window.finalSettingsChanges.then(arguments[0]);", "args": []},
                )
                assert set(final_changes) == {"contentBlockerLists", "contentBlockerListUpdateInProgress"}, (
                    final_changes
                )
                assert any(item["lastUpdatedAt"] is not None for item in final_changes["contentBlockerLists"])
                assert script(
                    "return draftInput === document.getElementById(draftInput.id) && draftInput.value === '999';"
                )

                command("url", {"url": "about:welcome"})
                command(
                    "execute/async",
                    {
                        "script": """
                        const done = arguments[0];
                        const options = document.querySelector('#setup-options');
                        if (!options.disabled) return done();
                        const observer = new MutationObserver(() => {
                            if (options.disabled) return;
                            observer.disconnect();
                            done();
                        });
                        observer.observe(options, {attributes: true, attributeFilter: ['disabled']});
                    """,
                        "args": [],
                    },
                )
                changes = change("setSearchEngineSettings", {"engine": "Google", "suggestions": True})
                assert set(changes) == {"searchEngine"}, changes
                assert script("""
                    return document.querySelector('input[name="search-engine"]:checked')?.value === 'Google'
                        && !document.querySelector('#start-browsing').disabled;
                """), "The welcome page did not apply a partial search setting"
                changes = change("setContentBlockerEnabled", False)
                assert set(changes) == {"contentBlockers"}, changes
                assert script("return !document.querySelector('#enable-content-blocking').checked;")
                print("PASS: settings changes contain only changed fields, preserve edits, and track downloads")
            finally:
                release_download.set()
                if session is not None:
                    helpers.request(port, "DELETE", f"/session/{session}")
                server.shutdown()
                server.server_close()


def test_search_engine_settings(webdriver_binary, graphical):
    with tempfile.TemporaryDirectory() as directory:
        config = Path(directory) / "config"
        config.mkdir()
        settings_file = config / "Settings.json"
        settings_file.write_text(
            json.dumps(
                {
                    "searchEngine": {
                        "name": "Google",
                        "custom": [{"name": "Plain custom", "url": "https://example.com/search?q=%s"}],
                    },
                    "autocompleteEngine": {"name": "Google"},
                }
            )
        )
        with helpers.webdriver_process(webdriver_binary, profile=directory, headless=not graphical) as (_, port):
            session = None
            try:
                helpers.wait_for_port(port)
                status, payload, raw = helpers.request(
                    port, "POST", "/session", {"capabilities": {"alwaysMatch": {"ladybird:headless": not graphical}}}
                )
                assert status == 200, raw
                session = payload["value"]["sessionId"]

                def command(path, body=None, method="POST"):
                    status, payload, raw = helpers.request(port, method, f"/session/{session}/{path}", body)
                    assert status == 200, raw
                    return payload["value"]

                def script(source, *args):
                    return command("execute/sync", {"script": source, "args": list(args)})

                def change_settings(action, condition, *args):
                    command(
                        "execute/async",
                        {
                            "script": f"""
                            const parameters = Array.from(arguments);
                            const done = parameters.pop();
                            document.addEventListener('WebUIMessage', function listener(event) {{
                                if (event.detail.name !== 'loadSettings') return;
                                if (!Object.hasOwn(event.detail.data, 'searchEngine')) return;
                                const settings = event.detail.data;
                                if (!({condition})) return;
                                document.removeEventListener('WebUIMessage', listener);
                                done();
                            }});
                            {action}
                        """,
                            "args": list(args),
                        },
                    )

                def select_engine(name):
                    change_settings(
                        """
                        [...document.querySelectorAll('#search-engine input')]
                            .find(radio => radio.value === parameters[0]).click();
                    """,
                        "(settings.searchEngine.engine || '') === parameters[0]",
                        name,
                    )

                def toggle_suggestions(enabled):
                    change_settings(
                        "document.querySelector('#search-suggestions-enabled').click();",
                        "settings.searchEngine.suggestions === parameters[0]",
                        enabled,
                    )

                def state():
                    return script("""
                        const checkbox = document.querySelector('#search-suggestions-enabled');
                        return [checkbox.checked, checkbox.disabled,
                                document.querySelector('#search-suggestions-description').textContent];
                    """)

                def load_settings(expected_engine):
                    command("url", {"url": "about:settings#search"})
                    command(
                        "execute/async",
                        {
                            "script": """
                            const expectedEngine = arguments[0];
                            const done = arguments[1];
                            const list = document.querySelector('#search-engine');
                            function ready() {
                                const radios = list.querySelectorAll('input');
                                return list.querySelector('input:checked')?.value === expectedEngine && radios.length > 10;
                            }
                            if (ready()) return done();
                            const observer = new MutationObserver(() => {
                                if (!ready()) return;
                                observer.disconnect();
                                done();
                            });
                            observer.observe(list, {childList: true});
                        """,
                            "args": [expected_engine],
                        },
                    )

                load_settings("Google")
                assert state() == [True, False, "Sends what you type to Google."]
                for engine in ("Bing", "Ecosia", "Yandex"):
                    select_engine(engine)
                    assert state() == [True, False, f"Sends what you type to {engine}."]
                select_engine("Mojeek")
                assert state() == [True, True, "Search suggestions aren't available for Mojeek."]
                select_engine("DuckDuckGo")
                assert state() == [True, False, "Sends what you type to DuckDuckGo."]
                select_engine("Brave")
                assert state() == [True, False, "Sends what you type to Brave."]
                toggle_suggestions(False)
                select_engine("Google")
                assert state()[:2] == [False, False]
                select_engine("Plain custom")
                assert state()[:2] == [False, True]

                script("document.querySelector('#search-settings').click()")
                command(
                    "execute/async",
                    {
                        "script": """
                        const done = arguments[0];
                        const dialog = document.querySelector('#search-dialog');
                        if (dialog.open) return done();
                        const observer = new MutationObserver(() => {
                            if (!dialog.open) return;
                            observer.disconnect();
                            done();
                        });
                        observer.observe(dialog, {attributes: true, attributeFilter: ['open']});
                    """,
                        "args": [],
                    },
                )
                script("""
                    document.querySelector('#search-custom-name').value = 'Custom suggestions';
                    document.querySelector('#search-custom-url').value = 'https://example.com/search?q=%s';
                """)
                for url in ("https://example.com/suggest", "file:///tmp/%s", "data:text/plain,%s"):
                    script(
                        """
                        document.querySelector('#search-custom-suggestions-url').value = arguments[0];
                        document.querySelector('#search-custom-add').click();
                    """,
                        url,
                    )
                    assert script(
                        "return document.querySelector('#search-custom-suggestions-url').classList.contains('error')"
                    )

                change_settings(
                    """
                    document.querySelector('#search-custom-suggestions-url').value = 'https://example.com/suggest?q=%s';
                    document.querySelector('#search-custom-add').click();
                """,
                    "settings.searchEngine.custom.some(engine => engine.name === 'Custom suggestions')",
                )
                script("document.querySelector('#search-close').click()")
                select_engine("Custom suggestions")
                assert state()[:2] == [False, False]
                toggle_suggestions(True)
                saved = json.loads(settings_file.read_text())
                assert "autocompleteEngine" not in saved
                assert saved["searchEngine"]["engine"] == "Custom suggestions"
                assert saved["searchEngine"]["suggestions"]
                assert "name" not in saved["searchEngine"]
                assert saved["searchEngine"]["custom"][1]["suggestionsUrl"] == "https://example.com/suggest?q=%s"

                helpers.request(port, "DELETE", f"/session/{session}")
                session = helpers.create_session(port)
                load_settings("Custom suggestions")
                assert state()[:2] == [True, False], "Combined search preference did not survive a new browser process"
                change_settings(
                    """
                    ladybird.sendMessage('removeCustomSearchEngine', {
                        name: 'Custom suggestions', url: 'https://example.com/search?q=%s'
                    });
                """,
                    "!settings.searchEngine.engine",
                )
                assert state() == [True, True, "Choose a search engine to use search suggestions."]
                print("PASS: search and suggestions use one engine, migrate, and preserve the user's toggle")
            finally:
                if session is not None:
                    helpers.request(port, "DELETE", f"/session/{session}")


def test_content_blockers(webdriver_binary):
    request = helpers.request
    hits = []
    release_downloads = threading.Event()
    release_stall = threading.Event()

    class Handler(http.server.BaseHTTPRequestHandler):
        def do_GET(self):
            hits.append(self.path)
            if self.path == "/favicon.ico":
                self.send_error(404)
                return
            if self.path == "/page":
                self.send_response(200)
                self.send_header("Content-Type", "text/html")
                self.end_headers()
                self.wfile.write(b"<!doctype html><body>")
                return
            if self.path == "/before-stall":
                release_downloads.wait()
            if self.path == "/stall":
                self.send_response(200)
                self.send_header("Content-Length", "5")
                self.end_headers()
                self.wfile.write(b"!")
                self.wfile.flush()
                release_stall.wait()
                return
            status = int(self.path.rsplit("/", 1)[1]) if self.path.startswith("/redirect/") else 302
            self.send_response(200 if self.path in ("/list", "/before-stall", "/after-stall") else status)
            if self.path != "/missing":
                self.send_header(
                    "Location",
                    "file:///not-a-filter-list"
                    if self.path == "/unsafe"
                    else "../list"
                    if self.path.startswith("/redirect/")
                    else "/loop",
                )
            self.send_header("Content-Length", "5")
            self.end_headers()
            self.wfile.write(b"!list")

        def log_message(self, format, *args):
            pass

    with tempfile.TemporaryDirectory() as directory:
        profile = Path(directory)
        (profile / "config").mkdir()
        (profile / "config" / "Settings.json").write_text(
            json.dumps(
                {
                    "contentBlockers": {"enabled": True, "builtInLists": {"easyList": False}},
                }
            )
        )
        server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
        threading.Thread(target=server.serve_forever, daemon=True).start()
        with helpers.webdriver_process(
            webdriver_binary,
            profile=directory,
            environment={**os.environ, "LADYBIRD_CONTENT_BLOCKER_LIST_UPDATE_TIMEOUT_MS": "1000"},
        ) as (_, port):
            session = None
            try:
                helpers.wait_for_port(port)
                session = helpers.create_session(port)

                def command(path, body):
                    status, payload, raw = request(port, "POST", f"/session/{session}/{path}", body)
                    assert status == 200, raw
                    return payload["value"]

                def settings(name="loadCurrentSettings", data=None, wait_for_idle=True):
                    return command(
                        "execute/async",
                        {
                            "script": """
                        const [name, data, waitForIdle, done] = arguments;
                        const receive = event => {
                            if (event.detail.name !== 'loadSettings') return;
                            // Wait for the requested full snapshot rather than a partial broadcast.
                            if (!Object.hasOwn(event.detail.data, 'firstRunComplete')) return;
                            if (waitForIdle && event.detail.data.contentBlockerListUpdateInProgress) {
                                setTimeout(() => ladybird.sendMessage('loadCurrentSettings'), 50);
                                return;
                            }
                            document.removeEventListener('WebUIMessage', receive);
                            done(event.detail.data);
                        };
                        document.addEventListener('WebUIMessage', receive);
                        ladybird.sendMessage(name, data);
                        if (name !== 'loadCurrentSettings') ladybird.sendMessage('loadCurrentSettings');
                    """,
                            "args": [name, data, wait_for_idle],
                        },
                    )

                command("timeouts", {"script": 60000})
                status, payload, raw = request(port, "GET", f"/session/{session}/window")
                assert status == 200, raw
                settings_window = payload["value"]
                page_window = command("window/new", {"type": "tab"})["handle"]
                command("window", {"handle": page_window})
                command("url", {"url": f"http://127.0.0.1:{server.server_port}/page"})
                command(
                    "execute/sync",
                    {
                        "script": """
                        window.popup = window.open('about:blank');
                        for (const page of [window, popup]) {
                            const advertisement = page.document.createElement('div');
                            advertisement.className = 'advertisement';
                            page.document.body.append(advertisement);
                        }
                    """,
                        "args": [],
                    },
                )
                command("window", {"handle": settings_window})
                command("url", {"url": "about:about"})
                command("url", {"url": "about:services"})
                settings()
                command("back", {})
                assert command("execute/sync", {"script": "return location.href", "args": []}) == "about:about"
                command("url", {"url": "about:blocking"})
                state = settings()
                assert command(
                    "execute/sync",
                    {
                        "script": """
                        const tab = document.querySelector('[aria-selected="true"]');
                        const panel = document.querySelector('#tab-blocking');
                        const lists = panel.querySelector('#content-blocker-lists');
                        const heading = panel.querySelector('.list-section .list-heading');
                        const details = lists.querySelector('.item-name .description');
                        const form = panel.querySelector('#add-filter-list-form');
                        return location.hash === '#blocking' && tab.textContent.trim() === 'Content blocking'
                            && heading.getBoundingClientRect().left === lists.getBoundingClientRect().left
                            && getComputedStyle(details).fontSize === '12px' && !details.textContent.includes('easylist.to')
                            && form.hidden
                            && ['label', 'summary', 'textarea'].every(selector =>
                                getComputedStyle(panel.querySelector(selector)).fontSize === '13px');
                    """,
                        "args": [],
                    },
                )
                for enabled in (False, True):
                    settings("setServicesNetworkAccessEnabled", enabled)
                    assert command(
                        "execute/sync",
                        {
                            "script": """
                            const toggle = document.querySelector('#filter-list-updates');
                            const label = document.querySelector('label[for="filter-list-updates"]');
                            return toggle.disabled === !arguments[0]
                                && getComputedStyle(label).opacity === (arguments[0] ? '1' : '0.5')
                                && getComputedStyle(toggle).opacity === '1';
                        """,
                            "args": [enabled],
                        },
                    )
                for rules, display in [("##.advertisement", "none"), ("", "block")]:
                    settings("setCustomContentBlockerFilters", rules)
                    command("window", {"handle": page_window})
                    assert command(
                        "execute/sync",
                        {
                            "script": "return [window, popup].map(page => page.getComputedStyle(page.document.querySelector('.advertisement')).display)",
                            "args": [],
                        },
                    ) == [display, display]
                    command("window", {"handle": settings_window})
                command("window", {"handle": page_window})
                command("execute/sync", {"script": "popup.close()", "args": []})
                command("window", {"handle": settings_window})
                command(
                    "execute/sync",
                    {
                        "script": """
                    const editor = document.querySelector('#custom-content-blocker-filters');
                    editor.value = 'unsaved rules';
                    editor.dispatchEvent(new Event('input', {bubbles: true}));
                    document.querySelector('#tab-button-services').click();
                """,
                        "args": [],
                    },
                )
                settings("setContentBlockerListEnabled", {"identifier": "easyPrivacy", "enabled": False})
                assert (
                    command(
                        "execute/sync",
                        {
                            "script": "return document.querySelector('#custom-content-blocker-filters').value",
                            "args": [],
                        },
                    )
                    == "unsaved rules"
                )
                command(
                    "execute/sync",
                    {
                        "script": """
                        document.querySelector('#save-custom-content-blocker-filters').click();
                        const editor = document.querySelector('#custom-content-blocker-filters');
                        editor.value = 'edits made while saving';
                        editor.dispatchEvent(new Event('input', {bubbles: true}));
                    """,
                        "args": [],
                    },
                )
                assert settings()["contentBlockers"]["customFilters"] == "unsaved rules"

                for path in [f"/redirect/{status}" for status in (301, 302, 303, 307, 308)] + [
                    "/loop",
                    "/unsafe",
                    "/missing",
                ]:
                    url = f"http://127.0.0.1:{server.server_port}{path}"
                    state = settings("addCustomContentBlockerSubscription", url)
                    identifier = next(item["identifier"] for item in state["contentBlockerLists"] if item["url"] == url)
                    cached = profile / "data" / "ContentBlocking" / "Lists" / f"{identifier}.txt"
                    if path.startswith("/redirect/"):
                        assert cached.read_text() == "!list"
                    else:
                        assert not cached.exists(), path
                assert hits.count("/loop") == 21, hits
                duplicate = settings(
                    "addCustomContentBlockerSubscription", f"http://127.0.0.1:{server.server_port}/missing"
                )
                assert len(duplicate["contentBlockerLists"]) == len(state["contentBlockerLists"])
                assert (
                    command(
                        "execute/sync",
                        {
                            "script": "return document.querySelector('#custom-content-blocker-subscription-status').textContent",
                            "args": [],
                        },
                    )
                    == "Subscription already added."
                )
                for path in ("/before-stall", "/stall", "/after-stall"):
                    settings(
                        "addCustomContentBlockerSubscription", f"http://127.0.0.1:{server.server_port}{path}", False
                    )
                release_downloads.set()
                state = settings()
                for item in state["contentBlockerLists"]:
                    if item["url"] and item["url"].endswith(("/before-stall", "/stall", "/after-stall")):
                        cached = profile / "data" / "ContentBlocking" / "Lists" / f"{item['identifier']}.txt"
                        assert cached.exists() == (not item["url"].endswith("/stall"))

                for character, count in (
                    ("a", 64 * 1024 * 1024),
                    ("\0", 11 * 1024 * 1024),
                    ("\r", 11 * 1024 * 1024),
                    ("\f", 11 * 1024 * 1024),
                    ("!", 1),
                ):
                    status = command(
                        "execute/async",
                        {
                            "script": """
                        const [character, count, done] = arguments;
                        const status = document.querySelector('#local-content-blocker-list-status');
                        status.textContent = '';
                        const observer = new MutationObserver(() => {
                            observer.disconnect();
                            done(status.textContent);
                        });
                        observer.observe(status, {childList: true, characterData: true, subtree: true});
                        const files = new DataTransfer();
                        files.items.add(new File([character.repeat(count)], 'import.txt'));
                        const picker = document.querySelector('#local-content-blocker-list-picker');
                        picker.files = files.files;
                        picker.dispatchEvent(new Event('change'));
                    """,
                            "args": [character, count],
                        },
                    )
                    assert status == (
                        "import.txt imported." if count == 1 else "The encoded list is too large to import."
                    ), status
                    settings()
                lists_directory = profile / "data" / "ContentBlocking" / "Lists"
                backup_directory = lists_directory.with_name("Lists-backup")
                lists_directory.rename(backup_directory)
                lists_directory.write_text("Not a directory")
                try:
                    settings("importLocalContentBlockerList", {"name": "failed-import.txt", "contents": "!"})
                    assert command(
                        "execute/sync",
                        {
                            "script": "return document.querySelector('#local-content-blocker-list-status').textContent",
                            "args": [],
                        },
                    ).startswith("Unable to import list:")
                finally:
                    lists_directory.unlink()
                    backup_directory.rename(lists_directory)
                assert (
                    command(
                        "execute/sync",
                        {
                            "script": "return document.querySelector('#custom-content-blocker-filters').value",
                            "args": [],
                        },
                    )
                    == "edits made while saving"
                )
            finally:
                release_downloads.set()
                release_stall.set()
                if session is not None:
                    helpers.request(port, "DELETE", f"/session/{session}")
                server.shutdown()
                server.server_close()


def test_web_ui_pages_share_a_process(webdriver_binary):
    with tempfile.TemporaryDirectory() as directory:
        with helpers.webdriver_session(webdriver_binary, profile=directory) as (port, session):

            def command(path, body):
                status, payload, raw = helpers.request(port, "POST", f"/session/{session}/{path}", body)
                assert status == 200, raw
                return payload["value"]

            def round_trip():
                return command(
                    "execute/async",
                    {
                        "script": """
                        const done = arguments[0];
                        document.addEventListener('WebUIMessage', function listener(event) {
                            if (event.detail.name !== 'loadSettings') return;
                            document.removeEventListener('WebUIMessage', listener);
                            done(true);
                        });
                        ladybird.sendMessage('loadCurrentSettings');
                    """,
                        "args": [],
                    },
                )

            command("timeouts", {"script": 5000})
            command("url", {"url": "about:settings"})
            assert round_trip()
            status, payload, raw = helpers.request(port, "GET", f"/session/{session}/window")
            assert status == 200, raw
            first_window = payload["value"]

            second_window = command("window/new", {"type": "tab"})["handle"]
            command("window", {"handle": second_window})
            command("url", {"url": "about:settings"})
            assert round_trip()

            command("window", {"handle": first_window})
            assert round_trip(), "The first page lost its WebUI channel"
            print("PASS: two WebUI pages in one process each keep their channel")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("webdriver_binary")
    parser.add_argument("--graphical", action="store_true")
    args = parser.parse_args()
    test_settings_changes(args.webdriver_binary)
    test_search_engine_settings(args.webdriver_binary, args.graphical)
    test_content_blockers(args.webdriver_binary)
    test_web_ui_pages_share_a_process(args.webdriver_binary)


if __name__ == "__main__":
    main()
