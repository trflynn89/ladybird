#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

import argparse

import webdriver_test_utils as helpers

from test_utils import temporary_profile
from webui_test_utils import settings_exchange


# A tab opened from a WebUI page starts in that page's process, so a second page of the same WebUI there must not take
# the first page's channel to the UI process.
def run_test(webdriver_binary):
    with temporary_profile() as directory:
        with helpers.webdriver_session(webdriver_binary, "--headless", "--profile-path", directory) as browser:
            port = browser.port
            session = browser.session_id

            command = browser.command

            def round_trip():
                settings_exchange(browser, "ladybird.sendMessage('loadCurrentSettings');")
                return True

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


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("webdriver_binary")
    run_test(parser.parse_args().webdriver_binary)
