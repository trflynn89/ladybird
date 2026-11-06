#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

import argparse
import http.client

import webdriver_test_utils as helpers

from test_utils import cleanup
from test_utils import stop_process

WEBDRIVER_REQUEST_TIMEOUT_SECONDS = 30


def run_test(webdriver_binary):
    webdriver, webdriver_port = helpers.launch_webdriver(webdriver_binary, "--headless")
    connection = None
    try:
        # Every request in this test is sent over the same keep-alive connection, matching how the
        # WPT wdspec harness drives WebDriver. An error response must leave the connection usable.
        connection = http.client.HTTPConnection("127.0.0.1", webdriver_port, timeout=WEBDRIVER_REQUEST_TIMEOUT_SECONDS)

        status, payload, response_body = helpers.request_on_connection(
            connection,
            "POST",
            "/session",
            {"capabilities": {"alwaysMatch": {"ladybird:headless": True}}},
        )
        value = payload.get("value")
        session_id = value.get("sessionId") if isinstance(value, dict) else None
        if status != 200 or not session_id:
            raise AssertionError(f"New Session failed with HTTP {status}: {response_body}")

        status, payload, response_body = helpers.request_on_connection(
            connection, "POST", f"/session/{session_id}/window", {"handle": "does-not-exist"}
        )
        if status != 404 or payload.get("value", {}).get("error") != "no such window":
            raise AssertionError(f"Switch To Window returned an unexpected response: HTTP {status}: {response_body}")

        status, payload, response_body = helpers.request_on_connection(
            connection, "GET", f"/session/{session_id}/title"
        )
        if status != 200:
            raise AssertionError(f"Get Title after error failed with HTTP {status}: {response_body}")

        status, payload, response_body = helpers.request_on_connection(
            connection, "GET", f"/session/{session_id}/unknown-command"
        )
        if status != 404 or payload.get("value", {}).get("error") != "unknown command":
            raise AssertionError(f"Unknown command returned an unexpected response: HTTP {status}: {response_body}")

        status, payload, response_body = helpers.request_on_connection(
            connection, "GET", f"/session/{session_id}/title"
        )
        if status != 200:
            raise AssertionError(f"Get Title after unknown command failed with HTTP {status}: {response_body}")

        status, payload, response_body = helpers.request_on_connection(connection, "DELETE", f"/session/{session_id}")
        if status != 200:
            raise AssertionError(f"Delete Session failed with HTTP {status}: {response_body}")
    finally:
        cleanup(
            lambda: connection.close() if connection is not None else None,
            lambda: stop_process(webdriver),
        )

    if webdriver.returncode not in (0, -15):
        raise RuntimeError(f"WebDriver exited with status {webdriver.returncode}")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("webdriver_binary")
    args = parser.parse_args()

    run_test(args.webdriver_binary)


if __name__ == "__main__":
    main()
