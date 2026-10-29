#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

import argparse
import concurrent.futures
import http.client
import time

from contextlib import closing

import webdriver_test as helpers


def test_delete_session(webdriver_binary):
    with helpers.webdriver_process(webdriver_binary) as (_, webdriver_port):
        session_id = helpers.create_session(webdriver_port)

        expected_responses = (
            (200, {"value": None}),
            (
                404,
                {
                    "value": {
                        "error": "invalid session id",
                        "message": "Invalid session id",
                        "stacktrace": "",
                    }
                },
            ),
        )
        for attempt, (expected_status, expected_payload) in enumerate(expected_responses, start=1):
            status, payload, response_body = helpers.request(webdriver_port, "DELETE", f"/session/{session_id}")
            if status != expected_status or payload != expected_payload:
                raise AssertionError(f"Delete Session attempt {attempt} failed with HTTP {status}: {response_body}")

        session_id = helpers.create_session(webdriver_port)
        with concurrent.futures.ThreadPoolExecutor(max_workers=3) as executor:
            pending_script = executor.submit(
                helpers.request,
                webdriver_port,
                "POST",
                f"/session/{session_id}/execute/async",
                {
                    "script": "setTimeout(arguments[0], 2000);",
                    "args": [],
                },
            )
            time.sleep(0.25)
            pending_deletes = [
                executor.submit(helpers.request, webdriver_port, "DELETE", f"/session/{session_id}") for _ in range(2)
            ]

            status, payload, response_body = pending_script.result()
            if status != 200 or payload != {"value": None}:
                raise AssertionError(f"Execute Async Script failed with HTTP {status}: {response_body}")

            delete_responses = [pending_delete.result() for pending_delete in pending_deletes]

        if sorted(status for status, _, _ in delete_responses) != [200, 404]:
            raise AssertionError(f"Queued Delete Session requests returned unexpected responses: {delete_responses}")

        for status, payload, response_body in delete_responses:
            expected_payload = dict(expected_responses)[status]
            if payload != expected_payload:
                raise AssertionError(f"Queued Delete Session failed with HTTP {status}: {response_body}")


def test_error_keep_alive(webdriver_binary):
    with helpers.webdriver_process(webdriver_binary) as (_, webdriver_port):
        with closing(http.client.HTTPConnection("127.0.0.1", webdriver_port, timeout=30)) as connection:
            # Every request in this test is sent over the same keep-alive connection, matching how the
            # WPT wdspec harness drives WebDriver. An error response must leave the connection usable.

            status, payload, response_body = helpers.request(
                connection,
                "POST",
                "/session",
                {"capabilities": {"alwaysMatch": {"ladybird:headless": True}}},
            )
            value = payload.get("value")
            session_id = value.get("sessionId") if isinstance(value, dict) else None
            if status != 200 or not session_id:
                raise AssertionError(f"New Session failed with HTTP {status}: {response_body}")

            status, payload, response_body = helpers.request(
                connection, "POST", f"/session/{session_id}/window", {"handle": "does-not-exist"}
            )
            if status != 404 or payload.get("value", {}).get("error") != "no such window":
                raise AssertionError(
                    f"Switch To Window returned an unexpected response: HTTP {status}: {response_body}"
                )

            status, payload, response_body = helpers.request(connection, "GET", f"/session/{session_id}/title")
            if status != 200:
                raise AssertionError(f"Get Title after error failed with HTTP {status}: {response_body}")

            status, payload, response_body = helpers.request(
                connection, "GET", f"/session/{session_id}/unknown-command"
            )
            if status != 404 or payload.get("value", {}).get("error") != "unknown command":
                raise AssertionError(f"Unknown command returned an unexpected response: HTTP {status}: {response_body}")

            status, payload, response_body = helpers.request(connection, "GET", f"/session/{session_id}/title")
            if status != 200:
                raise AssertionError(f"Get Title after unknown command failed with HTTP {status}: {response_body}")

            status, payload, response_body = helpers.request(connection, "DELETE", f"/session/{session_id}")
            if status != 200:
                raise AssertionError(f"Delete Session failed with HTTP {status}: {response_body}")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("webdriver_binary")
    args = parser.parse_args()
    test_delete_session(args.webdriver_binary)
    test_error_keep_alive(args.webdriver_binary)


if __name__ == "__main__":
    main()
