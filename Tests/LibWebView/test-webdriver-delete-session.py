#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

import argparse
import concurrent.futures
import time

from test_utils import cleanup
from test_utils import stop_process
from webdriver_test_utils import create_session
from webdriver_test_utils import launch_webdriver
from webdriver_test_utils import request


def run_test(webdriver_binary):
    webdriver, webdriver_port = launch_webdriver(webdriver_binary, "--headless")
    try:
        session_id = create_session(webdriver_port)

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
            status, payload, response_body = request(webdriver_port, "DELETE", f"/session/{session_id}")
            if status != expected_status or payload != expected_payload:
                raise AssertionError(f"Delete Session attempt {attempt} failed with HTTP {status}: {response_body}")

        session_id = create_session(webdriver_port)
        with concurrent.futures.ThreadPoolExecutor(max_workers=3) as executor:
            pending_script = executor.submit(
                request,
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
                executor.submit(request, webdriver_port, "DELETE", f"/session/{session_id}") for _ in range(2)
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
    finally:
        cleanup(lambda: stop_process(webdriver))

    if webdriver.returncode not in (0, -15):
        raise RuntimeError(f"WebDriver exited with status {webdriver.returncode}")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("webdriver_binary")
    args = parser.parse_args()

    run_test(args.webdriver_binary)


if __name__ == "__main__":
    main()
