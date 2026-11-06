#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

import argparse
import tempfile
import time

from pathlib import Path

import webdriver_test_utils as webdriver

from process_test_utils import isolated_environment
from test_utils import cleanup
from test_utils import stop_process


def launch(webdriver_binary, environment):
    process, port = webdriver.launch_webdriver(webdriver_binary, "--headless", env=environment)
    status, payload, response = webdriver.request(
        port, "POST", "/session", {"capabilities": {"alwaysMatch": {"ladybird:headless": True}}}
    )
    assert status == 200, response
    return process, port, payload["value"]["sessionId"]


def write_browser_report(reports_directory):
    # A report as the next launch recovers it from a browser that a fatal signal ended, which TestCrashReportStore
    # covers. Crashing the browser application itself would have the operating system report it to the user.
    reports_directory.mkdir(parents=True)
    reports_directory.chmod(0o700)
    report = reports_directory / "2026-09-29T10-00-00Z-Browser-abc123.txt"
    report.write_text(
        "Ladybird crash report\n"
        "Process: Browser\n"
        "Termination signal: SIGSEGV\n"
        "Termination signal number: 11\n"
        "\n"
        "Native stack (binary build ID, object address):\n"
        "#0 Unavailable\n"
    )
    report.chmod(0o600)
    return report


def run_test(webdriver_binary):
    with tempfile.TemporaryDirectory(prefix="ladybird-browser-crash-") as temporary:
        environment = isolated_environment(temporary)
        environment.pop("LADYBIRD_SOURCE_DIR", None)
        reports_directory = Path(temporary) / "data/Ladybird/CrashReports"

        report = write_browser_report(reports_directory)
        process, port, session = launch(webdriver_binary, environment)
        try:
            # A running browser keeps a signal-safe record, which the next launch recovers if a fatal signal ends it.
            deadline = time.monotonic() + webdriver.EVENT_TIMEOUT_SECONDS
            while not list(reports_directory.glob("Browser-*.pending")) and time.monotonic() < deadline:
                time.sleep(0.1)
            assert list(reports_directory.glob("Browser-*.pending")), (
                "Browser did not prepare signal-safe crash records"
            )

            # A browser driven by WebDriver never asks about saved reports, so this one is still waiting for the user.
            assert report.exists()
            assert not (reports_directory / "Seen" / report.name).exists()
            webdriver.request(port, "DELETE", f"/session/{session}")
        finally:
            cleanup(lambda: stop_process(process))


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("webdriver_binary")
    run_test(parser.parse_args().webdriver_binary)
