#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

import argparse
import tempfile
import time

from pathlib import Path

import webdriver_test_utils as helpers

from process_test_utils import descendants_named
from process_test_utils import isolated_environment
from test_utils import cleanup
from test_utils import stop_process

# Terminate a worker at each of these delays after constructing it, to catch its process at every stage of starting up.
TERMINATE_DELAYS_MS = list(range(0, 600, 25))

# How long a crashed worker's report gets to show up, once every worker process is gone.
CRASH_REPORT_GRACE_SECONDS = 2


def run_test(webdriver_binary):
    with tempfile.TemporaryDirectory(prefix="ladybird-worker-terminated-") as temporary:
        environment = isolated_environment(temporary)
        webdriver, port = helpers.launch_webdriver(webdriver_binary, "--headless", env=environment)
        try:
            session = helpers.create_session(port)
            status, _, body = helpers.request(
                port, "POST", f"/session/{session}/url", {"url": "data:text/html,<title>Workers</title>"}
            )
            assert status == 200, body

            # Each worker's process is launched before its constructor returns, and is still starting up when
            # terminate() tears the worker down.
            status, payload, body = helpers.request(
                port,
                "POST",
                f"/session/{session}/execute/async",
                {
                    "script": """
                    const [delays, done] = arguments;
                    const url = URL.createObjectURL(new Blob(["onmessage = () => {};"], {type: 'text/javascript'}));
                    let remaining = delays.length;
                    for (const delay of delays) {
                        const worker = new Worker(url);
                        const terminate = () => {
                            worker.terminate();
                            if (--remaining === 0)
                                done();
                        };
                        if (delay === 0)
                            terminate();
                        else
                            setTimeout(terminate, delay);
                    }
                """,
                    "args": [TERMINATE_DELAYS_MS],
                },
            )
            assert status == 200 and payload == {"value": None}, body

            deadline = time.monotonic() + helpers.EVENT_TIMEOUT_SECONDS
            while descendants_named(webdriver.pid, "WebWorker"):
                if time.monotonic() >= deadline:
                    raise AssertionError("A terminated worker's process never exited")
                time.sleep(0.05)

            time.sleep(CRASH_REPORT_GRACE_SECONDS)
            reports = list((Path(temporary) / "data/Ladybird/CrashReports").glob("*-WebWorker-*.txt"))
            assert not reports, reports[0].read_text()

            helpers.request(port, "DELETE", f"/session/{session}")
        finally:
            cleanup(lambda: stop_process(webdriver))


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("webdriver_binary")
    args = parser.parse_args()
    run_test(args.webdriver_binary)
