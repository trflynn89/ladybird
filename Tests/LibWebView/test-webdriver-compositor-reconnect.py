#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

# When the Compositor dies, the browser connects a new one, and the page's next rendering update
# has to bring the new Compositor up to date: its first frame carries the whole display list again,
# not only what changed since the old Compositor's last frame.
import argparse
import base64
import os
import signal
import tempfile
import time
import urllib.parse

import webdriver_test_utils as helpers

from process_test_utils import descendants_named
from process_test_utils import isolated_environment
from test_utils import cleanup
from test_utils import stop_process

PAGE = "data:text/html," + urllib.parse.quote(
    """<style>
#box { width: 100px; height: 100px; background: blue; }
</style><div id=box></div>"""
)


def execute(port, session, script):
    status, payload, body = helpers.request(
        port, "POST", f"/session/{session}/execute/sync", {"script": script, "args": []}
    )
    assert status == 200, body
    return payload["value"]


def take_screenshot(port, session):
    status, payload, body = helpers.request(port, "GET", f"/session/{session}/screenshot")
    assert status == 200, body
    png = base64.b64decode(payload["value"])
    assert png.startswith(b"\x89PNG\r\n\x1a\n"), "Screenshot is not a PNG"
    return png


def run_test(webdriver_binary):
    with tempfile.TemporaryDirectory(prefix="ladybird-compositor-reconnect-") as temporary:
        environment = isolated_environment(temporary)
        webdriver, port = helpers.launch_webdriver(webdriver_binary, "--headless", env=environment)
        try:
            session = helpers.create_session(port)
            status, _, body = helpers.request(port, "POST", f"/session/{session}/url", {"url": PAGE})
            assert status == 200, body
            first = take_screenshot(port, session)

            compositors = descendants_named(webdriver.pid, "Compositor")
            assert len(compositors) == 1, f"Expected one Compositor, found {len(compositors)}"
            os.kill(compositors[0], signal.SIGKILL)

            deadline = time.monotonic() + helpers.EVENT_TIMEOUT_SECONDS
            while True:
                replacements = [pid for pid in descendants_named(webdriver.pid, "Compositor") if pid != compositors[0]]
                if replacements:
                    break
                if time.monotonic() >= deadline:
                    raise AssertionError("No Compositor replaced the one that died")
                time.sleep(0.05)

            # The new Compositor paints the page as it was, then as it changes.
            assert take_screenshot(port, session) == first
            execute(port, session, "document.getElementById('box').style.background = 'green';")
            assert take_screenshot(port, session) != first
            helpers.request(port, "DELETE", f"/session/{session}")
        finally:
            cleanup(lambda: stop_process(webdriver))


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("webdriver_binary")
    args = parser.parse_args()
    run_test(args.webdriver_binary)
