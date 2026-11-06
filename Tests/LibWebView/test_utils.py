#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

import json
import subprocess
import sys
import tempfile
import threading

from contextlib import contextmanager
from pathlib import Path


def cleanup(*actions, primary_error=None):
    if primary_error is None:
        primary_error = sys.exc_info()[1]
    errors = []
    for action in actions:
        try:
            action()
        except BaseException as error:
            errors.append(error)
    if errors and primary_error is None:
        raise errors[0]


def stop_process(process):
    errors = []
    try:
        process.terminate()
    except BaseException as error:
        errors.append(error)
    try:
        process.wait(timeout=5)
    except BaseException as error:
        if not isinstance(error, subprocess.TimeoutExpired):
            errors.append(error)
        # A failed terminate or wait must not prevent the remaining attempts to reap the process.
        for action in (process.kill, lambda: process.wait(timeout=5)):
            try:
                action()
            except BaseException as error:
                errors.append(error)
    if errors:
        raise errors[0]


@contextmanager
def temporary_profile(*, settings=None, permissions=None, prefix="ladybird-test-"):
    with tempfile.TemporaryDirectory(prefix=prefix) as directory:
        profile = Path(directory)
        if settings is not None or permissions is not None:
            config = profile / "config"
            config.mkdir()
            for name, data in (("Settings.json", settings), ("Permissions.json", permissions)):
                if data is not None:
                    (config / name).write_text(json.dumps(data))
        yield profile


@contextmanager
def running_http_server(server):
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    try:
        yield server
    finally:
        cleanup(server.shutdown, server.server_close, thread.join)
