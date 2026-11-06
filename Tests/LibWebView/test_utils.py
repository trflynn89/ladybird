#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

import subprocess
import sys


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
