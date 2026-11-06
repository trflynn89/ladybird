#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

import os
import shlex
import subprocess

from pathlib import Path


def descendants(root_pid):
    rows = subprocess.check_output(["ps", "-axo", "pid=,ppid=,command="], text=True).splitlines()
    children = {}
    for row in rows:
        fields = row.strip().split(None, 2)
        pid, parent = fields[:2]
        command = fields[2] if len(fields) == 3 else ""
        children.setdefault(int(parent), []).append((int(pid), command))
    pending = [root_pid]
    while pending:
        for pid, command in children.get(pending.pop(), []):
            pending.append(pid)
            yield pid, shlex.split(command)


def descendants_named(root_pid, process_name):
    return [pid for pid, arguments in descendants(root_pid) if arguments and Path(arguments[0]).name == process_name]


def isolated_environment(directory):
    environment = os.environ.copy()
    for variable, name in (
        ("XDG_DATA_HOME", "data"),
        ("XDG_CONFIG_HOME", "config"),
        ("XDG_CACHE_HOME", "cache"),
    ):
        environment[variable] = str(Path(directory) / name)
    return environment
