#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

import http.client
import json
import socket
import subprocess
import sys
import tempfile
import time

from contextlib import contextmanager

EVENT_TIMEOUT_SECONDS = 30
WEBDRIVER_REQUEST_TIMEOUT_SECONDS = 60


def unused_port():
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
        sock.bind(("127.0.0.1", 0))
        return sock.getsockname()[1]


def wait_for_port(port, timeout=EVENT_TIMEOUT_SECONDS):
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        try:
            with socket.create_connection(("127.0.0.1", port), timeout=0.1):
                return
        except OSError:
            time.sleep(0.05)
    raise RuntimeError(f"Timed out waiting for port {port}")


def request(port_or_connection, method, path, body=None):
    owns_connection = isinstance(port_or_connection, int)
    connection = (
        http.client.HTTPConnection("127.0.0.1", port_or_connection, timeout=WEBDRIVER_REQUEST_TIMEOUT_SECONDS)
        if owns_connection
        else port_or_connection
    )
    encoded_body = json.dumps(body).encode() if body is not None else None
    headers = {"Connection": "keep-alive"}
    if body is not None:
        headers["Content-Type"] = "application/json"
    try:
        connection.request(method, path, encoded_body, headers)
        response = connection.getresponse()
        response_body = response.read().decode()
    finally:
        if owns_connection:
            connection.close()
    try:
        payload = json.loads(response_body) if response_body else {}
    except json.JSONDecodeError as error:
        raise RuntimeError(f"{method} {path} returned invalid JSON: {response_body}") from error
    return response.status, payload, response_body


def create_session(port, capabilities=None):
    status, payload, response_body = request(
        port,
        "POST",
        "/session",
        {"capabilities": {"alwaysMatch": capabilities if capabilities is not None else {"ladybird:headless": True}}},
    )
    value = payload.get("value")
    session_id = value.get("sessionId") if isinstance(value, dict) else None
    if status != 200 or not session_id:
        raise AssertionError(f"New Session failed with HTTP {status}: {response_body}")
    return session_id


def cleanup(actions, primary_error=None):
    errors = []
    for action in actions:
        try:
            action()
        except BaseException as error:
            errors.append(error)
    if errors and primary_error is None:
        raise errors[0]


def stop(process):
    def wait():
        try:
            process.wait(timeout=5)
        except BaseException as error:
            primary_error = None if isinstance(error, subprocess.TimeoutExpired) else error
            cleanup([process.kill, lambda: process.wait(timeout=5)], primary_error=primary_error)
            if primary_error is not None:
                raise

    cleanup([process.terminate, wait])


@contextmanager
def webdriver_process(binary, *, profile=None, arguments=(), environment=None, headless=True):
    with tempfile.TemporaryDirectory(prefix="ladybird-webdriver-") as temporary_profile:
        port = unused_port()
        command = [binary, "-l", "127.0.0.1", "-p", str(port), "--profile-path", profile or temporary_profile]
        if headless:
            command.append("--headless")
        process = subprocess.Popen([*command, *arguments], env=environment)
        try:
            wait_for_port(port)
            yield process, port
        finally:
            cleanup([lambda: stop(process)], primary_error=sys.exc_info()[1])
        if process.returncode not in (0, -15):
            raise RuntimeError(f"WebDriver exited with status {process.returncode}")


@contextmanager
def webdriver_session(binary, *, capabilities=None, **options):
    with webdriver_process(binary, **options) as (_, port):
        session = create_session(port, capabilities)
        try:
            yield port, session
        finally:
            cleanup([lambda: request(port, "DELETE", f"/session/{session}")], primary_error=sys.exc_info()[1])
