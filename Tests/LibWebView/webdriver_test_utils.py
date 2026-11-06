#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

import http.client
import json
import os
import socket
import subprocess
import sys
import threading
import time

from contextlib import contextmanager

from test_utils import cleanup
from test_utils import stop_process

EVENT_TIMEOUT_SECONDS = 30
WEBDRIVER_REQUEST_TIMEOUT_SECONDS = 60


def unused_port():
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
        sock.bind(("127.0.0.1", 0))
        return sock.getsockname()[1]


def launch_webdriver(webdriver_binary, *arguments, output=None, **popen_arguments):
    # A free port picked here could be taken by another test first, so WebDriver picks one and reports it.
    command = [webdriver_binary, *arguments, "-l", "127.0.0.1", "-p", "0"]
    if output is None:
        process = subprocess.Popen(command, stdout=subprocess.PIPE, bufsize=0, **popen_arguments)
        assert process.stdout is not None
        listening = process.stdout.readline()
        os.write(sys.stdout.fileno(), listening)
    else:
        process = subprocess.Popen(command, stdout=output, **popen_arguments)
        listening = wait_for_listening_line(process, output)
    if not listening.startswith(b"Listening on "):
        process.kill()
        process.wait()
        raise RuntimeError(f"WebDriver did not report its port: {listening!r}")
    if output is None:
        threading.Thread(target=forward_output, args=(process.stdout,), daemon=True).start()
    return process, int(listening.rsplit(b":", 1)[1])


def wait_for_listening_line(process, output):
    deadline = time.monotonic() + EVENT_TIMEOUT_SECONDS
    while True:
        written = os.pread(output.fileno(), 65536, 0)
        for line in written.splitlines(keepends=True):
            if line.startswith(b"Listening on ") and line.endswith(b"\n"):
                return line
        if process.poll() is not None or time.monotonic() >= deadline:
            return written
        time.sleep(0.05)


def forward_output(stream):
    # Unbuffered I/O takes no locks, so a browser still holding the pipe can't block Python's exit.
    while output := stream.read(65536):
        os.write(sys.stdout.fileno(), output)


def request(webdriver_port, method, path, body=None):
    connection = http.client.HTTPConnection("127.0.0.1", webdriver_port, timeout=WEBDRIVER_REQUEST_TIMEOUT_SECONDS)
    try:
        return request_on_connection(connection, method, path, body)
    finally:
        connection.close()


def create_session(webdriver_port, capabilities=None):
    status, payload, response_body = request(
        webdriver_port,
        "POST",
        "/session",
        {"capabilities": {"alwaysMatch": {"ladybird:headless": True, **(capabilities or {})}}},
    )
    value = payload.get("value")
    session_id = value.get("sessionId") if isinstance(value, dict) else None
    if status != 200 or not session_id:
        raise AssertionError(f"New Session failed with HTTP {status}: {response_body}")
    return session_id


def request_on_connection(connection, method, path, body=None):
    encoded_body = json.dumps(body).encode() if body is not None else None
    headers = {"Connection": "keep-alive"}
    if body is not None:
        headers["Content-Type"] = "application/json"
    connection.request(method, path, encoded_body, headers)
    response = connection.getresponse()
    response_body = response.read().decode()

    try:
        payload = json.loads(response_body) if response_body else {}
    except json.JSONDecodeError as error:
        raise RuntimeError(f"{method} {path} returned invalid JSON: {response_body}") from error
    return response.status, payload, response_body


class WebDriverSession:
    def __init__(self, process, port, capabilities=None):
        self.process = process
        self.port = port
        self.capabilities = capabilities
        self.session_id = None

    def create(self):
        self.session_id = create_session(self.port, self.capabilities)

    def close(self):
        if self.session_id is not None:
            status, _, raw = request(self.port, "DELETE", f"/session/{self.session_id}")
            assert status == 200, raw
            self.session_id = None

    def restart(self):
        self.close()
        self.create()

    def request(self, path, body=None, method="POST"):
        return request(self.port, method, f"/session/{self.session_id}/{path}", body)

    def command(self, path, body=None, method="POST"):
        status, payload, raw = self.request(path, body, method)
        assert status == 200, raw
        return payload["value"]

    def script(self, source, *args):
        return self.command("execute/sync", {"script": source, "args": list(args)})

    def async_script(self, source, *args):
        return self.command("execute/async", {"script": source, "args": list(args)})

    def wait_for_script(self, source, *args, expected: object = True, timeout=EVENT_TIMEOUT_SECONDS, interval=0.01):
        deadline = time.monotonic() + timeout
        state = None
        while time.monotonic() < deadline:
            status, payload, raw = self.request("execute/sync", {"script": source, "args": list(args)})
            if status == 500 and payload["value"]["message"] == "WebContent was replaced while executing the command":
                continue
            assert status == 200, raw
            state = payload["value"]
            if state == expected:
                return state
            time.sleep(interval)
        raise AssertionError(f"Expected {expected!r} from {source!r}, got {state!r}")


@contextmanager
def webdriver_session(webdriver_binary, *arguments, capabilities=None, **popen_arguments):
    process, port = launch_webdriver(webdriver_binary, *arguments, **popen_arguments)
    session = WebDriverSession(process, port, capabilities)
    try:
        session.create()
        yield session
    finally:
        cleanup(session.close, lambda: stop_process(process))
