#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

import json
import socket
import subprocess
import sys
import threading
import time

import webdriver_test as helpers

from webdriver_test import stop

TIMEOUT_SECONDS = helpers.EVENT_TIMEOUT_SECONDS


class ConnectionClosed(Exception):
    pass


class DevToolsClient:
    def __init__(self, port, deadline=None):
        # The server takes one client at a time, and turns another away until it's done with the previous one.
        if deadline is None:
            deadline = time.monotonic() + TIMEOUT_SECONDS
        while True:
            try:
                self.socket = socket.create_connection(("127.0.0.1", port), timeout=TIMEOUT_SECONDS)
            except OSError:
                # The server isn't listening yet.
                if time.monotonic() > deadline:
                    raise
                time.sleep(0.05)
                continue
            self.buffer = b""
            self.console = None
            try:
                self.receive()
                return
            except ConnectionClosed:
                self.socket.close()
                if time.monotonic() > deadline:
                    raise
                time.sleep(0.05)

    def send(self, packet):
        data = json.dumps(packet).encode()
        self.socket.sendall(str(len(data)).encode() + b":" + data)

    def receive(self):
        while True:
            separator = self.buffer.find(b":")
            if separator != -1:
                length = int(self.buffer[:separator])
                end = separator + 1 + length
                if len(self.buffer) >= end:
                    packet = json.loads(self.buffer[separator + 1 : end])
                    self.buffer = self.buffer[end:]
                    return packet
            chunk = self.socket.recv(65536)
            if not chunk:
                raise ConnectionClosed("The DevTools server closed the connection")
            self.buffer += chunk

    def receive_matching(self, predicate):
        while True:
            packet = self.receive()
            # The server replaces the frame target whenever a load finishes, and the new target's form can arrive
            # at any point of an exchange: follow it, as a client would, so that later requests go to the live target.
            if packet.get("type") == "target-available-form":
                self.console = packet["target"]["consoleActor"]
            if predicate(packet):
                return packet

    def tab(self):
        self.send({"to": "root", "type": "listTabs"})
        tabs = self.receive_matching(lambda packet: packet.get("from") == "root" and "tabs" in packet)["tabs"]
        assert len(tabs) == 1, tabs
        return tabs[0]

    def watch_frame_targets(self):
        tab = self.tab()["actor"]
        self.send({"to": tab, "type": "getWatcher"})
        watcher = self.receive_matching(lambda packet: packet.get("from") == tab and "actor" in packet)["actor"]
        self.send({"to": watcher, "type": "watchTargets", "targetType": "frame"})
        self.follow_frame_target()

    def follow_frame_target(self):
        self.receive_matching(lambda packet: packet.get("type") == "target-available-form")

    def evaluate(self, script):
        # A target switch between choosing the console and its answer loses the request: the old console is gone (an
        # unknownActor error naming it, with no resultID) or drops its pending result. Ask the new target's console
        # again then. An error for an earlier console is stale, and passes by.
        def unknown_actor(packet, console):
            return packet.get("error") == "unknownActor" and console in packet.get("message", "")

        while True:
            console = self.console
            self.send({"to": console, "type": "evaluateJSAsync", "text": script})
            packet = self.receive_matching(
                lambda packet, console=console: (
                    (packet.get("from") == console and "resultID" in packet)
                    or unknown_actor(packet, console)
                    or self.console != console
                )
            )
            if unknown_actor(packet, console) and self.console == console:
                self.follow_frame_target()
            if self.console != console:
                continue
            result_id = packet["resultID"]
            result = self.receive_matching(
                lambda packet, console=console, result_id=result_id: (
                    (packet.get("type") == "evaluationResult" and packet.get("resultID") == result_id)
                    or self.console != console
                )
            )
            if self.console != console:
                continue
            assert not result.get("hasException"), result.get("exceptionMessage")
            return result.get("result")

    def close(self):
        self.socket.close()


def wait_until(description, condition):
    deadline = time.monotonic() + TIMEOUT_SECONDS
    while time.monotonic() < deadline:
        if condition():
            return
        time.sleep(0.05)
    raise AssertionError(f"Timed out waiting until {description}")


def wait_for_page(client, url, path):
    # The tab takes the title of the page's document once that document is active.
    def tab_shows_page():
        tab = client.tab()
        return tab.get("url") == url and tab.get("title") == path

    wait_until(f"the tab shows {url}", tab_shows_page)


def launch(ladybird_binary, profile, url, *, arguments=()):
    # Launches the browser with DevTools on a free port and connects the first client, whose connection doubles as the
    # check that the server is up: a throwaway probe would hold the server's one connection slot until it noticed the
    # probe had gone, and turn the client away meanwhile.
    # Another process can take the port between unused_port() and the server's bind(), so try another one then. The
    # attempts share one deadline, so a server that never comes up fails the test within it.
    deadline = time.monotonic() + TIMEOUT_SECONDS
    for _ in range(5):
        if time.monotonic() >= deadline:
            break
        devtools_port = helpers.unused_port()
        process = subprocess.Popen(
            [
                ladybird_binary,
                "--headless=manual",
                f"--profile-path={profile}",
                f"--devtools={devtools_port}",
                "--expose-internals-object",
                *arguments,
                url,
            ],
            stderr=subprocess.PIPE,
            text=True,
        )
        bind_failed = threading.Event()

        def forward_stderr(stderr=process.stderr, bind_failed=bind_failed):
            assert stderr is not None
            for line in stderr:
                if line.startswith("Unable to launch devtools server"):
                    bind_failed.set()
                sys.stderr.write(line)

        threading.Thread(target=forward_stderr, daemon=True).start()

        while not bind_failed.is_set() and process.poll() is None and time.monotonic() < deadline:
            try:
                attempt_deadline = min(deadline, time.monotonic() + 1)
                return process, devtools_port, DevToolsClient(devtools_port, deadline=attempt_deadline)
            except OSError:
                pass
        stop(process)
    raise AssertionError("The DevTools server didn't start")
