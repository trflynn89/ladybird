#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

import unittest

from unittest.mock import Mock
from unittest.mock import patch

import webdriver_test_utils as webdriver

from test_utils import running_http_server


class WebDriverSessionTests(unittest.TestCase):
    def test_failed_session_creation_stops_the_driver(self):
        process = Mock()
        failure = RuntimeError("New Session failed")
        with (
            patch.object(webdriver, "launch_webdriver", return_value=(process, 1234)),
            patch.object(webdriver, "create_session", side_effect=failure),
            patch.object(webdriver, "stop_process") as stop,
            patch.object(webdriver, "request") as request,
        ):
            with self.assertRaises(RuntimeError) as raised:
                with webdriver.webdriver_session("WebDriver"):
                    self.fail("Session creation should have failed")
            self.assertIs(raised.exception, failure)
            stop.assert_called_once_with(process)
            request.assert_not_called()

    def test_cleanup_failures_preserve_the_test_failure(self):
        process = Mock()
        failure = AssertionError("Test failed")
        with (
            patch.object(webdriver, "launch_webdriver", return_value=(process, 1234)),
            patch.object(webdriver, "create_session", return_value="session"),
            patch.object(webdriver, "request", side_effect=OSError("Delete failed")),
            patch.object(webdriver, "stop_process", side_effect=OSError("Stop failed")) as stop,
        ):
            with self.assertRaises(AssertionError) as raised:
                with webdriver.webdriver_session("WebDriver"):
                    raise failure
            self.assertIs(raised.exception, failure)
            stop.assert_called_once_with(process)

    def test_session_restart_preserves_capabilities_and_cleans_up_the_new_session(self):
        process = Mock()
        capabilities = {"ladybird:headless": False}
        with (
            patch.object(webdriver, "launch_webdriver", return_value=(process, 1234)),
            patch.object(webdriver, "create_session", side_effect=["first", "second"]) as create,
            patch.object(webdriver, "request", return_value=(200, {"value": None}, "")) as request,
            patch.object(webdriver, "stop_process"),
        ):
            with webdriver.webdriver_session("WebDriver", capabilities=capabilities) as session:
                session.restart()
                self.assertEqual(session.session_id, "second")
            self.assertEqual(create.call_count, 2)
            create.assert_called_with(1234, capabilities)
            self.assertEqual([call.args[2] for call in request.call_args_list], ["/session/first", "/session/second"])


class ScriptWaitingTests(unittest.TestCase):
    def test_waiting_retries_renderer_replacement(self):
        session = webdriver.WebDriverSession(Mock(), 1234)
        session.session_id = "session"
        with patch.object(
            session,
            "request",
            side_effect=[
                (500, {"value": {"message": "WebContent was replaced while executing the command"}}, ""),
                (200, {"value": ["complete", 2]}, ""),
            ],
        ):
            self.assertEqual(session.wait_for_script("return state", expected=["complete", 2]), ["complete", 2])

    def test_waiting_does_not_retry_other_command_errors(self):
        session = webdriver.WebDriverSession(Mock(), 1234)
        with patch.object(
            session, "request", return_value=(500, {"value": {"message": "Script failed"}}, "Script failed")
        ) as request:
            with self.assertRaisesRegex(AssertionError, "Script failed"):
                session.wait_for_script("return state")
            request.assert_called_once()


class HTTPServerTests(unittest.TestCase):
    def test_shutdown_failure_still_closes_the_socket_and_joins_the_thread(self):
        server = Mock()
        failure = OSError("Shutdown failed")
        server.shutdown.side_effect = failure
        with patch("test_utils.threading.Thread") as thread:
            with self.assertRaises(OSError) as raised:
                with running_http_server(server):
                    pass
            self.assertIs(raised.exception, failure)
            server.server_close.assert_called_once()
            thread.return_value.join.assert_called_once()


if __name__ == "__main__":
    unittest.main()
