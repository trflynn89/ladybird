#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause

import http.server
import json

import devtools_test_utils as devtools

from test_utils import cleanup
from test_utils import run_test_cases
from test_utils import running_http_server
from test_utils import stop_process
from test_utils import temporary_profile

IFRAME_PAGE = b"""<!doctype html><title>iframe-console</title>
<iframe srcdoc="<script>console.log('logged by the iframe at load', { early: 1 }); window.loggedAtLoad = true;</script>"></iframe>
<iframe id="blank"></iframe>
"""
WORKER_PAGE = b"""<!doctype html><title>worker-console</title>
<script>
window.onerror = (message, filename, line, column) => {
    window.lastError = { message, filename, line, column };
};
</script>
"""
WORKER = b'\nthrow new Error("worker console sentinel");\n'


RESOURCES = {
    "/iframe": ("text/html", IFRAME_PAGE),
    "/worker": ("text/html", WORKER_PAGE),
    "/worker.js": ("text/javascript", WORKER),
}


class Handler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        resource = RESOURCES.get(self.path)
        if resource is None:
            self.send_error(404)
            return
        content_type, body = resource
        self.send_response(200)
        self.send_header("Content-Type", content_type)
        self.end_headers()
        self.wfile.write(body)

    def log_message(self, format, *args):
        pass


def test_iframe_console(ladybird_binary):
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    with running_http_server(server):
        url = f"http://127.0.0.1:{server.server_port}/iframe"
        with temporary_profile(prefix="ladybird-iframe-console-") as profile:
            process, _, client = devtools.launch(ladybird_binary, profile, url)
            try:
                devtools.wait_for_page(client, url, "iframe-console")
                watcher = client.watch_frame_targets()

                def message_logged(text):
                    return next((message for message in messages if message["arguments"][0] == text), None)

                # What the iframe logged before the client watched arrives once it does.
                devtools.wait_until(
                    "the page has loaded, and the iframe has logged",
                    lambda: client.evaluate("document.readyState === 'complete' && frames[0].loggedAtLoad === true"),
                )
                messages = client.watch_resources(watcher, "console-message")
                client.receive_matching(lambda packet: message_logged("logged by the iframe at load") is not None)

                # What the iframe logs afterward arrives as it comes, with objects the client can expand. The server
                # replaces the frame target when a load finishes, and the old target's object actors go with it; the
                # root actor answers a request to one of them with unknownActor, so log a fresh object then.
                def expand_logged_object():
                    for attempt in range(5):
                        text = f"logged by the iframe later ({attempt})"
                        client.evaluate(f"frames[0].console.log('{text}', {{ late: 2 }}); undefined")
                        if message_logged(text) is None:
                            client.receive_matching(lambda packet, text=text: message_logged(text) is not None)
                        logged = message_logged(text)
                        assert logged is not None, messages
                        grip = logged["arguments"][1]
                        assert grip.get("class") == "Object" and "actor" in grip, grip

                        actor = grip["actor"]
                        client.send({"to": actor, "type": "prototypeAndProperties"})
                        reply = client.receive_matching(
                            lambda packet, actor=actor: (
                                packet.get("from") == actor
                                or (packet.get("error") == "unknownActor" and f"'{actor}'" in packet.get("message", ""))
                            )
                        )
                        if reply.get("from") == actor:
                            return reply
                    raise AssertionError("The logged object's actor went away five times")

                properties = expand_logged_object()
                assert "late" in properties.get("ownProperties", {}), properties

                # An iframe that stays on its first document, about:blank, logs to the tab's console too.
                client.evaluate(
                    "document.getElementById('blank').contentWindow.console.log('logged by about:blank'); 0"
                )
                if message_logged("logged by about:blank") is None:
                    client.receive_matching(lambda packet: message_logged("logged by about:blank") is not None)
                print("PASS: An iframe's console messages reach the tab's console, and its logged objects expand")
            finally:
                cleanup(client.close, lambda: stop_process(process))


def test_worker_error_console(ladybird_binary):
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 0), Handler)
    with running_http_server(server):
        origin = f"http://127.0.0.1:{server.server_port}"
        url = origin + "/worker"
        worker_url = origin + "/worker.js"
        with temporary_profile(prefix="ladybird-worker-console-") as profile:
            process, _, client = devtools.launch(ladybird_binary, profile, url)
            try:
                devtools.wait_for_page(client, url, "worker-console")
                watcher = client.watch_frame_targets()
                # The page sends its errors once a client watches them.
                resources = client.watch_resources(watcher, "error-message")
                client.evaluate("window.worker = new Worker('/worker.js'); undefined")
                if not resources:
                    client.receive_matching(lambda packet: bool(resources))
                errors = [resource["pageError"] for resource in resources]
                preview = errors[0]["exception"]["preview"]
                event = json.loads(client.evaluate("JSON.stringify(window.lastError)"))
                assert "worker console sentinel" in event["message"], event
                assert preview["message"] == event["message"], preview
                assert event["filename"] == worker_url, event
                assert event["line"] == 2 and event["column"] > 0, event
                expected_location = f"@{worker_url}:{event['line']}:{event['column']}\n"
                assert preview.get("stack") == expected_location, preview
                assert not errors[0]["isPromiseRejection"], errors[0]
                print("PASS: An unhandled worker exception keeps its message and source location in the console")
            finally:
                cleanup(client.close, lambda: stop_process(process))


if __name__ == "__main__":
    run_test_cases([test_iframe_console, test_worker_error_console], "ladybird_binary")
