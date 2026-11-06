#!/usr/bin/env python3
#
# Copyright (c) 2026-present, the Ladybird developers.
#
# SPDX-License-Identifier: BSD-2-Clause


def settings_exchange(session, action, *args, condition="true"):
    return session.async_script(
        f"""
        const parameters = Array.from(arguments);
        const done = parameters.pop();
        document.addEventListener('WebUIMessage', function listener(event) {{
            if (event.detail.name !== 'loadSettings') return;
            const settings = event.detail.data;
            if (!({condition})) return;
            document.removeEventListener('WebUIMessage', listener);
            done(settings);
        }});
        {action}
        """,
        *args,
    )
