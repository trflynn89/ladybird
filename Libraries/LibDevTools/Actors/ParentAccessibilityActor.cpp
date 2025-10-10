/*
 * Copyright (c) 2025, Sam Atkins <sam@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibDevTools/Actors/AccessibilityActor.h>
#include <LibDevTools/Actors/ParentAccessibilityActor.h>
#include <LibDevTools/DevToolsServer.h>

namespace DevTools {

NonnullRefPtr<ParentAccessibilityActor> ParentAccessibilityActor::create(DevToolsServer& devtools, String name)
{
    return adopt_ref(*new ParentAccessibilityActor(devtools, move(name)));
}

ParentAccessibilityActor::ParentAccessibilityActor(DevToolsServer& devtools, String name)
    : Actor(devtools, move(name))
{
}

ParentAccessibilityActor::~ParentAccessibilityActor() = default;

void ParentAccessibilityActor::handle_message(Message const& message)
{
    if (message.type == "bootstrap"sv) {
        JsonObject state;
        state.set("canBeDisabled"sv, true);
        state.set("canBeEnabled"sv, true);

        JsonObject response;
        response.set("state"sv, move(state));
        send_response(message, move(response));
        return;
    }

    if (message.type == "enable"sv) {
        // Send a change event
        JsonObject response;
        response.set("canBeDisabled"sv, true);
        response.set("type"sv, "canBeDisabledChange"sv);
        send_response(message, move(response));

        // And then the blank response.
        send_message({});

        // And each AccessibilityActor is enabled and sends an "init" event
        for (auto const& [name, actor] : devtools().actor_registry()) {
            if (auto* accessibility_actor = as_if<AccessibilityActor>(*actor))
                accessibility_actor->enable();
        }
        return;
    }

    send_unrecognized_packet_type_error(message);
}

}
