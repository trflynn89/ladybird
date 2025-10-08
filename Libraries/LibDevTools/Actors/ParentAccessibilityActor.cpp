/*
 * Copyright (c) 2025, Sam Atkins <sam@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibDevTools/Actors/ParentAccessibilityActor.h>

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
        // Our accessibility tree is always enabled. No response expected.
        return;
    }

    send_unrecognized_packet_type_error(message);
}

}
