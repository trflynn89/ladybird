/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/JsonObject.h>
#include <LibDevTools/Actors/LadybirdActor.h>
#include <LibDevTools/DevToolsServer.h>

namespace DevTools {

NonnullRefPtr<LadybirdActor> LadybirdActor::create(DevToolsServer& devtools, String name)
{
    return adopt_ref(*new LadybirdActor(devtools, move(name)));
}

LadybirdActor::LadybirdActor(DevToolsServer& devtools, String name)
    : Actor(devtools, move(name))
{
}

LadybirdActor::~LadybirdActor() = default;

void LadybirdActor::handle_message(Message const& message)
{
    if (message.type == "connect"sv) {
        devtools().on_controller_connected();
        send_response(message, {});
        return;
    }

    send_unrecognized_packet_type_error(message);
}

void LadybirdActor::request_toolbox(u64 tab_id)
{
    JsonObject message;
    message.set("type"sv, "openToolbox"sv);
    message.set("tabId"sv, tab_id);
    send_message(move(message));
}

}
