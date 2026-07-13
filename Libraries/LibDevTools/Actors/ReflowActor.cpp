/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibDevTools/Actors/ReflowActor.h>

namespace DevTools {

NonnullRefPtr<ReflowActor> ReflowActor::create(DevToolsServer& devtools, String name)
{
    return adopt_ref(*new ReflowActor(devtools, move(name)));
}

ReflowActor::ReflowActor(DevToolsServer& devtools, String name)
    : Actor(devtools, move(name))
{
}

ReflowActor::~ReflowActor() = default;

void ReflowActor::handle_message(Message const& message)
{
    // The start and stop requests are one-way; the client does not await a response. Reflow events are not implemented
    // yet, so there is nothing to actually start or stop.
    if (message.type.is_one_of("start"sv, "stop"sv))
        return;

    send_unrecognized_packet_type_error(message);
}

}
