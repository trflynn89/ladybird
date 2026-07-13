/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/JsonArray.h>
#include <AK/JsonObject.h>
#include <LibDevTools/Actors/CompatibilityActor.h>

namespace DevTools {

NonnullRefPtr<CompatibilityActor> CompatibilityActor::create(DevToolsServer& devtools, String name)
{
    return adopt_ref(*new CompatibilityActor(devtools, move(name)));
}

CompatibilityActor::CompatibilityActor(DevToolsServer& devtools, String name)
    : Actor(devtools, move(name))
{
}

CompatibilityActor::~CompatibilityActor() = default;

void CompatibilityActor::handle_message(Message const& message)
{
    JsonObject response;

    if (message.type == "getCSSDeclarationBlockIssues"sv) {
        auto declaration_blocks = get_required_parameter<JsonArray>(message, "domRulesDeclarations"sv);
        if (!declaration_blocks.has_value())
            return;

        JsonArray issues;
        for (size_t i = 0; i < declaration_blocks->size(); ++i)
            issues.must_append(JsonArray {});

        response.set("compatibilityIssues"sv, move(issues));
        send_response(message, move(response));
        return;
    }

    if (message.type == "getNodeCssIssues"sv) {
        response.set("compatibilityIssues"sv, JsonArray {});
        send_response(message, move(response));
        return;
    }

    if (message.type == "getTraits"sv) {
        response.set("traits"sv, JsonObject {});
        send_response(message, move(response));
        return;
    }

    send_unrecognized_packet_type_error(message);
}

JsonObject CompatibilityActor::serialize_description() const
{
    JsonObject description;
    description.set("actor"sv, name());
    return description;
}

}
