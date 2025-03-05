/*
 * Copyright (c) 2025, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/JsonObject.h>
#include <AK/Optional.h>
#include <AK/Variant.h>
#include <LibDevTools/Actors/NodeActor.h>
#include <LibDevTools/Actors/TabActor.h>
#include <LibDevTools/Actors/WalkerActor.h>
#include <LibDevTools/DevToolsDelegate.h>
#include <LibDevTools/DevToolsServer.h>
#include <LibWebView/Attribute.h>

namespace DevTools {

struct AttributeModification {
    Optional<String> name_of_attribute_to_replace;
    Vector<WebView::Attribute> replacement_attributes;
};
static Optional<AttributeModification> parse_attribute_modification(JsonArray const& modifications)
{
    if (modifications.is_empty())
        return {};

    Optional<String> name_of_attribute_to_replace;
    Vector<WebView::Attribute> replacement_attributes;

    auto parse_modification = [&](JsonValue const& modification) -> Variant<Empty, String, WebView::Attribute> {
        if (!modification.is_object())
            return {};

        auto name = modification.as_object().get_string("attributeName"sv);
        if (!name.has_value())
            return {};

        auto value = modification.as_object().get_string("newValue"sv);
        if (!value.has_value())
            return *name;

        return WebView::Attribute { *name, *value };
    };

    auto modification = parse_modification(modifications.at(0));
    if (modification.has<Empty>())
        return {};

    modification.visit(
        [&](String& name) { name_of_attribute_to_replace = move(name); },
        [&](WebView::Attribute& attribute) { replacement_attributes.append(move(attribute)); },
        [](Empty) { VERIFY_NOT_REACHED(); });

    for (auto i = 1uz; i < modifications.size(); ++i) {
        auto modification = parse_modification(modifications.at(i));

        if (auto* attribute = modification.get_pointer<WebView::Attribute>())
            replacement_attributes.empend(move(attribute->name), move(attribute->value));
    }

    return AttributeModification { move(name_of_attribute_to_replace), move(replacement_attributes) };
}

NonnullRefPtr<NodeActor> NodeActor::create(DevToolsServer& devtools, String name, WeakPtr<WalkerActor> walker)
{
    return adopt_ref(*new NodeActor(devtools, move(name), move(walker)));
}

NodeActor::NodeActor(DevToolsServer& devtools, String name, WeakPtr<WalkerActor> walker)
    : Actor(devtools, move(name))
    , m_walker(move(walker))
{
}

NodeActor::~NodeActor() = default;

void NodeActor::handle_message(StringView type, JsonObject const& message)
{
    JsonObject response;
    response.set("from"sv, name());

    if (type == "getUniqueSelector"sv) {
        if (auto dom_node = WalkerActor::dom_node_for(m_walker, name()); dom_node.has_value())
            response.set("value"sv, dom_node->node.get_string("name"sv)->to_ascii_lowercase());

        send_message(move(response));
        return;
    }

    if (type == "modifyAttributes"sv) {
        auto modifications = message.get_array("modifications"sv);
        if (!modifications.has_value()) {
            send_missing_parameter_error("modifications"sv);
            return;
        }

        auto attribute_modification = parse_attribute_modification(*modifications);
        if (!attribute_modification.has_value())
            return;

        Vector<JsonValue> mutations;
        mutations.ensure_capacity(attribute_modification->replacement_attributes.size());

        for (auto const& attribute : attribute_modification->replacement_attributes) {
            JsonObject mutation;
            mutation.set("target"sv, name());
            mutation.set("type"sv, "attributes"sv);
            mutation.set("attributeName"sv, attribute.name);
            mutation.set("newValue"sv, attribute.value);
            mutations.empend(move(mutation));
        }

        if (auto dom_node = WalkerActor::dom_node_for(m_walker, name()); dom_node.has_value()) {
            auto block_token = block_responses();

            auto on_complete = [weak_self = make_weak_ptr<NodeActor>(), mutations = move(mutations), block_token = move(block_token)](ErrorOr<void> node_id) mutable {
                if (node_id.is_error()) {
                    dbgln_if(DEVTOOLS_DEBUG, "Unable to edit DOM node: {}", node_id.error());
                    return;
                }

                if (auto self = weak_self.strong_ref())
                    self->finished_editing_dom_node({ move(mutations) }, move(block_token));
            };

            if (attribute_modification->name_of_attribute_to_replace.has_value()) {
                devtools().delegate().replace_dom_node_attribute(
                    dom_node->tab->description(),
                    dom_node->id,
                    attribute_modification->name_of_attribute_to_replace.release_value(),
                    move(attribute_modification->replacement_attributes),
                    move(on_complete));
            } else {
                devtools().delegate().add_dom_node_attributes(
                    dom_node->tab->description(),
                    dom_node->id,
                    move(attribute_modification->replacement_attributes),
                    move(on_complete));
            }
        }

        return;
    }

    if (type == "setNodeValue"sv) {
        auto value = message.get_string("value"sv);
        if (!value.has_value()) {
            send_missing_parameter_error("value"sv);
            return;
        }

        JsonObject mutation;
        mutation.set("target"sv, name());
        mutation.set("type"sv, "characterData"sv);
        mutation.set("newValue"sv, *value);

        if (auto dom_node = WalkerActor::dom_node_for(m_walker, name()); dom_node.has_value()) {
            auto block_token = block_responses();

            devtools().delegate().set_dom_node_value(
                dom_node->tab->description(), dom_node->id, value.release_value(),
                [weak_self = make_weak_ptr<NodeActor>(), mutation = move(mutation), block_token = move(block_token)](ErrorOr<void> node_id) mutable {
                    if (node_id.is_error()) {
                        dbgln_if(DEVTOOLS_DEBUG, "Unable to edit DOM node: {}", node_id.error());
                        return;
                    }

                    if (auto self = weak_self.strong_ref())
                        self->finished_editing_dom_node({ move(mutation) }, move(block_token));
                });
        }

        return;
    }

    send_unrecognized_packet_type_error(type);
}

void NodeActor::finished_editing_dom_node(Vector<JsonValue> mutations, BlockToken block_token)
{
    if (auto walker = m_walker.strong_ref())
        walker->new_dom_node_mutations(move(mutations));

    JsonObject message;
    message.set("from"sv, name());
    send_message(move(message), move(block_token));
}

}
