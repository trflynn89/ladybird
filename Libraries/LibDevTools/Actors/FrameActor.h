/*
 * Copyright (c) 2025, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/NonnullRefPtr.h>
#include <AK/Types.h>
#include <LibDevTools/Actor.h>

namespace DevTools {

class FrameActor final : public Actor {
public:
    static constexpr auto base_name = "frame"sv;

    static NonnullRefPtr<FrameActor> create(DevToolsServer&, String name, WeakPtr<TabActor>, WeakPtr<CSSPropertiesActor>, WeakPtr<ConsoleActor>, WeakPtr<InspectorActor>, WeakPtr<ThreadActor>);
    virtual ~FrameActor() override;

    virtual void handle_message(StringView type, JsonObject const&) override;
    void send_frame_update_message();

    JsonObject serialize_target() const;

private:
    FrameActor(DevToolsServer&, String name, WeakPtr<TabActor>, WeakPtr<CSSPropertiesActor>, WeakPtr<ConsoleActor>, WeakPtr<InspectorActor>, WeakPtr<ThreadActor>);

    void received_console_message(i32 message_index);
    void received_console_messages(i32 start_index, ReadonlySpan<String> message_types, ReadonlySpan<String> messages);
    void request_console_messages();

    WeakPtr<TabActor> m_tab;

    WeakPtr<CSSPropertiesActor> m_css_properties;
    WeakPtr<ConsoleActor> m_console;
    WeakPtr<InspectorActor> m_inspector;
    WeakPtr<ThreadActor> m_thread;

    i32 m_highest_notified_message_index { -1 };
    i32 m_highest_received_message_index { -1 };
    bool m_waiting_for_messages { false };
};

}
