/*
 * Copyright (c) 2025, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Function.h>
#include <AK/NonnullRefPtr.h>
#include <AK/Optional.h>
#include <AK/RefCounted.h>
#include <AK/String.h>
#include <AK/StringView.h>
#include <AK/Variant.h>
#include <AK/Vector.h>
#include <AK/Weakable.h>
#include <LibGfx/Point.h>
#include <LibWebView/Forward.h>

namespace WebView {

enum class ActionID {
    NavigateBack,
    NavigateForward,
    Reload,

    CopySelection,
    Paste,
    SelectAll,

    SearchSelectedText,

    TakeVisibleScreenshot,
    TakeFullScreenshot,

    ViewSource,

    OpenInNewTab,
    CopyURL,

    OpenImage,
    CopyImage,

    OpenAudio,
    OpenVideo,
    PlayMedia,
    PauseMedia,
    MuteMedia,
    UnmuteMedia,
    ToggleMediaControlsState,
    ToggleMediaLoopState,
};

class WEBVIEW_API Action
    : public RefCounted<Action>
    , public Weakable<Action> {
public:
    static NonnullRefPtr<Action> create(String text, ActionID id, Function<void()> action);
    static NonnullRefPtr<Action> create_checkable(String text, ActionID id, Function<void()> action);

    void activate();

    String const& text() const { return m_text; }
    void set_text(String);

    String const& display_text() const { return m_display_text; }

    ActionID id() const { return m_id; }

    enum class Enabled {
        No,
        Yes,
    };
    Enabled enabled() const { return m_enabled; }
    void set_enabled(Enabled);

    enum class Visible {
        No,
        Yes,
    };
    Visible visible() const { return m_visible; }
    void set_visible(Visible);

    enum class Checked {
        No,
        Yes,
    };
    bool is_checkable() const { return m_checked.has_value(); }
    Checked checked() const { return *m_checked; }
    void set_checked(Checked);

    template<typename T>
    T* ui_action() const
    {
        if (m_ui_action)
            return static_cast<T*>(m_ui_action);
        return nullptr;
    }

    void set_ui_action(void* ui_action) { m_ui_action = ui_action; }

    Function<void()> on_text_changed;
    Function<void()> on_enabled_changed;
    Function<void()> on_visible_changed;
    Function<void()> on_checked_changed;

private:
    Action(String text, ActionID id, Function<void()> action);

    String m_text;
    String m_display_text;
    ActionID m_id;

    Enabled m_enabled { Enabled::Yes };
    Visible m_visible { Visible::Yes };
    Optional<Checked> m_checked;

    Function<void()> m_action;
    void* m_ui_action { nullptr };
};

struct WEBVIEW_API Separator { };

class WEBVIEW_API Menu : public RefCounted<Menu> {
public:
    using MenuItem = Variant<NonnullRefPtr<Action>, NonnullRefPtr<Menu>, Separator>;

    explicit Menu(StringView title);

    void add_action(NonnullRefPtr<Action> action);
    void add_submenu(NonnullRefPtr<Menu> submenu);
    void add_separator();

    StringView title() const { return m_title; }

    Span<MenuItem> items() { return m_items; }
    ReadonlySpan<MenuItem> items() const { return m_items; }

    using OnActivation = Function<void(Gfx::IntPoint)>;
    OnActivation on_activation;

private:
    StringView m_title;
    Vector<MenuItem> m_items;
};

}
