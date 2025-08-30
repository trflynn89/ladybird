/*
 * Copyright (c) 2025, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/Enumerate.h>
#include <LibWebView/Menu.h>

namespace WebView {

static String remove_ampersand_shortcut(String const& text)
{
    if (!text.contains('&'))
        return text;

    StringBuilder builder;
    auto view = text.bytes_as_string_view();

    for (auto [i, ch] : enumerate(view)) {
        if (ch == '&') {
            if ((i + 1 < view.length()) && (view[i + 1] != '&'))
                continue;
        }

        builder.append(ch);
    }

    return MUST(builder.to_string());
}

NonnullRefPtr<Action> Action::create(String text, ActionID id, Function<void()> action)
{
    return adopt_ref(*new Action { move(text), id, move(action) });
}

NonnullRefPtr<Action> Action::create_checkable(String text, ActionID id, Function<void()> action)
{
    auto checkable = create(move(text), id, move(action));
    checkable->m_checked = Checked::No;

    return checkable;
}

Action::Action(String text, ActionID id, Function<void()> action)
    : m_text(move(text))
    , m_display_text(remove_ampersand_shortcut(m_text))
    , m_id(id)
    , m_action(move(action))
{
}

void Action::activate()
{
    if (m_action)
        m_action();
}

void Action::set_text(String text)
{
    if (m_text == text)
        return;

    m_text = move(text);
    m_display_text = remove_ampersand_shortcut(m_text);

    if (on_text_changed)
        on_text_changed();
}

void Action::set_enabled(Enabled enabled)
{
    if (m_enabled == enabled)
        return;

    m_enabled = enabled;

    if (on_enabled_changed)
        on_enabled_changed();
}

void Action::set_visible(Visible visible)
{
    if (m_visible == visible)
        return;

    m_visible = visible;

    if (on_visible_changed)
        on_visible_changed();
}

void Action::set_checked(Checked checked)
{
    VERIFY(is_checkable());

    if (m_checked == checked)
        return;

    m_checked = checked;

    if (on_checked_changed)
        on_checked_changed();
}

Menu::Menu(StringView title)
    : m_title(title)
{
}

void Menu::add_action(NonnullRefPtr<Action> action)
{
    m_items.append(move(action));
}

void Menu::add_submenu(NonnullRefPtr<Menu> submenu)
{
    m_items.append(move(submenu));
}

void Menu::add_separator()
{
    m_items.append(Separator {});
}

}
