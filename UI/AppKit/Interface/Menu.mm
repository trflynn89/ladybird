/*
 * Copyright (c) 2025, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/WeakPtr.h>
#import <Interface/Menu.h>
#import <Utilities/Conversions.h>

namespace Ladybird {

static void setup_menu_item(WebView::Action& action, NSMenuItem* item)
{
    auto weak_action = action.make_weak_ptr();
    __weak NSMenuItem* weak_item = item;

    action.on_text_changed = [weak_action, weak_item]() {
        auto action = weak_action.strong_ref();
        NSMenuItem* item = weak_item;

        if (action && item)
            [item setTitle:string_to_ns_string(action->display_text())];
    };

    action.on_enabled_changed = [weak_action, weak_item]() {
        auto action = weak_action.strong_ref();
        NSMenuItem* item = weak_item;

        if (action && item)
            [item setEnabled:action->enabled() == WebView::Action::Enabled::Yes];
    };

    action.on_visible_changed = [weak_action, weak_item]() {
        auto action = weak_action.strong_ref();
        NSMenuItem* item = weak_item;

        if (action && item)
            [item setHidden:action->enabled() == WebView::Action::Enabled::No];
    };

    action.on_checked_changed = [weak_action, weak_item]() {
        auto action = weak_action.strong_ref();
        NSMenuItem* item = weak_item;

        if (action && item)
            [item setState:action->checked() == WebView::Action::Checked::Yes ? NSControlStateValueOn : NSControlStateValueOff];
    };

    action.on_text_changed();
    action.on_enabled_changed();
    action.on_visible_changed();

    if (action.is_checkable())
        action.on_checked_changed();
}

static void add_items_to_menu(NSMenu* menu, Span<WebView::Menu::MenuItem> menu_items)
{
    for (auto& menu_item : menu_items) {
        menu_item.visit(
            [&](NonnullRefPtr<WebView::Action>& action) {
                [menu addItem:create_application_menu_item(action)];
            },
            [&](NonnullRefPtr<WebView::Menu> const& submenu) {
                auto* application_submenu = [[NSMenu alloc] initWithTitle:string_to_ns_string(submenu->title())];
                add_items_to_menu(application_submenu, submenu->items());

                auto* item = [[NSMenuItem alloc] init];
                [item setSubmenu:application_submenu];

                [menu addItem:item];
            },
            [&](WebView::Separator) {
                [menu addItem:[NSMenuItem separatorItem]];
            });
    }
}

NSMenu* create_application_menu(WebView::Menu& menu, WebView::Menu::OnActivation on_activation)
{
    auto* application_menu = [[NSMenu alloc] initWithTitle:string_to_ns_string(menu.title())];
    add_items_to_menu(application_menu, menu.items());

    menu.on_activation = move(on_activation);

    return application_menu;
}

NSMenuItem* create_application_menu_item(WebView::Action& action)
{
    auto* item = [[NSMenuItem alloc] initWithTitle:string_to_ns_string(action.display_text())
                                            action:nil
                                     keyEquivalent:@""];
    setup_menu_item(action, item);

    return item;
}

}
