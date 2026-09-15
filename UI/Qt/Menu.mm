/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <UI/Qt/Menu.h>

#include <QMenu>

#import <AppKit/AppKit.h>

namespace Ladybird {

void enable_menu_icons([[maybe_unused]] QMenu& menu)
{
#if __MAC_OS_X_VERSION_MAX_ALLOWED >= 270000
    if (@available(macOS 27, *)) {
        NSMenu* native_menu = menu.toNSMenu();

        for (NSMenuItem* item in [native_menu itemArray]) {
            [item setPreferredImageVisibility:NSMenuItemImageVisibilityVisible];
        }
    }
#endif
}

void execute_context_menu(QMenu& menu, QPoint const& global_position)
{
    auto* parent = menu.parentWidget();
    if (!parent) {
        menu.exec(global_position);
        return;
    }

    auto* window = parent->window();
    auto* native_view = reinterpret_cast<NSView*>(window->winId());
    if (!native_view) {
        menu.exec(global_position);
        return;
    }

    auto* native_menu = menu.toNSMenu();
    if (!native_menu) {
        menu.exec(global_position);
        return;
    }

    auto local_position = window->mapFromGlobal(global_position);
    auto native_position = NSMakePoint(local_position.x(), local_position.y());
    if (!native_view.isFlipped)
        native_position.y = NSHeight(native_view.bounds) - native_position.y;

    [native_menu popUpMenuPositioningItem:nil atLocation:native_position inView:native_view];
}

}
