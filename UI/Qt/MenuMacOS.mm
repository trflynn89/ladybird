/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <UI/Qt/Menu.h>

#import <AppKit/AppKit.h>

#include <QAction>
#include <QMenu>
#include <QPointer>
#include <QWidget>

namespace Ladybird {

QAction* execute_popup_menu(QMenu& menu, QPoint const& global_position)
{
    auto* parent = menu.parentWidget();
    if (!parent)
        return menu.exec(global_position);

    auto* window = parent->window();
    auto* native_view = reinterpret_cast<NSView*>(window->winId());
    if (!native_view)
        return menu.exec(global_position);

    auto* native_menu = menu.toNSMenu();
    if (!native_menu)
        return menu.exec(global_position);

    QPointer<QAction> selected_action;
    QObject connection_context;
    QObject::connect(&menu, &QMenu::triggered, &connection_context, [&selected_action](QAction* action) {
        selected_action = action;
    });

    auto local_position = window->mapFromGlobal(global_position);
    auto native_position = NSMakePoint(local_position.x(), local_position.y());
    if (!native_view.isFlipped)
        native_position.y = NSHeight(native_view.bounds) - native_position.y;

    [native_menu popUpMenuPositioningItem:nil atLocation:native_position inView:native_view];
    return selected_action;
}

}
