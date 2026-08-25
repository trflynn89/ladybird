/*
 * Copyright (c) 2025-2026, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Platform.h>
#include <LibWebView/Menu.h>

class QAction;
class QMenu;
class QPoint;
class QWidget;

namespace Ladybird {

class WebContentView;

enum class ActionIconMode {
    None,
    Chrome,
    Menu,
};

QMenu* create_application_menu(QWidget& parent, WebView::Menu&);
void repopulate_application_menu(QMenu& menu, QWidget& parent, WebView::Menu& source);
void update_history_menu(QMenu& menu, WebContentView*);
void populate_session_history_traversal_menu(QMenu& menu, WebContentView&, int direction);

QMenu* create_context_menu(QWidget& parent, WebContentView&, WebView::Menu&);
QAction* create_application_action(QWidget& parent, WebView::Action&, ActionIconMode = ActionIconMode::Chrome);
QAction* execute_popup_menu(QMenu&, QPoint const& global_position);

#if defined(AK_OS_MACOS)
void initialize_native_menu_action(QAction&, WebView::ActionID, bool engaged = false);
#endif

}
