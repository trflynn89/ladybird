/*
 * Copyright (c) 2025, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <LibWebView/Menu.h>

class QAction;
class QMenu;
class QObject;
class QPalette;
class QWidget;

namespace Ladybird {

QMenu* create_application_menu(QWidget* parent, WebView::Menu&, WebView::Menu::OnActivation);

QAction* create_application_action(QObject* parent, QPalette const&, WebView::Action&);
void refresh_application_action_icon(QPalette const&, WebView::Action&);

}
