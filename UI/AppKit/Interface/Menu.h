/*
 * Copyright (c) 2025, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <Interface/Menu.h>
#include <LibWebView/Menu.h>

#import <Cocoa/Cocoa.h>

namespace Ladybird {

NSMenu* create_application_menu(WebView::Menu&, WebView::Menu::OnActivation);
NSMenuItem* create_application_menu_item(WebView::Action&);

}
