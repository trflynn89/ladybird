/*
 * Copyright (c) 2023-2026, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <LibWebView/PrivateBrowsing.h>

#import <Cocoa/Cocoa.h>

static constexpr NSInteger VERTICAL_TABS_NEXT_MENU_ITEM_TAG = 0x4c425601;
static constexpr NSInteger VERTICAL_TABS_PREVIOUS_MENU_ITEM_TAG = 0x4c425602;

@interface BrowserWindow : NSWindow

- (instancetype)init:(WebView::IsPrivate)is_private;
- (void)restoreSavedFrame;

@property (nonatomic, weak) NSResponder* preferred_first_responder;

@end
