/*
 * Copyright (c) 2023-2026, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <LibWebView/PrivateBrowsing.h>

#import <Cocoa/Cocoa.h>

@interface BrowserWindow : NSWindow

- (instancetype)init:(WebView::IsPrivate)is_private;

@property (nonatomic, weak) NSResponder* preferred_first_responder;

@end
