/*
 * Copyright (c) 2025, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#import <Cocoa/Cocoa.h>

@class BrowserWindow;

using InfoBarButtonClicked = void (^)(void);

@interface InfoBar : NSStackView

- (void)showWithMessage:(NSString*)message
       actionButtonTitle:(NSString*)action_title
     actionButtonClicked:(InfoBarButtonClicked)on_action
      dismissButtonTitle:(NSString*)title
    dismissButtonClicked:(InfoBarButtonClicked)on_dismissed
                activeTab:(BrowserWindow*)tab;
- (void)hide;

- (void)tabBecameActive:(BrowserWindow*)tab;

@end
