/*
 * Copyright (c) 2025, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <LibURL/Forward.h>

#import <Cocoa/Cocoa.h>

@class Tab;

using InfoBarDismissed = void (^)(void);

@interface InfoBar : NSStackView

- (void)showWithMessage:(NSString*)message
                helpLink:(URL::URL)help_link
      dismissButtonTitle:(NSString*)title
    dismissButtonClicked:(InfoBarDismissed)on_dismissed
               activeTab:(Tab*)tab;
- (void)hide;

- (void)tabBecameActive:(Tab*)tab;

@end
