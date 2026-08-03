/*
 * Copyright (c) 2026, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#import <Cocoa/Cocoa.h>

@class Tab;

NS_ASSUME_NONNULL_BEGIN

@interface SidebarViewController : NSViewController

- (void)setTabs:(NSArray<Tab*>*)tabs;
- (void)setSelectedTab:(nullable Tab*)tab;
- (void)reloadTab:(Tab*)tab;

@property (nonatomic, copy, nullable) void (^on_tab_selected)(Tab*);
@property (nonatomic, copy, nullable) void (^on_tab_closed)(Tab*);
@property (nonatomic, copy, nullable) void (^on_new_tab)(void);
@property (nonatomic, copy, nullable) void (^on_tabs_reordered)(NSArray<Tab*>*);

@end

NS_ASSUME_NONNULL_END
