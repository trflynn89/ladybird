/*
 * Copyright (c) 2026, Timothy Flynn <trflynn89@pm.me>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#import <Cocoa/Cocoa.h>

@class Tab;

@interface SidebarViewController : NSViewController

- (instancetype)initWithTabs:(NSArray<Tab*>*)tabs;
- (void)reloadTabs;
- (void)reloadTab:(Tab*)tab;
- (void)selectTab:(Tab*)tab;

@property (nonatomic, copy) NSArray<Tab*>* tabs;
@property (nonatomic, assign, getter=isExpanded) BOOL expanded;
@property (nonatomic, assign) BOOL draws_background;
@property (nonatomic, copy) void (^on_select_tab)(Tab*);
@property (nonatomic, copy) void (^on_close_tab)(Tab*);
@property (nonatomic, copy) void (^on_close_other_tabs)(Tab*);
@property (nonatomic, copy) void (^on_move_tab_to_new_window)(Tab*);
@property (nonatomic, copy) void (^on_new_tab)(void);
@property (nonatomic, copy) void (^on_toggle_tab_audio_state)(Tab*);

@end
