/*
 * Copyright (c) 2023-2026, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Function.h>
#include <AK/Types.h>
#include <LibURL/Forward.h>
#include <LibWebView/PrivateBrowsing.h>

#import <Cocoa/Cocoa.h>

@class LadybirdWebView;
@class BookmarksBar;
@class BrowserWindowController;

extern NSNotificationName const TabTitleDidChangeNotification;
extern NSNotificationName const TabFaviconDidChangeNotification;
extern NSNotificationName const TabAudioStateDidChangeNotification;

@interface Tab : NSViewController

- (instancetype)init:(WebView::IsPrivate)is_private;
- (instancetype)initAsChild:(Tab*)parent
                  pageIndex:(u64)page_index;

- (WebView::IsPrivate)isPrivate;
- (NSString*)displayTitle;
- (NSString*)windowTitle;
- (NSImage*)tabIcon;
- (NSImage*)iconForPageMuteState;
- (NSString*)toolTipForPageMuteState;
- (void)togglePageMuteState:(id)sender;

- (void)loadURL:(URL::URL const&)url;
- (void)prepareForPresentation;
- (BookmarksBar*)bookmarksBar;
- (BOOL)needsBeforeUnloadCheck;
- (void)requestClose;
- (Function<void()>)prepareForImmediateClose;

@property (nonatomic, assign) BOOL already_requested_close;

@property (nonatomic, strong, readonly) LadybirdWebView* web_view;
@property (nonatomic, weak) NSResponder* preferred_first_responder;
@property (nonatomic, strong) NSToolbar* toolbar;
@property (nonatomic, strong) BrowserWindowController* toolbar_controller;
@property (nonatomic, weak) BrowserWindowController* browser_window_controller;

@end
