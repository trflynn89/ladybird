/*
 * Copyright (c) 2023-2026, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Optional.h>
#include <AK/StringView.h>
#include <LibURL/URL.h>
#include <LibWeb/HTML/ActivateTab.h>
#include <LibWebView/PrivateBrowsing.h>

#import <Cocoa/Cocoa.h>

@class BrowserWindow;
@class BrowserWindowController;

class TabLocation {
private:
    enum class Kind {
        End,
        AfterTab,
    };

public:
    static TabLocation end() { return { Kind::End, nil }; }
    static TabLocation after_tab(BrowserWindow* _Nullable tab) { return { Kind::AfterTab, tab }; }

    bool is_after_tab() const { return m_kind == Kind::AfterTab; }
    BrowserWindow* _Nullable tab() const { return m_tab; }

private:
    TabLocation(Kind kind, BrowserWindow* _Nullable tab)
        : m_kind(kind)
        , m_tab(tab)
    {
    }

    Kind m_kind;
    BrowserWindow* _Nullable m_tab { nil };
};

@interface ApplicationDelegate : NSObject <NSApplicationDelegate>

- (nullable instancetype)init;

- (nonnull BrowserWindowController*)createNewTab:(Web::HTML::ActivateTab)activate_tab
                                         fromTab:(nullable BrowserWindow*)tab;

- (nonnull BrowserWindowController*)createNewTab:(Optional<URL::URL> const&)url
                                         fromTab:(nullable BrowserWindow*)tab
                                       isPrivate:(WebView::IsPrivate)is_private
                                     activateTab:(Web::HTML::ActivateTab)activate_tab
                                     tabLocation:(TabLocation)tab_location;

- (nonnull BrowserWindowController*)createChildTab:(Optional<URL::URL> const&)url
                                           fromTab:(nonnull BrowserWindow*)tab
                                       activateTab:(Web::HTML::ActivateTab)activate_tab
                                         pageIndex:(u64)page_index;

- (void)setActiveTab:(nonnull BrowserWindow*)tab;
- (nullable BrowserWindow*)activeTab;

- (void)removeTab:(nonnull BrowserWindowController*)controller;
- (NSUInteger)tabCount;

- (void)restartPrivateBrowsingSession;

- (void)rebuildBookmarksMenu;

- (void)onDevtoolsEnabled;
- (void)onDevtoolsDisabled;

@end
