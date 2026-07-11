/*
 * Copyright (c) 2023-2026, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/OwnPtr.h>
#include <LibWebView/Application.h>
#include <LibWebView/Settings.h>

#import <Application/Application.h>
#import <Application/ApplicationDelegate.h>
#import <Interface/BrowserWindow.h>
#import <Interface/BrowserWindowController.h>
#import <Interface/InfoBar.h>
#import <Interface/LadybirdWebView.h>
#import <Interface/Menu.h>
#import <Interface/Tab.h>
#import <Utilities/Conversions.h>

#if !__has_feature(objc_arc)
#    error "This project requires ARC"
#endif

class ApplicationSettingsObserver final : public WebView::SettingsObserver {
public:
    explicit ApplicationSettingsObserver(ApplicationDelegate* delegate)
        : m_delegate(delegate)
    {
    }

private:
    virtual void tab_settings_changed() override;
    virtual void show_bookmarks_bar_changed() override;

    __weak ApplicationDelegate* m_delegate { nil };
};

@interface ApplicationDelegate ()
{
    OwnPtr<ApplicationSettingsObserver> m_settings_observer;
    bool m_vertical_tabs_enabled;
}

@property (nonatomic, strong) NSMutableArray<BrowserWindowController*>* managed_tabs;
@property (nonatomic, weak) Tab* active_tab;

@property (nonatomic, strong) NSMenu* bookmarks_menu;

@property (nonatomic, strong) InfoBar* info_bar;

- (NSMenuItem*)createApplicationMenu;
- (NSMenuItem*)createFileMenu;
- (NSMenuItem*)createEditMenu;
- (NSMenuItem*)createViewMenu;
- (NSMenuItem*)createHistoryMenu;
- (NSMenuItem*)createBookmarksMenu;
- (NSMenuItem*)createInspectMenu;
- (NSMenuItem*)createDebugMenu;
- (NSMenuItem*)createWindowMenu;
- (NSMenuItem*)createHelpMenu;
- (void)tabSettingsChanged;
- (void)convertWindowsToVerticalTabs;
- (void)convertWindowsToHorizontalTabs;
- (Tab*)initializeBrowserWindowController:(BrowserWindowController*)controller
                               activateTab:(Web::HTML::ActivateTab)activate_tab
                                   fromTab:(nullable Tab*)tab;
- (Tab*)initializeBrowserWindowController:(BrowserWindowController*)controller
                               activateTab:(Web::HTML::ActivateTab)activate_tab
                                   fromTab:(nullable Tab*)tab
                               tabLocation:(TabLocation)tab_location;

@end

void ApplicationSettingsObserver::tab_settings_changed()
{
    [m_delegate tabSettingsChanged];
}

void ApplicationSettingsObserver::show_bookmarks_bar_changed()
{
    auto show_bookmarks_bar = WebView::Application::settings().show_bookmarks_bar();
    for (BrowserWindowController* controller in m_delegate.managed_tabs)
        [controller updateBookmarksBarDisplay:show_bookmarks_bar];
}

@implementation ApplicationDelegate

- (instancetype)init
{
    if (self = [super init]) {
        [NSApp setMainMenu:[[NSMenu alloc] init]];

        [[NSApp mainMenu] addItem:[self createApplicationMenu]];
        [[NSApp mainMenu] addItem:[self createFileMenu]];
        [[NSApp mainMenu] addItem:[self createEditMenu]];
        [[NSApp mainMenu] addItem:[self createViewMenu]];
        [[NSApp mainMenu] addItem:[self createHistoryMenu]];
        [[NSApp mainMenu] addItem:[self createBookmarksMenu]];
        [[NSApp mainMenu] addItem:[self createInspectMenu]];
        [[NSApp mainMenu] addItem:[self createDebugMenu]];
        [[NSApp mainMenu] addItem:[self createWindowMenu]];
        [[NSApp mainMenu] addItem:[self createHelpMenu]];

        self.managed_tabs = [[NSMutableArray alloc] init];
        m_vertical_tabs_enabled = WebView::Application::settings().tab_settings().vertical_tabs_enabled;
        m_settings_observer = make<ApplicationSettingsObserver>(self);

        // Reduce the tooltip delay, as the default delay feels quite long.
        [[NSUserDefaults standardUserDefaults] setObject:@100 forKey:@"NSInitialToolTipDelay"];
    }

    return self;
}

#pragma mark - Public methods

- (nonnull Tab*)createNewTab:(Web::HTML::ActivateTab)activate_tab
                     fromTab:(nullable Tab*)tab
{
    auto is_private = tab ? [tab isPrivate] : WebView::IsPrivate::No;
    auto* controller = [[BrowserWindowController alloc] init:is_private];

    return [self initializeBrowserWindowController:controller
                                       activateTab:activate_tab
                                           fromTab:tab];
}

- (Tab*)createNewTab:(Optional<URL::URL> const&)url
             fromTab:(Tab*)tab
           isPrivate:(WebView::IsPrivate)is_private
         activateTab:(Web::HTML::ActivateTab)activate_tab
         tabLocation:(TabLocation)tab_location
{
    auto* controller = [[BrowserWindowController alloc] init:is_private];

    auto* new_tab = [self initializeBrowserWindowController:controller
                                                activateTab:activate_tab
                                                    fromTab:tab
                                                tabLocation:tab_location];

    if (url.has_value()) {
        [new_tab loadURL:*url];

        if (*url != WebView::Application::settings().new_tab_page_url())
            [new_tab.toolbar_controller focusWebView];
    }

    return new_tab;
}

- (nonnull Tab*)createChildTab:(Optional<URL::URL> const&)url
                       fromTab:(nonnull Tab*)tab
                   activateTab:(Web::HTML::ActivateTab)activate_tab
                     pageIndex:(u64)page_index
{
    auto* child_tab = [self createChildTab:activate_tab fromTab:tab pageIndex:page_index];

    if (url.has_value()) {
        [child_tab loadURL:*url];
    }

    [child_tab.toolbar_controller focusWebView];

    return child_tab;
}

- (void)setActiveTab:(Tab*)tab
{
    if (tab == self.activeTab)
        return;

    self.active_tab = tab;

    if (self.info_bar) {
        [self.info_bar tabBecameActive:self.active_tab];
    }

    WebView::Application::the().update_bookmark_action_for_current_web_view();
}

- (Tab*)activeTab
{
    return self.active_tab;
}

- (void)removeTab:(BrowserWindowController*)controller
{
    [self.managed_tabs removeObject:controller];
}

- (void)moveTabToNewWindow:(Tab*)tab
{
    auto* source = tab.browser_window_controller;
    if (!source.isVerticalTabsPresentation || source.tabs.count <= 1)
        return;

    [source removeTab:tab];

    auto* destination = [[BrowserWindowController alloc] initWithTab:tab];
    [destination showWindow:nil];
    [destination presentVerticalTabs:YES];
    [self.managed_tabs addObject:destination];
    [destination.window makeKeyAndOrderFront:nil];
}

- (NSUInteger)tabCount
{
    NSUInteger count = 0;
    for (BrowserWindowController* controller in self.managed_tabs)
        count += controller.tabs.count;
    return count;
}

- (void)restartPrivateBrowsingSession
{
    for (BrowserWindowController* controller in [self.managed_tabs copy]) {
        if ([controller isPrivate] == WebView::IsPrivate::Yes)
            [[controller window] close];
    }

    WebView::Application::the().reset_private_browsing_session();
    [self openNewWindow:WebView::IsPrivate::Yes];
}

- (void)rebuildBookmarksMenu
{
    Ladybird::repopulate_application_menu(self.bookmarks_menu, WebView::Application::the().bookmarks_menu());

    for (BrowserWindowController* controller in self.managed_tabs) {
        [controller rebuildBookmarksBar];
    }
}

- (void)onDevtoolsEnabled
{
    if (!self.info_bar) {
        self.info_bar = [[InfoBar alloc] init];
    }

    auto message = MUST(String::formatted("DevTools is enabled on port {}", WebView::Application::browser_options().devtools_port));

    [self.info_bar showWithMessage:Ladybird::string_to_ns_string(message)
                dismissButtonTitle:@"Disable"
              dismissButtonClicked:^{
                  MUST(WebView::Application::the().toggle_devtools_enabled());
              }
                         activeTab:self.active_tab];
}

- (void)onDevtoolsDisabled
{
    if (self.info_bar) {
        [self.info_bar hide];
        self.info_bar = nil;
    }
}

- (void)tabSettingsChanged
{
    auto vertical_tabs_enabled = WebView::Application::settings().tab_settings().vertical_tabs_enabled;
    if (exchange(m_vertical_tabs_enabled, vertical_tabs_enabled) == vertical_tabs_enabled) {
        for (BrowserWindowController* controller in self.managed_tabs)
            [controller applyTabSettings];
        return;
    }

    if (vertical_tabs_enabled)
        [self convertWindowsToVerticalTabs];
    else
        [self convertWindowsToHorizontalTabs];
}

- (void)convertWindowsToVerticalTabs
{
    auto* converted_controllers = [NSMutableSet<BrowserWindowController*> set];

    for (BrowserWindowController* candidate in [self.managed_tabs copy]) {
        if (candidate.isVerticalTabsPresentation || [converted_controllers containsObject:candidate])
            continue;

        auto* windows = candidate.window.tabbedWindows ?: @[ candidate.window ];
        auto* selected_window = candidate.window.tabGroup.selectedWindow ?: candidate.window;
        auto* survivor = (BrowserWindowController*)selected_window.windowController;
        auto* selected_tab = survivor.selected_tab;
        auto* ordered_tabs = [NSMutableArray<Tab*> array];

        for (NSWindow* window in windows) {
            auto* controller = (BrowserWindowController*)window.windowController;
            if (![controller isKindOfClass:BrowserWindowController.class])
                continue;
            [converted_controllers addObject:controller];
            [ordered_tabs addObjectsFromArray:controller.tabs];
        }

        for (NSWindow* window in windows) {
            auto* controller = (BrowserWindowController*)window.windowController;
            if (![controller isKindOfClass:BrowserWindowController.class])
                continue;
            for (Tab* tab in [controller.tabs copy])
                [controller detachTabForTransfer:tab];
            if (controller != survivor) {
                [window.tabGroup removeWindow:window];
                [controller closeShellAfterTransfer];
            }
        }

        for (Tab* tab in ordered_tabs)
            [survivor addTab:tab atIndex:survivor.tabs.count];

        [survivor presentVerticalTabs:YES];
        [survivor selectTab:selected_tab];
        [survivor.window makeKeyAndOrderFront:nil];
    }
}

- (void)convertWindowsToHorizontalTabs
{
    for (BrowserWindowController* survivor in [self.managed_tabs copy]) {
        if (!survivor.isVerticalTabsPresentation)
            continue;

        NSArray<Tab*>* ordered_tabs = [survivor.tabs copy];
        auto* selected_tab = survivor.selected_tab;
        for (Tab* tab in ordered_tabs)
            [survivor detachTabForTransfer:tab];

        [survivor addTab:selected_tab atIndex:0];
        [survivor presentVerticalTabs:NO];
        [survivor selectTab:selected_tab];

        auto* tab_group = survivor.window.tabGroup;
        NSUInteger insertion_index = 0;

        for (Tab* tab in ordered_tabs) {
            if (tab == selected_tab) {
                ++insertion_index;
                continue;
            }

            auto* controller = [[BrowserWindowController alloc] initWithTab:tab];
            [controller showWindow:nil];
            [self.managed_tabs addObject:controller];
            [tab_group insertWindow:controller.window atIndex:insertion_index++];
        }

        tab_group.selectedWindow = survivor.window;
        [survivor.window makeKeyAndOrderFront:nil];
    }
}

#pragma mark - Private methods

- (void)openLocation:(id)sender
{
    auto* current_tab = [NSApp keyWindow];

    if (![current_tab isKindOfClass:[BrowserWindow class]]) {
        return;
    }

    auto* controller = (BrowserWindowController*)[current_tab windowController];
    [controller focusLocationToolbarItem];
}

- (void)createNewWindow:(id)sender
{
    [self openNewWindow:WebView::IsPrivate::No];
}

- (void)createNewPrivateWindow:(id)sender
{
    [self openNewWindow:WebView::IsPrivate::Yes];
}

- (void)openNewWindow:(WebView::IsPrivate)is_private
{
    // FIXME: Create a new tab page specific to private windows.
    [self createNewTab:WebView::Application::settings().new_tab_page_url()
               fromTab:nil
             isPrivate:is_private
           activateTab:Web::HTML::ActivateTab::Yes
           tabLocation:TabLocation::end()];
}

- (nonnull Tab*)createChildTab:(Web::HTML::ActivateTab)activate_tab
                       fromTab:(nonnull Tab*)tab
                     pageIndex:(u64)page_index
{
    auto* controller = [[BrowserWindowController alloc] initAsChild:tab pageIndex:page_index];
    return [self initializeBrowserWindowController:controller
                                       activateTab:activate_tab
                                           fromTab:tab];
}

- (Tab*)initializeBrowserWindowController:(BrowserWindowController*)controller
                               activateTab:(Web::HTML::ActivateTab)activate_tab
                                   fromTab:(nullable Tab*)tab
{
    return [self initializeBrowserWindowController:controller
                                       activateTab:activate_tab
                                           fromTab:tab
                                       tabLocation:TabLocation::end()];
}

- (Tab*)initializeBrowserWindowController:(BrowserWindowController*)controller
                               activateTab:(Web::HTML::ActivateTab)activate_tab
                                   fromTab:(nullable Tab*)tab
                               tabLocation:(TabLocation)tab_location
{
    if (m_vertical_tabs_enabled) {
        // Resolve the destination window by searching the managed windows, as the destination tab
        // may not be its window's selected tab, in which case its view is not installed anywhere.
        BrowserWindowController* destination = nil;

        auto* destination_tab = tab_location.is_after_tab() ? tab_location.tab() : tab;
        if (destination_tab && [destination_tab isPrivate] == [controller isPrivate]) {
            for (BrowserWindowController* candidate in self.managed_tabs) {
                if ([candidate.tabs indexOfObjectIdenticalTo:destination_tab] != NSNotFound) {
                    destination = candidate;
                    break;
                }
            }
        }

        if (destination) {
            auto* new_tab = [controller createTabForHosting];
            auto insertion_index = destination.tabs.count;
            if (tab_location.is_after_tab()) {
                auto index = [destination.tabs indexOfObjectIdenticalTo:destination_tab];
                if (index != NSNotFound)
                    insertion_index = index + 1;
            }

            [destination addTab:new_tab atIndex:insertion_index];
            if (activate_tab == Web::HTML::ActivateTab::Yes) {
                [destination selectTab:new_tab];
                [destination.window makeKeyAndOrderFront:nil];
                [destination focusLocationToolbarItem];
            } else {
                [new_tab.toolbar_controller focusWebViewWhenActivated];
            }
            return new_tab;
        }

        [controller showWindow:nil];
        [controller presentVerticalTabs:YES];
        [self.managed_tabs addObject:controller];
        if (activate_tab == Web::HTML::ActivateTab::Yes)
            [controller.window orderFrontRegardless];
        return controller.selected_tab;
    }

    Optional<NSUInteger> insertion_index;
    NSWindowTabGroup* tab_group = nil;

    auto* tab_for_location = tab_location.is_after_tab() ? tab_location.tab() : tab;
    if (tab_for_location && [tab_for_location isPrivate] != [controller isPrivate])
        tab_for_location = nil;

    if (tab_for_location) {
        tab_group = tab_for_location.view.window.tabGroup;

        if (tab_location.is_after_tab()) {
            auto* windows = [tab_group windows];
            auto tab_index = [windows indexOfObject:tab_for_location.view.window];
            if (tab_index != NSNotFound)
                insertion_index = tab_index + 1;
        }
    }

    [controller showWindow:nil];

    if (tab_for_location) {
        if (insertion_index.has_value())
            [tab_group insertWindow:controller.window atIndex:insertion_index.value()];
        else
            [tab_group addWindow:controller.window];

        // FIXME: Can we create the tabbed window above without it becoming active in the first place?
        if (activate_tab == Web::HTML::ActivateTab::No) {
            [tab_for_location.view.window orderFront:nil];
        }
    }

    if (activate_tab == Web::HTML::ActivateTab::Yes) {
        [[controller window] orderFrontRegardless];
        [controller focusLocationToolbarItem];
    }

    [self.managed_tabs addObject:controller];
    return controller.selected_tab;
}

- (void)closeCurrentTab:(id)sender
{
    auto* current_window = [NSApp keyWindow];
    auto* controller = (BrowserWindowController*)current_window.windowController;
    if ([controller isKindOfClass:BrowserWindowController.class])
        [controller closeTab:controller.selected_tab];
}

- (NSMenuItem*)createApplicationMenu
{
    auto* menu = [[NSMenuItem alloc] init];

    auto* process_name = [[NSProcessInfo processInfo] processName];
    auto* submenu = [[NSMenu alloc] initWithTitle:process_name];

    [submenu addItem:Ladybird::create_application_menu_item(WebView::Application::the().open_about_page_action())];
    [submenu addItem:[NSMenuItem separatorItem]];

    [submenu addItem:Ladybird::create_application_menu_item(WebView::Application::the().open_settings_page_action())];
    [submenu addItem:[NSMenuItem separatorItem]];

    [submenu addItem:[[NSMenuItem alloc] initWithTitle:[NSString stringWithFormat:@"Hide %@", process_name]
                                                action:@selector(hide:)
                                         keyEquivalent:@"h"]];
    [submenu addItem:[NSMenuItem separatorItem]];

    [submenu addItem:[[NSMenuItem alloc] initWithTitle:[NSString stringWithFormat:@"Quit %@", process_name]
                                                action:@selector(terminate:)
                                         keyEquivalent:@"q"]];

    [menu setSubmenu:submenu];
    return menu;
}

- (NSMenuItem*)createFileMenu
{
    auto* menu = [[NSMenuItem alloc] init];
    auto* submenu = [[NSMenu alloc] initWithTitle:@"File"];

    [submenu addItem:[[NSMenuItem alloc] initWithTitle:@"New Window"
                                                action:@selector(createNewWindow:)
                                         keyEquivalent:@"n"]];
    [submenu addItem:[[NSMenuItem alloc] initWithTitle:@"New Private Window"
                                                action:@selector(createNewPrivateWindow:)
                                         keyEquivalent:@"N"]];
    [submenu addItem:[[NSMenuItem alloc] initWithTitle:@"New Tab"
                                                action:@selector(createNewTab:)
                                         keyEquivalent:@"t"]];
    [submenu addItem:[[NSMenuItem alloc] initWithTitle:@"Close Tab"
                                                action:@selector(closeCurrentTab:)
                                         keyEquivalent:@"w"]];
    [submenu addItem:[NSMenuItem separatorItem]];

    [submenu addItem:Ladybird::create_application_menu_item(WebView::Application::the().open_downloads_page_action())];
    [submenu addItem:[[NSMenuItem alloc] initWithTitle:@"Open Location"
                                                action:@selector(openLocation:)
                                         keyEquivalent:@"l"]];

    [menu setSubmenu:submenu];
    return menu;
}

- (NSMenuItem*)createEditMenu
{
    auto* menu = [[NSMenuItem alloc] init];
    auto* submenu = [[NSMenu alloc] initWithTitle:@"Edit"];

    [submenu addItem:[[NSMenuItem alloc] initWithTitle:@"Undo"
                                                action:@selector(undo:)
                                         keyEquivalent:@"z"]];
    [submenu addItem:[[NSMenuItem alloc] initWithTitle:@"Redo"
                                                action:@selector(redo:)
                                         keyEquivalent:@"y"]];
    [submenu addItem:[NSMenuItem separatorItem]];

    [submenu addItem:Ladybird::create_application_menu_item(WebView::Application::the().cut_selection_action())];
    [submenu addItem:Ladybird::create_application_menu_item(WebView::Application::the().copy_selection_action())];
    [submenu addItem:Ladybird::create_application_menu_item(WebView::Application::the().paste_action())];
    [submenu addItem:[NSMenuItem separatorItem]];

    [submenu addItem:Ladybird::create_application_menu_item(WebView::Application::the().select_all_action())];
    [submenu addItem:[NSMenuItem separatorItem]];

    [submenu addItem:[[NSMenuItem alloc] initWithTitle:@"Find..."
                                                action:@selector(find:)
                                         keyEquivalent:@"f"]];
    [submenu addItem:[[NSMenuItem alloc] initWithTitle:@"Find Next"
                                                action:@selector(findNextMatch:)
                                         keyEquivalent:@"g"]];
    [submenu addItem:[[NSMenuItem alloc] initWithTitle:@"Find Previous"
                                                action:@selector(findPreviousMatch:)
                                         keyEquivalent:@"G"]];
    [submenu addItem:[[NSMenuItem alloc] initWithTitle:@"Use Selection for Find"
                                                action:@selector(useSelectionForFind:)
                                         keyEquivalent:@"e"]];

    [menu setSubmenu:submenu];
    return menu;
}

- (NSMenuItem*)createViewMenu
{
    auto* menu = [[NSMenuItem alloc] init];
    auto* submenu = [[NSMenu alloc] initWithTitle:@"View"];

    [submenu addItem:Ladybird::create_application_menu_item(WebView::Application::the().reload_action())];
    [submenu addItem:[NSMenuItem separatorItem]];

    [submenu addItem:Ladybird::create_application_menu_item(WebView::Application::the().zoom_menu())];
    [submenu addItem:[NSMenuItem separatorItem]];
    [submenu addItem:Ladybird::create_application_menu_item(WebView::Application::the().color_scheme_menu())];
    [submenu addItem:Ladybird::create_application_menu_item(WebView::Application::the().contrast_menu())];
    [submenu addItem:Ladybird::create_application_menu_item(WebView::Application::the().motion_menu())];
    [submenu addItem:[NSMenuItem separatorItem]];

    [menu setSubmenu:submenu];
    return menu;
}

- (NSMenuItem*)createHistoryMenu
{
    return Ladybird::create_application_menu_item(WebView::Application::the().history_menu());
}

- (NSMenuItem*)createBookmarksMenu
{
    auto* menu = Ladybird::create_application_menu_item(WebView::Application::the().bookmarks_menu());
    self.bookmarks_menu = [menu submenu];
    return menu;
}

- (NSMenuItem*)createInspectMenu
{
    return Ladybird::create_application_menu_item(WebView::Application::the().inspect_menu());
}

- (NSMenuItem*)createDebugMenu
{
    return Ladybird::create_application_menu_item(WebView::Application::the().debug_menu());
}

- (NSMenuItem*)createWindowMenu
{
    auto* menu = [[NSMenuItem alloc] init];
    auto* submenu = [[NSMenu alloc] initWithTitle:@"Window"];

    auto* next_tab = [[NSMenuItem alloc] initWithTitle:@"Show Next Tab" action:@selector(selectNextTab:) keyEquivalent:@"\t"];
    next_tab.keyEquivalentModifierMask = NSEventModifierFlagControl;
    [submenu addItem:next_tab];

    auto* previous_tab = [[NSMenuItem alloc] initWithTitle:@"Show Previous Tab" action:@selector(selectPreviousTab:) keyEquivalent:@"\t"];
    previous_tab.keyEquivalentModifierMask = NSEventModifierFlagControl | NSEventModifierFlagShift;
    [submenu addItem:previous_tab];

    [NSApp setWindowsMenu:submenu];

    [menu setSubmenu:submenu];
    return menu;
}

- (NSMenuItem*)createHelpMenu
{
    auto* menu = [[NSMenuItem alloc] init];
    auto* submenu = [[NSMenu alloc] initWithTitle:@"Help"];

    [NSApp setHelpMenu:submenu];

    [menu setSubmenu:submenu];
    return menu;
}

#pragma mark - NSApplicationDelegate

- (void)applicationDidFinishLaunching:(NSNotification*)notification
{
    auto const& browser_options = WebView::Application::browser_options();

    if (browser_options.devtools_port.has_value())
        [self onDevtoolsEnabled];

    Tab* tab = nil;

    for (auto const& url : browser_options.urls) {
        auto activate_tab = tab == nil ? Web::HTML::ActivateTab::Yes : Web::HTML::ActivateTab::No;

        tab = [self createNewTab:url
                         fromTab:tab
                       isPrivate:WebView::IsPrivate::No
                     activateTab:activate_tab
                     tabLocation:TabLocation::end()];
    }
}

- (void)applicationWillTerminate:(NSNotification*)notification
{
}

- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication*)sender
{
    return [(Application*)sender confirmCancelActiveDownloads];
}

- (void)applicationDidChangeScreenParameters:(NSNotification*)notification
{
    for (BrowserWindowController* controller in self.managed_tabs) {
        [controller.selected_tab.web_view handleDisplayRefreshRateChange];
    }
}

- (BOOL)validateMenuItem:(NSMenuItem*)menu
{
    SEL action = [menu action];

    if (action == @selector(closeCurrentTab:)) {
        return [[NSApp keyWindow] isKindOfClass:[BrowserWindow class]];
    }

    return YES;
}

@end
