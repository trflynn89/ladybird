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
    virtual void show_bookmarks_bar_changed() override;

    __weak ApplicationDelegate* m_delegate { nil };
};

@interface ApplicationDelegate ()
{
    OwnPtr<ApplicationSettingsObserver> m_settings_observer;
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

@end

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

    [self initializeBrowserWindowController:controller
                                activateTab:activate_tab
                                    fromTab:tab];

    return controller.selected_tab;
}

- (Tab*)createNewTab:(Optional<URL::URL> const&)url
             fromTab:(Tab*)tab
           isPrivate:(WebView::IsPrivate)is_private
         activateTab:(Web::HTML::ActivateTab)activate_tab
         tabLocation:(TabLocation)tab_location
{
    auto* controller = [[BrowserWindowController alloc] init:is_private];

    [self initializeBrowserWindowController:controller
                                activateTab:activate_tab
                                    fromTab:tab
                                tabLocation:tab_location];

    if (url.has_value()) {
        [controller.selected_tab loadURL:*url];

        if (*url != WebView::Application::settings().new_tab_page_url())
            [controller focusWebView];
    }

    return controller.selected_tab;
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
    [self initializeBrowserWindowController:controller
                                activateTab:activate_tab
                                    fromTab:tab];

    return controller.selected_tab;
}

- (void)initializeBrowserWindowController:(BrowserWindowController*)controller
                              activateTab:(Web::HTML::ActivateTab)activate_tab
                                  fromTab:(nullable Tab*)tab
{
    [self initializeBrowserWindowController:controller
                                activateTab:activate_tab
                                    fromTab:tab
                                tabLocation:TabLocation::end()];
}

- (void)initializeBrowserWindowController:(BrowserWindowController*)controller
                              activateTab:(Web::HTML::ActivateTab)activate_tab
                                  fromTab:(nullable Tab*)tab
                              tabLocation:(TabLocation)tab_location
{
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
