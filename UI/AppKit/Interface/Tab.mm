/*
 * Copyright (c) 2023-2026, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/OwnPtr.h>
#include <AK/String.h>
#include <LibCore/Resource.h>
#include <LibWebView/Application.h>
#include <LibWebView/Settings.h>
#include <LibWebView/ViewImplementation.h>
#include <LibWebView/WebContentClient.h>

#import <Application/ApplicationDelegate.h>
#import <Interface/BookmarksBar.h>
#import <Interface/BrowserWindowController.h>
#import <Interface/LadybirdWebView.h>
#import <Interface/SearchPanel.h>
#import <Interface/Tab.h>
#import <Utilities/Conversions.h>

#if !__has_feature(objc_arc)
#    error "This project requires ARC"
#endif

NSNotificationName const TabTitleDidChangeNotification = @"TabTitleDidChangeNotification";
NSNotificationName const TabFaviconDidChangeNotification = @"TabFaviconDidChangeNotification";
NSNotificationName const TabAudioStateDidChangeNotification = @"TabAudioStateDidChangeNotification";

static constexpr CGFloat const TAB_ICON_SIZE = 16;
static constexpr NSUInteger const TAB_LOADING_SPINNER_SEGMENT_COUNT = 12;

class TabSettingsObserver final : public WebView::SettingsObserver {
public:
    explicit TabSettingsObserver(Tab* tab)
        : m_tab(tab)
    {
    }

private:
    virtual void config_variable_changed(WebView::ConfigVariableID) override;

    __weak Tab* m_tab { nil };
};

static NSImage* tab_loading_spinner_icon(NSUInteger frame)
{
    return [NSImage imageWithSize:NSMakeSize(TAB_ICON_SIZE, TAB_ICON_SIZE)
                          flipped:NO
                   drawingHandler:^BOOL(NSRect) {
                       auto* context = [NSGraphicsContext currentContext].CGContext;
                       auto* color = [NSColor labelColor];
                       static constexpr CGFloat radians_per_segment = 2.0 * 3.14159265358979323846 / TAB_LOADING_SPINNER_SEGMENT_COUNT;

                       CGContextSaveGState(context);
                       CGContextTranslateCTM(context, TAB_ICON_SIZE / 2.0, TAB_ICON_SIZE / 2.0);
                       for (NSUInteger segment = 0; segment < TAB_LOADING_SPINNER_SEGMENT_COUNT; ++segment) {
                           auto alpha = static_cast<CGFloat>(((segment + frame % TAB_LOADING_SPINNER_SEGMENT_COUNT) % TAB_LOADING_SPINNER_SEGMENT_COUNT) + 1) / TAB_LOADING_SPINNER_SEGMENT_COUNT;
                           auto* segment_color = [color colorWithAlphaComponent:alpha];
                           CGContextSaveGState(context);
                           CGContextRotateCTM(context, static_cast<CGFloat>(segment) * radians_per_segment);
                           CGContextSetStrokeColorWithColor(context, segment_color.CGColor);
                           CGContextSetLineWidth(context, 2);
                           CGContextSetLineCap(context, kCGLineCapRound);
                           CGContextMoveToPoint(context, 0, -4);
                           CGContextAddLineToPoint(context, 0, -7);
                           CGContextStrokePath(context);
                           CGContextRestoreGState(context);
                       }
                       CGContextRestoreGState(context);
                       return YES;
                   }];
}

@interface Tab () <LadybirdWebViewObserver>
{
    BOOL m_loading;
    NSUInteger m_loading_spinner_frame;
    __strong NSTimer* m_loading_spinner_timer;
    OwnPtr<TabSettingsObserver> m_settings_observer;
}

@property (nonatomic, strong, readwrite) LadybirdWebView* web_view;
@property (nonatomic, strong) NSString* page_title;
@property (nonatomic, strong) NSImage* favicon;
@property (nonatomic, strong) SearchPanel* search_panel;

@end

@implementation Tab

+ (NSImage*)defaultFavicon
{
    static NSImage* default_favicon;
    static dispatch_once_t token;
    dispatch_once(&token, ^{
        auto resource = MUST(Core::Resource::load_from_uri("resource://icons/48x48/app-browser.png"sv));
        default_favicon = [[NSImage alloc] initWithContentsOfFile:Ladybird::string_to_ns_string(resource->filesystem_path())];
    });
    return default_favicon;
}

- (instancetype)init:(WebView::IsPrivate)is_private
{
    if (self = [super initWithNibName:nil bundle:nil]) {
        self.web_view = [[LadybirdWebView alloc] init:self isPrivate:is_private];
        [self initializeTab];
    }
    return self;
}

- (instancetype)initAsChild:(Tab*)parent pageIndex:(u64)page_index
{
    if (self = [super initWithNibName:nil bundle:nil]) {
        self.web_view = [[LadybirdWebView alloc] initAsChild:self parent:parent.web_view pageIndex:page_index];
        [self initializeTab];
    }
    return self;
}

- (void)initializeTab
{
    [self.web_view setClipsToBounds:YES];
    self.favicon = [Tab defaultFavicon];
    self.page_title = @"New Tab";
    m_settings_observer = make<TabSettingsObserver>(self);
}

#pragma mark - NSViewController

- (void)loadView
{
    self.search_panel = [[SearchPanel alloc] init];
    [self.search_panel setHidden:YES];

    auto* stack_view = [NSStackView stackViewWithViews:@[ self.search_panel, self.web_view ]];
    [stack_view setOrientation:NSUserInterfaceLayoutOrientationVertical];
    [stack_view setSpacing:0];
    self.view = stack_view;
    [[self.search_panel leadingAnchor] constraintEqualToAnchor:[stack_view leadingAnchor]].active = YES;
}

- (void)dealloc
{
    [m_loading_spinner_timer invalidate];
}

#pragma mark - Public methods

- (WebView::IsPrivate)isPrivate
{
    return self.web_view.view.is_private();
}

- (void)loadURL:(URL::URL const&)url
{
    [self.web_view loadURL:url];
}

- (void)prepareForPresentation
{
    [self.web_view handleResize];
    [self.web_view handleDevicePixelRatioChange];
    [self.web_view handleDisplayRefreshRateChange];
}

- (void)find:(id)sender
{
    [self.search_panel find:sender];
}

- (void)findNextMatch:(id)sender
{
    [self.search_panel findNextMatch:sender];
}

- (void)findPreviousMatch:(id)sender
{
    [self.search_panel findPreviousMatch:sender];
}

- (void)useSelectionForFind:(id)sender
{
    [self.search_panel useSelectionForFind:sender];
}

- (BrowserWindowController*)browserWindowController
{
    return (BrowserWindowController*)self.view.window.windowController;
}

- (BookmarksBar*)bookmarksBar
{
    return [[self browserWindowController] bookmarksBar];
}

- (NSImage*)tabIcon
{
    return m_loading ? tab_loading_spinner_icon(m_loading_spinner_frame) : self.favicon;
}

- (NSString*)displayTitle
{
    if (!WebView::Application::settings().config_variable_as_bool(WebView::ConfigVariableID::ShowWebContentProcessIDInTabTitle))
        return self.page_title;
    auto title = MUST(String::formatted("{} [{}]", Ladybird::ns_string_to_string(self.page_title), self.web_view.view.client().pid()));
    return Ladybird::string_to_ns_string(title);
}

- (NSString*)windowTitle
{
    if ([self isPrivate] == WebView::IsPrivate::Yes)
        return [NSString stringWithFormat:@"%@ (Private Browsing)", self.page_title];
    return self.page_title;
}

- (void)notifyTitleChanged
{
    [[NSNotificationCenter defaultCenter] postNotificationName:TabTitleDidChangeNotification object:self];
}

- (void)notifyFaviconChanged
{
    [[NSNotificationCenter defaultCenter] postNotificationName:TabFaviconDidChangeNotification object:self];
}

- (void)updateLoadingSpinner
{
    if (!m_loading)
        return;
    m_loading_spinner_frame = (m_loading_spinner_frame + 1) % TAB_LOADING_SPINNER_SEGMENT_COUNT;
    [self notifyFaviconChanged];
}

- (void)setTabLoading:(BOOL)loading
{
    if (m_loading == loading)
        return;
    m_loading = loading;
    m_loading_spinner_frame = 0;
    if (loading) {
        __weak Tab* weak_self = self;
        m_loading_spinner_timer = [NSTimer timerWithTimeInterval:0.08
                                                         repeats:YES
                                                           block:^(NSTimer*) {
                                                               [weak_self updateLoadingSpinner];
                                                           }];
        [[NSRunLoop mainRunLoop] addTimer:m_loading_spinner_timer forMode:NSRunLoopCommonModes];
    } else {
        [m_loading_spinner_timer invalidate];
        m_loading_spinner_timer = nil;
    }
    [self notifyFaviconChanged];
}

- (void)togglePageMuteState:(id)sender
{
    self.web_view.view.toggle_page_mute_state();
    [[NSNotificationCenter defaultCenter] postNotificationName:TabAudioStateDidChangeNotification object:self];
}

- (NSImage*)iconForPageMuteState
{
    if (self.web_view.view.page_mute_state() == Web::HTML::MuteState::Muted)
        return [NSImage imageNamed:NSImageNameTouchBarAudioOutputVolumeOffTemplate];
    return [NSImage imageNamed:NSImageNameTouchBarAudioOutputVolumeHighTemplate];
}

- (NSString*)toolTipForPageMuteState
{
    return self.web_view.view.page_mute_state() == Web::HTML::MuteState::Muted ? @"Unmute tab" : @"Mute tab";
}

#pragma mark - LadybirdWebViewObserver

- (String const&)onCreateNewTab:(Optional<URL::URL> const&)url activateTab:(Web::HTML::ActivateTab)activate_tab
{
    auto* delegate = (ApplicationDelegate*)NSApp.delegate;
    auto* tab = [delegate createNewTab:url fromTab:self isPrivate:[self isPrivate] activateTab:activate_tab tabLocation:TabLocation::end()];
    return tab.web_view.handle;
}

- (String const&)onCreateChildTab:(Optional<URL::URL> const&)url activateTab:(Web::HTML::ActivateTab)activate_tab pageIndex:(u64)page_index
{
    auto* delegate = (ApplicationDelegate*)NSApp.delegate;
    auto* tab = [delegate createChildTab:url fromTab:self activateTab:activate_tab pageIndex:page_index];
    return tab.web_view.handle;
}

- (void)onWebViewActivated
{
    [[self browserWindowController] selectTab:self];
    [self.view.window orderFront:nil];
}

- (void)onWebViewClosed
{
    [[self browserWindowController] closeTab:self];
}

- (void)onLoadStart
{
    [self setTabLoading:YES];
    [self.toolbar_controller onLoadStart];
}

- (void)onLoadFinish
{
    [self setTabLoading:NO];
    [self.toolbar_controller onLoadFinish];
}

- (void)onURLChange:(URL::URL const&)url
{
    [self.toolbar_controller onURLChange:url];
}

- (void)onTitleChange:(Utf16String const&)title
{
    self.page_title = Ladybird::utf16_string_to_ns_string(title);
    [self notifyTitleChanged];
}

- (void)onFaviconChange:(Optional<Gfx::Bitmap const&>)bitmap
{
    NSImage* favicon = nil;
    if (bitmap.has_value()) {
        favicon = Ladybird::gfx_bitmap_to_ns_image(*bitmap);
        [favicon setResizingMode:NSImageResizingModeStretch];
        self.favicon = favicon;
    } else {
        self.favicon = [Tab defaultFavicon];
    }
    [self notifyFaviconChanged];
    [self.toolbar_controller onFaviconChange:favicon];
}

- (void)onAudioPlayStateChange:(Web::HTML::AudioPlayState)play_state
{
    [[NSNotificationCenter defaultCenter] postNotificationName:TabAudioStateDidChangeNotification object:self];
}

- (void)onEnterFullscreenWindow
{
    [[self browserWindowController] onEnterFullscreenWindow];
}

- (void)onExitFullscreenWindow
{
    [[self browserWindowController] onExitFullscreenWindow];
}

- (void)onFindInPageResult:(size_t)current_match_index totalMatchCount:(Optional<size_t> const&)total_match_count
{
    [self.search_panel onFindInPageResult:current_match_index totalMatchCount:total_match_count];
}

@end

void TabSettingsObserver::config_variable_changed(WebView::ConfigVariableID variable)
{
    if (variable == WebView::ConfigVariableID::ShowWebContentProcessIDInTabTitle)
        [m_tab notifyTitleChanged];
}
