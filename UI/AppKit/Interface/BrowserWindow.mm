/*
 * Copyright (c) 2023-2026, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/ByteString.h>
#include <LibWebView/Application.h>

#import <Interface/BrowserWindow.h>
#import <Interface/BrowserWindowController.h>
#import <Interface/LadybirdWebView.h>
#import <Interface/Tab.h>
#import <Utilities/Conversions.h>

#if !__has_feature(objc_arc)
#    error "This project requires ARC"
#endif

static constexpr CGFloat const WINDOW_WIDTH = 1000;
static constexpr CGFloat const WINDOW_HEIGHT = 800;

static NSString* window_frame_autosave_name()
{
    auto const& profile = WebView::Application::profile();
    if (profile.is_temporary())
        return nil;

    auto name = ByteString::formatted("window-{}", WebView::Profile::routing_identifier(profile.paths().identity));
    return Ladybird::string_to_ns_string(name);
}

@implementation BrowserWindow

- (instancetype)init:(WebView::IsPrivate)is_private
{
    auto screen_rect = [[NSScreen mainScreen] frame];
    auto position_x = (NSWidth(screen_rect) - WINDOW_WIDTH) / 2;
    auto position_y = (NSHeight(screen_rect) - WINDOW_HEIGHT) / 2;
    auto window_rect = NSMakeRect(position_x, position_y, WINDOW_WIDTH, WINDOW_HEIGHT);
    static constexpr auto style_mask = NSWindowStyleMaskTitled | NSWindowStyleMaskClosable | NSWindowStyleMaskMiniaturizable | NSWindowStyleMaskResizable;

    if (self = [super initWithContentRect:window_rect
                                styleMask:style_mask
                                  backing:NSBackingStoreBuffered
                                    defer:NO]) {
        [self setTabbingIdentifier:is_private == WebView::IsPrivate::Yes
                ? @"LadybirdPrivateBrowsing"
                : @"LadybirdBrowsing"];

        auto* frame_autosave_name = window_frame_autosave_name();
        if (is_private == WebView::IsPrivate::No && frame_autosave_name != nil) {
            // Remember last window position.
            self.frameAutosaveName = frame_autosave_name;
        } else {
            // Adopt the last saved frame without persisting changes to it, and keep private and temporary-profile
            // windows out of window state restoration entirely.
            if (frame_autosave_name != nil)
                [self setFrameUsingName:frame_autosave_name];
            [self setRestorable:NO];
        }

        [self setTitleVisibility:NSWindowTitleHidden];
    }

    return self;
}

- (void)becomeKeyWindow
{
    [super becomeKeyWindow];

    if (self.preferred_first_responder && [self firstResponder] != self.preferred_first_responder)
        [self makeFirstResponder:self.preferred_first_responder];
}

- (void)setIsVisible:(BOOL)visible
{
    auto* controller = (BrowserWindowController*)self.windowController;
    [controller.selected_tab.web_view handleVisibility:visible];
    [super setIsVisible:visible];
}

- (void)setIsMiniaturized:(BOOL)miniaturized
{
    auto* controller = (BrowserWindowController*)self.windowController;
    [controller.selected_tab.web_view handleVisibility:!miniaturized];
    [super setIsMiniaturized:miniaturized];
}

- (BOOL)performKeyEquivalent:(NSEvent*)event
{
    auto* controller = (BrowserWindowController*)self.windowController;
    if (controller.isVerticalTabsPresentation) {
        auto modifiers = event.modifierFlags & (NSEventModifierFlagCommand | NSEventModifierFlagControl | NSEventModifierFlagOption | NSEventModifierFlagShift);
        if (modifiers == (NSEventModifierFlagCommand | NSEventModifierFlagShift)) {
            auto* characters = event.charactersIgnoringModifiers;
            if (characters.length == 1) {
                switch ([characters characterAtIndex:0]) {
                case '[':
                case '{':
                    [controller selectPreviousTab];
                    return YES;
                case ']':
                case '}':
                    [controller selectNextTab];
                    return YES;
                }
            }
        }
    }

    return [super performKeyEquivalent:event];
}

- (void)selectNextTab:(id)sender
{
    auto* controller = (BrowserWindowController*)self.windowController;
    if (controller.isVerticalTabsPresentation)
        [controller selectNextTab];
    else
        [super selectNextTab:sender];
}

- (void)selectPreviousTab:(id)sender
{
    auto* controller = (BrowserWindowController*)self.windowController;
    if (controller.isVerticalTabsPresentation)
        [controller selectPreviousTab];
    else
        [super selectPreviousTab:sender];
}

- (BOOL)validateMenuItem:(NSMenuItem*)menu_item
{
    auto action = menu_item.action;
    if (action == @selector(selectNextTab:) || action == @selector(selectPreviousTab:)) {
        auto* controller = (BrowserWindowController*)self.windowController;
        if (controller.isVerticalTabsPresentation)
            return controller.tabs.count > 1;
        return self.tabbedWindows.count > 1;
    }
    return [super validateMenuItem:menu_item];
}

@end
