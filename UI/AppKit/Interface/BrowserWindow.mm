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

@interface BrowserWindow ()

@property (nonatomic, strong) NSString* saved_frame_name;
@property (nonatomic, assign) BOOL should_autosave_frame;
@property (nonatomic, assign) BOOL has_saved_frame;
@property (nonatomic, assign) NSRect saved_frame;

@end

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
        self.saved_frame_name = frame_autosave_name;
        if (frame_autosave_name != nil && [self setFrameUsingName:frame_autosave_name]) {
            self.saved_frame = self.frame;
            self.has_saved_frame = YES;
        }
        if (is_private == WebView::IsPrivate::No && frame_autosave_name != nil) {
            // Defer enabling autosave until the controller has installed all window chrome. That
            // setup temporarily changes the frame and would otherwise overwrite the saved value.
            self.should_autosave_frame = YES;
        } else {
            // Private windows adopt the saved frame after their chrome is installed without
            // persisting changes to it. Keep them and temporary-profile windows out of window
            // state restoration entirely.
            [self setRestorable:NO];
        }

        [self setTitleVisibility:NSWindowTitleHidden];
    }

    return self;
}

- (void)restoreSavedFrame
{
    if (self.saved_frame_name == nil)
        return;

    if (self.should_autosave_frame)
        self.frameAutosaveName = self.saved_frame_name;
    if (self.has_saved_frame)
        [self setFrame:self.saved_frame display:NO];
}

- (void)becomeKeyWindow
{
    [super becomeKeyWindow];

    if (self.preferred_first_responder && [self firstResponder] != self.preferred_first_responder)
        [self makeFirstResponder:self.preferred_first_responder];
}

- (BOOL)performKeyEquivalent:(NSEvent*)event
{
    auto modifiers = event.modifierFlags & NSEventModifierFlagDeviceIndependentFlagsMask;
    auto allowed_modifiers = NSEventModifierFlagControl | NSEventModifierFlagShift;
    if ([event.charactersIgnoringModifiers isEqualToString:@"\t"]
        && (modifiers & NSEventModifierFlagControl)
        && (modifiers & ~allowed_modifiers) == 0) {
        auto* controller = (BrowserWindowController*)self.windowController;
        if (controller.isPresentingVerticalTabs && controller.tabs.count > 1) {
            if (modifiers & NSEventModifierFlagShift)
                [controller selectPreviousTab:self];
            else
                [controller selectNextTab:self];
            return YES;
        }
    }

    return [super performKeyEquivalent:event];
}

- (void)selectNextTab:(id)sender
{
    auto* controller = (BrowserWindowController*)self.windowController;
    if (controller.isPresentingVerticalTabs) {
        [controller selectNextTab:sender];
        return;
    }
    [super selectNextTab:sender];
}

- (void)selectPreviousTab:(id)sender
{
    auto* controller = (BrowserWindowController*)self.windowController;
    if (controller.isPresentingVerticalTabs) {
        [controller selectPreviousTab:sender];
        return;
    }
    [super selectPreviousTab:sender];
}

- (BOOL)validateMenuItem:(NSMenuItem*)item
{
    if (item.tag == VERTICAL_TABS_NEXT_MENU_ITEM_TAG || item.tag == VERTICAL_TABS_PREVIOUS_MENU_ITEM_TAG) {
        auto* controller = (BrowserWindowController*)self.windowController;
        auto show_item = controller.isPresentingVerticalTabs && controller.tabs.count > 1;
        item.hidden = !show_item;
        return show_item;
    }
    return [super validateMenuItem:item];
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

@end
