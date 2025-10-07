/*
 * Copyright (c) 2025, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#import <Application/ApplicationDelegate.h>
#import <Interface/InfoBar.h>
#import <Interface/Tab.h>

#if !__has_feature(objc_arc)
#    error "This project requires ARC"
#endif

static constexpr CGFloat const INFO_BAR_HEIGHT = 40;

@interface InfoBar ()
{
    URL::URL m_help_link;
}

@property (nonatomic, strong) NSTextField* text_label;
@property (nonatomic, strong) NSButton* help_button;
@property (nonatomic, strong) NSButton* dismiss_button;
@property (nonatomic, copy) InfoBarDismissed on_dismissed;

@end

@implementation InfoBar

- (instancetype)init
{
    if (self = [super init]) {
        self.text_label = [NSTextField labelWithString:@""];

        self.help_button = [NSButton buttonWithTitle:@""
                                              target:self
                                              action:@selector(openHelpLink:)];
        [self.help_button setBezelStyle:NSBezelStyleHelpButton];
        [self.help_button setToolTip:@"Help"];

        self.dismiss_button = [NSButton buttonWithTitle:@""
                                                 target:self
                                                 action:@selector(dismiss:)];
        [self.dismiss_button setBezelStyle:NSBezelStyleAccessoryBarAction];

        [self addView:self.text_label inGravity:NSStackViewGravityLeading];
        [self addView:self.help_button inGravity:NSStackViewGravityLeading];
        [self addView:self.dismiss_button inGravity:NSStackViewGravityTrailing];

        [self setOrientation:NSUserInterfaceLayoutOrientationHorizontal];
        [self setEdgeInsets:NSEdgeInsets { 0, 24, 0, 24 }];

        [[self heightAnchor] constraintEqualToConstant:INFO_BAR_HEIGHT].active = YES;
    }

    return self;
}

- (void)showWithMessage:(NSString*)message
                helpLink:(URL::URL)help_link
      dismissButtonTitle:(NSString*)title
    dismissButtonClicked:(InfoBarDismissed)on_dismissed
               activeTab:(Tab*)tab
{
    [self.text_label setStringValue:message];
    m_help_link = move(help_link);

    self.dismiss_button.title = title;
    self.on_dismissed = on_dismissed;

    if (tab) {
        [self attachToTab:tab];
    }

    [self setHidden:NO];
}

- (void)dismiss:(id)sender
{
    if (self.on_dismissed) {
        self.on_dismissed();
    }

    [self hide];
}

- (void)openHelpLink:(id)sender
{
    ApplicationDelegate* delegate = [NSApp delegate];

    [delegate createNewTab:m_help_link
                   fromTab:[delegate activeTab]
               activateTab:Web::HTML::ActivateTab::Yes];
}

- (void)hide
{
    [self removeFromSuperview];
    [self setHidden:YES];
}

- (void)tabBecameActive:(Tab*)tab
{
    if (![self isHidden]) {
        [self attachToTab:tab];
    }
}

- (void)attachToTab:(Tab*)tab
{
    [self removeFromSuperview];

    [tab.contentView addView:self inGravity:NSStackViewGravityTrailing];
    [[self leadingAnchor] constraintEqualToAnchor:[tab.contentView leadingAnchor]].active = YES;
}

@end
