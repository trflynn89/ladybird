/*
 * Copyright (c) 2026, Timothy Flynn <trflynn89@pm.me>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibWebView/ViewImplementation.h>

#import <Interface/SidebarViewController.h>
#import <Interface/Tab.h>

#if !__has_feature(objc_arc)
#    error "This project requires ARC"
#endif

static constexpr CGFloat const SIDEBAR_ROW_HEIGHT = 34;
static constexpr CGFloat const SIDEBAR_ICON_SIZE = 16;
static constexpr CGFloat const SIDEBAR_ROW_BACKGROUND_HORIZONTAL_INSET = 4;
static constexpr CGFloat const SIDEBAR_ROW_BACKGROUND_VERTICAL_INSET = 2;
static constexpr CGFloat const SIDEBAR_ROW_BACKGROUND_CORNER_RADIUS = 6;
static constexpr CGFloat const SIDEBAR_ROW_HOVER_ALPHA = 0.08;
static constexpr CGFloat const SIDEBAR_ROW_SELECTION_ALPHA = 0.18;
static constexpr CGFloat const SIDEBAR_ROW_INACTIVE_SELECTION_ALPHA = 0.12;
static NSUserInterfaceItemIdentifier const SIDEBAR_TAB_CELL_IDENTIFIER = @"SidebarTabCell";

@interface SidebarTabCellView : NSTableCellView

- (void)configureWithTab:(Tab*)tab
                expanded:(BOOL)expanded
                 hovered:(BOOL)hovered;

@property (nonatomic, weak) Tab* tab;
@property (nonatomic, weak) SidebarViewController* sidebar;
@property (nonatomic, strong) NSImageView* favicon_view;
@property (nonatomic, strong) NSTextField* title_label;
@property (nonatomic, strong) NSButton* audio_button;
@property (nonatomic, strong) NSButton* close_button;
@property (nonatomic, strong) NSArray<NSLayoutConstraint*>* expanded_constraints;
@property (nonatomic, strong) NSArray<NSLayoutConstraint*>* collapsed_constraints;
@property (nonatomic, assign) BOOL expanded;
@property (nonatomic, assign) BOOL hovered;

@end

@implementation SidebarTabCellView

- (instancetype)init
{
    if (self = [super initWithFrame:NSZeroRect]) {
        self.favicon_view = [[NSImageView alloc] init];
        self.favicon_view.imageScaling = NSImageScaleProportionallyUpOrDown;
        self.favicon_view.translatesAutoresizingMaskIntoConstraints = NO;

        self.title_label = [NSTextField labelWithString:@""];
        self.title_label.lineBreakMode = NSLineBreakByTruncatingTail;
        self.title_label.maximumNumberOfLines = 1;
        self.title_label.translatesAutoresizingMaskIntoConstraints = NO;

        self.audio_button = [NSButton buttonWithImage:[NSImage imageWithSystemSymbolName:@"speaker.wave.2.fill" accessibilityDescription:@"Mute tab"]
                                               target:self
                                               action:@selector(toggleAudio:)];
        self.audio_button.bordered = NO;
        self.audio_button.hidden = YES;
        self.audio_button.translatesAutoresizingMaskIntoConstraints = NO;

        self.close_button = [NSButton buttonWithImage:[NSImage imageWithSystemSymbolName:@"xmark" accessibilityDescription:@"Close Tab"]
                                               target:self
                                               action:@selector(closeTab:)];
        self.close_button.bordered = NO;
        self.close_button.toolTip = @"Close Tab";
        self.close_button.translatesAutoresizingMaskIntoConstraints = NO;

        [self addSubview:self.favicon_view];
        [self addSubview:self.title_label];
        [self addSubview:self.audio_button];
        [self addSubview:self.close_button];

        [NSLayoutConstraint activateConstraints:@[
            [self.favicon_view.centerYAnchor constraintEqualToAnchor:self.centerYAnchor],
            [self.favicon_view.widthAnchor constraintEqualToConstant:SIDEBAR_ICON_SIZE],
            [self.favicon_view.heightAnchor constraintEqualToConstant:SIDEBAR_ICON_SIZE],
            [self.audio_button.widthAnchor constraintEqualToConstant:SIDEBAR_ICON_SIZE],
            [self.audio_button.heightAnchor constraintEqualToConstant:SIDEBAR_ICON_SIZE],
            [self.close_button.centerYAnchor constraintEqualToAnchor:self.centerYAnchor],
            [self.close_button.widthAnchor constraintEqualToConstant:SIDEBAR_ICON_SIZE],
            [self.close_button.heightAnchor constraintEqualToConstant:SIDEBAR_ICON_SIZE],
        ]];

        // The trailing chain must not be required, so that transient narrow layouts (e.g. while the
        // sidebar is being dismantled) break these constraints instead of raising an exception.
        auto* title_leading = [self.title_label.leadingAnchor constraintEqualToAnchor:self.favicon_view.trailingAnchor
                                                                             constant:8];
        title_leading.priority = NSLayoutPriorityDefaultHigh;
        auto* audio_trailing = [self.audio_button.trailingAnchor constraintEqualToAnchor:self.close_button.leadingAnchor
                                                                                constant:-4];
        audio_trailing.priority = NSLayoutPriorityDefaultHigh;
        auto* close_trailing = [self.close_button.trailingAnchor constraintEqualToAnchor:self.trailingAnchor
                                                                                constant:-8];
        close_trailing.priority = NSLayoutPriorityDefaultHigh;

        self.expanded_constraints = @[
            [self.favicon_view.leadingAnchor constraintEqualToAnchor:self.leadingAnchor
                                                            constant:8],
            title_leading,
            [self.title_label.centerYAnchor constraintEqualToAnchor:self.centerYAnchor],
            [self.title_label.trailingAnchor constraintLessThanOrEqualToAnchor:self.audio_button.leadingAnchor
                                                                      constant:-4],
            [self.audio_button.centerYAnchor constraintEqualToAnchor:self.centerYAnchor],
            audio_trailing,
            close_trailing,
        ];

        // Collapsed rows show a centered favicon only; the close button replaces the favicon on
        // hover, and the audio indicator becomes a badge at the favicon's top-right corner.
        self.collapsed_constraints = @[
            [self.favicon_view.centerXAnchor constraintEqualToAnchor:self.centerXAnchor],
            [self.close_button.centerXAnchor constraintEqualToAnchor:self.centerXAnchor],
            [self.audio_button.centerXAnchor constraintEqualToAnchor:self.favicon_view.trailingAnchor],
            [self.audio_button.centerYAnchor constraintEqualToAnchor:self.favicon_view.topAnchor],
        ];

        [self addTrackingArea:[[NSTrackingArea alloc] initWithRect:NSZeroRect
                                                           options:NSTrackingMouseEnteredAndExited | NSTrackingActiveInKeyWindow | NSTrackingInVisibleRect
                                                             owner:self
                                                          userInfo:nil]];
    }
    return self;
}

- (void)configureWithTab:(Tab*)tab
                expanded:(BOOL)expanded
                 hovered:(BOOL)hovered
{
    self.tab = tab;
    self.expanded = expanded;
    self.hovered = hovered;
    self.favicon_view.image = tab.tabIcon;
    self.title_label.stringValue = tab.displayTitle;
    self.title_label.hidden = !expanded;
    [NSLayoutConstraint deactivateConstraints:expanded ? self.collapsed_constraints : self.expanded_constraints];
    [NSLayoutConstraint activateConstraints:expanded ? self.expanded_constraints : self.collapsed_constraints];
    self.toolTip = tab.displayTitle;
    [self updateCloseButton];
}

- (void)mouseEntered:(NSEvent*)event
{
    self.hovered = YES;
    [self updateCloseButton];
}

- (void)mouseExited:(NSEvent*)event
{
    self.hovered = NO;
    [self updateCloseButton];
}

- (void)updateCloseButton
{
    self.close_button.hidden = !self.hovered;
    self.favicon_view.hidden = !self.expanded && self.hovered;
}

- (void)closeTab:(id)sender
{
    if (self.sidebar.on_close_tab)
        self.sidebar.on_close_tab(self.tab);
}

- (void)toggleAudio:(id)sender
{
    if (self.sidebar.on_toggle_tab_audio_state)
        self.sidebar.on_toggle_tab_audio_state(self.tab);
}

@end

@interface SidebarNewTabCellView : NSTableCellView

- (instancetype)initWithSidebar:(SidebarViewController*)sidebar expanded:(BOOL)expanded;

@property (nonatomic, weak) SidebarViewController* sidebar;

@end

@implementation SidebarNewTabCellView

- (instancetype)initWithSidebar:(SidebarViewController*)sidebar expanded:(BOOL)expanded
{
    if (self = [super initWithFrame:NSZeroRect]) {
        self.sidebar = sidebar;

        auto* image = [[NSImageView alloc] initWithFrame:NSZeroRect];
        image.image = [NSImage imageWithSystemSymbolName:@"plus" accessibilityDescription:@"New Tab"];
        image.translatesAutoresizingMaskIntoConstraints = NO;
        auto* label = [NSTextField labelWithString:@"New Tab"];
        label.hidden = !expanded;
        label.translatesAutoresizingMaskIntoConstraints = NO;
        auto* button = [NSButton buttonWithTitle:@"" target:self action:@selector(createNewTab:)];
        button.bordered = NO;
        button.toolTip = @"New Tab";
        button.translatesAutoresizingMaskIntoConstraints = NO;
        [button setAccessibilityLabel:@"New Tab"];

        [self addSubview:image];
        [self addSubview:label];
        [self addSubview:button];
        [NSLayoutConstraint activateConstraints:@[
            expanded
                ? [image.leadingAnchor constraintEqualToAnchor:self.leadingAnchor constant:8]
                : [image.centerXAnchor constraintEqualToAnchor:self.centerXAnchor],
            [image.centerYAnchor constraintEqualToAnchor:self.centerYAnchor],
            [image.widthAnchor constraintEqualToConstant:SIDEBAR_ICON_SIZE],
            [image.heightAnchor constraintEqualToConstant:SIDEBAR_ICON_SIZE],
            [button.leadingAnchor constraintEqualToAnchor:self.leadingAnchor],
            [button.trailingAnchor constraintEqualToAnchor:self.trailingAnchor],
            [button.topAnchor constraintEqualToAnchor:self.topAnchor],
            [button.bottomAnchor constraintEqualToAnchor:self.bottomAnchor],
        ]];
        if (expanded) {
            [NSLayoutConstraint activateConstraints:@[
                [label.leadingAnchor constraintEqualToAnchor:image.trailingAnchor
                                                    constant:8],
                [label.centerYAnchor constraintEqualToAnchor:self.centerYAnchor],
            ]];
        }
    }
    return self;
}

- (void)createNewTab:(id)sender
{
    if (self.sidebar.on_new_tab)
        self.sidebar.on_new_tab();
}

@end

@interface SidebarTableRowView : NSTableRowView
- (void)drawBackgroundWithAlpha:(CGFloat)alpha;
@property (nonatomic, assign) BOOL hovered;
@end

@implementation SidebarTableRowView

- (instancetype)initWithFrame:(NSRect)frame
{
    if (self = [super initWithFrame:frame]) {
        [self addTrackingArea:[[NSTrackingArea alloc] initWithRect:NSZeroRect
                                                           options:NSTrackingMouseEnteredAndExited | NSTrackingActiveInActiveApp | NSTrackingInVisibleRect
                                                             owner:self
                                                          userInfo:nil]];
    }
    return self;
}

- (void)mouseEntered:(NSEvent*)event
{
    self.hovered = YES;
    [self setNeedsDisplay:YES];
}

- (void)mouseExited:(NSEvent*)event
{
    self.hovered = NO;
    [self setNeedsDisplay:YES];
}

- (NSBackgroundStyle)interiorBackgroundStyle
{
    return NSBackgroundStyleNormal;
}

- (void)drawBackgroundInRect:(NSRect)dirty_rect
{
    [super drawBackgroundInRect:dirty_rect];
    if (!self.selected && self.hovered)
        [self drawBackgroundWithAlpha:SIDEBAR_ROW_HOVER_ALPHA];
}

- (void)drawSelectionInRect:(NSRect)dirty_rect
{
    [self drawBackgroundWithAlpha:self.emphasized ? SIDEBAR_ROW_SELECTION_ALPHA : SIDEBAR_ROW_INACTIVE_SELECTION_ALPHA];
}

- (void)drawBackgroundWithAlpha:(CGFloat)alpha
{
    auto background_rect = NSInsetRect(self.bounds, SIDEBAR_ROW_BACKGROUND_HORIZONTAL_INSET, SIDEBAR_ROW_BACKGROUND_VERTICAL_INSET);
    [[NSColor.controlAccentColor colorWithAlphaComponent:alpha] setFill];
    [[NSBezierPath bezierPathWithRoundedRect:background_rect
                                     xRadius:SIDEBAR_ROW_BACKGROUND_CORNER_RADIUS
                                     yRadius:SIDEBAR_ROW_BACKGROUND_CORNER_RADIUS]
        fill];
}

@end

@interface SidebarTableView : NSTableView
@property (nonatomic, copy) void (^on_middle_click)(NSInteger);
@property (nonatomic, copy) NSMenu* (^context_menu_for_row)(NSInteger);
@end

@implementation SidebarTableView

- (void)otherMouseUp:(NSEvent*)event
{
    if (event.buttonNumber == 2 && self.on_middle_click) {
        auto row = [self rowAtPoint:[self convertPoint:event.locationInWindow fromView:nil]];
        self.on_middle_click(row);
        return;
    }
    [super otherMouseUp:event];
}

- (NSMenu*)menuForEvent:(NSEvent*)event
{
    if (self.context_menu_for_row) {
        auto row = [self rowAtPoint:[self convertPoint:event.locationInWindow fromView:nil]];
        if (auto* menu = self.context_menu_for_row(row))
            return menu;
    }
    return [super menuForEvent:event];
}

@end

@interface SidebarViewController () <NSTableViewDataSource, NSTableViewDelegate>
{
    BOOL m_is_updating_selection;
}
@property (nonatomic, strong) SidebarTableView* table_view;
@property (nonatomic, weak) Tab* selected_tab;
- (NSMenu*)contextMenuForRow:(NSInteger)row;
@end

@implementation SidebarViewController

- (instancetype)initWithTabs:(NSArray<Tab*>*)tabs
{
    if (self = [super initWithNibName:nil bundle:nil]) {
        self.tabs = tabs;
        self.expanded = YES;
    }
    return self;
}

- (void)loadView
{
    auto* scroll_view = [[NSScrollView alloc] initWithFrame:NSZeroRect];
    scroll_view.drawsBackground = NO;
    scroll_view.hasVerticalScroller = YES;

    self.table_view = [[SidebarTableView alloc] initWithFrame:NSZeroRect];
    self.table_view.headerView = nil;
    self.table_view.backgroundColor = NSColor.clearColor;
    self.table_view.rowHeight = SIDEBAR_ROW_HEIGHT;
    self.table_view.intercellSpacing = NSZeroSize;
    self.table_view.style = NSTableViewStyleSourceList;
    self.table_view.dataSource = self;
    self.table_view.delegate = self;
    self.table_view.columnAutoresizingStyle = NSTableViewUniformColumnAutoresizingStyle;

    auto* column = [[NSTableColumn alloc] initWithIdentifier:@"SidebarTabColumn"];
    column.resizingMask = NSTableColumnAutoresizingMask;
    [self.table_view addTableColumn:column];
    scroll_view.documentView = self.table_view;

    // The sidebar extends behind the title bar when the window has a full height sidebar, so pin
    // the scroll view to the safe area to keep tab rows below the window controls.
    auto* container = [[NSView alloc] initWithFrame:NSZeroRect];
    scroll_view.translatesAutoresizingMaskIntoConstraints = NO;
    [container addSubview:scroll_view];
    [NSLayoutConstraint activateConstraints:@[
        [scroll_view.topAnchor constraintEqualToAnchor:container.safeAreaLayoutGuide.topAnchor],
        [scroll_view.leadingAnchor constraintEqualToAnchor:container.leadingAnchor],
        [scroll_view.trailingAnchor constraintEqualToAnchor:container.trailingAnchor],
        [scroll_view.bottomAnchor constraintEqualToAnchor:container.bottomAnchor],
    ]];
    self.view = container;

    __weak SidebarViewController* weak_self = self;
    self.table_view.on_middle_click = ^(NSInteger row) {
        SidebarViewController* self = weak_self;
        if (self != nil && row >= 0 && row < (NSInteger)self.tabs.count && self.on_close_tab)
            self.on_close_tab(self.tabs[row]);
    };
    self.table_view.context_menu_for_row = ^NSMenu*(NSInteger row) {
        return [weak_self contextMenuForRow:row];
    };
}

- (NSMenu*)contextMenuForRow:(NSInteger)row
{
    if (row < 0 || row >= (NSInteger)self.tabs.count)
        return nil;

    auto* tab = self.tabs[row];
    auto* menu = [[NSMenu alloc] init];
    menu.autoenablesItems = NO;

    auto* close_tab = [[NSMenuItem alloc] initWithTitle:@"Close Tab"
                                                 action:@selector(closeTabFromContextMenu:)
                                          keyEquivalent:@""];
    close_tab.target = self;
    close_tab.representedObject = tab;
    close_tab.enabled = self.on_close_tab != nil;
    [menu addItem:close_tab];

    auto* close_other_tabs = [[NSMenuItem alloc] initWithTitle:@"Close Other Tabs"
                                                        action:@selector(closeOtherTabsFromContextMenu:)
                                                 keyEquivalent:@""];
    close_other_tabs.target = self;
    close_other_tabs.representedObject = tab;
    close_other_tabs.enabled = self.tabs.count > 1 && self.on_close_other_tabs != nil;
    [menu addItem:close_other_tabs];

    auto* move_to_new_window = [[NSMenuItem alloc] initWithTitle:@"Move Tab to New Window"
                                                          action:@selector(moveTabToNewWindowFromContextMenu:)
                                                   keyEquivalent:@""];
    move_to_new_window.target = self;
    move_to_new_window.representedObject = tab;
    move_to_new_window.enabled = self.tabs.count > 1 && self.on_move_tab_to_new_window != nil;
    [menu addItem:move_to_new_window];

    return menu;
}

- (void)closeTabFromContextMenu:(NSMenuItem*)item
{
    if (self.on_close_tab)
        self.on_close_tab(item.representedObject);
}

- (void)closeOtherTabsFromContextMenu:(NSMenuItem*)item
{
    if (self.on_close_other_tabs)
        self.on_close_other_tabs(item.representedObject);
}

- (void)moveTabToNewWindowFromContextMenu:(NSMenuItem*)item
{
    if (self.on_move_tab_to_new_window)
        self.on_move_tab_to_new_window(item.representedObject);
}

- (NSInteger)numberOfRowsInTableView:(NSTableView*)table_view
{
    return self.tabs.count + 1;
}

- (NSView*)tableView:(NSTableView*)table_view viewForTableColumn:(NSTableColumn*)column row:(NSInteger)row
{
    if (row == (NSInteger)self.tabs.count) {
        return [[SidebarNewTabCellView alloc] initWithSidebar:self expanded:self.expanded];
    }

    auto* cell = (SidebarTabCellView*)[table_view makeViewWithIdentifier:SIDEBAR_TAB_CELL_IDENTIFIER owner:self];
    if (cell == nil) {
        cell = [[SidebarTabCellView alloc] init];
        cell.identifier = SIDEBAR_TAB_CELL_IDENTIFIER;
    }
    BOOL hovered = NO;
    if (auto* window = table_view.window) {
        auto location = [table_view convertPoint:window.mouseLocationOutsideOfEventStream fromView:nil];
        hovered = NSMouseInRect(location, table_view.visibleRect, table_view.isFlipped)
            && [table_view rowAtPoint:location] == row;
    }
    cell.sidebar = self;
    [cell configureWithTab:self.tabs[row] expanded:self.expanded hovered:hovered];
    return cell;
}

- (NSTableRowView*)tableView:(NSTableView*)table_view rowViewForRow:(NSInteger)row
{
    return [[SidebarTableRowView alloc] initWithFrame:NSZeroRect];
}

- (BOOL)tableView:(NSTableView*)table_view shouldSelectRow:(NSInteger)row
{
    return row != (NSInteger)self.tabs.count;
}

- (void)tableViewSelectionDidChange:(NSNotification*)notification
{
    if (m_is_updating_selection)
        return;
    auto row = self.table_view.selectedRow;
    if (row >= 0 && row < (NSInteger)self.tabs.count && self.on_select_tab)
        self.on_select_tab(self.tabs[row]);
}

- (void)viewDidLayout
{
    [super viewDidLayout];

    // Keep the single column sized to the sidebar, so that row content is not laid out relative
    // to a clipped default column width.
    [self.table_view sizeLastColumnToFit];
}

- (void)setExpanded:(BOOL)expanded
{
    _expanded = expanded;
    [self reloadTabs];
}

- (void)reloadTabs
{
    [self.table_view reloadData];
    [self selectTab:self.selected_tab];
}

- (void)reloadTab:(Tab*)tab
{
    auto row = [self.tabs indexOfObjectIdenticalTo:tab];
    if (row != NSNotFound)
        [self.table_view reloadDataForRowIndexes:[NSIndexSet indexSetWithIndex:row] columnIndexes:[NSIndexSet indexSetWithIndex:0]];
}

- (void)selectTab:(Tab*)tab
{
    if (tab == nil)
        return;
    auto row = [self.tabs indexOfObjectIdenticalTo:tab];
    if (row == NSNotFound)
        return;
    self.selected_tab = tab;
    [self loadViewIfNeeded];
    m_is_updating_selection = YES;
    [self.table_view selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:NO];
    m_is_updating_selection = NO;
}

@end
