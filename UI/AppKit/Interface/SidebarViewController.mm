/*
 * Copyright (c) 2026, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibWebView/ViewImplementation.h>

#import <Interface/LadybirdWebView.h>
#import <Interface/SidebarViewController.h>
#import <Interface/Tab.h>

#if !__has_feature(objc_arc)
#    error "This project requires ARC"
#endif

static NSString* const SIDEBAR_TAB_CELL_IDENTIFIER = @"SidebarTabCellIdentifier";
static NSString* const SIDEBAR_NEW_TAB_CELL_IDENTIFIER = @"SidebarNewTabCellIdentifier";
static NSPasteboardType const SIDEBAR_TAB_PASTEBOARD_TYPE = @"org.ladybird.sidebar-tab";
static constexpr CGFloat SIDEBAR_ROW_HEIGHT = 33;
static constexpr CGFloat SIDEBAR_ICON_SIZE = 16;
static constexpr CGFloat SIDEBAR_ROW_HORIZONTAL_PADDING = 8;
static constexpr CGFloat SIDEBAR_ROW_CONTROL_SPACING = 6;
static constexpr CGFloat SIDEBAR_ROW_CORNER_RADIUS = 6;
static constexpr CGFloat SIDEBAR_ROW_HORIZONTAL_INSET = 4;
static constexpr CGFloat SIDEBAR_ROW_VERTICAL_INSET = 2;

@interface SidebarTabRowView : NSTableRowView
@end

@implementation SidebarTabRowView
{
    NSTrackingArea* m_tracking_area;
    BOOL m_hovered;
}

- (void)updateTrackingAreas
{
    [super updateTrackingAreas];

    if (m_tracking_area)
        [self removeTrackingArea:m_tracking_area];

    m_tracking_area = [[NSTrackingArea alloc] initWithRect:self.bounds
                                                   options:NSTrackingMouseEnteredAndExited | NSTrackingActiveInKeyWindow
                                                     owner:self
                                                  userInfo:nil];
    [self addTrackingArea:m_tracking_area];

    auto mouse_location = [self.window mouseLocationOutsideOfEventStream];
    m_hovered = NSPointInRect([self convertPoint:mouse_location fromView:nil], self.bounds);
}

- (void)mouseEntered:(NSEvent*)event
{
    m_hovered = YES;
    [self setNeedsDisplay:YES];
}

- (void)mouseExited:(NSEvent*)event
{
    m_hovered = NO;
    [self setNeedsDisplay:YES];
}

- (NSBezierPath*)backgroundPath
{
    auto background_rect = NSInsetRect(self.bounds, SIDEBAR_ROW_HORIZONTAL_INSET, SIDEBAR_ROW_VERTICAL_INSET);
    return [NSBezierPath bezierPathWithRoundedRect:background_rect
                                           xRadius:SIDEBAR_ROW_CORNER_RADIUS
                                           yRadius:SIDEBAR_ROW_CORNER_RADIUS];
}

- (void)drawBackgroundInRect:(NSRect)dirtyRect
{
    if (!m_hovered || self.isSelected)
        return;

    [[[NSColor labelColor] colorWithAlphaComponent:0.08] setFill];
    [[self backgroundPath] fill];
}

- (void)drawSelectionInRect:(NSRect)dirtyRect
{
    [[[NSColor labelColor] colorWithAlphaComponent:0.18] setFill];
    [[self backgroundPath] fill];
}

@end

@interface SidebarTabCellView : NSTableCellView

- (void)configureWithTab:(Tab*)tab selected:(BOOL)selected;

@property (nonatomic, copy) void (^on_close)(Tab*);

@end

@implementation SidebarTabCellView
{
    __weak Tab* m_tab;
    NSImageView* m_favicon_view;
    NSTextField* m_title_field;
    NSButton* m_audio_button;
    NSButton* m_close_button;
    NSTrackingArea* m_tracking_area;
    BOOL m_hovered;
    BOOL m_selected;
    BOOL m_audio_indicator_visible;
}

- (instancetype)initWithFrame:(NSRect)frameRect
{
    if (self = [super initWithFrame:frameRect]) {
        m_favicon_view = [[NSImageView alloc] init];
        [m_favicon_view setImageScaling:NSImageScaleProportionallyUpOrDown];
        [m_favicon_view setTranslatesAutoresizingMaskIntoConstraints:NO];

        m_title_field = [NSTextField labelWithString:@""];
        [m_title_field setMaximumNumberOfLines:1];
        [m_title_field setLineBreakMode:NSLineBreakByTruncatingTail];
        [m_title_field setContentCompressionResistancePriority:NSLayoutPriorityDefaultLow
                                                forOrientation:NSLayoutConstraintOrientationHorizontal];
        [m_title_field setTranslatesAutoresizingMaskIntoConstraints:NO];

        m_audio_button = [NSButton buttonWithImage:[NSImage imageWithSystemSymbolName:@"speaker.wave.2.fill"
                                                             accessibilityDescription:@"Audio playing"]
                                            target:self
                                            action:@selector(toggleTabAudio:)];
        [m_audio_button setBordered:NO];
        [m_audio_button setFocusRingType:NSFocusRingTypeNone];
        [m_audio_button setHidden:YES];
        [m_audio_button setTranslatesAutoresizingMaskIntoConstraints:NO];

        auto* close_image = [NSImage imageWithSystemSymbolName:@"xmark" accessibilityDescription:@"Close tab"];
        m_close_button = [NSButton buttonWithImage:close_image target:self action:@selector(closeTab:)];
        [m_close_button setBordered:NO];
        [m_close_button setFocusRingType:NSFocusRingTypeNone];
        [m_close_button setToolTip:@"Close tab"];
        [m_close_button setHidden:YES];
        [m_close_button setTranslatesAutoresizingMaskIntoConstraints:NO];

        [self addSubview:m_favicon_view];
        [self addSubview:m_title_field];
        [self addSubview:m_audio_button];
        [self addSubview:m_close_button];

        auto trailing_priority = static_cast<NSLayoutPriority>(999);
        auto* close_trailing = [m_close_button.trailingAnchor constraintEqualToAnchor:self.trailingAnchor
                                                                             constant:-SIDEBAR_ROW_HORIZONTAL_PADDING];
        close_trailing.priority = trailing_priority;
        auto* audio_trailing = [m_audio_button.trailingAnchor constraintEqualToAnchor:m_close_button.leadingAnchor
                                                                             constant:-SIDEBAR_ROW_CONTROL_SPACING];
        audio_trailing.priority = trailing_priority;
        auto* title_trailing = [m_title_field.trailingAnchor constraintLessThanOrEqualToAnchor:m_audio_button.leadingAnchor
                                                                                      constant:-SIDEBAR_ROW_CONTROL_SPACING];
        title_trailing.priority = trailing_priority;

        [NSLayoutConstraint activateConstraints:@[
            [m_favicon_view.leadingAnchor constraintEqualToAnchor:self.leadingAnchor
                                                         constant:SIDEBAR_ROW_HORIZONTAL_PADDING],
            [m_favicon_view.centerYAnchor constraintEqualToAnchor:self.centerYAnchor],
            [m_favicon_view.widthAnchor constraintEqualToConstant:SIDEBAR_ICON_SIZE],
            [m_favicon_view.heightAnchor constraintEqualToConstant:SIDEBAR_ICON_SIZE],
            [m_title_field.leadingAnchor constraintEqualToAnchor:m_favicon_view.trailingAnchor
                                                        constant:SIDEBAR_ROW_CONTROL_SPACING],
            [m_title_field.centerYAnchor constraintEqualToAnchor:self.centerYAnchor],
            title_trailing,
            [m_audio_button.centerYAnchor constraintEqualToAnchor:self.centerYAnchor],
            [m_audio_button.widthAnchor constraintEqualToConstant:SIDEBAR_ICON_SIZE],
            [m_audio_button.heightAnchor constraintEqualToConstant:SIDEBAR_ICON_SIZE],
            audio_trailing,
            [m_close_button.centerYAnchor constraintEqualToAnchor:self.centerYAnchor],
            [m_close_button.widthAnchor constraintEqualToConstant:SIDEBAR_ICON_SIZE],
            [m_close_button.heightAnchor constraintEqualToConstant:SIDEBAR_ICON_SIZE],
            close_trailing,
        ]];
    }

    return self;
}

- (void)updateTrackingAreas
{
    [super updateTrackingAreas];

    if (m_tracking_area)
        [self removeTrackingArea:m_tracking_area];

    m_tracking_area = [[NSTrackingArea alloc] initWithRect:self.bounds
                                                   options:NSTrackingMouseEnteredAndExited | NSTrackingActiveInKeyWindow
                                                     owner:self
                                                  userInfo:nil];
    [self addTrackingArea:m_tracking_area];
}

- (void)mouseEntered:(NSEvent*)event
{
    m_hovered = YES;
    [self updateCloseButtonVisibility];
}

- (void)mouseExited:(NSEvent*)event
{
    m_hovered = NO;
    [self updateCloseButtonVisibility];
}

- (void)configureWithTab:(Tab*)tab selected:(BOOL)selected
{
    m_tab = tab;
    m_selected = selected;

    [m_favicon_view setImage:tab.tabIcon];
    [m_title_field setStringValue:tab.displayTitle];
    [self setToolTip:tab.displayTitle];

    auto& view = tab.web_view.view;
    m_audio_indicator_visible = view.audio_play_state() == Web::HTML::AudioPlayState::Playing
        || view.page_mute_state() == Web::HTML::MuteState::Muted;
    [m_audio_button setHidden:!m_audio_indicator_visible];
    if (m_audio_indicator_visible) {
        [m_audio_button setImage:tab.iconForPageMuteState];
        [m_audio_button setToolTip:tab.toolTipForPageMuteState];
    }

    auto mouse_location = [self.window mouseLocationOutsideOfEventStream];
    m_hovered = NSPointInRect([self convertPoint:mouse_location fromView:nil], self.bounds);
    [self updateCloseButtonVisibility];
}

- (void)updateCloseButtonVisibility
{
    [m_close_button setHidden:!(m_hovered || m_selected)];
}

- (void)closeTab:(id)sender
{
    if (m_tab && self.on_close)
        self.on_close(m_tab);
}

- (void)toggleTabAudio:(id)sender
{
    if (m_tab && m_audio_indicator_visible)
        [m_tab togglePageMuteState:sender];
}

@end

@interface SidebarNewTabCellView : NSTableCellView
@end

@implementation SidebarNewTabCellView

- (instancetype)initWithFrame:(NSRect)frameRect
{
    if (self = [super initWithFrame:frameRect]) {
        auto* image_view = [[NSImageView alloc] init];
        [image_view setImage:[NSImage imageNamed:NSImageNameAddTemplate]];
        [image_view setImageScaling:NSImageScaleProportionallyUpOrDown];
        [image_view setTranslatesAutoresizingMaskIntoConstraints:NO];

        auto* title_field = [NSTextField labelWithString:@"New Tab"];
        [title_field setTranslatesAutoresizingMaskIntoConstraints:NO];

        [self addSubview:image_view];
        [self addSubview:title_field];

        [NSLayoutConstraint activateConstraints:@[
            [image_view.leadingAnchor constraintEqualToAnchor:self.leadingAnchor
                                                     constant:SIDEBAR_ROW_HORIZONTAL_PADDING],
            [image_view.centerYAnchor constraintEqualToAnchor:self.centerYAnchor],
            [image_view.widthAnchor constraintEqualToConstant:SIDEBAR_ICON_SIZE],
            [image_view.heightAnchor constraintEqualToConstant:SIDEBAR_ICON_SIZE],
            [title_field.leadingAnchor constraintEqualToAnchor:image_view.trailingAnchor
                                                      constant:SIDEBAR_ROW_CONTROL_SPACING],
            [title_field.centerYAnchor constraintEqualToAnchor:self.centerYAnchor],
            [title_field.trailingAnchor constraintLessThanOrEqualToAnchor:self.trailingAnchor
                                                                 constant:-SIDEBAR_ROW_HORIZONTAL_PADDING],
        ]];
    }

    return self;
}

@end

@interface SidebarTableView : NSTableView

@property (nonatomic) BOOL handling_tab_mouse_down;
@property (nonatomic) BOOL drag_session_started;
@property (nonatomic, copy) void (^on_tab_click)(NSInteger);
@property (nonatomic, copy) void (^on_middle_click)(NSInteger);
@property (nonatomic, copy) void (^on_new_tab_click)(void);

@end

@implementation SidebarTableView
{
    NSInteger m_middle_click_row;
}

- (void)mouseDown:(NSEvent*)event
{
    auto row = [self rowAtPoint:[self convertPoint:event.locationInWindow fromView:nil]];
    if (row == self.numberOfRows - 1) {
        if (self.on_new_tab_click)
            self.on_new_tab_click();
        return;
    }

    if (row < 0) {
        [super mouseDown:event];
        return;
    }

    self.handling_tab_mouse_down = YES;
    self.drag_session_started = NO;
    [self selectRowIndexes:[NSIndexSet indexSetWithIndex:row] byExtendingSelection:NO];
    [super mouseDown:event];
    self.handling_tab_mouse_down = NO;

    if (!self.drag_session_started && self.selectedRow == row && self.on_tab_click)
        self.on_tab_click(row);
}

- (void)otherMouseDown:(NSEvent*)event
{
    if (event.buttonNumber != 2) {
        [super otherMouseDown:event];
        return;
    }

    m_middle_click_row = [self rowAtPoint:[self convertPoint:event.locationInWindow fromView:nil]];
}

- (void)otherMouseUp:(NSEvent*)event
{
    if (event.buttonNumber != 2) {
        [super otherMouseUp:event];
        return;
    }

    auto row = [self rowAtPoint:[self convertPoint:event.locationInWindow fromView:nil]];
    if (row == m_middle_click_row && row >= 0 && row < self.numberOfRows - 1 && self.on_middle_click)
        self.on_middle_click(row);
    m_middle_click_row = -1;
}

@end

@interface SidebarViewController () <NSTableViewDataSource, NSTableViewDelegate>

@property (nonatomic, strong) SidebarTableView* table_view;

@end

@implementation SidebarViewController
{
    NSPointerArray* m_tabs;
    __weak Tab* m_selected_tab;
    __weak Tab* m_dragged_tab;
    BOOL m_programmatic_selection;
}

- (instancetype)init
{
    if (self = [super initWithNibName:nil bundle:nil]) {
        m_tabs = [NSPointerArray weakObjectsPointerArray];

        auto* notification_center = NSNotificationCenter.defaultCenter;
        [notification_center addObserver:self selector:@selector(tabDidChange:) name:TabTitleDidChangeNotification object:nil];
        [notification_center addObserver:self selector:@selector(tabDidChange:) name:TabFaviconDidChangeNotification object:nil];
        [notification_center addObserver:self selector:@selector(tabDidChange:) name:TabAudioStateDidChangeNotification object:nil];
    }

    return self;
}

- (void)dealloc
{
    auto* notification_center = NSNotificationCenter.defaultCenter;
    [notification_center removeObserver:self name:TabTitleDidChangeNotification object:nil];
    [notification_center removeObserver:self name:TabFaviconDidChangeNotification object:nil];
    [notification_center removeObserver:self name:TabAudioStateDidChangeNotification object:nil];
}

- (void)loadView
{
    self.table_view = [[SidebarTableView alloc] init];
    [self.table_view setDataSource:self];
    [self.table_view setDelegate:self];
    [self.table_view setAllowsEmptySelection:NO];
    [self.table_view setAllowsMultipleSelection:NO];
    [self.table_view setHeaderView:nil];
    [self.table_view setRowHeight:SIDEBAR_ROW_HEIGHT];
    [self.table_view setUsesAutomaticRowHeights:NO];
    [self.table_view setStyle:NSTableViewStyleSourceList];
    [self.table_view setColumnAutoresizingStyle:NSTableViewUniformColumnAutoresizingStyle];
    [self.table_view registerForDraggedTypes:@[ SIDEBAR_TAB_PASTEBOARD_TYPE ]];
    [self.table_view setDraggingSourceOperationMask:NSDragOperationMove forLocal:YES];

    auto* column = [[NSTableColumn alloc] initWithIdentifier:@"SidebarColumn"];
    [column setResizingMask:NSTableColumnAutoresizingMask];
    [self.table_view addTableColumn:column];

    __weak SidebarViewController* weak_self = self;
    [self.table_view setOn_tab_click:^(NSInteger row) {
        SidebarViewController* self = weak_self;
        if (self == nil)
            return;
        auto* tab = [self tabAtIndex:row];
        if (tab && self.on_tab_selected)
            self.on_tab_selected(tab);
    }];
    [self.table_view setOn_middle_click:^(NSInteger row) {
        SidebarViewController* self = weak_self;
        if (self == nil)
            return;
        auto* tab = [self tabAtIndex:row];
        if (tab && self.on_tab_closed)
            self.on_tab_closed(tab);
    }];
    [self.table_view setOn_new_tab_click:^{
        SidebarViewController* self = weak_self;
        if (self != nil && self.on_new_tab)
            self.on_new_tab();
    }];

    auto* scroll_view = [[NSScrollView alloc] init];
    [scroll_view setDrawsBackground:NO];
    [scroll_view setHasVerticalScroller:YES];
    [scroll_view setBorderType:NSNoBorder];
    [scroll_view setDocumentView:self.table_view];
    [scroll_view setTranslatesAutoresizingMaskIntoConstraints:NO];

    auto* container = [[NSView alloc] init];
    [container addSubview:scroll_view];
    [NSLayoutConstraint activateConstraints:@[
        [scroll_view.leadingAnchor constraintEqualToAnchor:container.leadingAnchor],
        [scroll_view.trailingAnchor constraintEqualToAnchor:container.trailingAnchor],
        [scroll_view.topAnchor constraintEqualToAnchor:container.safeAreaLayoutGuide.topAnchor],
        [scroll_view.bottomAnchor constraintEqualToAnchor:container.bottomAnchor],
    ]];
    self.view = container;

    [self.table_view reloadData];
    [self setSelectedTab:m_selected_tab];
}

- (void)viewDidLayout
{
    [super viewDidLayout];
    [self.table_view sizeLastColumnToFit];
}

- (void)setTabs:(NSArray<Tab*>*)tabs
{
    [m_tabs setCount:0];
    for (Tab* tab in tabs)
        [m_tabs addPointer:(__bridge void*)tab];

    [self.table_view reloadData];
    [self setSelectedTab:m_selected_tab];
}

- (void)setSelectedTab:(Tab*)tab
{
    m_selected_tab = tab;
    auto index = [self indexOfTab:tab];

    m_programmatic_selection = YES;
    if (index == NSNotFound)
        [self.table_view deselectAll:nil];
    else
        [self.table_view selectRowIndexes:[NSIndexSet indexSetWithIndex:index] byExtendingSelection:NO];
    m_programmatic_selection = NO;

    auto rows = [self.table_view rowsInRect:self.table_view.visibleRect];
    if (rows.length > 0)
        [self.table_view reloadDataForRowIndexes:[NSIndexSet indexSetWithIndexesInRange:rows]
                                   columnIndexes:[NSIndexSet indexSetWithIndex:0]];
}

- (void)reloadTab:(Tab*)tab
{
    auto index = [self indexOfTab:tab];
    if (index == NSNotFound)
        return;

    [self.table_view reloadDataForRowIndexes:[NSIndexSet indexSetWithIndex:index]
                               columnIndexes:[NSIndexSet indexSetWithIndex:0]];
}

- (Tab*)tabAtIndex:(NSInteger)index
{
    if (index < 0 || static_cast<NSUInteger>(index) >= m_tabs.count)
        return nil;
    return (__bridge Tab*)[m_tabs pointerAtIndex:index];
}

- (NSUInteger)indexOfTab:(Tab*)tab
{
    if (tab == nil)
        return NSNotFound;

    for (NSUInteger index = 0; index < m_tabs.count; ++index) {
        if ([self tabAtIndex:index] == tab)
            return index;
    }
    return NSNotFound;
}

- (void)tabDidChange:(NSNotification*)notification
{
    auto* tab = (Tab*)notification.object;
    if ([self indexOfTab:tab] != NSNotFound)
        [self reloadTab:tab];
}

#pragma mark - NSTableViewDataSource

- (NSInteger)numberOfRowsInTableView:(NSTableView*)tableView
{
    return m_tabs.count + 1;
}

- (id<NSPasteboardWriting>)tableView:(NSTableView*)tableView pasteboardWriterForRow:(NSInteger)row
{
    auto* tab = [self tabAtIndex:row];
    if (tab == nil)
        return nil;

    self.table_view.drag_session_started = YES;
    m_dragged_tab = tab;
    auto* item = [[NSPasteboardItem alloc] init];
    [item setString:@"tab" forType:SIDEBAR_TAB_PASTEBOARD_TYPE];
    return item;
}

- (NSDragOperation)tableView:(NSTableView*)tableView
                validateDrop:(id<NSDraggingInfo>)info
                 proposedRow:(NSInteger)row
       proposedDropOperation:(NSTableViewDropOperation)dropOperation
{
    if (info.draggingSource != tableView || m_dragged_tab == nil || row < 0 || static_cast<NSUInteger>(row) > m_tabs.count)
        return NSDragOperationNone;

    [tableView setDropRow:row dropOperation:NSTableViewDropAbove];
    return NSDragOperationMove;
}

- (BOOL)tableView:(NSTableView*)tableView
       acceptDrop:(id<NSDraggingInfo>)info
              row:(NSInteger)row
    dropOperation:(NSTableViewDropOperation)dropOperation
{
    auto source_index = [self indexOfTab:m_dragged_tab];
    if (source_index == NSNotFound || row < 0 || static_cast<NSUInteger>(row) > m_tabs.count)
        return NO;

    auto* reordered_tabs = [[NSMutableArray<Tab*> alloc] initWithCapacity:m_tabs.count];
    for (NSUInteger index = 0; index < m_tabs.count; ++index) {
        if (auto* tab = [self tabAtIndex:index])
            [reordered_tabs addObject:tab];
    }

    auto* dragged_tab = m_dragged_tab;
    [reordered_tabs removeObjectAtIndex:source_index];
    auto destination_index = static_cast<NSUInteger>(row);
    if (source_index < destination_index)
        --destination_index;
    destination_index = MIN(destination_index, reordered_tabs.count);
    [reordered_tabs insertObject:dragged_tab atIndex:destination_index];

    if (self.on_tabs_reordered)
        self.on_tabs_reordered(reordered_tabs);
    m_dragged_tab = nil;
    return YES;
}

- (void)tableView:(NSTableView*)tableView
    draggingSession:(NSDraggingSession*)session
       endedAtPoint:(NSPoint)screenPoint
          operation:(NSDragOperation)operation
{
    m_dragged_tab = nil;
    [self setSelectedTab:m_selected_tab];
}

#pragma mark - NSTableViewDelegate

- (NSTableRowView*)tableView:(NSTableView*)tableView
               rowViewForRow:(NSInteger)row
{
    if (row == static_cast<NSInteger>(m_tabs.count))
        return [[NSTableRowView alloc] initWithFrame:NSZeroRect];
    return [[SidebarTabRowView alloc] initWithFrame:NSZeroRect];
}

- (NSView*)tableView:(NSTableView*)tableView
    viewForTableColumn:(NSTableColumn*)tableColumn
                   row:(NSInteger)row
{
    if (row == static_cast<NSInteger>(m_tabs.count)) {
        auto* cell = (SidebarNewTabCellView*)[tableView makeViewWithIdentifier:SIDEBAR_NEW_TAB_CELL_IDENTIFIER owner:self];
        if (cell == nil) {
            cell = [[SidebarNewTabCellView alloc] initWithFrame:NSZeroRect];
            cell.identifier = SIDEBAR_NEW_TAB_CELL_IDENTIFIER;
        }
        return cell;
    }

    auto* cell = (SidebarTabCellView*)[tableView makeViewWithIdentifier:SIDEBAR_TAB_CELL_IDENTIFIER owner:self];
    if (cell == nil) {
        cell = [[SidebarTabCellView alloc] initWithFrame:NSZeroRect];
        cell.identifier = SIDEBAR_TAB_CELL_IDENTIFIER;
    }

    __weak SidebarViewController* weak_self = self;
    [cell setOn_close:^(Tab* tab) {
        SidebarViewController* self = weak_self;
        if (self != nil && self.on_tab_closed)
            self.on_tab_closed(tab);
    }];
    auto* tab = [self tabAtIndex:row];
    [cell configureWithTab:tab selected:tab == m_selected_tab];
    return cell;
}

- (NSIndexSet*)tableView:(NSTableView*)tableView
    selectionIndexesForProposedSelection:(NSIndexSet*)proposedSelectionIndexes
{
    NSMutableIndexSet* selection = [proposedSelectionIndexes mutableCopy];
    [selection removeIndex:m_tabs.count];
    return selection;
}

- (void)tableViewSelectionDidChange:(NSNotification*)notification
{
    if (m_programmatic_selection || self.table_view.handling_tab_mouse_down)
        return;

    auto row = self.table_view.selectedRow;
    auto* tab = [self tabAtIndex:row];
    if (tab && self.on_tab_selected)
        self.on_tab_selected(tab);
}

@end
