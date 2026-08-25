/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/StringView.h>
#include <LibWebView/Menu.h>

namespace Ladybird::MacOS {

inline constexpr auto folder_menu_icon_name = "folder"sv;

constexpr StringView menu_icon_name_for_action(WebView::ActionID action_id, bool engaged = false)
{
    switch (action_id) {
    case WebView::ActionID::NewTab:
    case WebView::ActionID::OpenInNewTab:
        return "plus.square.on.square"sv;
    case WebView::ActionID::NewWindow:
    case WebView::ActionID::OpenInNewWindow:
        return "macwindow.badge.plus"sv;
    case WebView::ActionID::NewPrivateWindow:
    case WebView::ActionID::OpenInNewPrivateWindow:
        return "eyeglasses"sv;
    case WebView::ActionID::ReopenRecentlyClosedTab:
        return "arrow.uturn.backward"sv;
    case WebView::ActionID::CloseCurrentTab:
        return "xmark.square"sv;
    case WebView::ActionID::OpenFile:
        return folder_menu_icon_name;
    case WebView::ActionID::OpenLocation:
        return "globe"sv;
    case WebView::ActionID::FindInPage:
    case WebView::ActionID::LookUpSelectedText:
    case WebView::ActionID::SearchSelectedText:
        return "magnifyingglass"sv;
    case WebView::ActionID::OpenNextTab:
        return "chevron.right"sv;
    case WebView::ActionID::OpenPreviousTab:
        return "chevron.left"sv;
    case WebView::ActionID::Quit:
        return "power"sv;

    case WebView::ActionID::NavigateBack:
        return "chevron.left"sv;
    case WebView::ActionID::NavigateForward:
        return "chevron.right"sv;
    case WebView::ActionID::Reload:
        return "arrow.clockwise"sv;
    case WebView::ActionID::ViewDownloads:
        return "arrow.down.circle"sv;
    case WebView::ActionID::ViewHistory:
        return "clock"sv;
    case WebView::ActionID::ClearBrowsingData:
        return "trash"sv;

    case WebView::ActionID::Undo:
        return "arrow.uturn.backward"sv;
    case WebView::ActionID::Redo:
        return "arrow.uturn.forward"sv;
    case WebView::ActionID::CopySelection:
        return "document.on.document"sv;
    case WebView::ActionID::CutSelection:
        return "scissors"sv;
    case WebView::ActionID::Paste:
        return "document.on.clipboard"sv;
    case WebView::ActionID::SelectAll:
        return "character.textbox"sv;

    case WebView::ActionID::ManageBookmarks:
        return "bookmark"sv;
    case WebView::ActionID::ToggleBookmark:
    case WebView::ActionID::ToggleBookmarkViaToolbar:
        return engaged ? "star.fill"sv : "star"sv;
    case WebView::ActionID::AddBookmarkAllTabs:
        return "square.badge.plus"sv;
    case WebView::ActionID::ToggleBookmarksBar:
        return "line.horizontal.star.fill.line.horizontal"sv;
    case WebView::ActionID::BookmarkItem:
        return "globe"sv;

    case WebView::ActionID::OpenAboutPage:
        return "info.circle"sv;
    case WebView::ActionID::OpenProcessesPage:
        return "gearshape.2"sv;
    case WebView::ActionID::OpenSettingsPage:
        return "gearshape"sv;
    case WebView::ActionID::ToggleDevTools:
        return "chevron.left.chevron.right"sv;
    case WebView::ActionID::ViewSource:
        return "text.document"sv;

    case WebView::ActionID::TakeVisibleScreenshot:
    case WebView::ActionID::TakeFullScreenshot:
        return "photo"sv;

    case WebView::ActionID::CopyURL:
        return "document.on.document"sv;

    case WebView::ActionID::OpenImage:
        return "photo"sv;
    case WebView::ActionID::SaveImage:
        return "square.and.arrow.down"sv;
    case WebView::ActionID::CopyImage:
        return "document.on.document"sv;

    case WebView::ActionID::OpenAudio:
        return "speaker.wave.1"sv;
    case WebView::ActionID::OpenVideo:
        return "video"sv;
    case WebView::ActionID::PlayMedia:
        return "play"sv;
    case WebView::ActionID::PauseMedia:
        return "pause"sv;
    case WebView::ActionID::MuteMedia:
        return "speaker.slash"sv;
    case WebView::ActionID::UnmuteMedia:
        return "speaker.wave.2"sv;
    case WebView::ActionID::ShowControls:
        return "eye"sv;
    case WebView::ActionID::HideControls:
        return "eye.slash"sv;
    case WebView::ActionID::ToggleMediaLoopState:
        return "arrow.clockwise"sv;
    case WebView::ActionID::EnterFullscreen:
        return "arrow.up.left.and.arrow.down.right"sv;
    case WebView::ActionID::ExitFullscreen:
        return "arrow.down.right.and.arrow.up.left"sv;

    case WebView::ActionID::ZoomIn:
        return "plus.magnifyingglass"sv;
    case WebView::ActionID::ZoomOut:
        return "minus.magnifyingglass"sv;
    case WebView::ActionID::ResetZoom:
        return "1.magnifyingglass"sv;

    default:
        return {};
    }
}

}
