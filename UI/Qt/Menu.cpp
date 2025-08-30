/*
 * Copyright (c) 2025, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/WeakPtr.h>
#include <UI/Qt/Icon.h>
#include <UI/Qt/Menu.h>
#include <UI/Qt/StringUtils.h>

#include <QAction>
#include <QApplication>
#include <QMenu>
#include <QWidget>

namespace Ladybird {

static void setup_qt_action(WebView::Action& action, QAction& qaction, QPalette const& palette)
{
    action.set_ui_action(&qaction);

    switch (action.id()) {
    case WebView::ActionID::NavigateBack:
        qaction.setIcon(create_tvg_icon_with_theme_colors("back", palette));
        qaction.setShortcuts(QKeySequence::keyBindings(QKeySequence::StandardKey::Back));
        break;
    case WebView::ActionID::NavigateForward:
        qaction.setIcon(create_tvg_icon_with_theme_colors("forward", palette));
        qaction.setShortcuts(QKeySequence::keyBindings(QKeySequence::StandardKey::Forward));
        break;
    case WebView::ActionID::Reload:
        qaction.setIcon(create_tvg_icon_with_theme_colors("reload", palette));
        qaction.setShortcuts({ QKeySequence(Qt::CTRL | Qt::Key_R), QKeySequence(Qt::Key_F5) });
        break;

    case WebView::ActionID::CopySelection:
        qaction.setIcon(load_icon_from_uri("resource://icons/16x16/edit-copy.png"sv));
        qaction.setShortcuts(QKeySequence::keyBindings(QKeySequence::StandardKey::Copy));
        break;
    case WebView::ActionID::Paste:
        qaction.setIcon(load_icon_from_uri("resource://icons/16x16/paste.png"sv));
        qaction.setShortcuts(QKeySequence::keyBindings(QKeySequence::StandardKey::Paste));
        break;
    case WebView::ActionID::SelectAll:
        qaction.setIcon(load_icon_from_uri("resource://icons/16x16/select-all.png"sv));
        qaction.setShortcuts(QKeySequence::keyBindings(QKeySequence::StandardKey::SelectAll));
        break;

    case WebView::ActionID::SearchSelectedText:
        qaction.setIcon(load_icon_from_uri("resource://icons/16x16/find.png"sv));
        break;

    case WebView::ActionID::ViewSource:
        qaction.setIcon(load_icon_from_uri("resource://icons/16x16/filetype-html.png"sv));
        qaction.setShortcut(QKeySequence(Qt::CTRL | Qt::Key_U));
        break;

    case WebView::ActionID::TakeVisibleScreenshot:
    case WebView::ActionID::TakeFullScreenshot:
        qaction.setIcon(load_icon_from_uri("resource://icons/16x16/filetype-image.png"sv));
        break;

    case WebView::ActionID::OpenInNewTab:
        qaction.setIcon(load_icon_from_uri("resource://icons/16x16/new-tab.png"sv));
        break;
    case WebView::ActionID::CopyURL:
        qaction.setIcon(load_icon_from_uri("resource://icons/16x16/edit-copy.png"sv));
        break;

    case WebView::ActionID::OpenImage:
        qaction.setIcon(load_icon_from_uri("resource://icons/16x16/filetype-image.png"sv));
        break;
    case WebView::ActionID::CopyImage:
        qaction.setIcon(load_icon_from_uri("resource://icons/16x16/edit-copy.png"sv));
        break;

    case WebView::ActionID::OpenAudio:
        qaction.setIcon(load_icon_from_uri("resource://icons/16x16/filetype-sound.png"sv));
        break;
    case WebView::ActionID::OpenVideo:
        qaction.setIcon(load_icon_from_uri("resource://icons/16x16/filetype-video.png"sv));
        break;
    case WebView::ActionID::PlayMedia:
        qaction.setIcon(load_icon_from_uri("resource://icons/16x16/play.png"sv));
        break;
    case WebView::ActionID::PauseMedia:
        qaction.setIcon(load_icon_from_uri("resource://icons/16x16/pause.png"sv));
        break;
    case WebView::ActionID::MuteMedia:
        qaction.setIcon(load_icon_from_uri("resource://icons/16x16/audio-volume-muted.png"sv));
        break;
    case WebView::ActionID::UnmuteMedia:
        qaction.setIcon(load_icon_from_uri("resource://icons/16x16/audio-volume-high.png"sv));
        break;
    case WebView::ActionID::ToggleMediaControlsState:
    case WebView::ActionID::ToggleMediaLoopState:
        break;
    }

    action.on_text_changed = [action = action.make_weak_ptr()]() {
        if (action)
            action->ui_action<QAction>()->setText(qstring_from_ak_string(action->text()));
    };

    action.on_enabled_changed = [action = action.make_weak_ptr()]() {
        if (action)
            action->ui_action<QAction>()->setEnabled(action->enabled() == WebView::Action::Enabled::Yes);
    };

    action.on_visible_changed = [action = action.make_weak_ptr()]() {
        if (action)
            action->ui_action<QAction>()->setVisible(action->visible() == WebView::Action::Visible::Yes);
    };

    action.on_checked_changed = [action = action.make_weak_ptr()]() {
        if (action)
            action->ui_action<QAction>()->setChecked(action->checked() == WebView::Action::Checked::Yes);
    };

    action.on_text_changed();
    action.on_enabled_changed();
    action.on_visible_changed();

    if (action.is_checkable()) {
        qaction.setCheckable(true);
        action.on_checked_changed();
    }
}

static void add_items_to_menu(QMenu& menu, QWidget* parent, Span<WebView::Menu::MenuItem> menu_items)
{
    for (auto& menu_item : menu_items) {
        menu_item.visit(
            [&](NonnullRefPtr<WebView::Action>& action) {
                auto* qaction = create_application_action(parent, parent->palette(), action);
                menu.addAction(qaction);
            },
            [&](NonnullRefPtr<WebView::Menu> const& submenu) {
                auto* qsubmenu = new QMenu(qstring_from_ak_string(submenu->title()), parent);
                add_items_to_menu(*qsubmenu, parent, submenu->items());

                menu.addMenu(qsubmenu);
            },
            [&](WebView::Separator) {
                menu.addSeparator();
            });
    }
}

QMenu* create_application_menu(QWidget* parent, WebView::Menu& menu, WebView::Menu::OnActivation on_activation)
{
    auto* application_menu = new QMenu(qstring_from_ak_string(menu.title()), parent);
    add_items_to_menu(*application_menu, parent, menu.items());

    menu.on_activation = move(on_activation);

    return application_menu;
}

QAction* create_application_action(QObject* parent, QPalette const& palette, WebView::Action& action)
{
    if (auto* qaction = action.ui_action<QAction>())
        return qaction;

    auto* qaction = new QAction(parent);
    setup_qt_action(action, *qaction, palette);

    QObject::connect(qaction, &QAction::triggered, [action = action.make_weak_ptr()]() {
        if (action)
            action->activate();
    });

    return qaction;
}

void refresh_application_action_icon(QPalette const& palette, WebView::Action& action)
{
    auto* qaction = action.ui_action<QAction>();
    if (!qaction)
        return;

    switch (action.id()) {
    case WebView::ActionID::NavigateBack:
        qaction->setIcon(create_tvg_icon_with_theme_colors("back", palette));
        break;
    case WebView::ActionID::NavigateForward:
        qaction->setIcon(create_tvg_icon_with_theme_colors("forward", palette));
        break;
    case WebView::ActionID::Reload:
        qaction->setIcon(create_tvg_icon_with_theme_colors("reload", palette));
        break;
    default:
        break;
    }
}

}
