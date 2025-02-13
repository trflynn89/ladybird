/*
 * Copyright (c) 2024, Andrew Kaster <akaster@serenityos.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibCore/ArgsParser.h>
#include <LibDevTools/Actors/TabActor.h>
#include <LibWebView/URL.h>
#include <UI/Qt/Application.h>
#include <UI/Qt/Settings.h>
#include <UI/Qt/StringUtils.h>
#include <UI/Qt/TaskManagerWindow.h>

#include <QFileDialog>
#include <QFileOpenEvent>

namespace Ladybird {

Application::Application(Badge<WebView::Application>, Main::Arguments& arguments)
    : QApplication(arguments.argc, arguments.argv)
{
}

void Application::create_platform_options(WebView::ChromeOptions&, WebView::WebContentOptions& web_content_options)
{
    web_content_options.config_path = Settings::the()->directory();
}

Application::~Application()
{
    close_task_manager_window();
}

bool Application::event(QEvent* event)
{
    switch (event->type()) {
    case QEvent::FileOpen: {
        if (!on_open_file)
            break;

        auto const& open_event = *static_cast<QFileOpenEvent const*>(event);
        auto file = ak_string_from_qstring(open_event.file());

        if (auto file_url = WebView::sanitize_url(file); file_url.has_value())
            on_open_file(file_url.release_value());
        break;
    }
    default:
        break;
    }

    return QApplication::event(event);
}

void Application::show_task_manager_window()
{
    if (!m_task_manager_window) {
        m_task_manager_window = new TaskManagerWindow(nullptr);
    }
    m_task_manager_window->show();
    m_task_manager_window->activateWindow();
    m_task_manager_window->raise();
}

void Application::close_task_manager_window()
{
    if (m_task_manager_window) {
        m_task_manager_window->close();
        delete m_task_manager_window;
        m_task_manager_window = nullptr;
    }
}

BrowserWindow& Application::new_window(Vector<URL::URL> const& initial_urls, BrowserWindow::IsPopupWindow is_popup_window, Tab* parent_tab, Optional<u64> page_index)
{
    auto* window = new BrowserWindow(initial_urls, is_popup_window, parent_tab, move(page_index));
    set_active_window(*window);
    window->show();
    if (initial_urls.is_empty()) {
        auto* tab = window->current_tab();
        if (tab) {
            tab->set_url_is_hidden(true);
            tab->focus_location_editor();
        }
    }
    window->activateWindow();
    window->raise();
    return *window;
}

Optional<ByteString> Application::ask_user_for_download_folder() const
{
    auto path = QFileDialog::getExistingDirectory(nullptr, "Select download directory", QDir::homePath());
    if (path.isNull())
        return {};

    return ak_byte_string_from_qstring(path);
}

Vector<DevTools::TabDescription> Application::tab_list() const
{
    if (!m_active_window)
        return {};

    Vector<DevTools::TabDescription> tabs;

    m_active_window->for_each_tab([&](Tab& tab) {
        tabs.empend(tab.view().id(), ak_byte_string_from_qstring(tab.title()), tab.view().url().to_byte_string());
    });

    return tabs;
}

void Application::inspect_tab(DevTools::TabDescription const& description, DevTools::DevToolsDelegate::OnTabInspectionComplete on_complete) const
{
    if (!m_active_window)
        return;

    bool found_tab = false;

    m_active_window->for_each_tab([&](Tab& tab) {
        if (found_tab || tab.view().id() != description.id)
            return;

        tab.view().on_received_dom_tree = [&tab, on_complete = move(on_complete)](ByteString const& dom_tree) {
            tab.view().on_received_dom_tree = nullptr;

            auto parsed_tree = JsonValue::from_string(dom_tree);

            if (parsed_tree.is_error()) {
                on_complete(parsed_tree.release_error());
                return;
            }

            on_complete(parsed_tree.release_value());
        };

        tab.view().inspect_dom_tree();
        found_tab = true;
    });

    if (!found_tab)
        on_complete(Error::from_string_literal("Unable to locate tab"));
}

}
