/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <QColor>
#include <QPalette>
#include <QString>

class QWidget;

namespace Ladybird::ChromeStyle {

enum class WindowVariant {
    Normal,
    Private,
};

bool is_dark(QPalette const&);
WindowVariant window_variant(QWidget const*);
QColor mix(QColor const& from, QColor const& to, double amount);
QColor chrome_text(QPalette const&, WindowVariant = WindowVariant::Normal);
QColor chrome_button_text(QPalette const&, WindowVariant = WindowVariant::Normal);
QColor chrome_background(QPalette const&, WindowVariant = WindowVariant::Normal);
QColor chrome_surface(QPalette const&, WindowVariant = WindowVariant::Normal);
QColor chrome_surface_recessed(QPalette const&, WindowVariant = WindowVariant::Normal);
QColor chrome_surface_hover(QPalette const&, WindowVariant = WindowVariant::Normal);
QColor chrome_surface_pressed(QPalette const&, WindowVariant = WindowVariant::Normal);
QColor chrome_control_border(QPalette const&, WindowVariant = WindowVariant::Normal);
QColor chrome_active_tab_surface_top(QPalette const&, WindowVariant = WindowVariant::Normal);
QColor chrome_active_tab_surface_bottom(QPalette const&, WindowVariant = WindowVariant::Normal);
QColor chrome_border(QPalette const&, WindowVariant = WindowVariant::Normal);
QColor chrome_window_outline(QPalette const&, WindowVariant = WindowVariant::Normal);
QColor chrome_accent(QPalette const&);
QColor chrome_muted_text(QPalette const&, WindowVariant = WindowVariant::Normal);

QString style_sheet_color(QColor const&);
QString application_style_sheet(QPalette const&);
QString toolbar_container_style_sheet(QPalette const&, WindowVariant = WindowVariant::Normal);
QString menu_bar_style_sheet(QPalette const&, WindowVariant = WindowVariant::Normal);
QString location_edit_style_sheet(QPalette const&, WindowVariant = WindowVariant::Normal);
QString bookmarks_bar_style_sheet(QPalette const&, WindowVariant = WindowVariant::Normal);
QString find_in_page_style_sheet(QPalette const&, WindowVariant = WindowVariant::Normal);
QString devtools_banner_style_sheet(QPalette const&, WindowVariant = WindowVariant::Normal);
QString tab_widget_style_sheet(QPalette const&, WindowVariant = WindowVariant::Normal);
QString autocomplete_popup_style_sheet(QPalette const&, WindowVariant = WindowVariant::Normal);

}
