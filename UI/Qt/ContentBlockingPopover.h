/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Function.h>
#include <LibWebView/ContentBlockingStats.h>
#include <UI/Qt/Popover.h>

class QCheckBox;
class QLabel;
class QKeyEvent;
class QMouseEvent;

namespace Ladybird {

class ContentBlockingPopover final : public Popover {
public:
    AK_ALLOC_WITH_KMALLOC;
    explicit ContentBlockingPopover(QWidget* parent);
    void set_snapshot(WebView::ContentBlockingSnapshot const&);
    void update_chrome_style(QPalette const&);
    void show_error(QString const&);
    void focus_switch();
    u64 load_id() const { return m_load_id; }
    Function<void(bool)> on_toggle;
    Function<void()> on_keyboard_dismiss;

private:
    virtual void keyPressEvent(QKeyEvent*) override;
    virtual void mousePressEvent(QMouseEvent*) override;
    void update_size();
    QLabel* m_site { nullptr };
    QLabel* m_state { nullptr };
    QLabel* m_count { nullptr };
    QLabel* m_note { nullptr };
    QLabel* m_error { nullptr };
    QCheckBox* m_switch { nullptr };
    u64 m_load_id { 0 };
};

}
