/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/kmalloc.h>
#include <LibWebView/ContentBlockingStats.h>
#include <QIcon>
#include <QToolButton>

namespace Ladybird {

QString content_blocking_state_text(WebView::ContentBlockingSnapshot const&);
QString content_blocking_count_text(u64);

class ContentBlockingButton final : public QToolButton {
public:
    AK_ALLOC_WITH_KMALLOC;
    explicit ContentBlockingButton(QWidget* parent);
    void set_snapshot(WebView::ContentBlockingSnapshot const&);
    QString badge_text() const;
    virtual QSize sizeHint() const override { return { 34, 34 }; }

private:
    virtual void paintEvent(QPaintEvent*) override;
    QIcon m_logo { ":/Icons/ladybird.png" };
    WebView::ContentBlockingSnapshot m_snapshot;
};

}
