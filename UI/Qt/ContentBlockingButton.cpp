/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <UI/Qt/ContentBlockingButton.h>
#include <UI/Qt/StringUtils.h>

#include <QPainter>
#include <QStyleOptionToolButton>
#include <QStylePainter>

namespace Ladybird {

QString content_blocking_state_text(WebView::ContentBlockingSnapshot const& snapshot)
{
    switch (snapshot.state) {
    case WebView::ContentBlockingState::Enabled:
        return "Ad blocking is on";
    case WebView::ContentBlockingState::NoRules:
        return "No filter rules are loaded.";
    case WebView::ContentBlockingState::SiteDisabled:
        return "Ad blocking is off for this site";
    case WebView::ContentBlockingState::GlobalDisabled:
        return "Ad blocking is off in Settings.";
    case WebView::ContentBlockingState::CommandLineDisabled:
        return "Ad blocking is disabled by a command-line option.";
    case WebView::ContentBlockingState::Unavailable:
        return "Site controls apply to HTTP and HTTPS pages.";
    }
    VERIFY_NOT_REACHED();
}

QString content_blocking_count_text(u64 count)
{
    return qformatted("{} request{} blocked", count, count == 1 ? ""sv : "s"sv);
}

ContentBlockingButton::ContentBlockingButton(QWidget* parent)
    : QToolButton(parent)
{
    setObjectName("ContentBlockingButton");
    setAutoRaise(true);
    setFixedSize(sizeHint());
    setFocusPolicy(Qt::StrongFocus);
    setAccessibleName("Ad blocking");
    set_snapshot({});
}

void ContentBlockingButton::set_snapshot(WebView::ContentBlockingSnapshot const& snapshot)
{
    auto description = content_blocking_state_text(snapshot);
    if (snapshot.site.has_value())
        description = qstring_from_ak_string(*snapshot.site) + "\n" + description;
    // The accessible description changes with policy, while the exact count is available in the popover.
    // Avoid announcing every background request as a change to the button's accessible name.
    if (accessibleDescription().isEmpty() || snapshot.site != m_snapshot.site || snapshot.state != m_snapshot.state)
        setAccessibleDescription(description);
    m_snapshot = snapshot;
    if (snapshot.blocked_request_count > 0)
        description += "\n" + content_blocking_count_text(snapshot.blocked_request_count) + " on this page";
    setToolTip(description);
    update();
}

QString ContentBlockingButton::badge_text() const
{
    if (!m_snapshot.enabled() || m_snapshot.blocked_request_count == 0)
        return {};
    return m_snapshot.blocked_request_count > 99 ? "99+" : qformatted("{}", m_snapshot.blocked_request_count);
}

void ContentBlockingButton::paintEvent(QPaintEvent*)
{
    QStylePainter painter(this);
    QStyleOptionToolButton option;
    initStyleOption(&option);
    option.icon = {};
    painter.drawComplexControl(QStyle::CC_ToolButton, option);
    painter.setRenderHint(QPainter::Antialiasing);
    auto logo_rect = QRect((width() - 20) / 2, (height() - 20) / 2, 20, 20);
    painter.save();
    if (!m_snapshot.enabled())
        painter.setOpacity(0.45);
    m_logo.paint(&painter, logo_rect);
    painter.restore();
    auto badge = badge_text();
    if (badge.isEmpty())
        return;
    auto badge_font = font();
    badge_font.setPointSizeF(7.5);
    badge_font.setBold(true);
    painter.setFont(badge_font);
    auto badge_width = painter.fontMetrics().horizontalAdvance(badge) + 6;
    auto badge_height = painter.fontMetrics().height() + 2;
    auto badge_rect = QRect(width() - badge_width - 1, 1, badge_width, badge_height);
    painter.setPen(Qt::NoPen);
    painter.setBrush(palette().color(QPalette::Active, QPalette::Highlight));
    painter.drawRoundedRect(badge_rect, 5, 5);
    painter.setPen(palette().color(QPalette::Active, QPalette::HighlightedText));
    painter.drawText(badge_rect, Qt::AlignCenter, badge);
}

}
