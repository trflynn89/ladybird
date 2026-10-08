/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <UI/Qt/ChromeStyle.h>
#include <UI/Qt/ContentBlockingButton.h>
#include <UI/Qt/ContentBlockingPopover.h>
#include <UI/Qt/StringUtils.h>

#include <QCheckBox>
#include <QFrame>
#include <QHBoxLayout>
#include <QKeyEvent>
#include <QLabel>
#include <QMouseEvent>
#include <QScreen>
#include <QSignalBlocker>
#include <QStyleOptionFocusRect>
#include <QStylePainter>
#include <QVBoxLayout>

namespace Ladybird {

class ContentBlockingSwitch final : public QCheckBox {
public:
    explicit ContentBlockingSwitch(QWidget* parent)
        : QCheckBox("Block ads on this site", parent)
    {
        setFocusPolicy(Qt::StrongFocus);
    }

    virtual QSize sizeHint() const override { return { fontMetrics().horizontalAdvance(text()) + 56, qMax(28, fontMetrics().height() + 8) }; }

private:
    virtual bool hitButton(QPoint const& position) const override { return rect().contains(position); }

    virtual void paintEvent(QPaintEvent*) override
    {
        QStylePainter painter(this);
        painter.setRenderHint(QPainter::Antialiasing);
        if (!isEnabled())
            painter.setOpacity(0.45);
        auto track = QStyle::visualRect(layoutDirection(), rect(), QRect(width() - 40, (height() - 22) / 2, 40, 22));
        auto track_color = isChecked() ? palette().color(QPalette::Active, QPalette::Highlight) : ChromeStyle::chrome_control_border(palette());
        if (underMouse() && isEnabled())
            track_color = ChromeStyle::mix(track_color, Qt::white, 0.12);
        painter.setPen(Qt::NoPen);
        painter.setBrush(track_color);
        painter.drawRoundedRect(track, 11, 11);
        auto thumb_offset = isChecked() != (layoutDirection() == Qt::RightToLeft) ? 21 : 3;
        painter.setBrush(palette().color(QPalette::Active, QPalette::HighlightedText));
        painter.drawEllipse(QRect(track.x() + thumb_offset, track.y() + 3, 16, 16));
        auto text_rect = QStyle::visualRect(layoutDirection(), rect(), QRect(0, 0, width() - 56, height()));
        painter.drawItemText(text_rect, Qt::AlignLeading | Qt::AlignVCenter, palette(), isEnabled(), text(), QPalette::WindowText);
        if (hasFocus()) {
            QStyleOptionFocusRect focus;
            focus.initFrom(this);
            focus.rect = rect();
            painter.drawPrimitive(QStyle::PE_FrameFocusRect, focus);
        }
    }
};

ContentBlockingPopover::ContentBlockingPopover(QWidget* parent)
    : Popover(parent)
{
    setAttribute(Qt::WA_NoMouseReplay);
    setFocusPolicy(Qt::StrongFocus);
    setAccessibleName("Ad blocking controls");
    card().setObjectName("LadybirdContentBlockingPopover");
    auto* layout = new QVBoxLayout(&card());
    layout->setContentsMargins(16, 14, 16, 14);
    layout->setSpacing(12);
    auto* heading = new QHBoxLayout;
    auto* logo = new QLabel(&card());
    logo->setPixmap(QIcon(":/Icons/ladybird.png").pixmap(24, 24));
    heading->addWidget(logo, 0, Qt::AlignTop);
    auto* text = new QVBoxLayout;
    m_site = new QLabel(&card());
    m_site->setObjectName("LadybirdContentBlockingSite");
    m_site->setTextFormat(Qt::PlainText);
    m_site->setSizePolicy(QSizePolicy::Ignored, QSizePolicy::Preferred);
    text->addWidget(m_site);
    m_state = new QLabel(&card());
    m_state->setTextFormat(Qt::PlainText);
    text->addWidget(m_state);
    heading->addLayout(text, 1);
    layout->addLayout(heading);
    m_switch = new ContentBlockingSwitch(&card());
    m_switch->setObjectName("ContentBlockingSiteSwitch");
    m_switch->setAccessibleName("Block ads on this site");
    m_switch->setFocusPolicy(Qt::StrongFocus);
    QObject::connect(m_switch, &QCheckBox::toggled, this, [this](bool enabled) {
        if (on_toggle)
            on_toggle(enabled);
    });
    layout->addWidget(m_switch);
    m_count = new QLabel(&card());
    m_count->setObjectName("LadybirdContentBlockingCount");
    m_count->setTextFormat(Qt::PlainText);
    m_count->setAlignment(Qt::AlignCenter);
    layout->addWidget(m_count);
    m_note = new QLabel(&card());
    m_note->setObjectName("LadybirdContentBlockingNote");
    m_note->setTextFormat(Qt::PlainText);
    layout->addWidget(m_note);
    m_error = new QLabel(&card());
    m_error->setTextFormat(Qt::PlainText);
    m_error->hide();
    layout->addWidget(m_error);
}

void ContentBlockingPopover::set_snapshot(WebView::ContentBlockingSnapshot const& snapshot)
{
    m_load_id = snapshot.load_id;
    auto site = snapshot.site.has_value() ? qstring_from_ak_string(*snapshot.site) : QString("This page");
    m_site->setToolTip(site);
    m_site->setAccessibleName(site);
    m_state->setText(content_blocking_state_text(snapshot));
    QSignalBlocker blocker(m_switch);
    m_switch->setChecked(snapshot.enabled());
    m_switch->setEnabled(snapshot.can_toggle());
    m_count->setText(content_blocking_count_text(snapshot.blocked_request_count) + " on this page");
    m_count->setVisible(snapshot.blocked_request_count > 0);
    m_note->setText("Changes reload this tab.");
    m_error->hide();
    update_size();
}

void ContentBlockingPopover::update_size()
{
    auto available_width = qMax(160, screen()->availableGeometry().width() - 48);
    auto count_font = m_count->font();
    count_font.setPixelSize(16);
    m_count->setFont(count_font);
    m_count->ensurePolished();
    auto count_width = m_count->fontMetrics().horizontalAdvance(m_count->text());
    if (count_width > available_width - 34) {
        // Keep the exact total on one line even on a narrow screen.
        count_font.setPixelSize(qMax(10, 16 * (available_width - 34) / (count_width + 1)));
        m_count->setFont(count_font);
    }
    m_state->ensurePolished();
    auto preferred_width = qMax(340, m_state->sizeHint().width() + 80);
    for (auto* label : { m_count, m_note, m_error }) {
        if (label->isHidden())
            continue;
        label->ensurePolished();
        preferred_width = qMax(preferred_width, label->sizeHint().width() + 34);
    }
    card().setFixedWidth(qMin(available_width, preferred_width));
    m_site->setText(m_site->fontMetrics().elidedText(m_site->toolTip(), Qt::ElideRight, card().width() - 80));
    adjustSize();
}

void ContentBlockingPopover::update_chrome_style(QPalette const& palette)
{
    auto active_palette = palette;
    active_palette.setCurrentColorGroup(QPalette::Active);
    setPalette(active_palette);
    setStyleSheet(ChromeStyle::content_blocking_popover_style_sheet(active_palette));
    update_size();
}

void ContentBlockingPopover::show_error(QString const& message)
{
    m_error->setText(message);
    m_error->show();
    update_size();
}

void ContentBlockingPopover::focus_switch()
{
    if (m_switch->isEnabled())
        m_switch->setFocus(Qt::PopupFocusReason);
    else
        setFocus(Qt::PopupFocusReason);
}

void ContentBlockingPopover::mousePressEvent(QMouseEvent* event)
{
    // The transparent shadow margin can overlap the toolbar button. Treat it as outside the card.
    if (!card().geometry().contains(event->position().toPoint())) {
        close();
        event->accept();
        return;
    }
    Popover::mousePressEvent(event);
}

void ContentBlockingPopover::keyPressEvent(QKeyEvent* event)
{
    if (event->key() == Qt::Key_Escape) {
        close();
        if (on_keyboard_dismiss)
            on_keyboard_dismiss();
        event->accept();
        return;
    }
    Popover::keyPressEvent(event);
}

}
