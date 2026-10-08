/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/Assertions.h>
#include <AK/NumericLimits.h>
#include <QAccessible>
#include <QApplication>
#include <QCheckBox>
#include <QKeyEvent>
#include <QLabel>
#include <QMouseEvent>
#include <QPixmap>
#include <QStyleHints>
#include <UI/Qt/ContentBlockingButton.h>
#include <UI/Qt/ContentBlockingPopover.h>

int main(int argc, char** argv)
{
    QApplication application(argc, argv);
    QWidget parent;
    parent.resize(600, 500);
    Ladybird::ContentBlockingButton button(&parent);
    WebView::ContentBlockingSnapshot snapshot;
    snapshot.site = "example.com"_string;
    snapshot.state = WebView::ContentBlockingState::Enabled;
    snapshot.load_id = 7;
    auto size = button.size();
    u64 counts[] = { 0, 1, 99, 100, NumericLimits<u64>::max() };
    for (u64 count : counts) {
        snapshot.blocked_request_count = count;
        button.set_snapshot(snapshot);
        VERIFY(button.size() == size);
        VERIFY(button.badge_text() == (count == 0 ? QString() : count > 99 ? QString("99+")
                                                                           : QString::number(count)));
        if (count == 0)
            VERIFY(!button.toolTip().contains("request"));
        else
            VERIFY(button.toolTip().contains(QString::number(count)));
        VERIFY(!button.grab().isNull());
    }
    VERIFY(!QIcon(":/Icons/ladybird.png").isNull());
    VERIFY(button.accessibleName() == "Ad blocking");
    Ladybird::ContentBlockingPopover popover(&parent);
    auto* toggle = popover.findChild<QCheckBox*>("ContentBlockingSiteSwitch");
    VERIFY(toggle);
    size_t toggles = 0;
    popover.on_toggle = [&](bool enabled) { VERIFY(!enabled); ++toggles; };
    popover.update_chrome_style(application.palette());
    popover.set_snapshot(snapshot);
    VERIFY(toggles == 0);
    VERIFY(toggle->isEnabled() && toggle->isChecked());
    auto* accessible = QAccessible::queryAccessibleInterface(toggle);
    VERIFY(accessible && accessible->state().checked);
    auto* site = popover.findChild<QLabel*>("LadybirdContentBlockingSite");
    VERIFY(site && site->textFormat() == Qt::PlainText);
    VERIFY(site->toolTip() == "example.com");
    auto* count_label = popover.findChild<QLabel*>("LadybirdContentBlockingCount");
    VERIFY(count_label && !count_label->wordWrap());
    VERIFY(count_label->text() == QString::number(NumericLimits<u64>::max()) + " requests blocked on this page");
    snapshot.blocked_request_count = 0;
    popover.set_snapshot(snapshot);
    VERIFY(count_label->isHidden());
    snapshot.blocked_request_count = 1;
    popover.set_snapshot(snapshot);
    VERIFY(!count_label->isHidden());
    VERIFY(count_label->text() == "1 request blocked on this page");
    snapshot.blocked_request_count = NumericLimits<u64>::max();
    popover.set_snapshot(snapshot);
    bool found_exact_count = false;
    for (auto* label : popover.findChildren<QLabel*>())
        found_exact_count |= label->text().contains(QString::number(NumericLimits<u64>::max()));
    VERIFY(found_exact_count);
    parent.show();
    popover.show();
    popover.focus_switch();
    application.processEvents();
    VERIFY(count_label->width() >= count_label->fontMetrics().horizontalAdvance(count_label->text()));
    for (auto* label : popover.findChildren<QLabel*>()) {
        VERIFY(!label->wordWrap());
        VERIFY(!label->text().contains('\n'));
    }
    QKeyEvent press(QEvent::KeyPress, Qt::Key_Space, Qt::NoModifier);
    QKeyEvent release(QEvent::KeyRelease, Qt::Key_Space, Qt::NoModifier);
    QApplication::sendEvent(toggle, &press);
    QApplication::sendEvent(toggle, &release);
    VERIFY(toggles == 1);
    for (auto state : { WebView::ContentBlockingState::Unavailable, WebView::ContentBlockingState::GlobalDisabled, WebView::ContentBlockingState::CommandLineDisabled }) {
        snapshot.state = state;
        button.set_snapshot(snapshot);
        popover.set_snapshot(snapshot);
        VERIFY(button.badge_text().isEmpty());
        VERIFY(!toggle->isEnabled());
    }
    snapshot.state = WebView::ContentBlockingState::SiteDisabled;
    popover.set_snapshot(snapshot);
    VERIFY(toggle->isEnabled() && !toggle->isChecked());
    snapshot.state = WebView::ContentBlockingState::NoRules;
    snapshot.blocked_request_count = 0;
    popover.set_snapshot(snapshot);
    VERIFY(toggle->isEnabled() && toggle->isChecked());
    VERIFY(toggles == 1);
    VERIFY(count_label->isHidden());
    popover.show_error("Unable to save the site preference.");
    bool dismissed = false;
    popover.on_keyboard_dismiss = [&] { dismissed = true; button.setFocus(); };
    QKeyEvent escape(QEvent::KeyPress, Qt::Key_Escape, Qt::NoModifier);
    QApplication::sendEvent(&popover, &escape);
    VERIFY(dismissed && !popover.isVisible());
    popover.show();
    QMouseEvent outside(QEvent::MouseButtonPress, QPointF(5, 5), popover.mapToGlobal(QPoint(5, 5)), Qt::LeftButton, Qt::LeftButton, Qt::NoModifier);
    QApplication::sendEvent(&popover, &outside);
    VERIFY(!popover.isVisible());
    QPalette dark;
    dark.setColor(QPalette::Window, QColor(30, 30, 30));
    dark.setColor(QPalette::WindowText, Qt::white);
    dark.setColor(QPalette::Base, QColor(30, 30, 30));
    dark.setColor(QPalette::Text, Qt::white);
    dark.setColor(QPalette::Button, QColor(50, 50, 50));
    dark.setColor(QPalette::ButtonText, Qt::white);
    application.styleHints()->setColorScheme(Qt::ColorScheme::Dark);
    popover.update_chrome_style(dark);
    button.setPalette(dark);
    snapshot.state = WebView::ContentBlockingState::Enabled;
    snapshot.blocked_request_count = 100;
    popover.set_snapshot(snapshot);
    button.set_snapshot(snapshot);
    auto counter_palette = dark;
    auto counter_color = QColor(128, 64, 128);
    auto inactive_counter_color = QColor(20, 160, 250);
    counter_palette.setColor(QPalette::Active, QPalette::Highlight, counter_color);
    counter_palette.setColor(QPalette::Inactive, QPalette::Highlight, inactive_counter_color);
    counter_palette.setColor(QPalette::Active, QPalette::WindowText, Qt::white);
    counter_palette.setColor(QPalette::Inactive, QPalette::WindowText, Qt::blue);
    counter_palette.setColor(QPalette::Active, QPalette::Text, Qt::white);
    counter_palette.setColor(QPalette::Inactive, QPalette::Text, Qt::blue);
    button.setPalette(counter_palette);
    auto verify_counter_color = [&] {
        auto image = button.grab().toImage();
        bool found_counter_color = false;
        for (int y = 0; y < image.height() / 2; ++y) {
            for (int x = image.width() / 2; x < image.width(); ++x) {
                found_counter_color |= image.pixelColor(x, y) == counter_color;
                VERIFY(image.pixelColor(x, y) != inactive_counter_color);
            }
        }
        VERIFY(found_counter_color);
    };
    verify_counter_color();
    popover.show();
    application.processEvents();
    verify_counter_color();
    auto count_color = count_label->palette().color(QPalette::WindowText);
    counter_palette.setCurrentColorGroup(QPalette::Inactive);
    popover.update_chrome_style(counter_palette);
    VERIFY(count_label->palette().color(QPalette::WindowText) == count_color);
    VERIFY(!popover.grab().isNull());
    if (argc == 2)
        VERIFY(popover.grab().save(QString::fromLocal8Bit(argv[1])));
    return 0;
}
