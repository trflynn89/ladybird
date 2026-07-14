/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibDevTools/Client/Status.h>
#include <UI/Qt/ChromeStyle.h>
#include <UI/Qt/DevToolsBanner.h>
#include <UI/Qt/StringUtils.h>

#include <QEvent>
#include <QHBoxLayout>
#include <QLabel>
#include <QPushButton>

namespace Ladybird {

DevToolsBanner::DevToolsBanner(QWidget* parent)
    : QWidget(parent)
{
    setObjectName("LadybirdDevToolsBanner");
    setAttribute(Qt::WA_StyledBackground);
    setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
    update_chrome_style();

    auto* layout = new QHBoxLayout(this);
    layout->setContentsMargins(12, 3, 12, 3);
    layout->setSpacing(6);

    m_label = new QLabel(this);
    layout->addWidget(m_label);
    layout->addStretch();

    m_launch_client_button = new QPushButton("Launch Client", this);
    connect(m_launch_client_button, &QPushButton::clicked, this, &DevToolsBanner::launch_client_requested);
    layout->addWidget(m_launch_client_button);

    auto* disable_button = new QPushButton("Disable", this);
    connect(disable_button, &QPushButton::clicked, this, &DevToolsBanner::disable_requested);
    layout->addWidget(disable_button);
}

void DevToolsBanner::set_idle_text()
{
    m_label->setText(qformatted("DevTools is enabled on port {}", m_port));
}

void DevToolsBanner::set_port(u16 port)
{
    m_port = port;
    set_idle_text();
    m_launch_client_button->setEnabled(true);
}

void DevToolsBanner::set_status(DevTools::Client::Status const& status)
{
    if (first_is_one_of(status.stage, DevTools::Client::Stage::Running, DevTools::Client::Stage::Failed)) {
        set_idle_text();
        m_launch_client_button->setEnabled(true);
    } else {
        m_label->setText(qstring_from_ak_string(DevTools::Client::pending_stage_to_string(status.stage)));
        m_launch_client_button->setEnabled(false);
    }
}

bool DevToolsBanner::event(QEvent* event)
{
    if (event->type() == QEvent::PaletteChange)
        update_chrome_style();

    return QWidget::event(event);
}

void DevToolsBanner::update_chrome_style()
{
    if (m_is_updating_chrome_style)
        return;

    m_is_updating_chrome_style = true;
    setStyleSheet(ChromeStyle::devtools_banner_style_sheet(palette()));
    m_is_updating_chrome_style = false;
}

}
