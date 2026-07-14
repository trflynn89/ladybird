/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Types.h>
#include <LibDevTools/Forward.h>

#include <QWidget>

class QLabel;
class QPushButton;

namespace Ladybird {

class DevToolsBanner final : public QWidget {
    Q_OBJECT

public:
    explicit DevToolsBanner(QWidget* parent = nullptr);

    void set_port(u16 port);
    void set_status(DevTools::Client::Status const&);

signals:
    void launch_client_requested();
    void disable_requested();

private:
    virtual bool event(QEvent*) override;

    void update_chrome_style();
    void set_idle_text();

    QLabel* m_label { nullptr };
    QPushButton* m_launch_client_button { nullptr };
    u16 m_port { 0 };
    bool m_is_updating_chrome_style { false };
};

}
