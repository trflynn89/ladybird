/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/NonnullRefPtr.h>
#include <LibDevTools/Actor.h>
#include <LibDevTools/Forward.h>

namespace DevTools {

class DEVTOOLS_API ReflowActor final : public Actor {
public:
    static constexpr auto base_name = "reflow"sv;

    static NonnullRefPtr<ReflowActor> create(DevToolsServer&, String name);
    virtual ~ReflowActor() override;

private:
    ReflowActor(DevToolsServer&, String name);

    virtual void handle_message(Message const&) override;
};

}
