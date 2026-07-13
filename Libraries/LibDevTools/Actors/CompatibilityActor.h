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

class DEVTOOLS_API CompatibilityActor final : public Actor {
public:
    static constexpr auto base_name = "compatibility"sv;

    static NonnullRefPtr<CompatibilityActor> create(DevToolsServer&, String name);
    virtual ~CompatibilityActor() override;

    JsonObject serialize_description() const;

private:
    CompatibilityActor(DevToolsServer&, String name);

    virtual void handle_message(Message const&) override;
};

}
