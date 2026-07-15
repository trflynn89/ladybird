/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/NonnullRefPtr.h>
#include <LibDevTools/Actor.h>
#include <LibDevTools/Forward.h>
#include <LibWeb/Forward.h>

namespace DevTools {

class DEVTOOLS_API LadybirdActor final : public Actor {
public:
    static constexpr auto base_name = "ladybird"sv;

    static NonnullRefPtr<LadybirdActor> create(DevToolsServer&, String name);
    virtual ~LadybirdActor() override;

    void request_toolbox(u64 tab_id);
    void request_element_inspection(u64 tab_id, Web::UniqueNodeID);

private:
    LadybirdActor(DevToolsServer&, String name);

    virtual void handle_message(Message const&) override;
};

}
