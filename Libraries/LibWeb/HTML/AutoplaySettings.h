/*
 * Copyright (c) 2023-2025, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Types.h>
#include <LibURL/Forward.h>
#include <LibWeb/Export.h>
#include <LibWeb/Forward.h>

namespace Web::HTML {

enum class AutoplayDecision : u8 {
    Allowed,
    AllowedIfInaudible,
    Blocked,
};

WEB_API AutoplayDecision autoplay_decision_for_origin(DOM::Document const&, URL::Origin const&);

}
