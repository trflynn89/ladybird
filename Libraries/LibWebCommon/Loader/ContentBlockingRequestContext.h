/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/Optional.h>
#include <AK/Utf16String.h>
#include <LibIPC/Forward.h>
#include <LibWebCommon/Export.h>
#include <LibWebCommon/HTML/CrossProcessId.h>
#include <LibWebCommon/HTML/Scripting/EnvironmentId.h>

namespace Web {

// Captured when a request is created, independently of its matching initiator URL.
struct ContentBlockingRequestContext {
    u64 load_id { 0 };
    HTML::CrossProcessId navigable_id;
    Optional<HTML::EnvironmentId> environment_id;
    Optional<Utf16String> navigation_id;
    bool is_navigation { false };
};

}

namespace IPC {

template<>
WEBCOMMON_API ErrorOr<void> encode(Encoder&, Web::ContentBlockingRequestContext const&);
template<>
WEBCOMMON_API ErrorOr<Web::ContentBlockingRequestContext> decode(Decoder&);

}
