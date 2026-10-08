/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <LibIPC/Decoder.h>
#include <LibIPC/Encoder.h>
#include <LibWebCommon/Loader/ContentBlockingRequestContext.h>

namespace IPC {

template<>
ErrorOr<void> encode(Encoder& encoder, Web::ContentBlockingRequestContext const& context)
{
    TRY(encoder.encode(context.load_id));
    TRY(encoder.encode(context.navigable_id));
    TRY(encoder.encode(context.environment_id));
    TRY(encoder.encode(context.navigation_id));
    TRY(encoder.encode(context.is_navigation));
    return {};
}

template<>
ErrorOr<Web::ContentBlockingRequestContext> decode(Decoder& decoder)
{
    return Web::ContentBlockingRequestContext {
        .load_id = TRY(decoder.decode<u64>()),
        .navigable_id = TRY(decoder.decode<Web::HTML::CrossProcessId>()),
        .environment_id = TRY(decoder.decode<Optional<Web::HTML::EnvironmentId>>()),
        .navigation_id = TRY(decoder.decode<Optional<Utf16String>>()),
        .is_navigation = TRY(decoder.decode<bool>()),
    };
}

}
