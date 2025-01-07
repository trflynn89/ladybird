/*
 * Copyright (c) 2024, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/CharacterTypes.h>
#include <AK/Utf16View.h>
#include <LibWeb/Encoding/AbstractOperations.h>

namespace Web::Encoding {

void process_item(u32 item, TextCodec::UTF8Encoder& encoder, ReadonlySpan<u16>, Vector<u8>&, ProcessMode mode)
{
    auto item_string = String::from_code_point(item);

    // 1. Assert: if encoderDecoder is an encoder instance, mode is not "replacement".
    VERIFY(mode != ProcessMode::Replacement);

    // 2. Assert: if encoderDecoder is a decoder instance, mode is not "html".

    // 3. Assert: if encoderDecoder is an encoder instance, item is not a surrogate.
    VERIFY(!is_unicode_surrogate(item));

    // 4. Let result be the result of running encoderDecoder’s handler on input and item.
    Vector<u8> result;

    MUST(encoder.process(
        item_string.code_points(),
        [&](u8 byte) -> ErrorOr<void> {
            result.append(byte);
            return {};
        },
        nullptr));

    // 5. If result is finished:
    //     1. Push end-of-queue to output.
    //     2. Return result.

    // 6. Otherwise, if result is one or more items:
    //     Assert: if encoderDecoder is a decoder instance, result does not contain any surrogates.
    //     Push result to output.

    // 7. Otherwise, if result is an error, switch on mode and run the associated steps:
    // "replacement"
    //     Push U+FFFD (�) to output.
    // "html"
    //     Push 0x26 (&), 0x23 (#), followed by the shortest sequence of 0x30 (0) to 0x39 (9), inclusive, representing result’s code point’s value in base ten, followed by 0x3B (;) to output.
    // "fatal"
    //     Return result.

    // Return continue.
}

}
