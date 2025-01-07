/*
 * Copyright (c) 2024, Tim Flynn <trflynn89@ladybird.org>
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <LibTextCodec/Decoder.h>
#include <LibTextCodec/Encoder.h>

namespace Web::Encoding {

enum class ProcessMode {
    Replacement,
    HTML,
    Fatal,
};
void process_item(u32 item, TextCodec::UTF8Encoder&, ReadonlySpan<u16>, Vector<u8>&, ProcessMode);
void process_item(u32 item, TextCodec::UTF8Decoder&, ReadonlySpan<u16>, Vector<u8>&, ProcessMode);

}
