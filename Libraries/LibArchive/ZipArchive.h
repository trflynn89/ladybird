/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#pragma once

#include <AK/ByteBuffer.h>
#include <AK/Error.h>
#include <AK/Optional.h>
#include <AK/Span.h>
#include <AK/StringView.h>

namespace Archive {

class ZipArchive {
public:
    struct Patch {
        StringView archive_path;
        ReadonlyBytes contents;
    };

    static ErrorOr<void> install_patches(StringView archive_path, ReadonlySpan<Patch> patches);
    static ErrorOr<Optional<ByteBuffer>> read_member(StringView archive_path, StringView member_path);
};

}
