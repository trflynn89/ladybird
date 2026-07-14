/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/Format.h>
#include <AK/StringView.h>
#include <LibArchive/TarExtractor.h>
#include <RustFFI.h>

namespace Archive {

ErrorOr<void> TarExtractor::extract(StringView archive_path, StringView destination)
{
    if (!FFI::archive_rust_extract_tar_xz(
            archive_path.bytes().data(), archive_path.length(),
            destination.bytes().data(), destination.length())) {
        return Error::from_string_literal("Failed to extract tar archive");
    }

    return {};
}

}
