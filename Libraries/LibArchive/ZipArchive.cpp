/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#include <AK/Format.h>
#include <AK/Vector.h>
#include <LibArchive/ZipArchive.h>
#include <RustFFI.h>

namespace Archive {

ErrorOr<void> ZipArchive::install_patches(StringView archive_path, ReadonlySpan<Patch> patches)
{
    Vector<FFI::ArchiveRustZipPatch> rust_patches;
    rust_patches.ensure_capacity(patches.size());

    for (auto const& patch : patches) {
        rust_patches.unchecked_append({
            .archive_path = patch.archive_path.bytes().data(),
            .archive_path_length = patch.archive_path.length(),
            .contents = patch.contents.data(),
            .contents_length = patch.contents.size(),
        });
    }

    if (!FFI::archive_rust_install_zip_patches(
            archive_path.bytes().data(), archive_path.length(),
            rust_patches.data(), rust_patches.size())) {
        return Error::from_string_literal("Failed to install ZIP archive patches");
    }

    return {};
}

static void append_member_data(void* context, u8 const* data, size_t data_length)
{
    auto& contents = *static_cast<ByteBuffer*>(context);
    contents.append({ data, data_length });
}

ErrorOr<Optional<ByteBuffer>> ZipArchive::read_member(StringView archive_path, StringView member_path)
{
    ByteBuffer contents;
    bool found = false;

    if (!FFI::archive_rust_read_zip_member(
            archive_path.bytes().data(), archive_path.length(),
            member_path.bytes().data(), member_path.length(),
            &found,
            &contents,
            append_member_data)) {
        return Error::from_string_literal("Failed to read ZIP archive member");
    }

    if (!found)
        return OptionalNone {};
    return contents;
}

}
