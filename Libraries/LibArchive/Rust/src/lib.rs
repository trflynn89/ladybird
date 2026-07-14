/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#[cfg(feature = "allocator")]
#[path = "../../../RustAllocator.rs"]
mod rust_allocator;

use lzma_rust2::XzReader;
use std::fs::File;
use std::panic::AssertUnwindSafe;
use std::panic::catch_unwind;
use std::path::Path;
use tar::Archive;

fn abort_on_panic<F: FnOnce() -> R, R>(f: F) -> R {
    match catch_unwind(AssertUnwindSafe(f)) {
        Ok(result) => result,
        Err(payload) => {
            let message = if let Some(message) = payload.downcast_ref::<&str>() {
                (*message).to_string()
            } else if let Some(message) = payload.downcast_ref::<String>() {
                message.clone()
            } else {
                "unknown panic".to_string()
            };
            eprintln!("Rust panic at FFI boundary: {message}");
            std::process::abort();
        }
    }
}

/// SAFETY: `data` must be valid for `length` bytes.
fn path_from_ffi<'a>(data: *const u8, length: usize) -> &'a Path {
    let bytes = unsafe { std::slice::from_raw_parts(data, length) };
    // FIXME: UTF-8 is most certainly not right for Windows.
    let path = std::str::from_utf8(bytes).unwrap();
    Path::new(path)
}

fn report_result(result: Result<(), String>) -> bool {
    match result {
        Ok(()) => true,
        Err(error) => {
            eprintln!("Archive operation failed: {error}");
            false
        }
    }
}

fn extract_tar_xz(archive_path: &Path, destination: &Path) -> Result<(), String> {
    let archive =
        File::open(archive_path).map_err(|error| format!("Failed to open {}: {error}", archive_path.display()))?;
    let decoder = XzReader::new(archive, false);
    Archive::new(decoder)
        .unpack(destination)
        .map_err(|error| format!("Failed to extract {}: {error}", archive_path.display()))
}

/// Extract an XZ-compressed TAR archive into a destination directory.
///
/// # Safety
/// - `archive_path` and `destination` must each be valid byte slices containing UTF-8 paths.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn archive_rust_extract_tar_xz(
    archive_path: *const u8,
    archive_path_length: usize,
    destination: *const u8,
    destination_length: usize,
) -> bool {
    abort_on_panic(|| {
        let archive_path = path_from_ffi(archive_path, archive_path_length);
        let destination = path_from_ffi(destination, destination_length);

        let result = extract_tar_xz(archive_path, destination);
        report_result(result)
    })
}
