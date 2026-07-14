/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#[cfg(feature = "allocator")]
#[path = "../../../RustAllocator.rs"]
mod rust_allocator;

mod zip;

use lzma_rust2::XzReader;
use std::ffi::c_void;
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
fn bytes_from_ffi<'a>(data: *const u8, length: usize) -> &'a [u8] {
    unsafe { std::slice::from_raw_parts(data, length) }
}

/// SAFETY: `data` must be valid for `length` bytes.
fn string_from_ffi<'a>(data: *const u8, length: usize) -> &'a str {
    let bytes = bytes_from_ffi(data, length);
    unsafe { std::str::from_utf8_unchecked(bytes) }
}

/// SAFETY: `data` must be valid for `length` bytes.
fn path_from_ffi<'a>(data: *const u8, length: usize) -> &'a Path {
    let bytes = bytes_from_ffi(data, length);
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

type DataCallback = unsafe extern "C" fn(context: *mut c_void, data: *const u8, data_length: usize);

#[repr(C)]
pub struct ArchiveRustZipPatch {
    archive_path: *const u8,
    archive_path_length: usize,
    contents: *const u8,
    contents_length: usize,
}

/// SAFETY: All FFI patch inputs are non-null.
fn zip_patches_from_ffi<'a>(patches: *const ArchiveRustZipPatch, patch_count: usize) -> Vec<zip::Patch<'a>> {
    let patches = if patch_count == 0 {
        &[]
    } else {
        unsafe { std::slice::from_raw_parts(patches, patch_count) }
    };

    patches
        .iter()
        .map(|patch| zip::Patch {
            archive_path: string_from_ffi(patch.archive_path, patch.archive_path_length),
            contents: bytes_from_ffi(patch.contents, patch.contents_length),
        })
        .collect::<Vec<zip::Patch>>()
}

/// Install a set of patches into a standard or Firefox-optimized ZIP archive.
///
/// # Safety
/// - `archive_path` must be a valid byte slice containing a UTF-8 path.
/// - `patches` must point to `patch_count` valid patch descriptions.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn archive_rust_install_zip_patches(
    archive_path: *const u8,
    archive_path_length: usize,
    patches: *const ArchiveRustZipPatch,
    patch_count: usize,
) -> bool {
    abort_on_panic(|| {
        let archive_path = path_from_ffi(archive_path, archive_path_length);
        let patches = zip_patches_from_ffi(patches, patch_count);

        let result = zip::install_patches(archive_path, &patches);
        report_result(result)
    })
}

/// Read a member from a standard or Firefox-optimized ZIP archive.
///
/// # Safety
/// - `archive_path` and `member_path` must each be valid byte slices containing UTF-8 strings.
/// - `found` must be a valid writable pointer.
/// - `on_data` must not retain data beyond the duration of their callbacks.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn archive_rust_read_zip_member(
    archive_path: *const u8,
    archive_path_length: usize,
    member_path: *const u8,
    member_path_length: usize,
    found: *mut bool,
    context: *mut c_void,
    on_data: DataCallback,
) -> bool {
    abort_on_panic(|| {
        if found.is_null() {
            return report_result(Err("ZIP member result pointer is null".to_string()));
        }

        let archive_path = path_from_ffi(archive_path, archive_path_length);
        let member_path = string_from_ffi(member_path, member_path_length);

        match zip::read_member(archive_path, member_path) {
            Ok(Some(contents)) => {
                unsafe { *found = true };
                unsafe { on_data(context, contents.as_ptr(), contents.len()) }
                true
            }
            Ok(None) => {
                unsafe { *found = false };
                true
            }
            Err(error) => report_result(Err(error)),
        }
    })
}
