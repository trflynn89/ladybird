/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

use adblock::lists::{FilterSet, ParseOptions};
use adblock::request::Request;
use adblock::Engine;
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::slice;

pub struct AdblockEngine(Engine);

fn abort_on_panic<F: FnOnce() -> R, R>(f: F) -> R {
    match catch_unwind(AssertUnwindSafe(f)) {
        Ok(result) => result,
        Err(_) => std::process::abort(),
    }
}

/// Create an adblock engine from a newline-delimited filter list (ABP/EasyList format).
/// Returns an opaque engine handle, or null on failure.
///
/// # Safety
/// `filter_list` must point to `filter_list_length` valid UTF-8 bytes.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn adblock_engine_create(
    filter_list: *const u8,
    filter_list_length: usize,
) -> *mut AdblockEngine {
    abort_on_panic(|| {
        let bytes = unsafe { slice::from_raw_parts(filter_list, filter_list_length) };
        let list_str = std::str::from_utf8(bytes).unwrap_or("");

        let rules: Vec<String> = list_str
            .lines()
            .filter(|line| !line.is_empty())
            .map(String::from)
            .collect();

        let mut filter_set = FilterSet::new(false);
        filter_set.add_filters(&rules, ParseOptions::default());
        let engine = Engine::from_filter_set(filter_set, true);

        Box::into_raw(Box::new(AdblockEngine(engine)))
    })
}

/// Free an adblock engine created by `adblock_engine_create`.
///
/// # Safety
/// `engine` must be a valid pointer returned by `adblock_engine_create`, or null.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn adblock_engine_free(engine: *mut AdblockEngine) {
    if !engine.is_null() {
        drop(unsafe { Box::from_raw(engine) });
    }
}

/// Check whether a URL should be blocked by the adblock engine.
///
/// # Safety
/// - `engine` must be a valid pointer returned by `adblock_engine_create`.
/// - `url` must point to `url_length` valid UTF-8 bytes.
/// - `source_url` must point to `source_url_length` valid UTF-8 bytes, or be null.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn adblock_engine_is_filtered(
    engine: *const AdblockEngine,
    url: *const u8,
    url_length: usize,
    source_url: *const u8,
    source_url_length: usize,
) -> bool {
    if engine.is_null() || url.is_null() {
        return false;
    }

    abort_on_panic(|| {
        let engine = unsafe { &*engine };

        let url_bytes = unsafe { slice::from_raw_parts(url, url_length) };
        let url_str = std::str::from_utf8(url_bytes).unwrap_or("");

        let source_url_str = if source_url.is_null() || source_url_length == 0 {
            ""
        } else {
            let bytes = unsafe { slice::from_raw_parts(source_url, source_url_length) };
            std::str::from_utf8(bytes).unwrap_or("")
        };

        let Ok(request) = Request::new(url_str, source_url_str, "other") else {
            return false;
        };

        engine.0.check_network_request(&request).matched
    })
}
