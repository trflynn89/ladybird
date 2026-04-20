/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

use std::panic::{AssertUnwindSafe, catch_unwind};

/// Catch any Rust panics to prevent undefined behavior from unwinding across
/// the FFI boundary. Aborts the process on panic.
pub fn abort_on_panic<F: FnOnce() -> R, R>(f: F) -> R {
    match catch_unwind(AssertUnwindSafe(f)) {
        Ok(result) => result,
        Err(error) => {
            let msg = if let Some(s) = error.downcast_ref::<&str>() {
                s.to_string()
            } else if let Some(s) = error.downcast_ref::<String>() {
                s.clone()
            } else {
                "unknown panic".to_string()
            };
            eprintln!("Rust panic at FFI boundary: {msg}");
            std::process::abort();
        }
    }
}
