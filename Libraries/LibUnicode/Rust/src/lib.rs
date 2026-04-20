/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

#[path = "../../../AbortOnPanic.rs"]
mod abort_on_panic;

#[cfg(feature = "allocator")]
#[path = "../../../RustAllocator.rs"]
mod rust_allocator;

pub mod calendar;
pub mod character_types;
pub mod normalize;
pub mod segmenter;
