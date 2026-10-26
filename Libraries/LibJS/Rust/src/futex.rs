/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

//! JavaScript and WebAssembly use LibCore's common FIFO waiter registry, including across dynamic library boundaries.
//! Platform blocking primitives do not select JavaScript waiters or determine notification counts.

use core::ffi::c_void;
use core::sync::atomic::{AtomicU32, AtomicU64};
use core::time::Duration;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum AtomicWaitResult {
    Woken,
    NotEqual,
    TimedOut,
}

impl AtomicWaitResult {
    pub(crate) fn from_raw(result: u8) -> Self {
        match result {
            0 => Self::Woken,
            1 => Self::NotEqual,
            2 => Self::TimedOut,
            _ => unreachable!("LibCore returns a valid AtomicWaitResult"),
        }
    }
}

#[derive(Clone, Copy)]
pub(crate) enum WaitWord<'a> {
    Word32(&'a AtomicU32),
    Word64(&'a AtomicU64),
}

unsafe extern "C" {
    pub(crate) fn core_atomic_wait(
        address: *mut c_void,
        expected: u64,
        size: usize,
        timeout_ns: i64,
        cancelled: Option<unsafe extern "C" fn(*mut c_void) -> bool>,
        context: *mut c_void,
        object_id: u64,
        byte_index: usize,
    ) -> u8;
    pub(crate) fn core_atomic_notify(
        address: *mut c_void,
        max_count: usize,
        object_id: u64,
        byte_index: usize,
    ) -> usize;
}

pub(crate) fn atomic_wait(word: WaitWord<'_>, expected: u64, timeout: Option<Duration>) -> AtomicWaitResult {
    atomic_wait_at(word, expected, timeout, 0, 0)
}

pub(crate) fn atomic_wait_at(
    word: WaitWord<'_>,
    expected: u64,
    timeout: Option<Duration>,
    object_id: u64,
    byte_index: usize,
) -> AtomicWaitResult {
    let (address, size) = match word {
        WaitWord::Word32(word) => (word.as_ptr().cast::<c_void>(), 4),
        WaitWord::Word64(word) => (word.as_ptr().cast::<c_void>(), 8),
    };
    let timeout_ns = timeout.map_or(-1, |timeout| i64::try_from(timeout.as_nanos()).unwrap_or(i64::MAX));
    // SAFETY: The atomic word is live, aligned for its width, and stays borrowed until the wait returns.
    let result = unsafe {
        core_atomic_wait(
            address,
            expected,
            size,
            timeout_ns,
            None,
            core::ptr::null_mut(),
            object_id,
            byte_index,
        )
    };
    AtomicWaitResult::from_raw(result)
}

pub(crate) fn atomic_notify(word: &AtomicU32, max_count: usize) -> usize {
    atomic_notify_at(word, max_count, 0, 0)
}

pub(crate) fn atomic_notify_at(word: &AtomicU32, max_count: usize, object_id: u64, byte_index: usize) -> usize {
    // SAFETY: The aligned atomic word stays live throughout the call. Notification never reads or writes its value.
    unsafe { core_atomic_notify(word.as_ptr().cast(), max_count, object_id, byte_index) }
}
