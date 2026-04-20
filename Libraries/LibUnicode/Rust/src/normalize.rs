/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

use crate::abort_on_panic::abort_on_panic;
use icu_normalizer::{ComposingNormalizer, DecomposingNormalizer};
use std::ffi::c_void;
use std::fmt;

#[repr(u8)]
enum NormalizationForm {
    NFD = 0,
    NFC = 1,
    NFKD = 2,
    NFKC = 3,
}

impl NormalizationForm {
    fn from_u8(value: u8) -> Option<Self> {
        match value {
            0 => Some(Self::NFD),
            1 => Some(Self::NFC),
            2 => Some(Self::NFKD),
            3 => Some(Self::NFKC),
            _ => None,
        }
    }
}

pub type NormalizeUtf8WriteCallback = unsafe extern "C" fn(context: *mut c_void, bytes: *const u8, len: usize) -> bool;

struct FfiWriteContext {
    context: *mut c_void,
    callback: NormalizeUtf8WriteCallback,
}

impl fmt::Write for FfiWriteContext {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        let wrote = unsafe { (self.callback)(self.context, s.as_ptr(), s.len()) };
        if wrote { Ok(()) } else { Err(fmt::Error) }
    }
}

/// # Safety
/// `input` must point to `input_length` bytes. `context` is passed through unchanged and
/// `callback` must accept UTF-8 chunks synchronously for the duration of the call.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn normalize(
    input: *const u8,
    input_length: usize,
    form: u8,
    context: *mut c_void,
    callback: NormalizeUtf8WriteCallback,
) -> bool {
    abort_on_panic(|| {
        let input = unsafe { std::slice::from_raw_parts(input, input_length) };

        let Some(form) = NormalizationForm::from_u8(form) else {
            return false;
        };

        let mut sink = FfiWriteContext { context, callback };

        let result = match form {
            NormalizationForm::NFD => DecomposingNormalizer::new_nfd().normalize_utf8_to(input, &mut sink),
            NormalizationForm::NFC => ComposingNormalizer::new_nfc().normalize_utf8_to(input, &mut sink),
            NormalizationForm::NFKD => DecomposingNormalizer::new_nfkd().normalize_utf8_to(input, &mut sink),
            NormalizationForm::NFKC => ComposingNormalizer::new_nfkc().normalize_utf8_to(input, &mut sink),
        };

        result.is_ok()
    })
}
