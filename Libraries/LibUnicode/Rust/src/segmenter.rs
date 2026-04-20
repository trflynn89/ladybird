/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

use crate::abort_on_panic::abort_on_panic;
use icu_locale_core::Locale;
use icu_segmenter::options::{
    LineBreakOptions, SentenceBreakInvariantOptions, SentenceBreakOptions, WordBreakInvariantOptions, WordBreakOptions,
};
use icu_segmenter::{GraphemeClusterSegmenter, LineSegmenter, SentenceSegmenter, WordSegmenter};
use std::ffi::c_void;

#[repr(u8)]
enum SegmenterGranularity {
    Grapheme = 0,
    Line = 1,
    Sentence = 2,
    Word = 3,
}

impl SegmenterGranularity {
    fn from_u8(value: u8) -> Option<Self> {
        match value {
            0 => Some(Self::Grapheme),
            1 => Some(Self::Line),
            2 => Some(Self::Sentence),
            3 => Some(Self::Word),
            _ => None,
        }
    }
}

enum SegmenterKind {
    Grapheme(GraphemeClusterSegmenter),
    Line(LineSegmenter),
    Sentence(SentenceSegmenter),
    Word(WordSegmenter),
}

pub struct IcuSegmenter {
    kind: SegmenterKind,
}

pub type SegmenterBoundaryCallback =
    unsafe extern "C" fn(context: *mut c_void, boundary: usize, word_like: bool) -> bool;

fn create_segmenter(granularity: SegmenterGranularity, locale: Option<&Locale>) -> Option<SegmenterKind> {
    match granularity {
        SegmenterGranularity::Grapheme => Some(SegmenterKind::Grapheme(
            GraphemeClusterSegmenter::new().static_to_owned(),
        )),
        SegmenterGranularity::Line => {
            let mut options = LineBreakOptions::default();
            if let Some(locale) = locale {
                options.content_locale = Some(&locale.id);
            }
            Some(SegmenterKind::Line(LineSegmenter::new_auto(options).static_to_owned()))
        }
        SegmenterGranularity::Sentence => {
            if let Some(locale) = locale {
                let mut options = SentenceBreakOptions::default();
                options.content_locale = Some(&locale.id);
                SentenceSegmenter::try_new(options).ok().map(SegmenterKind::Sentence)
            } else {
                Some(SegmenterKind::Sentence(
                    SentenceSegmenter::new(SentenceBreakInvariantOptions::default()).static_to_owned(),
                ))
            }
        }
        SegmenterGranularity::Word => {
            if let Some(locale) = locale {
                let mut options = WordBreakOptions::default();
                options.content_locale = Some(&locale.id);
                WordSegmenter::try_new_auto(options).ok().map(SegmenterKind::Word)
            } else {
                Some(SegmenterKind::Word(
                    WordSegmenter::new_auto(WordBreakInvariantOptions::default()).static_to_owned(),
                ))
            }
        }
    }
}

unsafe fn utf8_input<'a>(input: *const u8, input_length: usize) -> Option<&'a [u8]> {
    if input_length == 0 {
        return Some(&[]);
    }
    if input.is_null() {
        return None;
    }
    Some(unsafe { std::slice::from_raw_parts(input, input_length) })
}

unsafe fn utf16_input<'a>(input: *const u16, input_length: usize) -> Option<&'a [u16]> {
    if input_length == 0 {
        return Some(&[]);
    }
    if input.is_null() {
        return None;
    }
    Some(unsafe { std::slice::from_raw_parts(input, input_length) })
}

unsafe fn parse_locale(locale: *const u8, locale_length: usize) -> Option<Locale> {
    if locale_length == 0 {
        return None;
    }

    if locale.is_null() {
        return None;
    }

    let locale = unsafe { std::slice::from_raw_parts(locale, locale_length) };
    let locale = std::str::from_utf8(locale).ok()?;
    locale.parse().ok()
}

unsafe fn emit_boundary(
    callback: SegmenterBoundaryCallback,
    context: *mut c_void,
    boundary: usize,
    word_like: bool,
) -> bool {
    unsafe { callback(context, boundary, word_like) }
}

/// # Safety
/// `locale` must point to `locale_length` bytes when `locale_length > 0`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn icu_segmenter_create(
    granularity: u8,
    locale: *const u8,
    locale_length: usize,
) -> *mut IcuSegmenter {
    abort_on_panic(|| {
        let Some(granularity) = SegmenterGranularity::from_u8(granularity) else {
            return std::ptr::null_mut();
        };

        let locale = unsafe { parse_locale(locale, locale_length) };
        let Some(kind) = create_segmenter(granularity, locale.as_ref()) else {
            return std::ptr::null_mut();
        };

        Box::into_raw(Box::new(IcuSegmenter { kind }))
    })
}

/// # Safety
/// `segmenter` must either be null or a pointer returned by `icu_segmenter_create`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn icu_segmenter_destroy(segmenter: *mut IcuSegmenter) {
    abort_on_panic(|| {
        if segmenter.is_null() {
            return;
        }

        drop(unsafe { Box::from_raw(segmenter) });
    })
}

/// # Safety
/// `segmenter` must be a valid pointer returned by `icu_segmenter_create`. `input` must point to
/// `input_length` bytes when `input_length > 0`. `callback` must synchronously accept each boundary
/// for the duration of the call.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn icu_segmenter_segment_utf8(
    segmenter: *const IcuSegmenter,
    input: *const u8,
    input_length: usize,
    context: *mut c_void,
    callback: SegmenterBoundaryCallback,
) -> bool {
    abort_on_panic(|| {
        let Some(segmenter) = (unsafe { segmenter.as_ref() }) else {
            return false;
        };
        let Some(input) = (unsafe { utf8_input(input, input_length) }) else {
            return false;
        };

        match &segmenter.kind {
            SegmenterKind::Grapheme(segmenter) => {
                for boundary in segmenter.as_borrowed().segment_utf8(input) {
                    if !unsafe { emit_boundary(callback, context, boundary, false) } {
                        return false;
                    }
                }
            }
            SegmenterKind::Line(segmenter) => {
                for boundary in segmenter.as_borrowed().segment_utf8(input) {
                    if !unsafe { emit_boundary(callback, context, boundary, false) } {
                        return false;
                    }
                }
            }
            SegmenterKind::Sentence(segmenter) => {
                for boundary in segmenter.as_borrowed().segment_utf8(input) {
                    if !unsafe { emit_boundary(callback, context, boundary, false) } {
                        return false;
                    }
                }
            }
            SegmenterKind::Word(segmenter) => {
                let mut iterator = segmenter.as_borrowed().segment_utf8(input);
                while let Some(boundary) = iterator.next() {
                    if !unsafe { emit_boundary(callback, context, boundary, iterator.is_word_like()) } {
                        return false;
                    }
                }
            }
        }

        true
    })
}

/// # Safety
/// `segmenter` must be a valid pointer returned by `icu_segmenter_create`. `input` must point to
/// `input_length` UTF-16 code units when `input_length > 0`. `callback` must synchronously accept
/// each boundary for the duration of the call.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn icu_segmenter_segment_utf16(
    segmenter: *const IcuSegmenter,
    input: *const u16,
    input_length: usize,
    context: *mut c_void,
    callback: SegmenterBoundaryCallback,
) -> bool {
    abort_on_panic(|| {
        let Some(segmenter) = (unsafe { segmenter.as_ref() }) else {
            return false;
        };
        let Some(input) = (unsafe { utf16_input(input, input_length) }) else {
            return false;
        };

        match &segmenter.kind {
            SegmenterKind::Grapheme(segmenter) => {
                for boundary in segmenter.as_borrowed().segment_utf16(input) {
                    if !unsafe { emit_boundary(callback, context, boundary, false) } {
                        return false;
                    }
                }
            }
            SegmenterKind::Line(segmenter) => {
                for boundary in segmenter.as_borrowed().segment_utf16(input) {
                    if !unsafe { emit_boundary(callback, context, boundary, false) } {
                        return false;
                    }
                }
            }
            SegmenterKind::Sentence(segmenter) => {
                for boundary in segmenter.as_borrowed().segment_utf16(input) {
                    if !unsafe { emit_boundary(callback, context, boundary, false) } {
                        return false;
                    }
                }
            }
            SegmenterKind::Word(segmenter) => {
                let mut iterator = segmenter.as_borrowed().segment_utf16(input);
                while let Some(boundary) = iterator.next() {
                    if !unsafe { emit_boundary(callback, context, boundary, iterator.is_word_like()) } {
                        return false;
                    }
                }
            }
        }

        true
    })
}
