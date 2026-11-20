/*
 * Copyright (c) 2026-present, the Ladybird developers.
 *
 * SPDX-License-Identifier: BSD-2-Clause
 */

use super::*;
use std::borrow::Cow;

unsafe extern "C" {
    fn unicode_bidi_find_paragraph_separator(text: *const u16, length: usize) -> usize;
    fn unicode_bidi_resolve_paragraph(
        text: *const u16,
        length: i32,
        base_level: u8,
        first_strong: bool,
        levels: *mut u8,
    ) -> u8;
    fn unicode_bidi_reorder_line(paragraph_levels: *const u8, length: i32, base_level: u8, indices: *mut i32) -> i32;
}

// The level encoding from LibUnicode/Bidi.h. Layout uses the tags to split shaping runs.
const RESETS_AT_LINE_END: u8 = 0x80;
const LEVEL_MASK: u8 = !RESETS_AT_LINE_END;
pub(super) const PARAGRAPH_SEPARATOR_LEVEL: u8 = 0xff;

pub(super) fn direction_of_level(level: u8) -> u8 {
    if level & 1 == 0 { direction::LTR } else { direction::RTL }
}

pub(super) fn preserves_newlines(collapse: u8) -> bool {
    matches!(
        collapse,
        white_space_collapse::PRESERVE | white_space_collapse::PRESERVE_BREAKS | white_space_collapse::BREAK_SPACES
    )
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) struct Position {
    pub(super) paragraph: usize,
    pub(super) offset: usize,
}

#[derive(Debug, PartialEq, Eq)]
pub(super) struct Paragraph {
    levels: Box<[u8]>,
    pub(super) base_level: u8,
    pub(super) is_unidirectional_ltr: bool,
}

impl Paragraph {
    pub(super) fn level_at(&self, offset: usize) -> u8 {
        self.levels[offset]
    }

    pub(super) fn shaping_level_at(&self, offset: usize) -> u8 {
        let level = self.level_at(offset);
        if level & LEVEL_MASK == self.base_level {
            self.base_level
        } else {
            level
        }
    }

    /// The visual index of each UTF-16 code unit on a line, including the controls between styled runs.
    pub(super) fn visual_indices(&self, range: std::ops::Range<usize>) -> (Vec<i32>, usize) {
        let start = range.start;
        let paragraph_levels = &self.levels[range];
        let mut indices = vec![0; paragraph_levels.len()];
        let length = i32::try_from(paragraph_levels.len()).expect("a bidi line fits in its paragraph");
        // SAFETY: Both buffers contain length elements and the call only uses them synchronously.
        let trailing_start = unsafe {
            unicode_bidi_reorder_line(paragraph_levels.as_ptr(), length, self.base_level, indices.as_mut_ptr())
        };
        (indices, start + trailing_start as usize)
    }
}

#[derive(Clone, Copy)]
struct NodeRange {
    start: usize,
    length: usize,
    position: Position,
}

#[derive(Default)]
pub(super) struct InlineContent {
    pub(super) paragraphs: Vec<Paragraph>,
    nodes: HashMap<Node, Vec<NodeRange>>,
}

impl InlineContent {
    pub(super) fn position(&self, node: Node, offset: usize) -> Option<Position> {
        let ranges = self.nodes.get(&node)?;
        let range = ranges[ranges.partition_point(|range| range.start <= offset).checked_sub(1)?];
        (offset < range.start + range.length).then(|| Position {
            offset: range.position.offset + offset - range.start,
            ..range.position
        })
    }

    pub(super) fn levels_for_node(&self, node: Node) -> Option<Cow<'_, [u8]>> {
        let ranges = self.nodes.get(&node)?;
        if let [range] = ranges.as_slice() {
            let paragraph = &self.paragraphs[range.position.paragraph];
            let levels = &paragraph.levels[range.position.offset..][..range.length];
            if !levels
                .iter()
                .any(|&level| level == paragraph.base_level | RESETS_AT_LINE_END)
            {
                return Some(Cow::Borrowed(levels));
            }
        }
        Some(Cow::Owned(
            ranges
                .iter()
                .flat_map(|range| {
                    let paragraph = &self.paragraphs[range.position.paragraph];
                    (range.position.offset..range.position.offset + range.length)
                        .map(move |offset| paragraph.shaping_level_at(offset))
                })
                .collect(),
        ))
    }

    pub(super) fn resolve(pass: &LayoutPass<'_>, block: Node) -> Self {
        let style = StyleValues::for_node(pass, block);
        let mut builder = ParagraphBuilder {
            content: Self::default(),
            text: Vec::new(),
            controls: Vec::new(),
            base_level: u8::from(style.direction() == direction::RTL),
            first_strong: style.unicode_bidi() == unicode_bidi::PLAINTEXT,
        };
        if matches!(
            style.unicode_bidi(),
            unicode_bidi::BIDI_OVERRIDE | unicode_bidi::ISOLATE_OVERRIDE
        ) {
            builder.enter(
                block,
                Controls::for_style(unicode_bidi::BIDI_OVERRIDE, style.direction()),
            );
        }

        let mut node = pass.first_child(block);
        while !node.is_invalid() {
            let facts = NodeFacts::new(pass, node);
            if facts.is_text_node() {
                let style = StyleValues::for_node(pass, pass.parent(node));
                let text = &pass.text_content(node).text;
                builder.append_text(node, text, preserves_newlines(style.white_space_collapse()));
            } else if facts.is_floating_or_absolutely_positioned() || facts.is_svg_mask_box() {
                // Out-of-flow boxes do not affect the direction of the surrounding text.
            } else if facts.is_break_node() {
                builder.end_paragraph(Some((node, 0, &[u16::from(b'\n')])));
            } else if facts.is_inline_flow_interrupting_block() {
                builder.end_paragraph(None);
            } else if facts.is_word_break_opportunity_node() {
                builder.text.push(0x200b); // ZERO WIDTH SPACE
            } else if facts.is_fragmented_inline() {
                let style = StyleValues::for_node(pass, node);
                builder.enter(node, Controls::for_style(style.unicode_bidi(), style.direction()));
            } else if facts.is_box()
                && (!facts.is_list_item_marker_box()
                    || facts.list_marker_is_inside()
                    || NodeFacts::new(pass, pass.parent(node)).is_fragmented_inline())
            {
                let style = StyleValues::for_node(pass, node);
                let character = if facts.is_replaced_box()
                    && matches!(style.unicode_bidi(), unicode_bidi::EMBED | unicode_bidi::BIDI_OVERRIDE)
                {
                    if style.direction() == direction::RTL {
                        0x200f
                    } else {
                        0x200e
                    }
                } else {
                    0xfffc // OBJECT REPLACEMENT CHARACTER
                };
                builder.append(node, 0, &[character]);
            }

            let child = first_inline_child(pass, node);
            if !child.is_invalid() && !facts.is_svg_mask_box() {
                node = child;
                continue;
            }
            loop {
                builder.leave(node);
                let sibling = pass.next_sibling(node);
                if !sibling.is_invalid() {
                    node = sibling;
                    break;
                }
                node = pass.parent(node);
                if node == block || node.is_invalid() {
                    builder.finish_paragraph();
                    return builder.content;
                }
            }
        }
        builder.content
    }
}

/// The child an inline formatting context visits before proceeding to the next sibling.
pub(super) fn first_inline_child(pass: &LayoutPass<'_>, node: Node) -> Node {
    let child = pass.first_child(node);
    if child.is_invalid() {
        return child;
    }
    let facts = NodeFacts::new(pass, node);
    let child_facts = NodeFacts::new(pass, child);
    if (child_facts.is_inline()
        || child_facts.is_inline_flow_interrupting_block()
        || child_facts.is_floating_or_absolutely_positioned())
        && StyleValues::for_node(pass, node).display().is_flow_inside()
        && !facts.is_inline_flow_interrupting_block()
        && !facts.is_atomic_inline()
        && (!facts.is_box() || !facts.is_floating_or_absolutely_positioned())
    {
        child
    } else {
        NodeSlotId::INVALID
    }
}

struct Controls {
    opening: &'static [u16],
    closing: &'static [u16],
}

impl Controls {
    fn for_style(bidi: u8, direction: u8) -> Self {
        let rtl = direction == direction::RTL;
        let (opening, closing): (&[u16], &[u16]) = match (bidi, rtl) {
            (unicode_bidi::EMBED, false) => (&[0x202a], &[0x202c]), // LRE, PDF
            (unicode_bidi::EMBED, true) => (&[0x202b], &[0x202c]),  // RLE, PDF
            (unicode_bidi::BIDI_OVERRIDE, false) => (&[0x202d], &[0x202c]), // LRO, PDF
            (unicode_bidi::BIDI_OVERRIDE, true) => (&[0x202e], &[0x202c]), // RLO, PDF
            (unicode_bidi::ISOLATE, false) => (&[0x2066], &[0x2069]), // LRI, PDI
            (unicode_bidi::ISOLATE, true) => (&[0x2067], &[0x2069]), // RLI, PDI
            (unicode_bidi::ISOLATE_OVERRIDE, false) => (&[0x2068, 0x202d], &[0x202c, 0x2069]),
            (unicode_bidi::ISOLATE_OVERRIDE, true) => (&[0x2068, 0x202e], &[0x202c, 0x2069]),
            (unicode_bidi::PLAINTEXT, _) => (&[0x2068], &[0x2069]), // FSI, PDI
            _ => (&[], &[]),
        };
        Self { opening, closing }
    }
}

struct ParagraphBuilder {
    content: InlineContent,
    text: Vec<u16>,
    controls: Vec<(Node, Controls)>,
    base_level: u8,
    first_strong: bool,
}

impl ParagraphBuilder {
    fn enter(&mut self, node: Node, controls: Controls) {
        if !controls.opening.is_empty() {
            self.text.extend_from_slice(controls.opening);
            self.controls.push((node, controls));
        }
    }

    fn leave(&mut self, node: Node) {
        if self.controls.last().is_some_and(|(box_, _)| *box_ == node) {
            let (_, controls) = self.controls.pop().unwrap();
            self.text.extend_from_slice(controls.closing);
        }
    }

    fn append(&mut self, node: Node, start: usize, text: &[u16]) {
        if text.is_empty() {
            return;
        }
        self.content.nodes.entry(node).or_default().push(NodeRange {
            start,
            length: text.len(),
            position: Position {
                paragraph: self.content.paragraphs.len(),
                offset: self.text.len(),
            },
        });
        self.text.extend_from_slice(text);
    }

    fn append_text(&mut self, node: Node, text: &[u16], preserves_newlines: bool) {
        let mut start = 0;
        let mut next = 0;
        while next < text.len() {
            let remaining = &text[next..];
            // SAFETY: LibUnicode only reads the supplied slice during this call.
            let offset = next + unsafe { unicode_bidi_find_paragraph_separator(remaining.as_ptr(), remaining.len()) };
            if offset == text.len() {
                break;
            }
            let code_point = text_chunker::code_point_at(text, offset);
            next = offset + text_chunker::code_unit_length_for_code_point(code_point);
            self.append(node, start, &text[start..offset]);
            // CSS breaks at the LF of a preserved CRLF. Any CR or unpreserved LF stays whitespace for bidi.
            if code_point == u32::from(b'\r') || (code_point == u32::from(b'\n') && !preserves_newlines) {
                self.append(node, offset, &[u16::from(b' ')]);
            } else {
                self.end_paragraph(Some((node, offset, &text[offset..next])));
            }
            start = next;
        }
        self.append(node, start, &text[start..]);
    }

    fn end_paragraph(&mut self, newline: Option<(Node, usize, &[u16])>) {
        // A break closes the controls opened by CSS. The same boxes open them again in the new paragraph.
        for (_, controls) in self.controls.iter().rev() {
            self.text.extend_from_slice(controls.closing);
        }
        if let Some((node, offset, separator)) = newline {
            self.append(node, offset, separator);
        }
        self.resolve_levels();
        for (_, controls) in &self.controls {
            self.text.extend_from_slice(controls.opening);
        }
    }

    fn finish_paragraph(&mut self) {
        for (_, controls) in self.controls.iter().rev() {
            self.text.extend_from_slice(controls.closing);
        }
        self.resolve_levels();
    }

    fn resolve_levels(&mut self) {
        if self.text.is_empty() {
            return;
        }
        let length = i32::try_from(self.text.len()).expect("bidi paragraph length fits in 32 bits");
        let mut levels = vec![0; self.text.len()].into_boxed_slice();
        // SAFETY: Both buffers contain length elements and the call only uses them synchronously.
        let base_level = unsafe {
            unicode_bidi_resolve_paragraph(
                self.text.as_ptr(),
                length,
                self.base_level,
                self.first_strong,
                levels.as_mut_ptr(),
            )
        };
        let is_unidirectional_ltr = base_level == 0
            && levels
                .iter()
                .all(|&level| level == PARAGRAPH_SEPARATOR_LEVEL || level & 1 == 0);
        self.content.paragraphs.push(Paragraph {
            levels,
            base_level,
            is_unidirectional_ltr,
        });
        self.text.clear();
    }
}

#[cfg(test)]
mod ffi_stubs {
    #[unsafe(no_mangle)]
    extern "C" fn unicode_bidi_find_paragraph_separator(_: *const u16, _: usize) -> usize {
        unreachable!("Rust layout unit tests do not resolve bidi paragraphs")
    }

    #[unsafe(no_mangle)]
    extern "C" fn unicode_bidi_resolve_paragraph(_: *const u16, _: i32, _: u8, _: bool, _: *mut u8) -> u8 {
        unreachable!("Rust layout unit tests do not resolve bidi paragraphs")
    }

    #[unsafe(no_mangle)]
    extern "C" fn unicode_bidi_reorder_line(_: *const u8, _: i32, _: u8, _: *mut i32) -> i32 {
        unreachable!("Rust layout unit tests do not reorder bidi paragraphs")
    }
}
