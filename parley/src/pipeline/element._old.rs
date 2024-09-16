// Copyright 2021 the Parley Authors
// SPDX-License-Identifier: Apache-2.0 OR MIT

//! Common element types used for various layout representations.

use alloc::{string::String, vec::Vec};
use core::ops::Range;

/// Identifier for an element, used to associate layout objects with the
/// user's content.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct ElementId(pub u64);

/// Identifier for a brush, used to represent color properties.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct BrushId(pub u64);

/// Used to associate an element with its containing span.
pub type SpanIndex = usize;

/// Set of style properties for a range of text.
#[derive(Clone, Debug)]
pub struct Style {}

/// Fully resolved set of style properties for a range of text.
#[derive(Clone, Debug)]
pub struct ResolvedStyle {}

/// Either a style or resolved style.
#[derive(Clone, Debug)]
pub enum SpanStyle<'a> {
    Style(&'a Style),
    ResolvedStyle(&'a ResolvedStyle),
}

impl<'a> From<&'a Style> for SpanStyle<'a> {
    fn from(value: &'a Style) -> Self {
        Self::Style(value)
    }
}

impl<'a> From<&'a ResolvedStyle> for SpanStyle<'a> {
    fn from(value: &'a ResolvedStyle) -> Self {
        Self::ResolvedStyle(value)
    }
}

/// Defines the margin, border and padding of a span.
#[derive(Copy, Clone, Default, Debug)]
pub struct SpanBox {
    /// Left (or start) side properties.
    pub left: SpanBoxSide,
    /// Right (or end) side properties.
    pub right: SpanBoxSide,
    /// Top side properties.
    pub top: SpanBoxSide,
    /// Bottom side properties.
    pub bottom: SpanBoxSide,
}

/// One side of a span box.
#[derive(Copy, Clone, Default, Debug)]
pub struct SpanBoxSide {
    /// Width of margin in pixels.
    pub margin: f32,
    /// Width of border in pixels.
    pub border: f32,
    /// Optional color of the border.
    pub border_color: Option<BrushId>,
    /// Width of padding in pixels.
    pub padding: f32,
}

/// Sequence of characters in a layout.
#[derive(Copy, Clone, Debug)]
pub struct TextElement {
    /// Offset of this text element in the original source.
    ///
    /// This can be combined with [`Element::source_id`] to map the text element
    /// back to the user's representation.
    pub source_offset: usize,
}

/// Object that can be inserted into a layout.
#[derive(Copy, Clone, Debug)]
pub struct ObjectElement {
    /// Width of the object in pixels.
    pub width: f32,
    /// Height of the object in pixels.
    pub height: f32,
}

/// Tracked element in a layout.
#[derive(Copy, Clone, Debug)]
pub struct Element {
    /// User provided identifier.
    pub source_id: ElementId,
    /// Offset of this element in the generated layout text.
    pub text_start: usize,
    /// Length of this element.
    pub text_len: u16,
    /// Offset in characters in the generated layout text.
    pub char_start: usize,
    /// Length in characters of this element.
    pub char_len: u16,
    /// Flattened index of the span containing this element.
    pub span: SpanIndex,
    /// Payload containing the element specific data.
    pub kind: ElementKind,
}

impl Element {
    /// Returns the range of this element in the generated layout text.
    pub fn text_range(&self) -> Range<usize> {
        let end = self.text_start + self.text_len as usize;
        self.text_start..end
    }

    /// Returns the character range of this element in the generated layout
    /// text.
    pub fn char_range(&self) -> Range<usize> {
        let end = self.char_start + self.char_len as usize;
        self.char_start..end
    }
}

/// Payload for a tracked element in a layout.
#[derive(Copy, Clone, Debug)]
pub enum ElementKind {
    /// An inline object.
    Object(ObjectElement),
    /// A sequence of characters.
    Text(TextElement),
    /// Denotes the beginning of a span.
    SpanStart(SpanIndex),
    /// Denotes the end of a span.
    SpanEnd(SpanIndex),
}

/// Flattened represention of an element tree.
#[derive(Clone, Default, Debug)]
pub struct FlatElementTree {
    /// Aggregate of all text elements.
    text: String,
    /// Keep track of current character count for updating ranges.
    char_count: usize,
    /// Flattened sequence of elements.
    elements: Vec<Element>,
    /// Side table for span boxes.
    span_boxes: Vec<SpanBox>,
    /// List of spans.
    spans: Vec<SpanEntry>,
    /// Span for handling nested spans.
    span_stack: Vec<SpanStackEntry>,
}

#[derive(Clone, Debug)]
struct SpanEntry {
    style: ResolvedStyle,
    box_index: Option<usize>,
}

#[derive(Copy, Clone, Debug)]
struct SpanStackEntry {
    source_id: ElementId,
    index: SpanIndex,
    parent_index: SpanIndex,
}

impl FlatElementTree {
    pub fn clear(&mut self) {
        self.text.clear();
        self.char_count = 0;
        self.elements.clear();
        self.span_boxes.clear();
        self.spans.clear();
        self.span_stack.clear();
        // Always keep a default span
        self.spans.push(SpanEntry {
            style: ResolvedStyle {},
            box_index: None,
        });
        self.span_stack.push(SpanStackEntry {
            source_id: ElementId(0),
            index: 0,
            parent_index: 0,
        });
    }

    pub fn enter_span<'a>(
        &mut self,
        source_id: ElementId,
        style: impl Into<SpanStyle<'a>>,
        span_box: Option<SpanBox>,
    ) {
        let parent_span = self.current_span_index();
        let span_index = self.spans.len();
        let box_index = if let Some(span_box) = span_box {
            let index = self.span_boxes.len();
            self.span_boxes.push(span_box);
            Some(index)
        } else {
            None
        };
        // TODO: actually do something with this
        let style = match style.into() {
            SpanStyle::Style(_) => ResolvedStyle {},
            SpanStyle::ResolvedStyle(resolved) => resolved.clone(),
        };
        self.spans.push(SpanEntry { style, box_index });
        self.span_stack.push(SpanStackEntry {
            source_id,
            index: span_index,
            parent_index: parent_span,
        });
        // Should we add a character for bidi here?
        self.elements.push(Element {
            source_id,
            text_start: self.text.len(),
            text_len: 0,
            char_start: self.char_count,
            char_len: 0,
            span: parent_span,
            kind: ElementKind::SpanStart(span_index),
        });
    }

    pub fn leave_span(&mut self) {
        // Never pop the outer span
        if self.span_stack.len() > 1 {
            let entry = self.span_stack.pop().unwrap();
            let parent_span = entry.parent_index;
            // Should we add a character for bidi here?
            self.elements.push(Element {
                source_id: entry.source_id,
                text_start: self.text.len(),
                text_len: 0,
                char_start: self.char_count,
                char_len: 0,
                span: parent_span,
                kind: ElementKind::SpanEnd(entry.index),
            });
        }
    }

    pub fn push_text(&mut self, source_id: ElementId, source_offset: usize, text: &str) {
        let span = self.current_span_index();
        // We split text spans into 64k segments so that we can use 16 bit
        // offsets to save space later in the pipeline.
        for (segment_offset, segment) in split_max_len(text, u16::MAX) {
            let text_start = self.text.len();
            let len = segment.len() as u16;
            let char_len = segment.chars().count();
            self.text.push_str(segment);
            self.elements.push(Element {
                source_id,
                text_start,
                text_len: len,
                char_start: self.char_count,
                char_len: char_len as u16,
                span,
                kind: ElementKind::Text(TextElement {
                    source_offset: source_offset + segment_offset,
                }),
            });
            self.char_count += char_len;
        }
    }

    pub fn push_object(&mut self, source_id: ElementId, width: f32, height: f32) {
        const OBJECT_CHAR: char = '\u{FFFC}';
        const OBJECT_CHAR_LEN: u16 = OBJECT_CHAR.len_utf8() as u16;
        let span = self.current_span_index();
        let text_start = self.text.len();
        // add object replacement character to text buffer
        self.text.push(OBJECT_CHAR);
        self.elements.push(Element {
            source_id,
            text_start,
            text_len: OBJECT_CHAR_LEN,
            char_start: self.char_count,
            char_len: 1,
            span,
            kind: ElementKind::Object(ObjectElement { width, height }),
        });
        self.char_count += 1;
    }

    pub fn text(&self) -> &str {
        &self.text
    }

    pub fn elements(&self) -> &[Element] {
        &self.elements
    }

    pub fn span_style(&self, index: SpanIndex) -> Option<&ResolvedStyle> {
        self.spans.get(index).map(|entry| &entry.style)
    }

    pub fn span_box(&self, index: SpanIndex) -> Option<&SpanBox> {
        let index = self.spans.get(index)?.box_index?;
        self.span_boxes.get(index)
    }

    fn current_span_index(&self) -> SpanIndex {
        self.span_stack
            .last()
            .map(|entry| entry.index)
            .unwrap_or_default()
    }
}

pub struct FlatElementTreeRef<'a> {
    pub text: &'a str,
    pub elements: &'a [Element],
}

/// Splits text into segments of maximum length up to u16::MAX, making
/// sure to break at a valid character boundary.
fn split_max_len(text: &str, max_len: u16) -> impl Iterator<Item = (usize, &str)> {
    let max_len = max_len as usize;
    let len = text.len();
    let mut offset = 0;
    core::iter::from_fn(move || {
        let remaining = len - offset;
        if remaining == 0 {
            return None;
        }
        let segment_start = offset;
        let mut segment_end = segment_start + remaining.min(max_len);
        while !text.is_char_boundary(segment_end) {
            segment_end -= 1;
        }
        offset = segment_end;
        Some((segment_start, text.get(segment_start..segment_end)?))
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn split_text() {
        // basic example
        let basic_segments = split_and_check("hello world", 4);
        assert_eq!(basic_segments, [(0, "hell"), (4, "o wo"), (8, "rld")]);
        // some Bengali text that tries to break on a non-character boundary
        let bengali_segments = split_and_check("শুভ আশিস মাগে", 4);
        assert_eq!(
            bengali_segments,
            [
                (0, "শ"),
                (3, "\u{9c1}"),
                (6, "ভ "),
                (10, "আ"),
                (13, "শ"),
                (16, "ি"),
                (19, "স "),
                (23, "ম"),
                (26, "\u{9be}"),
                (29, "গ"),
                (32, "ে")
            ]
        );
    }

    fn split_and_check(text: &str, max_len: u16) -> Vec<(usize, &str)> {
        let segments = split_max_len(text, max_len).collect::<Vec<_>>();
        let text_segments = segments.iter().map(|segment| segment.1).collect::<Vec<_>>();
        // make sure we captured the whole string
        assert_eq!(text, text_segments.join(""));
        segments
    }
}
