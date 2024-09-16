// Copyright 2021 the Parley Authors
// SPDX-License-Identifier: Apache-2.0 OR MIT

//! Flattened representation of an element tree.

use super::{ElementId, SpanIndex};
use alloc::{string::String, vec::Vec};
use core::ops::Range;

/// Tracked element in a layout.
#[derive(Copy, Clone, Default, Debug)]
pub struct Element {
    /// Identifier for the element.
    pub id: ElementId,
    /// Index of the span that contains this element.
    pub parent_span: SpanIndex,
    /// Offset of this element in the source.
    pub source_offset: usize,
    /// Offset of this element in the generated layout text.
    pub text_start: usize,
    /// Length of this element.
    pub text_len: u16,
    /// Payload containing the element specific data.
    pub kind: ElementKind,
}

impl Element {
    /// Returns the range of this element in the generated layout text.
    pub fn text_range(&self) -> Range<usize> {
        let end = self.text_start + self.text_len as usize;
        self.text_start..end
    }
}

/// Payload for a tracked element in a layout.
#[derive(Copy, Clone, PartialEq, Eq, Default, Debug)]
pub enum ElementKind {
    /// A sequence of characters.
    #[default]
    Text,
    /// An inline object.
    Object,
    /// Denotes the beginning of a span.
    SpanStart,
    /// Denotes the end of a span.
    SpanEnd,
    /// Paragraph separator.
    LineBreak,
}

/// Owned representation of an element tree.
#[derive(Clone, Default, Debug)]
pub struct ElementTree {
    /// The aggregate text content of the tree.
    ///
    /// This may contain "marker" characters for various elements.
    pub text: String,
    /// Sequence of elements describing the structure of the tree.
    pub elements: Vec<Element>,
}

impl ElementTree {
    /// Removes all content from this element tree.
    pub fn clear(&mut self) {
        self.text.clear();
        self.elements.clear();
    }

    /// Returns a reference representation of this element tree.
    pub fn as_ref(&self) -> ElementTreeRef {
        ElementTreeRef {
            text: &self.text,
            elements: &self.elements,
        }
    }
}

/// Reference representation of an [`ElementTree`].
///
/// Further passes operate on this type so that an element tree can be
/// constructed without copying text. This is useful when dealing
/// with attributed text style layout.
#[derive(Copy, Clone, Default, Debug)]
pub struct ElementTreeRef<'a> {
    /// The aggregate text content of the tree.
    ///
    /// This may contain "marker" characters for various elements.
    pub text: &'a str,
    /// Sequence of elements describing the structure of the tree.
    pub elements: &'a [Element],
}

impl<'a> From<&'a ElementTree> for ElementTreeRef<'a> {
    fn from(value: &'a ElementTree) -> Self {
        value.as_ref()
    }
}

/// Builder for constructing a source element tree.
#[derive(Default, Debug)]
pub struct ElementTreeBuilder {
    tree: ElementTree,
    /// Keep track of current character count for updating ranges.
    char_count: usize,
    /// Tracks the number of allocated spans.
    span_count: SpanIndex,
    /// Stack for handling nested spans.
    span_stack: smallvec::SmallVec<[(ElementId, SpanIndex); 16]>,
}

impl<'a> ElementTreeBuilder {
    /// Creates a new builder for the given tree.
    ///
    /// This allows reuse of the memory backing an existing tree. The content
    /// in the tree will be cleared. To construct a new tree, use
    /// [`ElementTreeBuilder::default`].
    pub fn new(mut tree: ElementTree) -> Self {
        tree.clear();
        Self {
            tree,
            char_count: 0,
            // We always start at one, assuming 0 is the root span
            span_count: 1,
            span_stack: Default::default(),
        }
    }

    /// Pushes a new span with the given element identifier.
    pub fn push_span(&mut self, id: ElementId) -> SpanIndex {
        let parent_span = self.current_span_index();
        let text_start = self.tree.text.len();
        let span_index = self.span_count;
        self.span_count += 1;
        self.span_stack.push((id, span_index));
        self.tree.elements.push(Element {
            id,
            parent_span,
            source_offset: 0,
            text_start,
            text_len: 0,
            kind: ElementKind::SpanStart,
        });
        span_index
    }

    /// Pops the current span.
    pub fn pop_span(&mut self) -> Option<SpanIndex> {
        if let Some((id, span_index)) = self.span_stack.pop() {
            let parent_span = self.current_span_index();
            let text_start = self.tree.text.len();
            self.tree.elements.push(Element {
                id,
                parent_span,
                source_offset: 0,
                text_start,
                text_len: 0,
                kind: ElementKind::SpanEnd,
            });
            Some(span_index)
        } else {
            None
        }
    }

    /// Inserts a text element with the given identifier and content.
    pub fn text(&mut self, id: ElementId, content: &str) {
        let parent_span = self.current_span_index();
        // We split text spans into 64k segments so that we can use 16 bit
        // offsets to save space later in the pipeline.
        for (segment_offset, segment) in split_max_len(content, u16::MAX) {
            let text_start = self.tree.text.len();
            let len = segment.len() as u16;
            self.tree.text.push_str(segment);
            self.tree.elements.push(Element {
                id,
                parent_span,
                source_offset: segment_offset,
                text_start,
                text_len: len,
                kind: ElementKind::Text,
            });
        }
    }

    /// Inserts an object element with the given identifier.
    pub fn object(&mut self, id: ElementId) {
        const OBJECT_CHAR: char = '\u{FFFC}';
        const OBJECT_CHAR_LEN: u16 = OBJECT_CHAR.len_utf8() as u16;
        let parent_span = self.current_span_index();
        let text_start = self.tree.text.len();
        // add object replacement character to text buffer
        self.tree.text.push(OBJECT_CHAR);
        self.tree.elements.push(Element {
            id,
            parent_span,
            source_offset: 0,
            text_start,
            text_len: OBJECT_CHAR_LEN,
            kind: ElementKind::Object,
        });
        self.char_count += 1;
    }

    /// Inserts a line break element with the given identifier.
    pub fn line_break(&mut self, id: ElementId) {
        let parent_span = self.current_span_index();
        let text_start = self.tree.text.len();
        // add object replacement character to text buffer
        self.tree.text.push('\n');
        self.tree.elements.push(Element {
            id,
            parent_span,
            source_offset: 0,
            text_start,
            text_len: 1,
            kind: ElementKind::LineBreak,
        });
        self.char_count += 1;
    }

    /// Consumes the builder, finalizing any remaining operations and
    /// returns the resulting element tree.
    pub fn finish(mut self) -> ElementTree {
        while !self.span_stack.is_empty() {
            self.pop_span();
        }
        self.tree
    }

    fn current_span_index(&self) -> SpanIndex {
        self.span_stack.last().map(|x| x.1).unwrap_or_default()
    }
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
