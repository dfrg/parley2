// Copyright 2021 the Parley Authors
// SPDX-License-Identifier: Apache-2.0 OR MIT

//! Unicode bidirectional level segmentation.

use super::super::element_tree::{Element, ElementKind, ElementTreeRef};
use crate::bidi::BidiResolver;
use alloc::vec::Vec;
use core::ops::Range;
use swash::text::Codepoint;

/// A bidirectional level.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
#[repr(transparent)]
pub struct BidiLevel(pub u8);

impl BidiLevel {
    /// Returns true if this level represents a left-to-right sequence.
    pub fn is_ltr(self) -> bool {
        self.0 & 1 == 0
    }

    /// Returns true if this level represents a right-to-left sequence.
    pub fn is_rtl(self) -> bool {
        self.0 & 1 != 0
    }
}

/// Represents a range of text that has been segmented by BiDi level.
///
/// This is the maximum unit of text that is suitable for shaping but it
/// may be further constrained by other factors such as font selection or
/// style properties.
#[derive(Copy, Clone, Default, Debug)]
pub struct BidiSegment<T> {
    /// Index of the paragraph that contains this run.
    pub paragraph_index: usize,
    /// The resolved base level of the paragraph containing this run.
    pub paragraph_level: BidiLevel,
    /// The resolved level.
    pub level: BidiLevel,
    /// Data associated with this run.
    pub data: T,
}

/// Segmenter for splitting text into runs by BiDi level.
#[derive(Default)]
pub struct BidiSegmenter {
    resolver: BidiResolver,
}

impl BidiSegmenter {
    /// Creates a new BiDi segmenter.
    pub fn new() -> Self {
        Self::default()
    }

    /// Computes the segments for the specified text and base BiDi level.
    pub fn segment(
        &mut self,
        text: &str,
        base_level: Option<BidiLevel>,
    ) -> Vec<BidiSegment<Range<usize>>> {
        let mut segments = Vec::new();
        self.segment_to(text, base_level, &mut segments);
        segments
    }

    /// Computes the segments for the specified text and base BiDi level and
    /// stores them in the given `runs` vector.
    ///
    /// This allows reuse of the segments allocations.
    pub fn segment_to(
        &mut self,
        text: &str,
        base_level: Option<BidiLevel>,
        segments: &mut Vec<BidiSegment<Range<usize>>>,
    ) {
        segments.clear();
        let mut start = 0;
        for (i, para_text) in split_paragraphs(text).enumerate() {
            self.segment_paragraph(para_text, i, base_level, start, segments);
            start += para_text.len();
        }
    }

    fn segment_paragraph(
        &mut self,
        text: &str,
        paragraph_index: usize,
        base_level: Option<BidiLevel>,
        text_start: usize,
        segments: &mut Vec<BidiSegment<Range<usize>>>,
    ) {
        if text.is_empty() {
            return;
        }
        self.resolver.clear();
        self.resolver.resolve(
            text.chars().map(|ch| (ch, ch.bidi_class())),
            base_level.map(|level| level.0),
        );
        let base_level = self.resolver.base_level();
        let levels = self.resolver.levels();
        let mut run = BidiSegment {
            paragraph_index,
            paragraph_level: BidiLevel(base_level),
            level: BidiLevel(levels[0]),
            data: text_start..text_start,
        };
        for (ch, &bidi_level) in text.chars().zip(levels) {
            if bidi_level != run.level.0 {
                segments.push(run.clone());
                run.level = BidiLevel(bidi_level);
                run.data.start = run.data.end;
            }
            run.data.end += ch.len_utf8();
        }
        if !run.data.is_empty() {
            segments.push(run);
        }
    }
}

/// Returns a new sequence of elements for the given tree with each assigned
/// an appropriate BiDi level, splitting if necessary.
pub fn split_element_tree<'a>(
    tree: impl Into<ElementTreeRef<'a>>,
    segments: &'a [BidiSegment<Range<usize>>],
) -> impl Iterator<Item = BidiSegment<Element>> + 'a {
    let tree = tree.into();
    let mut elem_ix = 0;
    let mut segment_ix = 0;
    let mut cur_segment = segments.first().cloned();
    let mut cur_element = tree.elements.first().cloned();
    core::iter::from_fn(move || {
        let segment = cur_segment.as_mut()?;
        let element = cur_element.as_mut()?;
        // Find the run that overlaps this element
        while !segment.data.contains(&element.text_start) {
            segment_ix += 1;
            if let Some(next_segment) = segments.get(segment_ix).cloned() {
                *segment = next_segment;
            } else {
                break;
            }
        }
        let run_range = segment.data.clone();
        let element_range = element.text_range().clone();
        if element_range.end <= run_range.end || segment_ix >= segments.len() {
            // The element fits entirely within the run
            let element = element.clone();
            elem_ix += 1;
            cur_element = tree.elements.get(elem_ix).cloned();
            Some(BidiSegment {
                paragraph_index: segment.paragraph_index,
                paragraph_level: segment.paragraph_level,
                level: segment.level,
                data: element.clone(),
            })
        } else {
            // We need to split the element
            let new_len = run_range.end - element_range.start;
            let mut split_element = element.clone();
            split_element.text_len = new_len as u16;
            element.text_len -= split_element.text_len;
            element.text_start += new_len;
            if element.kind == ElementKind::Text {
                element.source_offset += new_len;
            }
            // We may have trailing elements that don't have associated text
            // so update the element if necessary
            // if element_range.end > run_range.end && run_ix >= segments.runs.len() {
            //     elem_ix += 1;
            //     cur_element = tree.elements.get(elem_ix).cloned();
            // }
            Some(BidiSegment {
                paragraph_index: segment.paragraph_index,
                paragraph_level: segment.paragraph_level,
                level: segment.level,
                data: split_element,
            })
        }
    })
}

/// Returns the byte range and content for each paragraph in the
/// given text.
fn split_paragraphs(text: &str) -> impl Iterator<Item = &str> {
    let mut chars = text.chars().peekable();
    let mut start = 0;
    let mut end = 0;
    core::iter::from_fn(move || loop {
        let ch = chars.next()?;
        let next_ch = chars.peek().copied();
        end += ch.len_utf8();
        match ch {
            '\n' | '\u{2029}' | '\u{2028}' | '\u{000B}' | '\u{000C}' => {}
            '\r' => {
                if next_ch == Some('\n') {
                    end += 1;
                    chars.next();
                }
            }
            _ => {
                if next_ch.is_some() {
                    continue;
                }
            }
        }
        let para = text.get(start..end)?;
        start = end;
        return Some(para);
    })
}

#[allow(unused)]
fn dump_segments(segments: &[BidiSegment<Range<usize>>], text: &str) {
    use alloc::string::String;
    let mut last_para_index = None;
    for (i, run) in segments.iter().enumerate() {
        if last_para_index != Some(run.paragraph_index) {
            println!("--- base level {}", run.paragraph_level.0);
            last_para_index = Some(run.paragraph_index);
        }
        let text = &text[run.data.clone()];
        println!("[{i}] <{}> {text:?}", run.level.0);
    }
}

#[cfg(test)]
mod tests {
    use super::super::super::{element_tree::ElementTreeBuilder, ElementId};
    use super::*;

    #[test]
    fn only_scripts() {
        let mut segmenter = BidiSegmenter::new();
        let text = "This is some\nBangla বিন্ধ্য and\r\nGreek γλώσσα.";
        let runs = segmenter.segment(text, None);
        dump_segments(&runs, text);
    }

    #[test]
    fn mixed_direction() {
        let mut segmenter = BidiSegmenter::new();
        let text = "And also\rsome\nاللغة العربية arabic\ntext.";
        let runs = segmenter.segment(text, None);
        dump_segments(&runs, text);
    }

    #[test]
    fn paragraphs() {
        // terminates in line separator
        let text = "this\nis\rmany\r\nlines\u{2028}";
        let paras = split_paragraphs(text).collect::<Vec<_>>();
        assert_eq!(paras, ["this\n", "is\r", "many\r\n", "lines\u{2028}",]);
        // terminates without line separator
        let text = "this\nis\rmany\r\nlines";
        let paras = split_paragraphs(text).collect::<Vec<_>>();
        assert_eq!(paras, ["this\n", "is\r", "many\r\n", "lines",]);
        // no line separators
        let text = "this";
        let paras = split_paragraphs(text).collect::<Vec<_>>();
        assert_eq!(paras, ["this"]);
    }

    #[test]
    fn split_elements() {
        let mut segmenter = BidiSegmenter::new();
        let mut tree_builder = ElementTreeBuilder::default();
        // let text = "This is some\nBangla বিন্ধ্য and\r\nGreek γλώσσα.";
        let text = "And also\rsome\nاللغة العربية arabic\ntext.";
        tree_builder.push_span(ElementId(0));
        tree_builder.text(ElementId(1), text);
        tree_builder.push_span(ElementId(1));
        tree_builder.text(ElementId(2), "בנושא");
        tree_builder.object(ElementId(3));
        let tree = tree_builder.finish();
        let bidi_runs = segmenter.segment(&tree.text, None);
        let elements = split_element_tree(&tree, &bidi_runs).collect::<Vec<_>>();

        let mut para_index = None;
        for (i, run) in elements.iter().enumerate() {
            if para_index != Some(run.paragraph_index) {
                println!("--- base level {}", run.paragraph_level.0);
                para_index = Some(run.paragraph_index);
            }
            let element = &run.data;
            let kind = element.kind;
            let text = &tree.text[element.text_range()];
            println!("[{i}] <{}> {kind:?}: {text:?}", run.level.0);
        }
    }
}
