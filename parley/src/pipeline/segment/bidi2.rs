// Copyright 2021 the Parley Authors
// SPDX-License-Identifier: Apache-2.0 OR MIT

//! Unicode bidirectional level segmentation.

use super::super::element_tree::{Element, ElementKind, ElementTreeRef};
use crate::bidi::BidiResolver;
use alloc::vec::Vec;
use core::ops::Range;
use swash::text::Codepoint;

/// Type alias for a BiDi level.
pub type BidiLevel = u8;

/// Represents a range of text that has been segmented by Unicode script
/// and BiDi level.
///
/// This is the maximum unit of text that is suitable for shaping but it
/// may be further constrained by other factors such as font selection or
/// style properties.
#[derive(Clone, Default, Debug)]
pub struct BidiRun {
    /// The resolved base level of the paragraph containing this run.
    pub base_level: BidiLevel,
    /// The resolved level.
    pub level: BidiLevel,
    /// The byte range of text for this run.
    pub text_range: Range<usize>,
}

/// A paragraph segmented by Unicode script and BiDi level.
#[derive(Clone, Debug)]
pub struct BidiParagraph {
    /// Base level of the paragraph.
    pub base_level: BidiLevel,
    /// Range of runs in [`ScriptBidiSegments::runs`].
    pub runs: Range<usize>,
}

/// Collections of paragraphs and runs that result from Unicode script and BiDi
/// segmentation.
#[derive(Clone, Default, Debug)]
pub struct BidiSegments {
    /// Range of runs for each paragraph.
    pub paragraphs: Vec<BidiParagraph>,
    /// Sequence of all runs.
    pub runs: Vec<BidiRun>,
}

impl BidiSegments {
    /// Creates a new set of script and BiDi segments.
    pub fn new() -> Self {
        Self::default()
    }

    /// Clears the segments.
    pub fn clear(&mut self) {
        self.paragraphs.clear();
        self.runs.clear();
    }

    /// Returns an iterator over the base level and run sequence for each
    /// paragraph.
    pub fn paragraphs(&self) -> impl Iterator<Item = (BidiLevel, &[BidiRun])> {
        self.paragraphs
            .iter()
            .map(|para| (para.base_level, &self.runs[para.runs.clone()]))
    }
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
    pub fn segment(&mut self, text: &str, base_level: Option<BidiLevel>) -> BidiSegments {
        let mut segments = BidiSegments::default();
        self.segment_to(text, base_level, &mut segments);
        segments
    }

    /// Computes the segments for the specified text and base BiDi level and
    /// stores them in the given [`ScriptBidiSegments`] object.
    ///
    /// This allows reuse of the segments allocations.
    pub fn segment_to(
        &mut self,
        text: &str,
        base_level: Option<BidiLevel>,
        segments: &mut BidiSegments,
    ) {
        segments.clear();
        let mut start = 0;
        for para_text in split_paragraphs(text) {
            let runs_start = segments.runs.len();
            let para_level =
                self.segment_paragraph(para_text, base_level, start, &mut segments.runs);
            start += para_text.len();
            let runs_end = segments.runs.len();
            segments.paragraphs.push(BidiParagraph {
                base_level: para_level,
                runs: runs_start..runs_end,
            });
        }
    }

    fn segment_paragraph(
        &mut self,
        text: &str,
        base_level: Option<u8>,
        text_start: usize,
        runs: &mut Vec<BidiRun>,
    ) -> u8 {
        if text.is_empty() {
            return 0;
        }
        self.resolver.clear();
        self.resolver
            .resolve(text.chars().map(|ch| (ch, ch.bidi_class())), base_level);
        let base_level = self.resolver.base_level();
        let levels = self.resolver.levels();
        let mut run = BidiRun {
            base_level,
            level: levels[0],
            text_range: text_start..text_start,
        };
        for (ch, &bidi_level) in text.chars().zip(levels) {
            if bidi_level != run.level {
                runs.push(run.clone());
                run.level = bidi_level;
                run.text_range.start = run.text_range.end;
            }
            run.text_range.end += ch.len_utf8();
        }
        if !run.text_range.is_empty() {
            runs.push(run);
        }
        self.resolver.base_level()
    }
}

/// Returns a new sequence of elements for the given tree with each assigned
/// an appropriate BiDi level, splitting if necessary.
pub fn split_element_tree<'a>(
    tree: &'a ElementTreeRef,
    segments: &'a BidiSegments,
) -> impl Iterator<Item = (BidiLevel, BidiLevel, Element)> + 'a {
    let mut elem_ix = 0;
    let mut run_ix = 0;
    let mut cur_run = segments.runs.get(0).cloned();
    let mut cur_element = tree.elements.get(0).cloned();
    core::iter::from_fn(move || {
        let run = cur_run.as_mut()?;
        let element = cur_element.as_mut()?;
        // Find the run that overlaps this element
        while !run.text_range.contains(&element.text_start) {
            run_ix += 1;
            if let Some(next_run) = segments.runs.get(run_ix).cloned() {
                *run = next_run;
            } else {
                break;
            }
        }
        let run_range = run.text_range.clone();
        let element_range = element.text_range().clone();
        if element_range.end <= run_range.end || run_ix >= segments.runs.len() {
            // The element fits entirely within the run
            let element = element.clone();
            elem_ix += 1;
            cur_element = tree.elements.get(elem_ix).cloned();
            Some((run.base_level, run.level, element.clone()))
        } else {
            // We need to split the element
            let new_len = run_range.end - element_range.start;
            let mut split_element = element.clone();
            split_element.text_len = new_len as u16;
            element.text_len -= split_element.text_len;
            element.text_start += new_len;
            // We may have trailing elements that don't have associated text
            // so update the element if necessary
            // if element_range.end > run_range.end && run_ix >= segments.runs.len() {
            //     elem_ix += 1;
            //     cur_element = tree.elements.get(elem_ix).cloned();
            // }   
            Some((run.base_level, run.level, split_element))
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
fn dump_runs(runs: &[BidiRun], text: &str) {
    use alloc::string::String;
    for (i, run) in runs.iter().enumerate() {
        let text = &text[run.text_range.clone()];
        println!("[{i}] <{}> {text:?}", run.level);
    }
}

#[allow(unused)]
fn dump_segments(segments: &BidiSegments, text: &str) {
    for (base_level, runs) in segments.paragraphs() {
        println!("--- base level {base_level}");
        dump_runs(runs, text);
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
        let segments = segmenter.segment(text, None);
        dump_segments(&segments, text);
    }

    #[test]
    fn mixed_direction() {
        let mut segmenter = BidiSegmenter::new();
        let text = "And also\rsome\nاللغة العربية arabic\ntext.";
        let segments = segmenter.segment(text, None);
        dump_segments(&segments, text);
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
        // tree_builder.object(ElementId(3));
        let tree = tree_builder.finish();
        let segments = segmenter.segment(&tree.text, None);
        let tree_ref = tree.as_ref();
        let elements = split_element_tree(&tree_ref, &segments).collect::<Vec<_>>();

        for (base_level, level, element) in &elements {
            let kind = element.kind;
            let text = &tree_ref.text[element.text_range()];
            println!("[{base_level}:{level}] {kind:?}: {text:?}");
        }
    }
}
