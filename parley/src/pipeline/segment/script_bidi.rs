// Copyright 2021 the Parley Authors
// SPDX-License-Identifier: Apache-2.0 OR MIT

//! Unicode script and bidirectional level segmentation.

use super::super::element_tree::{Element, ElementKind, ElementTreeRef};
use crate::bidi::BidiResolver;
use alloc::vec::Vec;
use core::ops::Range;
use swash::text::Codepoint;

pub use icu_properties::Script;

/// Type alias for a BiDi level.
pub type BidiLevel = u8;

/// Represents a range of text that has been segmented by Unicode script
/// and BiDi level.
///
/// This is the maximum unit of text that is suitable for shaping but it
/// may be further constrained by other factors such as font selection or
/// style properties.
#[derive(Clone, Debug)]
pub struct ScriptBidiRun {
    /// The Unicode script associated with this run.
    pub script: Script,
    /// The resolved BiDi level.
    pub level: BidiLevel,
    /// The byte range of text for this run.
    pub text_range: Range<usize>,
}

impl Default for ScriptBidiRun {
    fn default() -> Self {
        Self {
            script: Script::Unknown,
            level: 0,
            text_range: 0..0,
        }
    }
}

/// A paragraph segmented by Unicode script and BiDi level.
#[derive(Clone, Debug)]
pub struct ScriptBidiParagraph {
    /// Base level of the paragraph.
    pub base_level: BidiLevel,
    /// Range of runs in [`ScriptBidiSegments::runs`].
    pub runs: Range<usize>,
}

/// Collections of paragraphs and runs that result from Unicode script and BiDi
/// segmentation.
#[derive(Clone, Default, Debug)]
pub struct ScriptBidiSegments {
    /// Range of runs for each paragraph.
    pub paragraphs: Vec<ScriptBidiParagraph>,
    /// Sequence of all runs.
    pub runs: Vec<ScriptBidiRun>,
}

impl ScriptBidiSegments {
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
    pub fn paragraphs(&self) -> impl Iterator<Item = (BidiLevel, &[ScriptBidiRun])> {
        self.paragraphs
            .iter()
            .map(|para| (para.base_level, &self.runs[para.runs.clone()]))
    }
}

/// Segmenter for splitting text into runs by Unicode script and BiDi level.
#[derive(Default)]
pub struct ScriptBidiSegmenter {
    resolver: BidiResolver,
}

impl ScriptBidiSegmenter {
    /// Creates a new Unicode script and BiDi segmenter.
    pub fn new() -> Self {
        Self::default()
    }

    /// Computes the segments for the specified text and base BiDi level.
    pub fn segment(&mut self, text: &str, base_level: Option<BidiLevel>) -> ScriptBidiSegments {
        let mut segments = ScriptBidiSegments::default();
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
        segments: &mut ScriptBidiSegments,
    ) {
        segments.clear();
        let mut start = 0;
        for para_text in split_paragraphs(text) {
            let runs_start = segments.runs.len();
            let para_level =
                self.segment_paragraph(para_text, base_level, start, &mut segments.runs);
            start += para_text.len();
            let runs_end = segments.runs.len();
            segments.paragraphs.push(ScriptBidiParagraph {
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
        runs: &mut Vec<ScriptBidiRun>,
    ) -> u8 {
        if text.is_empty() {
            return 0;
        }
        self.resolver.clear();
        self.resolver
            .resolve(text.chars().map(|ch| (ch, ch.bidi_class())), base_level);
        let script_map = icu_properties::maps::script();
        let first_script = text.chars().map(|ch| script_map.get(ch)).next().unwrap();
        let levels = self.resolver.levels();
        let mut run = ScriptBidiRun {
            script: first_script,
            level: levels[0],
            text_range: text_start..text_start,
        };
        let mut run_has_real_script = is_real_script(run.script);
        for ((ch, mut script), &bidi_level) in
            text.chars().map(|ch| (ch, script_map.get(ch))).zip(levels)
        {
            let real_script = is_real_script(script);
            if real_script {
                if !run_has_real_script {
                    run.script = script;
                    run_has_real_script = true;
                }
            } else {
                script = run.script;
            }
            if script != run.script || bidi_level != run.level {
                runs.push(run.clone());
                run.script = script;
                run.level = bidi_level;
                run.text_range.start = run.text_range.end;
                run_has_real_script = real_script;
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
    segments: &'a ScriptBidiSegments,
) -> impl Iterator<Item = (Script, BidiLevel, Element)> + 'a {
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
            *run = segments.runs.get(run_ix).cloned()?;
        }
        let run_range = run.text_range.clone();
        let element_range = element.text_range().clone();
        if element_range.end <= run_range.end {
            // The element fits entirely within the run
            let element = element.clone();
            elem_ix += 1;
            cur_element = tree.elements.get(elem_ix).cloned();
            Some((run.script, run.level, element.clone()))
        } else {
            // We need to split the element
            let new_len = run_range.end - element_range.start;
            let mut split_element = element.clone();
            split_element.text_len = new_len as u16;
            element.text_len -= split_element.text_len;
            element.text_start += new_len;
            Some((run.script, run.level, split_element))
        }
    })
}

fn is_real_script(script: Script) -> bool {
    !matches!(script, Script::Common | Script::Inherited)
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
fn dump_runs(runs: &[ScriptBidiRun], text: &str) {
    use alloc::string::String;
    let mapper = Script::enum_to_short_name_mapper();
    for (i, run) in runs.iter().enumerate() {
        let text = &text[run.text_range.clone()];
        let script_tag = mapper.get(run.script).unwrap();
        println!("[{i}] <{}> {script_tag:?} {text:?}", run.level);
    }
}

#[allow(unused)]
fn dump_segments(segments: &ScriptBidiSegments, text: &str) {
    for (base_level, runs) in segments.paragraphs() {
        println!("--- base level {base_level}");
        dump_runs(runs, text);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::super::{element_tree::ElementTreeBuilder, ElementId};

    #[test]
    fn only_scripts() {
        let mut segmenter = ScriptBidiSegmenter::new();
        let text = "This is some\nBangla বিন্ধ্য and\r\nGreek γλώσσα.";
        let segments = segmenter.segment(text, None);
        dump_segments(&segments, text);
    }

    #[test]
    fn mixed_direction() {
        let mut segmenter = ScriptBidiSegmenter::new();
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
        let mut segmenter = ScriptBidiSegmenter::new();
        let mut tree_builder = ElementTreeBuilder::default();
        let text = "This is some\nBangla বিন্ধ্য and\r\nGreek γλώσσα.";
        tree_builder.push_span(ElementId(0));
        tree_builder.text(ElementId(1), text);
        let tree = tree_builder.finish();
        let segments = segmenter.segment(&tree.text, None);
        let tree_ref = tree.as_ref();
        let elements = split_element_tree(&tree_ref, &segments).collect::<Vec<_>>();

        let mapper = Script::enum_to_short_name_mapper();
        for (script, level, element) in &elements {
            let script_tag = mapper.get(*script).unwrap();
            let kind = element.kind;
            let text = &tree_ref.text[element.text_range()];
            println!("[{script_tag:?} {level}] {kind:?}: {text:?}");
        }
    }

}
