// Copyright 2021 the Parley Authors
// SPDX-License-Identifier: Apache-2.0 OR MIT

//! Boundary analysis.

use super::CharState;
use icu_segmenter::{GraphemeClusterSegmenter, LineSegmenter, WordSegmenter};

/// Analyzer for computing boundary properties.
pub struct BoundaryAnalyzer {
    graphemes: GraphemeClusterSegmenter,
    words: WordSegmenter,
    lines: LineSegmenter,
}

impl BoundaryAnalyzer {
    /// Creates a new boundary analyzer.
    pub fn new() -> Self {
        Self {
            graphemes: GraphemeClusterSegmenter::new(),
            words: WordSegmenter::new_auto(),
            lines: LineSegmenter::new_auto(),
        }
    }

    /// Returns the boundary state for each character in the given text.
    pub fn analyze<'a>(&'a self, text: &'a str) -> impl Iterator<Item = CharState> + 'a {
        const EMOJI_PRESENTATION: char = '\u{FE0F}';
        const TEXT_PRESENTATION: char = '\u{FE0E}';
        let ext_pict = icu_properties::sets::extended_pictographic();
        let emoji_pres = icu_properties::sets::emoji_presentation();
        let gcb = icu_properties::maps::grapheme_cluster_break();
        let mut graphemes = BoundaryTracker::new(self.graphemes.segment_str(text));
        let mut words = BoundaryTracker::new(self.words.segment_str(text));
        let mut lines = BoundaryTracker::new(self.lines.segment_str(text));
        let mut boundary_and_initial_content = move |byte_ix, ch| {
            if graphemes.is_boundary(byte_ix) {
                let mut boundary = if lines.is_boundary(byte_ix) {
                    CharState::LINE
                } else if words.is_boundary(byte_ix) {
                    CharState::WORD
                } else {
                    CharState::CLUSTER
                };
                if is_paragraph_separator(ch) {
                    boundary |= CharState::PARAGRAPH_SEPARATOR;
                }
                let content = if ext_pict.contains(ch) {
                    if emoji_pres.contains(ch) {
                        CharState::EMOJI_PRESENTATION
                    } else {
                        CharState::TEXT_PRESENTATION
                    }
                } else if gcb.get(ch) == icu_properties::GraphemeClusterBreak::RegionalIndicator {
                    CharState::FLAG
                } else {
                    0
                };
                Some((boundary, content))
            } else {
                None
            }
        };
        let mut chars = text.char_indices().chain(Some((text.len(), ' ')));
        let mut last_boundary = CharState::LINE;
        let mut last_content = 0;
        let mut last_trailing = 0;
        let mut pending_trailing = 0;
        core::iter::from_fn(move || {
            if pending_trailing > 0 {
                pending_trailing -= 1;
                return Some(CharState::default());
            }
            while let Some((byte_ix, ch)) = chars.next() {
                if let Some((boundary, content)) = boundary_and_initial_content(byte_ix, ch) {
                    if byte_ix > 0 {
                        let state = CharState::from_flags(last_boundary | last_content);
                        last_boundary = boundary;
                        last_content = content;
                        pending_trailing = last_trailing;
                        last_trailing = 0;
                        return Some(state);
                    } else {
                        last_boundary = boundary;
                        last_content = content;
                    }
                } else {
                    last_trailing += 1;
                    if CharState::from_flags(last_content).is_emoji() {
                        if ch == EMOJI_PRESENTATION {
                            last_content = CharState::EMOJI_PRESENTATION;
                        } else if ch == TEXT_PRESENTATION {
                            last_content = CharState::TEXT_PRESENTATION;
                        }
                    }
                }
            }
            None
        })
    }
}

/// Helper for syncing boundary state tracking between
/// multiple iterators.
struct BoundaryTracker<T> {
    iter: T,
    cur_ix: usize,
}

impl<T> BoundaryTracker<T>
where
    T: Iterator<Item = usize>,
{
    fn new(iter: T) -> Self {
        // The first character is always a valid boundary
        Self { iter, cur_ix: 0 }
    }

    /// Is the given byte index a boundary state according to the inner
    /// iterator?
    fn is_boundary(&mut self, ix: usize) -> bool {
        if ix == self.cur_ix {
            true
        } else if ix < self.cur_ix {
            false
        } else {
            while let Some(next_ix) = self.iter.next() {
                if next_ix >= ix {
                    self.cur_ix = next_ix;
                    return next_ix == ix;
                }
            }
            false
        }
    }
}

fn is_paragraph_separator(ch: char) -> bool {
    matches!(ch, '\n' | '\r' | '\u{2029}' | '\u{2028}' | '\u{000B}' | '\u{000C}')
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dump_boundaries2() {
        let text =
            "❤️ a 🏉 rugby  football and an 🏈\u{FE0E} american football 🧙🏼‍♀️ ☺ ❤❤️ বিন্ধ্য 🇫🇷 \r\n";
        let boundaries = BoundaryAnalyzer::new().analyze(text).collect::<Vec<_>>();
        for (i, ch) in text.chars().enumerate() {
            println!("[{i}] {:?} {ch:?}", boundaries[i]);
        }
    }

    #[test]
    fn dump_boundaries() {
        let text =
            "❤️ a 🏉 rugby  football and an 🏈\u{FE0E} american football 🧙🏼‍♀️ ☺ ❤❤️ বিন্ধ্য 🇫🇷 \r\n";
        let boundaries = BoundaryAnalyzer::new().analyze(text).collect::<Vec<_>>();
        for (i, ch) in text.chars().enumerate() {
            println!("[{i}] {:?} {ch:?}", boundaries[i]);
        }
    }
}
