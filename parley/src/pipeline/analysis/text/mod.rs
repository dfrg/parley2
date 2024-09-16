// Copyright 2021 the Parley Authors
// SPDX-License-Identifier: Apache-2.0 OR MIT

//! Font independent text analysis.

mod bidi;
mod boundary;

use alloc::vec::Vec;
use core::{borrow::Borrow, fmt, ops::Range};

/// Analyzer for determining font independent text properties.
#[derive(Default)]
pub struct TextAnalyzer {
    bidi_analyzer: bidi::BidiAnalyzer,
}

impl TextAnalyzer {
    /// Creates a new text analyzer.
    pub fn new() -> Self {
        Self::default()
    }

    pub fn analyze(&mut self, text: &str, base_direction: Option<BidiDirection>) -> Vec<CharState> {
        let mut vec = Vec::new();
        self.analyze_to(text, base_direction, &mut vec);
        vec
    }

    pub fn analyze_to(
        &mut self,
        text: &str,
        base_direction: Option<BidiDirection>,
        analysis: &mut Vec<CharState>,
    ) {
        analysis.clear();

        analysis.extend(boundary::BoundaryAnalyzer::new().analyze(text));
        let bc = icu_properties::maps::bidi_class();
        for (para, char_range) in split_paragraphs(text) {
            self.bidi_analyzer.clear();
            self.bidi_analyzer.resolve(
                para.chars().map(|ch| (ch, bc.get(ch).0)),
                base_direction.map(|dir| dir as u8),
            );
            let base_flags = if self.bidi_analyzer.base_level() != 0 {
                CharState::BASE_DIRECTION
            } else {
                0
            };
            for (state, level) in analysis[char_range]
                .iter_mut()
                .zip(self.bidi_analyzer.levels())
            {
                state.flags |= base_flags;
                state.bidi_level = *level;
            }
        }
    }
}

// A direction specific to bidirectional text resolution.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum BidiDirection {
    /// Text ordered visually from left to right.
    LeftToRight = 0,
    /// Text ordered visually from right to left.
    RightToLeft = 1,
}

impl BidiDirection {
    /// Returns the associated default bidirectional level for this direction.
    pub fn level(self) -> BidiLevel {
        BidiLevel(self as u8)
    }
}

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

/// Text analysis results for a single character.
#[derive(Copy, Clone, PartialEq, Eq, Default)]
pub struct CharState {
    flags: u8,
    bidi_level: u8,
}

impl CharState {
    const BOUNDARY_MASK: u8 = 0b11;
    const CLUSTER: u8 = 1;
    const WORD: u8 = 2;
    const LINE: u8 = 3;
    const CONTENT_MASK: u8 = 0b1100;
    const FLAG: u8 = 0b0100;
    const EMOJI_PRESENTATION: u8 = 0b1000;
    const TEXT_PRESENTATION: u8 = 0b1100;
    const PARAGRAPH_SEPARATOR: u8 = 0b10000;
    const BASE_DIRECTION: u8 = 0b100000;

    fn from_flags(flags: u8) -> Self {
        Self {
            flags,
            bidi_level: 0,
        }
    }

    /// Returns true if this is the start of a grapheme cluster.
    pub fn is_cluster_start(self) -> bool {
        self.flags & Self::BOUNDARY_MASK != 0
    }

    /// Returns true if this is a word boundary.
    pub fn is_word_boundary(self) -> bool {
        self.flags & Self::BOUNDARY_MASK >= Self::WORD
    }

    /// Returns true if this is a line break opportunity.
    pub fn is_line_break_opportunity(self) -> bool {
        self.flags & Self::BOUNDARY_MASK == Self::LINE
    }

    /// Returns true if this is a paragraph separator (i.e. a newline
    /// character).
    pub fn is_paragraph_separator(self) -> bool {
        self.flags & Self::PARAGRAPH_SEPARATOR != 0
    }

    /// Returns true if this is the start of an emoji cluster.
    pub fn is_emoji(self) -> bool {
        self.flags & Self::CONTENT_MASK > Self::FLAG
    }

    /// Returns true if this is the start of an emoji cluster with
    /// emoji (typically colorful) presentation.
    pub fn has_emoji_presentation(self) -> bool {
        self.flags & Self::CONTENT_MASK == Self::EMOJI_PRESENTATION
    }

    /// Returns true if this is the start of an emoji cluster with
    /// text presentation.
    pub fn has_text_presentation(self) -> bool {
        self.flags & Self::CONTENT_MASK == Self::TEXT_PRESENTATION
    }

    /// Returns true if this is the start of a flag (regional
    /// indicator) cluster.
    pub fn is_flag(self) -> bool {
        self.flags & Self::CONTENT_MASK == Self::FLAG
    }

    /// Returns the base direction of the paragraph containing this character.
    pub fn paragraph_direction(self) -> BidiDirection {
        if self.flags & Self::BASE_DIRECTION != 0 {
            BidiDirection::RightToLeft
        } else {
            BidiDirection::LeftToRight
        }
    }

    /// Returns the resolved bidirectional level for this character.
    pub fn bidi_level(self) -> BidiLevel {
        BidiLevel(self.bidi_level)
    }
}

impl fmt::Debug for CharState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_cluster_start() {
            let boundary = if self.is_paragraph_separator() {
                "P"
            } else if self.is_line_break_opportunity() {
                "L"
            } else if self.is_word_boundary() {
                "W"
            } else {
                "G"
            };
            let content = if self.has_emoji_presentation() {
                "e"
            } else if self.has_text_presentation() {
                "t"
            } else if self.is_flag() {
                "t"
            } else {
                "_"
            };
            write!(f, "{boundary}{content}")?;
        } else {
            write!(f, "__")?;
        }
        write!(f, ":{}", self.bidi_level)
    }
}

/// Given an iterator of character states, returns a new iterator yielding the
/// state for each character that begins a grapheme cluster along with the
/// length (in characters) of that cluster.
pub fn clusters<I>(states: I) -> impl Iterator<Item = (CharState, u32)>
where
    I: IntoIterator,
    I::Item: Borrow<CharState>,
{
    let mut states = states.into_iter().map(|state| *state.borrow()).peekable();
    core::iter::from_fn(move || {
        // Note: we always assume the first character is a cluster boundary
        let state = states.next()?;
        let mut count = 1;
        while let Some(next_state) = states.peek() {
            if next_state.is_cluster_start() {
                break;
            }
            count += 1;
            states.next();
        }
        Some((state, count))
    })
}

/// Returns the content and character range for each paragraph in the
/// given text.
fn split_paragraphs(text: &str) -> impl Iterator<Item = (&str, Range<usize>)> {
    let mut chars = text.chars().peekable();
    let mut start = 0;
    let mut end = 0;
    let mut char_start = 0;
    let mut char_end = 0;
    core::iter::from_fn(move || loop {
        let ch = chars.next()?;
        let next_ch = chars.peek().copied();
        char_end += 1;
        end += ch.len_utf8();
        match ch {
            '\n' | '\u{2029}' | '\u{2028}' | '\u{000B}' | '\u{000C}' => {}
            '\r' => {
                if next_ch == Some('\n') {
                    end += 1;
                    char_end += 1;
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
        let char_range = char_start..char_end;
        char_start = char_end;
        return Some((para, char_range));
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn complex_bidirectional() {
        let text = "And also\rsome\nاللغة العربية arabic\ntext. 🧙🏼‍♀️ ☺ ❤❤️ বিন্ধ্য 🇫🇷 \r\n";
        let mut analyzer = TextAnalyzer::new();
        let states = analyzer.analyze(text, None);
        let mut text_chars = text.chars();
        for (cluster_start, len) in clusters(&states) {
            let cluster_text = text_chars.by_ref().take(len as usize).collect::<Vec<_>>();
            println!("{cluster_start:?} {cluster_text:?}");
        }
    }
}
