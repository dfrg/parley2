// Copyright 2021 the Parley Authors
// SPDX-License-Identifier: Apache-2.0 OR MIT

//! Font selection with fallback.

use super::{
    super::{
        element_tree::{ElementKind, ElementTreeRef},
        SpanIndex,
    },
    text::CharState,
};
use crate::font::FontContext;
use alloc::vec::Vec;
use core::ops::Range;
use fontique::{
    Attributes, Collection, FamilyId, GenericFamily, Language, Query, QueryFamily, QueryStatus,
    Synthesis,
};
use icu_properties::Script;
use peniko::Font;
use skrifa::{charmap::Charmap, MetadataProvider, Tag};

/// Font style properties for selection and fallback.
#[derive(Clone, Default, Debug)]
pub struct FontDescriptor {
    pub families: Vec<FamilyId>,
    pub attributes: Attributes,
    pub language: Option<Language>,
    pub variations: Vec<(Tag, f32)>,
}

impl FontDescriptor {
    fn clear(&mut self) {
        self.families.clear();
        self.attributes = Attributes::default();
        self.language = None;
        self.variations.clear();
    }
}

/// Provider that generates a font descriptor for a given span index.
pub trait FontDescriptorProvider {
    fn font_descriptor(
        &self,
        span_index: SpanIndex,
        collection: &mut Collection,
        descriptor: &mut FontDescriptor,
    );
}

/// Resolved font, variation and synthesis for a run of text.
#[derive(Clone, PartialEq, Debug)]
pub struct FontSegment {
    pub font: Font,
    /// Range into the `variation_coords` field in [`FontSegments`].
    pub variation_coords: Range<usize>,
    pub synthesis: Synthesis,
    pub script: Script,
    pub language: Option<Language>,
    pub char_range: Range<usize>,
}

/// Collection of resolved font segments.
#[derive(Clone, Default, Debug)]
pub struct FontAnalysis {
    pub segments: Vec<FontSegment>,
    pub variation_coords: Vec<i16>,
}

impl FontAnalysis {
    pub fn clear(&mut self) {
        self.segments.clear();
        self.variation_coords.clear();
    }
}

/// Analyzer for splitting text into sequences that have the same font,
/// variations and attribute synthesis.
#[derive(Default)]
pub struct FontAnalyzer {
    desc: FontDescriptor,
    cluster_checker: ClusterGlyphChecker,
}

impl FontAnalyzer {
    pub fn analyze(
        &mut self,
        font_cx: &mut FontContext,
        descriptor_provider: &impl FontDescriptorProvider,
        chars: impl Iterator<Item = (char, CharState, SpanIndex)>,
    ) -> FontAnalysis {
        let mut analysis = FontAnalysis::default();
        self.analyze_to(font_cx, descriptor_provider, chars, &mut analysis);
        analysis
    }

    #[inline(never)]
    pub fn analyze_to(
        &mut self,
        font_cx: &mut FontContext,
        descriptor_provider: &impl FontDescriptorProvider,
        chars: impl Iterator<Item = (char, CharState, SpanIndex)>,
        analysis: &mut FontAnalysis,
    ) {
        analysis.clear();
        let script_map = icu_properties::maps::script();
        let mut query = font_cx.collection.query(&mut font_cx.source_cache);
        let mut last_span_index: Option<SpanIndex> = None;
        let mut last_script = None;
        let mut last_language = None;
        // If we have to break a cluster, carry over the state to the next
        // for emoji mapping purposes
        let mut carry_state = None;
        let mut chars = chars.enumerate().peekable();
        // Let's just process one cluster at a time
        while let Some((i, (ch, char_state, span_index))) = chars.next() {
            self.cluster_checker.clear();
            // The initial character state becomes the cluster state
            let cluster_start = i;
            let mut cluster_end = i + 1;
            let cluster_state = if char_state.is_cluster_start() {
                char_state
            } else {
                carry_state.take().unwrap_or(char_state)
            };
            carry_state = Some(cluster_state);
            let cluster_span_index = span_index;
            let cluster_script = script_map.get(ch);
            self.cluster_checker.chars.push(ch);
            // Capture the remaining characters
            while let Some((_, (ch, char_state, span_index))) = chars.peek().copied() {
                if char_state.is_cluster_start() {
                    break;
                }
                // If we have a different span index or script, then break
                // this cluster for font selection purposes. We might be able
                // to recover it later if we happen to choose the same font.
                // Otherwise, this will end up as a broken cluster anyway
                let next_script = script_map.get(ch);
                if (next_script != cluster_script && is_real_script(next_script))
                    || span_index != cluster_span_index
                {
                    break;
                }
                self.cluster_checker.chars.push(ch);
                chars.next();
                cluster_end += 1;
            }
            // Don't break font matching for newlines
            if cluster_state.is_paragraph_separator() {
                if let Some(last_segment) = analysis.segments.last_mut() {
                    last_segment.char_range.end = cluster_end;
                    continue;
                }
            }
            // Let's try to extend the previous segment, if possible
            if last_span_index == Some(cluster_span_index)
                && (last_script == Some(cluster_script) || !is_real_script(cluster_script))
                // TODO: this needs more robust checking
                && last_language.is_none()
            {
                if let Some(last_segment) = analysis.segments.last_mut() {
                    if let Ok(font_ref) = skrifa::FontRef::from_index(
                        last_segment.font.data.as_ref(),
                        last_segment.font.index,
                    ) {
                        let charmap = font_ref.charmap();
                        if self.cluster_checker.compute_score(&charmap) == 1.0 {
                            // Perfect match, so let's take it
                            last_segment.char_range.end = cluster_end;
                            continue;
                        }
                    }
                }
            }
            // Otherwise, let's do the match dance
            last_script = Some(cluster_script);
            last_span_index = Some(cluster_span_index);
            self.desc.clear();
            // Refill our descriptor with the new info
            descriptor_provider.font_descriptor(
                cluster_span_index,
                query.collection(),
                &mut self.desc,
            );
            last_language = self.desc.language.clone();
            // And update the query
            // TODO: handle emoji presentation and flags
            update_query(
                &self.desc,
                &mut query,
                cluster_script,
                cluster_state.is_emoji() || cluster_state.is_flag(),
            );
            // Now run through the fonts
            let mut best_font = None;
            let mut best_score = 0.0;
            query.matches_with(|font| {
                if let Ok(font_ref) = skrifa::FontRef::from_index(font.blob.as_ref(), font.index) {
                    let charmap = font_ref.charmap();
                    let score = self.cluster_checker.compute_score(&charmap);
                    if best_font.is_none() || score > best_score {
                        best_font = Some(font.clone());
                        best_score = score;
                    }
                    if score == 1.0 {
                        QueryStatus::Stop
                    } else {
                        QueryStatus::Continue
                    }
                } else {
                    QueryStatus::Continue
                }
            });
            if let Some(best_font) = best_font {
                analysis.segments.push(FontSegment {
                    font: Font::new(best_font.blob.clone(), best_font.index),
                    variation_coords: 0..0,
                    synthesis: best_font.synthesis,
                    script: cluster_script,
                    language: self.desc.language.clone(),
                    char_range: cluster_start..cluster_end,
                });
            }
        }
    }
}

#[derive(Default)]
struct ClusterGlyphChecker {
    chars: Vec<char>,
    decomposed: Vec<char>,
    composed: Vec<char>,
}

impl ClusterGlyphChecker {
    fn clear(&mut self) {
        self.chars.clear();
        self.decomposed.clear();
        self.composed.clear();
    }

    fn compute_score(&mut self, charmap: &Charmap) -> f32 {
        if self.chars.is_empty() {
            return 1.0;
        }
        let num_default_mapped = count_mapped(&self.chars, charmap);
        let default_score = num_default_mapped as f32 / self.chars.len() as f32;
        let normalized_score = if num_default_mapped < self.chars.len() {
            if self.decomposed.is_empty() {
                self.decomposed.extend(
                    icu_normalizer::DecomposingNormalizer::new_nfd()
                        .normalize_iter(self.chars.iter().copied()),
                );
            }
            let num_decomp_mapped = count_mapped(&self.decomposed, charmap);
            let decomp_score = num_decomp_mapped as f32 / self.decomposed.len() as f32;
            if num_decomp_mapped < self.decomposed.len() {
                if self.composed.is_empty() {
                    self.composed.extend(
                        icu_normalizer::ComposingNormalizer::new_nfc()
                            .normalize_iter(self.decomposed.iter().copied()),
                    );
                }
                let num_comp_mapped = count_mapped(&self.composed, charmap);
                let comp_score = num_comp_mapped as f32 / self.composed.len() as f32;
                comp_score.max(decomp_score)
            } else {
                decomp_score
            }
        } else {
            default_score
        };
        default_score.max(normalized_score)
    }
}

fn count_mapped(chars: &[char], charmap: &Charmap) -> usize {
    chars
        .iter()
        .map(|ch| (charmap.map(*ch).map(|gid| gid.to_u32()).unwrap_or_default() != 0) as usize)
        .sum::<usize>()
}

fn update_query(descriptor: &FontDescriptor, query: &mut Query, script: Script, is_emoji: bool) {
    if is_emoji {
        query.set_families(
            descriptor
                .families
                .iter()
                .map(|id| QueryFamily::Id(*id))
                .chain(Some(QueryFamily::Generic(GenericFamily::Emoji))),
        );
    } else {
        query.set_families(descriptor.families.iter().map(|id| QueryFamily::Id(*id)));
    }
    query.set_attributes(descriptor.attributes);
    if let Some(lang) = &descriptor.language {
        query.set_fallbacks((script, lang));
    } else {
        query.set_fallbacks(script);
    }
}

fn is_real_script(script: Script) -> bool {
    !matches!(script, Script::Common | Script::Inherited)
}

#[cfg(test)]
mod tests {
    use skrifa::string::StringId;

    use super::*;

    #[test]
    fn default_fonts() {
        let text = "And also\rsome\nاللغة العربية arabic\ntext. 🧙🏼‍♀️ ☺ ❤❤️ বিন্ধ্য 🇫🇷 \r\n";
        let chars = text.chars().collect::<Vec<_>>();
        let char_states = super::super::text::TextAnalyzer::new().analyze(text, None);
        let mut fa = FontAnalyzer::default();
        let mut fcx = FontContext::default();
        impl FontDescriptorProvider for () {
            fn font_descriptor(
                &self,
                span_index: SpanIndex,
                collection: &mut Collection,
                descriptor: &mut FontDescriptor,
            ) {
                if span_index == 1 {
                    descriptor.families.push(collection.family_id("Tahoma").unwrap());
                }
            }
        }
        let fonts = fa.analyze(
            &mut fcx,
            &mut (),
            text.chars()
                .zip(char_states)
                .map(|(ch, state)| (ch, state, if ch == 'e' { 1 } else { 0 })),
        );
        for font_seg in &fonts.segments {
            let font_ref =
                skrifa::FontRef::from_index(font_seg.font.data.as_ref(), font_seg.font.index)
                    .unwrap();
            let name = font_ref
                .localized_strings(StringId::TYPOGRAPHIC_FAMILY_NAME)
                .english_or_first()
                .or_else(|| {
                    font_ref
                        .localized_strings(StringId::FAMILY_NAME)
                        .english_or_first()
                })
                .unwrap()
                .to_string();
            let text: String = chars[font_seg.char_range.clone()].iter().collect();
            println!("{name} -> {text:?}");
        }
        //println!("{fonts:?}");
    }
}
