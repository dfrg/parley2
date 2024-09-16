// Copyright 2021 the Parley Authors
// SPDX-License-Identifier: Apache-2.0 OR MIT

//! Font segmentation.

use super::{
    super::{
        element_tree::{ElementKind, ElementTreeRef},
        SpanIndex,
    },
    cluster::Cluster,
};
use crate::font::FontContext;
use alloc::vec::Vec;
use core::ops::Range;
use fontique::{Attributes, FamilyId, Language, Synthesis};
use icu_properties::Script;
use peniko::Font;
use skrifa::{charmap::Charmap, Tag};

/// Style properties for font selection and fallback.
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

pub trait FontDescriptorProvider {
    fn font_style(&self, span_index: SpanIndex, descriptor: &mut FontDescriptor);
}

/// Resolved font, variation and synthesis for a run of text.
#[derive(Clone, PartialEq, Debug)]
pub struct FontSegment {
    pub font: Font,
    /// Range into the `variation_coords` field in [`FontSegments`].
    pub variation_coords: Range<usize>,
    pub synthesis: Synthesis,
    pub script: Script,
    pub language: Language,
    pub text_range: Range<usize>,
}

/// Collection of resolved font segments.
#[derive(Clone, Default)]
pub struct FontSegments {
    pub segments: Vec<FontSegment>,
    pub variation_coords: Vec<i16>,
}

impl FontSegments {
    pub fn clear(&mut self) {
        self.segments.clear();
        self.variation_coords.clear();
    }
}

/// Segmenter for splitting text into sequences that have the same font,
/// variations and attribute synthesis.
#[derive(Default)]
pub struct FontSegmenter {
    style: FontDescriptor,
    chars: Vec<char>,
    decomposed: Vec<char>,
    composed: Vec<char>,
}

impl FontSegmenter {
    pub fn segment<'a>(
        &mut self,
        font_cx: &mut FontContext,
        style_provider: &impl FontDescriptorProvider,
        span_index_ranges: impl Iterator<Item = (SpanIndex, Range<usize>)>,
        clusters: impl Iterator<Item = Cluster>,
    ) -> FontSegments {
        let mut segments = FontSegments::default();
        self.segment_to(
            font_cx,
            style_provider,
            span_index_ranges,
            clusters,
            &mut segments,
        );
        segments
    }

    pub fn segment_to<'a>(
        &mut self,
        font_cx: &mut FontContext,
        style_provider: &impl FontDescriptorProvider,
        mut span_index_ranges: impl Iterator<Item = (SpanIndex, Range<usize>)>,
        clusters: impl Iterator<Item = Cluster>,
        segments: &mut FontSegments,
    ) {
        segments.clear();
        let mut query = font_cx.collection.query(&mut font_cx.source_cache);
        let mut cur_span_index: Option<SpanIndex> = None;
        let mut cur_span_range = 0..0;
        let mut cur_font: Option<FontSegment> = None;
        for cluster in clusters {
            if !cur_span_range.contains(&cluster.text_range.start) {}
        }
    }

    fn compute_score(&mut self, text: &str, charmap: &Charmap) -> f32 {
        self.chars.clear();
        self.composed.clear();
        self.decomposed.clear();
        0.0
    }
}

fn is_real_script(script: Script) -> bool {
    !matches!(script, Script::Common | Script::Inherited)
}

/// Given an iterator of clusters and span ranges, returns a new iterator
/// yielding the span index for each cluster.
fn cluster_spans(
    clusters: impl Iterator<Item = Cluster>,
    mut span_index_ranges: impl Iterator<Item = (SpanIndex, Range<usize>)>,
) -> impl Iterator<Item = (SpanIndex, Cluster)> {
    let mut cur_span = span_index_ranges.next();
    let mut cur_span_index = cur_span.as_ref().map(|span| span.0).unwrap_or_default();
    clusters.map(move |cluster| {
        loop {
            if let Some(span) = &cur_span {
                if span.1.contains(&cluster.text_range.start)
                    || cluster.text_range.start < span.1.start
                {
                    return (cur_span_index, cluster);
                }
                cur_span = span_index_ranges.next();
                cur_span_index = cur_span.as_ref().map(|span| span.0).unwrap_or_default();
            }
        }
        (0, cluster)
    })
}
