// Copyright 2024 the Parley Authors
// SPDX-License-Identifier: Apache-2.0 OR MIT

//! Query support.

#[cfg(feature = "std")]
use super::super::{Collection, SourceCache};

#[cfg(not(feature = "std"))]
use alloc::vec::Vec;

use super::super::{Attributes, Blob, FallbackKey, FamilyId, FamilyInfo, GenericFamily, Synthesis};

#[derive(Clone, Default)]
pub(super) struct QueryState {
    families: Vec<CachedFamily>,
    fallback_families: Vec<CachedFamily>,
}

impl QueryState {
    fn clear(&mut self) {
        self.families.clear();
        self.fallback_families.clear();
    }
}

/// State for font selection.
pub struct Query<'a> {
    collection: &'a mut Collection,
    #[cfg(feature = "std")]
    source_cache: &'a mut SourceCache,
    attributes: Attributes,
    fallbacks: Option<FallbackKey>,
}

impl<'a> Query<'a> {
    #[cfg(feature = "std")]
    pub(super) fn new(collection: &'a mut Collection, source_cache: &'a mut SourceCache) -> Self {
        collection.query_state.clear();
        Self {
            collection,
            source_cache,
            attributes: Attributes::default(),
            fallbacks: None,
        }
    }

    /// Returns a mutable reference to the underyling collection.
    pub fn collection(&mut self) -> &mut Collection {
        self.collection
    }

    /// Sets the ordered sequence of families to match against.
    pub fn set_families<'f, I>(&mut self, families: I)
    where
        I: IntoIterator,
        I::Item: Into<QueryFamily<'f>>,
    {
        let collection = &mut self.collection.inner;
        let state = &mut self.collection.query_state;
        state.families.clear();
        for family in families {
            let family = family.into();
            match family {
                QueryFamily::Named(name) => {
                    if let Some(id) = collection.family_id(name) {
                        state.families.push(CachedFamily::new(id));
                    }
                }
                QueryFamily::Id(id) => {
                    state.families.push(CachedFamily::new(id));
                }
                QueryFamily::Generic(generic) => {
                    for id in collection.generic_families(generic) {
                        state.families.push(CachedFamily::new(id));
                    }
                }
            }
        }
    }

    /// Sets the primary attributes to match against.
    pub fn set_attributes(&mut self, attributes: Attributes) {
        if self.attributes != attributes {
            for family in &mut self.collection.query_state.families {
                family.clear_fonts();
            }
            self.attributes = attributes;
        }
    }

    /// Sets the script and locale for fallback fonts.
    pub fn set_fallbacks(&mut self, key: impl Into<FallbackKey>) {
        let key = key.into();
        if self.fallbacks != Some(key) {
            self.collection.query_state.fallback_families.clear();
            self.collection.query_state.fallback_families.extend(
                self.collection
                    .inner
                    .fallback_families(key)
                    .map(CachedFamily::new),
            );
            self.fallbacks = Some(key);
        }
    }

    /// Invokes the given callback with all fonts that match the current
    /// settings.
    #[cfg(feature = "std")]
    pub fn matches_with(&mut self, mut f: impl FnMut(&QueryFont) -> QueryStatus) {
        let collection = &mut self.collection.inner;
        let state = &mut self.collection.query_state;
        for family in state
            .families
            .iter_mut()
            .chain(state.fallback_families.iter_mut())
        {
            match &mut family.family {
                Entry::Error => continue,
                Entry::Ok(..) => {}
                status @ Entry::Vacant => {
                    if let Some(info) = collection.family(family.id) {
                        *status = Entry::Ok(info);
                    } else {
                        *status = Entry::Error;
                        continue;
                    }
                }
            }
            let Entry::Ok(family_info) = &family.family else {
                continue;
            };
            let mut best_index = None;
            if let Some(font) = load_font(
                family_info,
                self.attributes,
                &mut family.best,
                false,
                self.source_cache,
            ) {
                best_index = Some(font.family.1);
                if f(font) == QueryStatus::Stop {
                    return;
                }
            }
            // Don't invoke for the default font if it's the same as the
            // best match.
            if best_index == Some(family_info.default_font_index()) {
                continue;
            }
            if let Some(font) = load_font(
                family_info,
                self.attributes,
                &mut family.default,
                true,
                self.source_cache,
            ) {
                if f(font) == QueryStatus::Stop {
                    return;
                }
            }
        }
    }
}

impl Drop for Query<'_> {
    fn drop(&mut self) {
        self.collection.query_state.clear();
    }
}

/// Determines whether a font query operation will continue.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum QueryStatus {
    /// Query should continue with the next font.
    Continue,
    /// Query should stop.
    Stop,
}

/// Family descriptor for a font query.
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum QueryFamily<'a> {
    /// A named font family.
    Named(&'a str),
    /// An identifier for a font family.
    Id(FamilyId),
    /// A generic font family.
    Generic(GenericFamily),
}

impl<'a> From<&'a str> for QueryFamily<'a> {
    fn from(value: &'a str) -> Self {
        Self::Named(value)
    }
}

impl From<FamilyId> for QueryFamily<'static> {
    fn from(value: FamilyId) -> Self {
        Self::Id(value)
    }
}

impl From<GenericFamily> for QueryFamily<'static> {
    fn from(value: GenericFamily) -> Self {
        Self::Generic(value)
    }
}

/// Candidate font generated by a query.
#[derive(Clone, Debug)]
pub struct QueryFont {
    /// Family identifier and index of the font in the family font list.
    pub family: (FamilyId, usize),
    /// Blob containing the font data.
    pub blob: Blob<u8>,
    /// Index of a font in a font collection (ttc) file.
    pub index: u32,
    /// Synthesis suggestions for this font based on the requested attributes.
    pub synthesis: Synthesis,
}

#[cfg(feature = "std")]
fn load_font<'a>(
    family: &FamilyInfo,
    attributes: Attributes,
    font: &'a mut Entry<QueryFont>,
    is_default: bool,
    source_cache: &mut SourceCache,
) -> Option<&'a QueryFont> {
    match font {
        Entry::Error => None,
        Entry::Ok(font) => Some(font),
        status @ Entry::Vacant => {
            // Set to error in case we fail. This simplifies
            // the following code.
            *status = Entry::Error;
            let family_index = if is_default {
                family.default_font_index()
            } else {
                family.match_index(
                    attributes.stretch,
                    attributes.style,
                    attributes.weight,
                    true,
                )?
            };
            let font_info = family.fonts().get(family_index)?;
            let blob = font_info.load(Some(source_cache))?;
            let blob_index = font_info.index();
            let synthesis =
                font_info.synthesis(attributes.stretch, attributes.style, attributes.weight);
            *status = Entry::Ok(QueryFont {
                family: (family.id(), family_index),
                blob: blob.clone(),
                index: blob_index,
                synthesis,
            });
            if let Entry::Ok(font) = status {
                Some(font)
            } else {
                None
            }
        }
    }
}

#[derive(Clone)]
struct CachedFamily {
    id: FamilyId,
    family: Entry<FamilyInfo>,
    best: Entry<QueryFont>,
    default: Entry<QueryFont>,
}

impl CachedFamily {
    fn new(id: FamilyId) -> Self {
        Self {
            id,
            family: Entry::Vacant,
            best: Entry::Vacant,
            default: Entry::Vacant,
        }
    }

    fn clear_fonts(&mut self) {
        self.best = Entry::Vacant;
        self.default = Entry::Vacant;
    }
}

#[derive(Clone)]
enum Entry<T> {
    Ok(T),
    Vacant,
    Error,
}
