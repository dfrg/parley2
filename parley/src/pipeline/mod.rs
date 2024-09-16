// Copyright 2021 the Parley Authors
// SPDX-License-Identifier: Apache-2.0 OR MIT

//! Stages of text layout.

pub mod element_tree;
pub mod segment;
pub mod analysis;
// pub mod element;

/// Identifier for an element, used to associate layout objects with the
/// user's content.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug)]
pub struct ElementId(pub u64);

/// Type alias for the index of a span.
pub type SpanIndex = u32;
