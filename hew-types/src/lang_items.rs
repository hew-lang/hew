//! Compiler-recognised "lang item" registry.
//!
//! Trait declarations and trait methods can carry a `#[lang_item("key")]`
//! attribute that names them for compiler-special handling. This registry
//! collects those tags during trait registration so downstream passes
//! (HIR lowering in particular) can discover the trait/method symbols by
//! their **role** rather than by hard-coding the surface names.
//!
//! Two kinds of entries coexist in one map:
//!
//! * Trait-level: `#[lang_item("display")]` on a `trait Foo { ... }` →
//!   `{"display" → LangItemBinding { trait_name: "Foo", method_name: None }}`.
//! * Method-level: `#[lang_item("display_fmt")]` on `fn bar(...)` inside a
//!   tagged trait → `{"display_fmt" → LangItemBinding { trait_name: "Foo",
//!   method_name: Some("bar") }}`. The trait name is propagated from the
//!   enclosing trait so HIR can derive the impl symbol
//!   (`<SelfType>::<method_name>`).
//!
//! The HIR f-string lowering pass consults two well-known keys
//! ([`LANG_ITEM_DISPLAY`] and [`LANG_ITEM_DISPLAY_FMT`]) so renaming the
//! `Display` trait or its `fmt` method in stdlib only requires moving the
//! attributes — no code change.

use std::collections::HashMap;

use strum::{EnumIter, IntoEnumIterator};

/// Closed compiler-recognised lang-item key vocabulary.
///
/// The stdlib may move these attributes between declarations, but adding a new
/// semantic hook requires a compiler change so misspelled keys fail closed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumIter)]
pub enum LangItem {
    Index,
    IndexGet,
    IndexAt,
    Display,
    DisplayFmt,
    Option,
    OptionIsSome,
    OptionIsNone,
    OptionUnwrap,
    OptionUnwrapOr,
    Result,
    ResultIsOk,
    ResultIsErr,
    ResultUnwrap,
    ResultUnwrapOr,
    VecIterState,
    Iterator,
    IteratorNext,
    CollectionNew,
    MarkerSend,
    MarkerSync,
    MarkerFrozen,
    MarkerCopy,
    MarkerClone,
    MarkerEq,
    MarkerPartialOrd,
    MarkerOrd,
    MarkerNum,
    MarkerHash,
    MarkerDebug,
    MarkerDrop,
    MarkerDecode,
    MarkerEncode,
    MarkerSerializable,
    MarkerRcFree,
    MarkerResource,
}

impl LangItem {
    #[must_use]
    pub const fn key(self) -> &'static str {
        match self {
            Self::Index => "index",
            Self::IndexGet => "index_get",
            Self::IndexAt => "index_at",
            Self::Display => "display",
            Self::DisplayFmt => "display_fmt",
            Self::Option => "option",
            Self::OptionIsSome => "option.is_some",
            Self::OptionIsNone => "option.is_none",
            Self::OptionUnwrap => "option.unwrap",
            Self::OptionUnwrapOr => "option.unwrap_or",
            Self::Result => "result",
            Self::ResultIsOk => "result.is_ok",
            Self::ResultIsErr => "result.is_err",
            Self::ResultUnwrap => "result.unwrap",
            Self::ResultUnwrapOr => "result.unwrap_or",
            Self::VecIterState => "vec.iter_state",
            Self::Iterator => "iterator",
            Self::IteratorNext => "iterator.next",
            Self::CollectionNew => "collection.new",
            Self::MarkerSend => "marker.send",
            Self::MarkerSync => "marker.sync",
            Self::MarkerFrozen => "marker.frozen",
            Self::MarkerCopy => "marker.copy",
            Self::MarkerClone => "marker.clone",
            Self::MarkerEq => "marker.eq",
            Self::MarkerPartialOrd => "marker.partial_ord",
            Self::MarkerOrd => "marker.ord",
            Self::MarkerNum => "marker.num",
            Self::MarkerHash => "marker.hash",
            Self::MarkerDebug => "marker.debug",
            Self::MarkerDrop => "marker.drop",
            Self::MarkerDecode => "marker.decode",
            Self::MarkerEncode => "marker.encode",
            Self::MarkerSerializable => "marker.serializable",
            Self::MarkerRcFree => "marker.rc_free",
            Self::MarkerResource => "marker.resource",
        }
    }

    #[must_use]
    pub fn from_key(key: &str) -> Option<Self> {
        Self::iter().find(|item| item.key() == key)
    }
}

/// Well-known lang-item key for the trait through which f-string
/// interpolation dispatches.
pub const LANG_ITEM_DISPLAY: &str = LangItem::Display.key();

/// Well-known lang-item key for the method on the `display` trait used to
/// render an interpolant to a `string`.
pub const LANG_ITEM_DISPLAY_FMT: &str = LangItem::DisplayFmt.key();

/// One resolved lang-item entry: the trait that owns the tag plus, for
/// method-level tags, the method's surface name.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LangItemBinding {
    /// Surface name of the trait carrying the tag (or owning the tagged
    /// method).
    pub trait_name: String,
    /// `Some(method_name)` for method-level entries; `None` for trait-level
    /// entries.
    pub method_name: Option<String>,
}

/// Lang-item lookup table populated during trait registration and surfaced
/// through [`crate::TypeCheckOutput::lang_items`].
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct LangItemRegistry {
    entries: HashMap<String, LangItemBinding>,
}

impl LangItemRegistry {
    /// Construct an empty registry.
    #[must_use]
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
        }
    }

    /// Number of registered entries.
    #[must_use]
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    /// `true` when no entries are registered.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    /// Look up an entry by its lang-item key.
    #[must_use]
    pub fn get(&self, key: &str) -> Option<&LangItemBinding> {
        self.entries.get(key)
    }

    /// Insert a binding for `key`. Returns the previous binding if one
    /// existed (callers may surface a duplicate-definition diagnostic).
    pub fn insert(
        &mut self,
        key: impl Into<String>,
        binding: LangItemBinding,
    ) -> Option<LangItemBinding> {
        self.entries.insert(key.into(), binding)
    }

    /// Convenience accessor: the trait name registered under [`LANG_ITEM_DISPLAY`].
    #[must_use]
    pub fn display_trait(&self) -> Option<&str> {
        self.entries
            .get(LANG_ITEM_DISPLAY)
            .map(|b| b.trait_name.as_str())
    }

    /// Convenience accessor: the method name registered under
    /// [`LANG_ITEM_DISPLAY_FMT`].
    #[must_use]
    pub fn display_method(&self) -> Option<&str> {
        self.entries
            .get(LANG_ITEM_DISPLAY_FMT)
            .and_then(|b| b.method_name.as_deref())
    }

    /// Iterate registered (key, binding) pairs in arbitrary order.
    pub fn iter(&self) -> impl Iterator<Item = (&String, &LangItemBinding)> {
        self.entries.iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn every_lang_item_key_round_trips() {
        for item in LangItem::iter() {
            assert_eq!(LangItem::from_key(item.key()), Some(item));
        }
    }
}
