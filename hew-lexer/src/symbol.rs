//! Interned identifier spellings and syntax contexts.
//!
//! A [`Symbol`] is a spelling interned once per process: equality and hashing
//! are integer operations and the spelling is recovered with [`Symbol::as_str`].
//! The interner is process-global and append-only, so a `Symbol` is valid for
//! the life of the process and can be formatted without a table in hand.
//!
//! Ordering is by spelling, never by interning order: interning order depends
//! on what the process parsed first (an LSP session interns partial
//! identifiers as the user types), so any sorted output keyed by `Symbol` must
//! not depend on it.

use std::collections::HashMap;
use std::fmt;
use std::sync::{OnceLock, RwLock};

/// An interned identifier spelling.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Symbol(u32);

/// Spellings the resolver and parser decide on, interned at fixed indices so
/// they are usable as constants and in patterns.
pub mod sym {
    use super::Symbol;

    pub const SELF_VALUE: Symbol = Symbol(0);
    pub const SELF_TYPE: Symbol = Symbol(1);
    pub const MAIN: Symbol = Symbol(2);
    pub const UNDERSCORE: Symbol = Symbol(3);
    pub const SOME: Symbol = Symbol(4);
    pub const NONE: Symbol = Symbol(5);
    pub const OK: Symbol = Symbol(6);
    pub const ERR: Symbol = Symbol(7);
    pub const EVENT: Symbol = Symbol(8);

    /// Spellings of the constants above, in index order.
    pub(super) const PREINTERNED: [&str; 9] = [
        "self", "Self", "main", "_", "Some", "None", "Ok", "Err", "Event",
    ];
}

struct Interner {
    by_spelling: HashMap<&'static str, Symbol>,
    spellings: Vec<&'static str>,
}

impl Interner {
    fn new() -> Self {
        let mut interner = Self {
            by_spelling: HashMap::new(),
            spellings: Vec::new(),
        };
        for spelling in sym::PREINTERNED {
            interner.insert(spelling);
        }
        interner
    }

    fn insert(&mut self, spelling: &'static str) -> Symbol {
        let index = u32::try_from(self.spellings.len()).expect("symbol table exceeds u32 range");
        let symbol = Symbol(index);
        self.spellings.push(spelling);
        self.by_spelling.insert(spelling, symbol);
        symbol
    }
}

fn interner() -> &'static RwLock<Interner> {
    static INTERNER: OnceLock<RwLock<Interner>> = OnceLock::new();
    INTERNER.get_or_init(|| RwLock::new(Interner::new()))
}

impl Symbol {
    /// Intern `spelling`, returning the one symbol for it.
    ///
    /// # Panics
    ///
    /// Panics if the interner lock is poisoned or the table outgrows `u32`.
    #[must_use]
    pub fn intern(spelling: &str) -> Symbol {
        if let Some(symbol) = interner()
            .read()
            .expect("symbol interner poisoned")
            .by_spelling
            .get(spelling)
        {
            return *symbol;
        }
        let mut table = interner().write().expect("symbol interner poisoned");
        if let Some(symbol) = table.by_spelling.get(spelling) {
            return *symbol;
        }
        let leaked: &'static str = Box::leak(spelling.to_owned().into_boxed_str());
        table.insert(leaked)
    }

    /// The spelling this symbol was interned from.
    ///
    /// # Panics
    ///
    /// Panics if the interner lock is poisoned.
    #[must_use]
    pub fn as_str(self) -> &'static str {
        interner()
            .read()
            .expect("symbol interner poisoned")
            .spellings[self.0 as usize]
    }
}

impl PartialOrd for Symbol {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Symbol {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        if self == other {
            return std::cmp::Ordering::Equal;
        }
        self.as_str().cmp(other.as_str())
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.as_str(), f)
    }
}

impl serde::Serialize for Symbol {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(self.as_str())
    }
}

impl<'de> serde::Deserialize<'de> for Symbol {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let spelling = <std::borrow::Cow<'de, str>>::deserialize(deserializer)?;
        Ok(Symbol::intern(&spelling))
    }
}

/// The hygiene context of an identifier: which expansion wrote it.
///
/// Every identifier the parser reads from source carries [`SyntaxContext::ROOT`].
/// A macro expansion mints further contexts; resolution binds an identifier by
/// its `(Symbol, SyntaxContext)` pair.
#[derive(
    Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
#[serde(transparent)]
pub struct SyntaxContext(u32);

impl SyntaxContext {
    /// The context of source-written identifiers.
    pub const ROOT: SyntaxContext = SyntaxContext(0);

    /// The context at `row` of a compilation's context table. Only that
    /// table mints contexts; the row is meaningless outside it.
    #[must_use]
    pub const fn from_row(row: u32) -> Self {
        Self(row)
    }

    /// This context's row in its compilation's context table.
    #[must_use]
    pub const fn row(self) -> u32 {
        self.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn interning_is_idempotent_and_round_trips() {
        let a = Symbol::intern("interning_probe");
        assert_eq!(a, Symbol::intern("interning_probe"));
        assert_ne!(a, Symbol::intern("interning_probe_other"));
        assert_eq!(a.as_str(), "interning_probe");
    }

    #[test]
    fn preinterned_constants_match_their_spellings() {
        for (index, spelling) in sym::PREINTERNED.iter().enumerate() {
            assert_eq!(
                Symbol::intern(spelling),
                Symbol(u32::try_from(index).unwrap())
            );
        }
        assert_eq!(sym::MAIN.as_str(), "main");
    }

    #[test]
    fn ordering_follows_spelling_not_interning_order() {
        let interned_first = Symbol::intern("zzz_ordering_probe");
        let interned_second = Symbol::intern("aaa_ordering_probe");
        assert!(interned_second < interned_first);
        assert!(sym::ERR < sym::OK);
    }

    #[test]
    fn serializes_as_its_spelling() {
        let symbol = Symbol::intern("serde_probe");
        let json = serde_json::to_string(&symbol).unwrap();
        assert_eq!(json, "\"serde_probe\"");
        assert_eq!(serde_json::from_str::<Symbol>(&json).unwrap(), symbol);
    }
}
