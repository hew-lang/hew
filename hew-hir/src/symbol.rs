//! Linker symbols derived from declaration identity.
//!
//! A symbol renders a declaration through [`hew_types::DefTable::path`], so it
//! depends on the declaration's path and never on its row index: inserting a
//! declaration elsewhere in the program shifts ids but no symbol.

use hew_types::{DefId, DefTable};

/// The linker-safe spelling of a declaration's canonical path
/// (`pkg.helper` -> `pkg$helper`).
#[must_use]
pub fn declaration_symbol(defs: &DefTable, declaration: DefId) -> String {
    crate::mangle_dotted_name(defs.path(declaration))
}
