//! Shared symbol mangling for concrete declarations and layout carriers.

pub mod mangle;

pub use mangle::{mangle_instantiation, sanitize_for_symbol, ConstValue, SymbolClass};
