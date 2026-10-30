//! The runtime's view of the compiler-emitted debug tables.
//!
//! The compiler emits `__hew_file_table`, `__hew_site_table`, `__hew_symtab`
//! and `__hew_has_debug_info` into every image, release builds included; the
//! `hew-debug-abi` crate owns their layout. This module is the one place the
//! runtime reads them. An image without the tables (wasm, unit tests, an
//! object built before the tables existed) resolves nothing, and every
//! consumer omits location and frame names silently.
//!
//! The tables are not yet emitted, so every lookup below reports them absent.
//! The readers land with the `hew-debug-abi` layout; their callers already
//! treat absence as the normal case.

use std::ffi::c_char;

/// A resolved source position.
#[derive(Debug, Clone, Copy)]
pub struct SourcePosition {
    /// Source file, NUL-terminated static text from the file table.
    pub file: *const c_char,
    /// Module the position belongs to.
    pub module_id: u32,
    /// One-based line.
    pub line: u32,
    /// One-based column, zero if the table records none.
    pub col: u32,
}

/// A Hew function containing an address.
#[derive(Debug, Clone, Copy)]
pub struct HewFunction {
    /// Display name, NUL-terminated static text from the symbol table.
    pub name: *const c_char,
    /// Source position of the address, if the table records one.
    pub position: Option<SourcePosition>,
}

/// Whether the compiler emitted debug information for this image.
/// Async-signal-safe.
#[must_use]
pub fn has_debug_info() -> bool {
    false
}

/// Resolve the site value codegen attached to a fault.
#[must_use]
pub fn resolve_site(_site: u32) -> Option<SourcePosition> {
    None
}

/// The Hew function whose code contains `pc`. Async-signal-safe.
#[must_use]
pub fn function_at(_pc: usize) -> Option<HewFunction> {
    None
}
