//! The runtime's view of the compiler-emitted debug tables.
//!
//! The compiler emits `__hew_file_table`, `__hew_site_table`, `__hew_symtab`
//! and `__hew_has_debug_info` into every native image, release builds
//! included, and the file and site tables into wasm modules; the
//! `hew-debug-abi` crate owns their layout and this module is the one place
//! the runtime reads them. An image without a table (a Rust test binary, an
//! object built before the tables existed, the function table of a wasm
//! module) resolves nothing, and every consumer omits location and frame
//! names silently.
//!
//! `debug_tables.c` gives every table a weak zeroed default the program's own
//! table replaces at link time; a zero magic reads as absent. Every
//! lookup only reads them in place (no allocation, locks or thread-locals),
//! so a fault handler may call it.

use std::ffi::c_char;

use hew_debug_abi::{Table, TableHeader, TableKind, NO_FILE};

/// A resolved source position.
#[derive(Debug, Clone, Copy)]
pub struct SourcePosition {
    /// Source file, NUL-terminated static text from the file table.
    pub file: *const c_char,
    /// The file's id in the image's file table.
    pub module_id: u32,
    /// One-based line.
    pub line: u32,
    /// One-based column in Unicode scalar values, zero if the table records none.
    pub col: u32,
}

/// A Hew function containing an address.
#[derive(Debug, Clone, Copy)]
pub struct HewFunction {
    /// Display name, NUL-terminated static text from the symbol table.
    pub name: *const c_char,
    /// The function's declaration, if the table records one.
    pub position: Option<SourcePosition>,
}

mod image {
    unsafe extern "C" {
        fn hew_debug_file_table() -> *const u8;
        fn hew_debug_site_table() -> *const u8;
        fn hew_debug_symtab() -> *const u8;
        fn hew_debug_has_debug_info() -> *const u8;
    }

    pub(super) fn file_table() -> *const u8 {
        // SAFETY: the shim only returns a table's address.
        unsafe { hew_debug_file_table() }
    }

    pub(super) fn site_table() -> *const u8 {
        // SAFETY: as above.
        unsafe { hew_debug_site_table() }
    }

    pub(super) fn symtab() -> *const u8 {
        // SAFETY: as above.
        unsafe { hew_debug_symtab() }
    }

    pub(super) fn has_debug_info() -> *const u8 {
        // SAFETY: as above.
        unsafe { hew_debug_has_debug_info() }
    }
}

/// The table at `start`, bounded by the size its own header declares.
fn table(start: *const u8, kind: TableKind) -> Option<Table<'static>> {
    if start.is_null() {
        return None;
    }
    // SAFETY: a non-null table symbol starts with a full header; the
    // zeroed MSVC stand-in is exactly one header long.
    let header = unsafe { &*start.cast::<[u8; TableHeader::SIZE]>() };
    let size = Table::declared_size(header).ok()?;
    // SAFETY: the compiler emitted `size` bytes at the symbol, as its header
    // says; the image's read-only data lives for the process.
    let bytes = unsafe { std::slice::from_raw_parts(start, size) };
    Table::parse(bytes, kind).ok()
}

/// Whether the compiler emitted debug information for this image.
#[must_use]
pub fn has_debug_info() -> bool {
    let flag = image::has_debug_info();
    // SAFETY: a non-null flag symbol is one byte of read-only data.
    !flag.is_null() && unsafe { flag.read() } != 0
}

/// The NUL-terminated name and the line and column of `byte_offset` in file
/// `file_id`.
fn locate(file_id: u32, byte_offset: Option<u32>) -> Option<SourcePosition> {
    if file_id == NO_FILE {
        return None;
    }
    let file = table(image::file_table(), TableKind::File)?
        .file(file_id as usize)
        .ok()??;
    let (line, col) = match byte_offset {
        Some(offset) => file.line_starts.locate(offset)?,
        None => (0, 0),
    };
    Some(SourcePosition {
        // The writer ends every name with a NUL the length excludes.
        file: file.name.as_ptr().cast(),
        module_id: file_id,
        line,
        col,
    })
}

/// Resolve the site id codegen attached to a fault.
#[must_use]
pub fn resolve_site(site: u32) -> Option<SourcePosition> {
    let entry = table(image::site_table(), TableKind::Site)?
        .site(site as usize)
        .ok()??;
    let mut position = locate(entry.file_id, Some(entry.byte_offset))?;
    position.col = entry.column;
    Some(position)
}

/// The Hew function whose code contains `pc`. A linear scan, since the
/// linker decides the order and a fault handler cannot sort.
#[must_use]
pub fn function_at(pc: usize) -> Option<HewFunction> {
    let base = image::symtab();
    let symtab = table(base, TableKind::Symtab)?;
    let base = base as usize;
    (0..symtab.len()).find_map(|index| {
        let symbol = symtab.symbol(index).ok()??;
        let start = base.wrapping_add_signed(symbol.start as isize);
        let end = base.wrapping_add_signed(symbol.end as isize);
        if !(start..end).contains(&pc) {
            return None;
        }
        let position = locate(symbol.file_id, None).map(|position| SourcePosition {
            line: symbol.decl_line,
            ..position
        });
        Some(HewFunction {
            name: symbol.name.as_ptr().cast(),
            position: position.filter(|position| position.line != 0),
        })
    })
}
