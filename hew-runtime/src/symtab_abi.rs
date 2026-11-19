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
//! Every native entry registers its tables first (`hew_image_register`), which
//! is how a program linked against the shared runtime hands them over: its
//! definitions cannot replace a shared library's own symbols. For images that
//! never register (a Rust test binary, an object built before registration
//! existed), `debug_tables.c` gives every table a weak zeroed default the
//! program's own table replaces at static link time; a zero magic reads as
//! absent. Every
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
    use std::sync::atomic::{AtomicPtr, Ordering};

    unsafe extern "C" {
        fn hew_debug_file_table() -> *const u8;
        fn hew_debug_site_table() -> *const u8;
        fn hew_debug_symtab() -> *const u8;
        fn hew_debug_has_debug_info() -> *const u8;
    }

    /// Tables the executable registered at entry, in the order file, site,
    /// symbol, debug flag. Null until registration; a lookup then falls back to
    /// the link-time symbol. Atomic loads keep a fault handler's read safe.
    pub(super) static REGISTERED: [AtomicPtr<u8>; 4] = [
        AtomicPtr::new(std::ptr::null_mut()),
        AtomicPtr::new(std::ptr::null_mut()),
        AtomicPtr::new(std::ptr::null_mut()),
        AtomicPtr::new(std::ptr::null_mut()),
    ];

    fn registered_or(slot: usize, linked: unsafe extern "C" fn() -> *const u8) -> *const u8 {
        let table = REGISTERED[slot].load(Ordering::Acquire);
        if table.is_null() {
            // SAFETY: the shim only returns a table's address.
            unsafe { linked() }
        } else {
            table.cast_const()
        }
    }

    pub(super) fn file_table() -> *const u8 {
        registered_or(0, hew_debug_file_table)
    }

    pub(super) fn site_table() -> *const u8 {
        registered_or(1, hew_debug_site_table)
    }

    pub(super) fn symtab() -> *const u8 {
        registered_or(2, hew_debug_symtab)
    }

    pub(super) fn has_debug_info() -> *const u8 {
        registered_or(3, hew_debug_has_debug_info)
    }
}

/// The Hew release this runtime was built from. Programs record the release
/// that compiled them and register it at entry.
pub const RUNTIME_VERSION: &str = env!("CARGO_PKG_VERSION");

/// Exit status of a program whose runtime is from another Hew release
/// (`EX_SOFTWARE`).
const RUNTIME_MISMATCH_STATUS: i32 = 70;

/// Register the executable's debug tables and check that the runtime it
/// loaded is the one it was compiled for. Every native entry calls this before
/// anything else.
///
/// A statically linked program always carries its own runtime, so the check
/// only fails for a program linked against the shared runtime that found a
/// different release's library (a renamed file, an `LD_LIBRARY_PATH`
/// override). The runtime ABI is not stable across releases, so the process
/// exits before any Hew code runs, naming both releases and the library it
/// loaded, rather than misbehave. Exiting here keeps the entry a straight line:
/// it has no failure branch of its own to split its block.
///
/// # Safety
/// `version` is null or a NUL-terminated string. Each table pointer is null or
/// a compiler-emitted table that stays mapped for the life of the process.
#[no_mangle]
pub unsafe extern "C" fn hew_image_register(
    version: *const c_char,
    files: *const u8,
    sites: *const u8,
    symbols: *const u8,
    has_debug_info: *const u8,
) {
    let tables = [files, sites, symbols, has_debug_info];
    // SAFETY: forwarded from this function's own contract.
    let registered = unsafe { register_image(version, tables) };
    if let Err(message) = registered {
        use std::io::Write as _;
        let _ = std::io::stderr().write_all(message.as_bytes());
        std::process::exit(RUNTIME_MISMATCH_STATUS);
    }
}

/// [`hew_image_register`] without the exit: the diagnostic for a runtime from
/// another release, or the tables registered.
///
/// # Safety
/// As for [`hew_image_register`].
unsafe fn register_image(version: *const c_char, tables: [*const u8; 4]) -> Result<(), String> {
    use std::sync::atomic::Ordering;
    if !version.is_null() {
        // SAFETY: the caller passes a NUL-terminated string.
        let expected = unsafe { std::ffi::CStr::from_ptr(version) }.to_bytes();
        if expected != RUNTIME_VERSION.as_bytes() {
            let library = runtime_image_path();
            return Err(format!(
                "hew: this program was compiled by Hew {expected} but loaded the Hew {actual} \
                 runtime{from}. Install the Hew {expected} runtime, or rebuild the program \
                 with this Hew release.\n",
                expected = String::from_utf8_lossy(expected),
                actual = RUNTIME_VERSION,
                from = library.map_or_else(String::new, |path| format!(" from {path}")),
            ));
        }
    }
    for (slot, table) in tables.into_iter().enumerate() {
        if !table.is_null() {
            image::REGISTERED[slot].store(table.cast_mut(), Ordering::Release);
        }
    }
    Ok(())
}

/// The file the runtime was loaded from, when the platform can say.
#[cfg(unix)]
fn runtime_image_path() -> Option<String> {
    let mut info = std::mem::MaybeUninit::<libc::Dl_info>::zeroed();
    let address = hew_image_register as *const std::ffi::c_void;
    // SAFETY: dladdr only fills `info` for an address inside a loaded image.
    if unsafe { libc::dladdr(address, info.as_mut_ptr()) } == 0 {
        return None;
    }
    // SAFETY: dladdr succeeded, so `info` is initialized.
    let name = unsafe { info.assume_init() }.dli_fname;
    if name.is_null() {
        return None;
    }
    // SAFETY: a non-null dli_fname is a NUL-terminated path owned by the loader.
    let name = unsafe { std::ffi::CStr::from_ptr(name) };
    Some(name.to_string_lossy().into_owned())
}

#[cfg(not(unix))]
fn runtime_image_path() -> Option<String> {
    None
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
    // SAFETY: callers supply compiler-emitted tables whose image remains live
    // for the lookup, and whose allocation contains the declared size.
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
fn locate_in(files: *const u8, file_id: u32, byte_offset: Option<u32>) -> Option<SourcePosition> {
    if file_id == NO_FILE {
        return None;
    }
    let file = table(files, TableKind::File)?
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
    // SAFETY: executable tables remain live for the process lifetime.
    unsafe { resolve_site_in(image::file_table(), image::site_table(), site) }
}

/// Resolve a site against its originating image's tables.
///
/// # Safety
/// Each non-null pointer must point to a complete compiler-emitted table and
/// its image must remain loaded while the returned source pointer is used.
#[must_use]
pub unsafe fn resolve_site_in(
    files: *const u8,
    sites: *const u8,
    site: u32,
) -> Option<SourcePosition> {
    let entry = table(sites, TableKind::Site)?.site(site as usize).ok()??;
    let mut position = locate_in(files, entry.file_id, Some(entry.byte_offset))?;
    position.col = entry.column;
    Some(position)
}

/// The Hew function whose code contains `pc`. A linear scan, since the
/// linker decides the order and a fault handler cannot sort.
#[must_use]
pub fn function_at(pc: usize) -> Option<HewFunction> {
    // SAFETY: executable tables remain loaded for the process lifetime.
    unsafe { function_at_in(image::file_table(), image::symtab(), pc) }
}

/// Resolve a frame against the tables of its originating image.
///
/// # Safety
/// Both tables must remain loaded while the returned pointers are used.
#[must_use]
pub unsafe fn function_at_in(files: *const u8, base: *const u8, pc: usize) -> Option<HewFunction> {
    let symtab = table(base, TableKind::Symtab)?;
    let base = base as usize;
    (0..symtab.len()).find_map(|index| {
        let symbol = symtab.symbol(index).ok()??;
        let start = base.wrapping_add_signed(symbol.start as isize);
        let end = base.wrapping_add_signed(symbol.end as isize);
        if !(start..end).contains(&pc) {
            return None;
        }
        let position = locate_in(files, symbol.file_id, None).map(|position| SourcePosition {
            line: symbol.decl_line,
            ..position
        });
        Some(HewFunction {
            name: symbol.name.as_ptr().cast(),
            position: position.filter(|position| position.line != 0),
        })
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_debug_abi::{FileRecord, SiteEntry, TableWriter};
    use std::ffi::CStr;

    #[test]
    fn image_registration_refuses_a_program_from_another_release() {
        let null = std::ptr::null();
        // SAFETY: a NUL-terminated version and null tables.
        let error = unsafe { register_image(c"0.0.0-other".as_ptr(), [null; 4]) }.unwrap_err();
        assert!(
            error.contains("compiled by Hew 0.0.0-other")
                && error.contains(&format!("the Hew {RUNTIME_VERSION} runtime")),
            "{error}"
        );
        for slot in &image::REGISTERED {
            assert!(
                slot.load(std::sync::atomic::Ordering::Acquire).is_null(),
                "a refused image must not register tables"
            );
        }
    }

    #[test]
    fn image_registration_accepts_this_release() {
        let null = std::ptr::null();
        let version = std::ffi::CString::new(RUNTIME_VERSION).unwrap();
        // SAFETY: a NUL-terminated version and null tables, which register
        // nothing and leave the link-time tables in place.
        let registered = unsafe { register_image(version.as_ptr(), [null; 4]) };
        assert_eq!(registered, Ok(()));
    }

    #[test]
    fn site_ids_resolve_in_the_originating_image() {
        let files_a = TableWriter::files(&[FileRecord {
            name: "first.hew",
            line_starts: &[0],
            md5: [0; 16],
        }]);
        let files_b = TableWriter::files(&[FileRecord {
            name: "second.hew",
            line_starts: &[0, 10],
            md5: [0; 16],
        }]);
        let sites = TableWriter::sites(&[SiteEntry {
            file_id: 0,
            byte_offset: 12,
            column: 3,
        }]);
        // SAFETY: both complete table allocations live through these reads.
        let a = unsafe { resolve_site_in(files_a.as_ptr(), sites.as_ptr(), 0) }.unwrap();
        // SAFETY: as above, with a different originating image.
        let b = unsafe { resolve_site_in(files_b.as_ptr(), sites.as_ptr(), 0) }.unwrap();
        // SAFETY: the source pointers borrow the still-live table allocations.
        assert_eq!(unsafe { CStr::from_ptr(a.file) }.to_bytes(), b"first.hew");
        // SAFETY: as above.
        assert_eq!(unsafe { CStr::from_ptr(b.file) }.to_bytes(), b"second.hew");
        assert_eq!((a.line, b.line, b.col), (1, 2, 3));
    }
}
