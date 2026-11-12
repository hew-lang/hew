//! Layout of the tables every native Hew image carries.
//!
//! The compiler emits four read-only objects into every native build,
//! `--release` without `-g` included:
//!
//! | symbol | contents |
//! |---|---|
//! | [`FILE_TABLE_SYMBOL`] | one [`FileEntry`] per source file a function or site names |
//! | [`SITE_TABLE_SYMBOL`] | one [`SiteEntry`] per fault site, indexed by the id `hew_fault_set_site` receives |
//! | [`SYMTAB_SYMBOL`] | one [`SymEntry`] per non-artificial Hew function the optimizer kept |
//! | [`HAS_DEBUG_INFO_SYMBOL`] | one byte: 1 when the image was built with `-g` |
//!
//! The codegen writes them with [`TableWriter`], the runtime and
//! `hew-symbolize` read them with [`Table::parse`]; nothing else knows the
//! layout.
//!
//! # Encoding
//!
//! Every table is one contiguous little-endian object at its symbol: a
//! [`TableHeader`], `count` fixed-size entries, then a blob holding the
//! strings and line-start arrays the entries point into. Every offset in a
//! table is relative to the table's own first byte, so a table carries no
//! absolute address and needs no load-time relocation under PIE or ASLR.
//!
//! [`SymEntry::start`] and [`SymEntry::end`] are the only fields that name
//! code. Each holds the signed distance from the table's first byte to an
//! instruction address, which the linker resolves as one PC-relative fixup:
//! `R_X86_64_PC32` / `R_AARCH64_PREL32` on ELF, a `SUBTRACTOR`/`UNSIGNED` pair
//! on Mach-O and `IMAGE_REL_AMD64_REL32` / `IMAGE_REL_ARM64_REL32` on COFF. No
//! format needs an absolute pointer. A difference of two code addresses (a
//! size) is not used: COFF cannot express it from a data section, and the
//! assembler folds it against the wrong base. A reader adds `start` and `end`
//! to the table's address: the runtime to the table's run-time address,
//! `hew-symbolize` to the table symbol's address in the file.
//!
//! The linker decides the final function order, so symtab entries are in
//! emission order; readers sort by `start` before searching. A function's
//! `end` is the next function the compiler emitted, artificial glue
//! included, so an address inside glue never resolves to its neighbour.
//!
//! # Absence
//!
//! An image without a table (a wasm module, a Rust test binary that links the
//! runtime) is valid. Stable Rust has no `extern_weak` and Mach-O refuses a
//! weak reference nothing defines, so a C shim in the runtime gives each table
//! a zeroed default: an `__attribute__((weak))` definition on ELF, Mach-O and
//! wasm, an `/alternatename` stand-in on MSVC. The program's table replaces it
//! at link time; a reader treats the default's zero magic as no table.

#![no_std]

extern crate alloc;

use alloc::vec::Vec;

/// The source file table.
pub const FILE_TABLE_SYMBOL: &str = "__hew_file_table";
/// The fault-site table, indexed by site id.
pub const SITE_TABLE_SYMBOL: &str = "__hew_site_table";
/// The function table.
pub const SYMTAB_SYMBOL: &str = "__hew_symtab";
/// One byte, 1 when the image carries `-g` debug information.
pub const HAS_DEBUG_INFO_SYMBOL: &str = "__hew_has_debug_info";

/// `HEWT` read as a little-endian `u32`.
pub const TABLE_MAGIC: u32 = u32::from_le_bytes(*b"HEWT");
/// Layout version every table in this crate writes and accepts.
pub const TABLE_VERSION: u16 = 2;
/// The site id of a fault raised with no source attribution.
pub const NO_SITE: u32 = u32::MAX;
/// The file id of a function or site with no source file.
pub const NO_FILE: u32 = u32::MAX;

/// Which table a header introduces.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum TableKind {
    File = 1,
    Site = 2,
    Symtab = 3,
}

impl TableKind {
    const fn from_raw(raw: u16) -> Option<Self> {
        match raw {
            1 => Some(Self::File),
            2 => Some(Self::Site),
            3 => Some(Self::Symtab),
            _ => None,
        }
    }

    /// Bytes in one entry of this table.
    #[must_use]
    pub const fn entry_size(self) -> usize {
        match self {
            Self::File => FileEntry::SIZE,
            Self::Site => SiteEntry::SIZE,
            Self::Symtab => SymEntry::SIZE,
        }
    }
}

/// The 16 bytes that open every table.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct TableHeader {
    /// [`TABLE_MAGIC`].
    pub magic: u32,
    /// [`TABLE_VERSION`].
    pub version: u16,
    /// A [`TableKind`] discriminant.
    pub kind: u16,
    /// Number of entries after the header.
    pub count: u32,
    /// Total bytes: header, entries and blob.
    pub size: u32,
}

impl TableHeader {
    pub const SIZE: usize = 16;
}

/// One source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct FileEntry {
    /// Offset of the UTF-8 display path, followed by a NUL that `name_len`
    /// excludes. It is the `--trim-paths` spelling, never a host path when
    /// trimming is on.
    pub name_offset: u32,
    pub name_len: u32,
    /// Offset of `line_count` 4-byte-aligned `u32` byte offsets, one per line
    /// start; the first is always 0.
    pub lines_offset: u32,
    pub line_count: u32,
    /// MD5 of the file's bytes, matching the `DIFile` checksum.
    pub md5: [u8; 16],
}

impl FileEntry {
    pub const SIZE: usize = 32;
}

/// One fault site: where a trap, `panic`, `.expect`, `assert` or `?`
/// propagation raised or forwarded a failure.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct SiteEntry {
    pub file_id: u32,
    pub byte_offset: u32,
    /// One-based Unicode-scalar column, or zero when unavailable.
    pub column: u32,
}

impl SiteEntry {
    pub const SIZE: usize = 12;
}

/// One Hew function.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct SymEntry {
    /// Signed distance from the table's first byte to the function's entry.
    pub start: i32,
    /// Signed distance from the table's first byte to the next emitted
    /// function, which bounds this one.
    pub end: i32,
    /// Offset of the UTF-8 display name, followed by a NUL that `name_len`
    /// excludes.
    pub name_offset: u32,
    pub name_len: u32,
    /// The declaring file, or [`NO_FILE`].
    pub file_id: u32,
    /// One-based declaration line, or 0 when unknown.
    pub decl_line: u32,
}

impl SymEntry {
    pub const SIZE: usize = 24;
}

/// Why a byte range is not a table this crate can read.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TableError {
    Truncated,
    BadMagic,
    UnsupportedVersion(u16),
    UnknownKind(u16),
    WrongKind {
        expected: TableKind,
        found: TableKind,
    },
    EntryOutOfBounds,
}

impl core::fmt::Display for TableError {
    fn fmt(&self, formatter: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Truncated => formatter.write_str("table is shorter than its header says"),
            Self::BadMagic => formatter.write_str("table does not start with HEWT"),
            Self::UnsupportedVersion(version) => {
                write!(formatter, "table version {version} is not supported")
            }
            Self::UnknownKind(kind) => write!(formatter, "table kind {kind} is unknown"),
            Self::WrongKind { expected, found } => {
                write!(formatter, "expected a {expected:?} table, found {found:?}")
            }
            Self::EntryOutOfBounds => formatter.write_str("table entry points outside the table"),
        }
    }
}

fn read_u32(bytes: &[u8], at: usize) -> Result<u32, TableError> {
    let end = at.checked_add(4).ok_or(TableError::Truncated)?;
    let word = bytes.get(at..end).ok_or(TableError::Truncated)?;
    Ok(u32::from_le_bytes([word[0], word[1], word[2], word[3]]))
}

fn read_u16(bytes: &[u8], at: usize) -> Result<u16, TableError> {
    let word = bytes.get(at..at + 2).ok_or(TableError::Truncated)?;
    Ok(u16::from_le_bytes([word[0], word[1]]))
}

fn widen(value: u32) -> usize {
    // Every supported target has at least 32-bit addresses.
    value as usize
}

/// A validated table borrowed from an image or a file.
#[derive(Debug, Clone, Copy)]
pub struct Table<'a> {
    bytes: &'a [u8],
    kind: TableKind,
    count: usize,
}

impl<'a> Table<'a> {
    /// Read the header at the start of `bytes` and bound the table to the size
    /// it declares.
    ///
    /// # Errors
    /// Returns a [`TableError`] when the header is malformed, the table is
    /// longer than `bytes`, or its entries overrun it.
    pub fn parse(bytes: &'a [u8], expected: TableKind) -> Result<Self, TableError> {
        if read_u32(bytes, 0)? != TABLE_MAGIC {
            return Err(TableError::BadMagic);
        }
        let version = read_u16(bytes, 4)?;
        if version != TABLE_VERSION {
            return Err(TableError::UnsupportedVersion(version));
        }
        let raw_kind = read_u16(bytes, 6)?;
        let kind = TableKind::from_raw(raw_kind).ok_or(TableError::UnknownKind(raw_kind))?;
        if kind != expected {
            return Err(TableError::WrongKind {
                expected,
                found: kind,
            });
        }
        let count = widen(read_u32(bytes, 8)?);
        let size = widen(read_u32(bytes, 12)?);
        let bytes = bytes.get(..size).ok_or(TableError::Truncated)?;
        let entries_end = count
            .checked_mul(kind.entry_size())
            .and_then(|len| len.checked_add(TableHeader::SIZE))
            .ok_or(TableError::Truncated)?;
        if entries_end > size {
            return Err(TableError::Truncated);
        }
        Ok(Self { bytes, kind, count })
    }

    /// Read the header only, for a reader that holds a pointer to a table in
    /// its own image and must learn the table's size before borrowing it.
    ///
    /// # Errors
    /// Returns a [`TableError`] when `header` is not a table header.
    pub fn declared_size(header: &[u8; TableHeader::SIZE]) -> Result<usize, TableError> {
        if read_u32(header, 0)? != TABLE_MAGIC {
            return Err(TableError::BadMagic);
        }
        Ok(widen(read_u32(header, 12)?))
    }

    #[must_use]
    pub const fn len(&self) -> usize {
        self.count
    }

    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.count == 0
    }

    #[must_use]
    pub const fn kind(&self) -> TableKind {
        self.kind
    }

    fn entry_at(&self, index: usize) -> Option<usize> {
        (index < self.count).then(|| TableHeader::SIZE + index * self.kind.entry_size())
    }

    fn bytes_at(&self, offset: u32, len: u32) -> Result<&'a [u8], TableError> {
        let start = widen(offset);
        let end = start
            .checked_add(widen(len))
            .ok_or(TableError::EntryOutOfBounds)?;
        self.bytes
            .get(start..end)
            .ok_or(TableError::EntryOutOfBounds)
    }

    fn text_at(&self, offset: u32, len: u32) -> Result<&'a str, TableError> {
        core::str::from_utf8(self.bytes_at(offset, len)?).map_err(|_| TableError::EntryOutOfBounds)
    }

    /// The file entry at `index` of a file table.
    ///
    /// # Errors
    /// Returns a [`TableError`] when the entry or what it points to is out of
    /// bounds.
    pub fn file(&self, index: usize) -> Result<Option<File<'a>>, TableError> {
        debug_assert_eq!(self.kind, TableKind::File);
        let Some(at) = self.entry_at(index) else {
            return Ok(None);
        };
        let name_offset = read_u32(self.bytes, at)?;
        let name_len = read_u32(self.bytes, at + 4)?;
        let lines_offset = read_u32(self.bytes, at + 8)?;
        let line_count = read_u32(self.bytes, at + 12)?;
        let mut md5 = [0u8; 16];
        md5.copy_from_slice(
            self.bytes
                .get(at + 16..at + 32)
                .ok_or(TableError::Truncated)?,
        );
        let lines_len = line_count
            .checked_mul(4)
            .ok_or(TableError::EntryOutOfBounds)?;
        Ok(Some(File {
            name: self.text_at(name_offset, name_len)?,
            line_starts: LineStarts(self.bytes_at(lines_offset, lines_len)?),
            md5,
        }))
    }

    /// The site entry for site id `index` of a site table.
    ///
    /// # Errors
    /// Returns a [`TableError`] when the entry is truncated.
    pub fn site(&self, index: usize) -> Result<Option<SiteEntry>, TableError> {
        debug_assert_eq!(self.kind, TableKind::Site);
        let Some(at) = self.entry_at(index) else {
            return Ok(None);
        };
        Ok(Some(SiteEntry {
            file_id: read_u32(self.bytes, at)?,
            byte_offset: read_u32(self.bytes, at + 4)?,
            column: read_u32(self.bytes, at + 8)?,
        }))
    }

    /// The signed start and end offsets from the symtab base, without decoding the name.
    ///
    /// # Errors
    /// Returns a [`TableError`] when the entry is truncated.
    #[inline]
    pub fn symbol_range(&self, index: usize) -> Result<Option<(i32, i32)>, TableError> {
        debug_assert_eq!(self.kind, TableKind::Symtab);
        let Some(at) = self.entry_at(index) else {
            return Ok(None);
        };
        Ok(Some((
            read_u32(self.bytes, at)?.cast_signed(),
            read_u32(self.bytes, at + 4)?.cast_signed(),
        )))
    }

    /// The function entry at `index` of a symtab, with its name.
    ///
    /// # Errors
    /// Returns a [`TableError`] when the entry or its name is out of bounds.
    pub fn symbol(&self, index: usize) -> Result<Option<Symbol<'a>>, TableError> {
        debug_assert_eq!(self.kind, TableKind::Symtab);
        let Some(at) = self.entry_at(index) else {
            return Ok(None);
        };
        let start = read_u32(self.bytes, at)?.cast_signed();
        let end = read_u32(self.bytes, at + 4)?.cast_signed();
        let name_offset = read_u32(self.bytes, at + 8)?;
        let name_len = read_u32(self.bytes, at + 12)?;
        Ok(Some(Symbol {
            start,
            end,
            name: self.text_at(name_offset, name_len)?,
            file_id: read_u32(self.bytes, at + 16)?,
            decl_line: read_u32(self.bytes, at + 20)?,
        }))
    }
}

/// A decoded file entry.
#[derive(Debug, Clone, Copy)]
pub struct File<'a> {
    pub name: &'a str,
    pub line_starts: LineStarts<'a>,
    pub md5: [u8; 16],
}

/// A decoded function entry. `start` and `end` are relative to the symtab's
/// first byte.
#[derive(Debug, Clone, Copy)]
pub struct Symbol<'a> {
    pub start: i32,
    pub end: i32,
    pub name: &'a str,
    pub file_id: u32,
    pub decl_line: u32,
}

impl Symbol<'_> {
    /// Bytes from the function's entry to the next emitted function.
    #[must_use]
    pub fn size(&self) -> u32 {
        self.end.wrapping_sub(self.start).cast_unsigned()
    }
}

/// One file's line-start offsets.
#[derive(Debug, Clone, Copy)]
pub struct LineStarts<'a>(&'a [u8]);

impl LineStarts<'_> {
    #[must_use]
    pub fn len(&self) -> usize {
        self.0.len() / 4
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    fn get(&self, index: usize) -> u32 {
        let at = index * 4;
        u32::from_le_bytes([self.0[at], self.0[at + 1], self.0[at + 2], self.0[at + 3]])
    }

    /// One-based line and column of `byte_offset`. The column counts bytes.
    #[must_use]
    pub fn locate(&self, byte_offset: u32) -> Option<(u32, u32)> {
        if self.is_empty() {
            return None;
        }
        let (mut low, mut high) = (0usize, self.len());
        while low < high {
            let mid = low + (high - low) / 2;
            if self.get(mid) <= byte_offset {
                low = mid + 1;
            } else {
                high = mid;
            }
        }
        let line = low.checked_sub(1)?;
        let column = byte_offset - self.get(line) + 1;
        Some((u32::try_from(line + 1).ok()?, column))
    }
}

/// One-based line of `byte_offset` given the sorted line starts of its file,
/// or 0 when `starts` is empty. The writer and readers share this.
#[must_use]
pub fn line_of(starts: &[u32], byte_offset: u32) -> u32 {
    let index = starts.partition_point(|start| *start <= byte_offset);
    u32::try_from(index).unwrap_or(u32::MAX)
}

/// Byte offset of every line start in `text`, the first being 0.
#[must_use]
pub fn line_starts(text: &str) -> Vec<u32> {
    let mut starts = Vec::with_capacity(text.len() / 32 + 1);
    starts.push(0);
    for (offset, byte) in text.bytes().enumerate() {
        if byte == b'\n' {
            starts.push(u32::try_from(offset + 1).unwrap_or(u32::MAX));
        }
    }
    starts
}

/// A function entry before layout. `start` is not data: the codegen emits it
/// as a relocation, so [`TableWriter::symtab`] leaves a zero in its place and
/// reports where each one goes.
#[derive(Debug, Clone, Copy)]
pub struct SymbolRecord<'a> {
    pub name: &'a str,
    pub file_id: u32,
    pub decl_line: u32,
}

/// A file entry before layout.
#[derive(Debug, Clone, Copy)]
pub struct FileRecord<'a> {
    pub name: &'a str,
    pub line_starts: &'a [u32],
    pub md5: [u8; 16],
}

/// Serialize tables into the exact bytes the readers accept.
#[derive(Debug, Default)]
pub struct TableWriter {
    bytes: Vec<u8>,
}

impl TableWriter {
    fn begin(kind: TableKind, count: usize) -> Self {
        let mut bytes = Vec::with_capacity(TableHeader::SIZE + count * kind.entry_size());
        bytes.extend_from_slice(&TABLE_MAGIC.to_le_bytes());
        bytes.extend_from_slice(&TABLE_VERSION.to_le_bytes());
        bytes.extend_from_slice(&(kind as u16).to_le_bytes());
        bytes.extend_from_slice(&u32::try_from(count).unwrap_or(u32::MAX).to_le_bytes());
        bytes.extend_from_slice(&0u32.to_le_bytes());
        bytes.resize(TableHeader::SIZE + count * kind.entry_size(), 0);
        Self { bytes }
    }

    fn put_u32(&mut self, at: usize, value: u32) {
        self.bytes[at..at + 4].copy_from_slice(&value.to_le_bytes());
    }

    /// Append a name and the NUL after it, so a C reader or a failure record
    /// can point at the name in place.
    fn append_name(&mut self, name: &str) -> u32 {
        let offset = u32::try_from(self.bytes.len()).unwrap_or(u32::MAX);
        self.bytes.extend_from_slice(name.as_bytes());
        self.bytes.push(0);
        offset
    }

    fn align4(&mut self) {
        while !self.bytes.len().is_multiple_of(4) {
            self.bytes.push(0);
        }
    }

    fn finish(mut self) -> Vec<u8> {
        self.align4();
        let size = u32::try_from(self.bytes.len()).unwrap_or(u32::MAX);
        self.put_u32(12, size);
        self.bytes
    }

    /// The file table for `files`, in file-id order.
    #[must_use]
    pub fn files(files: &[FileRecord<'_>]) -> Vec<u8> {
        let mut writer = Self::begin(TableKind::File, files.len());
        for (index, file) in files.iter().enumerate() {
            let at = TableHeader::SIZE + index * FileEntry::SIZE;
            writer.align4();
            let lines_offset = u32::try_from(writer.bytes.len()).unwrap_or(u32::MAX);
            for start in file.line_starts {
                writer.bytes.extend_from_slice(&start.to_le_bytes());
            }
            let name_offset = writer.append_name(file.name);
            writer.put_u32(at, name_offset);
            writer.put_u32(at + 4, u32::try_from(file.name.len()).unwrap_or(u32::MAX));
            writer.put_u32(at + 8, lines_offset);
            writer.put_u32(
                at + 12,
                u32::try_from(file.line_starts.len()).unwrap_or(u32::MAX),
            );
            writer.bytes[at + 16..at + 32].copy_from_slice(&file.md5);
        }
        writer.finish()
    }

    /// The site table for `sites`, indexed by site id.
    #[must_use]
    pub fn sites(sites: &[SiteEntry]) -> Vec<u8> {
        let mut writer = Self::begin(TableKind::Site, sites.len());
        for (index, site) in sites.iter().enumerate() {
            let at = TableHeader::SIZE + index * SiteEntry::SIZE;
            writer.put_u32(at, site.file_id);
            writer.put_u32(at + 4, site.byte_offset);
            writer.put_u32(at + 8, site.column);
        }
        writer.finish()
    }

    /// The symtab for `symbols` with every `start` and `end` zero. The
    /// returned offsets locate each entry's `start` and `end` words, which
    /// the codegen fills with relocated expressions.
    #[must_use]
    pub fn symtab(symbols: &[SymbolRecord<'_>]) -> (Vec<u8>, Vec<SymbolSlots>) {
        let mut writer = Self::begin(TableKind::Symtab, symbols.len());
        let mut slots = Vec::with_capacity(symbols.len());
        for (index, symbol) in symbols.iter().enumerate() {
            let at = TableHeader::SIZE + index * SymEntry::SIZE;
            let name_offset = writer.append_name(symbol.name);
            writer.put_u32(at + 8, name_offset);
            writer.put_u32(
                at + 12,
                u32::try_from(symbol.name.len()).unwrap_or(u32::MAX),
            );
            writer.put_u32(at + 16, symbol.file_id);
            writer.put_u32(at + 20, symbol.decl_line);
            slots.push(SymbolSlots {
                start: at,
                end: at + 4,
            });
        }
        (writer.finish(), slots)
    }
}

/// Byte offsets of one symtab entry's relocated words.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SymbolSlots {
    pub start: usize,
    pub end: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn struct_layouts_match_the_documented_sizes() {
        assert_eq!(core::mem::size_of::<TableHeader>(), TableHeader::SIZE);
        assert_eq!(core::mem::size_of::<FileEntry>(), FileEntry::SIZE);
        assert_eq!(core::mem::size_of::<SiteEntry>(), SiteEntry::SIZE);
        assert_eq!(core::mem::size_of::<SymEntry>(), SymEntry::SIZE);
    }

    #[test]
    fn file_table_round_trips_names_lines_and_checksums() {
        let text = "fn main() {\n    println(1);\n}\n";
        let starts = line_starts(text);
        assert_eq!(starts, [0, 12, 28, 30]);
        let files = [
            FileRecord {
                name: "src/main.hew",
                line_starts: &starts,
                md5: [7; 16],
            },
            FileRecord {
                name: "std/builtins.hew",
                line_starts: &[0, 5],
                md5: [9; 16],
            },
        ];
        let bytes = TableWriter::files(&files);
        assert_eq!(bytes.len() % 4, 0);
        let table = Table::parse(&bytes, TableKind::File).unwrap();
        assert_eq!(table.len(), 2);
        let main = table.file(0).unwrap().unwrap();
        assert_eq!(main.name, "src/main.hew");
        assert_eq!(main.md5, [7; 16]);
        assert_eq!(main.line_starts.len(), 4);
        // `println` is on line 2, column 5.
        assert_eq!(main.line_starts.locate(16), Some((2, 5)));
        assert_eq!(main.line_starts.locate(0), Some((1, 1)));
        assert_eq!(table.file(1).unwrap().unwrap().name, "std/builtins.hew");
        assert!(table.file(2).unwrap().is_none());
    }

    #[test]
    fn site_ids_index_the_site_table() {
        let sites = [
            SiteEntry {
                file_id: 0,
                byte_offset: 40,
                column: 10,
            },
            SiteEntry {
                file_id: 3,
                byte_offset: 12,
                column: 7,
            },
        ];
        let bytes = TableWriter::sites(&sites);
        let table = Table::parse(&bytes, TableKind::Site).unwrap();
        assert_eq!(table.site(1).unwrap(), Some(sites[1]));
        assert_eq!(table.site(2).unwrap(), None);
    }

    #[test]
    fn symtab_leaves_relocated_words_for_the_codegen() {
        let symbols = [
            SymbolRecord {
                name: "main",
                file_id: 0,
                decl_line: 4,
            },
            SymbolRecord {
                name: "util.triple",
                file_id: 1,
                decl_line: 2,
            },
        ];
        let (mut bytes, slots) = TableWriter::symtab(&symbols);
        bytes[slots[1].start..slots[1].start + 4].copy_from_slice(&(-64i32).to_le_bytes());
        bytes[slots[1].end..slots[1].end + 4].copy_from_slice(&(-16i32).to_le_bytes());
        let table = Table::parse(&bytes, TableKind::Symtab).unwrap();
        let triple = table.symbol(1).unwrap().unwrap();
        assert_eq!(triple.name, "util.triple");
        assert_eq!((triple.start, triple.end), (-64, -16));
        assert_eq!(table.symbol_range(1).unwrap(), Some((-64, -16)));
        assert_eq!(table.symbol_range(2).unwrap(), None);
        assert_eq!((triple.file_id, triple.decl_line), (1, 2));
        assert_eq!(table.symbol(0).unwrap().unwrap().name, "main");
    }

    #[test]
    fn parse_rejects_foreign_and_truncated_tables() {
        let bytes = TableWriter::sites(&[]);
        assert_eq!(
            Table::parse(&bytes, TableKind::File).unwrap_err(),
            TableError::WrongKind {
                expected: TableKind::File,
                found: TableKind::Site
            }
        );
        assert_eq!(
            Table::parse(&bytes[..8], TableKind::Site).unwrap_err(),
            TableError::Truncated
        );
        let mut forged = bytes.clone();
        forged[8] = 9;
        assert_eq!(
            Table::parse(&forged, TableKind::Site).unwrap_err(),
            TableError::Truncated
        );
        assert_eq!(
            Table::parse(&[0; 16], TableKind::Site).unwrap_err(),
            TableError::BadMagic
        );
        let header: [u8; 16] = bytes[..16].try_into().unwrap();
        assert_eq!(Table::declared_size(&header), Ok(bytes.len()));
    }

    #[test]
    fn line_of_agrees_with_locate() {
        let starts = line_starts("a\nbb\n\nccc");
        assert_eq!(line_of(&starts, 0), 1);
        assert_eq!(line_of(&starts, 2), 2);
        assert_eq!(line_of(&starts, 5), 3);
        assert_eq!(line_of(&starts, 7), 4);
    }
}
