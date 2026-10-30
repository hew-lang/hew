//! The function and file tables every native Hew image carries
//! (`hew-debug-abi`), for frames in an image built without `-g`.
//!
//! A stripped image keeps the tables but not their symbols, so the reader
//! finds them by their header in the image's read-only sections. The layout
//! and its validation belong to [`hew_debug_abi::Table::parse`].

use hew_debug_abi::{Table, TableKind, TABLE_MAGIC};
use object::{Object, ObjectSection, SectionKind};

use crate::Location;

/// One Hew function, by static address.
struct HewFunction {
    start: u64,
    end: u64,
    name: String,
    file: Option<String>,
    decl_line: u32,
}

/// The Hew functions of one image, address-sorted.
#[derive(Default)]
pub(crate) struct HewTables {
    functions: Vec<HewFunction>,
}

impl HewTables {
    /// Read the tables of `file`; an image without them yields none.
    pub(crate) fn parse(file: &object::File<'_>) -> Self {
        let Some((symtab_address, symtab)) = find(file, TableKind::Symtab) else {
            return Self::default();
        };
        let files = find(file, TableKind::File).map(|(_, table)| table);
        let mut functions: Vec<HewFunction> = (0..symtab.len())
            .filter_map(|index| symtab.symbol(index).ok().flatten())
            .map(|symbol| HewFunction {
                start: symtab_address.wrapping_add_signed(i64::from(symbol.start)),
                end: symtab_address.wrapping_add_signed(i64::from(symbol.end)),
                name: symbol.name.to_owned(),
                file: files
                    .and_then(|files| files.file(symbol.file_id as usize).ok().flatten())
                    .map(|file| file.name.to_owned()),
                decl_line: symbol.decl_line,
            })
            .collect();
        functions.sort_unstable_by_key(|function| function.start);
        Self { functions }
    }

    /// The Hew function containing static address `address`.
    pub(crate) fn lookup(&self, address: u64) -> Option<Location> {
        let index = self
            .functions
            .partition_point(|function| function.start <= address)
            .checked_sub(1)?;
        let function = &self.functions[index];
        (address < function.end).then(|| Location {
            function: Some(function.name.clone()),
            file: function.file.clone(),
            line: (function.decl_line != 0).then_some(function.decl_line),
            column: None,
        })
    }
}

/// The first well-formed table of `kind` in the image's read-only data, with
/// its static address.
fn find<'data>(file: &object::File<'data>, kind: TableKind) -> Option<(u64, Table<'data>)> {
    let magic = TABLE_MAGIC.to_le_bytes();
    file.sections()
        .filter(|section| {
            // A linker may merge the tables into a string-merging `.rodata`
            // or place the relocated symtab in `.data.rel.ro`.
            matches!(
                section.kind(),
                SectionKind::ReadOnlyData
                    | SectionKind::ReadOnlyDataWithRel
                    | SectionKind::ReadOnlyString
                    | SectionKind::Data
            )
        })
        .find_map(|section| {
            let data = section.data().ok()?;
            (0..data.len().saturating_sub(magic.len() - 1))
                .step_by(4)
                .filter(|at| data[*at..].starts_with(&magic))
                .find_map(|at| {
                    let table = Table::parse(&data[at..], kind).ok()?;
                    Some((section.address() + at as u64, table))
                })
        })
}
