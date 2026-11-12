//! Windows program databases: procedures and their line tables, addressed by
//! offset from the image base.

use std::io::Cursor;
use std::path::Path;

use pdb::FallibleIterator;

use crate::{format_error, BuildId, Location, Result};

const MSF_MAGIC: &[u8] = b"Microsoft C/C++ MSF 7.00\r\n\x1a\x44\x53\x00\x00\x00";

pub(crate) fn is_pdb(data: &[u8]) -> bool {
    data.starts_with(MSF_MAGIC)
}

#[derive(Debug)]
pub(crate) struct PdbSymbols {
    build_id: BuildId,
    /// Procedures sorted by their first offset.
    procedures: Vec<Procedure>,
    files: Vec<String>,
}

#[derive(Debug)]
struct Procedure {
    start: u32,
    len: u32,
    name: String,
    /// Line rows sorted by offset.
    lines: Vec<Line>,
}

#[derive(Debug, Clone, Copy)]
struct Line {
    start: u32,
    number: u32,
    column: Option<u32>,
    file: usize,
}

impl PdbSymbols {
    pub(crate) fn parse(path: &Path, data: Vec<u8>) -> Result<Self> {
        read(data).map_err(|error| format_error(path, error))
    }

    pub(crate) fn build_id(&self) -> &BuildId {
        &self.build_id
    }

    /// Resolve an offset from the image base.
    pub(crate) fn lookup(&self, offset: u64) -> Vec<Location> {
        let Ok(rva) = u32::try_from(offset) else {
            return Vec::new();
        };
        let Some(procedure) = self
            .procedures
            .partition_point(|procedure| procedure.start <= rva)
            .checked_sub(1)
            .map(|index| &self.procedures[index])
            .filter(|procedure| rva - procedure.start < procedure.len.max(1))
        else {
            return Vec::new();
        };
        let line = procedure
            .lines
            .partition_point(|line| line.start <= rva)
            .checked_sub(1)
            .map(|index| procedure.lines[index]);
        vec![Location {
            function: Some(procedure.name.clone()),
            file: line.map(|line| self.files[line.file].clone()),
            line: line.map(|line| line.number).filter(|line| *line > 0),
            column: line
                .and_then(|line| line.column)
                .filter(|column| *column > 0),
        }]
    }
}

fn read(data: Vec<u8>) -> pdb::Result<PdbSymbols> {
    let mut pdb = pdb::PDB::open(Cursor::new(data))?;
    let information = pdb.pdb_information()?;
    let dbi = pdb.debug_information()?;
    let age = dbi.age().unwrap_or(information.age);
    let build_id = crate::dwarf::codeview_build_id(information.guid.to_bytes_le(), age);
    let address_map = pdb.address_map()?;
    let strings = pdb.string_table()?;

    let mut files: Vec<String> = Vec::new();
    let mut procedures = Vec::new();
    let mut modules = dbi.modules()?;
    while let Some(module) = modules.next()? {
        let Some(info) = pdb.module_info(&module)? else {
            continue;
        };
        let program = info.line_program()?;
        let mut symbols = info.symbols()?;
        while let Some(symbol) = symbols.next()? {
            let Ok(pdb::SymbolData::Procedure(procedure)) = symbol.parse() else {
                continue;
            };
            let Some(start) = procedure.offset.to_rva(&address_map) else {
                continue;
            };
            let mut lines = Vec::new();
            let mut rows = program.lines_for_symbol(procedure.offset);
            while let Some(row) = rows.next()? {
                let Some(row_start) = row.offset.to_rva(&address_map) else {
                    continue;
                };
                let name = program
                    .get_file_info(row.file_index)?
                    .name
                    .to_string_lossy(&strings)?
                    .into_owned();
                let file = files
                    .iter()
                    .position(|known| *known == name)
                    .unwrap_or_else(|| {
                        files.push(name);
                        files.len() - 1
                    });
                lines.push(Line {
                    start: row_start.0,
                    number: row.line_start,
                    column: row.column_start,
                    file,
                });
            }
            lines.sort_by_key(|line| line.start);
            procedures.push(Procedure {
                start: start.0,
                len: procedure.len,
                name: procedure.name.to_string().into_owned(),
                lines,
            });
        }
    }
    procedures.sort_by_key(|procedure| procedure.start);
    Ok(PdbSymbols {
        build_id,
        procedures,
        files,
    })
}
