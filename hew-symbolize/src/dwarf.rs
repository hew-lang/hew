//! ELF, Mach-O and PE images: DWARF line and function information, then the
//! symbol table for frames without DWARF.

use std::borrow::Cow;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use gimli::Reader as _;
use object::{Object, ObjectSection, ObjectSegment, ObjectSymbol};

use crate::{format_error, BuildId, Location, Result};

type Reader = gimli::EndianArcSlice<gimli::RunTimeEndian>;

/// A symbol file named by an executable that carries no DWARF itself.
pub(crate) enum Sidecar {
    /// A split ELF `.debug` file or the DWARF file of a `.dSYM` bundle, with
    /// the CRC-32 of the whole file that `.gnu_debuglink` records.
    Object { path: PathBuf, crc: Option<u32> },
    /// A Windows program database.
    Pdb(PathBuf),
}

pub(crate) struct ObjectSymbols {
    build_id: Option<BuildId>,
    /// The static address `image_base` corresponds to.
    static_base: u64,
    sidecar: Option<Sidecar>,
    /// DWARF from this file or from its split debug file.
    dwarf: Option<Dwarf>,
    /// Function symbols, address-sorted and non-overlapping.
    symbols: Vec<FunctionSymbol>,
}

struct FunctionSymbol {
    start: u64,
    /// One past the last byte: the symbol's size when the format records
    /// one, otherwise the next symbol or the end of its section.
    end: u64,
    name: String,
}

struct Dwarf {
    context: addr2line::Context<Reader>,
}

impl std::fmt::Debug for ObjectSymbols {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ObjectSymbols")
            .field("build_id", &self.build_id)
            .field("static_base", &self.static_base)
            .field("has_dwarf", &self.dwarf.is_some())
            .field("symbols", &self.symbols.len())
            .finish_non_exhaustive()
    }
}

impl ObjectSymbols {
    pub(crate) fn parse(path: &Path, data: &[u8]) -> Result<Self> {
        let file = object::File::parse(data).map_err(|error| format_error(path, error))?;
        let build_id = image_build_id(&file).map_err(|error| format_error(path, error))?;
        let static_base = match file.format() {
            object::BinaryFormat::Elf => 0,
            object::BinaryFormat::MachO => file
                .segments()
                .find(|segment| segment.name() == Ok(Some("__TEXT")))
                .map_or(0, |segment| segment.address()),
            _ => file.relative_address_base(),
        };
        let sidecar = sidecar(path, &file);
        let dwarf = load_dwarf(&file).map_err(|error| format_error(path, error))?;
        let symbols = function_symbols(&file);
        Ok(Self {
            build_id,
            static_base,
            sidecar,
            dwarf,
            symbols,
        })
    }

    pub(crate) fn build_id(&self) -> Option<&BuildId> {
        self.build_id.as_ref()
    }

    pub(crate) fn static_base(&self) -> u64 {
        self.static_base
    }

    pub(crate) fn has_dwarf(&self) -> bool {
        self.dwarf.is_some()
    }

    pub(crate) fn sidecar(&self) -> Option<Sidecar> {
        match &self.sidecar {
            Some(Sidecar::Object { path, crc }) => Some(Sidecar::Object {
                path: path.clone(),
                crc: *crc,
            }),
            Some(Sidecar::Pdb(path)) => Some(Sidecar::Pdb(path.clone())),
            None => None,
        }
    }

    /// Take DWARF (and any richer symbol table) from the image's split debug
    /// file, keeping this image's own layout.
    pub(crate) fn with_debug(mut self, debug: Self) -> Self {
        self.dwarf = debug.dwarf;
        if debug.symbols.len() > self.symbols.len() {
            self.symbols = debug.symbols;
        }
        self
    }

    /// Resolve an offset from the image's runtime base.
    pub(crate) fn lookup(&self, offset: u64, path: &Path) -> Result<Vec<Location>> {
        let address = self.static_base.wrapping_add(offset);
        if let Some(dwarf) = &self.dwarf {
            let locations = dwarf
                .lookup(address)
                .map_err(|error| format_error(path, error))?;
            if !locations.is_empty() {
                return Ok(locations);
            }
        }
        Ok(self.symbol_at(address).into_iter().collect())
    }

    fn symbol_at(&self, address: u64) -> Option<Location> {
        let index = self
            .symbols
            .partition_point(|symbol| symbol.start <= address)
            .checked_sub(1)?;
        let symbol = &self.symbols[index];
        (address < symbol.end).then(|| Location {
            function: Some(display_name(&symbol.name, None).into_owned()),
            ..Location::default()
        })
    }
}

impl Dwarf {
    fn lookup(&self, address: u64) -> std::result::Result<Vec<Location>, gimli::Error> {
        let unit = self.context.find_dwarf_and_unit(address).skip_all_loads();
        let mut frames = self.context.find_frames(address).skip_all_loads()?;
        let mut locations = Vec::new();
        while let Some(frame) = frames.next()? {
            let linkage = frame
                .function
                .as_ref()
                .map(addr2line::FunctionName::raw_name)
                .transpose()?;
            let declared = match (unit, frame.dw_die_offset) {
                (Some(unit), Some(offset)) => declared_name(unit, offset)?,
                _ => None,
            };
            let function = linkage
                .as_deref()
                .map(|linkage| display_name(linkage, declared.as_deref()).into_owned())
                .or(declared);
            let location = frame.location.as_ref();
            locations.push(Location {
                function,
                file: location.and_then(|l| l.file).map(str::to_owned),
                line: location.and_then(|l| l.line),
                column: location.and_then(|l| l.column).filter(|column| *column > 0),
            });
        }
        Ok(locations)
    }
}

/// The function name a frame shows: a Rust symbol's demangled path, otherwise
/// the name the debug information declares (Hew emits its display name
/// there), otherwise the linkage name.
fn display_name<'a>(linkage: &'a str, declared: Option<&'a str>) -> Cow<'a, str> {
    if let Ok(demangled) = rustc_demangle::try_demangle(linkage) {
        return Cow::Owned(format!("{demangled:#}"));
    }
    Cow::Borrowed(declared.unwrap_or(linkage))
}

/// `DW_AT_name` of a subprogram or inlined call, following the abstract
/// origin and specification links that carry it.
fn declared_name(
    unit: gimli::UnitRef<'_, Reader>,
    mut offset: gimli::UnitOffset<usize>,
) -> std::result::Result<Option<String>, gimli::Error> {
    // A chain longer than this would be malformed DWARF.
    for _ in 0..8 {
        let entry = unit.entry(offset)?;
        if let Some(name) = entry.attr_value(gimli::DW_AT_name) {
            let name = unit.attr_string(name)?;
            return Ok(Some(name.to_string_lossy()?.into_owned()));
        }
        let link = entry
            .attr_value(gimli::DW_AT_abstract_origin)
            .or_else(|| entry.attr_value(gimli::DW_AT_specification));
        match link {
            Some(gimli::AttributeValue::UnitRef(next)) => offset = next,
            _ => return Ok(None),
        }
    }
    Ok(None)
}

/// The image's function symbols with the extent each covers, so an address
/// past the last function (in another image, or in padding) names nothing.
fn function_symbols(file: &object::File<'_>) -> Vec<FunctionSymbol> {
    let mut raw: Vec<(u64, u64, u64, String)> = file
        .symbols()
        .chain(file.dynamic_symbols())
        .filter(|symbol| symbol.kind() == object::SymbolKind::Text && symbol.address() != 0)
        .filter_map(|symbol| {
            let section_end = symbol
                .section_index()
                .and_then(|index| file.section_by_index(index).ok())
                .map_or(symbol.address(), |section| {
                    section.address() + section.size()
                });
            let name = symbol.name().ok()?;
            // Mach-O prefixes every C-level symbol with `_`.
            let name = match file.format() {
                object::BinaryFormat::MachO => name.strip_prefix('_').unwrap_or(name),
                _ => name,
            }
            .to_string();
            Some((symbol.address(), symbol.size(), section_end, name))
        })
        .collect();
    raw.sort_unstable_by_key(|(start, ..)| *start);
    raw.dedup_by_key(|(start, ..)| *start);
    let next_starts: Vec<u64> = raw
        .iter()
        .skip(1)
        .map(|(start, ..)| *start)
        .chain(std::iter::once(u64::MAX))
        .collect();
    raw.into_iter()
        .zip(next_starts)
        .map(|((start, size, section_end, name), next)| FunctionSymbol {
            start,
            end: if size > 0 {
                start + size
            } else {
                next.min(section_end)
            },
            name,
        })
        .collect()
}

fn image_build_id(file: &object::File<'_>) -> object::Result<Option<BuildId>> {
    if let Some(id) = file.build_id()? {
        return Ok(Some(BuildId::new(id.to_vec())));
    }
    if let Some(uuid) = file.mach_uuid()? {
        return Ok(Some(BuildId::new(uuid.to_vec())));
    }
    if let Some(codeview) = file.pdb_info()? {
        return Ok(Some(codeview_build_id(codeview.guid(), codeview.age())));
    }
    Ok(None)
}

/// The Windows identity: the GUID as stored on disk, then the age.
pub(crate) fn codeview_build_id(guid: [u8; 16], age: u32) -> BuildId {
    let mut bytes = guid.to_vec();
    bytes.extend_from_slice(&age.to_le_bytes());
    BuildId::new(bytes)
}

/// The symbol file an image names, when it exists on disk.
fn sidecar(path: &Path, file: &object::File<'_>) -> Option<Sidecar> {
    let dir = path.parent().unwrap_or_else(|| Path::new("."));
    if let Ok(Some((name, crc))) = file.gnu_debuglink() {
        let name = std::str::from_utf8(name).ok()?;
        return [dir.join(name), dir.join(".debug").join(name)]
            .into_iter()
            .find(|candidate| candidate.is_file() && candidate != path)
            .map(|path| Sidecar::Object {
                path,
                crc: Some(crc),
            });
    }
    if file.format() == object::BinaryFormat::MachO {
        let name = path.file_name()?;
        let mut bundle = path.as_os_str().to_os_string();
        bundle.push(".dSYM");
        let dwarf = PathBuf::from(bundle)
            .join("Contents/Resources/DWARF")
            .join(name);
        return dwarf.is_file().then_some(Sidecar::Object {
            path: dwarf,
            crc: None,
        });
    }
    if let Ok(Some(codeview)) = file.pdb_info() {
        let recorded = Path::new(std::str::from_utf8(codeview.path()).ok()?);
        // The linker records the path it wrote; the PDB usually travels
        // beside the executable instead.
        let beside = recorded
            .to_str()
            .and_then(|text| text.rsplit(['\\', '/']).next())
            .map(|name| dir.join(name));
        return beside
            .into_iter()
            .chain(std::iter::once(recorded.to_path_buf()))
            .find(|candidate| candidate.is_file())
            .map(Sidecar::Pdb);
    }
    None
}

fn load_dwarf(file: &object::File<'_>) -> std::result::Result<Option<Dwarf>, gimli::Error> {
    let Some(info) = file.section_by_name(".debug_info") else {
        return Ok(None);
    };
    if info.size() == 0 {
        return Ok(None);
    }
    let endian = if file.is_little_endian() {
        gimli::RunTimeEndian::Little
    } else {
        gimli::RunTimeEndian::Big
    };
    let dwarf = gimli::Dwarf::load(|id| -> std::result::Result<Reader, gimli::Error> {
        let data = file
            .section_by_name(id.name())
            .and_then(|section| section.uncompressed_data().ok())
            .unwrap_or(Cow::Borrowed(&[]));
        Ok(gimli::EndianArcSlice::new(Arc::from(&*data), endian))
    })?;
    let context = addr2line::Context::from_dwarf(dwarf)?;
    Ok(Some(Dwarf { context }))
}
