//! Symbolization against ELF images forged in memory: DWARF, the symbol
//! table, split debug files and build-identity refusal.

use std::path::Path;

use gimli::write::{
    Address, AttributeValue, DwarfUnit, EndianVec, LineProgram, LineString, Sections,
};
use hew_symbolize::{BuildId, Error, Frame, Location, Symbolizer};
use object::write::{Object, StandardSection, Symbol, SymbolSection};
use object::{Architecture, BinaryFormat, Endianness, SectionKind, SymbolFlags, SymbolKind};

const LEVEL3: u64 = 0x1000;
const LEVEL3_LEN: u64 = 0x40;
const RAISE: u64 = 0x2000;
const LOAD_BASE: u64 = 0x5555_0000_0000;
const SOURCE: &str = "/src/fail.hew";

#[derive(Clone, Copy)]
struct Image<'a> {
    build_id: &'a [u8],
    dwarf: bool,
    symbols: bool,
    debuglink: Option<&'a str>,
    /// The CRC-32 the debuglink records for the file it names.
    debuglink_crc: u32,
}

fn forge(image: Image<'_>) -> Vec<u8> {
    let mut object = Object::new(BinaryFormat::Elf, Architecture::X86_64, Endianness::Little);
    let text = object.section_id(StandardSection::Text);
    object.set_section_data(text, vec![0xcc; 0x2100], 16);
    if image.symbols {
        for (name, value, size) in [
            ("__hew_fn_level3", LEVEL3, LEVEL3_LEN),
            ("hew_fault_raise", RAISE, 0x20),
            (
                "_ZN11hew_runtime5fault10trap_entry17h0123456789abcdefE",
                RAISE + 0x40,
                0x20,
            ),
        ] {
            object.add_symbol(Symbol {
                name: name.as_bytes().to_vec(),
                value,
                size,
                kind: SymbolKind::Text,
                scope: object::SymbolScope::Linkage,
                weak: false,
                section: SymbolSection::Section(text),
                flags: SymbolFlags::None,
            });
        }
    }

    if !image.build_id.is_empty() {
        add_build_id(&mut object, image.build_id);
    }

    if let Some(name) = image.debuglink {
        let link = object.add_section(Vec::new(), b".gnu_debuglink".to_vec(), SectionKind::Other);
        let mut data = name.as_bytes().to_vec();
        data.push(0);
        data.resize(data.len().next_multiple_of(4), 0);
        data.extend_from_slice(&image.debuglink_crc.to_le_bytes());
        object.set_section_data(link, data, 4);
    }

    if image.dwarf {
        let mut sections = Sections::new(EndianVec::new(gimli::LittleEndian));
        forge_dwarf().write(&mut sections).unwrap();
        sections
            .for_each(|id, data| {
                if !data.slice().is_empty() {
                    let section = object.add_section(
                        Vec::new(),
                        id.name().as_bytes().to_vec(),
                        SectionKind::Debug,
                    );
                    object.set_section_data(section, data.slice().to_vec(), 1);
                }
                Ok::<(), ()>(())
            })
            .unwrap();
    }
    object.write().unwrap()
}

fn add_build_id(object: &mut Object<'_>, build_id: &[u8]) {
    let note = object.add_section(
        Vec::new(),
        b".note.gnu.build-id".to_vec(),
        SectionKind::Note,
    );
    let mut desc = Vec::new();
    desc.extend_from_slice(&4u32.to_le_bytes());
    desc.extend_from_slice(&u32::try_from(build_id.len()).unwrap().to_le_bytes());
    desc.extend_from_slice(&3u32.to_le_bytes());
    desc.extend_from_slice(b"GNU\0");
    desc.extend_from_slice(build_id);
    desc.resize(desc.len().next_multiple_of(4), 0);
    object.set_section_data(note, desc, 4);
}

/// One compile unit with `level3` declared under its display name and linked
/// under its symbol name, and a line table placing the panic at 4:15.
fn forge_dwarf() -> DwarfUnit {
    let encoding = gimli::Encoding {
        format: gimli::Format::Dwarf32,
        version: 5,
        address_size: 8,
    };
    let mut dwarf = DwarfUnit::new(encoding);
    let mut program = LineProgram::new(
        encoding,
        gimli::LineEncoding::default(),
        LineString::String(b"/src".to_vec()),
        None,
        LineString::String(b"fail.hew".to_vec()),
        None,
    );
    let dir = program.default_directory();
    let file = program.add_file(LineString::String(b"fail.hew".to_vec()), dir, None);
    program.begin_sequence(Some(Address::Constant(LEVEL3)));
    for (offset, line, column) in [(0, 2, 1), (0x10, 4, 15), (0x30, 6, 5)] {
        let row = program.row();
        row.file = file;
        row.address_offset = offset;
        row.line = line;
        row.column = column;
        program.generate_row();
    }
    program.end_sequence(LEVEL3_LEN);
    dwarf.unit.line_program = program;

    let root = dwarf.unit.root();
    let unit = dwarf.unit.get_mut(root);
    unit.set(
        gimli::DW_AT_name,
        AttributeValue::String(SOURCE.as_bytes().to_vec()),
    );
    unit.set(
        gimli::DW_AT_comp_dir,
        AttributeValue::String(b"/src".to_vec()),
    );
    unit.set(
        gimli::DW_AT_low_pc,
        AttributeValue::Address(Address::Constant(LEVEL3)),
    );
    unit.set(gimli::DW_AT_high_pc, AttributeValue::Udata(LEVEL3_LEN));

    let function = dwarf.unit.add(root, gimli::DW_TAG_subprogram);
    let function = dwarf.unit.get_mut(function);
    function.set(
        gimli::DW_AT_name,
        AttributeValue::String(b"level3".to_vec()),
    );
    function.set(
        gimli::DW_AT_linkage_name,
        AttributeValue::String(b"__hew_fn_level3".to_vec()),
    );
    function.set(
        gimli::DW_AT_low_pc,
        AttributeValue::Address(Address::Constant(LEVEL3)),
    );
    function.set(gimli::DW_AT_high_pc, AttributeValue::Udata(LEVEL3_LEN));
    dwarf
}

const BUILD: &[u8] = &[0x0a, 0x78, 0xa9, 0x26, 0x22, 0x3b, 0x81, 0x27];
const OTHER_BUILD: &[u8] = &[0x59, 0xd8, 0xe0, 0x3a, 0x4e, 0x25, 0xaa, 0x3c];

fn frame(offset: u64, return_address: bool) -> Frame {
    Frame {
        pc: LOAD_BASE + offset,
        image_base: LOAD_BASE,
        build_id: Some(BuildId::new(BUILD.to_vec())),
        return_address,
    }
}

fn write(dir: &Path, name: &str, image: Image<'_>) -> std::path::PathBuf {
    let path = dir.join(name);
    std::fs::write(&path, forge(image)).unwrap();
    path
}

fn level3_at_panic() -> Location {
    Location {
        function: Some("level3".to_string()),
        file: Some(SOURCE.to_string()),
        line: Some(4),
        column: Some(15),
    }
}

const FULL: Image<'static> = Image {
    build_id: BUILD,
    dwarf: true,
    symbols: true,
    debuglink: None,
    debuglink_crc: 0,
};

#[test]
fn dwarf_names_the_declared_function_and_its_line() {
    let dir = tempfile::tempdir().unwrap();
    let symbols = Symbolizer::open(write(dir.path(), "fail", FULL)).unwrap();
    assert_eq!(symbols.build_id(), Some(&BuildId::new(BUILD.to_vec())));
    // The return address lies after the call; the call itself is at 4:15.
    assert_eq!(
        symbols.symbolize(&frame(LEVEL3 + 0x11, true)).unwrap(),
        vec![level3_at_panic()]
    );
    // A fault address names its own instruction.
    assert_eq!(
        symbols.symbolize(&frame(LEVEL3 + 0x30, false)).unwrap()[0].line,
        Some(6)
    );
}

#[test]
fn symbol_table_names_frames_without_dwarf() {
    let dir = tempfile::tempdir().unwrap();
    let symbols = Symbolizer::open(write(dir.path(), "fail", FULL)).unwrap();
    let raise = symbols.symbolize(&frame(RAISE + 4, false)).unwrap();
    assert_eq!(raise[0].function.as_deref(), Some("hew_fault_raise"));
    assert_eq!(raise[0].line, None);
    let rust = symbols.symbolize(&frame(RAISE + 0x44, false)).unwrap();
    assert_eq!(
        rust[0].function.as_deref(),
        Some("hew_runtime::fault::trap_entry")
    );
}

#[test]
fn stripped_image_loads_its_split_debug_file() {
    let dir = tempfile::tempdir().unwrap();
    write(dir.path(), "fail.debug", FULL);
    let stripped = write(
        dir.path(),
        "fail",
        Image {
            dwarf: false,
            symbols: false,
            debuglink: Some("fail.debug"),
            ..FULL
        },
    );
    let symbols = Symbolizer::open(&stripped).unwrap();
    assert_eq!(
        symbols.symbolize(&frame(LEVEL3 + 0x11, true)).unwrap(),
        vec![level3_at_panic()]
    );
}

#[test]
fn stripped_image_without_symbols_collapses_frames() {
    let dir = tempfile::tempdir().unwrap();
    let stripped = write(
        dir.path(),
        "fail",
        Image {
            dwarf: false,
            symbols: false,
            debuglink: Some("fail.debug"),
            ..FULL
        },
    );
    let symbols = Symbolizer::open(stripped).unwrap();
    assert!(symbols
        .symbolize(&frame(LEVEL3 + 0x11, true))
        .unwrap()
        .is_empty());
}

#[test]
fn split_debug_file_from_another_build_is_refused() {
    let dir = tempfile::tempdir().unwrap();
    write(
        dir.path(),
        "fail.debug",
        Image {
            build_id: OTHER_BUILD,
            ..FULL
        },
    );
    let stripped = write(
        dir.path(),
        "fail",
        Image {
            dwarf: false,
            symbols: false,
            debuglink: Some("fail.debug"),
            ..FULL
        },
    );
    let error = Symbolizer::open(stripped).unwrap_err();
    assert!(
        matches!(&error, Error::BuildIdMismatch { found: Some(found), .. } if found.as_bytes() == OTHER_BUILD),
        "{error}"
    );
}

#[test]
fn frame_from_another_build_is_refused() {
    let dir = tempfile::tempdir().unwrap();
    let debug = write(
        dir.path(),
        "other.debug",
        Image {
            build_id: OTHER_BUILD,
            ..FULL
        },
    );
    let symbols = Symbolizer::open(debug).unwrap();
    let error = symbols.symbolize(&frame(LEVEL3 + 0x11, true)).unwrap_err();
    assert!(
        matches!(&error, Error::BuildIdMismatch { expected, .. } if expected.as_bytes() == BUILD),
        "{error}"
    );
}

#[test]
fn addresses_past_every_symbol_collapse() {
    let dir = tempfile::tempdir().unwrap();
    let symbols = Symbolizer::open(write(dir.path(), "fail", FULL)).unwrap();
    // Past the last function's extent, still inside `.text`.
    assert!(symbols
        .symbolize(&frame(RAISE + 0x70, false))
        .unwrap()
        .is_empty());
    // Another image's code, far beyond this one.
    assert!(symbols
        .symbolize(&frame(0x2_0000_0000, false))
        .unwrap()
        .is_empty());
}

/// A stripped image with no build id that names `fail.debug` with `crc`.
const UNIDENTIFIED_STRIPPED: Image<'static> = Image {
    build_id: &[],
    dwarf: false,
    symbols: false,
    debuglink: Some("fail.debug"),
    debuglink_crc: 0,
};

fn unidentified_frame(offset: u64) -> Frame {
    Frame {
        build_id: None,
        ..frame(offset, true)
    }
}

#[test]
fn split_debug_file_without_build_id_is_matched_by_debuglink_crc() {
    let dir = tempfile::tempdir().unwrap();
    let debug = forge(Image {
        build_id: &[],
        ..FULL
    });
    std::fs::write(dir.path().join("fail.debug"), &debug).unwrap();
    let stripped = write(
        dir.path(),
        "fail",
        Image {
            debuglink_crc: crc32fast::hash(&debug),
            ..UNIDENTIFIED_STRIPPED
        },
    );
    let symbols = Symbolizer::open(stripped).unwrap();
    assert_eq!(symbols.build_id(), None);
    assert_eq!(
        symbols
            .symbolize(&unidentified_frame(LEVEL3 + 0x11))
            .unwrap(),
        vec![level3_at_panic()]
    );
}

#[test]
fn split_debug_file_without_build_id_from_another_build_is_refused() {
    let dir = tempfile::tempdir().unwrap();
    let ours = forge(Image {
        build_id: &[],
        ..FULL
    });
    // Another build's debug file under the name the executable records.
    let theirs = forge(Image {
        build_id: &[],
        symbols: false,
        ..FULL
    });
    std::fs::write(dir.path().join("fail.debug"), &theirs).unwrap();
    let stripped = write(
        dir.path(),
        "fail",
        Image {
            debuglink_crc: crc32fast::hash(&ours),
            ..UNIDENTIFIED_STRIPPED
        },
    );
    let error = Symbolizer::open(stripped).unwrap_err();
    assert!(
        matches!(&error, Error::ChecksumMismatch { expected, found, .. }
            if *expected == crc32fast::hash(&ours) && *found == crc32fast::hash(&theirs)),
        "{error}"
    );
}

#[test]
fn executables_naming_each_other_do_not_loop() {
    let dir = tempfile::tempdir().unwrap();
    for (name, other) in [("a", "b"), ("b", "a")] {
        write(
            dir.path(),
            name,
            Image {
                dwarf: false,
                symbols: false,
                debuglink: Some(other),
                ..FULL
            },
        );
    }
    let symbols = Symbolizer::open(dir.path().join("a")).unwrap();
    assert!(symbols
        .symbolize(&frame(LEVEL3 + 0x11, true))
        .unwrap()
        .is_empty());
}
