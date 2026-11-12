//! Symbolization against a stripped arm64 Mach-O executable and its `.dSYM`
//! bundle, built from `fixtures/tiny.c` with `clang -g -O0`, `dsymutil` and
//! `strip`.

use std::path::PathBuf;

use hew_symbolize::{BuildId, Error, Frame, Symbolizer};

/// Where dyld happened to map the image.
const LOAD_BASE: u64 = 0x1_0420_0000;
/// `level2` starts at `__TEXT` + 0x328, on line 2 of `tiny.c`.
const LEVEL2: u64 = 0x328;

fn fixture(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures")
        .join(name)
}

/// `LC_UUID` `B24D0F67-D96B-347A-8FE6-4AB91FE99B67`.
fn tiny_build() -> BuildId {
    BuildId::from_hex("b24d0f67d96b347a8fe64ab91fe99b67").unwrap()
}

fn frame(build_id: BuildId) -> Frame {
    Frame {
        pc: LOAD_BASE + LEVEL2 + 8,
        image_base: LOAD_BASE,
        build_id: Some(build_id),
        return_address: true,
    }
}

fn assert_level2(symbols: &Symbolizer) {
    let locations = symbols.symbolize(&frame(tiny_build())).unwrap();
    assert_eq!(locations.len(), 1, "{locations:?}");
    let location = &locations[0];
    assert_eq!(location.function.as_deref(), Some("level2"));
    assert!(
        location
            .file
            .as_deref()
            .is_some_and(|file| file.ends_with("tiny.c")),
        "{location:?}"
    );
    assert_eq!(location.line, Some(2));
}

#[test]
fn stripped_executable_loads_the_bundle_beside_it() {
    let exe = Symbolizer::open(fixture("tiny.macho")).unwrap();
    assert_eq!(exe.build_id(), Some(&tiny_build()));
    assert_level2(&exe);
}

#[test]
fn bundle_opens_on_its_own() {
    let bundle = Symbolizer::open(fixture("tiny.macho.dSYM")).unwrap();
    assert_eq!(bundle.build_id(), Some(&tiny_build()));
    assert_level2(&bundle);
}

#[test]
fn bundle_from_another_build_is_refused() {
    let bundle = Symbolizer::open(fixture("tiny.macho.dSYM")).unwrap();
    let other = BuildId::from_hex("b24d0f67d96b347a8fe64ab91fe99b68").unwrap();
    assert!(matches!(
        bundle.symbolize(&frame(other)),
        Err(Error::BuildIdMismatch { .. })
    ));
}

#[test]
fn file_addresses_resolve_against_the_text_segment() {
    let exe = Symbolizer::open(fixture("tiny.macho")).unwrap();
    assert_eq!(exe.static_base(), 0x1_0000_0000);
    // `nm` prints level2 at 0x100000328; a frame based at the static base
    // resolves that address unchanged.
    let locations = exe
        .symbolize(&Frame {
            pc: exe.static_base() + LEVEL2,
            image_base: exe.static_base(),
            build_id: None,
            return_address: false,
        })
        .unwrap();
    assert_eq!(locations[0].function.as_deref(), Some("level2"));
}

#[test]
fn symbol_table_names_drop_the_mach_o_underscore() {
    use object::write::{Object, StandardSection, Symbol, SymbolSection};
    use object::{Architecture, BinaryFormat, Endianness, SymbolFlags, SymbolKind};

    let mut object = Object::new(
        BinaryFormat::MachO,
        Architecture::Aarch64,
        Endianness::Little,
    );
    let text = object.section_id(StandardSection::Text);
    object.set_section_data(text, vec![0; 0x40], 4);
    // The writer applies the Mach-O `_` prefix to this name.
    object.add_symbol(Symbol {
        name: b"__hew_fn_level3".to_vec(),
        value: 0x10,
        size: 0x20,
        kind: SymbolKind::Text,
        scope: object::SymbolScope::Linkage,
        weak: false,
        section: SymbolSection::Section(text),
        flags: SymbolFlags::None,
    });
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("level3.o");
    std::fs::write(&path, object.write().unwrap()).unwrap();
    let symbols = Symbolizer::open(&path).unwrap();
    let locations = symbols
        .symbolize(&Frame {
            pc: symbols.static_base() + 0x14,
            image_base: symbols.static_base(),
            build_id: None,
            return_address: false,
        })
        .unwrap();
    assert_eq!(locations[0].function.as_deref(), Some("__hew_fn_level3"));
}
