//! Symbolization against a program database MSVC wrote for `fixtures/tiny.c`
//! (`cl /Zi /Od /GS-`, `link /DEBUG:FULL /NODEFAULTLIB /ENTRY:main`).
//! `tiny.pe` is the linked `tiny.exe`, renamed so the repository keeps it.

use std::path::PathBuf;

use hew_symbolize::{BuildId, Error, Frame, Location, Symbolizer};

const LOAD_BASE: u64 = 0x7ff6_1234_0000;
/// `level2` starts at RVA 0x1010, on line 2 of `tiny.c`.
const LEVEL2: u64 = 0x1010;

fn fixture(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures")
        .join(name)
}

/// `{741361F8-95F8-4214-BB24-6A25848B75CE}` as the PE debug directory stores
/// it, then age 1.
fn tiny_build() -> BuildId {
    BuildId::from_hex("f8611374f8951442bb246a25848b75ce01000000").unwrap()
}

fn frame(build_id: BuildId) -> Frame {
    Frame {
        pc: LOAD_BASE + LEVEL2 + 6,
        image_base: LOAD_BASE,
        build_id: Some(build_id),
        return_address: true,
    }
}

fn level2() -> Vec<Location> {
    vec![Location {
        function: Some("level2".to_string()),
        file: Some(r"C:\Users\slepp\symbolize-lane\tiny.c".to_string()),
        line: Some(2),
        column: None,
    }]
}

#[test]
fn executable_and_program_database_share_one_identity() {
    let exe = Symbolizer::open(fixture("tiny.pe")).unwrap();
    let pdb = Symbolizer::open(fixture("tiny.pdb")).unwrap();
    assert_eq!(exe.build_id(), Some(&tiny_build()));
    assert_eq!(pdb.build_id(), Some(&tiny_build()));
}

#[test]
fn program_database_names_the_procedure_and_line() {
    let pdb = Symbolizer::open(fixture("tiny.pdb")).unwrap();
    assert_eq!(pdb.symbolize(&frame(tiny_build())).unwrap(), level2());
}

#[test]
fn executable_loads_the_program_database_beside_it() {
    let exe = Symbolizer::open(fixture("tiny.pe")).unwrap();
    assert_eq!(exe.symbolize(&frame(tiny_build())).unwrap(), level2());
}

#[test]
fn program_database_from_another_build_is_refused() {
    let pdb = Symbolizer::open(fixture("tiny.pdb")).unwrap();
    let other = BuildId::from_hex("f8611374f8951442bb246a25848b75ce02000000").unwrap();
    assert!(matches!(
        pdb.symbolize(&frame(other)),
        Err(Error::BuildIdMismatch { .. })
    ));
}
