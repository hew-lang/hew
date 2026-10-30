//! The debug-information contract a `-g` object keeps, read back with
//! `gimli` independent of LLVM and of any debugger: subprogram names and
//! signatures, artificial shims, the line table a debugger steps through, the
//! source file a debugger opens, and code generation that `-g` leaves alone.
//! CodeView targets carry no DWARF; `physical_debug_tests` covers them.
#![cfg(not(target_env = "msvc"))]

use std::borrow::Cow;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use gimli::{constants, AttributeValue, EndianSlice, RunTimeEndian};
use hew_codegen_rs::physical::physical_target_for_inventory;
use hew_codegen_rs::{
    emit_physical_object, native_emission_triple, DebugSource, OptLevel, PhysicalEmitOptions,
};
use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_mir::physical::debug_names::{C_ENTRY_DEBUG_NAME, SHIM_PREFIX};
use hew_types::{module_registry::ModuleRegistry, Checker};
use object::{Object, ObjectSection, ObjectSymbol, RelocationTarget};

/// A recursive function splits into coroutine parts; `length` does not.
const PROGRAM: &str = "\
type Pair {
    a: i64;
    b: i64;
}

fn length(p: Pair, scale: i64) -> i64 {
    let sum = p.a + p.b;
    sum * scale
}

fn fact(n: i64) -> i64 {
    if n <= 1 {
        return 1;
    }
    n * fact(n - 1)
}

fn main() {
    println(length(Pair { a: 3, b: 4 }, 2));
    println(fact(10));
}
";

fn verified(source: &str) -> hew_mir::VerifiedPhysicalModule {
    let mut parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    hew_compile::attach_prelude_std_modules(&mut parsed.program);
    let mut checker = Checker::new(ModuleRegistry::new(Vec::new()));
    let facts = checker.check_program(&parsed.program);
    assert!(facts.errors.is_empty(), "type errors: {:#?}", facts.errors);
    let hir = lower_program_host_target(&parsed.program, &facts, &ResolutionCtx);
    assert!(
        hir.diagnostics.is_empty(),
        "HIR errors: {:#?}",
        hir.diagnostics
    );
    let semantic = hew_sir::lower_module(&hir.module, &facts).module;
    let triple = native_emission_triple();
    let inventory = hew_mir::physical::physical_type_inventory(&semantic);
    let target = physical_target_for_inventory(&triple, &inventory).expect("physical target");
    hew_mir::lower_physical_module(&semantic, target).expect("verified physical module")
}

/// Emit `PROGRAM` from `dir` and return the native object.
fn emit(dir: &Path, opt_level: OptLevel, debug: bool) -> PathBuf {
    emit_source(dir, PROGRAM, opt_level, debug)
}

fn emit_source(dir: &Path, program: &str, opt_level: OptLevel, debug: bool) -> PathBuf {
    let source = dir.join("contract.hew");
    std::fs::write(&source, program).expect("write fixture source");
    emit_physical_object(
        &verified(program),
        &PhysicalEmitOptions {
            module_name: "contract",
            out_dir: dir,
            target_triple: None,
            opt_level,
            emit_llvm: false,
            address_sanitizer: false,
            debug_source: debug.then_some(DebugSource {
                path: &source,
                text: program,
            }),
            link_freestanding_wasm: false,
            build_info: None,
            debug_script_dir: None,
        },
    )
    .expect("emit physical object")
    .native_obj_path
    .expect("native object")
}

/// Each section's addresses start at a distinct base, so code from different
/// sections never shares an address once relocations are applied.
fn section_base(index: object::SectionIndex) -> u64 {
    (index.0 as u64) << 32
}

fn relocated<'data>(file: &object::File<'data>, name: &str) -> Cow<'data, [u8]> {
    let Some(section) = file.section_by_name(name) else {
        return Cow::Borrowed(&[]);
    };
    let data = section.uncompressed_data().expect("section data");
    let mut relocations = section.relocations().peekable();
    if relocations.peek().is_none() {
        return data;
    }
    let mut bytes = data.into_owned();
    for (offset, relocation) in relocations {
        let base = match relocation.target() {
            RelocationTarget::Symbol(index) => {
                let symbol = file.symbol_by_index(index).expect("relocation symbol");
                symbol.section_index().map_or(0, section_base) + symbol.address()
            }
            RelocationTarget::Section(index) => section_base(index),
            _ => 0,
        };
        let offset = usize::try_from(offset).expect("relocation offset");
        let width = usize::from(relocation.size() / 8);
        let slot = &mut bytes[offset..offset + width];
        let mut existing = [0u8; 8];
        existing[..width].copy_from_slice(slot);
        let implicit = if relocation.has_implicit_addend() {
            u64::from_le_bytes(existing)
        } else {
            0
        };
        let value = base
            .wrapping_add_signed(relocation.addend())
            .wrapping_add(implicit);
        slot.copy_from_slice(&value.to_le_bytes()[..width]);
    }
    Cow::Owned(bytes)
}

/// One subprogram as a debugger reads it.
#[derive(Debug, Default)]
struct Subprogram {
    name: String,
    linkage_name: Option<String>,
    artificial: bool,
    prototyped: bool,
    decl_line: u64,
    low: u64,
    high: u64,
    parameters: Vec<String>,
}

#[derive(Debug, Clone, Copy)]
struct Row {
    address: u64,
    line: u64,
    is_stmt: bool,
    prologue_end: bool,
}

struct Unit {
    version: u16,
    comp_dir: String,
    file_has_md5: bool,
    subprograms: Vec<Subprogram>,
    rows: Vec<Row>,
}

fn read_unit(object: &Path) -> Unit {
    let bytes = std::fs::read(object).expect("read object");
    let file = object::File::parse(&*bytes).expect("parse object");
    let endian = if file.is_little_endian() {
        RunTimeEndian::Little
    } else {
        RunTimeEndian::Big
    };
    let sections = gimli::DwarfSections::load(|id| -> Result<_, gimli::Error> {
        let elf = relocated(&file, id.name());
        if !elf.is_empty() {
            return Ok(elf);
        }
        Ok(relocated(&file, &id.name().replacen('.', "__", 1)))
    })
    .expect("load DWARF");
    let dwarf = sections.borrow(|section| EndianSlice::new(section, endian));
    let header = dwarf
        .units()
        .next()
        .expect("read unit")
        .expect("a compile unit");
    let version = header.version();
    let unit = dwarf.unit(header).expect("parse unit");
    let comp_dir = unit
        .comp_dir
        .map(|dir| dir.to_string_lossy().into_owned())
        .unwrap_or_default();

    let mut subprograms = Vec::new();
    let mut current: Option<isize> = None;
    let mut entries = unit.entries();
    while let Some(entry) = entries.next_dfs().expect("walk DIEs") {
        let depth = entry.depth();
        if current.is_some_and(|level| depth <= level) {
            current = None;
        }
        let attr_string = |value| {
            dwarf
                .attr_string(&unit, value)
                .expect("string attribute")
                .to_string_lossy()
                .into_owned()
        };
        if entry.tag() == constants::DW_TAG_subprogram {
            let mut subprogram = Subprogram::default();
            for attribute in entry.attrs() {
                match attribute.name() {
                    constants::DW_AT_name => subprogram.name = attr_string(attribute.value()),
                    constants::DW_AT_linkage_name => {
                        subprogram.linkage_name = Some(attr_string(attribute.value()));
                    }
                    constants::DW_AT_artificial => subprogram.artificial = true,
                    constants::DW_AT_prototyped => subprogram.prototyped = true,
                    constants::DW_AT_decl_line => {
                        subprogram.decl_line = attribute.udata_value().unwrap_or(0);
                    }
                    constants::DW_AT_low_pc => {
                        if let AttributeValue::Addr(address) = attribute.value() {
                            subprogram.low = address;
                        } else if let Some(address) = dwarf
                            .attr_address(&unit, attribute.value())
                            .expect("low_pc")
                        {
                            subprogram.low = address;
                        }
                    }
                    constants::DW_AT_high_pc => {
                        subprogram.high = match attribute.value() {
                            AttributeValue::Addr(address) => address,
                            value => value.udata_value().map_or(0, |size| subprogram.low + size),
                        };
                    }
                    _ => {}
                }
            }
            subprograms.push(subprogram);
            current = Some(depth);
        } else if entry.tag() == constants::DW_TAG_formal_parameter && current == Some(depth - 1) {
            if let Some(name) = entry.attr_value(constants::DW_AT_name) {
                let name = attr_string(name);
                subprograms
                    .last_mut()
                    .expect("a parameter belongs to a subprogram")
                    .parameters
                    .push(name);
            }
        }
    }

    let program = unit.line_program.clone().expect("a line program");
    let file_has_md5 = program
        .header()
        .file_names()
        .iter()
        .any(|entry| entry.md5() != &[0; 16]);
    let mut rows = Vec::new();
    let mut lines = program.rows();
    while let Some((_, row)) = lines.next_row().expect("line row") {
        if row.end_sequence() {
            continue;
        }
        rows.push(Row {
            address: row.address(),
            line: row.line().map_or(0, std::num::NonZeroU64::get),
            is_stmt: row.is_stmt(),
            prologue_end: row.prologue_end(),
        });
    }
    Unit {
        version,
        comp_dir,
        file_has_md5,
        subprograms,
        rows,
    }
}

impl Unit {
    fn named(&self, name: &str) -> Vec<&Subprogram> {
        self.subprograms
            .iter()
            .filter(|subprogram| subprogram.name == name)
            .collect()
    }

    fn rows_in(&self, subprogram: &Subprogram) -> Vec<Row> {
        self.rows
            .iter()
            .filter(|row| subprogram.low <= row.address && row.address < subprogram.high)
            .copied()
            .collect()
    }
}

/// A subprogram is named for a person, carries no mangled linkage name a
/// debugger would index it by instead, and lists its source parameters in
/// every part that runs source. The unit is C11, so each subprogram is
/// prototyped and a debugger reads its parameter list as complete.
#[test]
fn subprograms_carry_display_names_and_prototyped_parameters() {
    let dir = tempfile::tempdir().expect("temp dir");
    let unit = read_unit(&emit(dir.path(), OptLevel::O0, true));

    for (name, parameters) in [("length", vec!["p", "scale"]), ("fact", vec!["n"])] {
        let parts = unit.named(name);
        assert!(
            !parts.is_empty(),
            "`{name}` is described by its display name"
        );
        for part in parts {
            assert!(!part.artificial, "`{name}` runs source");
            assert_eq!(part.parameters, parameters, "`{name}` keeps its parameters");
        }
    }
    // A name belongs to one function: a breakpoint on `fact` has one location
    // per call. The resume part of a split body runs the same source under
    // its own name.
    assert_eq!(unit.named("fact").len(), 1, "`fact` names its ramp alone");
    let resume = unit.named("fact.resume");
    assert_eq!(resume.len(), 1, "`fact` resumes in one part");
    assert_eq!(resume[0].parameters, ["n"], "the resume part keeps `n`");
    assert!(!unit.named("main").is_empty(), "the Hew entry is `main`");
    for part in unit.subprograms.iter().filter(|part| !part.artificial) {
        assert!(part.prototyped, "`{}` is prototyped", part.name);
    }
    for subprogram in &unit.subprograms {
        // LLVM's coroutine split names the clone it makes of a body after the
        // clone's symbol; we write no linkage name ourselves.
        assert!(
            subprogram
                .linkage_name
                .as_deref()
                .is_none_or(|linkage| linkage.ends_with(".resume")),
            "{} carries linkage name {:?}",
            subprogram.name,
            subprogram.linkage_name
        );
        assert!(
            subprogram.artificial || !subprogram.name.starts_with("__hew_fn_"),
            "{} is not named by its symbol",
            subprogram.name
        );
    }
}

/// Compiler-generated code is artificial and on line 0: the C entry shim
/// and the destroy part of a split coroutine never resolve a source name or
/// line, so a breakpoint or a step never lands in them.
#[test]
fn shims_and_coroutine_destroy_parts_are_artificial() {
    let dir = tempfile::tempdir().expect("temp dir");
    let unit = read_unit(&emit(dir.path(), OptLevel::O0, true));

    let entry = unit.named(C_ENTRY_DEBUG_NAME);
    assert_eq!(entry.len(), 1, "the C entry is described once");
    for subprogram in &unit.subprograms {
        if subprogram.name.starts_with(SHIM_PREFIX) {
            assert!(subprogram.artificial, "{} is artificial", subprogram.name);
        }
        if subprogram.artificial {
            assert_eq!(
                subprogram.decl_line, 0,
                "{} has no declaration line",
                subprogram.name
            );
            assert!(
                unit.rows_in(subprogram).iter().all(|row| row.line == 0),
                "{} attributes no source line",
                subprogram.name
            );
        }
    }
    // `fact` recurses, so it splits: its destroy part is artificial and is not
    // spelled `fact`.
    let destroy_parts = unit
        .subprograms
        .iter()
        .filter(|subprogram| subprogram.artificial && subprogram.name != C_ENTRY_DEBUG_NAME)
        .count();
    assert!(
        destroy_parts > 0,
        "a split coroutine's destroy part is artificial"
    );
}

/// Each source line starts one statement in each function that runs source,
/// so `next` stops on it once and a line breakpoint resolves to one address.
/// A function that is not split also places `prologue_end` on its first
/// statement, after the parameters are stored, not on its declaration.
#[test]
fn each_line_starts_one_statement_and_prologue_ends_on_the_body() {
    let dir = tempfile::tempdir().expect("temp dir");
    let unit = read_unit(&emit(dir.path(), OptLevel::O0, true));

    for subprogram in unit
        .subprograms
        .iter()
        .filter(|subprogram| !subprogram.artificial)
    {
        // LLVM's coroutine split re-lays a resume part's blocks, and it marks
        // the first row of a re-entered block as a statement on its own. The
        // ramp it leaves behind interleaves line-0 plumbing (frame address
        // homes, reloads) with the first statement and so restarts that line
        // too. Both halves of a split body are exempt; every body that is not
        // split must start each line once.
        let split = subprogram.linkage_name.is_some()
            || unit
                .subprograms
                .iter()
                .any(|part| part.name == format!("{}.resume", subprogram.name));
        // The row at the entry address carries the scope line, which is the
        // first statement; the prologue ends on that statement's own row.
        let mut starts: BTreeMap<u64, Vec<u64>> = BTreeMap::new();
        let rows = unit.rows_in(subprogram);
        // A coroutine ramp allocates its frame under the first statement's
        // line, which is where its prologue ends so that a function
        // breakpoint stops with the parameters homed, and then enters that
        // statement a second time as the body. That one frame-allocation
        // row is the only second start a line may have.
        let frame_allocation = rows
            .iter()
            .find(|row| row.prologue_end)
            .filter(|first| {
                rows.iter()
                    .any(|row| row.is_stmt && row.line == first.line && row.address > first.address)
            })
            .map(|row| row.address);
        for row in rows {
            if row.is_stmt
                && row.line != 0
                && row.address != subprogram.low
                && Some(row.address) != frame_allocation
            {
                starts.entry(row.line).or_default().push(row.address);
            }
        }
        for (line, addresses) in starts.into_iter().filter(|_| !split) {
            assert_eq!(
                addresses.len(),
                1,
                "{} line {line} starts one statement, not {addresses:x?}",
                subprogram.name
            );
        }
    }

    // A split body's ramp ends its prologue on the first statement too, so a
    // function breakpoint stops where the parameters are homed and readable.
    let ramp = unit
        .named("fact")
        .into_iter()
        .find(|part| part.linkage_name.is_none())
        .expect("`fact` has a ramp");
    assert!(
        unit.rows_in(ramp)
            .iter()
            .any(|row| row.prologue_end && row.line == ramp.decl_line + 1),
        "the ramp of `fact` ends its prologue on its first statement"
    );

    // Every subprogram states its declaration line as its scope line, so the
    // first row of a function is its declaration and gdb, which skips to the
    // next line row after the frame setup, stops with the parameters stored.
    // LLVM then puts prologue_end on the first argument spill; the rule here
    // is only that a function's prologue ends on a real line of its own.
    for part in unit.subprograms.iter().filter(|part| !part.artificial) {
        let first = unit.rows_in(part).first().expect("a first row").line;
        assert_eq!(
            first, part.decl_line,
            "`{}` starts on its declaration",
            part.name
        );
    }

    let length = unit.named("length");
    assert_eq!(length.len(), 1, "`length` is not split");
    let rows = unit.rows_in(length[0]);
    let prologue_end: Vec<_> = rows.iter().filter(|row| row.prologue_end).collect();
    assert_eq!(prologue_end.len(), 1, "`length` has one prologue end");
    assert_eq!(
        prologue_end[0].line,
        length[0].decl_line + 1,
        "`length` stops first on its first statement"
    );
}

/// The unit is DWARF 5 and names its source by an absolute directory and an
/// MD5 checksum, so a debugger started from any directory finds the text and
/// can tell whether it is the text compiled.
#[test]
fn the_unit_is_dwarf5_with_an_absolute_checksummed_source() {
    let dir = tempfile::tempdir().expect("temp dir");
    let unit = read_unit(&emit(dir.path(), OptLevel::O0, true));
    assert_eq!(unit.version, 5, "the unit is DWARF 5");
    assert!(
        Path::new(&unit.comp_dir).is_absolute(),
        "comp_dir {} is absolute",
        unit.comp_dir
    );
    assert!(unit.file_has_md5, "the source file carries its MD5");
}

fn text_of(object: &Path) -> Vec<u8> {
    let bytes = std::fs::read(object).expect("read object");
    let file = object::File::parse(&*bytes).expect("parse object");
    file.sections()
        .filter(|section| section.kind() == object::SectionKind::Text)
        .flat_map(|section| section.data().expect("text data").to_vec())
        .collect()
}

/// Debug information describes the code; it never changes it. An optimized
/// object's code is byte-identical with and without `-g`, for the shapes that
/// once differed: coroutine bodies (generators, defer, actors) and the
/// loop-chunk bodies of vectors and shadowed blocks.
#[test]
fn debug_information_leaves_optimized_code_unchanged() {
    let shapes = [
        ("recursion", PROGRAM),
        (
            "shadowing",
            include_str!("../../hew-cli/tests/fixtures/debug_corpus/02-shadowing-blocks.hew"),
        ),
        (
            "vec",
            include_str!("../../hew-cli/tests/fixtures/debug_corpus/04-vec.hew"),
        ),
        (
            "generators",
            include_str!("../../hew-cli/tests/fixtures/debug_corpus/11-generators.hew"),
        ),
        (
            "defer",
            include_str!("../../hew-cli/tests/fixtures/debug_corpus/13-defer.hew"),
        ),
        (
            "actors",
            include_str!("../../hew-cli/tests/fixtures/debug_corpus/16-actor-reply.hew"),
        ),
    ];
    for (name, program) in shapes {
        let plain = tempfile::tempdir().expect("temp dir");
        let debug = tempfile::tempdir().expect("temp dir");
        let without = text_of(&emit_source(plain.path(), program, OptLevel::O2, false));
        let with = text_of(&emit_source(debug.path(), program, OptLevel::O2, true));
        assert!(!without.is_empty(), "{name} has code");
        assert!(without == with, "-g changes optimized code of {name}");
    }
}
