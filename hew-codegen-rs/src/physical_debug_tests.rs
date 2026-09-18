//! Debug records written into the emitted object, and the absence of debug
//! metadata without `-g`.
//!
//! The textual IR proves what codegen requests; only the object proves what
//! LLVM's emitter wrote. The shadowed-local case is the one a flat subprogram
//! scope silently gets wrong: both bindings exist as DIEs, and a debugger reads
//! whichever it finds first.
//!
//! The object's format follows the host target. An MSVC-environment target
//! writes CodeView into `.debug$S`/`.debug$T` and no DWARF at all, so the DIE
//! assertions here run where the emitter writes DWARF and a CodeView twin
//! covers the same ground on Windows.

use std::path::Path;
use std::process::Command;

use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_types::{module_registry::ModuleRegistry, Checker};

use super::*;

const SHADOW: &str = "\
fn probe(selector: i32) -> i32 {
    let first = selector + 1;
    {
        let first = selector + 2;
        println(first);
    }
    first
}

fn main() {
    println(probe(41))
}
";

fn verified(source: &str) -> hew_mir::VerifiedPhysicalModule {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
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

/// A record payload behind a variant: the record's own members must stay named
/// and the enum must select one case, not lay every payload over the same bytes.
const ENUM: &str = "\
type Payload {
    code: i64,
}

enum Status {
    Idle,
    Packet(Payload),
}

fn main() {
    let status = Status.Packet(Payload { code: 7 });
    println(1);
}
";

fn emit(dir: &Path, debug: bool) -> EmitArtefacts {
    emit_source(dir, "shadow", SHADOW, debug)
}

fn emit_source(dir: &Path, name: &str, text: &str, debug: bool) -> EmitArtefacts {
    let source = dir.join(format!("{name}.hew"));
    std::fs::write(&source, text).expect("write fixture source");
    hew_codegen_emit(&verified(text), dir, debug.then_some(source.as_path()))
}

fn hew_codegen_emit(
    verified: &hew_mir::VerifiedPhysicalModule,
    dir: &Path,
    debug_source: Option<&Path>,
) -> EmitArtefacts {
    emit_physical_object(
        verified,
        &PhysicalEmitOptions {
            module_name: "fixture",
            out_dir: dir,
            target_triple: None,
            opt_level: OptLevel::O0,
            emit_llvm: true,
            address_sanitizer: false,
            debug_source,
        },
    )
    .expect("emit physical object")
}

fn llvm_tool(name: &str) -> std::path::PathBuf {
    Path::new(env!("HEW_LLVM_BINDIR")).join(name)
}

/// One DIE row of an `llvm-dwarfdump --debug-info` listing.
#[cfg(not(target_env = "msvc"))]
struct Die {
    depth: usize,
    tag: String,
    name: Option<String>,
    decl_line: Option<u32>,
    /// Present on a `DW_TAG_variant_part`: the member it selects on.
    discr: bool,
    /// Present on a `DW_TAG_variant`: the tag value that selects it.
    discr_value: Option<u64>,
}

#[cfg(not(target_env = "msvc"))]
fn dwarf_dies(object: &Path) -> Vec<Die> {
    let dwarfdump = llvm_tool("llvm-dwarfdump");
    let output = Command::new(&dwarfdump)
        .arg("--debug-info")
        .arg(object)
        .output()
        .unwrap_or_else(|error| panic!("run {}: {error}", dwarfdump.display()));
    assert!(
        output.status.success(),
        "llvm-dwarfdump failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    let text = String::from_utf8_lossy(&output.stdout).into_owned();
    let mut dies: Vec<Die> = Vec::new();
    for line in text.lines() {
        // A DIE row is `0x00000000:   DW_TAG_...`; its indentation is its depth.
        if let Some(rest) = line
            .strip_prefix("0x")
            .and_then(|rest| rest.split_once(": "))
        {
            let body = rest.1;
            let depth = body.len() - body.trim_start().len();
            let tag = body.trim().to_owned();
            if tag.starts_with("DW_TAG_") {
                dies.push(Die {
                    depth,
                    tag,
                    name: None,
                    decl_line: None,
                    discr: false,
                    discr_value: None,
                });
            }
            continue;
        }
        let Some(die) = dies.last_mut() else { continue };
        let trimmed = line.trim();
        if let Some(value) = trimmed.strip_prefix("DW_AT_name\t(\"") {
            die.name = value.strip_suffix("\")").map(ToOwned::to_owned);
        } else if let Some(value) = trimmed.strip_prefix("DW_AT_decl_line\t(") {
            die.decl_line = value.strip_suffix(')').and_then(|line| line.parse().ok());
        } else if trimmed.starts_with("DW_AT_discr\t(") {
            die.discr = true;
        } else if let Some(value) = trimmed.strip_prefix("DW_AT_discr_value\t(") {
            die.discr_value = value
                .strip_suffix(')')
                .and_then(|raw| raw.strip_prefix("0x"))
                .and_then(|raw| u64::from_str_radix(raw, 16).ok());
        }
    }
    dies
}

/// The two shadowed `first` bindings must be distinct DIEs in nested lexical
/// blocks, so a debugger at a PC inside the inner block resolves the inner
/// binding and one outside it resolves the outer.
#[test]
#[cfg(not(target_env = "msvc"))]
fn shadowed_locals_emit_nested_lexical_block_dies() {
    let dir = tempfile::tempdir().expect("temp dir");
    let artefacts = emit(dir.path(), true);
    let object = artefacts.native_obj_path.expect("native object");
    let dies = dwarf_dies(&object);

    let shadowed: Vec<&Die> = dies
        .iter()
        .filter(|die| die.tag == "DW_TAG_variable" && die.name.as_deref() == Some("first"))
        .collect();
    assert_eq!(
        shadowed.len(),
        2,
        "both shadowed bindings must survive as their own DIEs"
    );
    let outer = shadowed[0];
    let inner = shadowed[1];
    assert_eq!(
        outer.decl_line,
        Some(2),
        "outer `first` is declared on line 2"
    );
    assert_eq!(
        inner.decl_line,
        Some(4),
        "inner `first` is declared on line 4"
    );
    assert!(
        inner.depth > outer.depth,
        "the inner binding must sit in a deeper scope than the outer one"
    );

    // Each binding's parent DIE is a lexical block, and the inner block is a
    // child of the outer one. A flat subprogram scope fails both.
    let parent_tag = |child: &Die| {
        dies.iter()
            .take_while(|die| !std::ptr::eq(*die, child))
            .filter(|die| die.depth + 2 == child.depth)
            .last()
            .map(|die| die.tag.clone())
    };
    assert_eq!(
        parent_tag(outer).as_deref(),
        Some("DW_TAG_lexical_block"),
        "the outer binding is scoped to the body's lexical block"
    );
    assert_eq!(
        parent_tag(inner).as_deref(),
        Some("DW_TAG_lexical_block"),
        "the inner binding is scoped to the nested block"
    );
}

/// Negative control: a build without `-g` carries no debug metadata at all.
#[test]
fn a_build_without_debug_emits_no_debug_metadata() {
    let dir = tempfile::tempdir().expect("temp dir");
    let artefacts = emit(dir.path(), false);
    let ir = std::fs::read_to_string(artefacts.ll_path.expect("textual IR")).expect("read IR");
    assert!(
        !ir.contains("DICompileUnit"),
        "a build without -g must not create a compile unit"
    );
    assert!(
        !ir.contains("#dbg_") && !ir.contains("llvm.dbg."),
        "a build without -g must not record any variable location"
    );

    let debug = tempfile::tempdir().expect("temp dir");
    let with_debug = std::fs::read_to_string(emit(debug.path(), true).ll_path.expect("textual IR"))
        .expect("read IR");
    assert!(
        with_debug.contains("DICompileUnit") && with_debug.contains("#dbg_declare"),
        "the same build with -g must carry both"
    );
}

/// An enum local must reach the debugger as a variant part: one discriminant to
/// select on, one `DW_TAG_variant` per case carrying its tag value, and the
/// payload record's own field names. Describing the `{ tag, payload }` carrier
/// instead hands every variant's bytes over at once.
#[test]
#[cfg(not(target_env = "msvc"))]
fn an_enum_emits_a_variant_part_selected_by_its_discriminant() {
    let dir = tempfile::tempdir().expect("temp dir");
    let artefacts = emit_source(dir.path(), "enum", ENUM, true);
    let object = artefacts.native_obj_path.expect("native object");
    let dies = dwarf_dies(&object);

    let status = dies
        .iter()
        .position(|die| die.tag == "DW_TAG_structure_type" && die.name.as_deref() == Some("Status"))
        .expect("the enum is described by its source name");
    let part = &dies[status + 1];
    assert_eq!(
        part.tag, "DW_TAG_variant_part",
        "the enum's only element is its variant part"
    );
    assert!(
        part.discr,
        "the variant part must name the member a consumer selects on"
    );

    let cases: Vec<&Die> = dies[status..]
        .iter()
        .take_while(|die| die.depth > dies[status].depth || std::ptr::eq(*die, &dies[status]))
        .filter(|die| die.tag == "DW_TAG_variant")
        .collect();
    assert_eq!(cases.len(), 2, "both declared cases must be described");
    assert_eq!(cases[0].discr_value, Some(0), "`Idle` is selected by tag 0");
    assert_eq!(
        cases[1].discr_value,
        Some(1),
        "`Packet` is selected by tag 1"
    );

    let named = |name: &str| dies.iter().any(|die| die.name.as_deref() == Some(name));
    assert!(named("Idle") && named("Packet"), "each case is named");
    assert!(
        named("code"),
        "the payload record keeps its source field name"
    );
}

/// Negative control: a body with no enum emits no variant part, so the
/// assertion above cannot pass on debug metadata every build happens to carry.
#[test]
#[cfg(not(target_env = "msvc"))]
fn a_body_without_an_enum_emits_no_variant_part() {
    let dir = tempfile::tempdir().expect("temp dir");
    let artefacts = emit(dir.path(), true);
    let object = artefacts.native_obj_path.expect("native object");
    assert!(
        dwarf_dies(&object)
            .iter()
            .all(|die| die.tag != "DW_TAG_variant_part"),
        "a build with no enum local must not describe one"
    );
}

/// The CodeView twins of the DIE assertions above.
///
/// An MSVC-environment object carries `.debug$S` symbols and `.debug$T` types
/// and no DWARF, so the same claims are read with `llvm-pdbutil` instead. Each
/// pair is gated so exactly one side runs on a given host and neither platform
/// is left with a test that passes without asserting.
#[cfg(target_env = "msvc")]
mod codeview {
    use super::*;

    /// `llvm-pdbutil dump -<section>` over an object, as text.
    fn pdb_dump(object: &Path, section: &str) -> String {
        let pdbutil = llvm_tool("llvm-pdbutil");
        let output = Command::new(&pdbutil)
            .arg("dump")
            .arg(format!("-{section}"))
            .arg(object)
            .output()
            .unwrap_or_else(|error| panic!("run {}: {error}", pdbutil.display()));
        assert!(
            output.status.success(),
            "llvm-pdbutil failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
        String::from_utf8_lossy(&output.stdout).into_owned()
    }

    /// The nesting claim the DWARF twin makes about lexical blocks: both
    /// shadowed bindings survive as their own `S_LOCAL`, the inner one inside a
    /// block nested in the outer one's.
    #[test]
    fn shadowed_locals_emit_nested_codeview_block_scopes() {
        let dir = tempfile::tempdir().expect("temp dir");
        let artefacts = emit(dir.path(), true);
        let object = artefacts.native_obj_path.expect("native object");
        let symbols = pdb_dump(&object, "symbols");

        let records: Vec<&str> = symbols
            .lines()
            .filter_map(|line| line.split_once('|').map(|(_, record)| record.trim()))
            .collect();
        let probe = records
            .iter()
            .position(|record| record.contains("S_GPROC32_ID") && record.contains("__hew_fn_probe"))
            .expect("the probe body is described by its symbol name");
        let body: Vec<&&str> = records[probe..]
            .iter()
            .take_while(|record| !record.contains("S_GPROC32_ID") || records[probe] == **record)
            .collect();

        let blocks: Vec<usize> = body
            .iter()
            .enumerate()
            .filter(|(_, record)| record.contains("S_BLOCK32"))
            .map(|(index, _)| index)
            .collect();
        assert_eq!(
            blocks.len(),
            2,
            "the body's block and the nested one must each open a scope:\n{symbols}"
        );
        let locals: Vec<usize> = body
            .iter()
            .enumerate()
            .filter(|(_, record)| record.contains("S_LOCAL") && record.contains("`first`"))
            .map(|(index, _)| index)
            .collect();
        assert_eq!(
            locals.len(),
            2,
            "both shadowed bindings must survive as their own local:\n{symbols}"
        );
        assert!(
            blocks[0] < locals[0] && locals[0] < blocks[1] && blocks[1] < locals[1],
            "the inner binding must sit in a block opened inside the outer one's:\n{symbols}"
        );
    }

    /// CodeView has no variant-part record, so the enum is described by the
    /// carrier the emitter falls back to: a complete `LF_STRUCTURE` named for
    /// the source type, sized to the whole enum, with its members in a field
    /// list. Emitting the variant part here instead leaves `Status` with one
    /// unnamed `LF_NESTTYPE` and no members at all.
    #[test]
    fn an_enum_emits_a_complete_codeview_structure_for_its_carrier() {
        let dir = tempfile::tempdir().expect("temp dir");
        let artefacts = emit_source(dir.path(), "enum", ENUM, true);
        let object = artefacts.native_obj_path.expect("native object");
        let types = pdb_dump(&object, "types");

        // A record is its `0x....` header line plus the indented lines under
        // it, so the type index of the next record ends this one.
        let records: Vec<String> = types
            .split("\n0x")
            .map(|record| record.trim_end().to_owned())
            .collect();
        let status: Vec<&String> = records
            .iter()
            .filter(|record| record.contains("LF_STRUCTURE") && record.contains("`Status`"))
            .collect();
        assert!(
            !status.is_empty(),
            "the enum must be described by its source name:\n{types}"
        );
        let complete = status
            .iter()
            .find(|record| !record.contains("forward ref"))
            .unwrap_or_else(|| {
                panic!("the enum needs a complete record, not only a forward reference:\n{types}")
            });
        assert!(
            complete.contains("sizeof 16"),
            "the enum record must be sized to the whole carrier:\n{complete}"
        );
        let field_list = complete
            .lines()
            .find_map(|line| line.split("field list: ").nth(1))
            .and_then(|rest| rest.split_whitespace().next())
            .expect("the complete record names its field list");
        let fields = records
            .iter()
            .find(|record| record.starts_with(field_list.trim_start_matches("0x")))
            .unwrap_or_else(|| panic!("field list {field_list} is in the dump:\n{types}"));
        assert!(
            fields.contains("LF_MEMBER"),
            "the carrier's tag and payload must reach the field list:\n{fields}"
        );
        assert!(
            !fields.contains("LF_NESTTYPE"),
            "a variant part CodeView cannot resolve must not stand in for the members:\n{fields}"
        );
    }

    /// Negative control: a build without `-g` writes no CodeView at all.
    #[test]
    fn a_build_without_debug_emits_no_codeview() {
        let dir = tempfile::tempdir().expect("temp dir");
        let artefacts = emit(dir.path(), false);
        let object = artefacts.native_obj_path.expect("native object");
        let types = pdb_dump(&object, "types");
        assert!(
            !types.contains("LF_FUNC_ID"),
            "a build without -g must not describe any type:\n{types}"
        );
    }
}
