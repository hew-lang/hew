//! DWARF written into the emitted object, and the absence of debug metadata
//! without `-g`.
//!
//! The textual IR proves what codegen requests; only the object proves what
//! LLVM's DWARF emitter wrote. The shadowed-local case is the one a flat
//! subprogram scope silently gets wrong: both bindings exist as DIEs, and a
//! debugger reads whichever it finds first.

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

fn emit(dir: &Path, debug: bool) -> EmitArtefacts {
    let source = dir.join("shadow.hew");
    std::fs::write(&source, SHADOW).expect("write fixture source");
    hew_codegen_emit(&verified(SHADOW), dir, debug.then_some(source.as_path()))
}

fn hew_codegen_emit(
    verified: &hew_mir::VerifiedPhysicalModule,
    dir: &Path,
    debug_source: Option<&Path>,
) -> EmitArtefacts {
    emit_physical_object(
        verified,
        &PhysicalEmitOptions {
            module_name: "shadow",
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

/// One DIE row of an `llvm-dwarfdump --debug-info` listing.
struct Die {
    depth: usize,
    tag: String,
    name: Option<String>,
    decl_line: Option<u32>,
}

fn dwarf_dies(object: &Path) -> Vec<Die> {
    let dwarfdump = Path::new(env!("HEW_LLVM_BINDIR")).join("llvm-dwarfdump");
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
        }
    }
    dies
}

/// The two shadowed `first` bindings must be distinct DIEs in nested lexical
/// blocks, so a debugger at a PC inside the inner block resolves the inner
/// binding and one outside it resolves the outer.
#[test]
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
