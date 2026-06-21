//! gdb `-g` variable + type DWARF DIEs — the proving gate for source-level
//! value inspection. Compiles real Hew source through the front-end → MIR →
//! codegen with `debug: true`, then asserts the emitted textual LLVM IR carries
//! the variable DIEs (`!DILocalVariable`), the struct member DIEs, the enum
//! tag enumerators, and the parameter `arg:` index. A NEGATIVE case proves a
//! non-`-g` build emits zero debug-variable/type metadata, so the whole feature
//! stays gated on `-g` (the #2004 byte-clean invariant).
//!
//! This is a STATIC textual-IR assertion, not a launched debugger: gdb is
//! absent on macOS and lldb `process launch` hangs on the runtime startup, so a
//! live-debugger gate is neither host-portable nor deterministic. Asserting on
//! the IR is both. A live `gdb --batch` confirmation is a CI-only Linux follow.
//!
//! LESSONS applied:
//! - `assertions_distinguish` (P1): every assertion pins an EXACT name
//!   (`x`/`pt`/`shape`/struct members/enum variants), not a `count > 0`. A
//!   collapse-to-one-variable or a nameless DIE fails.
//! - `fail_closed_not_pretend` (P0): the negative non-`-g` case asserts ZERO
//!   variable/type DIEs.

use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_hir::{lower_program, verify_hir, ResolutionCtx};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

/// The fixture: an `i64` local, a record local, an enum-with-payload local, and
/// a function taking a typed parameter. Exercises every `resolve_di_type` arm.
const FIXTURE: &str = "\
record Point { x: i64, y: i64 }

enum Shape {
    Circle { radius: i64 };
    Rect { w: i64, h: i64 };
}

fn area(s: Shape) -> i64 {
    match s {
        Shape::Circle { radius } => radius * radius,
        Shape::Rect { w, h } => w * h,
    }
}

fn main() {
    let x: i64 = 42;
    let pt: Point = Point { x: 10, y: 20 };
    let shape: Shape = Shape::Circle { radius: 7 };
    let result: i64 = area(shape);
    print(x);
    print(pt.x);
    print(result);
}
";

/// Compile `FIXTURE` to textual LLVM IR. `debug` selects whether `-g` is on;
/// when on, the source is written to a temp file so the codegen line-index can
/// read it (spans are byte offsets into exactly this text).
fn emit_ll(debug: bool, slug: &str) -> String {
    let parsed = hew_parser::parse(FIXTURE);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let verify = verify_hir(&output.module);
    assert!(
        output.diagnostics.is_empty() && verify.is_empty(),
        "hir diagnostics: {:?} verify: {:?}",
        output.diagnostics,
        verify
    );
    let pipeline = hew_mir::lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "mir diagnostics: {:?}",
        pipeline.diagnostics
    );

    let tmp = std::env::temp_dir().join(format!("hew-dwarf-dies-{slug}"));
    let _ = std::fs::remove_dir_all(&tmp);
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let src_path = tmp.join("fixture.hew");
    std::fs::write(&src_path, FIXTURE).expect("write fixture source");

    let options = EmitOptions {
        module_name: "probe",
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
        debug,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: if debug {
            Some(src_path.as_path())
        } else {
            None
        },
    };
    let artefacts = emit_module(&pipeline, &options).expect("emit_module must succeed");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

/// `true` when `ir` contains a `!DILocalVariable(...)` whose `name:` field is
/// exactly `want` (matches `name: "want"` with a `)` or `,` after the quote).
fn has_local_var(ir: &str, want: &str) -> bool {
    let needle = format!("name: \"{want}\"");
    ir.lines()
        .filter(|l| l.contains("!DILocalVariable("))
        .any(|l| l.contains(&needle))
}

#[test]
fn debug_build_emits_named_local_and_param_variable_dies() {
    let ir = emit_ll(true, "locals");

    // The `let` locals appear as named `!DILocalVariable`s.
    assert!(
        has_local_var(&ir, "x"),
        "expected !DILocalVariable name \"x\"; IR:\n{ir}"
    );
    assert!(
        has_local_var(&ir, "pt"),
        "expected !DILocalVariable name \"pt\"; IR:\n{ir}"
    );
    assert!(
        has_local_var(&ir, "shape"),
        "expected !DILocalVariable name \"shape\"; IR:\n{ir}"
    );
    assert!(
        has_local_var(&ir, "result"),
        "expected !DILocalVariable name \"result\"; IR:\n{ir}"
    );

    // The `area(s: Shape)` parameter is a parameter variable (`arg:` set).
    let param_line = ir
        .lines()
        .find(|l| {
            l.contains("!DILocalVariable(") && l.contains("name: \"s\"") && l.contains("arg:")
        })
        .unwrap_or_else(|| {
            panic!("expected a parameter !DILocalVariable for `s` with arg:; IR:\n{ir}")
        });
    assert!(
        param_line.contains("arg: 1"),
        "parameter `s` must be arg: 1; got line: {param_line}"
    );

    // Each declare is wired to an `llvm.dbg.declare` record.
    assert!(
        ir.contains("#dbg_declare(") || ir.contains("llvm.dbg.declare"),
        "expected dbg.declare records binding allocas to the variable DIEs; IR:\n{ir}"
    );
}

#[test]
fn debug_build_emits_named_struct_member_dies() {
    let ir = emit_ll(true, "struct");

    // The `Point` record is a structure type with named members `x` and `y`.
    assert!(
        ir.contains("DW_TAG_structure_type") && ir.contains("name: \"Point\""),
        "expected a DW_TAG_structure_type named Point; IR:\n{ir}"
    );
    let has_member = |name: &str| {
        ir.lines()
            .any(|l| l.contains("DW_TAG_member") && l.contains(&format!("name: \"{name}\"")))
    };
    assert!(has_member("x"), "expected Point member `x`; IR:\n{ir}");
    assert!(has_member("y"), "expected Point member `y`; IR:\n{ir}");
}

#[test]
fn debug_build_emits_enum_tag_enumerators_by_variant_name() {
    let ir = emit_ll(true, "enum");

    // The Shape tag is a DW_TAG_enumeration_type whose enumerators are the
    // variant names — gdb prints the active variant by name.
    assert!(
        ir.contains("DW_TAG_enumeration_type"),
        "expected a DW_TAG_enumeration_type for the Shape tag; IR:\n{ir}"
    );
    let has_enumerator = |name: &str| {
        ir.lines()
            .any(|l| l.contains("!DIEnumerator(") && l.contains(&format!("name: \"{name}\"")))
    };
    assert!(
        has_enumerator("Circle"),
        "expected enumerator `Circle`; IR:\n{ir}"
    );
    assert!(
        has_enumerator("Rect"),
        "expected enumerator `Rect`; IR:\n{ir}"
    );
}

#[test]
fn debug_build_wires_subprogram_signature_types() {
    let ir = emit_ll(true, "signature");
    // The subroutine type is no longer typeless: `area(s: Shape) -> i64` has a
    // non-`{null}` `!DISubroutineType` types tuple (return + param).
    let subroutine_line = ir
        .lines()
        .find(|l| l.contains("!DISubroutineType("))
        .unwrap_or_else(|| panic!("expected a !DISubroutineType; IR:\n{ir}"));
    assert!(
        !subroutine_line.contains("types: !{null}"),
        "subroutine type must carry real return/param types, not the typeless \
         `{{null}}`; got: {subroutine_line}"
    );
}

#[test]
fn non_debug_build_emits_zero_variable_and_type_dies() {
    // The #2004 byte-clean invariant: without `-g`, NO variable/type debug
    // metadata is emitted. This is the fail-closed negative case with teeth.
    let ir = emit_ll(false, "nodebug");
    assert!(
        !ir.contains("!DILocalVariable("),
        "non-`-g` build must emit zero !DILocalVariable; IR:\n{ir}"
    );
    assert!(
        !ir.contains("DW_TAG_structure_type"),
        "non-`-g` build must emit zero struct type DIEs; IR:\n{ir}"
    );
    assert!(
        !ir.contains("DW_TAG_enumeration_type"),
        "non-`-g` build must emit zero enum type DIEs; IR:\n{ir}"
    );
    assert!(
        !ir.contains("!DIBasicType("),
        "non-`-g` build must emit zero base-type DIEs; IR:\n{ir}"
    );
}
