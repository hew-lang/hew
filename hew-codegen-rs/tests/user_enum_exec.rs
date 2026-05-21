//! Substrate-level tests for user-defined enum unit variant construction.
//!
//! Verifies that `EnumLayout` registration and `MachineVariantCtor`-emitted
//! MIR (tag store at `Place::MachineTag`) produce valid LLVM IR for
//! user-defined enums. The HIR pre-pass extension and MIR `machine_layout_names`
//! broadening are exercised end-to-end via `hew run` on
//! `examples/enums/run_colour.hew`; these tests target the codegen
//! substrate directly (hand-assembled MIR) to pin the LLVM shape without
//! round-tripping through the full pipeline.

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_mir::{
    BasicBlock, EnumLayout, FunctionCallConv, Instr, IrPipeline, MachineVariantLayout, Place,
    RawMirFunction, Terminator,
};
use hew_types::ResolvedTy;

fn emit_ll(pipeline: &IrPipeline, module_name: &str) -> String {
    let tmp = std::env::temp_dir().join(format!("hew-user-enum-exec-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create scratch dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    let artefacts =
        emit_module(pipeline, &options).expect("emit_module must succeed for user-enum fixture");
    let ll_path = artefacts
        .ll_path
        .expect("emit_module must produce an .ll when native=false");
    std::fs::read_to_string(&ll_path)
        .unwrap_or_else(|e| panic!("could not read {}: {e}", ll_path.display()))
}

/// Build a minimal `IrPipeline` that constructs a unit variant of
/// `enum Colour { Red; Green; Blue; }` and returns immediately.
///
/// MIR shape:
///   fn main() {
///       local 0: Colour   // dest slot
///       local 1: i64      // tag constant
///       Block 0:
///           ConstI64 { dest: local 1, value: 0 }     // tag for Red = 0
///           Move { dest: MachineTag(local 0), src: local 1 }
///           Return
///   }
fn colour_red_pipeline() -> IrPipeline {
    let colour_ty = ResolvedTy::Named {
        name: "Colour".to_string(),
        args: vec![],
    };
    let enum_layout = EnumLayout {
        name: "Colour".to_string(),
        tag_width: 2, // ceil(log2(3)) = 2 bits for 3 variants
        variants: vec![
            MachineVariantLayout {
                name: "Red".to_string(),
                field_tys: vec![],
            },
            MachineVariantLayout {
                name: "Green".to_string(),
                field_tys: vec![],
            },
            MachineVariantLayout {
                name: "Blue".to_string(),
                field_tys: vec![],
            },
        ],
    };
    let main_fn = RawMirFunction {
        name: "main".to_string(),
        return_ty: ResolvedTy::Unit,
        call_conv: FunctionCallConv::Default,
        params: vec![],
        locals: vec![colour_ty, ResolvedTy::I64],
        blocks: vec![BasicBlock {
            id: 0,
            statements: Vec::new(),
            instructions: vec![
                // Emit tag = 0 (Red).
                Instr::ConstI64 {
                    dest: Place::Local(1),
                    value: 0,
                },
                // Store tag into the Colour slot.
                Instr::Move {
                    dest: Place::MachineTag(0),
                    src: Place::Local(1),
                },
            ],
            terminator: Terminator::Return,
        }],
        decisions: Vec::new(),
    };
    IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![main_fn],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        diagnostics: Vec::new(),
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: vec![enum_layout],
    }
}

/// The tagged-union outer struct must be emitted under the enum type name
/// with the correct field shape: `{ iW tag, [N x iA] payload }`.
/// A 3-variant unit-only enum with `tag_width = 2` produces a 4-byte
/// alloca (i4 tag + zero-byte payload rounded to alignment) — but the
/// exact byte count is target-dependent. We assert structural markers only.
#[test]
fn enum_layout_registers_outer_struct() {
    let ll = emit_ll(&colour_red_pipeline(), "colour_red");
    // The outer named struct must be declared.
    assert!(
        ll.contains("%Colour"),
        "LLVM IR must contain a named struct '%Colour'; got:\n{ll}"
    );
}

/// `Place::MachineTag(local)` for an enum-typed local must produce a GEP
/// into field 0 of the outer struct. The tag store instruction must appear
/// in the IR.
#[test]
fn enum_unit_ctor_emits_tag_store() {
    let ll = emit_ll(&colour_red_pipeline(), "colour_tag_store");
    // The tag store: ConstI64 → Move to MachineTag → a `store` to the
    // GEP-0 of the Colour alloca. Exact token is target-shaped but the
    // LLVM `store` keyword is invariant.
    assert!(
        ll.contains("store"),
        "LLVM IR must contain a store instruction for the tag; got:\n{ll}"
    );
}

/// `emit_module` must produce a module that passes LLVM's own `verify()`.
/// This is the strongest structural signal: a misformed GEP (e.g. wrong
/// struct type or out-of-range field index) would cause LLVM verify to
/// report an error and `emit_module` to return `Err`.
#[test]
fn enum_unit_ctor_module_verifies() {
    // `emit_module` internally calls `Module::verify()` and returns
    // `Err(CodegenError::...)` on failure — the `expect` here is the gate.
    let pipeline = colour_red_pipeline();
    let tmp = std::env::temp_dir().join("hew-user-enum-exec-verify");
    std::fs::create_dir_all(&tmp).expect("create scratch dir");
    let options = EmitOptions {
        module_name: "colour_verify",
        out_dir: &tmp,
        native: false,
        wasm: false,
    };
    emit_module(&pipeline, &options)
        .expect("emit_module with user-enum layout must verify cleanly");
}

/// End-to-end signal for match-arm dispatch over a unit enum. Runs the
/// in-tree `hew` binary on `examples/enums/run_colour_match.hew` and
/// diffs stdout against the `.expected` file. The fixture exercises a
/// three-arm `match c { Red => 1, Green => 2, Blue => 3 }` and asserts
/// each scrutinee dispatches to its named variant body.
#[test]
fn run_colour_match_fixture_executes() {
    let manifest = env!("CARGO_MANIFEST_DIR");
    let hew_bin = std::path::PathBuf::from(manifest)
        .parent()
        .unwrap()
        .join("target")
        .join("debug")
        .join("hew");
    let fixture = std::path::PathBuf::from(manifest)
        .parent()
        .unwrap()
        .join("examples")
        .join("enums")
        .join("run_colour_match.hew");
    let expected_path = fixture.with_extension("expected");
    let expected = std::fs::read_to_string(&expected_path).expect("read .expected");

    let output = std::process::Command::new(&hew_bin)
        .arg("run")
        .arg(&fixture)
        .output()
        .expect("spawn hew run");
    assert!(
        output.status.success(),
        "hew run exited non-zero (status={:?}); stderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8(output.stdout).expect("stdout utf-8");
    assert_eq!(stdout, expected, "run_colour_match stdout mismatch");
}

/// End-to-end signal for wildcard arms in match-arm dispatch. Runs the
/// in-tree `hew` binary on `examples/enums/run_colour_match_wildcard.hew`
/// and diffs stdout against the `.expected` file. The fixture exercises
/// a single named arm followed by a `_` wildcard arm, asserting both
/// the named path and the catch-all path produce the expected outputs.
#[test]
fn run_colour_match_wildcard_fixture_executes() {
    let manifest = env!("CARGO_MANIFEST_DIR");
    let hew_bin = std::path::PathBuf::from(manifest)
        .parent()
        .unwrap()
        .join("target")
        .join("debug")
        .join("hew");
    let fixture = std::path::PathBuf::from(manifest)
        .parent()
        .unwrap()
        .join("examples")
        .join("enums")
        .join("run_colour_match_wildcard.hew");
    let expected_path = fixture.with_extension("expected");
    let expected = std::fs::read_to_string(&expected_path).expect("read .expected");

    let output = std::process::Command::new(&hew_bin)
        .arg("run")
        .arg(&fixture)
        .output()
        .expect("spawn hew run");
    assert!(
        output.status.success(),
        "hew run exited non-zero (status={:?}); stderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8(output.stdout).expect("stdout utf-8");
    assert_eq!(
        stdout, expected,
        "run_colour_match_wildcard stdout mismatch"
    );
}

/// End-to-end signal for `let n = match ...` — match-as-expression
/// in a binding position. Runs the in-tree `hew` binary on
/// `examples/enums/run_colour_match_let.hew` and diffs stdout against
/// the `.expected` file.
#[test]
fn run_colour_match_let_fixture_executes() {
    let manifest = env!("CARGO_MANIFEST_DIR");
    let hew_bin = std::path::PathBuf::from(manifest)
        .parent()
        .unwrap()
        .join("target")
        .join("debug")
        .join("hew");
    let fixture = std::path::PathBuf::from(manifest)
        .parent()
        .unwrap()
        .join("examples")
        .join("enums")
        .join("run_colour_match_let.hew");
    let expected_path = fixture.with_extension("expected");
    let expected = std::fs::read_to_string(&expected_path).expect("read .expected");

    let output = std::process::Command::new(&hew_bin)
        .arg("run")
        .arg(&fixture)
        .output()
        .expect("spawn hew run");
    assert!(
        output.status.success(),
        "hew run exited non-zero (status={:?}); stderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8(output.stdout).expect("stdout utf-8");
    assert_eq!(stdout, expected, "run_colour_match_let stdout mismatch");
}

/// End-to-end signal for tuple-payload variant construction and match
/// destructuring. Runs the in-tree `hew` binary on
/// `examples/enums/run_shape_tuple_ctor.hew` and diffs stdout against the
/// `.expected` file. Verifies that `Shape::Line(7)` constructs the
/// tagged-union value with the payload at the right offset and that
/// `match s { Shape::Line(x) => x; _ => 0 }` binds and reads `x` correctly.
#[test]
fn run_shape_tuple_ctor_fixture_executes() {
    run_enum_fixture_executes("run_shape_tuple_ctor");
}

/// End-to-end signal for struct-payload variant construction and match
/// destructuring. Runs `run_shape_struct_ctor.hew`: constructs
/// `Shape::Box { w: 3, h: 4 }`, matches it, multiplies the bound payload
/// fields, and prints the result.
#[test]
fn run_shape_struct_ctor_fixture_executes() {
    run_enum_fixture_executes("run_shape_struct_ctor");
}

/// End-to-end signal for a mixed-shape enum (unit + tuple + struct
/// variants in one declaration) dispatched through a single `match`
/// expression. Verifies the per-variant tag, payload offset, and payload
/// destructure all agree across declaration-order indices 0/1/2.
#[test]
fn run_shape_mixed_match_fixture_executes() {
    run_enum_fixture_executes("run_shape_mixed_match");
}

/// Pins the fail-closed surface for generic enums. The compiler must emit
/// `GenericEnumNotYetSupported` (classified as `E_NOT_YET_IMPLEMENTED`)
/// rather than silently registering a layout with under-specified payload
/// types. Confirms strictly narrower behaviour than the old "mixed enum"
/// surface this lane replaced.
#[test]
fn run_generic_enum_rejected_fixture_emits_diagnostic() {
    let manifest = env!("CARGO_MANIFEST_DIR");
    let hew_bin = std::path::PathBuf::from(manifest)
        .parent()
        .unwrap()
        .join("target")
        .join("debug")
        .join("hew");
    let fixture = std::path::PathBuf::from(manifest)
        .parent()
        .unwrap()
        .join("examples")
        .join("enums")
        .join("run_generic_enum_rejected.hew");
    let output = std::process::Command::new(&hew_bin)
        .arg("run")
        .arg(&fixture)
        .output()
        .expect("spawn hew run");
    assert!(
        !output.status.success(),
        "hew run must reject generic enum; got success exit"
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    let stdout = String::from_utf8_lossy(&output.stdout);
    let combined = format!("{stderr}{stdout}");
    assert!(
        combined.contains("GenericEnumNotYetSupported")
            || combined.contains("E_NOT_YET_IMPLEMENTED"),
        "expected GenericEnumNotYetSupported diagnostic; got:\nstderr: {stderr}\nstdout: {stdout}"
    );
    assert!(
        combined.contains("Maybe"),
        "diagnostic must name the offending enum (`Maybe`); got: {combined}"
    );
}

/// Shared helper: run `examples/enums/<name>.hew` through the in-tree
/// `hew` binary and diff stdout against `<name>.expected`.
fn run_enum_fixture_executes(name: &str) {
    let manifest = env!("CARGO_MANIFEST_DIR");
    let hew_bin = std::path::PathBuf::from(manifest)
        .parent()
        .unwrap()
        .join("target")
        .join("debug")
        .join("hew");
    let fixture = std::path::PathBuf::from(manifest)
        .parent()
        .unwrap()
        .join("examples")
        .join("enums")
        .join(format!("{name}.hew"));
    let expected_path = fixture.with_extension("expected");
    let expected = std::fs::read_to_string(&expected_path).expect("read .expected");

    let output = std::process::Command::new(&hew_bin)
        .arg("run")
        .arg(&fixture)
        .output()
        .expect("spawn hew run");
    assert!(
        output.status.success(),
        "hew run exited non-zero (status={:?}); stderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8(output.stdout).expect("stdout utf-8");
    assert_eq!(stdout, expected, "{name} stdout mismatch");
}
