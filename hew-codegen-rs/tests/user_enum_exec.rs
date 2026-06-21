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
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
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
        builtin: None,
        is_opaque: false,
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
        is_indirect: false,
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
        intrinsic_id: None,
        await_deadline_ns: std::collections::HashMap::new(),
        suspend_kinds: std::collections::HashMap::new(),

        lambda_actor_user_param_locals: Vec::new(),
        span: None,
        instr_spans: ::std::collections::BTreeMap::new(),
    };
    IrPipeline {
        thir: Vec::new(),
        raw_mir: vec![main_fn],
        checked_mir: Vec::new(),
        elaborated_mir: Vec::new(),
        diagnostics: Vec::new(),
        wire_layouts: std::sync::Arc::default(),
        opaque_handle_names: vec![],
        record_layouts: Vec::new(),
        actor_layouts: Vec::new(),
        supervisor_layouts: Vec::new(),
        machine_layouts: Vec::new(),
        enum_layouts: vec![enum_layout],
        regex_literals: vec![],
        user_consts: Vec::new(),
        extern_decls: vec![],
        dyn_vtable_registry: vec![],
        hashmap_lowering_facts: vec![],
        hashset_lowering_facts: vec![],
        actor_send_aliasing: std::collections::HashMap::new(),
        polymorphic_mir: Vec::new(),
        user_clone_record_seeds: vec![],
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
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
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

    let mut command = std::process::Command::new(&hew_bin);
    command.arg("run").arg(&fixture);
    let output = hew_testutil::run_command_bounded(
        &mut command,
        format!("hew run {}", fixture.display()),
        hew_testutil::DEFAULT_EXEC_TIMEOUT,
    )
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

    let mut command = std::process::Command::new(&hew_bin);
    command.arg("run").arg(&fixture);
    let output = hew_testutil::run_command_bounded(
        &mut command,
        format!("hew run {}", fixture.display()),
        hew_testutil::DEFAULT_EXEC_TIMEOUT,
    )
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

    let mut command = std::process::Command::new(&hew_bin);
    command.arg("run").arg(&fixture);
    let output = hew_testutil::run_command_bounded(
        &mut command,
        format!("hew run {}", fixture.display()),
        hew_testutil::DEFAULT_EXEC_TIMEOUT,
    )
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

/// End-to-end signal for `==` / `!=` on fieldless enum values. Runs
/// `examples/enums/run_colour_eq.hew` and verifies both equality and
/// inequality branches compare the enum discriminant tags correctly.
#[test]
fn run_colour_eq_fixture_executes() {
    run_enum_fixture_executes("run_colour_eq");
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

/// End-to-end regression for `indirect enum` (spec §3.7.4) — a two-variant
/// recursive heap-allocated enum `Expr { Lit(i64); Neg(Expr) }`.
///
/// Root cause: the `Node(Tree, Tree)` (or `Neg(Expr)`) variant payload was
/// sized as 0 bytes because `resolve_ty` returned the opaque (zero-size) named
/// struct for self-referential fields during `build_tagged_union_layout`, making
/// `hew_alloc(0, 1)` → null → SIGSEGV.
///
/// Fix (G1): variant field types for names in the opaque set now resolve to
/// `ptr` before the struct-layout lookup so self-referential indirect-enum
/// fields contribute pointer-sized (8-byte) slots.  The heap allocation size
/// is computed on an anonymous struct (same field list) to avoid the
/// standalone-`TargetData` named-struct ABI-size bug.  Function parameters and
/// return types of indirect-enum types are also declared as `ptr`.
#[test]
fn run_indirect_enum_eval_fixture_executes() {
    run_enum_fixture_executes("run_indirect_enum_eval");
}

/// End-to-end regression for a two-self-reference `indirect enum Tree`:
/// `Node(Tree, Tree)` has two pointer-sized fields; sum() recurses correctly.
///
/// Exercises the same G1 fix as `run_indirect_enum_eval_fixture_executes` but
/// with a branching (binary-tree) variant rather than a unary chain, ensuring
/// both recursive fields are pointer-typed and the alloc size covers both.
#[test]
fn run_indirect_tree_sum_fixture_executes() {
    run_enum_fixture_executes("run_indirect_tree_sum");
}

/// End-to-end proof that a MUTUALLY-recursive `indirect enum` pair compiles
/// and evaluates correctly.
///
/// `indirect enum A { ALeaf(i64); AWrap(B); }` and
/// `indirect enum B { BLeaf(i64); BWrap(A); }` each hold a pointer-sized
/// reference to the other. The layout cycle detector must skip indirect
/// dependencies (they are pointer-shaped, not inline), so this pair must
/// compile — NOT fail with "cyclic enum layout detected".
///
/// This is the e2e counterpart of the unit test
/// `mutual_indirect_enum_pair_compiles_successfully` in `llvm::tests`.
#[test]
fn run_mutual_indirect_enum_fixture_executes() {
    run_enum_fixture_executes("run_mutual_indirect_enum");
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

    let mut command = std::process::Command::new(&hew_bin);
    command.arg("run").arg(&fixture);
    let output = hew_testutil::run_command_bounded(
        &mut command,
        format!("hew run {}", fixture.display()),
        hew_testutil::DEFAULT_EXEC_TIMEOUT,
    )
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
