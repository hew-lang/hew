//! Recursive-collection element thunk synthesis — REAL end-to-end pipeline
//! coverage for a self-recursive enum whose self-edge passes through a `Vec`
//! payload (`enum Reply { Nil; Num(i64); Arr([Reply]) }`, the Redis/JSON reply
//! shape).
//!
//! The MIR state-clone classifier breaks the value cycle at the collection
//! boundary (a `Vec` is a heap indirection, so re-encountering the enum on the
//! far side of the buffer is finite). This test proves the downstream codegen
//! synthesises the enum's per-element drop thunk and that the thunk recurses
//! through the inner `Vec<Reply>` via the owned-collection release ABI
//! (`hew_vec_free_owned`) — the runtime recursion boundary. The synthesis is
//! lazy (`get_or_declare_*` + body-exists guard), so the self-referential thunk
//! cycle is representable without non-termination.
//!
//! Driven entirely by the real parse → check → HIR → MIR → LLVM path, never a
//! hand-fed registry.
//!
//! LESSONS applied:
//! - `recursive-admission-needs-indirection-witness` (P0): the admitted
//!   self-edge is through the container indirection; the thunk releases the
//!   element via the inner collection's own owned-release symbol.
//! - `vec-element-width-symmetric-abi` (P0): the element release routes through
//!   the single `collection_layout_witness` owned-Vec family the constructor
//!   and free agree on.

use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_hir::{lower_program, ResolutionCtx};
use hew_types::{module_registry::ModuleRegistry, Checker};

/// Run the full parse → check → HIR → MIR → LLVM pipeline on `source` and
/// return the emitted textual LLVM IR. Asserts each intermediate step is free
/// of blocking diagnostics so a regression in the lowering path surfaces here
/// rather than as a confusing IR-shape mismatch.
fn emit_ll(source: &str, module_name: &str) -> String {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type-check errors: {:#?}",
        tc_output.errors
    );
    let lowered = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        lowered.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        lowered.diagnostics
    );
    let pipeline = hew_mir::lower_hir_module(&lowered.module);
    use hew_mir::MirDiagnosticKind;
    let blocking: Vec<_> = pipeline
        .diagnostics
        .iter()
        .filter(|d| {
            matches!(
                d.kind,
                MirDiagnosticKind::NotYetImplemented { .. }
                    | MirDiagnosticKind::UnresolvedPlace { .. }
                    | MirDiagnosticKind::ActorStateCloneClassificationFailed { .. }
            )
        })
        .collect();
    assert!(
        blocking.is_empty(),
        "blocking MIR diagnostics: {blocking:#?}"
    );
    let tmp = std::env::temp_dir().join(format!("hew-recursive-collection-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
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
    let artefacts = emit_module(&pipeline, &options).expect("pipeline must emit successfully");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

/// A self-recursive enum built and dropped through an owned `Vec<Reply>` forces
/// synthesis of the enum's per-element drop thunk, whose body must recurse into
/// the inner `Vec<Reply>` payload through the owned-collection release symbol.
/// Before the MIR cycle-break this program failed classification with
/// `RecordCycle`; the assertion pins that it now flows to real recursive
/// release code, not a fail-closed trap.
#[test]
fn self_recursive_enum_through_vec_synthesises_recursive_drop_thunk() {
    let source = r#"
enum Reply { Nil; Num(i64); Arr([Reply]); }

fn main() -> i64 {
    let xs = [Reply::Num(7), Reply::Arr([Reply::Nil, Reply::Num(9)])];
    let n = xs.len();
    n
}
"#;
    let ir = emit_ll(source, "recursive-enum-drop");

    // The self-recursive enum's per-element drop thunk is synthesised.
    assert!(
        ir.contains("__hew_enum_drop_inplace_Reply"),
        "expected the self-recursive enum drop thunk to be synthesised; IR:\n{ir}"
    );
    // The recursion boundary: the element release runs through the owned-Vec
    // free symbol (the inner Vec<Reply> payload), which in turn invokes the
    // per-element enum thunk at runtime. Without the cycle-break this program
    // never reached codegen.
    assert!(
        ir.contains("hew_vec_free_owned"),
        "expected the recursive drop to release the inner Vec<Reply> via the \
         owned-collection release ABI; IR:\n{ir}"
    );
}
