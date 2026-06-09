//! W5.006 Slice 1 (revision) — REAL end-to-end pipeline coverage for
//! generic-enum-typed actor state fields.
//!
//! Unlike `state_clone_synthesis.rs` (which hand-feeds a pre-built
//! `EnumLayout` registry onto an already-classified `ActorLayout`), this
//! test drives the *actual* compilation pipeline —
//! parse → check → HIR lower (incl. generic-enum monomorphisation) →
//! `hew_mir::lower_hir_module` → LLVM emission — on source that declares an
//! actor whose state fields are typed as generic enum instantiations
//! (`Option<string>`, `Result<i64, string>`).
//!
//! It is the regression guard for the Slice-1 ordering fix: actor state-field
//! classification was moved to a second pass that runs AFTER generic enum
//! instantiations from `module.enum_layouts` are merged into the layout
//! registry. Before the fix, a generic-enum state field could not be
//! classified (the mangled `Option$$string` layout did not yet exist when the
//! item-loop classifier ran) and fell to the paired-`None` fail-closed path,
//! silently skipping tag-aware payload clone/drop.
//!
//! The assertions prove the generic-enum instantiation flows through the
//! `__hew_enum_clone_inplace_*` / `__hew_enum_drop_inplace_*` helpers with the
//! string payload deep-cloned / dropped and the trap-208 out-of-range default
//! — driven entirely by the real lowering path, never a hand-fed registry.
//!
//! LESSONS applied:
//! - `boundary-fail-closed` (P0): the classifier is the seam against the
//!   runtime restart-with-state contract; this proves it does not silently
//!   degrade a generic-enum field to the no-op path.
//! - `producer-bridge-before-codegen` (P1): the HIR generic-enum registry
//!   (`module.enum_layouts`) is the producer; this is the end-to-end contract
//!   that the MIR consumer reads it BEFORE actor classification.

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
    let tmp = std::env::temp_dir().join(format!("hew-generic-enum-state-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create out_dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
    };
    let artefacts = emit_module(&pipeline, &options).expect("pipeline must emit successfully");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

/// Extract the body of a `define ... @<symbol>(` function up to its closing
/// `\n}` so per-helper assertions don't accidentally match another function.
fn body_of<'a>(ir: &'a str, define_prefix: &str) -> &'a str {
    let start = ir
        .find(define_prefix)
        .unwrap_or_else(|| panic!("expected definition `{define_prefix}`; IR:\n{ir}"));
    let end = ir[start..]
        .find("\n}")
        .map(|p| start + p)
        .expect("definition terminates");
    &ir[start..end]
}

/// An actor with `Option<string>` and `Result<i64, string>` state fields,
/// each constructed in `init` and a handler so the HIR mono-pass discovers the
/// `Option$$string` and `Result$$i64$$string` instantiations. Driven through
/// the real pipeline, both fields must classify as enum-typed and route the
/// heap-owning string payload through the per-enum clone/drop helpers.
#[test]
fn generic_enum_state_fields_route_through_enum_clone_drop_helpers() {
    let source = r#"
actor Mailbox {
    let pending: Option<string>;
    let last: Result<i64, string>;

    init(seed: string) {
        pending = Some(seed);
        last = Ok(0);
    }

    receive fn store(msg: string) {
        pending = Some(msg);
    }

    receive fn fail(reason: string) {
        last = Err(reason);
    }
}

fn main() -> i64 {
    0
}
"#;
    let ir = emit_ll(source, "generic-enum-state");

    // ── Option$$string: helper synthesis + actor routing ──────────────────
    // Mangled symbols contain `$`, so LLVM emits them in quoted form.
    let opt_clone = r#"define internal i32 @"__hew_enum_clone_inplace_Option$$string"("#;
    let opt_drop = r#"define internal void @"__hew_enum_drop_inplace_Option$$string"("#;
    assert!(
        ir.contains(opt_clone),
        "expected Option<string> clone helper synthesised from the real pipeline; IR:\n{ir}"
    );
    assert!(
        ir.contains(opt_drop),
        "expected Option<string> drop helper synthesised from the real pipeline; IR:\n{ir}"
    );

    // ── Result$$i64$string: helper synthesis ──────────────────────────────
    let res_clone = r#"define internal i32 @"__hew_enum_clone_inplace_Result$$i64$string"("#;
    let res_drop = r#"define internal void @"__hew_enum_drop_inplace_Result$$i64$string"("#;
    assert!(
        ir.contains(res_clone),
        "expected Result<i64, string> clone helper synthesised from the real pipeline; IR:\n{ir}"
    );
    assert!(
        ir.contains(res_drop),
        "expected Result<i64, string> drop helper synthesised from the real pipeline; IR:\n{ir}"
    );

    // The actor's synthesised state clone/drop bodies must CALL the per-enum
    // helpers — proving the field classified as Enum (not the fail-closed
    // paired-None path, which would emit no such call).
    assert!(
        ir.contains(r#"call i32 @"__hew_enum_clone_inplace_Option$$string"("#)
            && ir.contains(r#"call i32 @"__hew_enum_clone_inplace_Result$$i64$string"("#),
        "actor state clone must route both generic-enum fields through their helpers; IR:\n{ir}"
    );
    assert!(
        ir.contains(r#"call void @"__hew_enum_drop_inplace_Option$$string"("#)
            && ir.contains(r#"call void @"__hew_enum_drop_inplace_Result$$i64$string"("#),
        "actor state drop must route both generic-enum fields through their helpers; IR:\n{ir}"
    );

    // ── Helper bodies: tag-dispatch, deep-clone/drop the string payload,
    //    trap-208 on out-of-range tag. ─────────────────────────────────────
    for prefix in [opt_clone, res_clone] {
        let body = body_of(&ir, prefix);
        assert!(
            body.contains("switch"),
            "enum clone helper `{prefix}` must tag-dispatch via switch; body:\n{body}"
        );
        assert!(
            body.contains("@hew_string_clone"),
            "enum clone helper `{prefix}` must deep-clone the string payload; body:\n{body}"
        );
        assert!(
            body.contains("call void @hew_trap_with_code(i32 208)"),
            "enum clone helper `{prefix}` must trap (208) on out-of-range tag; body:\n{body}"
        );
    }
    for prefix in [opt_drop, res_drop] {
        let body = body_of(&ir, prefix);
        assert!(
            body.contains("switch"),
            "enum drop helper `{prefix}` must tag-dispatch via switch; body:\n{body}"
        );
        assert!(
            body.contains("@hew_string_drop"),
            "enum drop helper `{prefix}` must drop the string payload; body:\n{body}"
        );
        assert!(
            body.contains("call void @hew_trap_with_code(i32 208)"),
            "enum drop helper `{prefix}` must trap (208) on out-of-range tag; body:\n{body}"
        );
    }
}
