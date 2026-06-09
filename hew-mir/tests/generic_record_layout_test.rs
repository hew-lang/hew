//! G-2.d — MIR per-instantiation record layouts.
//!
//! Pins the MIR consumer of `HirModule.record_layouts`. Every struct-init
//! site that targets a generic user record type produces an
//! `Instr::RecordInit` whose `ty` carries the post-substitution concrete
//! `ResolvedTy::Named { name, args: <concrete> }`, and the
//! `IrPipeline.record_layouts` vec contains one entry per distinct
//! instantiation under the mangled symbol name.
//!
//! These tests are the MIR-side counterpart to the HIR producer tests in
//! `hew-hir/tests/type_mono_registry_test.rs` (G-2.c). They additionally
//! close the `args: Vec::new()` HIR-side limitation noted by G-2.c's
//! reviewer: `Box { value: xs }` returned from a fn declared as
//! `-> Box<Vec<i64>>` round-trips through HIR/MIR without producing a
//! `ReturnTypeMismatch` diagnostic.

use hew_hir::{lower_program, mangle, ResolutionCtx};
use hew_mir::{lower_hir_module, Instr, IrPipeline};
use hew_types::{module_registry::ModuleRegistry, Checker, ResolvedTy};

fn pipeline_with_tc(source: &str) -> (IrPipeline, Vec<hew_hir::HirDiagnostic>) {
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
        "typecheck errors: {:#?}",
        tc_output.errors
    );
    let hir_out = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let mir = lower_hir_module(&hir_out.module);
    (mir, hir_out.diagnostics)
}

fn record_init_tys(pl: &IrPipeline, fn_name: &str) -> Vec<ResolvedTy> {
    pl.raw_mir
        .iter()
        .find(|f| f.name == fn_name)
        .unwrap_or_else(|| panic!("{fn_name} must be in raw_mir"))
        .blocks
        .iter()
        .flat_map(|b| b.instructions.iter())
        .filter_map(|instr| match instr {
            Instr::RecordInit { ty, .. } => Some(ty.clone()),
            _ => None,
        })
        .collect()
}

fn layout_names(pl: &IrPipeline) -> Vec<String> {
    pl.record_layouts.iter().map(|l| l.name.clone()).collect()
}

/// `Box { value: 42 }` against `pub type Box<T> { value: T }` lowers to
/// an `Instr::RecordInit` whose `ty` carries the concrete `Box<i64>`
/// (`Named { Box, [I64] }`) — not the bare `Named { Box, [] }` that
/// pre-G-2.d HIR emitted. The MIR pipeline contains one `RecordLayout`
/// under the mangled name `Box$$i64`.
#[test]
fn struct_init_carries_concrete_type_args_and_emits_mangled_layout() {
    let (pl, hir_diags) = pipeline_with_tc(
        "pub type Box<T> { value: T }
         fn main() -> i64 {
             let b = Box { value: 42 };
             0
         }",
    );
    assert!(
        hir_diags.is_empty(),
        "unexpected HIR diagnostics: {hir_diags:#?}"
    );

    let tys = record_init_tys(&pl, "main");
    assert_eq!(tys.len(), 1, "expected exactly one RecordInit in `main`");
    match &tys[0] {
        ResolvedTy::Named { name, args, .. } => {
            assert_eq!(name, "Box");
            assert_eq!(args, &vec![ResolvedTy::I64]);
        }
        other => panic!("unexpected RecordInit ty: {other:?}"),
    }

    let names = layout_names(&pl);
    let mangled = mangle("Box", &[ResolvedTy::I64]);
    assert_eq!(mangled, "Box$$i64");
    assert!(
        names.contains(&mangled),
        "expected {mangled} in MIR record_layouts; got {names:?}"
    );
    // No bare-name `Box` layout — generic record decls emit zero
    // bare-name layouts; only per-instantiation mangled entries.
    assert!(
        !names.iter().any(|n| n == "Box"),
        "bare `Box` layout must not appear (generic decl); got {names:?}"
    );
}

/// `let b: Box<i64> = Box { value: 42 }; let x = b.value;` round-trips
/// through HIR/MIR: the field access uses the same mangled key as the
/// struct-init, so `RecordFieldLoad` finds the offset and produces no
/// `NotYetImplemented` diagnostic.
#[test]
fn field_access_through_generic_record_round_trips() {
    let (pl, hir_diags) = pipeline_with_tc(
        "pub type Box<T> { value: T }
         fn main() -> i64 {
             let b: Box<i64> = Box { value: 42 };
             let x = b.value;
             x
         }",
    );
    assert!(
        hir_diags.is_empty(),
        "unexpected HIR diagnostics: {hir_diags:#?}"
    );
    // MIR may emit `DecisionMapTotal` for user-typed locals whose
    // value-class table doesn't carry an entry for the bare name `Box`
    // (the value-class registry is keyed by source-declared TypeDecl
    // name; user `pub type` without a `#[resource]` / `#[linear]`
    // marker is BitCopy by default). That diagnostic is orthogonal to
    // G-2.d's contract — assert only that the failure mode this slice
    // closes does NOT fire: no `NotYetImplemented` for unregistered
    // field-order on the generic record's mangled key.
    for diag in &pl.diagnostics {
        if let hew_mir::MirDiagnosticKind::NotYetImplemented { construct, .. } = &diag.kind {
            assert!(
                !construct.contains("unregistered record")
                    && !construct.contains("not registered in field-order table")
                    && !construct.contains("unknown field"),
                "regression: unexpected NotYetImplemented on generic record \
                 field-access: {construct}"
            );
        }
    }
    // Sanity: there is at least one RecordFieldLoad in `main`.
    let has_field_load = pl
        .raw_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main in raw_mir")
        .blocks
        .iter()
        .any(|b| {
            b.instructions
                .iter()
                .any(|i| matches!(i, Instr::RecordFieldLoad { .. }))
        });
    assert!(
        has_field_load,
        "expected at least one RecordFieldLoad for `b.value`"
    );
}

/// `fn wrap(xs: Vec<i64>) -> Box<Vec<i64>> { Box { value: xs } }` —
/// the explicit limitation flagged by G-2.c's reviewer at HIR
/// `lower.rs:~1953` (`args: Vec::new()` causing `ReturnTypeMismatch`).
/// With G-2.d's fix, the HIR-recorded `expr.ty` on the tail
/// `Box { value: xs }` is now `Named { Box, [Vec<i64>] }`, matching the
/// declared return type, so no diagnostic fires.
#[test]
fn generic_record_returned_from_fn_does_not_mismatch() {
    let (pl, hir_diags) = pipeline_with_tc(
        "pub type Box<T> { value: T }
         fn wrap(xs: Vec<i64>) -> Box<Vec<i64>> {
             Box { value: xs }
         }",
    );
    assert!(
        hir_diags.is_empty(),
        "unexpected HIR diagnostics (ReturnTypeMismatch regression?): {hir_diags:#?}"
    );

    // The RecordInit ty in `wrap`'s MIR is the concrete Box<Vec<i64>>.
    let tys = record_init_tys(&pl, "wrap");
    assert_eq!(tys.len(), 1);
    match &tys[0] {
        ResolvedTy::Named { name, args, .. } => {
            assert_eq!(name, "Box");
            assert_eq!(args.len(), 1);
            match &args[0] {
                ResolvedTy::Named {
                    name: inner,
                    args: inner_args,
                    ..
                } => {
                    assert_eq!(inner, "Vec");
                    assert_eq!(inner_args, &vec![ResolvedTy::I64]);
                }
                other => panic!("expected Vec<i64> inside Box, got {other:?}"),
            }
        }
        other => panic!("expected Named Box, got {other:?}"),
    }

    // And the MIR carries one mangled layout for the Box<Vec<i64>>
    // instantiation. (`Vec<i64>` itself stays builtin-injected, so it
    // does not produce an entry.)
    let expected = mangle(
        "Box",
        &[ResolvedTy::Named {
            name: "Vec".to_string(),
            args: vec![ResolvedTy::I64],
            builtin: None,
            is_opaque: false,
        }],
    );
    let names = layout_names(&pl);
    assert!(
        names.contains(&expected),
        "expected {expected} in MIR record_layouts; got {names:?}"
    );
}

/// Two distinct instantiations of the same generic record in the same
/// program produce two distinct `RecordLayout` entries with distinct
/// mangled names, and each `Instr::RecordInit` targets the corresponding
/// concrete type.
#[test]
fn two_distinct_instantiations_emit_two_layouts() {
    let (pl, hir_diags) = pipeline_with_tc(
        "pub type Box<T> { value: T }
         fn main() -> i64 {
             let a = Box { value: 42 };
             let b = Box { value: true };
             0
         }",
    );
    assert!(
        hir_diags.is_empty(),
        "unexpected HIR diagnostics: {hir_diags:#?}"
    );

    let names = layout_names(&pl);
    let box_i64 = mangle("Box", &[ResolvedTy::I64]);
    let box_bool = mangle("Box", &[ResolvedTy::Bool]);
    assert!(
        names.contains(&box_i64),
        "expected {box_i64} in layouts; got {names:?}"
    );
    assert!(
        names.contains(&box_bool),
        "expected {box_bool} in layouts; got {names:?}"
    );
    assert!(
        !names.iter().any(|n| n == "Box"),
        "no bare-name `Box` layout (generic decl); got {names:?}"
    );

    let tys = record_init_tys(&pl, "main");
    assert_eq!(tys.len(), 2, "expected two RecordInit instructions");
    let mut seen_i64 = false;
    let mut seen_bool = false;
    for ty in &tys {
        if let ResolvedTy::Named { name, args, .. } = ty {
            assert_eq!(name, "Box");
            assert_eq!(args.len(), 1);
            match args[0] {
                ResolvedTy::I64 => seen_i64 = true,
                ResolvedTy::Bool => seen_bool = true,
                _ => panic!("unexpected arg: {ty:?}"),
            }
        }
    }
    assert!(seen_i64 && seen_bool);
}
