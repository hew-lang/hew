//! Contract tests for the TO-3 MIR shape of `dyn Trait` dispatch:
//! `Instr::CoerceToDynTrait`, `Instr::CallTraitMethod`, and the
//! `DropKind::TraitObject` integration that codegen (TO-4) lowers to
//! vtable-slot-0 `drop_in_place`.
//!
//! These tests exercise the full pipeline (parse → typecheck → HIR lower
//! → MIR lower → drop elaboration) so the side-table bridge from
//! `TypeCheckOutput.dyn_trait_coercions` /
//! `TypeCheckOutput.dyn_trait_method_calls` through HIR's
//! `HirExprKind::CoerceToDynTrait` / `HirExprKind::CallDynMethod` to
//! the MIR `Instr` shape is end-to-end-verified, not mocked in isolation.

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, DropKind, Instr, IrPipeline};
use hew_types::module_registry::ModuleRegistry;
use hew_types::{Checker, ResolvedTy};

fn pipeline(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type errors: {:#?}",
        tc_output.errors
    );
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    lower_hir_module(&output.module)
}

/// Collect every `Instr` from every block of the named function's raw MIR.
fn all_instrs(p: &IrPipeline, fn_name: &str) -> Vec<Instr> {
    p.raw_mir
        .iter()
        .find(|f| f.name == fn_name)
        .expect("function must be present in raw_mir")
        .blocks
        .iter()
        .flat_map(|b| b.instructions.iter().cloned())
        .collect()
}

/// `T → dyn Display` argument coercion at a call site emits exactly one
/// `Instr::CoerceToDynTrait` with `trait_name = "Display"` and the
/// concrete `Self` type (`i64` after `i64` default materialisation).
#[test]
fn int_to_dyn_display_coercion_emits_coerce_instr() {
    let source = r"
        fn use_display(value: dyn Display) {}

        fn main() {
            use_display(42);
        }
    ";
    let p = pipeline(source);
    let instrs = all_instrs(&p, "main");
    let coercions: Vec<_> = instrs
        .iter()
        .filter_map(|i| match i {
            Instr::CoerceToDynTrait {
                trait_name,
                concrete_type,
                method_table,
                ..
            } => Some((
                trait_name.clone(),
                concrete_type.clone(),
                method_table.clone(),
            )),
            _ => None,
        })
        .collect();
    assert_eq!(
        coercions.len(),
        1,
        "main must contain exactly one CoerceToDynTrait; got: {coercions:#?}"
    );
    let (trait_name, concrete, method_table) = &coercions[0];
    assert_eq!(trait_name, "Display");
    assert_eq!(*concrete, ResolvedTy::I64);
    // Display has one method, `fmt`, resolved to the primitive impl key
    // `i64::fmt`.
    assert_eq!(
        method_table,
        &vec![("fmt".to_string(), "i64::fmt".to_string())]
    );
}

/// A method call on a `dyn Display` receiver lowers to
/// `Instr::CallTraitMethod` with the right trait, method, and pre-computed
/// slot index. Slot 3 = first trait method (slots 0..=2 are the runtime
/// prefix triple `drop_in_place`/`size_of`/`align_of`).
#[test]
fn call_on_dyn_display_emits_calltraitmethod_at_slot_three() {
    let source = r"
        fn drive(d: dyn Display) {
            d.fmt();
        }

        fn main() {
            drive(42);
        }
    ";
    let p = pipeline(source);
    let instrs = all_instrs(&p, "drive");
    let calls: Vec<_> = instrs
        .iter()
        .filter_map(|i| match i {
            Instr::CallTraitMethod {
                trait_name,
                method_name,
                slot,
                ..
            } => Some((trait_name.clone(), method_name.clone(), *slot)),
            _ => None,
        })
        .collect();
    assert_eq!(
        calls.len(),
        1,
        "drive must contain exactly one CallTraitMethod; got: {calls:#?}"
    );
    let (trait_name, method_name, slot) = &calls[0];
    assert_eq!(trait_name, "Display");
    assert_eq!(method_name, "fmt");
    assert_eq!(
        *slot, 3,
        "vtable prefix triple occupies slots 0..3; trait methods start at slot 3"
    );
}

/// Two distinct concrete types coerced to `dyn Display` at the same call
/// site emit two distinct `Instr::CoerceToDynTrait` instructions, each
/// carrying its own concrete type and impl-fn key in the method table.
/// This is the upstream of TO-4's vtable-static dedup key
/// `(trait_name, concrete_type)`.
#[test]
fn two_concrete_types_to_dyn_display_emit_distinct_coercions() {
    let source = r"
        fn use_display(value: dyn Display) {}

        fn main() {
            use_display(42);
            use_display(true);
        }
    ";
    let p = pipeline(source);
    let instrs = all_instrs(&p, "main");
    let coercions: Vec<_> = instrs
        .iter()
        .filter_map(|i| match i {
            Instr::CoerceToDynTrait {
                concrete_type,
                method_table,
                ..
            } => Some((concrete_type.clone(), method_table.clone())),
            _ => None,
        })
        .collect();
    assert_eq!(
        coercions.len(),
        2,
        "two coercion sites → two CoerceToDynTrait instructions; got: {coercions:#?}"
    );
    let by_concrete: std::collections::HashMap<_, _> = coercions.iter().cloned().collect();
    assert_eq!(
        by_concrete.get(&ResolvedTy::I64),
        Some(&vec![("fmt".to_string(), "i64::fmt".to_string())]),
        "i64 coercion must reference i64::fmt"
    );
    assert_eq!(
        by_concrete.get(&ResolvedTy::Bool),
        Some(&vec![("fmt".to_string(), "bool::fmt".to_string())]),
        "bool coercion must reference bool::fmt"
    );
}

/// `drop_kind_for(Local, ResolvedTy::TraitObject)` returns
/// `DropKind::TraitObject`. This is the boundary contract codegen (TO-4)
/// consumes to emit the vtable-slot-0 (`drop_in_place`) dispatch on every
/// owning-scope exit for a `dyn Trait` local. End-to-end drop-plan
/// integration (registering an owned `dyn Trait` binding in
/// `owned_locals` from `let`-statement / parameter lowering) requires
/// the same backend-slot plumbing that the structural-coercion test
/// documents as upstream and out-of-scope for TO-3. Pinning the
/// discriminator-correctness contract here is the MIR-side guarantee
/// that whenever the plumbing arrives, the codegen-bound `DropKind`
/// will match the runtime ABI's slot-0 dispatch.
#[test]
fn drop_kind_for_dyn_trait_object_selects_trait_object_kind() {
    use hew_mir::Place;
    use hew_types::ResolvedTraitBound;

    let dyn_ty = ResolvedTy::TraitObject {
        traits: vec![ResolvedTraitBound {
            trait_name: "Display".to_string(),
            args: vec![],
            assoc_bindings: vec![],
        }],
    };
    // dyn-Trait locals must route to `DropKind::TraitObject`, NOT
    // `DropKind::Resource` (the pre-M2 generic `@resource` close path).
    // Routing through `Resource` would silently emit a no-op drop
    // instead of the vtable-slot-0 dispatch.
    assert_eq!(
        hew_mir::drop_kind_for_test(Place::Local(0), &dyn_ty),
        DropKind::TraitObject,
        "dyn Trait Local must select DropKind::TraitObject"
    );
    // Non-dyn Locals stay on the Resource path.
    assert_eq!(
        hew_mir::drop_kind_for_test(Place::Local(0), &ResolvedTy::I64),
        DropKind::Resource,
        "non-dyn Local must stay on the Resource path"
    );
    // Handle Places remain discriminated by Place variant — dyn-ty
    // never overrides DuplexHandle / LambdaActorHandle.
    assert_eq!(
        hew_mir::drop_kind_for_test(Place::DuplexHandle(0), &dyn_ty),
        DropKind::DuplexClose,
        "DuplexHandle Place must override ty-based discrimination"
    );
}

/// Fail-closed contract: a method call on a `dyn Trait` receiver that
/// has no entry in `TypeCheckOutput::dyn_trait_method_calls` produces
/// an HIR diagnostic (`TraitObjectMethodNoSideTableEntry`) — never a
/// silent miscompile or a runtime panic. The diagnostic surfaces at
/// HIR-lowering time, before MIR sees the call.
#[test]
fn missing_dyn_method_side_table_entry_emits_hir_diagnostic() {
    use hew_hir::HirDiagnosticKind;
    use hew_types::SpanKey;

    let source = r"
        fn drive(d: dyn Display) {
            d.fmt();
        }
        fn main() {
            drive(42);
        }
    ";
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let mut tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type errors: {:#?}",
        tc_output.errors
    );
    // Drop every dyn-trait method-call entry to simulate a producer-side
    // breakage. HIR must reject the call with the dedicated diagnostic
    // rather than silently re-deriving a rewrite from the receiver type.
    let removed = tc_output.dyn_trait_method_calls.drain().count();
    assert!(
        removed > 0,
        "the source must include at least one dyn-trait method call to \
         exercise the missing-entry path; got {removed}"
    );
    // Also clear `method_call_rewrites` for the call span so the test
    // path doesn't accidentally fall through to the legacy rewrite arm.
    let call_spans: Vec<SpanKey> = tc_output.dyn_trait_coercions.keys().cloned().collect();
    for span in call_spans {
        tc_output.method_call_rewrites.remove(&span);
    }
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let fail_closed = output
        .diagnostics
        .iter()
        .any(|d| matches!(&d.kind, HirDiagnosticKind::TraitObjectMethodNoSideTableEntry { method } if method == "fmt"));
    assert!(
        fail_closed,
        "missing dyn-method side-table entry must emit \
         TraitObjectMethodNoSideTableEntry; got: {:#?}",
        output.diagnostics
    );
}

/// The structural-impl path (`impl Widget { fn name(self) -> string }`
/// satisfying `Named`) lowers through TO-3 *identically* to the nominal
/// `impl Named for Widget` path. Verified at the HIR layer because user
/// record types do not yet reach the MIR backend's field-order registry
/// in this slice — that gap (`record type not registered in field-order
/// table`) is unrelated to trait-object dispatch and predates TO-3. The
/// HIR coercion node is the same shape codegen consumes once the
/// upstream record-layout gap closes. Mirrors TO-2's
/// `structural_impl_populates_method_table_for_dyn_named`
/// (`hew-types/tests/dyn_trait_coercion.rs:228`).
fn walk_for_structural_coercion(expr: &hew_hir::HirExpr, found: &mut bool) {
    use hew_hir::HirExprKind;
    if let HirExprKind::CoerceToDynTrait {
        trait_name,
        concrete_type,
        method_table,
        ..
    } = &expr.kind
    {
        assert_eq!(trait_name, "Named");
        assert_eq!(
            *concrete_type,
            ResolvedTy::Named {
                name: "Widget".to_string(),
                args: vec![],
                builtin: None,
            }
        );
        assert_eq!(
            method_table,
            &vec![("name".to_string(), "Widget::name".to_string())]
        );
        *found = true;
    }
    match &expr.kind {
        HirExprKind::Call { callee, args } => {
            walk_for_structural_coercion(callee, found);
            for arg in args {
                walk_for_structural_coercion(arg, found);
            }
        }
        HirExprKind::CoerceToDynTrait { value, .. } => walk_for_structural_coercion(value, found),
        HirExprKind::CallDynMethod { receiver, args, .. } => {
            walk_for_structural_coercion(receiver, found);
            for arg in args {
                walk_for_structural_coercion(arg, found);
            }
        }
        HirExprKind::Unary { operand, .. } => walk_for_structural_coercion(operand, found),
        _ => {}
    }
}

#[test]
fn structural_impl_lowers_through_to_3_identically_to_nominal() {
    let source = r#"
        trait Named {
            fn name(val: Self) -> string;
        }

        type Widget { label: string; }

        impl Widget {
            fn name(val: Widget) -> string {
                val.label
            }
        }

        fn use_named(value: dyn Named) {}

        fn main() {
            let w = Widget { label: "x" };
            use_named(w);
        }
    "#;
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    assert!(
        tc_output.errors.is_empty(),
        "type errors: {:#?}",
        tc_output.errors
    );
    // TO-2 side-table: a single structural coercion entry with
    // `Widget::name` impl-fn key (identical to the nominal path).
    assert_eq!(
        tc_output.dyn_trait_coercions.len(),
        1,
        "checker must record exactly one structural dyn-trait coercion"
    );
    let coercion = tc_output.dyn_trait_coercions.values().next().unwrap();
    assert_eq!(coercion.trait_name, "Named");
    assert_eq!(
        coercion.method_table,
        vec![("name".to_string(), "Widget::name".to_string())]
    );

    // HIR layer: the coercion materialises as `HirExprKind::CoerceToDynTrait`
    // with the same trait/concrete/method-table shape — identical to what
    // the nominal `impl Named for Widget` path produces. MIR lowering of
    // this HIR node is identical 1:1 — the per-concrete branching lives
    // entirely in codegen (TO-4) keyed on `concrete_type`.
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let mut found_coerce = false;
    for item in &output.module.items {
        if let hew_hir::HirItem::Function(f) = item {
            if f.name == "main" {
                for stmt in &f.body.statements {
                    if let hew_hir::HirStmtKind::Expr(e) = &stmt.kind {
                        walk_for_structural_coercion(e, &mut found_coerce);
                    }
                }
            }
        }
    }
    assert!(
        found_coerce,
        "HIR must contain CoerceToDynTrait for the structural-impl coercion"
    );
}
