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

/// `drop_kind_for(Local, ResolvedTy::TraitObject, Some(storage))` returns
/// `DropKind::TraitObject { storage }` for both `FrameOwned` (coercion-
/// site / parameter shape) and `HeapBoxed` (call-result shape). This is
/// the boundary contract codegen (TO-4 / W3.031 Stage 6) consumes to
/// emit the vtable-slot-0 (`drop_in_place`) dispatch plus the storage-
/// discriminated release ritual on every owning-scope exit for a
/// `dyn Trait` local. End-to-end drop-plan integration (registering an
/// owned `dyn Trait` binding in `owned_locals` from `let`-statement /
/// parameter lowering) is covered by the MIR-pipeline regression test
/// `dyn_trait_object_local_elaborates_with_frame_owned_drop`; pinning
/// the discriminator-correctness contract here is the unit-level
/// guarantee that the codegen-bound `DropKind` will match the runtime
/// ABI's slot-0 dispatch.
#[test]
fn drop_kind_for_dyn_trait_object_selects_trait_object_kind() {
    use hew_mir::{Place, TraitObjectStorage};
    use hew_types::ResolvedTraitBound;

    let dyn_ty = ResolvedTy::TraitObject {
        traits: vec![ResolvedTraitBound {
            trait_name: "Display".to_string(),
            args: vec![],
            assoc_bindings: vec![],
        }],
    };
    // dyn-Trait locals must route to `DropKind::TraitObject { storage }`,
    // NOT `DropKind::Resource` (the pre-M2 generic `@resource` close
    // path). Routing through `Resource` would silently emit a no-op
    // drop instead of the vtable-slot-0 dispatch.
    assert_eq!(
        hew_mir::drop_kind_for_test(
            Place::Local(0),
            &dyn_ty,
            Some(TraitObjectStorage::FrameOwned)
        ),
        DropKind::TraitObject {
            storage: TraitObjectStorage::FrameOwned
        },
        "dyn Trait Local with FrameOwned hint must select DropKind::TraitObject {{ FrameOwned }}"
    );
    assert_eq!(
        hew_mir::drop_kind_for_test(
            Place::Local(0),
            &dyn_ty,
            Some(TraitObjectStorage::HeapBoxed)
        ),
        DropKind::TraitObject {
            storage: TraitObjectStorage::HeapBoxed
        },
        "dyn Trait Local with HeapBoxed hint must select DropKind::TraitObject {{ HeapBoxed }}"
    );
    // Non-dyn Locals stay on the Resource path; the storage hint is
    // ignored (the dispatcher reads it only when `(place, ty)` selects
    // the dyn-trait arm).
    assert_eq!(
        hew_mir::drop_kind_for_test(Place::Local(0), &ResolvedTy::I64, None),
        DropKind::Resource,
        "non-dyn Local must stay on the Resource path"
    );
    // Handle Places remain discriminated by Place variant — dyn-ty
    // never overrides DuplexHandle / LambdaActorHandle.
    assert_eq!(
        hew_mir::drop_kind_for_test(Place::DuplexHandle(0), &dyn_ty, None),
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
                is_opaque: false,
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

/// W3.031 Stage 1 regression: an owned `dyn Trait` local introduced by
/// a `CoerceToDynTrait` site is elaborated through `build_lifo_drops`
/// with `DropKind::TraitObject { storage: FrameOwned }`.
///
/// Pre-Stage-1, `dyn Trait` locals were classified as
/// `ValueClass::PersistentShare` and silently skipped by
/// `build_lifo_drops` — every owned trait-object binding leaked its
/// `drop_in_place` ritual. The new dyn-trait arm in `build_lifo_drops`
/// emits the drop with the storage discriminator sourced from the
/// per-binding `dyn_trait_storage` side table populated at the
/// introducing `let` statement.
///
/// `let d = <coerce-to-dyn>` populates the storage as `FrameOwned`
/// (the fat pointer's `data` word aliases the concrete value's frame
/// slot; no `hew_dyn_box_free` runs at drop time).
#[test]
fn dyn_trait_object_local_elaborates_with_frame_owned_drop() {
    use hew_mir::{ElabDrop, Place, TraitObjectStorage};

    let source = r"
        fn main() {
            let d: dyn Display = 42;
        }
    ";
    let p = pipeline(source);
    let elab = p
        .elaborated_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main must be present in elaborated_mir");

    // Collect every `ElabDrop` across every `(ExitPath, DropPlan)` exit.
    let dyn_drops: Vec<&ElabDrop> = elab
        .drop_plans
        .iter()
        .flat_map(|(_, plan)| plan.drops.iter())
        .filter(|d| matches!(d.kind, hew_mir::DropKind::TraitObject { .. }))
        .collect();
    assert_eq!(
        dyn_drops.len(),
        1,
        "main must have exactly one DropKind::TraitObject ElabDrop; got: {dyn_drops:#?}"
    );
    let drop = dyn_drops[0];
    assert!(
        matches!(drop.place, Place::Local(_)),
        "dyn Trait local drop must address a Local Place; got: {:?}",
        drop.place
    );
    assert!(
        matches!(drop.ty, ResolvedTy::TraitObject { .. }),
        "dyn Trait drop must carry ResolvedTy::TraitObject; got: {:?}",
        drop.ty
    );
    assert_eq!(
        drop.kind,
        hew_mir::DropKind::TraitObject {
            storage: TraitObjectStorage::FrameOwned
        },
        "coerced dyn Trait local must select FrameOwned storage"
    );
    assert_eq!(
        drop.drop_fn, None,
        "dyn Trait drop never carries a Type::close drop_fn; the ritual is the \
         vtable slot-0 dispatch (`drop_in_place`)"
    );
}

/// W3.031 Stage 1 negative regression: a genuinely owned non-dyn
/// `@resource` local continues to elaborate through the pre-existing
/// `AffineResource` path with `DropKind::Resource` + a `drop_fn`
/// pointing at the type's `close` method. The new dyn-trait arm must
/// NOT intercept non-dyn types.
#[test]
fn non_dyn_resource_local_still_elaborates_as_resource_drop() {
    use hew_mir::{ElabDrop, Place};

    let source = r"
        #[resource]
        type Handle {
            fd: i64;
            fn close(consuming self) -> i64 { 0 }
        }

        fn main() {
            let h: Handle = Handle { fd: 0 };
        }
    ";
    let p = pipeline(source);
    let elab = p
        .elaborated_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main must be present in elaborated_mir");

    // Every drop in main must be a `DropKind::Resource` — no dyn-trait
    // drop must leak into a non-dyn body.
    let drops: Vec<&ElabDrop> = elab
        .drop_plans
        .iter()
        .flat_map(|(_, plan)| plan.drops.iter())
        .collect();
    assert!(
        !drops.is_empty(),
        "main must have at least one elaborated drop for the Handle local"
    );
    for d in &drops {
        assert!(
            matches!(d.kind, hew_mir::DropKind::Resource),
            "non-dyn @resource local must elaborate as DropKind::Resource; got: {:?}",
            d.kind
        );
        assert_eq!(
            d.drop_fn,
            Some(hew_mir::DropFnSpec::UserClose("Handle::close".to_string())),
            "Resource drop must carry the user Handle::close ritual"
        );
        assert!(
            matches!(d.place, Place::Local(_)),
            "Resource drop must address a Local Place; got: {:?}",
            d.place
        );
    }
}

// ─────────────────────────────────────────────────────────────────────────
// Move/drop suppression at `CoerceToDynTrait` sites + transitive
// `BindingRef → BindingRef` rebind suppression.
//
// Invariant: every fat-pointer's vtable slot-0 `drop_in_place` ritual
// must run exactly once at the *final* binding's scope exit. Two
// independent paths could otherwise double-drop the underlying storage:
//
//   1. The concrete source binding consumed by a `CoerceToDynTrait`
//      producer (`let d: dyn T = c;`) — without producer-site
//      suppression, `c` would still appear in `owned_locals` and emit
//      its own scope-exit `DropKind::Resource` on the same storage the
//      dyn binding now owns.
//
//   2. Intermediate dyn bindings in a transitive rebind chain
//      (`let d1: dyn T = ...; let d2 = d1; let d3 = d2;`) — without
//      transitive suppression, every intermediate binding would emit
//      its own `DropKind::TraitObject` at its own scope exit and free
//      the fat pointer multiple times.
//
// These tests pin both paths.
// ─────────────────────────────────────────────────────────────────────────

/// `let c = <concrete @resource>; let d: dyn T = c;` — the concrete
/// binding's `DropKind::Resource` is suppressed at the
/// `CoerceToDynTrait` producer site, and only the dyn binding's
/// `DropKind::TraitObject { FrameOwned }` is elaborated.
#[test]
fn coerce_to_dyn_trait_suppresses_concrete_source_drop() {
    use hew_mir::{DropKind, ElabDrop, TraitObjectStorage};

    let source = r#"
        trait Named {
            fn name(val: Self) -> string;
        }

        #[resource]
        type Widget {
            label: string;
            fn close(consuming self) -> i64 { 0 }
        }

        impl Widget {
            fn name(val: Widget) -> string { val.label }
        }

        fn main() {
            let w = Widget { label: "x" };
            let d: dyn Named = w;
        }
    "#;
    let p = pipeline(source);
    let elab = p
        .elaborated_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main must be present in elaborated_mir");

    let drops: Vec<&ElabDrop> = elab
        .drop_plans
        .iter()
        .flat_map(|(_, plan)| plan.drops.iter())
        .collect();

    let resource_drops: Vec<&ElabDrop> = drops
        .iter()
        .copied()
        .filter(|d| matches!(d.kind, DropKind::Resource))
        .collect();
    assert!(
        resource_drops.is_empty(),
        "concrete Widget binding's DropKind::Resource must be suppressed once \
         the value is moved into a CoerceToDynTrait producer; got: {resource_drops:#?}"
    );

    let dyn_drops: Vec<&ElabDrop> = drops
        .iter()
        .copied()
        .filter(|d| matches!(d.kind, DropKind::TraitObject { .. }))
        .collect();
    assert_eq!(
        dyn_drops.len(),
        1,
        "exactly one DropKind::TraitObject must own the lifecycle; got: {dyn_drops:#?}"
    );
    assert_eq!(
        dyn_drops[0].kind,
        DropKind::TraitObject {
            storage: TraitObjectStorage::FrameOwned
        },
    );
}

/// `let d1: dyn T = ...; let d2 = d1;` — the source dyn binding `d1`
/// is removed from `owned_locals` at the transitive-rebind point so
/// the vtable ritual runs exactly once at `d2`'s scope exit.
#[test]
fn transitive_dyn_rebind_suppresses_source_drop_once() {
    use hew_mir::{DropKind, ElabDrop, TraitObjectStorage};

    let source = r"
        fn main() {
            let d1: dyn Display = 42;
            let d2 = d1;
        }
    ";
    let p = pipeline(source);
    let elab = p
        .elaborated_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main must be present in elaborated_mir");

    let dyn_drops: Vec<&ElabDrop> = elab
        .drop_plans
        .iter()
        .flat_map(|(_, plan)| plan.drops.iter())
        .filter(|d| matches!(d.kind, DropKind::TraitObject { .. }))
        .collect();
    assert_eq!(
        dyn_drops.len(),
        1,
        "transitive dyn rebind must elaborate exactly one DropKind::TraitObject \
         at the final binding's scope exit; got: {dyn_drops:#?}"
    );
    assert_eq!(
        dyn_drops[0].kind,
        DropKind::TraitObject {
            storage: TraitObjectStorage::FrameOwned
        },
        "rebind must propagate the source binding's FrameOwned storage",
    );
}

/// `let d1 = ...; let d2 = d1; let d3 = d2;` — three-link rebind chain
/// collapses to a single drop at `d3`'s scope exit.
#[test]
fn three_link_dyn_rebind_chain_collapses_to_single_drop() {
    use hew_mir::{DropKind, ElabDrop, TraitObjectStorage};

    let source = r"
        fn main() {
            let d1: dyn Display = 42;
            let d2 = d1;
            let d3 = d2;
        }
    ";
    let p = pipeline(source);
    let elab = p
        .elaborated_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main must be present in elaborated_mir");

    let dyn_drops: Vec<&ElabDrop> = elab
        .drop_plans
        .iter()
        .flat_map(|(_, plan)| plan.drops.iter())
        .filter(|d| matches!(d.kind, DropKind::TraitObject { .. }))
        .collect();
    assert_eq!(
        dyn_drops.len(),
        1,
        "three-link dyn rebind chain must elaborate exactly one \
         DropKind::TraitObject; got: {dyn_drops:#?}"
    );
    assert_eq!(
        dyn_drops[0].kind,
        DropKind::TraitObject {
            storage: TraitObjectStorage::FrameOwned
        },
    );
}

/// Negative regression: non-dyn move semantics for a `@resource` rebind
/// continue to elaborate exactly one `DropKind::Resource` at the final
/// binding's scope exit. The new dyn-suppression machinery must not
/// touch non-dyn-typed bindings.
#[test]
fn non_dyn_resource_rebind_still_elaborates_single_resource_drop() {
    use hew_mir::{DropKind, ElabDrop};

    let source = r"
        #[resource]
        type Handle {
            fd: i64;
            fn close(consuming self) -> i64 { 0 }
        }

        fn main() {
            let h: Handle = Handle { fd: 0 };
            let h2 = h;
        }
    ";
    let p = pipeline(source);
    let elab = p
        .elaborated_mir
        .iter()
        .find(|f| f.name == "main")
        .expect("main must be present in elaborated_mir");

    let resource_drops: Vec<&ElabDrop> = elab
        .drop_plans
        .iter()
        .flat_map(|(_, plan)| plan.drops.iter())
        .filter(|d| matches!(d.kind, DropKind::Resource))
        .collect();
    assert_eq!(
        resource_drops.len(),
        1,
        "@resource rebind must elaborate exactly one DropKind::Resource at \
         the final binding's scope exit (ordinary move semantics, unchanged \
         by dyn-suppression); got: {resource_drops:#?}"
    );

    let dyn_drops: Vec<&ElabDrop> = elab
        .drop_plans
        .iter()
        .flat_map(|(_, plan)| plan.drops.iter())
        .filter(|d| matches!(d.kind, DropKind::TraitObject { .. }))
        .collect();
    assert!(
        dyn_drops.is_empty(),
        "no DropKind::TraitObject must leak into a non-dyn body; got: {dyn_drops:#?}"
    );
}

// ---------------------------------------------------------------------------
// W3.031 Stage 1.6 — typed `FnSig` on `Instr::CallTraitMethod`.
// ---------------------------------------------------------------------------
//
// Stage 1.6 makes the substituted caller-side signature self-contained on
// the MIR instruction (Q-β / council blocking finding #4). Codegen (Stage 7)
// consumes it verbatim to derive the erased indirect-call type:
//   - drop `params[0]` (Self);
//   - prepend one `ptr` arg (the fat-pointer data word);
//   - lower the remaining params and `return_type` normally.
//
// The tests below pin three properties: (1) a single-arg dispatch carries
// the correctly substituted signature; (2) a multi-arg dispatch with
// trait-type-param substitution propagates every argument; (3) a producer
// fed an unresolved signature fails closed with the named MIR diagnostic
// rather than fabricating a default.

/// (1) `dyn Display::fmt(self) -> string` — single-receiver method. The
/// MIR-carried `signature` must match the trait declaration verbatim (no
/// substitution to do, but the field must be populated).
#[test]
fn stage_1_6_single_arg_dyn_dispatch_carries_substituted_signature() {
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
    let call = instrs
        .iter()
        .find_map(|i| match i {
            Instr::CallTraitMethod {
                trait_name,
                method_name,
                signature,
                ..
            } if trait_name == "Display" && method_name == "fmt" => Some(signature.clone()),
            _ => None,
        })
        .expect("drive must lower one Display::fmt dispatch");
    // Receiver is filtered out by `lookup_trait_method(skip_receiver=true)`,
    // so `params` is empty for `fmt(&self) -> string`.
    assert!(
        call.params.is_empty(),
        "Display::fmt has no non-receiver params; got: {:?}",
        call.params
    );
    assert_eq!(
        call.return_type,
        hew_types::Ty::String,
        "Display::fmt returns string; got: {:?}",
        call.return_type
    );
}

/// (2) `dyn Index<Output = T>::at(int) -> T` — multi-arg style: the
/// `index` is one arg, plus the associated-type projection
/// `Self::Output -> int` must be substituted from the receiver's
/// trait-object bound. This is the canonical multi-arg + substitution
/// case for the slot-3 indirect dispatch.
#[test]
fn stage_1_6_multi_arg_dyn_dispatch_propagates_assoc_substitution() {
    let source = r"
        fn first(idx: dyn Index<Output = i32>) -> i32 {
            idx[2]
        }
    ";
    let p = pipeline(source);
    let instrs = all_instrs(&p, "first");
    let call = instrs
        .iter()
        .find_map(|i| match i {
            Instr::CallTraitMethod {
                trait_name,
                method_name,
                signature,
                args,
                ..
            } if trait_name == "Index" && method_name == "at" => {
                Some((signature.clone(), args.len()))
            }
            _ => None,
        })
        .expect("first must lower one Index::at dispatch");
    let (sig, arg_count) = call;
    assert_eq!(arg_count, 1, "Index::at takes one non-receiver arg");
    assert_eq!(
        sig.params.len(),
        1,
        "Index::at signature has one non-receiver param; got: {:?}",
        sig.params
    );
    // The single index param is `Self::Key` which resolves to i32 here.
    assert!(
        matches!(sig.params[0], hew_types::Ty::I32),
        "Index::at param[0] (index) must be i32 after assoc-binding substitution; got: {:?}",
        sig.params[0]
    );
    // Return type is `Self::Output` projected through `Output = i32`.
    assert!(
        matches!(sig.return_type, hew_types::Ty::I32),
        "Index<Output = i32>::at must return i32 after assoc-binding \
         substitution; got: {:?}",
        sig.return_type
    );
}

/// (3) Trait+method-unresolved fail-closed: a producer that lowers a
/// `CallDynMethod` whose substituted `FnSig` still carries a `Ty::Var`
/// must emit `MirDiagnosticKind::CallTraitMethodSignatureUnresolved`
/// and NOT push the `Instr::CallTraitMethod` (which would otherwise
/// reach codegen with a degenerate erased indirect-call type).
///
/// We exercise this by surgically poisoning the checker's
/// `dyn_trait_method_calls` side table entry between checking and
/// HIR lowering so the corresponding `HirExprKind::CallDynMethod`
/// inherits the poisoned signature. (No source program reaches this
/// state under normal type-checking; the test is a structural
/// backstop for the type-inference boundary per copilot-instructions
/// §3 / LESSONS `checker-output-boundary`.)
#[test]
fn stage_1_6_unresolved_signature_fails_closed_with_named_diagnostic() {
    use hew_mir::MirDiagnosticKind;
    use hew_types::ty::TypeVar;

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
    // Poison every dyn-trait method call signature with an unresolved
    // type variable in the return slot. This simulates a checker /
    // HIR boundary breakage where substitution did not finish.
    let mut poisoned = 0usize;
    for entry in tc_output.dyn_trait_method_calls.values_mut() {
        entry.signature.return_type = hew_types::Ty::Var(TypeVar(9_999));
        poisoned += 1;
    }
    assert!(
        poisoned > 0,
        "source must contain at least one dyn-trait method call \
         to exercise the fail-closed path; got {poisoned}"
    );
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let mir = lower_hir_module(&output.module);
    let fail_closed = mir.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::CallTraitMethodSignatureUnresolved {
                trait_name,
                method_name,
                ..
            } if trait_name == "Display" && method_name == "fmt"
        )
    });
    assert!(
        fail_closed,
        "unresolved Ty::Var in CallDynMethod signature must surface as \
         CallTraitMethodSignatureUnresolved; got: {:#?}",
        mir.diagnostics
    );
    // And the instruction must NOT have been pushed (codegen would
    // otherwise consume a degenerate erased signature).
    let instrs = all_instrs(&mir, "drive");
    let has_call = instrs
        .iter()
        .any(|i| matches!(i, Instr::CallTraitMethod { .. }));
    assert!(
        !has_call,
        "Instr::CallTraitMethod must be suppressed when its signature \
         is unresolved; got: {instrs:#?}"
    );
}

// W3.031 Stage 2 — vtable static registry tests.
//
// Stage 2 reifies every reachable `Instr::CoerceToDynTrait` into a
// deduplicated, deterministically-ordered registry exposed on
// `IrPipeline::dyn_vtable_registry`. Codegen (later stages) emits one
// LLVM private constant per entry and references it by the
// `mangle_dyn_vtable_symbol(vtable_id)` symbol name. These tests pin
// the contract end-to-end through the real pipeline so the producer
// surface (MIR builder, registry pass) and the consumer surface
// (codegen vtable emission) cannot drift on dedup, ordering, or symbol
// naming.

/// A single `T → dyn Trait` coercion site produces exactly one
/// registry entry whose payload mirrors the source `Instr` and whose
/// symbol follows `__hew_vtable_{vtable_id}`.
#[test]
fn single_coercion_yields_one_registry_entry() {
    let source = r"
        fn use_display(value: dyn Display) {}

        fn main() {
            use_display(42);
        }
    ";
    let p = pipeline(source);
    assert_eq!(
        p.dyn_vtable_registry.len(),
        1,
        "one coercion site → one registry entry; got: {:#?}",
        p.dyn_vtable_registry
    );
    let inst = &p.dyn_vtable_registry[0];
    assert_eq!(inst.vtable_id, 0);
    assert_eq!(inst.symbol, "__hew_vtable__Display__i64__0");
    assert_eq!(
        inst.symbol,
        hew_mir::mangle_dyn_vtable_symbol(0, "Display", &ResolvedTy::I64)
    );
    assert_eq!(inst.trait_name, "Display");
    assert_eq!(inst.concrete_type, ResolvedTy::I64);
    assert_eq!(
        inst.method_table,
        vec![("fmt".to_string(), "i64::fmt".to_string())]
    );
    assert_eq!(inst.vtable_entries.len(), 1);
    assert_eq!(inst.vtable_entries[0].trait_name, "Display");
    assert_eq!(inst.vtable_entries[0].method_name, "fmt");
    assert_eq!(inst.vtable_entries[0].impl_fn_key, "i64::fmt");
}

/// Two `T → dyn Trait` coercion sites of distinct concrete types,
/// reached from the same call site, produce two separate registry
/// entries with stable 0-based `vtable_id`s and distinct symbols. The
/// ordering is deterministic — stable sort by `(trait_name,
/// concrete_type display)`.
#[test]
fn distinct_concretes_for_same_trait_produce_two_registry_entries() {
    let source = r"
        fn use_display(value: dyn Display) {}

        fn main() {
            use_display(42);
            use_display(true);
        }
    ";
    let p = pipeline(source);
    assert_eq!(
        p.dyn_vtable_registry.len(),
        2,
        "two distinct concretes → two registry entries; got: {:#?}",
        p.dyn_vtable_registry
    );
    // `bool` < `i64` lexicographically, so the bool entry sorts first
    // and gets vtable_id 0.
    let inst0 = &p.dyn_vtable_registry[0];
    let inst1 = &p.dyn_vtable_registry[1];
    assert_eq!(inst0.vtable_id, 0);
    assert_eq!(inst0.symbol, "__hew_vtable__Display__bool__0");
    assert_eq!(inst0.trait_name, "Display");
    assert_eq!(inst0.concrete_type, ResolvedTy::Bool);
    assert_eq!(inst1.vtable_id, 1);
    assert_eq!(inst1.symbol, "__hew_vtable__Display__i64__1");
    assert_eq!(inst1.trait_name, "Display");
    assert_eq!(inst1.concrete_type, ResolvedTy::I64);
}

/// Two identical `T → dyn Trait` coercions reached from two different
/// functions collapse to a single registry entry — the dedup key is
/// the `(trait_name, concrete_type, vtable_entries)` triple, not the
/// site at which the coercion appears.
#[test]
fn identical_coercions_across_functions_dedupe_to_one_entry() {
    let source = r"
        fn use_display(value: dyn Display) {}

        fn f1() {
            use_display(42);
        }

        fn f2() {
            use_display(99);
        }

        fn main() {
            f1();
            f2();
        }
    ";
    let p = pipeline(source);
    assert_eq!(
        p.dyn_vtable_registry.len(),
        1,
        "same (trait, concrete) coerced from two functions → one entry; \
         got: {:#?}",
        p.dyn_vtable_registry
    );
    assert_eq!(p.dyn_vtable_registry[0].vtable_id, 0);
    assert_eq!(
        p.dyn_vtable_registry[0].symbol,
        "__hew_vtable__Display__i64__0"
    );
    assert_eq!(p.dyn_vtable_registry[0].concrete_type, ResolvedTy::I64);
}

/// A module with no `dyn Trait` usage produces an empty registry. The
/// field is always present so codegen + tests can index it uniformly.
#[test]
fn module_without_dyn_trait_has_empty_registry() {
    let source = r"
        fn main() {
            let _x: i64 = 7;
        }
    ";
    let p = pipeline(source);
    assert!(
        p.dyn_vtable_registry.is_empty(),
        "no dyn Trait coercions → empty registry; got: {:#?}",
        p.dyn_vtable_registry
    );
}

// Erased method thunk symbol mangling.
//
// Codegen synthesises one thunk per `(vtable_id, method_index)` pair
// from `dyn_vtable_registry`. Producer (codegen `emit_dyn_trait_thunks`)
// and consumer (vtable static initialiser) both name the LLVM
// function through `mangle_dyn_thunk_symbol`. The format
// `__hew_dyn_thunk__{trait}__{concrete}__{vtable_id}_{method_index}`
// carries uniqueness in the numeric tail and disambiguating
// trait/concrete substrings in the middle.

/// The mangling is uniquely determined by
/// `(vtable_id, method_index, trait, concrete)`. Pinning the exact
/// strings makes drift between producer (codegen) and consumer
/// (vtable static initializer) immediately observable.
#[test]
fn mangle_dyn_thunk_symbol_format_carries_trait_concrete_and_id() {
    assert_eq!(
        hew_mir::mangle_dyn_thunk_symbol(0, 0, "Display", &ResolvedTy::I64),
        "__hew_dyn_thunk__Display__i64__0_0"
    );
    assert_eq!(
        hew_mir::mangle_dyn_thunk_symbol(0, 1, "Display", &ResolvedTy::I64),
        "__hew_dyn_thunk__Display__i64__0_1"
    );
    assert_eq!(
        hew_mir::mangle_dyn_thunk_symbol(7, 3, "Counter", &ResolvedTy::Bool),
        "__hew_dyn_thunk__Counter__bool__7_3"
    );
    // Sanity: no overlap across the (vtable_id, method_index, trait,
    // concrete) plane — a future change to any coordinate would
    // surface as a duplicate here rather than as a silent symbol-
    // collision at link time.
    let names = [
        hew_mir::mangle_dyn_thunk_symbol(0, 0, "Display", &ResolvedTy::I64),
        hew_mir::mangle_dyn_thunk_symbol(0, 1, "Display", &ResolvedTy::I64),
        hew_mir::mangle_dyn_thunk_symbol(1, 0, "Display", &ResolvedTy::Bool),
        hew_mir::mangle_dyn_thunk_symbol(1, 1, "Display", &ResolvedTy::Bool),
    ];
    let unique: std::collections::HashSet<&str> = names.iter().map(String::as_str).collect();
    assert_eq!(unique.len(), names.len(), "thunk names must be unique");
}

/// Sanitisation: non-`[A-Za-z0-9_]` characters in the trait or
/// concrete name (e.g. `+`, `<`, `,`, spaces) are replaced with `_`
/// so the resulting LLVM/symbol-table identifier is always legal.
/// The `vtable_id` numeric suffix preserves uniqueness even when
/// two distinct source names sanitise to the same substring.
#[test]
fn mangle_helpers_sanitize_non_identifier_characters() {
    let composite = ResolvedTy::Named {
        name: "Vec<i64>".into(),
        args: vec![],
        builtin: None,
        is_opaque: false,
    };
    let sym = hew_mir::mangle_dyn_vtable_symbol(0, "Trait1+Trait2", &composite);
    assert!(
        !sym.contains('+') && !sym.contains('<') && !sym.contains('>'),
        "sanitisation must strip non-identifier characters; got: {sym}"
    );
    assert!(
        sym.ends_with("__0"),
        "vtable_id suffix must be preserved; got: {sym}"
    );
}

/// Thunk symbols are disjoint from vtable static symbols. Both use
/// the `__hew_` prefix, but the `_dyn_thunk__` vs `_vtable__` infix
/// segregates the namespaces so codegen can emit both kinds into the
/// same LLVM module without collision.
#[test]
fn dyn_thunk_symbols_do_not_collide_with_vtable_static_symbols() {
    for id in 0..4u32 {
        let vt = hew_mir::mangle_dyn_vtable_symbol(id, "Display", &ResolvedTy::I64);
        for m in 0..4u32 {
            let thunk = hew_mir::mangle_dyn_thunk_symbol(id, m, "Display", &ResolvedTy::I64);
            assert_ne!(
                vt, thunk,
                "vtable static symbol must not collide with any thunk in the same vtable"
            );
        }
    }
}
