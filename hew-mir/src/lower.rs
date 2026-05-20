use std::collections::{HashMap, HashSet};

use hew_hir::stdlib_catalog;
use hew_hir::{
    named_type_names, BindingId, HirActorDecl, HirBinding, HirBlock, HirExpr, HirExprKind, HirFn,
    HirItem, HirLifecycleHookKind, HirLiteral, HirModule, HirNodeId, HirSelect, HirSelectArmKind,
    HirStmt, HirStmtKind, HirSupervisorChild, HirSupervisorDecl, IntentKind, ResolvedRef, ScopeId,
    SiteId, ValueClass,
};
use hew_parser::ast::BinaryOp;
use hew_types::{ChildKind, ChildSlot, ExecutionContextReader, ResolvedTy};

use crate::dataflow;
use crate::model::{
    ActorHandlerLayout, ActorLayout, BasicBlock, BlockKind, CheckedMirFunction, CmpPred,
    DecisionFact, DropKind, DropPlan, ElabBlock, ElabDrop, ElaboratedMirFunction, ExitPath,
    FieldOffset, FloatWidth, Instr, IntArithOp, IntSignedness, IrPipeline, LambdaCapture, MirCheck,
    MirDiagnostic, MirDiagnosticKind, MirStatement, Place, RawMirFunction, SelectArm,
    SelectArmKind, Strategy, Terminator, ThirFunction, TrapKind,
};

const HEW_CTX_OFFSET_ACTOR_ID: usize = 8;
const HEW_CTX_OFFSET_PARENT_SUPERVISOR: usize = 16;
const HEW_CTX_OFFSET_TRACE: usize = 56;
const HEW_TRACE_OFFSET_SPAN_ID: usize = 16;
const HEW_CTX_OFFSET_TRACE_SPAN: usize = HEW_CTX_OFFSET_TRACE + HEW_TRACE_OFFSET_SPAN_ID;

/// Synthetic MIR record type name for the `ChildLookupResult` C-ABI struct
/// returned by `hew_supervisor_child_get`. Registered unconditionally in every
/// module so the `FieldAccess` intercept arm can use `RecordFieldLoad` on the
/// struct-return place. S3 codegen recognises this name and emits the correct
/// LLVM struct ABI (`{ i8, i8, [6 x i8], ptr }` at the wire level).
///
/// WHY a synthetic name rather than a user-visible record: `ChildLookupResult`
/// is a runtime-internal type; user programs never name or construct it directly.
/// The double-underscore prefix (`__`) is outside the user-identifier namespace.
const CHILD_LOOKUP_RESULT_TY_NAME: &str = "__HewChildLookupResult";

/// Sentinel HIR IDs for the synthetic `__crash_code: i64` ABI binding injected
/// into `#[on(crash)]` handler prologues.
///
/// Checker-allocated IDs count upward from 0; these live at `u32::MAX` to avoid
/// collision with any real binding, site, or node emitted during type-checking.
/// One sentinel value per kind suffices because the injected binding is local to
/// the synthetic function scope and is never referenced outside it.
const SENTINEL_CRASH_CODE_BINDING: BindingId = BindingId(u32::MAX);
const SENTINEL_CRASH_CODE_SITE: SiteId = SiteId(u32::MAX);
const SENTINEL_CRASH_CODE_NODE: HirNodeId = HirNodeId(u32::MAX);

#[derive(Debug, Clone)]
struct ActorMethodInfo {
    msg_type: i32,
    param_tys: Vec<ResolvedTy>,
    return_ty: ResolvedTy,
}

fn context_reader_offset(reader: ExecutionContextReader) -> usize {
    match reader {
        ExecutionContextReader::ActorId => HEW_CTX_OFFSET_ACTOR_ID,
        ExecutionContextReader::Supervisor => HEW_CTX_OFFSET_PARENT_SUPERVISOR,
        ExecutionContextReader::TraceSpan => HEW_CTX_OFFSET_TRACE_SPAN,
    }
}

/// Classify a resolved integer type as signed or unsigned. Returns
/// `None` for non-integer types — callers that demand an integer
/// signedness (the B-2 overflow-trap lowering) fail closed when this
/// returns `None`. Platform-sized `Isize` / `Usize` are canonicalised
/// to their pointer-width LLVM type by codegen; here we only need the
/// signedness discriminator so the intrinsic family selection is
/// correct regardless of pointer width.
fn integer_signedness(ty: &ResolvedTy) -> Option<IntSignedness> {
    match ty {
        ResolvedTy::I8
        | ResolvedTy::I16
        | ResolvedTy::I32
        | ResolvedTy::I64
        | ResolvedTy::Isize => Some(IntSignedness::Signed),
        ResolvedTy::U8
        | ResolvedTy::U16
        | ResolvedTy::U32
        | ResolvedTy::U64
        | ResolvedTy::Usize => Some(IntSignedness::Unsigned),
        _ => None,
    }
}

/// Return the statically known bit-width for a concrete integer type.
///
/// `Isize` / `Usize` are platform-sized (32-bit on WASM32, 64-bit on
/// native) — their width is NOT knowable at MIR construction time.
/// Returns `None` for platform-sized types and all non-integer types.
/// Callers that require a static width (shift-range check, signed-MIN
/// constant emission) must fail-closed (`NotYetImplemented`) when this
/// returns `None`.
///
/// WHY-ISIZE-NONE: the shift-range bound `(count as unsigned) >= W`
/// requires `W` to be a compile-time constant in the generated
/// `Instr::ConstI64`. On `isize`/`usize` the correct constant is
/// target-dependent (32 vs 64); emitting the wrong constant would
/// silently admit out-of-range shifts on one target. Fail-closed is
/// the right answer for the v0.5 integer spine.
/// WHEN-OBSOLETE: when MIR carries target-info (pointer-width in
/// `IrPipeline` or a `TargetSpec` passed to the builder), re-wire to
/// emit the correct per-target constant and remove this `None` arm.
fn integer_bit_width(ty: &ResolvedTy) -> Option<i64> {
    match ty {
        ResolvedTy::I8 | ResolvedTy::U8 => Some(8),
        ResolvedTy::I16 | ResolvedTy::U16 => Some(16),
        ResolvedTy::I32 | ResolvedTy::U32 => Some(32),
        ResolvedTy::I64 | ResolvedTy::U64 => Some(64),
        // Isize / Usize are platform-sized (see doc comment) and all
        // other types are non-integer — both arms return None.
        _ => None,
    }
}

/// Classify a resolved type as a float width. Returns `None` for
/// non-float types. Used to dispatch float arithmetic lowering in
/// `lower_binary` and `lower_div_rem` before falling through to the
/// integer-only `IntArithChecked` / `lower_div_rem` paths.
fn float_width(ty: &ResolvedTy) -> Option<FloatWidth> {
    match ty {
        ResolvedTy::F32 => Some(FloatWidth::F32),
        ResolvedTy::F64 => Some(FloatWidth::F64),
        _ => None,
    }
}

/// Return the signed minimum value for a concrete signed integer type
/// as an `i64`. Used to emit the `lhs == iN::MIN` constant in the
/// signed-MIN/-1 trap check for `/` and `%`.
///
/// Returns `None` for unsigned types, `Isize` (platform-sized), and
/// all non-integer types. Callers must fail-closed when this returns
/// `None`.
fn signed_min_value(ty: &ResolvedTy) -> Option<i64> {
    match ty {
        ResolvedTy::I8 => Some(i64::from(i8::MIN)),
        ResolvedTy::I16 => Some(i64::from(i16::MIN)),
        ResolvedTy::I32 => Some(i64::from(i32::MIN)),
        ResolvedTy::I64 => Some(i64::MIN),
        // Isize: platform-sized, not knowable at MIR time.
        // Unsigned types: no MIN check needed.
        _ => None,
    }
}

fn actor_name_from_handle_ty(ty: &ResolvedTy) -> Option<&str> {
    match ty {
        ResolvedTy::Named { name, args }
            if matches!(name.as_str(), "LocalPid" | "ActorRef" | "Actor") && args.len() == 1 =>
        {
            match &args[0] {
                ResolvedTy::Named { name, args } if args.is_empty() => Some(name.as_str()),
                _ => None,
            }
        }
        _ => None,
    }
}

fn method_name_from_id(method_id: &str) -> &str {
    method_id.rsplit("::").next().unwrap_or(method_id)
}

fn is_self_expr(expr: &HirExpr) -> bool {
    matches!(
        &expr.kind,
        HirExprKind::BindingRef {
            name,
            resolved: ResolvedRef::Unresolved | ResolvedRef::Binding(_) | ResolvedRef::Item(_)
        } if name == "self"
    )
}

/// Run Checked MIR's legality passes over a function's statement
/// stream. Two real passes ship today (use-after-consume,
/// initialised-before-use); the aliasing, generator-borrow-across-
/// yield, and actor-send-escape variants are declared on `MirCheck`
/// but have no construction surface in the v0.5 integer spine yet
/// (no borrow ops in `Instr`, no projection variants on `Place`, no
/// construction site for `Terminator::Yield` / `Terminator::Send`).
/// The `MirCheck::DecisionMapTotal` invariant fires if any
/// `DecisionFact` in this function carries `Strategy::UnknownBlocked`.
///
/// Delegates to `dataflow::analyze` which runs the four-state lattice
/// (`Uninit / Live / Consumed / MaybeConsumed`) over the CFG's basic
/// blocks via a forward fixpoint. Per-block transfer emits
/// `InitialisedBeforeUse` on `Uninit` reads and `UseAfterConsume` on
/// `Consumed`/`MaybeConsumed` reads; the inter-block meet rule is
/// `Uninit ⊔ X = Uninit` (most-conservative). `If`-lowering (Slice 2)
/// produces `Branch` + two arm blocks + a join block, so the
/// path-sensitive cases that a flat-stream scan would mishandle
/// (false-positive on mutually-exclusive `consume` arms; false-negative
/// for a binding consumed on only one path) are handled correctly by
/// the per-block fixpoint. LESSONS: `boundary-fail-closed` — verify
/// the substrate is path-sensitive before relying on it for linear
/// safety, and mandate property tests on meet rules before landing.
fn check_function(
    builder: &Builder,
    blocks: &[BasicBlock],
    func: &HirFn,
) -> dataflow::DataflowResult {
    // Collect the BindingId of each parameter so the dataflow checker can
    // pre-seed them as `Live` at function entry.  Parameters are initialised
    // by the calling convention (LLVM function argument + parameter prologue
    // in codegen), never by a `Bind` statement in the checker-authority stream.
    let param_ids: Vec<hew_hir::BindingId> = func.params.iter().map(|p| p.id).collect();
    let mut result = dataflow::analyze(blocks, &builder.type_classes, &param_ids);
    let checks = &mut result.checks;

    // DecisionMapTotal. Every `DecisionFact` on this function must
    // carry a concrete `Strategy` — `Strategy::UnknownBlocked` is a
    // lowering escape valve that must never reach the emitter. This
    // pass is independent of the per-block dataflow.
    let offending: Vec<_> = builder
        .decisions
        .iter()
        .filter(|d| d.strategy == Strategy::UnknownBlocked)
        .map(|d| d.site)
        .collect();
    if !offending.is_empty() {
        checks.push(MirCheck::DecisionMapTotal {
            offending_sites: offending,
        });
    }

    // Aliasing, GeneratorBorrowAcrossYield, and ActorSendEscape have
    // no construction surface in the v0.5 integer spine: `Place` has
    // no projection variants, `Instr` has no borrow ops, and
    // `Terminator::Yield` / `Terminator::Send` are declared but never
    // built. The passes are no-ops on the current IR; they populate
    // when the construction surface for borrows, generators, and
    // actor sends lands.

    result
}

#[must_use]
#[allow(
    clippy::too_many_lines,
    reason = "two-phase orchestration: per-fn lowering + per-monomorphisation lowering live \
              in the same function so the producers share record_field_orders, type_classes, \
              and module_fn_names construction"
)]
pub fn lower_hir_module(module: &HirModule) -> IrPipeline {
    let mut thir = Vec::new();
    let mut raw_mir = Vec::new();
    let mut checked_mir = Vec::new();
    let mut elaborated_mir = Vec::new();
    let mut diagnostics = Vec::new();

    // Build the declaration-order field descriptor table once for the whole module.
    // Keys are record type names; values are (field_name, field_ty) pairs in
    // declaration order. Used by StructInit and FieldAccess lowering to resolve
    // a field name to its 0-based FieldOffset and to look up field types for
    // intermediate place allocation during functional-update desugaring. Tuple
    // records have an empty field list and their constructor is a Call, not a
    // StructInit, so they never appear here.
    let mut record_field_orders: HashMap<String, Vec<(String, ResolvedTy)>> = HashMap::new();
    let mut record_layouts: Vec<crate::model::RecordLayout> = Vec::new();
    let mut actor_layouts: Vec<crate::model::ActorLayout> = Vec::new();
    let mut supervisor_layouts: Vec<crate::model::SupervisorLayout> = Vec::new();
    for item in &module.items {
        match item {
            HirItem::Record(decl) => {
                // Generic record decls (`record Box<T> { ... }`) emit zero
                // bare-name layouts: their per-instantiation layouts come
                // from `module.record_layouts` under mangled names. Only
                // monomorphic records register a bare-name field order.
                if !decl.type_params.is_empty() {
                    continue;
                }
                let fields: Vec<(String, ResolvedTy)> = decl
                    .fields
                    .iter()
                    .map(|f| (f.name.clone(), f.ty.clone()))
                    .collect();
                // Named-form records have a non-empty `fields` list;
                // tuple-form records have an empty list (their positional
                // layout lives on the parser's `RecordKind::Tuple`
                // discriminator and is not promoted into HIR fields). Tuple
                // records construct via `Expr::Call`, never via
                // `StructInit`, so they need no layout descriptor in this
                // slice — codegen will fail-closed on any
                // `ResolvedTy::Named` reach-through that names a tuple
                // record.
                if !fields.is_empty() {
                    record_layouts.push(crate::model::RecordLayout {
                        name: decl.name.clone(),
                        field_tys: fields.iter().map(|(_, ty)| ty.clone()).collect(),
                    });
                }
                record_field_orders.insert(decl.name.clone(), fields);
            }
            HirItem::TypeDecl(decl) => {
                // `pub type Foo { ... }` and `pub type Foo<T> { ... }` are
                // structurally the named-record substrate that
                // `Expr::StructInit` targets. Generic `pub type Foo<T>`
                // declarations emit zero bare-name layouts (same rule as
                // generic records); their per-instantiation layouts come
                // from `module.record_layouts` under mangled names.
                if !decl.type_params.is_empty() {
                    continue;
                }
                let fields: Vec<(String, ResolvedTy)> = decl
                    .fields
                    .iter()
                    .map(|f| (f.name.clone(), f.ty.clone()))
                    .collect();
                if !fields.is_empty() {
                    record_layouts.push(crate::model::RecordLayout {
                        name: decl.name.clone(),
                        field_tys: fields.iter().map(|(_, ty)| ty.clone()).collect(),
                    });
                    record_field_orders.insert(decl.name.clone(), fields);
                }
            }
            HirItem::Actor(actor) => {
                if !actor.state_fields.is_empty() {
                    record_layouts.push(crate::model::RecordLayout {
                        name: actor.name.clone(),
                        field_tys: actor
                            .state_fields
                            .iter()
                            .map(|field| field.ty.clone())
                            .collect(),
                    });
                }
                actor_layouts.push(crate::model::ActorLayout {
                    name: actor.name.clone(),
                    state_field_names: actor
                        .state_fields
                        .iter()
                        .map(|field| field.name.clone())
                        .collect(),
                    state_field_tys: actor
                        .state_fields
                        .iter()
                        .map(|field| field.ty.clone())
                        .collect(),
                    init_param_names: actor
                        .init
                        .as_ref()
                        .map(|init| init.params.iter().map(|param| param.name.clone()).collect())
                        .unwrap_or_default(),
                    init_param_tys: actor
                        .init
                        .as_ref()
                        .map(|init| init.params.iter().map(|param| param.ty.clone()).collect())
                        .unwrap_or_default(),
                    init_symbol: actor
                        .init
                        .as_ref()
                        .map(|_| mangle_actor_init_handler(&actor.name)),
                    on_start_symbol: actor
                        .lifecycle_hooks
                        .iter()
                        .find(|hook| hook.kind == HirLifecycleHookKind::Start)
                        .map(|_| mangle_actor_start_handler(&actor.name)),
                    on_stop_symbols: actor
                        .lifecycle_hooks
                        .iter()
                        .enumerate()
                        .filter(|(_, hook)| hook.kind == HirLifecycleHookKind::Stop)
                        .map(|(idx, _)| mangle_actor_stop_handler_indexed(&actor.name, idx))
                        .collect(),
                    on_crash_symbol: actor
                        .lifecycle_hooks
                        .iter()
                        .find(|hook| hook.kind == HirLifecycleHookKind::Crash)
                        .map(|_| mangle_actor_crash_handler(&actor.name)),
                    max_heap_bytes: actor.max_heap_bytes,
                    handlers: lower_actor_handler_layouts(actor),
                });
            }
            HirItem::Supervisor(sup) => {
                // Supervisors compile to a layout (consumed by codegen for
                // the per-supervisor restart/registration table) plus a
                // bootstrap function (emitted in the second-pass loop below).
                // The layout's `children` vector is ordered by topological
                // spawn order so dependents observe their `wired_to:` deps
                // already spawned.
                if let Some(layout) = build_supervisor_layout(sup, &mut diagnostics) {
                    supervisor_layouts.push(layout);
                }
            }
            _ => {}
        }
    }

    // Emit one MIR `RecordLayout` per HIR `record_layouts` entry under the
    // mangled symbol name. The HIR registry has already substituted the
    // type-parameter symbols with concrete `ResolvedTy`s, so the MIR layer
    // can read the field list verbatim. Insertion-ordered to keep codegen
    // deterministic; per-instantiation field_orders are keyed by the
    // mangled name so `StructInit` / `FieldAccess` lowering can find them
    // by mangling `(record_name, type_args)` from the expression's type.
    for layout in &module.record_layouts {
        let fields: Vec<(String, ResolvedTy)> = layout.fields.clone();
        record_layouts.push(crate::model::RecordLayout {
            name: layout.mangled_name.clone(),
            field_tys: fields.iter().map(|(_, ty)| ty.clone()).collect(),
        });
        record_field_orders.insert(layout.mangled_name.clone(), fields);
    }
    // Register the synthetic `__HewChildLookupResult` record layout used by the
    // `FieldAccess` supervisor intercept arm (S2). The runtime struct is:
    //   { u8 tag, u8 reason, [u8;6] _pad, *mut HewActor handle }  (16 bytes, C ABI)
    // For MIR purposes we flatten this to two fields that S3 codegen maps to
    // `extractvalue` indices on the struct-return LLVM value:
    //   field 0 "tag"    : i64  (zero-extended from u8; tag 0=Live, 1=Transient, 2=Dead)
    //   field 1 "handle" : i64  (pointer-width integer; cast to actor pointer at codegen)
    // Using i64 for both avoids introducing a pointer-or-u8 type into MIR.
    // S3 emits the correct LLVM ABI types (i8 and ptr) when it lowers the
    // `CallRuntimeAbi`+`RecordFieldLoad` sequence into LLVM IR.
    //
    // Decision: option (b) from the plan — scratch-alloca + RecordFieldLoad.
    // WHY: reusing existing `CallRuntimeAbi` (dest = struct local) and
    //   `RecordFieldLoad` avoids any new `Instr` variant and keeps the S3
    //   match-arm cascade at zero lines for S2.
    // WHEN obsolete: when S3 wires LLVM emission for `hew_supervisor_child_get`.
    // WHAT: S3 interprets `CallRuntimeAbi` whose `dest` local is typed
    //   `__HewChildLookupResult` as a struct-return call and emits `extractvalue`.
    let child_lookup_fields: Vec<(String, ResolvedTy)> = vec![
        ("tag".to_string(), ResolvedTy::I64),
        ("handle".to_string(), ResolvedTy::I64),
    ];
    record_layouts.push(crate::model::RecordLayout {
        name: CHILD_LOOKUP_RESULT_TY_NAME.to_string(),
        field_tys: child_lookup_fields
            .iter()
            .map(|(_, ty)| ty.clone())
            .collect(),
    });
    record_field_orders.insert(CHILD_LOOKUP_RESULT_TY_NAME.to_string(), child_lookup_fields);

    // `PanicInfo` — the argument type of `#[on(crash)]` hooks in std/failure.hew.
    //
    // WHY registered here unconditionally: user programs do not `import std.failure`
    // explicitly; `PanicInfo` is seeded into the HIR TypeClassTable via
    // `builtin_type_classes` and into the checker's `known_types` via
    // `register_builtin_failure_surface`. Neither path inserts a `HirItem::TypeDecl`
    // into `module.items`, so the normal item-loop above never sees it.
    //
    // The on_crash MIR prologue injector (in `lower_lifecycle_hooks`) synthesises a
    // `HirExprKind::StructInit { name: "PanicInfo", fields: [("code", __crash_code)] }`
    // expression and prepends it to the handler body. That StructInit lowering looks up
    // "PanicInfo" in `record_field_orders`; without this unconditional registration the
    // lookup fails for any program that does not import std.failure explicitly.
    //
    // WHEN-OBSOLETE: when std/failure.hew is loaded through the module graph and emits
    // a `HirItem::TypeDecl` that the item-loop above already handles; at that point
    // this sentinel insert produces a harmless duplicate that the later module-graph
    // path overwrites with the same value.
    //
    // WHAT-REAL-SOLUTION: module-graph loading of std/*.hew so all stdlib types enter
    // the HIR item stream naturally.
    if !record_field_orders.contains_key("PanicInfo") {
        let panic_info_fields: Vec<(String, ResolvedTy)> =
            vec![("code".to_string(), ResolvedTy::I64)];
        record_layouts.push(crate::model::RecordLayout {
            name: "PanicInfo".to_string(),
            field_tys: vec![ResolvedTy::I64],
        });
        record_field_orders.insert("PanicInfo".to_string(), panic_info_fields);
    }

    let actor_layout_map: HashMap<String, ActorLayout> = actor_layouts
        .iter()
        .cloned()
        .map(|layout| (layout.name.clone(), layout))
        .collect();

    // Post-loop pass: populate on_crash_symbol and max_heap_bytes on each
    // SupervisorChildLayout using the now-complete actor_layout_map.
    // build_supervisor_layout runs inside the single-pass item loop, so actor
    // declarations that follow a supervisor in source order would not have been
    // visible yet. Deferring the lookup here makes ordering irrelevant.
    for sup_layout in &mut supervisor_layouts {
        for child in &mut sup_layout.children {
            let al = actor_layout_map.get(&child.actor_name);
            child.on_crash_symbol = al.and_then(|al| al.on_crash_symbol.clone());
            child.max_heap_bytes = al.and_then(|al| al.max_heap_bytes);
        }
    }

    let supervisor_layout_map: HashMap<String, crate::model::SupervisorLayout> = supervisor_layouts
        .iter()
        .cloned()
        .map(|layout| (layout.name.clone(), layout))
        .collect();

    // Collect the names every user-defined function will use as its
    // emitted MIR symbol. For non-generic functions this is the
    // source-declared name. For generic functions this is the set of
    // mangled per-monomorphisation names; the unspecialised generic
    // body is not emitted (it would carry symbolic type-parameter
    // names through to LLVM and could not be linked).
    //
    // The Call lowering arm dispatches on this set: a callee name in
    // this set → `Terminator::Call`; otherwise the runtime-ABI/indirect
    // fail-closed paths apply.
    let mut module_fn_names: HashSet<String> = HashSet::new();
    for entry in stdlib_catalog::entries() {
        if !matches!(
            entry.linkage,
            stdlib_catalog::BuiltinLinkage::CompilerIntrinsic { .. }
        ) {
            module_fn_names.insert(entry.name.to_string());
        }
    }
    for item in &module.items {
        if let HirItem::Function(f) = item {
            if f.type_params.is_empty() {
                module_fn_names.insert(f.name.clone());
            }
        }
    }
    for mono in &module.monomorphisations {
        module_fn_names.insert(mono.mangled_name.clone());
    }
    let mut emitted_actor_handler_symbols: HashMap<String, String> = module_fn_names
        .iter()
        .map(|name| (name.clone(), format!("function `{name}`")))
        .collect();

    // Build origin lookup: ItemId → &HirFn. Each monomorphisation's
    // `key.origin` resolves to the generic origin fn whose body is
    // lowered under per-monomorphisation substitution.
    let mut origin_fns: HashMap<hew_hir::ItemId, &HirFn> = HashMap::new();
    for item in &module.items {
        if let HirItem::Function(f) = item {
            origin_fns.insert(f.id, f);
        }
    }

    for item in &module.items {
        match item {
            HirItem::Function(func) => {
                // Skip unspecialised generic origins. Their bodies are
                // emitted via the monomorphisations loop below, once
                // per concrete instantiation.
                if !func.type_params.is_empty() {
                    continue;
                }
                let lowered = lower_function(
                    func,
                    func.name.clone(),
                    HashMap::new(),
                    &module.type_classes,
                    &record_field_orders,
                    &actor_layout_map,
                    &supervisor_layout_map,
                    None,
                    &module_fn_names,
                    &module.call_site_type_args,
                    &module.supervisor_child_slots,
                    crate::model::FunctionCallConv::Default,
                );
                thir.push(lowered.thir);
                raw_mir.push(lowered.raw);
                checked_mir.push(lowered.checked);
                elaborated_mir.push(lowered.elaborated);
                record_layouts.extend(lowered.record_layouts);
                for generated in lowered.generated {
                    thir.push(generated.thir);
                    raw_mir.push(generated.raw);
                    checked_mir.push(generated.checked);
                    elaborated_mir.push(generated.elaborated);
                    diagnostics.extend(generated.diagnostics);
                    record_layouts.extend(generated.record_layouts);
                }
                diagnostics.extend(lowered.diagnostics);
            }
            HirItem::Actor(actor) => {
                let lowered_handlers = lower_actor_body_handlers(
                    actor,
                    &module.type_classes,
                    &record_field_orders,
                    &actor_layout_map,
                    &module_fn_names,
                    &module.call_site_type_args,
                    &module.supervisor_child_slots,
                    &mut emitted_actor_handler_symbols,
                    &mut diagnostics,
                );
                for lowered in lowered_handlers {
                    thir.push(lowered.thir);
                    raw_mir.push(lowered.raw);
                    checked_mir.push(lowered.checked);
                    elaborated_mir.push(lowered.elaborated);
                    record_layouts.extend(lowered.record_layouts);
                    for generated in lowered.generated {
                        thir.push(generated.thir);
                        raw_mir.push(generated.raw);
                        checked_mir.push(generated.checked);
                        elaborated_mir.push(generated.elaborated);
                        diagnostics.extend(generated.diagnostics);
                        record_layouts.extend(generated.record_layouts);
                    }
                    diagnostics.extend(lowered.diagnostics);
                }
            }
            HirItem::Supervisor(sup) => {
                if let Some(lowered) = lower_supervisor_bootstrap(
                    sup,
                    &supervisor_layout_map,
                    &module.type_classes,
                    &record_field_orders,
                    &actor_layout_map,
                    &module_fn_names,
                    &module.call_site_type_args,
                    &module.supervisor_child_slots,
                    &mut emitted_actor_handler_symbols,
                    &mut diagnostics,
                ) {
                    thir.push(lowered.thir);
                    raw_mir.push(lowered.raw);
                    checked_mir.push(lowered.checked);
                    elaborated_mir.push(lowered.elaborated);
                    record_layouts.extend(lowered.record_layouts);
                    for generated in lowered.generated {
                        thir.push(generated.thir);
                        raw_mir.push(generated.raw);
                        checked_mir.push(generated.checked);
                        elaborated_mir.push(generated.elaborated);
                        diagnostics.extend(generated.diagnostics);
                        record_layouts.extend(generated.record_layouts);
                    }
                    diagnostics.extend(lowered.diagnostics);
                }
            }
            HirItem::Record(_) | HirItem::TypeDecl(_) | HirItem::Machine(_) => {
                // Type declarations and Lane A machine declarations have no
                // executable MIR bodies in S-A. TypeDecl markers are consumed
                // via `HirModule.type_classes`; machine codegen is Lane B.
            }
        }
    }

    // Emit one MIR function per monomorphisation. Each instantiation
    // re-lowers the generic origin's body with a substitution map
    // mapping origin type-parameter symbols to the concrete arg types
    // declared by the MonoKey. The emitted function name is the
    // mangled symbol the registry assigned.
    for mono in &module.monomorphisations {
        let Some(origin) = origin_fns.get(&mono.key.origin).copied() else {
            // Origin fn missing from module.items. The registry was
            // populated against a fn that does not appear here —
            // fail-closed by emitting a diagnostic and skipping.
            diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::UnsupportedNode {
                    reason: format!(
                        "monomorphisation `{}` references unknown origin fn id {:?}",
                        mono.mangled_name, mono.key.origin
                    ),
                },
                note: "the HIR monomorphisation registry referenced an origin \
                       ItemId not present in module.items"
                    .to_string(),
            });
            continue;
        };
        // Substitution map: type-parameter name → concrete ResolvedTy.
        let subst: HashMap<String, ResolvedTy> = origin
            .type_params
            .iter()
            .cloned()
            .zip(mono.key.type_args.iter().cloned())
            .collect();
        let lowered = lower_function(
            origin,
            mono.mangled_name.clone(),
            subst,
            &module.type_classes,
            &record_field_orders,
            &actor_layout_map,
            &supervisor_layout_map,
            None,
            &module_fn_names,
            &module.call_site_type_args,
            &module.supervisor_child_slots,
            crate::model::FunctionCallConv::Default,
        );
        thir.push(lowered.thir);
        raw_mir.push(lowered.raw);
        checked_mir.push(lowered.checked);
        elaborated_mir.push(lowered.elaborated);
        record_layouts.extend(lowered.record_layouts);
        for generated in lowered.generated {
            thir.push(generated.thir);
            raw_mir.push(generated.raw);
            checked_mir.push(generated.checked);
            elaborated_mir.push(generated.elaborated);
            diagnostics.extend(generated.diagnostics);
            record_layouts.extend(generated.record_layouts);
        }
        diagnostics.extend(lowered.diagnostics);
    }

    IrPipeline {
        thir,
        raw_mir,
        checked_mir,
        elaborated_mir,
        diagnostics,
        record_layouts,
        actor_layouts,
        supervisor_layouts,
    }
}

#[allow(
    clippy::too_many_arguments,
    reason = "actor receive lowering threads the same module tables as regular function lowering"
)]
fn lower_actor_receive_handlers(
    actor: &HirActorDecl,
    type_classes: &hew_hir::TypeClassTable,
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    actor_layouts: &HashMap<String, ActorLayout>,
    module_fn_names: &HashSet<String>,
    call_site_type_args: &HashMap<hew_hir::SiteId, Vec<ResolvedTy>>,
    supervisor_child_slots: &HashMap<hew_hir::SiteId, ChildSlot>,
    emitted_symbols: &mut HashMap<String, String>,
    diagnostics: &mut Vec<MirDiagnostic>,
) -> Vec<LoweredFunction> {
    let state_fields: HashSet<String> = actor
        .state_fields
        .iter()
        .map(|field| field.name.clone())
        .collect();
    let mut lowered = Vec::new();

    for handler in &actor.receive_handlers {
        if handler.is_generator {
            diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::UnsupportedNode {
                    reason: "actor receive fn declared as generator; generator MIR lowering is a separate lane"
                        .to_string(),
                },
                note: format!(
                    "actor `{}` receive fn `{}` is a generator and is skipped by ActorHandler MIR lowering",
                    actor.name, handler.name
                ),
            });
            continue;
        }

        let self_field_errors = unknown_self_fields_in_block(&handler.body, &state_fields);
        if !self_field_errors.is_empty() {
            for field in self_field_errors {
                diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::UnknownActorStateField {
                        actor: actor.name.clone(),
                        field: field.clone(),
                    },
                    note: format!(
                        "actor `{}` receive fn `{}` references unknown state field `self.{field}`",
                        actor.name, handler.name
                    ),
                });
            }
            continue;
        }

        let emit_name = mangle_actor_receive_handler(&actor.name, &handler.name);
        let duplicate_label = format!("actor `{}` receive fn `{}`", actor.name, handler.name);
        if let Some(existing) = emitted_symbols.get(&emit_name) {
            diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::ActorHandlerSymbolCollision {
                    symbol: emit_name,
                    existing: existing.clone(),
                    duplicate: duplicate_label,
                },
                note:
                    "actor receive handler symbol mangling must be one-to-one before MIR emission"
                        .to_string(),
            });
            continue;
        }
        emitted_symbols.insert(emit_name.clone(), duplicate_label);

        let synthetic_fn = HirFn {
            id: actor.id,
            node: actor.node,
            name: format!("{}::{}", actor.name, handler.name),
            type_params: Vec::new(),
            params: handler.params.clone(),
            return_ty: handler.return_ty.clone(),
            body: handler.body.clone(),
            span: handler.span.clone(),
        };
        lowered.push(lower_function(
            &synthetic_fn,
            emit_name,
            HashMap::new(),
            type_classes,
            record_field_orders,
            actor_layouts,
            &HashMap::new(),
            Some(&actor.name),
            module_fn_names,
            call_site_type_args,
            supervisor_child_slots,
            crate::model::FunctionCallConv::ActorHandler,
        ));
    }

    lowered
}

#[allow(
    clippy::too_many_arguments,
    reason = "actor body lowering threads the same module tables as regular function lowering"
)]
fn lower_actor_body_handlers(
    actor: &HirActorDecl,
    type_classes: &hew_hir::TypeClassTable,
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    actor_layouts: &HashMap<String, ActorLayout>,
    module_fn_names: &HashSet<String>,
    call_site_type_args: &HashMap<hew_hir::SiteId, Vec<ResolvedTy>>,
    supervisor_child_slots: &HashMap<hew_hir::SiteId, ChildSlot>,
    emitted_symbols: &mut HashMap<String, String>,
    diagnostics: &mut Vec<MirDiagnostic>,
) -> Vec<LoweredFunction> {
    let mut lowered = Vec::new();
    if let Some(init) = &actor.init {
        if let Some(func) = lower_actor_init_handler(
            actor,
            init,
            type_classes,
            record_field_orders,
            actor_layouts,
            module_fn_names,
            call_site_type_args,
            supervisor_child_slots,
            emitted_symbols,
            diagnostics,
        ) {
            lowered.push(func);
        }
    }
    lowered.extend(lower_actor_lifecycle_handlers(
        actor,
        type_classes,
        record_field_orders,
        actor_layouts,
        module_fn_names,
        call_site_type_args,
        supervisor_child_slots,
        emitted_symbols,
        diagnostics,
    ));
    lowered.extend(lower_actor_receive_handlers(
        actor,
        type_classes,
        record_field_orders,
        actor_layouts,
        module_fn_names,
        call_site_type_args,
        supervisor_child_slots,
        emitted_symbols,
        diagnostics,
    ));
    lowered
}

#[allow(
    clippy::too_many_arguments,
    reason = "actor init lowering threads the same module tables as regular function lowering"
)]
fn lower_actor_init_handler(
    actor: &HirActorDecl,
    init: &hew_hir::HirActorInit,
    type_classes: &hew_hir::TypeClassTable,
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    actor_layouts: &HashMap<String, ActorLayout>,
    module_fn_names: &HashSet<String>,
    call_site_type_args: &HashMap<hew_hir::SiteId, Vec<ResolvedTy>>,
    supervisor_child_slots: &HashMap<hew_hir::SiteId, ChildSlot>,
    emitted_symbols: &mut HashMap<String, String>,
    diagnostics: &mut Vec<MirDiagnostic>,
) -> Option<LoweredFunction> {
    let emit_name = mangle_actor_init_handler(&actor.name);
    let duplicate_label = format!("actor `{}` init", actor.name);
    if let Some(existing) = emitted_symbols.get(&emit_name) {
        diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::ActorHandlerSymbolCollision {
                symbol: emit_name,
                existing: existing.clone(),
                duplicate: duplicate_label,
            },
            note: "actor init handler symbol mangling must be one-to-one before MIR emission"
                .to_string(),
        });
        return None;
    }
    emitted_symbols.insert(emit_name.clone(), duplicate_label);

    let synthetic_fn = HirFn {
        id: actor.id,
        node: actor.node,
        name: format!("{}::init", actor.name),
        type_params: Vec::new(),
        params: init.params.clone(),
        return_ty: ResolvedTy::Unit,
        body: init.body.clone(),
        span: actor.span.clone(),
    };
    Some(lower_function(
        &synthetic_fn,
        emit_name,
        HashMap::new(),
        type_classes,
        record_field_orders,
        actor_layouts,
        &HashMap::new(),
        Some(&actor.name),
        module_fn_names,
        call_site_type_args,
        supervisor_child_slots,
        crate::model::FunctionCallConv::ActorHandler,
    ))
}

#[allow(
    clippy::too_many_arguments,
    reason = "actor lifecycle lowering threads the same module tables as regular function lowering"
)]
#[allow(
    clippy::too_many_lines,
    reason = "each lifecycle hook kind (Start, Stop, Crash, Upgrade) needs its own arm; extracting would not reduce conceptual complexity"
)]
fn lower_actor_lifecycle_handlers(
    actor: &HirActorDecl,
    type_classes: &hew_hir::TypeClassTable,
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    actor_layouts: &HashMap<String, ActorLayout>,
    module_fn_names: &HashSet<String>,
    call_site_type_args: &HashMap<hew_hir::SiteId, Vec<ResolvedTy>>,
    supervisor_child_slots: &HashMap<hew_hir::SiteId, ChildSlot>,
    emitted_symbols: &mut HashMap<String, String>,
    diagnostics: &mut Vec<MirDiagnostic>,
) -> Vec<LoweredFunction> {
    let mut lowered = Vec::new();
    for (hook_idx, hook) in actor.lifecycle_hooks.iter().enumerate() {
        match hook.kind {
            HirLifecycleHookKind::Start => {
                let emit_name = mangle_actor_start_handler(&actor.name);
                let duplicate_label =
                    format!("actor `{}` #[on(start)] hook `{}`", actor.name, hook.name);
                if let Some(existing) = emitted_symbols.get(&emit_name) {
                    diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::ActorHandlerSymbolCollision {
                            symbol: emit_name,
                            existing: existing.clone(),
                            duplicate: duplicate_label,
                        },
                        note: "actor #[on(start)] handler symbol mangling must be one-to-one before MIR emission"
                            .to_string(),
                    });
                    continue;
                }
                emitted_symbols.insert(emit_name.clone(), duplicate_label);

                let synthetic_fn = HirFn {
                    id: actor.id,
                    node: actor.node,
                    name: format!("{}::{}", actor.name, hook.name),
                    type_params: Vec::new(),
                    params: hook.params.clone(),
                    return_ty: hook.return_ty.clone(),
                    body: hook.body.clone(),
                    span: hook.span.clone(),
                };
                lowered.push(lower_function(
                    &synthetic_fn,
                    emit_name,
                    HashMap::new(),
                    type_classes,
                    record_field_orders,
                    actor_layouts,
                    &HashMap::new(),
                    Some(&actor.name),
                    module_fn_names,
                    call_site_type_args,
                    supervisor_child_slots,
                    crate::model::FunctionCallConv::ActorHandler,
                ));
            }
            HirLifecycleHookKind::Stop => {
                // Per-hook unique symbol: <Actor>__on_stop__<hook_idx>.
                // hook_idx is the position of this hook in the actor's full
                // lifecycle_hooks vec (not a stop-specific counter), matching
                // the index used when populating ActorLayout.on_stop_symbols.
                // This guarantees no collisions even when multiple #[on(stop)]
                // hooks are declared on the same actor.
                let emit_name = mangle_actor_stop_handler_indexed(&actor.name, hook_idx);
                let label = format!("actor `{}` #[on(stop)] hook `{}`", actor.name, hook.name);
                emitted_symbols.insert(emit_name.clone(), label);

                let synthetic_fn = HirFn {
                    id: actor.id,
                    node: actor.node,
                    name: format!("{}::{}", actor.name, hook.name),
                    type_params: Vec::new(),
                    params: hook.params.clone(),
                    return_ty: hook.return_ty.clone(),
                    body: hook.body.clone(),
                    span: hook.span.clone(),
                };
                lowered.push(lower_function(
                    &synthetic_fn,
                    emit_name,
                    HashMap::new(),
                    type_classes,
                    record_field_orders,
                    actor_layouts,
                    &HashMap::new(),
                    Some(&actor.name),
                    module_fn_names,
                    call_site_type_args,
                    supervisor_child_slots,
                    crate::model::FunctionCallConv::ActorHandler,
                ));
            }
            HirLifecycleHookKind::Crash => {
                let emit_name = mangle_actor_crash_handler(&actor.name);
                let duplicate_label =
                    format!("actor `{}` #[on(crash)] hook `{}`", actor.name, hook.name);
                if let Some(existing) = emitted_symbols.get(&emit_name) {
                    diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::ActorHandlerSymbolCollision {
                            symbol: emit_name,
                            existing: existing.clone(),
                            duplicate: duplicate_label,
                        },
                        note: "actor #[on(crash)] handler symbol mangling must be one-to-one before MIR emission"
                            .to_string(),
                    });
                    continue;
                }
                emitted_symbols.insert(emit_name.clone(), duplicate_label);

                // ABI PROLOGUE — PanicInfo parameter injection:
                //
                // The runtime ABI for `HewOnCrashFn` passes the crash code as
                // `i64` in the second argument register (updated from `c_int`
                // in this slice; see hew-runtime/src/internal/types.rs).
                //
                // User sources declare `info: PanicInfo` and access `info.code`.
                // To bridge the raw `i64` wire value to the user-visible struct,
                // we inject a synthetic prologue into the function body:
                //
                //   let __crash_code: i64 = <ABI param>;   // sentinel BindingId
                //   let info: PanicInfo = PanicInfo { code: __crash_code };
                //
                // The original user-visible `info: PanicInfo` param is replaced
                // with `__crash_code: I64` in `abi_params`; the original binding
                // ID for `info` is preserved in the `Let` statement so that every
                // `BindingRef { resolved: Binding(info_id) }` in the user body
                // continues to resolve correctly through `binding_locals`.
                //
                // RETURN coercion: the runtime supervisor ignores the handler's
                // return value in v0.5 and applies its own restart-policy enum.
                // Passing `ResolvedTy::Named { "CrashAction" }` through to codegen
                // trips the D10 fail-closed gate; we use I32 until v0.6 wires
                // CrashAction enum-variant construction.
                //
                // BODY-RETURN NOTE: enum-variant construction (`CrashAction::Restart`)
                // is not yet wired in HIR lowering (NotYetImplemented gate). Today
                // only `panic()`-diverging bodies compile, so no actual CrashAction
                // value can appear in the MIR return slot.
                //
                // WHEN obsolete: when v0.6 wires CrashAction return-shape consult,
                // remove the I32 return coercion and let the HIR type flow through.
                // The prologue injection itself stays (the ABI wire remains i64).

                // Find the `info: PanicInfo` param (if present) and build ABI params.
                // The PanicInfo param is replaced with `__crash_code: I64`; every
                // other param passes through unchanged.
                let panic_info_param: Option<HirBinding> = hook
                    .params
                    .iter()
                    .find(
                        |p| matches!(&p.ty, ResolvedTy::Named { name, .. } if name == "PanicInfo"),
                    )
                    .cloned();

                let abi_params: Vec<HirBinding> = hook
                    .params
                    .iter()
                    .map(|p| {
                        if matches!(&p.ty, ResolvedTy::Named { name, .. } if name == "PanicInfo") {
                            HirBinding {
                                id: SENTINEL_CRASH_CODE_BINDING,
                                name: "__crash_code".to_string(),
                                ty: ResolvedTy::I64,
                                mutable: false,
                                span: p.span.clone(),
                            }
                        } else {
                            p.clone()
                        }
                    })
                    .collect();

                // Inject a synthetic `let info = PanicInfo { code: __crash_code }`
                // at the front of the body when the original signature had a
                // `PanicInfo` param (which is always the case for `on(crash)`).
                let body = if let Some(info_param) = panic_info_param {
                    // Build the `__crash_code` BindingRef expression.
                    let crash_code_ref = HirExpr {
                        node: SENTINEL_CRASH_CODE_NODE,
                        site: SENTINEL_CRASH_CODE_SITE,
                        ty: ResolvedTy::I64,
                        value_class: ValueClass::BitCopy,
                        intent: IntentKind::Read,
                        kind: HirExprKind::BindingRef {
                            name: "__crash_code".to_string(),
                            resolved: ResolvedRef::Binding(SENTINEL_CRASH_CODE_BINDING),
                        },
                        span: info_param.span.clone(),
                    };

                    // Build `PanicInfo { code: __crash_code }` StructInit expression.
                    let struct_init = HirExpr {
                        node: SENTINEL_CRASH_CODE_NODE,
                        site: SENTINEL_CRASH_CODE_SITE,
                        ty: ResolvedTy::Named {
                            name: "PanicInfo".to_string(),
                            args: Vec::new(),
                        },
                        value_class: ValueClass::BitCopy,
                        intent: IntentKind::Unknown,
                        kind: HirExprKind::StructInit {
                            name: "PanicInfo".to_string(),
                            type_args: Vec::new(),
                            fields: vec![("code".to_string(), crash_code_ref)],
                            base: None,
                        },
                        span: info_param.span.clone(),
                    };

                    // Build `let info: PanicInfo = PanicInfo { code: __crash_code }`.
                    // Preserve `info_param.id` so user `BindingRef { resolved: Binding(id) }` resolves.
                    let let_info_stmt = HirStmt {
                        node: SENTINEL_CRASH_CODE_NODE,
                        kind: HirStmtKind::Let(
                            HirBinding {
                                id: info_param.id,
                                name: info_param.name.clone(),
                                ty: ResolvedTy::Named {
                                    name: "PanicInfo".to_string(),
                                    args: Vec::new(),
                                },
                                mutable: false,
                                span: info_param.span.clone(),
                            },
                            Some(struct_init),
                        ),
                        span: info_param.span.clone(),
                    };

                    // Prepend the synthetic let before the original body statements.
                    let mut stmts = Vec::with_capacity(hook.body.statements.len() + 1);
                    stmts.push(let_info_stmt);
                    stmts.extend(hook.body.statements.iter().cloned());
                    HirBlock {
                        statements: stmts,
                        ..hook.body.clone()
                    }
                } else {
                    hook.body.clone()
                };

                let synthetic_fn = HirFn {
                    id: actor.id,
                    node: actor.node,
                    name: format!("{}::{}", actor.name, hook.name),
                    type_params: Vec::new(),
                    params: abi_params,
                    return_ty: ResolvedTy::I32,
                    body,
                    span: hook.span.clone(),
                };
                lowered.push(lower_function(
                    &synthetic_fn,
                    emit_name,
                    HashMap::new(),
                    type_classes,
                    record_field_orders,
                    actor_layouts,
                    &HashMap::new(),
                    Some(&actor.name),
                    module_fn_names,
                    call_site_type_args,
                    supervisor_child_slots,
                    crate::model::FunctionCallConv::ActorHandler,
                ));
            }
            HirLifecycleHookKind::Upgrade => push_lifecycle_not_wired_diagnostic(
                diagnostics,
                &actor.name,
                &hook.name,
                "OnUpgradeNotYetWired",
                "upgrade",
                "hot-upgrade lifecycle invocation is pending explicit ratification",
            ),
        }
    }
    lowered
}

fn push_lifecycle_not_wired_diagnostic(
    diagnostics: &mut Vec<MirDiagnostic>,
    actor_name: &str,
    hook_name: &str,
    diagnostic_name: &str,
    hook_kind: &str,
    reason: &str,
) {
    diagnostics.push(MirDiagnostic {
        kind: MirDiagnosticKind::UnsupportedNode {
            reason: format!("{diagnostic_name}: #[on({hook_kind})] is not wired"),
        },
        note: format!(
            "`#[on({hook_kind})]` hook `{actor_name}::{hook_name}` would silently never run; {reason}"
        ),
    });
}

/// Deterministic actor receive-handler symbol mangling.
///
/// Scheme: `<Actor>__recv__<handler>`, with source identifiers preserved
/// verbatim. The module-level actor lowering loop rejects collisions with
/// existing function symbols and earlier handler symbols before body emission.
fn mangle_actor_receive_handler(actor_name: &str, handler_name: &str) -> String {
    format!("{actor_name}__recv__{handler_name}")
}

fn mangle_actor_init_handler(actor_name: &str) -> String {
    format!("{actor_name}__init")
}

fn mangle_actor_start_handler(actor_name: &str) -> String {
    format!("{actor_name}__on_start")
}

/// Per-hook stop-handler symbol: `<Actor>__on_stop__<hook_idx>`.
///
/// `hook_idx` is the position of this hook in the actor's full
/// `lifecycle_hooks` vec, matching the index used when populating
/// `ActorLayout.on_stop_symbols`. Multiple `#[on(stop)]` hooks on the
/// same actor each get a unique symbol; codegen synthesises a fan-out
/// trampoline that calls them all in this lexical order.
fn mangle_actor_stop_handler_indexed(actor_name: &str, hook_idx: usize) -> String {
    format!("{actor_name}__on_stop__{hook_idx}")
}

fn mangle_actor_crash_handler(actor_name: &str) -> String {
    format!("{actor_name}__on_crash")
}

/// Deterministic supervisor-bootstrap symbol mangling.
///
/// Scheme: `<Supervisor>__bootstrap`. The bootstrap function carries
/// `FunctionCallConv::Default` — the synthetic MIR body is a stub that
/// codegen overrides wholesale with the `hew_supervisor_*` call sequence
/// in S-D.3, so the bootstrap function itself does not need to carry an
/// execution-context parameter. Supervisors are still actor-likes at the
/// runtime level (the supervisor's own actor is spawned inside
/// `hew_supervisor_start`); the `call_conv` applies only to the bootstrap
/// function's LLVM signature.
fn mangle_supervisor_bootstrap(name: &str) -> String {
    format!("{name}__bootstrap")
}

/// Topologically sort supervisor children by `wired_to:` dependencies via
/// Kahn's algorithm.
///
/// Returns child references in spawn order — dependencies first, dependents
/// after. Siblings that share no dependency relationship preserve source
/// declaration order, courtesy of the FIFO queue.
///
/// Returns `None` if a cycle is detected. This is the MIR-side fail-closed
/// backstop; the primary cycle gate is S-B's
/// `check_supervisor_wired_to_cycles` in `hew-types`, which emits the
/// user-facing `E_SUPERVISOR_WIRED_CYCLE` diagnostic. A cycle reaching
/// MIR means either the checker was bypassed or has a bug; MIR refuses
/// to emit a bootstrap function in that case rather than infinite-loop
/// on the dep graph.
fn supervisor_children_in_spawn_order(sup: &HirSupervisorDecl) -> Option<Vec<&HirSupervisorChild>> {
    use std::collections::VecDeque;

    let child_names: HashSet<&str> = sup.children.iter().map(|c| c.name.as_str()).collect();

    // Build dep list: child_name -> list of sibling names it depends on.
    // Filter out unknown sibling names (S-B reports those; we treat them
    // as no-dep edges so the topo sort still produces a deterministic
    // order over the children that DO exist).
    let mut in_degree: HashMap<&str, usize> = sup
        .children
        .iter()
        .map(|c| (c.name.as_str(), 0usize))
        .collect();
    let mut dependents_of: HashMap<&str, Vec<&str>> = sup
        .children
        .iter()
        .map(|c| (c.name.as_str(), vec![]))
        .collect();

    for child in &sup.children {
        let Some(wired) = &child.wired_to else {
            continue;
        };
        for sibling in wired.values() {
            let sibling_str = sibling.as_str();
            if !child_names.contains(sibling_str) {
                continue;
            }
            // `child` depends on `sibling` -> edge sibling -> child.
            if let Some(deps) = dependents_of.get_mut(sibling_str) {
                deps.push(child.name.as_str());
            }
            if let Some(d) = in_degree.get_mut(child.name.as_str()) {
                *d += 1;
            }
        }
    }

    // FIFO queue preserves declaration order for in-degree-zero siblings.
    let mut queue: VecDeque<&str> = sup
        .children
        .iter()
        .filter(|c| in_degree.get(c.name.as_str()).copied().unwrap_or(0) == 0)
        .map(|c| c.name.as_str())
        .collect();

    // Look up child by name for the result vector.
    let by_name: HashMap<&str, &HirSupervisorChild> =
        sup.children.iter().map(|c| (c.name.as_str(), c)).collect();

    let mut ordered: Vec<&HirSupervisorChild> = Vec::with_capacity(sup.children.len());
    while let Some(name) = queue.pop_front() {
        if let Some(child) = by_name.get(name).copied() {
            ordered.push(child);
        }
        let dependents = dependents_of.get(name).cloned().unwrap_or_default();
        for dep in dependents {
            if let Some(d) = in_degree.get_mut(dep) {
                *d -= 1;
                if *d == 0 {
                    queue.push_back(dep);
                }
            }
        }
    }

    if ordered.len() != sup.children.len() {
        // Cycle. S-B should have caught this; MIR refuses to emit.
        return None;
    }
    Some(ordered)
}

/// Build a `SupervisorLayout` from an `HirSupervisorDecl`. Returns `None`
/// if the child dependency graph is cyclic — the MIR fail-closed backstop
/// to S-B's `E_SUPERVISOR_WIRED_CYCLE` diagnostic.
fn build_supervisor_layout(
    sup: &HirSupervisorDecl,
    diagnostics: &mut Vec<MirDiagnostic>,
) -> Option<crate::model::SupervisorLayout> {
    let Some(ordered) = supervisor_children_in_spawn_order(sup) else {
        diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::UnsupportedNode {
                reason: format!(
                    "supervisor `{}` has a cyclic `wired_to` dependency graph",
                    sup.name
                ),
            },
            note: "the type checker's E_SUPERVISOR_WIRED_CYCLE diagnostic is the \
                   primary gate; MIR is refusing to emit a bootstrap function as \
                   the structural backstop"
                .to_string(),
        });
        return None;
    };

    let children: Vec<crate::model::SupervisorChildLayout> = ordered
        .iter()
        .enumerate()
        .map(|(spawn_order, child)| crate::model::SupervisorChildLayout {
            name: child.name.clone(),
            actor_name: child.ty.clone(),
            restart_policy: child.restart_policy,
            is_pool: child.is_pool,
            slot_index: child.slot_index,
            wired_to: child.wired_to.clone().unwrap_or_default(),
            spawn_order: u32::try_from(spawn_order)
                .expect("supervisor child count exceeds u32::MAX — impossible in Hew"),
            // Populated after actor_layout_map is built (post-loop pass in
            // lower_hir_module) to handle any declaration order. Left None
            // here so build_supervisor_layout needs no actor-layout parameter.
            on_crash_symbol: None,
            max_heap_bytes: None,
        })
        .collect();

    Some(crate::model::SupervisorLayout {
        name: sup.name.clone(),
        strategy: sup.strategy,
        max_restarts: sup.max_restarts,
        window: sup.window.clone(),
        bootstrap_symbol: mangle_supervisor_bootstrap(&sup.name),
        children,
    })
}

/// Build the `ResolvedTy` for a `LocalPid<ChildActorName>` handle — the
/// type the checker assigns to `spawn ChildActor(...)` results
/// (`hew-types/src/check/expressions.rs::check_spawn`). The supervisor
/// bootstrap's synthetic HIR uses this type for the let-binding that
/// captures each spawned child's handle and for the `BindingRef`s that
/// pass sibling handles to dependents' `wired_to:` init params.
fn local_pid_of(actor_name: &str) -> ResolvedTy {
    ResolvedTy::Named {
        name: "LocalPid".to_string(),
        args: vec![ResolvedTy::Named {
            name: actor_name.to_string(),
            args: vec![],
        }],
    }
}

/// Lower an `HirItem::Supervisor` to a `FunctionCallConv::ActorHandler`
/// bootstrap function. The body is a topologically ordered sequence of
/// `spawn <ChildActor>(...)` statements; siblings referenced by
/// `wired_to:` resolve to the earlier-spawned binding's handle and ride
/// the existing `lower_spawn_actor` substrate as `init_args`.
///
/// Substrate reuse: this synthesises a `HirFn` and dispatches through the
/// same `lower_function` pipeline that emits every other function. The
/// fail-closed dataflow checker, the per-block topology builder, and
/// `bracket_actor_handler_blocks` all fire from the `call_conv` discriminator
/// — no per-supervisor variant of any of that machinery exists.
///
/// Q87 note: supervisors have no `receive fn` and no
/// `ActorProtocolDescriptor`. The bootstrap function itself is plain
/// MIR; the `SupervisorLayout` documents the deliberate descriptor
/// absence at its type docstring. This is the load-bearing call: future
/// supervisor-facing message protocols (programmatic restart, drain
/// APIs) would require introducing a descriptor at THAT point — not
/// retrofitting one here for hypothetical use.
///
/// Synthetic HIR IDs (`BindingId`, `SiteId`, `HirNodeId`, `ScopeId`) are
/// per-function counters scoped to the helper. The downstream MIR
/// builder consumes them as opaque tags — `Builder.binding_locals` and
/// the dataflow analyzer key on per-function uniqueness, which the
/// monotonic counter guarantees.
#[allow(
    clippy::too_many_arguments,
    reason = "mirrors lower_actor_init_handler's threading of module-wide tables"
)]
#[allow(
    clippy::too_many_lines,
    reason = "single coherent helper: synthetic-HIR construction + per-child wiring + \
              dispatch through lower_function. Splitting would obscure that the entire \
              function exists to materialize a single bootstrap HirFn whose statements \
              must be built in topological order with stable per-function ids."
)]
fn lower_supervisor_bootstrap(
    sup: &HirSupervisorDecl,
    supervisor_layouts: &HashMap<String, crate::model::SupervisorLayout>,
    type_classes: &hew_hir::TypeClassTable,
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    actor_layouts: &HashMap<String, ActorLayout>,
    module_fn_names: &HashSet<String>,
    call_site_type_args: &HashMap<hew_hir::SiteId, Vec<ResolvedTy>>,
    supervisor_child_slots: &HashMap<hew_hir::SiteId, ChildSlot>,
    emitted_symbols: &mut HashMap<String, String>,
    diagnostics: &mut Vec<MirDiagnostic>,
) -> Option<LoweredFunction> {
    let emit_name = mangle_supervisor_bootstrap(&sup.name);
    let duplicate_label = format!("supervisor `{}` bootstrap", sup.name);
    if let Some(existing) = emitted_symbols.get(&emit_name) {
        diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::ActorHandlerSymbolCollision {
                symbol: emit_name,
                existing: existing.clone(),
                duplicate: duplicate_label,
            },
            note: "supervisor bootstrap symbol mangling must be one-to-one before MIR emission"
                .to_string(),
        });
        return None;
    }
    emitted_symbols.insert(emit_name.clone(), duplicate_label);

    // Topo-ordered child list. None means cycle (S-B should have caught
    // first; build_supervisor_layout already pushed the structural
    // diagnostic if so).
    let ordered = supervisor_children_in_spawn_order(sup)?;

    // Verify every child names an actor we know about. Unknown actor
    // -> NotYetImplemented, skip emission. The checker validates child
    // types but a stale stdlib/registry could in principle desync; this
    // is the MIR-side fail-closed boundary.
    for child in &ordered {
        if !actor_layouts.contains_key(&child.ty) {
            diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!(
                        "supervisor `{}` child `{}` references unknown actor `{}`",
                        sup.name, child.name, child.ty
                    ),
                    // No real SiteId — use a synthetic 0; the user-facing
                    // gate is the checker, which already names the spans.
                    site: SiteId(0),
                },
                note: "MIR could not resolve the child's actor type in the actor-layout \
                       table; the type checker should have rejected this supervisor"
                    .to_string(),
            });
            return None;
        }
    }

    // ── Synthesize the bootstrap HirFn. ──────────────────────────────
    //
    // Body shape (in topo order, one statement per child):
    //
    //     let <child_name>: LocalPid<ChildActor> = spawn ChildActor(
    //         <init_param_1>: <expr_1>,
    //         ...
    //     );
    //
    // For a `wired_to:` mapping `{ init_param: sibling_name }`, the
    // value expression is `BindingRef { name: sibling_name, resolved:
    // Binding(<sibling's id>) }` — the dataflow analyzer sees a Bind +
    // Use chain that rubber-stamps because no Consume intent appears.
    //
    // Non-`wired_to:` init params are NOT lowered here. The current
    // surface only accepts `wired_to:`-driven init params; any other
    // shape is checker-rejected. If/when supervisors gain literal init
    // args, this is the site that grows them.

    let mut next_binding: u32 = 0;
    let mut next_site: u32 = 0;
    let mut next_node: u32 = 0;
    let mut fresh_binding = || {
        let id = BindingId(next_binding);
        next_binding += 1;
        id
    };
    let mut fresh_site = || {
        let id = SiteId(next_site);
        next_site += 1;
        id
    };
    let mut fresh_node = || {
        let id = HirNodeId(next_node);
        next_node += 1;
        id
    };

    // Map: child name -> (binding id of the let, handle type) so wired_to
    // arg expressions can resolve to the earlier spawn's binding.
    let mut child_bindings: HashMap<String, (BindingId, ResolvedTy)> = HashMap::new();

    let mut statements: Vec<HirStmt> = Vec::with_capacity(ordered.len());
    for child in &ordered {
        let handle_ty = local_pid_of(&child.ty);

        // Build the spawn's args vec from the wired_to map. We iterate
        // the child's wired_to map in a deterministic (sorted) order so
        // the emitted MIR is stable across runs.
        let mut spawn_args: Vec<(String, HirExpr)> = Vec::new();
        if let Some(wired) = &child.wired_to {
            let mut entries: Vec<(&String, &String)> = wired.iter().collect();
            entries.sort_by(|a, b| a.0.cmp(b.0));
            for (init_param, sibling_name) in entries {
                let Some((sibling_binding, sibling_ty)) = child_bindings.get(sibling_name).cloned()
                else {
                    // S-B should have rejected an unknown sibling; if we
                    // reach this branch the sibling either wasn't in the
                    // supervisor or appears after the dependent (which
                    // the topo sort just forbade). Either way, refuse to
                    // emit.
                    diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: format!(
                                "supervisor `{}` child `{}` wired_to refers to sibling \
                                 `{sibling_name}` which is unresolved at spawn time",
                                sup.name, child.name
                            ),
                            site: SiteId(0),
                        },
                        note: "topological spawn order should make every wired sibling \
                               available before its dependent; if this fires the checker \
                               wired_to validation was bypassed"
                            .to_string(),
                    });
                    return None;
                };
                let arg_expr = HirExpr {
                    node: fresh_node(),
                    site: fresh_site(),
                    ty: sibling_ty,
                    value_class: ValueClass::BitCopy,
                    intent: IntentKind::Read,
                    kind: HirExprKind::BindingRef {
                        name: sibling_name.clone(),
                        resolved: ResolvedRef::Binding(sibling_binding),
                    },
                    span: sup.span.clone(),
                };
                spawn_args.push((init_param.clone(), arg_expr));
            }
        }

        let spawn_expr = HirExpr {
            node: fresh_node(),
            site: fresh_site(),
            ty: handle_ty.clone(),
            value_class: ValueClass::BitCopy,
            intent: IntentKind::Read,
            kind: HirExprKind::Spawn {
                actor_name: child.ty.clone(),
                args: spawn_args,
            },
            span: sup.span.clone(),
        };

        let binding = HirBinding {
            id: fresh_binding(),
            name: child.name.clone(),
            ty: handle_ty.clone(),
            mutable: false,
            span: sup.span.clone(),
        };
        child_bindings.insert(child.name.clone(), (binding.id, handle_ty));

        statements.push(HirStmt {
            node: fresh_node(),
            kind: HirStmtKind::Let(binding, Some(spawn_expr)),
            span: sup.span.clone(),
        });
    }

    let body = HirBlock {
        node: fresh_node(),
        scope: ScopeId(0),
        statements,
        tail: None,
        ty: ResolvedTy::Unit,
        span: sup.span.clone(),
    };

    // Bootstrap returns `LocalPid<Sup>` so callers of `spawn Sup` receive a
    // typed handle. The body does not yet emit a return value — S-D.3 replaces
    // the body wholesale with the `hew_supervisor_*` call sequence and a real
    // return. For this slice the return slot is intentionally left unwritten;
    // the MIR-level dataflow checker does not enforce return-slot initialisation,
    // and codegen will never see this stub body (S-D.3 lands before codegen
    // is exercised on supervisor programs).
    //
    // SHIM(S-D.1→S-D.3): bootstrap body is a stub. Return slot unwritten.
    // WHY: type signature must be correct before the routing call site is
    //   wired; body replacement is a separate slice.
    // WHEN obsolete: S-D.3 replaces the body with the hew_supervisor_* sequence.
    // WHAT: emit hew_supervisor_start/register_child/etc in S-D.3.
    let synthetic_fn = HirFn {
        id: sup.id,
        node: sup.node,
        name: format!("{}::__bootstrap", sup.name),
        type_params: Vec::new(),
        params: Vec::new(),
        return_ty: local_pid_of(&sup.name),
        body,
        span: sup.span.clone(),
    };

    Some(lower_function(
        &synthetic_fn,
        emit_name,
        HashMap::new(),
        type_classes,
        record_field_orders,
        actor_layouts,
        supervisor_layouts,
        // Supervisors have no actor state. `lower_actor_init_handler`
        // passes `Some(&actor.name)` for the same role; here we pass
        // `None` because there's no state-field table to lift into the
        // Builder.
        None,
        module_fn_names,
        call_site_type_args,
        supervisor_child_slots,
        // `FunctionCallConv::Default`: codegen replaces the bootstrap body
        // wholesale with the `hew_supervisor_*` call sequence (S-D.3), so
        // the body never reads an execution context. The synthetic call
        // site at `lower_spawn` emits `Terminator::Call { args: [] }`; an
        // `ActorHandler` declaration would add a leading ctx LLVM parameter
        // that the caller does not supply, tripping `llvm.verify`. The
        // doc-comment on `mangle_supervisor_bootstrap` describing the
        // supervisor as actor-like still applies at the runtime level — the
        // supervisor's own actor is spawned by `hew_supervisor_start`, not
        // by the bootstrap's call_conv.
        crate::model::FunctionCallConv::Default,
    ))
}

/// Build the MIR `ActorHandlerLayout` row for every `receive fn` on this
/// actor, drawing the `msg_type` from the checker's protocol descriptor.
///
/// Pre-Q87 this iterated `receive_handlers.iter().enumerate()` and used the
/// source-order index as the wire-format `msg_id` — which silently flipped
/// the ABI on every handler reorder. Q87 slice 1 replaces that with the
/// stable, hash-derived id from
/// `HirActorDecl::protocol_descriptor`. Source-order rearrangement no
/// longer affects the protocol.
///
/// Fail-closed: an actor that carries `receive_handlers` but no descriptor
/// (the checker emitted an `ActorProtocolCollision` and refused to publish
/// the protocol, or an upstream bug) falls back to `i32::MAX`. The
/// upstream collision diagnostic is the user-facing error; the sentinel
/// here exists only to keep the MIR shape well-formed for the few internal
/// callers (e.g. `lower_program_smoke`) that exercise lowering on
/// descriptor-less inputs. Production builds never observe it because the
/// checker rejects collision-bearing programs before MIR runs.
fn lower_actor_handler_layouts(actor: &HirActorDecl) -> Vec<ActorHandlerLayout> {
    let descriptor = actor.protocol_descriptor.as_ref();
    actor
        .receive_handlers
        .iter()
        .map(|handler| {
            let msg_type = descriptor
                .and_then(|d| d.msg_id_for(&handler.name))
                .map_or(i32::MAX, |id| i32::from_ne_bytes(id.to_ne_bytes()));
            ActorHandlerLayout {
                name: handler.name.clone(),
                symbol: mangle_actor_receive_handler(&actor.name, &handler.name),
                msg_type,
                param_tys: handler
                    .params
                    .iter()
                    .map(|param| param.ty.clone())
                    .collect(),
                return_ty: handler.return_ty.clone(),
            }
        })
        .collect()
}

fn unknown_self_fields_in_block(block: &HirBlock, state_fields: &HashSet<String>) -> Vec<String> {
    let mut seen = HashSet::new();
    let mut unknown = Vec::new();
    collect_unknown_self_fields_in_block(block, state_fields, &mut seen, &mut unknown);
    unknown
}

fn collect_unknown_self_fields_in_block(
    block: &HirBlock,
    state_fields: &HashSet<String>,
    seen: &mut HashSet<String>,
    unknown: &mut Vec<String>,
) {
    for stmt in &block.statements {
        match &stmt.kind {
            HirStmtKind::Let(_, Some(expr))
            | HirStmtKind::Expr(expr)
            | HirStmtKind::Return(Some(expr)) => {
                collect_unknown_self_fields_in_expr(expr, state_fields, seen, unknown);
            }
            HirStmtKind::Assign { target, value } => {
                collect_unknown_self_fields_in_expr(target, state_fields, seen, unknown);
                collect_unknown_self_fields_in_expr(value, state_fields, seen, unknown);
            }
            HirStmtKind::Let(_, None) | HirStmtKind::Return(None) => {}
        }
    }
    if let Some(tail) = &block.tail {
        collect_unknown_self_fields_in_expr(tail, state_fields, seen, unknown);
    }
}

#[allow(
    clippy::too_many_lines,
    reason = "visitor mirrors the sealed HirExprKind surface so self-field validation is exhaustive"
)]
fn collect_unknown_self_fields_in_expr(
    expr: &HirExpr,
    state_fields: &HashSet<String>,
    seen: &mut HashSet<String>,
    unknown: &mut Vec<String>,
) {
    match &expr.kind {
        HirExprKind::Literal(_)
        | HirExprKind::BindingRef { .. }
        | HirExprKind::AwaitTask { .. }
        | HirExprKind::ContextReader { .. }
        | HirExprKind::Unsupported(_) => {}
        HirExprKind::Binary { left, right, .. } | HirExprKind::IdentityCompare { left, right } => {
            collect_unknown_self_fields_in_expr(left, state_fields, seen, unknown);
            collect_unknown_self_fields_in_expr(right, state_fields, seen, unknown);
        }
        HirExprKind::Call { callee, args } | HirExprKind::SpawnedCall { callee, args, .. } => {
            collect_unknown_self_fields_in_expr(callee, state_fields, seen, unknown);
            for arg in args {
                collect_unknown_self_fields_in_expr(arg, state_fields, seen, unknown);
            }
        }
        HirExprKind::Spawn { args, .. } => {
            for (_, arg) in args {
                collect_unknown_self_fields_in_expr(arg, state_fields, seen, unknown);
            }
        }
        HirExprKind::ActorSend { receiver, args, .. }
        | HirExprKind::ActorAsk { receiver, args, .. }
        | HirExprKind::CallDynMethod { receiver, args, .. } => {
            collect_unknown_self_fields_in_expr(receiver, state_fields, seen, unknown);
            for arg in args {
                collect_unknown_self_fields_in_expr(arg, state_fields, seen, unknown);
            }
        }
        HirExprKind::Block(block)
        | HirExprKind::Scope { body: block }
        | HirExprKind::ForkBlock { body: block, .. } => {
            collect_unknown_self_fields_in_block(block, state_fields, seen, unknown);
        }
        HirExprKind::If {
            condition,
            then_expr,
            else_expr,
        } => {
            collect_unknown_self_fields_in_expr(condition, state_fields, seen, unknown);
            collect_unknown_self_fields_in_expr(then_expr, state_fields, seen, unknown);
            if let Some(else_expr) = else_expr {
                collect_unknown_self_fields_in_expr(else_expr, state_fields, seen, unknown);
            }
        }
        HirExprKind::StructInit { fields, base, .. } => {
            for (_, field_expr) in fields {
                collect_unknown_self_fields_in_expr(field_expr, state_fields, seen, unknown);
            }
            if let Some(base) = base {
                collect_unknown_self_fields_in_expr(base, state_fields, seen, unknown);
            }
        }
        HirExprKind::FieldAccess { object, field } => {
            if matches!(
                &object.kind,
                HirExprKind::BindingRef {
                    name,
                    resolved: ResolvedRef::Unresolved | ResolvedRef::Binding(_) | ResolvedRef::Item(_)
                } if name == "self"
            ) && !state_fields.contains(field)
                && seen.insert(field.clone())
            {
                unknown.push(field.clone());
            }
            collect_unknown_self_fields_in_expr(object, state_fields, seen, unknown);
        }
        HirExprKind::ScopeDeadline { duration, body } => {
            collect_unknown_self_fields_in_expr(duration, state_fields, seen, unknown);
            collect_unknown_self_fields_in_block(body, state_fields, seen, unknown);
        }
        HirExprKind::Select(select) => {
            for arm in &select.arms {
                match &arm.kind {
                    hew_hir::HirSelectArmKind::StreamNext { stream } => {
                        collect_unknown_self_fields_in_expr(stream, state_fields, seen, unknown);
                    }
                    hew_hir::HirSelectArmKind::ActorAsk { actor, args, .. } => {
                        collect_unknown_self_fields_in_expr(actor, state_fields, seen, unknown);
                        for arg in args {
                            collect_unknown_self_fields_in_expr(arg, state_fields, seen, unknown);
                        }
                    }
                    hew_hir::HirSelectArmKind::TaskAwait { task } => {
                        collect_unknown_self_fields_in_expr(task, state_fields, seen, unknown);
                    }
                    hew_hir::HirSelectArmKind::AfterTimer { duration } => {
                        collect_unknown_self_fields_in_expr(duration, state_fields, seen, unknown);
                    }
                }
                collect_unknown_self_fields_in_expr(&arm.body, state_fields, seen, unknown);
            }
        }
        HirExprKind::SpawnLambdaActor { body, .. } | HirExprKind::Closure { body, .. } => {
            collect_unknown_self_fields_in_expr(body, state_fields, seen, unknown);
        }
        HirExprKind::TupleIndex { tuple, .. } => {
            collect_unknown_self_fields_in_expr(tuple, state_fields, seen, unknown);
        }
        HirExprKind::Index { container, index } => {
            collect_unknown_self_fields_in_expr(container, state_fields, seen, unknown);
            collect_unknown_self_fields_in_expr(index, state_fields, seen, unknown);
        }
        HirExprKind::Slice {
            container,
            start,
            end,
            ..
        } => {
            collect_unknown_self_fields_in_expr(container, state_fields, seen, unknown);
            if let Some(start) = start {
                collect_unknown_self_fields_in_expr(start, state_fields, seen, unknown);
            }
            if let Some(end) = end {
                collect_unknown_self_fields_in_expr(end, state_fields, seen, unknown);
            }
        }
        HirExprKind::CoerceToDynTrait { value, .. } => {
            collect_unknown_self_fields_in_expr(value, state_fields, seen, unknown);
        }
        HirExprKind::MachineEmit { fields, .. } => {
            for (_, field_val) in fields {
                collect_unknown_self_fields_in_expr(field_val, state_fields, seen, unknown);
            }
        }
    }
}

#[derive(Debug)]
struct LoweredFunction {
    thir: ThirFunction,
    raw: RawMirFunction,
    checked: CheckedMirFunction,
    elaborated: ElaboratedMirFunction,
    diagnostics: Vec<MirDiagnostic>,
    generated: Vec<LoweredFunction>,
    record_layouts: Vec<crate::model::RecordLayout>,
}

/// Insert execution-context carrier markers into a context-bearing CFG.
///
/// The entry block starts with `EnterContext`; every terminal block (`Return`
/// and `Trap` in today's MIR) ends with `ExitContext` immediately before the
/// terminator. The helper is idempotent so synthetic tests and future
/// context-bearing producers can call it before validation without
/// double-inserting markers.
pub fn bracket_actor_handler_blocks(blocks: &mut [BasicBlock]) {
    if let Some(entry) = blocks.first_mut() {
        if !matches!(entry.instructions.first(), Some(Instr::EnterContext)) {
            entry.instructions.insert(0, Instr::EnterContext);
        }
    }

    for block in blocks {
        if matches!(
            block.terminator,
            Terminator::Return | Terminator::Trap { .. }
        ) && !matches!(block.instructions.last(), Some(Instr::ExitContext))
        {
            block.instructions.push(Instr::ExitContext);
        }
    }
}

#[allow(
    clippy::too_many_arguments,
    clippy::too_many_lines,
    reason = "lowering threads shared module tables plus the call-convention discriminator"
)]
fn lower_function(
    func: &HirFn,
    emit_name: String,
    subst: HashMap<String, ResolvedTy>,
    type_classes: &hew_hir::TypeClassTable,
    record_field_orders: &HashMap<String, Vec<(String, ResolvedTy)>>,
    actor_layouts: &HashMap<String, ActorLayout>,
    supervisor_layout_map: &HashMap<String, crate::model::SupervisorLayout>,
    current_actor_name: Option<&str>,
    module_fn_names: &HashSet<String>,
    call_site_type_args: &HashMap<hew_hir::SiteId, Vec<ResolvedTy>>,
    supervisor_child_slots: &HashMap<hew_hir::SiteId, ChildSlot>,
    call_conv: crate::model::FunctionCallConv,
) -> LoweredFunction {
    let mut builder = Builder {
        type_classes: type_classes.clone(),
        record_field_orders: record_field_orders.clone(),
        actor_layouts: actor_layouts.clone(),
        supervisor_layout_map: supervisor_layout_map.clone(),
        current_actor_state_fields: current_actor_name
            .and_then(|name| actor_layouts.get(name))
            .map(|layout| {
                layout
                    .state_field_names
                    .iter()
                    .cloned()
                    .zip(layout.state_field_tys.iter().cloned())
                    .enumerate()
                    .map(|(idx, (name, ty))| {
                        (
                            name,
                            (
                                FieldOffset(
                                    u32::try_from(idx).expect("actor field count exceeds u32::MAX"),
                                ),
                                ty,
                            ),
                        )
                    })
                    .collect()
            })
            .unwrap_or_default(),
        module_fn_names: module_fn_names.clone(),
        subst,
        call_site_type_args: call_site_type_args.clone(),
        supervisor_child_slots: supervisor_child_slots.clone(),
        current_function_symbol: emit_name.clone(),
        ..Builder::default()
    };
    // Allocate parameter locals BEFORE lowering the function body so
    // that `BindingRef` expressions that reference parameters resolve
    // to real `Place::Local(i)` slots instead of hitting `UnresolvedPlace`.
    //
    // Each parameter gets its own `Place::Local` in declaration order;
    // subsequent body-local allocations are appended after these slots
    // (enforced by `alloc_local`'s monotone `locals.len()` counter).
    // Codegen emits a parameter-prologue that stores each LLVM function
    // argument into the corresponding alloca slot before the first instruction.
    builder.lower_params(func);
    builder.function_body(func);

    // Effective return type after type-parameter substitution.
    let return_ty = builder.subst_ty(&func.return_ty);

    // Drain the in-flight current block into a sealed `BasicBlock` with
    // a `Terminator::Return`. Slice 1's flat lowering always produces a
    // singleton blocks vector; Slice 2+ may surface multiple blocks
    // when `If` (and later `Match` / loops) split the CFG. The order is
    // monotone in block id.
    let mut blocks = builder.finalize_blocks(Terminator::Return);
    if call_conv.carries_execution_context() {
        bracket_actor_handler_blocks(&mut blocks);
    }
    // THIR's `statements` is the union of every block's checker stream
    // in CFG-construction order — the THIR snapshot's job is preserving
    // the pre-CFG flat-stream shape for diagnostic readers that haven't
    // been ported to block-aware iteration yet. Slice 3's per-block
    // dataflow consumes `RawMirFunction.blocks` directly and doesn't
    // touch this snapshot.
    let thir_statements: Vec<MirStatement> = blocks
        .iter()
        .flat_map(|b| b.statements.iter().cloned())
        .collect();
    let thir = ThirFunction {
        name: emit_name.clone(),
        return_ty: return_ty.clone(),
        statements: thir_statements,
    };
    // `CheckedMirFunction` mirrors `RawMirFunction.blocks` directly
    // (widened in Slice 2 from a single-block field to a vec). The
    // elaborator + check_function consume the block vec; legacy
    // single-block tests still see `blocks[0]` as the entry block.
    let raw = RawMirFunction {
        name: emit_name.clone(),
        return_ty: return_ty.clone(),
        call_conv,
        params: func
            .params
            .iter()
            .map(|p| builder.subst_ty(&p.ty))
            .collect(),
        locals: builder.locals.clone(),
        blocks,
        decisions: builder.decisions.clone(),
    };
    // Checked MIR's `checks` field is populated by `check_function`
    // from real dataflow over the checker-authority `MirStatement`
    // stream. The `MirDiagnostic` surface that the CLI rejects on is
    // projected from these checks — there is one source of truth for
    // move/borrow/init legality.
    let mut dataflow_result = check_function(&builder, &raw.blocks, func);
    dataflow_result
        .checks
        .extend(crate::model::validate_context_markers(&raw));
    let mut diagnostics: Vec<MirDiagnostic> = dataflow_result
        .checks
        .iter()
        .filter_map(check_to_diagnostic)
        .collect();

    // Collect diagnostics emitted by the builder (e.g., Unsupported HIR nodes).
    diagnostics.append(&mut builder.diagnostics);

    collect_unknown_type_diagnostics(func, &builder, &mut diagnostics);

    // Compute cooperate-check sites from the CFG. Empty for leaf functions
    // (< 10 MIR statements, no calls, no loops). Codegen reads
    // `cooperate_sites` to inject `call @hew_actor_cooperate()`.
    let cooperate_sites = dataflow::compute_cooperate_sites(&raw.blocks);

    let checked = CheckedMirFunction {
        name: emit_name,
        return_ty: return_ty.clone(),
        blocks: raw.blocks.clone(),
        decisions: builder.decisions.clone(),
        checks: dataflow_result.checks.clone(),
        cooperate_sites,
    };
    // Drop-elaboration pass. Consumes the CheckedMirFunction we just
    // built; emits an ElaboratedMirFunction whose `blocks` + `drop_plans`
    // are the authoritative description of what fires on every exit.
    //
    // The integer-only spine never lowers `@resource` or `@linear`
    // bindings (no construction surface yet for those types — see
    // R-C3.5), so on the current ladder `owned_locals` is empty
    // whenever the function passed type-checking AND the only
    // non-BitCopy class reaching MIR is `CowValue` (e.g. String) which
    // does not emit a Drop. The elaboration shape is exercised by
    // hew-mir's unit tests that hand-construct CheckedMirFunction
    // inputs with synthesized DecisionFact::value_class values.
    let elaborated = elaborate(&checked, &builder, &thir.statements, &dataflow_result);

    // Fail-closed validation of the elaborated drop plan. Surfaces a
    // `MirCheck::DropPlanUndetermined` for any Return-block whose
    // per-exit live-set decision the elaborator could not commit to.
    // No partial drops escape: a `DropPlanUndetermined` finding
    // upgrades into a `MirDiagnostic` via `check_to_diagnostic`, and
    // the CLI rejects the program before codegen runs. LESSONS:
    // cleanup-all-exits, boundary-fail-closed.
    for check in validate_drop_plan(&elaborated) {
        if let Some(diag) = check_to_diagnostic(&check) {
            diagnostics.push(diag);
        }
    }
    // Cross-block stale-handle detection. Walks the backend Instr
    // stream across the function's CFG and rejects drop plans that
    // fire `Place::DuplexHandle(N)` on a block whose reaching paths
    // have already moved the unified handle into a `SendHalf` /
    // `RecvHalf` split. Catches the case the slice-3 structural
    // checker cannot — a same-direction close emitted by codegen on
    // a handle whose previous owner has been moved out. LESSONS:
    // cleanup-all-exits, raii-null-after-move,
    // boundary-fail-closed.
    for check in validate_cross_block_split_consume(&raw.blocks, &elaborated) {
        if let Some(diag) = check_to_diagnostic(&check) {
            diagnostics.push(diag);
        }
    }

    LoweredFunction {
        thir,
        raw,
        checked,
        elaborated,
        diagnostics,
        generated: builder.generated_functions,
        record_layouts: builder.closure_record_layouts,
    }
}

fn collect_unknown_type_diagnostics(
    func: &HirFn,
    builder: &Builder,
    diagnostics: &mut Vec<MirDiagnostic>,
) {
    let mut reported = HashSet::new();

    for param in &func.params {
        push_unknown_type_diagnostics(&param.ty, builder, &mut reported, diagnostics);
    }
    push_unknown_type_diagnostics(&func.return_ty, builder, &mut reported, diagnostics);

    for decision in &builder.decisions {
        push_unknown_type_diagnostics(&decision.ty, builder, &mut reported, diagnostics);
        if decision.strategy == Strategy::UnknownBlocked
            && named_type_names(&decision.ty).is_empty()
        {
            push_unknown_type_diagnostic(format!("{:?}", decision.ty), &mut reported, diagnostics);
        }
    }

    for statement in &builder.statements {
        match statement {
            MirStatement::Bind { ty, .. }
            | MirStatement::Evaluate { ty, .. }
            | MirStatement::Use { ty, .. }
            | MirStatement::Return { ty, .. }
            | MirStatement::Drop { ty, .. } => {
                push_unknown_type_diagnostics(ty, builder, &mut reported, diagnostics);
            }
        }
    }
}

/// Emit `UnknownType` diagnostics for each Named type in `ty` that is absent
/// from `type_classes`. Names present in the registry are known — they carry
/// an `@linear` or `@resource` marker — and must not be treated as unknown.
/// This implements §3.1 "Checker authority survives downstream": the MIR layer
/// consumes the HIR checker's `type_classes` decision rather than re-deriving
/// Named-type knownness independently.
fn push_unknown_type_diagnostics(
    ty: &ResolvedTy,
    builder: &Builder,
    reported: &mut HashSet<String>,
    diagnostics: &mut Vec<MirDiagnostic>,
) {
    for name in named_type_names(ty) {
        if builder.type_classes.contains_key(&name)
            || matches!(name.as_str(), "LocalPid" | "ActorRef" | "Actor")
            || builder.actor_layouts.contains_key(&name)
            || builder.supervisor_layout_map.contains_key(&name)
        {
            continue;
        }
        push_unknown_type_diagnostic(name, reported, diagnostics);
    }
}

fn push_unknown_type_diagnostic(
    name: String,
    reported: &mut HashSet<String>,
    diagnostics: &mut Vec<MirDiagnostic>,
) {
    if reported.insert(name.clone()) {
        diagnostics.push(MirDiagnostic {
            kind: MirDiagnosticKind::UnknownType { name },
            note: "named user type has no known ValueClass at the MIR boundary; \
                   only builtin types are supported in slice 1"
                .to_string(),
        });
    }
}

#[derive(Debug, Default)]
struct Builder {
    /// Checker-authority stream for the *current* basic block. Drained
    /// into a `BasicBlock` when the cursor moves (`finish_current_block`)
    /// or at function-body finalisation. Once a block is sealed it lives
    /// in `pending_blocks` until the function's body walk completes.
    statements: Vec<MirStatement>,
    /// Backend-authority stream for the *current* basic block. Populated
    /// in lock-step with `statements` by `lower_value` so the checker
    /// and the emitter agree on what each `SiteId` resolves to. Drained
    /// at the same cursor-move site as `statements`.
    instructions: Vec<Instr>,
    /// Completed basic blocks in construction order. Block id `0` is the
    /// function's entry block; subsequent ids are monotone in allocation
    /// order. The currently-being-built block (`current_block_id` /
    /// `statements` / `instructions`) is appended at function-body
    /// finalisation. Slice 1 leaves this empty for every function (the
    /// cursor never moves under the CFG-flat lowering); Slice 2's `If`
    /// lowering is the first writer.
    pending_blocks: Vec<BasicBlock>,
    /// Monotone counter for fresh `BasicBlock` ids. `alloc_block` returns
    /// the next id without switching the cursor — the caller is
    /// responsible for `finish_current_block(...)` + `start_block(id)`
    /// at the right point in the lowering sequence.
    next_block_id: u32,
    /// Id of the block currently receiving `statements` / `instructions`.
    /// Initialised to `0` (the entry block). Updated by
    /// `start_block(id)` after a `finish_current_block(...)` seals the
    /// previous block into `pending_blocks`.
    current_block_id: u32,
    /// Type-indexed local registers. `locals[i]` is the `ResolvedTy` of
    /// `Place::Local(i as u32)`.
    locals: Vec<ResolvedTy>,
    /// Maps `BindingId` to the `Local(N)` slot that holds the binding's
    /// initialiser. Cluster 1 reads the slot directly; later clusters add
    /// drop-cleanup and rebinding semantics.
    binding_locals: HashMap<BindingId, Place>,
    decisions: Vec<DecisionFact>,
    owned_locals: Vec<(hew_hir::BindingId, String, ResolvedTy)>,
    /// Diagnostics collected during MIR building (e.g., Unsupported HIR nodes).
    diagnostics: Vec<MirDiagnostic>,
    /// Per-named-type marker registry, cloned from the parent `HirModule` at
    /// builder construction. Read by every `ValueClass::of_ty` call site in
    /// MIR lowering so the marker is the single fact about whether a Named
    /// type participates in the ownership-discipline surface.
    type_classes: hew_hir::TypeClassTable,
    /// Lambda-actor capture ledger collected across every
    /// `HirExprKind::SpawnLambdaActor` literal in the function body.
    /// Drained into `ElaboratedMirFunction.lambda_captures` at the
    /// elaboration boundary; the structural fail-closed checker
    /// `validate_lambda_captures` runs against the drained list.
    lambda_captures: Vec<LambdaCapture>,
    /// `Some(LambdaActorHandle)` while the producer is lowering the
    /// value of `let <name> = actor |..| { .. }`. The `HirStmtKind::Let`
    /// arm pre-allocates the actor's local and records the
    /// `LambdaActorHandle(N)` here BEFORE lowering the value, so
    /// `lower_spawn_lambda_actor` reuses the slot the binding already
    /// owns instead of allocating a second local. The HIR forward-bind
    /// already routed the binding's resolved name to the lambda's own
    /// `BindingId`; this mirror at MIR keeps the binding's `Place`
    /// alignment to that same handle so a Weak self-capture's slot
    /// resolves correctly.
    pending_lambda_actor_handle: Option<Place>,
    /// Tuple decomposition map for runtime-call results that produce multiple
    /// output Places (e.g. `hew_duplex_pair` → two `DuplexHandle` slots).
    ///
    /// Key: the `u32` local index of the "tuple proxy" `Place::Local(N)` that
    /// `lower_runtime_call` returns for a multi-output call.  Value: the
    /// ordered slice of output Places in source-declaration order (e.g.
    /// `[DuplexHandle(N0), DuplexHandle(N1)]` for `duplex_pair`).
    ///
    /// `TupleIndex` lowering looks this map up when the tuple sub-expression
    /// resolves to a proxy local: index `i` into the value vec to obtain the
    /// concrete output Place without emitting additional instructions.
    ///
    /// SHIM(E2→E3): only `hew_duplex_pair` populates this map today.
    /// WHY: MIR has no multi-return instruction; a proxy local threads the
    ///   output Places through the existing single-`Place` `BindingRef` lookup.
    /// WHEN obsolete: when a dedicated MIR multi-return or projection surface
    ///   lands and `TupleIndex` lowering is rewritten to use it directly.
    /// WHAT: replace with `Place::Projection { base, index }` variant or a
    ///   `Terminator::Call`-style multi-dest encoding.
    tuple_decomp: HashMap<u32, Vec<Place>>,
    /// Declaration-order field descriptors for every `record` type in the module.
    ///
    /// Key: record type name (e.g. `"Point"`).
    /// Value: `(field_name, field_ty)` pairs in declaration order.
    ///
    /// Used by `StructInit` and `FieldAccess` lowering to resolve a field
    /// name to its 0-based `FieldOffset` and to look up the field type when
    /// allocating intermediate places for functional-update base reads.
    /// Built from `HirItem::Record` items in `lower_hir_module` and threaded
    /// through to the builder.
    ///
    /// Tuple records have an empty field list by design (`HirRecordDecl.fields`
    /// is empty for tuple records — their constructor is a `Call`, not a
    /// `StructInit`). They will never be looked up here.
    record_field_orders: HashMap<String, Vec<(String, ResolvedTy)>>,
    actor_layouts: HashMap<String, ActorLayout>,
    /// Supervisor-layout map, mirroring `actor_layouts` for supervisor types.
    /// Used by `lower_spawn_actor` to route `spawn Sup` to the supervisor
    /// bootstrap call and by `push_unknown_type_diagnostics` to recognise
    /// supervisor names inside `LocalPid<Sup>` type args as known. Empty for
    /// functions whose call context cannot reference supervisor types (actor
    /// handlers, closure shims).
    supervisor_layout_map: HashMap<String, crate::model::SupervisorLayout>,
    current_actor_state_fields: HashMap<String, (FieldOffset, ResolvedTy)>,
    /// Names of every user-defined function declared in the module. Used by
    /// `lower_value` `HirExprKind::Call` to distinguish user-fn callees
    /// (→ `Terminator::Call`) from runtime-ABI callees (→
    /// `Instr::CallRuntimeAbi`) and from indirect/closure callees
    /// (→ `NotYetImplemented`). Name-string matching is the reliable
    /// discriminator here because the HIR bridge does not yet emit
    /// `ResolvedRef::Item` for function-item callees (see the SHIM comment
    /// at the Call lowering arm). The set is populated once per module by
    /// `lower_hir_module` before any function body is lowered, so forward
    /// references (calling a function declared later in the file) are
    /// handled correctly.
    module_fn_names: HashSet<String>,
    /// Substitution map from origin-fn type-parameter symbols to
    /// concrete `ResolvedTy`s, populated only when this Builder is
    /// lowering a generic function under a specific monomorphisation.
    /// Empty for non-generic fn bodies.
    ///
    /// Every type observed during body lowering is substituted via
    /// `subst_ty` before reaching the backend Instr stream so symbolic
    /// type-parameter `Named` types never escape into MIR/codegen.
    subst: HashMap<String, ResolvedTy>,
    /// Per-call-site `Vec<ResolvedTy>` recorded by HIR lowering for
    /// generic top-level user-fn callees. Cloned from
    /// `HirModule.call_site_type_args`. The Call lowering arm
    /// substitutes these via `subst_ty` and dispatches to the
    /// per-monomorphisation mangled symbol.
    call_site_type_args: HashMap<hew_hir::SiteId, Vec<ResolvedTy>>,
    /// Per-`FieldAccess` site-id → `ChildSlot` side-table, populated by HIR
    /// lowering from the checker's `supervisor_child_slots`. The `FieldAccess`
    /// arm checks this map BEFORE the `record_field_orders` lookup so that
    /// supervisor-typed LHS is intercepted and routed to
    /// `hew_supervisor_child_get` rather than a record-field load.
    ///
    /// Cloned from `HirModule.supervisor_child_slots`. Empty for functions
    /// (actor-handler shims, closure shims) whose bodies cannot contain
    /// supervisor field accesses — the empty map causes the intercept arm to
    /// skip immediately, adding zero overhead for the common case.
    supervisor_child_slots: HashMap<hew_hir::SiteId, hew_types::ChildSlot>,
    current_task_scope: Option<Place>,
    current_function_symbol: String,
    next_closure_id: u32,
    generated_functions: Vec<LoweredFunction>,
    closure_record_layouts: Vec<crate::model::RecordLayout>,
    capture_env_sources: HashMap<BindingId, CaptureEnvSource>,
}

#[derive(Debug, Clone)]
struct CaptureEnvSource {
    env: Place,
    env_ty: ResolvedTy,
    field_offset: FieldOffset,
    ty: ResolvedTy,
}

impl Builder {
    /// Apply the per-monomorphisation substitution map to a type.
    /// Returns the input unchanged when `subst` is empty (the
    /// non-generic-function case).
    fn subst_ty(&self, ty: &ResolvedTy) -> ResolvedTy {
        if self.subst.is_empty() {
            return ty.clone();
        }
        hew_hir::lower::substitute_ty(ty, &self.subst)
    }
    /// Allocate one `Place::Local` per function parameter and register each
    /// in `binding_locals` so that `BindingRef` expressions in the function
    /// body resolve to a real slot.
    ///
    /// Must be called BEFORE `function_body`. The allocated locals occupy
    /// `locals[0..params.len()]`; all subsequent `alloc_local` calls
    /// produce indices ≥ `params.len()`, maintaining the invariant documented
    /// on `RawMirFunction.params`.
    fn lower_params(&mut self, func: &HirFn) {
        for param in &func.params {
            let slot = self.alloc_local(param.ty.clone());
            self.binding_locals.insert(param.id, slot);
        }
    }

    #[allow(
        clippy::needless_pass_by_value,
        reason = "alloc_local takes ty by value historically; substitution \
                  applies via subst_ty(&ty) without forcing every call site to \
                  introduce a borrow"
    )]
    fn alloc_local(&mut self, ty: ResolvedTy) -> Place {
        // u32::MAX locals per function is well beyond any realistic Hew
        // function size; the cast is bounded by `locals.len()` growing one
        // entry at a time within a single function-body walk.
        let id = u32::try_from(self.locals.len())
            .expect("function exceeds u32::MAX locals — impossible in Hew");
        // Substitute origin type-parameter symbols via the
        // per-monomorphisation substitution map before recording the
        // local's `ResolvedTy`. A no-op when `subst` is empty (non-
        // generic origin functions take this path with an identity
        // map).
        let resolved = self.subst_ty(&ty);
        self.locals.push(resolved);
        Place::Local(id)
    }

    /// Allocate a fresh `BasicBlock` id without switching the cursor.
    /// The caller invokes `finish_current_block(terminator)` to seal
    /// the current block, then `start_block(id)` to route subsequent
    /// `statements` / `instructions` into the new block.
    ///
    /// The very first `alloc_block` call returns id `1` because id `0`
    /// is reserved for the function's entry block (the cursor starts
    /// there at `Builder::default()`-time).
    #[allow(
        dead_code,
        reason = "Slice 1 declares cursor helpers; Slice 2 is the first caller"
    )]
    fn alloc_block(&mut self) -> u32 {
        // `next_block_id` starts at 0; bump to 1 the first time
        // `alloc_block` is called (id 0 is the entry block, allocated by
        // construction). After that, monotone increment.
        if self.next_block_id == 0 {
            self.next_block_id = 1;
        }
        let id = self.next_block_id;
        self.next_block_id = self
            .next_block_id
            .checked_add(1)
            .expect("function exceeds u32::MAX blocks — impossible in Hew");
        id
    }

    /// Seal the current basic block with `terminator` and move its
    /// statements + instructions into `pending_blocks`. The cursor is
    /// left at the just-sealed block's id; `start_block(new_id)` must
    /// be called before any further `statements.push` /
    /// `instructions.push` routes into the new block.
    #[allow(
        dead_code,
        reason = "Slice 1 declares cursor helpers; Slice 2 is the first caller"
    )]
    fn finish_current_block(&mut self, terminator: Terminator) {
        let block = BasicBlock {
            id: self.current_block_id,
            statements: std::mem::take(&mut self.statements),
            instructions: std::mem::take(&mut self.instructions),
            terminator,
        };
        self.pending_blocks.push(block);
    }

    /// Move the cursor to `id`. `statements` and `instructions` must be
    /// empty before this call — typically reached by following a
    /// `finish_current_block(...)` which drains both. The new id is
    /// recorded for the next `finish_current_block` call's
    /// `BasicBlock.id` payload.
    #[allow(
        dead_code,
        reason = "Slice 1 declares cursor helpers; Slice 2 is the first caller"
    )]
    fn start_block(&mut self, id: u32) {
        debug_assert!(
            self.statements.is_empty() && self.instructions.is_empty(),
            "start_block must follow finish_current_block; \
             current block has {} statements / {} instructions buffered",
            self.statements.len(),
            self.instructions.len(),
        );
        self.current_block_id = id;
    }

    /// Finalise the function's CFG by sealing the in-flight current
    /// block with the provided terminator. Returns the full
    /// `Vec<BasicBlock>` in id order. Slice 1 always returns a singleton
    /// because no caller invokes `finish_current_block`/`start_block`
    /// during the function-body walk; Slice 2's `If` lowering is the
    /// first writer to produce a non-trivial CFG here.
    fn finalize_blocks(&mut self, terminator: Terminator) -> Vec<BasicBlock> {
        let last = BasicBlock {
            id: self.current_block_id,
            statements: std::mem::take(&mut self.statements),
            instructions: std::mem::take(&mut self.instructions),
            terminator,
        };
        let mut blocks = std::mem::take(&mut self.pending_blocks);
        blocks.push(last);
        // Sort by id so consumers can index by position when they want
        // RPO-ish iteration. Construction order is already monotone
        // because `alloc_block` is monotone, so this is a no-op in
        // every Slice 1 callsite (single-block) and a stable order in
        // Slice 2+.
        blocks.sort_by_key(|b| b.id);
        blocks
    }

    fn function_body(&mut self, func: &HirFn) {
        for stmt in &func.body.statements {
            self.stmt(stmt);
        }
        if let Some(tail) = &func.body.tail {
            let value_place = self.lower_value(tail);
            self.decide(tail);
            self.mark_returned_binding_moved(tail);
            self.statements.push(MirStatement::Return {
                site: Some(tail.site),
                ty: self.subst_ty(&tail.ty),
            });
            // Backend stream: write the tail's value into the return slot.
            // If `lower_value` declined to produce a Place (an unsupported
            // construct in the spine subset), skip the move; the
            // `Unsupported` diagnostic already short-circuits the pipeline.
            if let Some(src) = value_place {
                self.instructions.push(Instr::Move {
                    dest: Place::ReturnSlot,
                    src,
                });
            }
        }
    }

    fn stmt(&mut self, stmt: &hew_hir::HirStmt) {
        match &stmt.kind {
            HirStmtKind::Let(binding, Some(value)) => {
                // Mirror the HIR forward-bind discipline at the MIR
                // layer for actor-lambda RHS. When the value is
                // `HirExprKind::SpawnLambdaActor`, pre-allocate the
                // binding's backend slot as a
                // `Place::LambdaActorHandle(N)` BEFORE lowering the
                // value. The body walk then sees a `BindingRef` to
                // the let-name resolve to a `binding_locals` entry
                // that already points at the actor's own handle;
                // the producer reuses this slot via
                // `pending_lambda_actor_handle` instead of allocating
                // a second local. Without this pre-allocation, a
                // Weak self-capture would try to look up a slot for
                // the let-binding that doesn't exist yet.
                let pending = if matches!(&value.kind, HirExprKind::SpawnLambdaActor { .. }) {
                    let slot = self.alloc_local(binding.ty.clone());
                    let Place::Local(local_id) = slot else {
                        unreachable!("alloc_local returns Place::Local");
                    };
                    let handle = Place::LambdaActorHandle(local_id);
                    self.binding_locals.insert(binding.id, handle);
                    self.pending_lambda_actor_handle = Some(handle);
                    true
                } else {
                    false
                };
                let value_place = self.lower_value(value);
                if pending {
                    self.pending_lambda_actor_handle = None;
                }
                self.decide(value);
                let binding_ty = self.subst_ty(&binding.ty);
                self.statements.push(MirStatement::Bind {
                    binding: binding.id,
                    name: binding.name.clone(),
                    site: value.site,
                    ty: binding_ty.clone(),
                });
                if ValueClass::of_ty(&binding_ty, &self.type_classes) != ValueClass::BitCopy {
                    self.owned_locals
                        .push((binding.id, binding.name.clone(), binding_ty.clone()));
                }
                // Backend stream: the binding owns a fresh local that the
                // initialiser's value is moved into. The pre-allocated
                // actor-lambda case already wired `binding_locals` and
                // does not need a second slot.
                if pending {
                    // The lambda-actor case: the producer already
                    // routed the binding to its `LambdaActorHandle`;
                    // no Move instruction is required (the handle is
                    // the value).
                } else if let Some(src) = value_place {
                    // Handle-typed places (DuplexHandle, SendHalf, RecvHalf,
                    // LambdaActorHandle) ARE the binding's backend slot —
                    // they carry ownership-discipline semantics through the
                    // Place kind itself.  Emitting a `Move { dest:
                    // Local(M), src: DuplexHandle(N) }` would store the
                    // handle in a generic Local, losing the kind information
                    // that `drop_kind_for` and `validate_cross_block_*` rely
                    // on (`drop_kind_for(Local(_)) → DropKind::Resource`).
                    // Register the handle Place directly in `binding_locals`
                    // without allocating a second local or emitting a Move.
                    match src {
                        Place::DuplexHandle(_)
                        | Place::SendHalf(_)
                        | Place::RecvHalf(_)
                        | Place::LambdaActorHandle(_)
                        | Place::ActorHandle(_) => {
                            self.binding_locals.insert(binding.id, src);
                        }
                        Place::Local(n) if self.tuple_decomp.contains_key(&n) => {
                            // Tuple-proxy: store the proxy directly so TupleIndex can recover
                            // element Places via tuple_decomp[n] — the existing Local-Move arm
                            // would allocate a fresh slot and lose the index that tuple_decomp
                            // is keyed by, leaving owned_locals entries without binding_locals.
                            self.binding_locals.insert(binding.id, src);
                        }
                        Place::Local(_) | Place::ReturnSlot => {
                            let slot = self.alloc_local(binding.ty.clone());
                            self.instructions.push(Instr::Move { dest: slot, src });
                            self.binding_locals.insert(binding.id, slot);
                        }
                    }
                }
            }
            HirStmtKind::Let(_, None) => {}
            HirStmtKind::Expr(expr) => {
                let _ = self.lower_value(expr);
                self.statements.push(MirStatement::Evaluate {
                    site: expr.site,
                    ty: self.subst_ty(&expr.ty),
                });
            }
            HirStmtKind::Assign { target, value } => {
                self.assign(target, value);
                self.statements.push(MirStatement::Evaluate {
                    site: value.site,
                    ty: ResolvedTy::Unit,
                });
            }
            HirStmtKind::Return(Some(expr)) => {
                let value_place = self.lower_value(expr);
                self.decide(expr);
                self.mark_returned_binding_moved(expr);
                self.statements.push(MirStatement::Return {
                    site: Some(expr.site),
                    ty: self.subst_ty(&expr.ty),
                });
                if let Some(src) = value_place {
                    self.instructions.push(Instr::Move {
                        dest: Place::ReturnSlot,
                        src,
                    });
                }
            }
            HirStmtKind::Return(None) => {
                self.statements.push(MirStatement::Return {
                    site: None,
                    ty: ResolvedTy::Unit,
                });
            }
        }
    }

    fn assign(&mut self, target: &HirExpr, value: &HirExpr) {
        let Some(src) = self.lower_value(value) else {
            return;
        };
        if let Some((field_offset, _)) = self.actor_state_field_for_target(target) {
            self.instructions
                .push(Instr::ActorStateFieldStore { field_offset, src });
            return;
        }
        match &target.kind {
            HirExprKind::BindingRef {
                resolved: ResolvedRef::Binding(binding),
                name,
                ..
            } => {
                if let Some(dest) = self.binding_locals.get(binding).copied() {
                    self.instructions.push(Instr::Move { dest, src });
                } else {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnresolvedPlace {
                            binding: *binding,
                            name: name.clone(),
                            site: target.site,
                        },
                        note: format!("assignment target binding {binding:?} has no MIR place"),
                    });
                }
            }
            _ => self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::UnsupportedNode {
                    reason:
                        "only local bindings and actor state fields are assignable in MIR slice 4"
                            .to_string(),
                },
                note: "assignment target did not lower to a writable place".to_string(),
            }),
        }
    }

    fn actor_state_field_for_target(&self, expr: &HirExpr) -> Option<(FieldOffset, ResolvedTy)> {
        match &expr.kind {
            HirExprKind::BindingRef { name, .. } => {
                self.current_actor_state_fields.get(name).cloned()
            }
            HirExprKind::FieldAccess { object, field } if is_self_expr(object) => {
                self.current_actor_state_fields.get(field).cloned()
            }
            _ => None,
        }
    }

    /// Walk an expression, emit checker-stream `MirStatement`s plus
    /// backend-stream `Instr`s, and return the `Place` that holds the
    /// expression's value (or `None` if the construct is outside the
    /// spine subset — a `MirDiagnostic` is recorded in that case).
    #[allow(
        clippy::too_many_lines,
        reason = "single large match on HirExprKind variants; each arm is a fail-closed \
                  boundary rule and splitting would obscure the exhaustiveness requirement"
    )]
    fn lower_value(&mut self, expr: &HirExpr) -> Option<Place> {
        self.decide(expr);
        match &expr.kind {
            HirExprKind::Literal(lit) => self.lower_literal(lit, &expr.ty, expr.site),
            HirExprKind::ContextReader { reader } => {
                let dest = self.alloc_local(self.subst_ty(&expr.ty));
                self.instructions.push(Instr::ContextField {
                    dest,
                    offset: context_reader_offset(*reader),
                });
                Some(dest)
            }
            HirExprKind::BindingRef {
                name,
                resolved: ResolvedRef::Binding(id),
            } => {
                if !self.binding_locals.contains_key(id) {
                    if let Some((field_offset, ty)) =
                        self.current_actor_state_fields.get(name).cloned()
                    {
                        let dest = self.alloc_local(ty);
                        self.instructions
                            .push(Instr::ActorStateFieldLoad { field_offset, dest });
                        return Some(dest);
                    }
                }
                let use_ty = self.subst_ty(&expr.ty);
                self.statements.push(MirStatement::Use {
                    binding: *id,
                    name: name.clone(),
                    site: expr.site,
                    ty: use_ty.clone(),
                    intent: expr.intent,
                });
                if expr.intent == IntentKind::Consume
                    && ValueClass::of_ty(&use_ty, &self.type_classes) != ValueClass::BitCopy
                {
                    self.mark_binding_moved(*id);
                }
                if let Some(source) = self.capture_env_sources.get(id).cloned() {
                    let dest = self.alloc_local(source.ty.clone());
                    self.instructions.push(Instr::ClosureEnvFieldLoad {
                        env: source.env,
                        env_ty: source.env_ty,
                        field_offset: source.field_offset,
                        dest,
                    });
                    return Some(dest);
                }
                let place = self.binding_locals.get(id).copied();
                if place.is_none() {
                    // Function parameters and other bindings without a
                    // backend slot are out of Cluster 1's spine. Without a
                    // Place, the emitter would silently load an
                    // uninitialised return slot — fail closed here.
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::UnresolvedPlace {
                            binding: *id,
                            name: name.clone(),
                            site: expr.site,
                        },
                        note: "binding has no backend slot in the Cluster 1 spine \
                               (function parameters and captured bindings are not \
                               yet lowered)"
                            .to_string(),
                    });
                }
                place
            }
            HirExprKind::BindingRef { .. } => None,
            HirExprKind::Binary { op, left, right } => {
                // Short-circuit logical operators must intercept BEFORE the rhs
                // is lowered: evaluating `right` unconditionally would break
                // the short-circuit contract (rhs side effects would run even
                // when lhs already determines the result).
                match op {
                    BinaryOp::And => return self.lower_logical_and(left, right, &expr.ty),
                    BinaryOp::Or => return self.lower_logical_or(left, right, &expr.ty),
                    _ => {}
                }
                let lhs = self.lower_value(left);
                let rhs = self.lower_value(right);
                match (lhs, rhs) {
                    (Some(lhs), Some(rhs)) => self.lower_binary(*op, lhs, rhs, &expr.ty, expr.site),
                    _ => None,
                }
            }
            HirExprKind::Call { callee, args } => {
                // SHIM(E2→checker): callee classification uses the callee name
                // string rather than a checker-resolved `ResolvedRef`.
                // WHY: the typecheck→HIR bridge (E1) emits `BindingRef { name:
                //   c_symbol, resolved: ResolvedRef::Unresolved }` for every
                //   runtime-symbol callee because the Rust MIR pipeline does not
                //   thread `TypeCheckOutput.method_call_rewrites` resolver IDs
                //   into HIR's `ResolvedRef`.  The name is the only available
                //   discriminator at MIR time. User functions are identified by
                //   membership in `module_fn_names` (collected in
                //   `lower_hir_module` before any body is lowered).
                // WHEN obsolete: when HIR emits `ResolvedRef::Item` for user-fn
                //   callees and `ResolvedRef::Builtin` for runtime callees so MIR
                //   can match on the resolved variant instead of name strings.
                // WHAT: replace with variant-based dispatch; remove the
                //   `is_known_runtime_symbol` and `module_fn_names` checks.
                let callee_name = match &callee.kind {
                    HirExprKind::BindingRef { name, .. } => Some(name.as_str()),
                    _ => None,
                };
                if let Some(name) = callee_name {
                    // Direct `hew_*` C-ABI name (from method-call rewrites).
                    if crate::runtime_symbols::is_known_runtime_symbol(name) {
                        return self.lower_runtime_call(name, args, expr.site);
                    }
                    // User-facing builtin name (e.g. `duplex_pair`) that maps
                    // to a C-ABI symbol. HIR emits the source name because
                    // checker-registered builtins do not appear in the AST
                    // function-item registry (see `runtime_symbols::user_name_to_c_symbol`
                    // for the shim rationale).
                    if let Some(c_sym) = crate::runtime_symbols::user_name_to_c_symbol(name) {
                        return self.lower_runtime_call(c_sym, args, expr.site);
                    }
                    // Generic top-level user fn: HIR recorded
                    // `call_site_type_args[expr.site]` with the type
                    // arguments observed at this call site (possibly
                    // including the enclosing fn's type-parameter
                    // symbols when this call is inside a generic
                    // body). Substitute via this Builder's monomorph
                    // substitution map and dispatch to the
                    // per-instantiation mangled symbol.
                    if let Some(type_args) = self.call_site_type_args.get(&expr.site).cloned() {
                        let substituted: Vec<ResolvedTy> =
                            type_args.iter().map(|t| self.subst_ty(t)).collect();
                        let mangled = hew_hir::monomorph::mangle(name, &substituted);
                        // If the mangled symbol is in `module_fn_names`,
                        // a per-instantiation MIR function was emitted
                        // by `lower_hir_module`; dispatch to it
                        // directly. Otherwise fall through to the
                        // unmangled lookup — the unspecialised origin
                        // is being lowered in a context where no
                        // monomorphisation was registered (e.g. when
                        // tests directly invoke `lower_hir_module`
                        // with a HirModule that bypassed the producer).
                        if self.module_fn_names.contains(&mangled) {
                            let ret_ty = self.subst_ty(&expr.ty);
                            return self.lower_direct_call(&mangled, args, &ret_ty, expr.site);
                        }
                    }
                    // User-defined function in the same module: emit a call terminator.
                    // The callee symbol is the bare function name as declared;
                    // codegen resolves it against the module's fn_symbols table.
                    if self.module_fn_names.contains(name) {
                        return self.lower_direct_call(name, args, &expr.ty, expr.site);
                    }
                }
                if matches!(
                    callee.kind,
                    HirExprKind::BindingRef {
                        resolved: ResolvedRef::Item(_),
                        ..
                    }
                ) {
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: "function call".to_string(),
                            site: expr.site,
                        },
                        note: "resolved callee has no MIR body or runtime lowering in the current cutover spine"
                            .to_string(),
                    });
                    return None;
                }
                if matches!(
                    callee.ty,
                    ResolvedTy::Function { .. } | ResolvedTy::Closure { .. }
                ) {
                    let callee_place = self.lower_value(callee)?;
                    let mut arg_places = Vec::with_capacity(args.len());
                    for arg in args {
                        arg_places.push(self.lower_value(arg)?);
                    }
                    let ret_ty = self.subst_ty(&expr.ty);
                    let dest = if matches!(ret_ty, ResolvedTy::Unit) {
                        None
                    } else {
                        Some(self.alloc_local(ret_ty.clone()))
                    };
                    self.instructions.push(Instr::CallClosure {
                        callee: callee_place,
                        args: arg_places,
                        ret_ty,
                        dest,
                    });
                    return dest;
                }
                // Indirect calls (closures, higher-order function values,
                // or unresolved bindings): not yet supported. Walk the children
                // so any Unsupported inside an argument still surfaces, then
                // fail closed so the emitter never sees a return slot with no
                // producer (LESSONS `boundary-fail-closed`).
                let _ = self.lower_value(callee);
                for arg in args {
                    let _ = self.lower_value(arg);
                }
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "indirect or unresolved function call".to_string(),
                        site: expr.site,
                    },
                    note: "only direct calls to module-declared user functions and \
                           runtime-ABI builtins are supported; indirect/closure/\
                           higher-order calls are not yet lowered"
                        .to_string(),
                });
                None
            }
            HirExprKind::Block(block) => {
                // Every nested statement reaches the checker-authority
                // stream via `self.stmt`, not just `HirStmtKind::Expr`.
                // Forwarding only `Expr` here would silently drop nested
                // `let` / `return` statements from a block expression and
                // let a real `UseAfterConsume` / `InitialisedBeforeUse`
                // pattern slip past the move-checker (fail-closed gap).
                // The HIR-Block-as-expression case recurses through this
                // arm — `If` / `StructInit` / `Call` / `Binary` lower
                // their nested expressions via `lower_value`, so a block
                // embedded in any of those forms reaches this arm and is
                // lowered the same way.
                for stmt in &block.statements {
                    self.stmt(stmt);
                }
                block.tail.as_ref().and_then(|tail| self.lower_value(tail))
            }
            HirExprKind::If {
                condition,
                then_expr,
                else_expr,
            } => self.lower_if(condition, then_expr, else_expr.as_deref(), &expr.ty),
            HirExprKind::StructInit {
                name, fields, base, ..
            } => {
                // Resolve the record-key for the field-order table. For a
                // generic record instantiation the HIR-recorded `expr.ty`
                // is `Named { name, args: <concrete> }` and the layout was
                // registered under the mangled name; for a monomorphic
                // record `args` is empty and the bare name is the key.
                let record_key = match &expr.ty {
                    ResolvedTy::Named { name: tname, args } if !args.is_empty() => {
                        hew_hir::mangle(tname, args)
                    }
                    _ => name.clone(),
                };
                // Look up the declaration-order field list for this record.
                // If it's missing, the checker allowed a type that was never
                // registered — fail closed rather than silently producing
                // malformed MIR.
                let field_order =
                    if let Some(order) = self.record_field_orders.get(record_key.as_str()) {
                        order.clone()
                    } else {
                        // Walk sub-expressions for checker-stream coverage.
                        for (_, fexpr) in fields {
                            let _ = self.lower_value(fexpr);
                        }
                        if let Some(base_expr) = base {
                            let _ = self.lower_value(base_expr);
                        }
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: format!(
                                    "record type `{name}` (not registered in field-order table)"
                                ),
                                site: expr.site,
                            },
                            note: "record type was not found in the field-order table; \
                               this is a checker bug (the type must be declared before use)"
                                .to_string(),
                        });
                        return None;
                    };

                // Lower each explicit field value to a Place, keyed by name.
                let mut explicit: HashMap<String, Place> = HashMap::new();
                for (fname, fexpr) in fields {
                    if let Some(place) = self.lower_value(fexpr) {
                        explicit.insert(fname.clone(), place);
                    }
                }

                // Lower the functional-update base, if any.
                let base_place: Option<Place> = if let Some(base_expr) = base {
                    self.lower_value(base_expr)
                } else {
                    None
                };

                // Build the (offset, source) pairs in declaration order.
                // For each field: use the explicit value if present; otherwise
                // emit a RecordFieldLoad from the base and use that intermediate.
                let mut field_pairs: Vec<(FieldOffset, Place)> = Vec::new();
                for (idx, (fname, fty)) in field_order.iter().enumerate() {
                    let offset = FieldOffset(
                        u32::try_from(idx)
                            .expect("record field count exceeds u32::MAX — impossible in Hew"),
                    );
                    if let Some(&src) = explicit.get(fname.as_str()) {
                        field_pairs.push((offset, src));
                    } else if let Some(base_rec) = base_place {
                        // Field absent from the explicit list — load it from base.
                        // The intermediate place carries the declared field type.
                        let intermediate = self.alloc_local(fty.clone());
                        self.instructions.push(Instr::RecordFieldLoad {
                            record: base_rec,
                            field_offset: offset,
                            dest: intermediate,
                        });
                        field_pairs.push((offset, intermediate));
                    } else {
                        // No explicit value and no base — checker should have
                        // rejected this; fail closed.
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: format!(
                                    "record `{name}` missing field `{fname}` with no functional-update base"
                                ),
                                site: expr.site,
                            },
                            note: "field absent from initialiser and no `..base` provided; \
                                   the checker should have rejected this program"
                                .to_string(),
                        });
                        return None;
                    }
                }

                let dest = self.alloc_local(expr.ty.clone());
                self.instructions.push(Instr::RecordInit {
                    ty: expr.ty.clone(),
                    fields: field_pairs,
                    dest,
                });
                Some(dest)
            }
            HirExprKind::FieldAccess { object, field } => {
                if is_self_expr(object) {
                    if let Some((field_offset, ty)) =
                        self.current_actor_state_fields.get(field).cloned()
                    {
                        let dest = self.alloc_local(ty);
                        self.instructions
                            .push(Instr::ActorStateFieldLoad { field_offset, dest });
                        return Some(dest);
                    }
                }

                // ── Supervisor child-accessor intercept (S2) ────────────────
                // Before falling through to the record-field path, check whether
                // this `FieldAccess` site was tagged by the checker as a
                // supervisor child accessor. The checker populates
                // `HirModule.supervisor_child_slots` (keyed by SiteId) for every
                // expression of the form `supervisor_expr.child_name`.
                //
                // Decision: option (b) — scratch-alloca + RecordFieldLoad.
                // A `CallRuntimeAbi` with a struct-typed dest (typed
                // `__HewChildLookupResult`) carries the 16-byte return value.
                // Two `RecordFieldLoad` instructions then extract `tag` (field 0)
                // and `handle` (field 1). Tag 0 (Live) → success path; tag != 0
                // → `Terminator::Trap { kind: TrapKind::SupervisorChildUnavailable }`.
                // No new `Instr` variant is required; the match-arm cascade cost
                // for S2 is zero lines.
                //
                // LESSONS P0 `boundary-fail-closed`: no path through this arm
                // reaches the `record_field_orders` lookup for supervisor-typed LHS.
                if let Some(slot) = self.supervisor_child_slots.get(&expr.site).cloned() {
                    match slot.kind {
                        ChildKind::Pool => {
                            // Pool children are not supported in v0.5; the
                            // dedicated `hew_supervisor_pool_route` ABI call
                            // lands in v0.6 when pool routing is fully designed.
                            // Discard the object expression to avoid misleading
                            // "unused value" diagnostics further up the chain.
                            let _ = self.lower_value(object);
                            self.diagnostics.push(MirDiagnostic {
                                kind: MirDiagnosticKind::NotYetImplemented {
                                    construct: "pool child accessor (v0.6)".to_string(),
                                    site: expr.site,
                                },
                                note: "pool child slot routing is not yet implemented; \
                                       use a static child or wait for v0.6"
                                    .to_string(),
                            });
                            return None;
                        }
                        ChildKind::Static => {
                            // Nested-supervisor result: when the RESULT of the
                            // field access (`expr.ty`) is `LocalPid<T>` where T
                            // is itself a supervisor with declared children, we
                            // would need `hew_supervisor_nested_get` (v0.6).
                            // This is distinct from the common case where the
                            // LHS is a supervisor and the result is an actor PID.
                            // We detect nesting on `expr.ty`, not `object.ty`
                            // (which is always `LocalPid<ParentSupervisor>`).
                            let is_nested = matches!(&expr.ty,
                                ResolvedTy::Named { name, args }
                                if name == "LocalPid"
                                    && args.len() == 1
                                    && matches!(&args[0],
                                        ResolvedTy::Named { name: inner, .. }
                                        if self.supervisor_layout_map.contains_key(inner.as_str()))
                            );
                            if is_nested {
                                let _ = self.lower_value(object);
                                self.diagnostics.push(MirDiagnostic {
                                    kind: MirDiagnosticKind::NotYetImplemented {
                                        construct: "nested supervisor child accessor (v0.6)"
                                            .to_string(),
                                        site: expr.site,
                                    },
                                    note: "multi-segment supervisor dotted access requires \
                                           `hew_supervisor_nested_get`, which lands in v0.6"
                                        .to_string(),
                                });
                                return None;
                            }

                            return self.lower_supervisor_child_get(
                                object, slot.index, &expr.ty, expr.site,
                            );
                        }
                    }
                }
                // ── End supervisor intercept ─────────────────────────────────

                // Resolve the record type key from the object's type so we
                // can look up the field offset in the field-order table.
                // For a generic record instantiation (`b: Box<i64>` reading
                // `b.value`) the key is the mangled name `Box$$i64`; for a
                // monomorphic record the key is the bare name.
                let type_name = match &object.ty {
                    ResolvedTy::Named { name, args } if !args.is_empty() => {
                        hew_hir::mangle(name, args)
                    }
                    ResolvedTy::Named { name, .. } => name.clone(),
                    other => {
                        let _ = self.lower_value(object);
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: format!("field access on non-named type `{other:?}`"),
                                site: expr.site,
                            },
                            note: "field access is only supported on named record types"
                                .to_string(),
                        });
                        return None;
                    }
                };
                let field_order =
                    if let Some(order) = self.record_field_orders.get(type_name.as_str()) {
                        order.clone()
                    } else {
                        let _ = self.lower_value(object);
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: format!(
                                    "field access on unregistered record type `{type_name}`"
                                ),
                                site: expr.site,
                            },
                            note: "record type was not found in the field-order table; \
                                   this is a checker bug"
                                .to_string(),
                        });
                        return None;
                    };
                let field_offset = if let Some(idx) =
                    field_order.iter().position(|(f, _)| f == field.as_str())
                {
                    FieldOffset(
                        u32::try_from(idx)
                            .expect("field index exceeds u32::MAX — impossible in Hew"),
                    )
                } else {
                    let _ = self.lower_value(object);
                    self.diagnostics.push(MirDiagnostic {
                        kind: MirDiagnosticKind::NotYetImplemented {
                            construct: format!("unknown field `{field}` on record `{type_name}`"),
                            site: expr.site,
                        },
                        note: "field not found in declaration-order table; \
                                   this is a checker bug"
                            .to_string(),
                    });
                    return None;
                };
                let record_place = self.lower_value(object)?;
                let dest = self.alloc_local(expr.ty.clone());
                self.instructions.push(Instr::RecordFieldLoad {
                    record: record_place,
                    field_offset,
                    dest,
                });
                Some(dest)
            }
            HirExprKind::Scope { body } => Some(self.lower_task_scope(body)),
            HirExprKind::SpawnedCall {
                callee,
                args,
                task_ty,
            } => self.lower_spawned_call_task(callee, args, task_ty, expr.site),
            HirExprKind::ForkBlock { body, .. } => self.lower_fork_block_task(body, expr.site),
            HirExprKind::ScopeDeadline { duration, body } => {
                self.lower_scope_deadline(duration, body, expr.site)
            }
            HirExprKind::AwaitTask {
                binding_id,
                output_ty,
                ..
            } => self.lower_await_task(*binding_id, output_ty, expr.site),
            HirExprKind::Select(select) => self.lower_select(select, &expr.ty, expr.site),
            HirExprKind::SpawnLambdaActor { .. } => {
                // The lambda-actor literal allocates a fresh local
                // (typed as the actor's Duplex<Msg, Reply>) and
                // surfaces it as a Place::LambdaActorHandle so drop
                // elaboration selects DropKind::LambdaActorRelease.
                // The HIR's resolved capture set is forwarded into
                // the function's lambda_captures ledger; the
                // structural checker validate_lambda_captures pins
                // the Weak-on-LambdaActorHandle invariants on the
                // emitted list. Codegen for the lambda body itself
                // lands in a follow-up slice (it fails closed on a
                // Place::LambdaActorHandle today).
                Some(self.lower_spawn_lambda_actor(expr))
            }
            HirExprKind::Spawn { actor_name, args } => {
                self.lower_spawn_actor(actor_name, args, expr)
            }
            HirExprKind::ActorSend {
                receiver,
                method_id,
                args,
            } => self.lower_actor_send(receiver, method_id, args, expr.site),
            HirExprKind::ActorAsk {
                receiver,
                method_id,
                args,
                reply_ty,
            } => self.lower_actor_ask(receiver, method_id, args, reply_ty, expr.site),
            HirExprKind::Closure {
                params,
                ret_ty,
                body,
                captures,
            } => self.lower_closure_literal(expr, params, ret_ty, body, captures),
            HirExprKind::TupleIndex { tuple, index } => {
                // Walk the inner tuple expression.  If the tuple sub-expression
                // resolves to a proxy local from a multi-output runtime call
                // (e.g. `hew_duplex_pair` populates `self.tuple_decomp`), return
                // the indexed DuplexHandle Place directly without emitting any
                // additional instructions.  This is the complement of the
                // `lower_runtime_call` path that stores the output Places into
                // `tuple_decomp`.
                let inner_place = self.lower_value(tuple)?;
                if let Place::Local(local_idx) = inner_place {
                    if let Some(parts) = self.tuple_decomp.get(&local_idx) {
                        if *index < parts.len() {
                            return Some(parts[*index]);
                        }
                    }
                }
                // General case: the tuple is a regular tuple-typed local.
                // Emit `Instr::TupleFieldLoad` — codegen lowers this to a
                // GEP at `field_index` into the struct alloca + load.
                let field_index = u32::try_from(*index)
                    .expect("tuple index exceeds u32::MAX — impossible in Hew");
                let dest = self.alloc_local(expr.ty.clone());
                self.instructions.push(Instr::TupleFieldLoad {
                    tuple: inner_place,
                    field_index,
                    dest,
                });
                Some(dest)
            }
            HirExprKind::Index { container, index } => {
                self.lower_vec_index(container, index, &expr.ty, expr.site)
            }
            HirExprKind::Slice {
                container,
                start,
                end,
                inclusive,
            } => self.lower_vec_slice(
                container,
                start.as_deref(),
                end.as_deref(),
                *inclusive,
                &expr.ty,
                expr.site,
            ),
            HirExprKind::IdentityCompare { left, right } => {
                // `lhs is rhs` — emit `Instr::IdentityCompare` so codegen can
                // select `ptrtoint` + `icmp eq` for pointer-shaped handles or
                // plain `icmp eq` for machine-id integers.  The dest is typed
                // `ResolvedTy::Bool` (inherited from `expr.ty`) so the i1
                // result widening path in codegen works the same as `IntCmp`.
                // LESSONS: `checker-authority` (P0) — the allowance set was
                // validated by the checker; we just lower the node.
                let lhs = self.lower_value(left)?;
                let rhs = self.lower_value(right)?;
                let dest = self.alloc_local(expr.ty.clone());
                self.instructions
                    .push(Instr::IdentityCompare { dest, lhs, rhs });
                Some(dest)
            }
            HirExprKind::CoerceToDynTrait {
                value,
                trait_name,
                concrete_type,
                method_table,
                vtable_entries,
            } => {
                // Materialise the concrete value into a Place, then emit
                // `Instr::CoerceToDynTrait` to construct the fat pointer.
                // The dest is typed `ResolvedTy::TraitObject` (inherited
                // from `expr.ty`), so codegen can pick the 2-word layout.
                let value_place = self.lower_value(value)?;
                let dest = self.alloc_local(expr.ty.clone());
                self.instructions.push(Instr::CoerceToDynTrait {
                    value: value_place,
                    dest,
                    trait_name: trait_name.clone(),
                    concrete_type: concrete_type.clone(),
                    method_table: method_table.clone(),
                    vtable_entries: vtable_entries.clone(),
                });
                Some(dest)
            }
            HirExprKind::CallDynMethod {
                receiver,
                trait_name,
                method_name,
                slot,
                args,
                ret_ty,
            } => {
                // Lower the receiver (a `dyn Trait` fat pointer) and the
                // ordinary args. `Instr::CallTraitMethod` GEPs into the
                // vtable at `slot`, loads the function pointer, and calls
                // it with `fat_pointer.data` as the implicit receiver —
                // codegen materialises the data-ptr argument from the
                // fat pointer, so the args list here is the source-level
                // args without the synthetic receiver entry.
                let fat_pointer = self.lower_value(receiver)?;
                let mut lowered_args: Vec<Place> = Vec::with_capacity(args.len());
                for arg in args {
                    lowered_args.push(self.lower_value(arg)?);
                }
                let dest = if matches!(ret_ty, ResolvedTy::Unit) {
                    None
                } else {
                    Some(self.alloc_local(ret_ty.clone()))
                };
                self.instructions.push(Instr::CallTraitMethod {
                    fat_pointer,
                    dest,
                    trait_name: trait_name.clone(),
                    method_name: method_name.clone(),
                    slot: *slot,
                    args: lowered_args,
                });
                dest
            }
            HirExprKind::MachineEmit { .. } => {
                // `emit` expressions are valid HIR (Slice 2), but MIR
                // lowering for machine bodies is deferred to Lane B Slice 4b.
                // Fail closed so that a machine body reaching MIR via an
                // unanticipated path is rejected rather than silently
                // producing no-op code.
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::UnsupportedNode {
                        reason: "MachineEmit (Lane B Slice 4b not yet wired)".to_string(),
                    },
                    note: "machine emit expressions are not yet lowered to MIR".to_string(),
                });
                None
            }
            HirExprKind::Unsupported(reason) => {
                // Defense-in-depth: HIR lowering should have emitted
                // NotYetImplemented and the driver should have stopped
                // before reaching MIR. Emit a MirDiagnostic so the pipeline
                // is still rejected if somehow the gate was bypassed.
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::UnsupportedNode {
                        reason: reason.clone(),
                    },
                    note: "HIR Unsupported node reached MIR lowering; \
                           NotYetImplemented should have been caught earlier"
                        .to_string(),
                });
                None
            }
        }
    }

    fn lower_literal(
        &mut self,
        lit: &HirLiteral,
        ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        // All HirLiteral variants are wired. Each arm allocates a dest local,
        // pushes the corresponding Instr, and returns early with `Some(dest)`.
        // Fail-closed behaviour (LESSONS `boundary-fail-closed`) is preserved
        // through the float arm's type-mismatch guard, which still returns
        // `None` on checker-invariant violations.
        match lit {
            HirLiteral::Integer(value) => {
                let dest = self.alloc_local(ty.clone());
                self.instructions.push(Instr::ConstI64 {
                    dest,
                    value: *value,
                });
                Some(dest)
            }
            HirLiteral::Bool(value) => {
                // Bool lowers as an integer truth value (1 / 0) into the
                // dest local's natural width. The dest local's type is
                // whatever HIR resolved for the literal — `ResolvedTy::Bool`
                // on this base, which the codegen maps to i8. The
                // `ConstI64.value` is fed through the same store path as
                // ConstI64 for integer literals; `Instr::ConstI64`'s
                // emitter already truncates to the dest local's width.
                let dest = self.alloc_local(ty.clone());
                self.instructions.push(Instr::ConstI64 {
                    dest,
                    value: i64::from(*value),
                });
                Some(dest)
            }
            HirLiteral::Float(value) => {
                // `HirLiteral::Float` always carries an `f64` regardless of
                // the declared type. When the resolved type is `f32`, narrow
                // to single precision before encoding as a bit pattern so the
                // constant round-trips exactly through the MIR → codegen boundary.
                // Storing as bits avoids a floating-point field in the MIR model
                // (which would need special PartialEq treatment for NaN) while
                // keeping the round-trip exact (mirrors `ConstI64.value`).
                let (value_bits, width) = match ty {
                    ResolvedTy::F32 => {
                        // Narrow to f32 before encoding — f64 bits for a value
                        // that will be stored in an f32 slot would be wrong.
                        #[allow(
                            clippy::cast_possible_truncation,
                            reason = "literal coercion from f64 source value to f32 slot is \
                                      the intended semantics; checker accepted the source as \
                                      f32, so any precision loss is the developer's call"
                        )]
                        let narrowed = *value as f32;
                        (u64::from(narrowed.to_bits()), FloatWidth::F32)
                    }
                    ResolvedTy::F64 => (value.to_bits(), FloatWidth::F64),
                    _ => {
                        // Type mismatch: float literal with non-float resolved
                        // type is a checker bug. Fail closed per LESSONS
                        // `boundary-fail-closed`.
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: "float literal with non-float resolved type".to_string(),
                                site,
                            },
                            note: "HirLiteral::Float reached MIR lowering with a \
                                   non-float resolved type — checker invariant violated"
                                .to_string(),
                        });
                        return None;
                    }
                };
                let dest = self.alloc_local(ty.clone());
                self.instructions.push(Instr::FloatLit {
                    dest,
                    value_bits,
                    width,
                });
                Some(dest)
            }
            HirLiteral::String(s) => {
                // String literal lowering: allocate a `ResolvedTy::String`
                // local (an opaque pointer at the LLVM level) and emit
                // `Instr::StringLit` to fill it. The codegen emitter will
                // produce an LLVM global constant for the bytes + a pointer
                // store into the dest alloca — matching the C++ codegen's
                // `hew.global_string` / `llvm.mlir.addressof` pattern.
                //
                // Escape decoding: the parser's `unescape_string` already
                // ran; `s` is a decoded Rust String and `as_bytes()` gives
                // the correct UTF-8 byte sequence.
                let dest = self.alloc_local(ty.clone());
                self.instructions.push(Instr::StringLit {
                    bytes: s.as_bytes().to_vec(),
                    dest,
                });
                Some(dest)
            }
            HirLiteral::Char(c) => {
                // Hew `char` is a Unicode scalar value. Store as `u32` bit
                // pattern; codegen maps it to an `i32` constant. The cast is
                // total — Rust's `char` guarantees scalar-value range.
                let dest = self.alloc_local(ty.clone());
                self.instructions.push(Instr::CharLit {
                    value: *c as u32,
                    dest,
                });
                Some(dest)
            }
            HirLiteral::Unit => {
                // Unit is zero-sized; codegen may emit nothing. The dest
                // place is allocated so that any downstream use-after-consume
                // tracking has a definition point.
                //
                // NOTE: `HirLiteral::Unit` is currently unreachable from
                // real Hew source — no parser `Literal::Unit` exists and
                // the HIR lowerer does not produce this variant. This arm
                // exists for exhaustiveness so a future producer has a
                // corresponding MIR variant.
                let dest = self.alloc_local(ty.clone());
                self.instructions.push(Instr::UnitLit { dest });
                Some(dest)
            }
            HirLiteral::Duration(nanos) => {
                // Duration literals carry nanoseconds already (`i64`) from
                // parse time. Forward directly — no conversion needed.
                let dest = self.alloc_local(ty.clone());
                self.instructions.push(Instr::DurationLit {
                    nanos: *nanos,
                    dest,
                });
                Some(dest)
            }
        }
    }

    #[allow(
        clippy::too_many_lines,
        reason = "lower_binary is a flat dispatch over the BinaryOp enum; line count grows \
                  with the operator set (i64 + float arms). Splitting would obscure the \
                  per-operator codegen path each reader expects to find here."
    )]
    fn lower_binary(
        &mut self,
        op: BinaryOp,
        lhs: Place,
        rhs: Place,
        ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let dest = self.alloc_local(ty.clone());
        // Comparison binops: lower to `Instr::IntCmp` with a `CmpPred`
        // discriminator. The result Place is allocated to whatever type
        // HIR resolved for the expression (`ResolvedTy::Bool` for cmp
        // ops); codegen widens the LLVM `i1` cmp result to the dest's
        // stored width on the way to the store. Without this arm,
        // `if 1 == 1 { ... }` cannot construct a condition Place for
        // CFG-construction-lane `If` lowering — the boolean-condition
        // pre-requisite called out by the cluster plan §1 / Slice 0.
        let cmp_pred = match op {
            BinaryOp::Equal => Some(CmpPred::Eq),
            BinaryOp::NotEqual => Some(CmpPred::NotEq),
            BinaryOp::Less => Some(CmpPred::SignedLess),
            BinaryOp::LessEqual => Some(CmpPred::SignedLessEq),
            BinaryOp::Greater => Some(CmpPred::SignedGreater),
            BinaryOp::GreaterEqual => Some(CmpPred::SignedGreaterEq),
            _ => None,
        };
        if let Some(pred) = cmp_pred {
            self.instructions.push(Instr::IntCmp {
                dest,
                pred,
                lhs,
                rhs,
            });
            return Some(dest);
        }
        // B-4 wrapping arithmetic: `&+` / `&-` / `&*` lower to plain
        // two's-complement `IntAdd` / `IntSub` / `IntMul` — no overflow
        // flag, no CFG split, no Trap block. These are the first source-
        // level producers of `Instr::IntAdd/IntSub/IntMul`; previously
        // those variants were reachable only from hand-built fixtures.
        // LESSONS `boundary-fail-closed` (P0): the user has explicitly
        // opted into modular arithmetic by writing `&+`; no trap is the
        // correct behaviour here.
        let wrapping_instr = match op {
            BinaryOp::WrappingAdd => Some(Instr::IntAdd { dest, lhs, rhs }),
            BinaryOp::WrappingSub => Some(Instr::IntSub { dest, lhs, rhs }),
            BinaryOp::WrappingMul => Some(Instr::IntMul { dest, lhs, rhs }),
            _ => None,
        };
        if let Some(instr) = wrapping_instr {
            self.instructions.push(instr);
            return Some(dest);
        }

        // B-5 divide / modulo / shift lowering.
        //
        // These operators are handled here with early returns so they
        // don't fall through to the B-2 overflow-trap `IntArithChecked`
        // path below (which is only for `+`/`-`/`*`).
        match op {
            BinaryOp::Divide | BinaryOp::Modulo => {
                return self.lower_div_rem(op, dest, lhs, rhs, ty, site);
            }
            BinaryOp::Shl | BinaryOp::Shr => {
                return self.lower_shift(op, dest, lhs, rhs, ty, site);
            }
            _ => {}
        }

        // Bitwise operators: well-defined for any integer width × signedness.
        // No traps, no overflow checks — emit a single instruction directly.
        let bitwise_instr = match op {
            BinaryOp::BitAnd => Some(Instr::IntBitAnd { dest, lhs, rhs }),
            BinaryOp::BitOr => Some(Instr::IntBitOr { dest, lhs, rhs }),
            BinaryOp::BitXor => Some(Instr::IntBitXor { dest, lhs, rhs }),
            _ => None,
        };
        if let Some(instr) = bitwise_instr {
            self.instructions.push(instr);
            return Some(dest);
        }

        let arith_op = match op {
            BinaryOp::Add => IntArithOp::Add,
            BinaryOp::Subtract => IntArithOp::Sub,
            BinaryOp::Multiply => IntArithOp::Mul,
            // The spine subset still rejects range / send / regex binops.
            // Previously this arm silently popped the dest local and returned
            // `None`, letting the parent expression succeed with a missing
            // producer (quiet fail-soft — caller's `decide` ran,
            // `MirDiagnostic` did not). Fail closed now: drop the dest local,
            // emit a `NotYetImplemented` so the CLI rejection surface sees
            // the offending construct, and return `None`.
            // LESSONS `boundary-fail-closed`.
            _ => {
                self.locals.pop();
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!("binary operator `{op}`"),
                        site,
                    },
                    note: "binary operator is recognised by HIR but not yet lowered \
                           to the backend instruction stream"
                        .to_string(),
                });
                return None;
            }
        };
        // Float `+` / `-` / `*`: emit `Instr::Float{Add,Sub,Mul}` directly —
        // no trap blocks, no overflow flag. IEEE 754 overflow produces
        // ±inf, not a runtime trap.
        if let Some(width) = float_width(ty) {
            let float_instr = match arith_op {
                IntArithOp::Add => Instr::FloatAdd {
                    dest,
                    lhs,
                    rhs,
                    width,
                },
                IntArithOp::Sub => Instr::FloatSub {
                    dest,
                    lhs,
                    rhs,
                    width,
                },
                IntArithOp::Mul => Instr::FloatMul {
                    dest,
                    lhs,
                    rhs,
                    width,
                },
            };
            self.instructions.push(float_instr);
            return Some(dest);
        }

        // B-2 overflow-trap lowering. The default `+` / `-` / `*` on
        // integer types lowers to the checked LLVM intrinsic family
        // (`llvm.{s,u}{add,sub,mul}.with.overflow.iN`) with a hard
        // `Terminator::Trap { kind: TrapKind::IntegerOverflow }` on
        // the overflow path and a continuation block on the success
        // path. The MIR-level CFG split — current block ends with a
        // `Branch` on the overflow flag, with a trap block and a
        // continuation block as successors — is what makes the trap
        // visible to drop elaboration, the cross-block dataflow pass,
        // and every other MIR consumer (instead of being a codegen-
        // only emission). LESSONS `boundary-fail-closed` (P0 —
        // default arithmetic IS the boundary; trap-on-overflow is
        // fail-closed for accidental overflow).
        let Some(signed) = integer_signedness(ty) else {
            // Non-integer, non-float reaching `+` / `-` / `*` is a
            // B-1 mixed-width or unsupported-type violation upstream.
            // Fail closed rather than emit unchecked arithmetic.
            self.locals.pop();
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("binary operator `{op}` on non-integer, non-float type"),
                    site,
                },
                note: "overflow-trap lowering requires an integer-typed result \
                       (i8/i16/i32/i64/u8/u16/u32/u64/isize/usize)"
                    .to_string(),
            });
            return None;
        };
        // Allocate the overflow-flag local as a bool. Codegen widens
        // the i1 returned by `extractvalue` to the i8 backing slot.
        let overflow_flag = self.alloc_local(ResolvedTy::Bool);
        self.instructions.push(Instr::IntArithChecked {
            op: arith_op,
            signed,
            dest,
            lhs,
            rhs,
            overflow_flag,
        });
        // Seal the current block with a Branch on the overflow flag.
        // Then-target is the trap block; else-target is the
        // continuation block that subsequent lowering writes into.
        let trap_bb = self.alloc_block();
        let cont_bb = self.alloc_block();
        self.finish_current_block(Terminator::Branch {
            cond: overflow_flag,
            then_target: trap_bb,
            else_target: cont_bb,
        });
        // Trap block: a single Terminator::Trap with no instructions.
        self.start_block(trap_bb);
        self.finish_current_block(Terminator::Trap {
            kind: TrapKind::IntegerOverflow,
        });
        // Continuation block: the cursor lands here so the parent
        // expression's caller can keep emitting into the success path.
        self.start_block(cont_bb);
        Some(dest)
    }

    /// Lower integer `/` and `%` with divide-by-zero and (for signed
    /// types) signed-MIN/-1 trap guards.
    ///
    /// CFG shape:
    ///
    /// ```text
    /// entry_bb (current)
    ///   IntCmp { pred: Eq, dest: zero_flag, lhs: rhs, rhs: const_0 }
    ///   Branch { cond: zero_flag, then: dbz_trap_bb, else: after_zero_bb }
    ///
    /// dbz_trap_bb
    ///   Trap { kind: DivideByZero }
    ///
    /// after_zero_bb  [signed only]
    ///   IntCmp { pred: Eq, dest: min_flag, lhs: lhs, rhs: const_MIN }
    ///   Branch { cond: min_flag, then: min_check_bb, else: div_bb }
    ///
    /// min_check_bb   [signed only]
    ///   IntCmp { pred: Eq, dest: negone_flag, lhs: rhs, rhs: const_NEG1 }
    ///   Branch { cond: negone_flag, then: smno_trap_bb, else: div_bb }
    ///
    /// smno_trap_bb   [signed only]
    ///   Trap { kind: SignedMinDivNegOne }
    ///
    /// div_bb
    ///   IntDiv / IntRem { dest, lhs, rhs }
    ///   [cursor stays here for subsequent lowering]
    /// ```
    ///
    /// For unsigned types the after-zero block is `div_bb` directly.
    ///
    /// `dest` must already be allocated by the caller (`lower_binary`
    /// allocates it before dispatching here).
    #[allow(
        clippy::too_many_arguments,
        reason = "all arguments are structurally required: the builder state \
                  (&mut self), the opcode discriminator (op), the pre-allocated \
                  destination place (dest), both operand places (lhs, rhs), the \
                  result type (ty) for constant-emission width, and the site id \
                  for diagnostics. There is no natural grouping that reduces this."
    )]
    #[allow(
        clippy::too_many_lines,
        reason = "the function implements a single coherent CFG-emission \
                  pattern (zero-check → MIN/-1 check → div/rem) that must \
                  stay in one place for readability; extracting sub-steps \
                  would require passing more builder state around."
    )]
    fn lower_div_rem(
        &mut self,
        op: BinaryOp,
        dest: Place,
        lhs: Place,
        rhs: Place,
        ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        // Float `/` and `%`: emit `Instr::FloatDiv` / `Instr::FloatRem`
        // directly. IEEE 754 defines `x / 0.0` → ±inf and `x % 0.0` →
        // NaN — neither traps. Do NOT add a zero-check CFG split here.
        if let Some(width) = float_width(ty) {
            let float_instr = match op {
                BinaryOp::Divide => Instr::FloatDiv {
                    dest,
                    lhs,
                    rhs,
                    width,
                },
                BinaryOp::Modulo => Instr::FloatRem {
                    dest,
                    lhs,
                    rhs,
                    width,
                },
                _ => unreachable!("lower_div_rem called with non-div/rem op"),
            };
            self.instructions.push(float_instr);
            return Some(dest);
        }

        let Some(signed) = integer_signedness(ty) else {
            // Non-integer, non-float reaching `/` or `%` — B-1 violation upstream.
            self.locals.pop();
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("binary operator `{op}` on non-integer, non-float type"),
                    site,
                },
                note: "div/rem trap lowering requires an integer-typed result".to_string(),
            });
            return None;
        };

        // ── divide-by-zero check ────────────────────────────────────
        let zero_const = self.alloc_local(ty.clone());
        self.instructions.push(Instr::ConstI64 {
            dest: zero_const,
            value: 0,
        });
        let zero_flag = self.alloc_local(ResolvedTy::Bool);
        self.instructions.push(Instr::IntCmp {
            dest: zero_flag,
            pred: CmpPred::Eq,
            lhs: rhs,
            rhs: zero_const,
        });
        let dbz_trap_bb = self.alloc_block();
        let after_zero_bb = self.alloc_block();
        self.finish_current_block(Terminator::Branch {
            cond: zero_flag,
            then_target: dbz_trap_bb,
            else_target: after_zero_bb,
        });

        self.start_block(dbz_trap_bb);
        self.finish_current_block(Terminator::Trap {
            kind: TrapKind::DivideByZero,
        });

        self.start_block(after_zero_bb);

        // ── signed-MIN / -1 check (signed types only) ───────────────
        if signed == IntSignedness::Signed {
            let Some(min_val) = signed_min_value(ty) else {
                // isize: platform-sized, MIN not knowable at MIR time.
                // Fail closed rather than emit an incorrect guard.
                self.locals.pop();
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!("binary operator `{op}` on `isize`"),
                        site,
                    },
                    note: "signed-MIN/-1 trap for `isize` requires target-width \
                           information not available at MIR construction time. \
                           WHEN-OBSOLETE: when IrPipeline carries a TargetSpec, \
                           re-wire to emit the correct per-target MIN constant."
                        .to_string(),
                });
                return None;
            };
            let min_const = self.alloc_local(ty.clone());
            self.instructions.push(Instr::ConstI64 {
                dest: min_const,
                value: min_val,
            });
            let min_flag = self.alloc_local(ResolvedTy::Bool);
            self.instructions.push(Instr::IntCmp {
                dest: min_flag,
                pred: CmpPred::Eq,
                lhs,
                rhs: min_const,
            });
            let min_check_bb = self.alloc_block();
            let div_bb = self.alloc_block();
            self.finish_current_block(Terminator::Branch {
                cond: min_flag,
                then_target: min_check_bb,
                else_target: div_bb,
            });

            // min_check_bb: check whether rhs == -1
            self.start_block(min_check_bb);
            let negone_const = self.alloc_local(ty.clone());
            self.instructions.push(Instr::ConstI64 {
                dest: negone_const,
                value: -1,
            });
            let negone_flag = self.alloc_local(ResolvedTy::Bool);
            self.instructions.push(Instr::IntCmp {
                dest: negone_flag,
                pred: CmpPred::Eq,
                lhs: rhs,
                rhs: negone_const,
            });
            let smno_trap_bb = self.alloc_block();
            self.finish_current_block(Terminator::Branch {
                cond: negone_flag,
                then_target: smno_trap_bb,
                else_target: div_bb,
            });

            self.start_block(smno_trap_bb);
            self.finish_current_block(Terminator::Trap {
                kind: TrapKind::SignedMinDivNegOne,
            });

            self.start_block(div_bb);
        }

        // ── div / rem instruction on the safe path ──────────────────
        match op {
            BinaryOp::Divide => self.instructions.push(Instr::IntDiv {
                signed,
                dest,
                lhs,
                rhs,
            }),
            BinaryOp::Modulo => self.instructions.push(Instr::IntRem {
                signed,
                dest,
                lhs,
                rhs,
            }),
            _ => unreachable!("lower_div_rem called only for Divide / Modulo"),
        }
        Some(dest)
    }

    /// Lower `<<` and `>>` with a shift-out-of-range trap guard.
    ///
    /// The range check uses an unsigned ≥ compare on the shift count:
    ///   `(count as unsigned) >= bit_width(T)`
    /// This single compare catches both negative counts (which become
    /// large unsigned values after reinterpretation) and counts ≥ the
    /// type's width.
    ///
    /// `isize`/`usize` are rejected with `NotYetImplemented` because
    /// the bit-width is not statically known at MIR time (see
    /// `integer_bit_width` for the documented why / when-obsolete).
    ///
    /// CFG shape:
    /// ```text
    /// entry_bb (current)
    ///   ConstI64 { dest: width_const, value: bit_width }
    ///   IntCmp { pred: UnsignedGreaterEq, dest: oor_flag,
    ///            lhs: rhs (shift count), rhs: width_const }
    ///   Branch { cond: oor_flag, then: sor_trap_bb, else: shift_bb }
    ///
    /// sor_trap_bb
    ///   Trap { kind: ShiftOutOfRange }
    ///
    /// shift_bb
    ///   IntShl / IntShr { dest, lhs, rhs }
    ///   [cursor stays here]
    /// ```
    fn lower_shift(
        &mut self,
        op: BinaryOp,
        dest: Place,
        lhs: Place,
        rhs: Place,
        ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let Some(signed) = integer_signedness(ty) else {
            self.locals.pop();
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("binary operator `{op}` on non-integer type"),
                    site,
                },
                note: "shift trap lowering requires an integer-typed operand".to_string(),
            });
            return None;
        };

        let Some(width) = integer_bit_width(ty) else {
            // isize / usize: width not knowable at MIR time.
            self.locals.pop();
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("binary operator `{op}` on `isize`/`usize`"),
                    site,
                },
                note: "shift-range trap for `isize`/`usize` requires target-width \
                       information not available at MIR construction time. \
                       WHEN-OBSOLETE: when IrPipeline carries a TargetSpec, \
                       re-wire to emit the correct per-target width constant."
                    .to_string(),
            });
            return None;
        };

        // ── out-of-range check: (count as unsigned) >= width ────────
        let width_const = self.alloc_local(ty.clone());
        self.instructions.push(Instr::ConstI64 {
            dest: width_const,
            value: width,
        });
        let oor_flag = self.alloc_local(ResolvedTy::Bool);
        self.instructions.push(Instr::IntCmp {
            dest: oor_flag,
            pred: CmpPred::UnsignedGreaterEq,
            lhs: rhs, // shift count
            rhs: width_const,
        });
        let sor_trap_bb = self.alloc_block();
        let shift_bb = self.alloc_block();
        self.finish_current_block(Terminator::Branch {
            cond: oor_flag,
            then_target: sor_trap_bb,
            else_target: shift_bb,
        });

        self.start_block(sor_trap_bb);
        self.finish_current_block(Terminator::Trap {
            kind: TrapKind::ShiftOutOfRange,
        });

        self.start_block(shift_bb);

        // ── shift instruction on the safe path ──────────────────────
        match op {
            BinaryOp::Shl => self.instructions.push(Instr::IntShl { dest, lhs, rhs }),
            BinaryOp::Shr => self.instructions.push(Instr::IntShr {
                signed,
                dest,
                lhs,
                rhs,
            }),
            _ => unreachable!("lower_shift called only for Shl / Shr"),
        }
        Some(dest)
    }

    /// Lower an `If` expression into a real CFG with a `Branch`
    /// terminator on the entry block, separate `then` / `else` blocks
    /// each terminated by a `Goto join_bb`, and a join block that
    /// receives the result value.
    ///
    /// The expression's value Place is a result-local *alloca'd before
    /// the branch* — when each arm finishes lowering its tail
    /// expression, the arm emits an `Instr::Move { dest: result_local,
    /// src: arm_value }` before the `Goto`. The join block then loads
    /// the value through the result local. This matches the existing
    /// alloca-per-local pattern (`alloc_local`) and the codegen's
    /// `place_pointer` lookup (each Place is a stack slot); LLVM's
    /// mem2reg pass promotes the alloca to SSA at the LLVM layer if
    /// the optimiser sees fit. Phi at MIR is a v0.6 refactor
    /// (`R-CFG-V06-phi`).
    ///
    /// `else_expr: None` reaches here when the HIR types the If as
    /// `ResolvedTy::Unit` (no else block). The else arm is still
    /// emitted as a block that just `Goto join` — no Move, no value
    /// written to `result_place`. Downstream code that loads from
    /// `result_place` on the else path observes whatever the alloca
    /// was initialised with (LLVM `undef` for an i8 unit-stand-in,
    /// inconsequential because Unit's value is by definition never
    /// observed). No special fail-closed needed.
    fn lower_if(
        &mut self,
        condition: &HirExpr,
        then_expr: &HirExpr,
        else_expr: Option<&HirExpr>,
        result_ty: &ResolvedTy,
    ) -> Option<Place> {
        // Result local first, so it dominates every branch arm's Move.
        // Allocated even for Unit Ifs to keep a single Place-shape
        // contract on the value-bearing return; codegen never loads a
        // Unit result so the placeholder's initial value is unused.
        let result_place = self.alloc_local(result_ty.clone());

        // Lower the condition in the entry (current) block. Receive a
        // Place holding the truth value; codegen's `Terminator::Branch`
        // emitter loads it and compares non-zero.
        // Condition lowering failed (NotYetImplemented or similar) —
        // propagate by returning None via `?`. The diagnostic already
        // lives on `self.diagnostics`, so the CLI rejects the program;
        // the half-built If does not need to seal the current block.
        // Leaving the result_local dangling is benign — no Branch/Goto
        // refers to it.
        let cond_place = self.lower_value(condition)?;

        // Allocate the three CFG blocks: then arm, else arm, join.
        let then_bb = self.alloc_block();
        let else_bb = self.alloc_block();
        let join_bb = self.alloc_block();

        // Seal the entry block with a Branch on the cond Place.
        self.finish_current_block(Terminator::Branch {
            cond: cond_place,
            then_target: then_bb,
            else_target: else_bb,
        });

        // Then arm.
        self.start_block(then_bb);
        let then_value = self.lower_value(then_expr);
        if let Some(src) = then_value {
            self.instructions.push(Instr::Move {
                dest: result_place,
                src,
            });
        }
        self.finish_current_block(Terminator::Goto { target: join_bb });

        // Else arm. `else_expr: None` (the HIR-types-as-Unit case)
        // emits a Goto-only block — no Move, no value contributed.
        self.start_block(else_bb);
        if let Some(else_expr) = else_expr {
            let else_value = self.lower_value(else_expr);
            if let Some(src) = else_value {
                self.instructions.push(Instr::Move {
                    dest: result_place,
                    src,
                });
            }
        }
        self.finish_current_block(Terminator::Goto { target: join_bb });

        // Join. Subsequent lowering continues in this block; the If
        // expression's value Place is the result_local (loads happen
        // through the same Place that both arms wrote into).
        self.start_block(join_bb);
        Some(result_place)
    }

    /// Lower `lhs && rhs` with short-circuit semantics.
    ///
    /// CFG shape:
    ///
    /// ```text
    /// entry_bb (current):
    ///   result_place = false          // pessimistic default
    ///   lhs_place = lower(lhs)
    ///   Branch { cond: lhs_place, then: rhs_bb, else: join_bb }
    ///
    /// rhs_bb:
    ///   rhs_place = lower(rhs)
    ///   Move { dest: result_place, src: rhs_place }
    ///   Goto join_bb
    ///
    /// join_bb:
    ///   -- result_place holds the final bool --
    /// ```
    ///
    /// The rhs block is only entered when lhs is true, so rhs side effects
    /// are correctly guarded. On the false path, `result_place` retains the
    /// `false` constant written in the entry block.
    fn lower_logical_and(
        &mut self,
        lhs_expr: &HirExpr,
        rhs_expr: &HirExpr,
        result_ty: &ResolvedTy,
    ) -> Option<Place> {
        let result_place = self.alloc_local(result_ty.clone());
        // Write `false` as the pessimistic default (the join block reads
        // result_place, and the else path never writes to it).
        self.instructions.push(Instr::ConstI64 {
            dest: result_place,
            value: 0,
        });

        let lhs_place = self.lower_value(lhs_expr)?;

        let rhs_bb = self.alloc_block();
        let join_bb = self.alloc_block();

        self.finish_current_block(Terminator::Branch {
            cond: lhs_place,
            then_target: rhs_bb,
            else_target: join_bb,
        });

        // rhs_bb: lhs was true, evaluate rhs and move into result.
        self.start_block(rhs_bb);
        if let Some(rhs_place) = self.lower_value(rhs_expr) {
            self.instructions.push(Instr::Move {
                dest: result_place,
                src: rhs_place,
            });
        }
        self.finish_current_block(Terminator::Goto { target: join_bb });

        self.start_block(join_bb);
        Some(result_place)
    }

    /// Lower `lhs || rhs` with short-circuit semantics.
    ///
    /// CFG shape:
    ///
    /// ```text
    /// entry_bb (current):
    ///   result_place = true           // optimistic default
    ///   lhs_place = lower(lhs)
    ///   Branch { cond: lhs_place, then: join_bb, else: rhs_bb }
    ///
    /// rhs_bb:
    ///   rhs_place = lower(rhs)
    ///   Move { dest: result_place, src: rhs_place }
    ///   Goto join_bb
    ///
    /// join_bb:
    ///   -- result_place holds the final bool --
    /// ```
    ///
    /// The rhs block is only entered when lhs is false, so rhs side effects
    /// are correctly guarded. On the true path, `result_place` retains the
    /// `true` constant written in the entry block.
    fn lower_logical_or(
        &mut self,
        lhs_expr: &HirExpr,
        rhs_expr: &HirExpr,
        result_ty: &ResolvedTy,
    ) -> Option<Place> {
        let result_place = self.alloc_local(result_ty.clone());
        // Write `true` as the optimistic default (the then path never writes
        // to result_place; the else path writes the rhs value into it).
        self.instructions.push(Instr::ConstI64 {
            dest: result_place,
            value: 1,
        });

        let lhs_place = self.lower_value(lhs_expr)?;

        let rhs_bb = self.alloc_block();
        let join_bb = self.alloc_block();

        self.finish_current_block(Terminator::Branch {
            cond: lhs_place,
            then_target: join_bb,
            else_target: rhs_bb,
        });

        // rhs_bb: lhs was false, evaluate rhs and move into result.
        self.start_block(rhs_bb);
        if let Some(rhs_place) = self.lower_value(rhs_expr) {
            self.instructions.push(Instr::Move {
                dest: result_place,
                src: rhs_place,
            });
        }
        self.finish_current_block(Terminator::Goto { target: join_bb });

        self.start_block(join_bb);
        Some(result_place)
    }

    /// Lower `xs[i]` (`HirExprKind::Index`) for a `Vec<T>` container.
    ///
    /// CFG shape (C-2 OOB trap pattern, mirrors B-2/B-5 bounds-check
    /// discipline):
    ///
    /// ```text
    /// entry_bb (current):
    ///   CallRuntimeAbi { symbol: "hew_vec_len", args: [vec_place], dest: len_place }
    ///   IntCmp { pred: UnsignedGreaterEq, dest: oob_flag,
    ///            lhs: index_place, rhs: len_place }
    ///   Branch { cond: oob_flag, then: trap_bb, else: cont_bb }
    ///
    /// trap_bb:
    ///   Trap { kind: IndexOutOfBounds }
    ///
    /// cont_bb:
    ///   CallRuntimeAbi { symbol: "hew_vec_get_T",
    ///                    args: [vec_place, index_place], dest: result_place }
    /// ```
    ///
    /// The `UnsignedGreaterEq` predicate catches both negative indices
    /// (which wrap to values > `i64::MAX` when reinterpreted as unsigned)
    /// and indices ≥ `len` in a single compare — the same technique used
    /// by B-5's shift-range check. LESSONS: `boundary-fail-closed` (P0) —
    /// the trap is always emitted; the compiler never relies on the runtime's
    /// own bounds check.
    ///
    /// Element-type dispatch (`hew_vec_get_T`):
    /// - `i32` → `hew_vec_get_i32`
    /// - `i64` → `hew_vec_get_i64`
    /// - `f64` → `hew_vec_get_f64`
    /// - ptr-shaped (`Duplex`, `LambdaActorHandle`, Named heap types) → `hew_vec_get_ptr`
    ///
    /// Unsupported element types emit `MirDiagnostic::NotYetImplemented`
    /// and return `None` (tracked gap, not silent shim).
    fn lower_vec_index(
        &mut self,
        container: &HirExpr,
        index: &HirExpr,
        elem_ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        // Lower the container and index sub-expressions.
        let vec_place = self.lower_value(container)?;
        let index_place = self.lower_value(index)?;

        // Step 1: Call hew_vec_len(vec) -> i64 to get the length.
        let len_place = self.alloc_local(ResolvedTy::I64);
        self.instructions.push(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new("hew_vec_len", vec![vec_place], Some(len_place))
                .expect("hew_vec_len is an allowlisted runtime symbol"),
        ));

        // Step 2: Bounds check via UnsignedGreaterEq. A signed i64 index
        // that is negative will wrap to a value > i64::MAX when treated
        // as unsigned, which is ≥ any valid len. This catches both negative
        // and out-of-bounds indices in one compare.
        let oob_flag = self.alloc_local(ResolvedTy::Bool);
        self.instructions.push(Instr::IntCmp {
            dest: oob_flag,
            pred: CmpPred::UnsignedGreaterEq,
            lhs: index_place,
            rhs: len_place,
        });

        // Seal current block with Branch → trap or continue.
        let trap_bb = self.alloc_block();
        let cont_bb = self.alloc_block();
        self.finish_current_block(Terminator::Branch {
            cond: oob_flag,
            then_target: trap_bb,
            else_target: cont_bb,
        });

        // Trap block: hard-abort with IndexOutOfBounds.
        self.start_block(trap_bb);
        self.finish_current_block(Terminator::Trap {
            kind: TrapKind::IndexOutOfBounds,
        });

        // Continuation block: emit the actual element load.
        self.start_block(cont_bb);

        // Dispatch to the typed runtime getter based on element type.
        let get_symbol = match elem_ty {
            ResolvedTy::I32 | ResolvedTy::U32 => "hew_vec_get_i32",
            ResolvedTy::I64 | ResolvedTy::U64 => "hew_vec_get_i64",
            ResolvedTy::F64 => "hew_vec_get_f64",
            // Pointer-shaped heap handles: Duplex, LambdaActorHandle, and
            // Named types that are resource/linear (their heap-backing is
            // opaque to the element-load ABI). hew_vec_get_ptr returns a
            // *mut c_void which codegen casts to the appropriate pointer.
            ResolvedTy::Named { .. } => "hew_vec_get_ptr",
            other => {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!("Vec<{other:?}> element type for xs[i]"),
                        site,
                    },
                    note: "hew_vec_get_T dispatch: element types supported by this \
                           slice are i32/u32, i64/u64, f64, and Named heap types. \
                           String (hew_vec_get_str strdup ownership) is a follow-on \
                           slice. bool/char and other scalars map to i32/i64 in a \
                           future width-normalisation slice."
                        .to_string(),
                });
                return None;
            }
        };

        let result_place = self.alloc_local(elem_ty.clone());
        self.instructions.push(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(
                get_symbol,
                vec![vec_place, index_place],
                Some(result_place),
            )
            .expect("hew_vec_get_T is an allowlisted runtime symbol"),
        ));

        Some(result_place)
    }

    /// Lower `xs[a..b]` / `xs[a..=b]` / `xs[..b]` / `xs[a..]` / `xs[..]`
    /// (`HirExprKind::Slice`) for a `Vec<T>` container (C-3).
    ///
    /// CFG shape (extends C-2's OOB pattern with a two-stage bounds check
    /// and an optional integer-overflow trap for the inclusive form):
    ///
    /// ```text
    /// entry_bb (current):
    ///   [start open?]     start_place := ConstI64(0)
    ///   [end open?]       end_place := CallRuntimeAbi("hew_vec_len", [vec])
    ///   [inclusive?]      one := ConstI64(1)
    ///                     end_place := IntArithChecked(Add, signed, end_place, one)
    ///                       → on overflow → trap_overflow_bb { TrapKind::IntegerOverflow }
    ///                       → on success → cont1_bb (subsequent emission)
    ///   IntCmp { pred: SignedGreater, dest: bad1, lhs: start, rhs: end }
    ///   Branch { cond: bad1, then: trap_oob_bb, else: cont2_bb }
    ///
    /// cont2_bb:
    ///   [end_place already holds end; reuse]
    ///   len := CallRuntimeAbi("hew_vec_len", [vec])    -- second probe so
    ///                                                    inclusive +1 is not
    ///                                                    compared to the
    ///                                                    pre-Add len
    ///   IntCmp { pred: SignedGreater, dest: bad2, lhs: end_place, rhs: len }
    ///   Branch { cond: bad2, then: trap_oob_bb, else: cont3_bb }
    ///
    /// trap_oob_bb:
    ///   Trap { kind: IndexOutOfBounds }
    ///
    /// cont3_bb:
    ///   CallRuntimeAbi { hew_vec_slice_range_T, args: [vec, start, end],
    ///                    dest: result }
    /// ```
    ///
    /// `SignedGreater` is the right predicate for `start > end` and
    /// `end > len` because both endpoints are checker-validated i64. The
    /// inclusive overflow guard runs BEFORE the bounds check so an
    /// `i64::MAX..=i64::MAX` form traps as `IntegerOverflow` (not
    /// `IndexOutOfBounds`), matching B-2's discipline that each trap
    /// reports its true cause.
    ///
    /// Element-type dispatch (`hew_vec_slice_range_T`) covers the same
    /// set as `hew_vec_get_T` (C-2) plus `str`: i32/u32, i64/u64, f64,
    /// Named heap (ptr), and `String`. For Vec<String> the runtime
    /// strdups each element into the fresh vec and sets `elem_kind ==
    /// String` so the existing free-on-drop path frees them.
    #[expect(
        clippy::too_many_lines,
        reason = "explicit CFG construction: each block + bounds-check branch is its own \
                  step; splitting would obscure the trap-graph shape"
    )]
    fn lower_vec_slice(
        &mut self,
        container: &HirExpr,
        start: Option<&HirExpr>,
        end: Option<&HirExpr>,
        inclusive: bool,
        result_ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        // Resolve element type from the result Vec<T> for runtime dispatch.
        let elem_ty = match result_ty {
            ResolvedTy::Named { name, args } if name == "Vec" && !args.is_empty() => {
                args[0].clone()
            }
            other => {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!(
                            "Vec range-slice result type must be Vec<T>; got {other:?}"
                        ),
                        site,
                    },
                    note: "C-3 range-slice expects the checker to record `Vec<T>` as the \
                           expression type; receiving anything else indicates a checker/HIR \
                           boundary violation upstream"
                        .to_string(),
                });
                return None;
            }
        };

        let slice_symbol = match &elem_ty {
            ResolvedTy::I32 | ResolvedTy::U32 => "hew_vec_slice_range_i32",
            ResolvedTy::I64 | ResolvedTy::U64 => "hew_vec_slice_range_i64",
            ResolvedTy::F64 => "hew_vec_slice_range_f64",
            ResolvedTy::String => "hew_vec_slice_range_str",
            // Pointer-shaped heap handles (Duplex/LambdaActor/Named types).
            ResolvedTy::Named { .. } => "hew_vec_slice_range_ptr",
            other => {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!("Vec<{other:?}> element type for xs[a..b]"),
                        site,
                    },
                    note: "hew_vec_slice_range_T dispatch: element types supported are \
                           i32/u32, i64/u64, f64, str (String), and Named heap types. \
                           bool/char and other scalars map to i32/i64 in a future \
                           width-normalisation slice."
                        .to_string(),
                });
                return None;
            }
        };

        let vec_place = self.lower_value(container)?;

        // Resolve start. Open `start` materialises as ConstI64(0).
        let start_place = if let Some(s) = start {
            self.lower_value(s)?
        } else {
            let p = self.alloc_local(ResolvedTy::I64);
            self.instructions
                .push(Instr::ConstI64 { dest: p, value: 0 });
            p
        };

        // Resolve end. Open `end` materialises as `hew_vec_len(vec)`.
        // For inclusive `a..=b`, lower `b` first then add 1 with overflow trap.
        let end_place = if let Some(e) = end {
            let base = self.lower_value(e)?;
            if inclusive {
                // b + 1 via IntArithChecked(Add, Signed). The endpoint is i64
                // per the checker; overflow on i64::MAX traps as
                // TrapKind::IntegerOverflow.
                let one_place = self.alloc_local(ResolvedTy::I64);
                self.instructions.push(Instr::ConstI64 {
                    dest: one_place,
                    value: 1,
                });
                let bumped = self.alloc_local(ResolvedTy::I64);
                let overflow_flag = self.alloc_local(ResolvedTy::Bool);
                self.instructions.push(Instr::IntArithChecked {
                    op: IntArithOp::Add,
                    signed: IntSignedness::Signed,
                    dest: bumped,
                    lhs: base,
                    rhs: one_place,
                    overflow_flag,
                });
                let overflow_trap_bb = self.alloc_block();
                let after_inc_bb = self.alloc_block();
                self.finish_current_block(Terminator::Branch {
                    cond: overflow_flag,
                    then_target: overflow_trap_bb,
                    else_target: after_inc_bb,
                });
                self.start_block(overflow_trap_bb);
                self.finish_current_block(Terminator::Trap {
                    kind: TrapKind::IntegerOverflow,
                });
                self.start_block(after_inc_bb);
                bumped
            } else {
                base
            }
        } else {
            // Open end: probe length via hew_vec_len.
            let p = self.alloc_local(ResolvedTy::I64);
            self.instructions.push(Instr::CallRuntimeAbi(
                crate::model::RuntimeCall::new("hew_vec_len", vec![vec_place], Some(p))
                    .expect("hew_vec_len is an allowlisted runtime symbol"),
            ));
            p
        };

        // Bounds check 1: start <= end. Implemented as `start > end` ?
        // → trap_oob.
        let oob_trap_bb = self.alloc_block();
        let after_check1_bb = self.alloc_block();
        let bad1 = self.alloc_local(ResolvedTy::Bool);
        self.instructions.push(Instr::IntCmp {
            dest: bad1,
            pred: CmpPred::SignedGreater,
            lhs: start_place,
            rhs: end_place,
        });
        self.finish_current_block(Terminator::Branch {
            cond: bad1,
            then_target: oob_trap_bb,
            else_target: after_check1_bb,
        });

        // Bounds check 2 (in the success-of-check-1 block): end <= len.
        // Re-probe len here so the comparison uses the post-inclusive-bump
        // end against the current container length.
        self.start_block(after_check1_bb);
        let len_place = self.alloc_local(ResolvedTy::I64);
        self.instructions.push(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new("hew_vec_len", vec![vec_place], Some(len_place))
                .expect("hew_vec_len is an allowlisted runtime symbol"),
        ));
        let bad2 = self.alloc_local(ResolvedTy::Bool);
        self.instructions.push(Instr::IntCmp {
            dest: bad2,
            pred: CmpPred::SignedGreater,
            lhs: end_place,
            rhs: len_place,
        });
        let after_check2_bb = self.alloc_block();
        self.finish_current_block(Terminator::Branch {
            cond: bad2,
            then_target: oob_trap_bb,
            else_target: after_check2_bb,
        });

        // Single shared OOB trap block for both bounds-check branches.
        self.start_block(oob_trap_bb);
        self.finish_current_block(Terminator::Trap {
            kind: TrapKind::IndexOutOfBounds,
        });

        // Success path: emit the runtime slice call. The result is a fresh
        // `*mut HewVec<T>` handle (ptr-shaped local typed as Vec<T>).
        self.start_block(after_check2_bb);
        let result_place = self.alloc_local(result_ty.clone());
        self.instructions.push(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(
                slice_symbol,
                vec![vec_place, start_place, end_place],
                Some(result_place),
            )
            .expect("hew_vec_slice_range_T is an allowlisted runtime symbol"),
        ));

        Some(result_place)
    }

    /// Lower a recognised `hew_*` runtime-ABI call to
    /// `Instr::CallRuntimeAbi`.
    ///
    /// Called from `lower_value`'s `HirExprKind::Call` arm when the
    /// callee is a `BindingRef` whose name passes
    /// `runtime_symbols::is_known_runtime_symbol`.  The HIR args have
    /// already been validated by the HIR pipeline; this method lower
    /// each arg via `lower_value`, then emits the appropriate
    /// instruction sequence.
    ///
    /// # `hew_duplex_pair` encoding
    ///
    /// The runtime C-ABI takes `(s_cap, r_cap, *mut *mut HewDuplexHandle,
    /// *mut *mut HewDuplexHandle)`.  The user surface is `duplex_pair(N)`
    /// with one symmetric capacity arg.  E2 duplicates `args[0]` into
    /// both cap slots and passes two fresh `Place::DuplexHandle(N0/N1)`
    /// in the out-param positions (`args[2..=3]`).  Codegen (E4) takes
    /// the address of each `DuplexHandle` local and passes it as the
    /// actual pointer.  A "tuple proxy" `Place::Local(M)` is returned
    /// so that subsequent `TupleIndex` projections can recover the
    /// individual `DuplexHandle` Places via `self.tuple_decomp`.
    ///
    /// # `hew_duplex_send` encoding
    ///
    /// The runtime C-ABI takes `(*mut HewDuplexHandle, *const u8, usize)`.
    /// For an integer payload `42`, this method emits a prefatory
    /// `Instr::ConstI64 { value: 8 }` as the byte-length constant
    /// before the `CallRuntimeAbi`.  The message value Place is passed
    /// as-is; codegen (E4) stores it to a stack alloca and passes its
    /// address as the `*const u8`.
    ///
    /// # SHIM(E4) convention
    ///
    // SHIM(E4): codegen interprets Place::DuplexHandle(N) per-symbol convention:
    //   hew_duplex_send/recv/close: load the raw ptr from local-N's alloca, pass as *mut HewDuplexHandle.
    //   hew_duplex_pair out-params (args[2], args[3]): take address of local-N's alloca, pass as *mut *mut HewDuplexHandle.
    // The message-value Place::Local(N) in args[1] of hew_duplex_send is store-to-alloca + address-cast by E4.
    // The length Place::Local(N) in args[2] of hew_duplex_send carries the ConstI64(8) emitted above.
    // WHY: MIR names semantics; address materialisation is a codegen-target concern.
    // WHEN obsolete: when E4's lower_instr arm is wired and tested for each of these conventions.
    // WHAT: replace with direct LLVMBuildCall emission for each symbol group.
    fn task_scope_ty() -> ResolvedTy {
        ResolvedTy::Named {
            name: "HewTaskScope".to_string(),
            args: vec![],
        }
    }

    fn push_runtime_call(&mut self, symbol: &str, args: Vec<Place>, dest: Option<Place>) {
        self.instructions.push(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(symbol, args, dest)
                .unwrap_or_else(|_| panic!("{symbol} is an allowlisted runtime symbol")),
        ));
    }

    fn lower_task_scope(&mut self, body: &HirBlock) -> Place {
        let scope_place = self.alloc_local(Self::task_scope_ty());
        self.push_runtime_call("hew_task_scope_new", vec![], Some(scope_place));

        let previous_scope_place = self.alloc_local(Self::task_scope_ty());
        self.push_runtime_call(
            "hew_task_scope_set_current",
            vec![scope_place],
            Some(previous_scope_place),
        );

        let saved_scope = self.current_task_scope.replace(scope_place);
        for stmt in &body.statements {
            self.stmt(stmt);
        }
        if let Some(tail) = &body.tail {
            let _ = self.lower_value(tail);
        }
        self.current_task_scope = saved_scope;

        self.push_runtime_call("hew_task_scope_join_all", vec![scope_place], None);
        self.push_runtime_call("hew_task_scope_destroy", vec![scope_place], None);
        let restored_scope_place = self.alloc_local(Self::task_scope_ty());
        self.push_runtime_call(
            "hew_task_scope_set_current",
            vec![previous_scope_place],
            Some(restored_scope_place),
        );

        let unit_place = self.alloc_local(ResolvedTy::Unit);
        self.instructions.push(Instr::UnitLit { dest: unit_place });
        unit_place
    }

    fn direct_no_arg_unit_callee(
        &mut self,
        callee: &HirExpr,
        args: &[HirExpr],
        ret_ty: &ResolvedTy,
        site: hew_hir::SiteId,
        construct: &str,
    ) -> Option<String> {
        if !args.is_empty() || !matches!(ret_ty, ResolvedTy::Unit) {
            for arg in args {
                let _ = self.lower_value(arg);
            }
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: construct.to_string(),
                    site,
                },
                note: "cancellation-token task lowering currently supports only \
                       no-argument functions returning unit; value/result task \
                       propagation remains fail-closed"
                    .to_string(),
            });
            return None;
        }
        match &callee.kind {
            HirExprKind::BindingRef { name, .. } if self.module_fn_names.contains(name) => {
                Some(name.clone())
            }
            _ => {
                let _ = self.lower_value(callee);
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: construct.to_string(),
                        site,
                    },
                    note: "fork cancellation lowering requires a direct module function callee"
                        .to_string(),
                });
                None
            }
        }
    }

    fn lower_spawned_call_task(
        &mut self,
        callee: &HirExpr,
        args: &[HirExpr],
        task_ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let Some(scope_place) = self.current_task_scope else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "spawned call without current task scope".to_string(),
                    site,
                },
                note: "task spawn reached MIR without a scope-owned cancellation token; \
                       refusing to emit an unobservable cancellation edge"
                    .to_string(),
            });
            return None;
        };
        let ResolvedTy::Task(inner) = task_ty else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "spawned call with non-Task type".to_string(),
                    site,
                },
                note: "HIR SpawnedCall must carry Task<T>".to_string(),
            });
            return None;
        };
        if matches!(callee.kind, HirExprKind::Closure { .. }) {
            return self.lower_spawned_closure_task(
                callee,
                args,
                task_ty,
                inner,
                scope_place,
                site,
            );
        }
        let callee_symbol =
            self.direct_no_arg_unit_callee(callee, args, inner, site, "spawned call")?;

        let task_place = self.alloc_local(task_ty.clone());
        self.push_runtime_call("hew_task_new", vec![], Some(task_place));
        self.push_runtime_call("hew_task_scope_spawn", vec![scope_place, task_place], None);
        self.instructions.push(Instr::SpawnTaskDirect {
            task: task_place,
            callee_symbol,
        });
        Some(task_place)
    }

    fn lower_spawned_closure_task(
        &mut self,
        callee: &HirExpr,
        args: &[HirExpr],
        task_ty: &ResolvedTy,
        inner: &ResolvedTy,
        scope_place: Place,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let HirExprKind::Closure {
            params,
            ret_ty,
            body,
            captures,
        } = &callee.kind
        else {
            unreachable!("caller checked closure callee");
        };
        if !args.is_empty()
            || !params.is_empty()
            || !matches!(inner, ResolvedTy::Unit)
            || !matches!(ret_ty, ResolvedTy::Unit)
        {
            for arg in args {
                let _ = self.lower_value(arg);
            }
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "spawned closure".to_string(),
                    site,
                },
                note: "spawned-closure task lowering supports only zero-argument closures \
                       returning unit; value/result task propagation remains fail-closed"
                    .to_string(),
            });
            return None;
        }
        if let Some(capture) = captures.iter().find(|capture| !capture.is_send) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "spawned closure non-Send capture".to_string(),
                    site,
                },
                note: format!(
                    "closure capture `{}` is not Send; refusing to transfer its environment \
                     across the spawned task boundary",
                    capture.name
                ),
            });
            return None;
        }
        let (fn_symbol, env_ty, env_place) =
            self.materialize_closure_env(callee, params, ret_ty, body, captures)?;
        let task_place = self.alloc_local(task_ty.clone());
        self.push_runtime_call("hew_task_new", vec![], Some(task_place));
        self.push_runtime_call("hew_task_scope_spawn", vec![scope_place, task_place], None);
        self.instructions.push(Instr::SpawnTaskClosure {
            task: task_place,
            fn_symbol,
            env: env_place,
            env_ty,
        });
        Some(task_place)
    }

    fn lower_fork_block_task(&mut self, body: &HirBlock, site: hew_hir::SiteId) -> Option<Place> {
        let expr = if body.statements.len() == 1 && body.tail.is_none() {
            let HirStmtKind::Expr(expr) = &body.statements[0].kind else {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "fork block cancellation child".to_string(),
                        site,
                    },
                    note: "fork block task lowering currently supports expression-call statements only"
                        .to_string(),
                });
                return None;
            };
            expr
        } else if body.statements.is_empty() {
            let Some(tail) = &body.tail else {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "fork block cancellation child".to_string(),
                        site,
                    },
                    note: "fork block task lowering requires a no-argument unit function call"
                        .to_string(),
                });
                return None;
            };
            tail
        } else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "fork block cancellation child".to_string(),
                    site,
                },
                note: "fork block task lowering currently supports exactly one \
                       statement: a no-argument unit function call"
                    .to_string(),
            });
            return None;
        };
        let HirExprKind::Call { callee, args } = &expr.kind else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "fork block cancellation child".to_string(),
                    site,
                },
                note: "fork block task lowering currently supports a direct function call body"
                    .to_string(),
            });
            return None;
        };
        let task_ty = ResolvedTy::Task(Box::new(ResolvedTy::Unit));
        self.lower_spawned_call_task(callee, args, &task_ty, site)
    }

    fn lower_scope_deadline(
        &mut self,
        duration: &HirExpr,
        body: &HirBlock,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let Some(scope_place) = self.current_task_scope else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "scope deadline cancellation edge".to_string(),
                    site,
                },
                note: "deadline reached MIR without an active task scope token".to_string(),
            });
            return None;
        };
        if !body.statements.is_empty() || body.tail.is_some() {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "scope deadline body".to_string(),
                    site,
                },
                note: "deadline cancellation lowering supports empty after(...) bodies only; \
                       non-empty timeout bodies remain fail-closed until select/arm dispatch lands"
                    .to_string(),
            });
            return None;
        }
        let duration_place = self.lower_value(duration)?;
        self.push_runtime_call(
            "hew_task_scope_cancel_after_ns",
            vec![scope_place, duration_place],
            None,
        );
        let unit_place = self.alloc_local(ResolvedTy::Unit);
        self.instructions.push(Instr::UnitLit { dest: unit_place });
        Some(unit_place)
    }

    fn lower_await_task(
        &mut self,
        binding_id: BindingId,
        output_ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        if !matches!(output_ty, ResolvedTy::Unit) {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "await task result".to_string(),
                    site,
                },
                note: "await lowering currently supports unit tasks only; value task \
                       cancellation/result propagation remains fail-closed"
                    .to_string(),
            });
            return None;
        }
        let Some(task_place) = self.binding_locals.get(&binding_id).copied() else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::UnresolvedPlace {
                    binding: binding_id,
                    name: "<await-task>".to_string(),
                    site,
                },
                note: "await task binding has no backend task handle slot".to_string(),
            });
            return None;
        };
        self.push_runtime_call("hew_task_await_blocking", vec![task_place], None);
        let unit_place = self.alloc_local(ResolvedTy::Unit);
        self.instructions.push(Instr::UnitLit { dest: unit_place });
        Some(unit_place)
    }

    /// Emit `Terminator::Call` for a static call to a user-defined function
    /// in the same module. Arguments are lowered left-to-right; if any
    /// argument fails to produce a Place (an unsupported construct in its
    /// own right), the whole call fails closed and returns `None` —
    /// diagnostics from the argument lowering already capture the root cause.
    ///
    /// The `dest` Place is allocated here and written by the emitted
    /// call terminator. For unit-returning functions (`ret_ty` is
    /// `ResolvedTy::Unit`) the dest is `None`; the terminator emits only
    /// the call and branch. For all other return types a fresh local is
    /// allocated and returned so the caller can bind it.
    fn lower_direct_call(
        &mut self,
        callee_symbol: &str,
        hir_args: &[hew_hir::HirExpr],
        ret_ty: &ResolvedTy,
        _site: hew_hir::SiteId,
    ) -> Option<Place> {
        // Lower each argument left-to-right.  If any fails to produce a
        // Place, fail the whole call — argument diagnostics already capture
        // the root cause.
        let mut arg_places = Vec::with_capacity(hir_args.len());
        for arg in hir_args {
            match self.lower_value(arg) {
                Some(p) => arg_places.push(p),
                None => return None,
            }
        }

        // Allocate a destination local for the return value, unless the
        // callee is declared Unit-returning or divergent. Never-returning
        // runtime shims such as exit()/panic() have no value to materialise.
        let dest = if matches!(ret_ty, ResolvedTy::Unit | ResolvedTy::Never) {
            None
        } else {
            Some(self.alloc_local(ret_ty.clone()))
        };

        let next = self.alloc_block();
        self.finish_current_block(Terminator::Call {
            callee: callee_symbol.to_string(),
            args: arg_places,
            dest,
            next,
        });
        self.start_block(next);

        dest
    }

    fn lower_runtime_call(
        &mut self,
        symbol: &str,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        // Construction-time contract: the symbol must be in the allowlist.
        // This is the HIR-string-boundary gate: the caller dispatched this
        // symbol from a `BindingRef` name, so we assert in all build profiles
        // that it is known before we dispatch to a symbol-specific arm.
        // `RuntimeCall::new` enforces the same invariant at the MIR data level;
        // this assert defends the dispatch table (LESSONS `boundary-fail-closed`).
        assert!(
            crate::runtime_symbols::is_known_runtime_symbol(symbol),
            "lower_runtime_call called with unrecognised symbol `{symbol}`; \
             the call site must gate on is_known_runtime_symbol first"
        );

        match symbol {
            "hew_duplex_pair" => self.lower_duplex_pair(hir_args, site),
            "hew_duplex_send" => self.lower_duplex_send(hir_args, site),
            "hew_supervisor_stop" => self.lower_supervisor_stop(hir_args, site),
            _ => {
                // Known-allowlisted symbol but no producer arm yet.  Fail closed
                // so the pipeline rejects the program before codegen runs.
                // Individual symbol producers land in follow-up slices (recv,
                // half-handle split, close, lambda-actor lifecycle).
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!("runtime call `{symbol}`"),
                        site,
                    },
                    note: format!(
                        "`{symbol}` is a recognised runtime symbol but has no \
                         MIR producer arm yet; wired per-symbol in follow-up slices"
                    ),
                });
                None
            }
        }
    }

    /// Emit `Instr::CallRuntimeAbi` for `hew_duplex_pair`.
    ///
    /// HIR shape (from E1 bridge): `Call { callee: BindingRef("hew_duplex_pair"),
    /// args: [cap_expr] }` — one symmetric capacity arg.
    ///
    /// MIR emission:
    ///   1. Lower `cap_expr` → `cap_place`.
    ///   2. Allocate two fresh `DuplexHandle` locals (N0, N1).
    ///   3. Emit `CallRuntimeAbi { args: [cap, cap, DuplexHandle(N0), DuplexHandle(N1)], dest: None }`.
    ///   4. Allocate a "tuple proxy" `Place::Local(M)` to thread the two
    ///      output Places through the existing `BindingRef` lookup.
    ///   5. Register `tuple_decomp[M] = [DuplexHandle(N0), DuplexHandle(N1)]`.
    ///   6. Return `Some(Local(M))`.
    ///
    /// `TupleIndex` lowering recovers the individual `DuplexHandle` Places from
    /// `tuple_decomp`.  `owned_locals` registration for `a` and `b` happens
    /// naturally in `stmt()` when `let a = __tuple_N.0` stores
    /// `DuplexHandle(N0)` directly into `binding_locals` (see the handle-typed
    /// branch in the `stmt` Let arm).
    fn lower_duplex_pair(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        // E1 registers duplex_pair<S, R>(i64) — one symmetric capacity arg.
        // If E1 ever expands to two args (s_cap, r_cap), skip the duplication.
        let cap_place = if hir_args.len() == 1 {
            self.lower_value(&hir_args[0])
        } else if hir_args.len() >= 2 {
            // Future: two-arg form — just lower both and use the first two.
            self.lower_value(&hir_args[0])
        } else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "hew_duplex_pair with zero args".to_string(),
                    site,
                },
                note: "hew_duplex_pair requires at least one capacity argument".to_string(),
            });
            return None;
        };
        let Some(cap_place) = cap_place else {
            // Capacity expression failed to lower (e.g. nested Unsupported).
            // Diagnostic already recorded; propagate the failure.
            return None;
        };

        // If E1 emits two args, lower the second capacity independently.
        // For the one-arg case, duplicate the single capacity for both slots.
        let r_cap_place = if hir_args.len() >= 2 {
            self.lower_value(&hir_args[1]).unwrap_or(cap_place)
        } else {
            cap_place // symmetric capacity: s_cap == r_cap
        };

        // Allocate two DuplexHandle locals.  The local index is shared
        // between `Place::Local(N)` (for type bookkeeping in `self.locals`)
        // and `Place::DuplexHandle(N)` (for semantic kind tracking in the
        // instruction and drop streams).
        let local0 = self.alloc_local(ResolvedTy::Named {
            name: "Duplex".to_string(),
            args: vec![],
        });
        let Place::Local(n0) = local0 else {
            unreachable!("alloc_local returns Place::Local");
        };
        let dh0 = Place::DuplexHandle(n0);

        let local1 = self.alloc_local(ResolvedTy::Named {
            name: "Duplex".to_string(),
            args: vec![],
        });
        let Place::Local(n1) = local1 else {
            unreachable!("alloc_local returns Place::Local");
        };
        let dh1 = Place::DuplexHandle(n1);

        // Emit the runtime call.  The i32 return (error code) is discarded
        // (`dest: None`); the two DuplexHandle out-params are in args[2..=3].
        // Codegen (E4) interprets DuplexHandle places in args[2..=3] as
        // "pass the address of this local's alloca as *mut *mut DuplexHandle".
        self.instructions.push(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(
                "hew_duplex_pair",
                vec![cap_place, r_cap_place, dh0, dh1],
                None,
            )
            .expect("hew_duplex_pair is an allowlisted runtime symbol"),
        ));

        // Create a "tuple proxy" local so TupleIndex lowering can recover dh0/dh1.
        // The proxy carries no runtime value; its index is the key into tuple_decomp.
        // Using `ResolvedTy::Unit` for the proxy type so no spurious UnknownType
        // diagnostic fires for it.
        let proxy = self.alloc_local(ResolvedTy::Unit);
        let Place::Local(proxy_idx) = proxy else {
            unreachable!("alloc_local returns Place::Local");
        };
        self.tuple_decomp.insert(proxy_idx, vec![dh0, dh1]);

        Some(proxy)
    }

    /// Emit `Instr::CallRuntimeAbi` for `hew_supervisor_stop`.
    ///
    /// HIR shape: `Call { callee: BindingRef("supervisor_stop"), args: [sup_expr] }`.
    /// The checker registers `supervisor_stop(sup)` returning `Ty::Unit`, so
    /// this producer returns `None` — Unit is zero-sized and callers handle
    /// `None` as "no destination place" (see `CallClosure` and `CallTraitMethod`
    /// patterns in `lower_value`).
    ///
    /// MIR emission:
    ///   1. Lower `sup_expr` → `sup_place` (a `LocalPid<S>` — opaque ptr).
    ///   2. Emit `CallRuntimeAbi { "hew_supervisor_stop", args: [sup_place], dest: None }`.
    ///   3. Return `None` (Unit result).
    fn lower_supervisor_stop(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        if hir_args.len() != 1 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "supervisor_stop".to_string(),
                    site,
                },
                note: format!(
                    "supervisor_stop expects 1 argument (sup), got {}",
                    hir_args.len()
                ),
            });
            return None;
        }
        let sup_place = self.lower_value(&hir_args[0])?;
        self.instructions.push(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new("hew_supervisor_stop", vec![sup_place], None)
                .expect("hew_supervisor_stop is an allowlisted runtime symbol"),
        ));
        // Unit return — no destination place.
        None
    }

    /// Emit the MIR sequence for a static supervisor child-slot access.
    ///
    /// Called from the `HirExprKind::FieldAccess` intercept arm after the
    /// checker has confirmed the LHS is a supervisor with a static child at
    /// `slot_index`. Produces the following CFG shape:
    ///
    /// ```text
    /// entry_bb (current)
    ///   [lower object → sup_place]
    ///   ConstI64 { dest: idx_place, value: slot_index }
    ///   CallRuntimeAbi { "hew_supervisor_child_get",
    ///                    args: [sup_place, idx_place],
    ///                    dest: result_place }
    ///   RecordFieldLoad { record: result_place, field_offset: 0, dest: tag_place }   -- tag (i64)
    ///   IntCmp { pred: Eq, lhs: tag_place, rhs: zero_place, dest: is_live_flag }
    ///   Branch { cond: is_live_flag, then: success_bb, else: trap_bb }
    ///
    /// trap_bb
    ///   Trap { kind: SupervisorChildUnavailable }
    ///
    /// success_bb  [cursor here after call]
    ///   RecordFieldLoad { record: result_place, field_offset: 1, dest: raw_handle }  -- i64 handle
    ///   Move { dest: handle_place (ActorHandle(N)), src: raw_handle }
    ///   [cursor stays here for subsequent lowering]
    /// ```
    ///
    /// Returns `Some(handle_place)` on the success path. `handle_place` is
    /// `Place::ActorHandle(N)` where N is the backing local index of a freshly
    /// allocated `LocalPid<ChildActor>` local (typed as `result_ty` from the
    /// checker — the checker is the authority on the child actor type).
    ///
    /// S3 codegen interprets `CallRuntimeAbi` with a `__HewChildLookupResult`-typed
    /// dest as a struct-return call, emitting `{ i64, ptr }` in LLVM IR and storing
    /// the struct into the alloca slot. `RecordFieldLoad` at index 0 extracts `tag`
    /// and at index 1 extracts the handle pointer (reinterpreted as i64 at the MIR
    /// layer; S3 emits `ptrtoint` when writing to the handle alloca).
    fn lower_supervisor_child_get(
        &mut self,
        object: &HirExpr,
        slot_index: u32,
        result_ty: &ResolvedTy,
        _site: hew_hir::SiteId,
    ) -> Option<Place> {
        // Lower the supervisor object expression to get the supervisor PID place.
        let sup_place = self.lower_value(object)?;

        // Emit a constant for the static slot index.
        let idx_place = self.alloc_local(ResolvedTy::I64);
        self.instructions.push(Instr::ConstI64 {
            dest: idx_place,
            value: i64::from(slot_index),
        });

        // Allocate a local typed as the opaque `__HewChildLookupResult` record.
        // S3 codegen recognises this type name and emits a struct-return LLVM call.
        let result_place = self.alloc_local(ResolvedTy::Named {
            name: CHILD_LOOKUP_RESULT_TY_NAME.to_string(),
            args: vec![],
        });

        // Emit the runtime call. The dest carries the 16-byte struct return value.
        self.instructions.push(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(
                "hew_supervisor_child_get",
                vec![sup_place, idx_place],
                Some(result_place),
            )
            .expect("hew_supervisor_child_get is an allowlisted runtime symbol"),
        ));

        // Extract tag (field 0, type i64 — zero-extended from the u8 wire byte).
        let tag_place = self.alloc_local(ResolvedTy::I64);
        self.instructions.push(Instr::RecordFieldLoad {
            record: result_place,
            field_offset: FieldOffset(0),
            dest: tag_place,
        });

        // Emit `zero_place = 0i64` for the comparison.
        let zero_place = self.alloc_local(ResolvedTy::I64);
        self.instructions.push(Instr::ConstI64 {
            dest: zero_place,
            value: 0,
        });

        // Branch: tag == 0 (Live) → success_bb; tag != 0 → trap_bb.
        let is_live_flag = self.alloc_local(ResolvedTy::Bool);
        self.instructions.push(Instr::IntCmp {
            pred: CmpPred::Eq,
            lhs: tag_place,
            rhs: zero_place,
            dest: is_live_flag,
        });

        let trap_bb = self.alloc_block();
        let success_bb = self.alloc_block();
        self.finish_current_block(Terminator::Branch {
            cond: is_live_flag,
            then_target: success_bb,
            else_target: trap_bb,
        });

        // Trap block: child is Transient (1) or Dead (2) at observation time.
        self.start_block(trap_bb);
        self.finish_current_block(Terminator::Trap {
            kind: TrapKind::SupervisorChildUnavailable,
        });

        // Success block: extract the handle pointer (field 1, i64 at MIR level).
        // S3 emits a `ptrtoint`/`inttoptr` as needed for the wire representation.
        self.start_block(success_bb);
        let raw_handle = self.alloc_local(ResolvedTy::I64);
        self.instructions.push(Instr::RecordFieldLoad {
            record: result_place,
            field_offset: FieldOffset(1),
            dest: raw_handle,
        });

        // Allocate the final ActorHandle place typed as `result_ty`
        // (the checker-authority `LocalPid<ChildActor>` type for this site).
        let handle_local = self.alloc_local(result_ty.clone());
        let Place::Local(handle_id) = handle_local else {
            unreachable!("alloc_local always returns Place::Local");
        };
        let handle_place = Place::ActorHandle(handle_id);

        // Move the i64 wire value into the typed ActorHandle slot.
        // S3 emits the appropriate cast; at MIR level they are the same storage.
        self.instructions.push(Instr::Move {
            dest: handle_place,
            src: raw_handle,
        });

        // The `instr_places` function in lower.rs surfaces `handle_place` to the
        // dataflow seed pass, maintaining the same bookkeeping invariant as
        // `lower_spawn_actor`.

        Some(handle_place)
    }

    /// Emit `Instr::CallRuntimeAbi` for `hew_duplex_send`.
    ///
    /// HIR shape (from E1 bridge): `Call { callee: BindingRef("hew_duplex_send"),
    /// args: [receiver_expr, msg_expr] }` — receiver prepended by E1.
    ///
    /// MIR emission:
    ///   1. Lower `receiver_expr` → `recv_place` (expected `DuplexHandle(N)`).
    ///   2. Lower `msg_expr` → `msg_place` (the integer value's `Local(K)`).
    ///   3. Emit `ConstI64 { dest: len_place, value: 8 }` — the byte-length.
    ///   4. Emit `CallRuntimeAbi { symbol: "hew_duplex_send",
    ///         args: [recv_place, msg_place, len_place], dest: None }`.
    ///   5. Return `None` — send discards its i32 result.
    ///
    /// The receiver is NOT consumed (non-move send semantics); `owned_locals`
    /// for the receiver `DuplexHandle` must persist across multiple sends
    /// (LESSONS `raii-null-after-move`).
    fn lower_duplex_send(
        &mut self,
        hir_args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        if hir_args.len() < 2 {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: "hew_duplex_send with fewer than 2 args".to_string(),
                    site,
                },
                note: "hew_duplex_send requires receiver + message arguments".to_string(),
            });
            return None;
        }
        // args[0] = receiver (DuplexHandle — non-consuming borrow; no Move emitted).
        let recv_place = self.lower_value(&hir_args[0]);
        // args[1] = message value.
        let msg_place = self.lower_value(&hir_args[1]);

        let (Some(recv_place), Some(msg_place)) = (recv_place, msg_place) else {
            // Argument lowering failed; diagnostic already recorded.
            return None;
        };

        // Emit the byte-length constant.  The runtime ABI takes `*const u8 + usize`;
        // E4 codegen stores `msg_place`'s value to a stack alloca and passes its
        // address.  The length constant here encodes the fixed 8-byte integer size.
        //
        // SHIM(E4): the ConstI64(8) encodes the integer payload byte-length.
        // WHY: MIR has no "sizeof" expression; the integer spine always uses 8-byte
        //   i64 values, so the length is a compile-time constant for this skeleton.
        // WHEN obsolete: when the type system can express the payload size directly,
        //   or when hew_duplex_send uses a typed message rather than a byte slice.
        // WHAT: replace with a proper sizeof/alignof expression or a typed ABI.
        let len_place = self.alloc_local(ResolvedTy::I64);
        self.instructions.push(Instr::ConstI64 {
            dest: len_place,
            value: 8,
        });

        // Emit the runtime call.  `recv_place` is used as a borrow (not consumed);
        // the receiver's `owned_locals` entry survives for subsequent sends and the
        // scope-exit drop (LESSONS `raii-null-after-move`, `cleanup-all-exits`).
        self.instructions.push(Instr::CallRuntimeAbi(
            crate::model::RuntimeCall::new(
                "hew_duplex_send",
                vec![recv_place, msg_place, len_place],
                None,
            )
            .expect("hew_duplex_send is an allowlisted runtime symbol"),
        ));

        None // send result (i32 error code) is discarded
    }

    fn lower_actor_payload(
        &mut self,
        args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        match args {
            [] => Some(self.alloc_local(ResolvedTy::Unit)),
            [arg] => self.lower_value(arg),
            _ => {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: "actor receive call with more than one argument".to_string(),
                        site,
                    },
                    note: "slice-4 actor payload packing supports unit or one scalar argument"
                        .to_string(),
                });
                None
            }
        }
    }

    fn actor_method_info(
        &mut self,
        receiver_ty: &ResolvedTy,
        method_id: &str,
        site: hew_hir::SiteId,
    ) -> Option<ActorMethodInfo> {
        let actor_name = actor_name_from_handle_ty(receiver_ty)?;
        let method_name = method_name_from_id(method_id);
        let Some(layout) = self.actor_layouts.get(actor_name) else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("actor call on unknown actor `{actor_name}`"),
                    site,
                },
                note: "receiver type named an actor with no MIR actor layout".to_string(),
            });
            return None;
        };
        let Some(handler) = layout
            .handlers
            .iter()
            .find(|handler| handler.name == method_name)
        else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("unknown actor handler `{method_id}` on `{actor_name}`"),
                    site,
                },
                note:
                    "actor method dispatch side table named a handler absent from the actor layout"
                        .to_string(),
            });
            return None;
        };
        Some(ActorMethodInfo {
            msg_type: handler.msg_type,
            param_tys: handler.param_tys.clone(),
            return_ty: handler.return_ty.clone(),
        })
    }

    fn lower_actor_send(
        &mut self,
        receiver: &HirExpr,
        method_id: &str,
        args: &[hew_hir::HirExpr],
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let info = self.actor_method_info(&receiver.ty, method_id, site)?;
        if info.return_ty != ResolvedTy::Unit {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!(
                        "fire-and-forget actor send to non-unit handler `{method_id}`"
                    ),
                    site,
                },
                note: "ActorSend requires a unit-returning receive handler".to_string(),
            });
            return None;
        }
        if info.param_tys.len() != args.len() {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("actor send arity mismatch for `{method_id}`"),
                    site,
                },
                note: format!(
                    "handler expects {} argument(s), call supplied {}",
                    info.param_tys.len(),
                    args.len()
                ),
            });
            return None;
        }
        let actor = self.lower_value(receiver)?;
        let value = self.lower_actor_payload(args, site)?;
        let next = self.alloc_block();
        self.finish_current_block(Terminator::Send {
            actor,
            msg_type: info.msg_type,
            value,
            next,
        });
        self.start_block(next);
        None
    }

    /// Lower a sealed `select{}` expression to MIR.
    ///
    /// ## Shape produced
    ///
    /// ```text
    /// originating_bb:
    ///   <lower each arm's actor receiver + args / duration into Places>
    ///   Terminator::Select { arms, next: join_bb }
    ///
    /// arm_body_bb[i]:                    // entered when arm i wins
    ///   <body lowers; binding (if any) resolves through binding_locals
    ///    to the per-arm reply slot codegen writes via hew_reply_wait>
    ///   Move { dest: result_place, src: <arm body value> }
    ///   Terminator::Goto { target: join_bb }
    ///
    /// join_bb:                           // single convergence point
    ///   <subsequent function lowering continues here; the select's
    ///    value is result_place, written by exactly one arm body>
    /// ```
    ///
    /// ## Producer-bridge contract (consumed by codegen / slice 3)
    ///
    /// Codegen reads `Terminator::Select { arms, next }` and, for each
    /// currently-supported arm:
    ///   * `SelectArmKind::ActorAsk { actor, method, args }` — emits
    ///     `hew_reply_channel_new` + `hew_actor_ask_with_channel` per
    ///     arm in the originating block; calls `hew_select_first` to
    ///     pick a winner; on win, calls `hew_reply_wait` and writes the
    ///     reply into `arm.binding` (the reply slot MIR allocated),
    ///     then jumps to `arm.body_block`; on loss, calls
    ///     `hew_reply_channel_cancel` + `hew_reply_channel_free`.
    ///   * `SelectArmKind::AfterTimer { duration }` — wins when the
    ///     deadline elapses; jumps to `arm.body_block` with no binding.
    ///
    /// ## Out-of-scope consumer arm kinds (fail-closed)
    ///
    /// `StreamNext` and `TaskAwait` are produced in MIR so the producer
    /// boundary carries the sealed HIR shape forward. The
    /// defence-in-depth fail-closed at codegen's `Terminator::Select`
    /// arm-kind switch remains until the backend consumer lands.
    /// `StreamNext` remains fail-closed on wasm32 at this producer
    /// boundary because the stream runtime substrate is native-only.
    ///
    /// ## Cleanup-CFG composition (D24-2 / `ExitPath::Select`)
    ///
    /// The select terminator emits a `Terminator::Select`; the
    /// elaboration pass (`enumerate_exits` at lower.rs:6450) wires
    /// `ExitPath::Select { block: originating_bb, next: join_bb }`
    /// into `drop_plans` automatically — the function-wide LIFO drop
    /// plan is empty for this exit (per-arm loser cleanup happens at
    /// the codegen dispatch site, not at function exit).
    #[allow(
        clippy::too_many_lines,
        reason = "lower_select threads four phases — arm-kind rejection, \
                  block allocation, per-arm Place lowering + binding \
                  registration, and per-arm body emit — that don't \
                  factor cleanly into helpers without re-threading \
                  Builder state (binding_locals, statements buffer, \
                  current block cursor)"
    )]
    fn lower_select(
        &mut self,
        select: &HirSelect,
        expected_ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        // Native lowering produces all sealed arm kinds. On wasm32,
        // StreamNext stays fail-closed because the stream substrate is
        // native-only; keeping the typed producer diagnostic prevents a
        // malformed MIR shape from reaching later stages on that target.
        #[cfg(target_arch = "wasm32")]
        for arm in &select.arms {
            if matches!(arm.kind, HirSelectArmKind::StreamNext { .. }) {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::SelectArmNotImplemented {
                        arm_kind: "StreamNext".to_string(),
                        deferred_by: "native stream select substrate".to_string(),
                        site,
                    },
                    note: "select{} stream-next arms are native-only at the MIR \
                           producer boundary; wasm32 remains fail-closed"
                        .to_string(),
                });
                return None;
            }
        }

        // Result local first so it dominates every arm-body's Move.
        // For Unit-typed selects the placeholder write is benign — no
        // load occurs in the join block. Mirrors the `lower_if` pattern.
        let result_place = self.alloc_local(expected_ty.clone());

        // Allocate body blocks for every arm and the single join block
        // up front so each `SelectArm.body_block` is known before the
        // originating block seals with `Terminator::Select`.
        let body_bbs: Vec<u32> = (0..select.arms.len()).map(|_| self.alloc_block()).collect();
        let join_bb = self.alloc_block();

        // Lower per-arm operands and allocate per-arm value slots in the
        // ORIGINATING block.
        // Codegen consumes the SelectArm payload to emit the per-arm
        // setup (channel alloc + ask issue) in the same originating
        // block before the `hew_select_first` dispatch.
        let mut mir_arms: Vec<SelectArm> = Vec::with_capacity(select.arms.len());
        for (arm_index, arm) in select.arms.iter().enumerate() {
            let (kind, binding_place) = match &arm.kind {
                HirSelectArmKind::ActorAsk {
                    actor,
                    method,
                    args,
                } => {
                    // Resolve the actor handler's reply type so the
                    // reply slot is typed correctly. Mirrors the
                    // single-arm `lower_actor_ask` path; differs only
                    // in that the wait + bind happen across the
                    // Terminator::Select boundary, not Terminator::Ask.
                    let info = self.actor_method_info(&actor.ty, method, site)?;
                    if info.param_tys.len() != args.len() {
                        self.diagnostics.push(MirDiagnostic {
                            kind: MirDiagnosticKind::NotYetImplemented {
                                construct: format!(
                                    "select actor-ask arm arity mismatch for `{method}`"
                                ),
                                site,
                            },
                            note: format!(
                                "handler expects {} argument(s), arm supplied {}",
                                info.param_tys.len(),
                                args.len()
                            ),
                        });
                        return None;
                    }
                    let actor_place = self.lower_value(actor)?;
                    let arg_places: Option<Vec<Place>> =
                        args.iter().map(|a| self.lower_value(a)).collect();
                    let arg_places = arg_places?;
                    // Pack args into a single payload Place using the
                    // same helper single-shot ask lowering uses. This
                    // is the codegen-side ABI shape: one payload ptr
                    // + size threads through `hew_actor_ask_with_channel`.
                    let payload_place = self.lower_actor_payload(args, site)?;
                    // Per-arm reply slot. Codegen writes
                    // `hew_reply_wait`'s result here on win before
                    // jumping into the arm body. Register against the
                    // HIR binding so the body's BindingRef resolves to
                    // this slot.
                    let reply_dest = self.alloc_local(info.return_ty.clone());
                    if let Some(binding_id) = arm.binding_id {
                        self.binding_locals.insert(binding_id, reply_dest);
                    }
                    (
                        SelectArmKind::ActorAsk {
                            actor: actor_place,
                            method: method.clone(),
                            args: arg_places,
                            msg_type: info.msg_type,
                            value: payload_place,
                        },
                        Some(reply_dest),
                    )
                }
                HirSelectArmKind::StreamNext { stream } => {
                    let stream_place = self.lower_value(stream)?;
                    let item_dest = self.alloc_local(ResolvedTy::Unit);
                    if let Some(binding_id) = arm.binding_id {
                        self.binding_locals.insert(binding_id, item_dest);
                    }
                    (
                        SelectArmKind::StreamNext {
                            stream: stream_place,
                        },
                        Some(item_dest),
                    )
                }
                HirSelectArmKind::TaskAwait { task } => {
                    let task_place = self.lower_value(task)?;
                    let task_ty = self.subst_ty(&task.ty);
                    let await_ty = match task_ty {
                        ResolvedTy::Task(inner) => *inner,
                        _ => ResolvedTy::Unit,
                    };
                    let await_dest = self.alloc_local(await_ty);
                    if let Some(binding_id) = arm.binding_id {
                        self.binding_locals.insert(binding_id, await_dest);
                    }
                    (
                        SelectArmKind::TaskAwait { task: task_place },
                        Some(await_dest),
                    )
                }
                HirSelectArmKind::AfterTimer { duration } => {
                    let duration_place = self.lower_value(duration)?;
                    // AfterTimer arms bind no value — `binding_id` is
                    // None by construction (HIR forbids `<name> from
                    // after ...` patterns). Defensive: even if a
                    // future HIR shape attached a binding, we'd skip
                    // registration since codegen has no value to write.
                    debug_assert!(
                        arm.binding_id.is_none(),
                        "AfterTimer arms must not carry a binding_id"
                    );
                    (
                        SelectArmKind::AfterTimer {
                            duration: duration_place,
                        },
                        None,
                    )
                }
            };
            mir_arms.push(SelectArm {
                kind,
                body_block: body_bbs[arm_index],
                binding: binding_place,
            });
        }

        // Seal the originating block with the select terminator.
        self.finish_current_block(Terminator::Select {
            arms: mir_arms,
            next: join_bb,
        });

        // Per-arm body blocks. Each lowers the arm body; the body's
        // BindingRef (for ActorAsk arms with a binding) resolves
        // through `binding_locals` to the per-arm reply slot codegen
        // populated. AfterTimer arms have no binding by construction.
        // Every body block terminates with Goto join_bb so the join
        // converges (single-predecessor-per-arm CFG; the converging
        // result_place plays the role of an SSA phi for the join).
        for (arm_index, arm) in select.arms.iter().enumerate() {
            self.start_block(body_bbs[arm_index]);
            // ActorAsk arms with a value-bearing binding: emit a
            // `MirStatement::Bind` at the body-block entry so the
            // dataflow pass sees the binding initialised before the
            // body's `BindingRef` reads. Codegen writes
            // `hew_reply_wait`'s result into `SelectArm.binding` on
            // win, then jumps into this body block — the Bind here
            // mirrors that runtime initialisation in the MIR
            // statement stream.
            if let (Some(binding_id), Some(binding_name)) =
                (arm.binding_id, arm.binding_name.as_ref())
            {
                let binding_ty = self.subst_ty(&arm.body.ty);
                // The arm body's HIR type matches the bound reply
                // type (HIR's `select_arm_binding_ty` for ActorAsk
                // arms reads the same `actor_method_dispatch` reply
                // type the body's `BindingRef.ty` carries).
                // Use the arm-binding's resolved type (not the body
                // expression's) by consulting binding_locals' Place
                // which we populated earlier.
                let _ = binding_ty;
                let binding_place = self
                    .binding_locals
                    .get(&binding_id)
                    .copied()
                    .expect("ActorAsk arm registered its reply slot before body lowering");
                let ty_of_place: ResolvedTy = match binding_place {
                    Place::Local(n) => self.locals[n as usize].clone(),
                    _ => arm.body.ty.clone(),
                };
                self.statements.push(MirStatement::Bind {
                    binding: binding_id,
                    name: binding_name.clone(),
                    site: arm.body.site,
                    ty: ty_of_place,
                });
            }
            let body_value = self.lower_value(&arm.body);
            if let Some(src) = body_value {
                self.instructions.push(Instr::Move {
                    dest: result_place,
                    src,
                });
            }
            self.finish_current_block(Terminator::Goto { target: join_bb });
        }

        // Join block — subsequent lowering continues here.
        self.start_block(join_bb);
        Some(result_place)
    }

    fn lower_actor_ask(
        &mut self,
        receiver: &HirExpr,
        method_id: &str,
        args: &[hew_hir::HirExpr],
        reply_ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let info = self.actor_method_info(&receiver.ty, method_id, site)?;
        if info.return_ty != *reply_ty {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("actor ask reply type mismatch for `{method_id}`"),
                    site,
                },
                note: format!(
                    "handler returns {}, ask expression expects {}",
                    info.return_ty.user_facing(),
                    reply_ty.user_facing()
                ),
            });
            return None;
        }
        if info.param_tys.len() != args.len() {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("actor ask arity mismatch for `{method_id}`"),
                    site,
                },
                note: format!(
                    "handler expects {} argument(s), call supplied {}",
                    info.param_tys.len(),
                    args.len()
                ),
            });
            return None;
        }
        let actor = self.lower_value(receiver)?;
        let value = self.lower_actor_payload(args, site)?;
        let reply_dest = self.alloc_local(reply_ty.clone());
        let next = self.alloc_block();
        self.finish_current_block(Terminator::Ask {
            actor,
            msg_type: info.msg_type,
            value,
            reply_dest,
            next,
        });
        self.start_block(next);
        Some(reply_dest)
    }

    fn lower_spawn_actor(
        &mut self,
        actor_name: &str,
        args: &[(String, HirExpr)],
        expr: &HirExpr,
    ) -> Option<Place> {
        // ── Supervisor dispatch ───────────────────────────────────────────
        // Check if `actor_name` names a supervisor before falling through to
        // the actor-layout path. Supervisors are not in `actor_layouts`; their
        // spawn is routed to the synthesised bootstrap function via a
        // `Terminator::Call` rather than `Instr::SpawnActor`.
        if let Some(sup_layout) = self.supervisor_layout_map.get(actor_name).cloned() {
            if !args.is_empty() {
                // Supervisor init args are not yet supported. The checker
                // already rejects supervisor declarations with init params;
                // this branch catches any future surface that reaches MIR
                // before the checker guard does.
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!(
                            "supervisor spawn with init args (`spawn {actor_name}(…)`)"
                        ),
                        site: expr.site,
                    },
                    note: "supervisor init args deferred to follow-on lane; \
                           use `spawn {actor_name}` with no arguments"
                        .to_string(),
                });
                return None;
            }
            // Route `spawn Sup` → `Terminator::Call { bootstrap_symbol }`.
            // `lower_direct_call` allocates the destination local (typed
            // `LocalPid<Sup>` from `expr.ty`) and emits the call terminator.
            return self.lower_direct_call(
                &sup_layout.bootstrap_symbol,
                &[], // bootstrap takes no arguments
                &expr.ty,
                expr.site,
            );
        }
        // ── Actor dispatch (existing path) ───────────────────────────────
        let Some(layout) = self.actor_layouts.get(actor_name).cloned() else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("spawn of unknown actor `{actor_name}`"),
                    site: expr.site,
                },
                note: "named actor spawn requires a MIR actor layout".to_string(),
            });
            return None;
        };
        let explicit_init = layout.init_symbol.is_some();
        let expected_arg_names = if explicit_init {
            &layout.init_param_names
        } else {
            &layout.state_field_names
        };
        if args.len() != expected_arg_names.len() {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!(
                        "spawn `{actor_name}` {} arity mismatch",
                        if explicit_init { "init" } else { "state" }
                    ),
                    site: expr.site,
                },
                note: format!(
                    "actor expects {} {} argument(s), spawn supplied {} initializer(s)",
                    expected_arg_names.len(),
                    if explicit_init { "init" } else { "state" },
                    args.len()
                ),
            });
            return None;
        }
        let mut explicit: HashMap<&str, &HirExpr> = HashMap::new();
        for (name, arg) in args {
            explicit.insert(name.as_str(), arg);
        }
        let init_args =
            self.lower_spawn_actor_init_args(actor_name, &layout, explicit_init, &explicit, expr)?;
        let state = self
            .lower_spawn_actor_state(actor_name, &layout, explicit_init, &explicit, expr)
            .ok()?;
        let slot = self.alloc_local(expr.ty.clone());
        let Place::Local(local_id) = slot else {
            unreachable!("alloc_local returns Place::Local");
        };
        let dest = Place::ActorHandle(local_id);
        self.instructions.push(Instr::SpawnActor {
            actor_name: actor_name.to_string(),
            state,
            init_args,
            dest,
            max_heap_bytes: layout.max_heap_bytes,
        });
        Some(dest)
    }

    fn lower_spawn_actor_init_args(
        &mut self,
        actor_name: &str,
        layout: &ActorLayout,
        explicit_init: bool,
        explicit: &HashMap<&str, &HirExpr>,
        expr: &HirExpr,
    ) -> Option<Vec<Place>> {
        let mut init_args = Vec::new();
        if !explicit_init {
            return Some(init_args);
        }
        for param_name in &layout.init_param_names {
            let Some(arg) = explicit.get(param_name.as_str()) else {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!(
                            "spawn `{actor_name}` missing init parameter `{param_name}`"
                        ),
                        site: expr.site,
                    },
                    note: "actor spawn with an explicit init block requires every init parameter by name"
                        .to_string(),
                });
                return None;
            };
            init_args.push(self.lower_value(arg)?);
        }
        Some(init_args)
    }

    fn lower_spawn_actor_state(
        &mut self,
        actor_name: &str,
        layout: &ActorLayout,
        explicit_init: bool,
        explicit: &HashMap<&str, &HirExpr>,
        expr: &HirExpr,
    ) -> Result<Option<Place>, ()> {
        if layout.state_field_names.is_empty() {
            return Ok(None);
        }
        let state_ty = ResolvedTy::Named {
            name: actor_name.to_string(),
            args: Vec::new(),
        };
        let dest = self.alloc_local(state_ty.clone());
        let mut fields = Vec::new();
        for (idx, field_name) in layout.state_field_names.iter().enumerate() {
            let src = if explicit_init {
                self.default_actor_state_field_value(
                    actor_name,
                    field_name,
                    &layout.state_field_tys[idx],
                    expr.site,
                )
                .ok_or(())?
            } else {
                self.lower_spawn_actor_state_arg(actor_name, field_name, explicit, expr)
                    .ok_or(())?
            };
            fields.push((
                FieldOffset(
                    u32::try_from(idx)
                        .expect("actor state field count exceeds u32::MAX — impossible"),
                ),
                src,
            ));
        }
        self.instructions.push(Instr::RecordInit {
            ty: state_ty,
            fields,
            dest,
        });
        Ok(Some(dest))
    }

    fn lower_spawn_actor_state_arg(
        &mut self,
        actor_name: &str,
        field_name: &str,
        explicit: &HashMap<&str, &HirExpr>,
        expr: &HirExpr,
    ) -> Option<Place> {
        let Some(arg) = explicit.get(field_name) else {
            self.diagnostics.push(MirDiagnostic {
                kind: MirDiagnosticKind::NotYetImplemented {
                    construct: format!("spawn `{actor_name}` missing field `{field_name}`"),
                    site: expr.site,
                },
                note: "actor spawn without an init block requires every state field by declaration name"
                    .to_string(),
            });
            return None;
        };
        self.lower_value(arg)
    }

    fn default_actor_state_field_value(
        &mut self,
        actor_name: &str,
        field_name: &str,
        ty: &ResolvedTy,
        site: hew_hir::SiteId,
    ) -> Option<Place> {
        let dest = self.alloc_local(ty.clone());
        match ty {
            ResolvedTy::I8
            | ResolvedTy::I16
            | ResolvedTy::I32
            | ResolvedTy::I64
            | ResolvedTy::U8
            | ResolvedTy::U16
            | ResolvedTy::U32
            | ResolvedTy::U64
            | ResolvedTy::Isize
            | ResolvedTy::Usize
            | ResolvedTy::Bool
            | ResolvedTy::Char
            | ResolvedTy::Duration => {
                self.instructions.push(Instr::ConstI64 { dest, value: 0 });
                Some(dest)
            }
            ResolvedTy::F32 => {
                self.instructions.push(Instr::FloatLit {
                    dest,
                    value_bits: 0.0f32.to_bits().into(),
                    width: FloatWidth::F32,
                });
                Some(dest)
            }
            ResolvedTy::F64 => {
                self.instructions.push(Instr::FloatLit {
                    dest,
                    value_bits: 0.0f64.to_bits(),
                    width: FloatWidth::F64,
                });
                Some(dest)
            }
            ResolvedTy::Unit => {
                self.instructions.push(Instr::UnitLit { dest });
                Some(dest)
            }
            other => {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::NotYetImplemented {
                        construct: format!(
                            "actor init default state value for field `{actor_name}.{field_name}`"
                        ),
                        site,
                    },
                    note: format!(
                        "state field `{field_name}` has type `{other:?}`; spawn-time init currently only zero-initializes scalar state before calling `__init`"
                    ),
                });
                None
            }
        }
    }

    fn sanitize_symbol_component(input: &str) -> String {
        input
            .chars()
            .map(|ch| if ch.is_ascii_alphanumeric() { ch } else { '_' })
            .collect()
    }

    fn closure_env_pointer_ty(env_ty: &ResolvedTy) -> ResolvedTy {
        ResolvedTy::Pointer {
            is_mutable: false,
            pointee: Box::new(env_ty.clone()),
        }
    }

    fn lower_closure_literal(
        &mut self,
        expr: &HirExpr,
        params: &[hew_hir::HirBinding],
        ret_ty: &ResolvedTy,
        body: &HirExpr,
        captures: &[hew_hir::HirClosureCapture],
    ) -> Option<Place> {
        let (shim_name, _env_ty, env_place) =
            self.materialize_closure_env(expr, params, ret_ty, body, captures)?;
        let closure_place = self.alloc_local(expr.ty.clone());
        self.instructions.push(Instr::MakeClosure {
            fn_symbol: shim_name,
            env: env_place,
            dest: closure_place,
        });

        Some(closure_place)
    }

    fn materialize_closure_env(
        &mut self,
        expr: &HirExpr,
        params: &[hew_hir::HirBinding],
        ret_ty: &ResolvedTy,
        body: &HirExpr,
        captures: &[hew_hir::HirClosureCapture],
    ) -> Option<(String, ResolvedTy, Place)> {
        let closure_id = self.next_closure_id;
        self.next_closure_id = self
            .next_closure_id
            .checked_add(1)
            .expect("closure id overflow");
        let owner = Self::sanitize_symbol_component(&self.current_function_symbol);
        let env_name = format!("__hew_closure_env_{owner}_{closure_id}");
        let shim_name = format!("__hew_closure_invoke_{owner}_{closure_id}");
        let env_ty = ResolvedTy::Named {
            name: env_name.clone(),
            args: vec![],
        };

        self.closure_record_layouts
            .push(crate::model::RecordLayout {
                name: env_name,
                field_tys: captures.iter().map(|capture| capture.ty.clone()).collect(),
            });

        let mut field_pairs = Vec::with_capacity(captures.len());
        let mut failed = false;
        for (idx, capture) in captures.iter().enumerate() {
            let offset =
                FieldOffset(u32::try_from(idx).expect("closure capture count exceeds u32::MAX"));
            let src = if let Some(place) = self.binding_locals.get(&capture.binding).copied() {
                place
            } else if let Some(source) = self.capture_env_sources.get(&capture.binding).cloned() {
                let temp = self.alloc_local(source.ty.clone());
                self.instructions.push(Instr::ClosureEnvFieldLoad {
                    env: source.env,
                    env_ty: source.env_ty,
                    field_offset: source.field_offset,
                    dest: temp,
                });
                temp
            } else {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::CannotMaterializeClosureCapture {
                        binding: capture.binding,
                        name: capture.name.clone(),
                        site: expr.site,
                    },
                    note: format!(
                        "closure capture `{}` has no MIR backend slot or enclosing closure env field",
                        capture.name
                    ),
                });
                failed = true;
                continue;
            };
            field_pairs.push((offset, src));
        }
        if failed {
            return None;
        }

        let env_place = self.alloc_local(env_ty.clone());
        self.instructions.push(Instr::RecordInit {
            ty: env_ty.clone(),
            fields: field_pairs,
            dest: env_place,
        });

        let lowered = self.lower_closure_shim(&shim_name, &env_ty, params, ret_ty, body, captures);
        self.generated_functions.push(lowered);

        Some((shim_name, env_ty, env_place))
    }

    #[allow(
        clippy::too_many_lines,
        reason = "closure shim construction keeps raw/checked/elaborated MIR snapshots aligned"
    )]
    fn lower_closure_shim(
        &self,
        shim_name: &str,
        env_ty: &ResolvedTy,
        params: &[hew_hir::HirBinding],
        ret_ty: &ResolvedTy,
        body: &HirExpr,
        captures: &[hew_hir::HirClosureCapture],
    ) -> LoweredFunction {
        let env_ptr_ty = Self::closure_env_pointer_ty(env_ty);
        let mut builder = Builder {
            type_classes: self.type_classes.clone(),
            record_field_orders: self.record_field_orders.clone(),
            module_fn_names: self.module_fn_names.clone(),
            subst: self.subst.clone(),
            call_site_type_args: self.call_site_type_args.clone(),
            // A closure may capture a supervisor PID and access a child slot
            // field on it — the HIR checker registers those accesses in
            // `supervisor_child_slots` by site regardless of nesting context.
            // Propagate the parent module's map so the FieldAccess intercept arm
            // fires correctly for any closure body that contains such an access.
            supervisor_child_slots: self.supervisor_child_slots.clone(),
            current_function_symbol: shim_name.to_string(),
            ..Builder::default()
        };

        let env_place = builder.alloc_local(env_ptr_ty.clone());
        for (idx, capture) in captures.iter().enumerate() {
            builder.capture_env_sources.insert(
                capture.binding,
                CaptureEnvSource {
                    env: env_place,
                    env_ty: env_ty.clone(),
                    field_offset: FieldOffset(
                        u32::try_from(idx).expect("closure capture count exceeds u32::MAX"),
                    ),
                    ty: capture.ty.clone(),
                },
            );
        }
        for param in params {
            let place = builder.alloc_local(param.ty.clone());
            builder.binding_locals.insert(param.id, place);
        }

        if let Some(src) = builder.lower_value(body) {
            builder.instructions.push(Instr::Move {
                dest: Place::ReturnSlot,
                src,
            });
        }
        builder.statements.push(MirStatement::Return {
            site: Some(body.site),
            ty: ret_ty.clone(),
        });

        let blocks = builder.finalize_blocks(Terminator::Return);
        let thir_statements: Vec<MirStatement> = blocks
            .iter()
            .flat_map(|b| b.statements.iter().cloned())
            .collect();
        let thir = ThirFunction {
            name: shim_name.to_string(),
            return_ty: ret_ty.clone(),
            statements: thir_statements,
        };
        let mut raw_params = Vec::with_capacity(params.len() + 1);
        raw_params.push(env_ptr_ty);
        raw_params.extend(params.iter().map(|param| param.ty.clone()));
        let raw = RawMirFunction {
            name: shim_name.to_string(),
            return_ty: ret_ty.clone(),
            call_conv: crate::model::FunctionCallConv::ClosureInvoke,
            params: raw_params,
            locals: builder.locals.clone(),
            blocks,
            decisions: builder.decisions.clone(),
        };
        let synthetic_func = HirFn {
            id: hew_hir::ItemId(0),
            node: hew_hir::HirNodeId(0),
            name: shim_name.to_string(),
            type_params: Vec::new(),
            params: params.to_vec(),
            return_ty: ret_ty.clone(),
            body: hew_hir::HirBlock {
                node: hew_hir::HirNodeId(0),
                scope: hew_hir::ScopeId(0),
                statements: Vec::new(),
                tail: None,
                ty: ret_ty.clone(),
                span: body.span.clone(),
            },
            span: body.span.clone(),
        };
        let dataflow_result = check_function(&builder, &raw.blocks, &synthetic_func);
        let mut diagnostics: Vec<MirDiagnostic> = dataflow_result
            .checks
            .iter()
            .filter_map(check_to_diagnostic)
            .collect();
        diagnostics.append(&mut builder.diagnostics);
        collect_unknown_type_diagnostics(&synthetic_func, &builder, &mut diagnostics);
        let cooperate_sites = dataflow::compute_cooperate_sites(&raw.blocks);
        let checked = CheckedMirFunction {
            name: shim_name.to_string(),
            return_ty: ret_ty.clone(),
            blocks: raw.blocks.clone(),
            decisions: builder.decisions.clone(),
            checks: dataflow_result.checks.clone(),
            cooperate_sites,
        };
        let elaborated = elaborate(&checked, &builder, &thir.statements, &dataflow_result);

        LoweredFunction {
            thir,
            raw,
            checked,
            elaborated,
            diagnostics,
            generated: builder.generated_functions,
            record_layouts: builder.closure_record_layouts,
        }
    }

    /// Lower an `HirExprKind::SpawnLambdaActor` literal to a MIR
    /// `Place::LambdaActorHandle`. The literal allocates a fresh
    /// local (typed as the actor's `Duplex<Msg, Reply>`) and emits a
    /// `Place::LambdaActorHandle(local_id)` so drop elaboration
    /// selects `DropKind::LambdaActorRelease` — the
    /// stop-on-last-handle-drop protocol with weak-ref body capture
    /// (§5.9 ratification 2).
    ///
    /// Every HIR-resolved capture is forwarded into the function's
    /// `lambda_captures` ledger after proving that the source binding has a MIR
    /// `Place` in `binding_locals`. A capture whose source binding has no backend
    /// slot is a lowering error, never a silently smaller capture set.
    ///
    /// Body lowering (the actor's per-message dispatch) is a
    /// follow-up slice; the MIR shape only needs the handle Place plus
    /// the capture metadata. Codegen rejects `Place::LambdaActorHandle`
    /// today (fail-closed) so a runtime substrate is not required for
    /// the static checks to land.
    fn lower_spawn_lambda_actor(&mut self, expr: &HirExpr) -> Place {
        let HirExprKind::SpawnLambdaActor { captures, .. } = &expr.kind else {
            unreachable!("lower_spawn_lambda_actor called on non-SpawnLambdaActor kind");
        };
        // Two paths produce the handle:
        //   - `let <name> = actor |..| { .. }`: the `stmt` Let arm
        //     pre-allocates the binding's slot and stashes its
        //     `LambdaActorHandle` in `pending_lambda_actor_handle`
        //     so the body's Weak self-capture finds a backend slot
        //     for the let-binding. Reuse the pre-allocated handle.
        //   - any non-let position (return-position literal, an
        //     argument, etc.): allocate a fresh local on the fly.
        let handle = if let Some(handle) = self.pending_lambda_actor_handle {
            handle
        } else {
            let local = self.alloc_local(expr.ty.clone());
            let Place::Local(local_id) = local else {
                unreachable!("alloc_local returns Place::Local");
            };
            Place::LambdaActorHandle(local_id)
        };
        for capture in captures {
            // Each captured binding must already have a backend slot in the
            // enclosing function. The forward-bound recursive self capture is
            // the let-binding itself, whose `binding_locals` entry was populated
            // by the `stmt` Let arm before this producer ran.
            if !self.binding_locals.contains_key(&capture.binding) {
                self.diagnostics.push(MirDiagnostic {
                    kind: MirDiagnosticKind::CannotMaterializeClosureCapture {
                        binding: capture.binding,
                        name: capture.name.clone(),
                        site: expr.site,
                    },
                    note: format!(
                        "closure capture `{}` was resolved by HIR but has no MIR backend slot; \
                         refusing to drop the capture from the environment",
                        capture.name
                    ),
                });
                continue;
            }
            let capture_kind = match capture.kind {
                hew_hir::HirCaptureKind::Strong => crate::model::CaptureKind::Strong,
                hew_hir::HirCaptureKind::Weak => crate::model::CaptureKind::Weak,
            };
            self.lambda_captures.push(LambdaCapture {
                actor_handle: handle,
                captured: capture.binding,
                name: capture.name.clone(),
                capture_kind,
            });
        }
        handle
    }

    fn decide(&mut self, expr: &HirExpr) {
        if self
            .decisions
            .iter()
            .any(|decision| decision.site == expr.site)
        {
            return;
        }
        let value_class = if expr.value_class == ValueClass::Unknown {
            let inferred = ValueClass::of_ty(&expr.ty, &self.type_classes);
            if inferred != ValueClass::Unknown {
                inferred
            } else if self.is_known_actor_runtime_ty(&expr.ty) {
                ValueClass::BitCopy
            } else {
                ValueClass::Unknown
            }
        } else {
            expr.value_class
        };
        let strategy = match value_class {
            ValueClass::CowValue => Strategy::CowShare,
            // `@linear` and `@resource` (AffineResource) both move by default;
            // `MirCheck::MustConsume` rejects unconsumed `@linear` exits.
            ValueClass::AffineResource | ValueClass::Linear => Strategy::Move,
            ValueClass::Unknown => Strategy::UnknownBlocked,
            ValueClass::BitCopy | ValueClass::PersistentShare | ValueClass::View => {
                Strategy::BorrowRead
            }
        };
        let strategy = match (value_class, expr.intent) {
            (ValueClass::CowValue, IntentKind::Modify) => Strategy::EnsureUnique,
            (ValueClass::CowValue, IntentKind::Read | IntentKind::Capture) => Strategy::CowShare,
            (ValueClass::AffineResource, IntentKind::Read) => Strategy::BorrowRead,
            // `@linear` Read is *not* a borrow — the value must be consumed
            // exactly once; a read-without-consume leaves the binding
            // live for a later `MustConsume` rejection. Encode as Move
            // alongside the explicit Consume arm below.
            (ValueClass::Linear, IntentKind::Read | IntentKind::Capture)
            | (
                ValueClass::BitCopy
                | ValueClass::CowValue
                | ValueClass::AffineResource
                | ValueClass::Linear,
                IntentKind::Consume,
            ) => Strategy::Move,
            (_, IntentKind::Yield) => Strategy::Freeze,
            _ => strategy,
        };
        self.decisions.push(DecisionFact {
            site: expr.site,
            ty: self.subst_ty(&expr.ty),
            value_class,
            intent: expr.intent,
            strategy,
            why: "first vertical-slice classifier".to_string(),
        });
    }

    fn is_known_actor_runtime_ty(&self, ty: &ResolvedTy) -> bool {
        match ty {
            ResolvedTy::Named { name, .. }
                // SHIM: "PanicInfo" is added here to classify it as BitCopy so
                // the MIR decision map does not block on ValueClass::Unknown.
                // WHY: PanicInfo is registered in builtin_type_classes with
                // ResourceMarker::None → Unknown, but the injected prologue and
                // field-access lowering require it to be treated as a plain
                // value type (one i64 field, stack-allocated).
                // WHEN OBSOLETE: when PanicInfo is assigned a proper BitCopy
                // ResourceMarker in builtin_type_classes (or when that
                // classification mechanism is replaced by a richer type system
                // that infers value-semantics from struct shape).
                // REAL SOLUTION: ResourceMarker::BitCopy for PanicInfo in the
                // type-class registry, or a type-system query on struct shape.
                if matches!(name.as_str(), "LocalPid" | "ActorRef" | "Actor" | "PanicInfo") =>
            {
                true
            }
            ResolvedTy::Named { name, args } if args.is_empty() => {
                self.actor_layouts.contains_key(name)
            }
            _ => actor_name_from_handle_ty(ty).is_some(),
        }
    }

    fn mark_returned_binding_moved(&mut self, expr: &HirExpr) {
        let HirExprKind::BindingRef {
            resolved: ResolvedRef::Binding(id),
            ..
        } = expr.kind
        else {
            return;
        };
        self.mark_binding_moved(id);
    }

    fn mark_binding_moved(&mut self, id: BindingId) {
        self.owned_locals.retain(|(binding, _, _)| *binding != id);
    }
}

/// Project a Checked MIR finding to a `MirDiagnostic` for the CLI
/// rejection surface. `CheckedMirFunction::checks` is the single
/// source of truth for move/borrow/init legality; this function
/// adapts those findings to the older `MirDiagnostic` channel the
/// driver already consumes. Variants whose construction surface
/// isn't yet wired (`Aliasing`, `GeneratorBorrowAcrossYield`,
/// `ActorSendEscape`) cannot appear today; they yield `None` defensively.
fn check_to_diagnostic(check: &MirCheck) -> Option<MirDiagnostic> {
    match check {
        MirCheck::UseAfterConsume {
            binding,
            name,
            consumed_at,
            used_at,
        } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::UseAfterConsume {
                binding: *binding,
                name: name.clone(),
                consumed_at: *consumed_at,
                used_at: *used_at,
            },
            note: "binding is used after an owned value move in checked MIR".to_string(),
        }),
        MirCheck::InitialisedBeforeUse {
            binding,
            name,
            use_site,
        } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::InitialisedBeforeUse {
                binding: *binding,
                name: name.clone(),
                use_site: *use_site,
            },
            note: "binding is read before any initialising `let` for it appears".to_string(),
        }),
        MirCheck::DecisionMapTotal { offending_sites } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::DecisionMapTotal {
                offending_sites: offending_sites.clone(),
            },
            note: "DecisionFact carries Strategy::UnknownBlocked at MIR boundary; \
                   the emitter must never receive an undecided value-class site"
                .to_string(),
        }),
        MirCheck::MustConsume {
            binding,
            name,
            exit_site,
            ty,
        } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::MustConsume {
                binding: *binding,
                name: name.clone(),
                exit_site: *exit_site,
                ty: ty.clone(),
            },
            note: "@linear binding reached an exit without being consumed; \
                   declare a consuming method (e.g. `commit(consuming self)`) \
                   and ensure every reachable exit path invokes one"
                .to_string(),
        }),
        MirCheck::DropPlanUndetermined { block, reason } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::DropPlanUndetermined {
                block: *block,
                reason: reason.clone(),
            },
            note: "drop-elaboration could not determine the per-exit live-set \
                   for an M2 substrate handle (Duplex / lambda-actor / \
                   half-handle); the elaborator aborts rather than emit a \
                   partial drop plan (LESSONS cleanup-all-exits)"
                .to_string(),
        }),
        MirCheck::ContextBoundaryViolation {
            function,
            block,
            kind,
            reason,
        } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::ContextBoundaryViolation {
                function: function.clone(),
                block: *block,
                kind,
                reason: reason.clone(),
            },
            note: "actor-handler execution context markers are structurally invalid".to_string(),
        }),
        MirCheck::ContextBindingEscapes { place, block } => Some(MirDiagnostic {
            kind: MirDiagnosticKind::ContextBindingEscapes {
                place: *place,
                block: *block,
            },
            note: "context-derived MIR place escapes past ExitContext".to_string(),
        }),
        // No construction surface in the v0.5 integer spine. The
        // corresponding `MirDiagnosticKind` projections will land
        // alongside the construction surface for borrows, generators,
        // and actor sends.
        MirCheck::Aliasing { .. }
        | MirCheck::GeneratorBorrowAcrossYield { .. }
        | MirCheck::ActorSendEscape { .. }
        | MirCheck::ActorAskEscape { .. } => None,
    }
}

/// Drop-elaboration pass over a `CheckedMirFunction`.
///
/// Produces an `ElaboratedMirFunction` whose `blocks` + `drop_plans`
/// describe, structurally, what drops fire on every exit edge of the
/// function. The pass is intraprocedural and uses the
/// `DecisionFact::value_class` data already on the checked MIR (no
/// cross-join coalescing — council R-C3.1 / plan §5 commit 2: drops
/// fire at each exit independently; full NLL precision is deferred to
/// v0.6).
///
/// Algorithm per HEW-SPEC §3.7.8.4 (lexical scope teardown):
///   1. Walk the builder's `owned_locals` ledger (the per-function
///      ordered list of non-`BitCopy` bindings introduced by `let`).
///      The ledger is already maintained in source/declaration order
///      with bindings removed when consumed (`mark_binding_moved`).
///   2. For every `Terminator::Return` exit, emit a `DropPlan` whose
///      `drops` are the live owned-local list in reverse declaration
///      order (LIFO). `If`-lowering (Slice 2) constructs
///      `Terminator::Branch` and `Terminator::Goto` in addition to
///      `Terminator::Return`; `enumerate_exits` handles all three.
///   3. For declared-but-not-constructed terminators (`Panic`, `Yield`,
///      `Send`, `Call`), the pass enumerates them with an empty drop
///      plan when reached — later cluster additions add the construction
///      surfaces that turn these into populated plans.
///   4. A `BlockKind::Cleanup` block is emitted ONLY when a
///      `Terminator::Panic` is constructed in the function's CFG
///      (currently no spine surface — declared scaffold). Same for
///      `ExitPath::Cancel` (scope-structural cancellation, also
///      declared scaffold in v0.5).
///
/// Drop classification:
///   - `ValueClass::AffineResource` -> `ElabDrop { drop_fn: Some("<TypeName>::close") }`
///     (synthesised name; once `@resource` types reach the spine subset,
///     this is replaced by the resolved `FnId` of the type's `close`
///     consuming method).
///   - `ValueClass::Linear` -> NO implicit drop emitted. The move-checker
///     is the proof-of-consume; an unconsumed `Linear` binding has
///     already been rejected as `MirCheck::MustConsume` upstream.
///   - All other classes -> no drop emitted (`BitCopy`, `CowValue`, `View`,
///     `PersistentShare`, `Unknown` — `Unknown` is itself an upstream
///     rejection).
fn elaborate(
    checked: &CheckedMirFunction,
    builder: &Builder,
    flat_statements: &[MirStatement],
    dataflow_result: &dataflow::DataflowResult,
) -> ElaboratedMirFunction {
    // Statements stream: retained for snapshot/compat continuity with
    // the pre-Cluster-3 elaborator. Every non-`BitCopy` owned local
    // gets a checker-stream `Drop` entry in reverse-declaration order;
    // the structural drop plan in `drop_plans` is the authoritative
    // per-`ExitPath` answer. The flat stream is the union of every
    // block's `statements` in construction order — Slice 1 maintains
    // pre-CFG snapshot continuity by feeding the same union here.
    let mut elaborated_statements: Vec<MirStatement> = flat_statements.to_vec();
    for (binding, name, ty) in builder.owned_locals.iter().rev() {
        elaborated_statements.push(MirStatement::Drop {
            binding: *binding,
            name: name.clone(),
            ty: ty.clone(),
        });
    }

    // Function-wide LIFO drop sequence — one ElabDrop per
    // AffineResource owned local in reverse declaration order. The
    // per-Return-block exit live-set then narrows this sequence to
    // bindings still Live at that block's exit (drops fire only for
    // bindings whose state is Live at the exit; Consumed / Uninit
    // skip; MaybeConsumed is rejected upstream by the move-checker).
    let lifo_drops = build_lifo_drops(
        &builder.owned_locals,
        &builder.binding_locals,
        &builder.type_classes,
    );
    let (elab_blocks, drop_plans) = enumerate_exits(
        &checked.blocks,
        &lifo_drops,
        &dataflow_result.exit_states,
        &builder.binding_locals,
        &checked
            .cooperate_sites
            .iter()
            .map(|site| site.bb_id)
            .collect::<HashSet<_>>(),
    );

    ElaboratedMirFunction {
        name: checked.name.clone(),
        return_ty: checked.return_ty.clone(),
        statements: elaborated_statements,
        decisions: builder.decisions.clone(),
        blocks: elab_blocks,
        drop_plans,
        coroutine: None,
        // Lambda-actor capture set, populated by the MIR producer at
        // each `HirExprKind::SpawnLambdaActor` site (see
        // `Builder::lower_spawn_lambda_actor`). The HIR resolver
        // forward-binds the lambda's own let-name before lowering the
        // body, so a body-internal reference to that name resolves to
        // a `BindingRef { resolved: Binding(let_id) }`; the resolver
        // classifies that capture as `HirCaptureKind::Weak` and every
        // other free-variable reference as `Strong`. The MIR producer
        // copies the list through with the source binding's MIR
        // `Place` attached. The structural fail-closed checker
        // `validate_lambda_captures` enforces the invariants (Weak
        // attaches to LambdaActorHandle; at most one Weak per actor
        // handle) on the emitted ledger.
        lambda_captures: builder.lambda_captures.clone(),
    }
}

/// Owning-block id for an `ExitPath`. Every variant carries a `block`
/// field — surfacing it as a single function keeps `validate_drop_plan`
/// uniform across exit kinds.
#[must_use]
fn exit_block_id(exit: &ExitPath) -> u32 {
    match *exit {
        ExitPath::Return { block }
        | ExitPath::Goto { block, .. }
        | ExitPath::Branch { block, .. }
        | ExitPath::Call { block, .. }
        | ExitPath::Panic { block }
        | ExitPath::Cancel { block }
        | ExitPath::Yield { block, .. }
        | ExitPath::Send { block, .. }
        | ExitPath::Ask { block, .. }
        | ExitPath::Select { block, .. } => block,
    }
}

/// Human-readable label for an `ExitPath` discriminator — surfaced in
/// `DropPlanUndetermined` diagnostics so the rejected exit is named.
#[must_use]
fn exit_kind_label(exit: &ExitPath) -> &'static str {
    match exit {
        ExitPath::Return { .. } => "Return",
        ExitPath::Goto { .. } => "Goto",
        ExitPath::Branch { .. } => "Branch",
        ExitPath::Call { .. } => "Call",
        ExitPath::Panic { .. } => "Panic",
        ExitPath::Cancel { .. } => "Cancel",
        ExitPath::Yield { .. } => "Yield",
        ExitPath::Send { .. } => "Send",
        ExitPath::Ask { .. } => "Ask",
        ExitPath::Select { .. } => "Select",
    }
}

/// Structural validation of an elaborated drop plan. Walks every
/// `(ExitPath, DropPlan)` entry and every `ElabBlock.drops` cleanup
/// list, verifying that each drop's `kind` matches what the drop's
/// `place` would select via `drop_kind_for`, that the per-block
/// consume-on-split invariant holds, and that the lambda-actor
/// capture side-table honours the weak-ref discipline. A mismatch
/// indicates the elaborator's drop-plan construction lost coherence —
/// surface as `MirCheck::DropPlanUndetermined` so the program is
/// rejected before codegen observes a partial / inconsistent plan.
///
/// This is the M2 substrate's fail-closed boundary: a `Place::
/// DuplexHandle` paired with `DropKind::Resource` would otherwise
/// route through the generic `close` method dispatch instead of the
/// runtime's close-both-directions protocol — silently dropping the
/// recv-direction queue. Same idea for `LambdaActorHandle` paired
/// with `DropKind::DuplexClose` (would skip the actor's stop-
/// protocol).
///
/// The walk covers EVERY exit-path discriminator, not just `Return`:
///   - `Return` is the canonical Hew exit; carries the function-wide
///     LIFO drops narrowed by per-block live-set.
///   - `Panic` and `Cancel` exits transfer to a cleanup block whose
///     `ElabBlock.drops` carry the same LIFO drops; both the
///     `DropPlan` and the destination cleanup block's `drops` are
///     validated.
///   - `Yield`, `Send`, `Select` exits carry empty `DropPlan`s today
///     (per-arm cleanup lives in codegen for `Select`; coroutine and
///     actor-send surfaces have no construction site on the integer
///     spine) but the walk treats them uniformly — when a future
///     surface populates a non-empty plan, it is checked the same
///     way without retrofitting.
///   - `Goto`, `Branch`, `Call` carry empty `DropPlan`s (intra-CFG
///     edges that don't fire drops) but are walked for forward
///     compatibility.
///
/// `ElabBlock.drops` are walked symmetrically so a malformed cleanup
/// block (e.g. a panic-cleanup block with an over-broad close-both-
/// dirs drop on a half-handle binding) is rejected at the same
/// boundary.
///
/// LESSONS: boundary-fail-closed, cleanup-all-exits.
#[must_use]
fn validate_drop_plan(elab: &ElaboratedMirFunction) -> Vec<MirCheck> {
    let mut findings = Vec::new();
    for (exit, plan) in &elab.drop_plans {
        let block = exit_block_id(exit);
        let kind_label = exit_kind_label(exit);
        for drop in &plan.drops {
            let expected = drop_kind_for(drop.place, &drop.ty);
            if drop.kind != expected {
                findings.push(MirCheck::DropPlanUndetermined {
                    block,
                    reason: format!(
                        "drop on place {:?} has kind {:?}, but the place \
                         variant selects {:?}; elaborator must use the \
                         Place-driven kind (exit path: {kind_label})",
                        drop.place, drop.kind, expected,
                    ),
                });
            }
        }
        check_duplex_split_state(block, &plan.drops, &mut findings);
    }
    // Cleanup block drops are the panic / cancel landing pad. Validate
    // the same invariants against ElabBlock.drops so a malformed
    // cleanup block surfaces at the same boundary as a malformed
    // DropPlan.
    for block in &elab.blocks {
        for drop in &block.drops {
            let expected = drop_kind_for(drop.place, &drop.ty);
            if drop.kind != expected {
                findings.push(MirCheck::DropPlanUndetermined {
                    block: block.id,
                    reason: format!(
                        "cleanup drop on place {:?} has kind {:?}, but the \
                         place variant selects {:?}; elaborator must use the \
                         Place-driven kind",
                        drop.place, drop.kind, expected,
                    ),
                });
            }
        }
        check_duplex_split_state(block.id, &block.drops, &mut findings);
    }
    validate_lambda_captures(&elab.lambda_captures, &mut findings);
    findings
}

/// Lambda-actor capture invariants. The capture side-table encodes the
/// runtime's self-binding weak-ref discipline (§5.9 ratification 2):
/// the recursive forward-bind case
///
/// ```hew
/// let fib = actor |n| { ... fib(n - 1) ... };
/// ```
///
/// captures the lambda's own let-binding name as a `Weak` reference so
/// the body does NOT keep the actor alive past external refcount zero.
///
/// Two structural invariants:
///
/// 1. A `Weak` capture must attach to a `LambdaActorHandle`. Attaching
///    a `Weak` capture to any other `Place` (a `DuplexHandle`, a plain
///    `Local`, etc.) would silently relax the refcount discipline on a
///    non-actor resource.
/// 2. At most ONE `Weak` capture per `LambdaActorHandle`. The self-
///    binding-name discipline is a single-name discipline — every
///    lambda has exactly one let-binding name, so a second `Weak`
///    capture on the same actor handle is a lowering bug.
///
/// (a) is the existing "Weak must attach to `LambdaActorHandle`" check.
/// (b) is the new "exactly one Weak per `LambdaActorHandle`" check. The
/// non-recursive lambda case (`let f = actor |n| { n + 1 }`) has zero
/// `Weak` captures and is silently accepted — the discipline only
/// applies when the body references its own binding name.
///
/// LESSONS: boundary-fail-closed, raii-null-after-move (the weak-ref
/// is the actor's null-after-move equivalent for the self-binding
/// reference).
fn validate_lambda_captures(captures: &[LambdaCapture], findings: &mut Vec<MirCheck>) {
    use std::collections::BTreeMap;

    for capture in captures {
        if matches!(capture.capture_kind, crate::model::CaptureKind::Weak)
            && !matches!(capture.actor_handle, Place::LambdaActorHandle(_))
        {
            findings.push(MirCheck::DropPlanUndetermined {
                block: 0,
                reason: format!(
                    "weak capture of `{}` attached to non-lambda-actor handle \
                     {:?}; weak captures are exclusive to LambdaActorHandle \
                     places (§5.9 ratification 2)",
                    capture.name, capture.actor_handle,
                ),
            });
        }
    }

    // Tally Weak captures per LambdaActorHandle. The self-binding-name
    // discipline is a single-name discipline — every lambda has
    // exactly one let-binding name, so multiple Weak captures on the
    // same actor handle indicate a lowering bug.
    let mut weak_per_actor: BTreeMap<u32, Vec<&str>> = BTreeMap::new();
    for capture in captures {
        if !matches!(capture.capture_kind, crate::model::CaptureKind::Weak) {
            continue;
        }
        let Place::LambdaActorHandle(n) = capture.actor_handle else {
            continue; // already rejected above
        };
        weak_per_actor
            .entry(n)
            .or_default()
            .push(capture.name.as_str());
    }
    for (handle_id, names) in weak_per_actor {
        if names.len() > 1 {
            findings.push(MirCheck::DropPlanUndetermined {
                block: 0,
                reason: format!(
                    "LambdaActorHandle({handle_id}) has {} weak captures ({}); \
                     the self-binding-name weak-ref discipline is exactly one \
                     per actor (§5.9 ratification 2)",
                    names.len(),
                    names.join(", "),
                ),
            });
        }
    }
}

/// Parent-local id for a Duplex-family Place. `DuplexHandle(N)`,
/// `SendHalf(N)`, and `RecvHalf(N)` all reference the same parent
/// Duplex local `N`; the variants only differ in which directions
/// the drop closes. Returns `None` for non-Duplex-family Places.
#[must_use]
fn duplex_parent_local(place: Place) -> Option<u32> {
    match place {
        Place::DuplexHandle(n) | Place::SendHalf(n) | Place::RecvHalf(n) => Some(n),
        Place::LambdaActorHandle(_)
        | Place::ActorHandle(_)
        | Place::Local(_)
        | Place::ReturnSlot => None,
    }
}

/// Consume-on-split invariant for `Duplex<S, R>` handles.
///
/// A `Duplex` may be addressed by either the unified `DuplexHandle`
/// (close-both-dirs) or by the pair of direction-aliases
/// `SendHalf` / `RecvHalf` (close-one-dir each). The split operation
/// (`.send_half()` / `.recv_half()`) MOVES the underlying handle: after
/// a split, the original `DuplexHandle` binding is uninhabited and only
/// the half-handle binding(s) remain in the drop plan. If a stale
/// `DuplexHandle(N)` coexists with `SendHalf(N)` or `RecvHalf(N)` in the
/// same drop plan, codegen would emit close-both on the unified handle
/// AND close-one on the half — closing the same direction twice and, in
/// the runtime's refcount discipline, closing a direction that the
/// half-handle binding now owns. Same shape if two `SendHalf(N)` or two
/// `RecvHalf(N)` entries coexist (the half-handle is also a moved
/// resource; aliased drops would close the same queue twice).
///
/// The plan is sound iff, for each parent local `N`, the drop plan
/// contains at most one of `{DuplexHandle(N), <SendHalf(N) + RecvHalf(N)
/// pair>, SendHalf(N) alone, RecvHalf(N) alone}` — never a mix and
/// never duplicates within a half-class.
///
/// LESSONS: raii-null-after-move (consume-on-split is the affine
/// move-checker discipline expressed at the drop-plan layer),
/// cleanup-all-exits (each direction closes exactly once per exit
/// path), boundary-fail-closed (a stale-handle drop plan is rejected
/// before codegen observes it).
///
/// NOTE: the HIR construction surface for `.send_half()` /
/// `.recv_half()` is slice-4 work (`hew-types/src/builtin_names.rs:270`
/// WHEN-OBSOLETE marker). Until that surface exists, the upstream
/// lowering never EMITS a drop plan that violates the invariant; this
/// check is the structural backstop that fails closed when slice 4
/// wires the split methods through MIR. The synthetic property tests
/// in `slice3_invariants` exercise every legal and illegal split-state
/// shape against this checker today.
fn check_duplex_split_state(block: u32, drops: &[ElabDrop], findings: &mut Vec<MirCheck>) {
    use std::collections::BTreeMap;

    #[derive(Default)]
    struct PerParent {
        whole: u32,
        send: u32,
        recv: u32,
    }

    let mut per_parent: BTreeMap<u32, PerParent> = BTreeMap::new();
    for drop in drops {
        let Some(parent) = duplex_parent_local(drop.place) else {
            continue;
        };
        let entry = per_parent.entry(parent).or_default();
        match drop.place {
            Place::DuplexHandle(_) => entry.whole = entry.whole.saturating_add(1),
            Place::SendHalf(_) => entry.send = entry.send.saturating_add(1),
            Place::RecvHalf(_) => entry.recv = entry.recv.saturating_add(1),
            _ => {}
        }
    }

    for (parent, counts) in per_parent {
        // Whole + any half is the "stale unified handle after split"
        // bug: split would have moved the DuplexHandle out, so its
        // presence alongside a half means codegen would close the same
        // direction twice.
        if counts.whole > 0 && (counts.send > 0 || counts.recv > 0) {
            findings.push(MirCheck::DropPlanUndetermined {
                block,
                reason: format!(
                    "drop plan contains both DuplexHandle({parent}) and a half-handle \
                     (send={}, recv={}) for the same parent local; the split \
                     operation must consume the DuplexHandle (slice-4 HIR seam \
                     for .send_half() / .recv_half() must MOVE the unified handle)",
                    counts.send, counts.recv,
                ),
            });
        }
        // Multiple DuplexHandle entries for the same parent: should be
        // structurally impossible (one binding per local), but reject
        // defensively so a duplicated drop emission is caught.
        if counts.whole > 1 {
            findings.push(MirCheck::DropPlanUndetermined {
                block,
                reason: format!(
                    "drop plan contains {} DuplexHandle({parent}) entries for the \
                     same parent local; close-both-dirs must fire exactly once",
                    counts.whole,
                ),
            });
        }
        // Aliased halves: two SendHalf(N) or two RecvHalf(N) would
        // close the same queue twice. The split methods return a
        // single half per direction, so duplicates are a lowering bug.
        if counts.send > 1 {
            findings.push(MirCheck::DropPlanUndetermined {
                block,
                reason: format!(
                    "drop plan contains {} SendHalf({parent}) entries; \
                     the S-direction must close exactly once",
                    counts.send,
                ),
            });
        }
        if counts.recv > 1 {
            findings.push(MirCheck::DropPlanUndetermined {
                block,
                reason: format!(
                    "drop plan contains {} RecvHalf({parent}) entries; \
                     the R-direction must close exactly once",
                    counts.recv,
                ),
            });
        }
    }
}

/// Per-`Duplex` consume state at a program point. Tracks the
/// affine move-checker discipline expressed at the backend
/// instruction layer: a `.send_half()` / `.recv_half()` split
/// emits an `Instr::Move { src: Place::DuplexHandle(N), dest:
/// Place::SendHalf(N) | Place::RecvHalf(N) }`, which moves the
/// unified handle out and leaves only the half-handle binding(s)
/// live in the drop plan.
///
/// Lattice semantics over the CFG meet:
///   - `Live ⊓ Live = Live`
///   - `Live ⊓ Consumed = MaybeConsumed` (some-path-consumed)
///   - `Consumed ⊓ Consumed = Consumed`
///   - `MaybeConsumed ⊓ X = MaybeConsumed`
///
/// Either `Consumed` or `MaybeConsumed` at a block whose drop plan
/// contains `DuplexHandle(N)` is rejected — `Consumed` because the
/// drop is structurally stale, `MaybeConsumed` because the program
/// could close the same direction twice on some paths
/// (fail-closed for ambiguous shapes per the M2 substrate's
/// `boundary-fail-closed` discipline).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum DuplexSplitState {
    /// No `.send_half()` / `.recv_half()` consume of the unified
    /// handle has been observed on any reaching path.
    Live,
    /// The unified handle was moved on every reaching path; carries
    /// the earliest split-emitting block id for diagnostic anchoring.
    Consumed(u32),
    /// The unified handle was moved on some-but-not-all reaching
    /// paths; the drop plan cannot decide whether to fire the close.
    /// Carries the earliest split-emitting block id.
    MaybeConsumed(u32),
}

impl DuplexSplitState {
    /// Lattice meet over the three-state space. Commutative,
    /// associative, idempotent — pinned by tests below.
    fn meet(self, other: DuplexSplitState) -> DuplexSplitState {
        use DuplexSplitState::{Consumed, Live, MaybeConsumed};
        match (self, other) {
            (Live, Live) => Live,
            (Live, Consumed(b) | MaybeConsumed(b)) | (Consumed(b) | MaybeConsumed(b), Live) => {
                MaybeConsumed(b)
            }
            (Consumed(a), Consumed(b)) => Consumed(a.min(b)),
            (Consumed(a), MaybeConsumed(b)) | (MaybeConsumed(a), Consumed(b)) => {
                MaybeConsumed(a.min(b))
            }
            (MaybeConsumed(a), MaybeConsumed(b)) => MaybeConsumed(a.min(b)),
        }
    }
}

/// Walk the backend `Instr` stream across a function's CFG and
/// reject drop plans that fire `Place::DuplexHandle(N)` on a block
/// whose reaching paths have already moved the unified handle into
/// a `SendHalf` / `RecvHalf` split.
///
/// The slice-3 structural checker (`check_duplex_split_state`) catches
/// only same-drop-list collisions of `DuplexHandle(N)` with
/// `SendHalf(N)` / `RecvHalf(N)`. It cannot detect the cross-block
/// case where block A emits the split and block B (a successor)
/// drops the now-stale `DuplexHandle(N)`. This dataflow closes the
/// gap.
///
/// The transfer function scans each block's instructions for
/// `Instr::Move { src: Place::DuplexHandle(N), dest:
/// Place::SendHalf(N) | Place::RecvHalf(N) }` and transitions the
/// parent's state to `Consumed(block_id)`. Inter-block, the entry
/// state of each block is the meet over predecessor exit states;
/// the entry block starts with every parent `Live`.
///
/// The CFG is acyclic in v0.5 (the loop cluster is deferred), so
/// the worklist terminates in one RPO pass. The check then iterates
/// every `(ExitPath, DropPlan)` plus every cleanup
/// `ElabBlock.drops`, looking for `DuplexHandle(N)` drops whose
/// owning block's exit state is `Consumed` (definitely stale) or
/// `MaybeConsumed` (ambiguous). Either case produces a
/// `MirCheck::DropPlanUndetermined`.
///
/// Cleanup blocks inherit the state of the normal block they
/// trap-cleanup from — `enumerate_exits` clones the normal-path
/// `drops_template` unfiltered into the cleanup block's drops, so a
/// `DuplexHandle` left in the cleanup block's drop list reflects a
/// program shape where the normal-path consume already happened
/// but the cleanup path was forgotten. Validate against the normal
/// predecessor's exit state.
///
/// LESSONS: cleanup-all-exits, raii-null-after-move,
/// boundary-fail-closed.
#[allow(
    clippy::too_many_lines,
    reason = "single fixpoint + drop-list-walk; splitting would obscure the dataflow"
)]
fn validate_cross_block_split_consume(
    blocks: &[BasicBlock],
    elab: &ElaboratedMirFunction,
) -> Vec<MirCheck> {
    use std::collections::{BTreeMap, VecDeque};

    let mut findings = Vec::new();
    if blocks.is_empty() {
        return findings;
    }

    // Per-block per-parent exit state. Absent entries are implicitly
    // `Live` — the most-permissive default. The map carries only
    // parents whose state diverged from `Live` on at least one
    // reaching path; this keeps the lattice sparse.
    let mut exit_states: HashMap<u32, BTreeMap<u32, DuplexSplitState>> = HashMap::new();
    let by_id: HashMap<u32, &BasicBlock> = blocks.iter().map(|b| (b.id, b)).collect();

    // Predecessor edges for the meet step.
    let mut preds: HashMap<u32, Vec<u32>> = HashMap::new();
    for block in blocks {
        let mut emit = |target: u32| preds.entry(target).or_default().push(block.id);
        match &block.terminator {
            Terminator::Return | Terminator::Trap { .. } => {}
            Terminator::Goto { target } => emit(*target),
            Terminator::Branch {
                then_target,
                else_target,
                ..
            } => {
                emit(*then_target);
                emit(*else_target);
            }
            Terminator::Call { next, .. }
            | Terminator::Yield { next, .. }
            | Terminator::Send { next, .. }
            | Terminator::Ask { next, .. }
            | Terminator::Select { next, .. } => emit(*next),
        }
    }

    let successors = |block: &BasicBlock| -> Vec<u32> {
        match &block.terminator {
            Terminator::Return | Terminator::Trap { .. } => Vec::new(),
            Terminator::Goto { target } => vec![*target],
            Terminator::Branch {
                then_target,
                else_target,
                ..
            } => vec![*then_target, *else_target],
            Terminator::Call { next, .. }
            | Terminator::Yield { next, .. }
            | Terminator::Send { next, .. }
            | Terminator::Ask { next, .. }
            | Terminator::Select { next, .. } => vec![*next],
        }
    };

    // Forward fixpoint. The entry block is id 0 by construction.
    let entry_id = 0;
    let mut worklist: VecDeque<u32> = blocks.iter().map(|b| b.id).collect();
    let mut all_parents: HashSet<u32> = HashSet::new();
    // Seed all_parents from any DuplexHandle/Half-handle place that
    // appears in any block's instructions — those are the only
    // parents the cross-block dataflow needs to track.
    for block in blocks {
        for instr in &block.instructions {
            for place in instr_places(instr) {
                if let Some(parent) = duplex_parent_local(place) {
                    all_parents.insert(parent);
                }
            }
        }
    }
    if all_parents.is_empty() {
        return findings;
    }

    while let Some(bb_id) = worklist.pop_front() {
        let Some(block) = by_id.get(&bb_id) else {
            continue;
        };
        let entry: BTreeMap<u32, DuplexSplitState> = if bb_id == entry_id {
            BTreeMap::new()
        } else {
            let empty = Vec::new();
            let predecessors = preds.get(&bb_id).unwrap_or(&empty);
            meet_predecessors_split(predecessors, &exit_states, &all_parents)
        };
        let new_exit = transfer_block_split(entry, block);
        let changed = exit_states.get(&bb_id).is_none_or(|prev| *prev != new_exit);
        exit_states.insert(bb_id, new_exit);
        if changed {
            for succ in successors(block) {
                worklist.push_back(succ);
            }
        }
    }

    // Inspect every drop plan / cleanup-block-drop list against the
    // owning block's exit state for the relevant parent. A stale
    // unified-handle drop (`Consumed` or `MaybeConsumed`) is rejected.
    let report_drop = |block: u32, drops: &[ElabDrop], findings: &mut Vec<MirCheck>| {
        let Some(state_map) = exit_states.get(&block) else {
            return;
        };
        for drop in drops {
            let Place::DuplexHandle(parent) = drop.place else {
                continue;
            };
            let state = state_map
                .get(&parent)
                .copied()
                .unwrap_or(DuplexSplitState::Live);
            match state {
                DuplexSplitState::Live => {}
                DuplexSplitState::Consumed(consume_block) => {
                    findings.push(MirCheck::DropPlanUndetermined {
                        block,
                        reason: format!(
                            "drop plan contains DuplexHandle({parent}) but block {consume_block} \
                             on every reaching path moves the unified handle into a half-handle \
                             (split via .send_half()/.recv_half() consumes the DuplexHandle; \
                             the half-handle binding is the only remaining owner)"
                        ),
                    });
                }
                DuplexSplitState::MaybeConsumed(consume_block) => {
                    findings.push(MirCheck::DropPlanUndetermined {
                        block,
                        reason: format!(
                            "drop plan contains DuplexHandle({parent}) but block {consume_block} \
                             on some reaching paths moves the unified handle into a half-handle \
                             (split-on-some-paths leaves the drop ambiguous; fail-closed per \
                             cleanup-all-exits)"
                        ),
                    });
                }
            }
        }
    };

    for (exit, plan) in &elab.drop_plans {
        let block = exit_block_id(exit);
        report_drop(block, &plan.drops, &mut findings);
    }
    // Cleanup blocks: validate against the cleanup block's predecessor
    // (the normal block that trapped into it). The cleanup block id
    // itself is past the highest normal-block id and has no
    // predecessor entry in `exit_states`; consult the normal block
    // whose `Terminator::Panic` produced the cleanup edge.
    for elab_block in &elab.blocks {
        if elab_block.kind != BlockKind::Cleanup {
            continue;
        }
        // Find the normal block that traps into this cleanup. The
        // `Terminator::Panic` path generated cleanup ids past the
        // max normal id; the cleanup's drops mirror the normal
        // block's pre-terminator state. Without a back-reference,
        // we conservatively validate against EVERY normal block whose
        // Terminator is Panic (the elab structure has one cleanup
        // per Panic). For each, report any stale DuplexHandle drop.
        for raw_block in blocks {
            if matches!(raw_block.terminator, Terminator::Trap { .. }) {
                report_drop(raw_block.id, &elab_block.drops, &mut findings);
            }
        }
    }

    findings
}

fn meet_predecessors_split(
    preds: &[u32],
    exit_states: &std::collections::HashMap<u32, std::collections::BTreeMap<u32, DuplexSplitState>>,
    all_parents: &std::collections::HashSet<u32>,
) -> std::collections::BTreeMap<u32, DuplexSplitState> {
    use std::collections::BTreeMap;
    if preds.is_empty() {
        return BTreeMap::new();
    }
    let mut entry = BTreeMap::new();
    for &parent in all_parents {
        let acc = preds
            .iter()
            .map(|p| {
                exit_states
                    .get(p)
                    .and_then(|m| m.get(&parent).copied())
                    .unwrap_or(DuplexSplitState::Live)
            })
            .reduce(DuplexSplitState::meet)
            .unwrap_or(DuplexSplitState::Live);
        if !matches!(acc, DuplexSplitState::Live) {
            entry.insert(parent, acc);
        }
    }
    entry
}

fn transfer_block_split(
    entry: std::collections::BTreeMap<u32, DuplexSplitState>,
    block: &BasicBlock,
) -> std::collections::BTreeMap<u32, DuplexSplitState> {
    let mut state = entry;
    for instr in &block.instructions {
        if let Instr::Move { dest, src } = instr {
            if let (Place::DuplexHandle(parent), true) = (
                *src,
                matches!(dest, Place::SendHalf(_) | Place::RecvHalf(_)),
            ) {
                // Transition the parent to Consumed by this block. A
                // prior MaybeConsumed/Consumed entry is overwritten —
                // the move-checker upstream has already determined
                // whether a re-use after consume is legal; for the
                // drop-plan check, the latest consume in this block
                // is the relevant anchor.
                state.insert(parent, DuplexSplitState::Consumed(block.id));
            }
        }
    }
    state
}

/// Return every `Place` mentioned by a backend `Instr`. Used by the
/// cross-block split-state seed pass to discover which parent
/// locals participate in the dataflow.
#[allow(
    clippy::match_same_arms,
    reason = "i64 and float arithmetic arms share the same place-extraction shape but \
              represent semantically distinct ops; consolidating would force a later \
              re-split when codegen needs per-op dispatch"
)]
fn instr_places(instr: &Instr) -> Vec<Place> {
    match instr {
        Instr::EnterContext | Instr::ExitContext | Instr::CheckCancellation => Vec::new(),
        Instr::ContextField { dest, .. } => vec![*dest],
        // ConstI64 and StringLit both produce only their dest place.
        Instr::ConstI64 { dest, .. } | Instr::StringLit { dest, .. } => vec![*dest],
        Instr::IntAdd { dest, lhs, rhs }
        | Instr::IntSub { dest, lhs, rhs }
        | Instr::IntMul { dest, lhs, rhs }
        | Instr::IntDiv { dest, lhs, rhs, .. }
        | Instr::IntRem { dest, lhs, rhs, .. }
        | Instr::IntBitAnd { dest, lhs, rhs }
        | Instr::IntBitOr { dest, lhs, rhs }
        | Instr::IntBitXor { dest, lhs, rhs }
        | Instr::IntShl { dest, lhs, rhs }
        | Instr::IntShr { dest, lhs, rhs, .. }
        | Instr::IntCmp { dest, lhs, rhs, .. }
        | Instr::IdentityCompare { dest, lhs, rhs } => vec![*dest, *lhs, *rhs],
        Instr::IntArithChecked {
            dest,
            lhs,
            rhs,
            overflow_flag,
            ..
        } => vec![*dest, *lhs, *rhs, *overflow_flag],
        Instr::Move { dest, src } => vec![*dest, *src],
        Instr::Drop { place, .. } => vec![*place],
        Instr::CallRuntimeAbi(call) => {
            // Every Place participating in the runtime call surfaces
            // here so the cross-block split-state seed pass can
            // observe handle moves through C-ABI boundaries. The
            // `dest` (when present) is also a Place the dataflow
            // needs to discover.
            let mut places: Vec<Place> = call.args().to_vec();
            if let Some(d) = call.dest() {
                places.push(d);
            }
            places
        }
        Instr::RecordInit { fields, dest, .. } => {
            let mut places: Vec<Place> = fields.iter().map(|(_, p)| *p).collect();
            places.push(*dest);
            places
        }
        Instr::RecordFieldLoad { record, dest, .. } => vec![*record, *dest],
        Instr::ActorStateFieldLoad { dest, .. } => vec![*dest],
        Instr::ActorStateFieldStore { src, .. } => vec![*src],
        Instr::TupleFieldLoad { tuple, dest, .. } => vec![*tuple, *dest],
        Instr::FloatLit { dest, .. } => vec![*dest],
        Instr::FloatAdd { dest, lhs, rhs, .. }
        | Instr::FloatSub { dest, lhs, rhs, .. }
        | Instr::FloatMul { dest, lhs, rhs, .. }
        | Instr::FloatDiv { dest, lhs, rhs, .. }
        | Instr::FloatRem { dest, lhs, rhs, .. } => vec![*dest, *lhs, *rhs],
        // Char, Unit, and Duration literals each produce only their dest place
        // (no operand places). Grouped with the existing dest-only pattern
        // for clarity; kept as separate arms per the `match_same_arms` allow
        // above — they are semantically distinct even if the extraction shape
        // is identical.
        Instr::CharLit { dest, .. } | Instr::UnitLit { dest } | Instr::DurationLit { dest, .. } => {
            vec![*dest]
        }
        Instr::MakeClosure { env, dest, .. } => vec![*env, *dest],
        Instr::ClosureEnvFieldLoad { env, dest, .. } => vec![*env, *dest],
        Instr::CallClosure {
            callee,
            args,
            ret_ty: _,
            dest,
        } => {
            let mut places: Vec<Place> = vec![*callee];
            places.extend(args.iter().copied());
            if let Some(d) = dest {
                places.push(*d);
            }
            places
        }
        Instr::SpawnTaskDirect { task, .. } => vec![*task],
        Instr::SpawnTaskClosure { task, env, .. } => vec![*task, *env],
        Instr::SpawnActor {
            state,
            init_args,
            dest,
            ..
        } => {
            let mut places = Vec::new();
            if let Some(state) = state {
                places.push(*state);
            }
            places.extend(init_args.iter().copied());
            places.push(*dest);
            places
        }
        Instr::CoerceToDynTrait { value, dest, .. } => vec![*value, *dest],
        Instr::CallTraitMethod {
            fat_pointer,
            dest,
            args,
            ..
        } => {
            let mut places: Vec<Place> = vec![*fat_pointer];
            places.extend(args.iter().copied());
            if let Some(d) = dest {
                places.push(*d);
            }
            places
        }
    }
}

/// Resolve the `DropKind` for an `ElabDrop` given the addressable
/// `Place` and the binding's `ResolvedTy`.
///
/// The M2 substrate's drop kinds are selected by the `Place` variant
/// rather than the `ResolvedTy` alone — a binding whose type is
/// `Duplex<S, R>` may be addressed by either a `DuplexHandle`
/// (close-both-dirs) or a `SendHalf` / `RecvHalf` (close-one-dir
/// alias), and the kind must follow the Place. Lambda-actor handles
/// share the underlying `Duplex<Msg, Reply>` type but use
/// `LambdaActorHandle` Place addressing so they select
/// `LambdaActorRelease` — the stop-on-last-handle-drop protocol with
/// weak-ref body capture (§5.9 ratification 2).
///
/// `Place::Local` / `Place::ReturnSlot` fall through to
/// `DropKind::Resource` — the pre-M2 generic `@resource` close path.
///
/// LESSONS: cleanup-all-exits, raii-null-after-move,
/// boundary-fail-closed (kind is selected by Place; mismatching
/// Place + `DropKind` is structurally impossible because this function
/// is the single source of truth).
/// Test-only re-export: forwards to the private `drop_kind_for`.
/// Lives in this module so the function-private invariants stay
/// non-public while still being exercisable from the integration
/// test that pins the `dyn Trait` → `DropKind::TraitObject` contract.
#[doc(hidden)]
#[must_use]
pub fn drop_kind_for_test_only(place: Place, ty: &ResolvedTy) -> DropKind {
    drop_kind_for(place, ty)
}

#[must_use]
fn drop_kind_for(place: Place, ty: &ResolvedTy) -> DropKind {
    match place {
        Place::DuplexHandle(_) => DropKind::DuplexClose,
        Place::LambdaActorHandle(_) => DropKind::LambdaActorRelease,
        Place::SendHalf(_) => DropKind::DuplexHalfClose(crate::model::Direction::Send),
        Place::RecvHalf(_) => DropKind::DuplexHalfClose(crate::model::Direction::Recv),
        // `dyn Trait` locals carry their drop ritual in the vtable's slot 0
        // (`drop_in_place`); codegen emits the GEP-to-slot-0 dispatch.
        // Discriminated by `ResolvedTy::TraitObject` rather than a Place
        // variant because trait objects share `Place::Local` storage with
        // every other by-value owned binding.
        Place::Local(_) | Place::ReturnSlot if matches!(ty, ResolvedTy::TraitObject { .. }) => {
            DropKind::TraitObject
        }
        Place::ActorHandle(_) | Place::Local(_) | Place::ReturnSlot => DropKind::Resource,
    }
}

/// LIFO drop sequence for an owned-locals ledger. Only `AffineResource`
/// contributes; `Linear` is the move-checker's responsibility (`MustConsume`),
/// and other classes have no implicit drop.
///
/// The `binding_locals` map is consulted to resolve each owned-local's
/// real backend `Place`. A binding without an entry (function parameters
/// and other surfaces that don't populate `binding_locals`) does not
/// appear in `owned_locals` either today, so the `ReturnSlot` fallback
/// arm is structurally unreachable; it survives only as a fail-soft for
/// future surfaces that may extend `owned_locals` ahead of `binding_locals`.
fn build_lifo_drops(
    owned_locals: &[(BindingId, String, ResolvedTy)],
    binding_locals: &HashMap<BindingId, Place>,
    type_classes: &hew_hir::TypeClassTable,
) -> Vec<ElabDrop> {
    let mut drops = Vec::new();
    for (binding, _name, ty) in owned_locals.iter().rev() {
        match ValueClass::of_ty(ty, type_classes) {
            ValueClass::AffineResource => {
                // Registry-driven drop_fn dispatch. The HIR-lowering pass
                // populates `type_classes` with `(marker, Some(close_method))`
                // for every `#[resource]` type; reaching this arm without
                // a `close_method` is structurally unreachable because the
                // `E_RESOURCE_MISSING_CLOSE` HIR diagnostic short-circuits
                // the pipeline upstream. The string form is preserved as a
                // failsafe; codegen rejects `Some(_)` until runtime drop
                // dispatch lands (`hew-codegen-rs/src/llvm.rs:471`).
                let drop_fn = match ty {
                    ResolvedTy::Named { name, .. } => type_classes
                        .get(name)
                        .and_then(|(_, close)| close.as_ref())
                        .map(|m| format!("{name}::{m}")),
                    // Task<T> and all other types have no user-visible close
                    // method. Task<T> drop (hew_task_await_blocking +
                    // hew_task_free) lands as a runtime ABI call in a later
                    // slice (MIR/codegen glue); no close method name here.
                    _ => None,
                };
                // Resolve to the binding's real backend place. Falling
                // back to `ReturnSlot` for an unmapped binding would
                // drop the wrong slot — fail closed instead. The
                // `stmt` handler always populates `binding_locals` for
                // any binding that reaches `owned_locals` (see
                // `HirStmtKind::Let` arm), so this expect is a builder
                // invariant. A future surface that grows
                // `owned_locals` ahead of `binding_locals` must wire
                // a real `Place` before reaching here. LESSONS:
                // boundary-fail-closed.
                let place = *binding_locals.get(binding).unwrap_or_else(|| {
                    panic!(
                        "build_lifo_drops invariant: binding {binding:?} is in owned_locals \
                         but missing from binding_locals; lowering must wire a Place before \
                         the drop-elaboration pass observes the binding"
                    )
                });
                // Drop-kind classification for the M2 substrate. The
                // pre-M2 generic `@resource` path keeps `DropKind::Resource`;
                // M2 Duplex / lambda-actor / half-handle Places select
                // the specialised kinds so codegen (slice 5) and the
                // runtime (slice 4) emit the right close protocol.
                // LESSONS: cleanup-all-exits, raii-null-after-move.
                let kind = drop_kind_for(place, ty);
                drops.push(ElabDrop {
                    place,
                    ty: ty.clone(),
                    drop_fn,
                    kind,
                });
            }
            // Linear, BitCopy, CowValue, PersistentShare, View, Unknown:
            // no implicit drop. Linear is enforced by MustConsume; the
            // rest have no drop semantics by value-class definition.
            ValueClass::Linear
            | ValueClass::BitCopy
            | ValueClass::CowValue
            | ValueClass::PersistentShare
            | ValueClass::View
            | ValueClass::Unknown => {}
        }
    }
    drops
}

/// Build the elaborated block list + per-`ExitPath` drop plans for a
/// function's CFG. Every basic block becomes one `ElabBlock` of
/// `BlockKind::Normal`; `Terminator::Panic` synthesises a sibling
/// `BlockKind::Cleanup` block. Each block's terminator maps to one
/// `(ExitPath, DropPlan)` entry. `Return`-terminated blocks narrow
/// the function-wide LIFO `lifo` sequence to bindings whose state at
/// that block's exit is `Live` — bindings already `Consumed` on
/// every reaching path do not need their drop fired again
/// (LESSONS `raii-null-after-move`). `MaybeConsumed` at a Return
/// exit is rejected upstream by the move-checker; the elaborator
/// treats it as if `Live` for drop-plan purposes, but the program
/// would have already been rejected before reaching codegen so the
/// drop list is informational.
#[allow(
    clippy::too_many_lines,
    reason = "enumerate_exits is a flat match over Terminator variants \
              with per-arm payload construction; the line count is the \
              variant count, not deep nesting"
)]
fn enumerate_exits(
    blocks: &[BasicBlock],
    lifo: &[ElabDrop],
    exit_states: &std::collections::HashMap<
        u32,
        std::collections::BTreeMap<hew_hir::BindingId, dataflow::BindingState>,
    >,
    binding_locals: &HashMap<BindingId, Place>,
    cancellation_blocks: &HashSet<u32>,
) -> (Vec<ElabBlock>, Vec<(ExitPath, DropPlan)>) {
    // Track the highest block id observed so cleanup-block ids can
    // start past it. Slice 2 onwards may emit multiple non-trivial
    // blocks; reserving cleanup ids past the max keeps invariants from
    // the single-block era intact.
    let max_normal_id = blocks.iter().map(|b| b.id).max().unwrap_or(0);
    let mut elab_blocks: Vec<ElabBlock> = blocks
        .iter()
        .map(|b| ElabBlock {
            id: b.id,
            kind: BlockKind::Normal,
            drops: Vec::new(),
            successor: None,
        })
        .collect();
    let mut next_cleanup_id = max_normal_id.saturating_add(1);
    let mut plans: Vec<(ExitPath, DropPlan)> = Vec::new();
    let drops_template = lifo.to_vec();

    // Map each owned-local's Place back to its BindingId so the
    // per-exit filter can consult exit_states. The drops in `lifo`
    // already carry the binding's Place but not its id; reverse the
    // builder's `binding_locals` (BindingId -> Place) is the cleanest
    // bridge. Builds only as large as there are owned bindings.
    let place_to_binding: std::collections::HashMap<Place, BindingId> = binding_locals
        .iter()
        .map(|(binding, place)| (*place, *binding))
        .collect();

    let drops_for_exit = |block_id: u32| -> Vec<ElabDrop> {
        let Some(state_map) = exit_states.get(&block_id) else {
            // No dataflow result for this block (defensive — every
            // reachable block has an exit_state entry after
            // analyze). Fall back to the function-wide LIFO.
            return drops_template.clone();
        };
        drops_template
            .iter()
            .filter(|drop| match place_to_binding.get(&drop.place) {
                Some(binding) => matches!(
                    state_map
                        .get(binding)
                        .copied()
                        .unwrap_or(dataflow::BindingState::Uninit),
                    dataflow::BindingState::Live | dataflow::BindingState::MaybeConsumed(_)
                ),
                // No binding mapping → conservatively keep the drop.
                // This arm guards against future surfaces that build
                // drops outside the binding_locals registry; the
                // current `build_lifo_drops` `expect()` rules out
                // this path today, but keep it for forward safety.
                None => true,
            })
            .cloned()
            .collect()
    };

    for block in blocks {
        let block_id = block.id;
        let plan = match &block.terminator {
            Terminator::Return => (
                ExitPath::Return { block: block_id },
                DropPlan {
                    drops: drops_for_exit(block_id),
                },
            ),
            Terminator::Goto { target } => (
                ExitPath::Goto {
                    block: block_id,
                    target: *target,
                },
                DropPlan::default(),
            ),
            Terminator::Branch {
                cond: _,
                then_target,
                else_target,
            } => (
                ExitPath::Branch {
                    block: block_id,
                    then_target: *then_target,
                    else_target: *else_target,
                },
                DropPlan::default(),
            ),
            Terminator::Call {
                callee,
                args: _,
                dest: _,
                next,
            } => (
                ExitPath::Call {
                    block: block_id,
                    callee: callee.clone(),
                    next: *next,
                },
                DropPlan::default(),
            ),
            Terminator::Trap { .. } => {
                // Cleanup block: same LIFO drop plan as the normal exit
                // at this scope depth; no successor (trap is terminal).
                let cleanup_id = next_cleanup_id;
                next_cleanup_id = next_cleanup_id.saturating_add(1);
                elab_blocks.push(ElabBlock {
                    id: cleanup_id,
                    kind: BlockKind::Cleanup,
                    drops: drops_template.clone(),
                    successor: None,
                });
                (
                    ExitPath::Panic { block: block_id },
                    DropPlan {
                        drops: drops_template.clone(),
                    },
                )
            }
            Terminator::Yield { value: _, next } => (
                ExitPath::Yield {
                    block: block_id,
                    next: *next,
                },
                DropPlan::default(),
            ),
            Terminator::Send {
                actor: _,
                msg_type: _,
                value: _,
                next,
            } => (
                // `actor` is a Place; the ExitPath::Send slot carries
                // the callee name. Spine has no Send construction
                // surface, so this is unreachable in practice — empty
                // placeholder name.
                ExitPath::Send {
                    block: block_id,
                    actor: String::new(),
                    next: *next,
                },
                DropPlan::default(),
            ),
            Terminator::Ask {
                actor,
                msg_type: _,
                value: _,
                reply_dest: _,
                next,
            } => (
                ExitPath::Ask {
                    block: block_id,
                    actor: *actor,
                    next: *next,
                },
                DropPlan::default(),
            ),
            Terminator::Select { arms: _, next } => (
                // Per-arm select-loser cleanup lives in codegen, not in
                // the function-wide DropPlan. The DropPlan abstraction
                // models LIFO `@resource` drops over `place + drop_fn`;
                // select-loser cleanup needs two operands (the resource
                // and the runtime-allocated registration id returned by
                // the substrate primitive) and runs at the select
                // dispatch site, not the function exit. Keeping it out
                // of DropPlan avoids stretching the ElabDrop shape to
                // cover a case it was not designed for.
                //
                // The contract codegen must honour for each arm kind:
                //
                //   - StreamNext loser: emit
                //     `hew_stream_cancel_pending_read(stream, id)`
                //     where `id` is the PendingReadId returned by the
                //     winning-side `hew_stream_poll`. The stream
                //     binding remains usable in the enclosing scope
                //     (no item consumed). See `hew-runtime::stream`
                //     for the ABI and TOCTOU contract.
                //
                //   - ActorAsk loser: withdraw the envelope by
                //     correlation id if not yet dispatched, otherwise
                //     tombstone the reply sink; a late reply is
                //     classified as OrphanedAsk and dropped silently.
                //
                //   - TaskAwait loser: cancel the task at its next
                //     safepoint via the single-task cancel primitive;
                //     the awaitable handle is torn down.
                //
                //   - AfterTimer loser: cancel the timer; no callback
                //     fires.
                //
                // LESSONS: cleanup-all-exits — every select exit path
                // gets a non-empty cleanup at the codegen dispatch
                // site; the function-wide DropPlan is intentionally
                // empty for ExitPath::Select.
                ExitPath::Select {
                    block: block_id,
                    next: *next,
                },
                DropPlan::default(),
            ),
        };
        plans.push(plan);
        if cancellation_blocks.contains(&block_id) {
            plans.push((
                ExitPath::Cancel { block: block_id },
                DropPlan {
                    drops: drops_for_exit(block_id),
                },
            ));
        }
    }
    (elab_blocks, plans)
}

// ============================================================================
// Slice 3 (M2 substrate) drop-plan invariant tests.
//
// Pin the per-Return live-set narrowing semantics + the Place->DropKind
// invariants + the weak-ref capture discipline. Built directly against
// the internal helpers (`drop_kind_for`, `validate_drop_plan`) using
// synthetic `ElaboratedMirFunction` inputs so the HIR-construction gap
// (no LambdaActor/Duplex HIR shape yet) doesn't block coverage.
// ============================================================================

#[cfg(test)]
mod slice3_invariants {
    use super::*;
    use crate::model::{CaptureKind, Direction};

    fn reader_ty(reader: ExecutionContextReader) -> ResolvedTy {
        ResolvedTy::from_ty(&reader.ty()).expect("context reader type resolves")
    }

    fn function_evaluating_context_reader(reader: ExecutionContextReader) -> HirFn {
        let ty = reader_ty(reader);
        HirFn {
            id: hew_hir::ItemId(0),
            node: hew_hir::HirNodeId(0),
            name: "handler".to_string(),
            type_params: vec![],
            params: vec![],
            return_ty: ResolvedTy::Unit,
            body: HirBlock {
                node: hew_hir::HirNodeId(1),
                scope: hew_hir::ScopeId(0),
                statements: vec![HirStmt {
                    node: hew_hir::HirNodeId(2),
                    kind: HirStmtKind::Expr(HirExpr {
                        node: hew_hir::HirNodeId(3),
                        site: hew_hir::SiteId(0),
                        ty: ty.clone(),
                        value_class: ValueClass::of_ty(&ty, &hew_hir::TypeClassTable::default()),
                        intent: IntentKind::Read,
                        kind: HirExprKind::ContextReader { reader },
                        span: 0..0,
                    }),
                    span: 0..0,
                }],
                tail: None,
                ty: ResolvedTy::Unit,
                span: 0..0,
            },
            span: 0..0,
        }
    }

    #[test]
    fn context_readers_lower_to_context_field_offsets() {
        for (reader, expected_offset) in [
            (ExecutionContextReader::ActorId, HEW_CTX_OFFSET_ACTOR_ID),
            (
                ExecutionContextReader::Supervisor,
                HEW_CTX_OFFSET_PARENT_SUPERVISOR,
            ),
            (ExecutionContextReader::TraceSpan, HEW_CTX_OFFSET_TRACE_SPAN),
        ] {
            let func = function_evaluating_context_reader(reader);
            let lowered = lower_function(
                &func,
                "handler".to_string(),
                HashMap::new(),
                &hew_hir::TypeClassTable::default(),
                &HashMap::new(),
                &HashMap::new(),
                &HashMap::new(),
                None,
                &HashSet::new(),
                &HashMap::new(),
                &HashMap::new(),
                crate::model::FunctionCallConv::ActorHandler,
            );
            let offsets: Vec<_> = lowered
                .raw
                .blocks
                .iter()
                .flat_map(|block| &block.instructions)
                .filter_map(|instr| {
                    if let Instr::ContextField { offset, .. } = instr {
                        Some(*offset)
                    } else {
                        None
                    }
                })
                .collect();
            assert_eq!(offsets, vec![expected_offset], "{reader:?}");
            assert!(
                lowered.diagnostics.is_empty(),
                "{reader:?} diagnostics: {:?}",
                lowered.diagnostics
            );
        }
    }

    /// A `Duplex<i64, i64>` `ResolvedTy` used as a stand-in payload
    /// for synthetic `ElabDrop` entries. The body of these tests
    /// cares about `Place` + `DropKind`, not the inner type detail.
    fn duplex_int_int_ty() -> ResolvedTy {
        ResolvedTy::Named {
            name: "Duplex".to_string(),
            args: vec![ResolvedTy::I64, ResolvedTy::I64],
        }
    }

    fn make_elab(
        drop_plans: Vec<(ExitPath, DropPlan)>,
        lambda_captures: Vec<LambdaCapture>,
    ) -> ElaboratedMirFunction {
        ElaboratedMirFunction {
            name: "synthetic".to_string(),
            return_ty: ResolvedTy::Unit,
            statements: vec![],
            decisions: vec![],
            blocks: vec![],
            drop_plans,
            coroutine: None,
            lambda_captures,
        }
    }

    fn make_elab_with_drops(drops: Vec<ElabDrop>) -> ElaboratedMirFunction {
        make_elab(
            vec![(ExitPath::Return { block: 0 }, DropPlan { drops })],
            vec![],
        )
    }

    // ---------- drop_kind_for: Place -> DropKind mapping ----------

    #[test]
    fn drop_kind_for_duplex_handle_selects_duplex_close() {
        let ty = duplex_int_int_ty();
        assert_eq!(
            drop_kind_for(Place::DuplexHandle(0), &ty),
            DropKind::DuplexClose
        );
    }

    #[test]
    fn drop_kind_for_lambda_actor_handle_selects_lambda_actor_release() {
        let ty = duplex_int_int_ty();
        assert_eq!(
            drop_kind_for(Place::LambdaActorHandle(0), &ty),
            DropKind::LambdaActorRelease
        );
    }

    #[test]
    fn drop_kind_for_send_half_selects_duplex_half_close_send() {
        let ty = duplex_int_int_ty();
        assert_eq!(
            drop_kind_for(Place::SendHalf(0), &ty),
            DropKind::DuplexHalfClose(Direction::Send)
        );
    }

    #[test]
    fn drop_kind_for_recv_half_selects_duplex_half_close_recv() {
        let ty = duplex_int_int_ty();
        assert_eq!(
            drop_kind_for(Place::RecvHalf(0), &ty),
            DropKind::DuplexHalfClose(Direction::Recv)
        );
    }

    #[test]
    fn drop_kind_for_local_selects_resource() {
        // Pre-M2 path: generic Resource for Local Places. Pinning this
        // is the regression guard against accidentally routing Local
        // drops through a Duplex-specific protocol.
        let ty = duplex_int_int_ty();
        assert_eq!(drop_kind_for(Place::Local(0), &ty), DropKind::Resource);
    }

    // ---------- validate_drop_plan: structural invariants ----------

    #[test]
    fn validate_drop_plan_accepts_consistent_duplex_close() {
        // A DuplexHandle paired with DuplexClose — the canonical M2
        // substrate shape. No findings.
        let drops = vec![ElabDrop {
            place: Place::DuplexHandle(0),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::DuplexClose,
        }];
        let elab = make_elab_with_drops(drops);
        assert!(
            validate_drop_plan(&elab).is_empty(),
            "consistent (DuplexHandle, DuplexClose) must not flag"
        );
    }

    #[test]
    fn validate_drop_plan_rejects_duplex_handle_with_resource_kind() {
        // A DuplexHandle paired with DropKind::Resource would silently
        // route through generic Type::close dispatch and miss the
        // close-both-directions protocol. Must surface as
        // DropPlanUndetermined.
        let drops = vec![ElabDrop {
            place: Place::DuplexHandle(7),
            ty: duplex_int_int_ty(),
            drop_fn: Some("Duplex::close".to_string()),
            kind: DropKind::Resource,
        }];
        let elab = make_elab_with_drops(drops);
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1, "exactly one finding expected");
        let MirCheck::DropPlanUndetermined { block, reason } = &findings[0] else {
            panic!("expected DropPlanUndetermined, got {:?}", findings[0]);
        };
        assert_eq!(*block, 0);
        assert!(
            reason.contains("DuplexHandle") && reason.contains("Resource"),
            "diagnostic must name both the Place and the wrong kind: {reason}"
        );
    }

    #[test]
    fn validate_drop_plan_rejects_lambda_actor_handle_with_duplex_close_kind() {
        // LambdaActorHandle MUST select LambdaActorRelease (the
        // stop-protocol with weak-ref body capture). DuplexClose would
        // skip the actor's stop protocol — silently leaking the actor.
        let drops = vec![ElabDrop {
            place: Place::LambdaActorHandle(3),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::DuplexClose,
        }];
        let elab = make_elab_with_drops(drops);
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1);
        assert!(matches!(findings[0], MirCheck::DropPlanUndetermined { .. }));
    }

    #[test]
    fn validate_drop_plan_rejects_send_half_with_close_both_kind() {
        // SendHalf MUST close one direction only; pairing with
        // DuplexClose (close-both) would over-close the recv side.
        let drops = vec![ElabDrop {
            place: Place::SendHalf(1),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::DuplexClose,
        }];
        let elab = make_elab_with_drops(drops);
        assert_eq!(validate_drop_plan(&elab).len(), 1);
    }

    #[test]
    fn validate_drop_plan_rejects_recv_half_with_send_direction() {
        // RecvHalf MUST close Direction::Recv; pairing with
        // Direction::Send would close the wrong queue.
        let drops = vec![ElabDrop {
            place: Place::RecvHalf(2),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::DuplexHalfClose(Direction::Send),
        }];
        let elab = make_elab_with_drops(drops);
        assert_eq!(validate_drop_plan(&elab).len(), 1);
    }

    #[test]
    fn validate_drop_plan_accepts_half_handle_pair_closing_both_dirs() {
        // A SendHalf + RecvHalf pair, each closing its own direction —
        // the canonical "duplex split via .send_half() / .recv_half()"
        // shape. Together they close both directions; individually
        // each is a one-direction drop.
        let drops = vec![
            ElabDrop {
                place: Place::SendHalf(0),
                ty: duplex_int_int_ty(),
                drop_fn: None,
                kind: DropKind::DuplexHalfClose(Direction::Send),
            },
            ElabDrop {
                place: Place::RecvHalf(0),
                ty: duplex_int_int_ty(),
                drop_fn: None,
                kind: DropKind::DuplexHalfClose(Direction::Recv),
            },
        ];
        let elab = make_elab_with_drops(drops);
        assert!(validate_drop_plan(&elab).is_empty());
    }

    // ---------- consume-on-split invariant for Duplex<S, R> ----------

    /// Build an `ElabDrop` addressing a Duplex-family `Place` with its
    /// canonical `DropKind`. Used by every split-state shape below.
    fn duplex_drop(place: Place) -> ElabDrop {
        let ty = duplex_int_int_ty();
        let kind = drop_kind_for(place, &ty);
        ElabDrop {
            place,
            ty,
            drop_fn: None,
            kind,
        }
    }

    #[test]
    fn split_state_whole_only_is_accepted() {
        // Pre-split: only DuplexHandle(N) is in the drop plan. Canonical
        // "no .send_half() was ever called" shape.
        let elab = make_elab_with_drops(vec![duplex_drop(Place::DuplexHandle(0))]);
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn split_state_both_halves_only_is_accepted() {
        // Post-full-split: DuplexHandle is gone (consumed by both
        // split methods), only SendHalf(N) + RecvHalf(N) remain.
        let elab = make_elab_with_drops(vec![
            duplex_drop(Place::SendHalf(0)),
            duplex_drop(Place::RecvHalf(0)),
        ]);
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn split_state_send_half_only_is_accepted() {
        // .send_half() called, RecvHalf retained on the original (rare
        // but legal: caller kept the unified handle's recv side via a
        // .recv_half() that wasn't yet called). The plan-level shape
        // is one half — codegen closes that direction; the other side
        // stays open under runtime refcount until its handle drops.
        let elab = make_elab_with_drops(vec![duplex_drop(Place::SendHalf(0))]);
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn split_state_recv_half_only_is_accepted() {
        let elab = make_elab_with_drops(vec![duplex_drop(Place::RecvHalf(0))]);
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn split_state_whole_plus_send_half_is_rejected() {
        // The stale-unified-handle bug: split would have moved the
        // DuplexHandle out, so it must NOT coexist with a half.
        // Without this rejection, codegen emits close-both on the
        // stale handle AND close-send on the half — the S-direction
        // closes twice; the R-direction closes once on a half-handle
        // binding that doesn't own it.
        let elab = make_elab_with_drops(vec![
            duplex_drop(Place::DuplexHandle(0)),
            duplex_drop(Place::SendHalf(0)),
        ]);
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1, "expected exactly one finding");
        let MirCheck::DropPlanUndetermined { reason, .. } = &findings[0] else {
            panic!("expected DropPlanUndetermined, got {:?}", findings[0]);
        };
        assert!(
            reason.contains("DuplexHandle") && reason.contains("half-handle"),
            "diagnostic must name the stale-handle conflict: {reason}"
        );
    }

    #[test]
    fn split_state_whole_plus_recv_half_is_rejected() {
        let elab = make_elab_with_drops(vec![
            duplex_drop(Place::DuplexHandle(0)),
            duplex_drop(Place::RecvHalf(0)),
        ]);
        assert_eq!(validate_drop_plan(&elab).len(), 1);
    }

    #[test]
    fn split_state_whole_plus_both_halves_is_rejected() {
        let elab = make_elab_with_drops(vec![
            duplex_drop(Place::DuplexHandle(0)),
            duplex_drop(Place::SendHalf(0)),
            duplex_drop(Place::RecvHalf(0)),
        ]);
        assert_eq!(validate_drop_plan(&elab).len(), 1);
    }

    #[test]
    fn split_state_duplicate_send_half_is_rejected() {
        // Two SendHalf(N) entries would close the S-direction twice
        // — a lowering bug (the split method returns a single half).
        let elab = make_elab_with_drops(vec![
            duplex_drop(Place::SendHalf(0)),
            duplex_drop(Place::SendHalf(0)),
        ]);
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1);
        let MirCheck::DropPlanUndetermined { reason, .. } = &findings[0] else {
            unreachable!();
        };
        assert!(reason.contains("SendHalf") && reason.contains("S-direction"));
    }

    #[test]
    fn split_state_duplicate_recv_half_is_rejected() {
        let elab = make_elab_with_drops(vec![
            duplex_drop(Place::RecvHalf(0)),
            duplex_drop(Place::RecvHalf(0)),
        ]);
        assert_eq!(validate_drop_plan(&elab).len(), 1);
    }

    #[test]
    fn split_state_duplicate_whole_is_rejected() {
        let elab = make_elab_with_drops(vec![
            duplex_drop(Place::DuplexHandle(0)),
            duplex_drop(Place::DuplexHandle(0)),
        ]);
        assert_eq!(validate_drop_plan(&elab).len(), 1);
    }

    #[test]
    fn split_state_independent_parents_do_not_interfere() {
        // Two distinct Duplex parents (N=0 and N=1). Parent 0 is in
        // the whole state; parent 1 is in the both-halves state.
        // Neither should flag — the invariant is per-parent.
        let elab = make_elab_with_drops(vec![
            duplex_drop(Place::DuplexHandle(0)),
            duplex_drop(Place::SendHalf(1)),
            duplex_drop(Place::RecvHalf(1)),
        ]);
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn split_state_each_direction_closes_exactly_once_property() {
        // Property: for every legal split-state shape on a single
        // Duplex, the emitted drop plan closes the S-direction at
        // most once AND the R-direction at most once. The four
        // legal shapes (Whole, SendOnly, RecvOnly, BothHalves) all
        // satisfy this; the rejected shapes (Whole+Send, Whole+Recv,
        // Whole+Both, dup-Send, dup-Recv, dup-Whole) violate it.
        //
        // Encoded as exhaustive enumeration matching the project's
        // existing exhaustive-small-state test style (see
        // dataflow.rs meet-lattice tests).
        let parent = 0u32;
        let candidates: Vec<(&str, Vec<Place>)> = vec![
            ("Whole", vec![Place::DuplexHandle(parent)]),
            ("SendOnly", vec![Place::SendHalf(parent)]),
            ("RecvOnly", vec![Place::RecvHalf(parent)]),
            (
                "BothHalves",
                vec![Place::SendHalf(parent), Place::RecvHalf(parent)],
            ),
        ];
        for (label, places) in candidates {
            // Count how many times each direction closes under the
            // canonical Place->DropKind mapping.
            let mut s_count = 0u32;
            let mut r_count = 0u32;
            for p in &places {
                match drop_kind_for(*p, &duplex_int_int_ty()) {
                    DropKind::DuplexClose => {
                        s_count += 1;
                        r_count += 1;
                    }
                    DropKind::DuplexHalfClose(Direction::Send) => s_count += 1,
                    DropKind::DuplexHalfClose(Direction::Recv) => r_count += 1,
                    DropKind::Resource | DropKind::LambdaActorRelease | DropKind::TraitObject => {}
                }
            }
            assert!(
                s_count <= 1 && r_count <= 1,
                "{label}: each direction must close at most once (got S={s_count}, R={r_count})"
            );
            let drops: Vec<ElabDrop> = places.into_iter().map(duplex_drop).collect();
            let elab = make_elab_with_drops(drops);
            assert!(
                validate_drop_plan(&elab).is_empty(),
                "{label}: legal split state must validate"
            );
        }
    }

    // ---------- broadened validation: every ExitPath + ElabBlock.drops ----------

    /// Build a synthetic `ElaboratedMirFunction` whose sole `DropPlan`
    /// is attached to `exit`. Used to exercise non-`Return` `ExitPath`
    /// validation under the broadened walk.
    fn make_elab_with_exit_and_drops(
        exit: ExitPath,
        drops: Vec<ElabDrop>,
    ) -> ElaboratedMirFunction {
        make_elab(vec![(exit, DropPlan { drops })], vec![])
    }

    #[test]
    fn validate_walks_panic_exit_path() {
        // A Panic exit's DropPlan is the same LIFO sequence as the
        // Return exit at the same scope. A malformed kind here would
        // be silently accepted under the Return-only walk; the
        // broadened walk must reject it.
        let bad = ElabDrop {
            place: Place::DuplexHandle(0),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::Resource, // wrong: DuplexHandle wants DuplexClose
        };
        let elab = make_elab_with_exit_and_drops(ExitPath::Panic { block: 5 }, vec![bad]);
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1, "Panic-exit plan must be validated");
        let MirCheck::DropPlanUndetermined { block, reason } = &findings[0] else {
            unreachable!();
        };
        assert_eq!(*block, 5);
        assert!(
            reason.contains("Panic"),
            "diagnostic should name the exit kind: {reason}"
        );
    }

    #[test]
    fn validate_walks_cancel_exit_path() {
        // Cancel is the scope-structural cancellation exit. Same
        // shape as Panic for drop-plan purposes.
        let bad = ElabDrop {
            place: Place::SendHalf(0),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::DuplexClose, // wrong: SendHalf wants DuplexHalfClose(Send)
        };
        let elab = make_elab_with_exit_and_drops(ExitPath::Cancel { block: 9 }, vec![bad]);
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1);
        let MirCheck::DropPlanUndetermined { block, reason } = &findings[0] else {
            unreachable!();
        };
        assert_eq!(*block, 9);
        assert!(reason.contains("Cancel"));
    }

    #[test]
    fn validate_walks_yield_send_select_exit_paths() {
        // The three forward-compat exit kinds. Their DropPlans are
        // empty on the spine, but the validator must still apply the
        // Place-driven kind check when a future surface populates
        // them. Use a malformed DuplexHandle drop on each.
        let bad = ElabDrop {
            place: Place::DuplexHandle(0),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::Resource,
        };
        for exit in [
            ExitPath::Yield { block: 1, next: 99 },
            ExitPath::Send {
                block: 2,
                actor: String::new(),
                next: 99,
            },
            ExitPath::Select { block: 3, next: 99 },
        ] {
            let expected_label = exit_kind_label(&exit);
            let elab = make_elab_with_exit_and_drops(exit, vec![bad.clone()]);
            let findings = validate_drop_plan(&elab);
            assert_eq!(
                findings.len(),
                1,
                "exit {expected_label} must be walked by validator"
            );
        }
    }

    #[test]
    fn validate_walks_goto_branch_call_exit_paths() {
        // Intra-CFG edges. Empty DropPlans on the spine, but the
        // validator walks them uniformly so a future construction
        // surface can't slip a malformed drop past.
        let bad = ElabDrop {
            place: Place::LambdaActorHandle(0),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::DuplexClose, // wrong
        };
        for exit in [
            ExitPath::Goto {
                block: 1,
                target: 99,
            },
            ExitPath::Branch {
                block: 2,
                then_target: 98,
                else_target: 99,
            },
            ExitPath::Call {
                block: 3,
                callee: "f".to_string(),
                next: 99,
            },
        ] {
            let elab = make_elab_with_exit_and_drops(exit, vec![bad.clone()]);
            let findings = validate_drop_plan(&elab);
            assert_eq!(findings.len(), 1);
        }
    }

    #[test]
    fn validate_walks_split_state_invariant_on_panic_exit() {
        // Consume-on-split applies at every exit path, not just
        // Return. A Panic cleanup with both DuplexHandle + SendHalf
        // for the same parent must be rejected.
        let elab = make_elab_with_exit_and_drops(
            ExitPath::Panic { block: 4 },
            vec![
                duplex_drop(Place::DuplexHandle(0)),
                duplex_drop(Place::SendHalf(0)),
            ],
        );
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1, "split-state must apply to Panic exit");
        assert!(matches!(findings[0], MirCheck::DropPlanUndetermined { .. }));
    }

    #[test]
    fn validate_walks_elab_block_drops() {
        // Cleanup blocks carry drops directly in ElabBlock.drops.
        // A malformed kind here must be rejected at the same
        // structural boundary.
        let bad = ElabDrop {
            place: Place::RecvHalf(0),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::DuplexHalfClose(Direction::Send), // wrong queue
        };
        let elab = ElaboratedMirFunction {
            name: "synthetic".to_string(),
            return_ty: ResolvedTy::Unit,
            statements: vec![],
            decisions: vec![],
            blocks: vec![ElabBlock {
                id: 12,
                kind: BlockKind::Cleanup,
                drops: vec![bad],
                successor: None,
            }],
            drop_plans: vec![],
            coroutine: None,
            lambda_captures: vec![],
        };
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1);
        let MirCheck::DropPlanUndetermined { block, reason } = &findings[0] else {
            unreachable!();
        };
        assert_eq!(*block, 12);
        assert!(
            reason.contains("cleanup drop"),
            "diagnostic must name the cleanup-block context: {reason}"
        );
    }

    #[test]
    fn validate_walks_elab_block_drops_split_state() {
        // Consume-on-split applies inside cleanup blocks too. A
        // cleanup block whose drops list has DuplexHandle + RecvHalf
        // for the same parent must be rejected.
        let elab = ElaboratedMirFunction {
            name: "synthetic".to_string(),
            return_ty: ResolvedTy::Unit,
            statements: vec![],
            decisions: vec![],
            blocks: vec![ElabBlock {
                id: 13,
                kind: BlockKind::Cleanup,
                drops: vec![
                    duplex_drop(Place::DuplexHandle(0)),
                    duplex_drop(Place::RecvHalf(0)),
                ],
                successor: None,
            }],
            drop_plans: vec![],
            coroutine: None,
            lambda_captures: vec![],
        };
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1);
    }

    // ---------- per-Return live-set narrowing (synthetic) ----------

    #[test]
    fn per_return_live_set_drops_exactly_match_kept_handles() {
        // The plan's invariant: at each Return block, the set of
        // Duplex/lambda-actor places dropped is exactly
        // (places-defined-in-this-fn) - (places-moved-out). Synthetic
        // shape: two Duplex handles defined, one moved out, one
        // dropped. With consistent kinds, validate_drop_plan accepts;
        // the drop list is exactly the one not-moved place.
        let kept = ElabDrop {
            place: Place::DuplexHandle(0),
            ty: duplex_int_int_ty(),
            drop_fn: None,
            kind: DropKind::DuplexClose,
        };
        let elab = make_elab_with_drops(vec![kept.clone()]);
        let return_plan = elab
            .drop_plans
            .iter()
            .find(|(e, _)| matches!(e, ExitPath::Return { .. }))
            .unwrap();
        assert_eq!(return_plan.1.drops, vec![kept]);
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn per_return_no_spurious_drops_when_all_moved_out() {
        // If every defined Place was moved out before the Return, the
        // drop list is empty. (places-defined - places-moved-out = ∅.)
        // This is the dual of the previous test.
        let elab = make_elab_with_drops(vec![]);
        let return_plan = &elab.drop_plans[0];
        assert!(return_plan.1.drops.is_empty());
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn per_return_no_missed_drops_for_three_live_handles() {
        // Three live Duplex handles at the Return — all three must
        // appear in the drop list. Order doesn't matter for this
        // invariant; codegen consumes the list in LIFO source order
        // (a separate concern). Verify count + presence.
        let drops: Vec<ElabDrop> = (0..3u32)
            .map(|i| ElabDrop {
                place: Place::DuplexHandle(i),
                ty: duplex_int_int_ty(),
                drop_fn: None,
                kind: DropKind::DuplexClose,
            })
            .collect();
        let elab = make_elab_with_drops(drops.clone());
        let return_plan = &elab.drop_plans[0];
        assert_eq!(return_plan.1.drops.len(), 3);
        for d in &drops {
            assert!(
                return_plan.1.drops.contains(d),
                "missing drop for {:?}",
                d.place
            );
        }
        assert!(validate_drop_plan(&elab).is_empty());
    }

    // ---------- weak-ref capture invariants ----------

    fn make_elab_with_captures(captures: Vec<LambdaCapture>) -> ElaboratedMirFunction {
        make_elab(vec![], captures)
    }

    #[test]
    fn weak_capture_on_lambda_actor_handle_is_accepted() {
        // The canonical §5.9 ratification 2 shape:
        //   let fib = actor |n| { ... fib(n-1) ... };
        // The body's `fib` reference is captured as Weak attached to
        // the lambda-actor's own LambdaActorHandle. Validation must
        // accept this.
        let captures = vec![LambdaCapture {
            actor_handle: Place::LambdaActorHandle(0),
            captured: BindingId(7),
            name: "fib".to_string(),
            capture_kind: CaptureKind::Weak,
        }];
        let elab = make_elab_with_captures(captures);
        assert!(
            validate_drop_plan(&elab).is_empty(),
            "Weak capture on a LambdaActorHandle is the recursive self-case (§5.9 ratification 2)"
        );
    }

    #[test]
    fn weak_capture_on_duplex_handle_is_rejected() {
        // Weak captures are exclusive to LambdaActorHandle Places.
        // Attaching a Weak capture to a Duplex handle would silently
        // relax the refcount discipline on a non-actor resource.
        let captures = vec![LambdaCapture {
            actor_handle: Place::DuplexHandle(0),
            captured: BindingId(7),
            name: "ch".to_string(),
            capture_kind: CaptureKind::Weak,
        }];
        let elab = make_elab_with_captures(captures);
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1);
        let MirCheck::DropPlanUndetermined { reason, .. } = &findings[0] else {
            panic!("expected DropPlanUndetermined, got {:?}", findings[0]);
        };
        assert!(
            reason.contains("weak capture") && reason.contains("ch"),
            "diagnostic must name the capture and identify the misuse: {reason}"
        );
    }

    #[test]
    fn weak_capture_on_local_place_is_rejected() {
        // Same invariant for a plain Local — only LambdaActorHandle
        // can host a Weak capture.
        let captures = vec![LambdaCapture {
            actor_handle: Place::Local(5),
            captured: BindingId(2),
            name: "x".to_string(),
            capture_kind: CaptureKind::Weak,
        }];
        let elab = make_elab_with_captures(captures);
        assert_eq!(validate_drop_plan(&elab).len(), 1);
    }

    #[test]
    fn strong_capture_is_accepted_on_any_handle_kind() {
        // Strong captures are unrestricted — they're the default for
        // every non-self capture and the refcount discipline is
        // strong everywhere. Pair Strong with Local, DuplexHandle,
        // and LambdaActorHandle — all should accept.
        for handle in [
            Place::Local(0),
            Place::DuplexHandle(0),
            Place::LambdaActorHandle(0),
        ] {
            let captures = vec![LambdaCapture {
                actor_handle: handle,
                captured: BindingId(1),
                name: "captured".to_string(),
                capture_kind: CaptureKind::Strong,
            }];
            let elab = make_elab_with_captures(captures);
            assert!(
                validate_drop_plan(&elab).is_empty(),
                "Strong capture on {handle:?} must be accepted"
            );
        }
    }

    #[test]
    fn multiple_captures_one_weak_one_strong_validates_correctly() {
        // Mixed-capture case: the self-binding-name is Weak (on the
        // actor's own LambdaActorHandle), a non-self captured value
        // is Strong. Both must coexist without findings.
        let captures = vec![
            LambdaCapture {
                actor_handle: Place::LambdaActorHandle(0),
                captured: BindingId(1),
                name: "fib".to_string(),
                capture_kind: CaptureKind::Weak,
            },
            LambdaCapture {
                actor_handle: Place::LambdaActorHandle(0),
                captured: BindingId(2),
                name: "memo".to_string(),
                capture_kind: CaptureKind::Strong,
            },
        ];
        let elab = make_elab_with_captures(captures);
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn non_recursive_lambda_has_zero_weak_captures() {
        // The non-recursive lambda case: `let f = actor |n| { n + 1 }`.
        // The body does not reference its own binding name, so the
        // capture set contains zero Weak entries. The plan validates;
        // any Strong captures (closed-over outer bindings) coexist
        // freely.
        let captures = vec![LambdaCapture {
            actor_handle: Place::LambdaActorHandle(0),
            captured: BindingId(1),
            name: "outer".to_string(),
            capture_kind: CaptureKind::Strong,
        }];
        let elab = make_elab_with_captures(captures);
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn lambda_actor_capture_without_backend_slot_is_diagnostic() {
        let body = HirExpr {
            node: hew_hir::HirNodeId(1),
            site: hew_hir::SiteId(1),
            value_class: ValueClass::BitCopy,
            ty: ResolvedTy::Unit,
            intent: IntentKind::Read,
            kind: HirExprKind::Literal(HirLiteral::Unit),
            span: 0..0,
        };
        let expr = HirExpr {
            node: hew_hir::HirNodeId(2),
            site: hew_hir::SiteId(42),
            value_class: ValueClass::BitCopy,
            ty: ResolvedTy::Unit,
            intent: IntentKind::Read,
            kind: HirExprKind::SpawnLambdaActor {
                params: vec![],
                reply_ty: ResolvedTy::Unit,
                body: Box::new(body),
                captures: vec![hew_hir::HirLambdaCapture {
                    binding: BindingId(99),
                    name: "missing".to_string(),
                    kind: hew_hir::HirCaptureKind::Strong,
                }],
            },
            span: 0..0,
        };
        let mut builder = Builder::default();

        let _ = builder.lower_spawn_lambda_actor(&expr);

        assert!(
            builder.diagnostics.iter().any(|diag| matches!(
                diag.kind,
                MirDiagnosticKind::CannotMaterializeClosureCapture {
                    binding: BindingId(99),
                    site: hew_hir::SiteId(42),
                    ..
                }
            )),
            "missing backend slot must be diagnosed, got {:?}",
            builder.diagnostics
        );
        assert!(
            builder.lambda_captures.is_empty(),
            "unmaterialized captures must not enter lambda_captures"
        );
    }

    #[test]
    fn recursive_lambda_has_exactly_one_weak_capture() {
        // The canonical forward-bind recursive shape:
        //   let fib = actor |n| { ... fib(n - 1) ... };
        // The capture-set discovery (slice 4) must emit EXACTLY ONE
        // Weak capture (the `fib` self-reference) plus zero-or-more
        // Strong captures for outer bindings. Pin the "exactly one"
        // half of the discipline as a structural invariant on the
        // synthetic capture list.
        let captures = vec![LambdaCapture {
            actor_handle: Place::LambdaActorHandle(0),
            captured: BindingId(7),
            name: "fib".to_string(),
            capture_kind: CaptureKind::Weak,
        }];
        let elab = make_elab_with_captures(captures.clone());
        let weak_count = captures
            .iter()
            .filter(|c| matches!(c.capture_kind, CaptureKind::Weak))
            .count();
        assert_eq!(
            weak_count, 1,
            "recursive lambda must have exactly one Weak capture (the self-binding-name)"
        );
        assert!(validate_drop_plan(&elab).is_empty());
    }

    #[test]
    fn multiple_weak_captures_on_same_actor_handle_is_rejected() {
        // Two Weak captures on the same LambdaActorHandle would mean
        // the lambda has two self-binding-names — structurally
        // impossible (a let-binding has exactly one name). This is a
        // lowering bug; fail closed.
        let captures = vec![
            LambdaCapture {
                actor_handle: Place::LambdaActorHandle(0),
                captured: BindingId(1),
                name: "fib".to_string(),
                capture_kind: CaptureKind::Weak,
            },
            LambdaCapture {
                actor_handle: Place::LambdaActorHandle(0),
                captured: BindingId(2),
                name: "fib_shadow".to_string(),
                capture_kind: CaptureKind::Weak,
            },
        ];
        let elab = make_elab_with_captures(captures);
        let findings = validate_drop_plan(&elab);
        assert_eq!(findings.len(), 1, "expected one finding");
        let MirCheck::DropPlanUndetermined { reason, .. } = &findings[0] else {
            panic!("expected DropPlanUndetermined, got {:?}", findings[0]);
        };
        assert!(
            reason.contains("LambdaActorHandle(0)")
                && reason.contains("weak captures")
                && reason.contains("fib"),
            "diagnostic must name the actor handle, count, and capture names: {reason}"
        );
    }

    #[test]
    fn multiple_weak_captures_on_distinct_actor_handles_are_independent() {
        // Two distinct lambda-actors, each with their own Weak self-
        // capture. Validation must accept — the "exactly one" rule
        // is per-LambdaActorHandle.
        let captures = vec![
            LambdaCapture {
                actor_handle: Place::LambdaActorHandle(0),
                captured: BindingId(1),
                name: "fib".to_string(),
                capture_kind: CaptureKind::Weak,
            },
            LambdaCapture {
                actor_handle: Place::LambdaActorHandle(1),
                captured: BindingId(2),
                name: "fact".to_string(),
                capture_kind: CaptureKind::Weak,
            },
        ];
        let elab = make_elab_with_captures(captures);
        assert!(validate_drop_plan(&elab).is_empty());
    }
}

// ============================================================================
// Generative property tests for per-Return live-set narrowing.
//
// The fixed-shape tests in `slice3_invariants` pin three hand-built shapes
// (one live handle survives, every handle moved out, three live handles).
// These proptest cases exercise the narrowing algorithm over randomly-
// generated `(defined-bindings, moved-out-subset)` inputs so the
// `dropped(block) == defined(block) - moved_out(block)` invariant is
// checked across the full state space rather than three fixtures.
//
// The proptest dependency is scoped to `cfg(not(target_arch = "wasm32"))`
// in `Cargo.toml` (matches the pattern in `hew-runtime`). The
// `cfg_attr(target_arch = "wasm32", allow(unused))` on the module below
// keeps the wasm build clean — the entire module compiles away on wasm
// since `proptest` is not available there.
// ============================================================================

#[cfg(all(test, not(target_arch = "wasm32")))]
mod slice3_narrowing_proptests {
    use super::*;
    use crate::dataflow::BindingState;
    use proptest::prelude::*;
    use std::collections::BTreeMap;

    /// A `Duplex<i64, i64>` `ResolvedTy` payload — the inner type
    /// detail is irrelevant for narrowing.
    fn duplex_ty() -> ResolvedTy {
        ResolvedTy::Named {
            name: "Duplex".to_string(),
            args: vec![ResolvedTy::I64, ResolvedTy::I64],
        }
    }

    /// Build a single-block `BasicBlock` with a `Return` terminator.
    fn single_return_block(block_id: u32) -> BasicBlock {
        BasicBlock {
            id: block_id,
            statements: vec![],
            instructions: vec![],
            terminator: Terminator::Return,
        }
    }

    /// Build the function-wide LIFO drop template for `n` `DuplexHandle`
    /// bindings, indexed `0..n`. Each drop carries the canonical
    /// `(DuplexHandle(i), DropKind::DuplexClose)` shape.
    fn build_lifo(n: u32) -> Vec<ElabDrop> {
        (0..n)
            .map(|i| ElabDrop {
                place: Place::DuplexHandle(i),
                ty: duplex_ty(),
                drop_fn: None,
                kind: DropKind::DuplexClose,
            })
            .collect()
    }

    /// Build the `binding_locals` map: `BindingId(i) -> DuplexHandle(i)`.
    fn build_binding_locals(n: u32) -> HashMap<BindingId, Place> {
        (0..n)
            .map(|i| (BindingId(i), Place::DuplexHandle(i)))
            .collect()
    }

    /// Build the `exit_states` map for block 0, marking each binding in
    /// `moved_out` as `Consumed` and every other binding (up to `n`) as
    /// `Live`. The narrowing must keep Live + `MaybeConsumed` and drop
    /// Consumed.
    fn build_exit_states(
        n: u32,
        moved_out: &[u32],
    ) -> HashMap<u32, BTreeMap<BindingId, BindingState>> {
        let mut per_binding: BTreeMap<BindingId, BindingState> = BTreeMap::new();
        for i in 0..n {
            let binding = BindingId(i);
            if moved_out.contains(&i) {
                per_binding.insert(binding, BindingState::Consumed(SiteId(0)));
            } else {
                per_binding.insert(binding, BindingState::Live);
            }
        }
        let mut map = HashMap::new();
        map.insert(0u32, per_binding);
        map
    }

    proptest! {
        /// `dropped(block) == defined(block) - moved_out(block)`.
        ///
        /// For every randomly-generated `(N, moved_out_subset)` input:
        ///   - N is the number of defined DuplexHandle bindings.
        ///   - moved_out_subset is the indices Consumed before Return.
        ///   - The narrowed drop list must contain exactly the indices
        ///     NOT in moved_out_subset, each as a DuplexHandle(i) drop.
        ///
        /// Proptest's default 256 cases sweeps shapes from N=0 (empty
        /// drop list) up to N=8 (all eight handles live or moved
        /// across every subset combination).
        #[test]
        fn dropped_equals_defined_minus_moved_out(
            n in 0u32..8,
            moved_out_mask in 0u32..256,
        ) {
            let moved_out: Vec<u32> = (0..n)
                .filter(|i| (moved_out_mask >> i) & 1 == 1)
                .collect();

            let blocks = vec![single_return_block(0)];
            let lifo = build_lifo(n);
            let exit_states = build_exit_states(n, &moved_out);
            let binding_locals = build_binding_locals(n);

            let (_, plans) = enumerate_exits(&blocks, &lifo, &exit_states, &binding_locals, &HashSet::new());

            // Exactly one Return plan for the single block.
            prop_assert_eq!(plans.len(), 1);
            let (exit, plan) = &plans[0];
            let is_return_block_0 = matches!(exit, ExitPath::Return { block: 0 });
            prop_assert!(is_return_block_0);

            // Build the expected drop list: every index 0..n not in
            // moved_out, as a DuplexHandle drop. The order matches the
            // input `lifo` template's iteration order (build_lifo
            // emits 0..n in forward order; enumerate_exits' filter
            // preserves that order).
            let expected: Vec<Place> = (0..n)
                .filter(|i| !moved_out.contains(i))
                .map(Place::DuplexHandle)
                .collect();
            let actual: Vec<Place> = plan.drops.iter().map(|d| d.place).collect();
            prop_assert_eq!(actual, expected,
                "narrowing: defined={}, moved_out={:?}", n, moved_out);
        }

        /// `MaybeConsumed` at a Return is treated as Live for drop-plan
        /// purposes (the move-checker rejects the program upstream, but
        /// the drop list stays informational). Sweep random subsets
        /// where bindings are MaybeConsumed and assert each appears in
        /// the drop list alongside the Live bindings.
        #[test]
        fn maybe_consumed_appears_in_drop_list(
            n in 0u32..6,
            maybe_mask in 0u32..64,
            consumed_mask in 0u32..64,
        ) {
            // Decide per-binding state: Consumed wins over MaybeConsumed
            // wins over Live so the masks don't overlap meaningfully —
            // a binding is Consumed if its bit is set in consumed_mask,
            // else MaybeConsumed if set in maybe_mask, else Live.
            let mut per_binding: BTreeMap<BindingId, BindingState> = BTreeMap::new();
            for i in 0..n {
                let state = if (consumed_mask >> i) & 1 == 1 {
                    BindingState::Consumed(SiteId(0))
                } else if (maybe_mask >> i) & 1 == 1 {
                    BindingState::MaybeConsumed(SiteId(0))
                } else {
                    BindingState::Live
                };
                per_binding.insert(BindingId(i), state);
            }
            let mut exit_states = HashMap::new();
            exit_states.insert(0u32, per_binding);

            let blocks = vec![single_return_block(0)];
            let lifo = build_lifo(n);
            let binding_locals = build_binding_locals(n);

            let (_, plans) = enumerate_exits(&blocks, &lifo, &exit_states, &binding_locals, &HashSet::new());
            let (_, plan) = &plans[0];

            // Expected: every binding NOT Consumed survives in the drop
            // list (Live and MaybeConsumed both qualify).
            let dropped: std::collections::HashSet<u32> = plan
                .drops
                .iter()
                .filter_map(|d| match d.place {
                    Place::DuplexHandle(i) => Some(i),
                    _ => None,
                })
                .collect();
            for i in 0..n {
                let is_consumed = (consumed_mask >> i) & 1 == 1;
                prop_assert_eq!(
                    dropped.contains(&i),
                    !is_consumed,
                    "binding {} state should determine drop-list membership", i
                );
            }
        }

        /// The narrowing is deterministic: running `enumerate_exits`
        /// twice on the same inputs produces the same drops. (Catches
        /// any HashMap-iteration-order leakage into the output.)
        #[test]
        fn narrowing_is_deterministic(
            n in 0u32..8,
            moved_out_mask in 0u32..256,
        ) {
            let moved_out: Vec<u32> = (0..n)
                .filter(|i| (moved_out_mask >> i) & 1 == 1)
                .collect();
            let blocks = vec![single_return_block(0)];
            let lifo = build_lifo(n);
            let exit_states = build_exit_states(n, &moved_out);
            let binding_locals = build_binding_locals(n);

            let (b1, p1) = enumerate_exits(&blocks, &lifo, &exit_states, &binding_locals, &HashSet::new());
            let (b2, p2) = enumerate_exits(&blocks, &lifo, &exit_states, &binding_locals, &HashSet::new());

            prop_assert_eq!(b1.len(), b2.len());
            prop_assert_eq!(p1.len(), p2.len());
            for ((e1, plan1), (e2, plan2)) in p1.iter().zip(p2.iter()) {
                prop_assert_eq!(e1, e2);
                prop_assert_eq!(&plan1.drops, &plan2.drops);
            }
        }

        /// No binding outside the function's owned set ever appears in
        /// the narrowed drop list. The narrowing must be a subset
        /// operation — it never INVENTS a drop.
        #[test]
        fn narrowing_never_invents_drops(
            n in 0u32..8,
            moved_out_mask in 0u32..256,
        ) {
            let moved_out: Vec<u32> = (0..n)
                .filter(|i| (moved_out_mask >> i) & 1 == 1)
                .collect();
            let blocks = vec![single_return_block(0)];
            let lifo = build_lifo(n);
            let exit_states = build_exit_states(n, &moved_out);
            let binding_locals = build_binding_locals(n);

            let (_, plans) = enumerate_exits(&blocks, &lifo, &exit_states, &binding_locals, &HashSet::new());
            let (_, plan) = &plans[0];

            for d in &plan.drops {
                let Place::DuplexHandle(i) = d.place else {
                    panic!("non-DuplexHandle drop appeared: {:?}", d.place);
                };
                prop_assert!(i < n, "drop for binding {} but only {} defined", i, n);
            }
        }
    }
}

// ============================================================================
// Slice 3.5 cross-block stale-DuplexHandle detection — generative property
// tests against `validate_cross_block_split_consume` built directly on
// hand-constructed `BasicBlock` + `Instr` shapes. The full source pipeline
// can't drive these tests because the parser surface for `.send_half()` /
// `.recv_half()` is slice-4 work; the synthetic inputs mirror what slice 4
// will eventually emit.
// ============================================================================

#[cfg(all(test, not(target_arch = "wasm32")))]
mod slice35_cross_block_proptests {
    use super::*;
    use proptest::prelude::*;

    fn duplex_ty() -> ResolvedTy {
        ResolvedTy::Named {
            name: "Duplex".to_string(),
            args: vec![ResolvedTy::I64, ResolvedTy::I64],
        }
    }

    /// Build a single-block CFG that splits `DuplexHandle(parent)` into
    /// a `SendHalf(parent)` then attempts to drop the unified handle
    /// after the split. The structural same-list check already rejects
    /// this shape; the cross-block check must also catch the
    /// dataflow-derived stale state on the same block (a Live entry
    /// followed by a Move-to-half transitions to Consumed before the
    /// block terminator, but the drop plan was assembled from the
    /// pre-Move LIFO — fail-closed).
    fn build_split_block(parent: u32, terminator: Terminator) -> BasicBlock {
        BasicBlock {
            id: 0,
            statements: vec![],
            instructions: vec![Instr::Move {
                dest: Place::SendHalf(parent),
                src: Place::DuplexHandle(parent),
            }],
            terminator,
        }
    }

    fn elab_with_return_drop(parent: u32) -> ElaboratedMirFunction {
        ElaboratedMirFunction {
            name: "f".to_string(),
            return_ty: ResolvedTy::Unit,
            statements: vec![],
            decisions: vec![],
            blocks: vec![ElabBlock {
                id: 0,
                kind: BlockKind::Normal,
                drops: vec![],
                successor: None,
            }],
            drop_plans: vec![(
                ExitPath::Return { block: 0 },
                DropPlan {
                    drops: vec![ElabDrop {
                        place: Place::DuplexHandle(parent),
                        ty: duplex_ty(),
                        drop_fn: None,
                        kind: DropKind::DuplexClose,
                    }],
                },
            )],
            coroutine: None,
            lambda_captures: vec![],
        }
    }

    proptest! {
        /// Cross-block: A splits DuplexHandle(p) in block 0, a drop on
        /// it appears in the Return plan. The checker must fire
        /// DropPlanUndetermined.
        #[test]
        fn split_in_predecessor_rejects_stale_unified_drop(
            parent in 0u32..8,
        ) {
            let blocks = vec![build_split_block(parent, Terminator::Return)];
            let elab = elab_with_return_drop(parent);
            let findings = validate_cross_block_split_consume(&blocks, &elab);
            prop_assert!(
                findings
                    .iter()
                    .any(|f| matches!(f, MirCheck::DropPlanUndetermined { .. })),
                "expected DropPlanUndetermined when DuplexHandle({parent}) is split AND \
                 still appears in the drop plan; got {findings:?}"
            );
        }

        /// Cross-block: block 0 branches into 1 (split path) and 2
        /// (no-split path), both jump to block 3 whose Return plan
        /// drops the unified handle. The meet of preds at block 3 is
        /// `MaybeConsumed` — fail-closed.
        #[test]
        fn split_on_some_paths_rejects_unified_drop_at_join(
            parent in 0u32..8,
        ) {
            let blocks = vec![
                // Entry: branch on a dummy cond (cond Place won't be
                // evaluated by the validator — only Move shape matters).
                BasicBlock {
                    id: 0,
                    statements: vec![],
                    instructions: vec![],
                    terminator: Terminator::Branch {
                        cond: Place::Local(99),
                        then_target: 1,
                        else_target: 2,
                    },
                },
                // Then: split DuplexHandle(parent) into SendHalf.
                BasicBlock {
                    id: 1,
                    statements: vec![],
                    instructions: vec![Instr::Move {
                        dest: Place::SendHalf(parent),
                        src: Place::DuplexHandle(parent),
                    }],
                    terminator: Terminator::Goto { target: 3 },
                },
                // Else: no split.
                BasicBlock {
                    id: 2,
                    statements: vec![],
                    instructions: vec![],
                    terminator: Terminator::Goto { target: 3 },
                },
                // Join: Return — the drop plan is checked here.
                BasicBlock {
                    id: 3,
                    statements: vec![],
                    instructions: vec![],
                    terminator: Terminator::Return,
                },
            ];
            // The Return ExitPath references block 3 — point the elab
            // drop plan at it.
            let mut elab = elab_with_return_drop(parent);
            elab.drop_plans = vec![(
                ExitPath::Return { block: 3 },
                DropPlan {
                    drops: vec![ElabDrop {
                        place: Place::DuplexHandle(parent),
                        ty: duplex_ty(),
                        drop_fn: None,
                        kind: DropKind::DuplexClose,
                    }],
                },
            )];
            elab.blocks = vec![
                ElabBlock {
                    id: 0,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                },
                ElabBlock {
                    id: 1,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                },
                ElabBlock {
                    id: 2,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                },
                ElabBlock {
                    id: 3,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                },
            ];

            let findings = validate_cross_block_split_consume(&blocks, &elab);
            let has_undetermined = findings
                .iter()
                .any(|f| matches!(f, MirCheck::DropPlanUndetermined { .. }));
            prop_assert!(
                has_undetermined,
                "expected DropPlanUndetermined at join block 3 when DuplexHandle({parent}) \
                 is split on only the then-path; got {findings:?}"
            );
            // The reason text should anchor at the split-emitting block (1).
            let reason_mentions_block_1 = findings.iter().any(|f| match f {
                MirCheck::DropPlanUndetermined { reason, .. } => reason.contains("block 1"),
                _ => false,
            });
            prop_assert!(
                reason_mentions_block_1,
                "diagnostic reason should cite block 1 as the split-emitting block; \
                 got {findings:?}"
            );
        }

        /// Cross-block: split on EVERY path. The meet at the join is
        /// `Consumed`. The drop on the unified handle at the join
        /// must be rejected with a Consumed reason (not MaybeConsumed).
        #[test]
        fn split_on_every_path_rejects_unified_drop_at_join(
            parent in 0u32..8,
        ) {
            let blocks = vec![
                BasicBlock {
                    id: 0,
                    statements: vec![],
                    instructions: vec![],
                    terminator: Terminator::Branch {
                        cond: Place::Local(99),
                        then_target: 1,
                        else_target: 2,
                    },
                },
                BasicBlock {
                    id: 1,
                    statements: vec![],
                    instructions: vec![Instr::Move {
                        dest: Place::SendHalf(parent),
                        src: Place::DuplexHandle(parent),
                    }],
                    terminator: Terminator::Goto { target: 3 },
                },
                BasicBlock {
                    id: 2,
                    statements: vec![],
                    instructions: vec![Instr::Move {
                        dest: Place::RecvHalf(parent),
                        src: Place::DuplexHandle(parent),
                    }],
                    terminator: Terminator::Goto { target: 3 },
                },
                BasicBlock {
                    id: 3,
                    statements: vec![],
                    instructions: vec![],
                    terminator: Terminator::Return,
                },
            ];
            let mut elab = elab_with_return_drop(parent);
            elab.drop_plans = vec![(
                ExitPath::Return { block: 3 },
                DropPlan {
                    drops: vec![ElabDrop {
                        place: Place::DuplexHandle(parent),
                        ty: duplex_ty(),
                        drop_fn: None,
                        kind: DropKind::DuplexClose,
                    }],
                },
            )];
            elab.blocks = (0..=3)
                .map(|id| ElabBlock {
                    id,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                })
                .collect();

            let findings = validate_cross_block_split_consume(&blocks, &elab);
            prop_assert!(
                findings
                    .iter()
                    .any(|f| matches!(f, MirCheck::DropPlanUndetermined { .. })),
                "expected DropPlanUndetermined when DuplexHandle({parent}) is split on \
                 every reaching path; got {findings:?}"
            );
        }

        /// Non-regression: no split, no rejection. A drop plan that
        /// fires the unified DuplexHandle on a block with no
        /// predecessor split must accept silently — the checker is a
        /// fail-CLOSED gate, not a fail-OPEN one. (No findings is the
        /// expected outcome.)
        #[test]
        fn no_split_no_rejection(
            parent in 0u32..8,
        ) {
            let blocks = vec![
                BasicBlock {
                    id: 0,
                    statements: vec![],
                    instructions: vec![],
                    terminator: Terminator::Return,
                },
            ];
            let elab = elab_with_return_drop(parent);
            let findings = validate_cross_block_split_consume(&blocks, &elab);
            prop_assert!(
                findings.is_empty(),
                "no split observed; the cross-block check must not invent findings; \
                 got {findings:?}"
            );
        }

        /// Multi-return: two Return blocks, the unified DuplexHandle is
        /// split on the predecessor edge of one Return but not the
        /// other. The first Return's drop fires legally (no preceding
        /// split); the second's must be rejected.
        #[test]
        fn multi_return_per_path_drops_only_flag_split_path(
            parent in 0u32..8,
        ) {
            let blocks = vec![
                // Entry branches into two Return blocks.
                BasicBlock {
                    id: 0,
                    statements: vec![],
                    instructions: vec![],
                    terminator: Terminator::Branch {
                        cond: Place::Local(99),
                        then_target: 1,
                        else_target: 2,
                    },
                },
                // Then-arm: split + return.
                BasicBlock {
                    id: 1,
                    statements: vec![],
                    instructions: vec![Instr::Move {
                        dest: Place::SendHalf(parent),
                        src: Place::DuplexHandle(parent),
                    }],
                    terminator: Terminator::Return,
                },
                // Else-arm: no split, return.
                BasicBlock {
                    id: 2,
                    statements: vec![],
                    instructions: vec![],
                    terminator: Terminator::Return,
                },
            ];
            let mut elab = elab_with_return_drop(parent);
            // Two Return drop plans, one per Return-terminated block.
            elab.drop_plans = vec![
                (
                    ExitPath::Return { block: 1 },
                    DropPlan {
                        drops: vec![ElabDrop {
                            place: Place::DuplexHandle(parent),
                            ty: duplex_ty(),
                            drop_fn: None,
                            kind: DropKind::DuplexClose,
                        }],
                    },
                ),
                (
                    ExitPath::Return { block: 2 },
                    DropPlan {
                        drops: vec![ElabDrop {
                            place: Place::DuplexHandle(parent),
                            ty: duplex_ty(),
                            drop_fn: None,
                            kind: DropKind::DuplexClose,
                        }],
                    },
                ),
            ];
            elab.blocks = (0..=2)
                .map(|id| ElabBlock {
                    id,
                    kind: BlockKind::Normal,
                    drops: vec![],
                    successor: None,
                })
                .collect();

            let findings = validate_cross_block_split_consume(&blocks, &elab);
            // Exactly one finding: the Return at block 1 (split path).
            // Block 2's Return drop fires on the unified handle that
            // was never split on its path — accept.
            let on_block_1 = findings.iter().filter(|f| {
                matches!(f, MirCheck::DropPlanUndetermined { block, .. } if *block == 1)
            }).count();
            let on_block_2 = findings.iter().filter(|f| {
                matches!(f, MirCheck::DropPlanUndetermined { block, .. } if *block == 2)
            }).count();
            prop_assert_eq!(on_block_1, 1, "block 1 (split path) must reject the unified drop");
            prop_assert_eq!(on_block_2, 0, "block 2 (no-split path) must accept the unified drop");
        }
    }
}
