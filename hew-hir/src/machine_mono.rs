//! Dedicated post-function-mono machine instantiation discovery pass.
//!
//! Per W3.033c Stage 2 ratification (R244=B / R245=A / R246=A): this
//! pass runs **after** function-mono has fully populated
//! [`crate::node::HirModule::monomorphisations`] and **before** MIR
//! `machine_layouts` is built. It enumerates every distinct
//! `(machine_decl, type_args, const_args)` triple reachable from a
//! monomorphic entry point (top-level monomorphic-function bodies,
//! actor methods, machine bodies, every substituted generic-function
//! body in the function-mono output) and writes the result into
//! [`crate::node::HirModule::machine_instantiations`].
//!
//! ## Pass-ordering invariant (load-bearing)
//!
//! `run_machine_mono_pass` MUST be invoked exactly once per
//! `lower_program` invocation, immediately after
//! `closure_under_substitution` returns and immediately before the
//! `HirModule` is constructed. Calling earlier would observe a partial
//! function-mono registry — generic-function-mediated machine
//! instantiations (e.g. `fn make<T>() -> Lifecycle<T>` instantiated as
//! `make::<File>()`) would be missed because they are only reachable
//! after the surrounding function is substituted. Calling later — or
//! from inside `closure_under_substitution`'s worklist loop — has the
//! same defect for the same reason.
//!
//! Two passes (function-mono closure and machine-mono discovery) is
//! the minimum that satisfies the invariant; collapsing them produces
//! the partial-output bug above.
//!
//! ## Uniform path (R246 = A)
//!
//! Every machine declaration produces at least one entry:
//! - **Monomorphic machine** (`type_params.is_empty()`): one entry
//!   with `type_args: vec![]` and `source_span` set to the
//!   declaration's span.
//! - **Generic machine**: zero entries from the declaration alone;
//!   one entry per concrete `(type_args, const_args)` reach-through
//!   observed at a use site (substituted body, top-level annotation,
//!   struct-state init, ctor, spawn-target).
//!
//! Downstream consumers therefore observe a uniform "one
//! `MachineLayout` per `MachineMonoEntry`" shape — no special-case
//! lookup for non-generic machines.
//!
//! ## Fail-closed boundary
//!
//! Any `ResolvedTy::Named { name, args: [] }` whose `name` belongs to
//! the *originating declaration's own* `type_params` list and that
//! survives substitution is a function-mono defect (the closure pass
//! failed to reach this site with a concrete substitution map) or a
//! checker-authority gap. Either way the discovery walker emits
//! [`crate::diagnostic::HirDiagnosticKind::UnresolvedMachineTypeParamPostMono`]
//! and refuses to add the under-instantiated entry. No silent
//! propagation to MIR/codegen.
//!
//! The "residual domain" is **per walk-entry** — it is the
//! `type_params` of the declaration currently being walked, not a
//! module-wide name set. A concrete named type whose source-level
//! spelling happens to coincide with a type-parameter name on some
//! other declaration in the same module is NOT a residual: structural
//! identity (declaration id + param-name set) controls, not module-
//! wide name matching. See LESSONS DI-015
//! (substrate-api-fail-closed-on-name-discrimination).
//!
//! ## Discovery surface (every `HirItem` variant)
//!
//! Stage 2 walker coverage is exhaustive over `HirItem` — every
//! variant is either walked or carries an explicit comment justifying
//! its exclusion. Adjacent-walker audit (DI-012):
//!
//! - [`HirItem::Function`] — monomorphic bodies walked directly;
//!   generic bodies walked once per `MonomorphizedFn` under a
//!   substitution map mapping `origin.type_params` to
//!   `mono.key.type_args`.
//! - [`HirItem::Machine`] — uniform-path emission for monomorphic
//!   machines (R246); generic-machine entries come from use-site
//!   reach-throughs in other items' walks.
//! - [`HirItem::Actor`] — init block, receive handlers, methods,
//!   lifecycle hooks, and state fields walked.
//! - [`HirItem::Record`] — top-level field annotations on monomorphic
//!   records walked. Generic records contribute reach-throughs only
//!   via fn-mono-substituted struct-init expressions (whose `expr.ty`
//!   is concrete and is visited by `walk_expr`); walking the generic
//!   record declaration itself would inject residual `T`s.
//! - [`HirItem::TypeDecl`] — top-level field + enum-variant payload
//!   annotations on monomorphic type decls walked. Generic type decls
//!   are skipped for the same reason as generic records.
//! - [`HirItem::Supervisor`] — child specs iterated. The supervisor
//!   child's type is carried as a `String` name (no parametric
//!   arguments at HIR), so the uniform-path R246 emission already
//!   covers monomorphic-machine child targets; generic-machine child
//!   targets cannot be expressed at this surface today and so produce
//!   no reach-through.
//! - [`HirItem::Impl`] — associated-type bindings (`type Item = T;`)
//!   walked under the impl's own `type_params` residual domain.
//!   Inherent / trait methods are *also* emitted as separate
//!   [`HirItem::Function`] entries (see `HirImplBlock` docs), so the
//!   method bodies are walked through the `Function` branch above —
//!   not re-walked here to avoid double-emission.
//! - [`HirItem::ExternFn`] — param tys + return ty walked. Extern fns
//!   have no body and are not generic at HIR level, so no residual
//!   domain applies.
//!
//! LESSONS: `end-to-end-before-layer-thickening` (P1),
//! `checker-authority` (P0), `match-fail-closed` (P0).

#![allow(
    clippy::match_same_arms,
    reason = "discovery walker has many HirExprKind arms whose recursive descent happens to be structurally identical (same arg list to walk_expr); merging via `|` patterns would obscure which variants the walker explicitly handles vs. delegates"
)]
// The machine-mono walker visits the `#[deprecated]`
// `CallTraitMethodStatic` variant exhaustively. Allowlist test on
// construction sites is the structural enforcement.
#![allow(
    deprecated,
    reason = "legacy CallTraitMethodStatic variant is allowlist-gated; \
              see hew-hir/tests/call_trait_method_static_creation_allowlist.rs"
)]

use std::collections::{HashMap, HashSet};

use hew_parser::ast::Span;
use hew_types::ResolvedTy;

use crate::diagnostic::{HirDiagnostic, HirDiagnosticKind};
use crate::ids::ItemId;
use crate::mono::{MachineMonoEntry, MachineMonoKey};
use crate::monomorph::MonomorphizedFn;
use crate::node::{
    HirActorDecl, HirActorMethod, HirActorReceiveFn, HirBlock, HirExpr, HirExprKind, HirFn,
    HirItem, HirLifecycleHook, HirSelectArmKind, HirStmt, HirStmtKind,
};

/// Run the machine-mono discovery pass.
///
/// **Inputs** (in pass-ordering order):
/// 1. `items` — the lowered top-level item list (post-checker, post-
///    function-mono closure).
/// 2. `monomorphisations` — the closed-under-substitution function-
///    mono registry. Must be complete: every reachable
///    generic-function instantiation in the program is in this list.
/// 3. `mono_cap` — shared monomorphisation cap (see
///    [`crate::monomorph::MONOMORPHISATION_REGISTRY_CAP`]).
///
/// **Output**: insertion-ordered, deduplicated by full
/// [`MachineMonoKey`].
///
/// The pass is non-destructive — `items` and `monomorphisations` are
/// read-only references — and emits diagnostics through the returned
/// `Vec<HirDiagnostic>` rather than panicking.
#[must_use]
#[allow(
    clippy::too_many_lines,
    reason = "load-bearing top-level pass: surface-inventory build + monomorphic-walk + post-mono-walk + uniform-path emission all need to stay in one place for ordering clarity"
)]
pub fn run_machine_mono_pass(
    items: &[HirItem],
    monomorphisations: &[MonomorphizedFn],
    mono_cap: usize,
) -> (Vec<MachineMonoEntry>, Vec<HirDiagnostic>) {
    let mut diagnostics: Vec<HirDiagnostic> = Vec::new();

    // ── Build registries ──────────────────────────────────────────────
    //
    // `machine_decls`: machine name → (decl id, declared type_params,
    // declaration span). Drives uniform-path R246 emission and the
    // discovery walker's type-arg arity check.
    //
    // `origin_fns`: origin fn id → &HirFn. Required to re-walk the
    // generic body for each `MonomorphizedFn` entry under a fresh
    // substitution map.
    //
    // Residual domains are NOT module-global — they are built per-walk-
    // entry below from the declaration currently being walked. Module-
    // global name matching would falsely flag any concrete named type
    // whose spelling happens to coincide with a generic-parameter name
    // on some unrelated declaration; see DI-015
    // (substrate-api-fail-closed-on-name-discrimination).
    //
    // `known_type_names` collects every top-level name that can be the
    // target of a concrete type reference (TypeDecl/Record/Machine/
    // Actor). After function-mono substitution, a `Named { name, args:
    // [] }` whose `name` resolves to one of these decls is *always* a
    // concrete reference — by Hew's name-resolution semantics, the
    // turbofish at the call site supplied a concrete type-decl ref and
    // `substitute_ty` wrote that ref into the body, even when the
    // spelling collides with the origin fn's own type-param. The
    // residual predicate therefore excludes such spellings from each
    // walk's residual domain; only an origin type-param spelling that
    // does NOT shadow a concrete decl can structurally survive
    // substitution as an unresolved abstract.

    let mut machine_decls: HashMap<String, (ItemId, Vec<String>, Span)> = HashMap::new();
    let mut origin_fns: HashMap<ItemId, &HirFn> = HashMap::new();
    let mut known_type_names: HashSet<String> = HashSet::new();

    for item in items {
        match item {
            HirItem::Machine(md) => {
                machine_decls.insert(
                    md.name.clone(),
                    (md.id, md.type_params.clone(), md.span.clone()),
                );
                known_type_names.insert(md.name.clone());
            }
            HirItem::Function(f) => {
                origin_fns.insert(f.id, f);
            }
            HirItem::Record(r) => {
                known_type_names.insert(r.name.clone());
            }
            HirItem::TypeDecl(td) => {
                known_type_names.insert(td.name.clone());
            }
            HirItem::Actor(a) => {
                known_type_names.insert(a.name.clone());
            }
            // Adjacent-walker audit (DI-012): every other HirItem
            // variant is walked below in the discovery-surface loop —
            // none contribute new origin/decl/known-type registry rows.
            HirItem::Supervisor(_) | HirItem::Impl(_) | HirItem::ExternFn(_) => {}
            HirItem::Const(_) => {
                // Const declarations carry only a folded integer/string value
                // and a scalar type — no machine reach-through is possible, so
                // no registry rows are contributed here.
            }
        }
    }

    // ── Discovery state ───────────────────────────────────────────────
    //
    // `seen` deduplicates by full key; `order` preserves insertion order
    // for deterministic codegen iteration. Cap matches function-mono's
    // registry shape (`MONOMORPHISATION_REGISTRY_CAP`) so machine-mono
    // failures surface the same diagnostic class.

    let mut seen: HashSet<MachineMonoKey> = HashSet::new();
    let mut order: Vec<MachineMonoEntry> = Vec::new();
    let mut cap_diag_emitted = false;

    // ── R246 uniform-path emission ────────────────────────────────────
    //
    // Every monomorphic machine produces one entry with empty
    // `type_args`. Generic machines do NOT produce an entry from the
    // declaration alone — their entries come from use-site
    // reach-throughs below (so that an unused generic machine doesn't
    // accidentally request an under-instantiated layout from MIR).

    for item in items {
        if let HirItem::Machine(md) = item {
            if !md.type_params.is_empty() {
                continue;
            }
            let key = MachineMonoKey::new(md.id, md.name.clone(), Vec::new());
            try_insert(
                key,
                md.span.clone(),
                &mut seen,
                &mut order,
                mono_cap,
                &mut diagnostics,
                &mut cap_diag_emitted,
            );
        }
    }

    // ── Body walk (monomorphic fns + every monomorphisation) ──────────
    //
    // For each monomorphic fn, walk with an empty substitution map —
    // every `expr.ty` is already concrete by checker authority.
    //
    // For each `MonomorphizedFn`, find the origin fn, build the
    // substitution map (origin.type_params → mono.key.type_args), and
    // walk the same body under substitution. The walker substitutes
    // each ResolvedTy before checking for machine reach-throughs, so
    // `Lifecycle<T>` in a generic body becomes `Lifecycle<File>` when
    // walked under T=File.
    //
    // Each walk supplies its own `residual_domain` — the originating
    // declaration's `type_params`, set form. For monomorphic walks the
    // set is empty (residuals are impossible by checker authority); for
    // post-mono walks under substitution it's the origin fn's
    // type-param list, so a `Named { name in origin.type_params }` that
    // survives substitution is correctly flagged.

    let empty_subst: HashMap<String, ResolvedTy> = HashMap::new();
    let empty_domain: HashSet<String> = HashSet::new();
    for item in items {
        match item {
            HirItem::Function(f) if f.type_params.is_empty() => {
                walk_fn_body(
                    f,
                    &empty_subst,
                    &machine_decls,
                    &empty_domain,
                    &mut seen,
                    &mut order,
                    mono_cap,
                    &mut diagnostics,
                    &mut cap_diag_emitted,
                );
            }
            HirItem::Function(_) => {
                // Generic fn — its body is walked once per
                // `MonomorphizedFn` in the post-mono loop below under
                // the corresponding substitution. Walking the generic
                // body directly here would inject residual `T`s.
            }
            HirItem::Actor(actor) => {
                // Actors are not generic at HIR (no `type_params`
                // field on `HirActorDecl`); residual domain is empty.
                walk_actor_bodies(
                    actor,
                    &empty_subst,
                    &machine_decls,
                    &empty_domain,
                    &mut seen,
                    &mut order,
                    mono_cap,
                    &mut diagnostics,
                    &mut cap_diag_emitted,
                );
            }
            HirItem::Record(rec) if rec.type_params.is_empty() => {
                // Monomorphic record: each field carries a concrete
                // `ResolvedTy` that may reach through to a machine type
                // (e.g. `record Holder { m: Lifecycle<File>; }`).
                for field in &rec.fields {
                    visit_ty(
                        &field.ty,
                        &field.span,
                        &empty_subst,
                        &machine_decls,
                        &empty_domain,
                        &mut seen,
                        &mut order,
                        mono_cap,
                        &mut diagnostics,
                        &mut cap_diag_emitted,
                    );
                }
            }
            HirItem::Record(_) => {
                // Generic record: field types are abstract over the
                // record's own `T`s. Reach-throughs to a concrete
                // machine instantiation come via fn-mono-substituted
                // struct-init expressions whose `expr.ty` is concrete
                // and is visited by `walk_expr`. Walking the generic
                // declaration directly would inject residual `T`s.
            }
            HirItem::TypeDecl(td) if td.type_params.is_empty() => {
                // Monomorphic type decl: walk struct-form field tys
                // and every enum-variant payload ty.
                for field in &td.fields {
                    visit_ty(
                        &field.ty,
                        &field.span,
                        &empty_subst,
                        &machine_decls,
                        &empty_domain,
                        &mut seen,
                        &mut order,
                        mono_cap,
                        &mut diagnostics,
                        &mut cap_diag_emitted,
                    );
                }
                for variant in &td.variants {
                    for ty in variant.field_tys() {
                        visit_ty(
                            &ty,
                            &td.span,
                            &empty_subst,
                            &machine_decls,
                            &empty_domain,
                            &mut seen,
                            &mut order,
                            mono_cap,
                            &mut diagnostics,
                            &mut cap_diag_emitted,
                        );
                    }
                }
            }
            HirItem::TypeDecl(_) => {
                // Generic type decl: skipped for the same reason as
                // generic records — reach-throughs come via fn-mono
                // expression-type visits, not the declaration itself.
            }
            HirItem::Supervisor(sup) => {
                // Adjacent-walker audit (DI-012): supervisor child
                // specs name a type as a bare `String` — no parametric
                // arguments are carried at HIR. A bare name that
                // matches a monomorphic machine decl is already
                // covered by the R246 uniform-path emission above; a
                // bare name that matches a generic machine cannot
                // express its type-args at this surface and so is a
                // checker-rejected shape upstream. We iterate the
                // children here to make the coverage explicit and to
                // future-proof against a parametric carrier being
                // added.
                for _child in &sup.children {
                    // Intentionally empty: no machine annotation
                    // surface here today (see comment above).
                }
            }
            HirItem::Impl(impl_block) => {
                // Walk associated-type bindings (`type Item = ...;`)
                // under the impl's own `type_params` residual domain.
                // Methods inside the impl are *also* emitted as
                // separate `HirItem::Function` entries (per
                // `HirImplBlock` docs), so their bodies are walked
                // through the Function branch above — not re-walked
                // here to avoid double-emission.
                // NOTE: do NOT apply the `known_type_names` shadow filter
                // here. That filter is only sound for walkers that
                // actually run `substitute_ty` (e.g. the post-mono
                // function walk below at `origin_domain`), where a
                // surviving `Named { name, args: [] }` whose spelling
                // matches a top-level decl has provably been rewritten
                // *to* the concrete decl. The impl-block assoc-type
                // walk uses `empty_subst` — no rewrite happens — so a
                // `Named { name: "T", args: [] }` here is structurally
                // still the impl's own type-param even if a top-level
                // `type T` exists. Filtering it out would mask a
                // legitimate residual and silently let an
                // un-monomorphised impl assoc-type body through.
                let impl_domain: HashSet<String> = impl_block.type_params.iter().cloned().collect();
                for (_alias_name, alias_ty) in &impl_block.type_aliases {
                    visit_ty(
                        alias_ty,
                        &impl_block.span,
                        &empty_subst,
                        &machine_decls,
                        &impl_domain,
                        &mut seen,
                        &mut order,
                        mono_cap,
                        &mut diagnostics,
                        &mut cap_diag_emitted,
                    );
                }
            }
            HirItem::ExternFn(ext) => {
                // Extern fns have no body and no `type_params`; their
                // param/return tys may still reach through to a
                // machine (e.g. an extern fn returning a monomorphic
                // machine-typed handle is exotic but well-formed).
                for ty in &ext.param_tys {
                    visit_ty(
                        ty,
                        &ext.span,
                        &empty_subst,
                        &machine_decls,
                        &empty_domain,
                        &mut seen,
                        &mut order,
                        mono_cap,
                        &mut diagnostics,
                        &mut cap_diag_emitted,
                    );
                }
                visit_ty(
                    &ext.return_ty,
                    &ext.span,
                    &empty_subst,
                    &machine_decls,
                    &empty_domain,
                    &mut seen,
                    &mut order,
                    mono_cap,
                    &mut diagnostics,
                    &mut cap_diag_emitted,
                );
            }
            HirItem::Machine(_) => {
                // R246 uniform-path emission above already handled
                // monomorphic machines. Generic-machine declarations
                // are reach-through targets, not walk sources — their
                // bodies are not user code and do not contain machine
                // annotations to discover. (`states`/`events`/
                // `transitions` carry field types of the machine's
                // *own* type params, not foreign machine references.)
            }
            HirItem::Const(_) => {
                // Const declarations carry only a folded scalar value and a
                // concrete integer/string type; no machine reach-through is
                // expressible, so there is nothing to discover here.
            }
        }
    }

    for mono in monomorphisations {
        let Some(origin) = origin_fns.get(&mono.key.origin).copied() else {
            // Origin fn missing from items — the function-mono closure
            // observed an instantiation against a fn not in this module.
            // The function-mono pass already records its own diagnostic
            // for cross-module gaps; skip silently here rather than
            // double-reporting.
            continue;
        };
        if origin.type_params.len() != mono.key.type_args.len() {
            // Arity mismatch is a function-mono defect already surfaced
            // upstream; skip to avoid producing an under-instantiated key.
            continue;
        }
        let subst: HashMap<String, ResolvedTy> = origin
            .type_params
            .iter()
            .cloned()
            .zip(mono.key.type_args.iter().cloned())
            .collect();
        // Residual domain for this walk is the origin fn's
        // type-param list, minus any spelling that shadows a top-level
        // concrete type-decl: such a spelling, after function-mono
        // substitution, is unconditionally a concrete reference (the
        // call-site turbofish wrote the concrete decl ref into the
        // body), so it cannot structurally be an unresolved residual
        // regardless of name collision with the origin's param. A
        // `Named { name in origin.type_params, args: [] }` whose name
        // does NOT shadow a concrete decl and still survives
        // substitution is a function-mono defect.
        let origin_domain: HashSet<String> = origin
            .type_params
            .iter()
            .filter(|p| !known_type_names.contains(p.as_str()))
            .cloned()
            .collect();
        walk_fn_body(
            origin,
            &subst,
            &machine_decls,
            &origin_domain,
            &mut seen,
            &mut order,
            mono_cap,
            &mut diagnostics,
            &mut cap_diag_emitted,
        );
    }

    (order, diagnostics)
}

/// Attempt to insert a fresh `MachineMonoEntry`. Dedups by full key;
/// honours the shared monomorphisation cap with a one-shot diagnostic.
#[allow(
    clippy::too_many_arguments,
    reason = "discovery walker plumbs shared mutable state (seen/order/diagnostics/cap/registries/subst) — refactoring to a struct-with-methods is deferred"
)]
fn try_insert(
    key: MachineMonoKey,
    span: Span,
    seen: &mut HashSet<MachineMonoKey>,
    order: &mut Vec<MachineMonoEntry>,
    cap: usize,
    diagnostics: &mut Vec<HirDiagnostic>,
    cap_diag_emitted: &mut bool,
) {
    if seen.contains(&key) {
        return;
    }
    if order.len() >= cap {
        if !*cap_diag_emitted {
            *cap_diag_emitted = true;
            diagnostics.push(HirDiagnostic::new(
                HirDiagnosticKind::MachineMonomorphisationCapExceeded { cap },
                span,
                "too many distinct machine instantiations discovered during machine-mono \
                 discovery; the compiler refuses to monomorphise beyond the configured cap",
            ));
        }
        return;
    }
    seen.insert(key.clone());
    order.push(MachineMonoEntry {
        key,
        source_span: span,
    });
}

/// Walk a function's signature + body under `subst`, emit one machine
/// instantiation per concrete reach-through.
#[allow(
    clippy::too_many_arguments,
    reason = "discovery walker plumbs shared mutable state (seen/order/diagnostics/cap/registries/subst) — refactoring to a struct-with-methods is deferred"
)]
fn walk_fn_body(
    f: &HirFn,
    subst: &HashMap<String, ResolvedTy>,
    machine_decls: &HashMap<String, (ItemId, Vec<String>, Span)>,
    residual_domain: &HashSet<String>,
    seen: &mut HashSet<MachineMonoKey>,
    order: &mut Vec<MachineMonoEntry>,
    cap: usize,
    diagnostics: &mut Vec<HirDiagnostic>,
    cap_diag_emitted: &mut bool,
) {
    // Signature: param tys + return ty.
    for p in &f.params {
        visit_ty(
            &p.ty,
            &p.span,
            subst,
            machine_decls,
            residual_domain,
            seen,
            order,
            cap,
            diagnostics,
            cap_diag_emitted,
        );
    }
    visit_ty(
        &f.return_ty,
        &f.span,
        subst,
        machine_decls,
        residual_domain,
        seen,
        order,
        cap,
        diagnostics,
        cap_diag_emitted,
    );
    walk_block(
        &f.body,
        subst,
        machine_decls,
        residual_domain,
        seen,
        order,
        cap,
        diagnostics,
        cap_diag_emitted,
    );
}

#[allow(
    clippy::too_many_arguments,
    reason = "discovery walker plumbs shared mutable state (seen/order/diagnostics/cap/registries/subst) — refactoring to a struct-with-methods is deferred"
)]
fn walk_actor_bodies(
    actor: &HirActorDecl,
    subst: &HashMap<String, ResolvedTy>,
    machine_decls: &HashMap<String, (ItemId, Vec<String>, Span)>,
    residual_domain: &HashSet<String>,
    seen: &mut HashSet<MachineMonoKey>,
    order: &mut Vec<MachineMonoEntry>,
    cap: usize,
    diagnostics: &mut Vec<HirDiagnostic>,
    cap_diag_emitted: &mut bool,
) {
    // Actor state-field types.
    for field in &actor.state_fields {
        visit_ty(
            &field.ty,
            &field.span,
            subst,
            machine_decls,
            residual_domain,
            seen,
            order,
            cap,
            diagnostics,
            cap_diag_emitted,
        );
    }
    if let Some(init) = &actor.init {
        for p in &init.params {
            visit_ty(
                &p.ty,
                &p.span,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
        }
        walk_block(
            &init.body,
            subst,
            machine_decls,
            residual_domain,
            seen,
            order,
            cap,
            diagnostics,
            cap_diag_emitted,
        );
    }
    for rh in &actor.receive_handlers {
        walk_receive_handler(
            rh,
            subst,
            machine_decls,
            residual_domain,
            seen,
            order,
            cap,
            diagnostics,
            cap_diag_emitted,
        );
    }
    for m in &actor.methods {
        walk_actor_method(
            m,
            subst,
            machine_decls,
            residual_domain,
            seen,
            order,
            cap,
            diagnostics,
            cap_diag_emitted,
        );
    }
    for hook in &actor.lifecycle_hooks {
        walk_lifecycle_hook(
            hook,
            subst,
            machine_decls,
            residual_domain,
            seen,
            order,
            cap,
            diagnostics,
            cap_diag_emitted,
        );
    }
}

#[allow(
    clippy::too_many_arguments,
    reason = "discovery walker plumbs shared mutable state (seen/order/diagnostics/cap/registries/subst) — refactoring to a struct-with-methods is deferred"
)]
fn walk_receive_handler(
    rh: &HirActorReceiveFn,
    subst: &HashMap<String, ResolvedTy>,
    machine_decls: &HashMap<String, (ItemId, Vec<String>, Span)>,
    residual_domain: &HashSet<String>,
    seen: &mut HashSet<MachineMonoKey>,
    order: &mut Vec<MachineMonoEntry>,
    cap: usize,
    diagnostics: &mut Vec<HirDiagnostic>,
    cap_diag_emitted: &mut bool,
) {
    for p in &rh.params {
        visit_ty(
            &p.ty,
            &p.span,
            subst,
            machine_decls,
            residual_domain,
            seen,
            order,
            cap,
            diagnostics,
            cap_diag_emitted,
        );
    }
    visit_ty(
        &rh.return_ty,
        &rh.span,
        subst,
        machine_decls,
        residual_domain,
        seen,
        order,
        cap,
        diagnostics,
        cap_diag_emitted,
    );
    walk_block(
        &rh.body,
        subst,
        machine_decls,
        residual_domain,
        seen,
        order,
        cap,
        diagnostics,
        cap_diag_emitted,
    );
}

#[allow(
    clippy::too_many_arguments,
    reason = "discovery walker plumbs shared mutable state (seen/order/diagnostics/cap/registries/subst) — refactoring to a struct-with-methods is deferred"
)]
fn walk_actor_method(
    m: &HirActorMethod,
    subst: &HashMap<String, ResolvedTy>,
    machine_decls: &HashMap<String, (ItemId, Vec<String>, Span)>,
    residual_domain: &HashSet<String>,
    seen: &mut HashSet<MachineMonoKey>,
    order: &mut Vec<MachineMonoEntry>,
    cap: usize,
    diagnostics: &mut Vec<HirDiagnostic>,
    cap_diag_emitted: &mut bool,
) {
    for p in &m.params {
        visit_ty(
            &p.ty,
            &p.span,
            subst,
            machine_decls,
            residual_domain,
            seen,
            order,
            cap,
            diagnostics,
            cap_diag_emitted,
        );
    }
    visit_ty(
        &m.return_ty,
        &m.span,
        subst,
        machine_decls,
        residual_domain,
        seen,
        order,
        cap,
        diagnostics,
        cap_diag_emitted,
    );
    walk_block(
        &m.body,
        subst,
        machine_decls,
        residual_domain,
        seen,
        order,
        cap,
        diagnostics,
        cap_diag_emitted,
    );
}

#[allow(
    clippy::too_many_arguments,
    reason = "discovery walker plumbs shared mutable state (seen/order/diagnostics/cap/registries/subst) — refactoring to a struct-with-methods is deferred"
)]
fn walk_lifecycle_hook(
    hook: &HirLifecycleHook,
    subst: &HashMap<String, ResolvedTy>,
    machine_decls: &HashMap<String, (ItemId, Vec<String>, Span)>,
    residual_domain: &HashSet<String>,
    seen: &mut HashSet<MachineMonoKey>,
    order: &mut Vec<MachineMonoEntry>,
    cap: usize,
    diagnostics: &mut Vec<HirDiagnostic>,
    cap_diag_emitted: &mut bool,
) {
    walk_block(
        &hook.body,
        subst,
        machine_decls,
        residual_domain,
        seen,
        order,
        cap,
        diagnostics,
        cap_diag_emitted,
    );
}

#[allow(
    clippy::too_many_arguments,
    reason = "discovery walker plumbs shared mutable state (seen/order/diagnostics/cap/registries/subst) — refactoring to a struct-with-methods is deferred"
)]
fn walk_block(
    block: &HirBlock,
    subst: &HashMap<String, ResolvedTy>,
    machine_decls: &HashMap<String, (ItemId, Vec<String>, Span)>,
    residual_domain: &HashSet<String>,
    seen: &mut HashSet<MachineMonoKey>,
    order: &mut Vec<MachineMonoEntry>,
    cap: usize,
    diagnostics: &mut Vec<HirDiagnostic>,
    cap_diag_emitted: &mut bool,
) {
    visit_ty(
        &block.ty,
        &block.span,
        subst,
        machine_decls,
        residual_domain,
        seen,
        order,
        cap,
        diagnostics,
        cap_diag_emitted,
    );
    for stmt in &block.statements {
        walk_stmt(
            stmt,
            subst,
            machine_decls,
            residual_domain,
            seen,
            order,
            cap,
            diagnostics,
            cap_diag_emitted,
        );
    }
    if let Some(tail) = &block.tail {
        walk_expr(
            tail,
            subst,
            machine_decls,
            residual_domain,
            seen,
            order,
            cap,
            diagnostics,
            cap_diag_emitted,
        );
    }
}

#[allow(
    clippy::too_many_arguments,
    clippy::too_many_lines,
    reason = "discovery walker plumbs shared mutable state (seen/order/diagnostics/cap/registries/subst) — refactoring to a struct-with-methods is deferred"
)]
fn walk_stmt(
    stmt: &HirStmt,
    subst: &HashMap<String, ResolvedTy>,
    machine_decls: &HashMap<String, (ItemId, Vec<String>, Span)>,
    residual_domain: &HashSet<String>,
    seen: &mut HashSet<MachineMonoKey>,
    order: &mut Vec<MachineMonoEntry>,
    cap: usize,
    diagnostics: &mut Vec<HirDiagnostic>,
    cap_diag_emitted: &mut bool,
) {
    match &stmt.kind {
        HirStmtKind::Let(binding, value) => {
            visit_ty(
                &binding.ty,
                &binding.span,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            if let Some(v) = value {
                walk_expr(
                    v,
                    subst,
                    machine_decls,
                    residual_domain,
                    seen,
                    order,
                    cap,
                    diagnostics,
                    cap_diag_emitted,
                );
            }
        }
        HirStmtKind::Assign { target, value } => {
            walk_expr(
                target,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            walk_expr(
                value,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
        }
        HirStmtKind::Expr(e) | HirStmtKind::Return(Some(e)) => {
            walk_expr(
                e,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
        }
        HirStmtKind::Return(None) => {}
        HirStmtKind::Defer { body, .. } => {
            walk_expr(
                body,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
        }
        HirStmtKind::LetElse {
            scrutinee,
            bindings,
            success_prelude,
            else_body,
            ..
        } => {
            walk_expr(
                scrutinee,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            for binding in bindings {
                visit_ty(
                    &binding.ty,
                    &scrutinee.span,
                    subst,
                    machine_decls,
                    residual_domain,
                    seen,
                    order,
                    cap,
                    diagnostics,
                    cap_diag_emitted,
                );
            }
            // Aggregate-payload leaf binders (`Ok((n, s))`) and their
            // projection values may reach machine types not named elsewhere;
            // walk the prelude so discovery stays complete.
            for prelude_stmt in success_prelude {
                walk_stmt(
                    prelude_stmt,
                    subst,
                    machine_decls,
                    residual_domain,
                    seen,
                    order,
                    cap,
                    diagnostics,
                    cap_diag_emitted,
                );
            }
            walk_block(
                else_body,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
        }
    }
}

/// Walk every `HirExpr` in the body. The expression's own `ty` field is
/// the primary discovery vector — the checker propagated the type to
/// every site, so a reach-through to a machine type is observable
/// here without re-deriving from syntax.
#[allow(
    clippy::too_many_arguments,
    reason = "discovery walker plumbs shared mutable state (seen/order/diagnostics/cap/registries/subst) — refactoring to a struct-with-methods is deferred"
)]
#[allow(
    clippy::too_many_lines,
    reason = "single match over HirExprKind — splitting would scatter the discovery logic"
)]
fn walk_expr(
    expr: &HirExpr,
    subst: &HashMap<String, ResolvedTy>,
    machine_decls: &HashMap<String, (ItemId, Vec<String>, Span)>,
    residual_domain: &HashSet<String>,
    seen: &mut HashSet<MachineMonoKey>,
    order: &mut Vec<MachineMonoEntry>,
    cap: usize,
    diagnostics: &mut Vec<HirDiagnostic>,
    cap_diag_emitted: &mut bool,
) {
    visit_ty(
        &expr.ty,
        &expr.span,
        subst,
        machine_decls,
        residual_domain,
        seen,
        order,
        cap,
        diagnostics,
        cap_diag_emitted,
    );

    match &expr.kind {
        HirExprKind::Literal(_)
        | HirExprKind::RegexLiteralRef { .. }
        | HirExprKind::BindingRef { .. }
        | HirExprKind::ContextReader { .. }
        | HirExprKind::AwaitTask { .. }
        | HirExprKind::MachineFieldAccess { .. }
        | HirExprKind::MachineEventFieldAccess { .. }
        | HirExprKind::Yield { value: None, .. }
        | HirExprKind::ActorSelf
        | HirExprKind::Unsupported(_) => {}
        HirExprKind::CancellationTokenIsCancelled { receiver }
        | HirExprKind::GeneratorNext { receiver, .. }
        | HirExprKind::RecordCloneCall { src: receiver, .. } => {
            walk_expr(
                receiver,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
        }
        HirExprKind::WireCodec { operand, .. } => {
            walk_expr(
                operand,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
        }
        HirExprKind::ConnAwaitRead { conn, .. } => {
            walk_expr(
                conn,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
        }
        HirExprKind::AwaitRestart { child } => {
            walk_expr(
                child,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
        }
        HirExprKind::ListenerAwaitAccept { listener, .. } => {
            walk_expr(
                listener,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
        }
        HirExprKind::ChannelRecvAwait { receiver, .. } => {
            walk_expr(
                receiver,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
        }
        HirExprKind::StreamRecvAwait { stream, .. } => {
            walk_expr(
                stream,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
        }
        HirExprKind::Binary { left, right, .. } | HirExprKind::IdentityCompare { left, right } => {
            walk_expr(
                left,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            walk_expr(
                right,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
        }
        HirExprKind::Unary {
            operand,
            operand_ty,
            ..
        } => {
            visit_ty(
                operand_ty,
                &expr.span,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            walk_expr(
                operand,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
        }
        HirExprKind::NumericCast {
            value,
            from_ty,
            to_ty,
        }
        | HirExprKind::SaturatingWidthCast {
            value,
            from_ty,
            to_ty,
        }
        | HirExprKind::TryWidthCast {
            value,
            from_ty,
            to_ty,
            ..
        } => {
            visit_ty(
                from_ty,
                &expr.span,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            visit_ty(
                to_ty,
                &expr.span,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            walk_expr(
                value,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
        }
        HirExprKind::TupleLiteral { elements } => {
            for elem in elements {
                walk_expr(
                    elem,
                    subst,
                    machine_decls,
                    residual_domain,
                    seen,
                    order,
                    cap,
                    diagnostics,
                    cap_diag_emitted,
                );
            }
        }
        HirExprKind::Call { callee, args } | HirExprKind::SpawnedCall { callee, args, .. } => {
            walk_expr(
                callee,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            for a in args {
                walk_expr(
                    a,
                    subst,
                    machine_decls,
                    residual_domain,
                    seen,
                    order,
                    cap,
                    diagnostics,
                    cap_diag_emitted,
                );
            }
        }
        HirExprKind::Spawn { args, .. } => {
            for (_, arg) in args {
                walk_expr(
                    arg,
                    subst,
                    machine_decls,
                    residual_domain,
                    seen,
                    order,
                    cap,
                    diagnostics,
                    cap_diag_emitted,
                );
            }
        }
        HirExprKind::ActorSend { receiver, args, .. }
        | HirExprKind::ActorGenStream { receiver, args, .. } => {
            walk_expr(
                receiver,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            for a in args {
                walk_expr(
                    a,
                    subst,
                    machine_decls,
                    residual_domain,
                    seen,
                    order,
                    cap,
                    diagnostics,
                    cap_diag_emitted,
                );
            }
        }
        HirExprKind::ActorAsk {
            receiver,
            args,
            reply_ty,
            ..
        } => {
            visit_ty(
                reply_ty,
                &expr.span,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            walk_expr(
                receiver,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            for a in args {
                walk_expr(
                    a,
                    subst,
                    machine_decls,
                    residual_domain,
                    seen,
                    order,
                    cap,
                    diagnostics,
                    cap_diag_emitted,
                );
            }
        }
        HirExprKind::RemoteActorAsk {
            receiver,
            msg,
            timeout_ms,
            reply_ty,
        } => {
            visit_ty(
                reply_ty,
                &expr.span,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            for child in [receiver.as_ref(), msg.as_ref(), timeout_ms.as_ref()] {
                walk_expr(
                    child,
                    subst,
                    machine_decls,
                    residual_domain,
                    seen,
                    order,
                    cap,
                    diagnostics,
                    cap_diag_emitted,
                );
            }
        }
        HirExprKind::CallDynMethod { receiver, args, .. } => {
            walk_expr(
                receiver,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            for a in args {
                walk_expr(
                    a,
                    subst,
                    machine_decls,
                    residual_domain,
                    seen,
                    order,
                    cap,
                    diagnostics,
                    cap_diag_emitted,
                );
            }
        }
        HirExprKind::ResolvedImplCall { receiver, args, .. }
        | HirExprKind::CallTraitMethodStatic { receiver, args, .. }
        | HirExprKind::VarSelfMethodCall { receiver, args, .. } => {
            walk_expr(
                receiver,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            for a in args {
                walk_expr(
                    a,
                    subst,
                    machine_decls,
                    residual_domain,
                    seen,
                    order,
                    cap,
                    diagnostics,
                    cap_diag_emitted,
                );
            }
        }
        HirExprKind::Block(b) => walk_block(
            b,
            subst,
            machine_decls,
            residual_domain,
            seen,
            order,
            cap,
            diagnostics,
            cap_diag_emitted,
        ),
        HirExprKind::GenBlock { body, .. } => walk_block(
            body,
            subst,
            machine_decls,
            residual_domain,
            seen,
            order,
            cap,
            diagnostics,
            cap_diag_emitted,
        ),
        HirExprKind::Yield {
            value: Some(value), ..
        } => walk_expr(
            value,
            subst,
            machine_decls,
            residual_domain,
            seen,
            order,
            cap,
            diagnostics,
            cap_diag_emitted,
        ),
        HirExprKind::If {
            condition,
            then_expr,
            else_expr,
        } => {
            walk_expr(
                condition,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            walk_expr(
                then_expr,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            if let Some(e) = else_expr {
                walk_expr(
                    e,
                    subst,
                    machine_decls,
                    residual_domain,
                    seen,
                    order,
                    cap,
                    diagnostics,
                    cap_diag_emitted,
                );
            }
        }
        HirExprKind::StructInit {
            type_args,
            fields,
            base,
            ..
        } => {
            for ta in type_args {
                visit_ty(
                    ta,
                    &expr.span,
                    subst,
                    machine_decls,
                    residual_domain,
                    seen,
                    order,
                    cap,
                    diagnostics,
                    cap_diag_emitted,
                );
            }
            for (_, e) in fields {
                walk_expr(
                    e,
                    subst,
                    machine_decls,
                    residual_domain,
                    seen,
                    order,
                    cap,
                    diagnostics,
                    cap_diag_emitted,
                );
            }
            if let Some(b) = base {
                walk_expr(
                    b,
                    subst,
                    machine_decls,
                    residual_domain,
                    seen,
                    order,
                    cap,
                    diagnostics,
                    cap_diag_emitted,
                );
            }
        }
        HirExprKind::FieldAccess { object, .. } => walk_expr(
            object,
            subst,
            machine_decls,
            residual_domain,
            seen,
            order,
            cap,
            diagnostics,
            cap_diag_emitted,
        ),
        HirExprKind::Scope { body } | HirExprKind::ForkBlock { body, .. } => walk_block(
            body,
            subst,
            machine_decls,
            residual_domain,
            seen,
            order,
            cap,
            diagnostics,
            cap_diag_emitted,
        ),
        HirExprKind::ScopeDeadline { duration, body } => {
            walk_expr(
                duration,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            walk_block(
                body,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
        }
        HirExprKind::TupleIndex { tuple, .. } => walk_expr(
            tuple,
            subst,
            machine_decls,
            residual_domain,
            seen,
            order,
            cap,
            diagnostics,
            cap_diag_emitted,
        ),
        HirExprKind::Index { container, index } => {
            walk_expr(
                container,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            walk_expr(
                index,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
        }
        HirExprKind::Slice {
            container,
            start,
            end,
            ..
        } => {
            walk_expr(
                container,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            if let Some(s) = start {
                walk_expr(
                    s,
                    subst,
                    machine_decls,
                    residual_domain,
                    seen,
                    order,
                    cap,
                    diagnostics,
                    cap_diag_emitted,
                );
            }
            if let Some(e) = end {
                walk_expr(
                    e,
                    subst,
                    machine_decls,
                    residual_domain,
                    seen,
                    order,
                    cap,
                    diagnostics,
                    cap_diag_emitted,
                );
            }
        }
        HirExprKind::SpawnLambdaActor { body, .. } | HirExprKind::Closure { body, .. } => {
            walk_expr(
                body,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
        }
        HirExprKind::While {
            condition, body, ..
        } => {
            walk_expr(
                condition,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            walk_block(
                body,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
        }
        HirExprKind::ForRange {
            start,
            end,
            step,
            body,
            ..
        } => {
            walk_expr(
                start,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            walk_expr(
                end,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            walk_expr(
                step,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            walk_block(
                body,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
        }
        HirExprKind::Match { scrutinee, arms } => {
            walk_expr(
                scrutinee,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    walk_expr(
                        guard,
                        subst,
                        machine_decls,
                        residual_domain,
                        seen,
                        order,
                        cap,
                        diagnostics,
                        cap_diag_emitted,
                    );
                }
                walk_expr(
                    &arm.body,
                    subst,
                    machine_decls,
                    residual_domain,
                    seen,
                    order,
                    cap,
                    diagnostics,
                    cap_diag_emitted,
                );
            }
        }
        HirExprKind::WhileLet {
            scrutinee, body, ..
        } => {
            walk_expr(
                scrutinee,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            walk_block(
                body,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
        }
        HirExprKind::IfLet {
            scrutinee,
            body,
            else_body,
            ..
        } => {
            walk_expr(
                scrutinee,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            walk_block(
                body,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            if let Some(eb) = else_body {
                walk_block(
                    eb,
                    subst,
                    machine_decls,
                    residual_domain,
                    seen,
                    order,
                    cap,
                    diagnostics,
                    cap_diag_emitted,
                );
            }
        }
        HirExprKind::Break { value, .. } | HirExprKind::Return { value } => {
            if let Some(value) = value {
                walk_expr(
                    value,
                    subst,
                    machine_decls,
                    residual_domain,
                    seen,
                    order,
                    cap,
                    diagnostics,
                    cap_diag_emitted,
                );
            }
        }
        HirExprKind::Continue { .. } => {}
        HirExprKind::Loop { body, .. } => {
            walk_block(
                body,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
        }
        HirExprKind::NumericMethod { receiver, arg, .. } => {
            walk_expr(
                receiver,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            walk_expr(
                arg,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
        }
        HirExprKind::CoerceToDynTrait { value, .. } => walk_expr(
            value,
            subst,
            machine_decls,
            residual_domain,
            seen,
            order,
            cap,
            diagnostics,
            cap_diag_emitted,
        ),
        HirExprKind::MachineEmit { fields, .. } => {
            for (_, e) in fields {
                walk_expr(
                    e,
                    subst,
                    machine_decls,
                    residual_domain,
                    seen,
                    order,
                    cap,
                    diagnostics,
                    cap_diag_emitted,
                );
            }
        }
        HirExprKind::MachineStep {
            receiver, event, ..
        } => {
            walk_expr(
                receiver,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            walk_expr(
                event,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
        }
        HirExprKind::MachineTakeEmits {
            receiver, event, ..
        } => {
            walk_expr(
                receiver,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
            walk_expr(
                event,
                subst,
                machine_decls,
                residual_domain,
                seen,
                order,
                cap,
                diagnostics,
                cap_diag_emitted,
            );
        }
        HirExprKind::MachineStateName { receiver, .. } => walk_expr(
            receiver,
            subst,
            machine_decls,
            residual_domain,
            seen,
            order,
            cap,
            diagnostics,
            cap_diag_emitted,
        ),
        HirExprKind::MachineVariantCtor { payload, .. } => {
            if let Some(fields) = payload {
                for (_, e) in fields {
                    walk_expr(
                        e,
                        subst,
                        machine_decls,
                        residual_domain,
                        seen,
                        order,
                        cap,
                        diagnostics,
                        cap_diag_emitted,
                    );
                }
            }
        }
        HirExprKind::Select(sel) => {
            for arm in &sel.arms {
                match &arm.kind {
                    HirSelectArmKind::StreamNext { stream } => walk_expr(
                        stream,
                        subst,
                        machine_decls,
                        residual_domain,
                        seen,
                        order,
                        cap,
                        diagnostics,
                        cap_diag_emitted,
                    ),
                    HirSelectArmKind::ActorAsk { actor, args, .. } => {
                        walk_expr(
                            actor,
                            subst,
                            machine_decls,
                            residual_domain,
                            seen,
                            order,
                            cap,
                            diagnostics,
                            cap_diag_emitted,
                        );
                        for a in args {
                            walk_expr(
                                a,
                                subst,
                                machine_decls,
                                residual_domain,
                                seen,
                                order,
                                cap,
                                diagnostics,
                                cap_diag_emitted,
                            );
                        }
                    }
                    HirSelectArmKind::TaskAwait { task } => walk_expr(
                        task,
                        subst,
                        machine_decls,
                        residual_domain,
                        seen,
                        order,
                        cap,
                        diagnostics,
                        cap_diag_emitted,
                    ),
                    HirSelectArmKind::ChannelRecv { receiver, .. } => walk_expr(
                        receiver,
                        subst,
                        machine_decls,
                        residual_domain,
                        seen,
                        order,
                        cap,
                        diagnostics,
                        cap_diag_emitted,
                    ),
                    HirSelectArmKind::AfterTimer { duration } => walk_expr(
                        duration,
                        subst,
                        machine_decls,
                        residual_domain,
                        seen,
                        order,
                        cap,
                        diagnostics,
                        cap_diag_emitted,
                    ),
                }
                walk_expr(
                    &arm.body,
                    subst,
                    machine_decls,
                    residual_domain,
                    seen,
                    order,
                    cap,
                    diagnostics,
                    cap_diag_emitted,
                );
            }
        }
        HirExprKind::Join(join) => {
            for branch in &join.branches {
                walk_expr(
                    &branch.actor,
                    subst,
                    machine_decls,
                    residual_domain,
                    seen,
                    order,
                    cap,
                    diagnostics,
                    cap_diag_emitted,
                );
                for a in &branch.args {
                    walk_expr(
                        a,
                        subst,
                        machine_decls,
                        residual_domain,
                        seen,
                        order,
                        cap,
                        diagnostics,
                        cap_diag_emitted,
                    );
                }
            }
        }
    }
}

/// The core discovery primitive: given a checker-observed `ty` at
/// `span`, substitute under `subst`, and if the result is (recursively)
/// a reference to a known machine name with concrete args, emit one
/// `MachineMonoEntry`.
///
/// Fail-closed: if the substituted type still carries a residual
/// abstract symbol (a `Named { args: [] }` whose name is in
/// `residual_domain`), and the surrounding type names a machine,
/// emit `UnresolvedMachineTypeParamPostMono` and refuse to insert the
/// under-instantiated key.
#[allow(
    clippy::too_many_arguments,
    reason = "discovery walker plumbs shared mutable state (seen/order/diagnostics/cap/registries/subst) — refactoring to a struct-with-methods is deferred"
)]
fn visit_ty(
    ty: &ResolvedTy,
    span: &Span,
    subst: &HashMap<String, ResolvedTy>,
    machine_decls: &HashMap<String, (ItemId, Vec<String>, Span)>,
    residual_domain: &HashSet<String>,
    seen: &mut HashSet<MachineMonoKey>,
    order: &mut Vec<MachineMonoEntry>,
    cap: usize,
    diagnostics: &mut Vec<HirDiagnostic>,
    cap_diag_emitted: &mut bool,
) {
    let substituted = crate::lower::substitute_ty(ty, subst);
    let mut worklist = vec![substituted];
    while let Some(t) = worklist.pop() {
        match t {
            ResolvedTy::Named { name, args, .. } => {
                // Enqueue nested args first so a `Holder<Lifecycle<File>>`
                // emits entries for both `Holder<...>` (if it's a machine)
                // and `Lifecycle<File>`.
                for a in &args {
                    worklist.push(a.clone());
                }
                if let Some((decl_id, decl_type_params, _decl_span)) =
                    machine_decls.get(&name).cloned()
                {
                    // Arity gate: a `Named { name: "M", args: [] }`
                    // against a generic-machine decl signals an
                    // under-instantiated reach-through. Skip rather
                    // than emit; the uniform-path entry only applies
                    // when `decl_type_params.is_empty()`.
                    if decl_type_params.is_empty() && !args.is_empty() {
                        // Monomorphic machine with stray args — checker
                        // defect upstream; skip to avoid producing a
                        // malformed entry.
                        continue;
                    }
                    if !decl_type_params.is_empty() && args.is_empty() {
                        // Generic machine referenced bare. The
                        // uniform-path R246 emission for monomorphic
                        // machines already runs; for generic machines,
                        // a bare reference is a function-mono defect
                        // (substitution should have produced concrete
                        // args). Emit fail-closed diagnostic.
                        diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::UnresolvedMachineTypeParamPostMono {
                                machine: name.clone(),
                                residual_var: format!("(bare {name})"),
                            },
                            span.clone(),
                            "machine reference reached the machine-mono pass with no concrete \
                             type arguments; expected fully-substituted type after function-mono",
                        ));
                        continue;
                    }
                    // Fail-closed: residual abstract symbol anywhere
                    // in args.
                    if let Some(residual) = first_residual_param(&args, residual_domain) {
                        diagnostics.push(HirDiagnostic::new(
                            HirDiagnosticKind::UnresolvedMachineTypeParamPostMono {
                                machine: name.clone(),
                                residual_var: residual,
                            },
                            span.clone(),
                            "machine instantiation carries a residual abstract type-parameter \
                             after function-mono substitution; pass-ordering invariant violated",
                        ));
                        continue;
                    }
                    let key = MachineMonoKey::new(decl_id, name.clone(), args.clone());
                    try_insert(
                        key,
                        span.clone(),
                        seen,
                        order,
                        cap,
                        diagnostics,
                        cap_diag_emitted,
                    );
                }
            }
            ResolvedTy::Tuple(items) => {
                for x in items {
                    worklist.push(x);
                }
            }
            ResolvedTy::Array(elem, _) | ResolvedTy::Slice(elem) => worklist.push(*elem),
            ResolvedTy::Function { params, ret } => {
                for p in params {
                    worklist.push(p);
                }
                worklist.push(*ret);
            }
            ResolvedTy::Closure {
                params,
                ret,
                captures,
            } => {
                for p in params {
                    worklist.push(p);
                }
                worklist.push(*ret);
                for c in captures {
                    worklist.push(c);
                }
            }
            ResolvedTy::Pointer { pointee, .. } | ResolvedTy::Borrow { pointee } => {
                worklist.push(*pointee);
            }
            ResolvedTy::TraitObject { traits } => {
                for bound in traits {
                    for a in bound.args {
                        worklist.push(a);
                    }
                    for (_, t) in bound.assoc_bindings {
                        worklist.push(t);
                    }
                }
            }
            ResolvedTy::Task(inner) => worklist.push(*inner),
            _ => {}
        }
    }
}

/// Return the first residual abstract type-parameter name found in
/// `args` (recursively), or `None` if every leaf is concrete.
fn first_residual_param(args: &[ResolvedTy], residual_domain: &HashSet<String>) -> Option<String> {
    for a in args {
        if let Some(name) = residual_param_in_ty(a, residual_domain) {
            return Some(name);
        }
    }
    None
}

fn residual_param_in_ty(ty: &ResolvedTy, residual_domain: &HashSet<String>) -> Option<String> {
    match ty {
        ResolvedTy::Named { name, args, .. } => {
            if args.is_empty() && residual_domain.contains(name) {
                return Some(name.clone());
            }
            for a in args {
                if let Some(n) = residual_param_in_ty(a, residual_domain) {
                    return Some(n);
                }
            }
            None
        }
        ResolvedTy::Tuple(items) => items
            .iter()
            .find_map(|t| residual_param_in_ty(t, residual_domain)),
        ResolvedTy::Array(elem, _) | ResolvedTy::Slice(elem) => {
            residual_param_in_ty(elem, residual_domain)
        }
        ResolvedTy::Function { params, ret } => params
            .iter()
            .find_map(|p| residual_param_in_ty(p, residual_domain))
            .or_else(|| residual_param_in_ty(ret, residual_domain)),
        ResolvedTy::Closure {
            params,
            ret,
            captures,
        } => params
            .iter()
            .find_map(|p| residual_param_in_ty(p, residual_domain))
            .or_else(|| residual_param_in_ty(ret, residual_domain))
            .or_else(|| {
                captures
                    .iter()
                    .find_map(|c| residual_param_in_ty(c, residual_domain))
            }),
        ResolvedTy::Pointer { pointee, .. } | ResolvedTy::Borrow { pointee } => {
            residual_param_in_ty(pointee, residual_domain)
        }
        ResolvedTy::TraitObject { traits } => traits.iter().find_map(|bound| {
            bound
                .args
                .iter()
                .find_map(|a| residual_param_in_ty(a, residual_domain))
                .or_else(|| {
                    bound
                        .assoc_bindings
                        .iter()
                        .find_map(|(_, t)| residual_param_in_ty(t, residual_domain))
                })
        }),
        ResolvedTy::Task(inner) => residual_param_in_ty(inner, residual_domain),
        // A structural type parameter is residual when its name is still in
        // the abstract domain (DI-019: the wildcard must not absorb it).
        ResolvedTy::TypeParam { name } if residual_domain.contains(name) => Some(name.clone()),
        _ => None,
    }
}
