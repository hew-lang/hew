//! Dedicated post-function-mono record/enum layout discovery pass.
//!
//! This is the structural sibling of [`crate::machine_mono`]. Where
//! `machine_mono` discovers concrete *machine* instantiations reachable only
//! through a substituted generic-function body, this pass discovers concrete
//! *record* and *enum* layout instantiations reachable the same way and
//! registers them under the shared `origin$$arg1$arg2` mangling so MIR's
//! existing per-mangled-name layout merge (`hew-mir/src/lower.rs:961`) and
//! field-access / variant dispatch find them with no back-end change.
//!
//! ## The gap this closes
//!
//! The function-mono closure (`closure_under_substitution`) discovers inner
//! generic-fn `Call` and `CallTraitMethodStatic` sites inside a substituted
//! body, but it does **not** re-discover record/enum *layout* instantiations.
//! A generic record constructed inside a generic fn body — e.g.
//! `fn make<T>(x: T) -> Box<T> { Box { value: x } }` — registers no concrete
//! layout: at the origin the `Box { value: x }` site is abstract (`Box<T>`),
//! so `record_record_layout` produces nothing (it fails closed on abstract
//! args). The concrete `Box<i64>` shape only becomes observable once `make`
//! is substituted as `make$$i64`. This pass walks every such substituted body
//! and registers the concrete layouts.
//!
//! ## Pass-ordering invariant (load-bearing)
//!
//! `run_layout_mono_pass` MUST run **after** `closure_under_substitution` has
//! fully populated [`crate::node::HirModule::monomorphisations`] and **before**
//! the `HirModule` is constructed — the same window as `run_machine_mono_pass`.
//! Running earlier would observe a partial function-mono registry and miss
//! transitively-reachable instantiations.
//!
//! ## Fail-closed boundary
//!
//! The "residual domain" is **per walk-entry** — the `type_params` of the
//! generic fn currently being walked, not a module-wide name set. A type that
//! survives substitution still carrying a residual abstract symbol from that
//! domain is a function-mono defect or a checker-authority gap; the pass emits
//! [`crate::diagnostic::HirDiagnosticKind::UnresolvedLayoutTypeParamPostMono`]
//! and refuses to register the under-instantiated layout. This mirrors
//! `machine_mono`'s DI-015 residual discipline exactly (see that module for
//! the name-collision rationale): a concrete decl whose spelling collides with
//! an origin type-param name on some *unrelated* declaration must not be
//! flagged abstract, so the domain is built from the walked fn's own params.

// The layout-mono walker visits the `#[deprecated]` `CallTraitMethodStatic`
// variant exhaustively (same justification as `lower.rs` / `machine_mono.rs`).
// Construction sites are allowlist-gated by the test below.
#![allow(
    deprecated,
    reason = "legacy CallTraitMethodStatic variant is allowlist-gated; \
              see hew-hir/tests/call_trait_method_static_creation_allowlist.rs"
)]

use std::collections::{HashMap, HashSet};
use std::ops::Range;

use hew_types::ResolvedTy;

use crate::diagnostic::{HirDiagnostic, HirDiagnosticKind};
use crate::ids::ItemId;
use crate::lower::substitute_ty;
use crate::monomorph::{
    contains_recursive_polymorphic_self, mangle, EnumLayout, EnumMonoKey, EnumVariantLayout,
    MonomorphizedFn, RecordLayout, RecordMonoKey,
};
use crate::node::{
    HirBlock, HirExpr, HirExprKind, HirFn, HirItem, HirSelectArmKind, HirStmt, HirStmtKind,
    HirVariant,
};

/// A generic record declaration's structural shape, indexed by name.
struct RecordDecl {
    id: ItemId,
    type_params: Vec<String>,
    /// Source-declared field shapes (name, declared type) in order.
    fields: Vec<(String, ResolvedTy)>,
}

/// A generic enum declaration's structural shape, indexed by name.
struct EnumDecl {
    id: ItemId,
    type_params: Vec<String>,
    variants: Vec<HirVariant>,
}

/// Classification of a generic instantiation's type arguments — see
/// [`Discovery::classify_args`].
enum ArgClass {
    /// Every leaf is concrete; register the layout.
    Concrete,
    /// An abstract symbol borrowed from another declaration's signature
    /// (not a construction site); skip silently.
    BorrowedAbstract,
    /// A residual abstract symbol from the walked fn's own type-params
    /// survived substitution; a defect, surfaced fail-closed. Carries the
    /// offending symbol name for the diagnostic.
    ResidualDefect(String),
}

/// Shared mutable discovery state plumbed through the walk.
struct Discovery<'a> {
    record_decls: &'a HashMap<String, RecordDecl>,
    enum_decls: &'a HashMap<String, EnumDecl>,
    /// Program-wide set of every type-parameter name declared on any
    /// generic fn / record / enum. A type argument that names one of these
    /// (and is not in the current walk's residual domain) is an *abstract
    /// reference borrowed from another declaration's signature* — e.g. the
    /// callee `expr.ty` of `make(7)` inside a monomorphic body carries
    /// `fn(T) -> Box<T>`. Such a `Box<T>` is NOT an instantiation site and is
    /// skipped silently; only a residual-domain symbol that survives
    /// substitution is a defect (see `classify_args`).
    all_type_params: &'a HashSet<String>,
    /// Keys already present (origin-site registrations + this pass's own
    /// insertions) so a layout discovered both at a concrete site and through
    /// a substituted body is registered once.
    seen_records: HashSet<RecordMonoKey>,
    seen_enums: HashSet<EnumMonoKey>,
    /// Count of origin-site layouts that already exist before this pass runs.
    /// The cap is a registry-wide invariant: `existing + new` must never exceed
    /// it, so an instantiation the origin path already rejected at the cap must
    /// not be re-admitted here.
    existing_record_count: usize,
    existing_enum_count: usize,
    new_records: Vec<RecordLayout>,
    new_enums: Vec<EnumLayout>,
    cap: usize,
    record_cap_diag_emitted: bool,
    enum_cap_diag_emitted: bool,
    diagnostics: Vec<HirDiagnostic>,
}

/// Discover generic record/enum layout instantiations reachable through
/// function-mono-substituted bodies, returning the *additional* layouts to
/// merge into [`crate::node::HirModule::record_layouts`] /
/// [`crate::node::HirModule::enum_layouts`] plus any fail-closed diagnostics.
///
/// `existing_records` / `existing_enums` are the origin-site registrations
/// already produced during lowering; they seed the dedup set so this pass
/// never re-emits an entry the concrete-site path already produced.
#[must_use]
pub fn run_layout_mono_pass(
    items: &[HirItem],
    monomorphisations: &[MonomorphizedFn],
    existing_records: &[RecordLayout],
    existing_enums: &[EnumLayout],
    cap: usize,
) -> (Vec<RecordLayout>, Vec<EnumLayout>, Vec<HirDiagnostic>) {
    // ── Build decl registries from items (no module-global residual domain;
    //    each walk supplies its own from the fn being walked). ────────────
    let mut record_decls: HashMap<String, RecordDecl> = HashMap::new();
    let mut enum_decls: HashMap<String, EnumDecl> = HashMap::new();
    let mut origin_fns: HashMap<ItemId, &HirFn> = HashMap::new();
    // Program-wide abstract-symbol domain: every type-parameter name declared
    // anywhere. Used to recognise abstract type references *borrowed from
    // another declaration's signature* (e.g. a callee `expr.ty` of
    // `fn(T) -> Box<T>`) and skip them, rather than mistaking the borrowed `T`
    // for a concrete instantiation when the current walk's residual domain
    // doesn't contain it.
    let mut all_type_params: HashSet<String> = HashSet::new();

    for item in items {
        match item {
            HirItem::Record(r) => {
                all_type_params.extend(r.type_params.iter().cloned());
                record_decls.insert(
                    r.name.clone(),
                    RecordDecl {
                        id: r.id,
                        type_params: r.type_params.clone(),
                        fields: r
                            .fields
                            .iter()
                            .map(|f| (f.name.clone(), f.ty.clone()))
                            .collect(),
                    },
                );
            }
            HirItem::TypeDecl(td) => {
                all_type_params.extend(td.type_params.iter().cloned());
                // A struct-kind type decl contributes a record layout; an
                // enum-kind type decl contributes an enum layout. The two are
                // disjoint (a type decl has either `fields` or `variants`).
                if td.variants.is_empty() {
                    record_decls.insert(
                        td.name.clone(),
                        RecordDecl {
                            id: td.id,
                            type_params: td.type_params.clone(),
                            fields: td
                                .fields
                                .iter()
                                .map(|f| (f.name.clone(), f.ty.clone()))
                                .collect(),
                        },
                    );
                } else {
                    enum_decls.insert(
                        td.name.clone(),
                        EnumDecl {
                            id: td.id,
                            type_params: td.type_params.clone(),
                            variants: td.variants.clone(),
                        },
                    );
                }
            }
            HirItem::Function(f) => {
                all_type_params.extend(f.type_params.iter().cloned());
                origin_fns.insert(f.id, f);
            }
            // Impl-block type-params join the program-wide abstract domain so
            // an impl-method signature reference (`fn(T) -> Pair<T>`) borrowed
            // into another body is recognised as abstract, not a layout site.
            HirItem::Impl(im) => {
                all_type_params.extend(im.type_params.iter().cloned());
            }
            // Other item variants contribute no new record/enum decls. Their
            // bodies' reach-throughs are observed through the function-mono
            // walk below (impl methods are also emitted as `HirItem::Function`
            // entries; actor/machine bodies carry concrete `expr.ty`s walked
            // via their monomorphic-fn analogue when reachable).
            HirItem::Machine(_)
            | HirItem::Actor(_)
            | HirItem::Supervisor(_)
            | HirItem::ExternFn(_)
            | HirItem::Const(_) => {}
        }
    }

    let mut disc = Discovery {
        record_decls: &record_decls,
        enum_decls: &enum_decls,
        all_type_params: &all_type_params,
        seen_records: existing_records.iter().map(|r| r.key.clone()).collect(),
        seen_enums: existing_enums.iter().map(|e| e.key.clone()).collect(),
        existing_record_count: existing_records.len(),
        existing_enum_count: existing_enums.len(),
        new_records: Vec::new(),
        new_enums: Vec::new(),
        cap,
        record_cap_diag_emitted: false,
        enum_cap_diag_emitted: false,
        diagnostics: Vec::new(),
    };

    let empty_subst: HashMap<String, ResolvedTy> = HashMap::new();
    let empty_domain: HashSet<String> = HashSet::new();

    // ── Walk monomorphic fn bodies (empty subst, empty residual domain). ──
    // Their `expr.ty`s are already concrete by checker authority, so any
    // generic record/enum constructed directly here is concrete and dedups
    // against the origin-site registration.
    for item in items {
        if let HirItem::Function(f) = item {
            if f.type_params.is_empty() {
                walk_fn(f, &empty_subst, &empty_domain, &mut disc);
            }
        }
    }

    // ── Walk each monomorphisation's substituted body. ───────────────────
    for mono in monomorphisations {
        let Some(origin) = origin_fns.get(&mono.key.origin).copied() else {
            continue;
        };
        let subst: HashMap<String, ResolvedTy> = origin
            .type_params
            .iter()
            .cloned()
            .zip(mono.key.type_args.iter().cloned())
            .collect();
        let residual_domain: HashSet<String> = origin.type_params.iter().cloned().collect();
        walk_fn(origin, &subst, &residual_domain, &mut disc);
    }

    (disc.new_records, disc.new_enums, disc.diagnostics)
}

fn walk_fn(
    f: &HirFn,
    subst: &HashMap<String, ResolvedTy>,
    residual_domain: &HashSet<String>,
    disc: &mut Discovery,
) {
    for p in &f.params {
        disc.visit_ty(&p.ty, &p.span, subst, residual_domain);
    }
    disc.visit_ty(&f.return_ty, &f.span, subst, residual_domain);
    walk_block(&f.body, subst, residual_domain, disc);
}

fn walk_block(
    block: &HirBlock,
    subst: &HashMap<String, ResolvedTy>,
    residual_domain: &HashSet<String>,
    disc: &mut Discovery,
) {
    for stmt in &block.statements {
        walk_stmt(stmt, subst, residual_domain, disc);
    }
    if let Some(tail) = &block.tail {
        walk_expr(tail, subst, residual_domain, disc);
    }
}

fn walk_stmt(
    stmt: &HirStmt,
    subst: &HashMap<String, ResolvedTy>,
    residual_domain: &HashSet<String>,
    disc: &mut Discovery,
) {
    match &stmt.kind {
        HirStmtKind::Let(binding, init) => {
            disc.visit_ty(&binding.ty, &binding.span, subst, residual_domain);
            if let Some(e) = init {
                walk_expr(e, subst, residual_domain, disc);
            }
        }
        HirStmtKind::Assign { target, value } => {
            walk_expr(target, subst, residual_domain, disc);
            walk_expr(value, subst, residual_domain, disc);
        }
        HirStmtKind::Expr(e) | HirStmtKind::Return(Some(e)) => {
            walk_expr(e, subst, residual_domain, disc);
        }
        HirStmtKind::Return(None) => {}
        HirStmtKind::Defer { body, .. } => walk_expr(body, subst, residual_domain, disc),
    }
}

/// Walk one expression: visit its `expr.ty` (the source of concrete layout
/// reach-throughs once substituted), then recurse into every child expression
/// and block. The variant arms below are exhaustive over `HirExprKind` — every
/// child-bearing variant recurses; leaf variants are explicitly enumerated so a
/// future variant addition fails to compile rather than silently dropping a
/// discovery site.
#[allow(
    clippy::too_many_lines,
    reason = "single recursive walker spanning all HirExprKind variants — mirrors collect_call_sites_in_expr"
)]
#[allow(
    clippy::match_same_arms,
    reason = "many HirExprKind arms recurse identically (same args to walk_expr); merging via `|` would obscure which variants the walker explicitly handles vs. delegates — same rationale as machine_mono::walk_expr"
)]
fn walk_expr(
    expr: &HirExpr,
    subst: &HashMap<String, ResolvedTy>,
    residual_domain: &HashSet<String>,
    disc: &mut Discovery,
) {
    // Every node carries a checker-resolved type; once substituted it exposes
    // any generic record/enum instantiation produced or named at this site.
    disc.visit_ty(&expr.ty, &expr.span, subst, residual_domain);

    match &expr.kind {
        HirExprKind::Call { callee, args } | HirExprKind::SpawnedCall { callee, args, .. } => {
            walk_expr(callee, subst, residual_domain, disc);
            for a in args {
                walk_expr(a, subst, residual_domain, disc);
            }
        }
        HirExprKind::Spawn { args, .. } => {
            for (_, arg) in args {
                walk_expr(arg, subst, residual_domain, disc);
            }
        }
        HirExprKind::ActorSend { receiver, args, .. }
        | HirExprKind::ActorAsk { receiver, args, .. }
        | HirExprKind::ResolvedImplCall { receiver, args, .. }
        | HirExprKind::CallDynMethod { receiver, args, .. }
        | HirExprKind::CallTraitMethodStatic { receiver, args, .. }
        | HirExprKind::VarSelfMethodCall { receiver, args, .. } => {
            walk_expr(receiver, subst, residual_domain, disc);
            for arg in args {
                walk_expr(arg, subst, residual_domain, disc);
            }
        }
        HirExprKind::RemoteActorAsk {
            receiver,
            msg,
            timeout_ms,
            ..
        } => {
            walk_expr(receiver, subst, residual_domain, disc);
            walk_expr(msg, subst, residual_domain, disc);
            walk_expr(timeout_ms, subst, residual_domain, disc);
        }
        HirExprKind::Binary { left, right, .. } | HirExprKind::IdentityCompare { left, right } => {
            walk_expr(left, subst, residual_domain, disc);
            walk_expr(right, subst, residual_domain, disc);
        }
        HirExprKind::Unary { operand, .. } => walk_expr(operand, subst, residual_domain, disc),
        HirExprKind::NumericCast { value, .. } | HirExprKind::CoerceToDynTrait { value, .. } => {
            walk_expr(value, subst, residual_domain, disc);
        }
        HirExprKind::TupleLiteral { elements } => {
            for elem in elements {
                walk_expr(elem, subst, residual_domain, disc);
            }
        }
        HirExprKind::Block(b) | HirExprKind::GenBlock { body: b, .. } => {
            walk_block(b, subst, residual_domain, disc);
        }
        HirExprKind::Yield {
            value: Some(value), ..
        } => walk_expr(value, subst, residual_domain, disc),
        HirExprKind::If {
            condition,
            then_expr,
            else_expr,
        } => {
            walk_expr(condition, subst, residual_domain, disc);
            walk_expr(then_expr, subst, residual_domain, disc);
            if let Some(e) = else_expr {
                walk_expr(e, subst, residual_domain, disc);
            }
        }
        HirExprKind::StructInit { fields, base, .. } => {
            for (_, e) in fields {
                walk_expr(e, subst, residual_domain, disc);
            }
            if let Some(b) = base {
                walk_expr(b, subst, residual_domain, disc);
            }
        }
        HirExprKind::FieldAccess { object, .. } => {
            walk_expr(object, subst, residual_domain, disc);
        }
        HirExprKind::Scope { body }
        | HirExprKind::ForkBlock { body, .. }
        | HirExprKind::Loop { body } => {
            walk_block(body, subst, residual_domain, disc);
        }
        HirExprKind::ScopeDeadline { duration, body } => {
            walk_expr(duration, subst, residual_domain, disc);
            walk_block(body, subst, residual_domain, disc);
        }
        HirExprKind::TupleIndex { tuple, .. } => walk_expr(tuple, subst, residual_domain, disc),
        HirExprKind::Index { container, index } => {
            walk_expr(container, subst, residual_domain, disc);
            walk_expr(index, subst, residual_domain, disc);
        }
        HirExprKind::Slice {
            container,
            start,
            end,
            ..
        } => {
            walk_expr(container, subst, residual_domain, disc);
            if let Some(s) = start {
                walk_expr(s, subst, residual_domain, disc);
            }
            if let Some(e) = end {
                walk_expr(e, subst, residual_domain, disc);
            }
        }
        HirExprKind::SpawnLambdaActor { body, .. } | HirExprKind::Closure { body, .. } => {
            walk_expr(body, subst, residual_domain, disc);
        }
        HirExprKind::While { condition, body } => {
            walk_expr(condition, subst, residual_domain, disc);
            walk_block(body, subst, residual_domain, disc);
        }
        HirExprKind::ForRange {
            start, end, body, ..
        } => {
            walk_expr(start, subst, residual_domain, disc);
            walk_expr(end, subst, residual_domain, disc);
            walk_block(body, subst, residual_domain, disc);
        }
        HirExprKind::Match { scrutinee, arms } => {
            walk_expr(scrutinee, subst, residual_domain, disc);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    walk_expr(guard, subst, residual_domain, disc);
                }
                walk_expr(&arm.body, subst, residual_domain, disc);
            }
        }
        HirExprKind::WhileLet {
            scrutinee, body, ..
        } => {
            walk_expr(scrutinee, subst, residual_domain, disc);
            walk_block(body, subst, residual_domain, disc);
        }
        HirExprKind::IfLet {
            scrutinee,
            body,
            else_body,
            ..
        } => {
            walk_expr(scrutinee, subst, residual_domain, disc);
            walk_block(body, subst, residual_domain, disc);
            if let Some(eb) = else_body {
                walk_block(eb, subst, residual_domain, disc);
            }
        }
        HirExprKind::Break {
            value: Some(value), ..
        } => walk_expr(value, subst, residual_domain, disc),
        HirExprKind::NumericMethod { receiver, arg, .. } => {
            walk_expr(receiver, subst, residual_domain, disc);
            walk_expr(arg, subst, residual_domain, disc);
        }
        HirExprKind::MachineEmit { fields, .. } => {
            for (_, e) in fields {
                walk_expr(e, subst, residual_domain, disc);
            }
        }
        HirExprKind::MachineStep {
            receiver, event, ..
        } => {
            walk_expr(receiver, subst, residual_domain, disc);
            walk_expr(event, subst, residual_domain, disc);
        }
        HirExprKind::CancellationTokenIsCancelled { receiver }
        | HirExprKind::MachineStateName { receiver, .. } => {
            walk_expr(receiver, subst, residual_domain, disc);
        }
        HirExprKind::MachineVariantCtor { payload, .. } => {
            if let Some(fields) = payload {
                for (_, e) in fields {
                    walk_expr(e, subst, residual_domain, disc);
                }
            }
        }
        HirExprKind::Select(sel) => {
            for arm in &sel.arms {
                match &arm.kind {
                    HirSelectArmKind::StreamNext { stream } => {
                        walk_expr(stream, subst, residual_domain, disc);
                    }
                    HirSelectArmKind::ActorAsk { actor, args, .. } => {
                        walk_expr(actor, subst, residual_domain, disc);
                        for a in args {
                            walk_expr(a, subst, residual_domain, disc);
                        }
                    }
                    HirSelectArmKind::TaskAwait { task } => {
                        walk_expr(task, subst, residual_domain, disc);
                    }
                    HirSelectArmKind::AfterTimer { duration } => {
                        walk_expr(duration, subst, residual_domain, disc);
                    }
                }
                walk_expr(&arm.body, subst, residual_domain, disc);
            }
        }
        // Leaf variants: no child expressions. `expr.ty` was already visited
        // above, so a leaf typed as a concrete generic record/enum (e.g. a
        // `BindingRef` of type `Box<i64>`) still reaches through.
        HirExprKind::Literal(_)
        | HirExprKind::RegexLiteralRef { .. }
        | HirExprKind::BindingRef { .. }
        | HirExprKind::ContextReader { .. }
        | HirExprKind::AwaitTask { .. }
        | HirExprKind::MachineFieldAccess { .. }
        | HirExprKind::MachineEventFieldAccess { .. }
        | HirExprKind::Yield { value: None, .. }
        | HirExprKind::Break { value: None, .. }
        | HirExprKind::Continue { .. }
        | HirExprKind::Unsupported(_) => {}
    }
}

impl Discovery<'_> {
    /// Substitute `ty` through `subst`, then reach through every concrete
    /// generic-record / generic-enum instantiation it (transitively) names and
    /// register the corresponding layout.
    fn visit_ty(
        &mut self,
        ty: &ResolvedTy,
        span: &Range<usize>,
        subst: &HashMap<String, ResolvedTy>,
        residual_domain: &HashSet<String>,
    ) {
        let substituted = substitute_ty(ty, subst);
        let mut worklist = vec![substituted];
        while let Some(t) = worklist.pop() {
            // Enqueue nested args first so `Box<Pair<i64,bool>>` reaches
            // through both `Box<...>` and `Pair<i64,bool>`.
            collect_named_children(&t, &mut worklist);
            let ResolvedTy::Named { name, args, .. } = &t else {
                continue;
            };
            if args.is_empty() {
                // A bare name is either a monomorphic type (no per-instantiation
                // layout needed) or a residual abstract symbol. A residual is
                // caught when it appears *as an arg* of a generic instantiation
                // below; a top-level bare name is never a generic-layout site.
                continue;
            }
            // Classify the args once. An instantiation is registrable only
            // when every leaf is concrete; an abstract symbol borrowed from
            // another decl's signature is skipped silently, and a residual
            // symbol from the *walked fn's own* domain is a fail-closed defect.
            match self.classify_args(args, residual_domain) {
                ArgClass::Concrete => {}
                ArgClass::BorrowedAbstract => continue,
                ArgClass::ResidualDefect(residual) => {
                    self.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::UnresolvedLayoutTypeParamPostMono {
                            type_name: name.clone(),
                            residual_var: residual,
                        },
                        span.clone(),
                        "generic record/enum instantiation carries a residual abstract \
                         type-parameter after function-mono substitution; pass-ordering \
                         invariant violated",
                    ));
                    continue;
                }
            }
            if let Some(decl) = self.record_decls.get(name) {
                if decl.type_params.is_empty() {
                    // Monomorphic record referenced with stray args — checker
                    // defect upstream; skip rather than emit a malformed layout.
                    continue;
                }
                self.register_record(name, decl, args, span);
            } else if let Some(decl) = self.enum_decls.get(name) {
                if decl.type_params.is_empty() {
                    continue;
                }
                self.register_enum(name, decl, args, span);
            }
        }
    }

    /// Classify a generic instantiation's type arguments for registration.
    ///
    /// - [`ArgClass::Concrete`] — every leaf is a concrete type; register.
    /// - [`ArgClass::BorrowedAbstract`] — an abstract symbol that belongs to
    ///   *another* declaration (a signature reference, e.g. the callee
    ///   `expr.ty` of `make(7)` carries `fn(T) -> Box<T>`); skip silently. Not
    ///   a construction site.
    /// - [`ArgClass::ResidualDefect`] — an abstract symbol from the *walked
    ///   fn's own* type-params survived substitution; a function-mono /
    ///   checker-authority defect, surfaced fail-closed.
    fn classify_args(&self, args: &[ResolvedTy], residual_domain: &HashSet<String>) -> ArgClass {
        // Residual-domain symbols are checked first: a symbol that is in the
        // walked fn's own domain AND survived substitution is unambiguously a
        // defect (the closure failed to reach this site concretely).
        if let Some(residual) = first_residual(args, residual_domain) {
            return ArgClass::ResidualDefect(residual);
        }
        // Otherwise, any program-wide type-param symbol is an abstract
        // reference borrowed from another declaration's signature.
        if args.iter().any(|a| abstract_in_ty(a, self.all_type_params)) {
            return ArgClass::BorrowedAbstract;
        }
        ArgClass::Concrete
    }

    fn register_record(
        &mut self,
        name: &str,
        decl: &RecordDecl,
        args: &[ResolvedTy],
        span: &Range<usize>,
    ) {
        let RecordDecl {
            id, type_params, ..
        } = decl;
        if args.len() != type_params.len() {
            return;
        }
        let key = RecordMonoKey {
            origin: *id,
            origin_name: name.to_string(),
            type_args: args.to_vec(),
        };
        if !self.seen_records.insert(key.clone()) {
            return;
        }
        let substituted_fields: Vec<(String, ResolvedTy)> = decl
            .fields
            .iter()
            .map(|(fname, fty)| {
                (
                    fname.clone(),
                    crate::monomorph::substitute_type_params(fty, type_params, args),
                )
            })
            .collect();
        // Recursive polymorphic-self detection: same guard the origin-site
        // `record_record_layout` path uses (deferred to v0.6).
        for (_, fty) in &substituted_fields {
            if contains_recursive_polymorphic_self(fty, name, args) {
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::RecursiveGenericTypeUnsupported {
                        name: name.to_string(),
                    },
                    span.clone(),
                    "recursive generic type definition: a field substitutes to a reference \
                     to the same record with different concrete type arguments, which would \
                     force unbounded layout expansion (deferred to v0.6)",
                ));
                return;
            }
        }
        if self.existing_record_count + self.new_records.len() >= self.cap {
            // Emit at most one cap diagnostic, and only when THIS pass is the
            // one that crosses the cap. If the origin-site path already
            // overflowed (`existing >= cap`), it owns the diagnostic — emitting
            // a second here would double-report the same fail-closed condition.
            if !self.record_cap_diag_emitted && self.existing_record_count < self.cap {
                self.record_cap_diag_emitted = true;
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::RecordLayoutCapExceeded { cap: self.cap },
                    span.clone(),
                    "too many distinct generic-record instantiations discovered during \
                     post-function-mono layout closure; the compiler refuses to monomorphise \
                     beyond the configured cap",
                ));
            }
            return;
        }
        let mangled_name = mangle(name, args);
        self.new_records.push(RecordLayout {
            key,
            mangled_name,
            fields: substituted_fields,
            span: span.clone(),
        });
    }

    fn register_enum(
        &mut self,
        name: &str,
        decl: &EnumDecl,
        args: &[ResolvedTy],
        span: &Range<usize>,
    ) {
        let EnumDecl {
            id,
            type_params,
            variants,
        } = decl;
        if args.len() != type_params.len() {
            return;
        }
        let key = EnumMonoKey {
            origin: *id,
            origin_name: name.to_string(),
            type_args: args.to_vec(),
        };
        if !self.seen_enums.insert(key.clone()) {
            return;
        }
        let variant_layouts: Vec<EnumVariantLayout> = variants
            .iter()
            .map(|v| EnumVariantLayout {
                name: v.name.clone(),
                field_tys: v
                    .field_tys()
                    .iter()
                    .map(|ft| crate::monomorph::substitute_type_params(ft, type_params, args))
                    .collect(),
            })
            .collect();
        if self.existing_enum_count + self.new_enums.len() >= self.cap {
            // See `register_record`: only the boundary-crossing pass emits the
            // single cap diagnostic; if the origin path already overflowed it
            // owns the report.
            if !self.enum_cap_diag_emitted && self.existing_enum_count < self.cap {
                self.enum_cap_diag_emitted = true;
                self.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::EnumLayoutCapExceeded { cap: self.cap },
                    span.clone(),
                    "too many distinct generic-enum instantiations discovered during \
                     post-function-mono layout closure; the compiler refuses to monomorphise \
                     beyond the configured cap",
                ));
            }
            return;
        }
        let mangled_name = mangle(name, args);
        self.new_enums.push(EnumLayout {
            key,
            mangled_name,
            variants: variant_layouts,
        });
    }
}

/// Enqueue the immediate compound-type children of `ty` onto `worklist` so a
/// reach-through walk descends into every nested type. The `Named` case is
/// handled by the caller (which inspects `name`/`args`); this enqueues args
/// and the payloads of the structural type constructors.
fn collect_named_children(ty: &ResolvedTy, worklist: &mut Vec<ResolvedTy>) {
    match ty {
        ResolvedTy::Named { args, .. } => {
            for a in args {
                worklist.push(a.clone());
            }
        }
        ResolvedTy::Tuple(items) => {
            for x in items {
                worklist.push(x.clone());
            }
        }
        ResolvedTy::Array(elem, _) | ResolvedTy::Slice(elem) => worklist.push((**elem).clone()),
        ResolvedTy::Function { params, ret } => {
            for p in params {
                worklist.push(p.clone());
            }
            worklist.push((**ret).clone());
        }
        ResolvedTy::Closure {
            params,
            ret,
            captures,
        } => {
            for p in params {
                worklist.push(p.clone());
            }
            worklist.push((**ret).clone());
            for c in captures {
                worklist.push(c.clone());
            }
        }
        ResolvedTy::Pointer { pointee, .. } | ResolvedTy::Borrow { pointee } => {
            worklist.push((**pointee).clone());
        }
        ResolvedTy::TraitObject { traits } => {
            for bound in traits {
                for a in &bound.args {
                    worklist.push(a.clone());
                }
                for (_, t) in &bound.assoc_bindings {
                    worklist.push(t.clone());
                }
            }
        }
        ResolvedTy::Task(inner) => worklist.push((**inner).clone()),
        _ => {}
    }
}

/// Return the first residual abstract type-parameter name found anywhere in
/// `args` (recursively), or `None` if every leaf is concrete. A residual is a
/// `ResolvedTy::TypeParam` or a bare `Named { args: [] }` whose name is in the
/// walked fn's own `residual_domain` (per the DI-015 per-walk discipline).
fn first_residual(args: &[ResolvedTy], residual_domain: &HashSet<String>) -> Option<String> {
    args.iter().find_map(|a| residual_in_ty(a, residual_domain))
}

/// Does `ty` (recursively) name any program-wide abstract type-parameter, or
/// carry a structural `ResolvedTy::TypeParam`? Used to recognise an abstract
/// reference borrowed from another declaration's signature (e.g. a callee
/// `expr.ty` of `fn(T) -> Box<T>`) so it is not mistaken for a concrete layout
/// site. Mirrors `lower::contains_abstract_symbol` (a structural `TypeParam`
/// is abstract by construction).
fn abstract_in_ty(ty: &ResolvedTy, all_type_params: &HashSet<String>) -> bool {
    match ty {
        ResolvedTy::TypeParam { .. } => true,
        _ => residual_in_ty(ty, all_type_params).is_some(),
    }
}

fn residual_in_ty(ty: &ResolvedTy, residual_domain: &HashSet<String>) -> Option<String> {
    match ty {
        ResolvedTy::Named { name, args, .. } => {
            if args.is_empty() && residual_domain.contains(name) {
                return Some(name.clone());
            }
            args.iter().find_map(|a| residual_in_ty(a, residual_domain))
        }
        ResolvedTy::TypeParam { name } if residual_domain.contains(name) => Some(name.clone()),
        ResolvedTy::Tuple(items) => items
            .iter()
            .find_map(|t| residual_in_ty(t, residual_domain)),
        ResolvedTy::Array(elem, _) | ResolvedTy::Slice(elem) => {
            residual_in_ty(elem, residual_domain)
        }
        ResolvedTy::Function { params, ret } => params
            .iter()
            .find_map(|p| residual_in_ty(p, residual_domain))
            .or_else(|| residual_in_ty(ret, residual_domain)),
        ResolvedTy::Closure {
            params,
            ret,
            captures,
        } => params
            .iter()
            .find_map(|p| residual_in_ty(p, residual_domain))
            .or_else(|| residual_in_ty(ret, residual_domain))
            .or_else(|| {
                captures
                    .iter()
                    .find_map(|c| residual_in_ty(c, residual_domain))
            }),
        ResolvedTy::Pointer { pointee, .. } | ResolvedTy::Borrow { pointee } => {
            residual_in_ty(pointee, residual_domain)
        }
        ResolvedTy::TraitObject { traits } => traits.iter().find_map(|bound| {
            bound
                .args
                .iter()
                .find_map(|a| residual_in_ty(a, residual_domain))
                .or_else(|| {
                    bound
                        .assoc_bindings
                        .iter()
                        .find_map(|(_, t)| residual_in_ty(t, residual_domain))
                })
        }),
        ResolvedTy::Task(inner) => residual_in_ty(inner, residual_domain),
        _ => None,
    }
}
