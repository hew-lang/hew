//! Target, coroutine, supervisor-spawn and binary-operator gates.

use super::*;

// ── Target architecture gates ────────────────────────────────────────────────

/// Pre-pass that rejects coroutine-dependent constructs (actors, tasks) on
/// unsupported targets and blocking channel recv on wasm32.
///
/// Fail-closed per slepp A222: emit fatal diagnostics at compile time instead
/// of allowing runtime panics at `hew-runtime/src/coro.rs:391/:492` (P0.1/P0.2)
/// or `hew-runtime/src/lib.rs:378/:391` (P0.3/P0.4).
pub(super) fn check_target_gates(ctx: &mut LowerCtx, program: &Program) {
    // P0.1: Actor runtime ABI gate.
    //   - x86_64, aarch64: native multi-threaded work-stealing scheduler — admitted.
    //   - wasm32: cooperative single-threaded scheduler (`scheduler_wasm.rs`) with
    //     C ABI parity to native — admitted (#1821).
    //   - TargetArch::Other: no scheduler — rejected.
    //
    // P0.2: Supervisor restart machinery gate.
    //   Supervisors require `SupervisorChildGet`/`Stop`/nested-restart which are
    //   not implemented in the wasm32 runtime. Supervisors remain gated on
    //   {x86_64, aarch64} until #1475 is resolved.
    //
    // Suspension itself uses target-agnostic LLVM llvm.coro.*; the gap for
    // actors on wasm32 was the scheduler/mailbox ABI, now closed.
    if !matches!(ctx.target_arch, TargetArch::X86_64 | TargetArch::Aarch64) {
        check_coroutine_gate(ctx, program);
    }

    // NB: P0.3 + P0.4 (wasm blocking channel recv) intentionally NOT dispatched
    // here. They are dispatched separately AFTER the type pre-pass's
    // diagnostics.clear() at line ~921 so the gate's diagnostics survive into
    // LowerOutput. See check_wasm_blocking_recv_gate at the post-clear call
    // site for the dispatch. The coroutine gate IS duplicated by inline
    // Item::Actor / Item::Supervisor checks in the source-order pass below; the
    // wasm gate has no such inline counterpart so the survival-ordering is
    // essential.
}

/// Check for actor/supervisor usage on targets without the actor runtime ABI.
///
/// Called only when `target_arch` is not `x86_64` or `aarch64`.
///
/// Actor gate: wasm32 is admitted (cooperative scheduler + mailbox exist in
/// `scheduler_wasm.rs`; #1821). Only `TargetArch::Other` (unknown triples)
/// is rejected for actors.
///
/// Supervisor gate: wasm32 is still rejected — supervisor restart machinery
/// (`SupervisorChildGet`/`Stop`/nested-restart) is not yet implemented in
/// the wasm32 runtime (#1475).
pub(super) fn check_coroutine_gate(ctx: &mut LowerCtx, program: &Program) {
    let target_name = match ctx.target_arch {
        TargetArch::Wasm32 => "wasm32",
        TargetArch::Other => "unsupported",
        // x86_64/aarch64 shouldn't reach here (guarded by caller)
        _ => "other",
    };

    for (item, span) in &program.items {
        match item {
            Item::Actor(actor_decl) => {
                // Actors are admitted on wasm32: the cooperative scheduler
                // (`hew-runtime/src/scheduler_wasm.rs`) provides the full
                // actor ABI. Only reject for genuinely unsupported targets.
                if matches!(ctx.target_arch, TargetArch::Other) {
                    ctx.diagnostics.push(HirDiagnostic::new(
                        HirDiagnosticKind::TargetCoroutineUnsupported {
                            target_arch: target_name.to_string(),
                            construct: "actor decl".to_string(),
                        },
                        span.clone(),
                        format!(
                            "actor `{}` cannot be compiled for target `{}`: \
                             the actor runtime ABI (scheduler, mailbox) is not \
                             available on this target",
                            actor_decl.name, target_name
                        ),
                    ));
                }
            }
            Item::Supervisor(supervisor_decl) => {
                // Supervisors remain gated on wasm32: supervisor restart
                // machinery (`SupervisorChildGet`, `Stop`, nested-restart)
                // is not yet implemented in the wasm32 runtime (#1475).
                ctx.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::TargetCoroutineUnsupported {
                        target_arch: target_name.to_string(),
                        construct: "supervisor decl".to_string(),
                    },
                    span.clone(),
                    format!(
                        "supervisor `{}` cannot be compiled for target `{}`: \
                         supervisor restart machinery is not yet available on \
                         this target (#1475)",
                        supervisor_decl.name, target_name
                    ),
                ));
            }
            // TODO: When `scope{}` and `fork` are surface syntax, add checks here
            _ => {}
        }
    }

    // TODO: Add checks for:
    // - `scope{}` blocks (when they exist in surface AST)
    // - `fork` statements (when they exist in surface AST)
    // - `await` expressions (when they appear outside of existing actor/scope)
    // For now, actors and supervisors are the only coroutine entry points.
}

// ── FC-P1-A3: Supervisor spawn args gate ─────────────────────────────────────

/// Pre-pass that rejects `spawn AppSupervisor(...)` with non-empty init args.
///
/// Supervisors take their child specs declaratively (in the `supervisor`
/// declaration body); spawn-time init args have no defined semantics. The
/// checker already rejects supervisor *declarations* that take init params,
/// but this HIR gate is defense-in-depth: it catches any future surface that
/// could reach MIR (`hew-mir/src/lower.rs:8852`) before the checker guard does.
///
/// Per slepp A222 (fail-closed): surface a HIR fatal diagnostic at compile
/// time instead of a `NotYetImplemented` runtime-style diagnostic at MIR-
/// lowering time.
/// Structured supervisor registry consumed by the spawn-args gate.
///
/// Replaces the FC-P1-A3 v1 single-`HashSet<String>` approach (which lost
/// module context and produced both false negatives — `spawn other.Sup(args)`
/// on an imported supervisor slipped through — and false positives —
/// `spawn other.Root(args)` was rejected when the local `Root` was a
/// supervisor but `other.Root` was an actor). The structured form preserves
/// the spawn site's module qualifier so the lookup is module-scoped, matching
/// the checker's own resolution discipline.
///
/// Discharges the A237 string-fragility pattern (see LESSONS
/// `string-identifier-fragility-vs-structured-resolution`).
pub(super) struct SupervisorRegistry {
    /// Supervisor names declared at the root program level, mapped to whether
    /// the supervisor declares a construction-time config param (`supervisor
    /// App(config: T)`). Consulted for bare-identifier spawn targets (`spawn
    /// Sup(args)`): a config supervisor admits exactly its config arg; a
    /// no-config supervisor admits none.
    pub(super) root: std::collections::HashMap<String, bool>,
    /// Supervisor names declared in each imported module, keyed by its full
    /// declaration owner. Lexical import bindings are resolved through the
    /// checker-published table below before consulting this map.
    pub(super) by_module:
        std::collections::HashMap<String, std::collections::HashMap<String, bool>>,
    /// Exact owner selected for each lexical whole-module import binding.
    /// This is semantic authority for `spawn alias.Supervisor(...)`; the
    /// module leaf is never used as a lookup fallback.
    pub(super) module_bindings: HashMap<(Option<String>, u32, String), String>,
}

#[derive(Clone, Copy)]
pub(super) struct SupervisorScanScope<'a> {
    pub(super) module: Option<&'a str>,
    pub(super) file: u32,
}

impl SupervisorRegistry {
    /// True iff the program declares no supervisors anywhere (root or any
    /// module). When true, the gate walk can short-circuit.
    pub(super) fn is_empty(&self) -> bool {
        self.root.is_empty()
            && self
                .by_module
                .values()
                .all(std::collections::HashMap::is_empty)
    }

    pub(super) fn resolve_module_binding<'a>(
        &'a self,
        scope: SupervisorScanScope<'_>,
        lexical_binding: &'a str,
    ) -> Option<&'a str> {
        self.module_bindings
            .get(&(
                scope.module.map(str::to_string),
                scope.file,
                lexical_binding.to_string(),
            ))
            .map(String::as_str)
            .or_else(|| {
                self.by_module
                    .contains_key(lexical_binding)
                    .then_some(lexical_binding)
            })
    }
}

/// Collect supervisor declarations from the root program AND every module in
/// the program's `module_graph` (when present). Module-graph coverage is
/// required to reject `spawn other.MyServiceSup(args)` for supervisors
/// declared in imported modules.
pub(super) fn collect_supervisor_registry(
    program: &Program,
    module_bindings: HashMap<(Option<String>, u32, String), String>,
) -> SupervisorRegistry {
    let mut root: std::collections::HashMap<String, bool> = std::collections::HashMap::new();
    for (item, _) in &program.items {
        if let Item::Supervisor(decl) = item {
            root.insert(decl.name.clone(), !decl.params.is_empty());
        }
    }
    let mut by_module: std::collections::HashMap<String, std::collections::HashMap<String, bool>> =
        std::collections::HashMap::new();
    if let Some(mg) = &program.module_graph {
        for (mod_id, module) in &mg.modules {
            if *mod_id == mg.root {
                continue;
            }
            let module_owner = mod_id.path.join(".");
            if module_owner.is_empty() {
                continue;
            }
            let entry = by_module.entry(module_owner).or_default();
            for (item, _) in &module.items {
                if let Item::Supervisor(decl) = item {
                    entry.insert(decl.name.clone(), !decl.params.is_empty());
                }
            }
        }
    }
    SupervisorRegistry {
        root,
        by_module,
        module_bindings,
    }
}

pub(super) fn check_supervisor_spawn_gate(ctx: &mut LowerCtx, program: &Program) {
    let registry = collect_supervisor_registry(program, ctx.module_import_bindings.clone());
    if registry.is_empty() {
        // No supervisors declared anywhere → no spawn site can target one;
        // skip the walk entirely.
        return;
    }

    // Walk root items. `current_module = None` selects the root supervisor
    // set for bare-name spawn targets.
    let root_span_indices = program
        .module_graph
        .as_ref()
        .map(hew_parser::module::ModuleGraph::file_span_indices);
    for (item_idx, (item, _span)) in program.items.iter().enumerate() {
        let file = program
            .module_graph
            .as_ref()
            .zip(root_span_indices.as_ref())
            .and_then(|(graph, indices)| indices.item_index(&graph.root, item_idx))
            .unwrap_or_default();
        scan_item_for_supervisor_spawn(
            item,
            SupervisorScanScope { module: None, file },
            &registry,
            &mut ctx.diagnostics,
        );
    }
    // Walk every non-root module in the program's module graph. A supervisor
    // spawn with args inside a function/actor/impl/machine body in an imported
    // module must trigger the gate the same way the root does (A242: pre-pass
    // walkers must visit every body-bearing position in every module they're
    // logically scoped to, not just the root program).
    //
    // `current_module = Some(full_owner)` scopes the bare-identifier lookup
    // to that module's own supervisor set. Bare identifiers in module bodies
    // resolve under the module's local scope (cf. checker's resolution
    // rules — root names are NOT auto-imported into modules; a module wanting
    // to spawn a root supervisor must reach it via a module-qualified path,
    // which dispatches through `Expr::FieldAccess` below). Without this
    // scoping we'd both (a) miss `spawn LocalSup(args)` inside an imported
    // module whose `LocalSup` isn't declared at root (false-negative) and
    // (b) reject `spawn Foo(args)` inside an imported module that happens
    // to share a name with a root-declared supervisor even though the
    // module's `Foo` is something else entirely (false-positive). Both
    // failure modes are documented in the rev2 independent review finding.
    if let Some(mg) = &program.module_graph {
        let span_indices = mg.file_span_indices();
        for (mod_id, module) in &mg.modules {
            if *mod_id == mg.root {
                continue;
            }
            let module_owner = mod_id.path.join(".");
            for (item_idx, (item, _)) in module.items.iter().enumerate() {
                scan_item_for_supervisor_spawn(
                    item,
                    SupervisorScanScope {
                        module: Some(&module_owner),
                        file: span_indices
                            .item_index(mod_id, item_idx)
                            .unwrap_or_default(),
                    },
                    &registry,
                    &mut ctx.diagnostics,
                );
            }
        }
    }
}

// ── FC-P1-D: binary-operator HIR pre-pass gates ──────────────────────────────

/// Context carried by the binary-operator gate walker. Bundles the
/// diagnostic sink with the checker's `expr_types` side-table so the
/// `isize`/`usize` predicates can resolve operand types.
pub(super) struct BinopGateCtx<'a> {
    pub(super) diagnostics: &'a mut Vec<HirDiagnostic>,
}

/// FC-P1-D entry point. Scans every user expression body in `program` for
/// binary operators that the MIR backend cannot lower today and emits the
/// corresponding fatal HIR diagnostic. One closed gate remains:
///
/// 1. `..` / `..=` in value position (MIR site `:5336`).
///
/// (The former `isize`/`usize` div/rem and shift gates are gone: MIR now
/// threads the target pointer width and emits the correct per-target trap
/// constants, so those operators lower end-to-end.)
///
/// Walker shape mirrors `check_wasm_blocking_recv_gate` / `scan_*_for_
/// blocking_recv`. Range gate is exempted when the binary expression is the
/// direct iterable of a `for` loop (the `ForRange` lowering owns those).
pub(super) fn check_binary_operator_gates(ctx: &mut LowerCtx, program: &Program) {
    let mut gate_ctx = BinopGateCtx {
        diagnostics: &mut ctx.diagnostics,
    };
    for (item, _span) in &program.items {
        match item {
            Item::Function(fn_decl) => {
                scan_block_for_binop_gates(&fn_decl.body, &mut gate_ctx);
            }
            Item::Actor(actor_decl) => {
                if let Some(init) = &actor_decl.init {
                    scan_block_for_binop_gates(&init.body, &mut gate_ctx);
                }
                for recv_fn in &actor_decl.receive_fns {
                    scan_block_for_binop_gates(&recv_fn.body, &mut gate_ctx);
                }
                for method in &actor_decl.methods {
                    scan_block_for_binop_gates(&method.body, &mut gate_ctx);
                }
            }
            Item::Impl(impl_decl) => {
                for method in &impl_decl.methods {
                    scan_block_for_binop_gates(&method.body, &mut gate_ctx);
                }
            }
            // Machine bodies never reach here: normalization rewrites a machine
            // into ordinary declarations before checking, and a machine it refuses
            // fails type check before HIR. The expanded bodies are walked through
            // `Item::Impl` like any other method.
            // Variants below carry no user expression bodies that reach MIR
            // in v0.5; each is explicit (no `_` catch-all) so a future
            // `Item` variant trips compilation and forces an audit instead of
            // silently slipping past the gate (cf. A228 walker-scope rule).
            //
            // - Const: value expr is parsed but `Item::Const` is currently
            //   emitted as `unsupported top-level-item slice-2` in `lower.rs`
            //   (~line 1338); no MIR reachable, so no binop site to gate.
            // - Trait: default-method bodies live on `TraitMethod` but are
            //   not lowered in V0b (`lower.rs` ~line 3666: "out of scope for
            //   V0b"). Impl methods are scanned via the `Item::Impl` arm.
            // - Supervisor: `ChildSpec.args` are not lowered by
            //   `lower_supervisor` (`lower.rs` ~line 3936), so any binop in a
            //   child-arg position never reaches MIR.
            // - TypeDecl: struct/enum field types carry no expressions;
            //   `TypeBodyItem::Method` is "not lowered into HIR in v0.5"
            //   (`lower.rs` ~line 3822).
            // - TypeAlias / Wire / Record / ExternBlock / Import: no
            //   value-position user expressions.
            Item::Const(_)
            | Item::TypeDecl(_)
            | Item::TypeAlias(_)
            | Item::Trait(_)
            | Item::ExternBlock(_)
            | Item::Supervisor(_)
            | Item::Record(_)
            | Item::Machine(_)
            | Item::Import(_) => {}
        }
    }
}

pub(super) fn scan_block_for_binop_gates(block: &hew_parser::ast::Block, ctx: &mut BinopGateCtx) {
    for (stmt, _) in &block.stmts {
        scan_stmt_for_binop_gates(stmt, ctx);
    }
    if let Some(trailing) = &block.trailing_expr {
        scan_expr_for_binop_gates(&trailing.0, &trailing.1, false, ctx);
    }
}

#[allow(
    clippy::match_same_arms,
    reason = "explicit per-Stmt-variant arms read more clearly than collapsed or-patterns for this walker"
)]
pub(super) fn scan_stmt_for_binop_gates(stmt: &hew_parser::ast::Stmt, ctx: &mut BinopGateCtx) {
    match stmt {
        Stmt::Let { value: Some(v), .. } | Stmt::Var { value: Some(v), .. } => {
            scan_expr_for_binop_gates(&v.0, &v.1, false, ctx);
        }
        Stmt::Assign { target, value, .. } => {
            scan_expr_for_binop_gates(&target.0, &target.1, false, ctx);
            scan_expr_for_binop_gates(&value.0, &value.1, false, ctx);
        }
        Stmt::Expression(e) => {
            scan_expr_for_binop_gates(&e.0, &e.1, false, ctx);
        }
        Stmt::Return(Some(e)) => {
            scan_expr_for_binop_gates(&e.0, &e.1, false, ctx);
        }
        Stmt::Defer(e) => {
            scan_expr_for_binop_gates(&e.0, &e.1, false, ctx);
        }
        Stmt::Break { value: Some(v), .. } => {
            scan_expr_for_binop_gates(&v.0, &v.1, false, ctx);
        }
        Stmt::WhileLet {
            conditions, body, ..
        } => {
            for expr in condition_exprs(conditions) {
                scan_expr_for_binop_gates(&expr.0, &expr.1, false, ctx);
            }
            scan_block_for_binop_gates(body, ctx);
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            scan_expr_for_binop_gates(&condition.0, &condition.1, false, ctx);
            scan_block_for_binop_gates(then_block, ctx);
            if let Some(eb) = else_block {
                scan_else_block_for_binop_gates(eb, ctx);
            }
        }
        Stmt::IfLet {
            conditions,
            body,
            else_body,
        } => {
            for expr in condition_exprs(conditions) {
                scan_expr_for_binop_gates(&expr.0, &expr.1, false, ctx);
            }
            scan_block_for_binop_gates(body, ctx);
            if let Some(eb) = else_body {
                scan_expr_for_binop_gates(&eb.0, &eb.1, false, ctx);
            }
        }
        Stmt::Match { scrutinee, arms } => {
            scan_expr_for_binop_gates(&scrutinee.0, &scrutinee.1, false, ctx);
            for arm in arms {
                if let Some(g) = &arm.guard {
                    scan_expr_for_binop_gates(&g.0, &g.1, false, ctx);
                }
                scan_expr_for_binop_gates(&arm.body.0, &arm.body.1, false, ctx);
            }
        }
        Stmt::Loop { body, .. } => scan_block_for_binop_gates(body, ctx),
        Stmt::For { iterable, body, .. } => {
            // Range/RangeInclusive directly in the iterable position is
            // lowered via `ForRange`, so exempt the OUTER binop only.
            // Operand sub-expressions are still scanned without the
            // exemption (a nested range in `for i in (1..foo(2..3))` is
            // still value-position).
            //
            // A `(a..b).rev()` / `.step_by(k)` adapter chain also lowers via
            // `ForRange`, so the base range under the adapters is likewise
            // exempt.  Peel the `rev`/`step_by` wrappers and scan the base range
            // with the exemption while still scanning each `step_by` argument as
            // a value-position sub-expression.
            scan_for_iterable_for_binop_gates(iterable, ctx);
            scan_block_for_binop_gates(body, ctx);
        }
        Stmt::While {
            condition, body, ..
        } => {
            scan_expr_for_binop_gates(&condition.0, &condition.1, false, ctx);
            scan_block_for_binop_gates(body, ctx);
        }
        // Stmt::Break (no value), Stmt::Continue, Stmt::Return(None), etc.
        // carry no sub-expression to scan.
        _ => {}
    }
}

pub(super) fn scan_else_block_for_binop_gates(
    eb: &hew_parser::ast::ElseBlock,
    ctx: &mut BinopGateCtx,
) {
    if let Some(stmt) = &eb.if_stmt {
        scan_stmt_for_binop_gates(&stmt.0, ctx);
    }
    if let Some(b) = &eb.block {
        scan_block_for_binop_gates(b, ctx);
    }
}

/// Scan a `for`-loop iterable for binop-gate violations, exempting the base
/// range of a `(a..b).rev()` / `.step_by(k)` adapter chain.
///
/// A plain `a..b` iterable is exempted one-deep (the for-loop lowering handles
/// it).  An adapter chain over a range is also lowered via `ForRange`, so the
/// base range is exempt too; the `step_by` arguments are ordinary value-position
/// sub-expressions and are scanned without the exemption.
pub(super) fn scan_for_iterable_for_binop_gates(iterable: &Spanned<Expr>, ctx: &mut BinopGateCtx) {
    match &iterable.0 {
        Expr::MethodCall {
            receiver,
            method,
            args,
        } if matches!(method.as_str(), "rev" | "step_by") => {
            // Recurse into the receiver as a for-iterable (peeling further
            // adapters / reaching the base range); scan the adapter arguments
            // as plain value-position expressions.
            scan_for_iterable_for_binop_gates(receiver, ctx);
            for arg in args {
                let a = arg.expr();
                scan_expr_for_binop_gates(&a.0, &a.1, false, ctx);
            }
        }
        _ => scan_expr_for_binop_gates(&iterable.0, &iterable.1, true, ctx),
    }
}

/// Recursively walk an expression looking for binop-gate violations.
///
/// `in_for_iterable` is set when this expression is the direct iterable
/// child of `Stmt::For`. A top-level `Range`/`RangeInclusive` in that
/// position is exempted (the for-loop lowering handles it). The flag does
/// NOT propagate into sub-expressions.
#[allow(
    clippy::too_many_lines,
    reason = "exhaustive Expr-variant walker mirrors scan_expr_for_blocking_recv"
)]
pub(super) fn scan_expr_for_binop_gates(
    expr: &Expr,
    span: &Span,
    in_for_iterable: bool,
    ctx: &mut BinopGateCtx,
) {
    if let Expr::Binary { left, op, right } = expr {
        // Apply gates to THIS binop. Sub-expressions are recursed below
        // (always with in_for_iterable=false; the exemption is one-deep).
        apply_binop_gates(*op, left, right, span, in_for_iterable, ctx);
        scan_expr_for_binop_gates(&left.0, &left.1, false, ctx);
        scan_expr_for_binop_gates(&right.0, &right.1, false, ctx);
        return;
    }

    match expr {
        Expr::Binary { .. } => unreachable!("handled above"),
        Expr::Coalesce { left, right }
        | Expr::Handle {
            operand: left,
            body: right,
            ..
        } => {
            scan_expr_for_binop_gates(&left.0, &left.1, false, ctx);
            scan_expr_for_binop_gates(&right.0, &right.1, false, ctx);
        }
        Expr::Unary { operand, .. } => {
            scan_expr_for_binop_gates(&operand.0, &operand.1, false, ctx);
        }
        Expr::MethodCall { receiver, args, .. } => {
            scan_expr_for_binop_gates(&receiver.0, &receiver.1, false, ctx);
            for arg in args {
                let a = arg.expr();
                scan_expr_for_binop_gates(&a.0, &a.1, false, ctx);
            }
        }
        Expr::Call { function, args, .. } => {
            scan_expr_for_binop_gates(&function.0, &function.1, false, ctx);
            for arg in args {
                let a = arg.expr();
                scan_expr_for_binop_gates(&a.0, &a.1, false, ctx);
            }
        }
        Expr::Tuple(es) | Expr::Race(es) => {
            for e in es {
                scan_expr_for_binop_gates(&e.0, &e.1, false, ctx);
            }
        }
        Expr::Array(elements) => {
            for element in elements {
                let operand = element.expr();
                scan_expr_for_binop_gates(&operand.0, &operand.1, false, ctx);
            }
        }
        Expr::ArrayRepeat { value, count } => {
            scan_expr_for_binop_gates(&value.0, &value.1, false, ctx);
            scan_expr_for_binop_gates(&count.0, &count.1, false, ctx);
        }
        Expr::Block(b)
        | Expr::Scope { body: b }
        | Expr::ForkBlock { body: b }
        | Expr::GenBlock { body: b } => {
            scan_block_for_binop_gates(b, ctx);
        }
        Expr::If {
            condition,
            then_block,
            else_block,
            ..
        } => {
            scan_expr_for_binop_gates(&condition.0, &condition.1, false, ctx);
            scan_expr_for_binop_gates(&then_block.0, &then_block.1, false, ctx);
            if let Some(e) = else_block {
                scan_expr_for_binop_gates(&e.0, &e.1, false, ctx);
            }
        }
        Expr::IfLet {
            conditions,
            body,
            else_body,
        } => {
            for expr in condition_exprs(conditions) {
                scan_expr_for_binop_gates(&expr.0, &expr.1, false, ctx);
            }
            scan_block_for_binop_gates(body, ctx);
            if let Some(b) = else_body {
                scan_expr_for_binop_gates(&b.0, &b.1, false, ctx);
            }
        }
        Expr::Match { scrutinee, arms } => {
            scan_expr_for_binop_gates(&scrutinee.0, &scrutinee.1, false, ctx);
            for arm in arms {
                if let Some(g) = &arm.guard {
                    scan_expr_for_binop_gates(&g.0, &g.1, false, ctx);
                }
                scan_expr_for_binop_gates(&arm.body.0, &arm.body.1, false, ctx);
            }
        }
        Expr::Lambda { body, .. } | Expr::SpawnLambdaActor { body, .. } => {
            scan_expr_for_binop_gates(&body.0, &body.1, false, ctx);
        }
        Expr::Spawn { target, args, .. } => {
            scan_expr_for_binop_gates(&target.0, &target.1, false, ctx);
            for (_, v) in args {
                scan_expr_for_binop_gates(&v.0, &v.1, false, ctx);
            }
        }
        Expr::ScopeDeadline { duration, body } => {
            scan_expr_for_binop_gates(&duration.0, &duration.1, false, ctx);
            scan_block_for_binop_gates(body, ctx);
        }
        Expr::ForkChild { expr, .. } | Expr::Cast { expr, .. } => {
            scan_expr_for_binop_gates(&expr.0, &expr.1, false, ctx);
        }
        Expr::StructInit { fields, base, .. } => {
            for (_, v) in fields {
                scan_expr_for_binop_gates(&v.0, &v.1, false, ctx);
            }
            if let Some(b) = base {
                scan_expr_for_binop_gates(&b.0, &b.1, false, ctx);
            }
        }
        Expr::MapLiteral { entries } => {
            for (k, v) in entries {
                scan_expr_for_binop_gates(&k.0, &k.1, false, ctx);
                scan_expr_for_binop_gates(&v.0, &v.1, false, ctx);
            }
        }
        Expr::InterpolatedString(parts) => {
            for part in parts {
                if let hew_parser::ast::StringPart::Expr(e)
                | hew_parser::ast::StringPart::StructuralExpr(e) = part
                {
                    scan_expr_for_binop_gates(&e.0, &e.1, false, ctx);
                }
            }
        }
        Expr::Select { arms, timeout } => {
            for arm in arms {
                scan_expr_for_binop_gates(&arm.source.0, &arm.source.1, false, ctx);
                scan_expr_for_binop_gates(&arm.body.0, &arm.body.1, false, ctx);
            }
            if let Some(t) = timeout {
                scan_expr_for_binop_gates(&t.duration.0, &t.duration.1, false, ctx);
                scan_expr_for_binop_gates(&t.body.0, &t.body.1, false, ctx);
            }
        }
        Expr::UnsafeBlock(b) => scan_block_for_binop_gates(b, ctx),
        Expr::FieldAccess { object, .. } | Expr::PostfixTry(object) | Expr::Await(object) => {
            scan_expr_for_binop_gates(&object.0, &object.1, false, ctx);
        }
        Expr::Index { object, index } => {
            scan_expr_for_binop_gates(&object.0, &object.1, false, ctx);
            scan_expr_for_binop_gates(&index.0, &index.1, false, ctx);
        }
        Expr::Is { lhs, rhs } => {
            scan_expr_for_binop_gates(&lhs.0, &lhs.1, false, ctx);
            scan_expr_for_binop_gates(&rhs.0, &rhs.1, false, ctx);
        }
        Expr::Range { start, end, .. } => {
            // `Expr::Range` is the AST node for slice-index ranges
            // (`xs[a..b]`); separate from `Expr::Binary { op: Range }`.
            // No gate, but recurse into operands.
            if let Some(s) = start {
                scan_expr_for_binop_gates(&s.0, &s.1, false, ctx);
            }
            if let Some(e) = end {
                scan_expr_for_binop_gates(&e.0, &e.1, false, ctx);
            }
        }
        Expr::Yield(Some(e)) => scan_expr_for_binop_gates(&e.0, &e.1, false, ctx),
        Expr::MachineEmit { fields, .. } => {
            for (_, v) in fields {
                scan_expr_for_binop_gates(&v.0, &v.1, false, ctx);
            }
        }
        // Leaf nodes (Identifier, literals, Yield(None), etc.).
        _ => {}
    }
}

/// Apply the FC-P1-D binop gates to a single `Expr::Binary` node.
///
/// Exhaustive over `BinaryOp` so future operator additions surface as
/// compile errors here. WHEN-ADDING-BINOP: extend this match to gate or
/// explicitly admit the new operator.
///
/// `isize`/`usize` `/` `%` `<<` `>>` are admitted: MIR threads the target
/// pointer width (`PointerWidth`) and emits the correct per-target signed-MIN
/// and shift-range trap constants, so the old `PlatformSized{DivRem,Shift}`
/// gates here are removed (they were dead code — the checker already admitted
/// these operands as concrete integers).
pub(super) fn apply_binop_gates(
    op: BinaryOp,
    _left: &Spanned<Expr>,
    _right: &Spanned<Expr>,
    binop_span: &Span,
    in_for_iterable: bool,
    ctx: &mut BinopGateCtx,
) {
    match op {
        // Gate 1: Range / RangeInclusive in value position.
        BinaryOp::Range | BinaryOp::RangeInclusive => {
            if !in_for_iterable {
                ctx.diagnostics.push(HirDiagnostic::new(
                    HirDiagnosticKind::BinaryOperatorUnsupportedInMir {
                        op: format!("{op}"),
                    },
                    binop_span.clone(),
                    format!(
                        "binary operator `{op}` is not lowered to MIR in value \
                         position. Range operators are accepted only as the \
                         iterable of a `for` loop. LESSONS `boundary-fail-closed`."
                    ),
                ));
            }
        }
        // MIR-supported operators (admitted). Listed explicitly so adding a
        // new BinaryOp variant elsewhere triggers a non-exhaustive-match
        // compile error here. Divide/Modulo/Shl/Shr are admitted for every
        // integer width including platform-sized isize/usize — MIR emits the
        // target-width trap guards.
        BinaryOp::Add
        | BinaryOp::Subtract
        | BinaryOp::Multiply
        | BinaryOp::Divide
        | BinaryOp::Modulo
        | BinaryOp::Shl
        | BinaryOp::Shr
        | BinaryOp::Equal
        | BinaryOp::NotEqual
        | BinaryOp::Less
        | BinaryOp::LessEqual
        | BinaryOp::Greater
        | BinaryOp::GreaterEqual
        | BinaryOp::And
        | BinaryOp::Or
        | BinaryOp::BitAnd
        | BinaryOp::BitOr
        | BinaryOp::BitXor
        | BinaryOp::WrappingAdd
        | BinaryOp::WrappingSub
        | BinaryOp::WrappingMul => {}
    }
}
