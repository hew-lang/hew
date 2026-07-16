use std::collections::BTreeSet;

use hew_parser::ast::{
    BinaryOp, CallArg, Expr, ImportDecl, Item, Pattern, Program, Spanned, Stmt, TypeExpr,
};
use hew_types::{check::SpanKey, BuiltinType, Ty};

use crate::Diagnostic;

pub const DEFAULT_PROFILE_ALIAS: &str = "sandbox-vm-export";
pub const DEFAULT_PROFILE_CANONICAL: &str = "sandbox.sandbox-vm-export.v0";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[expect(
    non_camel_case_types,
    reason = "diagnostic enum variant names intentionally match the documented sandbox diagnostic vocabulary"
)]
enum Unsupported {
    NATIVE_ONLY,
}

impl Unsupported {
    fn diagnostic_kind(self) -> &'static str {
        match self {
            Self::NATIVE_ONLY => "Unsupported::NATIVE_ONLY",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NativeOnlySurface {
    FileIo,
    NetworkSockets,
    NativeFfi,
    OsEnvironment,
    OsProcesses,
    RealTime,
}

impl NativeOnlySurface {
    fn reason(self) -> &'static str {
        match self {
            Self::FileIo => "host filesystem and file-backed I/O are unavailable in the browser sandbox",
            Self::NetworkSockets => "host networking is unavailable in the browser sandbox",
            Self::NativeFfi => "native FFI declarations are unavailable in the browser sandbox",
            Self::OsEnvironment => "host OS interfaces and environment variables are unavailable in the browser sandbox",
            Self::OsProcesses => "host process execution and process inspection are unavailable in the browser sandbox",
            Self::RealTime => "real-clock APIs are unavailable; use virtual time fixtures instead",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProfileReport {
    pub diagnostics: Vec<Diagnostic>,
}

/// Canonicalize the caller-facing sandbox profile alias.
///
/// # Errors
///
/// Returns a profile diagnostic when the requested profile is not admitted by
/// this crate's explicit browser bytecode-export allowlist.
#[expect(
    clippy::result_large_err,
    reason = "profile selection failures are surfaced as the same diagnostic shape as compile gates"
)]
pub fn canonical_profile(profile: Option<&str>) -> Result<String, Diagnostic> {
    match profile.unwrap_or(DEFAULT_PROFILE_ALIAS).trim() {
        "" | DEFAULT_PROFILE_ALIAS | DEFAULT_PROFILE_CANONICAL => {
            Ok(DEFAULT_PROFILE_CANONICAL.to_string())
        }
        unknown => Err(Diagnostic::profile_error(
            0..0,
            "unknown_sandbox_profile",
            format!(
                "unknown sandbox profile `{unknown}`; use `{DEFAULT_PROFILE_ALIAS}` for browser bytecode export"
            ),
        )),
    }
}

pub fn check_program(
    program: &Program,
    type_output: &hew_types::TypeCheckOutput,
    _profile: &str,
) -> ProfileReport {
    let mut checker = ProfileChecker::new(program, type_output);
    checker.check_program(program);
    ProfileReport {
        diagnostics: checker.diagnostics,
    }
}

#[derive(Debug)]
struct ProfileChecker<'a> {
    diagnostics: Vec<Diagnostic>,
    imports: BTreeSet<String>,
    user_functions: BTreeSet<String>,
    user_types: BTreeSet<String>,
    enum_variants: BTreeSet<String>,
    /// Actor type names declared in the program (admits `spawn` + actor field/method access).
    actors: BTreeSet<String>,
    /// Receive-function names across all actors (admits `await ref.method(...)` asks).
    actor_methods: BTreeSet<String>,
    /// Machine type names declared in the program (admits `.step`/`.state_name`).
    machines: BTreeSet<String>,
    type_output: &'a hew_types::TypeCheckOutput,
    /// True while checking inside a receive handler body. Used to fail-closed on
    /// `return` statements, which bypass the implicit actor.reply insertion.
    in_receive_handler: bool,
}

impl<'a> ProfileChecker<'a> {
    fn new(program: &Program, type_output: &'a hew_types::TypeCheckOutput) -> Self {
        let mut checker = Self {
            diagnostics: Vec::new(),
            imports: BTreeSet::new(),
            user_functions: BTreeSet::new(),
            user_types: BTreeSet::new(),
            enum_variants: BTreeSet::new(),
            actors: BTreeSet::new(),
            actor_methods: BTreeSet::new(),
            machines: BTreeSet::new(),
            type_output,
            in_receive_handler: false,
        };
        checker.collect_declarations(program);
        checker
    }

    fn collect_declarations(&mut self, program: &Program) {
        for (item, _) in &program.items {
            match item {
                Item::Function(function) => {
                    self.user_functions.insert(function.name.clone());
                }
                Item::TypeDecl(type_decl) => {
                    self.user_types.insert(type_decl.name.clone());
                    for body_item in &type_decl.body {
                        if let hew_parser::ast::TypeBodyItem::Variant(variant) = body_item {
                            self.enum_variants.insert(variant.name.clone());
                        }
                    }
                }
                Item::Record(record) => {
                    self.user_types.insert(record.name.clone());
                }
                Item::Actor(actor) => {
                    self.actors.insert(actor.name.clone());
                    for receive_fn in &actor.receive_fns {
                        self.actor_methods.insert(receive_fn.name.clone());
                    }
                }
                Item::Machine(machine) => {
                    self.machines.insert(machine.name.clone());
                }
                _ => {}
            }
        }
    }

    fn check_program(&mut self, program: &Program) {
        for (item, span) in &program.items {
            match item {
                Item::Import(import) => self.check_import(import, span),
                Item::ExternBlock(_) => self.reject_native_only(
                    span.clone(),
                    NativeOnlySurface::NativeFfi,
                    NativeOnlySurface::NativeFfi.reason(),
                ),
                Item::Function(function) => self.check_block(&function.body),
                Item::Actor(actor) => {
                    // Actors are admitted: the VM runtime (ActorScheduler) executes
                    // actor bytecode. Walk each receive handler body fail-closed.
                    for receive_fn in &actor.receive_fns {
                        self.in_receive_handler = true;
                        self.check_block(&receive_fn.body);
                        self.in_receive_handler = false;
                    }
                }
                Item::Supervisor(supervisor) => {
                    // Supervisors are admitted: the VM scheduler models restart
                    // strategies. Walk each child's init-arg expressions.
                    // Child init args MUST be literals — the emitter bakes them
                    // into the supervisor layout at compile time. Non-literal args
                    // would silently become null placeholders, spawning actors with
                    // wrong initial state. Reject non-literals fail-closed.
                    for child in &supervisor.children {
                        for (field_name, value) in &child.args {
                            if !is_literal_expr(&value.0) {
                                self.reject(
                                    value.1.clone(),
                                    "supervisor_child_init_requires_literal",
                                    format!(
                                        "supervisor child `{}` field `{}` must be a literal value; \
                                         computed init args are not yet admitted in the browser sandbox \
                                         (the emitter bakes child state at compile time)",
                                        child.name, field_name
                                    ),
                                );
                            }
                            self.check_expr(value);
                        }
                    }
                }
                Item::Machine(machine) => self.check_machine(machine, span),
                Item::Trait(_) | Item::Impl(_) => self.reject(
                    span.clone(),
                    "reserved_runtime_feature",
                    "this declaration requires runtime features that are reserved for a later sandbox VM milestone",
                ),
                Item::Const(const_decl) => self.check_expr(&const_decl.value),
                Item::TypeDecl(type_decl) => {
                    // W3.030 V15: `#[resource]` types carry an implicit drop
                    // contract that dispatches `<T>::close` through the
                    // unified `ScopeExitPlan` stream. The sandbox-WASM
                    // bytecode export does not yet model that drop
                    // scheduling (the W3.021 `defer_rejected` follow-up
                    // tracks the same gap). Fail closed at the surface
                    // boundary with a named diagnostic so the sandbox can
                    // never silently miss a resource close — tracked in
                    // `.tmp/deferred-v05-followups.md`.
                    if !type_decl.resource_marker.is_none() {
                        self.reject(
                            span.clone(),
                            "user_resource_close_not_yet_admitted_sandbox",
                            "`#[resource]` and `#[linear]` types carry an \
                             implicit drop contract that is not yet admitted \
                             to sandbox bytecode export (W3.030 follow-up)",
                        );
                    }
                }
                Item::TypeAlias(_) => self.reject(
                    span.clone(),
                    "reserved_runtime_feature",
                    // Top-level type aliases are `E_NOT_YET_IMPLEMENTED` in the
                    // native HIR lowering, so a program using one cannot run
                    // natively at all — there is no native behaviour to be at
                    // parity with. The sandbox admitted it (no-op arm) and emitted
                    // bytecode that ran, diverging from native (which refuses to
                    // compile). Reject fail-closed so the sandbox never runs a
                    // construct the native engine itself rejects.
                    "top-level type aliases are not yet supported by the native engine, so they \
                     are not admitted to sandbox bytecode export",
                ),
                Item::Record(_) => {}
            }
        }
    }

    fn check_import(&mut self, import: &ImportDecl, span: &std::ops::Range<usize>) {
        let path = import.path.join("::");
        self.imports.insert(path.clone());
        if path == "std::text::regex" {
            return;
        }

        let rejected_prefixes = [
            ("std::fs", NativeOnlySurface::FileIo),
            ("std::io", NativeOnlySurface::FileIo),
            ("std::net", NativeOnlySurface::NetworkSockets),
            ("std::process", NativeOnlySurface::OsProcesses),
            ("std::os", NativeOnlySurface::OsEnvironment),
            ("std::env", NativeOnlySurface::OsEnvironment),
            ("std::time", NativeOnlySurface::RealTime),
        ];
        if let Some((_, surface)) = rejected_prefixes
            .iter()
            .find(|(prefix, _)| path == *prefix || path.starts_with(&format!("{prefix}::")))
        {
            self.reject_native_only(
                span.clone(),
                *surface,
                format!("`{path}` is rejected: {}", surface.reason()),
            );
            return;
        }

        if path.starts_with("std::") {
            self.reject(
                span.clone(),
                "unknown_stdlib_symbol",
                format!("`{path}` is not in the sandbox profile allowlist"),
            );
        } else {
            self.reject(
                span.clone(),
                "unknown_module_import",
                "browser sandbox bytecode export is single-source today; module imports are rejected by default",
            );
        }
    }

    fn check_block(&mut self, block: &hew_parser::ast::Block) {
        for (stmt, span) in &block.stmts {
            self.check_stmt(stmt, span);
        }
        if let Some(expr) = &block.trailing_expr {
            self.check_expr(expr);
        }
    }

    /// Admit a flat, declarative state machine. The VM models the transition
    /// table (`machine.new`/`step`/`state`), but does not yet run generic
    /// monomorphisation, Mealy `emit` outputs, transition guards, or
    /// state entry/exit lifecycle blocks — machines using those fail closed.
    fn check_machine(
        &mut self,
        machine: &hew_parser::ast::MachineDecl,
        span: &std::ops::Range<usize>,
    ) {
        let uses_unsupported = !machine.type_params.is_empty()
            || !machine.const_params.is_empty()
            || machine.where_clause.is_some()
            || !machine.emits.is_empty()
            || machine.has_default
            || machine
                .states
                .iter()
                .any(|state| state.entry.is_some() || state.exit.is_some())
            || machine
                .transitions
                .iter()
                .any(|transition| transition.guard.is_some());
        if uses_unsupported {
            self.reject(
                span.clone(),
                "reserved_runtime_feature",
                "this machine uses generics, guards, emit outputs, or lifecycle hooks that are reserved for a later sandbox VM milestone",
            );
        }

        // Transition bodies that do more than select the declared target state
        // are silently ignored by the sandbox runtime (machine.step follows the
        // static transition table). Fail closed if a body contains side-effects
        // or non-trivial expressions so authors are not surprised.
        for transition in &machine.transitions {
            if !is_trivial_machine_transition_body(&transition.body.0) {
                self.reject(
                    transition.body.1.clone(),
                    "machine_transition_body_not_admitted",
                    "machine transition bodies with side-effects or computed expressions are not \
                     admitted in the browser sandbox; the VM follows the static transition table \
                     and ignores the body — use a plain state-name identifier as the body",
                );
            }
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "check_stmt walks every admitted statement variant fail-closed; splitting would obscure the admission contract"
    )]
    fn check_stmt(&mut self, stmt: &Stmt, span: &std::ops::Range<usize>) {
        match stmt {
            Stmt::Let {
                pattern,
                value,
                ty,
                else_block,
            } => {
                if let Some((ty, _)) = ty {
                    self.check_type_expr(ty, span);
                }
                if let Some(expr) = value {
                    self.check_expr(expr);
                }
                self.check_pattern(pattern);
                if else_block.is_some() || !let_pattern_is_unconditional(&pattern.0) {
                    self.reject(
                        pattern.1.clone(),
                        "reserved_runtime_feature",
                        "refutable let patterns are reserved for a later sandbox VM milestone; use match or if let instead",
                    );
                }
            }
            Stmt::Var { value, ty, .. } => {
                if let Some((ty, _)) = ty {
                    self.check_type_expr(ty, span);
                }
                if let Some(expr) = value {
                    self.check_expr(expr);
                }
            }
            Stmt::Assign { target, op: _, value } => {
                // Compound assignment (`x += v`, `x -= v`, …): the arithmetic
                // forms (+, -, *, /, %) are now lowered type-correctly by the
                // emitter (reading the current binding, applying the type-directed
                // opcode, and writing back). Bitwise/shift forms that the emitter
                // cannot lower fall through to `emit_unsupported` (fail-loud trap).
                self.check_expr(target);
                self.check_expr(value);
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                self.check_expr(condition);
                self.check_block(then_block);
                if let Some(else_block) = else_block {
                    if let Some(block) = &else_block.block {
                        self.check_block(block);
                    }
                    if let Some(if_stmt) = &else_block.if_stmt {
                        self.check_stmt(&if_stmt.0, &if_stmt.1);
                    }
                }
            }
            Stmt::Match { scrutinee, arms } => {
                self.check_expr(scrutinee);
                for arm in arms {
                    self.check_pattern(&arm.pattern);
                    if let Some(guard) = &arm.guard {
                        self.check_expr(guard);
                    }
                    self.check_expr(&arm.body);
                }
            }
            Stmt::Return(expr) => {
                // Early `return` inside a receive handler is admitted: the sandbox
                // scheduler's trampoline fallback (scheduler.ts `stepActor`) resolves
                // the reply slot with the handler's return value after `invoke()` returns,
                // mirroring the native dispatch trampoline (`llvm.rs:24730-24784`).
                if let Some(expr) = expr.as_ref() {
                    self.check_expr(expr);
                }
            }
            Stmt::Expression(expr) => self.check_expr(expr),
            Stmt::Loop { body, label } => {
                if label.is_some() {
                    self.reject(
                        span.clone(),
                        "reserved_control_flow",
                        "labeled loops are not yet admitted in the sandbox profile",
                    );
                }
                self.check_block(body);
            }
            Stmt::For { is_await, iterable, body, label, .. } => {
                if *is_await {
                    self.reject(
                        span.clone(),
                        "reserved_runtime_feature",
                        "for-await requires async runtime support reserved for a later sandbox VM milestone",
                    );
                    return;
                }
                if label.is_some() {
                    self.reject(
                        span.clone(),
                        "reserved_control_flow",
                        "labeled for loops are not yet admitted in the sandbox profile",
                    );
                }
                self.check_range_operand(iterable);
                self.check_block(body);
            }
            Stmt::While { condition, body, label } => {
                if label.is_some() {
                    self.reject(
                        span.clone(),
                        "reserved_control_flow",
                        "labeled while loops are not yet admitted in the sandbox profile",
                    );
                }
                self.check_expr(condition);
                self.check_block(body);
            }
            Stmt::WhileLet {
                pattern,
                expr,
                body,
                label,
            } => {
                if label.is_some() {
                    self.reject(
                        span.clone(),
                        "reserved_control_flow",
                        "labeled while-let loops are not yet admitted in the sandbox profile",
                    );
                }
                self.check_pattern(pattern);
                if !matches!(pattern.0, Pattern::Constructor { .. }) {
                    self.reject(
                        pattern.1.clone(),
                        "reserved_runtime_feature",
                        "non-constructor while-let patterns are reserved until native HIR lowering supports them",
                    );
                }
                self.check_expr(expr);
                self.check_block(body);
            }
            Stmt::Break { value, label } => {
                if value.is_some() {
                    self.reject(
                        span.clone(),
                        "reserved_control_flow",
                        "break-with-value is not yet admitted in the sandbox profile",
                    );
                }
                // Labeled loops are themselves rejected (above), so a `break
                // @label` can only reference a rejected loop or a dangling label
                // the emitter cannot resolve. Reject the labeled break directly
                // so the rejection is named here, not deferred to an
                // `unsupported_instruction` trap on an unresolved label.
                if label.is_some() {
                    self.reject(
                        span.clone(),
                        "reserved_control_flow",
                        "labeled `break` is not yet admitted in the sandbox profile",
                    );
                }
            }
            Stmt::Continue { label } => {
                if label.is_some() {
                    self.reject(
                        span.clone(),
                        "reserved_control_flow",
                        "labeled `continue` is not yet admitted in the sandbox profile",
                    );
                }
            }
            Stmt::IfLet {
                pattern,
                expr,
                body,
                else_body,
            } => {
                self.check_pattern(pattern);
                if !matches!(pattern.0, Pattern::Constructor { .. }) {
                    self.reject(
                        pattern.1.clone(),
                        "reserved_runtime_feature",
                        "non-constructor if-let patterns are reserved until native HIR lowering supports them",
                    );
                }
                self.check_expr(expr);
                self.check_block(body);
                if let Some(block) = else_body {
                    self.check_block(block);
                }
            }
            Stmt::Defer(_) => self.reject(
                span.clone(),
                "defer_rejected",
                "defer needs runtime drop scheduling and is not admitted to sandbox bytecode export yet",
            ),
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "profile admission walks every parser expression variant fail-closed"
    )]
    fn check_expr(&mut self, expr: &Spanned<Expr>) {
        let (expr, span) = expr;
        match expr {
            Expr::Literal(_) | Expr::Identifier(_) | Expr::This | Expr::RegexLiteral(_) => {}
            Expr::Binary { left, op, right } => {
                // `a..b` / `a..=b` parse as `Expr::Binary` with a range operator.
                // In bare value position these have no sandbox lowering (the
                // emitter's `lower_binary` catch-all traps at runtime), so reject
                // them at the gate. The admitted structural positions (for-loop
                // iterables and slice subscripts) route through
                // `check_range_operand` and never reach this arm.
                if matches!(op, BinaryOp::Range | BinaryOp::RangeInclusive) {
                    self.reject(
                        span.clone(),
                        "reserved_runtime_feature",
                        "range values are not admitted in the browser sandbox except as \
                         `for` loop iterables and slice subscripts (`v[a..b]`)",
                    );
                }
                // Structural equality (`==`/`!=`) on records, payload enums,
                // Option, and Result is now admitted: `lower_binary` emits
                // `cmp.eq`/`cmp.ne` for all types, and the VM's `compare` handler
                // uses `canonicalComparable` which serialises any VmValue
                // (including records and enums with payload) to a canonical JSON
                // string, giving structural field-by-field equality that mirrors
                // native Hew semantics. Bitwise/shift/logic ops that the emitter
                // cannot lower fall through to `emit_unsupported` (fail-loud trap).
                self.check_expr(left);
                self.check_expr(right);
            }
            Expr::Is { lhs, rhs } => {
                self.reject(
                    span.clone(),
                    "reserved_runtime_feature",
                    "identity comparison needs heap identity semantics reserved for a later sandbox VM milestone",
                );
                self.check_expr(lhs);
                self.check_expr(rhs);
            }
            Expr::Unary { operand, .. }
            | Expr::Cast { expr: operand, .. }
            | Expr::PostfixTry(operand) => {
                self.check_expr(operand);
            }
            Expr::Return(operand) => {
                // `return` in expression position is not yet lowered by the
                // sandbox-wasm emitter (it emits `unsupported`). Reject it here
                // so the gap fails closed before emission. Still walk the
                // operand for completeness.
                self.reject(
                    span.clone(),
                    "reserved_runtime_feature",
                    "`return` in expression position is reserved for a later sandbox VM milestone",
                );
                if let Some(operand) = operand {
                    self.check_expr(operand);
                }
            }
            Expr::Clone(operand) => {
                // `clone expr` produces an independent deep copy. The emitter
                // lowers this by evaluating the operand then forcing a deep clone
                // via `local.set` (which calls `cloneValue` in the VM). This
                // correctly models vector aliasing safety: a cloned vector is
                // independent from the original, so mutations to one do not
                // affect the other.
                self.check_expr(operand);
            }
            Expr::Await(operand) => {
                // Only the actor-ask form `await ref.handler(args)` is admitted.
                // The emitter lowers this to `actor.ask`; any other await form is
                // not supported and must be rejected here rather than silently
                // trapping at runtime.
                if !matches!(&operand.0, Expr::MethodCall { .. }) {
                    self.reject(
                        span.clone(),
                        "reserved_runtime_feature",
                        "`await` is only admitted in actor-ask form: `await handle.handler(args...)`; \
                         other await forms are not supported in the browser sandbox",
                    );
                }
                self.check_expr(operand);
            }
            Expr::AwaitRestart(operand) => {
                // `await_restart` parks the actor on the native supervisor restart
                // observer (a cooperative-scheduler primitive); the browser sandbox
                // has no such substrate, so it is a reserved runtime feature.
                self.reject(
                    span.clone(),
                    "reserved_runtime_feature",
                    "`await_restart` suspends on the supervisor restart observer, \
                     which is not supported in the browser sandbox",
                );
                self.check_expr(operand);
            }
            Expr::Tuple(items) | Expr::Array(items) | Expr::Join(items) => {
                for item in items {
                    self.check_expr(item);
                }
            }
            Expr::ArrayRepeat { value, count } => {
                self.check_expr(value);
                self.check_expr(count);
            }
            Expr::MapLiteral { entries } => {
                for (key, value) in entries {
                    self.check_expr(key);
                    self.check_expr(value);
                }
            }
            Expr::Block(block) | Expr::Scope { body: block } | Expr::GenBlock { body: block } => {
                if matches!(expr, Expr::Scope { .. } | Expr::GenBlock { .. }) {
                    self.reject(
                        span.clone(),
                        "reserved_runtime_feature",
                        "structured concurrency and generator blocks are reserved for a later sandbox VM milestone",
                    );
                }
                self.check_block(block);
            }
            Expr::If {
                condition,
                then_block,
                else_block,
            } => {
                self.check_expr(condition);
                self.check_expr(then_block);
                if let Some(expr) = else_block {
                    self.check_expr(expr);
                }
            }
            Expr::IfLet {
                pattern,
                expr,
                body,
                else_body,
            } => {
                self.check_pattern(pattern);
                if !matches!(pattern.0, Pattern::Constructor { .. }) {
                    self.reject(
                        pattern.1.clone(),
                        "reserved_runtime_feature",
                        "non-constructor if-let patterns are reserved until native HIR lowering supports them",
                    );
                }
                self.check_expr(expr);
                self.check_block(body);
                if let Some(block) = else_body {
                    self.check_block(block);
                }
            }
            Expr::Match { scrutinee, arms } => {
                self.check_expr(scrutinee);
                for arm in arms {
                    self.check_pattern(&arm.pattern);
                    if let Some(guard) = &arm.guard {
                        self.check_expr(guard);
                    }
                    self.check_expr(&arm.body);
                }
            }
            Expr::Call { function, args, .. } => {
                self.check_call(function, args, span);
                for arg in args {
                    self.check_expr(arg.expr());
                }
            }
            Expr::MethodCall { receiver, method, args } => {
                self.check_expr(receiver);
                for arg in args {
                    self.check_expr(arg.expr());
                }
                // Security gate: crypto.random_bytes has no secure entropy source in
                // the browser sandbox (the native wasm32 link set omits the CSPRNG).
                // Surface a PlatformLimitation here — the same kind the type checker
                // emits on --target wasm32 — so playground error messages are
                // security-focused rather than generic "unknown method".
                if method == "random_bytes" {
                    if let Expr::Identifier(module) = &receiver.0 {
                        if module == "crypto" {
                            self.reject(
                                span.clone(),
                                "PlatformLimitation",
                                "`crypto.random_bytes` is not available in the browser sandbox: \
                                 secure entropy requires a native entropy source absent from the \
                                 wasm32 link set",
                            );
                            return;
                        }
                    }
                }
                if !self.method_is_admitted(receiver, method) {
                    self.reject(
                        span.clone(),
                        "unknown_method_symbol",
                        format!("method `{method}` is not in the sandbox profile allowlist"),
                    );
                }
            }
            Expr::StructInit { fields, base, .. } => {
                for (_, value) in fields {
                    self.check_expr(value);
                }
                if let Some(base) = base {
                    self.check_expr(base);
                }
            }
            Expr::Spawn { target, args, .. } => {
                // `spawn Actor(field: value)` — admitted; the VM scheduler spawns it.
                self.check_expr(target);
                for (_, value) in args {
                    self.check_expr(value);
                }
            }
            Expr::Select { .. }
            | Expr::SpawnLambdaActor { .. }
            | Expr::ForkChild { .. }
            | Expr::ForkBlock { .. }
            | Expr::ScopeDeadline { .. }
            | Expr::Timeout { .. }
            | Expr::Yield(_)
            | Expr::MachineEmit { .. } => self.reject(
                span.clone(),
                "reserved_runtime_feature",
                "this expression requires scheduler or machine runtime support reserved for a later sandbox VM milestone",
            ),
            Expr::InterpolatedString(parts) => {
                for part in parts {
                    if let hew_parser::ast::StringPart::Expr(expr) = part {
                        self.check_expr(expr);
                    }
                }
            }
            Expr::FieldAccess { object, field } => {
                // Value-position references to native-only stdlib functions must
                // produce a named PlatformLimitation rejection — the same diagnostic
                // the call form emits (Expr::MethodCall above).
                //
                // Guard: a FieldAccess fires on a *module name* only when the
                // type checker did NOT record a type for the object expression.
                // The type checker's check_field_access early-returns for module
                // identifiers without calling synthesize(object), so their spans
                // are absent from expr_types.  For local let-bindings and function
                // parameters (even ones whose name collides with a native-only
                // module short-name such as `net` or `stream`), synthesize(object)
                // IS called and the type IS recorded — so ty_for_expr returns Some
                // and this guard does not fire.  This is the correct scoping that
                // prevents the over-reject bug.
                if let Expr::Identifier(name) = &object.0 {
                    if self.ty_for_expr(object).is_none() {
                        // crypto.random_bytes: native-only secure-entropy source.
                        if name == "crypto" && field == "random_bytes" {
                            self.reject(
                                span.clone(),
                                "PlatformLimitation",
                                "`crypto.random_bytes` is not available in the browser sandbox: \
                                 secure entropy requires a native entropy source absent from the \
                                 wasm32 link set",
                            );
                            return;
                        }
                        // Native-only stdlib modules: any value-position field access
                        // on these module short-names must fail-closed.  Module set is
                        // the single authoritative constant from hew_types — no drift.
                        if hew_types::NATIVE_ONLY_WASM_MODULES.contains(&name.as_str()) {
                            self.reject(
                                span.clone(),
                                "PlatformLimitation",
                                format!(
                                    "`{name}` is a native-only stdlib module that is not \
                                     available in the browser sandbox"
                                ),
                            );
                            return;
                        }
                    }
                }
                self.check_expr(object);
            }
            Expr::Index { object, index } => {
                self.check_expr(object);
                // A range subscript (`v[a..b]`, `v[a..=b]`) is an admitted
                // slice form that the emitter lowers via `vector.range_slice`.
                // Open-ended forms are not lowered, so route them through the
                // ordinary value-position gate and reject them before emission.
                self.check_slice_range_operand(index);
            }
            Expr::Range { .. } => {
                // A range in bare value position (e.g. `let r = a..b;`) has no
                // sandbox lowering — `lower_expr` falls to `emit_unsupported`
                // and traps at runtime. Reject it at the profile gate so the
                // gap fails closed at compile time, matching native's rejection
                // of first-class range values. The two admitted structural
                // positions (for-loop iterables and slice subscripts) route
                // through `check_range_operand` and never reach this arm.
                self.reject(
                    span.clone(),
                    "reserved_runtime_feature",
                    "range values are not admitted in the browser sandbox except as \
                     `for` loop iterables and slice subscripts (`v[a..b]`)",
                );
                if let Expr::Range { start, end, .. } = expr {
                    if let Some(start) = start {
                        self.check_expr(start);
                    }
                    if let Some(end) = end {
                        self.check_expr(end);
                    }
                }
            }
            Expr::UnsafeBlock(block) => {
                self.reject(
                    span.clone(),
                    "unsafe_rejected",
                    "unsafe blocks are rejected by the browser sandbox profile",
                );
                self.check_block(block);
            }
            Expr::ByteStringLiteral(_) | Expr::ByteArrayLiteral(_) | Expr::Lambda { .. } => self.reject(
                span.clone(),
                "reserved_runtime_feature",
                "this value form is not admitted to sandbox bytecode export yet",
            ),
        }
    }

    /// Check an expression that appears as a `for` loop iterable. Range bounds
    /// are structural here, including an omitted start (`for i in ..end`), so
    /// descend into the present bounds without treating the range as a value.
    fn check_range_operand(&mut self, expr: &Spanned<Expr>) {
        match &expr.0 {
            Expr::Binary {
                left,
                op: BinaryOp::Range | BinaryOp::RangeInclusive,
                right,
            } => {
                self.check_expr(left);
                self.check_expr(right);
            }
            Expr::Range { start, end, .. } => {
                if let Some(start) = start {
                    self.check_expr(start);
                }
                if let Some(end) = end {
                    self.check_expr(end);
                }
            }
            _ => self.check_expr(expr),
        }
    }

    /// Check an index expression. The emitter only lowers slice ranges with
    /// both endpoints, so all open-ended forms must remain fail-closed at the
    /// profile gate instead of reaching `emit_unsupported`.
    fn check_slice_range_operand(&mut self, expr: &Spanned<Expr>) {
        match &expr.0 {
            Expr::Binary {
                left,
                op: BinaryOp::Range | BinaryOp::RangeInclusive,
                right,
            } => {
                self.check_expr(left);
                self.check_expr(right);
            }
            Expr::Range {
                start: Some(start),
                end: Some(end),
                ..
            } => {
                self.check_expr(start);
                self.check_expr(end);
            }
            _ => self.check_expr(expr),
        }
    }

    fn check_call(
        &mut self,
        function: &Spanned<Expr>,
        _args: &[CallArg],
        span: &std::ops::Range<usize>,
    ) {
        match &function.0 {
            Expr::Identifier(name) => {
                if self.user_functions.contains(name)
                    || self.enum_variants.contains(name)
                    || matches!(
                        name.as_str(),
                        "print" | "println" | "panic" | "Some" | "None" | "Ok" | "Err" | "Vec::new"
                    )
                {
                    return;
                }
                self.reject(
                    span.clone(),
                    "unknown_symbol",
                    format!("`{name}` is not in the sandbox profile allowlist"),
                );
            }
            Expr::FieldAccess { object, field } => {
                if let Expr::Identifier(module) = &object.0 {
                    let symbol = format!("{module}.{field}");
                    if symbol == "regex.new"
                        || symbol == "Vec.new"
                        || matches!(
                            symbol.as_str(),
                            "regex.is_match" | "regex.find" | "regex.replace"
                        )
                    {
                        return;
                    }
                }
                // `(rec.f)(args)` — fn-field call. The type-checker has admitted
                // this form (ac0bc0ed); admit it here when the callee expression
                // resolves to a function or closure type so the emitter can lower
                // it via `record.get` + `call.indirect`.  Fall through to reject
                // if the type is unavailable (admit-only when statically known).
                if let Some(callee_ty) = self.ty_for_expr(function) {
                    if matches!(callee_ty, Ty::Function { .. } | Ty::Closure { .. }) {
                        return;
                    }
                }
                self.reject(
                    span.clone(),
                    "unknown_stdlib_symbol",
                    "module-qualified call is not in the sandbox profile allowlist",
                );
            }
            _ => {
                // Reject ALL indirect calls regardless of argument count.
                // The prior guard `args.is_empty()` only rejected zero-arg calls,
                // allowing non-trivial indirect calls (`f(a, b)` where `f` is not
                // an Identifier or FieldAccess) to slip past the profile to a
                // generic runtime trap. The security-sensitive profile layer must
                // statically flag every non-admitted call form.
                self.reject(
                    span.clone(),
                    "unknown_indirect_call",
                    "indirect calls are not admitted to sandbox bytecode export yet",
                );
            }
        }
    }

    fn check_pattern(&mut self, pattern: &Spanned<Pattern>) {
        match &pattern.0 {
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::Identifier(_) => {}
            Pattern::Regex { .. } => self.reject(
                pattern.1.clone(),
                "reserved_regex_pattern",
                "regex match-arm patterns need dedicated predicate bytecode lowering and are reserved for a later sandbox VM milestone",
            ),
            Pattern::Constructor { patterns, .. } | Pattern::Tuple(patterns) => {
                for pattern in patterns {
                    self.check_pattern(pattern);
                }
            }
            Pattern::Struct { fields, .. } | Pattern::RecordShorthand { fields, .. } => {
                for field in fields {
                    if let Some(pattern) = &field.pattern {
                        self.check_pattern(pattern);
                    }
                }
            }
            Pattern::Or(left, right) => {
                self.check_pattern(left);
                self.check_pattern(right);
            }
        }
    }

    fn method_is_admitted(&self, receiver: &Spanned<Expr>, method: &str) -> bool {
        if matches!((&receiver.0, method), (Expr::Identifier(module), "new") if module == "regex" || module == "Vec")
        {
            return true;
        }

        // Actor asks: `await ref.handler(...)` where `handler` is a declared
        // receive function AND the receiver is a handle to an admitted actor.
        // Type-check the receiver: an actor handle is `LocalPid<ActorName>`
        // where `ActorName` is in the program's declared actors set.
        // A name-only check without the receiver type would admit spurious method
        // calls on non-actor receivers that happen to share a handler name.
        if self.actor_methods.contains(method) {
            if let Some(receiver_ty) = self.ty_for_expr(receiver) {
                if self.ty_is_actor_handle(&receiver_ty) {
                    return true;
                }
            }
            // If the type is unavailable (e.g. inference not resolved), fall through
            // to reject — the profile must not admit based on name alone.
            return false;
        }

        let Some(receiver_ty) = self.ty_for_expr(receiver) else {
            return false;
        };

        // Machine state queries and event steps on a declared machine handle.
        if matches!(method, "step" | "state_name") {
            if let Ty::Named { name, .. } = receiver_ty.materialize_literal_defaults() {
                if self.machines.contains(&name) {
                    return true;
                }
            }
        }
        match receiver_ty.materialize_literal_defaults() {
            // `clone` on String/Array/Slice/Vec/Regex lowers through the same
            // generic `local.set` → `cloneValue` path as the user-defined-record
            // arm below (`emit.rs`'s `"clone"` arm dispatches on method name
            // alone, not receiver type) — the VM's `cloneValue` has a case for
            // every `VmValue` kind these types resolve to (string, vector,
            // regex). `Option`/`Result` are deliberately excluded: native Hew
            // has no `Option::clone`/`Result::clone` (`hew check` reports
            // `UndefinedMethod`), so no valid program can reach this arm with
            // one — admitting it here would be dead, unreachable surface, not a
            // real capability.
            Ty::String => matches!(method, "len" | "slice" | "to_string" | "clone"),
            Ty::Array(_, _) | Ty::Slice(_) => {
                matches!(method, "len" | "get" | "to_string" | "clone")
            }
            Ty::Named { name, .. } if name == "Vec" => {
                matches!(
                    method,
                    "len" | "push" | "get" | "contains" | "to_string" | "clone"
                )
            }
            Ty::Named { name, .. }
                if name.eq_ignore_ascii_case("Regex")
                    || name.ends_with(".Regex")
                    || name == "regex.Pattern" =>
            {
                // `Pattern` is a `#[resource]`; `close()` is its canonical
                // release method (renamed from `free()` in the resource
                // migration). Mirror the stdlib surface here so the sandbox
                // admits the release call, plus `clone` which the emitter
                // already handles via the generic `local.set` deep-copy path.
                matches!(method, "find" | "is_match" | "replace" | "close" | "clone")
            }
            Ty::Named {
                builtin: Some(BuiltinType::Option),
                ..
            } => matches!(
                method,
                "is_some" | "is_none" | "unwrap" | "unwrap_or" | "to_string"
            ),
            Ty::Named {
                builtin: Some(BuiltinType::Result),
                ..
            } => matches!(
                method,
                "is_ok" | "is_err" | "unwrap" | "unwrap_or" | "to_string"
            ),
            // User-defined records and other named types admit `clone` (deep
            // structural copy via the VM's `local.set` → `cloneValue` path) and
            // `to_string` (the generic `fmt.to_string` stdlib call).
            _ => matches!(method, "clone" | "to_string"),
        }
    }

    fn ty_for_expr(&self, expr: &Spanned<Expr>) -> Option<Ty> {
        self.type_output
            .expr_types
            .get(&SpanKey::from(&expr.1))
            .cloned()
    }

    /// True if `ty` is an actor handle type for a declared actor in this program.
    /// Actor handles are `LocalPid<ActorName>` (the standard actor handle type)
    /// or `Named { name: ActorName }` when the typechecker inlines the actor
    /// type directly. Both are present in practice depending on the call site.
    fn ty_is_actor_handle(&self, ty: &Ty) -> bool {
        match ty.materialize_literal_defaults() {
            // `LocalPid<ActorName>` — the standard form from `spawn Actor(...)`.
            Ty::Named { name, args, .. } if name == "LocalPid" && args.len() == 1 => {
                if let Ty::Named {
                    name: actor_name, ..
                } = &args[0]
                {
                    return self.actors.contains(actor_name);
                }
                false
            }
            // Bare actor type name — seen when the typechecker resolves the handle
            // in scope (e.g. function parameters typed as the actor name).
            Ty::Named { name, .. } => self.actors.contains(&name),
            _ => false,
        }
    }

    fn check_type_expr(&mut self, ty: &TypeExpr, span: &std::ops::Range<usize>) {
        match ty {
            TypeExpr::Pointer { .. } => self.reject_native_only(
                span.clone(),
                NativeOnlySurface::NativeFfi,
                "raw pointer types are native FFI and are rejected by the browser sandbox profile",
            ),
            TypeExpr::Function {
                params,
                return_type,
            } => {
                for (param, param_span) in params {
                    self.check_type_expr(param, param_span);
                }
                self.check_type_expr(&return_type.0, &return_type.1);
            }
            TypeExpr::Result { ok, err } => {
                self.check_type_expr(&ok.0, &ok.1);
                self.check_type_expr(&err.0, &err.1);
            }
            TypeExpr::Option(inner)
            | TypeExpr::Array { element: inner, .. }
            | TypeExpr::Slice(inner)
            | TypeExpr::Borrow(inner) => self.check_type_expr(&inner.0, &inner.1),
            TypeExpr::Tuple(items) => {
                for (item, item_span) in items {
                    self.check_type_expr(item, item_span);
                }
            }
            TypeExpr::Named { .. } | TypeExpr::TraitObject(_) | TypeExpr::Infer => {}
        }
    }

    fn reject(
        &mut self,
        span: std::ops::Range<usize>,
        kind: &'static str,
        message: impl Into<String>,
    ) {
        self.diagnostics
            .push(Diagnostic::profile_error(span, kind, message));
    }

    fn reject_native_only(
        &mut self,
        span: std::ops::Range<usize>,
        _surface: NativeOnlySurface,
        message: impl Into<String>,
    ) {
        let message = message.into();
        self.reject(
            span.clone(),
            Unsupported::NATIVE_ONLY.diagnostic_kind(),
            message.clone(),
        );
        self.reject(span, "sandbox_profile_rejected", message);
    }
}

/// Returns true if a machine transition body is trivial and carries no
/// side-effects: a bare identifier (state-name reference), a block whose
/// only statement is a bare identifier or a single literal, or a literal.
/// Non-trivial bodies are fail-closed-rejected; the runtime ignores them and
/// only uses the declared `to` state from the transition table.
fn is_trivial_machine_transition_body(expr: &Expr) -> bool {
    match expr {
        // A bare `StateName`, `Machine::StateName` path, or a literal.
        Expr::Identifier(_) | Expr::Literal(_) => true,
        // A block `{ state_expr }` where the trailing expression is trivial and
        // there are no statements (no side-effects in the block body).
        Expr::Block(block) => {
            block.stmts.is_empty()
                && block
                    .trailing_expr
                    .as_ref()
                    .is_none_or(|e| is_trivial_machine_transition_body(&e.0))
        }
        _ => false,
    }
}

fn let_pattern_is_unconditional(pattern: &Pattern) -> bool {
    match pattern {
        Pattern::Wildcard | Pattern::Identifier(_) => true,
        Pattern::Tuple(patterns) => patterns
            .iter()
            .all(|pattern| let_pattern_is_unconditional(&pattern.0)),
        Pattern::Struct { fields, .. } | Pattern::RecordShorthand { fields, .. } => {
            fields.iter().all(|field| {
                field
                    .pattern
                    .as_ref()
                    .is_none_or(|pattern| let_pattern_is_unconditional(&pattern.0))
            })
        }
        Pattern::Literal(_)
        | Pattern::Constructor { .. }
        | Pattern::Or(_, _)
        | Pattern::Regex { .. } => false,
    }
}

/// Returns true if the expression is a constant literal that the emitter
/// can bake directly into a bytecode layout (integer, float, string, bool, char).
fn is_literal_expr(expr: &Expr) -> bool {
    matches!(
        expr,
        Expr::Literal(
            hew_parser::ast::Literal::Integer { .. }
                | hew_parser::ast::Literal::Float(_)
                | hew_parser::ast::Literal::String(_)
                | hew_parser::ast::Literal::Bool(_)
                | hew_parser::ast::Literal::Char(_)
        )
    )
}
