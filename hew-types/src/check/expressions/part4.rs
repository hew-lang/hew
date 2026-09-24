//! Split from `expressions.rs`: checker methods, part 4 of 5.
#![allow(
    unused_imports,
    redundant_imports,
    clippy::wildcard_imports,
    reason = "chunk files share the parent module's import header"
)]
use super::super::branch_join::BranchArmExit;
use super::super::coerce::{cast_is_valid, common_integer_type, common_numeric_type};
use super::super::types::GenericLambdaSig;
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::super::*;
use super::*;
use crate::check::types::{
    DeferredIsCheck, EqRequirement, GenericCallEdge, GenericCallee, GenericFnInstantiationSite,
    PendingInstantiation,
};
use crate::env::{PlaceConflict, PlacePath};
use crate::BuiltinType;
use std::collections::VecDeque;

impl Checker {
    /// Discharge concrete comparisons and the generic Eq obligations reachable
    /// through the program's instantiation graph.
    ///
    /// The walk starts at applications whose substitution is concrete in the
    /// caller's terms and follows generic → generic call edges, so an obligation
    /// raised two hops down still lands on the concrete application the
    /// programmer wrote. Every demand uses the same selected Eq authority.
    pub(in crate::check) fn finalize_eq_requirements(&mut self) {
        // WHY a hop budget: polymorphic recursion (`fn f<T>() { g::<Vec<T>>() }`)
        // generates an unbounded instantiation chain. Exceeding it is reported,
        // never skipped — see `generic_structural_eq_depth_error`.
        const MAX_INSTANTIATION_DEPTH: u32 = 64;

        let mut requirements = std::mem::take(&mut self.eq_requirements);
        for requirement in requirements.values_mut().flatten() {
            requirement.ty = self
                .normalize_for_use(&requirement.ty)
                .materialize_literal_defaults();
        }
        let sites = std::mem::take(&mut self.generic_fn_instantiation_sites);
        if requirements.is_empty() {
            return;
        }

        let mut service = TypeFactService::new(self.type_fact_context(), BTreeMap::new());
        let mut new_errors = self.check_concrete_eq_requirements(&requirements, &mut service);
        requirements.retain(|_, demands| {
            demands.retain(|demand| {
                Self::ty_mentions_type_params(&demand.ty, &demand.owner_type_params)
            });
            !demands.is_empty()
        });
        if requirements.is_empty() {
            self.errors.extend(new_errors);
            return;
        }
        let (roots, edges) = self.partition_generic_instantiation_sites(sites);
        let mut seen: HashSet<(String, String, usize, Option<String>)> = HashSet::new();
        let mut work: VecDeque<PendingInstantiation> = roots.into();

        while let Some(pending) = work.pop_front() {
            // Span offsets are module-local, so two modules can produce the same
            // (callee, args, offset) triple for genuinely different sites; the
            // module completes the identity.
            if !seen.insert((
                pending.callee.clone(),
                Self::render_substitution(&pending.substitution),
                pending.report_span.start,
                pending.report_module.clone(),
            )) {
                continue;
            }
            if pending.depth > MAX_INSTANTIATION_DEPTH {
                new_errors.push(Self::generic_structural_eq_depth_error(
                    &pending,
                    MAX_INSTANTIATION_DEPTH,
                ));
                continue;
            }

            for requirement in requirements
                .get(&Some(pending.callee.clone()))
                .into_iter()
                .flatten()
            {
                // Substitute, then collapse any associated-type projection the
                // substitution just made resolvable (`Option<C::Item>` with
                // `C = IntBox` becomes `Option<i64>`). A projection that
                // survives collapse has an unresolved carrier: the instantiation
                // is not decidable here, so do not answer for it.
                let concrete = self.project_assoc_types(
                    &requirement
                        .ty
                        .substitute_named_params_parallel(&pending.substitution),
                );
                if concrete.contains_error()
                    || concrete.has_inference_var()
                    || concrete.contains_assoc_type()
                {
                    continue;
                }
                // A parameter no source pinned leaves the obligation abstract;
                // deciding it here would be guessing.
                if Self::ty_mentions_type_params(&concrete, &requirement.owner_type_params) {
                    continue;
                }
                if !Self::selected_eq_available(
                    &mut service,
                    &concrete.materialize_literal_defaults(),
                ) {
                    new_errors.push(Self::generic_structural_eq_instantiation_error(
                        &requirement.ty,
                        &concrete,
                        &pending,
                    ));
                }
            }

            for edge in edges.get(&pending.callee).into_iter().flatten() {
                let mut chain = pending.chain.clone();
                chain.push(edge.callee.clone());
                work.push_back(PendingInstantiation {
                    callee: edge.callee.clone(),
                    substitution: edge
                        .substitution
                        .iter()
                        .map(|(param, ty)| {
                            (
                                param.clone(),
                                ty.substitute_named_params_parallel(&pending.substitution),
                            )
                        })
                        .collect(),
                    report_span: pending.report_span.clone(),
                    report_module: pending.report_module.clone(),
                    depth: pending.depth + 1,
                    chain,
                });
            }
        }

        self.errors.extend(new_errors);
    }

    /// Type-check an arithmetic operation where at least one operand is `duration` or `instant`.
    ///
    /// Supported operations:
    /// - `duration +/- duration → duration`
    /// - `duration % duration → duration`
    /// - `duration * int → duration`, `int * duration → duration`
    /// - `duration / int → duration`
    /// - `duration / duration → i64` (ratio)
    /// - `instant + duration → instant` (advance a point in time)
    /// - `duration + instant → instant` (commutative advance)
    pub(in crate::check) fn check_duration_arithmetic(
        &mut self,
        op: BinaryOp,
        left: &Ty,
        right: &Ty,
        span: &Span,
    ) -> Ty {
        match (left, right, op) {
            // duration +/- duration → duration, duration % duration → duration
            (Ty::Duration, Ty::Duration, BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Modulo) => {
                Ty::Duration
            }
            // duration * int → duration, int * duration → duration
            (Ty::Duration, r, BinaryOp::Multiply) if r.is_integer() => Ty::Duration,
            (l, Ty::Duration, BinaryOp::Multiply) if l.is_integer() => Ty::Duration,
            // duration / int → duration
            (Ty::Duration, r, BinaryOp::Divide) if r.is_integer() => Ty::Duration,
            // duration / duration → i64 (ratio)
            (Ty::Duration, Ty::Duration, BinaryOp::Divide) => Ty::I64,
            // instant + duration → instant (advance a point in time by a duration)
            (l, Ty::Duration, BinaryOp::Add) if l.is_instant() => left.clone(),
            // duration + instant → instant (commutative: duration + instant)
            (Ty::Duration, r, BinaryOp::Add) if r.is_instant() => right.clone(),
            // instant - duration → instant (rewind a point in time by a duration)
            (l, Ty::Duration, BinaryOp::Subtract) if l.is_instant() => left.clone(),
            // instant - instant → duration (the elapsed gap between two points;
            // both instants canonicalise to i64 nanos, the difference is a
            // signed nanosecond duration). Not commutative — `duration - instant`
            // is meaningless and stays on the error arm below.
            (l, r, BinaryOp::Subtract) if l.is_instant() && r.is_instant() => Ty::Duration,
            _ => {
                self.report_error(
                    TypeErrorKind::InvalidOperation,
                    span,
                    format!(
                        "cannot apply `{op}` to `{}` and `{}`",
                        left.user_facing(),
                        right.user_facing()
                    ),
                );
                Ty::Error
            }
        }
    }

    pub(in crate::check) fn require_unsafe(&mut self, name: &str, span: &Span) {
        // rc1-F1 stage B: `unsafe` gating is derived from the extern table's
        // declaration index — a call requires `unsafe` exactly when its
        // resolved declaration key names a registered extern declaration.
        // The canonical-owner probe covers root extern declarations, which
        // key `{root_module}.{name}` inside the checker while root call
        // sites spell the bare leaf.
        let scoped_unsafe = scoped_module_item_name(self.canonical_fn_owner(), name)
            .is_some_and(|qualified| self.extern_table.requires_unsafe(&qualified));
        if !self.in_unsafe && (scoped_unsafe || self.extern_table.requires_unsafe(name)) {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!("calling extern function `{name}` requires `unsafe {{ ... }}`"),
            );
        }
    }

    /// Reject escaping a borrowed affine-handle parameter without `.clone()`.
    /// Under borrow-on-call semantics the callee does not own its Rc/Weak
    /// parameters, so returning or embedding one would mint an owner without
    /// incrementing the matching reference count.
    pub(in crate::check) fn warn_affine_param_escape(&mut self, fd: &FnDecl) {
        // Collect dangerous params: those with explicit Rc<_>/Weak<_> types.
        //
        // NOTE: generic type params (e.g. `x: T`) are NOT flagged here because
        // the danger only materialises when `T` is instantiated with `Rc<U>` at
        // a call site.  Definition-site checking would reject all generic
        // identity patterns (`fn id<T>(x: T) -> T { x }`) which are safe for
        // non-Rc types.  Call-site / monomorphisation-time checking is deferred
        // to a future slice.
        let dangerous_params: DangerousRcScope = fd
            .params
            .iter()
            .filter_map(|p| {
                let ty = self.resolve_type_expr(&p.ty);
                if matches!(
                    ty,
                    Ty::Named {
                        builtin: Some(BuiltinType::Rc | BuiltinType::Weak),
                        ..
                    }
                ) {
                    return Some((p.name.clone(), Some(p.name.clone())));
                }
                None
            })
            .collect();
        if dangerous_params.is_empty() {
            return;
        }
        let mut scopes = vec![dangerous_params];
        self.scan_block_for_rc_param_return(&fd.body, &mut scopes);
    }

    /// Reject consuming a non-Copy by-value parameter into Rc-owned storage.
    /// A parameter is a borrow at the Hew call boundary; only an explicit
    /// clone or a freshly constructed value is an owned source.
    pub(in crate::check) fn reject_borrowed_parameter_consumption(
        &mut self,
        expr: &Expr,
        span: &Span,
        operation: &str,
    ) {
        let Expr::Identifier(name) = expr else {
            return;
        };
        let Some(binding) = self.env.lookup_ref(name) else {
            return;
        };
        let is_parameter = binding.is_param();
        let ty = self.subst.resolve(&binding.ty);
        if !is_parameter || self.registry.implements_marker(&ty, MarkerTrait::Copy) {
            return;
        }
        self.errors.push(TypeError {
            severity: crate::error::Severity::Error,
            kind: TypeErrorKind::BorrowedParamReturn,
            span: span.clone(),
            message: format!(
                "`{operation}` cannot consume borrowed parameter `{name}` of type `{}`",
                ty.user_facing()
            ),
            notes: vec![(
                span.clone(),
                "by-value function parameters are borrowed; the caller retains ownership"
                    .to_string(),
                self.current_module.clone(),
            )],
            suggestions: vec![format!(
                "use `{name}.clone()` to materialize an owned replacement"
            )],
            source_module: self.current_module.clone(),
        });
    }

    /// If `expr` is a bare identifier matching one of the visible dangerous Rc
    /// bindings (or a block expression whose trailing expression is), emit a
    /// fail-closed error.
    pub(in crate::check) fn check_expr_is_rc_param_return(
        &mut self,
        expr: &Expr,
        span: &Span,
        scopes: &[DangerousRcScope],
    ) {
        match expr {
            Expr::Coalesce { right, .. } => {
                self.check_expr_is_rc_param_return(&right.0, &right.1, scopes);
            }
            Expr::Handle { error, body, .. } => {
                let mut handler_scopes = scopes.to_vec();
                handler_scopes.push(HashMap::from([(error.0.clone(), None)]));
                self.check_expr_is_rc_param_return(&body.0, &body.1, &handler_scopes);
            }
            Expr::Identifier(name) => {
                if let Some(source_param) = Self::lookup_dangerous_binding(name, scopes) {
                    self.emit_borrowed_param_return(name, &source_param, span);
                }
            }
            // Descend into block expressions: `{ r }` or `unsafe { r }` wraps
            // the identifier in a block whose local bindings may also shadow
            // params.
            Expr::Block(blk) => {
                let mut nested_scopes = scopes.to_vec();
                self.scan_block_for_rc_param_return(blk, &mut nested_scopes);
            }
            Expr::UnsafeBlock(blk) => {
                let mut nested_scopes = scopes.to_vec();
                self.scan_block_for_rc_param_return(blk, &mut nested_scopes);
            }
            // Aggregate escapes: enum-variant constructors like Some(r), Ok(r),
            // Err(r) embed an Rc param in a container, transferring the borrowed
            // pointer without a clone.  Only check calls whose callee constructs
            // an aggregate (variant constructor or `Type::assoc` path) — regular
            // function calls are borrows under call-boundary ownership and safe.
            Expr::Call { function, args, .. }
                if self.callee_is_aggregate_constructor(&function.0) =>
            {
                if matches!(
                    &function.0,
                    Expr::Identifier(name)
                        if crate::runtime_call::RuntimeCallFamily::from_checker_signature(name)
                            == Some(crate::runtime_call::RuntimeCallFamily::RcNew)
                ) {
                    return;
                }
                for arg in args {
                    let (e, s) = arg.expr();
                    self.check_expr_is_rc_param_return(e, s, scopes);
                }
            }
            Expr::MethodCall {
                receiver,
                method,
                args,
            } if self
                .dotted_static_aggregate_identity(&receiver.0, method)
                .is_some() =>
            {
                let identity = self
                    .dotted_static_aggregate_identity(&receiver.0, method)
                    .expect("guarded by a successful static aggregate lookup");
                if crate::runtime_call::RuntimeCallFamily::from_checker_signature(&identity)
                    == Some(crate::runtime_call::RuntimeCallFamily::RcNew)
                {
                    return;
                }
                for arg in args {
                    let (e, s) = arg.expr();
                    self.check_expr_is_rc_param_return(e, s, scopes);
                }
            }
            // Tuple literals: (r, 0), (r,) embed the borrowed Rc param.
            Expr::Tuple(elems) => {
                for (e, s) in elems {
                    self.check_expr_is_rc_param_return(e, s, scopes);
                }
            }
            // Struct initializers: MyStruct { field: r } embeds the borrowed Rc param.
            Expr::StructInit { fields, .. } => {
                for (_field_name, (e, s)) in fields {
                    self.check_expr_is_rc_param_return(e, s, scopes);
                }
            }
            // Promoted tail-position if/if-let/match: each branch can return an Rc
            // param.  Scan each arm's body the same way scan_stmts_for_rc_param_return
            // does for the Stmt::If / Stmt::IfLet / Stmt::Match variants.
            Expr::If {
                then_block,
                else_block,
                ..
            } => {
                // then_block is Box<Spanned<Expr>> wrapping Expr::Block
                self.check_expr_is_rc_param_return(&then_block.0, &then_block.1, scopes);
                if let Some(else_expr) = else_block {
                    self.check_expr_is_rc_param_return(&else_expr.0, &else_expr.1, scopes);
                }
            }
            Expr::IfLet {
                conditions,
                body,
                else_body,
            } => {
                let mut then_scopes = scopes.to_vec();
                self.shadow_condition_bindings(conditions, &mut then_scopes);
                self.scan_block_for_rc_param_return(body, &mut then_scopes);
                if let Some(else_expr) = else_body {
                    self.check_expr_is_rc_param_return(&else_expr.0, &else_expr.1, scopes);
                }
            }
            Expr::Match { arms, .. } => {
                for arm in arms {
                    let mut arm_scopes = scopes.to_vec();
                    self.shadow_pattern_bindings(&arm.pattern.1, &mut arm_scopes);
                    self.check_expr_is_rc_param_return(&arm.body.0, &arm.body.1, &arm_scopes);
                }
            }
            _ => {}
        }
    }

    /// Returns `true` when a call's callee constructs an aggregate that may embed
    /// the argument into its result — an enum/struct variant constructor, or a
    /// `Type::assoc` path such as `Rc::new`.  Such a result, when returned,
    /// aliases the embedded argument, so the borrowed-param escape analysis must
    /// descend into the call's arguments.  Regular function calls pass arguments
    /// as borrows under call-boundary ownership (the callee owns the escape
    /// question), so they are NOT descended into.
    ///
    /// Classification is resolution-based, not casing-based (#2116): a bare
    /// identifier is a constructor iff it is a builtin `Option`/`Result` variant
    /// or resolves via `lookup_variant_constructor`.  This fixes two casing bugs:
    /// an uppercase regular function (`Helper(r)`) is no longer a false hit
    /// (spurious `BorrowedParamReturn`), and a lowercase user variant (`wrap(r)`)
    /// is no longer a false miss (a real aggregate escape that the old uppercase
    /// heuristic silently dropped).
    /// Whether `owner` is a machine whose own generated body is being
    /// checked, the one place its state names are written bare.
    pub(super) fn machine_state_is_bare_here(&self, owner: &str) -> bool {
        let is_machine = self.lookup_declaration(owner).is_some_and(|def| {
            self.identity.declaration_kind_by_path(def.full_path())
                == Some(crate::DeclarationKind::Machine)
        });
        is_machine
            && self.machine_body_owner.as_deref().is_some_and(|current| {
                super::calls::variant_owner_spelling(current)
                    == super::calls::variant_owner_spelling(owner)
            })
    }

    pub(in crate::check) fn callee_is_aggregate_constructor(&self, function: &Expr) -> bool {
        let name = match function {
            Expr::Identifier(name) => name,
            // A contextual variant (`.Some(r)`, `.Wrap(r)`) always constructs
            // and embeds its payload.
            Expr::ContextVariant(_) => return true,
            // Calling a function-valued field or closure (`(obj.f)(arg)`) passes
            // the argument as a borrow; it is never an aggregate constructor.
            _ => return false,
        };
        // `Type::assoc` / `E::Variant` paths construct or wrap a value and may
        // embed the argument (`Rc::new(r)`, `MyEnum::Variant(r)`).  Fail-closed:
        // descend on every qualified call so an aggregate escape is never missed.
        if name.contains("::") {
            return true;
        }
        // User enum / struct tuple-variant constructors, resolved by name
        // (any casing) rather than an uppercase-first heuristic.
        self.lookup_variant_constructor(name).is_some()
    }

    /// Return the checker identity for a dotted static call whose receiver is
    /// a declared type, rather than a value binding. Dotted variant/static
    /// calls parse as `MethodCall`, so the escape scanner must classify this
    /// shape alongside the internal qualified `Call` representation.
    pub(super) fn dotted_static_aggregate_identity(
        &self,
        receiver: &Expr,
        method: &str,
    ) -> Option<String> {
        let Expr::Identifier(owner) = receiver else {
            return None;
        };
        if self.env.lookup_ref(owner).is_some() {
            return None;
        }
        let identity = format!("{owner}::{method}");
        (self.lookup_variant_constructor(&identity).is_some()
            || self.source_nominal_declaration(owner).is_some()
            || crate::lookup_builtin_type(owner).is_some())
        .then_some(identity)
    }

    /// Return the nearest visible dangerous Rc binding for `name`.
    pub(super) fn lookup_dangerous_binding(
        name: &str,
        scopes: &[DangerousRcScope],
    ) -> Option<DangerousRcBinding> {
        for scope in scopes.iter().rev() {
            if let Some(binding) = scope.get(name) {
                return binding.clone();
            }
        }
        None
    }

    pub(super) fn current_dangerous_scope_mut(
        scopes: &mut [DangerousRcScope],
    ) -> &mut DangerousRcScope {
        scopes
            .last_mut()
            .expect("borrowed Rc tracking always maintains at least one scope")
    }

    pub(super) fn define_dangerous_binding(
        scopes: &mut [DangerousRcScope],
        name: String,
        binding: Option<DangerousRcBinding>,
    ) {
        Self::current_dangerous_scope_mut(scopes).insert(name, binding);
    }

    pub(super) fn update_dangerous_binding(
        scopes: &mut [DangerousRcScope],
        name: &str,
        binding: Option<DangerousRcBinding>,
    ) {
        for scope in scopes.iter_mut().rev() {
            if let Some(existing) = scope.get_mut(name) {
                *existing = binding;
                return;
            }
        }
        Self::define_dangerous_binding(scopes, name.to_string(), binding);
    }

    /// Shadow the borrowed-Rc bindings introduced by a match-arm / if-let /
    /// loop / destructuring pattern.
    ///
    /// Routes through the single binder authority instead of re-deriving the
    /// binder-vs-constructor decision locally (#2116): `bind_pattern` already
    /// recorded the exact set of names this pattern binds — its env delta —
    /// keyed by the pattern span in `pattern_bound_names`. We shadow precisely
    /// those names. A constructor identifier (e.g. `Red` in `Red | Green`, or a
    /// bare unit-variant arm) binds nothing, so it is absent from the set and
    /// does NOT shadow a dangerous param of the same name — keeping the escape
    /// analysis consistent with the real binding env. A pattern that bound
    /// nothing (or was never type-checked, e.g. a cascade off an earlier error)
    /// has no entry and shadows nothing, which is the fail-closed direction:
    /// the scanner then still sees the dangerous param and flags a genuine
    /// escape rather than silently masking it.
    /// Shadow every name a pattern condition binds (§12.5): one condition can
    /// carry several `let` operands, and all of their binders are live in the
    /// then block.
    pub(super) fn shadow_condition_bindings(
        &self,
        conditions: &[ConditionItem],
        scopes: &mut [DangerousRcScope],
    ) {
        for item in conditions {
            if let ConditionItem::Let { pattern, .. } = item {
                self.shadow_pattern_bindings(&pattern.1, scopes);
            }
        }
    }

    pub(super) fn shadow_pattern_bindings(
        &self,
        pattern_span: &Span,
        scopes: &mut [DangerousRcScope],
    ) {
        let key = super::types::SpanKey::in_module(pattern_span, self.current_module_idx);
        if let Some(names) = self.pattern_bound_names.get(&key) {
            for name in names {
                Self::define_dangerous_binding(scopes, name.clone(), None);
            }
        }
    }

    pub(super) fn emit_borrowed_param_return(
        &mut self,
        name: &str,
        source_param: &str,
        span: &Span,
    ) {
        let (message, note, suggestion) = if name == source_param {
            (
                format!(
                    "returning affine handle parameter `{name}` transfers a borrowed \
                     reference without cloning — both caller and callee result would \
                     release the same handle"
                ),
                "function parameters are borrowed under call-boundary ownership; \
                 the caller retains ownership and drops at scope exit"
                    .to_string(),
                format!(
                    "use `{name}.clone()` to create an owned copy with an incremented refcount"
                ),
            )
        } else {
            (
                format!(
                    "returning local `{name}` which contains borrowed parameter \
                     `{source_param}` — the parameter was stored without cloning, \
                     causing a double-free when both the caller's local and the \
                     return value are dropped"
                ),
                format!(
                    "parameter `{source_param}` is borrowed under call-boundary \
                     ownership; storing it in `{name}` does not transfer ownership"
                ),
                format!("clone the parameter before storing: `{source_param}.clone()`"),
            )
        };
        self.errors.push(TypeError {
            severity: crate::error::Severity::Error,
            kind: TypeErrorKind::BorrowedParamReturn,
            span: span.clone(),
            message,
            notes: vec![(span.clone(), note, self.current_module.clone())],
            suggestions: vec![suggestion],
            source_module: self.current_module.clone(),
        });
    }

    /// Check if `expr` directly names or structurally contains a visible
    /// dangerous Rc binding. Returns the first match or `None`.
    ///
    /// Structural descent mirrors `check_expr_is_rc_param_return`: aggregate
    /// constructors (variant constructors and `Type::assoc` paths, classified by
    /// resolution — see `callee_is_aggregate_constructor`), tuples, struct inits,
    /// and blocks are containers that embed the value.  Regular function/method
    /// calls are borrows under call-boundary ownership — the return value is
    /// unrelated, so we do NOT recurse into those.
    pub(in crate::check) fn dangerous_source_in_expr(
        &mut self,
        expr: &Expr,
        scopes: &[DangerousRcScope],
    ) -> Option<DangerousRcBinding> {
        match expr {
            Expr::Identifier(name) => Self::lookup_dangerous_binding(name, scopes),
            Expr::Call { function, args, .. }
                if self.callee_is_aggregate_constructor(&function.0) =>
            {
                if matches!(
                    &function.0,
                    Expr::Identifier(name)
                        if crate::runtime_call::RuntimeCallFamily::from_checker_signature(name)
                            == Some(crate::runtime_call::RuntimeCallFamily::RcNew)
                ) {
                    return None;
                }
                for arg in args {
                    let (e, _) = arg.expr();
                    if let Some(hit) = self.dangerous_source_in_expr(e, scopes) {
                        return Some(hit);
                    }
                }
                None
            }
            Expr::MethodCall {
                receiver,
                method,
                args,
            } if self
                .dotted_static_aggregate_identity(&receiver.0, method)
                .is_some() =>
            {
                let identity = self
                    .dotted_static_aggregate_identity(&receiver.0, method)
                    .expect("guarded by a successful static aggregate lookup");
                if crate::runtime_call::RuntimeCallFamily::from_checker_signature(&identity)
                    == Some(crate::runtime_call::RuntimeCallFamily::RcNew)
                {
                    return None;
                }
                for arg in args {
                    let (e, _) = arg.expr();
                    if let Some(hit) = self.dangerous_source_in_expr(e, scopes) {
                        return Some(hit);
                    }
                }
                None
            }
            Expr::Tuple(elems) => {
                for (e, _) in elems {
                    if let Some(hit) = self.dangerous_source_in_expr(e, scopes) {
                        return Some(hit);
                    }
                }
                None
            }
            Expr::StructInit { fields, .. } => {
                for (_, (e, _)) in fields {
                    if let Some(hit) = self.dangerous_source_in_expr(e, scopes) {
                        return Some(hit);
                    }
                }
                None
            }
            Expr::Block(_) | Expr::UnsafeBlock(_) => {
                let blk: &Block = match expr {
                    Expr::UnsafeBlock(blk) => blk,
                    Expr::Block(blk) => blk,
                    _ => unreachable!("guarded by the arm pattern"),
                };
                let mut nested_scopes = scopes.to_vec();
                nested_scopes.push(HashMap::new());
                self.scan_stmts_for_rc_param_return(&blk.stmts, &mut nested_scopes);
                blk.trailing_expr
                    .as_deref()
                    .and_then(|(e, _)| self.dangerous_source_in_expr(e, &nested_scopes))
            }
            _ => None,
        }
    }

    pub(in crate::check) fn scan_block_for_rc_param_return(
        &mut self,
        block: &Block,
        scopes: &mut Vec<DangerousRcScope>,
    ) {
        scopes.push(HashMap::new());
        self.scan_stmts_for_rc_param_return(&block.stmts, scopes);
        if let Some(trailing) = &block.trailing_expr {
            self.check_expr_is_rc_param_return(&trailing.0, &trailing.1, scopes);
        }
        scopes.pop();
    }

    /// Recursively scan statements for `return <rc_param_ident>`,
    /// `break <rc_param_ident>`, and nested control-flow bodies.
    #[expect(
        clippy::too_many_lines,
        reason = "borrowed Rc escape scanning covers many statement forms"
    )]
    pub(in crate::check) fn scan_stmts_for_rc_param_return(
        &mut self,
        stmts: &[Spanned<Stmt>],
        scopes: &mut Vec<DangerousRcScope>,
    ) {
        for (stmt, _span) in stmts {
            match stmt {
                Stmt::Let { pattern, value, .. } => {
                    let binding = value
                        .as_ref()
                        .and_then(|(expr, _)| self.dangerous_source_in_expr(expr, scopes));
                    match &pattern.0 {
                        // A let-position identifier that resolves to a unit
                        // variant is a refutable tag-test (`let None = opt else
                        // { … }`, `let red = color else { … }`) that binds
                        // NOTHING — the checker skips `bind_pattern` for it
                        // (statements.rs). Consult that SAME authority so the
                        // escape scanner does not invent a dangerous-scope
                        // shadow for such a name, which would otherwise mask an
                        // outer borrowed param of the same name and miss a real
                        // escape. A genuine binder still propagates the RHS
                        // danger (and shadows an outer dangerous param when the
                        // RHS is safe).
                        Pattern::Identifier(name) if !self.let_identifier_is_unit_variant(name) => {
                            Self::define_dangerous_binding(scopes, name.clone(), binding);
                        }
                        Pattern::Identifier(_) => {
                            // Unit-variant tag-test: binds nothing, shadows nothing.
                        }
                        _ => {
                            self.shadow_pattern_bindings(&pattern.1, scopes);
                        }
                    }
                }
                Stmt::Var { name, value, .. } => {
                    let binding = value
                        .as_ref()
                        .and_then(|(expr, _)| self.dangerous_source_in_expr(expr, scopes));
                    Self::define_dangerous_binding(scopes, name.clone(), binding);
                }
                Stmt::Assign {
                    target: (Expr::Identifier(name), _),
                    value: (expr, _),
                    ..
                } => {
                    let binding = self.dangerous_source_in_expr(expr, scopes);
                    Self::update_dangerous_binding(scopes, name, binding);
                }
                Stmt::Assign {
                    target: (Expr::FieldAccess { object, .. }, _),
                    value: (expr, _),
                    ..
                } => {
                    if let Expr::Identifier(obj_name) = &object.0 {
                        if let Some(binding) = self.dangerous_source_in_expr(expr, scopes) {
                            Self::update_dangerous_binding(scopes, obj_name, Some(binding));
                        }
                    }
                }
                Stmt::Return(Some((expr, es)))
                | Stmt::Break {
                    value: Some((expr, es)),
                    ..
                } => {
                    self.check_expr_is_rc_param_return(expr, es, scopes);
                }
                Stmt::Expression((
                    Expr::MethodCall {
                        receiver,
                        method,
                        args,
                        ..
                    },
                    _,
                )) => {
                    const STORING_METHODS: &[&str] = &["push", "set", "insert", "append"];
                    if STORING_METHODS.contains(&method.as_str()) {
                        if let Expr::Identifier(recv_name) = &receiver.0 {
                            for arg in args {
                                let (expr, _) = arg.expr();
                                if let Some(binding) = self.dangerous_source_in_expr(expr, scopes) {
                                    Self::update_dangerous_binding(
                                        scopes,
                                        recv_name,
                                        Some(binding),
                                    );
                                    break;
                                }
                            }
                        }
                    }
                }
                Stmt::Expression((Expr::Block(block), _)) => {
                    self.scan_block_for_rc_param_return(block, scopes);
                }
                Stmt::If {
                    then_block,
                    else_block,
                    ..
                } => {
                    self.scan_block_for_rc_param_return(then_block, scopes);
                    if let Some(else_blk) = else_block {
                        if let Some(if_stmt) = &else_blk.if_stmt {
                            // else-if: recurse into the nested Stmt::If
                            self.scan_stmts_for_rc_param_return(
                                std::slice::from_ref(if_stmt.as_ref()),
                                scopes,
                            );
                        }
                        if let Some(blk) = &else_blk.block {
                            self.scan_block_for_rc_param_return(blk, scopes);
                        }
                    }
                }
                Stmt::For { pattern, body, .. } => {
                    scopes.push(HashMap::new());
                    self.shadow_pattern_bindings(&pattern.1, scopes);
                    self.scan_stmts_for_rc_param_return(&body.stmts, scopes);
                    if let Some(trailing) = &body.trailing_expr {
                        self.check_expr_is_rc_param_return(&trailing.0, &trailing.1, scopes);
                    }
                    scopes.pop();
                }
                Stmt::Loop { body, .. } | Stmt::While { body, .. } => {
                    self.scan_block_for_rc_param_return(body, scopes);
                }
                Stmt::WhileLet {
                    conditions, body, ..
                } => {
                    scopes.push(HashMap::new());
                    self.shadow_condition_bindings(conditions, scopes);
                    self.scan_stmts_for_rc_param_return(&body.stmts, scopes);
                    if let Some(trailing) = &body.trailing_expr {
                        self.check_expr_is_rc_param_return(&trailing.0, &trailing.1, scopes);
                    }
                    scopes.pop();
                }
                Stmt::IfLet {
                    conditions,
                    body,
                    else_body,
                } => {
                    scopes.push(HashMap::new());
                    self.shadow_condition_bindings(conditions, scopes);
                    self.scan_stmts_for_rc_param_return(&body.stmts, scopes);
                    if let Some(then_trailing) = &body.trailing_expr {
                        self.check_expr_is_rc_param_return(
                            &then_trailing.0,
                            &then_trailing.1,
                            scopes,
                        );
                    }
                    scopes.pop();
                    if let Some(else_expr) = else_body {
                        self.check_expr_is_rc_param_return(&else_expr.0, &else_expr.1, scopes);
                    }
                }
                Stmt::Match { arms, .. } => {
                    for arm in arms {
                        scopes.push(HashMap::new());
                        self.shadow_pattern_bindings(&arm.pattern.1, scopes);
                        // Match arm body is an Expr — check if it's a bare Rc param
                        self.check_expr_is_rc_param_return(&arm.body.0, &arm.body.1, scopes);
                        scopes.pop();
                    }
                }
                _ => {}
            }
        }
    }

    pub(in crate::check) fn reject_owned_handle_field_accessors(&mut self, fd: &FnDecl) {
        let Some((type_name, _)) = self.current_self_type.clone() else {
            return;
        };
        if !self.struct_is_handle_bearing(&type_name) {
            return;
        }
        let Some(receiver_name) = fd
            .params
            .first()
            .filter(|param| self.is_receiver_param(param))
            .map(|param| param.name.clone())
        else {
            return;
        };

        // `bindings` maps let-binding variable names to the (field_name,
        // handle_type_name) they alias, so `return p` after `let p = self.field`
        // fires the same diagnostic as a direct `return self.field`.
        let mut bindings: HashMap<String, (String, String)> = HashMap::new();
        self.scan_block_for_owned_handle_field_return(
            &fd.body,
            &receiver_name,
            &type_name,
            &fd.name,
            &mut bindings,
        );
    }

    pub(super) fn struct_is_handle_bearing(&mut self, type_name: &str) -> bool {
        self.ensure_handle_bearing_fresh();
        self.handle_bearing_structs.contains(type_name)
            || self
                .registered_type_def_name(type_name)
                .is_some_and(|name| self.handle_bearing_structs.contains(&name))
            || self
                .strip_module_prefix(type_name)
                .is_some_and(|name| self.handle_bearing_structs.contains(name))
    }

    pub(super) fn scan_block_for_owned_handle_field_return(
        &mut self,
        block: &Block,
        receiver_name: &str,
        type_name: &str,
        method_name: &str,
        bindings: &mut HashMap<String, (String, String)>,
    ) {
        self.scan_stmts_for_owned_handle_field_return(
            &block.stmts,
            receiver_name,
            type_name,
            method_name,
            bindings,
        );
        if let Some(trailing) = &block.trailing_expr {
            self.check_expr_for_owned_handle_field_return(
                &trailing.0,
                &trailing.1,
                receiver_name,
                type_name,
                method_name,
                bindings,
            );
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "owned-handle bind-then-return scanning covers many statement forms"
    )]
    pub(super) fn scan_stmts_for_owned_handle_field_return(
        &mut self,
        stmts: &[Spanned<Stmt>],
        receiver_name: &str,
        type_name: &str,
        method_name: &str,
        bindings: &mut HashMap<String, (String, String)>,
    ) {
        for (stmt, _) in stmts {
            match stmt {
                // Track `let p = receiver.field` when `field` is an owned
                // handle — a subsequent `return p` is the same double-free risk
                // as `return receiver.field` directly. A unit-variant let-else
                // (`let None = receiver.field else { … }`) binds nothing, so it
                // is excluded via the shared binder authority — it must not
                // register a phantom owned-handle binding.
                Stmt::Let {
                    pattern: (Pattern::Identifier(var_name), _),
                    value: Some((Expr::FieldAccess { object, field }, _)),
                    ..
                } if matches!(&object.0, Expr::Identifier(n) if n == receiver_name)
                    && !self.let_identifier_is_unit_variant(var_name) =>
                {
                    if let Some((field_name, handle_name)) =
                        self.owned_handle_field_return_by_name(field, type_name)
                    {
                        bindings.insert(var_name.clone(), (field_name, handle_name));
                    }
                }
                Stmt::Return(Some((expr, span))) => self.check_expr_for_owned_handle_field_return(
                    expr,
                    span,
                    receiver_name,
                    type_name,
                    method_name,
                    bindings,
                ),
                Stmt::Expression((Expr::Block(block), _))
                | Stmt::Loop { body: block, .. }
                | Stmt::While { body: block, .. }
                | Stmt::For { body: block, .. }
                | Stmt::WhileLet { body: block, .. } => self
                    .scan_block_for_owned_handle_field_return(
                        block,
                        receiver_name,
                        type_name,
                        method_name,
                        bindings,
                    ),
                Stmt::If {
                    then_block,
                    else_block,
                    ..
                } => {
                    self.scan_block_for_owned_handle_field_return(
                        then_block,
                        receiver_name,
                        type_name,
                        method_name,
                        bindings,
                    );
                    if let Some(else_block) = else_block {
                        if let Some(if_stmt) = &else_block.if_stmt {
                            self.scan_stmts_for_owned_handle_field_return(
                                std::slice::from_ref(if_stmt.as_ref()),
                                receiver_name,
                                type_name,
                                method_name,
                                bindings,
                            );
                        }
                        if let Some(block) = &else_block.block {
                            self.scan_block_for_owned_handle_field_return(
                                block,
                                receiver_name,
                                type_name,
                                method_name,
                                bindings,
                            );
                        }
                    }
                }
                Stmt::IfLet {
                    body, else_body, ..
                } => {
                    self.scan_block_for_owned_handle_field_return(
                        body,
                        receiver_name,
                        type_name,
                        method_name,
                        bindings,
                    );
                    if let Some(else_expr) = else_body {
                        self.check_expr_for_owned_handle_field_return(
                            &else_expr.0,
                            &else_expr.1,
                            receiver_name,
                            type_name,
                            method_name,
                            bindings,
                        );
                    }
                }
                Stmt::Match { arms, .. } => {
                    for arm in arms {
                        self.check_expr_for_owned_handle_field_return(
                            &arm.body.0,
                            &arm.body.1,
                            receiver_name,
                            type_name,
                            method_name,
                            bindings,
                        );
                    }
                }
                Stmt::Let { .. }
                | Stmt::Var { .. }
                | Stmt::Assign { .. }
                | Stmt::Break { .. }
                | Stmt::Continue { .. }
                | Stmt::Return(None)
                | Stmt::Expression(_)
                | Stmt::Defer(_) => {}
            }
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "owned-handle return-position scanning covers many expression forms"
    )]
    pub(super) fn check_expr_for_owned_handle_field_return(
        &mut self,
        expr: &Expr,
        span: &Span,
        receiver_name: &str,
        type_name: &str,
        method_name: &str,
        bindings: &mut HashMap<String, (String, String)>,
    ) {
        // Direct `return receiver.field` — the original check.
        if let Some((field_name, handle_name)) =
            self.owned_handle_field_return(expr, receiver_name, type_name)
        {
            self.report_owned_handle_field_return(
                span,
                method_name,
                type_name,
                &field_name,
                &handle_name,
                None,
            );
            return;
        }

        // Bind-then-return: `let p = receiver.field; … return p`
        // The alias `p` is still an unprotected return path — same double-free
        // risk as returning the field directly. The check is conservative: if
        // `p` has been observed as an alias of any owned handle field in this
        // method body, we always flag it, even if there are intermediate uses.
        if let Expr::Identifier(var_name) = expr {
            if let Some((field_name, handle_name)) = bindings.get(var_name).cloned() {
                self.report_owned_handle_field_return(
                    span,
                    method_name,
                    type_name,
                    &field_name,
                    &handle_name,
                    Some(var_name),
                );
                return;
            }
        }

        match expr {
            Expr::Block(block) => self.scan_block_for_owned_handle_field_return(
                block,
                receiver_name,
                type_name,
                method_name,
                bindings,
            ),
            Expr::If {
                then_block,
                else_block,
                ..
            } => {
                self.check_expr_for_owned_handle_field_return(
                    &then_block.0,
                    &then_block.1,
                    receiver_name,
                    type_name,
                    method_name,
                    bindings,
                );
                if let Some(else_expr) = else_block {
                    self.check_expr_for_owned_handle_field_return(
                        &else_expr.0,
                        &else_expr.1,
                        receiver_name,
                        type_name,
                        method_name,
                        bindings,
                    );
                }
            }
            Expr::IfLet {
                body, else_body, ..
            } => {
                self.scan_block_for_owned_handle_field_return(
                    body,
                    receiver_name,
                    type_name,
                    method_name,
                    bindings,
                );
                if let Some(else_expr) = else_body {
                    self.check_expr_for_owned_handle_field_return(
                        &else_expr.0,
                        &else_expr.1,
                        receiver_name,
                        type_name,
                        method_name,
                        bindings,
                    );
                }
            }
            Expr::Match { arms, .. } => {
                for arm in arms {
                    self.check_expr_for_owned_handle_field_return(
                        &arm.body.0,
                        &arm.body.1,
                        receiver_name,
                        type_name,
                        method_name,
                        bindings,
                    );
                }
            }
            Expr::Coalesce { right: body, .. } | Expr::Handle { body, .. } => {
                let mut branch_bindings = bindings.clone();
                if let Expr::Handle { error, .. } = expr {
                    branch_bindings.remove(&error.0);
                }
                self.check_expr_for_owned_handle_field_return(
                    &body.0,
                    &body.1,
                    receiver_name,
                    type_name,
                    method_name,
                    &mut branch_bindings,
                );
            }
            Expr::Binary { .. }
            | Expr::Unary { .. }
            | Expr::Clone(_)
            | Expr::Literal(_)
            | Expr::Identifier(_)
            | Expr::ContextVariant(_)
            | Expr::GenericApplySuffix { .. }
            | Expr::RecordInitSuffix { .. }
            | Expr::QualifiedAssoc(_)
            | Expr::Tuple(_)
            | Expr::Array(_)
            | Expr::ArrayRepeat { .. }
            | Expr::MapLiteral { .. }
            | Expr::Lambda { .. }
            | Expr::Spawn { .. }
            | Expr::SpawnLambdaActor { .. }
            | Expr::Scope { .. }
            | Expr::ForkChild { .. }
            | Expr::ForkBlock { .. }
            | Expr::ScopeDeadline { .. }
            | Expr::InterpolatedString(_)
            | Expr::Call { .. }
            | Expr::MethodCall { .. }
            | Expr::StructInit { .. }
            | Expr::Select { .. }
            | Expr::Race(_)
            | Expr::UnsafeBlock(_)
            | Expr::Yield(_)
            | Expr::Return(_)
            | Expr::ReturnError(_)
            | Expr::FieldAccess { .. }
            | Expr::Index { .. }
            | Expr::Cast { .. }
            | Expr::PostfixTry(_)
            | Expr::Range { .. }
            | Expr::Await(_)
            | Expr::AwaitRestart(_)
            | Expr::RegexLiteral(_)
            | Expr::ByteStringLiteral(_)
            | Expr::ByteArrayLiteral(_)
            | Expr::MachineEmit { .. }
            | Expr::Is { .. }
            | Expr::GenBlock { .. } => {}
        }
    }

    pub(super) fn report_owned_handle_field_return(
        &mut self,
        span: &Span,
        method_name: &str,
        type_name: &str,
        field_name: &str,
        handle_name: &str,
        via_binding: Option<&str>,
    ) {
        let via_note =
            via_binding.map_or_else(String::new, |b| format!(" (via let-binding `{b}`)"));
        self.errors.push(TypeError {
            severity: crate::error::Severity::Error,
            kind: TypeErrorKind::InvalidOperation,
            span: span.clone(),
            message: format!(
                "method `{method_name}` exposes owned handle field `{field_name}` from \
                 `{type_name}`{via_note} — returning the raw `{handle_name}` aliases the \
                 wrapper's drop path and can double-free the handle"
            ),
            notes: vec![(
                span.clone(),
                "handle-bearing structs are dropped field-by-field; returning the raw handle \
                 bypasses that ownership proof"
                    .to_string(),
                self.current_module.clone(),
            )],
            suggestions: vec![
                "use a dedicated consume/release API instead of returning the raw handle field"
                    .to_string(),
                "prefer a borrow-style accessor once mutable receivers / borrow returns land \
                 (#1295)"
                    .to_string(),
            ],
            source_module: self.current_module.clone(),
        });
    }

    pub(super) fn owned_handle_field_return(
        &self,
        expr: &Expr,
        receiver_name: &str,
        type_name: &str,
    ) -> Option<(String, String)> {
        let Expr::FieldAccess { object, field } = expr else {
            return None;
        };
        if !matches!(&object.0, Expr::Identifier(name) if name == receiver_name) {
            return None;
        }
        self.owned_handle_field_return_by_name(field, type_name)
    }

    /// Check whether the named field of `type_name` holds an owned handle.
    /// Returns `Some((field_name, handle_type_name))` when it does.
    /// Used by both the direct-return check and the bind-then-return scan.
    pub(super) fn owned_handle_field_return_by_name(
        &self,
        field: &str,
        type_name: &str,
    ) -> Option<(String, String)> {
        let type_def = self.lookup_type_def(type_name)?;
        let field_ty = type_def.fields.get(field)?;
        let Ty::Named {
            name: field_type_name,
            ..
        } = field_ty
        else {
            return None;
        };
        self.canonical_owned_handle_type_name(field_type_name)
            .map(|handle_name| (field.to_string(), handle_name))
    }

    pub(in crate::check) fn check_field_access(
        &mut self,
        object: &Spanned<Expr>,
        field: &str,
        span: &Span,
    ) -> Ty {
        self.check_field_access_with_type_args(object, field, None, span)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "field access handles many type variants"
    )]
    pub(super) fn check_field_access_with_type_args(
        &mut self,
        object: &Spanned<Expr>,
        field: &str,
        type_args: Option<&[Spanned<TypeExpr>]>,
        span: &Span,
    ) -> Ty {
        if type_args.is_some()
            && matches!(&object.0, Expr::Identifier(name) if self.env.lookup_ref(name).is_some())
        {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                "explicit type arguments require a function declaration, not a value field"
                    .to_string(),
            );
            return Ty::Error;
        }

        // `self.count` is the receiver spelling of the actor state binding
        // `count`. Delegate to the bare-name shell so the read gets the same
        // type, the same use-after-move reporting, and the same HIR binding
        // reference the bare spelling gets at this site.
        if let Some(state_field) = self.actor_self_state_field(&object.0, field) {
            self.record_actor_self_state_field(span);
            return self.synthesize_identifier(state_field, span);
        }
        if self.is_actor_self_receiver(&object.0) {
            let similar = crate::error::find_similar(
                field,
                self.current_actor_fields.iter().map(|f| f.name.as_str()),
            );
            self.report_error_with_suggestions(
                TypeErrorKind::UndefinedField,
                span,
                format!("actor state has no field `{field}`"),
                similar,
            );
            return Ty::Error;
        }
        if let Some(head) = self.resolve_dotted_type_head(object, field) {
            if let Some(result) = self.dispatch_dotted_type_member(
                &head,
                field,
                &DottedTypeMemberUse::Reference { span },
            ) {
                self.mark_resolved_nominal_owner_used(&head.canonical_type);
                return result;
            }
        }

        // Dotted type members were dispatched from the canonical head above.
        // Remaining identifiers are ordinary value projections or unresolved
        // names and continue through the existing diagnostics.

        // Dotted module-qualified unit constructor:
        // `module.Type.Variant`. The parser represents this as nested field
        // access, but neither `module` nor `module.Type` is a runtime value.
        // Resolve the complete constructor before synthesising the inner
        // projection so it shares the exact export and variant authority of
        // the existing `module.Type::Variant` surface.
        if let Expr::FieldAccess {
            object: module,
            field: type_name,
        } = &object.0
        {
            if let Expr::Identifier(module_short) = &module.0 {
                if self.module_binding_in_current_file(module_short)
                    && self.env.lookup_ref(module_short).is_none()
                {
                    let constructor = format!("{module_short}.{type_name}::{field}");
                    return self.synthesize_identifier(&constructor, span);
                }
            }
        }

        // Pre-dispatch: module-qualified value-constructor reference, e.g.
        // `m.Type::Variant` (unit or tuple-naked).  This must run BEFORE
        // `synthesize(object)` because `module` is not bound in `self.env`
        // — without the early dispatch the synthesize call would emit the
        // leaky "undefined variable `module`" diagnostic.
        //
        // Mirrors the `module_fn_exports` guard pattern at
        // `check_method_call` (methods.rs).  Gated on:
        //   - object is a bare `Expr::Identifier`
        //   - `field` contains `::` (the type-variant separator)
        //   - the identifier is neither a value binding nor a known type
        // The neither-binding-nor-type guard preserves all existing
        // field-on-value access semantics — only shapes that could only be a
        // module-qualified reference take the new path.  Nested-module paths
        // (`a.b.Type::Variant`) are out of scope for v0.5.
        if let Expr::Identifier(name) = &object.0 {
            if let Some(pos) = field.find("::") {
                let receiver_is_binding = self.env.lookup_ref(name).is_some();
                let receiver_is_known_type = self.type_defs.contains_key(name);
                if !receiver_is_binding && !receiver_is_known_type {
                    let type_name = &field[..pos];
                    let variant_name = &field[pos + 2..];
                    return self.check_module_qualified_variant_ref(
                        name,
                        type_name,
                        variant_name,
                        span,
                    );
                }
            }
        }

        // Pre-dispatch: module-qualified constant reference, e.g. `module.CONST_NAME`.
        // Must run BEFORE `synthesize(object)` for the same reason as the variant
        // arm above — the module short-name is not in env as a value binding.
        //
        // Gated on:
        //   - object is a bare `Expr::Identifier`
        //   - field does NOT contain `::` (plain const name, not a variant)
        //   - receiver is not a value binding or known type
        //   - the lexical module binding resolves to an exact owner-qualified
        //     constant key registered in env
        if let Expr::Identifier(name) = &object.0 {
            if !field.contains("::") {
                let receiver_is_binding = self.env.lookup_ref(name).is_some();
                let receiver_is_known_type = self.type_defs.contains_key(name);
                if !receiver_is_binding && !receiver_is_known_type {
                    let lexical_key = format!("{name}.{field}");
                    let qualified_key = self
                        .module_import_bindings
                        .get(&(
                            self.current_module.clone(),
                            self.current_module_idx,
                            name.clone(),
                        ))
                        .map_or_else(|| lexical_key.clone(), |owner| format!("{owner}.{field}"));
                    if let Some(binding) = self.env.lookup_ref(&qualified_key) {
                        let ty = binding.ty.clone();
                        if self.module_binding_in_current_file(name) {
                            self.used_modules.borrow_mut().insert(ImportKey::in_file(
                                self.current_module.clone(),
                                self.current_module_idx,
                                name.clone(),
                            ));
                        }
                        return ty;
                    }
                    // If the receiver looks like a module (known to self.modules) but
                    // the const is not exported, emit a targeted diagnostic rather than
                    // falling through to the generic "undefined variable `module`" error.
                    if self.module_binding_in_current_file(name) {
                        if self.fn_sigs.contains_key(&qualified_key) {
                            self.used_modules.borrow_mut().insert(ImportKey::in_file(
                                self.current_module.clone(),
                                self.current_module_idx,
                                name.clone(),
                            ));
                            self.reject_wasm_native_only_module_function(name, field, span);
                            if self.is_shipped_crypto_module(name)
                                && matches!(field, "random_bytes" | "try_random_bytes")
                            {
                                self.reject_wasm_feature(
                                    span,
                                    WasmUnsupportedFeature::CryptoRandom,
                                );
                            }
                            self.record_call_edge(&qualified_key);
                            return self.instantiate_function_value(
                                &qualified_key,
                                type_args,
                                span,
                            );
                        }
                        let similar = crate::error::find_similar(
                            field,
                            self.env
                                .all_names()
                                .filter_map(|k| k.strip_prefix(&format!("{name}.")))
                                .filter(|k| !k.contains('.')),
                        );
                        if self.resolve_module_type(name, field).is_some() {
                            self.report_error(
                                TypeErrorKind::PathKindMismatch,
                                span,
                                format!("module member `{name}.{field}` is a type, not a value"),
                            );
                            return Ty::Error;
                        }
                        self.report_error_with_suggestions(
                            TypeErrorKind::PathMemberNotFound,
                            span,
                            format!("module `{name}` has no exported value `{field}`"),
                            similar,
                        );
                        return Ty::Error;
                    }
                }
            }
        }

        // The object is the BASE of this projection, not a whole-value use of
        // itself: `h.other` stays legal after `h.sock` moved out.
        self.place_base_depth += 1;
        let obj_ty = self.synthesize(&object.0, &object.1);
        self.place_base_depth -= 1;
        // Reading this projection after it (or storage under it) was consumed
        // is a use-after-move. Assignment targets are exempt: the outermost
        // target place is written, not read.
        if self.place_write_depth == 0 || self.place_base_depth > 0 {
            if let Some((root, mut path)) = self.expr_place(&object.0) {
                path.push(field.to_string());
                self.report_place_use_after_move(&root, &path, span);
            }
        }
        let resolved = self.normalize_for_use(&obj_ty);
        if self.reject_sealed_delivery_access(&resolved, span) {
            return Ty::Error;
        }

        match &resolved {
            // `Range<T>` exposes its bounds as `start`/`end`. It carries no
            // `TypeDef` (it is a compiler builtin, not a user declaration), so
            // the two fields resolve straight from the type's own argument
            // instead of the `type_defs` table the generic `Named` arm below
            // reads from. Any other field name falls through to that arm,
            // finds no `TypeDef` for `Range`, and reports `UndefinedField`
            // exactly as before.
            Ty::Named {
                builtin: Some(BuiltinType::Range),
                args,
                ..
            } if args.len() == 1 && matches!(field, "start" | "end") => args[0].clone(),
            Ty::Named { name, args, .. } => {
                // A role retains the child's complete type after substituting
                // the owning supervisor's concrete arguments.
                if let Some(Ty::Named {
                    name: sup_name,
                    args: sup_args,
                    ..
                }) = resolved.as_local_actor_ref()
                {
                    if let Some(children) = self.supervisor_children.get(sup_name).cloned() {
                        let selected = children
                            .statics
                            .iter()
                            .enumerate()
                            .map(|(index, child)| (super::types::ChildKind::Static, index, child))
                            .chain(children.pools.iter().enumerate().map(|(index, child)| {
                                (super::types::ChildKind::Pool, index, child)
                            }))
                            .find(|(_, _, (name, _))| name == field);
                        if let Some((kind, index, (child_name, template))) = selected {
                            let parameters = self
                                .type_defs
                                .get(sup_name)
                                .map_or_else(Vec::new, |definition| definition.type_params.clone());
                            let substitution = parameters
                                .into_iter()
                                .zip(sup_args.iter().cloned())
                                .collect();
                            let child_ty = template.substitute_named_params_parallel(&substitution);
                            if let Ty::Named { name, args, .. } = &child_ty {
                                self.enforce_type_def_instantiation_bounds(name, args, span);
                            }
                            self.supervisor_child_slots.insert(
                                SpanKey::in_module(span, self.current_module_idx),
                                super::types::ChildSlot {
                                    kind,
                                    index: u32::try_from(index)
                                        .expect("supervisor child count exceeds u32"),
                                    child_ty: child_ty.user_facing().to_string(),
                                    child_name: child_name.clone(),
                                    supervisor: sup_name.clone(),
                                },
                            );
                            if kind == super::types::ChildKind::Pool {
                                return Ty::supervisor_pool(
                                    Ty::actor_handle(sup_name.clone(), sup_args.clone()),
                                    child_ty,
                                );
                            }
                            return Ty::child_ref(child_ty);
                        }
                        let names = children
                            .statics
                            .iter()
                            .chain(children.pools.iter())
                            .map(|(name, _)| name.as_str());
                        let similar = crate::error::find_similar(field, names);
                        self.report_error_with_suggestions(
                            TypeErrorKind::UndefinedField,
                            span,
                            format!("supervisor `{sup_name}` has no child named `{field}`"),
                            similar,
                        );
                        return Ty::Error;
                    }
                }
                if let Some(td) = self.lookup_type_def(name) {
                    if let Some(field_ty) = td.fields.get(field) {
                        // Substitute generic type params with concrete args in
                        // parallel so a swap instantiation like `Pair<B, A>` does
                        // not alias: sequential A→B then B→A would produce A again.
                        let subst_map: HashMap<String, Ty> = td
                            .type_params
                            .iter()
                            .zip(args.iter())
                            .map(|(p, a)| (p.clone(), a.clone()))
                            .collect();
                        field_ty.substitute_named_params_parallel(&subst_map)
                    } else {
                        let similar =
                            crate::error::find_similar(field, td.fields.keys().map(String::as_str));
                        self.report_error_with_suggestions(
                            TypeErrorKind::UndefinedField,
                            span,
                            format!("no field `{field}` on type `{name}`"),
                            similar,
                        );
                        Ty::Error
                    }
                } else {
                    self.report_error(
                        TypeErrorKind::UndefinedField,
                        span,
                        format!(
                            "cannot access field `{field}` on `{}`",
                            resolved.user_facing()
                        ),
                    );
                    Ty::Error
                }
            }
            Ty::Tuple(elems) => {
                // Tuple field access by index: t.0, t.1
                if let Ok(idx) = field.parse::<usize>() {
                    if idx < elems.len() {
                        elems[idx].clone()
                    } else {
                        self.report_error(
                            TypeErrorKind::UndefinedField,
                            span,
                            format!("tuple index {idx} out of range (len {})", elems.len()),
                        );
                        Ty::Error
                    }
                } else {
                    Ty::Error
                }
            }
            _ => {
                if resolved != Ty::Error {
                    self.report_error(
                        TypeErrorKind::UndefinedField,
                        span,
                        format!(
                            "cannot access field `{field}` on `{}`",
                            resolved.user_facing()
                        ),
                    );
                }
                Ty::Error
            }
        }
    }

    pub(in crate::check) fn check_match_expr(
        &mut self,
        scrutinee_ty: &Ty,
        scrutinee: &Spanned<Expr>,
        arms: &[MatchArm],
        span: &Span,
        expected: Option<&Ty>,
    ) -> Ty {
        if arms.is_empty() {
            let resolved = self.subst.resolve(scrutinee_ty);
            let uninhabited = match &resolved {
                Ty::Never => true,
                Ty::Named { name, .. } => self.lookup_type_def(name).is_some_and(|definition| {
                    definition.kind == TypeDefKind::Enum && definition.variants.is_empty()
                }),
                _ => false,
            };
            if uninhabited {
                return Ty::Never;
            }
            if resolved != Ty::Error {
                self.report_error(
                    TypeErrorKind::NonExhaustiveMatch,
                    span,
                    format!(
                        "an empty match cannot cover inhabited type `{}`",
                        resolved.user_facing()
                    ),
                );
            }
            return Ty::Error;
        }

        let scrutinee_place = self.expr_place(&scrutinee.0);
        let scrutinee_loan = self.collection_borrow_origin(&scrutinee.0, &scrutinee.1);
        // If the enclosing context supplies a concrete expected type (e.g. the
        // function's declared return type), pre-seed result_ty so every arm body
        // is checked with check_against rather than having the first arm's
        // synthesized type (which defaults literals to i64) propagate to later arms.
        let resolved_expected = expected.map(|ty| self.subst.resolve(ty));
        let mut result_ty: Option<Ty> = match &resolved_expected {
            Some(ty) if !matches!(ty, Ty::Var(_) | Ty::Error) => Some(ty.clone()),
            _ => None,
        };
        // When this `match` is itself a function-return tail, every arm body
        // flows to the return and may Ok-coerce. Capture the armed state once;
        // the per-arm guard check and pattern binding are not tail positions, so
        // re-arm immediately before each arm body.
        let tail_ok_armed = std::mem::replace(&mut self.tail_ok_armed, false);
        // Exactly one arm BODY runs, so each body starts from the ownership
        // state at the match's entry rather than from whatever the previous arm
        // left behind, and the state after the match is the union over the arms
        // that actually reach the join.
        //
        // Guards are not bodies. A guard runs whenever its pattern matched and
        // every earlier arm did not, so guard N and body N+1 both execute on one
        // path. Guards therefore thread through a running fall-through state —
        // the same treatment an `else if` chain's conditions get — and each body
        // starts from the fall-through its own guard produced.
        //
        // A guard that DIVERGES is the exception, and it cuts both ways. A later
        // arm is reached only when this arm's pattern failed, and then the guard
        // never ran at all — so a diverging guard contributes nothing to the
        // fall-through. Its own body is unreachable for the same reason, so the
        // body's exit must stay out of the join no matter what the body does.
        let ownership_entry = self.env.ownership_snapshot();
        let mut fall_through = ownership_entry.clone();
        let mut arm_exits = Vec::with_capacity(arms.len());
        for arm in arms {
            self.env.push_scope();
            self.env.restore_ownership(&fall_through);
            self.bind_scrutinee_pattern(
                &arm.pattern,
                scrutinee_ty,
                false,
                scrutinee_place.clone(),
                scrutinee_loan.clone(),
            );
            self.record_arm_resolution(&arm.pattern.0, &arm.pattern.1, scrutinee_ty);

            let mut guard_diverges = false;
            if let Some((guard, gs)) = &arm.guard {
                // Pattern bindings borrow during candidate testing. Their
                // field transfers happen only after the guard selects this
                // arm, so a declined candidate cannot move the source.
                let pattern_entry = fall_through.clone();
                let selected_pattern = self.env.ownership_snapshot();
                self.env.restore_ownership(&fall_through);
                let guard_ty = self.check_against(guard, gs, &Ty::Bool);
                if Self::arm_skips_join(&guard_ty) {
                    guard_diverges = true;
                    // Rewind the guard's consumes: neither the unreachable body
                    // below nor any later arm ever observes them.
                    self.env.restore_ownership(&fall_through);
                } else {
                    // The guard ran and returned false; later arms see its state.
                    fall_through = self.env.ownership_snapshot();
                    self.env
                        .apply_pattern_moves(&pattern_entry, &selected_pattern);
                }
            }

            self.tail_ok_armed = tail_ok_armed;
            let arm_ty = if let Some(expected) = &result_ty {
                if expected.contains_callable() && resolved_expected.is_none() {
                    self.synthesize(&arm.body.0, &arm.body.1)
                } else {
                    self.check_expr_with_expected(&arm.body.0, &arm.body.1, expected)
                }
            } else {
                self.synthesize(&arm.body.0, &arm.body.1)
            };
            self.record_value_transfer(&arm.body.0, &arm.body.1);
            arm_exits.push(BranchArmExit {
                ownership: self.env.ownership_snapshot(),
                diverges: guard_diverges || Self::arm_skips_join(&arm_ty),
            });
            // Skip Never/Error when setting the expected type — diverging arms
            // (return, panic, break) shouldn't constrain the match result type.
            if !matches!(arm_ty, Ty::Never | Ty::Error) {
                result_ty = Some(if let Some(previous) = result_ty {
                    if previous.contains_callable() || arm_ty.contains_callable() {
                        self.unify_branches(&previous, &arm_ty, span)
                    } else {
                        previous
                    }
                } else {
                    arm_ty
                });
            }

            self.env.pop_scope();
        }
        self.join_branch_ownership(&ownership_entry, &arm_exits);
        // Leave the flag disarmed: the arm loop set it per-arm, and the
        // exhaustiveness check below is not a tail position.
        self.tail_ok_armed = false;

        // Exhaustiveness check for enums/Option/Result
        self.check_exhaustiveness(scrutinee_ty, arms, span);

        // If all arms diverge (Never/Error), the match itself diverges
        result_ty.unwrap_or(Ty::Never)
    }

    #[expect(
        clippy::too_many_arguments,
        reason = "lambda checking combines contextual inference with capture analysis"
    )]
    pub(in crate::check) fn check_lambda(
        &mut self,
        is_move: bool,
        private_captures: &[Spanned<String>],
        type_params: Option<&[TypeParam]>,
        params: &[LambdaParam],
        return_type: Option<&Spanned<TypeExpr>>,
        body: &Spanned<Expr>,
        expected: Option<(&[Ty], &Ty)>,
        span: &Span,
        is_actor_body: bool,
        is_fork_body: bool,
    ) -> Ty {
        let key = SpanKey::in_module(span, self.current_module_idx);
        let owner = super::effects::EffectBody::Closure(key.clone());
        self.effect_graph.bodies.entry(owner.clone()).or_default();
        let previous = self.effect_graph.current_body.replace(owner);
        let result = self.check_lambda_body(
            is_move,
            private_captures,
            type_params,
            params,
            return_type,
            body,
            expected,
            span,
            is_actor_body,
            is_fork_body,
        );
        self.effect_graph.current_body = previous;
        result
    }

    #[expect(
        clippy::too_many_arguments,
        clippy::too_many_lines,
        reason = "lambda checking combines contextual inference with capture analysis"
    )]
    pub(super) fn check_lambda_body(
        &mut self,
        is_move: bool,
        private_captures: &[Spanned<String>],
        type_params: Option<&[TypeParam]>,
        params: &[LambdaParam],
        return_type: Option<&Spanned<TypeExpr>>,
        body: &Spanned<Expr>,
        expected: Option<(&[Ty], &Ty)>,
        span: &Span,
        is_actor_body: bool,
        is_fork_body: bool,
    ) -> Ty {
        let private_bindings = self.resolve_private_captures(private_captures);
        let body_environment = self
            .env
            .closure_environment(&private_bindings, is_actor_body);
        let outer_environment = std::mem::replace(&mut self.env, body_environment);
        // Save/restore capture tracking state for nested lambdas
        let prev_capture_depth = self.lambda_capture_depth;
        let prev_captures = std::mem::take(&mut self.lambda_captures);
        let prev_capture_facts = std::mem::take(&mut self.lambda_capture_facts);
        let prev_actor_handler_context = self.in_actor_handler_context;
        self.in_actor_handler_context = false;
        // A lambda body does not inherit the lexical task scope it is written
        // inside: the closure may run after the scope has joined, so `fork`
        // statements inside it have no spawn context.
        let prev_task_scope_depth = self.task_scope_depth;
        self.task_scope_depth = 0;
        let prev_in_lambda_actor_body = self.in_lambda_actor_body;
        // Set is_actor_body for the duration of this lambda's body; nested fn-closures
        // are called with is_actor_body=false, so they get false regardless of the outer flag.
        self.in_lambda_actor_body = is_actor_body;

        // Record the scope depth BEFORE pushing the lambda scope — any variable
        // found below this depth during body checking is a capture.
        let capture_depth = self.env.depth();
        self.lambda_capture_depth = Some(capture_depth);

        // Clear any stale scratch state from a previous call in a non-let or
        // nested context.  We unconditionally reset first so that re-entrant
        // calls (e.g., a generic lambda inside a function argument) cannot
        // bleed their type-var pairs out to an unrelated enclosing Stmt::Let.
        self.last_lambda_generic_sig = None;

        let mut generic_bindings = std::collections::HashMap::new();
        let mut generic_param_names = HashMap::new();
        let mut generic_type_vars = Vec::new();
        if let Some(tps) = type_params {
            for tp in tps {
                let tv = TypeVar::fresh();
                generic_bindings.insert(tp.name.clone(), Ty::Var(tv));
                generic_param_names.insert(tv.0, tp.name.clone());
                generic_type_vars.push(tv);
            }
        }
        if !generic_bindings.is_empty() {
            self.generic_ctx.push(generic_bindings);
        }

        self.env.push_scope();
        let prev_in_generator = self.in_generator;
        self.in_generator = false;

        // Check arity mismatch: lambda parameter count must match expected function type
        if let Some((expected_params, _)) = &expected {
            if params.len() != expected_params.len() {
                self.errors.push(TypeError::new(
                    TypeErrorKind::ArityMismatch,
                    span.clone(),
                    format!(
                        "lambda has {} parameters but expected function type has {}",
                        params.len(),
                        expected_params.len()
                    ),
                ));
            }
        }

        let mut param_tys = Vec::new();
        for (i, p) in params.iter().enumerate() {
            let ty = if let Some(annotation) = &p.ty {
                let (annotated_ty, hole_vars) = self.resolve_annotation_holes(annotation);
                // Unify the annotated type against the expected param type regardless
                // of whether the annotation contains holes.  The holes path (deferred
                // inference) is orthogonal: a fully-concrete annotation (`|x: i64|`)
                // must still be rejected when the expected param type is `bool`.
                if let Some((expected_params, _)) = &expected {
                    if let Some(expected_ty) = expected_params.get(i) {
                        self.expect_type(expected_ty, &annotated_ty, &annotation.1);
                    }
                }
                if !hole_vars.is_empty() {
                    self.record_deferred_inference_holes(
                        annotation,
                        format!("lambda parameter `{}`", p.name),
                        hole_vars,
                    );
                }
                self.subst.resolve(&annotated_ty)
            } else if let Some((expected_params, _)) = &expected {
                expected_params
                    .get(i)
                    .cloned()
                    .unwrap_or_else(|| Ty::Var(TypeVar::fresh()))
            } else {
                Ty::Var(TypeVar::fresh())
            };
            self.check_shadowing(&p.name, &p.name_span);
            self.env
                .define_param_with_span(p.name.clone(), ty.clone(), false, p.name_span.clone());
            param_tys.push(ty);
        }

        // Save enclosing return type and install the lambda's own return type so
        // that PostfixTry (`?`) context checks see the lambda's return type,
        // not the outer function's.
        let prev_return_type = self.current_return_type.take();
        let previous_defer = self.deferred_body.take();
        let prev_fails = std::mem::replace(&mut self.current_fails, false);

        let previous_inferred_returns = self.inferred_lambda_returns.take();
        let ret_ty = if let Some(annotation) = return_type {
            let (expected_ret, hole_vars) = self.resolve_annotation_holes(annotation);
            // Unify the annotated return type against the contextual expected return
            // type regardless of holes — same rationale as annotated param types above.
            if let Some((_, contextual_ret)) = expected {
                self.expect_type(contextual_ret, &expected_ret, &annotation.1);
            }
            if !hole_vars.is_empty() {
                self.record_deferred_inference_holes(annotation, "lambda return type", hole_vars);
            }
            self.current_return_type = Some(expected_ret.clone());
            // Guard: do not pre-seed body with Ty::Error (unresolvable annotation).
            // Synthesize instead so internal body errors are still reported.
            let resolved_ret = self.subst.resolve(&expected_ret);
            if matches!(resolved_ret, Ty::Error) {
                self.synthesize(&body.0, &body.1);
            } else {
                self.check_against(&body.0, &body.1, &expected_ret);
            }
            self.subst.resolve(&expected_ret)
        } else if let Some((_, expected_ret)) = expected {
            self.current_return_type = Some(expected_ret.clone());
            self.check_against(&body.0, &body.1, expected_ret);
            expected_ret.clone()
        } else {
            self.infer_lambda_result(body)
        };
        self.inferred_lambda_returns = previous_inferred_returns;
        self.record_value_transfer(&body.0, &body.1);

        self.current_return_type = prev_return_type;
        self.deferred_body = previous_defer;
        self.current_fails = prev_fails;
        self.in_actor_handler_context = prev_actor_handler_context;
        self.task_scope_depth = prev_task_scope_depth;
        self.in_lambda_actor_body = prev_in_lambda_actor_body;
        self.in_generator = prev_in_generator;
        self.env.pop_scope();

        if let Some(tps) = type_params {
            if !tps.is_empty() {
                let type_param_bounds = tps
                    .iter()
                    .filter_map(|tp| {
                        if tp.bounds.is_empty() {
                            None
                        } else {
                            Some((
                                tp.name.clone(),
                                tp.bounds.iter().map(|bound| bound.name.clone()).collect(),
                            ))
                        }
                    })
                    .collect();
                self.last_lambda_generic_sig = Some(GenericLambdaSig {
                    call_sig: FnSig {
                        type_params: tps.iter().map(|tp| tp.name.clone()).collect(),
                        type_param_bounds,
                        param_names: params.iter().map(|param| param.name.clone()).collect(),
                        params: param_tys
                            .iter()
                            .map(|param| {
                                Self::lambda_generic_schema_ty(param, &generic_param_names)
                            })
                            .collect(),
                        return_type: Self::lambda_generic_schema_ty(&ret_ty, &generic_param_names),
                        ..FnSig::default()
                    },
                    type_vars: generic_type_vars,
                });
                self.generic_ctx.pop();
            }
        }

        let body_environment = std::mem::replace(&mut self.env, outer_environment);
        self.env.merge_closure_reads(&body_environment);
        let raw_capture_facts = std::mem::take(&mut self.lambda_capture_facts);
        // Acquisition happens in the enclosing scope, not in the new closure.
        self.lambda_capture_depth = prev_capture_depth;
        let capture_facts = self.finish_closure_captures(
            raw_capture_facts,
            &private_bindings,
            &body_environment,
            is_move,
            span,
            is_fork_body,
        );
        let capabilities = self.closure_capabilities(&capture_facts);
        self.closure_capture_facts.insert(
            SpanKey::in_module(span, self.current_module_idx),
            capture_facts.clone(),
        );

        // The callable payload and its guarantees come from the same resolved captures.
        let captures: Vec<Ty> = capture_facts.iter().map(|fact| fact.ty.clone()).collect();

        // Restore outer capture tracking state
        self.lambda_captures = prev_captures;
        self.lambda_capture_facts = prev_capture_facts;
        if let Some(depth) = prev_capture_depth {
            for fact in &capture_facts {
                if self
                    .env
                    .lookup_with_depth(&fact.name)
                    .is_some_and(|(binding_depth, binding)| {
                        binding_depth < depth && binding.id == fact.binding_id
                    })
                {
                    self.lambda_capture_facts.push(fact.clone());
                }
            }
        }

        // Every literal has a concrete environment type, including an empty one.
        // Callable guarantees do not erase the identity needed by HIR and SIR.
        Ty::Closure {
            capabilities,
            params: param_tys,
            ret: Box::new(ret_ty),
            captures,
            identity: super::effects::EffectBody::Closure(SpanKey::in_module(
                span,
                self.current_module_idx,
            )),
        }
    }

    /// Resolve a module-qualified value-constructor reference of the form
    /// `module.Type::Variant` to its result type.  Emits a fail-closed
    /// diagnostic for each of the four error shapes (unknown module alias,
    /// no exported type, no such variant, struct-variant without braces)
    /// — never falls through to the leaky "undefined variable" /
    /// "undefined type" surface.  Called only from the
    /// `check_field_access` pre-dispatch arm.
    #[expect(
        clippy::too_many_lines,
        reason = "qualified variant resolution handles each failure shape together"
    )]
    pub(in crate::check) fn check_module_qualified_variant_ref(
        &mut self,
        module_short: &str,
        type_name: &str,
        variant_name: &str,
        span: &Span,
    ) -> Ty {
        let lifecycle_surface = format!("{module_short}.{type_name}::{variant_name}");
        let Ok(canonical_lifecycle) =
            self.canonicalize_source_lifecycle_value_path(&lifecycle_surface, span)
        else {
            return Ty::Error;
        };
        if !self.module_binding_in_current_file(module_short) {
            let similar =
                crate::error::find_similar(module_short, self.modules.iter().map(String::as_str));
            self.report_error_with_suggestions(
                TypeErrorKind::UndefinedVariable,
                span,
                format!("unknown module alias `{module_short}`"),
                similar,
            );
            return Ty::Error;
        }
        self.used_modules.borrow_mut().insert(ImportKey::in_file(
            self.current_module.clone(),
            self.current_module_idx,
            module_short.to_string(),
        ));
        let Some(td) = self.resolve_module_type(module_short, type_name) else {
            let similar = self
                .module_type_exports_for_binding(module_short)
                .map(|set| crate::error::find_similar(type_name, set.iter().map(String::as_str)))
                .unwrap_or_default();
            self.report_error_with_suggestions(
                TypeErrorKind::PathMemberNotFound,
                span,
                format!("module `{module_short}` has no exported type `{type_name}`"),
                similar,
            );
            return Ty::Error;
        };
        let Some((_td_again, variant)) =
            self.resolve_module_variant(module_short, type_name, variant_name)
        else {
            let similar =
                crate::error::find_similar(variant_name, td.variants.keys().map(String::as_str));
            self.report_error_with_suggestions(
                TypeErrorKind::PathMemberNotFound,
                span,
                format!("type `{module_short}.{type_name}` has no variant `{variant_name}`"),
                similar,
            );
            return Ty::Error;
        };
        let qualified_type = canonical_lifecycle
            .as_deref()
            .and_then(|path| path.split_once("::").map(|(ty, _)| ty.to_string()))
            .unwrap_or_else(|| {
                format!(
                    "{}.{type_name}",
                    self.canonical_module_import_owner(module_short)
                )
            });
        match variant {
            VariantDef::Unit => {
                // Instantiate type params with fresh inference vars so generic
                // enums (e.g. `Option<T>::None`) unify against later annotations.
                // Mirrors the unit-variant path in `resolve_identifier_variant`.
                let args: Vec<Ty> = td
                    .type_params
                    .iter()
                    .map(|_| Ty::Var(TypeVar::fresh()))
                    .collect();
                Ty::normalize_named(qualified_type, args)
            }
            VariantDef::Tuple(params) => {
                // Tuple-variant naked reference (no call): treat as a function
                // value, matching the bare-identifier function-value path at
                // expressions.rs (resolve_identifier).  The call form
                // `m.Type::V(args)` is handled by `check_method_call` via
                // `lookup_variant_constructor`.
                let args: Vec<Ty> = td
                    .type_params
                    .iter()
                    .map(|_| Ty::Var(TypeVar::fresh()))
                    .collect();
                let ctor_subst_map: HashMap<String, Ty> = td
                    .type_params
                    .iter()
                    .zip(args.iter())
                    .map(|(p, a)| (p.clone(), a.clone()))
                    .collect();
                let subst_params: Vec<Ty> = params
                    .iter()
                    .map(|p| p.substitute_named_params_parallel(&ctor_subst_map))
                    .collect();
                let ret = Ty::normalize_named(qualified_type, args);
                Ty::Function {
                    capabilities: crate::CallableCapabilities::FUNCTION_ITEM,
                    params: subst_params,
                    ret: Box::new(ret),
                }
            }
            VariantDef::Struct(_) => {
                // Struct variants require the braced initialiser; parse never
                // reaches this arm in the StructInit shape (that goes through
                // `check_struct_init`).  A naked `m.E::V` for a struct variant
                // is a user error — emit a hint rather than silently typing it.
                self.report_error(
                    TypeErrorKind::UndefinedField,
                    span,
                    format!(
                        "variant `{module_short}.{type_name}.{variant_name}` is a struct \
                         variant; use `{module_short}.{type_name}.{variant_name} {{ ... }}` \
                         to construct it"
                    ),
                );
                Ty::Error
            }
        }
    }

    #[expect(
        clippy::type_complexity,
        reason = "exact variant-owner lookup carries the owner, fields, and type parameters together"
    )]
    pub(super) fn lookup_struct_variant_init(
        &self,
        surface_name: &str,
    ) -> Option<(String, Vec<(String, Ty)>, Vec<String>)> {
        let variant_name = surface_name.rsplit("::").next().unwrap_or(surface_name);
        let mut candidates: Vec<(String, Vec<(String, Ty)>, Vec<String>)> = self
            .type_defs
            .iter()
            .filter_map(|(type_name, td)| {
                let canonical_type_name = self
                    .canonical_nominal_name(type_name)
                    .unwrap_or_else(|| type_name.clone());
                let expected = Ty::Named {
                    name: canonical_type_name.clone(),
                    args: vec![],
                    builtin: None,
                };
                if !self.variant_surface_owner_matches(surface_name, &expected) {
                    return None;
                }
                match td
                    .variants
                    .get(variant_name)
                    .or_else(|| td.variants.get(surface_name))
                {
                    Some(VariantDef::Struct(fields)) => {
                        Some((canonical_type_name, fields.clone(), td.type_params.clone()))
                    }
                    _ => None,
                }
            })
            .collect();
        candidates.sort_by(|a, b| a.0.cmp(&b.0));
        candidates.dedup_by(|a, b| a.0 == b.0);

        if !surface_name.contains("::") {
            let mut local = candidates
                .iter()
                .filter(|(type_name, _, _)| {
                    self.local_type_defs.contains(type_name)
                        || self.source_type_defs.contains(type_name)
                })
                .cloned();
            let first = local.next();
            if first.is_some() && local.next().is_none() {
                return first;
            }
        }

        match candidates.as_slice() {
            [only] => Some(only.clone()),
            _ => None,
        }
    }
}
