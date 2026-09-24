//! Checker methods grouped by responsibility: borrow diagnostics.
//! Split from `expressions.rs`: checker methods, part 1 of 5.
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
                handler_scopes.push(HashMap::from([(error.0.to_string(), None)]));
                self.check_expr_is_rc_param_return(&body.0, &body.1, &handler_scopes);
            }
            Expr::Ident(ident) => {
                if let Some(source) = Self::lookup_dangerous_binding(ident.name.as_str(), scopes) {
                    self.emit_borrowed_param_return(ident.name.as_str(), &source, span);
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
                    Expr::Ident(name)
                        if crate::runtime_call::RuntimeCallFamily::from_checker_signature(name.name.as_str())
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
                .dotted_static_aggregate_identity(&receiver.0, method.0.name.as_str())
                .is_some() =>
            {
                let identity = self
                    .dotted_static_aggregate_identity(&receiver.0, method.0.name.as_str())
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
            self.defs.declaration_kind_by_path(self.defs.path(def))
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
            Expr::Ident(name) => name,
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
        if name.name.as_str().contains("::") {
            return true;
        }
        // User enum / struct tuple-variant constructors, resolved by name
        // (any casing) rather than an uppercase-first heuristic.
        self.lookup_variant_constructor(name.name.as_str())
            .is_some()
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
        let Expr::Ident(owner) = receiver else {
            return None;
        };
        if self.env.lookup_ref(owner.name.as_str()).is_some() {
            return None;
        }
        let identity = format!("{owner}::{method}");
        (self.lookup_variant_constructor(&identity).is_some()
            || self
                .source_nominal_declaration(owner.name.as_str())
                .is_some()
            || crate::lookup_builtin_type(owner.name.as_str()).is_some())
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
            Expr::Ident(name) => Self::lookup_dangerous_binding(name.name.as_str(), scopes),
            Expr::Call { function, args, .. }
                if self.callee_is_aggregate_constructor(&function.0) =>
            {
                if matches!(
                    &function.0,
                    Expr::Ident(name)
                        if crate::runtime_call::RuntimeCallFamily::from_checker_signature(name.name.as_str())
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
                .dotted_static_aggregate_identity(&receiver.0, method.0.name.as_str())
                .is_some() =>
            {
                let identity = self
                    .dotted_static_aggregate_identity(&receiver.0, method.0.name.as_str())
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
                        Pattern::Identifier(name)
                            if !self.let_identifier_is_unit_variant(name.name.as_str()) =>
                        {
                            Self::define_dangerous_binding(scopes, name.to_string(), binding);
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
                    Self::define_dangerous_binding(scopes, name.to_string(), binding);
                }
                Stmt::Assign {
                    target: (Expr::Ident(name), _),
                    value: (expr, _),
                    ..
                } => {
                    let binding = self.dangerous_source_in_expr(expr, scopes);
                    Self::update_dangerous_binding(scopes, name.name.as_str(), binding);
                }
                Stmt::Assign {
                    target: (Expr::FieldAccess { object, .. }, _),
                    value: (expr, _),
                    ..
                } => {
                    if let Expr::Ident(obj_name) = &object.0 {
                        if let Some(binding) = self.dangerous_source_in_expr(expr, scopes) {
                            Self::update_dangerous_binding(
                                scopes,
                                obj_name.name.as_str(),
                                Some(binding),
                            );
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
                    if STORING_METHODS.contains(&method.0.name.as_str()) {
                        if let Expr::Ident(recv_name) = &receiver.0 {
                            for arg in args {
                                let (expr, _) = arg.expr();
                                if let Some(binding) = self.dangerous_source_in_expr(expr, scopes) {
                                    Self::update_dangerous_binding(
                                        scopes,
                                        recv_name.name.as_str(),
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
            .map(|param| param.name)
        else {
            return;
        };

        // `bindings` maps let-binding variable names to the (field_name,
        // handle_type_name) they alias, so `return p` after `let p = self.field`
        // fires the same diagnostic as a direct `return self.field`.
        let mut bindings: HashMap<String, (String, String)> = HashMap::new();
        self.scan_block_for_owned_handle_field_return(
            &fd.body,
            receiver_name.name.as_str(),
            &type_name,
            fd.name.name.as_str(),
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
                } if matches!(&object.0, Expr::Ident(n) if n.name.as_str() == receiver_name)
                    && !self.let_identifier_is_unit_variant(var_name.name.as_str()) =>
                {
                    if let Some((field_name, handle_name)) =
                        self.owned_handle_field_return_by_name(field.0.name.as_str(), type_name)
                    {
                        bindings.insert(var_name.to_string(), (field_name, handle_name));
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
        if let Expr::Ident(var_name) = expr {
            if let Some((field_name, handle_name)) = bindings.get(var_name.name.as_str()).cloned() {
                self.report_owned_handle_field_return(
                    span,
                    method_name,
                    type_name,
                    &field_name,
                    &handle_name,
                    Some(var_name.name.as_str()),
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
                    branch_bindings.remove(error.0.name.as_str());
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
            | Expr::Ident(_)
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
        if !matches!(&object.0, Expr::Ident(name) if name.name.as_str() == receiver_name) {
            return None;
        }
        self.owned_handle_field_return_by_name(field.0.name.as_str(), type_name)
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
}
