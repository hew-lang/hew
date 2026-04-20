#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;

impl Checker {
    pub(super) fn check_item(&mut self, item: &Item, span: &Span) {
        match item {
            Item::Function(fd) => self.check_function(fd),
            Item::Actor(ad) => self.check_actor(ad),
            Item::Const(cd) => self.check_const(cd, span),
            Item::Impl(id) => self.check_impl(id, span),
            Item::Machine(md) => self.check_machine_exhaustiveness(md, span),
            Item::Trait(td) => self.check_trait_defaults(td),
            Item::Import(_)
            | Item::TypeDecl(_)
            | Item::TypeAlias(_)
            | Item::Wire(_)
            | Item::ExternBlock(_)
            | Item::Supervisor(_) => {} // Already handled during earlier checker passes
        }
    }

    pub(super) fn check_function(&mut self, fd: &FnDecl) {
        let fn_name = scoped_module_item_name(self.current_module.as_deref(), &fd.name)
            .unwrap_or_else(|| fd.name.clone());
        self.check_function_as(fd, &fn_name);
    }

    /// Check a function body using `fn_name` for the `fn_sigs` lookup.
    ///
    /// Impl methods are registered under qualified names (e.g. `Connection::close`)
    /// but `FnDecl::name` is bare (e.g. `close`). Using the qualified name prevents
    /// collisions with builtins or inlined functions from other modules.
    pub(super) fn check_function_as(&mut self, fd: &FnDecl, fn_name: &str) {
        let prev_function = self.current_function.take();
        self.current_function = Some(fn_name.to_string());
        let prev_in_pure = self.in_pure_function;
        self.in_pure_function = fd.is_pure;
        self.env.push_scope();

        // If inside an actor, push a separate scope for parameters so
        // shadowing checks detect collisions with actor field names.
        let in_actor = !self.current_actor_fields.is_empty();
        if in_actor {
            self.env.push_scope();
        }

        // Bind params — only the first parameter can be the receiver
        for (i, p) in fd.params.iter().enumerate() {
            let mut ty = self.resolve_type_expr(&p.ty);
            if i == 0 && self.is_receiver_param(p) {
                if let Some((self_name, self_args)) = &self.current_self_type {
                    ty = Ty::Named {
                        name: self_name.clone(),
                        args: self_args.clone(),
                    };
                }
            }
            // If inside an actor, check that params don't shadow actor fields
            if in_actor {
                self.check_shadowing(&p.name, &p.ty.1);
            }
            self.env.define(p.name.clone(), ty, p.is_mutable);
        }

        // Use the return type from the already-registered fn signature so that
        // TypeExpr::Infer (-> _) reuses the same Ty::Var that call sites see.
        // This ensures body-checking unification updates the shared type variable.
        let declared_ret = if let Some(sig) = self.fn_sigs.get(fn_name) {
            sig.return_type.clone()
        } else {
            fd.return_type.as_ref().map_or(Ty::Unit, |annotation| {
                let ty = self.resolve_type_expr(annotation);
                self.validate_concrete_collection_types(&ty, &annotation.1);
                ty
            })
        };
        // Generator bodies don't return the declared type — they yield it.
        // The body itself should return Unit (falls off the end).
        let expected_ret = if fd.is_generator {
            Ty::Unit
        } else {
            declared_ret.clone()
        };
        // Store the declared yields type so Expr::Yield can check against it.
        self.current_return_type = Some(declared_ret);
        let prev_in_generator = self.in_generator;
        self.in_generator = fd.is_generator;

        // Pass expected_ret so the trailing expression is checked with check_against,
        // enabling integer/float literal coercion for function return positions.
        // check_block handles error reporting for the trailing expression;
        // expect_type below handles the remaining mismatch for non-trailing paths
        // (e.g. a Stmt::Expression followed by no trailing expr).
        //
        // Guard: do not pre-seed with Ty::Error (e.g. from an unknown return-type
        // annotation). Doing so would suppress diagnostics from the body because
        // expect_type short-circuits when either side is Ty::Error.
        let resolved_expected_ret = self.subst.resolve(&expected_ret);
        let block_expected = if matches!(resolved_expected_ret, Ty::Error) {
            None
        } else {
            Some(&expected_ret)
        };
        let actual = self.check_block(&fd.body, block_expected);
        if !matches!(self.subst.resolve(&expected_ret), Ty::Error) {
            self.expect_type(
                &expected_ret,
                &actual,
                &(fd.body
                    .stmts
                    .last()
                    .map_or(fd.decl_span.clone(), |(_, s)| s.clone())),
            );
        }

        // ── Rc<T> call-boundary safety: warn on returning a borrowed Rc param ──
        // Under borrow-on-call semantics the callee does not own function params.
        // Returning an Rc param without .clone() aliases the caller's pointer —
        // both caller-local drop and callee-result drop will fire on the same
        // allocation → double-free.  Emit a warning with a .clone() suggestion.
        if !fd.is_generator {
            self.warn_rc_param_return(fd);
        }
        self.reject_owned_handle_field_accessors(fd);

        self.in_generator = prev_in_generator;
        self.in_pure_function = prev_in_pure;
        self.current_return_type = None;
        self.current_function = prev_function;
        if in_actor {
            self.env.pop_scope();
        }
        self.emit_scope_warnings();
    }

    /// Check trait default method bodies to populate authority side-tables
    /// (e.g. `assign_target_kinds`) for assignments in those bodies.
    /// Trait default methods are not re-checked per impl; they are checked
    /// once here so every assignment target in a default body gets classified.
    pub(super) fn check_trait_defaults(&mut self, td: &TraitDecl) {
        use hew_parser::ast::Visibility;
        for trait_item in &td.items {
            if let TraitItem::Method(method) = trait_item {
                if let Some(body) = &method.body {
                    let fn_decl = FnDecl {
                        attributes: vec![],
                        is_async: false,
                        is_generator: false,
                        visibility: Visibility::Private,
                        is_pure: method.is_pure,
                        name: method.name.clone(),
                        type_params: method.type_params.clone(),
                        params: method.params.clone(),
                        return_type: method.return_type.clone(),
                        where_clause: method.where_clause.clone(),
                        body: body.clone(),
                        doc_comment: None,
                        decl_span: 0..0,
                        fn_span: 0..0,
                    };
                    let qualified = format!("{}::{}", td.name, method.name);
                    self.check_function_as(&fn_decl, &qualified);
                }
            }
        }
    }

    pub(super) fn check_actor(&mut self, ad: &ActorDecl) {
        let actor_ty = Ty::Named {
            name: ad.name.clone(),
            args: vec![],
        };
        let prev_actor_type = self.current_actor_type.replace(actor_ty);
        let prev_actor_fields = std::mem::replace(
            &mut self.current_actor_fields,
            ad.fields.iter().map(|f| f.name.clone()).collect(),
        );

        // Type-check init body if present
        if let Some(init) = &ad.init {
            self.check_actor_init(&ad.name, init, &ad.fields);
        }
        // Type-check terminate body if present
        if let Some(term) = &ad.terminate {
            self.check_actor_terminate(&ad.name, term, &ad.fields);
        }

        for rf in &ad.receive_fns {
            self.check_receive_fn(&ad.name, rf, &ad.fields);
        }
        for method in &ad.methods {
            self.env.push_scope();
            // Bind actor fields directly in scope (bare field access)
            self.bind_actor_fields(&ad.fields);
            let qualified = format!("{}::{}", ad.name, method.name);
            self.check_function_as(method, &qualified);
            self.env.pop_scope();
        }

        self.current_actor_type = prev_actor_type;
        self.current_actor_fields = prev_actor_fields;
    }

    pub(super) fn bind_actor_fields(&mut self, fields: &[FieldDecl]) {
        for field in fields {
            let field_ty = self.resolve_type_expr(&field.ty);
            self.env.define(field.name.clone(), field_ty, true);
        }
    }

    /// Type-check an actor's `init()` block. The init body runs once when
    /// the actor is spawned and has access to actor fields (bare names)
    /// and init parameters, but not to receive fn parameters.
    pub(super) fn check_actor_init(
        &mut self,
        actor_name: &str,
        init: &ActorInit,
        fields: &[FieldDecl],
    ) {
        self.env.push_scope();

        let qualified_name = format!("{actor_name}::init");
        let prev_function = self.current_function.take();
        self.current_function = Some(qualified_name);

        // Bind actor fields directly in scope (bare field access, mutable
        // in init body). Hew uses bare names, not `self.field`.
        self.bind_actor_fields(fields);

        // Bind init parameters
        for p in &init.params {
            let ty = self.resolve_annotation_with_holes(
                &p.ty,
                format!("init parameter `{}` of actor `{actor_name}`", p.name),
            );
            self.env.define(p.name.clone(), ty, p.is_mutable);
        }

        // Init returns unit — no meaningful return type
        self.current_return_type = Some(Ty::Unit);
        self.check_block(&init.body, None);
        self.current_return_type = None;

        self.current_function = prev_function;
        self.env.pop_scope();
    }

    /// Type-check an actor's `terminate { ... }` block. The terminate body
    /// runs when the actor is stopped and has access to actor fields (bare
    /// names) but takes no parameters and cannot return a value.
    pub(super) fn check_actor_terminate(
        &mut self,
        actor_name: &str,
        term: &ActorTerminate,
        fields: &[FieldDecl],
    ) {
        self.env.push_scope();

        let qualified_name = format!("{actor_name}::terminate");
        let prev_function = self.current_function.take();
        self.current_function = Some(qualified_name);

        // Bind actor fields directly in scope (bare field access, mutable
        // so the terminate body can read/modify fields for cleanup).
        self.bind_actor_fields(fields);

        // Terminate returns unit — no meaningful return type
        self.current_return_type = Some(Ty::Unit);
        self.check_block(&term.body, None);
        self.current_return_type = None;

        self.current_function = prev_function;
        self.env.pop_scope();
    }

    /// Validate `#[every(duration)]` attributes on a receive fn.
    pub(super) fn validate_every_attribute(&mut self, rf: &ReceiveFnDecl) {
        let every_attrs: Vec<_> = rf.attributes.iter().filter(|a| a.name == "every").collect();

        if every_attrs.is_empty() {
            // Check for unknown attributes on receive fns.
            for attr in &rf.attributes {
                self.errors.push(TypeError::new(
                    TypeErrorKind::InvalidOperation,
                    attr.span.clone(),
                    format!(
                        "unknown attribute `#[{}]` on receive fn `{}`",
                        attr.name, rf.name
                    ),
                ));
            }
            return;
        }

        if every_attrs.len() > 1 {
            self.errors.push(TypeError::new(
                TypeErrorKind::InvalidOperation,
                every_attrs[1].span.clone(),
                format!(
                    "receive fn `{}` has multiple #[every] attributes; only one is allowed",
                    rf.name
                ),
            ));
            return;
        }

        let attr = every_attrs[0];

        // Must have exactly one duration argument.
        if attr.args.len() != 1 {
            self.errors.push(TypeError::new(
                TypeErrorKind::InvalidOperation,
                attr.span.clone(),
                format!(
                    "#[every] requires exactly one duration argument, e.g. #[every(5s)], got {} arguments",
                    attr.args.len()
                ),
            ));
            return;
        }

        let mut valid_every_duration = false;
        match &attr.args[0] {
            AttributeArg::Duration(ns) => {
                if *ns <= 0 {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::InvalidOperation,
                        attr.span.clone(),
                        "#[every] duration must be positive",
                    ));
                } else {
                    valid_every_duration = true;
                }
            }
            _ => {
                self.errors.push(TypeError::new(
                    TypeErrorKind::InvalidOperation,
                    attr.span.clone(),
                    "#[every] argument must be a duration literal, e.g. #[every(100ms)]",
                ));
            }
        }

        if valid_every_duration {
            self.warn_wasm_limitation(&attr.span, WasmUnsupportedFeature::Timers);
        }

        // Periodic handlers must not have parameters (they receive no message payload).
        if !rf.params.is_empty() {
            self.errors.push(TypeError::new(
                TypeErrorKind::InvalidOperation,
                rf.span.clone(),
                format!(
                    "#[every] receive fn `{}` must not have parameters; periodic handlers are called automatically with no arguments",
                    rf.name
                ),
            ));
        }

        // Periodic handlers must not have a return type (fire-and-forget).
        if rf.return_type.is_some() {
            self.errors.push(TypeError::new(
                TypeErrorKind::InvalidOperation,
                rf.span.clone(),
                format!(
                    "#[every] receive fn `{}` must not have a return type; periodic handlers are fire-and-forget",
                    rf.name
                ),
            ));
        }
    }

    pub(super) fn check_receive_fn(
        &mut self,
        actor_name: &str,
        rf: &ReceiveFnDecl,
        fields: &[FieldDecl],
    ) {
        // Validate #[every(duration)] attribute if present.
        self.validate_every_attribute(rf);

        let prev_in_pure = self.in_pure_function;
        self.in_pure_function = rf.is_pure;
        let prev_in_receive_fn = self.in_receive_fn;
        self.in_receive_fn = true;
        self.env.push_scope();

        // Set current_function so calls within this receive fn are recorded
        // in the call graph (enables dead-code reachability analysis).
        let qualified_name = format!("{}::{}", actor_name, rf.name);
        let prev_function = self.current_function.take();
        self.current_function = Some(qualified_name.clone());

        let mut generic_bindings = std::collections::HashMap::new();
        if let Some(type_params) = &rf.type_params {
            for tp in type_params {
                generic_bindings.insert(
                    tp.name.clone(),
                    Ty::Named {
                        name: tp.name.clone(),
                        args: vec![],
                    },
                );
            }
        }
        if !generic_bindings.is_empty() {
            self.generic_ctx.push(generic_bindings);
        }

        // Bind actor fields directly in scope (bare field access).
        self.bind_actor_fields(fields);

        // Push a separate scope for parameters so shadowing checks can
        // detect collisions with actor field names in the outer scope.
        self.env.push_scope();

        for p in &rf.params {
            self.check_shadowing(&p.name, &p.ty.1);
            let ty = self.resolve_type_expr(&p.ty);
            self.env.define(p.name.clone(), ty, p.is_mutable);
        }

        let declared_ret = if let Some(sig) = self.fn_sigs.get(&qualified_name) {
            if rf.is_generator {
                sig.return_type
                    .as_stream()
                    .cloned()
                    .unwrap_or_else(|| sig.return_type.clone())
            } else {
                sig.return_type.clone()
            }
        } else {
            rf.return_type
                .as_ref()
                .map_or(Ty::Unit, |annotation| self.resolve_type_expr(annotation))
        };
        let expected_ret = if rf.is_generator {
            Ty::Unit
        } else {
            declared_ret.clone()
        };
        // Store declared yields type so Expr::Yield can check against it.
        self.current_return_type = Some(declared_ret);
        let prev_in_generator = self.in_generator;
        self.in_generator = rf.is_generator;

        // Same as check_fn_decl: pass expected_ret so trailing literals coerce
        // correctly, but guard against Ty::Error to avoid suppressing diagnostics.
        let resolved_expected_ret = self.subst.resolve(&expected_ret);
        let block_expected = if matches!(resolved_expected_ret, Ty::Error) {
            None
        } else {
            Some(&expected_ret)
        };
        let actual = self.check_block(&rf.body, block_expected);
        if !matches!(self.subst.resolve(&expected_ret), Ty::Error) {
            self.expect_type(
                &expected_ret,
                &actual,
                &(rf.body
                    .stmts
                    .last()
                    .map_or(rf.span.clone(), |(_, s)| s.clone())),
            );
        }

        self.in_generator = prev_in_generator;
        self.in_receive_fn = prev_in_receive_fn;
        self.in_pure_function = prev_in_pure;
        self.current_return_type = None;
        self.current_function = prev_function;
        if rf.type_params.as_ref().is_some_and(|tp| !tp.is_empty()) {
            self.generic_ctx.pop();
        }
        self.env.pop_scope(); // params scope
        self.env.pop_scope(); // fields scope
    }

    pub(super) fn check_const(&mut self, cd: &ConstDecl, _span: &Span) {
        let expected =
            self.resolve_annotation_with_holes(&cd.ty, format!("constant `{}`", cd.name));
        let actual = self.check_against(&cd.value.0, &cd.value.1, &expected);
        // Store compile-time values for default-width numeric consts so later
        // coercion sites can reuse the original literal kind/value instead of
        // depending on synthesis-time i64/f64 defaults.
        let is_default_int = expected == Ty::I64;
        let is_default_float = expected == Ty::F64;
        if is_default_int {
            if let Some(v) = extract_integer_literal_value(&cd.value.0) {
                self.const_values
                    .insert(cd.name.clone(), ConstValue::Integer(v));
            }
        } else if is_default_float {
            if let Some(v) = extract_float_literal_value(&cd.value.0) {
                self.const_values
                    .insert(cd.name.clone(), ConstValue::Float(v));
            }
        }
        self.env.define(cd.name.clone(), actual, false);
    }

    pub(super) fn check_impl(&mut self, id: &ImplDecl, span: &Span) {
        if let TypeExpr::Named {
            name: type_name,
            type_args,
        } = &id.target_type.0
        {
            let target_is_struct = self
                .lookup_type_def(type_name)
                .is_some_and(|td| td.kind == TypeDefKind::Struct);
            // Orphan rule check: if implementing a trait, either the type or the
            // trait must be defined in the current compilation unit.
            if let Some(tb) = &id.trait_bound {
                let type_is_local = self.local_type_defs.contains(type_name);
                let trait_is_local = self.local_trait_defs.contains(&tb.name);
                if !type_is_local && !trait_is_local {
                    self.warnings.push(TypeError {
                        severity: crate::error::Severity::Warning,
                        kind: TypeErrorKind::OrphanImpl,
                        span: span.clone(),
                        message: format!(
                            "impl `{}` for `{type_name}`: neither the trait nor the type is defined in this module",
                            tb.name
                        ),
                        notes: vec![],
                        suggestions: vec![
                            "define the trait or the type in this module".to_string(),
                            "this may be disallowed in a future version (orphan rule)".to_string(),
                        ],
                        source_module: self.current_module.clone(),
                    });
                }
            }

            // Bind impl-level type params (e.g. T in `impl<T> Wrapper<T>`)
            // so method bodies can reference them.
            let mut generic_bindings = std::collections::HashMap::new();
            if let Some(tps) = &id.type_params {
                for tp in tps {
                    generic_bindings.insert(
                        tp.name.clone(),
                        Ty::Named {
                            name: tp.name.clone(),
                            args: vec![],
                        },
                    );
                }
            }
            let pushed_generic = !generic_bindings.is_empty();
            if pushed_generic {
                self.generic_ctx.push(generic_bindings);
            }

            // Set current_self_type for resolving `Self` in parameters
            let prev_self_type = self.current_self_type.take();
            let self_type_args: Vec<Ty> = type_args
                .as_ref()
                .map(|args| {
                    args.iter()
                        .map(|type_arg| self.resolve_type_expr(type_arg))
                        .collect()
                })
                .unwrap_or_default();
            self.current_self_type = Some((type_name.clone(), self_type_args.clone()));
            let scope_pushed = self.enter_impl_scope(id, span, Some(type_name.as_str()), true);

            for method in &id.methods {
                if target_is_struct {
                    // Only the first parameter can be the receiver; checking all
                    // params would false-positive on a non-receiver whose type
                    // happens to match the impl target.
                    if let Some(self_param) = method
                        .params
                        .first()
                        .filter(|param| self.is_receiver_param(param) && param.is_mutable)
                    {
                        self.report_error_with_suggestions(
                            TypeErrorKind::MutabilityError,
                            &self_param.ty.1,
                            "`var self` in struct impl methods has no effect — struct methods receive self by value".to_string(),
                            vec![
                                "return a modified copy of the receiver instead".to_string(),
                                "convert this type to an actor if you need mutable shared state".to_string(),
                            ],
                        );
                    }
                }
                self.env.push_scope();
                // Use qualified name (e.g. Connection::close) so the fn_sigs
                // lookup finds the impl method, not a same-named builtin or
                // inlined function from another module.
                let qualified = format!("{type_name}::{}", method.name);
                self.check_function_as(method, &qualified);
                self.env.pop_scope();
            }

            // Restore previous self type
            self.current_self_type = prev_self_type;
            if scope_pushed {
                self.exit_impl_scope();
            }
            if pushed_generic {
                self.generic_ctx.pop();
            }
        }
    }
}
