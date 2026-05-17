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
            // All of these are fully handled during earlier registration passes
            // and require no second-pass body checking.  Record declarations
            // specifically are registered by `register_record_decl`; they have
            // no method bodies, variants, or wire attributes in v0.5.
            Item::Record(_)
            | Item::Import(_)
            | Item::TypeDecl(_)
            | Item::TypeAlias(_)
            | Item::Wire(_)
            | Item::ExternBlock(_) => {}
            Item::Supervisor(sd) => self.check_supervisor(sd, span),
        }
    }

    /// Validate a `supervisor` declaration at the structural level.
    ///
    /// Checks:
    /// - Duplicate child names (`E_SUPERVISOR_DUPLICATE_CHILD`).
    /// - `wired_to` keys each reference a declared sibling (`E_SUPERVISOR_WIRED_TO_UNKNOWN_SIBLING`).
    /// - `wired_to` sibling ref type matches the dependent actor's init param type
    ///   (`E_SUPERVISOR_WIRED_TO_TYPE_MISMATCH`).
    /// - No `wired_to` dependency cycles (`E_SUPERVISOR_WIRED_CYCLE`).
    /// - `simple_one_for_one` strategy requires exactly one `pool` child and no `child` decls
    ///   (`E_SUPERVISOR_STRATEGY_POOL_MISMATCH`).
    /// - Any other strategy rejects `pool` decls (`E_SUPERVISOR_STRATEGY_POOL_MISMATCH`).
    pub(super) fn check_supervisor(&mut self, sd: &SupervisorDecl, span: &Span) {
        // ── 1. Duplicate child names ─────────────────────────────────────────
        self.check_supervisor_duplicate_children(sd, span);

        // ── 2. Strategy / pool consistency ──────────────────────────────────
        self.check_supervisor_strategy_pool(sd, span);

        // ── 3. wired_to key resolution + type compatibility ──────────────────
        self.check_supervisor_wired_to(sd, span);

        // ── 4. Dependency cycle detection ────────────────────────────────────
        self.check_supervisor_wired_to_cycles(sd, span);
    }

    fn check_supervisor_duplicate_children(&mut self, sd: &SupervisorDecl, span: &Span) {
        let mut seen: std::collections::HashMap<&str, usize> = std::collections::HashMap::new();
        for (i, child) in sd.children.iter().enumerate() {
            match seen.entry(child.name.as_str()) {
                std::collections::hash_map::Entry::Vacant(e) => {
                    e.insert(i);
                }
                std::collections::hash_map::Entry::Occupied(_) => {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::DuplicateDefinition,
                        span.clone(),
                        format!(
                            "E_SUPERVISOR_DUPLICATE_CHILD: supervisor `{}` declares child `{}` \
                             more than once; child names must be unique within a supervisor",
                            sd.name, child.name
                        ),
                    ));
                }
            }
        }
    }

    fn check_supervisor_strategy_pool(&mut self, sd: &SupervisorDecl, span: &Span) {
        let pool_children: Vec<&ChildSpec> = sd.children.iter().filter(|c| c.is_pool).collect();
        let static_children: Vec<&ChildSpec> = sd.children.iter().filter(|c| !c.is_pool).collect();

        let is_soo = matches!(sd.strategy, Some(SupervisorStrategy::SimpleOneForOne));

        if is_soo {
            // simple_one_for_one: exactly one pool child, no static children.
            if pool_children.len() != 1 {
                self.errors.push(TypeError::new(
                    TypeErrorKind::InvalidOperation,
                    span.clone(),
                    format!(
                        "E_SUPERVISOR_STRATEGY_POOL_MISMATCH: supervisor `{}` uses \
                         `simple_one_for_one` strategy but has {} `pool` child declaration(s); \
                         exactly one `pool` child is required",
                        sd.name,
                        pool_children.len()
                    ),
                ));
            }
            if !static_children.is_empty() {
                let names: Vec<&str> = static_children.iter().map(|c| c.name.as_str()).collect();
                self.errors.push(TypeError::new(
                    TypeErrorKind::InvalidOperation,
                    span.clone(),
                    format!(
                        "E_SUPERVISOR_STRATEGY_POOL_MISMATCH: supervisor `{}` uses \
                         `simple_one_for_one` strategy but has `child` declarations ({}); \
                         `simple_one_for_one` supervisors may only contain a single `pool` child",
                        sd.name,
                        names.join(", ")
                    ),
                ));
            }
        } else if !pool_children.is_empty() {
            // Any non-simple_one_for_one strategy (or no strategy specified) rejects pool children.
            let names: Vec<&str> = pool_children.iter().map(|c| c.name.as_str()).collect();
            let strategy_label = sd.strategy.map_or("default (one_for_one)", |s| match s {
                SupervisorStrategy::OneForOne => "one_for_one",
                SupervisorStrategy::OneForAll => "one_for_all",
                SupervisorStrategy::RestForOne => "rest_for_one",
                SupervisorStrategy::SimpleOneForOne => unreachable!(),
            });
            self.errors.push(TypeError::new(
                TypeErrorKind::InvalidOperation,
                span.clone(),
                format!(
                    "E_SUPERVISOR_STRATEGY_POOL_MISMATCH: supervisor `{}` uses `{}` strategy \
                     but has `pool` child declarations ({}); `pool` children require \
                     `simple_one_for_one` strategy",
                    sd.name,
                    strategy_label,
                    names.join(", ")
                ),
            ));
        }
    }

    fn check_supervisor_wired_to(&mut self, sd: &SupervisorDecl, span: &Span) {
        // Build a sibling-name → actor-type map for fast resolution.
        let sibling_types: std::collections::HashMap<&str, &str> = sd
            .children
            .iter()
            .map(|c| (c.name.as_str(), c.actor_type.as_str()))
            .collect();

        for child in &sd.children {
            let Some(wired_to) = &child.wired_to else {
                continue;
            };

            for (param_key, sibling_name) in wired_to {
                // ── Key resolution: sibling must exist ──────────────────────
                let Some(&sibling_type) = sibling_types.get(sibling_name.as_str()) else {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::InvalidOperation,
                        span.clone(),
                        format!(
                            "E_SUPERVISOR_WIRED_TO_UNKNOWN_SIBLING: in supervisor `{}`, \
                             child `{}` has `wired_to: {{ {param_key}: {sibling_name} }}` but \
                             `{sibling_name}` is not a declared child of this supervisor",
                            sd.name, child.name
                        ),
                    ));
                    continue;
                };

                // Self-reference is a degenerate cycle — caught separately but
                // the unknown-sibling check fires first if the name doesn't exist.
                // If the child wires itself, it's a cycle; we flag it during cycle
                // detection rather than here to avoid double-reporting.
                if sibling_name.as_str() == child.name.as_str() {
                    // Will be caught by cycle detection.
                    continue;
                }

                // ── Type compatibility ──────────────────────────────────────
                // The dependent child's actor init must have a param named `param_key`
                // with type `ActorRef<sibling_type>`.
                self.check_supervisor_wired_to_type_compat(
                    &sd.name,
                    &child.name,
                    &child.actor_type,
                    param_key,
                    sibling_type,
                    span,
                );
            }
        }
    }

    /// Verify that `dependent_actor`'s init has a parameter `param_key` typed
    /// `ActorRef<sibling_type>`. Emits `E_SUPERVISOR_WIRED_TO_TYPE_MISMATCH` on failure.
    ///
    /// If the actor type is completely unknown (not registered at all), the check
    /// is skipped — a separate undefined-type diagnostic covers that case.
    /// Actors registered with no `init` block have an empty param list; the
    /// "no parameter named X" branch below fires for those.
    fn check_supervisor_wired_to_type_compat(
        &mut self,
        supervisor_name: &str,
        dependent_child_name: &str,
        dependent_actor_type: &str,
        param_key: &str,
        expected_sibling_type: &str,
        span: &Span,
    ) {
        let Some(params) = self.actor_init_params.get(dependent_actor_type).cloned() else {
            // Actor not registered at all (unknown type). A separate undefined-type
            // diagnostic covers the missing actor. Skip here to avoid double-reporting.
            return;
        };

        let Some(param) = params.iter().find(|(name, _, _)| name == param_key) else {
            self.errors.push(TypeError::new(
                TypeErrorKind::InvalidOperation,
                span.clone(),
                format!(
                    "E_SUPERVISOR_WIRED_TO_TYPE_MISMATCH: in supervisor `{supervisor_name}`, \
                     child `{dependent_child_name}` (`{dependent_actor_type}`) is wired via key \
                     `{param_key}` but `{dependent_actor_type}::init` has no parameter named \
                     `{param_key}`"
                ),
            ));
            return;
        };

        let (_, outer_type, first_inner_type) = param;

        // Expected: outer = "ActorRef", inner = expected_sibling_type.
        let type_ok = outer_type == "ActorRef"
            && first_inner_type
                .as_deref()
                .is_some_and(|t| t == expected_sibling_type);

        if !type_ok {
            let actual_type = if first_inner_type.is_some() {
                format!(
                    "{}<{}>",
                    outer_type,
                    first_inner_type.as_deref().unwrap_or("?")
                )
            } else {
                outer_type.clone()
            };
            self.errors.push(TypeError::new(
                TypeErrorKind::InvalidOperation,
                span.clone(),
                format!(
                    "E_SUPERVISOR_WIRED_TO_TYPE_MISMATCH: in supervisor `{supervisor_name}`, \
                     child `{dependent_child_name}` wires `{param_key}` to sibling of type \
                     `{expected_sibling_type}`, but `{dependent_actor_type}::init` parameter \
                     `{param_key}` has type `{actual_type}` (expected `ActorRef<{expected_sibling_type}>`)"
                ),
            ));
        }
    }

    /// Topological sort of children by `wired_to` deps. If a cycle is found,
    /// emits `E_SUPERVISOR_WIRED_CYCLE`. Self-references are also rejected here.
    fn check_supervisor_wired_to_cycles(&mut self, sd: &SupervisorDecl, span: &Span) {
        use std::collections::VecDeque;

        // Build an adjacency list: child_name → set of child_names it depends on.
        let child_names: std::collections::HashSet<&str> =
            sd.children.iter().map(|c| c.name.as_str()).collect();

        // Only track deps that actually exist as siblings (unknown siblings already reported).
        // Maps child_name → list of siblings it depends on (via wired_to values).
        let deps: std::collections::HashMap<&str, Vec<&str>> = sd
            .children
            .iter()
            .map(|c| {
                let dep_list: Vec<&str> = c
                    .wired_to
                    .as_ref()
                    .map(|wt| {
                        wt.values()
                            .filter(|s| child_names.contains(s.as_str()))
                            .map(std::string::String::as_str)
                            .collect()
                    })
                    .unwrap_or_default();
                (c.name.as_str(), dep_list)
            })
            .collect();

        // Kahn's algorithm for cycle detection.
        // Dependency graph: if child A wired_to B, edge A→B means A depends on B.
        // For Kahn's: build reverse edges (dep → dependents) and count in-degrees
        // (how many deps each child still has).
        let mut reverse_deps: std::collections::HashMap<&str, Vec<&str>> =
            child_names.iter().map(|&n| (n, vec![])).collect();
        let mut in_degree: std::collections::HashMap<&str, usize> =
            child_names.iter().map(|&n| (n, 0usize)).collect();

        for (&child, dep_list) in &deps {
            for &dep in dep_list {
                // child depends on dep → dep must come before child.
                // In topo-sort: dep→child edge; dep has in-degree += 0; child in-degree += 1.
                if let Some(v) = reverse_deps.get_mut(dep) {
                    v.push(child);
                }
                if let Some(d) = in_degree.get_mut(child) {
                    *d += 1;
                }
            }
        }

        let mut queue: VecDeque<&str> = in_degree
            .iter()
            .filter(|(_, &d)| d == 0)
            .map(|(&n, _)| n)
            .collect();

        let mut visited = 0usize;
        while let Some(node) = queue.pop_front() {
            visited += 1;
            if let Some(dependents) = reverse_deps.get(node) {
                for &dep in &dependents.clone() {
                    if let Some(d) = in_degree.get_mut(dep) {
                        *d -= 1;
                        if *d == 0 {
                            queue.push_back(dep);
                        }
                    }
                }
            }
        }

        if visited < child_names.len() {
            // Cycle exists. Collect the names in the cycle.
            let cycle_nodes: Vec<&str> = in_degree
                .iter()
                .filter(|(_, &d)| d > 0)
                .map(|(&n, _)| n)
                .collect();
            let mut sorted_cycle = cycle_nodes.clone();
            sorted_cycle.sort_unstable();
            self.errors.push(TypeError::new(
                TypeErrorKind::InvalidOperation,
                span.clone(),
                format!(
                    "E_SUPERVISOR_WIRED_CYCLE: supervisor `{}` has a `wired_to` dependency \
                     cycle involving children: {}; children must form a DAG so start order \
                     can be determined",
                    sd.name,
                    sorted_cycle.join(", ")
                ),
            ));
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

        // Diagnostic-only stack-allocation hint pass — runs unconditionally on
        // every function/actor body. Output is consumed by the CLI's
        // `--show-stack-hints` printer; never affects exit code or codegen.
        //
        // The walk is kept always-on rather than gated on a flag because its
        // cost is O(bindings): a recursive AST walk with one HashMap lookup and
        // one enum match per binding, pushing to a Vec only for heap-class
        // matches. The results are stored in `TypeCheckOutput::stack_hints` and
        // discarded by the caller when `--show-stack-hints` is not set. At the
        // scale of real Hew programs (< 10k bindings) this is unmeasurable
        // noise compared to unification and constraint solving. If a future
        // large-scale benchmark shows otherwise, gate it via a flag threaded
        // through `FrontendOptions`.
        //
        // See `classify_stack_hints` in expressions.rs for the slice plan.
        self.classify_stack_hints(fd);

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

        // Record the per-actor arena cap from `#[max_heap(N)]` if present.
        // The parser already converted suffixes (kb, mb) to bytes; we record
        // `None`-absent as "no annotation" (unbounded) and `Some(cap)` as the
        // caller-supplied cap. Codegen reads `actor_max_heap` to decide between
        // `hew_arena_new` (unbounded) and `hew_arena_new_with_cap(cap)` (bounded).
        if let Some(cap) = ad.max_heap_bytes {
            self.actor_max_heap.insert(ad.name.clone(), cap);
        }

        // Type-check init body if present
        if let Some(init) = &ad.init {
            self.check_actor_init(&ad.name, init, &ad.fields);
        }

        for rf in &ad.receive_fns {
            self.check_receive_fn(&ad.name, rf, &ad.fields);
        }

        // Separate lifecycle-hook fns from regular methods. Hooks carry
        // one of `#[on(start)]`, `#[on(stop)]`, `#[on(crash)]`, or
        // `#[on(upgrade)]`. The start/stop variants have a fixed
        // signature checked here; the crash/upgrade variants are
        // recognised in E1 but their signature shape is owned by E2
        // (failure-philosophy phase 2). Regular methods carry no
        // attributes and are checked as ordinary actor methods.
        //
        // `#[on(start)]` is at most once per actor; `#[on(stop)]` may
        // appear multiple times (lexical declaration order is the
        // run order — see HEW-SPEC-2026 §9.1.2).
        self.check_actor_methods(ad);

        self.current_actor_type = prev_actor_type;
        self.current_actor_fields = prev_actor_fields;
    }

    /// Walk an actor's `methods` list, dispatching each fn to either the
    /// lifecycle-hook validator or the regular-method validator based on
    /// its `#[on(<event>)]` annotation. Tracks `#[on(start)]` uniqueness
    /// across the loop.
    fn check_actor_methods(&mut self, ad: &ActorDecl) {
        let mut on_start_seen: Option<Span> = None;
        for method in &ad.methods {
            let hook_attrs: Vec<_> = method
                .attributes
                .iter()
                .filter(|a| a.name.as_str() == "on")
                .collect();

            if hook_attrs.is_empty() {
                self.env.push_scope();
                self.bind_actor_fields(&ad.fields);
                let qualified = format!("{}::{}", ad.name, method.name);
                self.check_function_as(method, &qualified);
                self.env.pop_scope();
                continue;
            }

            if hook_attrs.len() > 1 {
                self.errors.push(TypeError::new(
                    TypeErrorKind::InvalidOperation,
                    hook_attrs[1].span.clone(),
                    format!(
                        "function `{}` in actor `{}` cannot carry more than one \
                         lifecycle-hook annotation; combine the bodies or split into separate fns",
                        method.name, ad.name
                    ),
                ));
            }
            let hook_attr = hook_attrs[0];

            let Some(hook_kind_str) = self.resolve_on_hook_kind(&ad.name, &method.name, hook_attr)
            else {
                continue;
            };

            if hook_kind_str == "start" {
                if let Some(prev) = &on_start_seen {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::InvalidOperation,
                        hook_attr.span.clone(),
                        format!(
                            "actor `{}` declares more than one `#[on(start)]` hook; \
                             only one is allowed (see prior at {}..{})",
                            ad.name, prev.start, prev.end
                        ),
                    ));
                } else {
                    on_start_seen = Some(hook_attr.span.clone());
                }
            }

            // `#[on(crash)]` and `#[on(upgrade)]` diverge from start/stop
            // signature-wise: crash takes a `PanicInfo` parameter and returns
            // `CrashAction`; upgrade is a reserved marker with the same
            // no-params/unit shape as stop but no runtime invocation in v0.5
            // (tracked in #1817).
            match hook_kind_str {
                "crash" => {
                    self.check_crash_hook(&ad.name, method, &ad.fields);
                    continue;
                }
                "upgrade" => {
                    self.check_upgrade_hook(&ad.name, method, &ad.fields);
                    continue;
                }
                _ => {}
            }

            // Validate signature and body. Hooks bind actor fields in
            // scope (bare names) and have no parameters beyond `self`.
            let display_kind = format!("on({hook_kind_str})");
            self.check_lifecycle_hook(&ad.name, method, &display_kind, &ad.fields);
        }
    }

    /// Resolve and validate the event identifier inside `#[on(<event>)]`.
    /// Pushes a diagnostic and returns `None` if the event is missing,
    /// unknown, or carries extra arguments that the event does not accept.
    fn resolve_on_hook_kind<'a>(
        &mut self,
        actor_name: &str,
        method_name: &str,
        hook_attr: &'a hew_parser::ast::Attribute,
    ) -> Option<&'a str> {
        let hook_kind = hook_attr.args.first().map(AttributeArg::as_str);
        match hook_kind {
            None | Some("") => {
                self.errors.push(TypeError::new(
                    TypeErrorKind::InvalidOperation,
                    hook_attr.span.clone(),
                    format!(
                        "`#[on]` on `{actor_name}::{method_name}` requires a hook kind argument; \
                         valid hook kinds are: start, stop, crash, upgrade"
                    ),
                ));
                return None;
            }
            Some("start" | "stop" | "crash" | "upgrade") => {}
            Some(unknown) => {
                self.errors.push(TypeError::new(
                    TypeErrorKind::InvalidOperation,
                    hook_attr.span.clone(),
                    format!(
                        "`#[on({unknown})]` on `{actor_name}::{method_name}` is not a recognised \
                         lifecycle hook; valid hook kinds are: start, stop, crash, upgrade"
                    ),
                ));
                return None;
            }
        }
        let hook_kind_str = hook_kind.unwrap();

        // Reject `#[on(crash, …)]` / `#[on(upgrade, …)]` with extra arguments.
        // start/stop already reject extra args via `check_lifecycle_hook`'s
        // signature checks; for crash/upgrade we validate the attribute
        // shape here because their signature/body checking is owned by E2.
        if matches!(hook_kind_str, "crash" | "upgrade") && hook_attr.args.len() > 1 {
            self.errors.push(TypeError::new(
                TypeErrorKind::InvalidOperation,
                hook_attr.span.clone(),
                format!(
                    "`#[on({hook_kind_str})]` on `{actor_name}::{method_name}` does not accept \
                     extra arguments"
                ),
            ));
            return None;
        }

        Some(hook_kind_str)
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

    /// Type-check an actor lifecycle hook (`#[on(start)]` or `#[on(stop)]`).
    ///
    /// Required shape (HEW-SPEC-2026 §9.1.2 rules 2-4):
    /// - no parameters (actor fields are in scope by bare name, same as `init { }`)
    /// - no type parameters
    /// - no `where` clause
    /// - not `pure`
    /// - return type `()` (omitted or explicitly unit)
    ///
    /// Hooks bind actor fields as bare names in scope (mutable) so the
    /// body can read and modify fields exactly like an `init { }` body.
    /// Diagnostics emitted here cover both signature shape (rejected
    /// statically) and body type-checking (delegated to `check_block`).
    pub(super) fn check_lifecycle_hook(
        &mut self,
        actor_name: &str,
        hook: &FnDecl,
        hook_kind: &str,
        fields: &[FieldDecl],
    ) {
        // ── Signature validation ────────────────────────────────────────
        if hook.is_pure {
            self.errors.push(TypeError::new(
                TypeErrorKind::InvalidOperation,
                hook.decl_span.clone(),
                format!(
                    "lifecycle hook `#[{hook_kind}]` on `{actor_name}::{}` cannot be `pure`",
                    hook.name
                ),
            ));
        }
        if hook.type_params.as_ref().is_some_and(|tps| !tps.is_empty()) {
            self.errors.push(TypeError::new(
                TypeErrorKind::InvalidOperation,
                hook.decl_span.clone(),
                format!(
                    "lifecycle hook `#[{hook_kind}]` on `{actor_name}::{}` cannot \
                     have type parameters",
                    hook.name
                ),
            ));
        }
        if hook.where_clause.is_some() {
            self.errors.push(TypeError::new(
                TypeErrorKind::InvalidOperation,
                hook.decl_span.clone(),
                format!(
                    "lifecycle hook `#[{hook_kind}]` on `{actor_name}::{}` cannot \
                     have a `where` clause",
                    hook.name
                ),
            ));
        }
        if let Some(rt) = &hook.return_type {
            // Only `()` is permitted; reject any explicit return type.
            // `resolve_type_expr` returns `Ty::Unit` for the unit type
            // and anything else for everything else.
            let ty = self.resolve_type_expr(rt);
            if !matches!(ty, Ty::Unit) {
                self.errors.push(TypeError::new(
                    TypeErrorKind::InvalidOperation,
                    rt.1.clone(),
                    format!(
                        "lifecycle hook `#[{hook_kind}]` on `{actor_name}::{}` must \
                         return `()` (the unit type); declared return type rejected",
                        hook.name
                    ),
                ));
            }
        }

        // Parameter shape: hooks take no explicit parameters. Hew actor
        // methods bind actor fields as bare names in scope (mirroring
        // `init { }`); a hook reaches into mutable actor state the same
        // way. Reject any parameter list — the user's intent is almost
        // certainly to use a `self`-style receiver, which is not how
        // Hew actor methods work.
        if !hook.params.is_empty() {
            self.errors.push(TypeError::new(
                TypeErrorKind::InvalidOperation,
                hook.decl_span.clone(),
                format!(
                    "lifecycle hook `#[{hook_kind}]` on `{actor_name}::{}` must take \
                     no parameters; actor fields are in scope by bare name (see \
                     `init {{ }}` for the same convention)",
                    hook.name
                ),
            ));
        }

        // ── Body checking ───────────────────────────────────────────────
        self.env.push_scope();

        let qualified_name = format!("{actor_name}::{}", hook.name);
        let prev_function = self.current_function.take();
        self.current_function = Some(qualified_name);

        // Bind actor fields directly in scope as bare names (mutable so
        // the hook can read/modify them).
        self.bind_actor_fields(fields);

        self.current_return_type = Some(Ty::Unit);
        self.check_block(&hook.body, None);
        self.current_return_type = None;

        self.current_function = prev_function;
        self.env.pop_scope();
    }

    /// Type-check an actor `#[on(crash)]` hook.
    ///
    /// Signature shape (failure-philosophy plan E2, Q45/A22, Q46/A23):
    /// - exactly one parameter `info: PanicInfo` (the runtime supplies
    ///   the int-tag payload — string fields wait on the spine-widening
    ///   lane).
    /// - return type `CrashAction` (variants `Restart | Escalate | Kill`;
    ///   the supervisor consults but honours its own budget rules).
    /// - not `pure`, no type parameters, no `where` clause.
    ///
    /// `PanicInfo` and `CrashAction` are provided by `std/failure.hew`
    /// (also pre-bound via `register_builtin_failure_surface` for inline
    /// tests).  Body type-checking binds actor fields as bare names in
    /// scope, same idiom as `init { }` / `#[on(start)]` / `#[on(stop)]`.
    ///
    /// Runtime invocation of this hook is owned by failure-philosophy
    /// slice E3.  This slice validates the signature shape so the
    /// compiled actor method symbol (`<Actor>::on_crash`, emitted via
    /// the existing actor-method serialize path) has the contract the
    /// runtime will rely on.
    /// Reject `pure`, generic, and `where`-clause modifiers shared by every
    /// `#[on(<event>)]` lifecycle hook.  Extracted from `check_crash_hook`
    /// because the same triad applies to future event-specific validators
    /// and keeps the per-event entry under the clippy `too_many_lines`
    /// threshold.
    fn reject_hook_modifier_set(&mut self, actor_name: &str, hook: &FnDecl, hook_kind: &str) {
        if hook.is_pure {
            self.errors.push(TypeError::new(
                TypeErrorKind::InvalidOperation,
                hook.decl_span.clone(),
                format!(
                    "lifecycle hook `#[{hook_kind}]` on `{actor_name}::{}` cannot be `pure`",
                    hook.name
                ),
            ));
        }
        if hook.type_params.as_ref().is_some_and(|tps| !tps.is_empty()) {
            self.errors.push(TypeError::new(
                TypeErrorKind::InvalidOperation,
                hook.decl_span.clone(),
                format!(
                    "lifecycle hook `#[{hook_kind}]` on `{actor_name}::{}` cannot \
                     have type parameters",
                    hook.name
                ),
            ));
        }
        if hook.where_clause.is_some() {
            self.errors.push(TypeError::new(
                TypeErrorKind::InvalidOperation,
                hook.decl_span.clone(),
                format!(
                    "lifecycle hook `#[{hook_kind}]` on `{actor_name}::{}` cannot \
                     have a `where` clause",
                    hook.name
                ),
            ));
        }
    }

    /// Validate the parameter list of a `#[on(crash)]` hook: exactly one
    /// parameter typed `PanicInfo`.  Diagnostics live here rather than in
    /// `check_crash_hook` to keep that entry under the clippy line limit.
    fn check_crash_hook_param(&mut self, actor_name: &str, hook: &FnDecl, hook_kind: &str) {
        match hook.params.as_slice() {
            [p] => {
                let pty = self.resolve_type_expr(&p.ty);
                let is_panic_info = matches!(
                    &pty,
                    Ty::Named { name, args } if name == "PanicInfo" && args.is_empty()
                );
                if !is_panic_info {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::InvalidOperation,
                        p.ty.1.clone(),
                        format!(
                            "lifecycle hook `#[{hook_kind}]` on `{actor_name}::{}` parameter \
                             must have type `PanicInfo` (from `std::failure`)",
                            hook.name
                        ),
                    ));
                }
            }
            other => {
                self.errors.push(TypeError::new(
                    TypeErrorKind::InvalidOperation,
                    hook.decl_span.clone(),
                    format!(
                        "lifecycle hook `#[{hook_kind}]` on `{actor_name}::{}` must take \
                         exactly one parameter `info: PanicInfo`; got {} parameter(s)",
                        hook.name,
                        other.len()
                    ),
                ));
            }
        }
    }

    /// Validate and resolve the declared return type of a `#[on(crash)]`
    /// hook.  Returns the resolved `Ty` (falling back to a bare
    /// `Ty::Named("CrashAction")` when the user omitted the return type)
    /// so the body checker has a target type for the trailing expression.
    fn check_crash_hook_return_type(
        &mut self,
        actor_name: &str,
        hook: &FnDecl,
        hook_kind: &str,
    ) -> Ty {
        if let Some(rt) = &hook.return_type {
            let ty = self.resolve_type_expr(rt);
            if !matches!(&ty, Ty::Named { name, args } if name == "CrashAction" && args.is_empty())
            {
                self.errors.push(TypeError::new(
                    TypeErrorKind::InvalidOperation,
                    rt.1.clone(),
                    format!(
                        "lifecycle hook `#[{hook_kind}]` on `{actor_name}::{}` must \
                         return `CrashAction` (from `std::failure`)",
                        hook.name
                    ),
                ));
            }
            ty
        } else {
            self.errors.push(TypeError::new(
                TypeErrorKind::InvalidOperation,
                hook.decl_span.clone(),
                format!(
                    "lifecycle hook `#[{hook_kind}]` on `{actor_name}::{}` must declare \
                     a return type of `CrashAction` (from `std::failure`)",
                    hook.name
                ),
            ));
            Ty::Named {
                name: "CrashAction".to_string(),
                args: vec![],
            }
        }
    }

    pub(super) fn check_crash_hook(
        &mut self,
        actor_name: &str,
        hook: &FnDecl,
        fields: &[FieldDecl],
    ) {
        let hook_kind = "on(crash)";

        self.reject_hook_modifier_set(actor_name, hook, hook_kind);
        self.check_crash_hook_param(actor_name, hook, hook_kind);
        let return_ty = self.check_crash_hook_return_type(actor_name, hook, hook_kind);

        // ── Body checking ───────────────────────────────────────────────
        self.env.push_scope();

        let qualified_name = format!("{actor_name}::{}", hook.name);
        let prev_function = self.current_function.take();
        self.current_function = Some(qualified_name);

        // Bind actor fields as bare names (mutable), then the `info`
        // parameter on top of them.  Field-shadowing by the param name
        // is intentionally permitted — same precedent as `init` and
        // receive fn parameters (HEW-SPEC-2026 §9.1.1).
        self.bind_actor_fields(fields);
        if let Some(p) = hook.params.first() {
            let pty = self.resolve_type_expr(&p.ty);
            self.env.define(p.name.clone(), pty, p.is_mutable);
        }

        self.current_return_type = Some(return_ty);
        self.check_block(&hook.body, None);
        self.current_return_type = None;

        self.current_function = prev_function;
        self.env.pop_scope();
    }

    /// Type-check an actor `#[on(upgrade)]` hook.
    ///
    /// v0.5 reserves the surface but defers runtime invocation. The hook
    /// must therefore have the minimal shape that the runtime can ignore
    /// safely: no parameters, return type `()` — identical to the
    /// `#[on(stop)]` shape today.  Runtime invocation, state-handoff,
    /// and WASM hot-reload land later; tracked in #1817.
    ///
    /// `WASM-TODO(#1817)`: `#[on(upgrade)]` codegen and runtime
    /// invocation are deferred to a follow-up slice (failure-philosophy
    /// E3+).  The actor method serializes as a regular function symbol
    /// through the existing actor-method path; no invocation site exists
    /// in the v0.5 runtime or WASM scheduler.
    pub(super) fn check_upgrade_hook(
        &mut self,
        actor_name: &str,
        hook: &FnDecl,
        fields: &[FieldDecl],
    ) {
        // Same shape constraints as `#[on(stop)]`, expressed inline so the
        // diagnostic carries the `on(upgrade)` kind string.
        self.check_lifecycle_hook(actor_name, hook, "on(upgrade)", fields);
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
