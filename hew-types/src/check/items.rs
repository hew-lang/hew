#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;

impl Checker {
    fn registered_fn_type_param_scope(&self, fn_name: &str) -> TypeParamScope {
        self.fn_sigs
            .get(fn_name)
            .map(|sig| {
                TypeParamScope::new(
                    sig.type_param_bounds.clone(),
                    self.fn_type_param_assoc_bindings
                        .get(fn_name)
                        .cloned()
                        .unwrap_or_default(),
                )
            })
            .unwrap_or_default()
    }

    pub(super) fn check_item(&mut self, item: &Item, span: &Span) {
        match item {
            Item::Function(fd) => self.check_function(fd),
            Item::Actor(ad) => {
                if !crate::ty::is_reserved_type_name(&ad.name) {
                    if matches!(
                        ad.overflow_policy.as_ref(),
                        Some(hew_parser::ast::OverflowPolicy::Coalesce {
                            fallback: Some(hew_parser::ast::OverflowFallback::Block),
                            ..
                        })
                    ) {
                        self.report_error(
                            TypeErrorKind::InvalidOperation,
                            span,
                            "coalesce fallback 'block' is unsupported: use top-level `overflow block` for cooperative backpressure"
                                .to_string(),
                        );
                    }
                    self.check_actor(ad);
                }
            }
            Item::Const(cd) => self.check_const(cd, span),
            Item::Impl(id) => self.check_impl(id, span),
            Item::Trait(td) => {
                if !crate::ty::is_reserved_type_name(&td.name) {
                    self.check_trait_defaults(td);
                }
            }
            // All of these are fully handled during earlier registration passes
            // and require no second-pass body checking.  Record declarations
            // specifically are registered by `register_record_decl`; they have
            // no method bodies, variants, or wire attributes in v0.5.
            // Machines are normalized into ordinary declarations before this
            // pass runs, so no machine declaration reaches item checking.
            Item::Record(_)
            | Item::Import(_)
            | Item::TypeDecl(_)
            | Item::TypeAlias(_)
            | Item::Machine(_)
            | Item::ExternBlock(_) => {}
            Item::Supervisor(sd) => self.check_supervisor(sd, span),
        }
    }

    /// Validate a `supervisor` declaration at the structural level.
    ///
    /// Checks:
    /// - Every child type resolves lexically to an actor
    ///   (`E_SUPERVISOR_UNKNOWN_CHILD_ACTOR` /
    ///   `E_SUPERVISOR_CHILD_NOT_SUPERVISABLE`).
    /// - Duplicate child names (`E_SUPERVISOR_DUPLICATE_CHILD`).
    /// - `wired_to` keys each reference a declared sibling (`E_SUPERVISOR_WIRED_TO_UNKNOWN_SIBLING`).
    /// - `wired_to` sibling ref type matches the dependent actor's init param type
    ///   (`E_SUPERVISOR_WIRED_TO_TYPE_MISMATCH`).
    /// - No `wired_to` dependency cycles (`E_SUPERVISOR_WIRED_CYCLE`).
    /// - `simple_one_for_one` strategy requires exactly one `pool` child and no `child` decls
    ///   (`E_SUPERVISOR_STRATEGY_POOL_MISMATCH`).
    /// - Any other strategy rejects `pool` decls (`E_SUPERVISOR_STRATEGY_POOL_MISMATCH`).
    pub(super) fn check_supervisor(&mut self, sd: &SupervisorDecl, span: &Span) {
        let scope = self.enter_primary_sig_scope(&[(Some(&sd.type_params), None)]);
        let bounds = self
            .type_defs
            .get(&sd.name)
            .map_or_else(HashMap::new, |definition| definition.bounds.clone());
        self.current_type_param_bounds
            .push(TypeParamScope::new(bounds, HashMap::new()));
        // A dotted child type (`child a: bank.Account`) references the
        // module's actor; mark the import used so the program does not get a
        // spurious unused-import warning when the supervisor is the only
        // reference.
        for child in &sd.children {
            if let Some((module, _)) = child.actor_type.split_once('.') {
                if self.modules.contains(module) {
                    self.used_modules.borrow_mut().insert(ImportKey::in_file(
                        self.current_module.clone(),
                        self.current_module_idx,
                        module.to_string(),
                    ));
                }
            }
        }

        // ── 2. Duplicate child names ─────────────────────────────────────────
        self.check_supervisor_duplicate_children(sd, span);

        // ── 3. Strategy / pool consistency ──────────────────────────────────
        self.check_supervisor_strategy_pool(sd, span);

        // ── 4. wired_to key resolution + type compatibility ──────────────────
        self.check_supervisor_wired_to(sd, span);

        // ── 5. Dependency cycle detection ────────────────────────────────────
        self.check_supervisor_wired_to_cycles(sd, span);

        // ── 7. Intensity restart-budget sanity ──────────────────────────────
        self.check_supervisor_intensity(sd, span);

        // Child construction uses the ordinary spawn contract with retained
        // config parameters in scope. Each restart repeats this construction.
        self.check_supervisor_init_args(sd, span);
        self.current_type_param_bounds.pop();
        self.exit_primary_sig_scope(scope);
    }

    /// Prove lexical child authority before publishing a typed child handle.
    fn resolve_checked_supervisor_child(
        &mut self,
        supervisor: &SupervisorDecl,
        child: &hew_parser::ast::ChildSpec,
        span: &Span,
    ) -> Option<String> {
        let child_span = if child.span.is_empty() {
            span.clone()
        } else {
            child.span.clone()
        };
        let identity = self.resolve_supervisor_child_type(&child.actor_type);
        let definition = identity.as_ref().and_then(|name| self.type_defs.get(name));
        if definition.is_some_and(|definition| {
            matches!(
                definition.kind,
                TypeDefKind::Actor | TypeDefKind::Supervisor
            )
        }) || identity
            .as_ref()
            .is_some_and(|name| definition.is_none() && self.supervisor_children.contains_key(name))
        {
            return identity;
        }
        let (subkind, message) = if let Some(identity) = identity.filter(|_| definition.is_some()) {
            (SupervisorErrorKind::ChildNotSupervisable, format!(
                "E_SUPERVISOR_CHILD_NOT_SUPERVISABLE: supervisor `{}` child `{}` names `{}`; the selected declaration `{identity}` is neither an actor nor a supervisor",
                supervisor.name, child.name, child.actor_type,
            ))
        } else {
            (SupervisorErrorKind::UnknownChildActor, format!(
                "E_SUPERVISOR_UNKNOWN_CHILD_ACTOR: supervisor `{}` child `{}` references unknown actor `{}`; import a public actor into this scope or qualify it through a module binding",
                supervisor.name, child.name, child.actor_type,
            ))
        };
        self.errors.push(TypeError::new(
            TypeErrorKind::SupervisorError { subkind },
            child_span,
            message,
        ));
        None
    }

    /// Check child construction with the supervisor's retained config in scope.
    /// Store each complete child handle at its source site for HIR to consume.
    fn check_supervisor_init_args(&mut self, sd: &SupervisorDecl, span: &Span) {
        self.env.push_scope();

        // Bind each config param to its resolved type. `resolve_annotation_with_holes`
        // resolves the config struct name to a `Ty::Named` so a `config.field`
        // access inside an init-arg expr resolves through its record layout.
        for param in &sd.params {
            let ty = self.resolve_annotation_with_holes(
                &param.ty,
                format!(
                    "config parameter `{}` of supervisor `{}`",
                    param.name, sd.name
                ),
            );
            self.env.define_param_with_span(
                param.name.clone(),
                ty,
                param.is_mutable,
                param.ty.1.clone(),
            );
        }

        for child in &sd.children {
            let Some(identity) = self.resolve_checked_supervisor_child(sd, child, span) else {
                continue;
            };
            let target = (Expr::Identifier(identity), child.span.clone());
            let handle = self.check_spawn(&target, &child.type_args, &child.args, &child.span);
            self.record_type(&child.span, &handle);
            if let Some(child_ty) = handle.as_actor_handle() {
                if let Some(children) = self.supervisor_children.get_mut(&sd.name) {
                    let entries = if child.is_pool {
                        &mut children.pools
                    } else {
                        &mut children.statics
                    };
                    if let Some((_, ty)) = entries.iter_mut().find(|(name, _)| name == &child.name)
                    {
                        *ty = child_ty.clone();
                    }
                }
            }
        }

        // Validate every pool child's `count:` clause (presence, integer type,
        // positive literal) while config params are still in scope so a
        // `count: config.workers` expr resolves.
        self.check_supervisor_pool_count(sd, span);

        self.env.pop_scope();
    }

    /// Validate the `count:` clause on every `pool` child declaration.
    ///
    /// A static pool (`pool workers: Worker(..) count: N`) spawns exactly N
    /// fungible members at bootstrap. The clause is REQUIRED on a pool
    /// declaration, must type as an integer, and — when a compile-time integer
    /// literal — must be positive. A non-literal expr (`count: config.workers`)
    /// is accepted here (the type resolves through the config record layout),
    /// but codegen currently rejects it at compile time with
    /// `CodegenError::FailClosed` — the dynamic `0..N` bootstrap loop is not
    /// yet emitted. Use a literal `count: N` until that loop lands.
    ///
    /// Must run with the supervisor's config params bound in scope (it is
    /// invoked from `check_supervisor_init_args`) so a `config.field` count
    /// expr resolves through the config struct's record layout.
    fn check_supervisor_pool_count(&mut self, sd: &SupervisorDecl, span: &Span) {
        for child in &sd.children {
            if !child.is_pool {
                continue;
            }

            let Some(count_expr) = child.count.as_ref() else {
                // A pool declares a fixed-size fleet; without `count:` the size
                // is undefined. Fail closed rather than defaulting to a silent 1.
                self.errors.push(TypeError::new(
                    TypeErrorKind::SupervisorError {
                        subkind: SupervisorErrorKind::PoolCountMissing,
                    },
                    span.clone(),
                    format!(
                        "E_SUPERVISOR_POOL_COUNT_MISSING: supervisor `{}` pool child `{}` is \
                         missing the required `count:` clause; a static pool declares a \
                         fixed-size fleet, e.g. `pool {}: {}(..) count: 5`",
                        sd.name, child.name, child.name, child.actor_type
                    ),
                ));
                continue;
            };

            // The count expr must type as an integer. Synthesise (not
            // check_against a single width) so `config.workers` resolves and an
            // integer-literal stays `IntLiteral`; reject a non-integer result.
            let count_ty = self.synthesize(&count_expr.0, &count_expr.1);
            let resolved = self.subst.resolve(&count_ty);
            if !resolved.is_integer() && !matches!(resolved, Ty::IntLiteral | Ty::Error) {
                self.errors.push(TypeError::new(
                    TypeErrorKind::SupervisorError {
                        subkind: SupervisorErrorKind::PoolCountType,
                    },
                    count_expr.1.clone(),
                    format!(
                        "E_SUPERVISOR_POOL_COUNT_TYPE: supervisor `{}` pool child `{}` `count:` \
                         must be an integer, found `{}`",
                        sd.name,
                        child.name,
                        resolved.user_facing()
                    ),
                ));
            }

            // A compile-time-evaluable count must be positive — a zero or
            // negative pool size is a static error. A dynamic count
            // (`config.workers`, a call, anything not in the const allow-list)
            // is accepted and fails closed at bootstrap (trap on `N <= 0`).
            //   Ok(0)                 → reject (zero pool)
            //   Ok(n > 0)             → accept
            //   Err(Overflow)         → reject (negative / out-of-range literal)
            //   Err(NotConstant|...)  → accept (dynamic; runtime fail-closed)
            let const_env = super::const_eval::ConstEnv::new();
            match super::const_eval::eval_const_expr(count_expr, &const_env) {
                Ok(0) => {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::SupervisorError {
                            subkind: SupervisorErrorKind::PoolCountNonPositive,
                        },
                        count_expr.1.clone(),
                        format!(
                            "E_SUPERVISOR_POOL_COUNT_NON_POSITIVE: supervisor `{}` pool child \
                             `{}` `count:` must be a positive integer, found `0`",
                            sd.name, child.name
                        ),
                    ));
                }
                Err(super::const_eval::ConstEvalError::Overflow) => {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::SupervisorError {
                            subkind: SupervisorErrorKind::PoolCountNonPositive,
                        },
                        count_expr.1.clone(),
                        format!(
                            "E_SUPERVISOR_POOL_COUNT_NON_POSITIVE: supervisor `{}` pool child \
                             `{}` `count:` must be a positive integer (a negative or \
                             out-of-range size is not allowed)",
                            sd.name, child.name
                        ),
                    ));
                }
                // Positive constant or genuinely dynamic — both accepted.
                Ok(_) | Err(_) => {}
            }
        }
    }

    /// Validate the `intensity: N within <duration>` restart budget: the
    /// restart count must be non-negative and the window must parse to a
    /// positive duration. The parser already guarantees the window is a real
    /// duration literal (not a bare integer), so this catches the remaining
    /// semantically-empty cases (e.g. `intensity: -1 within 0s`).
    fn check_supervisor_intensity(&mut self, sd: &SupervisorDecl, span: &Span) {
        let Some(intensity) = &sd.intensity else {
            return;
        };
        if intensity.restarts < 0 {
            self.errors.push(TypeError::new(
                TypeErrorKind::SupervisorError {
                    subkind: SupervisorErrorKind::IntensityRestarts,
                },
                span.clone(),
                format!(
                    "E_SUPERVISOR_INTENSITY_RESTARTS: supervisor `{}` has a negative restart \
                     budget `{}`; `intensity:` requires a non-negative restart count",
                    sd.name, intensity.restarts
                ),
            ));
        }
        match hew_parser::parse_duration_ns(&intensity.window) {
            Some(ns) if ns > 0 => {}
            Some(_) => self.errors.push(TypeError::new(
                TypeErrorKind::SupervisorError {
                    subkind: SupervisorErrorKind::IntensityWindow,
                },
                span.clone(),
                format!(
                    "E_SUPERVISOR_INTENSITY_WINDOW: supervisor `{}` has a zero-length restart \
                     window `{}`; the window must be a positive duration",
                    sd.name, intensity.window
                ),
            )),
            None => self.errors.push(TypeError::new(
                TypeErrorKind::SupervisorError {
                    subkind: SupervisorErrorKind::IntensityWindow,
                },
                span.clone(),
                format!(
                    "E_SUPERVISOR_INTENSITY_WINDOW: supervisor `{}` window `{}` is not a valid \
                     duration literal",
                    sd.name, intensity.window
                ),
            )),
        }
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
                        TypeErrorKind::SupervisorError {
                            subkind: SupervisorErrorKind::DuplicateChild,
                        },
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
                    TypeErrorKind::SupervisorError {
                        subkind: SupervisorErrorKind::StrategyPoolMismatch,
                    },
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
                    TypeErrorKind::SupervisorError {
                        subkind: SupervisorErrorKind::StrategyPoolMismatch,
                    },
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
                TypeErrorKind::SupervisorError {
                    subkind: SupervisorErrorKind::StrategyPoolMismatch,
                },
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
        // Build a sibling-name → actor-type map for fast resolution. The stored
        // child type is the raw user spelling; canonicalize each to the
        // registered actor identity so a package-module sibling compares against
        // the same identity the dependent's init-param annotation resolves to.
        let sibling_types: std::collections::HashMap<&str, String> = sd
            .children
            .iter()
            .map(|c| {
                (
                    c.name.as_str(),
                    self.canonical_supervisor_child_type(&c.actor_type),
                )
            })
            .collect();

        for child in &sd.children {
            let Some(wired_to) = &child.wired_to else {
                continue;
            };

            for (param_key, sibling_name) in wired_to {
                // ── Key resolution: sibling must exist ──────────────────────
                let Some(sibling_type) = sibling_types.get(sibling_name.as_str()) else {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::SupervisorError {
                            subkind: SupervisorErrorKind::WiredToUnknownSibling,
                        },
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
                // typed as `sibling_type`'s own actor-handle type.
                let dependent_identity = self.canonical_supervisor_child_type(&child.actor_type);
                self.check_supervisor_wired_to_type_compat(
                    &sd.name,
                    &child.name,
                    &dependent_identity,
                    param_key,
                    sibling_type,
                    span,
                );
            }
        }
    }

    /// Verify that `dependent_actor`'s init has a parameter `param_key` typed
    /// as `sibling_type`'s own actor-handle type. Emits
    /// `E_SUPERVISOR_WIRED_TO_TYPE_MISMATCH` on failure.
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

        let Some(param) = params.iter().find(|param| param.name == param_key) else {
            self.errors.push(TypeError::new(
                TypeErrorKind::SupervisorError {
                    subkind: SupervisorErrorKind::WiredToTypeMismatch,
                },
                span.clone(),
                format!(
                    "E_SUPERVISOR_WIRED_TO_TYPE_MISMATCH: in supervisor `{supervisor_name}`, \
                     child `{dependent_child_name}` (`{dependent_actor_type}`) is wired via key \
                     `{param_key}` but `{dependent_actor_type}.init` has no parameter named \
                     `{param_key}`"
                ),
            ));
            return;
        };

        // Expected: outer has the supervisor-local pid role, inner = expected_sibling_type.
        // RemotePid is intentionally rejected here by role: supervisors are local, and a
        // wired_to child param typed `RemotePid<Sibling>` is semantically invalid.
        let type_ok = supervisor_local_pid_target(&param.ty)
            .is_some_and(|target_type| target_type == expected_sibling_type);

        if !type_ok {
            self.errors.push(TypeError::new(
                TypeErrorKind::SupervisorError {
                    subkind: SupervisorErrorKind::WiredToTypeMismatch,
                },
                span.clone(),
                format!(
                    "E_SUPERVISOR_WIRED_TO_TYPE_MISMATCH: in supervisor `{supervisor_name}`, \
                     child `{dependent_child_name}` wires `{param_key}` to sibling of type \
                     `{expected_sibling_type}`, but `{dependent_actor_type}.init` parameter \
                     `{param_key}` has type `{}` (expected `{expected_sibling_type}`)",
                    param.ty.user_facing()
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
                TypeErrorKind::SupervisorError {
                    subkind: SupervisorErrorKind::WiredCycle,
                },
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
        // rc1-F1 stage A: body checking resolves the same canonical key
        // `register_fn_sig_with_name` minted — root items included — so
        // `current_function` (a `fn_sigs`-family key) is always canonical.
        let fn_name = self.canonical_fn_identity(self.canonical_fn_owner(), &fd.name);
        self.check_function_as(fd, &fn_name);
    }

    /// Check a function body with the tail Ok-coercion armed for an explicit
    /// `Result<_, _>` return.
    ///
    /// Arms `tail_ok_armed` only when `resolved_expected_ret` is `Result<_, _>`
    /// and the function is not a generator (whose body yields Unit, not the
    /// declared type). `check_block` and `synthesize` disarm the flag everywhere
    /// but genuine function-return tail positions, so the coercion performed in
    /// `check_against` is strictly tail-only and never fires in a non-tail
    /// expression position.
    fn check_body_with_tail_ok_coercion(
        &mut self,
        fd: &FnDecl,
        resolved_expected_ret: &Ty,
        block_expected: Option<&Ty>,
    ) -> Ty {
        let prev_tail_ok_armed = self.tail_ok_armed;
        if self.current_fails {
            self.tail_ok_armed = false;
            let actual = self.check_block(&fd.body, block_expected);
            if !matches!(self.subst.resolve(&actual), Ty::Never | Ty::Error) {
                if let Some(tail) = &fd.body.trailing_expr {
                    self.tail_ok_coercions
                        .insert(SpanKey::in_module(&tail.1, self.current_module_idx));
                } else if actual == Ty::Unit {
                    if let Some(annotation) = &fd.return_type {
                        self.result_return_coercions.insert(
                            SpanKey::in_module(&annotation.1, self.current_module_idx),
                            super::ResultReturnKind::Success,
                        );
                    }
                }
            }
            self.tail_ok_armed = prev_tail_ok_armed;
            return actual;
        }
        self.tail_ok_armed = !fd.is_generator && resolved_expected_ret.as_result().is_some();
        let actual = self.check_block(&fd.body, block_expected);
        self.tail_ok_armed = prev_tail_ok_armed;
        if let Some(tail) = &fd.body.trailing_expr {
            self.reject_borrowed_return_transfer(tail, resolved_expected_ret);
        }
        actual
    }

    /// Bind parameters and establish independent mutable parameter values.
    fn bind_function_parameters(&mut self, fd: &FnDecl, in_actor: bool) {
        // Only the first parameter can be the receiver.
        for (i, p) in fd.params.iter().enumerate() {
            let (ty, is_receiver) = self.resolve_param_binding_ty(i, p);
            let private_copy = p.is_mutable
                && !p.is_consume
                && !is_receiver
                && self.parameter_has_independent_clone(&ty);
            if in_actor {
                self.check_shadowing(&p.name, &p.ty.1);
            }
            if is_receiver {
                self.env.define_receiver_param_with_span(
                    p.name.clone(),
                    ty,
                    p.is_mutable,
                    p.ty.1.clone(),
                );
            } else {
                self.env
                    .define_param_with_span(p.name.clone(), ty, p.is_mutable, p.ty.1.clone());
            }
            self.env
                .set_parameter_consume(&p.name, p.is_consume || (is_receiver && fd.consumes_self));
            if private_copy {
                self.env.reinit_place(&p.name, &[]);
            }
        }
    }

    /// Check a function body using `fn_name` for the `fn_sigs` lookup.
    ///
    /// Impl methods are registered under qualified names (e.g. `Connection::close`)
    /// but `FnDecl::name` is bare (e.g. `close`). Using the qualified name prevents
    /// collisions with builtins or inlined functions from other modules.
    pub(super) fn check_function_as(&mut self, fd: &FnDecl, fn_name: &str) {
        let body = self
            .identity
            .declaration_by_path(fn_name)
            .cloned()
            .or_else(|| self.impl_method_declaration_ids.get(fn_name).cloned())
            .map(|id| {
                let creator = super::effects::EffectBody::Declaration(id.clone());
                if fd.is_generator {
                    self.effect_graph.bodies.entry(creator).or_default();
                    super::effects::EffectBody::Generator(id)
                } else {
                    creator
                }
            });
        let previous = std::mem::replace(&mut self.effect_graph.current_body, body.clone());
        if let Some(body) = body {
            self.effect_graph.bodies.entry(body).or_default();
        }
        self.check_function_body_as(fd, fn_name);
        self.effect_graph.current_body = previous;
    }

    fn check_function_body_as(&mut self, fd: &FnDecl, fn_name: &str) {
        // Functions marked `#[intrinsic("key")]` are typed declaration stubs
        // whose bodies are empty placeholders; the real semantics live in the
        // catalog. Skip body type-checking entirely — the signature was already
        // registered by `register_fn_sig_with_name`.
        if fd.intrinsic.is_some() {
            return;
        }
        let prev_function = self.current_function.take();
        self.current_function = Some(fn_name.to_string());
        self.env.push_scope();
        // Scratch map of per-pattern bound names is function-local: the
        // borrowed-affine escape scanner (`warn_affine_param_escape`, run at
        // the end of this same call) consumes only this function's pattern spans. Clear so
        // it never accumulates across the whole program.
        self.pattern_bound_names.clear();

        // Push this fn's type-param bounds onto the resolver stack so
        // `T::Bar` projections inside `let x: T::Bar = ...` and other in-body
        // type annotations resolve. Popped at end of body check.
        let body_bounds = self.registered_fn_type_param_scope(fn_name);
        let pushed_body_bounds = !body_bounds.bounds.is_empty();
        if pushed_body_bounds {
            self.current_type_param_bounds.push(body_bounds);
        }

        // If inside an actor, push a separate scope for parameters so
        // shadowing checks detect collisions with actor field names.
        let in_actor = !self.current_actor_fields.is_empty();
        if in_actor {
            self.env.push_scope();
        }

        self.bind_function_parameters(fd, in_actor);

        // Use the return type from the already-registered fn signature so that
        // TypeExpr::Infer (-> _) reuses the same Ty::Var that call sites see.
        // This ensures body-checking unification updates the shared type variable.
        //
        // For an impl method (`Type::method`) checked inside a module, prefer the
        // module-local type def's method sig. The bare `fn_sigs["Type::method"]`
        // key is last-write-wins across every module that declares a same-named
        // type, so two modules each `impl ServerMethods for Server` would clobber
        // each other's `accept` return type. The qualified `{short}.Type` def
        // carries this module's own method, registered from its own impl.
        let module_local_method = fn_name
            .split_once("::")
            .and_then(|(type_name, method)| self.module_local_method_sig(type_name, method));
        let declared_ret = if let Some(sig) = module_local_method {
            sig.return_type.clone()
        } else if let Some(sig) = self.fn_sigs.get(fn_name) {
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
        let prev_fails = self.current_fails;
        self.current_fails = matches!(
            fd.return_type.as_ref().map(|ty| &ty.0),
            Some(TypeExpr::Fallible { .. })
        );
        let expected_ret = self.function_body_return_type(fd, &declared_ret);
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
        let actual =
            self.check_body_with_tail_ok_coercion(fd, &resolved_expected_ret, block_expected);
        // A completely empty body on a method whose `Self` is a compiler
        // builtin (`ActorHandle`, `RemotePid`, `Vec`, …) is a fail-closed
        // declaration stub: no source constructor exists for an opaque pid
        // handle or an abstract `T`, a self-call would stack-overflow, and the
        // real value is produced by codegen / the runtime. The placeholder body
        // is deliberately empty and lowers to a fail-closed zero-value, so its
        // non-`unit` return type is by design. Skip the body-vs-return mismatch
        // for these — mirrors the `#[intrinsic]` body-skip above, scoped to
        // builtin-typed impls so an ordinary user function with a forgotten body
        // still flags the missing return value.
        let empty_builtin_self_stub = fd.body.stmts.is_empty()
            && fd.body.trailing_expr.is_none()
            && self
                .current_self_type
                .as_ref()
                .is_some_and(|(name, _)| Ty::is_named_builtin(name));
        if !empty_builtin_self_stub && !matches!(self.subst.resolve(&expected_ret), Ty::Error) {
            self.expect_type(
                &expected_ret,
                &actual,
                &(fd.body
                    .stmts
                    .last()
                    .map_or(fd.decl_span.clone(), |(_, s)| s.clone())),
            );
        }

        // Borrowed affine parameters must be cloned before they escape.
        if !fd.is_generator {
            self.warn_affine_param_escape(fd);
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
        self.current_fails = prev_fails;
        self.current_return_type = None;
        self.current_function = prev_function;
        if pushed_body_bounds {
            self.current_type_param_bounds.pop();
        }
        if in_actor {
            self.env.pop_scope();
        }
        self.emit_scope_warnings();
    }

    fn function_body_return_type(&self, fd: &FnDecl, declared: &Ty) -> Ty {
        if self.current_fails {
            declared
                .as_result()
                .map_or(Ty::Error, |(success, _)| success.clone())
        } else if fd.is_generator {
            Ty::Unit
        } else {
            declared.clone()
        }
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
                        origin: hew_parser::ast::DeclarationOrigin::Authored,
                        attributes: vec![],
                        is_generator: false,
                        visibility: Visibility::Private,
                        name: method.name.clone(),
                        type_params: method.type_params.clone(),
                        params: method.params.clone(),
                        return_type: method.return_type.clone(),
                        where_clause: method.where_clause.clone(),
                        body: body.clone(),
                        doc_comment: None,
                        decl_span: 0..0,
                        fn_span: 0..0,
                        intrinsic: None,
                        consumes_self: false,
                    };
                    let qualified = format!("{}::{}", td.name, method.name);

                    // Bind the trait's own method-set to the abstract `Self`
                    // receiver for the duration of this default-body check.
                    //
                    // Without this, `val.other_method()` inside a default body
                    // resolves `val: Self` against an empty type-param-bounds map
                    // and reports "no method `other_method` on `Self`".  The
                    // fix mirrors the generic-bound dispatch path
                    // (methods.rs: "Type-parameter method dispatch"): that path
                    // reads `fn_sigs[current_function].type_param_bounds["T"]`
                    // to find which traits bound `T`, then looks up the method
                    // in those traits.  We inject `Self → [TraitName]` into the
                    // registered sig for `Trait::method` so the same path
                    // resolves sibling trait-method calls on the `Self` receiver.
                    let prev_sig = self.fn_sigs.get(&qualified).cloned();
                    if let Some(sig) = self.fn_sigs.get_mut(&qualified) {
                        if !sig.type_params.contains(&"Self".to_string()) {
                            sig.type_params.push("Self".to_string());
                        }
                        sig.type_param_bounds
                            .entry("Self".to_string())
                            .or_insert_with(Vec::new)
                            .push(td.name.clone());
                    }

                    self.check_function_as(&fn_decl, &qualified);

                    // Restore the original sig — the `Self` type-param is an
                    // internal default-body-check artefact and must not persist
                    // into the signature visible to call sites.
                    if let Some(original) = prev_sig {
                        self.fn_sigs.insert(qualified, original);
                    }
                }
            }
        }
    }

    pub(super) fn check_actor(&mut self, ad: &ActorDecl) {
        // The actor's checker identity: dotted `{module_path}.{name}` for a
        // module actor (the body is checked with `current_module` set), bare
        // for root/flat actors. All signature lookups during body checking
        // (`fn_sigs["{identity}::{rf}"]`), the `this` receiver type, and the
        // max-heap table key must use the same identity the registration
        // pass authored, or a same-named actor from another module would be
        // consulted instead.
        let identity = Self::actor_identity(self.current_module.as_deref(), &ad.name);
        let actor_ty = Ty::Named {
            builtin: None,
            name: identity.clone(),
            args: ad
                .type_params
                .iter()
                .map(|parameter| Ty::Named {
                    builtin: None,
                    name: parameter.name.clone(),
                    args: Vec::new(),
                })
                .collect(),
        };
        let generic_bindings: HashMap<_, _> = ad
            .type_params
            .iter()
            .map(|parameter| {
                (
                    parameter.name.clone(),
                    Ty::Named {
                        builtin: None,
                        name: parameter.name.clone(),
                        args: Vec::new(),
                    },
                )
            })
            .collect();
        let has_parameters = !generic_bindings.is_empty();
        if has_parameters {
            self.generic_ctx.push(generic_bindings);
            let bounds = self
                .type_defs
                .get(&identity)
                .map_or_else(HashMap::new, |definition| definition.bounds.clone());
            self.current_type_param_bounds
                .push(TypeParamScope::new(bounds, HashMap::new()));
        }
        let prev_actor_type = self.current_actor_type.replace(actor_ty);
        let deferred_fields = self
            .actor_deferred_fields
            .get(&identity)
            .cloned()
            .unwrap_or_default();
        for field in &ad.fields {
            if deferred_fields.contains(&field.name) {
                self.actor_deferred_field_decls
                    .insert(SpanKey::in_module(&field.ty.1, self.current_module_idx));
            }
        }
        let prev_actor_fields = std::mem::replace(
            &mut self.current_actor_fields,
            ad.fields
                .iter()
                .map(|f| ActorFieldInfo {
                    name: f.name.clone(),
                    is_mutable: f.is_mutable,
                    decl_span: f.ty.1.clone(),
                    deferred: deferred_fields.contains(&f.name),
                })
                .collect(),
        );

        // Record the per-actor arena cap from `#[max_heap(N)]` if present.
        // The parser already converted suffixes (kb, mb) to bytes; we record
        // `None`-absent as "no annotation" (unbounded) and `Some(cap)` as the
        // caller-supplied cap. Codegen reads `actor_max_heap` to decide between
        // `hew_arena_new` (unbounded) and `hew_arena_new_with_cap(cap)` (bounded).
        if let Some(cap) = ad.max_heap_bytes {
            self.actor_max_heap.insert(identity.clone(), cap);
        }

        let previous_function = self.current_function.replace(format!("{identity}::init"));
        self.check_actor_field_defaults(ad);
        self.current_function = previous_function;

        // Type-check init body if present
        if let Some(init) = &ad.init {
            self.check_actor_init(&identity, init, &ad.fields);
        }

        for rf in &ad.receive_fns {
            self.check_receive_fn(&identity, rf, &ad.fields);
        }

        // Separate lifecycle-hook fns from regular methods. Hooks carry
        // one of `#[on(start)]`, `#[on(stop)]`, or `#[on(crash)]`.
        // `#[on(upgrade)]` is parsed but rejected below: it is reserved and
        // not supported. Regular methods carry no attributes and
        // are checked as ordinary actor methods.
        //
        // `#[on(start)]` is at most once per actor; `#[on(stop)]` may
        // appear multiple times (lexical declaration order is the
        // run order — see HEW-SPEC-2026 §9.1.2).
        self.check_actor_methods(ad, &identity);

        if has_parameters {
            self.current_type_param_bounds.pop();
            self.generic_ctx.pop();
        }
        self.current_actor_type = prev_actor_type;
        self.current_actor_fields = prev_actor_fields;
    }

    fn check_actor_field_defaults(&mut self, ad: &ActorDecl) {
        for field in &ad.fields {
            let Some(default) = &field.default else {
                continue;
            };
            let expected = self.resolve_type_expr(&field.ty);
            self.check_expr_with_expected(&default.0, &default.1, &expected);
        }
    }

    /// Walk an actor's `methods` list, dispatching each fn to either the
    /// lifecycle-hook validator or the regular-method validator based on
    /// its `#[on(<event>)]` annotation. Tracks `#[on(start)]` uniqueness
    /// across the loop.
    fn check_actor_methods(&mut self, ad: &ActorDecl, identity: &str) {
        let mut on_start_seen: Option<Span> = None;
        let mut on_down_seen: Option<Span> = None;
        for method in &ad.methods {
            let hook_attrs: Vec<_> = method
                .attributes
                .iter()
                .filter(|a| a.name.as_str() == "on")
                .collect();

            if hook_attrs.is_empty() {
                self.env.push_scope();
                self.bind_actor_fields(&ad.fields);
                let qualified = format!("{identity}::{}", method.name);
                self.check_function_as(method, &qualified);
                self.reject_unplugged_actor_state_fields(&ad.fields);
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
            if hook_kind_str == "down" {
                if let Some(prev) = &on_down_seen {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::InvalidOperation,
                        hook_attr.span.clone(),
                        format!(
                            "actor `{}` declares more than one `#[on(down)]` hook; \
                             only one is allowed (see prior at {}..{})",
                            ad.name, prev.start, prev.end
                        ),
                    ));
                } else {
                    on_down_seen = Some(hook_attr.span.clone());
                }
            }

            // `#[on(crash)]` diverges from start/stop signature-wise:
            // crash takes a `CrashInfo` parameter and returns `CrashAction`.
            match hook_kind_str {
                "crash" => {
                    self.check_crash_hook(&ad.name, method, &ad.fields);
                    continue;
                }
                "exit" => {
                    self.check_exit_hook(&ad.name, method, &ad.fields);
                    continue;
                }
                "down" => {
                    self.check_down_hook(&ad.name, method, &ad.fields);
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
                        "`#[on]` on `{actor_name}.{method_name}` requires a hook kind argument; \
                         valid hook kinds are: start, stop, crash, exit, down"
                    ),
                ));
                return None;
            }
            Some("start" | "stop" | "crash" | "exit" | "down") => {}
            Some(unknown) => {
                self.errors.push(TypeError::new(
                    TypeErrorKind::InvalidOperation,
                    hook_attr.span.clone(),
                    format!(
                        "`#[on({unknown})]` on `{actor_name}.{method_name}` is not a recognised \
                         lifecycle hook; valid hook kinds are: start, stop, crash, exit, down"
                    ),
                ));
                return None;
            }
        }
        let hook_kind_str = hook_kind.unwrap();

        // Reject typed hooks with extra arguments.
        // start/stop already reject extra args via `check_lifecycle_hook`'s
        // signature checks; for typed hooks we validate the attribute shape here
        // because their signature/body checking is event-specific.
        if matches!(hook_kind_str, "crash" | "exit" | "down") && hook_attr.args.len() > 1 {
            self.errors.push(TypeError::new(
                TypeErrorKind::InvalidOperation,
                hook_attr.span.clone(),
                format!(
                    "`#[on({hook_kind_str})]` on `{actor_name}.{method_name}` does not accept \
                     extra arguments"
                ),
            ));
            return None;
        }

        Some(hook_kind_str)
    }

    /// Bind actor fields as bare names with their *declared* mutability:
    /// `var` fields are writable, `let` and bare fields are read-only.
    /// Handler, method, and lifecycle-hook bodies use this binding so that
    /// assignment to an immutable field is rejected at the assignment site.
    pub(super) fn bind_actor_fields(&mut self, fields: &[FieldDecl]) {
        for field in fields {
            let field_ty = self.resolve_type_expr(&field.ty);
            self.env
                .define(field.name.clone(), field_ty, field.is_mutable);
        }
    }

    /// Reject an actor state field left CONSUMED at the end of a body that
    /// bound the actor's fields as bare names.
    ///
    /// An actor's state outlives every handler invocation and messages arrive
    /// in arbitrary order, so a consuming use of a state field that the body
    /// does not re-initialise leaves a hole the NEXT message consumes again —
    /// `receive fn steal() { return sock.detach(); }` detaches one socket once
    /// per message. No ordering discipline can plug that from outside; the
    /// sound rule is that the body must plug it itself, on every path. Because
    /// place facts join by union, a field re-initialised on only some paths is
    /// still reported.
    ///
    /// This is deliberately a CHECKER reject and NOT a lowering change: a naive
    /// retain at `ActorStateFieldLoad` regressed once
    /// (`state-load-retains-unless-borrow-proven`) and an unconditional
    /// projected-leaf drop double-freed once
    /// (`state-drop-unconditional-projected-leaf-is-borrow`). The escape hatch
    /// is re-initialisation, which the assignment path already discharges.
    ///
    /// Call sites run this AFTER popping any parameter scope, so a parameter
    /// that shadows a field name cannot be mistaken for the field.
    pub(super) fn reject_unplugged_actor_state_fields(&mut self, fields: &[FieldDecl]) {
        for field in fields {
            let Some(binding) = self.env.lookup_ref(&field.name) else {
                continue;
            };
            let (place, moved_at) = if binding.is_moved {
                let Some(moved_at) = binding.moved_at.clone() else {
                    continue;
                };
                (field.name.clone(), moved_at)
            } else {
                let Some(moved) = binding.moved_places.first() else {
                    continue;
                };
                (
                    std::iter::once(field.name.as_str())
                        .chain(moved.path.iter().map(String::as_str))
                        .collect::<Vec<_>>()
                        .join("."),
                    moved.moved_at.clone(),
                )
            };
            let mut error = TypeError::new(
                TypeErrorKind::UseAfterConsume,
                moved_at,
                format!(
                    "actor state `{place}` is consumed here and never re-initialised; \
                     the next message would consume it again"
                ),
            )
            .with_note(
                field.ty.1.clone(),
                "actor state outlives the handler that consumed it",
            )
            .with_suggestion(format!(
                "re-initialise `{place}` before the body returns, or take a copy \
                 instead of consuming the state"
            ));
            if let Some(source_module) = &self.current_module {
                error = error.with_source_module(source_module.clone());
            }
            self.errors.push(error);
        }
    }

    /// Bind actor fields as writable regardless of declared mutability.
    ///
    /// `init { }` is the actor's constructor: it must be able to assign
    /// `let` fields their initial values, so the immutable-field rule does
    /// not apply inside the init body.
    pub(super) fn bind_actor_fields_for_init(&mut self, fields: &[FieldDecl]) {
        for field in fields {
            let field_ty = self.resolve_type_expr(&field.ty);
            let deferred = self
                .current_actor_fields
                .iter()
                .any(|info| info.name == field.name && info.deferred);
            if deferred {
                self.env.define_deferred_field(&field.name, field_ty);
            } else {
                self.env.define(field.name.clone(), field_ty, true);
            }
        }
    }

    /// Every deferred field must hold a value when init finishes normally
    /// (D447): a normal exit publishes the state to the actor.
    pub(super) fn require_deferred_fields_initialized(&mut self, exit: &str) {
        let missing: Vec<_> = self
            .current_actor_fields
            .iter()
            .filter(|field| field.deferred && self.env.deferred_field_uninitialized(&field.name))
            .map(|field| (field.name.clone(), field.decl_span.clone()))
            .collect();
        for (name, decl_span) in missing {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                &decl_span,
                format!(
                    "E_ACTOR_FIELD_UNINITIALIZED: `init` can {exit} without initializing state \
                     field `{name}`; assign it on every path before init finishes"
                ),
            );
        }
    }

    /// A plain actor method binds every state field as initialized, so `init`
    /// may call one only after every deferred field holds a value (D447).
    pub(super) fn require_deferred_fields_initialized_for_call(
        &mut self,
        method_key: &str,
        span: &Span,
    ) {
        let missing: Vec<_> = self
            .current_actor_fields
            .iter()
            .filter(|field| field.deferred && self.env.deferred_field_uninitialized(&field.name))
            .map(|field| field.name.clone())
            .collect();
        let method = method_key.rsplit("::").next().unwrap_or(method_key);
        for name in missing {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                span,
                format!(
                    "E_ACTOR_FIELD_UNINITIALIZED: `init` calls `{method}` before initializing \
                     state field `{name}`; assign it first"
                ),
            );
        }
    }

    /// A branch or loop join left a deferred field initialized on some paths
    /// only (D447). Every arm must initialize it, or none may.
    pub(super) fn report_deferred_init_conflicts(
        &mut self,
        conflicts: &[crate::env::TypeBindingId],
    ) {
        if conflicts.is_empty() {
            return;
        }
        let conflicting: Vec<_> = self
            .current_actor_fields
            .iter()
            .filter(|field| {
                field.deferred
                    && self
                        .env
                        .deferred_field_id(&field.name)
                        .is_some_and(|id| conflicts.contains(&id))
            })
            .map(|field| (field.name.clone(), field.decl_span.clone()))
            .collect();
        for (name, decl_span) in conflicting {
            self.report_error(
                TypeErrorKind::InvalidOperation,
                &decl_span,
                format!(
                    "E_ACTOR_FIELD_CONDITIONAL_INIT: state field `{name}` is initialized on \
                     only some paths of a branch or loop in `init`; initialize it in every \
                     arm, or before the branch"
                ),
            );
        }
    }

    /// Close the innermost loop boundary and report deferred fields whose
    /// initialization differs between its entry and its exits.
    pub(super) fn exit_loop_checked(&mut self) {
        let conflicts = self.env.exit_loop();
        self.report_deferred_init_conflicts(&conflicts);
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

        // Bind actor fields directly in scope (bare field access, all
        // writable in the init body — init is where `let` fields receive
        // their initial values). Hew uses bare names, not `self.field`.
        self.bind_actor_fields_for_init(fields);

        // Push a separate scope for parameters so shadowing checks can
        // detect collisions with actor field names in the outer scope,
        // exactly as `check_receive_fn` does (D458): an init parameter
        // named like a field is refused, not a silent alias for it.
        self.env.push_scope();

        // Bind init parameters
        for p in &init.params {
            self.check_shadowing(&p.name, &p.ty.1);
            let ty = self.resolve_annotation_with_holes(
                &p.ty,
                format!("init parameter `{}` of actor `{actor_name}`", p.name),
            );
            self.env
                .define_param_with_span(p.name.clone(), ty, p.is_mutable, p.ty.1.clone());
        }

        // Init returns unit — no meaningful return type
        self.current_return_type = Some(Ty::Unit);
        let previous_init = std::mem::replace(&mut self.checking_actor_init, true);
        let body_ty = self.check_block(&init.body, None);
        self.checking_actor_init = previous_init;
        if !matches!(body_ty, Ty::Never) {
            self.require_deferred_fields_initialized("finish");
        }
        self.current_return_type = None;

        self.current_function = prev_function;
        self.env.pop_scope(); // params scope
        self.reject_unplugged_actor_state_fields(fields);
        self.env.pop_scope(); // fields scope
    }

    /// Type-check an actor lifecycle hook (`#[on(start)]` or `#[on(stop)]`).
    ///
    /// Required shape (HEW-SPEC-2026 §9.1.2 rules 2-4):
    /// - no parameters (actor fields are in scope by bare name, same as `init { }`)
    /// - no type parameters
    /// - no `where` clause
    /// - return type `()` (omitted or explicitly unit)
    ///
    /// Hooks bind actor fields as bare names in scope with their declared
    /// mutability — `var` fields can be modified, `let` fields are
    /// read-only (only `init { }` may assign them).
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
        if hook.type_params.as_ref().is_some_and(|tps| !tps.is_empty()) {
            self.errors.push(TypeError::new(
                TypeErrorKind::InvalidOperation,
                hook.decl_span.clone(),
                format!(
                    "lifecycle hook `#[{hook_kind}]` on `{actor_name}.{}` cannot \
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
                    "lifecycle hook `#[{hook_kind}]` on `{actor_name}.{}` cannot \
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
                        "lifecycle hook `#[{hook_kind}]` on `{actor_name}.{}` must \
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
                    "lifecycle hook `#[{hook_kind}]` on `{actor_name}.{}` must take \
                     no parameters; actor fields are in scope by bare name (see \
                     `init {{ }}` for the same convention)",
                    hook.name
                ),
            ));
        }

        // ── Body checking ───────────────────────────────────────────────
        let prev_actor_handler_context = self.in_actor_handler_context;
        self.in_actor_handler_context = true;
        self.env.push_scope();

        let qualified_name = format!("{actor_name}::{}", hook.name);
        let prev_function = self.current_function.take();
        self.current_function = Some(qualified_name);

        // Bind actor fields directly in scope as bare names with their
        // declared mutability (`var` writable, `let` read-only).
        self.bind_actor_fields(fields);

        self.current_return_type = Some(Ty::Unit);
        self.check_block(&hook.body, None);
        self.current_return_type = None;
        self.in_actor_handler_context = prev_actor_handler_context;

        self.current_function = prev_function;
        self.reject_unplugged_actor_state_fields(fields);
        self.env.pop_scope();
    }

    /// Type-check an actor `#[on(crash)]` hook.
    ///
    /// Signature shape (failure-philosophy plan E2, Q45/A22, Q46/A23):
    /// - exactly one parameter `info: CrashInfo` (the runtime supplies
    ///   the int-tag payload — string fields wait on the spine-widening
    ///   lane).
    /// - return type `CrashAction` (variants `Restart | Escalate | Kill`;
    ///   the supervisor consults but honours its own budget rules).
    /// - no type parameters, no `where` clause.
    ///
    /// `CrashInfo` and `CrashAction` are provided by `std/failure.hew`
    /// (also pre-bound via `register_builtin_failure_surface` for inline
    /// tests).  Body type-checking binds actor fields as bare names in
    /// scope, same idiom as `init { }` / `#[on(start)]` / `#[on(stop)]`.
    ///
    /// Runtime invocation of this hook is owned by failure-philosophy
    /// slice E3.  This slice validates the signature shape so the
    /// compiled actor method symbol (`<Actor>::on_crash`, emitted via
    /// the existing actor-method serialize path) has the contract the
    /// runtime will rely on.
    /// Reject generic and `where`-clause modifiers shared by every
    /// `#[on(<event>)]` lifecycle hook.  Extracted from `check_crash_hook`
    /// because the same triad applies to future event-specific validators
    /// and keeps the per-event entry under the clippy `too_many_lines`
    /// threshold.
    fn reject_hook_modifier_set(&mut self, actor_name: &str, hook: &FnDecl, hook_kind: &str) {
        if hook.type_params.as_ref().is_some_and(|tps| !tps.is_empty()) {
            self.errors.push(TypeError::new(
                TypeErrorKind::InvalidOperation,
                hook.decl_span.clone(),
                format!(
                    "lifecycle hook `#[{hook_kind}]` on `{actor_name}.{}` cannot \
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
                    "lifecycle hook `#[{hook_kind}]` on `{actor_name}.{}` cannot \
                     have a `where` clause",
                    hook.name
                ),
            ));
        }
    }

    /// Validate the parameter list of a `#[on(crash)]` hook: exactly one
    /// parameter typed `CrashInfo`.  Diagnostics live here rather than in
    /// `check_crash_hook` to keep that entry under the clippy line limit.
    fn check_crash_hook_param(&mut self, actor_name: &str, hook: &FnDecl, hook_kind: &str) {
        match hook.params.as_slice() {
            [p] => {
                let pty = self.resolve_type_expr(&p.ty);
                let is_crash_info = is_canonical_std_named_type(
                    &pty,
                    crate::BuiltinType::CrashInfo,
                    "std.failure.CrashInfo",
                );
                if !is_crash_info {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::InvalidOperation,
                        p.ty.1.clone(),
                        format!(
                            "lifecycle hook `#[{hook_kind}]` on `{actor_name}.{}` parameter \
                             must have type `CrashInfo` (from `std.failure`)",
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
                        "lifecycle hook `#[{hook_kind}]` on `{actor_name}.{}` must take \
                         exactly one parameter `info: CrashInfo`; got {} parameter(s)",
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
            if !is_canonical_std_named_type(
                &ty,
                crate::BuiltinType::CrashAction,
                "std.failure.CrashAction",
            ) {
                self.errors.push(TypeError::new(
                    TypeErrorKind::InvalidOperation,
                    rt.1.clone(),
                    format!(
                        "lifecycle hook `#[{hook_kind}]` on `{actor_name}.{}` must \
                         return `CrashAction` (from `std.failure`)",
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
                    "lifecycle hook `#[{hook_kind}]` on `{actor_name}.{}` must declare \
                     a return type of `CrashAction` (from `std.failure`)",
                    hook.name
                ),
            ));
            crate::builtin_enums::monomorphic_builtin_enum_ty("CrashAction")
                .expect("generated builtin enum catalog must contain CrashAction")
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
        let prev_actor_handler_context = self.in_actor_handler_context;
        self.in_actor_handler_context = true;
        self.env.push_scope();

        let qualified_name = format!("{actor_name}::{}", hook.name);
        let prev_function = self.current_function.take();
        self.current_function = Some(qualified_name);

        // Bind actor fields as bare names (declared mutability), then the
        // `info` parameter on top of them.  Field-shadowing by the param
        // name is intentionally permitted — same precedent as `init` and
        // receive fn parameters (HEW-SPEC-2026 §9.1.1).
        self.bind_actor_fields(fields);
        if let Some(p) = hook.params.first() {
            let pty = self.resolve_type_expr(&p.ty);
            self.env
                .define_param_with_span(p.name.clone(), pty, p.is_mutable, p.ty.1.clone());
        }

        self.current_return_type = Some(return_ty);
        // M-4: the `CrashAction` enum-variant return is now wired end-to-end
        // (MIR extracts the variant tag at the return boundary; the supervisor
        // honours it). The former `in_crash_hook` fail-closed gate that rejected
        // a `CrashAction`-returning body is removed — a `#[on(crash)]` hook may
        // now return `Restart`/`Escalate`/`Kill` (or `panic(...)`) freely. The
        // standard return-type checking against `current_return_type` covers it.
        let _body_ty = self.check_block(&hook.body, None);
        self.current_return_type = None;
        self.in_actor_handler_context = prev_actor_handler_context;

        self.current_function = prev_function;
        self.reject_unplugged_actor_state_fields(fields);
        self.env.pop_scope();
    }

    /// Validate a `#[on(exit)]` linked-actor exit hook (M-7-R, Q210/A211).
    ///
    /// Fires when an actor THIS actor is linked to crashes/exits, delivering a
    /// typed `CrashNotification { actor_id, kind }`. Mirrors `#[on(crash)]`'s
    /// attribute shape (one typed payload param) but, unlike crash, returns
    /// `()` — an exit hook reacts (log / re-establish a link / drop work), it
    /// does not steer a restart decision. The signature is
    /// `fn on_exit(note: CrashNotification)`.
    pub(super) fn check_exit_hook(
        &mut self,
        actor_name: &str,
        hook: &FnDecl,
        fields: &[FieldDecl],
    ) {
        let hook_kind = "on(exit)";

        self.reject_hook_modifier_set(actor_name, hook, hook_kind);

        // Param: exactly one canonical `note: CrashNotification`. Match either
        // the compiler discriminator or the exact source-owned std identity:
        // the module graph resolves the declaration to
        // `std.failure.CrashNotification` without a builtin marker, while an
        // unrelated user type can share the short name.
        match hook.params.as_slice() {
            [p] => {
                let pty = self.resolve_type_expr(&p.ty);
                let is_crash_notification =
                    is_canonical_lifecycle_source_type(&pty, "std.failure.CrashNotification");
                if !is_crash_notification && !matches!(pty, Ty::Error) {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::InvalidOperation,
                        p.ty.1.clone(),
                        format!(
                            "lifecycle hook `#[{hook_kind}]` on `{actor_name}.{}` parameter \
                             must have type `CrashNotification` (from `std.failure`)",
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
                        "lifecycle hook `#[{hook_kind}]` on `{actor_name}.{}` must take \
                         exactly one parameter `note: CrashNotification`; got {} parameter(s)",
                        hook.name,
                        other.len()
                    ),
                ));
            }
        }

        // Return type: `()`. Reject a declared non-unit return.
        if let Some(rt) = &hook.return_type {
            let ty = self.resolve_type_expr(rt);
            if !matches!(ty, Ty::Unit) {
                self.errors.push(TypeError::new(
                    TypeErrorKind::InvalidOperation,
                    rt.1.clone(),
                    format!(
                        "lifecycle hook `#[{hook_kind}]` on `{actor_name}.{}` must return `()`; \
                         an exit hook reacts to a peer's failure, it does not return a value",
                        hook.name
                    ),
                ));
            }
        }

        // ── Body checking ───────────────────────────────────────────────
        let prev_actor_handler_context = self.in_actor_handler_context;
        self.in_actor_handler_context = true;
        self.env.push_scope();

        let qualified_name = format!("{actor_name}::{}", hook.name);
        let prev_function = self.current_function.take();
        self.current_function = Some(qualified_name);

        self.bind_actor_fields(fields);
        if let Some(p) = hook.params.first() {
            let pty = self.resolve_type_expr(&p.ty);
            self.env
                .define_param_with_span(p.name.clone(), pty, p.is_mutable, p.ty.1.clone());
        }

        self.current_return_type = Some(Ty::Unit);
        let _body_ty = self.check_block(&hook.body, None);
        self.current_return_type = None;
        self.in_actor_handler_context = prev_actor_handler_context;

        self.current_function = prev_function;
        self.reject_unplugged_actor_state_fields(fields);
        self.env.pop_scope();
    }

    /// Validate a typed monitor DOWN hook.
    pub(super) fn check_down_hook(
        &mut self,
        actor_name: &str,
        hook: &FnDecl,
        fields: &[FieldDecl],
    ) {
        let hook_kind = "on(down)";
        self.reject_hook_modifier_set(actor_name, hook, hook_kind);

        match hook.params.as_slice() {
            [p] => {
                let pty = self.resolve_type_expr(&p.ty);
                let is_down_notification =
                    is_canonical_lifecycle_source_type(&pty, "std.link_monitor.DownNotification");
                if !is_down_notification && !matches!(pty, Ty::Error) {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::InvalidOperation,
                        p.ty.1.clone(),
                        format!(
                            "lifecycle hook `#[{hook_kind}]` on `{actor_name}.{}` parameter \
                             must have type `DownNotification` (from `std.link_monitor`)",
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
                        "lifecycle hook `#[{hook_kind}]` on `{actor_name}.{}` must take \
                         exactly one parameter `note: DownNotification`; got {} parameter(s)",
                        hook.name,
                        other.len()
                    ),
                ));
            }
        }

        if let Some(rt) = &hook.return_type {
            let ty = self.resolve_type_expr(rt);
            if !matches!(ty, Ty::Unit) {
                self.errors.push(TypeError::new(
                    TypeErrorKind::InvalidOperation,
                    rt.1.clone(),
                    format!(
                        "lifecycle hook `#[{hook_kind}]` on `{actor_name}.{}` must return `()`",
                        hook.name
                    ),
                ));
            }
        }

        let prev_actor_handler_context = self.in_actor_handler_context;
        self.in_actor_handler_context = true;
        self.env.push_scope();
        let qualified_name = format!("{actor_name}::{}", hook.name);
        let prev_function = self.current_function.take();
        self.current_function = Some(qualified_name);
        self.bind_actor_fields(fields);
        if let Some(p) = hook.params.first() {
            let pty = self.resolve_type_expr(&p.ty);
            self.env
                .define_param_with_span(p.name.clone(), pty, p.is_mutable, p.ty.1.clone());
        }
        self.current_return_type = Some(Ty::Unit);
        let _body_ty = self.check_block(&hook.body, None);
        self.current_return_type = None;
        self.in_actor_handler_context = prev_actor_handler_context;
        self.current_function = prev_function;
        self.reject_unplugged_actor_state_fields(fields);
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
                } else if *ns < 1_000_000 {
                    // The periodic timer ABI (`hew_actor_schedule_periodic`)
                    // is millisecond-grained and treats a 0 ms interval as
                    // invalid; a sub-millisecond duration would floor to 0 at
                    // MIR lowering and be refused at spawn. Catch it here
                    // with a source-level diagnostic instead.
                    self.errors.push(TypeError::new(
                        TypeErrorKind::InvalidOperation,
                        attr.span.clone(),
                        "#[every] duration is less than 1ms, which floors to a 0ms timer interval; the minimum periodic interval is 1ms",
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
            self.warn_wasm_limitation(&attr.span, WasmUnsupportedFeature::PeriodicTimers);
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

        // Periodic handlers must not be generators. Generator receive fns
        // have no dispatchable MIR body (`lower_actor_handler_layouts` skips
        // them), so a periodic timer could never deliver a tick to one —
        // reject at check time rather than arm a message no handler accepts.
        if rf.is_generator {
            self.errors.push(TypeError::new(
                TypeErrorKind::InvalidOperation,
                rf.span.clone(),
                format!(
                    "#[every] receive fn `{}` must not be a generator; periodic handlers are plain fire-and-forget receive fns",
                    rf.name
                ),
            ));
        }
    }

    /// Reject an opaque handle type used as a receive-fn (actor message)
    /// parameter, directly or nested inside a record/enum/tuple payload.
    ///
    /// Actor message payloads are CBOR-serialized to cross the mailbox
    /// dispatch boundary. Opaque handle types (e.g. `net.Listener`,
    /// `net.Connection`, user `#[opaque]` types) are pointer-shaped runtime
    /// resources with no serializable record layout, so they can never be a
    /// message payload. Without this checker-layer diagnostic the failure
    /// surfaces late as a raw codegen-front `wire CBOR serialize: named type
    /// ... is not a registered record layout` message that names an internal
    /// wire detail instead of the offending parameter. See #2511.
    fn reject_opaque_message_payload(&mut self, ty: &Ty, param_span: &Span, handler: &str) {
        let mut visiting = std::collections::HashSet::new();
        let Some(opaque_name) = self.ty_message_payload_contains_opaque(ty, &mut visiting) else {
            return;
        };
        self.report_error(
            TypeErrorKind::OpaqueMessagePayload {
                type_name: opaque_name.clone(),
                handler: handler.to_string(),
            },
            param_span,
            format!(
                "opaque type `{opaque_name}` cannot be used as a receive-fn \
                 parameter; actor message payloads must be CBOR-serializable, \
                 and opaque handles have no serializable layout. Open or \
                 construct the handle locally inside the handler instead \
                 [E_OPAQUE_MESSAGE_PAYLOAD]"
            ),
        );
    }

    #[expect(
        clippy::too_many_lines,
        reason = "receive body checking establishes actor and callable contexts"
    )]
    pub(super) fn check_receive_fn(
        &mut self,
        actor_name: &str,
        rf: &ReceiveFnDecl,
        fields: &[FieldDecl],
    ) {
        // Validate #[every(duration)] attribute if present.
        self.validate_every_attribute(rf);
        self.actor_handler_state_guards.insert(
            SpanKey::in_module(&rf.span, self.current_module_idx),
            ActorStateGuard::Exclusive,
        );

        let prev_in_receive_fn = self.in_receive_fn;
        self.in_receive_fn = true;
        let prev_actor_handler_context = self.in_actor_handler_context;
        self.in_actor_handler_context = true;
        self.env.push_scope();

        // Set current_function so calls within this receive fn are recorded
        // in the call graph (enables dead-code reachability analysis).
        let qualified_name = format!("{}::{}", actor_name, rf.name);
        let prev_function = self.current_function.take();
        self.current_function = Some(qualified_name.clone());
        let effect_body = self
            .identity
            .declaration_by_path(&qualified_name)
            .cloned()
            .map(super::effects::EffectBody::Declaration);
        let previous_effect_body =
            std::mem::replace(&mut self.effect_graph.current_body, effect_body.clone());
        if let Some(body) = effect_body {
            self.effect_graph.bodies.entry(body).or_default();
        }

        let mut generic_bindings = std::collections::HashMap::new();
        if let Some(type_params) = &rf.type_params {
            for tp in type_params {
                generic_bindings.insert(
                    tp.name.clone(),
                    Ty::Named {
                        builtin: None,
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
            self.reject_opaque_message_payload(&ty, &p.ty.1, &qualified_name);
            self.env
                .define_param_with_span(p.name.clone(), ty, p.is_mutable, p.ty.1.clone());
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
        // A `fails` handler spells failure exactly as every `fails` fn does:
        // `return error e`, `?`, and a bare success tail the compiler wraps.
        // The body is checked against the success type, and the declared
        // `Result` stays in `current_return_type` for `return` and `?`.
        let prev_fails = self.current_fails;
        self.current_fails = !rf.is_generator
            && matches!(
                rf.return_type.as_ref().map(|ty| &ty.0),
                Some(TypeExpr::Fallible { .. })
            );
        let expected_ret = if rf.is_generator {
            Ty::Unit
        } else if self.current_fails {
            declared_ret
                .as_result()
                .map_or(Ty::Error, |(success, _)| success.clone())
        } else {
            declared_ret.clone()
        };
        // Store the type that drives `yield`/`return` checking inside the body.
        // For a `receive gen fn` the declared `-> T` is the yield element type;
        // the body falls off the end with Unit, so the handler value is
        // `Generator<Yield = T, Return = Unit>`. Shape `current_return_type` as
        // that generator type — exactly like a standalone `gen fn`
        // (`check_fn_decl`) — so `Stmt::Return` extracts the Unit Return
        // component (rejecting `return <T>;`, accepting bare `return;`) and
        // `synthesize_yield` extracts the `T` Yield component. Storing the bare
        // yield type instead fails open: `return <T>;` would unify against `T`
        // and a bare `return;` would be wrongly rejected.
        let current_return_type = if rf.is_generator {
            Ty::generator(declared_ret, Ty::Unit)
        } else {
            declared_ret
        };
        self.current_return_type = Some(current_return_type);
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
        let prev_tail_ok_armed = self.tail_ok_armed;
        if self.current_fails {
            self.tail_ok_armed = false;
        }
        let actual = self.check_block(&rf.body, block_expected);
        if self.current_fails && !matches!(self.subst.resolve(&actual), Ty::Never | Ty::Error) {
            if let Some(tail) = &rf.body.trailing_expr {
                self.tail_ok_coercions
                    .insert(SpanKey::in_module(&tail.1, self.current_module_idx));
            } else if actual == Ty::Unit {
                if let Some(annotation) = &rf.return_type {
                    self.result_return_coercions.insert(
                        SpanKey::in_module(&annotation.1, self.current_module_idx),
                        super::ResultReturnKind::Success,
                    );
                }
            }
        }
        self.tail_ok_armed = prev_tail_ok_armed;
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

        self.current_fails = prev_fails;
        self.in_generator = prev_in_generator;
        self.in_receive_fn = prev_in_receive_fn;
        self.in_actor_handler_context = prev_actor_handler_context;
        self.current_return_type = None;
        self.current_function = prev_function;
        self.effect_graph.current_body = previous_effect_body;
        if rf.type_params.as_ref().is_some_and(|tp| !tp.is_empty()) {
            self.generic_ctx.pop();
        }
        self.env.pop_scope(); // params scope
        self.reject_unplugged_actor_state_fields(fields);
        self.env.pop_scope(); // fields scope
    }

    pub(super) fn check_const(&mut self, cd: &ConstDecl, span: &Span) {
        if self.reject_protected_prelude_declaration(&cd.name, span) {
            return;
        }
        let expected =
            self.resolve_annotation_with_holes(&cd.ty, format!("constant `{}`", cd.name));
        let actual = self.check_against(&cd.value.0, &cd.value.1, &expected);
        // Module constants are target-typed integer expressions. Route the
        // accepted AST through the shared evaluator now, at the checker
        // boundary, so an invalid arithmetic result is a user semantic error
        // and cannot become a later HIR `E_NOT_YET_IMPLEMENTED`.
        if let Ok(resolved) = ResolvedTy::from_ty(&self.subst.resolve(&expected)) {
            if let Some(target) = super::const_eval::ConstIntegerTarget::from_resolved_ty(
                &resolved,
                self.pointer_width(),
            ) {
                match super::const_eval::eval_integer_const_expr(
                    &cd.value,
                    &super::const_eval::ConstEnv::new(),
                    target,
                ) {
                    Ok(_)
                    | Err(
                        super::const_eval::ConstEvalError::NotConstant
                        | super::const_eval::ConstEvalError::UnknownConst(_),
                    ) => {}
                    Err(super::const_eval::ConstEvalError::ArithmeticOverflow) => {
                        self.report_error(
                            TypeErrorKind::ConstInitializer,
                            &cd.value.1,
                            format!(
                                "constant initializer arithmetic overflows declared type `{}`",
                                resolved.user_facing()
                            ),
                        );
                    }
                    Err(super::const_eval::ConstEvalError::DivisionByZero) => {
                        self.report_error(
                            TypeErrorKind::ConstInitializer,
                            &cd.value.1,
                            "constant initializer divides by zero".to_string(),
                        );
                    }
                    Err(super::const_eval::ConstEvalError::OutOfRange) => {
                        self.report_error(
                            TypeErrorKind::ConstInitializer,
                            &cd.value.1,
                            format!(
                                "constant initializer value does not fit in declared type `{}`",
                                resolved.user_facing()
                            ),
                        );
                    }
                    // The target evaluator never emits the legacy machine
                    // wrapper class. Keep an explicit fail-closed arm so a
                    // future authority regression cannot silently accept it.
                    Err(super::const_eval::ConstEvalError::Overflow) => {
                        self.report_error(
                            TypeErrorKind::ConstInitializer,
                            &cd.value.1,
                            "constant initializer overflows its declared integer type".to_string(),
                        );
                    }
                }
            }
        }
        // Store compile-time values for default-width numeric consts so later
        // coercion sites can reuse the original literal kind/value instead of
        // depending on synthesis-time i64/f64 defaults.
        let is_default_int = expected == Ty::I64;
        let is_default_float = expected == Ty::F64;
        let const_value = if is_default_int {
            extract_integer_literal_value(&cd.value.0).map(ConstValue::Integer)
        } else if is_default_float {
            extract_float_literal_value(&cd.value.0).map(ConstValue::Float)
        } else {
            None
        };
        self.env.define(cd.name.clone(), actual, false);
        if let Some(value) = const_value {
            let binding_id = self
                .env
                .lookup_ref(&cd.name)
                .expect("constant binding was just defined")
                .id;
            self.const_values.insert(cd.name.clone(), value);
            self.declared_const_bindings
                .insert(cd.name.clone(), binding_id);
        }
        self.record_root_value_binding(&cd.name);
    }

    /// A trait's supertraits are part of its obligation: `trait Error: Display`
    /// means `impl Error for X` promises `X` renders. Report the missing impl
    /// where the promise is made, not at some later call that needs it.
    fn require_supertrait_impls(&mut self, type_name: &str, trait_name: &str, span: &Span) {
        let declared_key = self.trait_defs_key_for_bound(trait_name);
        let mut stack = self
            .trait_super
            .get(&declared_key)
            .cloned()
            .unwrap_or_default();
        let mut visited = std::collections::HashSet::new();
        let type_identity = self.trait_impl_type_identity(type_name);
        while let Some(super_trait) = stack.pop() {
            let super_key = self.trait_defs_key_for_bound(&super_trait);
            if !visited.insert(super_key.clone()) {
                continue;
            }
            if let Some(nested) = self.trait_super.get(&super_key) {
                stack.extend(nested.iter().cloned());
            }
            if self
                .trait_impls_set
                .contains(&(type_identity.clone(), super_key))
            {
                continue;
            }
            let super_display = crate::short_name(&super_trait);
            let trait_display = crate::short_name(trait_name);
            self.report_error_with_suggestions(
                TypeErrorKind::BoundsNotSatisfied,
                span,
                format!(
                    "`impl {trait_display} for {type_name}` requires its supertrait \
                     `{super_display}`, which `{type_name}` does not implement"
                ),
                vec![format!(
                    "write `impl {super_display} for {type_name}` as well"
                )],
            );
        }
    }

    pub(super) fn check_impl(&mut self, id: &ImplDecl, span: &Span) {
        if Self::impl_decl_is_drop_impl(id) {
            // The registration pass already emitted the fail-closed diagnostic.
            // Do not body-check an unsupported destructor and risk cascading
            // errors after its method symbols were deliberately withheld.
            return;
        }
        if let TypeExpr::Named {
            name: type_name,
            type_args: _,
        } = &id.target_type.0
        {
            if let Some(tb) = &id.trait_bound {
                let type_is_local = self.local_type_defs.contains(type_name)
                    || self.intrinsic_type_is_local_to_builtin_surface(type_name);
                // Route through the one canonical resolver: a trait reference is
                // "local" exactly when it resolves to a local declaration (the
                // resolver's local-shadow step), so the orphan-rule warning keys
                // on the same authoritative identity every other trait-reference
                // site does — never the bare spelling in isolation.
                let trait_is_local = self.trait_ref_is_local(&tb.name);
                // hew-compile injects one source-less std.builtins node that
                // contains only the embedded prelude's Display impls. A user
                // module retains a source path and cannot claim this authority.
                let is_embedded_builtins_impl = self.current_item_source.is_none()
                    && self.checking_canonical_stdlib_source("std.builtins");
                if !type_is_local && !trait_is_local && !is_embedded_builtins_impl {
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
                self.require_supertrait_impls(type_name, &tb.name, span);
            }

            // Bind impl-level type params (e.g. T in `impl<T> Wrapper<T>`)
            // so method bodies can reference them.
            let mut generic_bindings = std::collections::HashMap::new();
            if let Some(tps) = &id.type_params {
                for tp in tps {
                    generic_bindings.insert(
                        tp.name.clone(),
                        Ty::Named {
                            builtin: None,
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

            // Resolve the whole target through the source-aware resolver once,
            // while impl generic parameters are in scope. Besides supplying
            // the arguments used to recognize `Self`, this preserves nominal
            // identity for the receiver binding: a source-defined `Option<T>`
            // must not later be reconstructed as builtin `Option<T>`, while a
            // builtin `Vec<T>` must retain its builtin discriminator.
            // The impl's own parameter bounds are what satisfy the target
            // type's declared bounds, so resolve the target inside that scope
            // rather than before it: `impl<T: Clone> St<T>` for a
            // `St<T: Clone>` proves its argument from the impl header.
            let target_bounds = self.collect_type_param_scope_with_bounds(
                id.type_params.as_ref(),
                id.where_clause.as_ref(),
            );
            let pushed_target_bounds = !target_bounds.is_empty();
            if pushed_target_bounds {
                self.current_type_param_bounds.push(TypeParamScope::new(
                    target_bounds,
                    std::collections::HashMap::new(),
                ));
            }
            let resolved_self_binding_ty = self.resolve_type_expr(&id.target_type);
            if pushed_target_bounds {
                self.current_type_param_bounds.pop();
            }
            let prev_self_type = self.current_self_type.take();
            let self_type_args = match &resolved_self_binding_ty {
                Ty::Named { args, .. } => args.clone(),
                _ => Vec::new(),
            };
            self.current_self_type = Some((type_name.clone(), self_type_args.clone()));
            let prev_self_binding_ty = self
                .current_self_binding_ty
                .replace(resolved_self_binding_ty);
            let scope_pushed = self.enter_impl_scope(id, span, Some(type_name.as_str()), true);

            for method in &id.methods {
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
            self.current_self_binding_ty = prev_self_binding_ty;
            if scope_pushed {
                self.exit_impl_scope();
            }
            if pushed_generic {
                self.generic_ctx.pop();
            }
        }
    }

    /// Compiler-intrinsic types have no source declaration to seed
    /// `local_type_defs`. Their canonical declarative surface owns their impls
    /// for coherence purposes, but only when the active item's canonical path
    /// is below the stdlib root owned by the running compiler installation.
    fn intrinsic_type_is_local_to_builtin_surface(&self, type_name: &str) -> bool {
        self.current_item_source.as_ref().is_some_and(|source| {
            self.module_registry
                .source_has_stdlib_authority(source, "std.builtins")
        }) && (Ty::from_name(type_name).is_some()
            || self
                .resolved_builtin_type(type_name)
                .is_some_and(|builtin| !builtin.requires_source_import()))
    }
}

impl Checker {
    fn resolve_param_binding_ty(&mut self, index: usize, param: &Param) -> (Ty, bool) {
        let is_receiver = index == 0 && self.is_receiver_param(param);
        if is_receiver {
            if let Some(receiver_ty) = self.current_self_binding_ty.clone() {
                return (receiver_ty, true);
            }
        }

        let ty = self.resolve_type_expr(&param.ty);
        if !is_receiver {
            return (ty, false);
        }
        let Some((self_name, self_args)) = &self.current_self_type else {
            return (ty, true);
        };

        // Trait declarations and other receiver contexts outside an impl do
        // not have a source-resolved impl target to reuse. Preserve the
        // existing primitive/nominal fallback for those contexts.
        let receiver_ty = Ty::from_name(self_name).unwrap_or_else(|| Ty::Named {
            builtin: None,
            name: self_name.clone(),
            args: self_args.clone(),
        });
        (receiver_ty, true)
    }
}

fn supervisor_local_pid_target(ty: &Ty) -> Option<&str> {
    match ty {
        Ty::Named {
            name,
            args,
            builtin: Some(builtin),
        } if builtin.has_role(crate::builtin_type::BuiltinTypeRole::SupervisorHandle)
            && args.is_empty() =>
        {
            Some(name.as_str())
        }
        _ => None,
    }
}

/// Whether `ty` is a canonical stdlib nominal used by a lifecycle hook.
///
/// Some compilation surfaces attach the compiler builtin discriminator, while
/// module-graph resolution retains the exact source-owned identity instead.
/// Both denote the same std type; a bare or foreign same-short-name user type
/// denotes a different type and must remain rejected.
fn is_canonical_std_named_type(
    ty: &Ty,
    builtin: crate::BuiltinType,
    source_identity: &str,
) -> bool {
    matches!(
        ty,
        Ty::Named {
            name,
            args,
            builtin: resolved_builtin,
        } if args.is_empty()
            && (*resolved_builtin == Some(builtin)
                || (resolved_builtin.is_none() && name == source_identity))
    )
}

/// Lifecycle payload records are source-owned: after resolution, only their
/// canonical owner-qualified source identity proves the hook ABI.  Unlike
/// compiler-intrinsic carriers, a bare builtin discriminator is not authority
/// for these records because it has no accompanying source layout.
fn is_canonical_lifecycle_source_type(ty: &Ty, source_identity: &str) -> bool {
    matches!(
        ty,
        Ty::Named {
            name,
            args,
            builtin: None,
        } if args.is_empty() && name == source_identity
    )
}

#[cfg(test)]
mod lifecycle_std_identity_tests {
    use super::*;

    fn named(name: &str, builtin: Option<crate::BuiltinType>) -> Ty {
        Ty::Named {
            name: name.to_string(),
            args: Vec::new(),
            builtin,
        }
    }

    #[test]
    fn canonical_lifecycle_type_requires_marker_or_exact_source_owner() {
        let builtin = crate::BuiltinType::CrashNotification;
        let source_identity = "failure.CrashNotification";

        assert!(is_canonical_std_named_type(
            &named("CrashNotification", Some(builtin)),
            builtin,
            source_identity,
        ));
        assert!(is_canonical_std_named_type(
            &named(source_identity, None),
            builtin,
            source_identity,
        ));
        assert!(!is_canonical_std_named_type(
            &named("CrashNotification", None),
            builtin,
            source_identity,
        ));
        assert!(!is_canonical_std_named_type(
            &named("foreign.CrashNotification", None),
            builtin,
            source_identity,
        ));
    }
}
