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
                    self.check_actor(ad);
                }
            }
            Item::Const(cd) => self.check_const(cd, span),
            Item::Impl(id) => self.check_impl(id, span),
            Item::Machine(md) => {
                if !crate::ty::is_reserved_type_name(&md.name) {
                    self.check_machine_exhaustiveness(md, span);
                }
            }
            Item::Trait(td) => {
                if !crate::ty::is_reserved_type_name(&td.name) {
                    self.check_trait_defaults(td);
                }
            }
            // All of these are fully handled during earlier registration passes
            // and require no second-pass body checking.  Record declarations
            // specifically are registered by `register_record_decl`; they have
            // no method bodies, variants, or wire attributes in v0.5.
            Item::Record(_)
            | Item::Import(_)
            | Item::TypeDecl(_)
            | Item::TypeAlias(_)
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
        // A dotted child type (`child a: bank.Account`) references the
        // module's actor; mark the import used so the program does not get a
        // spurious unused-import warning when the supervisor is the only
        // reference.
        for child in &sd.children {
            if let Some((module, _)) = child.actor_type.split_once('.') {
                if self.modules.contains(module) {
                    self.used_modules.borrow_mut().insert(ImportKey::new(
                        self.current_module.clone(),
                        module.to_string(),
                    ));
                }
            }
        }

        // ── 1. Duplicate child names ─────────────────────────────────────────
        self.check_supervisor_duplicate_children(sd, span);

        // ── 2. Strategy / pool consistency ──────────────────────────────────
        self.check_supervisor_strategy_pool(sd, span);

        // ── 3. wired_to key resolution + type compatibility ──────────────────
        self.check_supervisor_wired_to(sd, span);

        // ── 4. Dependency cycle detection ────────────────────────────────────
        self.check_supervisor_wired_to_cycles(sd, span);

        // ── 5. Permanent children must not have owned-heap state fields ──────
        self.check_supervisor_permanent_owned_heap(sd, span);

        // ── 6. Intensity restart-budget sanity ──────────────────────────────
        self.check_supervisor_intensity(sd, span);

        // ── 7. Children must not declare #[every] periodic handlers ──────────
        self.check_supervisor_periodic_children(sd, span);

        // ── 8. Type-check child init-arg expressions against the config
        //       params, then validate the resolved arg types (byte-copy wall).
        //       Step 8 binds `sd.params` (the construction-time config params,
        //       `supervisor App(config: T)`) in a fresh scope and synthesises
        //       the type of each child init-arg EXPRESSION so a `config.field`
        //       read resolves through the config struct's record layout and
        //       lands in `expr_types`. HIR/MIR lowering reads those resolved
        //       types (`object.ty = Named{config_ty}`, `source_expr.ty = field
        //       type`) to emit the init-closure thunk; without this step the
        //       init-arg exprs are never type-checked and `config.field` carries
        //       no resolved type. The resolved arg type is also what the
        //       BitCopy wall now validates (a scalar `config.field` passes; an
        //       owned one is still walled until the owned init thunk lands).
        self.check_supervisor_init_args(sd, span);
    }

    /// Bind the supervisor's construction-time config params in scope, type-
    /// check every child's init-arg expressions, and validate the resolved arg
    /// types against the byte-copy wall.
    ///
    /// The config params (`supervisor App(config: T)`) are in scope only for
    /// the child init-arg expressions (`child cache: Cache(capacity:
    /// config.size)`), mirroring the actor `init(params)` shape. Each init-arg
    /// expression is synthesised so `config.field` resolves through the config
    /// struct's record layout to the field's type and `expr_types` carries both
    /// the field-access result type and the config-param identifier type — the
    /// resolved discriminators HIR stamps onto the HIR exprs and MIR reads to
    /// lower the `ConfigField` init arg.
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

        // Synthesise the type of every child's init-arg expressions so the
        // resolved types land in `expr_types` (the discriminator HIR/MIR read).
        // Surfacing real errors here (`config.nonexistent_field`, a config-field
        // / actor-param type mismatch) is a new user-facing diagnostic.
        //
        // Pool children now route their per-member init args through the same
        // init-closure thunk path as static children (one shared template,
        // re-run per member), so their init-arg exprs are type-checked here too.
        // The reserved `count:` arg designates the pool size — it is checked
        // separately by `check_supervisor_pool_count`, not as a per-member init
        // field — so it is excluded from the per-member synthesis here.
        for child in &sd.children {
            for (arg_name, arg_expr) in &child.args {
                if child.is_pool && arg_name == "count" {
                    continue;
                }
                self.synthesize(&arg_expr.0, &arg_expr.1);
            }
        }

        // Validate the resolved init-arg types against the byte-copy wall while
        // the config params are still in scope.
        self.check_supervisor_init_args_bitcopy(sd, span);

        // Validate every pool child's reserved `count:` arg (presence, integer
        // type, positive literal) while config params are still in scope so a
        // `count: config.workers` expr resolves.
        self.check_supervisor_pool_count(sd, span);

        self.env.pop_scope();
    }

    /// Validate the reserved `count:` arg on every `pool` child declaration.
    ///
    /// A static pool (`pool workers: Worker(count: N)`) spawns exactly N
    /// fungible members at bootstrap. The `count` arg is REQUIRED on a pool
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

            let count_arg = child.args.iter().find(|(name, _)| name == "count");

            let Some((_, count_expr)) = count_arg else {
                // A pool declares a fixed-size fleet; without `count:` the size
                // is undefined. Fail closed rather than defaulting to a silent 1.
                self.errors.push(TypeError::new(
                    TypeErrorKind::SupervisorError {
                        subkind: SupervisorErrorKind::PoolCountMissing,
                    },
                    span.clone(),
                    format!(
                        "E_SUPERVISOR_POOL_COUNT_MISSING: supervisor `{}` pool child `{}` is \
                         missing the required `count:` argument; a static pool declares a \
                         fixed-size fleet, e.g. `pool {}: {}(count: 5)`",
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

    /// Reject supervisor children whose actor type declares `#[every(duration)]`
    /// periodic receive handlers.
    ///
    /// WHY: periodic timers are armed by spawn-site codegen
    /// (`emit_periodic_handler_arming`, hew-codegen-rs/src/llvm.rs), but
    /// supervisor children are spawned — and restarted — by the runtime from
    /// a `HewChildSpec`, a path that never reaches the codegen spawn site.
    /// The child's timers would silently never fire, and even spawn-site
    /// arming could not survive a restart (`hew_actor_free` cancels all
    /// timers for the crashed instance). Fail-closed: reject at check time
    /// rather than ship a silent no-op.
    /// WHEN-OBSOLETE: when `HewChildSpec` carries a periodic-handler table
    /// (`msg_type` + interval per handler) and the runtime arms timers in the
    /// child-start path AND re-arms them on restart.
    /// WHAT: extend `HewChildSpec` + the supervisor child-start/restart paths
    /// in `hew-runtime/src/supervisor.rs` with that table, then delete this
    /// check and flip its tests to accept.
    fn check_supervisor_periodic_children(&mut self, sd: &SupervisorDecl, span: &Span) {
        for child in &sd.children {
            if let Some(handler) = self.actors_with_periodic_handlers.get(&child.actor_type) {
                self.errors.push(TypeError::new(
                    TypeErrorKind::SupervisorError {
                        subkind: SupervisorErrorKind::PeriodicChild,
                    },
                    span.clone(),
                    format!(
                        "E_SUPERVISOR_PERIODIC_CHILD: supervisor `{}` child `{}` (actor `{}`) \
                         declares #[every] periodic handler `{}`; periodic handlers are not yet \
                         armed for supervisor-spawned children — spawn the actor directly, or \
                         drive `{}` with explicit sends",
                        sd.name, child.name, child.actor_type, handler, handler
                    ),
                ));
            }
        }
    }

    /// Reject supervised child init args unless the actor init-parameter type is
    /// reproducible by the init-closure restart thunk.
    ///
    /// The v0.6 init-closure restart model re-runs every init arg on each
    /// incarnation: a scalar `config.field` is re-loaded and an owned
    /// `string`/`bytes` field is deep-cloned per restart, so each child gets a
    /// fresh, unaliased owned value. `ty_is_supervisor_init_reproducible` admits
    /// scalars together with `string` and `bytes` (the types the thunk has a
    /// per-field clone for). Owned collections, records, enums, generic, alias,
    /// user-defined, and `#[resource]` handle types stay walled (fail-closed):
    /// their clone-in-thunk codegen is not wired (collections/records) or is
    /// structurally forbidden (re-cloning a handle would alias a live resource).
    fn check_supervisor_init_args_bitcopy(&mut self, sd: &SupervisorDecl, _span: &Span) {
        for child in &sd.children {
            // Only children with explicit init args carry init-arg types.
            if child.args.is_empty() {
                continue;
            }

            // Look up the actor's init parameter list.  Unknown actors are
            // handled elsewhere; skip here to avoid duplicate diagnostics.
            let Some(init_params) = self.actor_init_params.get(&child.actor_type).cloned() else {
                continue;
            };

            for (arg_name, _arg_expr) in &child.args {
                // A pool child's reserved `count:` arg is the pool size, not a
                // per-member init field — it is validated by
                // `check_supervisor_pool_count`, not the reproducibility wall.
                if child.is_pool && arg_name == "count" {
                    continue;
                }

                // Find the matching init parameter by name.
                let Some(param) = init_params.iter().find(|param| param.name == *arg_name) else {
                    // Missing-param errors are reported elsewhere (wired_to check
                    // or MIR lowering); skip here.
                    continue;
                };

                // The wall is keyed on the actor init-PARAMETER type: that type
                // is what the child's state field stores, so it is what the
                // init-closure thunk must re-produce per incarnation. A scalar
                // param (`capacity: i64`) is reproducible by a load; an owned
                // `string`/`bytes` by a per-field deep-clone; everything else
                // stays walled (fail-closed). The arg-EXPRESSION typing that
                // `config.field` requires happens in `check_supervisor_init_args`'
                // synthesis pass; this is the param-type gate.
                if !ty_is_supervisor_init_reproducible(&param.ty) {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::SupervisorError {
                            subkind: SupervisorErrorKind::InitArgNonBitcopy,
                        },
                        param.span.clone(),
                        format!(
                            "E_SUPERVISOR_INIT_ARG_NON_BITCOPY: supervisor `{}` child `{}` \
                             (actor `{}`) passes init arg `{}` of type `{}`; supervised actor \
                             init args are re-produced by the init-closure restart model on \
                             every restart. Scalar primitives (`i8`..`u64`, `f32`, `f64`, \
                             `bool`, `char`) and owned `string` / `bytes` are admitted (the \
                             thunk loads or deep-clones them per incarnation). Owned \
                             collections, records, enums, generic, alias, user-defined, and \
                             `#[resource]` handle types are rejected — their clone-in-thunk \
                             codegen is not wired (or is structurally forbidden for handles)",
                            sd.name,
                            child.name,
                            child.actor_type,
                            arg_name,
                            param.ty.user_facing()
                        ),
                    ));
                }
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

    /// Guard against C1 UAF: a supervisor with a permanent restart policy will
    /// byte-copy `spec.init_state` into the fresh actor on restart.  If the
    /// actor's state contains an owned-heap field (Vec, String, `HashMap`,
    /// `HashSet`, Bytes), that byte-copy aliases the pointer from the crashed
    /// actor, and the next `state_drop_fn` call produces a use-after-free.
    ///
    /// This check is a hard compile error per R89 ("stop the compile until we
    /// can address it").  Full fix (`init_state_clone_fn`) is tracked as
    /// v0.5.0.1 P0.
    ///
    /// EXEMPTION (v0.6 init-closure restart model): a child whose owned field is
    /// supplied by a REPRODUCIBLE init arg (`name: config.label` where `name` is
    /// a `string`) takes the init-thunk path — codegen produces the field by a
    /// per-incarnation deep-clone, not a byte-copy template. That is exactly the
    /// structural fix this wall flags as deferred, so such a field is no longer a
    /// UAF hazard and is exempt. Owned fields NOT covered by a reproducible init
    /// arg still byte-copy and stay walled.
    fn check_supervisor_permanent_owned_heap(&mut self, sd: &SupervisorDecl, span: &Span) {
        for child in &sd.children {
            // Pool children are dynamically spawned, not restarted from a
            // fixed spec, so they are exempt from this check.
            if child.is_pool {
                continue;
            }

            // RestartPolicy::None defaults to permanent per llvm.rs:3013.
            let is_permanent = child.restart.is_none_or(|p| p == RestartPolicy::Permanent);
            if !is_permanent {
                continue;
            }

            // Look up the actor's TypeDef.  If the type is unknown or is not
            // an actor, a separate diagnostic already covers it.
            let Some(type_def) = self.type_defs.get(&child.actor_type).cloned() else {
                continue;
            };
            if type_def.kind != TypeDefKind::Actor {
                continue;
            }

            // Init params for the child's actor, used to resolve whether an init
            // arg covering an owned field is reproducible (init-thunk path).
            let init_params = self.actor_init_params.get(&child.actor_type).cloned();

            for (field_name, field_ty) in &type_def.fields {
                if !ty_is_known_owned_heap(field_ty) {
                    continue;
                }
                // Exempt the field if the child supplies a reproducible init arg
                // for it: the init thunk deep-clones it per incarnation, so the
                // byte-copy aliasing hazard does not apply. An arg `name`
                // resolves either to a state field of the same name or to an
                // init param of that name; both route the value into the field.
                let covered_by_reproducible_init = child.args.iter().any(|(arg_name, _)| {
                    if arg_name != field_name {
                        return false;
                    }
                    // The arg names this field. It is reproducible if the field
                    // type itself is reproducible (state-field arg) or the
                    // matching init param type is (init-param arg).
                    if ty_is_supervisor_init_reproducible(field_ty) {
                        return true;
                    }
                    init_params.as_ref().is_some_and(|params| {
                        params
                            .iter()
                            .find(|p| p.name == *arg_name)
                            .is_some_and(|p| ty_is_supervisor_init_reproducible(&p.ty))
                    })
                });
                if covered_by_reproducible_init {
                    continue;
                }
                self.errors.push(TypeError::new(
                    TypeErrorKind::SupervisorError {
                        subkind: SupervisorErrorKind::PermanentOwnedHeap,
                    },
                    span.clone(),
                    format!(
                        "E_SUPERVISOR_PERMANENT_OWNED_HEAP: supervisor `{}` child `{}` \
                         (actor `{}`) has field `{}` of type `{}` which is an owned-heap \
                         type; restarting a permanent child byte-copies init_state, \
                         aliasing the heap pointer from the crashed actor and causing a \
                         use-after-free on the next state_drop_fn call — use \
                         `restart: transient` or `restart: temporary`, or remove owned-heap \
                         fields from the actor state; full fix (init_state_clone_fn) tracked \
                         as v0.5.0.1 P0",
                        sd.name, child.name, child.actor_type, field_name, field_ty
                    ),
                ));
            }
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
                // with type `LocalPid<sibling_type>`.
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
    /// `LocalPid<sibling_type>`. Emits `E_SUPERVISOR_WIRED_TO_TYPE_MISMATCH` on failure.
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
                     `{param_key}` but `{dependent_actor_type}::init` has no parameter named \
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
                     `{expected_sibling_type}`, but `{dependent_actor_type}::init` parameter \
                     `{param_key}` has type `{}` (expected `LocalPid<{expected_sibling_type}>`)",
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
        let fn_name = scoped_module_item_name(self.current_module.as_deref(), &fd.name)
            .unwrap_or_else(|| fd.name.clone());
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
        self.tail_ok_armed = !fd.is_generator && resolved_expected_ret.as_result().is_some();
        let actual = self.check_block(&fd.body, block_expected);
        self.tail_ok_armed = prev_tail_ok_armed;
        actual
    }

    /// Check a function body using `fn_name` for the `fn_sigs` lookup.
    ///
    /// Impl methods are registered under qualified names (e.g. `Connection::close`)
    /// but `FnDecl::name` is bare (e.g. `close`). Using the qualified name prevents
    /// collisions with builtins or inlined functions from other modules.
    pub(super) fn check_function_as(&mut self, fd: &FnDecl, fn_name: &str) {
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
        // borrowed-Rc escape scanner (`warn_rc_param_return`, run at the end of
        // this same call) consumes only this function's pattern spans. Clear so
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

        // Bind params — only the first parameter can be the receiver
        for (i, p) in fd.params.iter().enumerate() {
            let (ty, is_receiver) = self.resolve_param_binding_ty(i, p);
            self.reject_ineffective_mutable_value_param(p, &ty, is_receiver);
            // If inside an actor, check that params don't shadow actor fields
            if in_actor {
                self.check_shadowing(&p.name, &p.ty.1);
            }
            self.env
                .define_param_with_span(p.name.clone(), ty, p.is_mutable, p.ty.1.clone());
        }

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
        let actual =
            self.check_body_with_tail_ok_coercion(fd, &resolved_expected_ret, block_expected);
        // A completely empty body on a method whose `Self` is a compiler
        // builtin (`LocalPid`, `RemotePid`, `Vec`, …) is a fail-closed
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
        // The actor's checker identity: dotted `{module_short}.{name}` for a
        // module actor (the body is checked with `current_module` set), bare
        // for root/flat actors. All signature lookups during body checking
        // (`fn_sigs["{identity}::{rf}"]`), the `this` receiver type, and the
        // max-heap table key must use the same identity the registration
        // pass authored, or a same-named actor from another module would be
        // consulted instead.
        let identity = Self::actor_identity(self.current_module_short(), &ad.name);
        let actor_ty = Ty::Named {
            builtin: None,
            name: identity.clone(),
            args: vec![],
        };
        let prev_actor_type = self.current_actor_type.replace(actor_ty);
        let prev_actor_fields = std::mem::replace(
            &mut self.current_actor_fields,
            ad.fields
                .iter()
                .map(|f| ActorFieldInfo {
                    name: f.name.clone(),
                    is_mutable: f.is_mutable,
                    decl_span: f.ty.1.clone(),
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

        self.check_actor_field_defaults(ad);

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
                "upgrade" => {
                    self.reject_upgrade_hook(&ad.name, &method.name, hook_attr.span.clone());
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
                         valid hook kinds are: start, stop, crash, exit, upgrade"
                    ),
                ));
                return None;
            }
            Some("start" | "stop" | "crash" | "exit" | "upgrade") => {}
            Some(unknown) => {
                self.errors.push(TypeError::new(
                    TypeErrorKind::InvalidOperation,
                    hook_attr.span.clone(),
                    format!(
                        "`#[on({unknown})]` on `{actor_name}::{method_name}` is not a recognised \
                         lifecycle hook; valid hook kinds are: start, stop, crash, exit, upgrade"
                    ),
                ));
                return None;
            }
        }
        let hook_kind_str = hook_kind.unwrap();

        // Reject `#[on(crash, …)]` / `#[on(exit, …)]` with extra arguments.
        // start/stop already reject extra args via `check_lifecycle_hook`'s
        // signature checks; for crash/exit we validate the attribute shape here
        // because their signature/body checking is event-specific.
        if (hook_kind_str == "crash" || hook_kind_str == "exit") && hook_attr.args.len() > 1 {
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

    fn reject_upgrade_hook(&mut self, actor_name: &str, method_name: &str, attr_span: Span) {
        // `#[on(upgrade)]` is a reserved attribute with no runtime behaviour:
        // the runtime never invokes it, so accepting it would create a hook
        // that silently never runs. Reject it fail-closed.
        self.errors.push(TypeError::new(
            TypeErrorKind::OnUpgradeNotYetWired,
            attr_span,
            format!(
                "`#[on(upgrade)]` on `{actor_name}::{method_name}` is reserved and not supported: \
                 the runtime never invokes this hook, so it would silently never run; \
                 remove the attribute"
            ),
        ));
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

    /// Bind actor fields as writable regardless of declared mutability.
    ///
    /// `init { }` is the actor's constructor: it must be able to assign
    /// `let` fields their initial values, so the immutable-field rule does
    /// not apply inside the init body.
    pub(super) fn bind_actor_fields_for_init(&mut self, fields: &[FieldDecl]) {
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

        // Bind actor fields directly in scope (bare field access, all
        // writable in the init body — init is where `let` fields receive
        // their initial values). Hew uses bare names, not `self.field`.
        self.bind_actor_fields_for_init(fields);

        // Bind init parameters
        for p in &init.params {
            let ty = self.resolve_annotation_with_holes(
                &p.ty,
                format!("init parameter `{}` of actor `{actor_name}`", p.name),
            );
            self.env
                .define_param_with_span(p.name.clone(), ty, p.is_mutable, p.ty.1.clone());
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
    /// parameter typed `CrashInfo`.  Diagnostics live here rather than in
    /// `check_crash_hook` to keep that entry under the clippy line limit.
    fn check_crash_hook_param(&mut self, actor_name: &str, hook: &FnDecl, hook_kind: &str) {
        match hook.params.as_slice() {
            [p] => {
                let pty = self.resolve_type_expr(&p.ty);
                let is_crash_info = matches!(
                    &pty,
                    Ty::Named {
                        builtin: Some(crate::BuiltinType::CrashInfo),
                        args,
                        ..
                    } if args.is_empty()
                );
                if !is_crash_info {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::InvalidOperation,
                        p.ty.1.clone(),
                        format!(
                            "lifecycle hook `#[{hook_kind}]` on `{actor_name}::{}` parameter \
                             must have type `CrashInfo` (from `std::failure`)",
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
            if !matches!(
                &ty,
                Ty::Named {
                    builtin: Some(crate::BuiltinType::CrashAction),
                    args,
                    ..
                } if args.is_empty()
            ) {
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
                builtin: None,
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

        // Param: exactly one `note: CrashNotification`. CrashNotification is a
        // std/failure.hew type (not a compiler builtin), so match by resolved
        // type name rather than a builtin marker.
        match hook.params.as_slice() {
            [p] => {
                let pty = self.resolve_type_expr(&p.ty);
                let is_crash_notification = matches!(
                    &pty,
                    Ty::Named { name, args, .. }
                        if name == "CrashNotification" && args.is_empty()
                );
                if !is_crash_notification {
                    self.errors.push(TypeError::new(
                        TypeErrorKind::InvalidOperation,
                        p.ty.1.clone(),
                        format!(
                            "lifecycle hook `#[{hook_kind}]` on `{actor_name}::{}` parameter \
                             must have type `CrashNotification` (from `std::failure`)",
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
                        "lifecycle hook `#[{hook_kind}]` on `{actor_name}::{}` must return `()`; \
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
        let Some(opaque_name) = self.ty_field_contains_opaque(ty, &mut visiting) else {
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
        let expected_ret = if rf.is_generator {
            Ty::Unit
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
        self.in_actor_handler_context = prev_actor_handler_context;
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
        if Self::impl_decl_is_drop_impl(id) {
            // The registration pass already emitted the fail-closed diagnostic.
            // Do not body-check an unsupported destructor and risk cascading
            // errors after its method symbols were deliberately withheld.
            return;
        }
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
                // Route through the one canonical resolver: a trait reference is
                // "local" exactly when it resolves to a local declaration (the
                // resolver's local-shadow step), so the orphan-rule warning keys
                // on the same authoritative identity every other trait-reference
                // site does — never the bare spelling in isolation.
                let trait_is_local = self.trait_ref_is_local(&tb.name);
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
                // Inherent (non-trait) struct impl methods still reject a
                // mutable receiver: there is no trait contract that a `var
                // self` receiver could satisfy, so mutations on a by-value
                // receiver in an inherent method would be local to the
                // callee's stack frame with no path to the caller.
                //
                // Trait impl methods (the `trait_bound.is_some()` arm) lift
                // this gate: the trait declaration is the authoritative
                // contract for receiver mutability, and the impl-vs-trait
                // signature equivalence check (Q004, see
                // `check_impl_method_against_trait`) enforces that the
                // impl's receiver mutability matches what the trait
                // declared. Callers receive a separate "receiver requires
                // mutable binding" diagnostic at the call site when they
                // try to dispatch through a non-`var` binding.
                //
                // LESSONS row `diagnostic-trust`: keep the diagnostic
                // surface alive on the inherent-impl path; do not silently
                // accept what was previously rejected on the trait-impl
                // path — the trait-impl arm now relies on the trait
                // declaration + the equivalence check + the call-site
                // gate to cover the cases this diagnostic used to flag.
                if target_is_struct && id.trait_bound.is_none() {
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
                            "`var self` on an inherent impl method has no effect — \
                             inherent methods receive self by value with no trait contract \
                             to make the mutation observable to the caller"
                                .to_string(),
                            vec![
                                "return a modified copy of the receiver instead".to_string(),
                                "declare the method on a trait whose receiver is `var self`, \
                                 then implement that trait for this type"
                                    .to_string(),
                                "convert this type to an actor if you need mutable shared state"
                                    .to_string(),
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

/// Fail-closed scalar allowlist for supervised child init args.
///
/// Authority: this enumerates the scalar `Ty` variants from the checker-owned
/// primitive set in `hew-types/src/ty.rs` (`PRIMITIVE_ALIASES` and
/// `Ty::from_canonical_primitive_name`): fixed-width ints, floats, `bool`, and
/// `char`. Other primitive variants in that set (`isize`, `usize`, `string`,
/// `bytes`, `duration`, unit, never) are deliberately not admitted.
fn ty_is_supervisor_init_bitcopy_scalar(ty: &Ty) -> bool {
    matches!(
        ty,
        Ty::I8
            | Ty::I16
            | Ty::I32
            | Ty::I64
            | Ty::U8
            | Ty::U16
            | Ty::U32
            | Ty::U64
            | Ty::F32
            | Ty::F64
            | Ty::Bool
            | Ty::Char
    )
}

/// Whether a supervised child init-param type can be re-produced by the
/// init-closure restart thunk on every incarnation.
///
/// Admits:
/// - every scalar `BitCopy` primitive (re-produced by a plain load), and
/// - the owned heap types the init thunk has a per-field deep-clone for:
///   `string` (allocating clone) and `bytes` (refcounted clone). A restarted
///   child gets a fresh, unaliased owned value per incarnation.
///
/// Rejects (stays walled, fail-closed):
/// - owned collections (`Vec`/`HashMap`/`HashSet`), user records, enums, and
///   tuples — their per-field clone-in-thunk codegen is not yet wired, so
///   admitting them would reach a codegen path that cannot deep-clone;
/// - `#[resource]` handle types — re-cloning a handle per restart would alias a
///   live resource (double-close / UAF). Handles are never reproducible.
///
/// WHEN-OBSOLETE: when the init thunk grows per-field clone for collections /
/// records / enums (reusing the actor state-clone spine), widen this predicate
/// to those kinds; `#[resource]` handles stay rejected permanently.
fn ty_is_supervisor_init_reproducible(ty: &Ty) -> bool {
    ty_is_supervisor_init_bitcopy_scalar(ty) || matches!(ty, Ty::String | Ty::Bytes)
}

impl Checker {
    fn resolve_param_binding_ty(&mut self, index: usize, param: &Param) -> (Ty, bool) {
        let ty = self.resolve_type_expr(&param.ty);
        let is_receiver = index == 0 && self.is_receiver_param(param);
        if !is_receiver {
            return (ty, false);
        }
        let Some((self_name, self_args)) = &self.current_self_type else {
            return (ty, true);
        };

        // Primitive receivers bind to canonical primitives so in-body method
        // calls route through primitive dispatch instead of nominal lookup.
        let receiver_ty = if self_args.is_empty() {
            Ty::from_name(self_name).unwrap_or_else(|| Ty::Named {
                builtin: None,
                name: self_name.clone(),
                args: self_args.clone(),
            })
        } else {
            Ty::Named {
                builtin: None,
                name: self_name.clone(),
                args: self_args.clone(),
            }
        };
        (receiver_ty, true)
    }

    fn reject_ineffective_mutable_value_param(
        &mut self,
        param: &Param,
        ty: &Ty,
        is_receiver: bool,
    ) {
        let resolved_param_ty = self.subst.resolve(ty);
        if !param.is_mutable
            || is_receiver
            || !self.is_non_copy_aggregate_param_type(&resolved_param_ty)
        {
            return;
        }
        self.report_error_with_suggestions(
            TypeErrorKind::MutabilityError,
            &param.ty.1,
            format!(
                "`var {}` on a by-value parameter of type `{}` has no caller-visible effect",
                param.name,
                resolved_param_ty.user_facing()
            ),
            vec![
                "return the modified value to the caller".to_string(),
                "move the mutation into an actor or a mutable receiver method".to_string(),
            ],
        );
    }

    fn is_non_copy_aggregate_param_type(&self, ty: &Ty) -> bool {
        match ty {
            Ty::Named {
                builtin: None,
                name,
                ..
            } => self.lookup_type_def(name).is_some() && !self.vec_element_has_copy_layout(ty),
            Ty::Tuple(_) | Ty::Array(_, _) => !ty.is_copy(),
            _ => false,
        }
    }
}

fn supervisor_local_pid_target(ty: &Ty) -> Option<&str> {
    match ty {
        Ty::Named {
            args,
            builtin: Some(builtin),
            ..
        } if builtin.has_role(crate::builtin_type::BuiltinTypeRole::SupervisorLocalPid) => {
            match args.as_slice() {
                [Ty::Named { name, args, .. }] if args.is_empty() => Some(name.as_str()),
                _ => None,
            }
        }
        _ => None,
    }
}

/// Returns `true` for types that carry owned heap allocations and therefore
/// cannot be safely byte-copied as `init_state` for a permanent supervisor
/// child restart (C1 UAF guard — v0.5.0.1 P0).
///
/// Covers `String`, `Bytes`, and the three generic collections `Vec<_>`,
/// `HashMap<_,_>`, `HashSet<_>`.  Nested ownership (e.g. `Vec<Vec<i64>>`) is
/// detected at the outer level.  Fields typed as user-defined records that
/// *contain* owned-heap types are a known residual gap; see the
/// `KNOWN-RESIDUAL` test in check/tests.rs.
fn ty_is_known_owned_heap(ty: &Ty) -> bool {
    matches!(ty, Ty::String | Ty::Bytes)
        || matches!(
            ty,
            Ty::Named {
                builtin: Some(
                    crate::BuiltinType::Vec
                        | crate::BuiltinType::HashMap
                        | crate::BuiltinType::HashSet
                ),
                ..
            }
        )
}
