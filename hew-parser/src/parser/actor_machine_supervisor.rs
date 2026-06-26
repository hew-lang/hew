//! Actor, machine, and supervisor declaration parsing.

#[allow(
    clippy::wildcard_imports,
    reason = "grammar-area submodules share the parent parser namespace via the split"
)]
use super::*;

impl Parser<'_> {
    #[expect(
        clippy::too_many_lines,
        reason = "actor decl parsing has many fields and sections"
    )]
    pub(crate) fn parse_actor_decl(&mut self, visibility: Visibility) -> Option<ActorDecl> {
        let name = self.expect_ident()?;

        // Optional `<T, U: Bound>` type-parameter list immediately after the actor name.
        let type_params = if self.eat(&Token::Less) {
            self.parse_type_params()?
        } else {
            vec![]
        };

        let super_traits = self.parse_optional_super_traits()?;

        self.expect(&Token::LeftBrace)?;

        let mut init = None;
        let mut fields = Vec::new();
        let mut receive_fns = Vec::new();
        let mut methods = Vec::new();
        let mut mailbox_capacity = None;
        let mut overflow_policy = None;

        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            // Collect doc comments and attributes in either order.
            let mut doc_comment = self.collect_doc_comments();
            let attrs = self.parse_attributes();
            if doc_comment.is_none() {
                doc_comment = self.collect_doc_comments();
            }

            // `#[extern_symbol]` belongs on `extern "C"` fns and `impl`
            // methods — not on actor body members (init, receive fn,
            // receive gen fn, or inherent methods).
            for attr in &attrs {
                if attr.name == "extern_symbol" {
                    self.error_at(
                        "`#[extern_symbol]` is only valid on `fn` declarations inside an \
                         `extern \"C\"` block or an `impl` block; actor members cannot \
                         bind to a runtime C-ABI symbol"
                            .to_string(),
                        attr.span.clone(),
                    );
                }
            }

            if self.peek() == Some(&Token::Init) {
                if !attrs.is_empty() {
                    self.error("attributes are not supported on init blocks".to_string());
                }
                self.advance();
                self.expect(&Token::LeftParen)?;
                let params = self.parse_params();
                self.expect(&Token::RightParen)?;
                let body = self.parse_block()?;
                init = Some(ActorInit { params, body });
            } else if self.peek() == Some(&Token::Receive) {
                let recv_start = self.peek_span().start;
                self.advance();
                let is_generator = if self.eat(&Token::Gen) {
                    if !self.eat(&Token::Fn) {
                        self.error("expected 'fn' after 'receive gen'".to_string());
                        return None;
                    }
                    true
                } else {
                    if !self.eat(&Token::Fn) {
                        self.error("expected 'fn' after 'receive'".to_string());
                        return None;
                    }
                    false
                };
                let Some(handler_name) = self.expect_ident() else {
                    // Name is missing or is a reserved keyword.  The diagnostic
                    // has already been emitted by `expect_ident`.  Skip tokens
                    // up to the next actor-body item so we don't cascade.
                    self.skip_to_actor_item_boundary();
                    continue;
                };
                let type_params = self.parse_opt_type_params()?;
                self.expect(&Token::LeftParen)?;
                let params = self.parse_params();
                self.expect(&Token::RightParen)?;

                let return_type = self.parse_opt_return_type()?;
                let where_clause = self.parse_opt_where_clause()?;

                let body = self.parse_block()?;
                let recv_end = self.peek_span().start;
                receive_fns.push(ReceiveFnDecl {
                    is_generator,
                    name: handler_name,
                    type_params,
                    params,
                    return_type,
                    where_clause,
                    body,
                    span: recv_start..recv_end,
                    attributes: attrs,
                    doc_comment,
                });
            } else if self.peek() == Some(&Token::Fn) {
                // Lifecycle-hook attributes `#[on(start)]` and `#[on(stop)]` are
                // permitted on plain `fn` declarations inside an actor body.
                // All other attributes on actor methods are rejected: they
                // belong on `receive fn` declarations.
                let mut hook_attrs = Vec::new();
                let mut other_attrs = Vec::new();
                for attr in attrs {
                    if is_lifecycle_hook_attr(&attr.name) {
                        hook_attrs.push(attr);
                    } else {
                        other_attrs.push(attr);
                    }
                }
                if !other_attrs.is_empty() {
                    self.error("attributes are not supported on actor methods; use them on receive fn declarations".to_string());
                }
                let fn_start = self.peek_span().start;
                self.advance();
                if let Some(mut method) =
                    self.parse_function(fn_start, false, false, Visibility::Private, hook_attrs)
                {
                    method.doc_comment = doc_comment;
                    methods.push(method);
                } else {
                    // parse_function emitted its own diagnostic.  Skip to the
                    // next item boundary so one bad method name doesn't cascade.
                    self.skip_to_actor_item_boundary();
                }
            } else if self.peek() == Some(&Token::Let) {
                if !attrs.is_empty() {
                    self.error("attributes are not supported on field declarations".to_string());
                }
                self.advance();
                let field_name = self.expect_ident()?;
                self.expect(&Token::Colon)?;
                let ty = self.parse_type()?;
                let default = if self.eat(&Token::Equal) {
                    Some(self.parse_expr()?)
                } else {
                    None
                };
                if !self.eat(&Token::Semicolon) && self.peek() == Some(&Token::Comma) {
                    self.error("use `;` instead of `,` to separate fields".to_string());
                    self.advance();
                }
                fields.push(FieldDecl {
                    name: field_name,
                    ty,
                    is_mutable: false,
                    default,
                    doc_comment,
                });
            } else if self.peek() == Some(&Token::Var) {
                self.advance();
                let field_name = self.expect_ident()?;
                self.expect(&Token::Colon)?;
                let ty = self.parse_type()?;
                let default = if self.eat(&Token::Equal) {
                    Some(self.parse_expr()?)
                } else {
                    None
                };
                if !self.eat(&Token::Semicolon) && self.peek() == Some(&Token::Comma) {
                    self.error("use `;` instead of `,` to separate fields".to_string());
                    self.advance();
                }
                fields.push(FieldDecl {
                    name: field_name,
                    ty,
                    is_mutable: true,
                    default,
                    doc_comment,
                });
            } else if matches!(self.peek(), Some(Token::Identifier(s)) if *s == "mailbox") {
                self.advance();
                if let Some(Token::Integer(n)) = self.peek() {
                    if let Some(cap) = parse_int_literal(n)
                        .ok()
                        .and_then(|(v, _)| u32::try_from(v).ok())
                    {
                        mailbox_capacity = Some(cap);
                    }
                    self.advance();
                }
                // Parse optional `overflow policy`
                if matches!(self.peek(), Some(Token::Identifier(s)) if *s == "overflow") {
                    self.advance();
                    overflow_policy = self.parse_overflow_policy();
                }
                self.eat(&Token::Semicolon);
            } else if self.peek_is_field_decl() {
                let field_name = self.expect_ident()?;
                self.expect(&Token::Colon)?;
                let ty = self.parse_type()?;
                let default = if self.eat(&Token::Equal) {
                    Some(self.parse_expr()?)
                } else {
                    None
                };
                if !self.eat(&Token::Semicolon) && self.peek() == Some(&Token::Comma) {
                    self.error("use `;` instead of `,` to separate fields".to_string());
                    self.advance();
                }
                fields.push(FieldDecl {
                    name: field_name,
                    ty,
                    is_mutable: false,
                    default,
                    doc_comment,
                });
            } else {
                self.error(format!("unexpected token in actor body: {:?}", self.peek()));
                self.advance(); // error recovery
            }
        }

        self.expect(&Token::RightBrace)?;

        Some(ActorDecl {
            visibility,
            name,
            type_params,
            super_traits,
            init,
            fields,
            receive_fns,
            methods,
            mailbox_capacity,
            overflow_policy,
            is_isolated: false,
            doc_comment: None,
            max_heap_bytes: None, // set by parse_item from outer #[max_heap] attr
        })
    }

    pub(crate) fn parse_overflow_policy(&mut self) -> Option<OverflowPolicy> {
        match self.peek() {
            Some(Token::Identifier(s)) => {
                let policy_name = (*s).to_owned();
                match &*policy_name {
                    "drop_new" => {
                        self.advance();
                        Some(OverflowPolicy::DropNew)
                    }
                    "drop_old" => {
                        self.advance();
                        Some(OverflowPolicy::DropOld)
                    }
                    "block" => {
                        self.advance();
                        Some(OverflowPolicy::Block)
                    }
                    "fail" => {
                        self.advance();
                        Some(OverflowPolicy::Fail)
                    }
                    "coalesce" => {
                        self.advance();
                        self.expect(&Token::LeftParen)?;
                        let key_field = self.expect_ident()?;
                        self.expect(&Token::RightParen)?;
                        let fallback = if matches!(self.peek(), Some(Token::Identifier(s)) if *s == "fallback")
                        {
                            self.advance();
                            match self.peek() {
                                Some(Token::Identifier(s)) => {
                                    let fb = (*s).to_owned();
                                    self.advance();
                                    match &*fb {
                                        "drop_new" => Some(OverflowFallback::DropNew),
                                        "drop_old" => Some(OverflowFallback::DropOld),
                                        "block" => Some(OverflowFallback::Block),
                                        "fail" => Some(OverflowFallback::Fail),
                                        _ => {
                                            self.error_with_hint(
                                                format!("unknown fallback policy '{fb}'"),
                                                "valid fallbacks: drop_new, drop_old, block, fail",
                                            );
                                            None
                                        }
                                    }
                                }
                                _ => None,
                            }
                        } else {
                            None
                        };
                        Some(OverflowPolicy::Coalesce {
                            key_field,
                            fallback,
                        })
                    }
                    _ => {
                        self.error_with_hint(
                            format!("unknown overflow policy '{policy_name}'"),
                            "valid policies: drop_new, drop_old, block, fail, coalesce(key)",
                        );
                        None
                    }
                }
            }
            _ => None,
        }
    }

    pub(crate) fn parse_machine_decl(&mut self, visibility: Visibility) -> Option<MachineDecl> {
        let name = self.expect_ident()?;

        // Optional generic type parameters: `machine Name<T, U> { ... }` or
        // `machine Name<T: Trait, U> { ... }`. Trait bounds are accepted at
        // the parser level — bound enforcement is the type-checker's job
        // (see `docs/specs/HEW-SPEC-2026.md` §3.11.8). Variance markers,
        // defaults, and machine-over-machine generics remain unsupported.
        let (type_params, const_params) = if self.eat(&Token::Less) {
            self.parse_machine_generic_params()?
        } else {
            (Vec::new(), Vec::new())
        };

        // Optional `where T: Trait, U: Trait + Trait` clause between the
        // generic parameter list and the body's `{`. The clause may also
        // appear when no `<…>` list is present (mirrors fn/type/trait
        // sibling decls). Bound enforcement is the type-checker's job;
        // the parser threads the clause verbatim onto `MachineDecl`.
        let where_clause = self.parse_opt_where_clause()?;

        self.expect(&Token::LeftBrace)?;

        let mut states = Vec::new();
        let mut events = Vec::new();
        let mut emits = Vec::new();
        let mut transitions = Vec::new();
        let mut composite_groups = Vec::new();
        let mut has_default = false;

        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            if self.peek_machine_kw("events") {
                // `events { Name; Name { f: T; } … }` — the input-event
                // vocabulary header (contextual keyword; replaces the former
                // interleaved `event Name;` declarations).
                self.advance();
                self.expect(&Token::LeftBrace)?;
                while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
                    let event_name = self.expect_ident()?;
                    let fields = self.parse_machine_event_fields()?;
                    self.eat(&Token::Semicolon);
                    events.push(MachineEvent {
                        name: event_name,
                        fields,
                    });
                }
                self.expect(&Token::RightBrace)?;
            } else if self.peek_machine_kw("emits") {
                // `emits { Name; … }` — optional Mealy-output manifest. Each
                // entry names a declared event the machine may `emit`. Stored
                // as a bare name list; HIR cross-checks emit sites against it.
                self.advance();
                self.expect(&Token::LeftBrace)?;
                while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
                    let emitted = self.expect_ident()?;
                    self.eat(&Token::Semicolon);
                    emits.push(emitted);
                }
                self.expect(&Token::RightBrace)?;
            } else if self.peek() == Some(&Token::State) {
                self.parse_machine_state_or_composite(
                    &mut states,
                    &mut transitions,
                    &mut composite_groups,
                )?;
            } else if self.peek() == Some(&Token::Event) {
                // Legacy interleaved `event Name;` form is a hard cutover —
                // events now live in the `events { … }` header.
                let span = self.peek_span();
                self.error_at(
                    "interleaved `event` declarations are no longer supported; \
                     declare events in an `events { … }` header at the top of the machine body"
                        .to_string(),
                    span,
                );
                self.advance();
            } else if self.peek() == Some(&Token::On) {
                let transition = self.parse_machine_transition()?;
                transitions.push(transition);
            } else if self.peek() == Some(&Token::Default) {
                // `default { state }` — unhandled events stay in current state.
                self.advance();
                if self.eat(&Token::LeftBrace) {
                    let mut depth = 1;
                    while depth > 0 && !self.at_end() {
                        if self.peek() == Some(&Token::LeftBrace) {
                            depth += 1;
                        }
                        if self.peek() == Some(&Token::RightBrace) {
                            depth -= 1;
                            if depth == 0 {
                                break;
                            }
                        }
                        self.advance();
                    }
                    self.expect(&Token::RightBrace)?;
                } else {
                    self.eat(&Token::Semicolon);
                }
                has_default = true;
            } else {
                self.error(
                    "expected `events`, `emits`, `state`, `on`, or `default` in machine body"
                        .to_string(),
                );
                self.advance();
            }
        }

        self.expect(&Token::RightBrace)?;

        // Splice composite entry/exit hooks into every boundary-crossing
        // transition (top-level + expanded) now that the full flat list is
        // assembled. Done as a post-pass so a top-level `Outside => Sk` enter
        // and `Sk => Outside` leave are covered, not just parent-rule clones.
        Self::splice_all_composite_hooks(&mut transitions, &composite_groups);

        Some(MachineDecl {
            visibility,
            name,
            type_params,
            const_params,
            where_clause,
            states,
            events,
            emits,
            transitions,
            has_default,
            composite_groups,
        })
    }

    /// Post-pass: splice composite `entry`/`exit` hooks into every transition
    /// that crosses a composite boundary, for each depth-1 group. A transition
    /// entering composite C (source ∉ C, target ∈ C) gets `C.entry` prepended;
    /// one leaving C (source ∈ C, target ∉ C) gets `C.exit` prepended. With the
    /// MIR firing the source substate's own `exit` before the body and the
    /// target substate's own `entry` after, this yields Harel ordering.
    pub(crate) fn splice_all_composite_hooks(
        transitions: &mut [MachineTransition],
        composite_groups: &[CompositeGroup],
    ) {
        // First resolve any transition targeting a composite BY NAME
        // (`=> Connected`) to that composite's initial substate, so the live
        // target is a real leaf state and the entry chain (D2) splices.
        for group in composite_groups {
            for transition in transitions.iter_mut() {
                if transition.target_state == group.name {
                    transition.target_state.clone_from(&group.initial);
                    // Rewrite a bare-identifier passthrough body that named the
                    // composite to name the initial substate instead.
                    if let Expr::Identifier(name) = &transition.body.0 {
                        if name == &group.name {
                            transition.body.0 = Expr::Identifier(group.initial.clone());
                        }
                    }
                }
            }
        }

        for group in composite_groups {
            let member_set: std::collections::HashSet<String> =
                group.members.iter().cloned().collect();
            for transition in transitions.iter_mut() {
                Self::splice_composite_hooks(
                    transition,
                    &transition.source_state.clone(),
                    &member_set,
                    group.entry.as_ref(),
                    group.exit.as_ref(),
                );
            }
        }
    }

    /// Parse a single `on …` machine transition (the new `=>` / `reenter`
    /// surface, with optional `on E(bindings):` head binding). Used both at the
    /// top level of a machine body and inside composite blocks.
    pub(crate) fn parse_machine_transition(&mut self) -> Option<MachineTransition> {
        self.expect(&Token::On)?;
        let event_name = self.expect_ident()?;

        // Optional head binding: `on E(a, b): …`. The names alias the event's
        // payload fields so the body can reference them directly instead of
        // `event.field`. Threaded into the body as a let-binding prelude by
        // `apply_event_head_bindings`.
        let head_bindings = if self.eat(&Token::LeftParen) {
            let mut binds = Vec::new();
            while !self.at_end() && self.peek() != Some(&Token::RightParen) {
                binds.push(self.expect_ident()?);
                if !self.eat(&Token::Comma) {
                    break;
                }
            }
            self.expect(&Token::RightParen)?;
            binds
        } else {
            Vec::new()
        };

        self.expect(&Token::Colon)?;
        let source_state = self.parse_state_pattern()?;
        // Hard cutover: `=>` (Token::FatArrow) is the state-routing arrow.
        self.expect(&Token::FatArrow)?;
        let target_state = self.parse_state_pattern()?;

        // Optional `reenter` contextual keyword (self-transition Mealy
        // re-entry). Grammar slot: `on E(b): Src => Tgt reenter [when g] [body]`.
        let reenter = self.eat_machine_kw("reenter");

        // Optional guard: `when <expr>`.
        let guard = if self.peek() == Some(&Token::When) {
            self.advance();
            Some(self.parse_expr()?)
        } else {
            None
        };

        // Body forms:
        //   on Event: Source => Target;                     ← no body (unit)
        //   on Event: Source => Target { field: expr, ... } ← struct fields, target inferred
        //   on Event: Source => Target { expression }       ← explicit body
        let (body, body_start, body_end) = if self.eat(&Token::Semicolon) {
            let span_pos = self.peek_span().start;
            let body_expr = Expr::Identifier(target_state.clone());
            (body_expr, span_pos, span_pos)
        } else if target_state != "_" && self.is_struct_init_body() {
            let bs = self.peek_span().start;
            self.expect(&Token::LeftBrace)?;
            let mut fields = Vec::new();
            while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
                let fname = self.expect_ident()?;
                self.expect(&Token::Colon)?;
                let fval = self.parse_expr()?;
                fields.push((fname, fval));
                if !self.eat(&Token::Comma) {
                    break;
                }
            }
            self.expect(&Token::RightBrace)?;
            let be = self.peek_span().start;
            let struct_init = Expr::StructInit {
                name: target_state.clone(),
                fields,
                type_args: None,
                base: None,
            };
            (struct_init, bs, be)
        } else {
            let bs = self.peek_span().start;
            let block = self.parse_block()?;
            let be = self.peek_span().start;
            (Expr::Block(block), bs, be)
        };

        let body = if head_bindings.is_empty() {
            body
        } else {
            Self::apply_event_head_bindings(&head_bindings, body)
        };

        Some(MachineTransition {
            event_name,
            source_state,
            target_state,
            event_bindings: head_bindings,
            composite_prelude_len: 0,
            guard,
            body: (body, body_start..body_end),
            reenter,
        })
    }

    /// Parse the field list of a single event declaration inside `events { }`:
    /// either `;` (no fields) or `{ name: Type; … }`.
    pub(crate) fn parse_machine_event_fields(
        &mut self,
    ) -> Option<Vec<(String, Spanned<TypeExpr>)>> {
        if self.eat(&Token::LeftBrace) {
            let mut fields = Vec::new();
            while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
                let field_name = self.expect_ident()?;
                self.expect(&Token::Colon)?;
                let ty = self.parse_type()?;
                if !self.eat(&Token::Semicolon) {
                    self.eat(&Token::Comma);
                }
                fields.push((field_name, ty));
            }
            self.expect(&Token::RightBrace)?;
            Some(fields)
        } else {
            Some(Vec::new())
        }
    }

    /// Parse a `state Name … ` declaration that begins at the current `state`
    /// token. A leaf state pushes one `MachineState`; a composite (a `state`
    /// body containing substate declarations) desugars to flat substates plus
    /// concrete-source transitions (D1–D5) and records a `CompositeGroup` for
    /// the formatter / diagram renderer.
    pub(crate) fn parse_machine_state_or_composite(
        &mut self,
        states: &mut Vec<MachineState>,
        transitions: &mut Vec<MachineTransition>,
        composite_groups: &mut Vec<CompositeGroup>,
    ) -> Option<()> {
        self.expect(&Token::State)?;
        let state_name = self.expect_ident()?;
        let mut fields: Vec<(String, Spanned<TypeExpr>)> = Vec::new();
        let mut entry_block: Option<Block> = None;
        let mut exit_block: Option<Block> = None;

        if self.eat(&Token::LeftBrace) {
            loop {
                if self.at_end() || self.peek() == Some(&Token::RightBrace) {
                    break;
                }
                if self.peek() == Some(&Token::Entry) {
                    self.advance();
                    entry_block = Some(self.parse_block()?);
                } else if self.peek() == Some(&Token::Exit) {
                    self.advance();
                    exit_block = Some(self.parse_block()?);
                } else if self.peek() == Some(&Token::State) || self.peek_machine_kw("initial") {
                    // Composite block: this `state` owns substates. Hand the
                    // already-parsed prefix (fields, entry, exit) to the
                    // composite parser, which consumes the rest of the brace.
                    return self.parse_composite_block(
                        &state_name,
                        fields,
                        entry_block,
                        exit_block,
                        states,
                        transitions,
                        composite_groups,
                    );
                } else {
                    let field_name = self.expect_ident()?;
                    self.expect(&Token::Colon)?;
                    let ty = self.parse_type()?;
                    if !self.eat(&Token::Semicolon) {
                        self.eat(&Token::Comma);
                    }
                    fields.push((field_name, ty));
                }
            }
            self.expect(&Token::RightBrace)?;
        }
        self.eat(&Token::Semicolon);

        states.push(MachineState {
            name: state_name,
            fields,
            entry: entry_block,
            exit: exit_block,
        });
        Some(())
    }

    /// Parse a depth-1 composite-state block and desugar it to the flat state
    /// and transition lists, recording a `CompositeGroup` for the formatter and
    /// diagram renderer. The `state`/`initial state` cursor is positioned at
    /// the first substate; `composite_name`/`fields`/`entry`/`exit` are the
    /// prefix already parsed by the caller.
    ///
    /// Desugar (all at parser/AST level, per the hierarchy contract):
    ///   * each substate becomes a flat `MachineState`; composite-owned fields
    ///     are stamped onto every member (shared-layout shorthand).
    ///   * D1: each parent-level `on E: _ => T { body }` expands to one
    ///     transition per member `Sk` with a CONCRETE `source_state = "Sk"`
    ///     (never literal `_`, which the checker rejects for `self.field`).
    ///     An explicit per-member `(Sk, E)` rule wins (override-skip).
    ///   * D2/D3: composite `entry`/`exit` hooks splice into transition bodies
    ///     so the existing MIR per-state hook firing yields Harel ordering:
    ///     enter C ⇒ `C.entry` then child entry; leave C ⇒ child exit then
    ///     `C.exit`. Intra-composite moves fire no composite hook (D4).
    #[expect(
        clippy::too_many_arguments,
        reason = "threads the leaf-parse prefix plus the machine-body \
                  accumulators the composite desugar appends to"
    )]
    pub(crate) fn parse_composite_block(
        &mut self,
        composite_name: &str,
        fields: Vec<(String, Spanned<TypeExpr>)>,
        entry: Option<Block>,
        exit: Option<Block>,
        states: &mut Vec<MachineState>,
        transitions: &mut Vec<MachineTransition>,
        composite_groups: &mut Vec<CompositeGroup>,
    ) -> Option<()> {
        let mut members: Vec<MachineState> = Vec::new();
        let mut member_names: Vec<String> = Vec::new();
        let mut initial: Option<String> = None;
        let mut parent_transitions: Vec<MachineTransition> = Vec::new();

        // ── Substate + parent-rule body of the composite block. ──────────────
        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            let is_initial = self.eat_machine_kw("initial");
            if self.peek() == Some(&Token::State) {
                let substate = self.parse_machine_substate(composite_name)?;
                if is_initial {
                    if initial.is_some() {
                        self.error_at(
                            format!(
                                "composite state `{composite_name}` declares more than one \
                                 `initial` substate; exactly one is required"
                            ),
                            self.peek_span(),
                        );
                    }
                    initial = Some(substate.name.clone());
                }
                member_names.push(substate.name.clone());
                members.push(substate);
            } else if is_initial {
                self.error_at(
                    "`initial` must be followed by a substate declaration \
                     (`initial state Name`)"
                        .to_string(),
                    self.peek_span(),
                );
                break;
            } else if self.peek() == Some(&Token::On) {
                // Parent-level rule. Source is `_` (any member of THIS
                // composite); kept verbatim on the group for the formatter and
                // expanded to concrete members below.
                parent_transitions.push(self.parse_machine_transition()?);
            } else {
                self.error_at(
                    format!(
                        "expected a substate (`state`/`initial state`) or a parent-level \
                         `on …` rule inside composite state `{composite_name}`"
                    ),
                    self.peek_span(),
                );
                break;
            }
        }
        self.expect(&Token::RightBrace)?;
        self.eat(&Token::Semicolon);

        let Some(initial_name) = initial else {
            self.error_at(
                format!(
                    "composite state `{composite_name}` must mark exactly one substate \
                     `initial` (`initial state Name`)"
                ),
                self.peek_span(),
            );
            return Some(());
        };

        // ── Desugar to the flat lists. ───────────────────────────────────────
        let member_set: std::collections::HashSet<String> = member_names.iter().cloned().collect();

        // Stamp composite-owned fields onto every member (shared layout).
        for member in &mut members {
            for (fname, fty) in &fields {
                if !member.fields.iter().any(|(n, _)| n == fname) {
                    member.fields.push((fname.clone(), fty.clone()));
                }
            }
        }

        // Explicit per-member rules already authored (inside the block as
        // `on E: Sk => …`, or at the machine top level). Used for D1
        // override-skip so an explicit rule beats the expanded parent rule.
        let explicit_keys: std::collections::HashSet<(String, String)> = transitions
            .iter()
            .chain(parent_transitions.iter())
            .filter(|t| member_set.contains(&t.source_state))
            .map(|t| (t.source_state.clone(), t.event_name.clone()))
            .collect();

        // D1: expand each parent rule to one concrete-source transition per
        // member. Composite entry/exit hooks are spliced uniformly in a
        // post-pass (`splice_all_composite_hooks`) once every transition —
        // top-level and expanded — is in the flat list, so boundary-crossing
        // top-level transitions are covered too.
        for pt in &parent_transitions {
            for member in &member_names {
                if explicit_keys.contains(&(member.clone(), pt.event_name.clone())) {
                    continue;
                }
                let mut expanded = pt.clone();
                expanded.source_state.clone_from(member);
                transitions.push(expanded);
            }
        }

        composite_groups.push(CompositeGroup {
            name: composite_name.to_string(),
            members: member_names,
            initial: initial_name,
            entry,
            exit,
            fields,
            parent_transitions,
        });

        for member in members {
            states.push(member);
        }
        Some(())
    }

    /// Parse a single substate declaration (`state Name;` /
    /// `state Name { fields; entry {} exit {} }`). A `state` inside a substate
    /// body is depth>1 nesting, rejected with a v0.6 diagnostic.
    pub(crate) fn parse_machine_substate(&mut self, composite_name: &str) -> Option<MachineState> {
        self.expect(&Token::State)?;
        let name = self.expect_ident()?;
        let mut fields = Vec::new();
        let mut entry_block: Option<Block> = None;
        let mut exit_block: Option<Block> = None;
        if self.eat(&Token::LeftBrace) {
            while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
                if self.peek() == Some(&Token::Entry) {
                    self.advance();
                    entry_block = Some(self.parse_block()?);
                } else if self.peek() == Some(&Token::Exit) {
                    self.advance();
                    exit_block = Some(self.parse_block()?);
                } else if self.peek() == Some(&Token::State) || self.peek_machine_kw("initial") {
                    self.error_at(
                        format!(
                            "nested composite states (depth > 1) are reserved for v0.6; \
                             substate `{name}` of composite `{composite_name}` may not contain \
                             further substates"
                        ),
                        self.peek_span(),
                    );
                    // Recover: skip the nested block.
                    let mut depth = 0;
                    while !self.at_end() {
                        match self.peek() {
                            Some(Token::LeftBrace) => depth += 1,
                            Some(Token::RightBrace) => {
                                if depth == 0 {
                                    break;
                                }
                                depth -= 1;
                            }
                            _ => {}
                        }
                        self.advance();
                    }
                } else {
                    let field_name = self.expect_ident()?;
                    self.expect(&Token::Colon)?;
                    let ty = self.parse_type()?;
                    if !self.eat(&Token::Semicolon) {
                        self.eat(&Token::Comma);
                    }
                    fields.push((field_name, ty));
                }
            }
            self.expect(&Token::RightBrace)?;
        }
        self.eat(&Token::Semicolon);
        Some(MachineState {
            name,
            fields,
            entry: entry_block,
            exit: exit_block,
        })
    }

    /// Splice composite `entry`/`exit` hook statements into a transition body
    /// so the existing MIR per-state hook firing produces Harel ordering. The
    /// composite hooks are prepended as body statements; MIR runs the source
    /// substate's own `exit` before the body and the target substate's own
    /// `entry` after it, so:
    ///
    ///   * leaving C (source ∈ C, target ∉ C): MIR `Sk.exit` → spliced
    ///     `C.exit` → next.
    ///   * entering C (source ∉ C, target ∈ C): spliced `C.entry` → MIR
    ///     `Sk.entry`.
    ///   * intra-composite moves splice nothing (both endpoints ∈ C).
    pub(crate) fn splice_composite_hooks(
        transition: &mut MachineTransition,
        source_member: &str,
        member_set: &std::collections::HashSet<String>,
        entry: Option<&Block>,
        exit: Option<&Block>,
    ) {
        let target = &transition.target_state;
        let source_in = member_set.contains(source_member);
        let target_in = target != "_" && member_set.contains(target);

        // Prepend order matters: statements pushed last end up first. For a
        // cross-composite move we want `exit-of-source-composite` before
        // `entry-of-target-composite`; here each call handles ONE composite, so
        // a single prepend per relevant hook is correct.
        if source_in && !target_in {
            if let Some(exit_block) = exit {
                Self::prepend_block_stmts(transition, exit_block);
            }
        } else if !source_in && target_in {
            if let Some(entry_block) = entry {
                Self::prepend_block_stmts(transition, entry_block);
            }
        }
        // Intra-composite (source_in && target_in) or unrelated: no splice.
    }

    /// Prepend a hook block's statements to the front of a transition body,
    /// wrapping a non-block body into a block whose tail is the original value.
    pub(crate) fn prepend_block_stmts(transition: &mut MachineTransition, hook: &Block) {
        let body = std::mem::replace(&mut transition.body.0, Expr::Identifier(String::new()));
        let mut block = match body {
            Expr::Block(block) => block,
            other => Block {
                stmts: Vec::new(),
                trailing_expr: Some(Box::new((other, transition.body.1.clone()))),
            },
        };
        let mut prelude = hook.stmts.clone();
        // If the hook block has a trailing expression (rare for entry/exit),
        // treat it as a statement so it runs for its effect.
        if let Some(tail) = &hook.trailing_expr {
            prelude.push((
                Stmt::Expression((tail.0.clone(), tail.1.clone())),
                tail.1.clone(),
            ));
        }
        // Record how many leading statements are composite-hook splices so the
        // formatter can strip them and re-emit the authored transition body.
        transition.composite_prelude_len += prelude.len();
        prelude.append(&mut block.stmts);
        block.stmts = prelude;
        transition.body.0 = Expr::Block(block);
    }

    /// Rewrite a transition body so the named head bindings (`on E(a, b): …`)
    /// are in scope as `let a = event.a; let b = event.b;` prelude statements.
    /// This lowers identically to writing `event.a` directly — no new HIR kind.
    pub(crate) fn apply_event_head_bindings(bindings: &[String], body: Expr) -> Expr {
        let mut stmts: Vec<Spanned<Stmt>> = Vec::with_capacity(bindings.len());
        for name in bindings {
            let value = Expr::FieldAccess {
                object: Box::new((Expr::Identifier("event".to_string()), 0..0)),
                field: name.clone(),
            };
            stmts.push((
                Stmt::Let {
                    pattern: (Pattern::Identifier(name.clone()), 0..0),
                    ty: None,
                    value: Some((value, 0..0)),
                    else_block: None,
                },
                0..0,
            ));
        }
        // Splice the prelude in front of the existing body. A bare expression /
        // struct-init body becomes the block's tail; an existing block has the
        // prelude prepended to its statements.
        match body {
            Expr::Block(mut block) => {
                let mut all = stmts;
                all.append(&mut block.stmts);
                block.stmts = all;
                Expr::Block(block)
            }
            other => Expr::Block(Block {
                stmts,
                trailing_expr: Some(Box::new((other, 0..0))),
            }),
        }
    }

    /// Parse a state pattern: `_` (wildcard) or a state name, optionally a
    /// dotted qualified name (`Composite.Leaf`). The dotted form is a
    /// readability aid — the parser strips it to the leaf name, which is what
    /// reaches the AST (substate names are flat and globally unique).
    pub(crate) fn parse_state_pattern(&mut self) -> Option<String> {
        if matches!(self.peek(), Some(Token::Identifier(name)) if *name == "_") {
            self.advance();
            return Some("_".to_string());
        }
        let mut name = self.expect_ident()?;
        // Strip any qualifier prefix: `Composite.Leaf` → `Leaf`.
        while self.peek() == Some(&Token::Dot) {
            self.advance();
            name = self.expect_ident()?;
        }
        Some(name)
    }

    /// Check if the next tokens look like a struct init body: `{ ident: expr }`.
    /// Used to detect `on Event: S -> T { field: expr }` shorthand.
    pub(crate) fn is_struct_init_body(&self) -> bool {
        // Peek at `{`, then `ident`, then `:` — if all three, it's struct init
        if self.peek() != Some(&Token::LeftBrace) {
            return false;
        }
        // Look ahead: tokens[pos+1] should be Identifier, tokens[pos+2] should be Colon
        let pos = self.pos;
        if pos + 2 >= self.tokens.len() {
            return false;
        }
        matches!(
            (&self.tokens[pos + 1].0, &self.tokens[pos + 2].0),
            (Token::Identifier(_), Token::Colon)
        )
    }

    #[expect(
        clippy::too_many_lines,
        reason = "supervisor parsing requires sequential field handling"
    )]
    pub(crate) fn parse_supervisor_decl(
        &mut self,
        visibility: Visibility,
    ) -> Option<SupervisorDecl> {
        let name = self.expect_ident()?;

        // Optional construction-time config params: `supervisor App(config: T)`.
        // Mirrors the actor `init(params)` shape; the child init-arg exprs in the
        // body reference these bindings, so a child's init value can derive from
        // runtime config (the v0.6 init-closure restart model's dynamic source).
        let params = if self.eat(&Token::LeftParen) {
            let params = self.parse_params();
            self.expect(&Token::RightParen)?;
            params
        } else {
            Vec::new()
        };

        self.expect(&Token::LeftBrace)?;

        let mut strategy = None;
        let mut intensity = None;
        let mut children = Vec::new();

        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            match self.peek() {
                Some(Token::Strategy) => {
                    self.advance();
                    self.expect(&Token::Colon)?;
                    strategy = match self.peek() {
                        Some(Token::OneForOne) => {
                            self.advance();
                            Some(SupervisorStrategy::OneForOne)
                        }
                        Some(Token::OneForAll) => {
                            self.advance();
                            Some(SupervisorStrategy::OneForAll)
                        }
                        Some(Token::RestForOne) => {
                            self.advance();
                            Some(SupervisorStrategy::RestForOne)
                        }
                        Some(Token::SimpleOneForOne) => {
                            self.advance();
                            Some(SupervisorStrategy::SimpleOneForOne)
                        }
                        _ => None,
                    };
                    if !self.eat(&Token::Semicolon) {
                        self.eat(&Token::Comma);
                    }
                }
                // `intensity: N within <duration>` — the restart-budget contract.
                // Fuses the legacy `max_restarts:` + `window:` fields. `within`
                // is a contextual keyword (a plain identifier outside this body).
                Some(Token::Identifier(s)) if *s == "intensity" => {
                    self.advance();
                    self.expect(&Token::Colon)?;
                    let restarts = if let Some(Token::Integer(num_str)) = self.peek() {
                        let n = parse_int_literal(num_str).ok().map(|(v, _)| v);
                        self.advance();
                        n
                    } else {
                        self.error_with_hint(
                            "supervisor `intensity:` requires a restart count, \
                             e.g. `intensity: 5 within 60s`"
                                .to_string(),
                            "write the maximum number of restarts as an integer",
                        );
                        None
                    };
                    // The `within` contextual keyword separates the count from
                    // the window duration. Require it so the budget reads as one
                    // English clause and can never be half-specified.
                    if matches!(self.peek(), Some(Token::Identifier(w)) if *w == "within") {
                        self.advance();
                    } else {
                        self.error_with_hint(
                            "supervisor `intensity:` requires `within <duration>`, \
                             e.g. `intensity: 5 within 60s`"
                                .to_string(),
                            "add `within` followed by a duration literal (60s, 5m, 1h)",
                        );
                    }
                    // The window is a real `Token::Duration` literal. A bare
                    // integer is a parse error with a fix-it — the implicit-unit
                    // ambiguity of the old `window:` field is gone.
                    let window = match self.peek() {
                        Some(Token::Duration(d)) => {
                            let d = d.to_string();
                            self.advance();
                            Some(d)
                        }
                        Some(Token::Integer(n)) => {
                            let n = n.to_string();
                            self.error_with_hint(
                                format!(
                                    "supervisor `intensity:` window must be a duration literal, \
                                     not a bare integer `{n}`"
                                ),
                                format!("write `{n}s` (or another unit: {n}m, {n}h)"),
                            );
                            self.advance();
                            None
                        }
                        _ => {
                            self.error_with_hint(
                                "supervisor `intensity:` requires a duration window, \
                                 e.g. `intensity: 5 within 60s`"
                                    .to_string(),
                                "add a duration literal after `within` (60s, 5m, 1h)",
                            );
                            None
                        }
                    };
                    if let (Some(restarts), Some(window)) = (restarts, window) {
                        intensity = Some(Intensity { restarts, window });
                    }
                    if !self.eat(&Token::Semicolon) {
                        self.eat(&Token::Comma);
                    }
                }
                // Legacy `max_restarts:` / `window:` fields — removed in the
                // flat-reliability-fields cutover. Emit a migration diagnostic
                // naming the new `intensity:` form, then skip the field.
                Some(Token::Identifier(s)) if *s == "max_restarts" || *s == "window" => {
                    let field = (*s).to_string();
                    self.error_with_hint(
                        format!(
                            "supervisor field `{field}:` was replaced by the fused \
                             `intensity: N within <duration>` field"
                        ),
                        "write `intensity: <max_restarts> within <window>s`, \
                         e.g. `intensity: 5 within 60s`",
                    );
                    self.advance();
                    self.expect(&Token::Colon)?;
                    // Skip the value up to the field terminator for recovery.
                    while !self.at_end()
                        && !matches!(
                            self.peek(),
                            Some(Token::Semicolon | Token::Comma | Token::RightBrace)
                        )
                    {
                        self.advance();
                    }
                    if !self.eat(&Token::Semicolon) {
                        self.eat(&Token::Comma);
                    }
                }
                Some(Token::Child | Token::Pool) => {
                    let is_pool = matches!(self.peek(), Some(Token::Pool));
                    self.advance();
                    let child_name = self.expect_ident()?;
                    self.expect(&Token::Colon)?;
                    // Child type position accepts the module-qualified dotted
                    // form (`child a: bank.Account(...)`), matching spawn and
                    // method-call qualification. The dotted string is the
                    // actor's qualified identity downstream (checker
                    // `type_defs["bank.Account"]`, MIR layout key); a bare
                    // name stays the root/local identity.
                    let mut actor_type = self.expect_ident()?;
                    if self.eat(&Token::Dot) {
                        let type_name = self.expect_ident()?;
                        actor_type = format!("{actor_type}.{type_name}");
                    }

                    // Parse named init args: `child w: Worker(field: expr, ...)`.
                    // Mirrors plain `spawn Worker(field: expr, ...)` at parser.rs:6047.
                    // Positional args (no `name:` prefix) are rejected with a migration
                    // diagnostic to guide users to the named form.
                    let mut args: Vec<(String, Spanned<Expr>)> = Vec::new();
                    if self.eat(&Token::LeftParen) {
                        while !self.at_end() && !matches!(self.peek(), Some(Token::RightParen)) {
                            // Try to parse `ident_or_kw: expr` (named form). Speculatively
                            // consume the potential field name; if a `:` follows, commit.
                            // If no `:` follows, it's a positional arg — reject it.
                            let saved = self.save_pos();
                            let maybe_field = self.expect_ident();
                            if let Some(field_name) = maybe_field {
                                if !matches!(self.peek(), Some(Token::Colon)) {
                                    // Ident not followed by `:` — positional arg.
                                    self.restore_pos(saved);
                                    self.error(
                                        "supervisor child init args must use named form: \
                                         `child w: Worker(field: value)` \
                                         — positional args are not accepted"
                                            .to_string(),
                                    );
                                    while !self.at_end()
                                        && !matches!(self.peek(), Some(Token::RightParen))
                                    {
                                        self.advance();
                                    }
                                    break;
                                }
                                // Named form confirmed: `field_name: expr`.
                                self.expect(&Token::Colon)?;
                                let value = self.parse_expr()?;
                                args.push((field_name, value));
                            } else {
                                // Either the ident parse failed or no `:` follows — positional.
                                self.restore_pos(saved);
                                self.error(
                                    "supervisor child init args must use named form: \
                                     `child w: Worker(field: value)` \
                                     — positional args are not accepted"
                                        .to_string(),
                                );
                                // Consume through to `)` for error recovery.
                                while !self.at_end()
                                    && !matches!(self.peek(), Some(Token::RightParen))
                                {
                                    self.advance();
                                }
                                break;
                            }
                            if !self.eat(&Token::Comma) {
                                break;
                            }
                        }
                        self.expect(&Token::RightParen)?;
                    }

                    // Legacy bare restart keyword (e.g. `child n: T permanent`)
                    // was removed in the flat-reliability-fields cutover. The
                    // only restart spelling is the `restart: <policy>` clause.
                    if matches!(
                        self.peek(),
                        Some(Token::Permanent | Token::Transient | Token::Temporary)
                    ) {
                        self.error_with_hint(
                            "bare restart keyword on a supervisor child was removed; \
                             use the `restart:` clause"
                                .to_string(),
                            "write `restart: permanent` (or `transient` / `temporary`)",
                        );
                        self.advance();
                    }
                    // Legacy `with restart:` form was removed; only `restart:`.
                    if matches!(self.peek(), Some(Token::Identifier(s)) if *s == "with") {
                        self.error_with_hint(
                            "the `with restart:` form was removed; \
                             use the `restart:` clause directly"
                                .to_string(),
                            "drop `with` and write `restart: <policy>`",
                        );
                        self.advance(); // consume `with`
                    }

                    // Per-child suffix clauses, accepted in any order:
                    //   restart: permanent | transient | temporary
                    //   shutdown: <duration> | brutal_kill | infinity
                    //   wired_to: { param: sibling, bare_sibling }
                    let mut restart: Option<RestartPolicy> = None;
                    let mut shutdown: Option<ShutdownDirective> = None;
                    let mut wired_to: Option<std::collections::HashMap<String, String>> = None;
                    loop {
                        match self.peek() {
                            // `restart: <policy>` clause (the only restart spelling).
                            Some(Token::Restart) => {
                                self.advance();
                                self.expect(&Token::Colon)?;
                                restart = match self.peek() {
                                    Some(Token::Permanent) => {
                                        self.advance();
                                        Some(RestartPolicy::Permanent)
                                    }
                                    Some(Token::Transient) => {
                                        self.advance();
                                        Some(RestartPolicy::Transient)
                                    }
                                    Some(Token::Temporary) => {
                                        self.advance();
                                        Some(RestartPolicy::Temporary)
                                    }
                                    _ => {
                                        self.error_with_hint(
                                            "supervisor child `restart:` requires a policy"
                                                .to_string(),
                                            "write `restart: permanent` \
                                             (or `transient` / `temporary`)",
                                        );
                                        None
                                    }
                                };
                            }
                            // `shutdown: <duration> | brutal_kill | infinity` clause.
                            // `infinity` is accepted-only in v0.5 (no per-child
                            // deadline wheel yet) and is a contextual keyword.
                            Some(Token::Identifier(s)) if *s == "shutdown" => {
                                self.advance();
                                self.expect(&Token::Colon)?;
                                shutdown = match self.peek() {
                                    Some(Token::Duration(d)) => {
                                        let d = d.to_string();
                                        self.advance();
                                        Some(ShutdownDirective::Timeout(d))
                                    }
                                    Some(Token::BrutalKill) => {
                                        self.advance();
                                        Some(ShutdownDirective::BrutalKill)
                                    }
                                    Some(Token::Identifier(k)) if *k == "infinity" => {
                                        self.advance();
                                        Some(ShutdownDirective::Infinity)
                                    }
                                    Some(Token::Integer(n)) => {
                                        let n = n.to_string();
                                        self.error_with_hint(
                                            format!(
                                                "supervisor child `shutdown:` must be a duration \
                                                 literal, `brutal_kill`, or `infinity`, \
                                                 not a bare integer `{n}`"
                                            ),
                                            format!("write `{n}s` (or another unit: {n}ms, {n}m)"),
                                        );
                                        self.advance();
                                        None
                                    }
                                    _ => {
                                        self.error_with_hint(
                                            "supervisor child `shutdown:` requires a duration, \
                                             `brutal_kill`, or `infinity`"
                                                .to_string(),
                                            "write `shutdown: 30s`, `shutdown: brutal_kill`, \
                                             or `shutdown: infinity`",
                                        );
                                        None
                                    }
                                };
                            }
                            // `wired_to: { key: sibling, bare_sibling }` clause.
                            Some(Token::Identifier(s)) if *s == "wired_to" => {
                                self.advance(); // consume `wired_to`
                                self.expect(&Token::Colon)?;
                                self.expect(&Token::LeftBrace)?;
                                let mut map = std::collections::HashMap::new();
                                while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
                                    let key = self.expect_ident()?;
                                    if self.eat(&Token::Colon) {
                                        // explicit `key: sibling_name` form
                                        let val = self.expect_ident()?;
                                        map.insert(key, val);
                                    } else {
                                        // shorthand `sibling_name` — key == value
                                        map.insert(key.clone(), key);
                                    }
                                    if !self.eat(&Token::Comma) {
                                        break;
                                    }
                                }
                                self.expect(&Token::RightBrace)?;
                                wired_to = if map.is_empty() { None } else { Some(map) };
                            }
                            _ => break,
                        }
                    }

                    if !self.eat(&Token::Semicolon) {
                        self.eat(&Token::Comma);
                    }
                    children.push(ChildSpec {
                        name: child_name,
                        actor_type,
                        args,
                        restart,
                        wired_to,
                        is_pool,
                        shutdown,
                    });
                }
                _ => {
                    self.error(format!("unknown supervisor field: {:?}", self.peek()));
                    self.advance();
                    // skip to next comma or closing brace
                    while self.peek() != Some(&Token::Comma)
                        && self.peek() != Some(&Token::RightBrace)
                        && self.peek().is_some()
                    {
                        self.advance();
                    }
                }
            }
        }

        self.expect(&Token::RightBrace)?;

        Some(SupervisorDecl {
            visibility,
            name,
            params,
            strategy,
            intensity,
            children,
        })
    }
}
