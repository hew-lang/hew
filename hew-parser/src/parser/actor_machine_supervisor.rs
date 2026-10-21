//! Actor, machine, and supervisor declaration parsing.

#[allow(
    clippy::wildcard_imports,
    reason = "grammar-area submodules share the parent parser namespace via the split"
)]
use super::*;

/// Which side of a machine transition head a state pattern occupies.
///
/// The two positions have different grammars on purpose. `Source` is a
/// pattern matched against the machine's current state — it has no expected
/// type and therefore has no contextual (`.Variant`) form. `Target` is a state
/// name in the machine's own namespace (HEW-SPEC-2026 §3.11.3), written bare;
/// the contextual spelling is tolerated there and carries no bare-variant lint
/// either way.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum StatePatternPosition {
    Source,
    Target,
}

/// A parsed state pattern together with the authored spelling and source span.
pub(crate) struct StatePattern {
    pub name: Ident,
    pub path: Vec<Ident>,
    /// True when the author wrote the contextual `.Variant` form. Only ever
    /// true in `StatePatternPosition::Target`.
    pub is_contextual: bool,
    /// Span of the pattern as authored, including a leading `.` when present.
    pub span: Span,
}

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
        let mut mailbox_span = None;

        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            // Collect doc comments and attributes in either order.
            let mut doc_comment = self.collect_doc_comments();
            let attrs = self.parse_attributes();
            if doc_comment.is_none() {
                doc_comment = self.collect_doc_comments();
            }
            let member_start = self.peek_span().start;

            if self.peek() == Some(&Token::Init) {
                // No attribute is legal on an `init` block.
                self.validate_attributes_for(&attrs, AttrPosition::Unsupported);
                self.advance();
                // `init { .. }` is the parameterless form written without its
                // list. Refuse it with the spelling and keep the block, so the
                // rest of the actor still parses.
                let params = if self.peek() == Some(&Token::LeftBrace) {
                    self.error_with_hint(
                        "`init` takes a parameter list".to_string(),
                        "write `init() { .. }`",
                    );
                    Vec::new()
                } else {
                    self.expect(&Token::LeftParen)?;
                    let params = self.parse_params();
                    self.expect(&Token::RightParen)?;
                    params
                };
                let body = self.parse_block()?;
                init = Some(ActorInit {
                    params,
                    body,
                    span: member_start..self.last_token_end,
                });
                self.refuse_mark_after_body();
            } else if self.peek() == Some(&Token::Receive) {
                self.validate_attributes_for(&attrs, AttrPosition::ActorReceiveFn);
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
                self.refuse_mark_after_body();
            } else if self.peek() == Some(&Token::Fn) {
                // Lifecycle-hook attributes `#[on(start)]` and `#[on(stop)]` are
                // permitted on plain `fn` declarations inside an actor body.
                // All other attributes on actor methods are rejected: they
                // belong on `receive fn` declarations.
                self.validate_attributes_for(&attrs, AttrPosition::ActorMemberFn);
                let hook_attrs: Vec<_> = attrs
                    .into_iter()
                    .filter(|attr| {
                        is_lifecycle_hook_attr(&attr.name)
                            || attr.name == "intercept"
                            || !super::attributes::is_builtin_attribute(&attr.name)
                    })
                    .collect();
                let fn_start = self.peek_span().start;
                self.advance();
                if let Some(mut method) =
                    self.parse_function(fn_start, false, Visibility::Private, hook_attrs)
                {
                    method.doc_comment = doc_comment;
                    methods.extend(self.expand_member_attributes(method));
                    self.refuse_mark_after_body();
                } else {
                    // parse_function emitted its own diagnostic.  Skip to the
                    // next item boundary so one bad method name doesn't cascade.
                    self.skip_to_actor_item_boundary();
                }
            } else if self.peek() == Some(&Token::Let) {
                self.validate_attributes_for(&attrs, AttrPosition::Unsupported);
                self.advance();
                let (field_name, name_span) = self.expect_ident_spanned()?;
                self.expect(&Token::Colon)?;
                let ty = self.parse_type()?;
                let default = if self.eat(&Token::Equal) {
                    Some(self.parse_expr()?)
                } else {
                    None
                };
                self.expect_member_terminator("field");
                fields.push(FieldDecl {
                    name: field_name,
                    name_span,
                    ty,
                    is_mutable: false,
                    default,
                    doc_comment,
                    span: member_start..self.last_token_end,
                });
            } else if self.peek() == Some(&Token::Var) {
                self.validate_attributes_for(&attrs, AttrPosition::Unsupported);
                self.advance();
                let (field_name, name_span) = self.expect_ident_spanned()?;
                self.expect(&Token::Colon)?;
                let ty = self.parse_type()?;
                let default = if self.eat(&Token::Equal) {
                    Some(self.parse_expr()?)
                } else {
                    None
                };
                self.expect_member_terminator("field");
                fields.push(FieldDecl {
                    name: field_name,
                    name_span,
                    ty,
                    is_mutable: true,
                    default,
                    doc_comment,
                    span: member_start..self.last_token_end,
                });
            } else if matches!(self.peek(), Some(Token::Identifier(s)) if *s == "mailbox") {
                self.validate_attributes_for(&attrs, AttrPosition::Unsupported);
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
                self.expect_member_terminator("field");
                mailbox_span = Some(member_start..self.last_token_end);
            } else if self.peek_is_field_decl() {
                self.validate_attributes_for(&attrs, AttrPosition::Unsupported);
                self.error_at_with_kind_and_hint(
                    "actor state is declared with `let` or `var`".to_string(),
                    member_start..member_start,
                    "insert `let ` before the field",
                    ParseDiagnosticKind::ActorFieldBinding,
                );
                let (field_name, name_span) = self.expect_ident_spanned()?;
                self.expect(&Token::Colon)?;
                let ty = self.parse_type()?;
                let default = if self.eat(&Token::Equal) {
                    Some(self.parse_expr()?)
                } else {
                    None
                };
                self.expect_member_terminator("field");
                fields.push(FieldDecl {
                    name: field_name,
                    name_span,
                    ty,
                    is_mutable: false,
                    default,
                    doc_comment,
                    span: member_start..self.last_token_end,
                });
            } else {
                self.error(format!(
                    "unexpected token in actor body: {}",
                    self.peek()
                        .map_or("end of file".to_string(), ToString::to_string)
                ));
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
            mailbox_span,
            is_isolated: false,
            is_confined: false,
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

    #[expect(
        clippy::too_many_lines,
        reason = "machine body parsing has one arm per section"
    )]
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

        let body_span = self.expect(&Token::LeftBrace)?;
        let body_context = self.contexts.at(&body_span);

        let mut states = Vec::new();
        let mut events = Vec::new();
        let mut emits = Vec::new();
        let mut transitions = Vec::new();
        let mut composite_groups = Vec::new();
        let mut has_default = false;

        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            if self.peek_machine_kw("events") {
                // `events { Name, Name { f: T }, … }` — the input-event
                // vocabulary header (contextual keyword; replaces the former
                // interleaved `event Name;` declarations).
                self.advance();
                self.expect(&Token::LeftBrace)?;
                while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
                    let event_start = self.peek_span().start;
                    let event_name = self.expect_ident()?;
                    let bodied = self.peek() == Some(&Token::LeftBrace);
                    let fields = self.parse_machine_event_fields()?;
                    if bodied {
                        self.refuse_mark_after_body();
                    } else {
                        self.expect_member_terminator("event");
                    }
                    events.push(MachineEvent {
                        name: event_name,
                        fields,
                        span: event_start..self.last_token_end,
                    });
                }
                self.expect(&Token::RightBrace)?;
                self.refuse_mark_after_body();
            } else if self.peek_machine_kw("emits") {
                // Outputs have their own typed vocabulary, separate from inputs.
                self.advance();
                self.expect(&Token::LeftBrace)?;
                while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
                    let emitted_start = self.peek_span().start;
                    let emitted = self.expect_ident()?;
                    let bodied = self.peek() == Some(&Token::LeftBrace);
                    let fields = self.parse_machine_event_fields()?;
                    if bodied {
                        self.refuse_mark_after_body();
                    } else {
                        self.expect_member_terminator("emission");
                    }
                    emits.push(MachineEvent {
                        name: emitted,
                        fields,
                        span: emitted_start..self.last_token_end,
                    });
                }
                self.expect(&Token::RightBrace)?;
                self.refuse_mark_after_body();
            } else if self.peek_machine_kw("parallel") {
                self.parse_machine_parallel(&mut states, &mut composite_groups)?;
            } else if self.peek() == Some(&Token::State) {
                self.parse_machine_state_or_composite(&mut states, &mut composite_groups)?;
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
            } else if self.eat_machine_kw("default") {
                // `default { state }` — unhandled events stay in current state.
                // One spelling: a bare `default` or `default;` said the same
                // thing and is refused (D480).
                if self.peek() == Some(&Token::LeftBrace) {
                    let block = self.parse_block()?;
                    if !block.stmts.is_empty()
                        || !matches!(block.trailing_expr.as_deref(), Some((Expr::Ident(name), _)) if name.name.as_str() == "state" && name.ctx == body_context)
                    {
                        self.error("machine default must be `default { state }`; use an explicit rule for computation".to_string());
                    }
                } else {
                    let span = self.peek_span();
                    self.error_at_with_hint(
                        "machine default must be `default { state }`".to_string(),
                        span,
                        "write the identity body: `default { state }`",
                    );
                    self.eat(&Token::Semicolon);
                }
                has_default = true;
                self.refuse_mark_after_body();
            } else {
                self.error(
                    "expected `events`, `emits`, `state`, `on`, or `default` in machine body"
                        .to_string(),
                );
                self.advance();
            }
        }

        self.expect(&Token::RightBrace)?;

        Some(MachineDecl {
            visibility,
            name,
            body_context,
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

    /// The field names a transition head binds: `{ a, b }`, or the retired
    /// `(a, b)`, which recovers to the same names with a fix-it.
    fn parse_event_head_bindings(
        &mut self,
        rule_start: usize,
        event_name: Ident,
    ) -> Option<Vec<Spanned<Ident>>> {
        let legacy = self.peek() == Some(&Token::LeftParen);
        let close_token = if legacy {
            Token::RightParen
        } else {
            Token::RightBrace
        };
        let open = self.peek_span();
        self.advance();
        let mut binds = Vec::new();
        while !self.at_end() && self.peek() != Some(&close_token) {
            let start = self.peek_span().start;
            let name = self.expect_ident()?;
            binds.push((name, start..self.last_token_end));
            if !self.eat(&Token::Comma) {
                break;
            }
        }
        let close = self.peek_span();
        self.expect(&close_token)?;
        let names = binds
            .iter()
            .map(|(name, _)| name.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        if legacy {
            let fix = if binds.is_empty() {
                format!("on {event_name}:")
            } else {
                format!("on {event_name} {{ {names} }}:")
            };
            self.error_at_with_kind_and_hint(
                "a transition head binds event fields in braces".to_string(),
                open.start..close.end,
                format!("write `{fix}`"),
                ParseDiagnosticKind::LegacyEventHead,
            );
        } else if binds.is_empty() {
            self.error_at_with_kind_and_hint(
                "an empty field list is written without braces".to_string(),
                rule_start..close.end,
                format!("write `on {event_name}:`"),
                ParseDiagnosticKind::EmptyKeyBraces,
            );
        }
        Some(binds)
    }

    /// Parse a single `on …` machine transition (the new `=>` / `reenter`
    /// surface, with optional `on E { bindings }:` head binding). Used both at the
    /// top level of a machine body and inside composite blocks.
    pub(crate) fn parse_machine_transition(&mut self) -> Option<MachineTransition> {
        let rule_start = self.peek_span().start;
        self.expect(&Token::On)?;
        let event_name = self.expect_ident()?;

        // Optional head binding: `on E { a, b }: …`. The names alias the
        // event's payload fields so the guard and body can reference them
        // directly instead of `event.field`. Machine normalization binds them.
        let head_bindings = match self.peek() {
            Some(Token::LeftBrace | Token::LeftParen) => {
                self.parse_event_head_bindings(rule_start, event_name)?
            }
            _ => Vec::new(),
        };

        self.expect(&Token::Colon)?;
        let source = self.parse_state_pattern(StatePatternPosition::Source)?;
        let source_state = source.name;
        // Hard cutover: `=>` (Token::FatArrow) is the state-routing arrow.
        self.expect(&Token::FatArrow)?;
        let target = self.parse_state_pattern(StatePatternPosition::Target)?;
        let StatePattern {
            name: target_state,
            path: target_path,
            is_contextual: target_is_contextual,
            span: target_span,
        } = target;

        // Optional `reenter` contextual keyword (self-transition Mealy
        // re-entry). Grammar slot: `on E { b }: Src => Tgt reenter [when g] [body]`.
        let reenter = self.eat_machine_kw("reenter");

        // Optional guard: `when <expr>`. A guard shares the `if`/`while`
        // condition's no-struct-literal context: a bare identifier directly
        // followed by `{` is the transition's body opener (a field-list or
        // block body), never a struct literal starting the guard expression
        // — `when active { n: state.n + 1 }` is the guard `active` with body
        // `{ n: state.n + 1 }`, not a guard `active { n: ... }` with an
        // elided implicit body.
        let guard = if self.peek() == Some(&Token::When) {
            self.advance();
            Some(self.parse_cond_expr()?)
        } else {
            None
        };

        // Body forms:
        //   on Event: Source => Target;                     ← no body (unit)
        //   on Event: Source => Target { field: expr, ... } ← field list, target elided
        //   on Event: Source => Target { expression }       ← computed body
        let (body, body_form, body_start, body_end) = if self.peek() != Some(&Token::LeftBrace) {
            self.expect_member_terminator("rule");
            // The implicit body is synthesized from the target state, so it is
            // spanned on the target token — not on whatever token follows the
            // `;`. A diagnostic on this expression has to point at the text it
            // asks the author to change.
            let body_expr = if target_is_contextual {
                Expr::ContextVariant(ContextVariantExpr {
                    name: target_state,
                    record: None,
                })
            } else {
                Expr::Ident(target_state)
            };
            (
                body_expr,
                MachineTransitionBodyForm::Implicit,
                target_span.start,
                target_span.end,
            )
        } else if target_state.name != sym::UNDERSCORE && self.is_struct_init_body() {
            let bs = self.peek_span().start;
            self.expect(&Token::LeftBrace)?;
            // The head already named the target, so the braces hold exactly a
            // record literal's field list — `..base` included.
            let (fields, field_labels, base) = self.parse_struct_init_fields()?;
            let be = self.peek_span().start;
            // A contextual target keeps its contextual form when it carries a
            // payload: `=> .Faulted { error }` resolves against the machine's
            // state enum exactly as `=> .Faulted,` does.
            let payload = if target_is_contextual {
                Expr::ContextVariant(ContextVariantExpr {
                    name: target_state,
                    record: Some(Box::new(ContextVariantRecord {
                        fields,
                        field_labels,
                        base,
                    })),
                })
            } else {
                Expr::StructInit {
                    path: Path::single(target_state, target_span.clone()),
                    fields,
                    field_labels,
                    type_args: None,
                    base,
                }
            };
            (payload, MachineTransitionBodyForm::PayloadShorthand, bs, be)
        } else {
            let bs = self.peek_span().start;
            let block = self.parse_block()?;
            let be = self.peek_span().start;
            (Expr::Block(block), MachineTransitionBodyForm::Block, bs, be)
        };
        if body_form != MachineTransitionBodyForm::Implicit {
            self.refuse_mark_after_body();
        }

        Some(MachineTransition {
            event_name,
            source_state,
            target_state,
            target_is_contextual,
            source_path: source.path,
            target_path,
            event_bindings: head_bindings,
            guard,
            body: (body, body_start..body_end),
            body_form,
            reenter,
            span: rule_start..self.last_token_end,
        })
    }

    /// Parse the field list of a single event declaration inside `events { }`:
    /// either `;` (no fields) or `{ name: Type; … }`.
    pub(crate) fn parse_machine_event_fields(&mut self) -> Option<Vec<(Ident, Spanned<TypeExpr>)>> {
        if self.eat(&Token::LeftBrace) {
            let mut fields = Vec::new();
            while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
                let field_name = self.expect_ident()?;
                self.expect(&Token::Colon)?;
                let ty = self.parse_type()?;
                self.expect_member_terminator("field");
                fields.push((field_name, ty));
            }
            self.expect(&Token::RightBrace)?;
            Some(fields)
        } else {
            Some(Vec::new())
        }
    }

    /// Parse a `state Name … ` declaration that begins at the current `state`
    /// token. Keep compound membership and rules for semantic checking.
    pub(crate) fn parse_machine_state_or_composite(
        &mut self,
        states: &mut Vec<MachineState>,
        composite_groups: &mut Vec<CompositeGroup>,
    ) -> Option<Ident> {
        let _guard = self.enter_recursion()?;
        let state_start = self.peek_span().start;
        self.expect(&Token::State)?;
        let state_name = self.expect_ident()?;
        let mut fields: Vec<(Ident, Spanned<TypeExpr>)> = Vec::new();
        let mut entry_block: Option<Block> = None;
        let mut exit_block: Option<Block> = None;

        let bodied = self.eat(&Token::LeftBrace);
        if bodied {
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
                        state_start,
                        state_name,
                        fields,
                        entry_block,
                        exit_block,
                        states,
                        composite_groups,
                    );
                } else {
                    let field_name = self.expect_ident()?;
                    self.expect(&Token::Colon)?;
                    let ty = self.parse_type()?;
                    self.expect_member_terminator("field");
                    fields.push((field_name, ty));
                }
            }
            self.expect(&Token::RightBrace)?;
        }
        if bodied {
            self.refuse_mark_after_body();
        } else {
            self.expect_member_terminator("state");
        }

        states.push(MachineState {
            name: state_name,
            fields,
            entry: entry_block,
            exit: exit_block,
            span: state_start..self.last_token_end,
        });
        Some(state_name)
    }

    /// Parse nested declarations without resolving targets or splicing hooks.
    #[expect(
        clippy::too_many_arguments,
        reason = "the parsed compound prefix and declaration accumulators"
    )]
    pub(crate) fn parse_composite_block(
        &mut self,
        composite_start: usize,
        composite_name: Ident,
        fields: Vec<(Ident, Spanned<TypeExpr>)>,
        entry: Option<Block>,
        exit: Option<Block>,
        states: &mut Vec<MachineState>,
        composite_groups: &mut Vec<CompositeGroup>,
    ) -> Option<Ident> {
        let mut member_names: Vec<Ident> = Vec::new();
        let mut initial: Option<Ident> = None;
        let mut parent_transitions: Vec<MachineTransition> = Vec::new();

        // ── Substate + parent-rule body of the composite block. ──────────────
        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            let is_initial = self.eat_machine_kw("initial");
            if self.peek() == Some(&Token::State) {
                let substate = self.parse_machine_state_or_composite(states, composite_groups)?;
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
                    initial = Some(substate);
                }
                member_names.push(substate);
            } else if self.peek_machine_kw("parallel") {
                let substate = self.parse_machine_parallel(states, composite_groups)?;
                if is_initial && initial.replace(substate).is_some() {
                    self.error_at(
                        format!("composite state `{composite_name}` declares more than one initial substate"),
                        self.peek_span(),
                    );
                }
                member_names.push(substate);
            } else if is_initial {
                self.error_at(
                    "`initial` must be followed by a substate declaration (`initial state Name`)"
                        .to_string(),
                    self.peek_span(),
                );
                break;
            } else if self.peek() == Some(&Token::On) {
                // `_` is scoped to this compound; checking selects its descendants.
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
        self.refuse_mark_after_body();

        let Some(initial_name) = initial else {
            self.error_at(
                format!(
                    "composite state `{composite_name}` must mark exactly one substate \
                     `initial` (`initial state Name`)"
                ),
                self.peek_span(),
            );
            return Some(composite_name);
        };

        composite_groups.push(CompositeGroup {
            name: composite_name,
            members: member_names,
            initial: Some(initial_name),
            regions: Vec::new(),
            entry,
            exit,
            fields,
            parent_transitions,
            span: composite_start..self.last_token_end,
        });

        Some(composite_name)
    }

    /// Parse a parallel compound whose active configuration is represented by
    /// one typed value per region.
    fn parse_machine_parallel(
        &mut self,
        states: &mut Vec<MachineState>,
        composite_groups: &mut Vec<CompositeGroup>,
    ) -> Option<Ident> {
        let start = self.peek_span().start;
        self.eat_machine_kw("parallel");
        self.expect(&Token::State)?;
        let name = self.expect_ident()?;
        self.expect(&Token::LeftBrace)?;
        let mut fields = Vec::new();
        let mut entry = None;
        let mut exit = None;
        let mut regions = Vec::new();
        let mut parent_transitions = Vec::new();
        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            if self.peek() == Some(&Token::Entry) {
                self.advance();
                entry = Some(self.parse_block()?);
            } else if self.peek() == Some(&Token::Exit) {
                self.advance();
                exit = Some(self.parse_block()?);
            } else if self.peek() == Some(&Token::On) {
                parent_transitions.push(self.parse_machine_transition()?);
            } else if self.peek_machine_kw("region") {
                regions.push(self.parse_machine_region(states, composite_groups)?);
            } else {
                let field = self.expect_ident()?;
                self.expect(&Token::Colon)?;
                let ty = self.parse_type()?;
                self.expect_member_terminator("field");
                fields.push((field, ty));
            }
        }
        self.expect(&Token::RightBrace)?;
        self.refuse_mark_after_body();
        if regions.is_empty() {
            self.error_at(
                format!("parallel state `{name}` must declare at least one `region`"),
                start..self.last_token_end,
            );
        }
        composite_groups.push(CompositeGroup {
            name,
            members: Vec::new(),
            initial: None,
            regions,
            entry,
            exit,
            fields,
            parent_transitions,
            span: start..self.last_token_end,
        });
        Some(name)
    }

    /// Parse one region and its direct initial child.
    fn parse_machine_region(
        &mut self,
        states: &mut Vec<MachineState>,
        composite_groups: &mut Vec<CompositeGroup>,
    ) -> Option<MachineRegion> {
        let start = self.peek_span().start;
        self.eat_machine_kw("region");
        let name = self.expect_ident()?;
        self.expect(&Token::LeftBrace)?;
        let mut members = Vec::new();
        let mut initial = None;
        let mut initial_value = None;
        let mut parent_transitions = Vec::new();
        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            let is_initial = self.eat_machine_kw("initial");
            let child = if self.peek() == Some(&Token::State) {
                Some(self.parse_machine_state_or_composite(states, composite_groups)?)
            } else if self.peek_machine_kw("parallel") {
                Some(self.parse_machine_parallel(states, composite_groups)?)
            } else if is_initial {
                let (name, value) = self.parse_machine_initial_value()?;
                if initial.replace(name).is_some() {
                    self.error_at(
                        "a region has exactly one initial destination".to_string(),
                        value.1.clone(),
                    );
                }
                initial_value = Some(value);
                continue;
            } else if self.peek() == Some(&Token::On) && !is_initial {
                parent_transitions.push(self.parse_machine_transition()?);
                continue;
            } else {
                self.error_at(
                    format!(
                        "expected a state declaration or parent-level `on` rule in region `{name}`"
                    ),
                    self.peek_span(),
                );
                break;
            };
            let child = child?;
            if is_initial && initial.replace(child).is_some() {
                self.error_at(
                    format!("region `{name}` declares more than one initial state"),
                    self.peek_span(),
                );
            }
            members.push(child);
        }
        self.expect(&Token::RightBrace)?;
        self.refuse_mark_after_body();
        let Some(initial) = initial else {
            self.error_at(
                format!("region `{name}` must mark exactly one direct child `initial`"),
                start..self.last_token_end,
            );
            return None;
        };
        Some(MachineRegion {
            name,
            members,
            initial,
            initial_value,
            parent_transitions,
            span: start..self.last_token_end,
        })
    }

    fn parse_machine_initial_value(&mut self) -> Option<(Ident, Spanned<Expr>)> {
        let value = self.parse_expr()?;
        let name = match &value.0 {
            Expr::Ident(name) => *name,
            Expr::StructInit { path, .. } => path.segments.last()?.0,
            Expr::ContextVariant(variant) => variant.name,
            _ => {
                self.error_at(
                    "an initial destination must name a state constructor".to_string(),
                    value.1.clone(),
                );
                return None;
            }
        };
        self.expect_member_terminator("initial destination");
        Some((name, value))
    }

    /// Parse a state pattern: `_` (wildcard) or a state name, optionally a
    /// contextual (`.Leaf`, target position only) or qualified
    /// (`Composite.Leaf`) dotted name, retained for checking.
    pub(crate) fn parse_state_pattern(
        &mut self,
        position: StatePatternPosition,
    ) -> Option<StatePattern> {
        let start = self.peek_span().start;
        if matches!(self.peek(), Some(Token::Identifier(name)) if *name == "_") {
            self.advance();
            return Some(StatePattern {
                name: Ident::from_symbol(sym::UNDERSCORE),
                path: Vec::new(),
                is_contextual: false,
                span: start..self.last_token_end,
            });
        }
        if self.peek() == Some(&Token::Dot) {
            // Source states are patterns matched against the current state,
            // not expressions in the machine's enum context: there is no
            // expected type there and no bare-variant lint, so `.Src` has no
            // meaning to preserve. Reject it rather than admit a second
            // spelling nobody is being migrated towards.
            if position == StatePatternPosition::Source {
                let dot = self.peek_span();
                self.error_at_with_hint(
                    "a machine transition source state is a pattern and cannot be written with a leading `.`"
                        .to_string(),
                    dot,
                    "write the state name without the leading `.`; the contextual form is only accepted on the transition target",
                );
                return None;
            }
            self.advance();
            // `_` is a wildcard, not a variant of the state enum, so there is
            // no contextual form of it to accept in the target position
            // either. Reject `._` here so the one spelling of a wildcard
            // target is `_`, matching the source position and the guide.
            if matches!(self.peek(), Some(Token::Identifier(name)) if *name == "_") {
                let span = start..self.peek_span().end;
                self.error_at_with_hint(
                    "a machine transition wildcard target is written `_`, not `._`".to_string(),
                    span,
                    "remove the leading `.`; `_` matches any state and is not a variant of the state enum",
                );
                return None;
            }
            let name = self.expect_ident()?;
            return Some(StatePattern {
                name,
                path: Vec::new(),
                is_contextual: true,
                span: start..self.last_token_end,
            });
        }
        let mut name = self.expect_ident()?;
        let mut path = vec![name];
        while self.peek() == Some(&Token::Dot) {
            self.advance();
            name = self.expect_ident()?;
            path.push(name);
        }
        if path.len() == 1 {
            path.clear();
        }
        Some(StatePattern {
            name,
            path,
            is_contextual: false,
            span: start..self.last_token_end,
        })
    }

    /// Check if the next tokens look like a struct init body: `{ ident: expr }`,
    /// `{ ident, ... }` or `{ ..base }`. Used to detect the
    /// `on Event: S -> T { field: expr }` payload form. A lone `{ ident }` is
    /// the block form: the body reads `ident`, it does not name a field.
    pub(crate) fn is_struct_init_body(&self) -> bool {
        // Peek at `{`, then `ident`, then `:` — if all three, it's struct init
        if self.peek() != Some(&Token::LeftBrace) {
            return false;
        }
        // Look ahead: tokens[pos+1] should be Identifier, tokens[pos+2] should
        // be Colon — or tokens[pos+1] is `..`, the record spread that supplies
        // the fields the list does not name (§3.1).
        let pos = self.pos;
        if pos + 2 >= self.tokens.len() {
            return false;
        }
        matches!(&self.tokens[pos + 1].0, Token::DotDot)
            || matches!(
                (&self.tokens[pos + 1].0, &self.tokens[pos + 2].0),
                (Token::Identifier(_), Token::Colon | Token::Comma)
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
        let type_params = if self.eat(&Token::Less) {
            self.parse_type_params()?
        } else {
            Vec::new()
        };

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
        let mut methods = Vec::new();

        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            let mut doc_comment = self.collect_doc_comments();
            let attributes = self.parse_attributes();
            if doc_comment.is_none() {
                doc_comment = self.collect_doc_comments();
            }
            if self.peek() == Some(&Token::Fn) {
                self.validate_attributes_for(&attributes, AttrPosition::SupervisorMemberFn);
                let start = self.peek_span().start;
                self.advance();
                let mut method =
                    self.parse_function(start, false, Visibility::Private, attributes)?;
                method.doc_comment = doc_comment;
                methods.push(method);
                self.refuse_mark_after_body();
                continue;
            }
            self.validate_attributes_for(&attributes, AttrPosition::Unsupported);
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
                    self.expect_member_terminator("field");
                }
                // `intensity: N within <duration>` — the restart-budget contract.
                // Fuses the legacy `max_restarts:` + `window:` fields. `within`
                // is a contextual keyword (a plain identifier outside this body).
                Some(Token::Identifier(s)) if *s == "intensity" => {
                    self.advance();
                    self.expect(&Token::Colon)?;
                    let restarts = if let Some(Token::Integer(num_str)) = self.peek() {
                        // The declared restart count keeps its `i64` range;
                        // the literal carrier is wider than the field.
                        let n = parse_int_literal(num_str)
                            .ok()
                            .and_then(|(v, _)| i64::try_from(v).ok());
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
                    self.expect_member_terminator("field");
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
                    self.expect_member_terminator("field");
                }
                Some(Token::Child | Token::Identifier("pool")) => {
                    let child_start = self.peek_span().start;
                    let is_pool = matches!(self.peek(), Some(Token::Identifier("pool")));
                    self.advance();
                    let child_name = self.expect_ident()?;
                    self.expect(&Token::Colon)?;
                    // Child type position accepts the module-qualified dotted
                    // form (`child a: bank.Account(...)`), matching spawn and
                    // method-call qualification. The dotted string is the
                    // actor's qualified identity downstream (checker
                    // `type_defs["bank.Account"]`, MIR layout key); a bare
                    // name stays the root/local identity.
                    let actor_type = self.parse_syntactic_path()?;
                    let type_args = if self.eat(&Token::Less) {
                        self.parse_type_args()?
                    } else {
                        Vec::new()
                    };

                    let (mut args, mut arg_labels) = self.parse_construction_keys(
                        child_start,
                        ParseDiagnosticKind::LegacyChildArgs,
                    )?;

                    // Pool arity is a clause, not an init field, so a `count:`
                    // key in the braces is the actor's own field.
                    // On a pool child with no arity clause it is instead the
                    // retired arity spelling; the decision needs the clause
                    // loop's result, so remember the position for now.
                    let paren_count_pos = args
                        .iter()
                        .position(|(name, _)| name.name.as_str() == "count");

                    // A supervisor child accepts restart policy only through the
                    // `restart: <policy>` clause.
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
                    // `with restart:` is not a valid supervisor-child clause;
                    // use `restart:` directly.
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
                    //   stop: <duration>
                    //   wired_to: { param: sibling, bare_sibling }
                    //   count: <expr>            (pool children only)
                    let mut restart: Option<RestartPolicy> = None;
                    let mut count: Option<Spanned<Expr>> = None;
                    let mut stop: Option<Spanned<Expr>> = None;
                    let mut wired_to: Option<std::collections::HashMap<String, String>> = None;
                    let mut seen_wired_to = false;
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
                            Some(Token::Identifier(s)) if *s == "stop" => {
                                let clause_start = self.peek_span().start;
                                self.advance();
                                self.expect(&Token::Colon)?;
                                let value = match self.peek() {
                                    Some(Token::Integer(n)) => {
                                        let n = n.to_string();
                                        self.error_with_hint(
                                            format!(
                                                "supervisor child `stop:` needs a duration, \
                                                 not a bare integer `{n}`"
                                            ),
                                            format!("write `{n}s` (or another unit: {n}ms, {n}m)"),
                                        );
                                        self.advance();
                                        None
                                    }
                                    Some(Token::Comma | Token::Semicolon | Token::RightBrace)
                                    | None => {
                                        self.error_with_hint(
                                            "supervisor child `stop:` requires a duration"
                                                .to_string(),
                                            "write `stop: 30s` or `stop: 0s` to terminate at once",
                                        );
                                        None
                                    }
                                    _ => self.parse_expr(),
                                };
                                if stop.is_some() {
                                    self.error_at(
                                        "duplicate supervisor child `stop:` clause".to_string(),
                                        clause_start..self.last_token_end,
                                    );
                                }
                                stop = value;
                            }
                            // `wired_to: { key: sibling, bare_sibling }` clause.
                            Some(Token::Identifier(s)) if *s == "wired_to" => {
                                if seen_wired_to {
                                    self.error(
                                        "duplicate supervisor child `wired_to:` clause".to_string(),
                                    );
                                }
                                seen_wired_to = true;
                                self.advance(); // consume `wired_to`
                                self.expect(&Token::Colon)?;
                                self.expect(&Token::LeftBrace)?;
                                let mut map = std::collections::HashMap::new();
                                while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
                                    let key = self.expect_ident()?.to_string();
                                    let value = if self.eat(&Token::Colon) {
                                        self.expect_ident()?.to_string()
                                    } else {
                                        key.clone()
                                    };
                                    if map.insert(key.clone(), value).is_some() {
                                        self.error(format!(
                                            "duplicate supervisor wiring key `{key}`"
                                        ));
                                    }
                                    if !self.eat(&Token::Comma) {
                                        break;
                                    }
                                }
                                self.expect(&Token::RightBrace)?;
                                wired_to = if map.is_empty() { None } else { Some(map) };
                            }
                            // `count: <expr>` clause — pool arity. Contextual,
                            // like `stop` and `wired_to`, so `count` stays
                            // an ordinary identifier everywhere else (including
                            // as an actor field name inside the init-arg list).
                            Some(Token::Identifier(s)) if *s == "count" => {
                                self.advance();
                                self.expect(&Token::Colon)?;
                                let count_expr = self.parse_expr()?;
                                if is_pool {
                                    count = Some(count_expr);
                                } else {
                                    // A static child is one actor, so it has no
                                    // arity to declare. Refuse rather than
                                    // accept-and-ignore.
                                    self.error_with_hint(
                                        "`count:` is a pool clause; a `child` declaration \
                                         has no arity"
                                            .to_string(),
                                        format!(
                                            "write `pool {child_name}: {actor_type} \
                                             count: N;` to declare a pool, or drop `count:`"
                                        ),
                                    );
                                }
                            }
                            _ => break,
                        }
                    }

                    // A pool child that carries `count:` in its parentheses and
                    // no arity clause is written in the retired spelling. With
                    // an arity clause present the parenthesised entry is
                    // unambiguously the actor's own `count` field, which is
                    // exactly the collision the clause form removes, so it is
                    // left alone.
                    if is_pool && count.is_none() {
                        if let Some(pos) = paren_count_pos {
                            let (_, count_expr) = args.remove(pos);
                            arg_labels.remove(pos);
                            self.error_at_with_hint(
                                "pool arity is a child clause, not an init field: \
                                 a `count:` key is no longer the arity"
                                    .to_string(),
                                count_expr.1.clone(),
                                format!(
                                    "write `pool {child_name}: {actor_type} {{ .. }} count: N;` \
                                     — the braces hold the actor's own fields"
                                ),
                            );
                            // Carry the expr into the clause slot regardless:
                            // the refusal above already fails the compile, and a
                            // well-formed child keeps the checker from stacking
                            // a missing-arity cascade on top of it.
                            count = Some(count_expr);
                        }
                    }

                    self.expect_member_terminator("field");
                    children.push(ChildSpec {
                        name: child_name,
                        actor_type,
                        type_args,
                        args,
                        arg_labels,
                        restart,
                        wired_to,
                        is_pool,
                        stop,
                        count,
                        span: child_start..self.last_token_end,
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
            type_params,
            params,
            strategy,
            intensity,
            children,
            methods,
        })
    }
}
