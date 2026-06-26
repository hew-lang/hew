//! Type-expression, generic-parameter, trait-bound, where-clause, and
//! function-parameter parsing.

#[allow(
    clippy::wildcard_imports,
    reason = "grammar-area submodules share the parent parser namespace via the split"
)]
use super::*;

impl Parser<'_> {
    // ── Types ──
    #[expect(
        clippy::too_many_lines,
        reason = "recursive descent parser requires sequential case handling"
    )]
    pub(crate) fn parse_type(&mut self) -> Option<Spanned<TypeExpr>> {
        let _guard = self.enter_recursion()?;
        let start = self.peek_span().start;

        let ty = match self.peek() {
            Some(Token::LeftParen) => {
                self.advance();
                if self.eat(&Token::RightParen) {
                    // Unit type represented as empty tuple
                    TypeExpr::Tuple(Vec::new())
                } else {
                    let mut types = vec![self.parse_type()?];
                    while self.eat(&Token::Comma) {
                        if self.peek() == Some(&Token::RightParen) {
                            break;
                        }
                        types.push(self.parse_type()?);
                    }
                    self.expect(&Token::RightParen)?;

                    if types.len() == 1 {
                        // Safe: len == 1 guarantees next() yields one element.
                        return Some(types.into_iter().next().unwrap());
                    }
                    TypeExpr::Tuple(types)
                }
            }
            Some(Token::LeftBracket) => {
                self.advance();
                let element_type = self.parse_type()?;

                if self.eat(&Token::Semicolon) {
                    // Array: [T; N] - but AST expects u64, not expr for size
                    if let Some(Token::Integer(num_str)) = self.peek() {
                        if let Some(size) = parse_int_literal(num_str)
                            .ok()
                            .and_then(|(v, _)| u64::try_from(v).ok())
                        {
                            self.advance();
                            self.expect(&Token::RightBracket)?;
                            TypeExpr::Array {
                                element: Box::new(element_type),
                                size,
                            }
                        } else {
                            self.error("array size must be integer literal".to_string());
                            return None;
                        }
                    } else {
                        self.error("expected array size".to_string());
                        return None;
                    }
                } else {
                    // Slice: [T]
                    self.expect(&Token::RightBracket)?;
                    TypeExpr::Slice(Box::new(element_type))
                }
            }
            Some(Token::Star) => {
                let star_span = self.peek_span();
                self.advance();
                // v0.5 canonical pointer spelling: `*const T` and `*mut T`.
                // Bare `*T` and legacy `*var T` are explicitly rejected so
                // callers cannot silently get one mutability and assume
                // the other.
                let is_mutable = match self.peek() {
                    Some(Token::Mut) => {
                        self.advance();
                        true
                    }
                    Some(Token::Const) => {
                        self.advance();
                        false
                    }
                    Some(Token::Var) => {
                        let span = self.peek_span();
                        self.error_at(
                            "pointer type uses canonical spelling `*mut T` — \
                             `*var T` is no longer accepted"
                                .to_string(),
                            span,
                        );
                        return None;
                    }
                    _ => {
                        self.error_at(
                            "pointer type must specify mutability: write `*const T` \
                             or `*mut T`"
                                .to_string(),
                            star_span,
                        );
                        return None;
                    }
                };
                let pointee = self.parse_type()?;
                TypeExpr::Pointer {
                    is_mutable,
                    pointee: Box::new(pointee),
                }
            }
            Some(Token::Ampersand) => {
                // `&T` — immutable non-owning borrow marker (Q320).
                // Only `&T` is supported in v0.5; `&mut T` / `&var T` are
                // reserved (locked out of scope by Q320) and produce a
                // diagnostic.
                self.advance();
                // Reject `&mut T` and `&var T` with a helpful diagnostic.
                if matches!(self.peek(), Some(Token::Mut | Token::Var)) {
                    let span = self.peek_span();
                    self.error_at(
                        "mutable borrows (`&mut T`) are not supported in v0.5; \
                         use `&T` for an immutable borrow"
                            .to_string(),
                        span,
                    );
                    return None;
                }
                let inner = self.parse_type()?;
                TypeExpr::Borrow(Box::new(inner))
            }
            Some(Token::Dyn) => {
                self.advance();
                // dyn TraitName or dyn (Trait1 + Trait2)
                let bounds = if self.eat(&Token::LeftParen) {
                    // Multi-trait: dyn (Trait1 + Trait2 + ...)
                    let mut bounds = Vec::new();
                    loop {
                        let name = self.expect_ident()?;
                        let (type_args, assoc_type_bindings) = if self.eat(&Token::Less) {
                            self.parse_trait_bound_args()?
                        } else {
                            (None, Vec::new())
                        };
                        bounds.push(TraitBound {
                            name,
                            type_args,
                            assoc_type_bindings,
                        });

                        if !self.eat(&Token::Plus) {
                            break;
                        }
                    }
                    self.expect(&Token::RightParen)?;
                    bounds
                } else {
                    // Single trait: dyn TraitName
                    let name = self.expect_ident()?;
                    let (type_args, assoc_type_bindings) = if self.eat(&Token::Less) {
                        self.parse_trait_bound_args()?
                    } else {
                        (None, Vec::new())
                    };
                    vec![TraitBound {
                        name,
                        type_args,
                        assoc_type_bindings,
                    }]
                };
                TypeExpr::TraitObject(bounds)
            }
            Some(Token::Fn) => {
                self.advance();
                self.expect(&Token::LeftParen)?;

                let mut params = Vec::new();
                while !self.at_end() && self.peek() != Some(&Token::RightParen) {
                    params.push(self.parse_type()?);
                    if !self.eat(&Token::Comma) {
                        break;
                    }
                }
                self.expect(&Token::RightParen)?;

                let return_type = if self.eat(&Token::Arrow) {
                    Box::new(self.parse_type()?)
                } else {
                    // Default to unit type
                    Box::new((TypeExpr::Tuple(Vec::new()), 0..0))
                };

                TypeExpr::Function {
                    params,
                    return_type,
                }
            }
            _ => {
                // Named type: identifier or contextual keyword, with optional qualification
                let mut name = self.expect_ident()?;
                // `_` in type position means infer the type
                if name == "_" {
                    TypeExpr::Infer
                } else {
                    loop {
                        if self.eat(&Token::Dot) {
                            let type_name = self.expect_ident()?;
                            name = format!("{name}.{type_name}");
                            continue;
                        }
                        if self.eat(&Token::DoubleColon) {
                            let type_name = self.expect_ident()?;
                            name = format!("{name}::{type_name}");
                            continue;
                        }
                        break;
                    }
                    let type_args = if self.eat(&Token::Less) {
                        Some(self.parse_type_args()?)
                    } else {
                        None
                    };
                    TypeExpr::Named { name, type_args }
                }
            }
        };

        let end = self.peek_span().start;
        Some((ty, start..end))
    }

    pub(crate) fn parse_type_params(&mut self) -> Option<Vec<TypeParam>> {
        let mut params = Vec::new();

        // Detect and reject empty `<>` immediately — a declaration like
        // `pub type Box<>` has no meaningful semantics; the author almost
        // certainly forgot to name the type parameter.
        if self.at_closing_angle() {
            self.error(
                "empty type parameter list: add at least one type parameter, e.g. `<T>`"
                    .to_string(),
            );
            self.eat_closing_angle();
            return None;
        }

        while !self.at_end() && !self.at_closing_angle() {
            let name = self.expect_ident()?;

            let bounds = if self.eat(&Token::Colon) {
                self.parse_trait_bound_list()?
            } else {
                Vec::new()
            };

            params.push(TypeParam { name, bounds });

            if !self.eat(&Token::Comma) {
                break;
            }
        }

        if !self.eat_closing_angle() {
            self.error("expected '>'".to_string());
            return None;
        }
        Some(params)
    }

    /// Parse `<...>` after `machine Name`, admitting both type parameters
    /// and Phase 0 const-generic parameters (`const N: usize` /
    /// `const N: usize = 16`). The opening `<` has already been consumed.
    ///
    /// Source position convention: all type parameters must appear before
    /// any const parameters in the parameter list (e.g. `<T, U, const N: usize>`).
    /// Mixed orderings such as `<const N: usize, T>` are rejected with a
    /// typed diagnostic to keep the Phase 0 monomorphisation key shape
    /// (`type_args` then `const_args`) trivially derivable from source order.
    pub(crate) fn parse_machine_generic_params(
        &mut self,
    ) -> Option<(Vec<TypeParam>, Vec<ConstParam>)> {
        let mut type_params: Vec<TypeParam> = Vec::new();
        let mut const_params: Vec<ConstParam> = Vec::new();
        let mut seen_const = false;

        // Reject empty `<>` immediately (mirrors `parse_type_params`).
        if self.at_closing_angle() {
            self.error(
                "empty type parameter list: add at least one type or const parameter, \
                 e.g. `<T>` or `<const N: usize>`"
                    .to_string(),
            );
            self.eat_closing_angle();
            return None;
        }

        while !self.at_end() && !self.at_closing_angle() {
            if self.eat(&Token::Const) {
                // Const-generic parameter: `const N: usize` or
                // `const N: usize = 16`.
                let name = self.expect_ident()?;
                self.expect(&Token::Colon)?;
                // R269=A: usize only in Phase 0; reject anything else
                // by name. The element type is parsed as an identifier
                // rather than a full `TypeExpr` so the error message
                // can pinpoint the unsupported width.
                let ty_name = self.expect_ident()?;
                let ty = match ty_name.as_str() {
                    "usize" => ConstParamTy::Usize,
                    other => {
                        self.error(format!(
                            "only `usize` is supported as a const parameter type \
                             in Phase 0; found `{other}`"
                        ));
                        ConstParamTy::Usize
                    }
                };
                let default = if self.eat(&Token::Equal) {
                    if let Some(Token::Integer(n_str)) = self.peek() {
                        let value = parse_int_literal(n_str)
                            .ok()
                            .and_then(|(v, _)| u64::try_from(v).ok());
                        if value.is_none() {
                            self.error(format!(
                                "invalid default value for const parameter `{name}`: \
                                 must be a non-negative integer fitting in usize"
                            ));
                        }
                        self.advance();
                        value
                    } else {
                        self.error(format!(
                            "expected integer literal as default value for \
                             const parameter `{name}`"
                        ));
                        None
                    }
                } else {
                    None
                };
                const_params.push(ConstParam { name, ty, default });
                seen_const = true;
            } else {
                if seen_const {
                    self.error(
                        "type parameters must precede const parameters in \
                         a machine generic-parameter list"
                            .to_string(),
                    );
                }
                let name = self.expect_ident()?;
                let bounds = if self.eat(&Token::Colon) {
                    self.parse_trait_bound_list()?
                } else {
                    Vec::new()
                };
                type_params.push(TypeParam { name, bounds });
            }

            if !self.eat(&Token::Comma) {
                break;
            }
        }

        if !self.eat_closing_angle() {
            self.error("expected '>'".to_string());
            return None;
        }
        Some((type_params, const_params))
    }

    /// Parse optional `<T, U: Trait>` type parameters after a name.
    #[expect(
        clippy::option_option,
        reason = "None vs Some(None) vs Some(Some(v)) distinguishes absent, present-but-empty, and present-with-value"
    )]
    pub(crate) fn parse_opt_type_params(&mut self) -> Option<Option<Vec<TypeParam>>> {
        if self.eat(&Token::Less) {
            Some(Some(self.parse_type_params()?))
        } else {
            Some(None)
        }
    }

    /// Parse optional `-> Type` return type annotation.
    #[expect(
        clippy::option_option,
        reason = "None vs Some(None) vs Some(Some(v)) distinguishes absent, present-but-empty, and present-with-value"
    )]
    pub(crate) fn parse_opt_return_type(&mut self) -> Option<Option<Spanned<TypeExpr>>> {
        if self.eat(&Token::Arrow) {
            Some(Some(self.parse_type()?))
        } else {
            Some(None)
        }
    }

    /// Parse optional `where T: Trait` clause.
    #[expect(
        clippy::option_option,
        reason = "None vs Some(None) vs Some(Some(v)) distinguishes absent, present-but-empty, and present-with-value"
    )]
    pub(crate) fn parse_opt_where_clause(&mut self) -> Option<Option<WhereClause>> {
        if self.peek() == Some(&Token::Where) {
            self.advance();
            Some(Some(self.parse_where_clause()?))
        } else {
            Some(None)
        }
    }

    pub(crate) fn parse_type_args(&mut self) -> Option<Vec<Spanned<TypeExpr>>> {
        let mut args = Vec::new();

        while !self.at_end() && !self.at_closing_angle() {
            args.push(self.parse_type()?);
            if !self.eat(&Token::Comma) {
                break;
            }
        }

        if !self.eat_closing_angle() {
            self.error("expected '>'".to_string());
            return None;
        }
        Some(args)
    }

    pub(crate) fn parse_trait_bound_args(&mut self) -> Option<ParsedTraitBoundArgs> {
        let mut type_args = Vec::new();
        let mut assoc_type_bindings = Vec::new();

        while !self.at_end() && !self.at_closing_angle() {
            if matches!(self.peek(), Some(Token::Identifier(_)))
                && self.peek_at(self.pos + 1) == Some(&Token::Equal)
            {
                let name = self.expect_ident()?;
                self.expect(&Token::Equal)?;
                let ty = self.parse_type()?;
                assoc_type_bindings.push(AssocTypeBinding { name, ty });
            } else {
                type_args.push(self.parse_type()?);
            }

            if !self.eat(&Token::Comma) {
                break;
            }
        }

        if !self.eat_closing_angle() {
            self.error("expected '>'".to_string());
            return None;
        }
        let type_args = if type_args.is_empty() {
            None
        } else {
            Some(type_args)
        };
        Some((type_args, assoc_type_bindings))
    }

    pub(crate) fn parse_trait_bound(&mut self) -> Option<TraitBound> {
        let name = self.expect_ident()?;

        let (type_args, assoc_type_bindings) = if self.eat(&Token::Less) {
            self.parse_trait_bound_args()?
        } else {
            (None, Vec::new())
        };

        Some(TraitBound {
            name,
            type_args,
            assoc_type_bindings,
        })
    }

    #[allow(
        clippy::option_option,
        reason = "outer Option is parser error propagation; inner distinguishes colon-present from absent"
    )]
    pub(crate) fn parse_optional_super_traits(&mut self) -> Option<Option<Vec<TraitBound>>> {
        if self.eat(&Token::Colon) {
            Some(Some(self.parse_trait_bound_list()?))
        } else {
            Some(None)
        }
    }

    pub(crate) fn parse_trait_bound_list(&mut self) -> Option<Vec<TraitBound>> {
        let mut bounds = Vec::new();
        loop {
            bounds.push(self.parse_trait_bound()?);
            if !self.eat(&Token::Plus) {
                break;
            }
        }
        Some(bounds)
    }

    pub(crate) fn parse_where_clause(&mut self) -> Option<WhereClause> {
        let mut predicates = Vec::new();

        loop {
            let ty = self.parse_type()?;
            self.expect(&Token::Colon)?;

            let bounds = self.parse_trait_bound_list()?;

            predicates.push(WherePredicate { ty, bounds });

            if !self.eat(&Token::Comma) {
                break;
            }
            // Allow a trailing comma: stop when the next token can't begin a
            // new predicate (e.g. `{` opens a body, `;` closes an extern decl).
            if matches!(
                self.peek(),
                None | Some(Token::LeftBrace | Token::Semicolon)
            ) {
                break;
            }
        }

        Some(WhereClause { predicates })
    }

    pub(crate) fn parse_params(&mut self) -> Vec<Param> {
        self.parse_params_with_implicit_self(false)
    }

    pub(crate) fn parse_params_with_implicit_self(
        &mut self,
        allow_implicit_self: bool,
    ) -> Vec<Param> {
        let mut params = Vec::new();

        while !self.at_end() && self.peek() != Some(&Token::RightParen) {
            let is_mutable = self.eat(&Token::Var);
            let Some(name) = self.expect_ident() else {
                break;
            };

            if name == "self" {
                let span = self
                    .tokens
                    .get(self.pos.wrapping_sub(1))
                    .map_or(self.peek_span(), |(_, s)| s.clone());
                // `self` and `var self` are both valid implicit receivers.
                // The receiver-mutability axis is the contract carrier for
                // trait methods with mutable receivers (see `Iterator::next`
                // in `std/builtins.hew`); rejecting `var self` here would
                // make the surface unreachable from valid programs.
                if allow_implicit_self && params.is_empty() && self.peek() != Some(&Token::Colon) {
                    params.push(Param {
                        name,
                        ty: (
                            TypeExpr::Named {
                                name: "Self".to_string(),
                                type_args: None,
                            },
                            span,
                        ),
                        is_mutable,
                    });
                    if !self.eat(&Token::Comma) {
                        break;
                    }
                    continue;
                }
                self.errors.push(ParseError {
                    message: "`self` is not a valid parameter name in Hew; \
                              use bare `self` as the first parameter of a trait/impl method, \
                              or use a named receiver with explicit type: \
                              `fn method(val: Self)` in traits or `fn method(p: Point)` in impls"
                        .to_string(),
                    span,
                    hint: None,
                    severity: Severity::Error,
                    kind: ParseDiagnosticKind::Other,
                });
                // Consume the optional `: Type` annotation so recovery is clean
                if self.eat(&Token::Colon) {
                    self.parse_type();
                }
                // Skip any trailing comma to continue parsing further params
                if self.eat(&Token::Comma) {
                    continue;
                }
                break;
            }

            if !self.eat(&Token::Colon) {
                self.error(format!(
                    "expected ':' and type annotation for parameter '{name}'"
                ));
                break;
            }

            if let Some(ty) = self.parse_type() {
                params.push(Param {
                    name,
                    ty,
                    is_mutable,
                });
            }

            if !self.eat(&Token::Comma) {
                break;
            }
        }

        params
    }

    /// Parse a parameter list, optionally accepting a `consuming self` receiver.
    ///
    /// When `allow_consuming_self` is true (type-body method context), the first
    /// token pair `consuming self` is recognised as a consuming-self receiver.
    /// The receiver does not appear in the returned `Vec<Param>`; instead the
    /// boolean return indicates its presence so the caller can record it in
    /// `TypeDecl.consuming_methods`.
    ///
    /// `consuming self` is only valid at the first-parameter position. If it
    /// appears elsewhere (or `allow_consuming_self` is false), it falls through
    /// to the regular error path.
    pub(crate) fn parse_params_with_receiver(
        &mut self,
        allow_consuming_self: bool,
    ) -> (Vec<Param>, bool) {
        let mut has_consuming_self = false;

        // Check for `consuming self` at the first-parameter position.
        if allow_consuming_self && !self.at_end() && self.peek() != Some(&Token::RightParen) {
            // `consuming` lexes as Token::Identifier("consuming").
            let is_consuming_kw =
                matches!(self.peek(), Some(Token::Identifier(s)) if *s == "consuming");
            if is_consuming_kw {
                // Peek one further to check for `self`.
                let next_is_self = matches!(
                    self.tokens.get(self.pos + 1),
                    Some((Token::Identifier(s), _)) if *s == "self"
                );
                if next_is_self {
                    // Consume both tokens.
                    self.advance(); // consuming
                    self.advance(); // self
                    has_consuming_self = true;
                    // Skip optional trailing comma before further params.
                    self.eat(&Token::Comma);
                }
            }
        }

        // Parse remaining ordinary parameters.
        let params = self.parse_params();
        (params, has_consuming_self)
    }

    /// Returns true if the expression is a block-like construct that doesn't need a trailing semicolon.
    pub(crate) fn is_block_expr(expr: &Expr) -> bool {
        matches!(
            expr,
            Expr::Block(_)
                | Expr::If { .. }
                | Expr::IfLet { .. }
                | Expr::Match { .. }
                | Expr::Scope { .. }
                | Expr::ForkBlock { .. }
                | Expr::ScopeDeadline { .. }
                | Expr::UnsafeBlock(_)
                | Expr::Select { .. }
        )
    }

    pub(crate) fn fork_starts_child_binding(&self) -> bool {
        self.peek().is_some_and(Self::is_ident_token)
            && self.peek_at(self.pos + 1) == Some(&Token::Equal)
    }
}
