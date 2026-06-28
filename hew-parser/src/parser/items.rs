//! Top-level item parsing: programs, functions, structs/enums, records,
//! traits, impls, extern blocks, imports, and constants.

#[allow(
    clippy::wildcard_imports,
    reason = "grammar-area submodules share the parent parser namespace via the split"
)]
use super::*;

impl Parser<'_> {
    // ── Program and Items ──
    pub fn parse_program(&mut self) -> Program {
        let mut items = Vec::new();

        // Collect inner doc comments (`//!`) at the start of the file
        let module_doc = self.collect_inner_doc_comments();

        while !self.at_end() {
            // Skip any inner doc comments that appear between items
            while matches!(self.peek(), Some(Token::InnerDocComment(_))) {
                self.advance();
            }
            if self.at_end() {
                break;
            }
            if let Some(item) = self.parse_item() {
                items.push(item);
            } else {
                // Skip past the failed item to the next top-level boundary so
                // a single bad declaration (e.g. a reserved keyword used as a
                // name) does not cascade into one error per remaining token.
                self.skip_to_top_level_item_boundary();
            }
        }

        Program {
            items,
            module_doc,
            module_graph: None,
        }
    }

    /// Parse zero or more `#[name]` or `#[name(arg1, arg2)]` attributes.
    pub(crate) fn parse_attributes(&mut self) -> Vec<Attribute> {
        let mut attrs = Vec::new();
        while self.peek() == Some(&Token::HashBracket) {
            let start = self.peek_span().start;
            self.advance(); // consume `#[`
            let Some(name) = self.expect_ident() else {
                break;
            };
            let mut args = Vec::new();
            if self.eat(&Token::LeftParen) {
                while self.peek() != Some(&Token::RightParen) && !self.at_end() {
                    if self.peek().is_some_and(|tok| Self::is_ident_token(tok)) {
                        // Safe to call: we know the token is identifier-like
                        let key = self.expect_ident().unwrap_or_default();
                        // Check for key = value syntax
                        if self.eat(&Token::Equal) {
                            let value = if self.peek().is_some_and(|tok| Self::is_ident_token(tok))
                            {
                                Some(self.expect_ident().unwrap_or_default())
                            } else if let Some(Token::StringLit(s) | Token::RawString(s)) =
                                self.peek()
                            {
                                let val = unquote_str(s).to_string();
                                self.advance();
                                Some(val)
                            } else if let Some(Token::Integer(n)) = self.peek() {
                                let val = n.to_string();
                                self.advance();
                                Some(val)
                            } else {
                                let span = self.peek_span();
                                let found = match self.peek() {
                                    Some(Token::Comma | Token::RightParen) | None => {
                                        "missing value".to_string()
                                    }
                                    Some(tok) => format!("unexpected {tok}"),
                                };
                                self.error_at_with_hint(
                                    format!("invalid value for attribute `{key}`: {found}"),
                                    span,
                                    "expected identifier, string literal, or integer literal",
                                );
                                if self.peek().is_some_and(|tok| {
                                    !matches!(tok, Token::Comma | Token::RightParen)
                                }) {
                                    self.advance();
                                }
                                None
                            };
                            if let Some(value) = value {
                                args.push(AttributeArg::KeyValue { key, value });
                            }
                        } else {
                            args.push(AttributeArg::Positional(key));
                        }
                    } else if let Some(Token::StringLit(s) | Token::RawString(s)) = self.peek() {
                        let val = unquote_str(s).to_string();
                        self.advance();
                        args.push(AttributeArg::Positional(val));
                    } else if let Some(Token::Duration(s)) = self.peek() {
                        let s = s.to_string();
                        self.advance();
                        if let Some(nanos) = parse_duration_literal(&s) {
                            args.push(AttributeArg::Duration(nanos));
                        } else {
                            self.error(format!("invalid duration literal: {s}"));
                        }
                    } else if let Some(Token::Integer(n)) = self.peek() {
                        // Bare integer positional, e.g. `#[max_heap(1024)]`.
                        let n_str = n.to_string();
                        self.advance();
                        args.push(AttributeArg::Positional(n_str));
                        // Consume an immediately following identifier as a unit suffix
                        // (no comma required), e.g. `#[max_heap(1 kb)]`.  Only ident
                        // tokens are valid unit suffixes; anything else is left for the
                        // outer loop's comma-or-break check.
                        if self.peek().is_some_and(|tok| {
                            Self::is_ident_token(tok)
                                && !matches!(tok, Token::RightParen | Token::Comma)
                        }) {
                            let unit = self.expect_ident().unwrap_or_default();
                            args.push(AttributeArg::Positional(unit));
                        }
                    } else {
                        break;
                    }
                    if !self.eat(&Token::Comma) {
                        break;
                    }
                }
                let _ = self.expect(&Token::RightParen);
            }
            let end = self
                .expect(&Token::RightBracket)
                .map_or_else(|| self.peek_span().start, |span| span.end);
            attrs.push(Attribute {
                name,
                args,
                span: start..end,
            });
        }
        attrs
    }

    /// Parse a visibility modifier.
    ///
    /// Recognises two standalone-keyword forms:
    /// - `pub`     → [`Visibility::Pub`]
    /// - `package` → [`Visibility::Package`]
    ///
    /// Must be called when the current token is `Token::Pub` or `Token::Package`.
    pub(crate) fn parse_visibility(&mut self) -> Visibility {
        if self.eat(&Token::Package) {
            Visibility::Package
        } else {
            assert!(self.eat(&Token::Pub));
            Visibility::Pub
        }
    }

    /// Parse a function declaration with optional `async`/`gen` modifiers.
    /// The current token must be `fn`, `async`, or `gen`.
    #[expect(clippy::ref_option, reason = "avoids cloning option contents")]
    pub(crate) fn parse_fn_with_modifiers(
        &mut self,
        vis: Visibility,
        attrs: Vec<Attribute>,
        doc_comment: &Option<String>,
    ) -> Option<Item> {
        let (fn_start, is_async, is_gen) = match self.peek() {
            Some(Token::Fn) => {
                let fn_start = self.peek_span().start;
                self.advance();
                (fn_start, false, false)
            }
            Some(Token::Async) => {
                self.advance();
                if self.eat(&Token::Gen) {
                    let fn_start = self.peek_span().start;
                    if !self.eat(&Token::Fn) {
                        self.error("expected 'fn' after 'async gen'".to_string());
                        return None;
                    }
                    (fn_start, true, true)
                } else {
                    self.error("expected 'gen fn' after 'async'".to_string());
                    return None;
                }
            }
            Some(Token::Gen) => {
                self.advance();
                let fn_start = self.peek_span().start;
                if !self.eat(&Token::Fn) {
                    self.error("expected 'fn' after 'gen'".to_string());
                    return None;
                }
                (fn_start, false, true)
            }
            _ => unreachable!("parse_fn_with_modifiers called without fn/async/gen"),
        };
        let mut f = self.parse_function(fn_start, is_async, is_gen, vis, attrs)?;
        f.doc_comment.clone_from(doc_comment);
        Some(Item::Function(f))
    }

    #[expect(clippy::too_many_lines, reason = "parser function with many branches")]
    pub(crate) fn parse_item(&mut self) -> Option<Spanned<Item>> {
        // Collect any outer doc comments (`///`) and attributes before this item.
        // Support both orderings: `/// docs #[attr]` and `#[attr] /// docs`.
        let mut doc_comment = self.collect_doc_comments();
        let attrs = self.parse_attributes();
        if doc_comment.is_none() {
            doc_comment = self.collect_doc_comments();
        }
        // `#[extern_symbol("…")]` attaches to a `fn` declaration nested inside
        // either an `extern "C" { … }` block or an `impl Ty { … }` / `impl Trait
        // for Ty { … }` block (parsed via `parse_extern_block` and
        // `parse_impl_decl`'s body loop respectively). At item level it is
        // never valid — reject it here with a clear diagnostic rather than
        // silently dropping it onto whatever item follows.
        for attr in &attrs {
            if attr.name == "extern_symbol" {
                self.error_at(
                    "`#[extern_symbol]` is only valid on `fn` declarations inside an \
                     `extern \"C\"` block or an `impl` block"
                        .to_string(),
                    attr.span.clone(),
                );
            }
        }
        let start = self.peek_span().start;
        // Pre-compute attribute span before attrs is moved into the item.
        let attr_start = attrs.first().map(|a| a.span.start);

        // Extract `#[max_heap]` before dispatch so we can set it on the actor
        // and reject it on any non-actor item.  We pull both the result and the
        // diagnostic span out as owned values so `attrs` can be moved freely
        // into the match arms below.
        let (max_heap_result, max_heap_attr_span): (Option<Result<u64, String>>, Option<Span>) =
            if let Some(a) = attrs.iter().find(|a| a.name == "max_heap") {
                (Some(resolve_max_heap_args(&a.args)), Some(a.span.clone()))
            } else {
                (None, None)
            };

        let item = match self.peek() {
            Some(Token::Import) => {
                self.advance();
                Item::Import(self.parse_import()?)
            }
            Some(Token::Const) => {
                self.advance();
                Item::Const(self.parse_const_decl(Visibility::Private, doc_comment)?)
            }
            Some(Token::Pub | Token::Package) => {
                let vis = self.parse_visibility();
                match self.peek() {
                    Some(Token::Fn | Token::Async | Token::Gen) => {
                        self.parse_fn_with_modifiers(vis, attrs, &doc_comment)?
                    }
                    Some(Token::Struct) if attrs.iter().any(|a| a.name == "wire") => {
                        let mut t = self.parse_wire_struct(&attrs, vis)?;
                        t.doc_comment = doc_comment;
                        Item::TypeDecl(t)
                    }
                    Some(Token::Struct) => {
                        self.error("use 'type' instead of 'struct' to declare types".to_string());
                        return None;
                    }
                    Some(Token::Indirect) => {
                        let mut t = self.parse_indirect_enum(vis)?;
                        t.doc_comment = doc_comment;
                        Item::TypeDecl(t)
                    }
                    Some(Token::Enum) if attrs.iter().any(|a| a.name == "wire") => {
                        let mut t = self.parse_wire_enum(&attrs, vis)?;
                        t.doc_comment = doc_comment;
                        Item::TypeDecl(t)
                    }
                    Some(Token::Enum) => {
                        let mut t = self.parse_struct_or_enum(vis, &attrs)?;
                        t.doc_comment = doc_comment;
                        Item::TypeDecl(t)
                    }
                    Some(Token::Record) => {
                        let mut r = self.parse_record_decl(vis)?;
                        r.doc_comment = doc_comment;
                        Item::Record(r)
                    }
                    Some(Token::Type) => {
                        if self.is_type_alias_lookahead() {
                            Item::TypeAlias(self.parse_type_alias(vis, doc_comment)?)
                        } else {
                            let mut t = self.parse_struct_or_enum(vis, &attrs)?;
                            t.doc_comment = doc_comment;
                            Item::TypeDecl(t)
                        }
                    }
                    Some(Token::Trait) => {
                        self.advance();
                        let mut t = self.parse_trait_decl(vis, &attrs)?;
                        t.doc_comment = doc_comment;
                        Item::Trait(t)
                    }
                    Some(Token::Actor) => {
                        self.advance();
                        let mut a = self.parse_actor_decl(vis)?;
                        a.doc_comment = doc_comment;
                        Item::Actor(a)
                    }
                    Some(Token::Supervisor) => {
                        self.advance();
                        Item::Supervisor(self.parse_supervisor_decl(vis)?)
                    }
                    Some(Token::Machine) => {
                        self.advance();
                        Item::Machine(self.parse_machine_decl(vis)?)
                    }
                    Some(Token::Const) => {
                        self.advance();
                        Item::Const(self.parse_const_decl(vis, doc_comment)?)
                    }
                    _ => {
                        self.error(
                            "invalid item after visibility modifier (expected fn, type, trait, actor, machine, supervisor, record, enum, indirect, or const)".to_string(),
                        );
                        return None;
                    }
                }
            }
            Some(Token::Fn | Token::Async | Token::Gen) => {
                self.parse_fn_with_modifiers(Visibility::Private, attrs, &doc_comment)?
            }
            Some(Token::Struct) if attrs.iter().any(|a| a.name == "wire") => {
                let mut t = self.parse_wire_struct(&attrs, Visibility::Private)?;
                t.doc_comment = doc_comment;
                Item::TypeDecl(t)
            }
            Some(Token::Struct) => {
                self.error("use 'type' instead of 'struct' to declare types".to_string());
                return None;
            }
            Some(Token::Indirect) => {
                let mut t = self.parse_indirect_enum(Visibility::Private)?;
                t.doc_comment = doc_comment;
                Item::TypeDecl(t)
            }
            Some(Token::Enum) if attrs.iter().any(|a| a.name == "wire") => {
                let mut t = self.parse_wire_enum(&attrs, Visibility::Private)?;
                t.doc_comment = doc_comment;
                Item::TypeDecl(t)
            }
            Some(Token::Enum) => {
                let mut t = self.parse_struct_or_enum(Visibility::Private, &attrs)?;
                t.doc_comment = doc_comment;
                Item::TypeDecl(t)
            }
            Some(Token::Record) => {
                let mut r = self.parse_record_decl(Visibility::Private)?;
                r.doc_comment = doc_comment;
                Item::Record(r)
            }
            Some(Token::Type) => {
                if self.is_type_alias_lookahead() {
                    Item::TypeAlias(self.parse_type_alias(Visibility::Private, doc_comment)?)
                } else {
                    let mut t = self.parse_struct_or_enum(Visibility::Private, &attrs)?;
                    t.doc_comment = doc_comment;
                    Item::TypeDecl(t)
                }
            }
            Some(Token::Trait) => {
                self.advance();
                let mut t = self.parse_trait_decl(Visibility::Private, &attrs)?;
                t.doc_comment = doc_comment;
                Item::Trait(t)
            }
            Some(Token::Impl) => {
                self.advance();
                Item::Impl(self.parse_impl_decl()?)
            }
            Some(Token::Actor) => {
                self.advance();
                let mut a = self.parse_actor_decl(Visibility::Private)?;
                a.doc_comment = doc_comment;
                Item::Actor(a)
            }
            Some(Token::Supervisor) => {
                self.advance();
                Item::Supervisor(self.parse_supervisor_decl(Visibility::Private)?)
            }
            Some(Token::Machine) => {
                self.advance();
                Item::Machine(self.parse_machine_decl(Visibility::Private)?)
            }
            Some(Token::Extern) => {
                self.advance();
                Item::ExternBlock(self.parse_extern_block()?)
            }
            Some(Token::Foreign) => {
                self.error_with_hint(
                    "unexpected 'foreign'".to_string(),
                    "use 'extern' instead of 'foreign'",
                );
                return None;
            }
            _ => {
                let found = match self.peek() {
                    Some(tok) => format!("{tok}"),
                    None => "end of file".to_string(),
                };
                // Detect common keywords from other languages
                if let Some(Token::Identifier(id)) = self.peek() {
                    match *id {
                        "struct" => {
                            self.error_with_hint(
                                format!("unexpected '{id}'"),
                                "Hew uses 'type' to declare structs: type Name { ... }",
                            );
                            return None;
                        }
                        "class" | "object" => {
                            self.error_with_hint(
                                format!("unexpected '{id}'"),
                                "Hew uses 'actor' for stateful objects: actor Name { ... }",
                            );
                            return None;
                        }
                        "func" | "function" | "def" | "sub" | "proc" | "method" => {
                            self.error_with_hint(
                                format!("unexpected '{id}'"),
                                "Hew uses 'fn' to declare functions: fn name() { ... }",
                            );
                            return None;
                        }
                        "interface" | "protocol" => {
                            self.error_with_hint(
                                format!("unexpected '{id}'"),
                                "Hew uses 'trait' to declare interfaces: trait Name { ... }",
                            );
                            return None;
                        }
                        "wire" => {
                            self.error_with_hint(
                                "unexpected 'wire'".to_string(),
                                "use the #[wire] attribute instead: `#[wire] struct Name { ... }` or `#[wire] enum Name { ... }`",
                            );
                            return None;
                        }
                        _ => {}
                    }
                }
                self.error(format!(
                    "expected item (fn, actor, machine, type, import, ...), found {found}"
                ));
                return None;
            }
        };

        // Wire `#[max_heap]` into the actor, or emit a diagnostic if it appears
        // on a non-actor item.
        let item = match (item, max_heap_result) {
            (Item::Actor(mut actor), Some(Ok(bytes))) => {
                actor.max_heap_bytes = Some(bytes);
                Item::Actor(actor)
            }
            (Item::Actor(actor), Some(Err(msg))) => {
                let span = max_heap_attr_span.unwrap_or(start..start);
                self.error_at(msg, span);
                Item::Actor(actor)
            }
            (Item::Actor(actor), None) => Item::Actor(actor),
            (other_item, Some(_)) => {
                let span = max_heap_attr_span.unwrap_or(start..start);
                self.error_at(
                    "#[max_heap] is only allowed on actor declarations".to_string(),
                    span,
                );
                other_item
            }
            (other_item, None) => other_item,
        };

        let end = self.peek_span().start;
        // Extend span to cover leading attributes if present.
        let item_start = attr_start.unwrap_or(start);
        Some((item, item_start..end))
    }

    pub(crate) fn parse_function(
        &mut self,
        fn_start: usize,
        is_async: bool,
        is_gen: bool,
        visibility: Visibility,
        attributes: Vec<Attribute>,
    ) -> Option<FnDecl> {
        // Capture the byte position of the name token so the debug pipeline
        // can emit DW_AT_decl_line pointing at the method declaration rather
        // than the enclosing impl-block span.
        let decl_start = self.peek_span().start;
        let name = self.expect_ident()?;
        let decl_end = self.peek_span().start;

        let type_params = self.parse_opt_type_params()?;

        self.expect(&Token::LeftParen)?;
        // Inherent-impl methods (where `self` receivers are allowed) may declare
        // a `consuming self` receiver — the terminal single-consume surface
        // (`fn build(consuming self) -> T`, a `#[linear]` type's consuming
        // method). The receiver is lowered as the by-value `self` parameter; the
        // `consumes_self` fact drives the checker's consume-receiver marking. A
        // `consuming self` outside the first position, or in a context that does
        // not allow `self` receivers (a free function), falls through to the
        // regular param path and is rejected there.
        let consuming_self_span = self.peek_span();
        let consumes_self = self.allow_implicit_self_params && self.eat_consuming_self_receiver();
        let mut params = self.parse_params_with_implicit_self(self.allow_implicit_self_params);
        // A `consuming self` receiver is materialised as a leading by-value
        // `self: Self` parameter so the checker's receiver binding and HIR's
        // method-symbol minting treat it identically to a bare `self` receiver;
        // the move (consume) semantics ride on `consumes_self`, not on the
        // parameter shape.
        if consumes_self {
            params.insert(
                0,
                Param {
                    name: "self".to_string(),
                    ty: (
                        TypeExpr::Named {
                            name: "Self".to_string(),
                            type_args: None,
                        },
                        consuming_self_span,
                    ),
                    is_mutable: false,
                },
            );
        }
        self.expect(&Token::RightParen)?;

        let return_type = self.parse_opt_return_type()?;
        let where_clause = self.parse_opt_where_clause()?;

        // Extract intrinsic key from `#[intrinsic("name")]` if present.
        // An intrinsic declaration may omit the body and use `;` instead.
        let intrinsic = attributes
            .iter()
            .find(|a| a.name == "intrinsic")
            .and_then(|a| a.args.first().map(|arg| arg.as_str().to_string()));

        let (body, fn_end) = if intrinsic.is_some() && self.peek() == Some(&Token::Semicolon) {
            // `#[intrinsic("key")] pub fn name(...) -> T;` — bodyless form.
            // Produce an empty block so the rest of the pipeline sees a well-formed FnDecl.
            let semi_end = self.peek_span().end;
            self.advance(); // consume `;`
            let empty_block = Block {
                stmts: vec![],
                trailing_expr: None,
            };
            (empty_block, semi_end)
        } else {
            let body = self.parse_block()?;
            let fn_end = self.peek_span().start;
            (body, fn_end)
        };

        Some(FnDecl {
            attributes,
            is_async,
            is_generator: is_gen,
            visibility,
            name,
            type_params,
            params,
            return_type,
            where_clause,
            body,
            doc_comment: None,
            decl_span: decl_start..decl_end,
            fn_span: fn_start..fn_end,
            intrinsic,
            consumes_self,
        })
    }

    /// Parse a method inside a type body, returning `(FnDecl, has_consuming_self)`.
    ///
    /// Unlike `parse_function`, this variant accepts `consuming self` as the first
    /// parameter. The boolean return indicates whether the method declared such a
    /// receiver; callers record this in `TypeDecl.consuming_methods`.
    pub(crate) fn parse_type_method(
        &mut self,
        fn_start: usize,
        attributes: Vec<Attribute>,
    ) -> Option<(FnDecl, bool)> {
        let decl_start = self.peek_span().start;
        let name = self.expect_ident()?;
        let decl_end = self.peek_span().start;

        let type_params = self.parse_opt_type_params()?;

        self.expect(&Token::LeftParen)?;
        let (params, has_consuming_self) = self.parse_params_with_receiver(true);
        self.expect(&Token::RightParen)?;

        let return_type = self.parse_opt_return_type()?;
        let where_clause = self.parse_opt_where_clause()?;

        let body = self.parse_block()?;
        let fn_end = self.peek_span().start;

        let decl = FnDecl {
            attributes,
            is_async: false,
            is_generator: false,
            visibility: Visibility::Private,
            name,
            type_params,
            params,
            return_type,
            where_clause,
            body,
            doc_comment: None,
            decl_span: decl_start..decl_end,
            fn_span: fn_start..fn_end,
            intrinsic: None,
            consumes_self: has_consuming_self,
        };
        Some((decl, has_consuming_self))
    }

    pub(crate) fn is_type_alias_lookahead(&self) -> bool {
        // Check for "type Name =" pattern (Name can be a contextual keyword)
        matches!(self.tokens.get(self.pos), Some((Token::Type, _)))
            && self
                .tokens
                .get(self.pos + 1)
                .is_some_and(|(tok, _)| Self::is_ident_token(tok))
            && matches!(self.tokens.get(self.pos + 2), Some((Token::Equal, _)))
    }

    pub(crate) fn parse_type_alias(
        &mut self,
        visibility: Visibility,
        doc_comment: Option<String>,
    ) -> Option<TypeAliasDecl> {
        self.expect(&Token::Type)?;
        let name = self.expect_ident()?;
        self.expect(&Token::Equal)?;
        let ty = self.parse_type()?;
        self.expect(&Token::Semicolon)?;
        Some(TypeAliasDecl {
            visibility,
            name,
            ty,
            doc_comment,
        })
    }

    pub(crate) fn parse_struct_or_enum(
        &mut self,
        visibility: Visibility,
        attrs: &[Attribute],
    ) -> Option<TypeDecl> {
        // Extract ownership-discipline marker from caller-supplied attributes.
        // `#[resource]` and `#[linear]` are consumed here and do not propagate
        // to TypeBodyItem fields or the formatter's attribute list.
        let resource_marker = self.extract_resource_marker(attrs);
        // `#[opaque]` marks a pointer-width opaque runtime handle. Validated
        // post-body (must be an empty-body struct). Representation axis —
        // orthogonal to the `resource_marker` ownership axis.
        let is_opaque = attrs.iter().any(|a| a.name == "opaque");

        let kind = match self.peek() {
            Some(Token::Type) => {
                self.advance();
                TypeDeclKind::Struct
            }
            Some(Token::Enum) => {
                self.advance();
                TypeDeclKind::Enum
            }
            _ => {
                let found = match self.peek() {
                    Some(tok) => format!("{tok}"),
                    None => "end of file".to_string(),
                };
                self.error(format!("expected 'type' or 'enum', found {found}"));
                return None;
            }
        };

        let name = self.expect_ident()?;

        let type_params = self.parse_opt_type_params()?;
        let where_clause = self.parse_opt_where_clause()?;

        self.expect(&Token::LeftBrace)?;

        let mut body = Vec::new();
        let mut consuming_methods = Vec::new();
        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            if let Some((item, has_consuming_self)) = self.parse_type_body_item(kind) {
                if has_consuming_self {
                    // Record the method name so the checker can validate ownership rules.
                    if let TypeBodyItem::Method(ref m) = item {
                        consuming_methods.push(m.name.clone());
                    }
                }
                body.push(item);
            } else {
                let found = match self.peek() {
                    Some(tok) => format!("{tok}"),
                    None => "end of file".to_string(),
                };
                self.error(format!("unexpected {found} in type body"));
                self.advance();
            }
        }

        self.expect(&Token::RightBrace)?;

        if is_opaque && (kind != TypeDeclKind::Struct || !body.is_empty()) {
            self.error(
                "#[opaque] type must be an empty-body struct — an opaque handle \
                 has no fields and is produced only via FFI [E_OPAQUE_TYPE_SHAPE]"
                    .to_string(),
            );
        }

        Some(TypeDecl {
            visibility,
            kind,
            name,
            type_params,
            where_clause,
            body,
            doc_comment: None,
            wire: None,
            is_indirect: false,
            resource_marker,
            is_opaque,
            consuming_methods,
        })
    }

    /// Parse a `record` declaration in either named-field or tuple-positional form.
    ///
    /// Named form:  `record Name<T>? where...? { field: Type, ... }`
    /// Tuple form:  `record Name<T>? (Type, ...) ;`
    ///
    /// The `record` keyword must already be consumed before this is called.
    /// Both forms reject empty field lists.
    pub(crate) fn parse_record_decl(&mut self, visibility: Visibility) -> Option<RecordDecl> {
        let start = self.peek_span().start;

        // Consume `record`
        self.advance();

        let name = self.expect_ident()?;
        let type_params = self.parse_opt_type_params()?;
        let where_clause = self.parse_opt_where_clause()?;

        if self.eat(&Token::LeftParen) {
            // Tuple-positional form: `record Name(T1, T2, ...) ;`
            let mut field_types: Vec<Spanned<TypeExpr>> = Vec::new();

            while !self.at_end() && self.peek() != Some(&Token::RightParen) {
                let ty = self.parse_type()?;
                field_types.push(ty);

                if self.peek() == Some(&Token::Comma) {
                    self.advance();
                } else {
                    break;
                }
            }

            if field_types.is_empty() {
                self.error("tuple record must have at least one positional field".to_string());
                return None;
            }

            let end = self.peek_span().start;
            self.expect(&Token::RightParen)?;
            self.expect(&Token::Semicolon)?;

            Some(RecordDecl {
                visibility,
                name,
                type_params,
                where_clause,
                kind: RecordKind::Tuple(field_types),
                doc_comment: None,
                span: start..end,
            })
        } else {
            // Named-field form: `record Name { field: Type, ... }`
            self.expect(&Token::LeftBrace)?;

            let mut fields: Vec<RecordField> = Vec::new();
            while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
                let field_start = self.peek_span().start;

                // Field name
                let field_name = if let Some(Token::Identifier(_)) = self.peek() {
                    self.expect_ident()?
                } else {
                    let found = match self.peek() {
                        Some(tok) => format!("{tok}"),
                        None => "end of file".to_string(),
                    };
                    self.error(format!("expected field name, found {found}"));
                    return None;
                };

                self.expect(&Token::Colon)?;

                let ty = self.parse_type()?;
                let field_end = self.peek_span().start;

                fields.push(RecordField {
                    name: field_name,
                    ty,
                    doc_comment: None,
                    span: field_start..field_end,
                });

                // Comma or end of body. Semicolons are common when users
                // switch from `type` fields; keep them invalid but recover
                // with a targeted hint instead of cascading item-level errors.
                if self.peek() == Some(&Token::Comma) {
                    self.advance();
                } else if self.peek() == Some(&Token::Semicolon) {
                    let semi_span = self.peek_span();
                    self.error_at_with_hint(
                        "expected `,` or `}` after record field, found `;`".to_string(),
                        semi_span,
                        "record fields use commas; write `field: Type,` instead of `field: Type;`",
                    );
                    self.advance();
                } else {
                    break;
                }
            }

            if fields.is_empty() {
                self.error("record body must contain at least one field".to_string());
                return None;
            }

            let end = self.peek_span().start;
            self.expect(&Token::RightBrace)?;

            Some(RecordDecl {
                visibility,
                name,
                type_params,
                where_clause,
                kind: RecordKind::Named(fields),
                doc_comment: None,
                span: start..end,
            })
        }
    }

    /// Extract `ResourceMarker` from a pre-parsed attribute slice.
    ///
    /// `#[resource]` → `ResourceMarker::Resource`
    /// `#[linear]`   → `ResourceMarker::Linear`
    ///
    /// Emits a diagnostic if both `#[resource]` and `#[linear]` appear on the
    /// same type (they declare incompatible ownership disciplines).  Emits a
    /// diagnostic for any attribute that is not a recognised type-decl attribute;
    /// unrecognised names could be ownership-marker typos and are rejected
    /// fail-closed rather than silently ignored.
    ///
    /// The attribute is not consumed from the slice; callers that render
    /// attributes should filter `resource` / `linear` out themselves if they
    /// want to suppress them in output.  For now the checker is the only
    /// consumer, so we leave the slice untouched.
    ///
    /// Valid type-decl attributes: `resource`, `linear`, `wire`, `json`,
    /// `yaml`, `deprecated`, `opaque`.  Anything else triggers `E_UNKNOWN_TYPE_MARKER`.
    pub(crate) fn extract_resource_marker(&mut self, attrs: &[Attribute]) -> ResourceMarker {
        // Known-valid attributes for type declarations.  These are consumed by
        // other parser paths (wire metadata, deprecation, naming-case, opaque
        // representation); only `resource` and `linear` belong to the
        // ownership-discipline surface this helper returns.
        const KNOWN_TYPE_ATTRS: &[&str] = &[
            "resource",
            "linear",
            "wire",
            "json",
            "yaml",
            "deprecated",
            "opaque",
        ];

        let mut resource_span: Option<std::ops::Range<usize>> = None;
        let mut linear_span: Option<std::ops::Range<usize>> = None;
        let mut marker = ResourceMarker::None;

        for attr in attrs {
            match attr.name.as_str() {
                "resource" => {
                    if let Some(ref prev) = linear_span {
                        // `#[linear]` already seen — conflict.
                        self.error_at(
                            "cannot combine #[resource] and #[linear] on the same type \
                             — they declare incompatible ownership disciplines \
                             [E_TYPE_MARKER_CONFLICT]"
                                .to_string(),
                            attr.span.clone(),
                        );
                        let _ = prev; // span used for context above
                    }
                    resource_span = Some(attr.span.clone());
                    marker = ResourceMarker::Resource;
                }
                "linear" => {
                    if let Some(ref prev) = resource_span {
                        // `#[resource]` already seen — conflict.
                        self.error_at(
                            "cannot combine #[resource] and #[linear] on the same type \
                             — they declare incompatible ownership disciplines \
                             [E_TYPE_MARKER_CONFLICT]"
                                .to_string(),
                            attr.span.clone(),
                        );
                        let _ = prev;
                    }
                    linear_span = Some(attr.span.clone());
                    marker = ResourceMarker::Linear;
                }
                name if !KNOWN_TYPE_ATTRS.contains(&name) => {
                    self.error_at(
                        format!(
                            "unrecognised type attribute '#[{name}]' \
                             [E_UNKNOWN_TYPE_MARKER]"
                        ),
                        attr.span.clone(),
                    );
                }
                _ => {}
            }
        }
        marker
    }

    pub(crate) fn parse_indirect_enum(&mut self, visibility: Visibility) -> Option<TypeDecl> {
        self.expect(&Token::Indirect)?;
        if self.peek() != Some(&Token::Enum) {
            self.error("'indirect' can only be used with 'enum'".to_string());
            return None;
        }
        // Indirect enums (recursive boxed enums) do not participate in the
        // `#[resource]` / `#[linear]` ownership-discipline surface; pass an
        // empty attribute slice so no marker is extracted.
        let mut decl = self.parse_struct_or_enum(visibility, &[])?;
        decl.is_indirect = true;
        Some(decl)
    }

    /// Parse one item in a type body, returning `(item, has_consuming_self)`.
    ///
    /// `has_consuming_self` is `true` only when the item is a method whose
    /// first parameter is a `consuming self` receiver.  The caller records
    /// consuming method names in `TypeDecl.consuming_methods`.
    pub(crate) fn parse_type_body_item(
        &mut self,
        kind: TypeDeclKind,
    ) -> Option<(TypeBodyItem, bool)> {
        // Collect any doc comments before attributes or the field/variant/method
        // itself. Support both `/// docs #[attr]` and `#[attr] /// docs`.
        let mut doc_comment = self.collect_doc_comments();
        // Capture the start of this item after doc-comment trivia is consumed.
        // The formatter uses this position to flush inline `//` comments that
        // appear in the source before this item without being doc comments.
        let item_start = self.peek_span().start;
        match kind {
            TypeDeclKind::Struct => {
                let attributes = self.parse_attributes();
                if doc_comment.is_none() {
                    doc_comment = self.collect_doc_comments();
                }

                // `#[extern_symbol]` belongs on `extern "C"` fns and `impl`
                // methods — not on methods declared inline in a type body.
                for attr in &attributes {
                    if attr.name == "extern_symbol" {
                        self.error_at(
                            "`#[extern_symbol]` is only valid on `fn` declarations inside an \
                             `extern \"C\"` block or an `impl` block; declare the method in an \
                             `impl` block instead"
                                .to_string(),
                            attr.span.clone(),
                        );
                    }
                }

                if self.peek() == Some(&Token::Fn) {
                    let fn_start = self.peek_span().start;
                    self.advance();
                    // Use parse_type_method so `consuming self` receivers are accepted.
                    let (mut method, has_consuming_self) =
                        self.parse_type_method(fn_start, attributes)?;
                    method.doc_comment = doc_comment;
                    Some((TypeBodyItem::Method(method), has_consuming_self))
                } else {
                    // Field with optional attributes (e.g. #[encode(rename = "x")])
                    let name = self.expect_ident()?;
                    self.expect(&Token::Colon)?;
                    let ty = self.parse_type()?;
                    if !self.eat(&Token::Semicolon) {
                        self.eat(&Token::Comma);
                    }
                    // peek_span().start is now the first token after the `;` or `,`,
                    // which captures any trailing comment on this field's line in the
                    // range item_start..item_end (comments are skipped by the lexer,
                    // but extract_comments scans the raw source for them).
                    let item_end = self.peek_span().start;
                    Some((
                        TypeBodyItem::Field {
                            name,
                            ty,
                            attributes,
                            doc_comment,
                            span: item_start..item_end,
                        },
                        false,
                    ))
                }
            }
            TypeDeclKind::Enum => {
                // Enum variant
                let name = self.expect_ident()?;
                let kind = if self.eat(&Token::LeftParen) {
                    let mut fields = Vec::new();
                    while !self.at_end() && self.peek() != Some(&Token::RightParen) {
                        fields.push(self.parse_type()?);
                        if !self.eat(&Token::Comma) {
                            break;
                        }
                    }
                    self.expect(&Token::RightParen)?;
                    VariantKind::Tuple(fields)
                } else if self.eat(&Token::LeftBrace) {
                    let mut fields = Vec::new();
                    while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
                        let field_name = self.expect_ident()?;
                        self.expect(&Token::Colon)?;
                        let ty = self.parse_type()?;
                        fields.push((field_name, ty));
                        if !(self.eat(&Token::Comma) || self.eat(&Token::Semicolon)) {
                            break;
                        }
                    }
                    self.expect(&Token::RightBrace)?;
                    VariantKind::Struct(fields)
                } else {
                    VariantKind::Unit
                };

                if !self.eat(&Token::Semicolon) && self.peek() == Some(&Token::Comma) {
                    self.error("use `;` instead of `,` to separate variants".to_string());
                    self.advance();
                }
                // peek_span() is now the position after the trailing `;`
                let item_end = self.peek_span().start;
                Some((
                    TypeBodyItem::Variant(VariantDecl {
                        name,
                        kind,
                        doc_comment,
                        span: item_start..item_end,
                    }),
                    false,
                ))
            }
        }
    }

    pub(crate) fn parse_trait_decl(
        &mut self,
        visibility: Visibility,
        attrs: &[Attribute],
    ) -> Option<TraitDecl> {
        let name = self.expect_ident()?;

        let type_params = self.parse_opt_type_params()?;

        let super_traits = self.parse_optional_super_traits()?;

        self.expect(&Token::LeftBrace)?;

        let mut items = Vec::new();
        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            if let Some(item) = self.parse_trait_item() {
                items.push(item);
            } else {
                self.error(format!(
                    "expected trait item (fn or type), found {:?}",
                    self.peek()
                ));
                self.advance(); // error recovery
            }
        }

        self.expect(&Token::RightBrace)?;

        let lang_item = attrs
            .iter()
            .find(|a| a.name == "lang_item")
            .and_then(|a| a.args.first().map(|arg| arg.as_str().to_string()));

        Some(TraitDecl {
            visibility,
            name,
            type_params,
            super_traits,
            items,
            doc_comment: None,
            lang_item,
        })
    }

    pub(crate) fn parse_trait_item(&mut self) -> Option<TraitItem> {
        let doc_comment = self.collect_doc_comments();
        let attrs = self.parse_attributes();

        // `#[extern_symbol]` belongs on `extern "C"` fns and `impl`
        // methods — not on trait-item declarations. Trait items describe
        // an abstract surface; the C-ABI binding lives on the concrete
        // `impl` method.
        for attr in &attrs {
            if attr.name == "extern_symbol" {
                self.error_at(
                    "`#[extern_symbol]` is only valid on `fn` declarations inside an \
                     `extern \"C\"` block or an `impl` block; bind the symbol on the \
                     concrete `impl` method instead"
                        .to_string(),
                    attr.span.clone(),
                );
            }
        }

        match self.peek() {
            Some(Token::Fn) => {
                let fn_start = self.peek_span().start;
                self.advance();
                let name = self.expect_ident()?;
                let type_params = self.parse_opt_type_params()?;

                self.expect(&Token::LeftParen)?;
                let params = self.parse_params_with_implicit_self(true);
                self.expect(&Token::RightParen)?;

                let return_type = self.parse_opt_return_type()?;
                let where_clause = self.parse_opt_where_clause()?;

                let body = if self.peek() == Some(&Token::LeftBrace) {
                    Some(self.parse_block()?)
                } else {
                    self.expect(&Token::Semicolon)?;
                    None
                };
                let fn_end = self.peek_span().start;

                let lang_item = attrs
                    .iter()
                    .find(|a| a.name == "lang_item")
                    .and_then(|a| a.args.first().map(|arg| arg.as_str().to_string()));

                Some(TraitItem::Method(TraitMethod {
                    name,
                    type_params,
                    params,
                    return_type,
                    where_clause,
                    body,
                    span: fn_start..fn_end,
                    doc_comment,
                    lang_item,
                }))
            }
            Some(Token::Type) => {
                let type_start = self.peek_span().start;
                self.advance();
                let name = self.expect_ident()?;

                let bounds = if self.eat(&Token::Colon) {
                    self.parse_trait_bound_list()?
                } else {
                    Vec::new()
                };

                let default = if self.eat(&Token::Equal) {
                    Some(self.parse_type()?)
                } else {
                    None
                };

                let semi_span = self.expect(&Token::Semicolon)?;
                Some(TraitItem::AssociatedType {
                    name,
                    bounds,
                    default,
                    span: type_start..semi_span.end,
                })
            }
            _ => {
                let found = match self.peek() {
                    Some(tok) => format!("{tok}"),
                    None => "end of file".to_string(),
                };
                self.error_with_hint(
                    format!("expected trait item, found {found}"),
                    "trait items must be 'fn' signatures or 'type' declarations",
                );
                None
            }
        }
    }

    pub(crate) fn parse_impl_decl(&mut self) -> Option<ImplDecl> {
        let type_params = self.parse_opt_type_params()?;

        // Try to parse trait bound first
        let saved_pos = self.save_pos();
        let trait_bound = if let Some(bound) = self.parse_trait_bound() {
            if self.eat(&Token::For) {
                Some(bound)
            } else {
                self.restore_pos(saved_pos);
                None
            }
        } else {
            self.restore_pos(saved_pos);
            None
        };

        let target_type = self.parse_type()?;
        let where_clause = self.parse_opt_where_clause()?;

        self.expect(&Token::LeftBrace)?;

        let mut methods = Vec::new();
        let mut type_aliases = Vec::new();
        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            let doc_comment = self.collect_doc_comments();
            let method_attrs = self.parse_attributes();
            let vis = if matches!(self.peek(), Some(Token::Pub | Token::Package)) {
                self.parse_visibility()
            } else {
                Visibility::Private
            };
            match self.peek() {
                Some(Token::Type) => {
                    if !method_attrs.is_empty() {
                        let span = method_attrs.first().map_or(0..0, |a| a.span.clone());
                        self.error_at(
                            "attributes are not supported on impl-block type aliases".to_string(),
                            span,
                        );
                    }
                    if vis != Visibility::Private {
                        self.error(
                            "type aliases in impl bodies cannot have visibility modifiers"
                                .to_string(),
                        );
                    }
                    self.advance();
                    let name = self.expect_ident()?;
                    self.expect(&Token::Equal)?;
                    let ty = self.parse_type()?;
                    self.expect(&Token::Semicolon)?;
                    type_aliases.push(ImplTypeAlias { name, ty });
                }
                Some(Token::Fn) => {
                    let fn_start = self.peek_span().start;
                    self.advance();
                    let prev_allow_implicit_self =
                        std::mem::replace(&mut self.allow_implicit_self_params, true);
                    let parsed_method =
                        self.parse_function(fn_start, false, false, vis, method_attrs);
                    self.allow_implicit_self_params = prev_allow_implicit_self;
                    if let Some(mut method) = parsed_method {
                        if let Some(doc) = doc_comment {
                            method.doc_comment = Some(doc);
                        }
                        methods.push(method);
                    }
                }
                other => {
                    let other_msg =
                        format!("expected 'fn' or 'type' in impl body, found {other:?}");
                    if !method_attrs.is_empty() {
                        let span = method_attrs.first().map_or(0..0, |a| a.span.clone());
                        self.error_at(
                            "attributes inside an impl block must be followed by a `fn` \
                             declaration"
                                .to_string(),
                            span,
                        );
                    }
                    self.error(other_msg);
                    self.advance(); // error recovery: skip the bad token
                }
            }
        }

        self.expect(&Token::RightBrace)?;

        Some(ImplDecl {
            type_params,
            trait_bound,
            target_type,
            where_clause,
            type_aliases,
            methods,
        })
    }

    /// Checks if the current position looks like a field declaration (ident: type).
    pub(crate) fn peek_is_field_decl(&mut self) -> bool {
        let saved = self.save_pos();
        let result = if self.expect_ident().is_some() {
            self.peek() == Some(&Token::Colon)
        } else {
            false
        };
        self.restore_pos(saved);
        result
    }

    pub(crate) fn parse_extern_block(&mut self) -> Option<ExternBlock> {
        let abi = if let Some(Token::StringLit(s) | Token::RawString(s)) = self.peek() {
            let abi = unquote_str(s).to_string();
            self.advance();
            abi
        } else {
            "C".to_string()
        };

        self.expect(&Token::LeftBrace)?;

        let mut functions = Vec::new();
        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            // Collect any per-fn attributes (e.g. `#[extern_symbol("hew_x")]`).
            // Doc comments are not supported inside extern blocks today; only
            // attributes between the opening `{` and the next `fn` are parsed.
            let attributes = self.parse_attributes();
            if self.peek() == Some(&Token::Fn) {
                let item_start = attributes
                    .first()
                    .map_or_else(|| self.peek_span().start, |a| a.span.start);
                self.advance();
                let name = self.expect_ident()?;

                self.expect(&Token::LeftParen)?;
                let params = self.parse_params();

                let is_variadic = self.eat(&Token::DotDot);
                self.expect(&Token::RightParen)?;

                let return_type = self.parse_opt_return_type()?;

                self.expect(&Token::Semicolon)?;

                // peek_span().start is now the first token after the `;`,
                // so the byte range item_start..item_end covers the full
                // extern fn declaration including any trailing comment
                // on the same line (comments are not lex tokens, but
                // extract_comments scans the raw source for them).
                let item_end = self.peek_span().start;

                functions.push(ExternFnDecl {
                    attributes,
                    name,
                    params,
                    return_type,
                    is_variadic,
                    span: item_start..item_end,
                });
            } else {
                if !attributes.is_empty() {
                    let span = attributes.first().map_or(0..0, |a| a.span.clone());
                    self.error_at(
                        "attributes inside an extern block must be followed by a `fn` \
                         declaration"
                            .to_string(),
                        span,
                    );
                }
                self.error(format!(
                    "expected 'fn' in extern block, found {:?}",
                    self.peek()
                ));
                self.advance(); // error recovery
            }
        }

        self.expect(&Token::RightBrace)?;

        Some(ExternBlock { abi, functions })
    }

    pub(crate) fn parse_import(&mut self) -> Option<ImportDecl> {
        // File-path import: import "path/to/file.hew";
        if let Some(Token::StringLit(s) | Token::RawString(s)) = self.peek() {
            let raw = *s;
            self.advance();
            self.expect(&Token::Semicolon)?;
            let file_path = unquote_str(raw).to_owned();
            return Some(ImportDecl {
                path: Vec::new(),
                spec: None,
                module_alias: None,
                file_path: Some(file_path),
                resolved_items: None,
                resolved_item_source_paths: Vec::new(),
                resolved_source_paths: Vec::new(),
            });
        }

        let mut path = Vec::new();

        loop {
            path.push(self.expect_import_path_segment()?);
            // Only continue path if :: is followed by an identifier
            if self.peek() == Some(&Token::DoubleColon) {
                let saved = self.save_pos();
                self.advance(); // consume ::
                if !self.peek().is_some_and(Self::is_import_path_segment_token) {
                    // :: followed by *, {, etc. — restore and let spec parsing handle it
                    self.restore_pos(saved);
                    break;
                }
            } else {
                break;
            }
        }

        let spec = if self.eat(&Token::DoubleColon) {
            if self.eat(&Token::Star) {
                Some(ImportSpec::Glob)
            } else if self.eat(&Token::LeftBrace) {
                let mut names = Vec::new();
                while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
                    let name = self.expect_ident()?;
                    let alias = if self.eat(&Token::As) {
                        Some(self.expect_ident()?)
                    } else {
                        None
                    };
                    names.push(ImportName { name, alias });
                    if !self.eat(&Token::Comma) {
                        break;
                    }
                }
                self.expect(&Token::RightBrace)?;
                Some(ImportSpec::Names(names))
            } else {
                let found = self
                    .peek()
                    .map_or_else(|| "end of input".to_string(), |t| format!("{t}"));
                self.error(format!("expected `*` or `{{` after `::`, found {found}"));
                return None;
            }
        } else {
            None
        };

        // Whole-module alias: `import path::to::mod as alias;`. Legal only for
        // the whole-module form; `import m::{ A } as f` and `import m::* as f`
        // have no meaning and are rejected, so `as` is only consumed when no
        // brace/glob spec was parsed.
        let module_alias = if self.eat(&Token::As) {
            if spec.is_some() {
                self.error(
                    "`as` cannot alias a `::{ }` or `::*` import; alias the whole module \
                     instead (`import path as alias;`)"
                        .to_string(),
                );
                return None;
            }
            Some(self.expect_ident()?)
        } else {
            None
        };

        self.expect(&Token::Semicolon)?;

        Some(ImportDecl {
            path,
            spec,
            module_alias,
            file_path: None,
            resolved_items: None,
            resolved_item_source_paths: Vec::new(),
            resolved_source_paths: Vec::new(),
        })
    }

    pub(crate) fn parse_const_decl(
        &mut self,
        visibility: Visibility,
        doc_comment: Option<String>,
    ) -> Option<ConstDecl> {
        let name = self.expect_ident()?;
        self.expect(&Token::Colon)?;
        let ty = self.parse_type()?;
        self.expect(&Token::Equal)?;
        let value = self.parse_expr()?;
        self.expect(&Token::Semicolon)?;

        Some(ConstDecl {
            visibility,
            name,
            ty,
            value,
            doc_comment,
        })
    }
}
