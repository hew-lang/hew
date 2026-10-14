#[allow(clippy::wildcard_imports, reason = "shared parser grammar namespace")]
use super::*;
use crate::macro_syntax::{
    Delimiter, ExpansionCategory, MacroCall, MacroDecl, MacroRule, SharedExpander, TokenTree,
};

impl Parser<'_> {
    #[must_use]
    pub fn with_expander(mut self, expander: SharedExpander, source: std::path::PathBuf) -> Self {
        self.expander = Some(expander);
        self.macro_source = source;
        self
    }

    pub fn finish_expanded(mut self) -> crate::ParseResult {
        let program = self.parse_program();
        crate::ParseResult {
            program,
            errors: self.errors,
        }
    }

    pub(crate) fn is_macro_call(&self) -> bool {
        let mut cursor = self.pos;
        loop {
            if !matches!(self.peek_at(cursor), Some(Token::Identifier(_))) {
                return false;
            }
            cursor += 1;
            match self.peek_at(cursor) {
                Some(Token::Bang) => return true,
                Some(Token::Dot) => cursor += 1,
                _ => return false,
            }
        }
    }

    pub(crate) fn parse_macro_call(&mut self) -> Option<MacroCall> {
        let mut segments = vec![self.expect_ident_spanned()?];
        while self.eat(&Token::Dot) {
            segments.push(self.expect_ident_spanned()?);
        }
        self.expect(&Token::Bang)?;
        let input = self.parse_token_tree(Delimiter::Parenthesis)?;
        Some(MacroCall {
            path: Path { segments },
            input,
        })
    }

    pub(crate) fn expand_macro_expression(&mut self, call: &MacroCall) -> Option<Spanned<Expr>> {
        self.with_macro_expansion(call, ExpansionCategory::Expr, |parser| {
            let expression = parser.parse_expr();
            if !parser.at_end() {
                parser.error("an expression macro must produce exactly one expression".into());
            }
            expression
        })
    }

    pub(crate) fn expand_macro_items(&mut self, call: &MacroCall) -> Option<Vec<Spanned<Item>>> {
        self.with_macro_expansion(call, ExpansionCategory::Items, |parser| {
            Some(parser.parse_program().items)
        })
    }

    fn with_macro_expansion<T>(
        &mut self,
        call: &MacroCall,
        category: ExpansionCategory,
        parse: impl FnOnce(&mut Parser<'_>) -> Option<T>,
    ) -> Option<T> {
        let expander = Rc::clone(self.expander.as_ref()?);
        let generated = match expander
            .borrow_mut()
            .begin(call, category, &self.macro_source)
        {
            Ok(generated) => generated,
            Err(message) => {
                self.error_at(
                    message,
                    call.path.segments.first()?.1.start..call.input.span().end,
                );
                return None;
            }
        };
        let mut parser =
            Parser::with_contexts(&generated.source, generated.offset, generated.contexts)
                .with_fragments(generated.fragments)
                .with_expander(Rc::clone(&expander), self.macro_source.clone());
        let result = parse(&mut parser);
        self.errors.extend(parser.errors);
        expander.borrow_mut().complete();
        result
    }

    /// Parse one contextual `macro` declaration.
    pub fn parse_macro_decl(&mut self, visibility: Visibility) -> Option<MacroDecl> {
        self.advance();
        let name = self.expect_ident()?;
        self.expect(&Token::LeftBrace)?;
        let mut rules = Vec::new();
        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            let start = self.peek_span().start;
            let attribute = if self.eat(&Token::HashBracket) {
                let pattern = self.parse_token_tree(Delimiter::Parenthesis)?;
                self.expect(&Token::RightBracket)?;
                self.expect(&Token::Dollar)?;
                let name = self.expect_ident()?;
                self.expect(&Token::Colon)?;
                let method = if self.eat(&Token::Fn) {
                    false
                } else if self.expect_ident()?.name.as_str() == "method" {
                    true
                } else {
                    self.error("expected `fn` or `method` fragment".into());
                    return None;
                };
                let fails = self.eat(&Token::Identifier("fails"));
                Some((
                    pattern,
                    crate::macro_syntax::AttributePattern {
                        name,
                        method,
                        fails,
                    },
                ))
            } else {
                None
            };
            let (pattern, attribute) = if let Some((pattern, attribute)) = attribute {
                (pattern, Some(attribute))
            } else {
                (self.parse_token_tree(Delimiter::Parenthesis)?, None)
            };
            self.expect(&Token::FatArrow)?;
            let category = match self.expect_ident()?.name.as_str() {
                "expr" => ExpansionCategory::Expr,
                "items" => ExpansionCategory::Items,
                _ => {
                    self.error("macro rule category must be `expr` or `items`".into());
                    return None;
                }
            };
            let template = self.parse_token_tree(Delimiter::Brace)?;
            let end = template.span().end;
            rules.push(MacroRule {
                attribute,
                pattern,
                template,
                category,
                span: start..end,
            });
            self.refuse_mark_after_body();
        }
        self.expect(&Token::RightBrace)?;
        if rules.is_empty() {
            self.error("a macro must declare at least one rule".into());
            return None;
        }
        Some(MacroDecl {
            visibility,
            name,
            rules,
        })
    }

    pub(crate) fn parse_token_tree(&mut self, delimiter: Delimiter) -> Option<TokenTree> {
        let _guard = self.enter_recursion()?;
        let (open, close) = match delimiter {
            Delimiter::Parenthesis => (Token::LeftParen, Token::RightParen),
            Delimiter::Bracket => (Token::LeftBracket, Token::RightBracket),
            Delimiter::Brace => (Token::LeftBrace, Token::RightBrace),
            Delimiter::Attribute => (Token::HashBracket, Token::RightBracket),
        };
        let leading = self.token_tree_trivia();
        let start = self.expect(&open)?.start;
        let mut children = Vec::new();
        while !self.at_end() && self.peek() != Some(&close) {
            if let Some(syntax) = self.peek_fragment() {
                let mut tokens = Vec::new();
                while !self.at_end() && self.peek_span().start < syntax.span().end {
                    tokens.push(self.parse_raw_token_tree_atom()?);
                }
                children.push(TokenTree::Fragment {
                    syntax: Box::new(syntax),
                    tokens,
                });
                continue;
            }
            children.push(self.parse_raw_token_tree_atom()?);
        }
        let closing = self.token_tree_trivia();
        let end = self.expect(&close)?.end;
        Some(TokenTree::Group {
            delimiter,
            span: start..end,
            children,
            leading,
            closing,
        })
    }

    pub(crate) fn parse_raw_token_tree_atom(&mut self) -> Option<TokenTree> {
        if let Some(Token::InterpolatedString(raw)) = self.peek() {
            let raw = raw.to_string();
            let leading = self.token_tree_trivia();
            let (_, span) = self.advance()?;
            let mut expressions = Vec::new();
            for range in super::interpolation_spans(&raw, span.start) {
                let text = &raw[range.start - span.start..range.end - span.start];
                let mut parser =
                    Parser::with_contexts(text, range.start, Arc::clone(&self.contexts))
                        .with_fragments(self.fragments.within(range.clone()));
                let mut tokens = Vec::new();
                while !parser.at_end() {
                    if let Some(fragment) = parser.peek_fragment() {
                        let mut captured = Vec::new();
                        while !parser.at_end() && parser.peek_span().start < fragment.span().end {
                            captured.push(parser.parse_raw_token_tree_atom()?);
                        }
                        tokens.push(TokenTree::Fragment {
                            syntax: Box::new(fragment),
                            tokens: captured,
                        });
                    } else {
                        tokens.push(parser.parse_raw_token_tree_atom()?);
                    }
                }
                self.errors.extend(parser.errors);
                expressions.push((range, tokens));
            }
            return Some(TokenTree::Interpolated {
                spelling: raw,
                span,
                leading,
                expressions,
            });
        }
        let nested = match self.peek()? {
            Token::LeftParen => Some(Delimiter::Parenthesis),
            Token::LeftBracket => Some(Delimiter::Bracket),
            Token::LeftBrace => Some(Delimiter::Brace),
            Token::HashBracket => Some(Delimiter::Attribute),
            Token::RightParen | Token::RightBracket | Token::RightBrace => {
                self.error("mismatched delimiter in macro syntax".into());
                return None;
            }
            _ => None,
        };
        if let Some(nested) = nested {
            self.parse_token_tree(nested)
        } else {
            let leading = self.token_tree_trivia();
            let (_, span) = self.advance()?;
            let spelling = self
                .source
                .get(span.start - self.source_offset..span.end - self.source_offset)?
                .to_string();
            let context = self.contexts.at(&span);
            Some(TokenTree::Token {
                spelling,
                span,
                context,
                leading,
            })
        }
    }

    fn token_tree_trivia(&self) -> String {
        self.source
            .get(
                self.last_token_end - self.source_offset
                    ..self.peek_span().start - self.source_offset,
            )
            .unwrap_or_default()
            .to_string()
    }
}

impl Parser<'_> {
    pub fn allow_macro_receivers(&mut self) {
        self.allow_implicit_self_params = true;
    }

    pub fn function_input(&self, function: FnDecl) -> Option<crate::macro_syntax::FunctionInput> {
        let span = function.fn_span.clone();
        let text = self.source.get(
            span.start.checked_sub(self.source_offset)?
                ..span.end.checked_sub(self.source_offset)?,
        )?;
        let mut parser = Parser::with_contexts(text, span.start, Arc::clone(&self.contexts))
            .with_fragments(self.fragments.within(span));
        let mut tokens = Vec::new();
        if function.visibility != Visibility::Private {
            tokens.push(TokenTree::Token {
                spelling: if function.visibility == Visibility::Pub {
                    "pub"
                } else {
                    "package"
                }
                .into(),
                span: function.fn_span.start..function.fn_span.start,
                context: function.name.ctx,
                leading: String::new(),
            });
        }
        if function.is_generator {
            tokens.push(TokenTree::Token {
                spelling: "gen".into(),
                span: function.fn_span.start..function.fn_span.start,
                context: function.name.ctx,
                leading: String::new(),
            });
        }
        while !parser.at_end() {
            tokens.push(parser.parse_raw_token_tree_atom()?);
        }
        // Trait suspension remains on the surrounding trait-method contract.
        if let Some(index) = tokens
            .iter()
            .position(|tree| matches!(tree, TokenTree::Token { spelling, .. } if spelling == "fn"))
        {
            if matches!(
                tokens.get(index + 1),
                Some(TokenTree::Group {
                    delimiter: Delimiter::Bracket,
                    ..
                })
            ) {
                tokens.remove(index + 1);
            }
        }
        Some(crate::macro_syntax::FunctionInput { function, tokens })
    }

    pub(crate) fn expand_member_attributes(&mut self, function: FnDecl) -> Vec<FnDecl> {
        let span = function.fn_span.clone();
        let previous = std::mem::replace(&mut self.allow_implicit_self_params, true);
        let items = self.expand_function_attributes(function);
        self.allow_implicit_self_params = previous;
        items
            .into_iter()
            .filter_map(|(item, _)| {
                if let Item::Function(function) = item {
                    Some(function)
                } else {
                    self.error_at(
                        "E_ATTRIBUTE_MACRO_POSITION: a member attribute must produce functions"
                            .into(),
                        span.clone(),
                    );
                    None
                }
            })
            .collect()
    }

    pub(crate) fn expand_function_attributes(
        &mut self,
        mut function: FnDecl,
    ) -> Vec<Spanned<Item>> {
        if self.expander.is_none() {
            let span = function.fn_span.clone();
            return vec![(Item::Function(function), span)];
        }
        let Some(index) = function
            .attributes
            .iter()
            .rposition(|attr| !super::attributes::is_builtin_attribute(&attr.name))
        else {
            let span = function.fn_span.clone();
            return vec![(Item::Function(function), span)];
        };
        let attr = function.attributes.remove(index);
        let [AttributeArg::Tokens(call)] = attr.args.as_slice() else {
            return Vec::new();
        };
        let held = std::mem::take(&mut function.attributes);
        let name = function.name;
        let doc = function.doc_comment.clone();
        let Some(input) = self.function_input(function) else {
            self.error_at("could not capture attributed function".into(), attr.span);
            return Vec::new();
        };
        let expander = Rc::clone(self.expander.as_ref().unwrap());
        let generated = match expander.borrow_mut().begin_attribute(
            call,
            &input,
            attr.span.clone(),
            &self.macro_source,
        ) {
            Ok(generated) => generated,
            Err(message) => {
                self.error_at(message, attr.span);
                return Vec::new();
            }
        };
        let mut parser =
            Parser::with_contexts(&generated.source, generated.offset, generated.contexts)
                .with_fragments(generated.fragments)
                .with_expander(Rc::clone(&expander), self.macro_source.clone());
        parser.allow_implicit_self_params = self.allow_implicit_self_params;
        let parsed = parser.parse_program_with_attributes(false);
        let mut result = Vec::new();
        let carriers = parsed
            .items
            .iter()
            .filter(|(item, _)| matches!(item, Item::Function(function) if function.name == name))
            .count();
        if carriers != 1 {
            self.error_at("E_MACRO_NAME_CARRIER: an attribute macro must preserve exactly one caller function name".into(), attr.span);
        }
        for (item, span) in parsed.items {
            if let Item::Function(mut function) = item {
                if function.name == name {
                    function.attributes.splice(0..0, held.clone());
                    function.doc_comment.clone_from(&doc);
                    function.intrinsic = function
                        .attributes
                        .iter()
                        .find(|attr| attr.name == "intrinsic")
                        .and_then(|attr| attr.args.first().map(|arg| arg.as_str().to_string()));
                }
                result.extend(parser.expand_function_attributes(function));
            } else {
                result.push((item, span));
            }
        }
        self.errors.extend(parser.errors);
        expander.borrow_mut().complete();
        result
    }
}

impl Parser<'_> {
    pub(crate) fn expand_trait_attributes(&mut self, method: TraitMethod) -> Vec<TraitMethod> {
        if method.body.is_none()
            || self.expander.is_none()
            || method
                .attributes
                .iter()
                .all(|attr| super::attributes::is_builtin_attribute(&attr.name))
        {
            return vec![method];
        }
        let name = method.name;
        let suspends = method.suspends;
        let lang_item = method.lang_item.clone();
        let function = FnDecl {
            origin: crate::ast::DeclarationOrigin::Authored,
            attributes: method.attributes,
            is_generator: false,
            visibility: Visibility::Private,
            name,
            type_params: method.type_params,
            params: method.params,
            return_type: method.return_type,
            where_clause: method.where_clause,
            body: method.body.unwrap(),
            doc_comment: method.doc_comment,
            decl_span: method.span.clone(),
            fn_span: method.span,
            intrinsic: None,
            consumes_self: method.consumes_self,
        };
        let previous = std::mem::replace(&mut self.allow_implicit_self_params, true);
        let methods = self.expand_member_attributes(function);
        self.allow_implicit_self_params = previous;
        methods
            .into_iter()
            .map(|function| TraitMethod {
                attributes: function.attributes,
                name: function.name,
                type_params: function.type_params,
                params: function.params,
                return_type: function.return_type,
                where_clause: function.where_clause,
                body: Some(function.body),
                span: function.fn_span,
                doc_comment: function.doc_comment,
                lang_item: if function.name == name {
                    lang_item.clone()
                } else {
                    None
                },
                consumes_self: function.consumes_self,
                suspends,
            })
            .collect()
    }
}
