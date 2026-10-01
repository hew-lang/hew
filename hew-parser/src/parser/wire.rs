//! Wire (`@wire`) type and enum parsing, plus the wire-field modifier helpers.

#[allow(
    clippy::wildcard_imports,
    reason = "grammar-area submodules share the parent parser namespace via the split"
)]
use super::*;

#[derive(Debug, Default)]
pub(crate) struct WireFieldModifiers {
    is_optional: bool,
    is_deprecated: bool,
    is_repeated: bool,
    /// The text key a retired `json(..)`/`yaml(..)` modifier named.
    legacy_key: Option<String>,
    since: Option<u32>,
}

#[derive(Debug)]
pub(crate) struct ParsedWireField {
    explicit_number: Option<u32>,
    modifiers: WireFieldModifiers,
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum WireFieldParseMode {
    Struct,
}

impl Parser<'_> {
    pub(crate) fn parse_wire_since_modifier(&mut self) -> Option<u32> {
        if let Some(Token::Integer(n_str)) = self.peek() {
            let version = parse_int_literal(n_str)
                .ok()
                .and_then(|(v, _)| u32::try_from(v).ok());
            if version.is_none() {
                self.error("invalid version number after 'since'".to_string());
            }
            self.advance();
            version
        } else {
            self.error("expected version number after 'since'".to_string());
            None
        }
    }

    pub(crate) fn parse_wire_field_modifiers(&mut self) -> WireFieldModifiers {
        let mut modifiers = WireFieldModifiers::default();

        loop {
            match self.peek() {
                Some(Token::Optional) => {
                    self.advance();
                    modifiers.is_optional = true;
                }
                Some(Token::Deprecated) => {
                    self.advance();
                    modifiers.is_deprecated = true;
                }
                Some(tok) if Self::is_ident_token(tok) => {
                    let saved = self.save_pos();
                    let ident = self.expect_word();
                    match ident.as_str() {
                        "repeated" => {
                            modifiers.is_repeated = true;
                        }
                        "since" => {
                            modifiers.since = self.parse_wire_since_modifier();
                        }
                        legacy @ ("json" | "yaml" | "json_name" | "yaml_name") => {
                            let start = self.last_token_end - legacy.len();
                            let named = if legacy.ends_with("_name") {
                                self.eat(&Token::Equal)
                            } else {
                                self.eat(&Token::LeftParen)
                            };
                            let key = match self.peek() {
                                Some(Token::StringLit(s) | Token::RawString(s)) if named => {
                                    let key = unquote_str(s).to_string();
                                    self.advance();
                                    Some(key)
                                }
                                _ => None,
                            };
                            if named && !legacy.ends_with("_name") {
                                let _ = self.expect(&Token::RightParen);
                            }
                            self.error_at_with_kind_and_hint(
                                format!(
                                    "the `{legacy}` wire modifier is retired: a field has one \
                                     text key, set with `#[serial(key = \"..\")]` [E_SERIAL_LEGACY_SPELLING]"
                                ),
                                start..self.last_token_end,
                                "`hew fmt --migrate` rewrites it",
                                ParseDiagnosticKind::LegacySerialSpelling,
                            );
                            if modifiers.legacy_key.is_none() {
                                modifiers.legacy_key = key;
                            }
                        }
                        _ => {
                            self.restore_pos(saved);
                            break;
                        }
                    }
                }
                _ => break,
            }
        }

        modifiers
    }

    pub(crate) fn parse_wire_field_number_after_marker(
        &mut self,
        mode: WireFieldParseMode,
    ) -> Result<u32, ()> {
        let Some(Token::Integer(num_str)) = self.peek() else {
            match mode {
                WireFieldParseMode::Struct => {
                    self.error("expected field number after '@'".to_string());
                }
            }
            return Err(());
        };

        let raw = (*num_str).to_string();
        self.advance();

        parse_int_literal(&raw)
            .ok()
            .and_then(|(value, _)| u32::try_from(value).ok())
            .ok_or_else(|| match mode {
                WireFieldParseMode::Struct => {
                    self.error("invalid field number after '@'".to_string());
                }
            })
    }

    pub(crate) fn parse_wire_field_number_and_modifiers(
        &mut self,
        mode: WireFieldParseMode,
    ) -> ParsedWireField {
        let explicit_number = match mode {
            WireFieldParseMode::Struct => {
                if self.eat(&Token::At) {
                    self.parse_wire_field_number_after_marker(mode).ok()
                } else {
                    None
                }
            }
        };

        let modifiers = self.parse_wire_field_modifiers();
        ParsedWireField {
            explicit_number,
            modifiers,
        }
    }

    /// The text-key case a type declares with `#[serial(case = "..")]`. A
    /// retired `#[json(..)]` or `#[yaml(..)]` is refused; the first one in
    /// source order still supplies the case `hew fmt --migrate` writes.
    pub(crate) fn parse_serial_case(&mut self, attrs: &[Attribute]) -> Option<NamingCase> {
        let mut case = None;
        let mut legacy = None;
        for attr in attrs {
            match attr.name.as_str() {
                "serial" => {
                    for arg in &attr.args {
                        match arg {
                            AttributeArg::KeyValue { key, value } if key == "case" => {
                                if let Some(parsed) = NamingCase::parse(value) {
                                    case = Some(parsed);
                                } else {
                                    self.serial_argument_error(
                                        attr,
                                        &format!("`{value}` is not a case"),
                                        "use camelCase, PascalCase, snake_case, SCREAMING_SNAKE or kebab-case",
                                    );
                                }
                            }
                            _ => self.serial_argument_error(
                                attr,
                                "a type takes `case = \"..\"`",
                                "for example #[serial(case = \"camelCase\")]",
                            ),
                        }
                    }
                    if attr.args.is_empty() {
                        self.serial_argument_error(
                            attr,
                            "a type takes `case = \"..\"`",
                            "for example #[serial(case = \"camelCase\")]",
                        );
                    }
                }
                "json" | "yaml" => {
                    self.error_at_with_kind_and_hint(
                        format!(
                            "`#[{}]` is retired: a type has one text naming, set with \
                             `#[serial(case = \"..\")]` [E_SERIAL_LEGACY_SPELLING]",
                            attr.name
                        ),
                        attr.span.clone(),
                        "`hew fmt --migrate` rewrites it",
                        ParseDiagnosticKind::LegacySerialSpelling,
                    );
                    if legacy.is_none() {
                        legacy = attr
                            .args
                            .first()
                            .and_then(|arg| NamingCase::parse_legacy(arg.as_str()));
                    }
                }
                _ => {}
            }
        }
        case.or(legacy)
    }

    /// A field's `#[serial(..)]` takes `key = ".."` and `skip`.
    pub(crate) fn validate_serial_field(&mut self, attr: &Attribute) {
        for arg in &attr.args {
            match arg {
                AttributeArg::KeyValue { key, value } if key == "key" && !value.is_empty() => {}
                AttributeArg::Positional(word) if word == "skip" => {}
                _ => self.serial_argument_error(
                    attr,
                    "a field takes `key = \"..\"` and `skip`",
                    "for example #[serial(key = \"type\")]",
                ),
            }
        }
        if attr.args.is_empty() {
            self.serial_argument_error(
                attr,
                "a field takes `key = \"..\"` and `skip`",
                "for example #[serial(key = \"type\")]",
            );
        }
    }

    fn serial_argument_error(&mut self, attr: &Attribute, what: &str, hint: &str) {
        self.error_at_with_kind_and_hint(
            format!("invalid arguments for `#[serial]`: {what} [E_ATTRIBUTE_ARGUMENT]"),
            attr.span.clone(),
            hint,
            ParseDiagnosticKind::AttributeArgument,
        );
    }

    fn refuse_generic_wire(&mut self, span: Span) {
        self.error_at(
            "`#[wire]` types cannot be generic: a tag must mean the same member in every \
             instantiation [E_WIRE_GENERIC]"
                .to_string(),
            span,
        );
    }

    /// Parse `#[wire] type Name { field: Type, ... }` into a `TypeDecl` with wire metadata.
    #[expect(
        clippy::too_many_lines,
        reason = "expression parsing handles all expression types"
    )]
    pub(crate) fn parse_wire_struct(
        &mut self,
        attrs: &[Attribute],
        visibility: Visibility,
    ) -> Option<TypeDecl> {
        // `#[wire]` is exclusive with `#[resource]` and `#[linear]`: wire types
        // describe runtime traffic shapes; resource/linear are ownership-discipline
        // markers.  They operate at different levels and cannot compose on the same
        // declaration.
        for attr in attrs {
            if attr.name == "resource" || attr.name == "linear" {
                self.error_at(
                    format!(
                        "#[wire] cannot be combined with #[{}] on the same type — \
                         wire types are traffic-shape declarations; #[resource] and \
                         #[linear] are ownership-discipline markers \
                         [E_TYPE_MARKER_CONFLICT]",
                        attr.name
                    ),
                    attr.span.clone(),
                );
            }
        }
        self.expect(&Token::Type)?;
        let name = self.expect_ident()?;
        if self.peek() == Some(&Token::Less) {
            let start = self.peek_span().start;
            let _ = self.parse_opt_type_params();
            self.refuse_generic_wire(start..self.last_token_end);
        }
        self.expect(&Token::LeftBrace)?;

        let mut fields = Vec::new();
        let mut field_meta = Vec::new();
        let mut reserved_numbers: Vec<u32> = Vec::new();
        let mut explicit_numbers: Vec<u32> = Vec::new();
        let mut seen_explicit_numbers = std::collections::HashSet::new();

        while self.peek() != Some(&Token::RightBrace) && !self.at_end() {
            // Check for `reserved @N, @M, ...;`
            if self.peek() == Some(&Token::Reserved) {
                self.advance();
                while self.peek() != Some(&Token::Semicolon) && !self.at_end() {
                    self.expect(&Token::At)?;
                    if let Some(Token::Integer(n_str)) = self.peek() {
                        if let Some(num) = parse_int_literal(n_str)
                            .ok()
                            .and_then(|(v, _)| u32::try_from(v).ok())
                        {
                            reserved_numbers.push(num);
                        } else {
                            self.error("invalid field number after '@'".to_string());
                        }
                        self.advance();
                    } else {
                        self.error("expected field number after '@'".to_string());
                        break;
                    }
                    if !self.eat(&Token::Comma) {
                        break;
                    }
                }
                self.expect_member_terminator("reserved");
                continue;
            }

            // Parse field: [#[serial(..)]] name: Type [@N] [modifiers];
            let field_start = self.peek_span().start;
            let mut attributes = self.parse_attributes();
            self.validate_attributes_for(&attributes, AttrPosition::Field);
            let field_name = self.expect_ident()?;
            self.expect(&Token::Colon)?;
            let ty = self.parse_type()?;
            let parsed_field =
                self.parse_wire_field_number_and_modifiers(WireFieldParseMode::Struct);
            if let Some(explicit_num) = parsed_field.explicit_number {
                if reserved_numbers.contains(&explicit_num) {
                    self.error(format!("wire field number @{explicit_num} is reserved"));
                }
                if !seen_explicit_numbers.insert(explicit_num) {
                    self.error(format!("duplicate wire field number @{explicit_num}"));
                }
                explicit_numbers.push(explicit_num);
            }

            if let Some(key) = parsed_field.modifiers.legacy_key {
                if !attributes.iter().any(|attr| attr.name == "serial") {
                    attributes.push(Attribute {
                        name: "serial".to_string(),
                        args: vec![AttributeArg::KeyValue {
                            key: "key".to_string(),
                            value: key,
                        }],
                        span: 0..0,
                    });
                }
            }
            fields.push(TypeBodyItem::Field {
                name: field_name,
                ty,
                attributes,
                doc_comment: None,
                span: field_start..self.last_token_end,
            });
            field_meta.push((
                field_name.to_string(),
                parsed_field.explicit_number,
                parsed_field.modifiers.is_optional,
                parsed_field.modifiers.is_deprecated,
                parsed_field.modifiers.is_repeated,
                parsed_field.modifiers.since,
            ));

            self.expect_member_terminator("field");
        }
        self.expect(&Token::RightBrace)?;

        // Auto-assign field numbers: 1, 2, 3... skipping explicit @N and reserved numbers
        let used_numbers: std::collections::HashSet<u32> = explicit_numbers
            .iter()
            .chain(reserved_numbers.iter())
            .copied()
            .collect();
        let mut auto_counter: u32 = 1;
        let mut resolved_meta = Vec::new();

        for (field_name, explicit_num, is_optional, is_deprecated, is_repeated, since) in field_meta
        {
            let field_number = if let Some(n) = explicit_num {
                n
            } else {
                while used_numbers.contains(&auto_counter) {
                    auto_counter += 1;
                }
                let n = auto_counter;
                auto_counter += 1;
                n
            };
            resolved_meta.push(WireFieldMeta {
                field_name,
                field_number,
                is_optional,
                is_deprecated,
                is_repeated,
                since,
            });
        }

        let serial_case = self.parse_serial_case(attrs);

        // Extract version and min_version from #[wire(version = N, min_version = M)]
        let wire_attr = attrs.iter().find(|a| a.name == "wire");
        let version = wire_attr.and_then(|a| {
            a.args.iter().find_map(|arg| match arg {
                AttributeArg::KeyValue { key, value } if key == "version" => value.parse().ok(),
                _ => None,
            })
        });
        let min_version = wire_attr.and_then(|a| {
            a.args.iter().find_map(|arg| match arg {
                AttributeArg::KeyValue { key, value } if key == "min_version" => value.parse().ok(),
                _ => None,
            })
        });

        Some(TypeDecl {
            origin: crate::ast::DeclarationOrigin::Authored,
            visibility,
            kind: TypeDeclKind::Struct,
            name,
            type_params: None,
            where_clause: None,
            body: fields,
            doc_comment: None,
            wire: Some(WireMetadata {
                field_meta: resolved_meta,
                reserved_numbers,
                version,
                min_version,
            }),
            is_indirect: false,
            resource_marker: ResourceMarker::None,
            is_opaque: false,
            consuming_methods: Vec::new(),
            lang_item: attrs
                .iter()
                .find(|a| a.name == "lang_item")
                .and_then(|a| a.args.first().map(|arg| arg.as_str().to_string())),
            serial_case,
        })
    }

    /// Parse `#[wire] enum Name { Variant1, Variant2(T), Variant3 { f: U } }` into a
    /// `TypeDecl` with wire metadata attached.  Enums carry the type-level wire
    /// metadata (`version`, `min_version`) but no
    /// per-field tag numbers — variant payloads are tagged by the variant
    /// index, not by `@N` annotations on individual fields.
    ///
    /// The enum body itself is parsed by the shared `parse_struct_or_enum`
    /// helper (which handles unit / tuple / struct variant payloads, type
    /// parameters, and where clauses).  This function attaches the wire
    /// metadata to the resulting `TypeDecl` and validates the marker-conflict
    /// rules that also apply to `#[wire] type`.
    pub(crate) fn parse_wire_enum(
        &mut self,
        attrs: &[Attribute],
        visibility: Visibility,
    ) -> Option<TypeDecl> {
        // `#[wire]` is exclusive with `#[resource]` and `#[linear]`: wire types
        // describe runtime traffic shapes; resource/linear are ownership-
        // discipline markers.  Same rule as `parse_wire_struct`.
        for attr in attrs {
            if attr.name == "resource" || attr.name == "linear" {
                self.error_at(
                    format!(
                        "#[wire] cannot be combined with #[{}] on the same type — \
                         wire types are traffic-shape declarations; #[resource] and \
                         #[linear] are ownership-discipline markers \
                         [E_TYPE_MARKER_CONFLICT]",
                        attr.name
                    ),
                    attr.span.clone(),
                );
            }
        }

        // Delegate enum-body parsing to the shared helper.  It consumes the
        // `enum` keyword, name, type-params, where-clause, body braces, and
        // populates `body` with `TypeBodyItem::Variant` entries.  The
        // resulting `TypeDecl` has `wire: None`, which we override below.
        let mut td = self.parse_struct_or_enum(visibility, attrs)?;
        if td.kind != TypeDeclKind::Enum {
            // Caller dispatched us on `Token::Enum`; parse_struct_or_enum
            // should have produced an Enum.  Anything else is a parser bug.
            self.error("internal: parse_wire_enum reached non-enum TypeDecl".to_string());
            return None;
        }

        if td.type_params.is_some() {
            let span = attrs
                .iter()
                .find(|a| a.name == "wire")
                .map_or(0..0, |a| a.span.clone());
            self.refuse_generic_wire(span);
        }
        self.require_wire_variant_tags(&mut td);

        // Extract version/min_version from `#[wire(version = N, min_version = M)]`.
        let wire_attr = attrs.iter().find(|a| a.name == "wire");
        let version = wire_attr.and_then(|a| {
            a.args.iter().find_map(|arg| match arg {
                AttributeArg::KeyValue { key, value } if key == "version" => value.parse().ok(),
                _ => None,
            })
        });
        let min_version = wire_attr.and_then(|a| {
            a.args.iter().find_map(|arg| match arg {
                AttributeArg::KeyValue { key, value } if key == "min_version" => value.parse().ok(),
                _ => None,
            })
        });

        td.wire = Some(WireMetadata {
            field_meta: Vec::new(),
            reserved_numbers: Vec::new(),
            version,
            min_version,
        });
        Some(td)
    }

    /// Every `#[wire]` variant carries a unique stable tag. A missing tag is
    /// refused; the declaration ordinal fills it in so `hew fmt --migrate`
    /// writes the tag the variant already had on the wire.
    fn require_wire_variant_tags(&mut self, td: &mut TypeDecl) {
        let mut seen = std::collections::HashSet::new();
        for (ordinal, item) in td.body.iter_mut().enumerate() {
            let TypeBodyItem::Variant(variant) = item else {
                continue;
            };
            let tag = if let Some(tag) = variant.tag {
                tag
            } else {
                let tag = u32::try_from(ordinal).unwrap_or(u32::MAX);
                self.error_at_with_kind_and_hint(
                    format!(
                        "`#[wire]` variant `{}` needs a stable tag [E_WIRE_VARIANT_TAG]",
                        variant.name
                    ),
                    variant.span.clone(),
                    format!(
                        "write `{} @N;`; `hew fmt --migrate` writes its current tag, @{tag}",
                        variant.name
                    ),
                    ParseDiagnosticKind::WireVariantTagMissing,
                );
                variant.tag = Some(tag);
                tag
            };
            if !seen.insert(tag) {
                self.error_at(
                    format!("duplicate wire variant tag @{tag}"),
                    variant.span.clone(),
                );
            }
        }
    }
}
