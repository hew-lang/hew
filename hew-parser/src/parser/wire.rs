//! Wire (`#[wire]`) declarations and wire-field modifier parsing.

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

    pub(crate) fn wire_metadata(attrs: &[Attribute]) -> Option<WireMetadata> {
        let attr = attrs.iter().find(|attr| attr.name == "wire")?;
        let version = |name: &str| {
            attr.args.iter().find_map(|arg| match arg {
                AttributeArg::KeyValue { key, value } if key == name => value.parse().ok(),
                _ => None,
            })
        };
        Some(WireMetadata {
            field_meta: Vec::new(),
            reserved: Vec::new(),
            version: version("version"),
            min_version: version("min_version"),
        })
    }

    pub(crate) fn parse_wire_reserved(&mut self) -> Option<WireReserved> {
        let start = self.expect(&Token::Reserved)?.start;
        let mut numbers = Vec::new();
        loop {
            self.expect(&Token::At)?;
            numbers.push(
                self.parse_wire_field_number_after_marker(WireFieldParseMode::Struct)
                    .ok()?,
            );
            if !self.eat(&Token::Comma) {
                break;
            }
        }
        let clause = WireReserved {
            numbers,
            span: start..self.last_token_end,
        };
        self.expect_member_terminator("reserved");
        Some(clause)
    }

    fn resolve_wire_numbers(
        &mut self,
        explicit: &[Option<u32>],
        reserved: &[u32],
        start: u32,
        member: &str,
    ) -> Option<Vec<u32>> {
        crate::wire::assign_wire_numbers(explicit, reserved, start)
            .map_err(|error| {
                use crate::wire::WireNumberError;
                self.error(match error {
                    WireNumberError::Duplicate(number) => {
                        format!("duplicate wire {member} @{number}")
                    }
                    WireNumberError::Reserved(number) => {
                        format!("wire {member} @{number} is reserved")
                    }
                    WireNumberError::Exhausted => {
                        format!("no wire {member} remains for automatic assignment")
                    }
                });
            })
            .ok()
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
        let type_params = self.parse_opt_type_params()?;
        let where_clause = self.parse_opt_where_clause()?;
        self.expect(&Token::LeftBrace)?;

        let mut fields = Vec::new();
        let mut field_meta = Vec::new();
        let mut reserved = Vec::new();

        while self.peek() != Some(&Token::RightBrace) && !self.at_end() {
            let mut doc_comment = self.collect_doc_comments();
            // Check for `reserved @N, @M, ...;`
            if self.peek() == Some(&Token::Reserved) {
                reserved.push(self.parse_wire_reserved()?);
                continue;
            }

            // Parse field: [#[serial(..)]] name: Type [@N] [modifiers];
            let field_start = self.peek_span().start;
            let mut attributes = self.parse_attributes();
            if doc_comment.is_none() {
                doc_comment = self.collect_doc_comments();
            }
            self.validate_attributes_for(&attributes, AttrPosition::Field);
            let field_name = self.expect_ident()?;
            self.expect(&Token::Colon)?;
            let ty = self.parse_type()?;
            let parsed_field =
                self.parse_wire_field_number_and_modifiers(WireFieldParseMode::Struct);
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
                doc_comment,
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

        let explicit = field_meta.iter().map(|field| field.1).collect::<Vec<_>>();
        let reserved_numbers = reserved
            .iter()
            .flat_map(|clause| clause.numbers.iter().copied())
            .collect::<Vec<_>>();
        let numbers = self.resolve_wire_numbers(&explicit, &reserved_numbers, 1, "field number")?;
        let mut resolved_meta = Vec::new();

        for (
            (field_name, explicit_number, is_optional, is_deprecated, is_repeated, since),
            field_number,
        ) in field_meta.into_iter().zip(numbers)
        {
            resolved_meta.push(WireFieldMeta {
                field_name,
                field_number,
                field_number_explicit: explicit_number.is_some(),
                is_optional,
                is_deprecated,
                is_repeated,
                since,
            });
        }

        let serial_case = self.parse_serial_case(attrs);

        let mut wire = Self::wire_metadata(attrs).expect("wire type has a wire attribute");
        wire.field_meta = resolved_meta;
        wire.reserved = reserved;

        Some(TypeDecl {
            origin: crate::ast::DeclarationOrigin::Authored,
            visibility,
            kind: TypeDeclKind::Struct,
            name,
            type_params,
            where_clause,
            body: fields,
            doc_comment: None,
            wire: Some(wire),
            is_indirect: false,
            resource_marker: ResourceMarker::None,
            is_opaque: false,
            lang_item: attrs
                .iter()
                .find(|a| a.name == "lang_item")
                .and_then(|a| a.args.first().map(|arg| arg.as_str().to_string())),
            serial_case,
        })
    }

    /// Parse `#[wire] enum Name { Variant1, Variant2(T), Variant3 { f: U } }` into a
    /// `TypeDecl` with wire metadata attached.  Enums carry the type-level wire
    /// metadata (`version`, `min_version`) and optional explicit variant tags.
    /// Missing tags are assigned from zero, skipping explicit and reserved tags.
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
        // populates `body` with `TypeBodyItem::Variant` entries and retains
        // reserved clauses in the declaration's wire metadata.
        let mut td = self.parse_struct_or_enum(visibility, attrs)?;
        if td.kind != TypeDeclKind::Enum {
            // Caller dispatched us on `Token::Enum`; parse_struct_or_enum
            // should have produced an Enum.  Anything else is a parser bug.
            self.error("internal: parse_wire_enum reached non-enum TypeDecl".to_string());
            return None;
        }

        let variants = td
            .body
            .iter_mut()
            .filter_map(|item| match item {
                TypeBodyItem::Variant(variant) => Some(variant),
                TypeBodyItem::Field { .. } => None,
            })
            .collect::<Vec<_>>();
        let explicit = variants
            .iter()
            .map(|variant| variant.tag)
            .collect::<Vec<_>>();
        let reserved = td
            .wire
            .as_ref()
            .expect("wire enum has wire metadata")
            .reserved
            .iter()
            .flat_map(|clause| clause.numbers.iter().copied())
            .collect::<Vec<_>>();
        let numbers = self.resolve_wire_numbers(&explicit, &reserved, 0, "variant tag")?;
        for (variant, number) in variants.into_iter().zip(numbers) {
            variant.tag = Some(number);
        }
        Some(td)
    }
}
