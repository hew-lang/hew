//! `#[extern_symbol("…")]` attribute: structured template parsing.
//!
//! Stage 1 (PR e66c0798) wired the attribute through the parser and
//! validated attachment positions (extern `"C"` block fns, inherent
//! impl fns, trait-impl fns). The captured payload is a raw string
//! (`attr.args[0].as_str()`) — Stage 2 promotes it to a **structured
//! template value** so downstream stages (call-site monomorphization,
//! signature-match validator, collision detector) never re-parse the
//! string at the boundary.
//!
//! This matches the `string-identifier-fragility` invariant: once a
//! structured representation is available, callers must consume it.
//!
//! ## Grammar (W3.001 scope)
//!
//! ```text
//! extern_symbol_value := '"' symbol_template '"'
//! symbol_template     := segment+
//! segment             := literal_run | placeholder
//! literal_run         := [A-Za-z0-9_]+
//! placeholder         := "{T}"
//! ```
//!
//! W3.001 supports the single placeholder `{T}`. Multi-parameter
//! templates (`{K}`, `{V}`) are explicit W3.002 work; this module's
//! [`PlaceholderName`] enum is closed so adding a new placeholder
//! requires a deliberate language-substrate edit.
//!
//! ## Failure modes (Stage-2 diagnostics)
//!
//! Every failure surfaces as
//! [`crate::error::TypeErrorKind::InvalidExternSymbolTemplate`] with a
//! short, deterministic `reason` string that downstream tests can pin
//! exactly. Stage 2 fires the diagnostic at FnSig-ingest time; Stage 3
//! wires the resolved template into call-site expansion.

use hew_parser::ast::Span;

use crate::check::TypeDef;
use crate::runtime_calling_convention::RuntimeCallingConvention;
use crate::ty::Ty;
use std::collections::HashMap;

/// The closed set of placeholders the W3.001 template grammar accepts.
///
/// Closed by design: adding a placeholder is a deliberate
/// language-substrate change (W3.002 adds `{K}` / `{V}` for `HashMap`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PlaceholderName {
    /// `{T}` — element-type placeholder (Vec's case).
    T,
}

impl PlaceholderName {
    /// The source spelling of this placeholder (without braces).
    #[must_use]
    pub const fn surface(self) -> &'static str {
        match self {
            Self::T => "T",
        }
    }

    /// Parse a single placeholder name (the text between `{` and `}`).
    ///
    /// Returns `None` if the spelling is not in the closed
    /// [`PlaceholderName`] set; the caller turns that into an
    /// `InvalidExternSymbolTemplate { reason: "unknown placeholder …" }`
    /// diagnostic.
    #[must_use]
    pub fn from_surface(s: &str) -> Option<Self> {
        match s {
            "T" => Some(Self::T),
            _ => None,
        }
    }
}

/// A parsed `#[extern_symbol]` template — a sequence of literal runs
/// interleaved with placeholders.
///
/// `raw` is preserved (lossless) for diagnostic output. `segments` is
/// the structured form that Stage-3 expansion consumes. `placeholders`
/// is the de-duplicated set of placeholders that appear, in
/// declaration order — Stage 3 uses this to know which `Ty` arguments
/// it must look up at each monomorphization.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ExternSymbolTemplate {
    /// The original template string (as captured by the parser,
    /// without the surrounding `"…"` quotes).
    pub raw: String,
    /// Structured segments in source order.
    pub segments: Vec<TemplateSegment>,
    /// De-duplicated placeholders that appear in the template, in
    /// first-occurrence order. Empty for a fully monomorphic symbol
    /// (e.g. `"hew_vec_len"`).
    pub placeholders: Vec<PlaceholderName>,
}

/// A single template segment.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TemplateSegment {
    /// A literal run of identifier-allowed characters.
    Literal(String),
    /// A placeholder, e.g. `{T}` → `Placeholder(PlaceholderName::T)`.
    Placeholder(PlaceholderName),
}

/// A structured failure produced by [`ExternSymbolTemplate::parse`].
///
/// The `reason` field becomes the body of the
/// `InvalidExternSymbolTemplate` diagnostic; the variant
/// discriminates programmatically for tests and Stage-3 routing.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TemplateError {
    /// The template was the empty string.
    Empty,
    /// A `{` was not closed by a matching `}`.
    UnbalancedOpenBrace { byte_offset: usize },
    /// A `}` appeared without a preceding unmatched `{`.
    UnbalancedCloseBrace { byte_offset: usize },
    /// A `{` was nested inside another `{` (e.g. `{{T}}`).
    /// W3.001 grammar is single-level.
    NestedPlaceholder { byte_offset: usize },
    /// A placeholder body was empty (`{}`).
    EmptyPlaceholder { byte_offset: usize },
    /// A placeholder spelling is not in the closed [`PlaceholderName`]
    /// set (e.g. `{Q}`, `{Foo}`).
    UnknownPlaceholder { name: String, byte_offset: usize },
    /// A literal character outside the allowed identifier alphabet
    /// `[A-Za-z0-9_]` appeared in a literal run (e.g. `-`, `.`,
    /// whitespace, `"`). The W3.001 grammar is deliberately narrow so
    /// future hooks (e.g. dlsym, mangled names) require an explicit
    /// substrate widening.
    InvalidLiteralChar { ch: char, byte_offset: usize },
}

impl TemplateError {
    /// Short, deterministic, test-pinnable reason string.
    ///
    /// Stable: downstream diagnostic-precision tests (Stage 5) pin
    /// against these exact reasons.
    #[must_use]
    pub fn reason(&self) -> String {
        match self {
            Self::Empty => "empty template".to_string(),
            Self::UnbalancedOpenBrace { byte_offset } => {
                format!("unbalanced `{{` at byte offset {byte_offset}")
            }
            Self::UnbalancedCloseBrace { byte_offset } => {
                format!("unbalanced `}}` at byte offset {byte_offset}")
            }
            Self::NestedPlaceholder { byte_offset } => {
                format!("nested `{{` inside placeholder at byte offset {byte_offset}")
            }
            Self::EmptyPlaceholder { byte_offset } => {
                format!("empty placeholder `{{}}` at byte offset {byte_offset}")
            }
            Self::UnknownPlaceholder { name, byte_offset } => {
                format!("unknown placeholder `{{{name}}}` at byte offset {byte_offset}")
            }
            Self::InvalidLiteralChar { ch, byte_offset } => {
                format!(
                    "invalid character `{ch}` in template literal at byte offset {byte_offset} \
                     (allowed: ASCII letters, digits, `_`)"
                )
            }
        }
    }
}

impl ExternSymbolTemplate {
    /// Parse a `#[extern_symbol("…")]` template body.
    ///
    /// Input is the raw payload without the surrounding quotes (i.e.
    /// what `attr.args[0].as_str()` returns from the parser).
    ///
    /// Returns the structured template on success or a precise
    /// [`TemplateError`] on failure. The caller is responsible for
    /// turning the error into an
    /// `InvalidExternSymbolTemplate { reason }` diagnostic span-anchored
    /// at the attribute's `attr.span`.
    ///
    /// Total: the function classifies every byte; it never silently
    /// drops content (fail-closed per CLAUDE.md #2).
    ///
    /// # Errors
    ///
    /// Returns a [`TemplateError`] describing the first grammar
    /// violation encountered (empty template, unbalanced braces,
    /// nested or empty placeholder, unknown placeholder name, or a
    /// literal character outside the identifier alphabet
    /// `[A-Za-z0-9_]`). Errors are produced eagerly; the parser does
    /// not continue past the first violation.
    pub fn parse(raw: &str) -> Result<Self, TemplateError> {
        if raw.is_empty() {
            return Err(TemplateError::Empty);
        }

        let mut segments: Vec<TemplateSegment> = Vec::new();
        let mut placeholders: Vec<PlaceholderName> = Vec::new();
        let mut current_literal = String::new();

        let bytes = raw.as_bytes();
        let mut i = 0;
        while i < bytes.len() {
            let b = bytes[i];
            match b {
                b'{' => {
                    // Flush any pending literal run.
                    if !current_literal.is_empty() {
                        segments.push(TemplateSegment::Literal(std::mem::take(
                            &mut current_literal,
                        )));
                    }
                    // Scan until matching `}`.
                    let open_at = i;
                    i += 1;
                    let body_start = i;
                    let mut body_end = None;
                    while i < bytes.len() {
                        match bytes[i] {
                            b'}' => {
                                body_end = Some(i);
                                break;
                            }
                            b'{' => {
                                return Err(TemplateError::NestedPlaceholder { byte_offset: i });
                            }
                            _ => i += 1,
                        }
                    }
                    let Some(end) = body_end else {
                        return Err(TemplateError::UnbalancedOpenBrace {
                            byte_offset: open_at,
                        });
                    };
                    if end == body_start {
                        return Err(TemplateError::EmptyPlaceholder {
                            byte_offset: open_at,
                        });
                    }
                    // Safety: `body_start..end` is a sub-slice of valid
                    // ASCII (we only consume non-`{`/`}` bytes here),
                    // but we use `from_utf8` to honour Rust's safety
                    // requirements.
                    let body = std::str::from_utf8(&bytes[body_start..end]).map_err(|_| {
                        TemplateError::InvalidLiteralChar {
                            ch: '\u{FFFD}',
                            byte_offset: body_start,
                        }
                    })?;
                    let Some(name) = PlaceholderName::from_surface(body) else {
                        return Err(TemplateError::UnknownPlaceholder {
                            name: body.to_string(),
                            byte_offset: open_at,
                        });
                    };
                    segments.push(TemplateSegment::Placeholder(name));
                    if !placeholders.contains(&name) {
                        placeholders.push(name);
                    }
                    // Advance past the `}`.
                    i = end + 1;
                }
                b'}' => {
                    return Err(TemplateError::UnbalancedCloseBrace { byte_offset: i });
                }
                _ => {
                    // Literal character — must be in the identifier
                    // alphabet `[A-Za-z0-9_]`. The grammar is
                    // deliberately narrow; widening is a substrate
                    // change.
                    let ch = bytes[i] as char;
                    if !(ch.is_ascii_alphanumeric() || ch == '_') {
                        return Err(TemplateError::InvalidLiteralChar { ch, byte_offset: i });
                    }
                    current_literal.push(ch);
                    i += 1;
                }
            }
        }

        if !current_literal.is_empty() {
            segments.push(TemplateSegment::Literal(current_literal));
        }

        // An empty `raw` was rejected up-front; if we reach here with
        // zero segments the parser is buggy.
        debug_assert!(
            !segments.is_empty(),
            "non-empty raw template produced zero segments: {raw:?}",
        );

        Ok(Self {
            raw: raw.to_string(),
            segments,
            placeholders,
        })
    }

    /// `true` if the template has no placeholders (a fully monomorphic
    /// symbol like `"hew_vec_len"`).
    #[must_use]
    pub fn is_monomorphic(&self) -> bool {
        self.placeholders.is_empty()
    }

    /// Expand this template to a concrete runtime symbol given the
    /// resolved type argument bound to `{T}`.
    ///
    /// Substitution uses the canonical token from
    /// [`RuntimeCallingConvention`] for the type, computed via
    /// [`RuntimeCallingConvention::for_ty_with_layout`] — the path that
    /// can positively prove heap-handle-ness for `Ty::Named` by
    /// consulting `TypeDef::is_indirect`. Callers without a resolved
    /// `TypeDef` table (e.g. tests) pass an empty map and the routine
    /// falls back to the context-free classification, which fails
    /// closed for unproven `Ty::Named` (routes to `LayoutDescriptor`
    /// and surfaces as [`TemplateExpansionError::UnsupportedCallingConvention`]).
    ///
    /// Stage 3 (W3.001) defines the parallel call path that the
    /// differential test pins against the legacy `resolve_vec_method`
    /// magic table. Stage 4 deletes the magic table; Stage 5 wires
    /// the e2e path so the attribute path drives production codegen.
    ///
    /// # Errors
    ///
    /// * [`TemplateExpansionError::UnsupportedCallingConvention`] —
    ///   the substituted type maps to a calling convention with no
    ///   runtime entry point in W3.001 scope (today: `LayoutDescriptor`).
    ///   The diagnostic carries the expected `*_layout` symbol name so
    ///   Stage 5 negative tests can pin it exactly.
    pub fn expand(
        &self,
        type_arg: &Ty,
        type_defs: &HashMap<String, TypeDef>,
    ) -> Result<String, TemplateExpansionError> {
        if self.is_monomorphic() {
            return Ok(self.raw.clone());
        }
        let cc = RuntimeCallingConvention::for_ty_with_layout(type_arg, type_defs);
        if cc == RuntimeCallingConvention::LayoutDescriptor {
            // W3.003 Stage 3a fail-closed: BitCopy runtime entry points
            // exist, but codegen does not yet synthesize the layout
            // descriptor pseudo-FFI operands needed to call them safely.
            // Emit the *would-be* symbol name so negative tests can pin the
            // diagnostic exactly.
            let mut would_be = String::with_capacity(self.raw.len());
            for seg in &self.segments {
                match seg {
                    TemplateSegment::Literal(s) => would_be.push_str(s),
                    TemplateSegment::Placeholder(_) => {
                        would_be.push_str(cc.canonical_token());
                    }
                }
            }
            return Err(TemplateExpansionError::UnsupportedCallingConvention {
                expected_symbol: would_be,
                convention: cc,
            });
        }
        let token = cc.canonical_token();
        let mut out = String::with_capacity(self.raw.len());
        for seg in &self.segments {
            match seg {
                TemplateSegment::Literal(s) => out.push_str(s),
                TemplateSegment::Placeholder(_) => out.push_str(token),
            }
        }
        Ok(out)
    }
}

/// Failure produced by [`ExternSymbolTemplate::expand`] when a
/// monomorphization cannot resolve to a runtime entry point.
///
/// W3.003 Stage 3a scope: the only failure mode today is
/// [`Self::UnsupportedCallingConvention`] — the requested type maps to
/// [`RuntimeCallingConvention::LayoutDescriptor`], for which codegen cannot
/// yet synthesize safe layout descriptor pseudo-FFI operands. The
/// `expected_symbol` field carries the would-be symbol name so negative tests
/// can pin the diagnostic verbatim
/// (`UnsupportedRuntimeCallingConvention { expected_symbol: "hew_vec_push_layout" }`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TemplateExpansionError {
    /// The substituted type resolves to a calling convention that lacks
    /// complete codegen pseudo-FFI lowering in W3.003 Stage 3a scope.
    UnsupportedCallingConvention {
        /// The fully-expanded would-be symbol (with the convention's
        /// canonical token substituted). Used by negative-test
        /// diagnostic pinning in Stage 5.
        expected_symbol: String,
        /// The calling convention the type resolved to.
        convention: RuntimeCallingConvention,
    },
}

/// A parsed `#[extern_symbol]` attribute stored on a [`FnSig`].
///
/// Carries the structured template plus the source span of the
/// originating attribute. Stage 3 expansion consults `template` at
/// each call-site monomorphization and threads `span` into any
/// resulting diagnostic so the user is pointed at the attribute, not
/// at the call site.
///
/// [`FnSig`]: crate::check::FnSig
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExternSymbolSpec {
    /// Structured template parsed from `attr.args[0]`.
    pub template: ExternSymbolTemplate,
    /// Source span of the `#[extern_symbol(...)]` attribute (from `#`
    /// through `]`), preserved for downstream diagnostics.
    pub span: Span,
}

#[cfg(test)]
mod tests {
    use super::*;

    // ── happy paths ─────────────────────────────────────────────────

    #[test]
    fn parses_monomorphic_template() {
        let t = ExternSymbolTemplate::parse("hew_vec_len").unwrap();
        assert_eq!(t.raw, "hew_vec_len");
        assert!(t.is_monomorphic());
        assert_eq!(t.placeholders, vec![]);
        assert_eq!(
            t.segments,
            vec![TemplateSegment::Literal("hew_vec_len".to_string())]
        );
    }

    #[test]
    fn parses_template_with_single_t_placeholder() {
        let t = ExternSymbolTemplate::parse("hew_vec_push_{T}").unwrap();
        assert_eq!(t.raw, "hew_vec_push_{T}");
        assert!(!t.is_monomorphic());
        assert_eq!(t.placeholders, vec![PlaceholderName::T]);
        assert_eq!(
            t.segments,
            vec![
                TemplateSegment::Literal("hew_vec_push_".to_string()),
                TemplateSegment::Placeholder(PlaceholderName::T),
            ]
        );
    }

    #[test]
    fn parses_template_with_multiple_t_placeholders_dedupes_in_placeholders_list() {
        let t = ExternSymbolTemplate::parse("hew_{T}_to_{T}").unwrap();
        // The placeholders list is de-duplicated, but the segments
        // preserve every occurrence.
        assert_eq!(t.placeholders, vec![PlaceholderName::T]);
        assert_eq!(
            t.segments,
            vec![
                TemplateSegment::Literal("hew_".to_string()),
                TemplateSegment::Placeholder(PlaceholderName::T),
                TemplateSegment::Literal("_to_".to_string()),
                TemplateSegment::Placeholder(PlaceholderName::T),
            ]
        );
    }

    #[test]
    fn parses_template_starting_with_placeholder() {
        let t = ExternSymbolTemplate::parse("{T}_tail").unwrap();
        assert_eq!(
            t.segments,
            vec![
                TemplateSegment::Placeholder(PlaceholderName::T),
                TemplateSegment::Literal("_tail".to_string()),
            ]
        );
    }

    // ── failure paths (Stage-2 diagnostic gates) ────────────────────

    #[test]
    fn empty_template_is_rejected_with_empty_reason() {
        let err = ExternSymbolTemplate::parse("").unwrap_err();
        assert_eq!(err, TemplateError::Empty);
        assert_eq!(err.reason(), "empty template");
    }

    #[test]
    fn unbalanced_open_brace_is_rejected() {
        let err = ExternSymbolTemplate::parse("hew_{T").unwrap_err();
        assert!(
            matches!(err, TemplateError::UnbalancedOpenBrace { byte_offset: 4 }),
            "got {err:?}"
        );
        assert!(err.reason().contains("unbalanced `{`"));
    }

    #[test]
    fn unbalanced_close_brace_is_rejected() {
        let err = ExternSymbolTemplate::parse("hew_T}").unwrap_err();
        assert!(
            matches!(err, TemplateError::UnbalancedCloseBrace { byte_offset: 5 }),
            "got {err:?}"
        );
    }

    #[test]
    fn nested_placeholder_is_rejected() {
        let err = ExternSymbolTemplate::parse("hew_{{T}}").unwrap_err();
        assert!(
            matches!(err, TemplateError::NestedPlaceholder { byte_offset: 5 }),
            "got {err:?}"
        );
    }

    #[test]
    fn empty_placeholder_is_rejected() {
        let err = ExternSymbolTemplate::parse("hew_{}").unwrap_err();
        assert!(
            matches!(err, TemplateError::EmptyPlaceholder { byte_offset: 4 }),
            "got {err:?}"
        );
    }

    #[test]
    fn unknown_placeholder_is_rejected_with_name_in_reason() {
        let err = ExternSymbolTemplate::parse("hew_{Q}").unwrap_err();
        match &err {
            TemplateError::UnknownPlaceholder { name, .. } => assert_eq!(name, "Q"),
            other => panic!("expected UnknownPlaceholder, got {other:?}"),
        }
        assert!(
            err.reason().contains("`{Q}`"),
            "reason was {:?}",
            err.reason()
        );
    }

    #[test]
    fn k_and_v_placeholders_are_rejected_in_w3_001_scope() {
        // W3.002 introduces {K}/{V}; W3.001 must fail closed.
        for name in ["K", "V"] {
            let raw = format!("hew_{{{name}}}");
            let err = ExternSymbolTemplate::parse(&raw).unwrap_err();
            assert!(
                matches!(err, TemplateError::UnknownPlaceholder { ref name, .. } if name == name),
                "expected UnknownPlaceholder for {raw:?}, got {err:?}",
            );
        }
    }

    #[test]
    fn invalid_literal_char_is_rejected() {
        for bad in ["hew-vec", "hew.vec", "hew vec", "hew{T}-bad"] {
            let err = ExternSymbolTemplate::parse(bad).unwrap_err();
            assert!(
                matches!(err, TemplateError::InvalidLiteralChar { .. }),
                "expected InvalidLiteralChar for {bad:?}, got {err:?}",
            );
        }
    }

    // ── round-trip / preservation ───────────────────────────────────

    #[test]
    fn raw_is_preserved_verbatim() {
        let raw = "hew_vec_push_{T}";
        let t = ExternSymbolTemplate::parse(raw).unwrap();
        assert_eq!(t.raw, raw);
    }

    // ── expand: substitution semantics ──────────────────────────────

    fn type_defs_with_vec_handle() -> HashMap<String, TypeDef> {
        // W3.001 differential cutover: stand-in for the checker's
        // resolved TypeDef table, populated with the heap-handle
        // nominals the differential matrix exercises (Vec). Pointer
        // discrimination flows through `TypeDef::is_indirect`, not
        // via name comparison.
        use crate::check::TypeDefKind;
        let mut m = HashMap::new();
        m.insert(
            "Vec".to_string(),
            TypeDef {
                kind: TypeDefKind::Struct,
                name: "Vec".to_string(),
                type_params: vec!["T".to_string()],
                fields: HashMap::new(),
                variants: HashMap::new(),
                methods: HashMap::new(),
                doc_comment: None,
                field_order: vec![],
                is_indirect: true,
            },
        );
        m
    }

    #[test]
    fn expand_monomorphic_returns_raw_unchanged() {
        let t = ExternSymbolTemplate::parse("hew_vec_len").unwrap();
        let out = t.expand(&Ty::I32, &HashMap::new()).unwrap();
        assert_eq!(out, "hew_vec_len");
    }

    #[test]
    fn expand_substitutes_t_with_calling_convention_canonical_token() {
        let t = ExternSymbolTemplate::parse("hew_vec_push_{T}").unwrap();
        let cases = [
            (Ty::I8, "hew_vec_push_i8"),
            (Ty::U8, "hew_vec_push_u8"),
            (Ty::I16, "hew_vec_push_i16"),
            (Ty::U16, "hew_vec_push_u16"),
            (Ty::I32, "hew_vec_push_i32"),
            (Ty::I64, "hew_vec_push_i64"),
            (Ty::U32, "hew_vec_push_i32"),
            (Ty::U64, "hew_vec_push_i64"),
            (Ty::F32, "hew_vec_push_f32"),
            (Ty::F64, "hew_vec_push_f64"),
            (Ty::String, "hew_vec_push_str"),
        ];
        for (ty, expected) in cases {
            let out = t.expand(&ty, &HashMap::new()).unwrap();
            assert_eq!(out, expected, "expand mismatch for {ty:?}");
        }
    }

    #[test]
    fn expand_routes_proven_heap_handle_named_to_ptr_token() {
        let t = ExternSymbolTemplate::parse("hew_vec_push_{T}").unwrap();
        let type_defs = type_defs_with_vec_handle();
        let nested = Ty::Named {
            name: "Vec".to_string(),
            args: vec![Ty::I32],
            builtin: None,
        };
        let out = t.expand(&nested, &type_defs).unwrap();
        assert_eq!(out, "hew_vec_push_ptr");
    }

    #[test]
    fn expand_fails_closed_for_layout_descriptor_with_expected_symbol() {
        let t = ExternSymbolTemplate::parse("hew_vec_push_{T}").unwrap();
        // No TypeDef entry for `Connection` — fail-closed
        // `for_ty_with_layout` returns `LayoutDescriptor`, expansion
        // reports the would-be `_layout`-suffixed symbol.
        let user = Ty::Named {
            name: "Connection".to_string(),
            args: vec![],
            builtin: None,
        };
        let err = t.expand(&user, &HashMap::new()).unwrap_err();
        match err {
            TemplateExpansionError::UnsupportedCallingConvention {
                expected_symbol,
                convention,
            } => {
                assert_eq!(expected_symbol, "hew_vec_push_layout");
                assert_eq!(convention, RuntimeCallingConvention::LayoutDescriptor);
            }
        }
    }

    #[test]
    fn expand_routes_bool_and_char_to_stage2_vec_symbols() {
        let t = ExternSymbolTemplate::parse("hew_vec_push_{T}").unwrap();
        assert_eq!(
            t.expand(&Ty::Bool, &HashMap::new()).unwrap(),
            "hew_vec_push_bool"
        );
        assert_eq!(
            t.expand(&Ty::Char, &HashMap::new()).unwrap(),
            "hew_vec_push_i32"
        );
    }
}
