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
}
