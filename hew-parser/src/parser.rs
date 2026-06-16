//! Hand-written recursive-descent parser with Pratt precedence for operator expressions.

use crate::ast::{
    ActorDecl, ActorInit, AssocTypeBinding, Attribute, AttributeArg, BinaryOp, Block, CallArg,
    ChildSpec, CompositeGroup, CompoundAssignOp, ConstDecl, ConstParam, ConstParamTy, ElseBlock,
    Expr, ExternBlock, ExternFnDecl, FieldDecl, FnDecl, ImplDecl, ImplTypeAlias, ImportDecl,
    ImportName, ImportSpec, IntRadix, Intensity, Item, LambdaParam, Literal, MachineDecl,
    MachineEvent, MachineState, MachineTransition, MatchArm, NamingCase, OverflowFallback,
    OverflowPolicy, Param, Pattern, PatternField, Program, ReceiveFnDecl, RecordDecl, RecordField,
    RecordKind, ResourceMarker, RestartPolicy, SelectArm, ShutdownDirective, Span, Spanned, Stmt,
    StringPart, SupervisorDecl, SupervisorStrategy, TimeoutClause, TraitBound, TraitDecl,
    TraitItem, TraitMethod, TypeAliasDecl, TypeBodyItem, TypeDecl, TypeDeclKind, TypeExpr,
    TypeParam, UnaryOp, VariantDecl, VariantKind, Visibility, WhereClause, WherePredicate,
    WireDecl, WireDeclKind, WireFieldDecl, WireFieldMeta, WireMetadata,
};
use hew_lexer::Token;
use serde::Serialize;
use std::cell::Cell;
use std::rc::Rc;

type ParsedTraitBoundArgs = (Option<Vec<Spanned<TypeExpr>>>, Vec<AssocTypeBinding>);
type StructInitFields = (Vec<(String, Spanned<Expr>)>, Option<Box<Spanned<Expr>>>);

/// Parse an integer literal string, returning both value and radix.
///
/// Handles hex (`0x`), octal (`0o`), binary (`0b`) prefixes and underscore separators.
/// Merges the old `parse_int_literal` + `detect_int_radix` to avoid scanning twice.
fn parse_int_literal(s: &str) -> Result<(i64, IntRadix), std::num::ParseIntError> {
    let cleaned: String = s.chars().filter(|c| *c != '_').collect();
    if let Some(hex) = cleaned
        .strip_prefix("0x")
        .or_else(|| cleaned.strip_prefix("0X"))
    {
        i64::from_str_radix(hex, 16).map(|v| (v, IntRadix::Hex))
    } else if let Some(oct) = cleaned
        .strip_prefix("0o")
        .or_else(|| cleaned.strip_prefix("0O"))
    {
        i64::from_str_radix(oct, 8).map(|v| (v, IntRadix::Octal))
    } else if let Some(bin) = cleaned
        .strip_prefix("0b")
        .or_else(|| cleaned.strip_prefix("0B"))
    {
        i64::from_str_radix(bin, 2).map(|v| (v, IntRadix::Binary))
    } else {
        cleaned.parse::<i64>().map(|v| (v, IntRadix::Decimal))
    }
}

/// Parse a duration literal string (e.g. "100ns", "5s") into nanoseconds.
/// Parse a duration literal source string (e.g. `"60s"`, `"5m"`, `"100ms"`)
/// into a count of nanoseconds. Returns `None` on an unrecognised unit suffix
/// or an out-of-range value. This is the single source of truth for duration
/// unit interpretation; downstream stages (codegen) call it rather than
/// reimplementing unit math, keeping one duration parser.
#[must_use]
pub fn parse_duration_ns(s: &str) -> Option<i64> {
    parse_duration_literal(s)
}

fn parse_duration_literal(s: &str) -> Option<i64> {
    let s = &s.replace('_', "");
    if let Some(num) = s.strip_suffix("ns") {
        num.parse::<i64>().ok()
    } else if let Some(num) = s.strip_suffix("us") {
        num.parse::<i64>().ok().and_then(|v| v.checked_mul(1_000))
    } else if let Some(num) = s.strip_suffix("ms") {
        num.parse::<i64>()
            .ok()
            .and_then(|v| v.checked_mul(1_000_000))
    } else if let Some(num) = s.strip_suffix('h') {
        num.parse::<i64>()
            .ok()
            .and_then(|v| v.checked_mul(3_600_000_000_000))
    } else if let Some(num) = s.strip_suffix('m') {
        num.parse::<i64>()
            .ok()
            .and_then(|v| v.checked_mul(60_000_000_000))
    } else if let Some(num) = s.strip_suffix('s') {
        num.parse::<i64>()
            .ok()
            .and_then(|v| v.checked_mul(1_000_000_000))
    } else {
        None
    }
}

/// Resolve a `#[max_heap(…)]` attribute's argument(s) to a byte count.
///
/// Accepted forms and their `args` representation after `parse_attributes`:
/// - `#[max_heap(1024)]`  → `[Positional("1024")]`               → 1024 bytes
/// - `#[max_heap(512 b)]` → `[Positional("512"), Positional("b")]`   → 512 bytes
/// - `#[max_heap(2 kb)]`  → `[Positional("2"),   Positional("kb")]`  → 2048 bytes
/// - `#[max_heap(1 mb)]`  → `[Positional("1"),   Positional("mb")]`  → 1 048 576 bytes
///
/// Returns `Ok(bytes)` on success, `Err(message)` on unsupported suffix or bad integer.
fn resolve_max_heap_args(args: &[AttributeArg]) -> Result<u64, String> {
    match args {
        // Bare integer: `#[max_heap(1024)]`
        [AttributeArg::Positional(n)] => n
            .parse::<u64>()
            .map_err(|_| format!("invalid integer in `#[max_heap]`: `{n}`")),
        // Integer + unit suffix: `#[max_heap(2 kb)]`
        [AttributeArg::Positional(n), AttributeArg::Positional(unit)] => {
            let base: u64 = n
                .parse::<u64>()
                .map_err(|_| format!("invalid integer in `#[max_heap]`: `{n}`"))?;
            match unit.as_str() {
                "b" => Ok(base),
                "kb" => base
                    .checked_mul(1024)
                    .ok_or_else(|| format!("`#[max_heap]` value overflows u64: {n} kb")),
                "mb" => base
                    .checked_mul(1024 * 1024)
                    .ok_or_else(|| format!("`#[max_heap]` value overflows u64: {n} mb")),
                other => Err(format!(
                    "unsupported unit `{other}` in `#[max_heap]`; accepted suffixes: b, kb, mb \
                     (gb and larger are not supported in v0.5)"
                )),
            }
        }
        _ => Err(
            "`#[max_heap]` requires exactly one argument: a byte count optionally followed by \
             a unit (b, kb, mb)"
                .to_string(),
        ),
    }
}

/// Strip surrounding quotes from a `StringLit` or `RawString` token value.
///
/// Handles `r"..."` (raw) and `"..."` (regular) forms, returning the inner content.
fn unquote_str(s: &str) -> &str {
    s.strip_prefix("r\"")
        .or_else(|| s.strip_prefix('"'))
        .and_then(|s| s.strip_suffix('"'))
        .unwrap_or(s)
}

/// Returns true if the attribute name designates an actor lifecycle hook.
/// These attributes are permitted on plain `fn` declarations inside an
/// actor body; all other attributes on such fns are rejected.
///
/// The parameterized form `#[on(<event>)]` uses a single attribute name
/// `on` with the hook kind as a positional argument. Recognised events
/// in v0.5 are `start`, `stop`, `crash`, and `upgrade`. Validation of
/// the event identifier and per-event signature shape lives in the
/// type-checker (`hew-types::check::items`).
pub(crate) fn is_lifecycle_hook_attr(name: &str) -> bool {
    name == "on"
}

fn push_unescaped_sequence(
    chars: &[(usize, char)],
    idx: usize,
    out: &mut String,
    extra_escapes: &[char],
) -> (usize, Option<&'static str>) {
    debug_assert_eq!(chars[idx].1, '\\');

    let Some((_, next)) = chars.get(idx + 1).copied() else {
        out.push('\\');
        return (1, None);
    };

    match next {
        'n' => out.push('\n'),
        't' => out.push('\t'),
        'r' => out.push('\r'),
        '"' => out.push('"'),
        '0' => out.push('\0'),
        'x' if idx + 3 < chars.len() => {
            let (_, hi) = chars[idx + 2];
            let (_, lo) = chars[idx + 3];
            if let Ok(byte) = u8::from_str_radix(&format!("{hi}{lo}"), 16) {
                out.push(byte as char);
            } else {
                out.push('\\');
                out.push('x');
                out.push(hi);
                out.push(lo);
            }
            return (4, None);
        }
        'x' => {
            out.push('\\');
            out.push('x');
        }
        '\\' => out.push('\\'),
        'u' if chars.get(idx + 2).map(|c| c.1) == Some('{') => {
            // \u{H...} — 1–6 hex digits forming a Unicode scalar value.
            let mut consumed = 3; // backslash, u, {
            let mut hex = String::new();
            let mut found_close = false;
            let mut non_hex = false;
            while let Some(&(_, ch)) = chars.get(idx + consumed) {
                if ch == '}' {
                    consumed += 1;
                    found_close = true;
                    break;
                } else if ch.is_ascii_hexdigit() && !non_hex {
                    hex.push(ch);
                    consumed += 1;
                } else {
                    non_hex = true;
                    consumed += 1;
                }
            }
            if !found_close {
                return (
                    consumed,
                    Some("invalid Unicode escape: missing `}` in \\u{...}"),
                );
            }
            if non_hex {
                return (
                    consumed,
                    Some("invalid Unicode escape: non-hex digit in \\u{...}"),
                );
            }
            if hex.is_empty() {
                return (
                    consumed,
                    Some("invalid Unicode escape: \\u{} must contain 1–6 hex digits"),
                );
            }
            if hex.len() > 6 {
                return (
                    consumed,
                    Some("invalid Unicode escape: \\u{...} must have at most 6 hex digits"),
                );
            }
            let codepoint = u32::from_str_radix(&hex, 16).unwrap();
            if (0xD800..=0xDFFF).contains(&codepoint) {
                return (
                    consumed,
                    Some(
                        "invalid Unicode escape: surrogate codepoints (U+D800–U+DFFF) are not valid Unicode scalars",
                    ),
                );
            }
            if codepoint > 0x0010_FFFF {
                return (
                    consumed,
                    Some("invalid Unicode escape: codepoint exceeds U+10FFFF"),
                );
            }
            out.push(char::from_u32(codepoint).unwrap());
            return (consumed, None);
        }
        other if extra_escapes.contains(&other) => out.push(other),
        other => {
            out.push('\\');
            out.push(other);
        }
    }

    (2, None)
}

/// Normalise a `re"..."` token into the regex pattern string.
///
/// Regex literals use different escape semantics from Hew string literals:
/// backslashes are passed through verbatim to the regex engine (so `re"\s+"`
/// is whitespace, `re"\."` is a literal dot). Only the delimiter escape
/// `\"` is consumed to allow a literal double-quote inside the pattern.
/// Any other two-character `\x` sequence is emitted as-is (both backslash
/// and the next character) so regex operators like `\d`, `\w`, `\b`,
/// `(?P<name>...)` named groups, and anchors survive unchanged.
///
/// `raw` must be the full token text including the `re"` prefix and `"` suffix.
pub(crate) fn normalize_regex_literal(raw: &str) -> String {
    // Strip re" prefix and " suffix.
    let inner = raw
        .strip_prefix("re\"")
        .and_then(|s| s.strip_suffix('"'))
        .unwrap_or(raw);

    let mut out = String::with_capacity(inner.len());
    let mut chars = inner.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('"') => out.push('"'), // delimiter escape: \" → "
                Some(next) => {
                    // All other backslash sequences: pass through both characters.
                    out.push('\\');
                    out.push(next);
                }
                None => out.push('\\'), // trailing lone backslash — pass through
            }
        } else {
            out.push(c);
        }
    }
    out
}

/// Process escape sequences in a string literal, converting `\n`, `\t`, `\r`,
/// `\\`, `\"`, `\0`, `\xHH`, and `\u{HHHHHH}` to their corresponding characters.
///
/// Returns the decoded string together with a list of `(byte_offset, message)` pairs for
/// any malformed escape sequences, where `byte_offset` is relative to the start of `s`.
fn unescape_string(s: &str) -> (String, Vec<(usize, &'static str)>) {
    let mut out = String::with_capacity(s.len());
    let mut errors: Vec<(usize, &'static str)> = Vec::new();
    let chars: Vec<_> = s.char_indices().collect();
    let mut idx = 0;
    while idx < chars.len() {
        if chars[idx].1 == '\\' {
            let byte_off = chars[idx].0;
            let (consumed, err) = push_unescaped_sequence(&chars, idx, &mut out, &[]);
            if let Some(msg) = err {
                errors.push((byte_off, msg));
            }
            idx += consumed;
        } else {
            out.push(chars[idx].1);
            idx += 1;
        }
    }
    (out, errors)
}

/// Split an interpolated string (f-string or template literal) into literal
/// segments and parsed expression segments.
///
/// * `raw` — the full token text including delimiters (e.g. `f"hello {x}"`)
/// * `prefix_len` — bytes to strip from the front (2 for `f"`, 1 for `` ` ``)
/// * `suffix_len` — bytes to strip from the end (1 for `"` or `` ` ``)
/// * `expr_open` — the marker that opens an expression (`"{"` or `"${"`)
/// * `span_start` — byte offset of the token in the original source
#[expect(
    clippy::too_many_lines,
    reason = "string interpolation parsing and escape diagnostics are handled together"
)]
fn parse_string_parts(
    raw: &str,
    prefix_len: usize,
    suffix_len: usize,
    expr_open: &str,
    span_start: usize,
    errors: &mut Vec<ParseError>,
) -> Vec<StringPart> {
    let inner = &raw[prefix_len..raw.len() - suffix_len];
    let inner_offset = span_start + prefix_len;
    let mut parts = Vec::new();
    let mut literal_buf = String::new();
    let chars: Vec<(usize, char)> = inner.char_indices().collect();
    let mut idx = 0;

    while idx < chars.len() {
        let (byte_pos, c) = chars[idx];

        // Handle escape sequences — prevents `\{` or `\$` from opening an expr
        if c == '\\' {
            let (consumed, unescape_err) =
                push_unescaped_sequence(&chars, idx, &mut literal_buf, &['{', '}', '$', '`']);
            if let Some(msg) = unescape_err {
                let abs_off = inner_offset + chars[idx].0;
                errors.push(ParseError {
                    message: msg.to_string(),
                    span: abs_off..abs_off + consumed,
                    hint: None,
                    severity: Severity::Error,
                    kind: ParseDiagnosticKind::InvalidLiteral,
                });
            }
            idx += consumed;
            continue;
        }

        // Check for expression opening marker
        if inner[byte_pos..].starts_with(expr_open) {
            // Flush accumulated literal text
            if !literal_buf.is_empty() {
                parts.push(StringPart::Literal(std::mem::take(&mut literal_buf)));
            }

            let open_char_len = expr_open.chars().count();
            idx += open_char_len;

            let expr_start_byte = if idx < chars.len() {
                chars[idx].0
            } else {
                inner.len()
            };

            // Scan for matching `}`, respecting nested braces and string literals
            let mut depth: u32 = 1;
            while idx < chars.len() {
                let ch = chars[idx].1;
                match ch {
                    '{' => depth += 1,
                    '}' => {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                    }
                    '"' => {
                        // Skip over string literals inside the expression
                        idx += 1;
                        while idx < chars.len() && chars[idx].1 != '"' {
                            if chars[idx].1 == '\\' {
                                idx += 1;
                            }
                            idx += 1;
                        }
                    }
                    _ => {}
                }
                idx += 1;
            }

            let expr_end_byte = if idx < chars.len() {
                chars[idx].0
            } else {
                inner.len()
            };

            // Skip the closing `}`
            if idx < chars.len() {
                idx += 1;
            }

            let expr_text = &inner[expr_start_byte..expr_end_byte];
            if expr_text.trim().is_empty() {
                errors.push(ParseError {
                    message: "empty interpolation expression".to_string(),
                    span: (inner_offset + expr_start_byte)..(inner_offset + expr_end_byte),
                    hint: None,
                    severity: Severity::Error,
                    kind: ParseDiagnosticKind::InvalidLiteral,
                });
            } else if !expr_text.is_empty() {
                let mut sub_parser = Parser::new(expr_text);
                let parsed = sub_parser.parse_expr();
                errors.extend(sub_parser.errors);
                if let Some((expr, sub_span)) = parsed {
                    let adjusted_start = inner_offset + expr_start_byte + sub_span.start;
                    let adjusted_end = inner_offset + expr_start_byte + sub_span.end;
                    parts.push(StringPart::Expr((expr, adjusted_start..adjusted_end)));
                } else {
                    errors.push(ParseError {
                        message: "failed to parse interpolation expression".to_string(),
                        span: (inner_offset + expr_start_byte)..(inner_offset + expr_end_byte),
                        hint: None,
                        severity: Severity::Error,
                        kind: ParseDiagnosticKind::InvalidLiteral,
                    });
                }
            }
            continue;
        }

        literal_buf.push(c);
        idx += 1;
    }

    if !literal_buf.is_empty() {
        parts.push(StringPart::Literal(literal_buf));
    }

    parts
}

/// Maximum nesting depth for recursive parse functions.
const MAX_DEPTH: usize = 256;

/// RAII guard that decrements the parser recursion depth on drop.
///
/// Holds a cloned `Rc` of the depth counter so that `&mut self` reborrows
/// in parse methods cannot invalidate the reference — eliminating the
/// use-after-invalidation UB that a raw-pointer approach would introduce.
#[derive(Debug)]
struct RecursionGuard(Rc<Cell<usize>>);

impl Drop for RecursionGuard {
    fn drop(&mut self) {
        let cell = &*self.0;
        cell.set(cell.get() - 1);
    }
}

/// Restores the `no_struct_literal` restriction to its previous value on drop.
/// Holds the shared cell directly (not a `&mut Parser`) so it survives every
/// early `return`/`?` path inside a delimited-expression arm without borrowing
/// the parser for its whole lifetime.
struct NoStructLiteralGuard {
    cell: Rc<Cell<bool>>,
    prev: bool,
}

impl Drop for NoStructLiteralGuard {
    fn drop(&mut self) {
        self.cell.set(self.prev);
    }
}

/// Snapshot of parser position for speculative (backtracking) parses.
struct SavedPos {
    pos: usize,
    error_count: usize,
    angle_mutation_count: usize,
}

#[derive(Debug, Default)]
struct WireFieldModifiers {
    is_optional: bool,
    is_deprecated: bool,
    is_repeated: bool,
    json_name: Option<String>,
    yaml_name: Option<String>,
    since: Option<u32>,
}

#[derive(Debug)]
struct ParsedWireField {
    explicit_number: Option<u32>,
    field_number: Option<u32>,
    modifiers: WireFieldModifiers,
}

#[derive(Debug, Clone, Copy)]
enum WireFieldParseMode {
    Struct,
    Decl { next_auto_number: u32 },
}

#[derive(Debug, Default)]
struct WireNamingCases {
    json_case: Option<NamingCase>,
    yaml_case: Option<NamingCase>,
}

/// Parser state wrapping a token stream.
#[derive(Debug)]
pub struct Parser<'src> {
    tokens: Vec<(Token<'src>, Span)>,
    pos: usize,
    errors: Vec<ParseError>,
    depth: Rc<Cell<usize>>,
    /// Stack of token mutations performed by `eat_closing_angle`, so they can
    /// be rolled back on speculative-parse backtrack.
    angle_mutations: Vec<(usize, (Token<'src>, Span))>,
    /// True while parsing an impl-method parameter list that accepts bare
    /// `self` as sugar for a `Self` receiver parameter.
    allow_implicit_self_params: bool,
    /// Number of enclosing `scope { ... }` expression bodies being parsed.
    scope_expr_depth: usize,
    /// Number of enclosing `fork { ... }` child-task block bodies being parsed.
    fork_block_depth: usize,
    /// True while parsing an `if`/`while` condition or `match` scrutinee at the
    /// top level (outside any bracketing delimiter). In that position a bare
    /// identifier immediately followed by `{` must NOT be read as a struct
    /// literal — the `{` opens the then-block / loop body / match arms. Without
    /// this, `if flag { }` parses `flag { }` as an empty struct literal and the
    /// real block goes missing. The flag is cleared the moment we descend into a
    /// delimited sub-expression (`(...)`, `[...]`, call args, index, struct
    /// body) so a struct literal nested there — e.g. `if (Foo { a: 1 }).b {…}`
    /// — still parses.
    ///
    /// Held in an `Rc<Cell<bool>>` (like `depth`) so a `NoStructLiteralGuard`
    /// can restore the previous value on drop without borrowing the parser for
    /// its whole lifetime.
    no_struct_literal: Rc<Cell<bool>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
}

/// Structured discriminant for parse diagnostics.
///
/// Carries only the payload that lets a client act programmatically on the
/// error class without re-parsing the human-readable `message` field.
/// `Other` covers every error site that does not yet have a dedicated variant;
/// the `message` field on `ParseError` already holds the full text.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[serde(tag = "kind")]
pub enum ParseDiagnosticKind {
    /// A token was present but a different token was required.
    UnexpectedToken {
        /// What the parser required (e.g. `";"`, `"identifier"`).
        expected: String,
        /// What was actually found (e.g. `"}"`, `"end of file"`).
        got: String,
    },
    /// The input ended while more tokens were required.
    UnexpectedEof,
    /// A literal value (integer, float, character, byte, string interpolation) was malformed.
    InvalidLiteral,
    /// An expression was required but the current token cannot start one.
    MissingExpression {
        /// The token (or `"end of file"`) that was encountered.
        got: String,
    },
    /// A pattern was required but the current token cannot start one.
    InvalidPattern {
        /// The token (or `"end of file"`) that was encountered.
        got: String,
    },
    /// Pipe-closure syntax is malformed or incomplete.
    ClosurePipeSyntax,
    /// Every other error not yet assigned a structured variant.
    Other,
}

impl ParseDiagnosticKind {
    /// Returns a stable string identifier suitable for use in LSP `data` payloads.
    #[must_use]
    pub fn as_kind_str(&self) -> &'static str {
        match self {
            Self::UnexpectedToken { .. } => "UnexpectedToken",
            Self::UnexpectedEof => "UnexpectedEof",
            Self::InvalidLiteral => "InvalidLiteral",
            Self::MissingExpression { .. } => "MissingExpression",
            Self::InvalidPattern { .. } => "InvalidPattern",
            Self::ClosurePipeSyntax => "ClosurePipeSyntax",
            Self::Other => "Other",
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
    /// Optional actionable suggestion for fixing the error.
    pub hint: Option<String>,
    pub severity: Severity,
    /// Structured discriminant that clients can use without re-parsing `message`.
    pub kind: ParseDiagnosticKind,
}

#[derive(Debug)]
pub struct ParseResult {
    pub program: Program,
    pub errors: Vec<ParseError>,
}

impl<'src> Parser<'src> {
    #[must_use]
    pub fn new(source: &'src str) -> Self {
        let raw_tokens = hew_lexer::lex(source);
        let mut errors = Vec::new();
        let mut tokens = Vec::new();
        for (t, s) in raw_tokens {
            let span = s.start..s.end;
            if matches!(t, Token::Error) {
                errors.push(ParseError {
                    message: "unexpected character".to_string(),
                    span,
                    hint: None,
                    severity: Severity::Error,
                    kind: ParseDiagnosticKind::InvalidLiteral,
                });
            } else {
                tokens.push((t, span));
            }
        }
        Self {
            tokens,
            pos: 0,
            errors,
            depth: Rc::new(Cell::new(0)),
            angle_mutations: Vec::new(),
            allow_implicit_self_params: false,
            scope_expr_depth: 0,
            fork_block_depth: 0,
            no_struct_literal: Rc::new(Cell::new(false)),
        }
    }

    // ── Helpers ──
    fn peek(&self) -> Option<&Token<'src>> {
        self.tokens.get(self.pos).map(|(t, _)| t)
    }

    fn peek_span(&self) -> Span {
        self.tokens.get(self.pos).map_or(0..0, |(_, s)| s.clone())
    }

    fn advance(&mut self) -> Option<(Token<'src>, Span)> {
        if self.pos < self.tokens.len() {
            let tok = self.tokens[self.pos].clone();
            self.pos += 1;
            Some(tok)
        } else {
            None
        }
    }

    fn expect(&mut self, expected: &Token<'_>) -> Option<Span> {
        if let Some(tok) = self.peek() {
            if std::mem::discriminant(tok) == std::mem::discriminant(expected) {
                let Some((_, span)) = self.advance() else {
                    self.error_unexpected_eof(expected);
                    return None;
                };
                return Some(span);
            }
        }
        let found = match self.peek() {
            Some(tok) => format!("{tok}"),
            None => "end of file".to_string(),
        };
        // Add a hint when a semicolon is expected but a statement keyword follows
        if matches!(expected, Token::Semicolon) && self.peek_starts_stmt() {
            self.error_unexpected_token_with_hint(
                format!("{expected}"),
                &found,
                "add `;` at the end of the previous statement",
            );
        } else {
            self.error_unexpected_token(format!("{expected}"), &found);
        }
        None
    }

    fn eat(&mut self, expected: &Token<'_>) -> bool {
        if let Some(tok) = self.peek() {
            if std::mem::discriminant(tok) == std::mem::discriminant(expected) {
                self.advance();
                return true;
            }
        }
        false
    }

    /// Returns true if the current token could start a new statement.
    fn peek_starts_stmt(&self) -> bool {
        matches!(
            self.peek(),
            Some(
                Token::Let
                    | Token::Var
                    | Token::If
                    | Token::For
                    | Token::While
                    | Token::Loop
                    | Token::Return
                    | Token::Break
                    | Token::Continue
                    | Token::Defer
                    | Token::Spawn
                    | Token::Import
                    | Token::Fn
            )
        )
    }

    fn at_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    /// Peek at the token at an absolute position in the token stream.
    fn peek_at(&self, index: usize) -> Option<&Token<'src>> {
        self.tokens.get(index).map(|(t, _)| t)
    }

    /// Whether the current position is a contextual `clone <operand>` prefix.
    ///
    /// True only when the current token is the identifier `clone` AND the next
    /// token begins an operand (`token_begins_clone_operand`). When `clone` is
    /// followed by a continuation token (`.`, `(`, `[`, `?`, an infix operator,
    /// or a terminator) it stays an ordinary identifier — so `x.clone()`,
    /// `fn clone(...)`, and `clone(args)` are unaffected. The adjacency check
    /// is precedence-free: `clone x` was always a parse error before (two
    /// adjacent primaries), so repurposing it cannot change the meaning of any
    /// previously valid program.
    fn peek_is_clone_prefix(&self) -> bool {
        matches!(self.peek(), Some(Token::Identifier(name)) if *name == "clone")
            && self
                .peek_at(self.pos + 1)
                .is_some_and(token_begins_clone_operand)
    }

    /// Check whether the current token starts with `>` (i.e. is `>`, `>>`, `>=`, or `>>=`).
    /// Used in type-argument / type-parameter parsing so that `Vec<Vec<i32>>`
    /// works without requiring a space before `>>`.
    fn at_closing_angle(&self) -> bool {
        matches!(
            self.peek(),
            Some(
                Token::Greater
                    | Token::GreaterGreater
                    | Token::GreaterEqual
                    | Token::GreaterGreaterEqual
            )
        )
    }

    /// Consume a single `>` from the current token, splitting compound tokens
    /// (`>>`, `>=`, `>>=`) as needed.  Returns `true` if a `>` was consumed.
    fn eat_closing_angle(&mut self) -> bool {
        let Some((tok, span)) = self.tokens.get(self.pos) else {
            return false;
        };
        match tok {
            Token::Greater => {
                self.pos += 1;
                true
            }
            Token::GreaterGreater => {
                // `>>` → consume first `>`, leave `>` for the outer context
                self.angle_mutations
                    .push((self.pos, self.tokens[self.pos].clone()));
                let mid = span.start + 1;
                let remaining_span = mid..span.end;
                self.tokens[self.pos] = (Token::Greater, remaining_span);
                true
            }
            Token::GreaterEqual => {
                // `>=` → consume `>`, leave `=`
                self.angle_mutations
                    .push((self.pos, self.tokens[self.pos].clone()));
                let mid = span.start + 1;
                let remaining_span = mid..span.end;
                self.tokens[self.pos] = (Token::Equal, remaining_span);
                true
            }
            Token::GreaterGreaterEqual => {
                // `>>=` → consume first `>`, leave `>=`
                self.angle_mutations
                    .push((self.pos, self.tokens[self.pos].clone()));
                let mid = span.start + 1;
                let remaining_span = mid..span.end;
                self.tokens[self.pos] = (Token::GreaterEqual, remaining_span);
                true
            }
            _ => false,
        }
    }

    fn error(&mut self, message: String) {
        let span = self.peek_span();
        self.error_at(message, span);
    }

    fn error_at(&mut self, message: String, span: Span) {
        self.errors.push(ParseError {
            message,
            span,
            hint: None,
            severity: Severity::Error,
            kind: ParseDiagnosticKind::Other,
        });
    }

    fn error_with_hint(&mut self, message: String, hint: impl Into<String>) {
        let span = self.peek_span();
        self.error_at_with_hint(message, span, hint);
    }

    fn error_at_with_hint(&mut self, message: String, span: Span, hint: impl Into<String>) {
        self.errors.push(ParseError {
            message,
            span,
            hint: Some(hint.into()),
            severity: Severity::Error,
            kind: ParseDiagnosticKind::Other,
        });
    }

    fn error_closure_pipe_syntax(
        &mut self,
        message: impl Into<String>,
        span: Span,
        hint: impl Into<String>,
    ) {
        self.errors.push(ParseError {
            message: message.into(),
            span,
            hint: Some(hint.into()),
            severity: Severity::Error,
            kind: ParseDiagnosticKind::ClosurePipeSyntax,
        });
    }

    /// Record an `UnexpectedToken` diagnostic at the current position.
    fn error_unexpected_token(&mut self, expected: impl Into<String>, got: impl Into<String>) {
        let span = self.peek_span();
        self.error_unexpected_token_at(expected, got, span);
    }

    fn error_unexpected_token_at(
        &mut self,
        expected: impl Into<String>,
        got: impl Into<String>,
        span: Span,
    ) {
        let expected = expected.into();
        let got = got.into();
        let message = format!("expected {expected}, found {got}");
        self.errors.push(ParseError {
            message,
            span,
            hint: None,
            severity: Severity::Error,
            kind: ParseDiagnosticKind::UnexpectedToken { expected, got },
        });
    }

    fn error_unexpected_token_with_hint(
        &mut self,
        expected: impl Into<String>,
        got: impl Into<String>,
        hint: impl Into<String>,
    ) {
        let span = self.peek_span();
        let expected = expected.into();
        let got = got.into();
        let message = format!("expected {expected}, found {got}");
        self.errors.push(ParseError {
            message,
            span,
            hint: Some(hint.into()),
            severity: Severity::Error,
            kind: ParseDiagnosticKind::UnexpectedToken { expected, got },
        });
    }

    fn error_unexpected_eof(&mut self, context: impl std::fmt::Display) {
        let span = self.peek_span();
        self.errors.push(ParseError {
            message: format!("unexpected end of input, expected {context}"),
            span,
            hint: None,
            severity: Severity::Error,
            kind: ParseDiagnosticKind::UnexpectedEof,
        });
    }

    fn error_invalid_literal(&mut self, message: String) {
        let span = self.peek_span();
        self.errors.push(ParseError {
            message,
            span,
            hint: None,
            severity: Severity::Error,
            kind: ParseDiagnosticKind::InvalidLiteral,
        });
    }

    fn error_missing_expression(&mut self, got: impl Into<String>) {
        let got = got.into();
        let span = self.peek_span();
        self.errors.push(ParseError {
            message: format!("expected expression, found {got}"),
            span,
            hint: None,
            severity: Severity::Error,
            kind: ParseDiagnosticKind::MissingExpression { got },
        });
    }

    fn error_invalid_pattern(&mut self, got: impl Into<String>) {
        let got = got.into();
        let span = self.peek_span();
        self.errors.push(ParseError {
            message: format!("expected pattern, found {got}"),
            span,
            hint: None,
            severity: Severity::Error,
            kind: ParseDiagnosticKind::InvalidPattern { got },
        });
    }

    fn error_invalid_literal_with_hint(&mut self, message: String, hint: impl Into<String>) {
        let span = self.peek_span();
        self.errors.push(ParseError {
            message,
            span,
            hint: Some(hint.into()),
            severity: Severity::Error,
            kind: ParseDiagnosticKind::InvalidLiteral,
        });
    }

    fn parse_char_escape(&mut self, s: &str) -> Option<char> {
        let mut chars = s.chars();
        let Some(c) = chars.next() else {
            self.error_invalid_literal("invalid char literal".to_string());
            return None;
        };
        let result = if c == '\\' {
            let Some(escaped) = chars.next() else {
                self.error_invalid_literal("invalid escape sequence".to_string());
                return None;
            };
            match escaped {
                'n' => '\n',
                't' => '\t',
                'r' => '\r',
                '0' => '\0',
                '\\' => '\\',
                '\'' => '\'',
                _ => {
                    self.error_invalid_literal("invalid escape sequence".to_string());
                    return None;
                }
            }
        } else {
            c
        };
        if chars.next().is_some() {
            self.error_invalid_literal("invalid char literal".to_string());
            return None;
        }
        Some(result)
    }

    fn warning_at(&mut self, message: String, span: Span) {
        self.errors.push(ParseError {
            message,
            span,
            hint: None,
            severity: Severity::Warning,
            kind: ParseDiagnosticKind::Other,
        });
    }

    /// Increment recursion depth and return a guard that decrements on drop.
    /// Returns `None` (after recording an error) if `MAX_DEPTH` is exceeded.
    fn enter_recursion(&mut self) -> Option<RecursionGuard> {
        let d = self.depth.get() + 1;
        self.depth.set(d);
        if d > MAX_DEPTH {
            self.error("maximum nesting depth exceeded".to_string());
            self.depth.set(d - 1);
            return None;
        }
        Some(RecursionGuard(Rc::clone(&self.depth)))
    }

    /// If the token is a contextual keyword, return its identifier name.
    fn contextual_keyword_name(tok: &Token<'_>) -> Option<&'static str> {
        match tok {
            Token::After => Some("after"),
            Token::From => Some("from"),
            Token::Init => Some("init"),
            Token::Child => Some("child"),
            Token::Restart => Some("restart"),
            Token::Budget => Some("budget"),
            Token::Strategy => Some("strategy"),
            Token::Permanent => Some("permanent"),
            Token::Transient => Some("transient"),
            Token::Temporary => Some("temporary"),
            Token::OneForOne => Some("one_for_one"),
            Token::OneForAll => Some("one_for_all"),
            Token::RestForOne => Some("rest_for_one"),
            Token::Wire => Some("wire"),
            Token::Optional => Some("optional"),
            Token::Deprecated => Some("deprecated"),
            Token::Reserved => Some("reserved"),
            Token::State => Some("state"),
            Token::Event => Some("event"),
            Token::On => Some("on"),
            Token::When => Some("when"),
            Token::Join => Some("join"),
            // Machine-block keywords that can also appear as external function names
            // or identifiers in other positions.
            Token::Entry => Some("entry"),
            Token::Exit => Some("exit"),
            Token::Emit => Some("emit"),
            _ => None,
        }
    }

    fn looks_like_scope_deadline(&self) -> bool {
        if !matches!(self.peek(), Some(Token::After)) {
            return false;
        }
        if !matches!(
            self.tokens.get(self.pos + 1).map(|(token, _)| token),
            Some(Token::LeftParen)
        ) {
            return false;
        }

        let mut paren_depth = 0usize;
        for idx in (self.pos + 1)..self.tokens.len() {
            match &self.tokens[idx].0 {
                Token::LeftParen => paren_depth += 1,
                Token::RightParen => {
                    paren_depth = paren_depth.saturating_sub(1);
                    if paren_depth == 0 {
                        return matches!(
                            self.tokens.get(idx + 1).map(|(token, _)| token),
                            Some(Token::LeftBrace)
                        );
                    }
                }
                _ => {}
            }
        }
        false
    }

    /// Returns true if the token can be used as an identifier (regular or contextual keyword).
    fn is_ident_token(tok: &Token<'_>) -> bool {
        matches!(tok, Token::Identifier(_)) || Self::contextual_keyword_name(tok).is_some()
    }

    /// Returns true when the current token is a bare `Identifier` matching `kw`.
    ///
    /// Used for machine-body contextual keywords (`events`, `emits`, `reenter`,
    /// `initial`) that are NOT lexer keywords — they tokenize as ordinary
    /// identifiers and only carry keyword meaning inside the machine body, so
    /// they cost nothing in the global identifier namespace.
    fn peek_machine_kw(&self, kw: &str) -> bool {
        matches!(self.peek(), Some(Token::Identifier(name)) if *name == kw)
    }

    /// Consumes the current token iff it is a bare `Identifier` matching `kw`.
    /// See `peek_machine_kw` for the contextual-keyword rationale.
    fn eat_machine_kw(&mut self, kw: &str) -> bool {
        if self.peek_machine_kw(kw) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect_ident(&mut self) -> Option<String> {
        match self.peek() {
            Some(Token::Identifier(name)) => {
                let name = name.to_string();
                self.advance();
                Some(name)
            }
            Some(tok) => {
                if let Some(name) = Self::contextual_keyword_name(tok) {
                    self.advance();
                    Some(name.to_string())
                } else if let Some(kw) = tok.keyword_str() {
                    // Reserved keyword in a name position — emit a targeted
                    // diagnostic so the user knows the word is off-limits.
                    self.error_at_with_hint(
                        format!("`{kw}` is a reserved word and cannot be used as a name"),
                        self.peek_span(),
                        format!("rename this item to something other than `{kw}`"),
                    );
                    None
                } else {
                    self.error(format!("expected identifier, found {tok}"));
                    None
                }
            }
            None => {
                self.error("expected identifier, found end of file".to_string());
                None
            }
        }
    }

    fn is_import_path_segment_token(tok: &Token<'_>) -> bool {
        matches!(tok, Token::Identifier(_) | Token::Actor)
            || Self::contextual_keyword_name(tok).is_some()
    }

    fn expect_import_path_segment(&mut self) -> Option<String> {
        match self.peek() {
            Some(Token::Actor) => {
                self.advance();
                Some("actor".to_string())
            }
            Some(tok) if Self::contextual_keyword_name(tok).is_some() => {
                let name = Self::contextual_keyword_name(tok).expect("checked above");
                self.advance();
                Some(name.to_string())
            }
            _ => self.expect_ident(),
        }
    }

    /// Skip tokens until the next actor-body item boundary (`receive`, `fn`,
    /// `let`, `var`, or the actor's closing `}`) or end-of-file.  Nested
    /// brace groups (`{…}`) are consumed wholesale so we stop only at the
    /// actor-level boundary, not inside a method body.
    ///
    /// Used for error recovery after a bad item name inside an actor body so
    /// a single bad declaration does not cascade into dozens of errors.
    fn skip_to_actor_item_boundary(&mut self) {
        let mut depth: usize = 0;
        while !self.at_end() {
            match self.peek() {
                Some(Token::LeftBrace) => {
                    depth += 1;
                    self.advance();
                }
                Some(Token::RightBrace) => {
                    if depth == 0 {
                        // This is the actor-body closing brace — stop here.
                        break;
                    }
                    depth -= 1;
                    self.advance();
                }
                Some(Token::Receive | Token::Fn | Token::Let | Token::Var) if depth == 0 => {
                    break;
                }
                _ => {
                    self.advance();
                }
            }
        }
    }

    fn is_match_arm_pattern_start(&self) -> bool {
        match self.peek() {
            Some(Token::Minus) => matches!(
                self.peek_at(self.pos + 1),
                Some(Token::Integer(_) | Token::Float(_))
            ),
            Some(Token::Dot) => matches!(self.peek_at(self.pos + 1), Some(Token::Identifier(_))),
            Some(
                Token::Identifier(_)
                | Token::LeftParen
                | Token::Integer(_)
                | Token::StringLit(_)
                | Token::CharLit(_)
                | Token::RawString(_)
                | Token::RegexLiteral(_)
                | Token::True
                | Token::False,
            ) => true,
            Some(tok) => Self::contextual_keyword_name(tok).is_some(),
            None => false,
        }
    }

    /// Recover after a malformed match arm by consuming the bad arm, then
    /// stopping before the closing `}` or the next arm's pattern.
    fn skip_to_match_arm_boundary(&mut self) {
        let mut depth: usize = 0;
        let mut closed_top_level_block = false;
        while !self.at_end() {
            match self.peek() {
                Some(Token::RightBrace) if depth == 0 => break,
                Some(Token::Comma) if depth == 0 => {
                    self.advance();
                    break;
                }
                Some(Token::LeftBrace) => {
                    depth += 1;
                    closed_top_level_block = false;
                    self.advance();
                }
                Some(Token::RightBrace) => {
                    depth -= 1;
                    self.advance();
                    closed_top_level_block = depth == 0;
                }
                Some(_)
                    if depth == 0
                        && closed_top_level_block
                        && self.is_match_arm_pattern_start() =>
                {
                    break;
                }
                _ => {
                    closed_top_level_block = false;
                    self.advance();
                }
            }
        }
    }

    /// Returns true when the token can start a top-level item.
    fn is_top_level_item_start(tok: &Token<'_>) -> bool {
        matches!(
            tok,
            Token::Fn
                | Token::Pub
                | Token::Actor
                | Token::Type
                | Token::Trait
                | Token::Impl
                | Token::Import
                | Token::Record
                | Token::Enum
                | Token::Machine
                | Token::Supervisor
                | Token::Const
                | Token::Wire
                | Token::Indirect
                | Token::Async
                | Token::Gen
                | Token::Extern
                | Token::HashBracket
                | Token::DocComment(_)
        )
    }

    /// Skip tokens until the next top-level item start or end-of-file.
    /// Always advances at least one token, then skips nested brace groups
    /// wholesale.  Used for error recovery in `parse_program` so a single
    /// bad declaration does not cascade into one error per remaining token.
    fn skip_to_top_level_item_boundary(&mut self) {
        // Consume at least the current (failed) token so we always make
        // forward progress even if it looks like an item-start keyword.
        if !self.at_end() {
            self.advance();
        }
        let mut depth: usize = 0;
        while !self.at_end() {
            match self.peek() {
                Some(Token::LeftBrace) => {
                    depth += 1;
                    self.advance();
                }
                Some(Token::RightBrace) => {
                    if depth == 0 {
                        self.advance(); // consume stray `}` and stop
                        break;
                    }
                    depth -= 1;
                    self.advance();
                }
                Some(tok) if depth == 0 && Self::is_top_level_item_start(tok) => {
                    break;
                }
                _ => {
                    self.advance();
                }
            }
        }
    }

    fn save_pos(&self) -> SavedPos {
        SavedPos {
            pos: self.pos,
            error_count: self.errors.len(),
            angle_mutation_count: self.angle_mutations.len(),
        }
    }

    #[expect(
        clippy::needless_pass_by_value,
        reason = "SavedPos is consumed to restore parser state"
    )]
    fn restore_pos(&mut self, saved: SavedPos) {
        self.pos = saved.pos;
        self.errors.truncate(saved.error_count);
        // Undo any token mutations made by eat_closing_angle since this save point
        while self.angle_mutations.len() > saved.angle_mutation_count {
            let (idx, tok) = self.angle_mutations.pop().unwrap();
            self.tokens[idx] = tok;
        }
    }

    fn parse_wire_since_modifier(&mut self) -> Option<u32> {
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

    fn parse_wire_field_modifiers(&mut self) -> WireFieldModifiers {
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
                    let ident = self.expect_ident().unwrap_or_default();
                    match ident.as_str() {
                        "repeated" => {
                            modifiers.is_repeated = true;
                        }
                        "since" => {
                            modifiers.since = self.parse_wire_since_modifier();
                        }
                        "json" if self.eat(&Token::LeftParen) => {
                            if let Some(Token::StringLit(s) | Token::RawString(s)) = self.peek() {
                                modifiers.json_name = Some(unquote_str(s).to_string());
                                self.advance();
                            }
                            let _ = self.expect(&Token::RightParen);
                        }
                        "yaml" if self.eat(&Token::LeftParen) => {
                            if let Some(Token::StringLit(s) | Token::RawString(s)) = self.peek() {
                                modifiers.yaml_name = Some(unquote_str(s).to_string());
                                self.advance();
                            }
                            let _ = self.expect(&Token::RightParen);
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

    fn parse_wire_field_number_after_marker(
        &mut self,
        mode: WireFieldParseMode,
    ) -> Result<u32, ()> {
        let Some(Token::Integer(num_str)) = self.peek() else {
            match mode {
                WireFieldParseMode::Struct => {
                    self.error("expected field number after '@'".to_string());
                }
                WireFieldParseMode::Decl { .. } => {
                    self.error("expected integer for wire field number".to_string());
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
                WireFieldParseMode::Decl { .. } => {
                    self.error(format!("invalid wire field number: {raw}"));
                }
            })
    }

    fn parse_wire_field_number_and_modifiers(
        &mut self,
        mode: WireFieldParseMode,
    ) -> Option<ParsedWireField> {
        let (explicit_number, field_number) = match mode {
            WireFieldParseMode::Struct => {
                if self.eat(&Token::At) {
                    let explicit_number = self.parse_wire_field_number_after_marker(mode).ok();
                    (explicit_number, explicit_number)
                } else {
                    (None, None)
                }
            }
            WireFieldParseMode::Decl { next_auto_number } => {
                if self.eat(&Token::Equal) || self.eat(&Token::At) {
                    let field_number = self.parse_wire_field_number_after_marker(mode).ok()?;
                    (Some(field_number), Some(field_number))
                } else {
                    (None, Some(next_auto_number))
                }
            }
        };

        let modifiers = self.parse_wire_field_modifiers();
        Some(ParsedWireField {
            explicit_number,
            field_number,
            modifiers,
        })
    }

    fn extract_wire_naming_cases(attrs: &[Attribute]) -> WireNamingCases {
        let parse_case = |attr_name| {
            attrs
                .iter()
                .find(|attr| attr.name == attr_name)
                .and_then(|attr| {
                    attr.args
                        .first()
                        .and_then(|arg| NamingCase::from_attr(arg.as_str()))
                })
        };

        WireNamingCases {
            json_case: parse_case("json"),
            yaml_case: parse_case("yaml"),
        }
    }

    /// Collect consecutive doc comment tokens with the given prefix and return
    /// the concatenated content, or `None` if no matching comments are present.
    fn collect_doc_comments_with_prefix(
        &mut self,
        prefix: &str,
        is_match: fn(&Token<'src>) -> Option<&'src str>,
    ) -> Option<String> {
        let mut lines = Vec::new();
        while let Some(s) = self.peek().and_then(is_match) {
            let content = s.strip_prefix(prefix).unwrap_or(s);
            // Strip one leading space if present (conventional formatting)
            let content = content.strip_prefix(' ').unwrap_or(content);
            lines.push(content.to_string());
            self.advance();
        }
        if lines.is_empty() {
            None
        } else {
            Some(lines.join("\n"))
        }
    }

    /// Collect consecutive outer doc comment (`///`) tokens and return
    /// the concatenated content, or `None` if no doc comments are present.
    fn collect_doc_comments(&mut self) -> Option<String> {
        self.collect_doc_comments_with_prefix("///", |t| match t {
            Token::DocComment(s) => Some(s),
            _ => None,
        })
    }

    /// Collect consecutive inner doc comment (`//!`) tokens at the start of
    /// the file and return the concatenated content.
    fn collect_inner_doc_comments(&mut self) -> Option<String> {
        self.collect_doc_comments_with_prefix("//!", |t| match t {
            Token::InnerDocComment(s) => Some(s),
            _ => None,
        })
    }

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
    fn parse_attributes(&mut self) -> Vec<Attribute> {
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
    /// Consumes `pub`, `pub(package)`, or `pub(super)` and returns the
    /// corresponding [`Visibility`] variant. Must be called when the current
    /// token is `Token::Pub`.
    fn parse_visibility(&mut self) -> Visibility {
        assert!(self.eat(&Token::Pub));
        if self.eat(&Token::LeftParen) {
            let vis = match self.peek() {
                Some(Token::Package) => {
                    self.advance();
                    Visibility::PubPackage
                }
                Some(Token::Super) => {
                    self.advance();
                    Visibility::PubSuper
                }
                _ => {
                    self.error("expected 'package' or 'super' after 'pub('".to_string());
                    return Visibility::Private;
                }
            };
            if self.expect(&Token::RightParen).is_none() {
                return Visibility::Private;
            }
            vis
        } else {
            Visibility::Pub
        }
    }

    /// Parse a function declaration with optional `async`/`gen` modifiers.
    /// The current token must be `fn`, `async`, or `gen`.
    #[expect(clippy::ref_option, reason = "avoids cloning option contents")]
    fn parse_fn_with_modifiers(
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
    fn parse_item(&mut self) -> Option<Spanned<Item>> {
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
            Some(Token::Pub) => {
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
                    Some(Token::Wire) => {
                        self.advance();
                        let wd = self.parse_wire_decl(&attrs, vis)?;
                        if wd.kind == WireDeclKind::Struct {
                            Item::TypeDecl(wd.into_type_decl())
                        } else {
                            Item::Wire(wd)
                        }
                    }
                    Some(Token::Const) => {
                        self.advance();
                        Item::Const(self.parse_const_decl(vis, doc_comment)?)
                    }
                    _ => {
                        self.error("invalid item after 'pub'".to_string());
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
            Some(Token::Wire) => {
                self.advance();
                let wd = self.parse_wire_decl(&attrs, Visibility::Private)?;
                if wd.kind == WireDeclKind::Struct {
                    Item::TypeDecl(wd.into_type_decl())
                } else {
                    Item::Wire(wd)
                }
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

    fn parse_function(
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
        let params = self.parse_params_with_implicit_self(self.allow_implicit_self_params);
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
        })
    }

    /// Parse a method inside a type body, returning `(FnDecl, has_consuming_self)`.
    ///
    /// Unlike `parse_function`, this variant accepts `consuming self` as the first
    /// parameter. The boolean return indicates whether the method declared such a
    /// receiver; callers record this in `TypeDecl.consuming_methods`.
    fn parse_type_method(
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
        };
        Some((decl, has_consuming_self))
    }

    fn is_type_alias_lookahead(&self) -> bool {
        // Check for "type Name =" pattern (Name can be a contextual keyword)
        matches!(self.tokens.get(self.pos), Some((Token::Type, _)))
            && self
                .tokens
                .get(self.pos + 1)
                .is_some_and(|(tok, _)| Self::is_ident_token(tok))
            && matches!(self.tokens.get(self.pos + 2), Some((Token::Equal, _)))
    }

    fn parse_type_alias(
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

    fn parse_struct_or_enum(
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
    fn parse_record_decl(&mut self, visibility: Visibility) -> Option<RecordDecl> {
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
    fn extract_resource_marker(&mut self, attrs: &[Attribute]) -> ResourceMarker {
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

    fn parse_indirect_enum(&mut self, visibility: Visibility) -> Option<TypeDecl> {
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
    fn parse_type_body_item(&mut self, kind: TypeDeclKind) -> Option<(TypeBodyItem, bool)> {
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

    fn parse_trait_decl(
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

    fn parse_trait_item(&mut self) -> Option<TraitItem> {
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

    fn parse_impl_decl(&mut self) -> Option<ImplDecl> {
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
            let vis = if self.peek() == Some(&Token::Pub) {
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
    fn peek_is_field_decl(&mut self) -> bool {
        let saved = self.save_pos();
        let result = if self.expect_ident().is_some() {
            self.peek() == Some(&Token::Colon)
        } else {
            false
        };
        self.restore_pos(saved);
        result
    }

    #[expect(
        clippy::too_many_lines,
        reason = "actor decl parsing has many fields and sections"
    )]
    fn parse_actor_decl(&mut self, visibility: Visibility) -> Option<ActorDecl> {
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

    fn parse_overflow_policy(&mut self) -> Option<OverflowPolicy> {
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

    fn parse_machine_decl(&mut self, visibility: Visibility) -> Option<MachineDecl> {
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
    fn splice_all_composite_hooks(
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
    fn parse_machine_transition(&mut self) -> Option<MachineTransition> {
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
    fn parse_machine_event_fields(&mut self) -> Option<Vec<(String, Spanned<TypeExpr>)>> {
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
    fn parse_machine_state_or_composite(
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
    fn parse_composite_block(
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
    fn parse_machine_substate(&mut self, composite_name: &str) -> Option<MachineState> {
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
    fn splice_composite_hooks(
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
    fn prepend_block_stmts(transition: &mut MachineTransition, hook: &Block) {
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
    fn apply_event_head_bindings(bindings: &[String], body: Expr) -> Expr {
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
    fn parse_state_pattern(&mut self) -> Option<String> {
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
    fn is_struct_init_body(&self) -> bool {
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
    fn parse_supervisor_decl(&mut self, visibility: Visibility) -> Option<SupervisorDecl> {
        let name = self.expect_ident()?;

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
            strategy,
            intensity,
            children,
        })
    }

    fn parse_extern_block(&mut self) -> Option<ExternBlock> {
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

    /// Parse `#[wire] struct Name { field: Type, ... }` into a `TypeDecl` with wire metadata.
    #[expect(
        clippy::too_many_lines,
        reason = "expression parsing handles all expression types"
    )]
    fn parse_wire_struct(
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
        self.expect(&Token::Struct)?;
        let name = self.expect_ident()?;
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
                self.eat(&Token::Semicolon);
                continue;
            }

            // Parse field: name: Type [@N] [modifiers] [,|;]
            let field_name = self.expect_ident()?;
            self.expect(&Token::Colon)?;
            let ty = self.parse_type()?;
            let parsed_field =
                self.parse_wire_field_number_and_modifiers(WireFieldParseMode::Struct)?;
            if let Some(explicit_num) = parsed_field.explicit_number {
                if reserved_numbers.contains(&explicit_num) {
                    self.error(format!("wire field number @{explicit_num} is reserved"));
                }
                if !seen_explicit_numbers.insert(explicit_num) {
                    self.error(format!("duplicate wire field number @{explicit_num}"));
                }
                explicit_numbers.push(explicit_num);
            }

            fields.push(TypeBodyItem::Field {
                name: field_name.clone(),
                ty,
                attributes: Vec::new(),
                doc_comment: None,
                span: 0..0,
            });
            field_meta.push((
                field_name,
                parsed_field.explicit_number,
                parsed_field.modifiers.is_optional,
                parsed_field.modifiers.is_deprecated,
                parsed_field.modifiers.is_repeated,
                parsed_field.modifiers.json_name,
                parsed_field.modifiers.yaml_name,
                parsed_field.modifiers.since,
            ));

            // Accept comma or semicolon as separator
            if !self.eat(&Token::Comma) {
                self.eat(&Token::Semicolon);
            }
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

        for (
            field_name,
            explicit_num,
            is_optional,
            is_deprecated,
            is_repeated,
            json_name,
            yaml_name,
            since,
        ) in field_meta
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
                json_name,
                yaml_name,
                since,
            });
        }

        let WireNamingCases {
            json_case,
            yaml_case,
        } = Self::extract_wire_naming_cases(attrs);

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
                json_case,
                yaml_case,
                version,
                min_version,
            }),
            is_indirect: false,
            resource_marker: ResourceMarker::None,
            is_opaque: false,
            consuming_methods: Vec::new(),
        })
    }

    /// Parse `#[wire] enum Name { Variant1, Variant2(T), Variant3 { f: U } }` into a
    /// `TypeDecl` with wire metadata attached.  Enums carry the type-level wire
    /// metadata (`version`, `min_version`, `json_case`, `yaml_case`) but no
    /// per-field tag numbers — variant payloads are tagged by the variant
    /// index, not by `@N` annotations on individual fields.
    ///
    /// The enum body itself is parsed by the shared `parse_struct_or_enum`
    /// helper (which handles unit / tuple / struct variant payloads, type
    /// parameters, and where clauses).  This function attaches the wire
    /// metadata to the resulting `TypeDecl` and validates the marker-conflict
    /// rules that also apply to `#[wire] struct`.
    fn parse_wire_enum(&mut self, attrs: &[Attribute], visibility: Visibility) -> Option<TypeDecl> {
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

        let WireNamingCases {
            json_case,
            yaml_case,
        } = Self::extract_wire_naming_cases(attrs);

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
            json_case,
            yaml_case,
            version,
            min_version,
        });
        Some(td)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "wire decl parsing has many fields and variants"
    )]
    fn parse_wire_decl(&mut self, attrs: &[Attribute], visibility: Visibility) -> Option<WireDecl> {
        let kind = match self.peek() {
            Some(Token::Type) => {
                self.advance();
                WireDeclKind::Struct
            }
            Some(Token::Enum) => {
                self.advance();
                WireDeclKind::Enum
            }
            _ => return None,
        };

        let name = self.expect_ident()?;

        self.expect(&Token::LeftBrace)?;

        let mut fields = Vec::new();
        let mut variants = Vec::new();

        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            match kind {
                WireDeclKind::Struct => {
                    // Handle reserved(...) directive
                    if self.peek() == Some(&Token::Reserved) {
                        self.advance();
                        if self.eat(&Token::LeftParen) {
                            while !self.at_end() && self.peek() != Some(&Token::RightParen) {
                                self.advance();
                                self.eat(&Token::Comma);
                            }
                            self.expect(&Token::RightParen)?;
                        }
                        if !self.eat(&Token::Semicolon) {
                            self.eat(&Token::Comma);
                        }
                        continue;
                    }

                    // Parse wire field
                    let field_name = self.expect_ident()?;
                    self.expect(&Token::Colon)?;
                    let ty = self.expect_ident()?;

                    #[expect(
                        clippy::cast_possible_truncation,
                        reason = "wire field numbers won't exceed u32::MAX"
                    )]
                    let next_auto_number = fields.len() as u32 + 1;
                    let parsed_field =
                        self.parse_wire_field_number_and_modifiers(WireFieldParseMode::Decl {
                            next_auto_number,
                        })?;
                    let field_number = parsed_field.field_number?;

                    fields.push(WireFieldDecl {
                        name: field_name,
                        ty,
                        field_number,
                        is_optional: parsed_field.modifiers.is_optional,
                        is_repeated: parsed_field.modifiers.is_repeated,
                        is_reserved: false,
                        is_deprecated: parsed_field.modifiers.is_deprecated,
                        json_name: parsed_field.modifiers.json_name,
                        yaml_name: parsed_field.modifiers.yaml_name,
                        since: parsed_field.modifiers.since,
                    });

                    if !self.eat(&Token::Semicolon) {
                        self.eat(&Token::Comma);
                    }
                }
                WireDeclKind::Enum => {
                    // Parse enum variant
                    let variant_name = self.expect_ident()?;
                    let kind = if self.eat(&Token::LeftParen) {
                        let mut variant_fields = Vec::new();
                        while !self.at_end() && self.peek() != Some(&Token::RightParen) {
                            variant_fields.push(self.parse_type()?);
                            if !self.eat(&Token::Comma) {
                                break;
                            }
                        }
                        self.expect(&Token::RightParen)?;
                        VariantKind::Tuple(variant_fields)
                    } else if self.eat(&Token::LeftBrace) {
                        let mut variant_fields = Vec::new();
                        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
                            let field_name = self.expect_ident()?;
                            self.expect(&Token::Colon)?;
                            let ty = self.parse_type()?;
                            variant_fields.push((field_name, ty));
                            if !(self.eat(&Token::Comma) || self.eat(&Token::Semicolon)) {
                                break;
                            }
                        }
                        self.expect(&Token::RightBrace)?;
                        VariantKind::Struct(variant_fields)
                    } else {
                        VariantKind::Unit
                    };

                    if !self.eat(&Token::Comma) {
                        self.eat(&Token::Semicolon);
                    }
                    variants.push(VariantDecl {
                        name: variant_name,
                        kind,
                        doc_comment: None,
                        span: 0..0,
                    });
                }
            }
        }

        self.expect(&Token::RightBrace)?;

        let WireNamingCases {
            json_case,
            yaml_case,
        } = Self::extract_wire_naming_cases(attrs);

        Some(WireDecl {
            visibility,
            kind,
            name,
            fields,
            variants,
            json_case,
            yaml_case,
        })
    }

    fn parse_import(&mut self) -> Option<ImportDecl> {
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

    fn parse_const_decl(
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

    // ── Types ──
    #[expect(
        clippy::too_many_lines,
        reason = "recursive descent parser requires sequential case handling"
    )]
    fn parse_type(&mut self) -> Option<Spanned<TypeExpr>> {
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

    fn parse_type_params(&mut self) -> Option<Vec<TypeParam>> {
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
    fn parse_machine_generic_params(&mut self) -> Option<(Vec<TypeParam>, Vec<ConstParam>)> {
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
    fn parse_opt_type_params(&mut self) -> Option<Option<Vec<TypeParam>>> {
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
    fn parse_opt_return_type(&mut self) -> Option<Option<Spanned<TypeExpr>>> {
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
    fn parse_opt_where_clause(&mut self) -> Option<Option<WhereClause>> {
        if self.peek() == Some(&Token::Where) {
            self.advance();
            Some(Some(self.parse_where_clause()?))
        } else {
            Some(None)
        }
    }

    fn parse_type_args(&mut self) -> Option<Vec<Spanned<TypeExpr>>> {
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

    fn parse_trait_bound_args(&mut self) -> Option<ParsedTraitBoundArgs> {
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

    fn parse_trait_bound(&mut self) -> Option<TraitBound> {
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
    fn parse_optional_super_traits(&mut self) -> Option<Option<Vec<TraitBound>>> {
        if self.eat(&Token::Colon) {
            Some(Some(self.parse_trait_bound_list()?))
        } else {
            Some(None)
        }
    }

    fn parse_trait_bound_list(&mut self) -> Option<Vec<TraitBound>> {
        let mut bounds = Vec::new();
        loop {
            bounds.push(self.parse_trait_bound()?);
            if !self.eat(&Token::Plus) {
                break;
            }
        }
        Some(bounds)
    }

    fn parse_where_clause(&mut self) -> Option<WhereClause> {
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

    fn parse_params(&mut self) -> Vec<Param> {
        self.parse_params_with_implicit_self(false)
    }

    fn parse_params_with_implicit_self(&mut self, allow_implicit_self: bool) -> Vec<Param> {
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
    fn parse_params_with_receiver(&mut self, allow_consuming_self: bool) -> (Vec<Param>, bool) {
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
    fn is_block_expr(expr: &Expr) -> bool {
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

    fn fork_starts_child_binding(&self) -> bool {
        self.peek().is_some_and(Self::is_ident_token)
            && self.peek_at(self.pos + 1) == Some(&Token::Equal)
    }

    // ── Statements ──
    fn parse_block(&mut self) -> Option<Block> {
        self.expect(&Token::LeftBrace)?;

        let mut stmts = Vec::new();
        let mut trailing_expr = None;

        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            // Try to parse as statement first.
            if let Some(stmt) = self.parse_stmt() {
                // If this is the last item in the block (`}` follows immediately, no
                // semicolon consumed), and the statement is a value-bearing form (`if`,
                // `if let`, `match`), promote it to the block's trailing expression.
                // These forms produce a value — the HIR/MIR pipeline reads
                // `block.trailing_expr` as the return value of the block; a
                // `Stmt::If`/`Stmt::Match` leaves the return slot uninitialised.
                let at_tail = self.peek() == Some(&Token::RightBrace);
                let is_value_bearing = matches!(
                    stmt.0,
                    Stmt::If { .. } | Stmt::IfLet { .. } | Stmt::Match { .. }
                );
                if at_tail && is_value_bearing {
                    // SAFETY: `is_value_bearing` ensures promote_stmt_to_trailing_expr
                    // will always return Some for these exact variants.
                    let expr = Self::promote_stmt_to_trailing_expr(stmt)
                        .expect("promote_stmt_to_trailing_expr must succeed for If/IfLet/Match");
                    trailing_expr = Some(Box::new(expr));
                    break;
                }
                stmts.push(stmt);
                while self.peek() == Some(&Token::Semicolon) {
                    let span = self.peek_span();
                    self.advance();
                    self.warning_at("unnecessary semicolon".to_string(), span);
                }
                continue;
            }

            // Try as expression
            if let Some(expr) = self.parse_expr() {
                // Check for assignment
                if let Some(op) = self.parse_compound_assign_op() {
                    let value = self.parse_expr()?;
                    self.expect(&Token::Semicolon)?;
                    let span = expr.1.start..value.1.end;
                    stmts.push((
                        Stmt::Assign {
                            target: expr,
                            op: Some(op),
                            value,
                        },
                        span,
                    ));
                } else if self.eat(&Token::Equal) {
                    let value = self.parse_expr()?;
                    self.expect(&Token::Semicolon)?;
                    let span = expr.1.start..value.1.end;
                    stmts.push((
                        Stmt::Assign {
                            target: expr,
                            op: None,
                            value,
                        },
                        span,
                    ));
                } else if self.eat(&Token::Semicolon) {
                    // Expression statement
                    while self.peek() == Some(&Token::Semicolon) {
                        let semi_span = self.peek_span();
                        self.advance();
                        self.warning_at("unnecessary semicolon".to_string(), semi_span);
                    }
                    let span = expr.1.clone();
                    stmts.push((Stmt::Expression(expr), span));
                } else if Self::is_block_expr(&expr.0)
                    && (self.peek() != Some(&Token::RightBrace)
                        || matches!(expr.0, Expr::ForkBlock { .. } | Expr::ScopeDeadline { .. }))
                {
                    // Block-like expressions (if, match, blocks, loops) don't need semicolons
                    let span = expr.1.clone();
                    stmts.push((Stmt::Expression(expr), span));
                } else {
                    // Trailing expression (no semicolon)
                    trailing_expr = Some(Box::new(expr));
                    break;
                }
            } else {
                let found = match self.peek() {
                    Some(tok) => format!("{tok}"),
                    None => "end of file".to_string(),
                };
                self.error(format!("unexpected {found} in block"));
                self.advance();
            }
        }

        self.expect(&Token::RightBrace)?;

        Some(Block {
            stmts,
            trailing_expr,
        })
    }

    /// Convert a value-bearing statement (`Stmt::If`, `Stmt::IfLet`,
    /// `Stmt::Match`) that appears in tail position into the equivalent
    /// expression form, so the block's `trailing_expr` slot can be populated.
    ///
    /// Returns `None` for statement forms that are not value-bearing (i.e.
    /// statements that never reach this function in practice because the caller
    /// guards on `is_value_bearing` first).
    ///
    /// Span notes: `Stmt::If` does not store individual spans for `then_block`
    /// and `else_block`.  We wrap their `Block` values inside `Expr::Block`
    /// nodes and assign each one a span that starts at the end of the
    /// preceding element and ends at the close of the outer statement.  This is
    /// a conservative approximation; diagnostic positions within the blocks are
    /// still anchored by the expressions and statements inside them, so the
    /// wrapper span matters only for coarse messages like "this block has type
    /// T" — where "the if statement" is equally informative.
    fn promote_stmt_to_trailing_expr(stmt: Spanned<Stmt>) -> Option<Spanned<Expr>> {
        let (stmt_kind, stmt_span) = stmt;
        match stmt_kind {
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                // Wrap `then_block: Block` as `Expr::Block` with an empty
                // (zero-length) span.
                //
                // WHY empty span: `Stmt::If` does not store individual block
                // spans; they are consumed inside `parse_block` and not
                // propagated.  Recovering them without modifying `Stmt::If`
                // (which would cascade through HIR/MIR and serialisers) is out
                // of scope for this slice.
                //
                // CONSERVATIVE CORRECTNESS: `span_contains_offset` in
                // completions.rs returns `true` for any empty span, so LSP
                // completions walk both branches unconditionally — matching the
                // pre-promotion behaviour where `collect_locals_from_stmt` for
                // `Stmt::If` also walks both branches without a span guard.
                //
                // WHEN obsolete: once `Stmt::If` carries `then_block_span` and
                // `else_block_span` fields, thread them through here and drop
                // the empty-span approximation.
                //
                // WHAT the real fix looks like: capture `self.peek_span()` at
                // each `parse_block()` call site for `then_block` and `else_block`
                // inside `parse_stmt`, store those ranges on `Stmt::If`, then
                // use them here.
                let block_start = condition.1.end;
                let then_expr = Box::new((Expr::Block(then_block), block_start..block_start));
                let else_expr = else_block
                    .map(|eb| Box::new(Self::convert_else_block_to_expr(eb, stmt_span.end)));
                Some((
                    Expr::If {
                        condition: Box::new(condition),
                        then_block: then_expr,
                        else_block: else_expr,
                    },
                    stmt_span,
                ))
            }
            Stmt::IfLet {
                pattern,
                expr,
                body,
                else_body,
            } => {
                // Stmt::IfLet and Expr::IfLet share the same `else_body: Option<Block>` type.
                Some((
                    Expr::IfLet {
                        pattern,
                        expr,
                        body,
                        else_body,
                    },
                    stmt_span,
                ))
            }
            Stmt::Match { scrutinee, arms } => Some((
                Expr::Match {
                    scrutinee: Box::new(scrutinee),
                    arms,
                },
                stmt_span,
            )),
            _ => None,
        }
    }

    /// Convert an `ElseBlock` (from `Stmt::If`) into a `Spanned<Expr>` usable
    /// as the `else_block` field of `Expr::If`.
    fn convert_else_block_to_expr(else_block: ElseBlock, parent_end: usize) -> Spanned<Expr> {
        if let Some(if_stmt) = else_block.if_stmt {
            // `else if ...` — recursively promote the nested `Stmt::If`.
            let span = if_stmt.1.clone();
            if let Some(promoted) = Self::promote_stmt_to_trailing_expr(*if_stmt) {
                return promoted;
            }
            // Fallback: should be unreachable if the nested stmt is Stmt::If.
            (
                Expr::Block(Block {
                    stmts: vec![],
                    trailing_expr: None,
                }),
                span,
            )
        } else if let Some(block) = else_block.block {
            // `else { ... }` — Block has no own span; use an empty span so that
            // span_contains_offset always returns true (empty spans are treated as
            // "universally contained").  This is conservative but correct: callers
            // still consult inner statement spans for finer positioning.
            // See the WHY/WHEN/WHAT on the Stmt::If arm in
            // `promote_stmt_to_trailing_expr` for the full rationale.
            (Expr::Block(block), parent_end..parent_end)
        } else {
            // Malformed ElseBlock — produce an empty block as a safe default.
            (
                Expr::Block(Block {
                    stmts: vec![],
                    trailing_expr: None,
                }),
                parent_end..parent_end,
            )
        }
    }

    #[expect(clippy::too_many_lines, reason = "parser function with many branches")]
    fn parse_stmt(&mut self) -> Option<Spanned<Stmt>> {
        let _guard = self.enter_recursion()?;
        let start = self.peek_span().start;

        // Check for labeled loop/while: @label: loop/while. A bare @name in
        // expression position is a context-reader candidate.
        if matches!(self.peek(), Some(Token::Label(_)))
            && self.peek_at(self.pos + 1) == Some(&Token::Colon)
        {
            return self.parse_labeled_stmt(start);
        }

        let stmt = match self.peek() {
            Some(Token::Let) => {
                self.advance();
                let pattern = self.parse_pattern()?;

                // `let r? = expr;` is syntactic sugar for `let r = expr?;`.
                // The `?` must immediately follow a simple identifier pattern;
                // complex patterns (tuples, constructors) cannot carry the
                // propagation suffix — the binding site is ambiguous without a
                // single name to anchor the unwrapped value to.
                let propagate = if self.peek() == Some(&Token::Question) {
                    let q_span = self.peek_span();
                    if !matches!(pattern.0, Pattern::Identifier(_)) {
                        self.error_at(
                            "`?` propagation suffix requires a simple identifier pattern"
                                .to_string(),
                            q_span,
                        );
                        return None;
                    }
                    self.advance();
                    true
                } else {
                    false
                };

                let ty = if self.eat(&Token::Colon) {
                    Some(self.parse_type()?)
                } else {
                    None
                };

                let value = if self.eat(&Token::Equal) {
                    let (expr, expr_span) = self.parse_expr()?;
                    if propagate {
                        // Desugar: wrap RHS in PostfixTry so `let r? = e;`
                        // is exactly `let r = e?;` from the type-checker onward.
                        // The span covers the full RHS so diagnostics from the
                        // `?` type-check land on the expression, not on `r?`.
                        let end = expr_span.end;
                        Some((
                            Expr::PostfixTry(Box::new((expr, expr_span))),
                            pattern.1.start..end,
                        ))
                    } else {
                        Some((expr, expr_span))
                    }
                } else if propagate {
                    self.error(
                        "`let r? = expr;` requires an initialiser; `let r?;` is not valid"
                            .to_string(),
                    );
                    return None;
                } else {
                    None
                };

                self.expect(&Token::Semicolon)?;

                Stmt::Let { pattern, ty, value }
            }
            Some(Token::Var) => {
                self.advance();
                let name = self.expect_ident()?;

                let ty = if self.eat(&Token::Colon) {
                    Some(self.parse_type()?)
                } else {
                    None
                };

                let value = if self.eat(&Token::Equal) {
                    Some(self.parse_expr()?)
                } else {
                    None
                };

                self.expect(&Token::Semicolon)?;

                Stmt::Var { name, ty, value }
            }
            // These don't need semicolons (they have blocks)
            Some(Token::If) => {
                self.advance();
                if self.eat(&Token::Let) {
                    let pattern = Box::new(self.parse_pattern()?);
                    self.expect(&Token::Equal)?;
                    let expr = Box::new(self.parse_expr()?);
                    let body = self.parse_block()?;
                    let else_body = if self.eat(&Token::Else) {
                        Some(self.parse_block()?)
                    } else {
                        None
                    };
                    Stmt::IfLet {
                        pattern,
                        expr,
                        body,
                        else_body,
                    }
                } else {
                    let condition = self.parse_cond_expr()?;
                    let then_block = self.parse_block()?;

                    let else_block = if self.eat(&Token::Else) {
                        if self.peek() == Some(&Token::If) {
                            // else if
                            let if_stmt = Box::new(self.parse_stmt()?);
                            Some(ElseBlock {
                                is_if: true,
                                if_stmt: Some(if_stmt),
                                block: None,
                            })
                        } else {
                            // else block
                            let block = self.parse_block()?;
                            Some(ElseBlock {
                                is_if: false,
                                if_stmt: None,
                                block: Some(block),
                            })
                        }
                    } else {
                        None
                    };

                    Stmt::If {
                        condition,
                        then_block,
                        else_block,
                    }
                }
            }
            Some(Token::Match) => {
                self.advance();
                let scrutinee = self.parse_cond_expr()?;
                self.expect(&Token::LeftBrace)?;

                let mut arms = Vec::new();
                while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
                    let before = self.pos;
                    if let Some(arm) = self.parse_match_arm() {
                        arms.push(arm);
                    } else {
                        self.skip_to_match_arm_boundary();
                        if self.pos == before && !self.at_end() {
                            self.advance();
                        }
                    }
                }

                self.expect(&Token::RightBrace)?;

                Stmt::Match { scrutinee, arms }
            }
            Some(Token::Loop) => {
                self.advance();
                let body = self.parse_block()?;
                Stmt::Loop { label: None, body }
            }
            Some(Token::While) => {
                self.advance();
                if self.eat(&Token::Let) {
                    let pattern = Box::new(self.parse_pattern()?);
                    self.expect(&Token::Equal)?;
                    let expr = Box::new(self.parse_expr()?);
                    let body = self.parse_block()?;
                    Stmt::WhileLet {
                        label: None,
                        pattern,
                        expr,
                        body,
                    }
                } else {
                    let condition = self.parse_cond_expr()?;
                    let body = self.parse_block()?;
                    Stmt::While {
                        label: None,
                        condition,
                        body,
                    }
                }
            }
            Some(Token::For) => {
                self.advance();
                let is_await = self.eat(&Token::Await);
                let pattern = self.parse_pattern()?;
                self.expect(&Token::In)?;
                let iterable = self.parse_expr()?;
                let body = self.parse_block()?;
                Stmt::For {
                    label: None,
                    is_await,
                    pattern,
                    iterable,
                    body,
                }
            }
            Some(Token::Break) => {
                self.advance();
                let label = if let Some(Token::Label(l)) = self.peek() {
                    let name = l[1..].to_string();
                    self.advance();
                    Some(name)
                } else {
                    None
                };
                let value = if self.peek() == Some(&Token::Semicolon) {
                    None
                } else {
                    Some(self.parse_expr()?)
                };
                self.expect(&Token::Semicolon)?;
                Stmt::Break { label, value }
            }
            Some(Token::Continue) => {
                self.advance();
                let label = if let Some(Token::Label(l)) = self.peek() {
                    let name = l[1..].to_string();
                    self.advance();
                    Some(name)
                } else {
                    None
                };
                self.expect(&Token::Semicolon)?;
                Stmt::Continue { label }
            }
            Some(Token::Return) => {
                self.advance();
                let value = if self.peek() == Some(&Token::Semicolon) {
                    None
                } else {
                    Some(self.parse_expr()?)
                };
                self.expect(&Token::Semicolon)?;
                Stmt::Return(value)
            }
            Some(Token::Defer) => {
                self.advance();
                let expr = self.parse_expr()?;
                // Block expressions don't need a trailing semicolon
                // (consistent with if/while/for).
                if !matches!(expr.0, Expr::Block(_)) {
                    self.expect(&Token::Semicolon)?;
                }
                Stmt::Defer(Box::new(expr))
            }
            _ => {
                // Not a recognized statement keyword - this will be handled by the caller
                return None;
            }
        };

        let end = self.peek_span().start;
        Some((stmt, start..end))
    }

    /// Parse a labeled statement: `'label: while ...` or `'label: loop ...`
    fn parse_labeled_stmt(&mut self, start: usize) -> Option<Spanned<Stmt>> {
        let label_tok = self.advance()?;
        let label = if let (Token::Label(l), _) = label_tok {
            l[1..].to_string()
        } else {
            return None;
        };
        self.expect(&Token::Colon)?;

        let stmt = match self.peek() {
            Some(Token::While) => {
                self.advance();
                if self.eat(&Token::Let) {
                    let pattern = Box::new(self.parse_pattern()?);
                    self.expect(&Token::Equal)?;
                    let expr = Box::new(self.parse_expr()?);
                    let body = self.parse_block()?;
                    Stmt::WhileLet {
                        label: Some(label),
                        pattern,
                        expr,
                        body,
                    }
                } else {
                    let condition = self.parse_cond_expr()?;
                    let body = self.parse_block()?;
                    Stmt::While {
                        label: Some(label),
                        condition,
                        body,
                    }
                }
            }
            Some(Token::Loop) => {
                self.advance();
                let body = self.parse_block()?;
                Stmt::Loop {
                    label: Some(label),
                    body,
                }
            }
            Some(Token::For) => {
                self.advance();
                let is_await = self.eat(&Token::Await);
                let pattern = self.parse_pattern()?;
                self.expect(&Token::In)?;
                let iterable = self.parse_expr()?;
                let body = self.parse_block()?;
                Stmt::For {
                    label: Some(label),
                    is_await,
                    pattern,
                    iterable,
                    body,
                }
            }
            _ => {
                self.error("expected `while`, `loop`, or `for` after label".to_string());
                return None;
            }
        };

        let end = self.peek_span().start;
        Some((stmt, start..end))
    }

    fn parse_compound_assign_op(&mut self) -> Option<CompoundAssignOp> {
        match self.peek() {
            Some(Token::PlusEqual) => {
                self.advance();
                Some(CompoundAssignOp::Add)
            }
            Some(Token::MinusEqual) => {
                self.advance();
                Some(CompoundAssignOp::Subtract)
            }
            Some(Token::StarEqual) => {
                self.advance();
                Some(CompoundAssignOp::Multiply)
            }
            Some(Token::SlashEqual) => {
                self.advance();
                Some(CompoundAssignOp::Divide)
            }
            Some(Token::PercentEqual) => {
                self.advance();
                Some(CompoundAssignOp::Modulo)
            }
            Some(Token::AmpEqual) => {
                self.advance();
                Some(CompoundAssignOp::BitAnd)
            }
            Some(Token::PipeEqual) => {
                self.advance();
                Some(CompoundAssignOp::BitOr)
            }
            Some(Token::CaretEqual) => {
                self.advance();
                Some(CompoundAssignOp::BitXor)
            }
            Some(Token::LessLessEqual) => {
                self.advance();
                Some(CompoundAssignOp::Shl)
            }
            Some(Token::GreaterGreaterEqual) => {
                self.advance();
                Some(CompoundAssignOp::Shr)
            }
            _ => None,
        }
    }

    // ── Expressions (Pratt Precedence) ──
    fn parse_expr(&mut self) -> Option<Spanned<Expr>> {
        let _guard = self.enter_recursion()?;
        self.parse_expr_bp(0)
    }

    /// True while a bare identifier directly followed by `{` must be read as an
    /// identifier (block opener) rather than a struct literal — i.e. inside an
    /// `if`/`while` condition or `match` scrutinee at the top level.
    fn no_struct_literal(&self) -> bool {
        self.no_struct_literal.get()
    }

    /// Set `no_struct_literal` to `value` and return a guard that restores the
    /// previous value on drop. Survives every early `return`/`?` path.
    fn set_no_struct_literal(&self, value: bool) -> NoStructLiteralGuard {
        let prev = self.no_struct_literal.get();
        self.no_struct_literal.set(value);
        NoStructLiteralGuard {
            cell: Rc::clone(&self.no_struct_literal),
            prev,
        }
    }

    /// Parse an `if`/`while` condition or `match` scrutinee. In this position a
    /// bare identifier directly followed by `{` opens the block, never a struct
    /// literal, so the `no_struct_literal` restriction is set for the duration
    /// of the parse. The restriction is lifted again inside any bracketing
    /// delimiter (see the `(...)`, `[...]`, call-args, index, and struct-body
    /// parse sites), so `if (Foo { a: 1 }).b {…}` and other delimited struct
    /// literals in the condition still parse.
    fn parse_cond_expr(&mut self) -> Option<Spanned<Expr>> {
        let _guard = self.set_no_struct_literal(true);
        self.parse_expr()
    }

    /// Run `f` with the `no_struct_literal` restriction lifted. Used when the
    /// parser descends through an unambiguous delimiter (`(...)`, `[...]`, call
    /// args, index, struct body): the enclosing `{` is no longer the condition's
    /// block, so a struct literal there is no longer ambiguous.
    fn with_struct_literals_allowed<T>(&mut self, f: impl FnOnce(&mut Self) -> T) -> T {
        let _guard = self.set_no_struct_literal(false);
        f(self)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "Pratt parser covers many expression forms"
    )]
    fn parse_expr_bp(&mut self, min_bp: u8) -> Option<Spanned<Expr>> {
        let start = self.peek_span().start;

        // `&expr` is not an expression in Hew. `&` is infix bitwise-and and the
        // type-level borrow marker (`&T`, ABI substrate); there is no
        // prefix-reference/borrow expression. Duplication is spelled `clone x`.
        // Catch a leading `&` in operand position with a targeted diagnostic so
        // the reader is pointed at `clone` instead of a generic parse error,
        // then recover by parsing the operand as if the `&` were absent (one
        // diagnostic, no cascade).
        if self.peek() == Some(&Token::Ampersand) {
            self.error_with_hint(
                "`&` is not a prefix operator; Hew has no reference or borrow \
                 expression"
                    .to_string(),
                "to duplicate a value, write `clone x`",
            );
            self.advance()?; // consume `&` and recover on the operand
            return self.parse_expr_bp(min_bp);
        }

        // Prefix operators
        let mut lhs = if self.peek_is_clone_prefix() {
            // Contextual `clone <operand>` duplication prefix. `clone` is not a
            // reserved word — it is also a method/free-fn name — so it only acts
            // as the prefix when it sits in operator position immediately
            // followed by an operand token (`peek_is_clone_prefix`). Binds at
            // unary precedence so `clone a + b` is `(clone a) + b` and
            // `clone x.field` / `clone foo()` clone the whole postfix chain.
            self.advance()?; // consume `clone`
            let operand = self.parse_expr_bp(CLONE_PREFIX_BP)?;
            let end = operand.1.end;
            (Expr::Clone(Box::new(operand)), start..end)
        } else if let Some(rbp) = self.peek().and_then(prefix_bp) {
            let (op_tok, _) = self.advance()?;
            match op_tok {
                Token::Bang => {
                    let operand = self.parse_expr_bp(rbp)?;
                    let end = operand.1.end;
                    (
                        Expr::Unary {
                            op: UnaryOp::Not,
                            operand: Box::new(operand),
                        },
                        start..end,
                    )
                }
                Token::Minus => {
                    let operand = self.parse_expr_bp(rbp)?;
                    let end = operand.1.end;
                    (
                        Expr::Unary {
                            op: UnaryOp::Negate,
                            operand: Box::new(operand),
                        },
                        start..end,
                    )
                }
                Token::Tilde => {
                    let operand = self.parse_expr_bp(rbp)?;
                    let end = operand.1.end;
                    (
                        Expr::Unary {
                            op: UnaryOp::BitNot,
                            operand: Box::new(operand),
                        },
                        start..end,
                    )
                }
                Token::Await => {
                    let operand = self.parse_expr_bp(rbp)?;
                    let end = operand.1.end;
                    (Expr::Await(Box::new(operand)), start..end)
                }
                Token::Star => {
                    // Raw pointer dereference (`*expr`).  v0.5 parses but
                    // the type checker rejects with either
                    // `UnsafeOperationRequiresBlock` (outside unsafe) or a
                    // "not lowered in v0.5" diagnostic (inside unsafe).
                    let operand = self.parse_expr_bp(rbp)?;
                    let end = operand.1.end;
                    (
                        Expr::Unary {
                            op: UnaryOp::RawDeref,
                            operand: Box::new(operand),
                        },
                        start..end,
                    )
                }
                _ => unreachable!(),
            }
        } else {
            self.parse_primary()?
        };

        // Infix + postfix
        loop {
            // Try postfix first (highest precedence)
            match self.peek() {
                Some(Token::Dot) => {
                    lhs = self.parse_dot_postfix(lhs)?;
                    continue;
                }
                Some(Token::LeftParen) => {
                    lhs = self.parse_call_postfix(lhs)?;
                    continue;
                }
                Some(Token::LeftBracket) => {
                    lhs = self.parse_index_postfix(lhs)?;
                    continue;
                }
                Some(Token::Question) => {
                    self.advance();
                    let end = self.peek_span().start;
                    lhs = (Expr::PostfixTry(Box::new(lhs)), start..end);
                    continue;
                }
                Some(Token::As) => {
                    self.advance();
                    let ty = self.parse_type()?;
                    let end = ty.1.end;
                    let expr_start = lhs.1.start;
                    lhs = (
                        Expr::Cast {
                            expr: Box::new(lhs),
                            ty,
                        },
                        expr_start..end,
                    );
                    continue;
                }
                _ => {}
            }

            // Generic call: ident<Type1, Type2>(args)
            // Must check BEFORE infix operators consume '<' as less-than.
            if self.peek() == Some(&Token::Less) {
                if let Expr::Identifier(_) = &lhs.0 {
                    let saved = self.save_pos();
                    self.advance(); // consume '<'
                    if let Some(type_args) = self.parse_type_args() {
                        if self.peek() == Some(&Token::LeftParen) {
                            self.advance(); // consume '('
                            let args = self.parse_call_args()?;
                            self.expect(&Token::RightParen)?;
                            let end = self.peek_span().start;
                            lhs = (
                                Expr::Call {
                                    function: Box::new(lhs),
                                    type_args: Some(type_args),
                                    args,
                                    is_tail_call: false,
                                },
                                start..end,
                            );
                            continue;
                        }
                    }
                    // Not a generic call — backtrack
                    self.restore_pos(saved);
                }
            }

            // Timeout combinator: expr | after duration
            // Checked before infix so `| after` is not consumed as bitwise OR.
            if self.peek() == Some(&Token::Pipe) {
                let saved = self.save_pos();
                self.advance(); // consume |
                if self.peek() == Some(&Token::After) {
                    // Binding power 13 (same as bitwise OR left bp)
                    if 13 >= min_bp {
                        self.advance(); // consume after
                        let duration = self.parse_expr_bp(14)?;
                        let end = duration.1.end;
                        lhs = (
                            Expr::Timeout {
                                expr: Box::new(lhs),
                                duration: Box::new(duration),
                            },
                            start..end,
                        );
                        continue;
                    }
                }
                self.restore_pos(saved);
            }

            // Detect removed `=~` and `!~` regex operators.  The lexer never
            // produced `EqTilde`/`BangTilde` tokens, so the character sequences
            // tokenise as adjacent `=`+`~` or `!`+`~`.  Neither `=` nor `!` has
            // infix binding power, so the loop would break and leave a confusing
            // "expected `;`" error — check here before the infix break.
            {
                let next = self.peek();
                let is_eq_tilde = next == Some(&Token::Equal)
                    && self.peek_at(self.pos + 1) == Some(&Token::Tilde)
                    && {
                        let eq_end = self.peek_span().end;
                        self.tokens
                            .get(self.pos + 1)
                            .is_some_and(|(_, s)| s.start == eq_end)
                    };
                let is_bang_tilde = next == Some(&Token::Bang)
                    && self.peek_at(self.pos + 1) == Some(&Token::Tilde)
                    && {
                        let bang_end = self.peek_span().end;
                        self.tokens
                            .get(self.pos + 1)
                            .is_some_and(|(_, s)| s.start == bang_end)
                    };
                if is_eq_tilde || is_bang_tilde {
                    let op_str = if is_eq_tilde { "=~" } else { "!~" };
                    let op_start = self.peek_span().start;
                    self.advance(); // consume `=` or `!`
                    let op_end = self.peek_span().end;
                    self.advance(); // consume `~`
                    let op_span = op_start..op_end;
                    self.error_at_with_hint(
                        format!(
                            "E_REGEX_OP_REMOVED: the `{op_str}` regex operator has been removed; \
                             use a match arm or `Pattern.is_match()` instead (HEW-SPEC-2026 §5)"
                        ),
                        op_span.clone(),
                        format!(
                            "replace `expr {op_str} pattern` with `match expr {{ re\"...\" => true, _ => false }}`"
                        ),
                    );
                    while !matches!(
                        self.peek(),
                        Some(&Token::Semicolon | &Token::RightBrace) | None
                    ) {
                        self.advance();
                    }
                    if self.peek() == Some(&Token::Semicolon) {
                        self.advance();
                    }
                    lhs = (Expr::Tuple(vec![]), op_span);
                    break;
                }
            }

            // Then try infix
            let Some((lbp, rbp)) = self.peek().and_then(infix_bp) else {
                break;
            };
            if lbp < min_bp {
                break;
            }

            // Detect the removed `<-` send operator: lexer now produces two tokens
            // `<` (at pos) and `-` (at pos+1) adjacently.  Emit E_OPERATOR_REMOVED,
            // then skip to the statement boundary (`;` or `}`) so that the caller
            // does not produce cascading "unexpected token" diagnostics for the
            // right-hand side tokens.
            if self.peek() == Some(&Token::Less) {
                let less_end = self.peek_span().end;
                if self.peek_at(self.pos + 1) == Some(&Token::Minus) {
                    let minus_start = self
                        .tokens
                        .get(self.pos + 1)
                        .map_or(usize::MAX, |(_, s)| s.start);
                    if less_end == minus_start {
                        let op_span = self.peek_span().start..minus_start + 1;
                        self.advance(); // consume `<`
                        self.advance(); // consume `-`
                        self.error_at_with_hint(
                            "E_OPERATOR_REMOVED: the `<-` send operator has been removed; \
                             use `handle(msg)` call syntax instead (HEW-SPEC-2026 §4.x)"
                                .to_string(),
                            op_span.clone(),
                            "replace `target <- msg` with `target(msg)`".to_string(),
                        );
                        // Skip tokens through the end of the statement to suppress
                        // cascading "unexpected token" diagnostics on the RHS.
                        while !matches!(
                            self.peek(),
                            Some(&Token::Semicolon | &Token::RightBrace) | None
                        ) {
                            self.advance();
                        }
                        // Consume the `;` now so that parse_block treats this as a
                        // fully consumed expression statement rather than seeing the
                        // semicolon as unexpected.
                        if self.peek() == Some(&Token::Semicolon) {
                            self.advance();
                        }
                        // Return a synthetic unit expression so the block parser
                        // completes the statement without entering the error-recovery path.
                        lhs = (Expr::Tuple(vec![]), op_span);
                        break;
                    }
                }
            }

            // `is` is a keyword token (not a symbol), handled as a special infix form.
            if self.peek() == Some(&Token::Is) {
                self.advance(); // consume `is`
                let rhs = self.parse_expr_bp(rbp)?;
                let end = rhs.1.end;
                let lhs_start = lhs.1.start;
                lhs = (
                    Expr::Is {
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    },
                    lhs_start..end,
                );
                continue;
            }

            let (op_tok, _) = self.advance()?;
            let Some(op) = token_to_binop(&op_tok) else {
                self.error(format!("invalid binary operator token: {op_tok:?}"));
                return None;
            };
            let rhs = self.parse_expr_bp(rbp)?;
            let end = rhs.1.end;

            lhs = (
                Expr::Binary {
                    left: Box::new(lhs),
                    op,
                    right: Box::new(rhs),
                },
                start..end,
            );
        }

        Some(lhs)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "expression parser with many branches"
    )]
    fn parse_primary(&mut self) -> Option<Spanned<Expr>> {
        let start = self.peek_span().start;

        let expr = match self.peek()? {
            Token::Duration(s) => {
                if let Some(nanos) = parse_duration_literal(s) {
                    self.advance();
                    Expr::Literal(Literal::Duration(nanos))
                } else {
                    self.error_invalid_literal_with_hint(
                        "invalid duration literal".to_string(),
                        "valid formats: 100ms, 5s, 2m, 1h, 500us, 10ns",
                    );
                    return None;
                }
            }
            Token::Integer(s) => {
                if let Ok((val, radix)) = parse_int_literal(s) {
                    self.advance();
                    Expr::Literal(Literal::Integer { value: val, radix })
                } else {
                    self.error_invalid_literal_with_hint(
                        format!("invalid integer literal '{s}'"),
                        "integer literals support decimal, 0x hex, 0o octal, and 0b binary",
                    );
                    return None;
                }
            }
            Token::Float(s) => {
                let cleaned: String = s.chars().filter(|c| *c != '_').collect();
                if let Ok(val) = cleaned.parse::<f64>() {
                    self.advance();
                    Expr::Literal(Literal::Float(val))
                } else {
                    self.error_invalid_literal(format!("invalid float literal '{s}'"));
                    return None;
                }
            }
            Token::StringLit(s) => {
                let inner = unquote_str(s);
                let tok_start = start;
                let (unescaped, unescape_errs) = unescape_string(inner);
                for (off, msg) in unescape_errs {
                    let err_start = tok_start + 1 + off;
                    self.errors.push(ParseError {
                        message: msg.to_string(),
                        span: err_start..err_start + 2,
                        hint: None,
                        severity: Severity::Error,
                        kind: ParseDiagnosticKind::InvalidLiteral,
                    });
                }
                self.advance();
                Expr::Literal(Literal::String(unescaped))
            }
            Token::CharLit(s) => {
                let inner = s
                    .strip_prefix('\'')
                    .and_then(|s| s.strip_suffix('\''))
                    .unwrap_or(s);
                if let Some(c) = self.parse_char_escape(inner) {
                    self.advance();
                    Expr::Literal(Literal::Char(c))
                } else {
                    return None;
                }
            }
            Token::RawString(s) => {
                let s = unquote_str(s).to_string();
                self.advance();
                Expr::Literal(Literal::String(s))
            }
            Token::ByteStringLit(s) => {
                // Strip b"..." wrapper and unescape.
                let inner = s
                    .strip_prefix("b\"")
                    .and_then(|s| s.strip_suffix('"'))
                    .unwrap_or(s);
                let tok_start = start;
                let (unescaped, unescape_errs) = unescape_string(inner);
                for (off, msg) in unescape_errs {
                    let err_start = tok_start + 2 + off;
                    self.errors.push(ParseError {
                        message: msg.to_string(),
                        span: err_start..err_start + 2,
                        hint: None,
                        severity: Severity::Error,
                        kind: ParseDiagnosticKind::InvalidLiteral,
                    });
                }
                self.advance();
                Expr::ByteStringLiteral(unescaped.into_bytes())
            }
            Token::InterpolatedString(s) => {
                let s = s.to_string();
                self.advance();
                let parts = parse_string_parts(&s, 2, 1, "{", start, &mut self.errors);
                Expr::InterpolatedString(parts)
            }
            Token::RegexLiteral(s) => {
                // Normalise the token: strip delimiters and decode only the
                // delimiter escape `\"`. Regex backslashes are passed through
                // verbatim so `re"\s+"` reaches the engine as `\s+`.
                let pattern = normalize_regex_literal(s);
                self.advance();
                Expr::RegexLiteral(pattern)
            }
            Token::True => {
                self.advance();
                Expr::Literal(Literal::Bool(true))
            }
            Token::False => {
                self.advance();
                Expr::Literal(Literal::Bool(false))
            }
            Token::Label(label) => {
                let name = (*label).to_string();
                self.advance();
                Expr::Identifier(name)
            }
            Token::Identifier(name)
                if *name == "bytes" && self.peek_at(self.pos + 1) == Some(&Token::LeftBracket) =>
            {
                self.advance(); // consume "bytes"
                self.advance(); // consume "["

                let mut values: Vec<u8> = Vec::new();
                while self.peek() != Some(&Token::RightBracket) {
                    let elem_expr = self.parse_expr()?;
                    if let Expr::Literal(Literal::Integer { value, .. }) = &elem_expr.0 {
                        if *value < 0 || *value > 255 {
                            self.error_invalid_literal(format!(
                                "byte value {value} out of range (must be 0..255)"
                            ));
                            return None;
                        }
                        #[expect(
                            clippy::cast_possible_truncation,
                            clippy::cast_sign_loss,
                            reason = "Checked to be 0..=255 above"
                        )]
                        values.push(*value as u8);
                    } else {
                        self.error(
                            "byte array literal elements must be integer literals".to_string(),
                        );
                        return None;
                    }
                    if !self.eat(&Token::Comma) {
                        break;
                    }
                }
                self.expect(&Token::RightBracket)?;
                Expr::ByteArrayLiteral(values)
            }
            Token::Identifier(name) => {
                let mut name = name.to_string();
                self.advance();

                // Handle path expressions like Vec::new, HashMap::new
                // Optionally accept Rust-style turbofish on a path segment:
                // `Type::<T, U>::method` — the `<...>` are stashed and surface
                // as the call's `type_args` when the path is invoked with `(`.
                let mut turbofish: Option<Vec<Spanned<TypeExpr>>> = None;
                while self.eat(&Token::DoubleColon) {
                    if self.peek() == Some(&Token::Less) {
                        let saved = self.save_pos();
                        self.advance(); // consume '<'
                        if let Some(args) = self.parse_type_args() {
                            if turbofish.is_some() {
                                self.error(
                                    "turbofish `::<...>` may appear at most once in a path"
                                        .to_string(),
                                );
                            }
                            turbofish = Some(args);
                            // A turbofish must be followed by another `::ident`
                            // segment or the call site `(`. If we don't see
                            // `::` next we fall out of the loop and let the
                            // surrounding logic (`(args)`) consume the call.
                            continue;
                        }
                        self.restore_pos(saved);
                        break;
                    }
                    if let Some(segment) = self.expect_ident() {
                        name = format!("{name}::{segment}");
                    } else {
                        break;
                    }
                }

                // If turbofish was present, the path must be invoked as a call.
                // Build the `Expr::Call` here so the explicit type args reach the
                // checker via `Call.type_args` exactly as for the bare-call form
                // `Type::method<T>(args)`. The struct-init / generic-call paths
                // below are skipped because turbofish is unambiguously a call.
                if let Some(type_args) = turbofish {
                    if self.peek() != Some(&Token::LeftParen) {
                        self.error(
                            "turbofish `::<...>` must be followed by a function call `(...)`"
                                .to_string(),
                        );
                        return None;
                    }
                    self.advance(); // consume '('
                    let args = self.parse_call_args()?;
                    self.expect(&Token::RightParen)?;
                    let end = self.peek_span().start;
                    let func_span = start..end;
                    let func = (Expr::Identifier(name), func_span.clone());
                    return Some((
                        Expr::Call {
                            function: Box::new(func),
                            type_args: Some(type_args),
                            args,
                            is_tail_call: false,
                        },
                        start..end,
                    ));
                }

                // Check for struct initialization — including the explicit-type-arg form
                // `Name<T, ...> { field: expr, ... }`.  We need a speculative parse
                // because `<` is also a comparison operator: only commit when we see
                // a closing `>` **immediately** followed by `{`.
                let explicit_type_args: Option<Vec<Spanned<TypeExpr>>> =
                    if self.peek() == Some(&Token::Less) {
                        let saved_pos = self.save_pos();
                        self.advance(); // consume '<'
                        if let Some(type_args) = self.parse_type_args() {
                            // Accept only if `{` follows (this is a struct init, not a
                            // comparison expression like `if x < y { ... }`).
                            if self.peek() == Some(&Token::LeftBrace) {
                                Some(type_args)
                            } else {
                                self.restore_pos(saved_pos);
                                None
                            }
                        } else {
                            self.restore_pos(saved_pos);
                            None
                        }
                    } else {
                        None
                    };

                if explicit_type_args.is_some() || self.peek() == Some(&Token::LeftBrace) {
                    // In `if`/`while` condition or `match` scrutinee position a
                    // bare identifier followed by `{` opens the block, not a
                    // struct literal — so the probe is suppressed there. The
                    // restriction only applies to the bare-identifier form; the
                    // explicit-type-arg form (`Name<T> { ... }`) is already gated
                    // on `>{` and is unambiguous, so it is left intact.
                    let is_struct_init = explicit_type_args.is_some()
                        || (!self.no_struct_literal() && self.probe_struct_init_brace());

                    if is_struct_init {
                        self.advance(); // consume {
                                        // Inside the struct body the `{` is consumed, so any
                                        // nested bare-ident struct literal is unambiguous again.
                        let (fields, base) =
                            self.with_struct_literals_allowed(Self::parse_struct_init_body)?;
                        Expr::StructInit {
                            name,
                            fields,
                            type_args: explicit_type_args,
                            base,
                        }
                    } else {
                        Expr::Identifier(name)
                    }
                } else {
                    Expr::Identifier(name)
                }
            }
            Token::Less => {
                // Speculative parse to detect old generic lambda: <T>(x: T) => expr.
                // This form was removed in v0.5; type-parameterized closures are not
                // supported. Detect the form and emit a typed migration diagnostic.
                let saved_pos = self.save_pos();
                self.advance(); // consume '<'

                let is_old_generic_lambda = if let Some(_type_params) = self.parse_type_params() {
                    if self.peek() == Some(&Token::LeftParen) {
                        self.advance(); // consume '('
                        if self.try_parse_lambda_params().is_some() {
                            if self.expect(&Token::RightParen).is_some() {
                                // Check for optional return type then `=>`
                                if self.eat(&Token::Arrow) {
                                    self.parse_type().is_some()
                                        && self.peek() == Some(&Token::FatArrow)
                                } else {
                                    self.peek() == Some(&Token::FatArrow)
                                }
                            } else {
                                false
                            }
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                } else {
                    false
                };

                self.restore_pos(saved_pos);

                if is_old_generic_lambda {
                    // Consume through the form for error recovery, then emit typed error.
                    self.advance(); // consume '<'
                    self.parse_type_params();
                    self.expect(&Token::LeftParen);
                    self.try_parse_lambda_params();
                    self.expect(&Token::RightParen);
                    self.parse_opt_return_type();
                    if self.eat(&Token::FatArrow) {
                        self.parse_expr();
                    }
                    self.error_closure_pipe_syntax(
                        "E_CLOSURE_PIPE_SYNTAX: `<T>(params) => body` has been removed; \
                         type-parameterized closures are not supported in v0.5"
                            .to_string(),
                        start..self.peek_span().start,
                        "use `|params| body` — omit type parameters on closure expressions",
                    );
                    return None;
                }
                self.error("unexpected '<' at start of expression".to_string());
                return None;
            }
            Token::LeftParen => {
                self.advance();
                // Inside `(...)` the enclosing `{` is no longer the condition's
                // block, so a struct literal here is unambiguous — lift the
                // restriction for the parenthesised sub-expression (e.g.
                // `if (Foo { a: 1 }).b {…}`). Restored on arm exit.
                let _allow_struct = self.set_no_struct_literal(false);

                // Detect and reject old `(params) => body` parenthesized lambda syntax.
                // This form was removed in v0.5; the current form is `|params| body`.
                // Keep the detection here so we can surface a typed migration diagnostic
                // rather than a cryptic parse error when `=>` is encountered later.
                let saved_pos = self.save_pos();
                let is_old_paren_lambda = if self.try_parse_lambda_params().is_some() {
                    if self.expect(&Token::RightParen).is_some() {
                        // Check for optional return type annotation then `=>`
                        if self.eat(&Token::Arrow) {
                            self.parse_type().is_some() && self.peek() == Some(&Token::FatArrow)
                        } else {
                            self.peek() == Some(&Token::FatArrow)
                        }
                    } else {
                        false
                    }
                } else {
                    false
                };
                self.restore_pos(saved_pos);

                if is_old_paren_lambda {
                    // Consume through the entire form for error recovery continuity.
                    self.try_parse_lambda_params();
                    self.expect(&Token::RightParen);
                    self.parse_opt_return_type();
                    if self.eat(&Token::FatArrow) {
                        self.parse_expr();
                    }
                    self.error_closure_pipe_syntax(
                        "E_CLOSURE_PIPE_SYNTAX: `(params) => body` has been removed; \
                         use `|params| body` instead"
                            .to_string(),
                        start..self.peek_span().start,
                        "replace `(params) => body` with `|params| body`",
                    );
                    return None;
                } else if self.eat(&Token::RightParen) {
                    // Unit tuple
                    Expr::Tuple(Vec::new())
                } else {
                    // Parenthesized expression or tuple
                    let mut exprs = vec![self.parse_expr()?];
                    while self.eat(&Token::Comma) {
                        if self.peek() == Some(&Token::RightParen) {
                            break;
                        }
                        exprs.push(self.parse_expr()?);
                    }
                    self.expect(&Token::RightParen)?;

                    if exprs.len() == 1 {
                        // Safe: len == 1 guarantees next() yields one element.
                        return Some(exprs.into_iter().next().unwrap());
                    }
                    Expr::Tuple(exprs)
                }
            }
            Token::LeftBracket => {
                self.advance();
                // Inside `[...]` the enclosing `{` is no longer the condition's
                // block, so struct literals in array elements are unambiguous.
                let _allow_struct = self.set_no_struct_literal(false);
                if self.eat(&Token::RightBracket) {
                    return Some((Expr::Array(Vec::new()), start..self.peek_span().start));
                }

                let first = self.parse_expr()?;
                if self.eat(&Token::Semicolon) {
                    let count = self.parse_expr()?;
                    self.expect(&Token::RightBracket)?;
                    return Some((
                        Expr::ArrayRepeat {
                            value: Box::new(first),
                            count: Box::new(count),
                        },
                        start..self.peek_span().start,
                    ));
                }

                let mut elements = vec![first];
                while self.eat(&Token::Comma) {
                    if self.peek() == Some(&Token::RightBracket) {
                        break;
                    }
                    elements.push(self.parse_expr()?);
                }

                self.expect(&Token::RightBracket)?;
                Expr::Array(elements)
            }
            Token::Pipe | Token::PipePipe => self.parse_pipe_lambda(false, start)?,
            Token::LeftBrace => {
                // Disambiguate: {"str": expr, ...} → MapLiteral, else → Block
                // Note: bare {} remains a Block — empty HashMap coercion is
                // handled in the type checker when expected type is HashMap.
                // Use direct lookahead (no save/restore) for the common block path.
                //
                // A `{` reaching this arm is unambiguously a block (or map)
                // expression — even in `if`/`while` condition or `match`
                // scrutinee position (`if { let x = Foo { a: 1 }; x.a } {…}`).
                // The `{` opens the block's body, so the enclosing condition's
                // block is no longer ambiguous: lift `no_struct_literal` so a
                // struct literal INSIDE this block parses. Restored on arm exit.
                let _allow_struct = self.set_no_struct_literal(false);
                if matches!(self.peek_at(self.pos + 1), Some(Token::StringLit(_)))
                    && self.peek_at(self.pos + 2) == Some(&Token::Colon)
                {
                    self.advance(); // consume '{'
                    self.parse_map_literal_entries()?
                } else {
                    Expr::Block(self.parse_block()?)
                }
            }
            Token::If => {
                self.advance();
                if self.eat(&Token::Let) {
                    let pattern = Box::new(self.parse_pattern()?);
                    self.expect(&Token::Equal)?;
                    let expr = Box::new(self.parse_expr()?);
                    let body = self.parse_block()?;
                    let else_body = if self.eat(&Token::Else) {
                        Some(self.parse_block()?)
                    } else {
                        None
                    };
                    Expr::IfLet {
                        pattern,
                        expr,
                        body,
                        else_body,
                    }
                } else {
                    let condition = Box::new(self.parse_cond_expr()?);
                    let then_block = Box::new(self.parse_expr()?);
                    let else_block = if self.eat(&Token::Else) {
                        Some(Box::new(self.parse_expr()?))
                    } else {
                        None
                    };
                    Expr::If {
                        condition,
                        then_block,
                        else_block,
                    }
                }
            }
            Token::Match => {
                self.advance();
                let scrutinee = Box::new(self.parse_cond_expr()?);
                self.expect(&Token::LeftBrace)?;

                let mut arms = Vec::new();
                while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
                    let before = self.pos;
                    if let Some(arm) = self.parse_match_arm() {
                        arms.push(arm);
                    } else {
                        self.skip_to_match_arm_boundary();
                        if self.pos == before && !self.at_end() {
                            self.advance();
                        }
                    }
                }

                self.expect(&Token::RightBrace)?;
                Expr::Match { scrutinee, arms }
            }
            // actor [move] |params| [-> Ret] { body }
            // Lambda actor literal (replaces `spawn (...) => ...`).
            Token::Actor => {
                self.advance();

                let is_move = self.eat(&Token::Move);

                if self.peek() != Some(&Token::Pipe) {
                    self.error_with_hint(
                        "E_SPAWN_LAMBDA_SYNTAX_REMOVED: expected `|` to begin actor parameter list"
                            .to_string(),
                        "use `actor |params| { body }` to declare a lambda actor".to_string(),
                    );
                    return None;
                }
                self.advance(); // consume `|`

                let params = self.try_parse_lambda_params().unwrap_or_default();

                self.expect(&Token::Pipe)?;

                let return_type = self.parse_opt_return_type()?;

                // Lambda actor body must be a braced block; parse_block consumes the `{`.
                if self.peek() != Some(&Token::LeftBrace) {
                    self.error_with_hint(
                        "E_SPAWN_LAMBDA_SYNTAX_REMOVED: expected `{` to begin actor body"
                            .to_string(),
                        "use `actor |params| { body }` — the body must be a braced block"
                            .to_string(),
                    );
                    return None;
                }
                let body_block = self.parse_block()?;
                let body_end = self.peek_span().start;
                let body = Box::new((Expr::Block(body_block), start..body_end));

                return Some((
                    Expr::SpawnLambdaActor {
                        is_move,
                        params,
                        return_type,
                        body,
                    },
                    start..self.peek_span().start,
                ));
            }
            Token::Spawn => {
                self.advance();

                // Check whether the user wrote the legacy `spawn (params) => body` form.
                // This form was removed in favour of `actor |params| { body }`.
                // Consume an optional `move` keyword only to detect legacy syntax; it is not
                // used in the regular `spawn ActorName(...)` form.
                let _is_move_legacy = self.eat(&Token::Move);

                if self.peek() == Some(&Token::LeftParen) {
                    let saved_pos = self.save_pos();
                    self.advance();
                    let is_legacy_lambda = self.try_parse_lambda_params().is_some() && {
                        self.expect(&Token::RightParen).is_some()
                            && (self.peek() == Some(&Token::FatArrow)
                                || self.peek() == Some(&Token::Arrow))
                    };
                    self.restore_pos(saved_pos);

                    if is_legacy_lambda {
                        // Consume through the entire legacy form so recovery can continue.
                        self.advance(); // (
                        self.try_parse_lambda_params();
                        self.expect(&Token::RightParen);
                        self.parse_opt_return_type();
                        if self.eat(&Token::FatArrow) {
                            self.parse_expr();
                        }
                        self.error_at_with_hint(
                            "E_SPAWN_LAMBDA_SYNTAX_REMOVED: `spawn (...) => ...` has been removed; \
                             use `actor |...| { ... }` instead (HEW-SPEC-2026 §4.x)"
                                .to_string(),
                            start..self.peek_span().start,
                            "replace `spawn (params) => body` with `actor |params| { body }`"
                                .to_string(),
                        );
                        return None;
                    }
                }

                // Regular spawn: spawn ActorName(...) or spawn module.ActorName(...)
                // or spawn ActorName<T>(...) with explicit turbofish type args.
                let name = self.expect_ident()?;
                let name_end = self.peek_span().start;
                let target = if self.eat(&Token::Dot) {
                    let actor_name = self.expect_ident()?;
                    let actor_end = self.peek_span().start;
                    Box::new((
                        Expr::FieldAccess {
                            object: Box::new((Expr::Identifier(name), start..name_end)),
                            field: actor_name,
                        },
                        start..actor_end,
                    ))
                } else {
                    Box::new((Expr::Identifier(name), start..name_end))
                };

                // Optional turbofish type-argument list `<T, U>` before `(`.
                // A bare `<` here is unambiguous: spawn does not admit
                // comparison in this position (the target is a name, not an
                // expression), so we eagerly parse the angle-bracket list.
                let type_args = if self.eat(&Token::Less) {
                    self.parse_type_args().unwrap_or_default()
                } else {
                    vec![]
                };

                let args = if self.eat(&Token::LeftParen) {
                    let mut args = Vec::new();
                    while !self.at_end() && self.peek() != Some(&Token::RightParen) {
                        let field_name = self.expect_ident()?;
                        self.expect(&Token::Colon)?;
                        let value = self.parse_expr()?;
                        args.push((field_name, value));
                        if !self.eat(&Token::Comma) {
                            break;
                        }
                    }
                    self.expect(&Token::RightParen)?;
                    args
                } else {
                    Vec::new()
                };

                Expr::Spawn {
                    target,
                    type_args,
                    args,
                }
            }
            Token::Move => {
                self.advance();
                if matches!(self.peek(), Some(Token::Pipe | Token::PipePipe)) {
                    self.parse_pipe_lambda(true, start)?
                } else if self.peek() == Some(&Token::LeftParen) {
                    // Old `move (params) => body` form — detect and diagnose.
                    // Consume through the form for recovery, then emit a typed error.
                    let saved_pos = self.save_pos();
                    self.advance(); // consume '('
                    let is_old_paren_lambda = self.try_parse_lambda_params().is_some()
                        && self.expect(&Token::RightParen).is_some()
                        && (self.peek() == Some(&Token::FatArrow)
                            || self.peek() == Some(&Token::Arrow));
                    self.restore_pos(saved_pos);

                    if is_old_paren_lambda {
                        self.advance(); // consume '('
                        self.try_parse_lambda_params();
                        self.expect(&Token::RightParen);
                        self.parse_opt_return_type();
                        if self.eat(&Token::FatArrow) {
                            self.parse_expr();
                        }
                        self.error_closure_pipe_syntax(
                            "E_CLOSURE_PIPE_SYNTAX: `move (params) => body` has been removed; \
                             use `move |params| expr` instead"
                                .to_string(),
                            start..self.peek_span().start,
                            "replace `move (params) => body` with `move |params| expr`",
                        );
                        return None;
                    }
                    self.error_closure_pipe_syntax(
                        "E_CLOSURE_PIPE_SYNTAX: expected `|` after `move` to begin a closure"
                            .to_string(),
                        self.peek_span(),
                        "write `move |params| expr`",
                    );
                    return None;
                } else {
                    self.error_closure_pipe_syntax(
                        "E_CLOSURE_PIPE_SYNTAX: expected `|` after `move` to begin a closure"
                            .to_string(),
                        self.peek_span(),
                        "write `move |params| expr`",
                    );
                    return None;
                }
            }
            // gen { yield ...; } — lazy generator block expression.
            // Must be followed immediately by a braced block; `gen fn` (item-level
            // generator functions) is parsed separately and does not reach here.
            // Bare `gen` without a block emits a typed diagnostic.
            Token::Gen if self.peek_at(self.pos + 1) == Some(&Token::LeftBrace) => {
                self.advance(); // consume `gen`
                let body = self.parse_block()?;
                Expr::GenBlock { body }
            }
            Token::Gen => {
                self.advance(); // consume `gen`
                let found = match self.peek() {
                    Some(tok) => format!("`{tok}`"),
                    None => "end of file".to_string(),
                };
                self.error_with_hint(
                    format!("E_GEN_BLOCK_SYNTAX: `gen` must be followed by a block; found {found}"),
                    "write `gen { yield expr; }` to create a generator block".to_string(),
                );
                return None;
            }
            Token::Return => {
                self.advance();
                // Return expressions are not typically parsed in expression context
                // This might be a parsing context issue - skip for now
                self.error("return statement in expression context".to_string());
                return None;
            }
            Token::Scope => {
                self.advance();
                // Reject obsolete surfaces: `scope.method()` and `scope |s| { ... }`.
                if self.eat(&Token::Dot) {
                    self.error(
                        "'scope.method()' syntax has been removed; use 'scope { ... }' with `fork name = expr;` bindings instead"
                            .to_string(),
                    );
                    return None;
                }
                if self.peek() == Some(&Token::Pipe) {
                    self.error(
                        "'scope |s| { s.launch / s.spawn / s.cancel }' has been removed; use 'scope { fork name = call(...); }' instead"
                            .to_string(),
                    );
                    return None;
                }
                self.scope_expr_depth += 1;
                let body = self.parse_block()?;
                self.scope_expr_depth -= 1;
                Expr::Scope { body }
            }
            Token::Fork => {
                let fork_span = self.peek_span();
                self.advance();
                // `fork` is now exclusively the child-start verb inside a scope block:
                // `fork name = call(...);` or bare `fork call(...);`.
                if self.peek() == Some(&Token::LeftBrace) {
                    if self.scope_expr_depth == 0 {
                        self.error_at(
                            "`fork { ... }` child-task blocks are only valid inside `scope { ... }`"
                                .to_string(),
                            fork_span,
                        );
                        return None;
                    }
                    if self.fork_block_depth > 0 {
                        self.error_at(
                            "nested `fork { ... }` blocks are not a CT-2 surface; use an inner `scope { ... }`"
                                .to_string(),
                            fork_span,
                        );
                        return None;
                    }
                    self.fork_block_depth += 1;
                    let body = self.parse_block()?;
                    self.fork_block_depth -= 1;
                    Expr::ForkBlock { body }
                } else {
                    let binding = if self.fork_starts_child_binding() {
                        let name = self.expect_ident()?;
                        self.expect(&Token::Equal)?;
                        Some(name)
                    } else {
                        None
                    };
                    let expr = self.parse_expr()?;
                    Expr::ForkChild {
                        binding,
                        expr: Box::new(expr),
                    }
                }
            }
            Token::After if self.looks_like_scope_deadline() => {
                let after_span = self.peek_span();
                if self.scope_expr_depth == 0 {
                    self.error_at(
                        "`after(duration) { ... }` deadline clauses are only valid inside `scope { ... }`"
                            .to_string(),
                        after_span,
                    );
                    return None;
                }
                self.advance();
                self.expect(&Token::LeftParen)?;
                let duration = self.parse_expr()?;
                self.expect(&Token::RightParen)?;
                let body = self.parse_block()?;
                Expr::ScopeDeadline {
                    duration: Box::new(duration),
                    body,
                }
            }
            Token::Try => {
                self.error(
                    "'try'/'catch' blocks have been removed; use the '?' operator instead"
                        .to_string(),
                );
                return None;
            }
            Token::Unsafe => {
                self.advance();
                if self.peek() != Some(&Token::LeftBrace) {
                    self.error(
                        "expected `{` after `unsafe`; `unsafe` must be followed by a block"
                            .to_string(),
                    );
                    return None;
                }
                Expr::UnsafeBlock(Box::new(self.parse_block()?))
            }
            Token::Select => {
                self.advance();
                self.expect(&Token::LeftBrace)?;

                let mut arms = Vec::new();
                let mut timeout = None;
                while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
                    if self.peek() == Some(&Token::After)
                        || matches!(self.peek(), Some(Token::Identifier(s)) if *s == "after")
                    {
                        self.advance();
                        let duration = self.parse_expr()?;
                        self.expect(&Token::FatArrow)?;
                        let body = self.parse_expr()?;
                        self.eat(&Token::Comma);
                        timeout = Some(Box::new(TimeoutClause {
                            duration: Box::new(duration),
                            body: Box::new(body),
                        }));
                        break;
                    }
                    arms.push(self.parse_select_arm()?);
                }

                self.expect(&Token::RightBrace)?;

                Expr::Select { arms, timeout }
            }
            Token::Race => {
                self.error("'race' blocks have been removed; use 'select' instead".to_string());
                return None;
            }
            Token::Join => {
                self.advance();
                // Accept either parentheses or braces for join
                let (open, close) = if self.peek() == Some(&Token::LeftBrace) {
                    (Token::LeftBrace, Token::RightBrace)
                } else {
                    (Token::LeftParen, Token::RightParen)
                };
                self.expect(&open)?;

                let mut exprs = Vec::new();
                while !self.at_end() && self.peek() != Some(&close) {
                    exprs.push(self.parse_expr()?);
                    if !self.eat(&Token::Comma) {
                        break;
                    }
                }

                self.expect(&close)?;
                Expr::Join(exprs)
            }
            Token::Yield => {
                self.advance();
                let value = if matches!(self.peek(), Some(Token::Semicolon | Token::RightBrace)) {
                    None
                } else {
                    Some(Box::new(self.parse_expr()?))
                };
                Expr::Yield(value)
            }
            Token::Cooperate => {
                self.error(
                    "'cooperate' is compiler-internal; explicit cooperate expressions are not supported"
                        .to_string(),
                );
                return None;
            }
            Token::This => {
                self.advance();
                Expr::This
            }
            Token::Emit => {
                self.advance();
                let event_name = self.expect_ident()?;
                let fields = if self.eat(&Token::LeftBrace) {
                    let mut fields = Vec::new();
                    while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
                        let field_name = self.expect_ident()?;
                        self.expect(&Token::Colon)?;
                        let field_val = self.parse_expr()?;
                        fields.push((field_name, field_val));
                        if !self.eat(&Token::Comma) {
                            break;
                        }
                    }
                    self.expect(&Token::RightBrace)?;
                    fields
                } else {
                    Vec::new()
                };
                Expr::MachineEmit { event_name, fields }
            }
            // Contextual keywords that can be used as identifiers in expressions
            tok if Self::contextual_keyword_name(tok).is_some() => {
                let name = Self::contextual_keyword_name(tok).unwrap();
                self.advance();
                Expr::Identifier(name.to_string())
            }
            _ => {
                let found = match self.peek() {
                    Some(tok) => format!("{tok}"),
                    None => "end of file".to_string(),
                };
                self.error_missing_expression(found);
                return None;
            }
        };

        let end = self.peek_span().start;
        Some((expr, start..end))
    }

    fn parse_pipe_lambda(&mut self, is_move: bool, start: usize) -> Option<Expr> {
        let params = if self.eat(&Token::PipePipe) {
            Vec::new()
        } else {
            self.expect(&Token::Pipe)?;
            let params = self.try_parse_pipe_lambda_params().or_else(|| {
                self.error_closure_pipe_syntax(
                    "E_CLOSURE_PIPE_SYNTAX: malformed closure parameter list".to_string(),
                    start..self.peek_span().start,
                    "write parameters as `|name|` or `|name: Type|`",
                );
                None
            })?;
            self.expect(&Token::Pipe).or_else(|| {
                self.error_closure_pipe_syntax(
                    "E_CLOSURE_PIPE_SYNTAX: expected `|` to close closure parameters".to_string(),
                    start..self.peek_span().start,
                    "write `|params| expr`",
                );
                None
            })?;
            params
        };

        let return_type = self.parse_opt_return_type()?;
        let body = if return_type.is_some() {
            if self.peek() != Some(&Token::LeftBrace) {
                self.error_closure_pipe_syntax(
                    "E_CLOSURE_PIPE_SYNTAX: typed pipe closures require a braced body".to_string(),
                    self.peek_span(),
                    "write `|params| -> Type { expr }`",
                );
                return None;
            }
            let body_start = self.peek_span().start;
            let body_block = self.parse_block()?;
            let body_end = self.peek_span().start;
            Box::new((Expr::Block(body_block), body_start..body_end))
        } else {
            if matches!(
                self.peek(),
                Some(Token::Semicolon | Token::RightBrace) | None
            ) {
                self.error_closure_pipe_syntax(
                    "E_CLOSURE_PIPE_SYNTAX: closure body is required".to_string(),
                    self.peek_span(),
                    "write `|| expr` or `|| { ... }`",
                );
                return None;
            }
            Box::new(self.parse_expr()?)
        };

        Some(Expr::Lambda {
            is_move,
            type_params: None,
            params,
            return_type,
            body,
        })
    }

    /// Parse map literal entries after the opening `{` has already been consumed.
    /// Expects at least one `key: value` pair, followed by optional comma-separated pairs.
    fn parse_map_literal_entries(&mut self) -> Option<Expr> {
        let mut entries = Vec::new();
        loop {
            let key = self.parse_expr()?;
            self.expect(&Token::Colon)?;
            let value = self.parse_expr()?;
            entries.push((key, value));

            if !self.eat(&Token::Comma) {
                break;
            }
            if self.peek() == Some(&Token::RightBrace) {
                break; // trailing comma
            }
        }
        self.expect(&Token::RightBrace)?;
        Some(Expr::MapLiteral { entries })
    }

    fn try_parse_pipe_lambda_params(&mut self) -> Option<Vec<LambdaParam>> {
        let mut params = Vec::new();

        while !self.at_end() && self.peek() != Some(&Token::Pipe) {
            let name = self.expect_ident()?;

            let ty = if self.eat(&Token::Colon) {
                Some(self.parse_type()?)
            } else {
                None
            };

            params.push(LambdaParam { name, ty });

            if !self.eat(&Token::Comma) {
                break;
            }
        }

        Some(params)
    }

    fn try_parse_lambda_params(&mut self) -> Option<Vec<LambdaParam>> {
        let mut params = Vec::new();

        while !self.at_end() && self.peek() != Some(&Token::RightParen) {
            let name = self.expect_ident()?;

            let ty = if self.eat(&Token::Colon) {
                Some(self.parse_type()?)
            } else {
                None
            };

            params.push(LambdaParam { name, ty });

            if !self.eat(&Token::Comma) {
                break;
            }
        }

        Some(params)
    }

    #[allow(
        clippy::too_many_lines,
        reason = "single dispatch over all postfix-dot syntactic forms; splitting fragments shared start/cursor state"
    )]
    fn parse_dot_postfix(&mut self, lhs: Spanned<Expr>) -> Option<Spanned<Expr>> {
        let start = lhs.1.start;
        self.advance(); // consume .

        // Handle tuple index: t.0, t.1, etc.
        if let Some(Token::Integer(n)) = self.peek() {
            let field = n.to_string();
            self.advance();
            let end = self.peek_span().start;
            return Some((
                Expr::FieldAccess {
                    object: Box::new(lhs),
                    field,
                },
                start..end,
            ));
        }

        let field = self.expect_ident()?;

        // Accumulate optional `::Segment` pairs after the first identifier.
        // This handles cross-module enum variant construction: `fs.IoError::TimedOut(0)`.
        // Mirrors the DoubleColon accumulation loop in parse_primary's Identifier branch.
        let mut method = field;
        while self.eat(&Token::DoubleColon) {
            if let Some(segment) = self.expect_ident() {
                method = format!("{method}::{segment}");
            } else {
                break;
            }
        }

        if method.contains("::") {
            if let Some(mut name) = Self::dotted_expr_name(&lhs.0) {
                name.push('.');
                name.push_str(&method);
                // A `{` here begins a struct literal only when struct literals
                // are allowed in this position and the brace actually opens a
                // field list. In `if`/`while` condition or `match` scrutinee
                // position the `no_struct_literal` restriction is active, so
                // `if m.E::V { } else { }` opens the then-block — the `{` must
                // NOT be consumed as an empty struct literal here (which would
                // orphan the `else`). The probe also guards `{ stmt; }` blocks.
                if self.peek() == Some(&Token::LeftBrace)
                    && !self.no_struct_literal()
                    && self.probe_struct_init_brace()
                {
                    self.advance(); // consume {
                                    // Inside the struct body the `{` is consumed, so any nested
                                    // bare-ident struct literal is unambiguous again.
                    let (fields, base) =
                        self.with_struct_literals_allowed(Self::parse_struct_init_fields)?;
                    let end = self.peek_span().start;
                    return Some((
                        Expr::StructInit {
                            name,
                            fields,
                            type_args: None,
                            base,
                        },
                        start..end,
                    ));
                }
                let end = self.peek_span().start;
                return Some((Expr::Identifier(name), start..end));
            }
        }

        // Check for method call
        if self.peek() == Some(&Token::LeftParen) {
            self.advance();
            let args = self.parse_call_args()?;

            self.expect(&Token::RightParen)?;
            let end = self.peek_span().start;

            Some((
                Expr::MethodCall {
                    receiver: Box::new(lhs),
                    method,
                    args,
                },
                start..end,
            ))
        } else if self.peek() == Some(&Token::LeftBrace) && method.contains("::") {
            // Module-qualified struct literal: `module.Type::Variant { fields }`.
            //
            // Gate: `::` must have been consumed above — `method` carries the
            // full `Type::Variant` path segment.  A plain field access like
            // `obj.field { ... }` does NOT enter this arm (no `::` in field),
            // preserving the existing parse error / block interpretation for that
            // shape.
            //
            // The receiver (`lhs`) must be a bare `Ident` (single-level module
            // alias).  Nested-module paths (`a.b.Type::Variant`) fall through to
            // FieldAccess; that limit is deferred to v0.5.1.
            //
            // Disambiguate via the shared probe: `{ field: val }` → struct init;
            // `{ stmt; }` or `{ expr }` → not a struct literal, fall through.
            //
            // In `if`/`while` condition or `match` scrutinee position the
            // `no_struct_literal` restriction is active, so `if m.E::V { }` opens
            // the block rather than starting a struct literal — consistent with
            // the bare-identifier form above.
            if !self.no_struct_literal() && self.probe_struct_init_brace() {
                // Build the qualified type name: `module.Type::Variant`.
                // `lhs` is the module identifier; `method` is `Type::Variant`.
                // Non-identifier receivers (e.g. chained `a.b.C::D { }`) fall
                // through to FieldAccess — nested-module paths are out of scope
                // for v0.5; the checker surfaces an error via the field-access
                // path rather than a misleading struct-literal attempt.
                let module_name = if let Expr::Identifier(n) = &lhs.0 {
                    n.clone()
                } else {
                    let end = self.peek_span().start;
                    return Some((
                        Expr::FieldAccess {
                            object: Box::new(lhs),
                            field: method,
                        },
                        start..end,
                    ));
                };
                let qualified_name = format!("{module_name}.{method}");
                self.advance(); // consume {
                let (fields, base) =
                    self.with_struct_literals_allowed(Self::parse_struct_init_body)?;
                let end = self.peek_span().start;
                Some((
                    Expr::StructInit {
                        name: qualified_name,
                        fields,
                        type_args: None,
                        base,
                    },
                    start..end,
                ))
            } else {
                // Brace begins a block, not a struct literal — fall through to
                // FieldAccess.  The brace will be parsed as the next statement.
                let end = self.peek_span().start;
                Some((
                    Expr::FieldAccess {
                        object: Box::new(lhs),
                        field: method,
                    },
                    start..end,
                ))
            }
        } else {
            // Field access (method == field when no :: was consumed; otherwise a
            // unit-variant or bare type-path reference — preserved as FieldAccess).
            let end = self.peek_span().start;
            Some((
                Expr::FieldAccess {
                    object: Box::new(lhs),
                    field: method,
                },
                start..end,
            ))
        }
    }

    fn parse_struct_init_fields(&mut self) -> Option<StructInitFields> {
        let mut fields = Vec::new();
        let mut base: Option<Box<Spanned<Expr>>> = None;
        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            if self.peek() == Some(&Token::DotDot) {
                self.advance(); // consume `..`
                let base_expr = self.parse_expr()?;
                base = Some(Box::new(base_expr));
                self.eat(&Token::Comma);
                if self.peek() != Some(&Token::RightBrace) {
                    self.error(
                        "functional-update `..base` must be the last item in the field list"
                            .to_string(),
                    );
                }
                break;
            }
            let field_name = self.expect_ident()?;
            self.expect(&Token::Colon)?;
            let value = self.parse_expr()?;
            fields.push((field_name, value));

            if !self.eat(&Token::Comma) {
                break;
            }
        }
        self.expect(&Token::RightBrace)?;
        Some((fields, base))
    }

    fn dotted_expr_name(expr: &Expr) -> Option<String> {
        match expr {
            Expr::Identifier(name) => Some(name.clone()),
            Expr::FieldAccess { object, field } => {
                let mut name = Self::dotted_expr_name(&object.0)?;
                name.push('.');
                name.push_str(field);
                Some(name)
            }
            _ => None,
        }
    }

    /// Parse a comma-separated list of call arguments, supporting both
    /// positional (`expr`) and named (`name: expr`) forms.
    ///
    /// Named args must come after all positional args.
    fn parse_call_args(&mut self) -> Option<Vec<CallArg>> {
        // Call arguments sit inside `(...)`, so a struct literal here is
        // unambiguous even in condition position (`if f(Foo { a: 1 }) {…}`).
        let _allow_struct = self.set_no_struct_literal(false);
        let mut args = Vec::new();
        let mut seen_named = false;

        while !self.at_end() && self.peek() != Some(&Token::RightParen) {
            // Check for named arg: identifier followed by colon.
            // We peek at the current token AND the next one to distinguish
            // `name: expr` from a plain expression that starts with an identifier.
            let is_named_arg = self.peek().is_some_and(|t| Self::is_ident_token(t))
                && self.peek_at(self.pos + 1) == Some(&Token::Colon);

            if is_named_arg {
                let name = self.expect_ident()?;
                self.advance(); // consume ':'
                let value = self.parse_expr()?;
                args.push(CallArg::Named { name, value });
                seen_named = true;
                if !self.eat(&Token::Comma) {
                    break;
                }
                continue;
            }

            // Positional argument
            if seen_named {
                self.error("positional arguments must come before named arguments".to_string());
                if self.parse_expr().is_none() {
                    break;
                }
                if !self.eat(&Token::Comma) {
                    break;
                }
                continue;
            }
            match self.parse_expr() {
                Some(expr) => args.push(CallArg::Positional(expr)),
                None => break,
            }
            if !self.eat(&Token::Comma) {
                break;
            }
        }

        Some(args)
    }

    fn parse_call_postfix(&mut self, lhs: Spanned<Expr>) -> Option<Spanned<Expr>> {
        let start = lhs.1.start;
        self.advance(); // consume (

        let args = self.parse_call_args()?;

        self.expect(&Token::RightParen)?;
        let end = self.peek_span().start;

        Some((
            Expr::Call {
                function: Box::new(lhs),
                type_args: None, // postfix calls don't parse type args (ambiguous with <)
                args,
                is_tail_call: false,
            },
            start..end,
        ))
    }

    fn parse_index_postfix(&mut self, lhs: Spanned<Expr>) -> Option<Spanned<Expr>> {
        let start = lhs.1.start;
        self.advance(); // consume [
                        // Index contents sit inside `[...]`, so a struct literal here is
                        // unambiguous even in condition position (`if xs[Foo { a: 1 }.k] {…}`).
        let _allow_struct = self.set_no_struct_literal(false);

        // C-3 range-slice support: detect the five range forms in index
        // position and emit `Expr::Range` (with the inclusive flag) so the
        // checker/HIR can route to slice lowering. Forms:
        //   xs[..]      — both endpoints open
        //   xs[..b]     — open start, closed end (also `..=b`)
        //   xs[a..]     — closed start, open end
        //   xs[a..b]    — both endpoints closed
        //   xs[a..=b]   — closed start, inclusive end
        // All other contents (e.g. `xs[i]`, `xs[f()]`) remain `Expr::Index`.

        let bracket_start_span = self.peek_span();

        // Form 1: leading `..` or `..=` — open start.
        if matches!(self.peek(), Some(Token::DotDot | Token::DotDotEqual)) {
            let inclusive = matches!(self.peek(), Some(Token::DotDotEqual));
            let dotdot_span = self.peek_span();
            self.advance(); // consume `..` or `..=`
                            // After the `..`, either `]` (xs[..] / xs[..=] — the latter is
                            // ill-formed but we accept it as `..` with a closed inclusive flag
                            // and let the checker complain about the missing endpoint via
                            // type inference) or an expression for the closed end.
            let end_expr = if self.peek() == Some(&Token::RightBracket) {
                None
            } else {
                Some(Box::new(self.parse_expr()?))
            };
            self.expect(&Token::RightBracket)?;
            let end_pos = self.peek_span().start;
            let range_span = dotdot_span.start..end_pos;
            let range_expr = (
                Expr::Range {
                    start: None,
                    end: end_expr,
                    inclusive,
                },
                range_span,
            );
            return Some((
                Expr::Index {
                    object: Box::new(lhs),
                    index: Box::new(range_expr),
                },
                start..end_pos,
            ));
        }

        // Parse the start sub-expression with `min_bp = 5` — above the
        // range precedence (3, 4) — so the Pratt loop does NOT fold a
        // trailing `..` / `..=` into a binary range. We then inspect the
        // next token to discriminate `xs[a]` (single-element index) from
        // `xs[a..]` (open-end slice) and `xs[a..b]` / `xs[a..=b]` (closed
        // slice). This lets `xs[a..]` succeed even though the normal
        // Pratt loop would demand a RHS after `..` and bail.
        let first = self.parse_expr_bp(5)?;

        let (inclusive, has_range_op) = match self.peek() {
            Some(Token::DotDot) => (false, true),
            Some(Token::DotDotEqual) => (true, true),
            _ => (false, false),
        };

        if has_range_op {
            // `xs[a..]` / `xs[a..b]` / `xs[a..=b]`.
            let range_start_pos = first.1.start;
            self.advance(); // consume `..` or `..=`
            let end_expr = if self.peek() == Some(&Token::RightBracket) {
                None
            } else {
                // Parse the upper bound at full `parse_expr` precedence
                // so nested expressions (`xs[a..b+1]`) work.
                Some(Box::new(self.parse_expr()?))
            };
            self.expect(&Token::RightBracket)?;
            let end_pos = self.peek_span().start;
            let range_span = range_start_pos..end_pos;
            let range_expr = (
                Expr::Range {
                    start: Some(Box::new(first)),
                    end: end_expr,
                    inclusive,
                },
                range_span,
            );
            return Some((
                Expr::Index {
                    object: Box::new(lhs),
                    index: Box::new(range_expr),
                },
                start..end_pos,
            ));
        }

        // No range operator after the start expression: this is a single-
        // element index (`xs[i]`, `xs[a + b]`, etc.). `parse_expr_bp(5)`
        // already absorbed every infix operator above range precedence,
        // which is the right closure for single-element indexing — range
        // operators in index position go through the branch above.
        let index = first;
        // Silence unused-binding warning: `bracket_start_span` is consulted
        // only by the leading-`..` branch above.
        let _ = bracket_start_span;

        self.expect(&Token::RightBracket)?;
        let end = self.peek_span().start;

        Some((
            Expr::Index {
                object: Box::new(lhs),
                index: Box::new(index),
            },
            start..end,
        ))
    }

    // ── Patterns ──
    fn parse_pattern(&mut self) -> Option<Spanned<Pattern>> {
        let _guard = self.enter_recursion()?;
        let mut result = self.parse_base_pattern()?;

        // Handle OR patterns: `1 | 2 | 3` becomes Or(Or(1, 2), 3)
        while self.peek() == Some(&Token::Pipe) {
            self.advance();
            let right = self.parse_base_pattern()?;
            let span = result.1.start..right.1.end;
            result = (Pattern::Or(Box::new(result), Box::new(right)), span);
        }

        Some(result)
    }

    #[expect(
        clippy::too_many_lines,
        reason = "recursive descent parser requires sequential case handling"
    )]
    fn parse_base_pattern(&mut self) -> Option<Spanned<Pattern>> {
        let _guard = self.enter_recursion()?;
        let start = self.peek_span().start;

        let pattern = match self.peek() {
            Some(Token::Minus)
                if matches!(
                    self.peek_at(self.pos + 1),
                    Some(Token::Integer(_) | Token::Float(_))
                ) =>
            {
                self.advance(); // consume '-'
                let (next, _) = self.advance()?;
                match next {
                    Token::Integer(s) => {
                        if let Ok((val, radix)) = parse_int_literal(s) {
                            Pattern::Literal(Literal::Integer { value: -val, radix })
                        } else {
                            self.error_invalid_literal_with_hint(
                                format!("invalid integer literal '-{s}'"),
                                "integer literals support decimal, 0x hex, 0o octal, and 0b binary",
                            );
                            return None;
                        }
                    }
                    Token::Float(s) => {
                        let cleaned: String = s.chars().filter(|c| *c != '_').collect();
                        if let Ok(val) = cleaned.parse::<f64>() {
                            Pattern::Literal(Literal::Float(-val))
                        } else {
                            self.error_invalid_literal(format!("invalid float literal '-{s}'"));
                            return None;
                        }
                    }
                    _ => unreachable!(),
                }
            }
            // Leading-dot variant pattern: `.Variant`, `.Variant(p, ..)`, or
            // `.Variant { f: p }`. The enum type is left implicit — the
            // type-checker resolves the bare short name against the match
            // scrutinee's type (`resolve_variant_match`), exactly as it does for
            // a leading-dot constructor expression. Fires only when the dot is
            // immediately followed by an identifier, so `..` range patterns
            // (a distinct `Token::DotDot`) are untouched. Emits the SAME
            // `Pattern::Constructor` / `Pattern::Struct` / `Pattern::Identifier`
            // a bare (unqualified) name would, so downstream resolution and HIR
            // lowering need no new pattern variant.
            Some(Token::Dot)
                if matches!(self.peek_at(self.pos + 1), Some(Token::Identifier(_))) =>
            {
                self.advance(); // consume '.'
                let name = self.expect_ident()?;
                if self.eat(&Token::LeftParen) {
                    let mut patterns = Vec::new();
                    while !self.at_end() && self.peek() != Some(&Token::RightParen) {
                        patterns.push(self.parse_pattern()?);
                        if !self.eat(&Token::Comma) {
                            break;
                        }
                    }
                    self.expect(&Token::RightParen)?;
                    Pattern::Constructor { name, patterns }
                } else if self.eat(&Token::LeftBrace) {
                    let mut fields = Vec::new();
                    while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
                        let field_name = self.expect_ident()?;
                        let pattern = if self.eat(&Token::Colon) {
                            Some(self.parse_pattern()?)
                        } else {
                            None
                        };
                        fields.push(PatternField {
                            name: field_name,
                            pattern,
                        });
                        if !self.eat(&Token::Comma) {
                            break;
                        }
                    }
                    self.expect(&Token::RightBrace)?;
                    Pattern::Struct { name, fields }
                } else {
                    Pattern::Identifier(name)
                }
            }
            Some(Token::Identifier(name)) => {
                let mut name = name.to_string();
                self.advance();
                // Wildcard pattern
                if name == "_" {
                    Pattern::Wildcard
                } else {
                    // Handle qualified names like Colour::Red
                    while self.eat(&Token::DoubleColon) {
                        if let Some(segment) = self.expect_ident() {
                            name = format!("{name}::{segment}");
                        } else {
                            break;
                        }
                    }

                    if self.eat(&Token::LeftParen) {
                        // Constructor pattern
                        let mut patterns = Vec::new();
                        while !self.at_end() && self.peek() != Some(&Token::RightParen) {
                            patterns.push(self.parse_pattern()?);
                            if !self.eat(&Token::Comma) {
                                break;
                            }
                        }
                        self.expect(&Token::RightParen)?;
                        Pattern::Constructor { name, patterns }
                    } else if self.eat(&Token::LeftBrace) {
                        // Struct pattern
                        let mut fields = Vec::new();
                        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
                            let field_name = self.expect_ident()?;
                            let pattern = if self.eat(&Token::Colon) {
                                Some(self.parse_pattern()?)
                            } else {
                                None
                            };
                            fields.push(PatternField {
                                name: field_name,
                                pattern,
                            });

                            if !self.eat(&Token::Comma) {
                                break;
                            }
                        }
                        self.expect(&Token::RightBrace)?;
                        Pattern::Struct { name, fields }
                    } else {
                        Pattern::Identifier(name)
                    }
                }
            }
            Some(Token::LeftParen) => {
                self.advance();
                if self.eat(&Token::RightParen) {
                    // Unit pattern - represented as empty tuple
                    Pattern::Tuple(Vec::new())
                } else {
                    let mut patterns = vec![self.parse_pattern()?];
                    while self.eat(&Token::Comma) {
                        if self.peek() == Some(&Token::RightParen) {
                            break;
                        }
                        patterns.push(self.parse_pattern()?);
                    }
                    self.expect(&Token::RightParen)?;

                    if patterns.len() == 1 {
                        // Safe: len == 1 guarantees next() yields one element.
                        return Some(patterns.into_iter().next().unwrap());
                    }
                    Pattern::Tuple(patterns)
                }
            }
            Some(Token::Integer(s)) => {
                if let Ok((val, radix)) = parse_int_literal(s) {
                    self.advance();
                    Pattern::Literal(Literal::Integer { value: val, radix })
                } else {
                    self.error_invalid_literal_with_hint(
                        format!("invalid integer literal '{s}'"),
                        "integer literals support decimal, 0x hex, 0o octal, and 0b binary",
                    );
                    return None;
                }
            }
            Some(Token::StringLit(s)) => {
                let inner = unquote_str(s);
                let tok_start = self.peek_span().start;
                let (unescaped, unescape_errs) = unescape_string(inner);
                for (off, msg) in unescape_errs {
                    let err_start = tok_start + 1 + off;
                    self.errors.push(ParseError {
                        message: msg.to_string(),
                        span: err_start..err_start + 2,
                        hint: None,
                        severity: Severity::Error,
                        kind: ParseDiagnosticKind::InvalidLiteral,
                    });
                }
                self.advance();
                Pattern::Literal(Literal::String(unescaped))
            }
            Some(Token::CharLit(s)) => {
                let inner = s
                    .strip_prefix('\'')
                    .and_then(|s| s.strip_suffix('\''))
                    .unwrap_or(s);
                if let Some(c) = self.parse_char_escape(inner) {
                    self.advance();
                    Pattern::Literal(Literal::Char(c))
                } else {
                    return None;
                }
            }
            Some(Token::RawString(s)) => {
                let s = unquote_str(s).to_string();
                self.advance();
                Pattern::Literal(Literal::String(s))
            }
            Some(Token::True) => {
                self.advance();
                Pattern::Literal(Literal::Bool(true))
            }
            Some(Token::False) => {
                self.advance();
                Pattern::Literal(Literal::Bool(false))
            }
            // Regex literal pattern: re"pattern" in a match arm.
            // `captures` is empty here; the checker populates it from the
            // regex engine's named-capture list after validating the pattern.
            Some(Token::RegexLiteral(s)) => {
                let s = *s;
                let pattern = normalize_regex_literal(s);
                self.advance();
                Pattern::Regex {
                    pattern,
                    captures: vec![],
                }
            }
            // Contextual keywords used as identifiers in patterns
            Some(tok) if Self::contextual_keyword_name(tok).is_some() => {
                let name = Self::contextual_keyword_name(self.peek().unwrap()).unwrap();
                self.advance();
                Pattern::Identifier(name.to_string())
            }
            _ => {
                let found = match self.peek() {
                    Some(tok) => format!("{tok}"),
                    None => "end of file".to_string(),
                };
                self.error_invalid_pattern(found);
                return None;
            }
        };

        let end = self.peek_span().start;
        Some((pattern, start..end))
    }

    fn parse_match_arm(&mut self) -> Option<MatchArm> {
        let pattern = self.parse_pattern()?;

        let guard = if self.eat(&Token::If) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        if self.peek() == Some(&Token::Equal) {
            self.error_with_hint(
                "expected '=>' in match arm, found '='".to_string(),
                "use '=>' (fat arrow) to separate pattern from body",
            );
            self.advance();
        } else {
            self.expect(&Token::FatArrow)?;
        }
        let body = self.parse_expr()?;
        if self.peek() == Some(&Token::RightBrace) {
            self.eat(&Token::Comma); // trailing comma optional on last arm
        } else if Self::is_block_expr(&body.0) {
            self.eat(&Token::Comma);
        } else {
            self.expect(&Token::Comma)?;
        }

        Some(MatchArm {
            pattern,
            guard,
            body,
        })
    }

    fn parse_select_arm(&mut self) -> Option<SelectArm> {
        let binding = self.parse_pattern()?;
        self.expect(&Token::From)?;
        let source = self.parse_expr()?;
        self.expect(&Token::FatArrow)?;
        let body = self.parse_expr()?;
        self.eat(&Token::Comma);

        Some(SelectArm {
            binding,
            source,
            body,
        })
    }

    // ── Struct-literal disambiguation helpers ──────────────────────────────

    /// Speculatively probe whether the `{` at the current position begins a
    /// struct literal rather than a block statement.
    ///
    /// Contract: caller must have peeked `Token::LeftBrace` but NOT yet
    /// consumed it. This method saves, consumes `{`, inspects one token of
    /// lookahead, then restores position — so the caller's position is
    /// unchanged on return.
    ///
    /// Returns `true` when:
    ///   - the brace is immediately followed by `}` (empty struct literal)
    ///   - the brace is followed by `..` (functional-update-only form)
    ///   - the brace is followed by `ident :` (named field)
    ///
    /// Returns `false` otherwise (block beginning with a statement, expression,
    /// keyword, etc.).
    fn probe_struct_init_brace(&mut self) -> bool {
        let saved_pos = self.save_pos();
        self.advance(); // consume {
        let probe = if self.peek() == Some(&Token::RightBrace) {
            // Empty struct literal: Foo {}
            true
        } else if self.peek() == Some(&Token::DotDot) {
            // Functional-update-only form: `Foo { ..base }`.
            true
        } else if self.peek().is_some_and(|tok| Self::is_ident_token(tok)) {
            self.advance();
            self.peek() == Some(&Token::Colon)
        } else {
            false
        };
        self.restore_pos(saved_pos);
        probe
    }

    /// Parse the body of a struct literal after the opening `{` has been
    /// consumed.  Handles named fields, an optional trailing comma, and the
    /// functional-update `..base` tail.
    ///
    /// Returns `(fields, base)` where `fields` is a vec of `(name, expr)`
    /// pairs and `base` is `Some(expr)` when a `..base` suffix was present.
    ///
    /// Returns `None` if parsing fails (error is recorded on `self`).
    #[allow(
        clippy::type_complexity,
        reason = "return tuple encodes (fields, base) for struct literal body; extracting a named type would require a public struct in a private-helper context"
    )]
    fn parse_struct_init_body(
        &mut self,
    ) -> Option<(Vec<(String, Spanned<Expr>)>, Option<Box<Spanned<Expr>>>)> {
        let mut fields = Vec::new();
        let mut base: Option<Box<Spanned<Expr>>> = None;

        while !self.at_end() && self.peek() != Some(&Token::RightBrace) {
            if self.peek() == Some(&Token::DotDot) {
                // `..base_expr` — must be the last item in the list.
                self.advance(); // consume `..`
                let base_expr = self.parse_expr()?;
                base = Some(Box::new(base_expr));
                // Allow an optional trailing comma before `}`.
                self.eat(&Token::Comma);
                if self.peek() != Some(&Token::RightBrace) {
                    self.error(
                        "functional-update `..base` must be the last item in the field list"
                            .to_string(),
                    );
                }
                break;
            }
            let field_name = self.expect_ident()?;
            self.expect(&Token::Colon)?;
            let value = self.parse_expr()?;
            fields.push((field_name, value));

            if !self.eat(&Token::Comma) {
                break;
            }
        }
        self.expect(&Token::RightBrace)?;

        Some((fields, base))
    }
}

// ── Precedence Functions ──

/// Get binding power for infix operators (left, right).
/// Higher numbers = tighter binding.
fn infix_bp(op: &Token) -> Option<(u8, u8)> {
    // Precedence follows Rust's ordering: bitwise ops bind tighter than
    // comparisons, which bind tighter than logical ops.
    match op {
        // Range
        Token::DotDot | Token::DotDotEqual => Some((3, 4)),
        // Logical OR
        Token::PipePipe => Some((5, 6)),
        // Logical AND
        Token::AmpAmp => Some((7, 8)),
        // Equality and identity
        Token::EqualEqual | Token::NotEqual | Token::Is => Some((9, 10)),
        // Relational
        Token::Less | Token::LessEqual | Token::Greater | Token::GreaterEqual => Some((11, 12)),
        // Bitwise OR
        Token::Pipe => Some((13, 14)),
        // Bitwise XOR
        Token::Caret => Some((15, 16)),
        // Bitwise AND
        Token::Ampersand => Some((17, 18)),
        // Shift
        Token::LessLess | Token::GreaterGreater => Some((19, 20)),
        // Additive: plain `+`/`-` and wrapping `&+`/`&-` share precedence 21.
        Token::Plus | Token::Minus | Token::AmpPlus | Token::AmpMinus => Some((21, 22)),
        // Multiplicative: plain `*`/`/`/`%` and wrapping `&*` share precedence 23.
        Token::Star | Token::Slash | Token::Percent | Token::AmpStar => Some((23, 24)),
        _ => None,
    }
}

fn prefix_bp(op: &Token) -> Option<u8> {
    match op {
        // `*expr` is a raw-pointer dereference.  v0.5 parses it only so
        // the type checker can reject it deterministically — no codegen
        // path is reached.  Same binding power as the other unary prefixes.
        Token::Bang | Token::Minus | Token::Tilde | Token::Await | Token::Star => Some(25),
        _ => None,
    }
}

/// Right binding power of the contextual `clone <operand>` prefix.
///
/// Matches the other unary prefixes (`!`, `-`, `~`) so `clone a + b` parses as
/// `(clone a) + b` and a postfix chain binds into the operand: `clone x.f()`
/// clones the result of `x.f()`.
const CLONE_PREFIX_BP: u8 = 25;

/// Whether `tok` begins an operand for the contextual `clone` prefix.
///
/// Restricted to tokens that unambiguously start a fresh primary expression:
/// any identifier or literal. Deliberately excludes `(`, `[`, `.`, `?`, and
/// the infix/unary operator symbols so that `clone(args)` stays a call,
/// `clone.field` / `clone[i]` stay identifier postfixes, and `clone - x` stays
/// subtraction — `clone` remains a usable identifier in every position where
/// it is followed by a continuation rather than a new operand.
fn token_begins_clone_operand(tok: &Token) -> bool {
    matches!(
        tok,
        Token::Identifier(_)
            | Token::Integer(_)
            | Token::Float(_)
            | Token::StringLit(_)
            | Token::CharLit(_)
            | Token::RawString(_)
            | Token::ByteStringLit(_)
            | Token::InterpolatedString(_)
            | Token::RegexLiteral(_)
            | Token::Duration(_)
            | Token::True
            | Token::False
    )
}

fn token_to_binop(token: &Token) -> Option<BinaryOp> {
    match token {
        Token::Plus => Some(BinaryOp::Add),
        Token::Minus => Some(BinaryOp::Subtract),
        Token::Star => Some(BinaryOp::Multiply),
        Token::Slash => Some(BinaryOp::Divide),
        Token::Percent => Some(BinaryOp::Modulo),
        Token::EqualEqual => Some(BinaryOp::Equal),
        Token::NotEqual => Some(BinaryOp::NotEqual),
        Token::Less => Some(BinaryOp::Less),
        Token::LessEqual => Some(BinaryOp::LessEqual),
        Token::Greater => Some(BinaryOp::Greater),
        Token::GreaterEqual => Some(BinaryOp::GreaterEqual),
        Token::AmpAmp => Some(BinaryOp::And),
        Token::PipePipe => Some(BinaryOp::Or),
        Token::Ampersand => Some(BinaryOp::BitAnd),
        Token::Pipe => Some(BinaryOp::BitOr),
        Token::Caret => Some(BinaryOp::BitXor),
        Token::LessLess => Some(BinaryOp::Shl),
        Token::GreaterGreater => Some(BinaryOp::Shr),
        Token::DotDot => Some(BinaryOp::Range),
        Token::DotDotEqual => Some(BinaryOp::RangeInclusive),
        Token::AmpPlus => Some(BinaryOp::WrappingAdd),
        Token::AmpMinus => Some(BinaryOp::WrappingSub),
        Token::AmpStar => Some(BinaryOp::WrappingMul),
        _ => None,
    }
}

// ── Public API ──

/// Parse a Hew source file into an AST with error reporting.
#[must_use]
pub fn parse(source: &str) -> ParseResult {
    let mut parser = Parser::new(source);
    let program = parser.parse_program();
    ParseResult {
        program,
        errors: parser.errors,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_simple_function() {
        let source = "fn main() { let x = 1 + 2; }";
        let result = parse(source);
        assert!(result.errors.is_empty());
        assert_eq!(result.program.items.len(), 1);
    }

    /// Supervisor child declarations accept the module-qualified dotted type
    /// (`child a: bank.Account`), carrying the qualified actor identity
    /// verbatim; bare child types stay the root/local spelling.
    #[test]
    fn parse_supervisor_child_dotted_module_qualified_type() {
        let source = "supervisor S {\n\
                      \x20   strategy: one_for_one;\n\
                      \x20   intensity: 1 within 60s;\n\
                      \n\
                      \x20   child a: bank.Account(n: 1);\n\
                      \x20   child b: Local;\n\
                      }\n";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let Item::Supervisor(sd) = &result.program.items[0].0 else {
            panic!("expected supervisor, got {:?}", result.program.items[0].0);
        };
        assert_eq!(sd.children[0].actor_type, "bank.Account");
        assert_eq!(sd.children[0].args.len(), 1);
        assert_eq!(sd.children[1].actor_type, "Local");
    }

    #[test]
    fn parse_doc_comment_on_function() {
        let source = "/// Adds numbers.\nfn add(a: i32, b: i32) -> i32 { a + b }";
        let result = parse(source);
        assert!(result.errors.is_empty());
        assert_eq!(result.program.items.len(), 1);
        if let Item::Function(f) = &result.program.items[0].0 {
            assert_eq!(f.doc_comment.as_deref(), Some("Adds numbers."));
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn parse_module_doc_comment() {
        let source = "//! Module docs.\n//! Line two.\nfn foo() {}";
        let result = parse(source);
        assert!(result.errors.is_empty());
        assert_eq!(
            result.program.module_doc.as_deref(),
            Some("Module docs.\nLine two.")
        );
    }

    #[test]
    fn parse_no_doc_comment() {
        let source = "fn bare() {}";
        let result = parse(source);
        assert!(result.errors.is_empty());
        if let Item::Function(f) = &result.program.items[0].0 {
            assert!(f.doc_comment.is_none());
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn parse_struct_decl() {
        let source = "type Point { x: i32; y: i32; }";
        let result = parse(source);
        assert!(result.errors.is_empty());
        assert_eq!(result.program.items.len(), 1);
    }

    #[test]
    fn parse_record_named_fields_with_semicolon_emits_hint() {
        let source = "record Point { x: i32; y: i32 }";
        let result = parse(source);
        assert_eq!(result.errors.len(), 1, "errors: {:?}", result.errors);
        assert_eq!(
            result.errors[0].message,
            "expected `,` or `}` after record field, found `;`"
        );
        assert_eq!(
            result.errors[0].hint.as_deref(),
            Some("record fields use commas; write `field: Type,` instead of `field: Type;`")
        );
        assert_eq!(result.program.items.len(), 1);
        let Item::Record(record) = &result.program.items[0].0 else {
            panic!("expected recovered record item");
        };
        let RecordKind::Named(fields) = &record.kind else {
            panic!("expected named record");
        };
        assert_eq!(fields.len(), 2);
    }

    #[test]
    fn parse_actor_decl() {
        let source =
            "actor Counter { var count: i32 = 0; receive fn increment() { count = count + 1; } }";
        let result = parse(source);
        assert!(result.errors.is_empty());
        assert_eq!(result.program.items.len(), 1);
        if let Item::Actor(actor) = &result.program.items[0].0 {
            assert_eq!(actor.fields.len(), 1);
            assert_eq!(actor.fields[0].name, "count");
            assert_eq!(actor.receive_fns.len(), 1);
        } else {
            panic!("expected actor item");
        }
    }

    #[test]
    fn actor_keyword_still_starts_actor_item() {
        let source = "actor ActorPathRegression { receive fn ping() {} }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        assert!(matches!(&result.program.items[0].0, Item::Actor(_)));
    }

    // ---------------------------------------------------------------------------
    // Reserved-word-as-name diagnostics and recovery
    // ---------------------------------------------------------------------------

    /// A `receive fn` whose name is a reserved keyword produces exactly ONE
    /// error that names the keyword and suggests renaming, instead of a
    /// cascade of parse errors for each remaining token in the declaration.
    #[test]
    fn receive_fn_reserved_keyword_name_emits_single_clear_error() {
        let source = "actor Metrics { receive fn record(n: i64) {} }";
        let result = parse(source);
        assert_eq!(
            result.errors.len(),
            1,
            "expected exactly 1 error, got {}: {:?}",
            result.errors.len(),
            result.errors
        );
        let msg = &result.errors[0].message;
        assert!(
            msg.contains("record"),
            "error message must name the reserved word; got: {msg}"
        );
        assert!(
            msg.contains("reserved") || msg.contains("keyword"),
            "error message must say reserved/keyword; got: {msg}"
        );
    }

    /// Variant: `match` as the name of a plain `fn` method inside an actor.
    #[test]
    fn actor_method_reserved_keyword_name_emits_single_clear_error() {
        let source = "actor A { fn match(n: i64) {} }";
        let result = parse(source);
        assert_eq!(
            result.errors.len(),
            1,
            "expected exactly 1 error, got {}: {:?}",
            result.errors.len(),
            result.errors
        );
        let msg = &result.errors[0].message;
        assert!(
            msg.contains("match"),
            "error must name the reserved word; got: {msg}"
        );
    }

    /// Variant: `record` as the name of a top-level `fn` declaration.
    #[test]
    fn toplevel_fn_reserved_keyword_name_emits_single_clear_error() {
        let source = "fn record(n: i64) -> i64 { n }";
        let result = parse(source);
        assert_eq!(
            result.errors.len(),
            1,
            "expected exactly 1 error, got {}: {:?}",
            result.errors.len(),
            result.errors
        );
        let msg = &result.errors[0].message;
        assert!(
            msg.contains("record"),
            "error must name the reserved word; got: {msg}"
        );
    }

    #[test]
    fn toplevel_fn_actor_name_still_reports_reserved_word() {
        let source = "fn actor() {}";
        let result = parse(source);
        assert!(
            result.errors.iter().any(|err| err
                .message
                .contains("`actor` is a reserved word and cannot be used as a name")),
            "expected reserved-word diagnostic for actor function name, got: {:?}",
            result.errors
        );
    }

    /// A valid actor with non-keyword names continues to parse cleanly.
    #[test]
    fn actor_with_non_keyword_receive_fn_name_parses_ok() {
        let source = "actor Metrics { receive fn emit_record(n: i64) {} fn helper() {} }";
        let result = parse(source);
        assert!(
            result.errors.is_empty(),
            "expected no errors, got: {:?}",
            result.errors
        );
        let Item::Actor(actor) = &result.program.items[0].0 else {
            panic!("expected actor");
        };
        assert_eq!(actor.receive_fns.len(), 1);
        assert_eq!(actor.methods.len(), 1);
    }

    /// Recovery: a second valid receive fn after a reserved-keyword name is
    /// still parsed correctly (the error does not swallow the rest of the actor).
    #[test]
    fn reserved_keyword_name_recovery_continues_parsing_sibling_methods() {
        let source = "actor Metrics { receive fn record(n: i64) {} receive fn add(m: i64) {} }";
        let result = parse(source);
        // One error for the bad name, but the actor and sibling method survive.
        assert_eq!(
            result.errors.len(),
            1,
            "expected 1 error, got {}: {:?}",
            result.errors.len(),
            result.errors
        );
        assert_eq!(
            result.program.items.len(),
            1,
            "actor item must still be produced"
        );
        let Item::Actor(actor) = &result.program.items[0].0 else {
            panic!("expected actor");
        };
        // The second (valid) receive fn must be recovered.
        assert_eq!(
            actor.receive_fns.len(),
            1,
            "sibling receive fn `add` must be parsed"
        );
        assert_eq!(actor.receive_fns[0].name, "add");
    }

    #[test]
    fn parse_receive_gen_fn() {
        let source = "actor NumberStream { receive gen fn numbers() -> i32 { yield 1; } }";
        let result = parse(source);
        assert!(result.errors.is_empty());
        if let Item::Actor(actor) = &result.program.items[0].0 {
            assert_eq!(actor.receive_fns.len(), 1);
            assert!(actor.receive_fns[0].is_generator);
        } else {
            panic!("expected actor item");
        }
    }

    #[test]
    fn parse_receive_fn_type_params_and_where_clause() {
        let source = "actor Foo { receive fn bar<T>(x: T) -> T where T: Display { x } }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Actor(actor) = &result.program.items[0].0 {
            assert_eq!(actor.receive_fns.len(), 1);
            let rf = &actor.receive_fns[0];
            assert_eq!(rf.name, "bar");
            let tps = rf.type_params.as_ref().expect("expected type_params");
            assert_eq!(tps.len(), 1);
            assert_eq!(tps[0].name, "T");
            let wc = rf.where_clause.as_ref().expect("expected where_clause");
            assert_eq!(wc.predicates.len(), 1);
            assert_eq!(wc.predicates[0].bounds[0].name, "Display");
        } else {
            panic!("expected actor item");
        }
    }

    #[test]
    fn parse_where_clause_trailing_comma_fn() {
        // A standalone function with a trailing comma after the last predicate.
        let source = "fn foo<T, U>(a: T, b: U) where T: Display, U: Clone, { }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Function(f) = &result.program.items[0].0 {
            let wc = f.where_clause.as_ref().expect("expected where_clause");
            assert_eq!(wc.predicates.len(), 2);
            assert_eq!(wc.predicates[0].bounds[0].name, "Display");
            assert_eq!(wc.predicates[1].bounds[0].name, "Clone");
        } else {
            panic!("expected function item");
        }
    }

    #[test]
    fn parse_where_clause_trailing_comma_receive_fn() {
        // A receive fn with a trailing comma — multiline style inlined here.
        let source =
            "actor Foo { receive fn bar<T, U>(x: T) -> T where T: Display, U: Clone, { x } }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Actor(actor) = &result.program.items[0].0 {
            let rf = &actor.receive_fns[0];
            let wc = rf.where_clause.as_ref().expect("expected where_clause");
            assert_eq!(wc.predicates.len(), 2);
            assert_eq!(wc.predicates[0].bounds[0].name, "Display");
            assert_eq!(wc.predicates[1].bounds[0].name, "Clone");
        } else {
            panic!("expected actor item");
        }
    }

    #[test]
    fn parse_where_clause_single_trailing_comma() {
        // Single predicate with trailing comma is the minimal reproduction case.
        let source = "fn foo<T>(a: T) where T: Display, { }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Function(f) = &result.program.items[0].0 {
            let wc = f.where_clause.as_ref().expect("expected where_clause");
            assert_eq!(wc.predicates.len(), 1);
            assert_eq!(wc.predicates[0].bounds[0].name, "Display");
        } else {
            panic!("expected function item");
        }
    }

    /// Regression: `if` in tail position (the last and only item before `}`,
    /// with no semicolon) must become the function's `trailing_expr`, not a
    /// discarded `Stmt::If`.  The downstream HIR/MIR pipeline reads
    /// `trailing_expr` as the return value; a `Stmt::If` leaves the return
    /// slot uninitialised.
    #[test]
    fn if_in_tail_position_is_trailing_expr() {
        let source = "fn f(n: i64) -> i64 { if n <= 1 { n } else { n + 1 } }";
        let result = parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let Item::Function(func) = &result.program.items[0].0 else {
            panic!("expected function item");
        };
        assert!(
            func.body.stmts.is_empty(),
            "stmts must be empty when `if` is the trailing expr; got {:?}",
            func.body.stmts
        );
        assert!(
            func.body.trailing_expr.is_some(),
            "trailing_expr must be Some(Expr::If {{ .. }}) for a tail-position if"
        );
        assert!(
            matches!(
                func.body.trailing_expr.as_deref(),
                Some((Expr::If { .. }, _))
            ),
            "trailing_expr must be Expr::If, got {:?}",
            func.body.trailing_expr.as_deref().map(|(e, _)| e)
        );
    }

    /// Regression: `match` in tail position must become `trailing_expr`, not
    /// a discarded `Stmt::Match`.
    #[test]
    fn match_in_tail_position_is_trailing_expr() {
        let source = "fn f(n: i64) -> i64 { match n { 0 => 0, _ => n } }";
        let result = parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let Item::Function(func) = &result.program.items[0].0 else {
            panic!("expected function item");
        };
        assert!(
            func.body.stmts.is_empty(),
            "stmts must be empty when `match` is the trailing expr; got {:?}",
            func.body.stmts
        );
        assert!(
            matches!(
                func.body.trailing_expr.as_deref(),
                Some((Expr::Match { .. }, _))
            ),
            "trailing_expr must be Expr::Match"
        );
    }

    /// `if` followed by a semicolon must remain a statement, not a trailing
    /// expression.  The semicolon suppresses tail-position promotion and
    /// causes an "unnecessary semicolon" warning (block-expression forms do
    /// not need `;`).
    #[test]
    fn if_with_semicolon_is_statement_not_trailing() {
        let source = "fn main() { if true { 1 } else { 2 }; }";
        let result = parse(source);
        // The trailing `;` after a block-expression emits an "unnecessary
        // semicolon" warning — that is expected and not a hard error.
        let has_real_error = result.errors.iter().any(|e| e.severity == Severity::Error);
        assert!(
            !has_real_error,
            "unexpected hard errors: {:?}",
            result.errors
        );
        let Item::Function(func) = &result.program.items[0].0 else {
            panic!("expected function item");
        };
        assert!(
            func.body.trailing_expr.is_none(),
            "semicoloned if must not become trailing_expr"
        );
        assert_eq!(
            func.body.stmts.len(),
            1,
            "semicoloned if must produce one statement"
        );
    }

    /// `if` as a non-tail statement (followed by more items) must remain a
    /// statement, not a trailing expression.
    #[test]
    fn if_as_non_tail_statement_stays_statement() {
        let source = "fn main() { if true { 1 } else { 2 } let x = 3; }";
        let result = parse(source);
        let has_real_error = result.errors.iter().any(|e| e.severity == Severity::Error);
        assert!(
            !has_real_error,
            "unexpected hard errors: {:?}",
            result.errors
        );
        let Item::Function(func) = &result.program.items[0].0 else {
            panic!("expected function item");
        };
        // `if` not at tail position → Stmt::If stays in stmts
        assert_eq!(
            func.body.stmts.len(),
            2,
            "non-tail if followed by let must produce two statements"
        );
        assert!(
            func.body.trailing_expr.is_none(),
            "non-tail if must not become trailing_expr"
        );
    }

    #[test]
    fn parse_if_expression() {
        let source = "fn main() { let result = if x > 0 { x } else { -x }; }";
        let result = parse(source);
        if !result.errors.is_empty() {
            for error in &result.errors {
                eprintln!("Error: {} at {:?}", error.message, error.span);
            }
        }
        assert!(result.errors.is_empty());
    }

    #[test]
    fn parse_match_expression() {
        let source = "fn main() { match opt { Some(x) => x, None => 0, } }";
        let result = parse(source);
        assert!(result.errors.is_empty());
    }

    #[test]
    fn parse_match_block_arms_without_commas() {
        let source = "fn main() { match opt { Some(x) => { x } None => { 0 } } }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_match_bad_arm_pattern_recovers_to_later_arms() {
        let source = "fn main() { match n { fn => 0, 1 => 1, _ => 2 } }";
        let result = parse(source);
        assert_eq!(
            result.errors.len(),
            1,
            "bad arm pattern should not cascade: {:?}",
            result.errors
        );
        assert!(
            matches!(
                result.errors[0].kind,
                ParseDiagnosticKind::InvalidPattern { .. }
            ),
            "expected invalid-pattern diagnostic, got: {:?}",
            result.errors
        );
        let Item::Function(function) = &result.program.items[0].0 else {
            panic!("expected function item");
        };
        let Some((Expr::Match { arms, .. }, _)) = function.body.trailing_expr.as_deref() else {
            panic!("expected trailing match expression");
        };
        assert_eq!(arms.len(), 2, "valid later arms should still parse");
    }

    #[test]
    fn parse_lambda() {
        let source = "fn main() { let f = |x: i32| x * 2; }";
        let result = parse(source);
        if !result.errors.is_empty() {
            for error in &result.errors {
                eprintln!("Error: {} at {:?}", error.message, error.span);
            }
        }
        assert!(result.errors.is_empty());
    }

    #[test]
    fn parse_pipe_closure_forms() {
        for source in [
            "fn main() { let f = |x| x + 1; }",
            "fn main() { let f = |x: i32| x + 1; }",
            "fn main() { let f = |x: i32| -> i32 { x + 1 }; }",
            "fn main() { let f = || 42; }",
            "fn main() { let f = move |x| x; }",
        ] {
            let result = parse(source);
            assert!(
                result.errors.is_empty(),
                "expected pipe closure to parse cleanly: {source}\nerrors: {:?}",
                result.errors
            );
        }
    }

    #[test]
    fn parse_pipe_closure_malformed_surfaces_are_explicit_errors() {
        for source in [
            "fn main() { let f = ||; }",
            "fn main() { let f = |x| -> i32 x + 1; }",
        ] {
            let result = parse(source);
            assert!(
                result.errors.iter().any(|error| matches!(
                    error.kind,
                    ParseDiagnosticKind::ClosurePipeSyntax
                ) && error
                    .message
                    .contains("E_CLOSURE_PIPE_SYNTAX")),
                "expected typed E_CLOSURE_PIPE_SYNTAX for {source}, got {:?}",
                result.errors
            );
        }
    }

    #[test]
    fn parse_mutable_function_param() {
        let source = "fn f(var x: int) -> int { x }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        match &result.program.items[0].0 {
            Item::Function(f) => {
                assert_eq!(f.params.len(), 1);
                assert_eq!(f.params[0].name, "x");
                assert!(f.params[0].is_mutable);
            }
            other => panic!("expected function item, got: {other:?}"),
        }
    }

    #[test]
    fn parse_lambda_var_param_is_error() {
        let source = "fn main() { let f = (var x: int) => x; }";
        let result = parse(source);
        assert!(
            !result.errors.is_empty(),
            "expected parse error for lambda var parameter"
        );
        assert!(
            result
                .errors
                .iter()
                .any(|error| error.message.contains("expected expression")),
            "expected expression parse error, got: {:?}",
            result.errors
        );
    }

    #[test]
    fn parse_fibonacci_example() {
        let source = include_str!("../../examples/fibonacci.hew");
        let result = parse(source);
        if !result.errors.is_empty() {
            for error in &result.errors {
                eprintln!("Error: {} at {:?}", error.message, error.span);
            }
        }
        assert!(result.errors.is_empty());
    }

    #[test]
    fn parse_match_or_pattern() {
        let source = "fn classify(n: i32) -> i32 { match n { 1 | 2 | 3 => 1, _ => 0, } }";
        let result = parse(source);
        if !result.errors.is_empty() {
            for error in &result.errors {
                eprintln!("Error: {} at {:?}", error.message, error.span);
            }
        }
        assert!(result.errors.is_empty());
    }

    #[test]
    fn parse_labeled_while_break_continue() {
        let source = r"fn main() -> i32 {
            var i = 0;
            @outer: while i < 5 {
                var j = 0;
                while j < 5 {
                    if j == 3 { break @outer; }
                    j = j + 1;
                }
                i = i + 1;
            }
            0
        }";
        let result = parse(source);
        for e in &result.errors {
            eprintln!("Error: {} at {:?}", e.message, e.span);
        }
        assert!(result.errors.is_empty());

        // Verify the label was parsed
        if let Item::Function(ref f) = result.program.items[0].0 {
            // Third statement should be the labeled while
            if let Stmt::While { ref label, .. } = f.body.stmts[1].0 {
                assert_eq!(label.as_deref(), Some("outer"));
            } else {
                panic!("expected While statement");
            }
        } else {
            panic!("expected Function item");
        }
    }

    #[test]
    fn parse_context_reader_as_identifier_expression() {
        let source = "fn main() -> u64 { @actor_id }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "{:?}", result.errors);
        let Item::Function(function) = &result.program.items[0].0 else {
            panic!("expected function item");
        };
        let Some((Expr::Identifier(name), _)) = function.body.trailing_expr.as_deref() else {
            panic!("expected context reader identifier tail");
        };
        assert_eq!(name, "@actor_id");
    }

    #[test]
    fn parse_labeled_loop() {
        let source = r"fn main() -> i32 {
            @top: loop {
                break @top;
            }
            0
        }";
        let result = parse(source);
        assert!(result.errors.is_empty());
        if let Item::Function(ref f) = result.program.items[0].0 {
            if let Stmt::Loop { ref label, .. } = f.body.stmts[0].0 {
                assert_eq!(label.as_deref(), Some("top"));
            } else {
                panic!("expected Loop statement");
            }
        }
    }

    #[test]
    fn parse_labeled_continue() {
        let source = r"fn main() -> i32 {
            var i = 0;
            @outer: while i < 5 {
                i = i + 1;
                continue @outer;
            }
            0
        }";
        let result = parse(source);
        for e in &result.errors {
            eprintln!("Error: {} at {:?}", e.message, e.span);
        }
        assert!(result.errors.is_empty());
    }

    #[test]
    fn parse_for_await_loop() {
        let source = r"fn main() {
            for await item in stream {
                println(item);
            }
        }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

        if let Item::Function(ref f) = result.program.items[0].0 {
            if let Stmt::For { is_await, .. } = &f.body.stmts[0].0 {
                assert!(*is_await);
            } else {
                panic!("expected For statement");
            }
        } else {
            panic!("expected Function item");
        }
    }

    #[test]
    fn parse_async_fn_is_rejected() {
        // `async fn` has no meaning in Hew — async-ness comes from fork{} context
        // (architecture §4.1, D2 ratification). Only `async gen fn` is accepted.
        // The parser emits a generic "expected 'gen fn' after 'async'" error and
        // returns None, so the item is absent from the parsed program.
        let source = "async fn fetch() -> i32 { 42 }";
        let result = parse(source);
        assert!(
            !result.errors.is_empty(),
            "expected a parse error for bare `async fn`"
        );
        assert!(
            result.errors[0]
                .message
                .contains("expected 'gen fn' after 'async'"),
            "expected rejection diagnostic, got: {:?}",
            result.errors[0].message
        );
    }

    #[test]
    fn parse_async_gen_fn() {
        let source = "async gen fn count_up() -> i32 { yield 1; yield 2; }";
        let result = parse(source);
        for _e in &result.errors {}
        match &result.program.items[0].0 {
            Item::Function(_f) => {}
            _ => panic!("expected Function item"),
        }
    }

    #[test]
    fn parse_pub_async_gen_fn() {
        let source = "pub async gen fn numbers() -> i32 { yield 42; }";
        let result = parse(source);
        match &result.program.items[0].0 {
            Item::Function(_f) => {}
            _ => panic!("expected Function item"),
        }
    }

    #[test]
    fn parse_pattern_underscore_integer() {
        let source = "fn main() { match x { 1_000 => 1, _ => 0, } }";
        let result = parse(source);
        assert!(result.errors.is_empty());
    }

    #[test]
    fn parse_negative_literal_pattern() {
        let source = "fn main() { match x { -1 => 0, _ => 1, } }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

        let Item::Function(func) = &result.program.items[0].0 else {
            panic!("expected function item");
        };
        // A bare `match` as the last item in a block is a trailing expression (value-bearing
        // position), not a `Stmt::Match`.  The block has no `;` after the match and no further
        // items before `}`.
        let Some((Expr::Match { arms, .. }, _)) = func.body.trailing_expr.as_deref() else {
            panic!("expected trailing match expression");
        };
        let (Pattern::Literal(Literal::Integer { value, radix }), _) = &arms[0].pattern else {
            panic!("expected literal integer pattern");
        };
        assert_eq!(*value, -1);
        assert_eq!(*radix, IntRadix::Decimal);
    }

    #[test]
    fn parse_pattern_contextual_keywords() {
        // All contextual keywords that can appear as identifiers in patterns.
        // state/event/on/when/join were previously missing from the inline list.
        let keywords = [
            "after",
            "from",
            "init",
            "child",
            "restart",
            "budget",
            "strategy",
            "permanent",
            "transient",
            "temporary",
            "one_for_one",
            "one_for_all",
            "rest_for_one",
            "wire",
            "optional",
            "deprecated",
            "reserved",
            "state",
            "event",
            "on",
            "when",
            "join",
        ];
        for kw in &keywords {
            let source = format!("fn check(x: i32) -> i32 {{ match x {{ {kw} => 1, _ => 0, }} }}");
            let result = parse(&source);
            assert!(
                result.errors.is_empty(),
                "contextual keyword '{kw}' should be usable as pattern identifier, \
                 but got errors: {:?}",
                result.errors,
            );
            let Item::Function(func) = &result.program.items[0].0 else {
                panic!("expected function for keyword '{kw}'");
            };
            // A bare `match` as the last item in a block is a trailing expression.
            let Some((Expr::Match { arms, .. }, _)) = func.body.trailing_expr.as_deref() else {
                panic!("expected trailing match expression for keyword '{kw}'");
            };
            let (Pattern::Identifier(name), _) = &arms[0].pattern else {
                panic!(
                    "expected identifier pattern for '{kw}', got {:?}",
                    arms[0].pattern
                );
            };
            assert_eq!(name, *kw, "pattern name mismatch for keyword '{kw}'");
        }
    }

    #[test]
    fn parse_lexer_error_reported() {
        // The backtick is not a valid token; it should produce a parse error
        let source = "fn main() { let x = `; }";
        let result = parse(source);
        assert!(result
            .errors
            .iter()
            .any(|e| e.message.contains("unexpected character")));
    }

    #[test]
    fn parse_string_escape_sequences() {
        let source = r#"fn main() -> i32 { let a = "hello\nworld"; let b = "tab\there"; let c = "quote\"end"; let d = "back\\slash"; let e = "null\0byte"; 0 }"#;
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let body = match &result.program.items[0].0 {
            Item::Function(f) => &f.body,
            _ => panic!("expected Function item"),
        };
        let stmts = &body.stmts;
        let get_str = |idx: usize| -> &str {
            if let (
                Stmt::Let {
                    value: Some((Expr::Literal(Literal::String(s)), _)),
                    ..
                },
                _,
            ) = &stmts[idx]
            {
                return s.as_str();
            }
            panic!("expected let with string literal at index {idx}");
        };
        assert_eq!(get_str(0), "hello\nworld");
        assert_eq!(get_str(1), "tab\there");
        assert_eq!(get_str(2), "quote\"end");
        assert_eq!(get_str(3), "back\\slash");
        assert_eq!(get_str(4), "null\0byte");
    }

    #[test]
    fn parse_interpolated_string_contains_expr_part() {
        let result = parse(r#"fn main() { let s = f"hello {name}"; }"#);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let Item::Function(f) = &result.program.items[0].0 else {
            panic!("expected function");
        };
        let Stmt::Let {
            value: Some((Expr::InterpolatedString(parts), _)),
            ..
        } = &f.body.stmts[0].0
        else {
            panic!("expected interpolated string");
        };
        assert!(parts.iter().any(|p| matches!(p, StringPart::Expr(_))));
    }

    #[test]
    fn parse_interpolated_string_with_nested_string_literal() {
        let result = parse(r#"fn main() { let s = f"x={func("a")}"; }"#);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let Item::Function(f) = &result.program.items[0].0 else {
            panic!("expected function");
        };
        let Stmt::Let {
            value: Some((Expr::InterpolatedString(parts), _)),
            ..
        } = &f.body.stmts[0].0
        else {
            panic!("expected interpolated string");
        };
        assert!(
            matches!(parts.as_slice(), [StringPart::Literal(prefix), StringPart::Expr(_)] if prefix == "x=")
        );
    }

    #[test]
    fn parse_interpolated_string_shared_escapes_decode_and_escaped_delimiters_stay_literal() {
        let result = parse(r#"fn main() { let s = f"left \{ \} \x41 {name}"; }"#);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let Item::Function(f) = &result.program.items[0].0 else {
            panic!("expected function");
        };
        let Stmt::Let {
            value: Some((Expr::InterpolatedString(parts), _)),
            ..
        } = &f.body.stmts[0].0
        else {
            panic!("expected interpolated string");
        };

        assert_eq!(parts.len(), 2);
        assert_eq!(parts[0], StringPart::Literal("left { } A ".to_string()));
        assert!(matches!(
            parts[1],
            StringPart::Expr((Expr::Identifier(ref name), _)) if name == "name"
        ));
    }

    #[test]
    fn parse_interpolated_string_empty_expr_reports_error() {
        let result = parse(r#"fn main() { let s = f"hello {}"; }"#);
        assert!(
            !result.errors.is_empty(),
            "expected parse errors for malformed interpolation"
        );
    }

    #[test]
    fn parse_deeply_nested_expr_produces_error() {
        // 300 levels of parenthesized nesting exceeds MAX_DEPTH (256).
        // Use a child thread with an explicit stack size to avoid the test
        // runner's own stack limit being hit before our guard triggers.
        let result = std::thread::Builder::new()
            .stack_size(16 * 1024 * 1024)
            .spawn(|| {
                let open: String = "(".repeat(300);
                let close: String = ")".repeat(300);
                let source = format!("fn main() -> i32 {{ {open}1{close} }}");
                parse(&source)
            })
            .expect("failed to spawn thread")
            .join()
            .expect("thread panicked");

        assert!(
            result
                .errors
                .iter()
                .any(|e| e.message.contains("maximum nesting depth exceeded")),
            "expected nesting depth error, got: {:?}",
            result.errors
        );
    }

    #[test]
    fn parse_error_missing_brace() {
        let source = "fn main() -> i32 {\n    let x = 42;\n    0\n";
        let result = parse(source);
        assert!(
            result.errors.iter().any(|e| e.message.contains("`}`")),
            "expected `}}` error, got: {:?}",
            result.errors
        );
    }

    #[test]
    fn parse_error_unexpected_token() {
        let source = "fn main() {\n    let x = 42 + + + ;\n}";
        let result = parse(source);
        assert!(
            !result.errors.is_empty(),
            "expected parse errors for unexpected tokens"
        );
    }

    #[test]
    fn parse_error_unclosed_string() {
        let source = "fn main() {\n    println(\"hello world);\n}";
        let result = parse(source);
        assert!(
            !result.errors.is_empty(),
            "expected parse errors for unclosed string"
        );
    }

    #[test]
    fn parse_error_missing_expr() {
        let source = "fn main() {\n    let x = ;\n}";
        let result = parse(source);
        assert!(
            result
                .errors
                .iter()
                .any(|e| e.message.contains("expected expression")),
            "expected 'expected expression' error, got: {:?}",
            result.errors
        );
    }

    #[test]
    fn parse_error_missing_semicolon() {
        let source = "fn main() {\n    let x = 42\n    let y = 10;\n}";
        let result = parse(source);
        assert!(
            result.errors.iter().any(|e| e.message.contains("expected")),
            "expected error about missing semicolon, got: {:?}",
            result.errors
        );
    }

    // -----------------------------------------------------------------------
    // Edge case tests: nested generics, chained methods, complex expressions
    // -----------------------------------------------------------------------

    #[test]
    fn parse_nested_generic_types() {
        let source = "fn main() { let v: Vec<Vec<i32>> = Vec::new(); }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_deeply_nested_generics() {
        let source = "fn main() { let v: HashMap<string, Vec<Vec<i32>>> = HashMap::new(); }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_chained_method_calls() {
        let source = "fn main() { a.b().c().d(); }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_chained_methods_with_args() {
        let source = "fn main() { x.filter(1).map(2).collect(); }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_operator_precedence_complex() {
        let source = "fn main() -> i32 { x + y * z - w / v }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        // Verify the trailing expression exists (the complex expression)
        if let Item::Function(f) = &result.program.items[0].0 {
            assert!(f.body.trailing_expr.is_some());
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn parse_mixed_precedence_with_parens() {
        let source = "fn main() -> i32 { (a + b) * (c - d) / e }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_empty_function_body() {
        let source = "fn noop() {}";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Function(f) = &result.program.items[0].0 {
            assert!(f.body.stmts.is_empty());
            assert!(f.body.trailing_expr.is_none());
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn parse_empty_actor_body() {
        let source = "actor Empty {}";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Actor(a) = &result.program.items[0].0 {
            assert!(a.fields.is_empty());
            assert!(a.receive_fns.is_empty());
        } else {
            panic!("expected actor");
        }
    }

    #[test]
    fn parse_actor_lifecycle_hook_and_receive_attributes() {
        // The `terminate { }` block surface was removed in favour of the
        // annotation-based hook surface; cleanup logic is expressed as a
        // plain `fn` annotated with `#[on(stop)]` (and `#[on(start)]` for
        // startup logic).
        let source = r"actor Worker {
    #[on(stop)]
    fn shutdown() { stop(); }

    #[every(50ms)]
    receive fn tick() { work(); }
}";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Actor(actor) = &result.program.items[0].0 {
            assert_eq!(actor.methods.len(), 1);
            assert_eq!(actor.methods[0].attributes.len(), 1);
            assert_eq!(actor.methods[0].attributes[0].name, "on");

            assert_eq!(actor.receive_fns.len(), 1);
            assert_eq!(actor.receive_fns[0].attributes.len(), 1);
            assert_eq!(actor.receive_fns[0].attributes[0].name, "every");
        } else {
            panic!("expected actor");
        }
    }

    #[test]
    fn parse_actor_on_crash_hook_attaches_to_method() {
        // E1: `#[on(crash)]` parses on an actor method. Signature shape
        // (params/return type) is owned by E2 — parser just attaches
        // the attribute to the FnDecl.
        let source = r"actor Worker {
    #[on(crash)]
    fn on_crash() { }
}";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let Item::Actor(actor) = &result.program.items[0].0 else {
            panic!("expected actor");
        };
        assert_eq!(actor.methods.len(), 1);
        assert_eq!(actor.methods[0].attributes.len(), 1);
        assert_eq!(actor.methods[0].attributes[0].name, "on");
        assert_eq!(actor.methods[0].attributes[0].args.len(), 1);
        assert_eq!(actor.methods[0].attributes[0].args[0].as_str(), "crash");
    }

    #[test]
    fn parse_actor_on_upgrade_hook_attaches_to_method() {
        // E1: `#[on(upgrade)]` parses on an actor method. The parser accepts
        // it; the type-checker rejects it as a reserved, unsupported attribute.
        let source = r"actor Worker {
    #[on(upgrade)]
    fn on_upgrade() { }
}";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let Item::Actor(actor) = &result.program.items[0].0 else {
            panic!("expected actor");
        };
        assert_eq!(actor.methods.len(), 1);
        assert_eq!(actor.methods[0].attributes.len(), 1);
        assert_eq!(actor.methods[0].attributes[0].name, "on");
        assert_eq!(actor.methods[0].attributes[0].args.len(), 1);
        assert_eq!(actor.methods[0].attributes[0].args[0].as_str(), "upgrade");
    }

    #[test]
    fn parse_attribute_key_value_missing_value_emits_error_without_empty_fallback() {
        let source = r"
#[meta(rename = , version = 2)]
fn demo() {}
";
        let result = parse(source);
        assert_eq!(result.errors.len(), 1, "errors: {:?}", result.errors);
        assert_eq!(
            result.errors[0].message,
            "invalid value for attribute `rename`: missing value"
        );
        assert_eq!(
            result.errors[0].hint.as_deref(),
            Some("expected identifier, string literal, or integer literal")
        );

        let Item::Function(func) = &result.program.items[0].0 else {
            panic!("expected function");
        };
        assert_eq!(func.attributes.len(), 1);
        assert_eq!(func.attributes[0].name, "meta");
        assert_eq!(func.attributes[0].args.len(), 1);
        assert!(matches!(
            &func.attributes[0].args[0],
            AttributeArg::KeyValue { key, value } if key == "version" && value == "2"
        ));
    }

    #[test]
    fn parse_unicode_in_string_literal() {
        let source = r#"fn main() { let s = "Hello, 世界! 🦀"; }"#;
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Function(f) = &result.program.items[0].0 {
            if let (
                Stmt::Let {
                    value: Some(val), ..
                },
                _,
            ) = &f.body.stmts[0]
            {
                if let (Expr::Literal(Literal::String(s)), _) = val {
                    assert!(s.contains("世界"));
                    assert!(s.contains("🦀"));
                } else {
                    panic!("expected string literal");
                }
            }
        }
    }

    #[test]
    fn parse_empty_string_literal() {
        let source = r#"fn main() { let s = ""; }"#;
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_large_integer_literal() {
        let source = "fn main() -> i64 { 9_223_372_036_854_775_807 }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Function(f) = &result.program.items[0].0 {
            if let Some(boxed) = &f.body.trailing_expr {
                if let (Expr::Literal(Literal::Integer { value: n, .. }), _) = boxed.as_ref() {
                    assert_eq!(*n, i64::MAX);
                } else {
                    panic!("expected integer literal");
                }
            }
        }
    }

    #[test]
    fn parse_hex_integer_literal() {
        let source = "fn main() -> i64 { 0xFF }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Function(f) = &result.program.items[0].0 {
            if let Some(boxed) = &f.body.trailing_expr {
                if let (Expr::Literal(Literal::Integer { value: n, radix }), _) = boxed.as_ref() {
                    assert_eq!(*n, 255);
                    assert_eq!(*radix, IntRadix::Hex);
                } else {
                    panic!("expected integer literal");
                }
            }
        }
    }

    #[test]
    fn parse_binary_integer_literal() {
        let source = "fn main() -> i64 { 0b1010 }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Function(f) = &result.program.items[0].0 {
            if let Some(boxed) = &f.body.trailing_expr {
                if let (Expr::Literal(Literal::Integer { value: n, radix }), _) = boxed.as_ref() {
                    assert_eq!(*n, 10);
                    assert_eq!(*radix, IntRadix::Binary);
                } else {
                    panic!("expected integer literal");
                }
            }
        }
    }

    #[test]
    fn parse_octal_integer_literal() {
        let source = "fn main() -> i64 { 0o77 }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Function(f) = &result.program.items[0].0 {
            if let Some(boxed) = &f.body.trailing_expr {
                if let (Expr::Literal(Literal::Integer { value: n, radix }), _) = boxed.as_ref() {
                    assert_eq!(*n, 63);
                    assert_eq!(*radix, IntRadix::Octal);
                } else {
                    panic!("expected integer literal");
                }
            }
        }
    }

    #[test]
    fn parse_multiple_items() {
        let source = "fn foo() {} fn bar() {} type Baz {}";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        assert_eq!(result.program.items.len(), 3);
    }

    #[test]
    fn parse_empty_program() {
        let result = parse("");
        assert!(result.errors.is_empty());
        assert!(result.program.items.is_empty());
    }

    #[test]
    fn parse_nested_if_else() {
        let source = "fn main() -> i32 { if a > 0 { if b > 0 { 1 } else { 2 } } else { 3 } }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_match_with_struct_pattern() {
        let source = "fn main() { match p { Point { x, y } => x + y, _ => 0, } }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_tuple_expression() {
        let source = "fn main() { let t = (1, 2, 3); }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_array_expression() {
        let source = "fn main() { let a = [1, 2, 3]; }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_unary_operators() {
        let source = "fn main() { let a = -x; let b = !flag; }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    /// Extract the initializer expression of the first `let` in the first
    /// function body. Panics if the shape does not match — tests want a loud
    /// failure when the AST drifts.
    fn first_let_value(result: &ParseResult) -> &Expr {
        let Item::Function(f) = &result.program.items[0].0 else {
            panic!("expected first item to be a function");
        };
        let Stmt::Let { value, .. } = &f.body.stmts[0].0 else {
            panic!("expected first statement to be a `let`");
        };
        &value.as_ref().expect("let initializer present").0
    }

    #[test]
    fn parse_clone_prefix_expression() {
        let source = "fn main() { let a = clone x; }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        match first_let_value(&result) {
            Expr::Clone(operand) => {
                assert!(
                    matches!(&operand.0, Expr::Identifier(name) if name == "x"),
                    "clone operand should be identifier `x`, got: {:?}",
                    operand.0
                );
            }
            other => panic!("expected Expr::Clone, got: {other:?}"),
        }
    }

    #[test]
    fn parse_clone_prefix_takes_whole_postfix_chain() {
        // `clone x.field` must clone the field access, not `(clone x).field`.
        let source = "fn main() { let a = clone x.field; }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        match first_let_value(&result) {
            Expr::Clone(operand) => assert!(
                matches!(&operand.0, Expr::FieldAccess { field, .. } if field == "field"),
                "clone operand should be the field access, got: {:?}",
                operand.0
            ),
            other => panic!("expected Expr::Clone wrapping a field access, got: {other:?}"),
        }
    }

    #[test]
    fn parse_clone_prefix_binds_below_binary() {
        // `clone x + y` is `(clone x) + y`, matching other unary prefixes.
        let source = "fn main() { let a = clone x + y; }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        match first_let_value(&result) {
            Expr::Binary { left, op, .. } => {
                assert_eq!(*op, BinaryOp::Add);
                assert!(
                    matches!(&left.0, Expr::Clone(_)),
                    "left of `+` should be the clone, got: {:?}",
                    left.0
                );
            }
            other => panic!("expected Expr::Binary with a clone on the left, got: {other:?}"),
        }
    }

    #[test]
    fn parse_clone_call_is_not_a_prefix() {
        // `clone(x)` stays a call to a function named `clone`; the contextual
        // prefix only triggers when an operand token (not `(`) follows.
        let source = "fn main() { let a = clone(x); }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        match first_let_value(&result) {
            Expr::Call { function, .. } => assert!(
                matches!(&function.0, Expr::Identifier(name) if name == "clone"),
                "expected a call to `clone`, got: {:?}",
                function.0
            ),
            other => panic!("expected Expr::Call, got: {other:?}"),
        }
    }

    #[test]
    fn parse_clone_as_identifier_still_works() {
        // `clone` is not a reserved word: usable as a binding and in operator
        // position when not followed by an operand token.
        let source = "fn main() { let clone = 5; let y = clone + 1; }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_ampersand_prefix_is_rejected() {
        let source = "fn main() { let y = &x; }";
        let result = parse(source);
        assert!(
            !result.errors.is_empty(),
            "expected a parse error for prefix `&`"
        );
        let err = &result.errors[0];
        assert!(
            err.message.contains("not a prefix operator"),
            "message should explain `&` is not a prefix operator, got: {}",
            err.message
        );
        assert!(
            err.hint.as_deref().is_some_and(|h| h.contains("clone")),
            "hint should point at `clone`, got: {:?}",
            err.hint
        );
    }

    #[test]
    fn parse_comparison_chain() {
        let source = "fn main() -> bool { a < b && b > c || d == e && f != g }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_import_statement() {
        let source = "import std::fs;";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Import(imp) = &result.program.items[0].0 {
            assert_eq!(imp.path, vec!["std", "fs"]);
        } else {
            panic!("expected import");
        }
    }

    #[test]
    fn parse_import_actor_path_segment() {
        let source = "import std::actor::monitor;";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Import(imp) = &result.program.items[0].0 {
            assert_eq!(imp.path, vec!["std", "actor", "monitor"]);
        } else {
            panic!("expected import");
        }
    }

    #[test]
    fn parse_trait_declaration() {
        let source = "trait Printable { fn print(self); }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_bare_self_in_free_fn_is_error() {
        let source = "fn print(self) {}";
        let result = parse(source);
        assert!(
            !result.errors.is_empty(),
            "expected parse error for bare `self` in free function"
        );
        assert!(
            result.errors[0]
                .message
                .contains("not a valid parameter name"),
            "error message should mention self is not a valid parameter name, got: {}",
            result.errors[0].message,
        );
    }

    #[test]
    fn parse_typed_self_is_error() {
        let source = "type Foo { x: int } impl Foo { fn bar(self: Foo) -> int { 0 } }";
        let result = parse(source);
        assert!(
            !result.errors.is_empty(),
            "expected parse error for `self: Type` parameter"
        );
        assert!(
            result.errors[0]
                .message
                .contains("not a valid parameter name"),
            "error message should mention self is not a valid parameter name, got: {}",
            result.errors[0].message,
        );
    }

    #[test]
    fn parse_impl_method_bare_self_receiver() {
        let source = "type Foo { x: int } impl Foo { fn bar(self) -> int { self.x } }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_error_duplicate_keyword() {
        let source = "fn fn main() {}";
        let result = parse(source);
        assert!(
            !result.errors.is_empty(),
            "expected parse error for duplicate fn keyword"
        );
    }

    // Duration literal parsing
    #[test]
    fn parse_duration_literals() {
        let source = "fn main() { let a = 100ms; let b = 5s; let c = 1m; let d = 2h; }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Function(f) = &result.program.items[0].0 {
            let stmts = &f.body.stmts;
            assert_eq!(stmts.len(), 4);
            // 100ms → 100_000_000
            if let Stmt::Let {
                value: Some((Expr::Literal(Literal::Duration(ns)), _)),
                ..
            } = &stmts[0].0
            {
                assert_eq!(*ns, 100_000_000);
            } else {
                panic!("expected Duration literal for 100ms");
            }
            // 5s → 5_000_000_000
            if let Stmt::Let {
                value: Some((Expr::Literal(Literal::Duration(ns)), _)),
                ..
            } = &stmts[1].0
            {
                assert_eq!(*ns, 5_000_000_000);
            } else {
                panic!("expected Duration literal for 5s");
            }
            // 1m → 60_000_000_000
            if let Stmt::Let {
                value: Some((Expr::Literal(Literal::Duration(ns)), _)),
                ..
            } = &stmts[2].0
            {
                assert_eq!(*ns, 60_000_000_000);
            } else {
                panic!("expected Duration literal for 1m");
            }
            // 2h → 7_200_000_000_000
            if let Stmt::Let {
                value: Some((Expr::Literal(Literal::Duration(ns)), _)),
                ..
            } = &stmts[3].0
            {
                assert_eq!(*ns, 7_200_000_000_000);
            } else {
                panic!("expected Duration literal for 2h");
            }
        } else {
            panic!("expected function");
        }
    }

    #[test]
    fn parse_foreign_block() {
        // `foreign` is no longer accepted — only `extern` is valid
        let source = "foreign { fn ext_add(a: i32, b: i32) -> i32; fn ext_print(msg: string); }";
        let result = parse(source);
        assert!(
            result
                .errors
                .iter()
                .any(|e| e.message.contains("unexpected 'foreign'")
                    && e.hint
                        .as_deref()
                        .is_some_and(|h| h.contains("use 'extern'"))),
            "expected foreign rejection error, got: {:?}",
            result.errors
        );

        // Verify that `extern` still works for the same purpose
        let source =
            "extern \"C\" { fn ext_add(a: i32, b: i32) -> i32; fn ext_print(msg: string); }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        assert_eq!(result.program.items.len(), 1);
    }

    // ── extern "rt" block tests ──────────────────────────────────────────

    #[test]
    fn extern_rt_block_empty() {
        let result = parse("extern \"rt\" { }");
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        assert_eq!(result.program.items.len(), 1);
        let Item::ExternBlock(block) = &result.program.items[0].0 else {
            panic!("expected ExternBlock");
        };
        assert_eq!(block.abi, "rt");
        assert!(block.functions.is_empty());
    }

    #[test]
    fn extern_rt_block_single_fn() {
        let result = parse("extern \"rt\" { fn println(s: string); }");
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        assert_eq!(result.program.items.len(), 1);
        let Item::ExternBlock(block) = &result.program.items[0].0 else {
            panic!("expected ExternBlock");
        };
        assert_eq!(block.abi, "rt");
        assert_eq!(block.functions.len(), 1);
        assert_eq!(block.functions[0].name, "println");
        assert_eq!(block.functions[0].params.len(), 1);
        assert_eq!(block.functions[0].params[0].name, "s");
    }

    #[test]
    fn extern_rt_block_multiple_fns() {
        let result = parse(
            "extern \"rt\" { fn println(s: string); fn print(s: string); fn assert(cond: bool); }",
        );
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let Item::ExternBlock(block) = &result.program.items[0].0 else {
            panic!("expected ExternBlock");
        };
        assert_eq!(block.abi, "rt");
        assert_eq!(block.functions.len(), 3);
        assert_eq!(block.functions[0].name, "println");
        assert_eq!(block.functions[1].name, "print");
        assert_eq!(block.functions[2].name, "assert");
    }

    #[test]
    fn extern_rt_block_fn_with_body_rejected() {
        // Bodies are forbidden in extern blocks (any ABI): the parser expects `;`
        // after the parameter list. A `{` in its place produces a parse error.
        let result = parse("extern \"rt\" { fn println(s: string) { todo() } }");
        assert!(
            result
                .errors
                .iter()
                .any(|e| e.message.contains("expected `;`")),
            "expected a 'expected `;`' error, got: {:?}",
            result.errors
        );
    }

    #[test]
    fn extern_unknown_abi_accepted_at_parse_level() {
        // The parser is ABI-agnostic; unknown ABI strings parse successfully.
        // Rejection of unsupported ABIs is deferred to the type-checker.
        let result = parse("extern \"xyz\" { fn foo(); }");
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let Item::ExternBlock(block) = &result.program.items[0].0 else {
            panic!("expected ExternBlock");
        };
        assert_eq!(block.abi, "xyz");
        assert_eq!(block.functions.len(), 1);
        assert_eq!(block.functions[0].name, "foo");
    }

    #[test]
    fn parse_timeout_combinator() {
        let source = "fn main() { let r = foo() | after 5000; }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        // Check the expression is Timeout wrapping a call
        let stmt = &result.program.items[0];
        if let Item::Function(f) = &stmt.0 {
            if let (
                Stmt::Let {
                    value: Some(val), ..
                },
                _,
            ) = &f.body.stmts[0]
            {
                assert!(
                    matches!(val.0, Expr::Timeout { .. }),
                    "expected Timeout, got {:?}",
                    val.0
                );
            } else {
                panic!("expected let binding");
            }
        } else {
            panic!("expected FnDecl");
        }
    }
    // -- Import aliasing --

    #[test]
    fn parse_import_alias() {
        let source = r"import std::net::{http as h, websocket as ws};";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Import(imp) = &result.program.items[0].0 {
            assert_eq!(imp.path, vec!["std", "net"]);
            if let Some(ImportSpec::Names(names)) = &imp.spec {
                assert_eq!(names.len(), 2);
                assert_eq!(names[0].name, "http");
                assert_eq!(names[0].alias.as_deref(), Some("h"));
                assert_eq!(names[1].name, "websocket");
                assert_eq!(names[1].alias.as_deref(), Some("ws"));
            } else {
                panic!("expected ImportSpec::Names, got {:?}", imp.spec);
            }
        } else {
            panic!("expected import item");
        }
    }

    #[test]
    fn parse_import_alias_single() {
        let source = r"import mymod::{foo as bar};";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Import(imp) = &result.program.items[0].0 {
            if let Some(ImportSpec::Names(names)) = &imp.spec {
                assert_eq!(names.len(), 1);
                assert_eq!(names[0].name, "foo");
                assert_eq!(names[0].alias.as_deref(), Some("bar"));
            } else {
                panic!("expected ImportSpec::Names");
            }
        } else {
            panic!("expected import item");
        }
    }

    #[test]
    fn parse_import_whole_module_alias() {
        // `import path::to::mod as alias;` sets module_alias; spec stays None.
        let source = r"import std::net as n;";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Import(imp) = &result.program.items[0].0 {
            assert_eq!(imp.path, vec!["std", "net"]);
            assert!(imp.spec.is_none(), "whole-module alias keeps spec None");
            assert_eq!(imp.module_alias.as_deref(), Some("n"));
        } else {
            panic!("expected import item");
        }
    }

    #[test]
    fn parse_import_no_module_alias_is_none() {
        // A plain whole-module import leaves module_alias unset.
        let source = r"import std::net;";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Import(imp) = &result.program.items[0].0 {
            assert!(imp.module_alias.is_none());
        } else {
            panic!("expected import item");
        }
    }

    #[test]
    fn parse_import_alias_of_brace_rejected() {
        // Aliasing a `::{ }` spec has no meaning and is a parse error.
        let source = r"import mymod::{ Foo } as f;";
        let result = parse(source);
        assert!(
            !result.errors.is_empty(),
            "expected parse error for aliasing a brace import"
        );
    }

    #[test]
    fn parse_import_alias_of_glob_rejected() {
        // Aliasing a glob has no meaning and is a parse error.
        let source = r"import mymod::* as g;";
        let result = parse(source);
        assert!(
            !result.errors.is_empty(),
            "expected parse error for aliasing a glob import"
        );
    }

    #[test]
    fn parse_import_no_alias_preserves_name() {
        // Names without `as` should have alias = None
        let source = r"import mymod::{foo, bar};";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Import(imp) = &result.program.items[0].0 {
            if let Some(ImportSpec::Names(names)) = &imp.spec {
                assert_eq!(names.len(), 2);
                assert_eq!(names[0].name, "foo");
                assert!(names[0].alias.is_none());
                assert_eq!(names[1].name, "bar");
                assert!(names[1].alias.is_none());
            } else {
                panic!("expected ImportSpec::Names");
            }
        } else {
            panic!("expected import item");
        }
    }

    #[test]
    fn parse_import_bare_colons_rejected() {
        // `import foo::;` is syntactically invalid — `::` must be followed by `*` or `{`
        let source = r"import foo::;";
        let result = parse(source);
        assert!(
            !result.errors.is_empty(),
            "expected parse error for `import foo::;`"
        );
    }

    #[test]
    fn parse_import_glob() {
        let source = r"import utils::*;";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        if let Item::Import(imp) = &result.program.items[0].0 {
            assert_eq!(imp.spec, Some(ImportSpec::Glob));
        } else {
            panic!("expected import item");
        }
    }

    #[test]
    fn parse_float_with_underscore_separators() {
        let source = "fn main() { let x = 1_000.5; }";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }

    #[test]
    fn parse_raw_string_literal() {
        let source = r#"fn main() { let x = r"hello\nworld"; }"#;
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
    }
    #[test]
    fn test_hex_escape_in_string() {
        // \x41 = 'A', \x42 = 'B'
        assert_eq!(unescape_string(r"\x41\x42").0, "AB");
        // Mixed with normal text and other escapes
        assert_eq!(unescape_string(r"hi\x21\n").0, "hi!\n");
        // Invalid hex digits preserved as-is
        assert_eq!(unescape_string(r"\xZZ").0, "\\xZZ");
        // Truncated hex escape (only one char after \x)
        assert_eq!(unescape_string("\\x4").0, "\\x4");
    }

    #[test]
    fn parse_visibility_modifiers() {
        use crate::ast::Visibility;

        // pub fn → Visibility::Pub
        let r = parse("pub fn foo() {}");
        assert!(r.errors.is_empty(), "errors: {:?}", r.errors);
        if let Item::Function(f) = &r.program.items[0].0 {
            assert_eq!(f.visibility, Visibility::Pub);
        } else {
            panic!("expected function");
        }

        // pub(package) fn → Visibility::PubPackage
        let r = parse("pub(package) fn bar() {}");
        assert!(r.errors.is_empty(), "errors: {:?}", r.errors);
        if let Item::Function(f) = &r.program.items[0].0 {
            assert_eq!(f.visibility, Visibility::PubPackage);
        } else {
            panic!("expected function");
        }

        // pub(super) fn → Visibility::PubSuper
        let r = parse("pub(super) fn baz() {}");
        assert!(r.errors.is_empty(), "errors: {:?}", r.errors);
        if let Item::Function(f) = &r.program.items[0].0 {
            assert_eq!(f.visibility, Visibility::PubSuper);
        } else {
            panic!("expected function");
        }

        // fn (no pub) → Visibility::Private
        let r = parse("fn private() {}");
        assert!(r.errors.is_empty(), "errors: {:?}", r.errors);
        if let Item::Function(f) = &r.program.items[0].0 {
            assert_eq!(f.visibility, Visibility::Private);
        } else {
            panic!("expected function");
        }

        // pub(package) type → Visibility::PubPackage
        let r = parse("pub(package) type Point { x: i32; y: i32 }");
        assert!(r.errors.is_empty(), "errors: {:?}", r.errors);
        if let Item::TypeDecl(t) = &r.program.items[0].0 {
            assert_eq!(t.visibility, Visibility::PubPackage);
        } else {
            panic!("expected type decl");
        }

        // pub(super) const → Visibility::PubSuper
        let r = parse("pub(super) const X: i32 = 1;");
        assert!(r.errors.is_empty(), "errors: {:?}", r.errors);
        if let Item::Const(c) = &r.program.items[0].0 {
            assert_eq!(c.visibility, Visibility::PubSuper);
        } else {
            panic!("expected const decl");
        }
    }
    #[test]
    fn parse_generic_lambda_removed_emits_typed_diagnostic() {
        // Generic lambda `<T>(params) => body` was removed in v0.5.
        // The parser must emit a typed E_CLOSURE_PIPE_SYNTAX diagnostic,
        // not silently accept or produce a cryptic error.
        for source in [
            "fn main() { let id = <T>(x: T) => x; }",
            "fn main() { let add = <T: Add>(x: T, y: T) => x + y; }",
            "fn main() { let id = <T>(x: T) -> T => x; }",
        ] {
            let result = parse(source);
            assert!(
                result.errors.iter().any(|e| matches!(
                    e.kind,
                    ParseDiagnosticKind::ClosurePipeSyntax
                ) && e.message.contains("E_CLOSURE_PIPE_SYNTAX")),
                "expected typed E_CLOSURE_PIPE_SYNTAX for removed generic lambda: {source}\ngot: {:?}",
                result.errors
            );
        }
    }

    #[test]
    fn parse_paren_lambda_removed_emits_typed_diagnostic() {
        // Parenthesized `(params) => body` was removed in v0.5.
        // The parser must emit a typed E_CLOSURE_PIPE_SYNTAX diagnostic.
        for source in [
            "fn main() { let f = (x) => x; }",
            "fn main() { let f = (x: i32) => x; }",
            "fn main() { let f = (x: i32) -> i32 => x; }",
            "fn main() { let f = move (x) => x; }",
        ] {
            let result = parse(source);
            assert!(
                result.errors.iter().any(|e| matches!(
                    e.kind,
                    ParseDiagnosticKind::ClosurePipeSyntax
                ) && e.message.contains("E_CLOSURE_PIPE_SYNTAX")),
                "expected typed E_CLOSURE_PIPE_SYNTAX for removed paren lambda: {source}\ngot: {:?}",
                result.errors
            );
        }
    }

    #[test]
    fn parse_gen_block_expression() {
        // `gen { yield ...; }` in expression position.
        let source = "fn main() { let g = gen { yield 1; yield 2; }; }";
        let result = parse(source);
        assert!(
            result.errors.is_empty(),
            "expected gen block to parse cleanly: {source}\nerrors: {:?}",
            result.errors
        );
        if let Item::Function(f) = &result.program.items[0].0 {
            if let Stmt::Let {
                value: Some((Expr::GenBlock { .. }, _)),
                ..
            } = &f.body.stmts[0].0
            {
                // Correct: let binding holds a GenBlock
            } else {
                panic!("expected let with GenBlock, got: {:?}", f.body.stmts[0].0);
            }
        } else {
            panic!("expected function item");
        }
    }

    #[test]
    fn parse_gen_block_empty_body() {
        // Empty gen block is syntactically valid (checker will reject it without item type).
        let source = "fn main() { let g = gen {}; }";
        let result = parse(source);
        assert!(
            result.errors.is_empty(),
            "expected empty gen block to parse cleanly: {source}\nerrors: {:?}",
            result.errors
        );
    }

    #[test]
    fn parse_gen_without_block_emits_diagnostic() {
        // `gen` without a following brace must emit a diagnostic.
        let source = "fn main() { let g = gen 42; }";
        let result = parse(source);
        assert!(
            !result.errors.is_empty(),
            "expected error for `gen` without block"
        );
        assert!(
            result
                .errors
                .iter()
                .any(|e| e.message.contains("E_GEN_BLOCK_SYNTAX")),
            "expected E_GEN_BLOCK_SYNTAX diagnostic, got: {:?}",
            result.errors
        );
    }

    #[test]
    fn parse_wire_struct_preserves_since_modifier() {
        let source = "\
#[wire]
struct Msg {
    added: String @2 optional since 2 json(\"added\"),
}
";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

        let Item::TypeDecl(decl) = &result.program.items[0].0 else {
            panic!("expected type declaration");
        };
        let wire = decl.wire.as_ref().expect("expected wire metadata");
        let meta = &wire.field_meta[0];
        assert_eq!(meta.field_number, 2);
        assert!(meta.is_optional);
        assert_eq!(meta.json_name.as_deref(), Some("added"));
        assert_eq!(meta.since, Some(2));
    }

    #[test]
    fn wire_struct_field_metadata_preserves_number_and_outer_naming_cases() {
        let source = "\
#[wire]
#[json(\"camelCase\")]
#[yaml(\"snake_case\")]
struct Msg {
    added: String @2 optional yaml(\"added_name\"),
}
";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

        let Item::TypeDecl(decl) = &result.program.items[0].0 else {
            panic!("expected type declaration");
        };
        let wire = decl.wire.as_ref().expect("expected wire metadata");
        assert_eq!(wire.json_case, Some(NamingCase::CamelCase));
        assert_eq!(wire.yaml_case, Some(NamingCase::SnakeCase));

        let meta = &wire.field_meta[0];
        assert_eq!(meta.field_number, 2);
        assert!(meta.is_optional);
        assert_eq!(meta.yaml_name.as_deref(), Some("added_name"));
    }

    /// `#[wire] enum E { A; B; C; }` — unit-only variants attach wire metadata
    /// at the type level (version / naming-cases) and produce
    /// `Item::TypeDecl { kind: Enum, wire: Some(_) }`.  Variants carry no
    /// per-field tag numbers (variants are tagged by index, not `@N`).
    #[test]
    fn parse_wire_enum_unit_variants() {
        let source = "\
#[wire]
enum Command {
    Start;
    Stop;
    Pause;
}
";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

        let Item::TypeDecl(decl) = &result.program.items[0].0 else {
            panic!("expected type declaration");
        };
        assert_eq!(decl.kind, TypeDeclKind::Enum);
        assert_eq!(decl.name, "Command");
        let wire = decl.wire.as_ref().expect("expected wire metadata on enum");
        assert!(wire.field_meta.is_empty(), "enum variants have no @N tags");
        assert!(wire.reserved_numbers.is_empty());
        assert_eq!(decl.body.len(), 3);
        for item in &decl.body {
            match item {
                TypeBodyItem::Variant(v) => assert!(matches!(v.kind, VariantKind::Unit)),
                _ => panic!("expected variant, got {item:?}"),
            }
        }
    }

    /// `#[wire] enum E { V1 { x: i64 }; V2 { y: String } }` — struct-payload
    /// variants parse through the shared enum-body helper; wire metadata at
    /// the type level is attached by `parse_wire_enum`.
    #[test]
    fn parse_wire_enum_struct_payload_variants() {
        let source = "\
#[wire(version = 2, min_version = 1)]
enum Packet {
    V1 { x: i64 };
    V2 { y: String, z: bool };
}
";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

        let Item::TypeDecl(decl) = &result.program.items[0].0 else {
            panic!("expected type declaration");
        };
        assert_eq!(decl.kind, TypeDeclKind::Enum);
        let wire = decl.wire.as_ref().expect("expected wire metadata on enum");
        assert_eq!(wire.version, Some(2));
        assert_eq!(wire.min_version, Some(1));
        assert_eq!(decl.body.len(), 2);
        let TypeBodyItem::Variant(v1) = &decl.body[0] else {
            panic!("expected variant V1");
        };
        assert_eq!(v1.name, "V1");
        assert!(matches!(v1.kind, VariantKind::Struct(ref fs) if fs.len() == 1));
        let TypeBodyItem::Variant(v2) = &decl.body[1] else {
            panic!("expected variant V2");
        };
        assert_eq!(v2.name, "V2");
        assert!(matches!(v2.kind, VariantKind::Struct(ref fs) if fs.len() == 2));
    }

    /// `#[wire] enum E { A(i64); B(String, bool) }` — tuple-payload variants.
    #[test]
    fn parse_wire_enum_tuple_payload_variants() {
        let source = "\
#[wire]
enum Op {
    Push(i64);
    Pair(String, bool);
}
";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

        let Item::TypeDecl(decl) = &result.program.items[0].0 else {
            panic!("expected type declaration");
        };
        assert_eq!(decl.kind, TypeDeclKind::Enum);
        let wire = decl.wire.as_ref().expect("expected wire metadata on enum");
        assert!(wire.field_meta.is_empty());
        assert_eq!(decl.body.len(), 2);
        let TypeBodyItem::Variant(push) = &decl.body[0] else {
            panic!("expected variant Push");
        };
        assert!(matches!(push.kind, VariantKind::Tuple(ref ts) if ts.len() == 1));
        let TypeBodyItem::Variant(pair) = &decl.body[1] else {
            panic!("expected variant Pair");
        };
        assert!(matches!(pair.kind, VariantKind::Tuple(ref ts) if ts.len() == 2));
    }

    /// `#[wire]` is exclusive with `#[resource]` / `#[linear]` on enums (same
    /// rule as `#[wire] struct`).
    #[test]
    fn parse_wire_enum_rejects_resource_marker_combo() {
        let source = "\
#[wire]
#[resource]
enum Bad {
    A;
}
";
        let result = parse(source);
        assert!(
            result
                .errors
                .iter()
                .any(|e| e.message.contains("E_TYPE_MARKER_CONFLICT")),
            "expected E_TYPE_MARKER_CONFLICT, got {:?}",
            result.errors
        );
    }

    /// `#[wire] #[json("camelCase")] #[yaml("kebab-case")] enum E { … }` —
    /// type-level naming attributes flow into `WireMetadata`.
    #[test]
    fn parse_wire_enum_preserves_naming_cases() {
        let source = "\
#[wire]
#[json(\"camelCase\")]
#[yaml(\"kebab-case\")]
enum Command {
    Start;
    Stop;
}
";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

        let Item::TypeDecl(decl) = &result.program.items[0].0 else {
            panic!("expected type declaration");
        };
        let wire = decl.wire.as_ref().expect("expected wire metadata on enum");
        assert_eq!(wire.json_case, Some(NamingCase::CamelCase));
        assert_eq!(wire.yaml_case, Some(NamingCase::KebabCase));
    }

    /// `pub #[wire] enum E { ... }` — the visibility-prefixed dispatch arm
    /// (parser.rs `:1491`-area, inside the `Some(Token::Pub)` branch) must
    /// route through `parse_wire_enum` and preserve `Visibility::Pub` on the
    /// resulting `TypeDecl`.  Exercises the pub-prefixed arm specifically
    /// (the existing wire-enum tests all hit the bare arm at `:1581`).
    #[test]
    fn parses_visibility_prefixed_wire_enum() {
        let source = "\
#[wire]
pub enum Command {
    Start;
    Stop;
}
";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

        let Item::TypeDecl(decl) = &result.program.items[0].0 else {
            panic!("expected type declaration");
        };
        assert_eq!(decl.kind, TypeDeclKind::Enum);
        assert_eq!(decl.name, "Command");
        assert_eq!(decl.visibility, Visibility::Pub);
        let wire = decl
            .wire
            .as_ref()
            .expect("expected wire metadata on pub enum");
        assert!(wire.field_meta.is_empty());
        assert!(wire.reserved_numbers.is_empty());
        assert_eq!(decl.body.len(), 2);
        for item in &decl.body {
            match item {
                TypeBodyItem::Variant(v) => assert!(matches!(v.kind, VariantKind::Unit)),
                _ => panic!("expected variant, got {item:?}"),
            }
        }
    }

    /// `#[wire] enum X { A; B(i64); C { x: String, y: i32 } }` — a single
    /// enum body mixing unit, tuple, and struct variant payloads.  Verifies
    /// each variant kind parses correctly, source order is preserved, and
    /// type-level wire metadata is attached.
    #[test]
    fn parses_mixed_variant_wire_enum() {
        let source = "\
#[wire]
enum Mixed {
    A;
    B(i64);
    C { x: String, y: i32 };
}
";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

        let Item::TypeDecl(decl) = &result.program.items[0].0 else {
            panic!("expected type declaration");
        };
        assert_eq!(decl.kind, TypeDeclKind::Enum);
        assert_eq!(decl.name, "Mixed");
        assert!(
            decl.wire.is_some(),
            "expected wire metadata on mixed-variant enum"
        );
        assert_eq!(decl.body.len(), 3, "expected 3 variants in source order");

        // Variant 0: A — unit
        let TypeBodyItem::Variant(a) = &decl.body[0] else {
            panic!("expected variant A at index 0");
        };
        assert_eq!(a.name, "A");
        assert!(
            matches!(a.kind, VariantKind::Unit),
            "variant A should be unit, got {:?}",
            a.kind
        );

        // Variant 1: B(i64) — tuple
        let TypeBodyItem::Variant(b) = &decl.body[1] else {
            panic!("expected variant B at index 1");
        };
        assert_eq!(b.name, "B");
        assert!(
            matches!(b.kind, VariantKind::Tuple(ref ts) if ts.len() == 1),
            "variant B should be tuple(1), got {:?}",
            b.kind
        );

        // Variant 2: C { x: String, y: i32 } — struct
        let TypeBodyItem::Variant(c) = &decl.body[2] else {
            panic!("expected variant C at index 2");
        };
        assert_eq!(c.name, "C");
        match &c.kind {
            VariantKind::Struct(fields) => {
                assert_eq!(fields.len(), 2, "variant C should have 2 fields");
                assert_eq!(fields[0].0, "x");
                assert_eq!(fields[1].0, "y");
            }
            other => panic!("variant C should be struct, got {other:?}"),
        }
    }

    #[test]
    fn parse_legacy_wire_type_preserves_since_modifier() {
        let source = "\
wire type Msg {
    added: String @2 repeated since 3 yaml(\"added\");
}
";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

        let Item::TypeDecl(decl) = &result.program.items[0].0 else {
            panic!("expected type declaration");
        };
        let wire = decl.wire.as_ref().expect("expected wire metadata");
        let meta = &wire.field_meta[0];
        assert!(meta.is_repeated);
        assert_eq!(meta.yaml_name.as_deref(), Some("added"));
        assert_eq!(meta.since, Some(3));
    }

    #[test]
    fn legacy_wire_type_field_metadata_preserves_number_and_outer_naming_cases() {
        let source = "\
#[json(\"camelCase\")]
#[yaml(\"kebab-case\")]
wire type Msg {
    added: String = 4 repeated json(\"added_name\");
}
";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);

        let Item::TypeDecl(decl) = &result.program.items[0].0 else {
            panic!("expected type declaration");
        };
        let wire = decl.wire.as_ref().expect("expected wire metadata");
        assert_eq!(wire.json_case, Some(NamingCase::CamelCase));
        assert_eq!(wire.yaml_case, Some(NamingCase::KebabCase));

        let meta = &wire.field_meta[0];
        assert_eq!(meta.field_number, 4);
        assert!(meta.is_repeated);
        assert_eq!(meta.json_name.as_deref(), Some("added_name"));
    }

    #[test]
    fn parse_wire_since_reports_invalid_version() {
        let source = "\
wire type Msg {
    added: String @2 since 4294967296;
}
";
        let result = parse(source);
        assert!(
            result
                .errors
                .iter()
                .any(|e| e.message.contains("invalid version number after 'since'")),
            "expected invalid since error, got {:?}",
            result.errors
        );

        let Item::TypeDecl(decl) = &result.program.items[0].0 else {
            panic!("expected type declaration");
        };
        let wire = decl.wire.as_ref().expect("expected wire metadata");
        assert_eq!(wire.field_meta[0].since, None);
    }

    /// Helper: parse `fn main() { let x = <source>; }` and return the expression.
    fn parse_let_expr(source: &str) -> Expr {
        let full = format!("fn main() {{ let x = {source}; }}");
        let result = parse(&full);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let Item::Function(f) = &result.program.items[0].0 else {
            panic!("expected function");
        };
        let Stmt::Let {
            value: Some((expr, _)),
            ..
        } = &f.body.stmts[0].0
        else {
            panic!("expected let with value");
        };
        expr.clone()
    }

    fn parse_main_body(source: &str) -> Block {
        let full = format!("fn main() {{ {source} }}");
        let result = parse(&full);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let Item::Function(f) = &result.program.items[0].0 else {
            panic!("expected function");
        };
        f.body.clone()
    }

    #[test]
    fn parse_empty_braces_is_block() {
        // {} is always a block — empty HashMap coercion happens in the type checker
        let expr = parse_let_expr("{}");
        assert!(
            matches!(expr, Expr::Block(_)),
            "expected Block, got {expr:?}"
        );
    }

    #[test]
    fn parse_map_literal_single_entry() {
        let expr = parse_let_expr(r#"{"a": 1}"#);
        assert!(
            matches!(expr, Expr::MapLiteral { ref entries } if entries.len() == 1),
            "expected MapLiteral with 1 entry, got {expr:?}"
        );
    }

    #[test]
    fn parse_map_literal_multiple_entries() {
        let expr = parse_let_expr(r#"{"a": 1, "b": 2, "c": 3}"#);
        assert!(
            matches!(expr, Expr::MapLiteral { ref entries } if entries.len() == 3),
            "expected MapLiteral with 3 entries, got {expr:?}"
        );
    }

    #[test]
    fn parse_map_literal_trailing_comma() {
        let expr = parse_let_expr(r#"{"a": 1, "b": 2,}"#);
        assert!(
            matches!(expr, Expr::MapLiteral { ref entries } if entries.len() == 2),
            "expected MapLiteral with 2 entries, got {expr:?}"
        );
    }

    #[test]
    fn parse_block_still_works() {
        let expr = parse_let_expr("{ let y = 1; y }");
        assert!(
            matches!(expr, Expr::Block(_)),
            "expected Block, got {expr:?}"
        );
    }

    #[test]
    fn scope_keyword_emits_scope_ast_variant() {
        let expr = parse_let_expr("scope { 1 }");
        assert!(
            matches!(expr, Expr::Scope { .. }),
            "expected Scope, got {expr:?}"
        );
    }

    #[test]
    fn parser_scope_block_distinct_from_fork_child() {
        let body = parse_main_body("let block = scope { 1 };\nscope { fork child = run(); };\n");
        let Stmt::Let {
            value: Some((Expr::Scope { .. }, _)),
            ..
        } = &body.stmts[0].0
        else {
            panic!(
                "expected let binding to use scope block: {:?}",
                body.stmts[0]
            );
        };
        let Stmt::Expression((Expr::Scope { body: inner }, _)) = &body.stmts[1].0 else {
            panic!("expected outer scope block: {:?}", body.stmts[1]);
        };
        let Stmt::Expression((Expr::ForkChild { binding, .. }, _)) = &inner.stmts[0].0 else {
            panic!("expected child fork expression: {:?}", inner.stmts[0]);
        };
        assert_eq!(binding.as_deref(), Some("child"));
    }

    #[test]
    fn parse_fork_child_with_binding() {
        let body = parse_main_body("fork child = run();");
        let Stmt::Expression((Expr::ForkChild { binding, expr }, _)) = &body.stmts[0].0 else {
            panic!("expected fork child expression: {:?}", body.stmts[0]);
        };
        assert_eq!(binding.as_deref(), Some("child"));
        assert!(
            matches!(&expr.0, Expr::Call { .. }),
            "expected child expression call, got {:?}",
            expr.0
        );
    }

    #[test]
    fn parse_fork_child_bare() {
        let body = parse_main_body("fork run();");
        let Stmt::Expression((Expr::ForkChild { binding, expr }, _)) = &body.stmts[0].0 else {
            panic!("expected bare fork child expression: {:?}", body.stmts[0]);
        };
        assert!(binding.is_none(), "expected bare fork child binding");
        assert!(
            matches!(&expr.0, Expr::Call { .. }),
            "expected bare child expression call, got {:?}",
            expr.0
        );
    }

    #[test]
    fn parse_nested_scope_block_and_child() {
        let expr = parse_let_expr("scope { fork run(); fork child = work(); child }");
        let Expr::Scope { body } = expr else {
            panic!("expected scope block");
        };
        assert_eq!(body.stmts.len(), 2, "expected two child statements");
        assert!(matches!(
            &body.stmts[0].0,
            Stmt::Expression((Expr::ForkChild { binding: None, .. }, _))
        ));
        assert!(matches!(
            &body.stmts[1].0,
            Stmt::Expression((
                Expr::ForkChild {
                    binding: Some(name),
                    ..
                },
                _
            )) if name == "child"
        ));
        assert!(matches!(
            body.trailing_expr.as_deref(),
            Some((Expr::Identifier(name), _)) if name == "child"
        ));
    }

    #[test]
    fn parse_scope_fork_block_after_deadline() {
        let expr = parse_let_expr("scope { fork { long_op(); } after(5s) { } }");
        let Expr::Scope { body } = expr else {
            panic!("expected scope block");
        };
        assert_eq!(body.stmts.len(), 2, "expected fork block and deadline");
        let Stmt::Expression((Expr::ForkBlock { body: fork_body }, _)) = &body.stmts[0].0 else {
            panic!("expected fork block: {:?}", body.stmts[0]);
        };
        assert_eq!(fork_body.stmts.len(), 1);
        let Stmt::Expression((Expr::ScopeDeadline { duration, body }, _)) = &body.stmts[1].0 else {
            panic!("expected scope deadline: {:?}", body.stmts[1]);
        };
        assert!(
            matches!(duration.0, Expr::Literal(Literal::Duration(5_000_000_000))),
            "deadline duration should be parsed as 5s duration literal: {:?}",
            duration.0
        );
        assert!(body.stmts.is_empty(), "deadline body should be empty");
    }

    #[test]
    fn parse_unscoped_fork_block_rejects() {
        let result = parse("fn main() { fork { long_op(); } }");
        assert!(
            result
                .errors
                .iter()
                .any(|err| err.message.contains("only valid inside `scope")),
            "unscoped fork block must be rejected: {:?}",
            result.errors
        );
    }

    #[test]
    fn capture_doc_comment_on_trait_method() {
        let source = "trait T {\n    /// First line.\n    /// Second line.\n    fn m();\n}\n";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let Item::Trait(t) = &result.program.items[0].0 else {
            panic!("expected trait");
        };
        let TraitItem::Method(m) = &t.items[0] else {
            panic!("expected method");
        };
        assert_eq!(m.doc_comment.as_deref(), Some("First line.\nSecond line."));
    }

    #[test]
    fn capture_doc_comment_on_enum_variant() {
        let source = "enum E {\n    /// The only variant.\n    A;\n}\n";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let Item::TypeDecl(t) = &result.program.items[0].0 else {
            panic!("expected type decl");
        };
        let TypeBodyItem::Variant(v) = &t.body[0] else {
            panic!("expected variant");
        };
        assert_eq!(v.doc_comment.as_deref(), Some("The only variant."));
    }

    #[test]
    fn capture_doc_comment_on_struct_field() {
        let source = "type S {\n    /// The x coord.\n    x: i32;\n}\n";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let Item::TypeDecl(t) = &result.program.items[0].0 else {
            panic!("expected type decl");
        };
        let TypeBodyItem::Field { doc_comment, .. } = &t.body[0] else {
            panic!("expected field");
        };
        assert_eq!(doc_comment.as_deref(), Some("The x coord."));
    }

    #[test]
    fn capture_doc_comment_on_receive_fn_and_actor_field() {
        let source = "actor A {\n    /// The counter.\n    let n: i32;\n    /// Increment handler.\n    receive fn inc() {}\n}\n";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let Item::Actor(a) = &result.program.items[0].0 else {
            panic!("expected actor");
        };
        assert_eq!(a.fields[0].doc_comment.as_deref(), Some("The counter."));
        assert_eq!(
            a.receive_fns[0].doc_comment.as_deref(),
            Some("Increment handler."),
        );
    }

    #[test]
    fn capture_doc_comment_on_const_and_type_alias() {
        let source =
            "/// The answer.\npub const ANSWER: i32 = 42;\n\n/// An id.\npub type Id = i64;\n";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let Item::Const(c) = &result.program.items[0].0 else {
            panic!("expected const");
        };
        assert_eq!(c.doc_comment.as_deref(), Some("The answer."));
        let Item::TypeAlias(ta) = &result.program.items[1].0 else {
            panic!("expected type alias");
        };
        assert_eq!(ta.doc_comment.as_deref(), Some("An id."));
    }

    #[test]
    fn capture_doc_comment_on_actor_method() {
        let source = "actor A {\n    /// Reset the counter.\n    fn reset() {}\n}\n";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let Item::Actor(a) = &result.program.items[0].0 else {
            panic!("expected actor");
        };
        assert_eq!(a.methods.len(), 1);
        assert_eq!(
            a.methods[0].doc_comment.as_deref(),
            Some("Reset the counter.")
        );
    }

    // ── ParseDiagnosticKind serialisation ──────────────────────────────────

    #[test]
    fn parse_diagnostic_kind_unexpected_token_serialises() {
        let kind = ParseDiagnosticKind::UnexpectedToken {
            expected: ";".to_string(),
            got: "}".to_string(),
        };
        let json = serde_json::to_value(&kind).unwrap();
        assert_eq!(json["kind"], "UnexpectedToken");
        assert_eq!(json["expected"], ";");
        assert_eq!(json["got"], "}");
        assert_eq!(kind.as_kind_str(), "UnexpectedToken");
    }

    #[test]
    fn parse_diagnostic_kind_unexpected_eof_serialises() {
        let kind = ParseDiagnosticKind::UnexpectedEof;
        let json = serde_json::to_value(&kind).unwrap();
        assert_eq!(json["kind"], "UnexpectedEof");
        assert_eq!(kind.as_kind_str(), "UnexpectedEof");
    }

    #[test]
    fn parse_diagnostic_kind_invalid_literal_serialises() {
        let kind = ParseDiagnosticKind::InvalidLiteral;
        let json = serde_json::to_value(&kind).unwrap();
        assert_eq!(json["kind"], "InvalidLiteral");
        assert_eq!(kind.as_kind_str(), "InvalidLiteral");
    }

    #[test]
    fn parse_diagnostic_kind_missing_expression_serialises() {
        let kind = ParseDiagnosticKind::MissingExpression {
            got: "end of file".to_string(),
        };
        let json = serde_json::to_value(&kind).unwrap();
        assert_eq!(json["kind"], "MissingExpression");
        assert_eq!(json["got"], "end of file");
        assert_eq!(kind.as_kind_str(), "MissingExpression");
    }

    #[test]
    fn parse_diagnostic_kind_invalid_pattern_serialises() {
        let kind = ParseDiagnosticKind::InvalidPattern {
            got: "42".to_string(),
        };
        let json = serde_json::to_value(&kind).unwrap();
        assert_eq!(json["kind"], "InvalidPattern");
        assert_eq!(json["got"], "42");
        assert_eq!(kind.as_kind_str(), "InvalidPattern");
    }

    #[test]
    fn parse_diagnostic_kind_other_serialises() {
        let kind = ParseDiagnosticKind::Other;
        let json = serde_json::to_value(&kind).unwrap();
        assert_eq!(json["kind"], "Other");
        assert_eq!(kind.as_kind_str(), "Other");
    }

    // ── ParseDiagnosticKind round-trip through ParseError ─────────────────

    #[test]
    fn unexpected_token_kind_set_on_expect_failure() {
        // `expect` should produce UnexpectedToken when a required token is absent.
        let result = parse("fn foo(");
        let missing_paren = result
            .errors
            .iter()
            .find(|e| matches!(&e.kind, ParseDiagnosticKind::UnexpectedToken { .. }));
        assert!(
            missing_paren.is_some(),
            "expected UnexpectedToken in errors, got: {:?}",
            result.errors
        );
    }

    #[test]
    fn invalid_literal_kind_set_on_bad_char_escape() {
        let result = parse("fn f() { let x: char = '\\z'; }");
        let lit_err = result
            .errors
            .iter()
            .find(|e| matches!(&e.kind, ParseDiagnosticKind::InvalidLiteral));
        assert!(
            lit_err.is_some(),
            "expected InvalidLiteral in errors, got: {:?}",
            result.errors
        );
    }

    #[test]
    fn missing_expression_kind_set_at_parse_primary_fallthrough() {
        // `)` cannot start an expression in an expression context.
        let result = parse("fn f() { let x = ); }");
        let expr_err = result.errors.iter().find(|e| {
            matches!(
                &e.kind,
                ParseDiagnosticKind::MissingExpression { .. }
                    | ParseDiagnosticKind::UnexpectedToken { .. }
            )
        });
        assert!(
            expr_err.is_some(),
            "expected MissingExpression or UnexpectedToken in errors, got: {:?}",
            result.errors
        );
    }

    // ── StructInit with explicit type arguments ────────────────────────────

    #[test]
    fn struct_init_explicit_single_type_arg_parses() {
        let src = r#"
            type Wrapper<T> { value: T }
            fn main() { let w = Wrapper<string> { value: "hello" }; }
        "#;
        let result = parse(src);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        // Drill into the let initialiser and confirm type_args is Some([String]).
        let Item::Function(f) = &result.program.items[1].0 else {
            panic!("expected function");
        };
        let stmt = &f.body.stmts[0];
        let Stmt::Let {
            value: Some((expr, _)),
            ..
        } = &stmt.0
        else {
            panic!("expected let with value");
        };
        let Expr::StructInit {
            name, type_args, ..
        } = expr
        else {
            panic!("expected StructInit, got {expr:?}");
        };
        assert_eq!(name, "Wrapper");
        let args = type_args.as_ref().expect("type_args should be Some");
        assert_eq!(args.len(), 1, "expected one type arg");
        assert!(
            matches!(&args[0].0, TypeExpr::Named { name, .. } if name == "string"),
            "expected string type arg, got {:?}",
            args[0].0
        );
    }

    #[test]
    fn struct_init_without_type_args_leaves_type_args_none() {
        let src = r#"
            type Wrapper<T> { value: T }
            fn main() { let w = Wrapper { value: "hello" }; }
        "#;
        let result = parse(src);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let Item::Function(f) = &result.program.items[1].0 else {
            panic!("expected function");
        };
        let stmt = &f.body.stmts[0];
        let Stmt::Let {
            value: Some((expr, _)),
            ..
        } = &stmt.0
        else {
            panic!("expected let with value");
        };
        let Expr::StructInit { type_args, .. } = expr else {
            panic!("expected StructInit, got {expr:?}");
        };
        assert!(type_args.is_none(), "type_args should be None when omitted");
    }

    #[test]
    fn struct_init_explicit_multi_type_arg_parses() {
        let src = r#"
            type Pair<A, B> { first: A, second: B }
            fn main() { let p = Pair<int, string> { first: 1, second: "x" }; }
        "#;
        let result = parse(src);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let Item::Function(f) = &result.program.items[1].0 else {
            panic!("expected function");
        };
        let stmt = &f.body.stmts[0];
        let Stmt::Let {
            value: Some((expr, _)),
            ..
        } = &stmt.0
        else {
            panic!("expected let with value");
        };
        let Expr::StructInit { type_args, .. } = expr else {
            panic!("expected StructInit, got {expr:?}");
        };
        let args = type_args.as_ref().expect("type_args should be Some");
        assert_eq!(args.len(), 2, "expected two type args");
        assert!(
            matches!(&args[0].0, TypeExpr::Named { name, .. } if name == "int"),
            "first type arg should be int"
        );
        assert!(
            matches!(&args[1].0, TypeExpr::Named { name, .. } if name == "string"),
            "second type arg should be string"
        );
    }

    #[test]
    fn struct_init_explicit_type_arg_empty_struct_parses() {
        // Edge case: struct with type param but no fields at the init site is
        // not something the parser should crash on (it's a checker concern).
        let src = r"
            type Tag<T> {}
            fn main() { let t = Tag<int> {}; }
        ";
        let result = parse(src);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let Item::Function(f) = &result.program.items[1].0 else {
            panic!("expected function");
        };
        let stmt = &f.body.stmts[0];
        let Stmt::Let {
            value: Some((expr, _)),
            ..
        } = &stmt.0
        else {
            panic!("expected let with value");
        };
        let Expr::StructInit { type_args, .. } = expr else {
            panic!("expected StructInit, got {expr:?}");
        };
        let args = type_args.as_ref().expect("type_args should be Some");
        assert_eq!(args.len(), 1);
    }

    #[test]
    fn struct_init_comparison_does_not_consume_lt_as_type_arg() {
        // `if x < y { ... }` must NOT be parsed as a struct init with type arg y.
        let src = r"
            fn main() {
                let x = 1;
                let y = 2;
                if x < y { let _ = x; }
            }
        ";
        let result = parse(src);
        assert!(
            result.errors.is_empty(),
            "parse errors (regression: `<` in comparison swallowed): {:?}",
            result.errors
        );
    }
    #[test]
    fn parses_resource_marker_and_consuming_method() {
        let source = r"
            #[resource]
            type File {
                fd: int
                fn close(consuming self) -> int { 0 }
            }
        ";
        let result = parse(source);
        assert!(
            result.errors.is_empty(),
            "unexpected parse errors: {:?}",
            result.errors
        );
        assert_eq!(result.program.items.len(), 1);
        let (item, _span) = &result.program.items[0];
        let Item::TypeDecl(td) = item else {
            panic!("expected TypeDecl, got {item:?}");
        };
        assert_eq!(td.resource_marker, ResourceMarker::Resource);
        assert_eq!(td.consuming_methods, vec!["close".to_string()]);
        assert_eq!(td.name, "File");
    }

    #[test]
    fn parses_linear_marker_and_multiple_consuming_methods() {
        let source = r"
            #[linear]
            type Txn {
                id: int
                fn commit(consuming self) -> int { 0 }
                fn rollback(consuming self) -> int { 1 }
                fn id(t: Txn) -> int { 0 }
            }
        ";
        let result = parse(source);
        assert!(
            result.errors.is_empty(),
            "unexpected parse errors: {:?}",
            result.errors
        );
        let (Item::TypeDecl(td), _) = &result.program.items[0] else {
            panic!("expected TypeDecl");
        };
        assert_eq!(td.resource_marker, ResourceMarker::Linear);
        // Only `commit` and `rollback` consume; `id(self: Txn)` does not.
        assert_eq!(
            td.consuming_methods,
            vec!["commit".to_string(), "rollback".to_string()],
        );
    }

    #[test]
    fn unmarked_type_has_no_resource_marker() {
        let source = "type Point { x: int; y: int }";
        let result = parse(source);
        assert!(result.errors.is_empty());
        let (Item::TypeDecl(td), _) = &result.program.items[0] else {
            panic!("expected TypeDecl");
        };
        assert_eq!(td.resource_marker, ResourceMarker::None);
        assert!(td.consuming_methods.is_empty());
    }

    #[test]
    fn resource_and_linear_combined_emits_conflict_diagnostic() {
        let source = r"
            #[resource]
            #[linear]
            type Bad { x: int }
        ";
        let result = parse(source);
        assert!(
            !result.errors.is_empty(),
            "expected a conflict diagnostic but got none"
        );
        assert!(
            result.errors[0].message.contains("E_TYPE_MARKER_CONFLICT"),
            "expected E_TYPE_MARKER_CONFLICT in message, got: {:?}",
            result.errors[0].message
        );
    }

    #[test]
    fn linear_then_resource_combined_emits_conflict_diagnostic() {
        // Order is reversed: linear first, resource second — conflict at resource.
        let source = r"
            #[linear]
            #[resource]
            type Bad { x: int }
        ";
        let result = parse(source);
        assert!(
            !result.errors.is_empty(),
            "expected a conflict diagnostic but got none"
        );
        assert!(
            result.errors[0].message.contains("E_TYPE_MARKER_CONFLICT"),
            "expected E_TYPE_MARKER_CONFLICT in message, got: {:?}",
            result.errors[0].message
        );
    }

    #[test]
    fn unknown_type_marker_emits_diagnostic() {
        let source = r"
            #[both]
            type Bad { x: int }
        ";
        let result = parse(source);
        assert!(
            !result.errors.is_empty(),
            "expected E_UNKNOWN_TYPE_MARKER diagnostic but got none"
        );
        assert!(
            result.errors[0].message.contains("E_UNKNOWN_TYPE_MARKER"),
            "expected E_UNKNOWN_TYPE_MARKER in message, got: {:?}",
            result.errors[0].message
        );
    }

    #[test]
    fn wire_and_resource_combined_emits_conflict_diagnostic() {
        // `#[wire] struct` + `#[resource]` is disallowed: wire types are
        // traffic-shape declarations; ownership discipline is separate.
        let source = r"
            #[wire]
            #[resource]
            struct Bad { x: int @1 }
        ";
        let result = parse(source);
        assert!(
            !result.errors.is_empty(),
            "expected E_TYPE_MARKER_CONFLICT diagnostic but got none"
        );
        assert!(
            result.errors[0].message.contains("E_TYPE_MARKER_CONFLICT"),
            "expected E_TYPE_MARKER_CONFLICT in message, got: {:?}",
            result.errors[0].message
        );
    }

    #[test]
    fn deprecated_attr_on_type_is_accepted() {
        // `#[deprecated]` is a known-valid type-decl attribute and must not
        // trigger E_UNKNOWN_TYPE_MARKER.
        let source = r"
            #[deprecated]
            type Old { x: int }
        ";
        let result = parse(source);
        // No ownership-marker diagnostics; other errors (deprecation warnings
        // etc.) are out of scope for the parser.
        assert!(
            result
                .errors
                .iter()
                .all(|e| !e.message.contains("E_UNKNOWN_TYPE_MARKER")),
            "deprecated attr triggered unknown-marker diagnostic: {:?}",
            result.errors
        );
    }
    // ── #[max_heap] attribute tests ──────────────────────────────────────────

    #[test]
    fn max_heap_attribute_bare_integer_bytes() {
        let source = "#[max_heap(1024)] actor Demo {}";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let Item::Actor(actor) = &result.program.items[0].0 else {
            panic!("expected actor");
        };
        assert_eq!(actor.max_heap_bytes, Some(1024));
    }

    #[test]
    fn max_heap_attribute_kb_suffix() {
        let source = "#[max_heap(2 kb)] actor Demo {}";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let Item::Actor(actor) = &result.program.items[0].0 else {
            panic!("expected actor");
        };
        assert_eq!(actor.max_heap_bytes, Some(2 * 1024));
    }

    #[test]
    fn max_heap_attribute_mb_suffix() {
        let source = "#[max_heap(1 mb)] actor Demo {}";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let Item::Actor(actor) = &result.program.items[0].0 else {
            panic!("expected actor");
        };
        assert_eq!(actor.max_heap_bytes, Some(1024 * 1024));
    }

    #[test]
    fn max_heap_attribute_b_suffix() {
        let source = "#[max_heap(512 b)] actor Demo {}";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let Item::Actor(actor) = &result.program.items[0].0 else {
            panic!("expected actor");
        };
        assert_eq!(actor.max_heap_bytes, Some(512));
    }

    #[test]
    fn max_heap_attribute_zero_accepted_as_unbounded() {
        // cap=0 means unbounded (same as the legacy default); accepted explicitly.
        let source = "#[max_heap(0)] actor Demo {}";
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let Item::Actor(actor) = &result.program.items[0].0 else {
            panic!("expected actor");
        };
        assert_eq!(actor.max_heap_bytes, Some(0));
    }

    #[test]
    fn max_heap_attribute_gb_suffix_rejected() {
        let source = "#[max_heap(1 gb)] actor Demo {}";
        let result = parse(source);
        assert!(
            !result.errors.is_empty(),
            "expected error for unsupported `gb` suffix"
        );
        assert!(
            result.errors.iter().any(|e| e.message.contains("gb")),
            "expected error mentioning `gb`, got: {:?}",
            result.errors
        );
    }

    #[test]
    fn max_heap_attribute_on_fn_rejected() {
        let source = "#[max_heap(1024)] fn foo() {}";
        let result = parse(source);
        assert!(
            !result.errors.is_empty(),
            "expected error: #[max_heap] only allowed on actor declarations"
        );
        assert!(
            result
                .errors
                .iter()
                .any(|e| e.message.contains("max_heap") && e.message.contains("actor")),
            "expected error mentioning max_heap and actor, got: {:?}",
            result.errors
        );
    }

    #[test]
    fn max_heap_attribute_on_type_rejected() {
        let source = "#[max_heap(1 kb)] type Bar { x: i32; }";
        let result = parse(source);
        assert!(
            !result.errors.is_empty(),
            "expected error: #[max_heap] only allowed on actor declarations"
        );
        assert!(
            result
                .errors
                .iter()
                .any(|e| e.message.contains("max_heap") && e.message.contains("actor")),
            "expected error mentioning max_heap and actor, got: {:?}",
            result.errors
        );
    }
    // ── is operator tests ──────────────────────────────────────────────

    /// Helper: extract the trailing expression from the first statement of the
    /// first function in the parse result.
    fn first_fn_trailing(source: &str) -> Expr {
        let result = parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let Item::Function(f) = &result.program.items[0].0 else {
            panic!("expected function item");
        };
        f.body
            .trailing_expr
            .as_ref()
            .expect("expected trailing expr")
            .0
            .clone()
    }

    #[test]
    fn is_operator_simple_identifiers() {
        let expr = first_fn_trailing("fn f() { x is y }");
        assert!(
            matches!(expr, Expr::Is { .. }),
            "expected Expr::Is, got {expr:?}"
        );
        if let Expr::Is { lhs, rhs } = expr {
            assert!(matches!(lhs.0, Expr::Identifier(ref s) if s == "x"));
            assert!(matches!(rhs.0, Expr::Identifier(ref s) if s == "y"));
        }
    }

    #[test]
    fn is_operator_named_type_rhs() {
        // Parser admits any expression on the rhs; checker rejects scalars (D-2).
        let expr = first_fn_trailing("fn f() { value is Point }");
        assert!(matches!(expr, Expr::Is { .. }));
    }

    #[test]
    fn is_operator_scalar_rhs_parses_ok() {
        // `x is int` — parser admits; checker rejects (slice D-2 / D-3 scope).
        let result = parse("fn f() { let x = value is int; }");
        assert!(
            result.errors.is_empty(),
            "unexpected parse errors: {:?}",
            result.errors
        );
    }

    #[test]
    fn is_operator_tuple_rhs_parses_ok() {
        // `x is (a, b)` — parser admits a tuple on the rhs; checker will reject later.
        let result = parse("fn f() { let x = value is (a, b); }");
        assert!(
            result.errors.is_empty(),
            "unexpected parse errors: {:?}",
            result.errors
        );
    }

    #[test]
    fn is_operator_chained_left_assoc() {
        // `a is b is c` parses as `(a is b) is c` (left-assoc at equality BP).
        let expr = first_fn_trailing("fn f() { a is b is c }");
        // Outer must be Is; its lhs must also be Is.
        let Expr::Is { lhs, .. } = expr else {
            panic!("expected outer Expr::Is");
        };
        assert!(
            matches!(lhs.0, Expr::Is { .. }),
            "expected inner lhs to be Expr::Is (left-assoc), got {:?}",
            lhs.0
        );
    }

    #[test]
    fn is_operator_equality_precedence_binds_tighter_than_logical_and() {
        // `a is b && c is d` should parse as `(a is b) && (c is d)`
        let expr = first_fn_trailing("fn f() { a is b && c is d }");
        let Expr::Binary { left, op, right } = expr else {
            panic!("expected Binary at top level");
        };
        assert_eq!(op, BinaryOp::And);
        assert!(matches!(left.0, Expr::Is { .. }), "left should be Is");
        assert!(matches!(right.0, Expr::Is { .. }), "right should be Is");
    }

    #[test]
    fn is_operator_equality_precedence_with_eq() {
        // `a is b == true` should parse as `(a is b) == true` (same precedence, left-to-right)
        let expr = first_fn_trailing("fn f() { a is b == true }");
        let Expr::Binary { left, op, right } = expr else {
            panic!("expected Binary at top level");
        };
        assert_eq!(op, BinaryOp::Equal);
        assert!(matches!(left.0, Expr::Is { .. }));
        assert!(matches!(right.0, Expr::Literal(Literal::Bool(true))));
    }

    #[test]
    fn is_keyword_reserved_not_identifier() {
        // `is` must not parse as an identifier (it's a reserved keyword).
        use hew_lexer::{lex, Token};
        let toks: Vec<Token<'_>> = lex("is").into_iter().map(|(t, _)| t).collect();
        assert_eq!(toks, vec![Token::Is]);
        assert_ne!(toks[0], Token::Identifier("is"));
    }
    // ---------------------------------------------------------------------------
    // Wrapping operator parsing
    // ---------------------------------------------------------------------------

    fn first_fn_expr(source: &str) -> Expr {
        let r = parse(source);
        assert!(r.errors.is_empty(), "parse errors: {:?}", r.errors);
        let Item::Function(f) = &r.program.items[0].0 else {
            panic!("expected function item");
        };
        // FnDecl.body is a Block struct directly (not wrapped in Spanned<Expr>).
        let body = &f.body;
        if let Some(e) = &body.trailing_expr {
            e.0.clone()
        } else {
            panic!("expected trailing expression in function body");
        }
    }

    #[test]
    fn wrapping_binary_ops_parse_correctly() {
        // `a &+ b` should parse as Binary { op: WrappingAdd, .. }
        let expr = first_fn_expr("fn f(a: i64, b: i64) -> i64 { a &+ b }");
        let Expr::Binary { op, .. } = expr else {
            panic!("expected Binary");
        };
        assert_eq!(op, BinaryOp::WrappingAdd);

        let expr = first_fn_expr("fn f(a: i64, b: i64) -> i64 { a &- b }");
        let Expr::Binary { op, .. } = expr else {
            panic!("expected Binary");
        };
        assert_eq!(op, BinaryOp::WrappingSub);

        let expr = first_fn_expr("fn f(a: i64, b: i64) -> i64 { a &* b }");
        let Expr::Binary { op, .. } = expr else {
            panic!("expected Binary");
        };
        assert_eq!(op, BinaryOp::WrappingMul);
    }

    #[test]
    fn wrapping_add_precedence_matches_plain_add() {
        // `a &+ b * c` should parse as `a &+ (b * c)` — `*` binds tighter.
        let expr = first_fn_expr("fn f(a: i64, b: i64, c: i64) -> i64 { a &+ b * c }");
        let Expr::Binary { op, right, .. } = expr else {
            panic!("expected outer Binary");
        };
        assert_eq!(op, BinaryOp::WrappingAdd, "outer op must be &+");
        // The right sub-expression must be `b * c` (plain multiply).
        let Expr::Binary { op: inner_op, .. } = right.0 else {
            panic!("expected inner Binary");
        };
        assert_eq!(inner_op, BinaryOp::Multiply, "inner op must be *");
    }

    #[test]
    fn wrapping_mul_precedence_matches_plain_mul() {
        // `a + b &* c` should parse as `a + (b &* c)` — `&*` binds tighter than `+`.
        let expr = first_fn_expr("fn f(a: i64, b: i64, c: i64) -> i64 { a + b &* c }");
        let Expr::Binary { op, right, .. } = expr else {
            panic!("expected outer Binary");
        };
        assert_eq!(op, BinaryOp::Add, "outer op must be +");
        let Expr::Binary { op: inner_op, .. } = right.0 else {
            panic!("expected inner Binary");
        };
        assert_eq!(inner_op, BinaryOp::WrappingMul, "inner op must be &*");
    }

    #[test]
    fn wrapping_ops_parse_with_no_errors() {
        for src in &[
            "fn f(a: i64, b: i64) -> i64 { a &+ b }",
            "fn f(a: i64, b: i64) -> i64 { a &- b }",
            "fn f(a: i64, b: i64) -> i64 { a &* b }",
        ] {
            let r = parse(src);
            assert!(
                r.errors.is_empty(),
                "parse errors for `{src}`: {:?}",
                r.errors
            );
        }
    }

    // ── functional_update: `R { x: 5, ..base }` ───────────────────────────

    #[test]
    fn functional_update_basic_parses() {
        // `Point { x: 1, ..old }` must parse as a StructInit with base = Some(old).
        let src = r"
            record Point { x: int, y: int }
            fn f(old: Point) { let p = Point { x: 1, ..old }; }
        ";
        let result = parse(src);
        assert!(
            result.errors.is_empty(),
            "functional update must parse without errors; got: {:?}",
            result.errors
        );
        let Item::Function(f) = &result.program.items[1].0 else {
            panic!("expected function");
        };
        let stmt = &f.body.stmts[0];
        let Stmt::Let {
            value: Some((expr, _)),
            ..
        } = &stmt.0
        else {
            panic!("expected let with value");
        };
        let Expr::StructInit { fields, base, .. } = expr else {
            panic!("expected StructInit, got {expr:?}");
        };
        assert_eq!(fields.len(), 1, "one explicit field expected");
        let base = base.as_ref().expect("base should be Some");
        assert!(
            matches!(&base.0, Expr::Identifier(name) if name == "old"),
            "base should be Identifier 'old', got {:?}",
            base.0
        );
    }

    #[test]
    fn functional_update_no_explicit_fields_parses() {
        // `Point { ..old }` (zero explicit fields, only base) must also parse.
        let src = r"
            record Point { x: int, y: int }
            fn f(old: Point) { let p = Point { ..old }; }
        ";
        let result = parse(src);
        assert!(
            result.errors.is_empty(),
            "functional update with no explicit fields must parse; got: {:?}",
            result.errors
        );
        let Item::Function(f) = &result.program.items[1].0 else {
            panic!("expected function");
        };
        let stmt = &f.body.stmts[0];
        let Stmt::Let {
            value: Some((expr, _)),
            ..
        } = &stmt.0
        else {
            panic!("expected let with value");
        };
        let Expr::StructInit { fields, base, .. } = expr else {
            panic!("expected StructInit, got {expr:?}");
        };
        assert_eq!(fields.len(), 0, "no explicit fields expected");
        assert!(base.is_some(), "base should be Some");
    }

    #[test]
    fn functional_update_mid_list_base_is_rejected() {
        // `Point { ..base, x: 1 }` — base is not last; must produce a parse error.
        let src = r"
            record Point { x: int, y: int }
            fn f(old: Point) { let p = Point { ..old, x: 1 }; }
        ";
        let result = parse(src);
        assert!(
            !result.errors.is_empty(),
            "functional-update base not at end must produce a parse error"
        );
    }

    #[test]
    fn functional_update_base_is_none_for_regular_struct_init() {
        // A plain struct literal must have base = None.
        let src = r"
            record Point { x: int, y: int }
            fn f() { let p = Point { x: 1, y: 2 }; }
        ";
        let result = parse(src);
        assert!(
            result.errors.is_empty(),
            "plain struct init must parse without errors; got: {:?}",
            result.errors
        );
        let Item::Function(f) = &result.program.items[1].0 else {
            panic!("expected function");
        };
        let stmt = &f.body.stmts[0];
        let Stmt::Let {
            value: Some((expr, _)),
            ..
        } = &stmt.0
        else {
            panic!("expected let with value");
        };
        let Expr::StructInit { base, .. } = expr else {
            panic!("expected StructInit, got {expr:?}");
        };
        assert!(base.is_none(), "plain struct init must have base = None");
    }

    #[test]
    fn functional_update_double_base_is_rejected() {
        // `R { ..a, ..b }` must be a parse error — only one base allowed.
        let src = r"
            record Point { x: int, y: int }
            fn f(a: Point, b: Point) { let p = Point { ..a, ..b }; }
        ";
        let result = parse(src);
        let has_expected_error = result
            .errors
            .iter()
            .any(|e| e.message.contains("must be the last item"));
        assert!(
            has_expected_error,
            "double base `..a, ..b` must produce a 'must be the last item' error; got: {:?}",
            result.errors
        );
    }

    #[test]
    fn intrinsic_attribute_parsed_with_semicolon_body() {
        // A function annotated with `#[intrinsic("key")]` and a `;` terminator
        // must parse without errors and carry the intrinsic key in `FnDecl.intrinsic`.
        let src = r#"#[intrinsic("math.sqrt")] pub fn sqrt(x: f64) -> f64;"#;
        let result = parse(src);
        assert!(
            result.errors.is_empty(),
            "#[intrinsic] with semicolon body must parse cleanly; got: {:?}",
            result.errors
        );
        let (item, _) = result.program.items.first().expect("expected one item");
        if let crate::ast::Item::Function(fd) = item {
            assert_eq!(
                fd.intrinsic.as_deref(),
                Some("math.sqrt"),
                "FnDecl.intrinsic must carry the catalog key"
            );
        } else {
            panic!("expected Item::Function, got something else");
        }
    }

    #[test]
    fn intrinsic_attribute_key_is_none_for_normal_fns() {
        // Regular functions must have `intrinsic == None`.
        let src = r"pub fn add(a: i64, b: i64) -> i64 { a + b }";
        let result = parse(src);
        assert!(result.errors.is_empty());
        let (item, _) = result.program.items.first().expect("expected one item");
        if let crate::ast::Item::Function(fd) = item {
            assert!(
                fd.intrinsic.is_none(),
                "normal fn must have intrinsic == None, got {:?}",
                fd.intrinsic
            );
        } else {
            panic!("expected Item::Function");
        }
    }

    // --- #[extern_symbol] attribute --------------------------------------

    #[test]
    fn extern_symbol_attribute_on_extern_c_fn_is_captured() {
        // The attribute must parse via the existing Attribute infrastructure
        // and ride on `ExternFnDecl.attributes`. The template string is
        // captured verbatim — `{T}` is a literal character sequence inside
        // a `StringLit` and is not yet parsed as a placeholder (Stage 2).
        let src = r#"
            extern "C" {
                #[extern_symbol("hew_vec_push_{T}")]
                fn hew_vec_push(v: ptr, x: ptr);
            }
        "#;
        let result = parse(src);
        assert!(
            result.errors.is_empty(),
            "expected no parse errors, got: {:?}",
            result.errors
        );
        let crate::ast::Item::ExternBlock(block) = &result.program.items[0].0 else {
            panic!("expected ExternBlock");
        };
        assert_eq!(block.functions.len(), 1);
        let extern_fn = &block.functions[0];
        assert_eq!(extern_fn.name, "hew_vec_push");
        assert_eq!(extern_fn.attributes.len(), 1, "must carry one attribute");
        let attr = &extern_fn.attributes[0];
        assert_eq!(attr.name, "extern_symbol");
        assert_eq!(attr.args.len(), 1);
        assert_eq!(attr.args[0].as_str(), "hew_vec_push_{T}");
    }

    #[test]
    fn extern_symbol_attribute_on_impl_method_is_captured() {
        // The attribute must also flow through `parse_impl_decl`'s body loop
        // onto inherent impl methods.
        let src = r#"
            impl<T> Vec<T> {
                #[extern_symbol("hew_vec_push_{T}")]
                fn push(self, x: T) {}
            }
        "#;
        let result = parse(src);
        assert!(
            result.errors.is_empty(),
            "expected no parse errors, got: {:?}",
            result.errors
        );
        let crate::ast::Item::Impl(impl_decl) = &result.program.items[0].0 else {
            panic!("expected Impl");
        };
        assert_eq!(impl_decl.methods.len(), 1);
        let method = &impl_decl.methods[0];
        assert_eq!(method.name, "push");
        assert_eq!(method.attributes.len(), 1);
        assert_eq!(method.attributes[0].name, "extern_symbol");
        assert_eq!(method.attributes[0].args[0].as_str(), "hew_vec_push_{T}");
    }

    #[test]
    fn extern_symbol_attribute_on_trait_impl_method_is_captured() {
        let src = r#"
            impl<T> SomeTrait for Vec<T> {
                #[extern_symbol("hew_vec_join_str")]
                fn join(self, sep: string) -> string {}
            }
        "#;
        let result = parse(src);
        assert!(
            result.errors.is_empty(),
            "expected no parse errors, got: {:?}",
            result.errors
        );
        let crate::ast::Item::Impl(impl_decl) = &result.program.items[0].0 else {
            panic!("expected Impl");
        };
        assert!(impl_decl.trait_bound.is_some(), "must be a trait impl");
        assert_eq!(impl_decl.methods.len(), 1);
        let method = &impl_decl.methods[0];
        assert_eq!(method.attributes.len(), 1);
        assert_eq!(method.attributes[0].name, "extern_symbol");
        assert_eq!(method.attributes[0].args[0].as_str(), "hew_vec_join_str");
    }

    #[test]
    fn extern_symbol_attribute_on_free_fn_is_rejected() {
        // Attachment rule: `#[extern_symbol]` is only valid on
        // `fn` declarations inside `extern "C"` blocks or `impl` blocks.
        // Placement on a free fn must surface as a parser diagnostic.
        let src = r#"
            #[extern_symbol("hew_foo")]
            pub fn foo() {}
        "#;
        let result = parse(src);
        assert!(
            result
                .errors
                .iter()
                .any(|e| e.message.contains("`#[extern_symbol]`")),
            "expected an `#[extern_symbol]` placement diagnostic, got: {:?}",
            result.errors
        );
    }

    #[test]
    fn extern_symbol_attribute_on_actor_is_rejected() {
        let src = r#"
            #[extern_symbol("hew_actor")]
            actor MyActor {}
        "#;
        let result = parse(src);
        assert!(
            result
                .errors
                .iter()
                .any(|e| e.message.contains("`#[extern_symbol]`")),
            "expected an `#[extern_symbol]` placement diagnostic, got: {:?}",
            result.errors
        );
    }

    #[test]
    fn extern_symbol_attribute_on_type_decl_method_is_rejected() {
        // Methods declared inline in a type body are not `impl` methods —
        // declare them in an `impl` block instead.
        let src = r#"
            type Foo {
                x: i64,
                #[extern_symbol("hew_foo_bar")]
                fn bar(self) {}
            }
        "#;
        let result = parse(src);
        assert!(
            result
                .errors
                .iter()
                .any(|e| e.message.contains("`#[extern_symbol]`")),
            "expected an `#[extern_symbol]` placement diagnostic, got: {:?}",
            result.errors
        );
    }

    #[test]
    fn extern_symbol_attribute_on_actor_receive_fn_is_rejected() {
        // Actor `receive fn` (and `receive gen fn`) declarations cannot bind
        // to a runtime C-ABI symbol — extern_symbol is only valid in an
        // `extern "C"` block or an `impl` block.
        let src = r#"
            actor MyActor {
                #[extern_symbol("hew_actor_recv")]
                receive fn ping() {}
            }
        "#;
        let result = parse(src);
        assert!(
            result
                .errors
                .iter()
                .any(|e| e.message.contains("`#[extern_symbol]`")),
            "expected an `#[extern_symbol]` placement diagnostic, got: {:?}",
            result.errors
        );
    }

    #[test]
    fn extern_symbol_attribute_on_trait_item_fn_is_rejected() {
        // Trait items describe an abstract surface; the C-ABI binding belongs
        // on the concrete `impl` method, not the trait declaration.
        let src = r#"
            trait Pushable<T> {
                #[extern_symbol("hew_vec_push_{T}")]
                fn push(self, value: T);
            }
        "#;
        let result = parse(src);
        assert!(
            result
                .errors
                .iter()
                .any(|e| e.message.contains("`#[extern_symbol]`")),
            "expected an `#[extern_symbol]` placement diagnostic, got: {:?}",
            result.errors
        );
    }

    #[test]
    fn extern_symbol_attribute_missing_string_arg_is_rejected() {
        // `parse_attributes` enforces value-shape rules; a key-value form
        // here surfaces an "invalid value" diagnostic. (Stage 2 owns
        // semantic template-grammar validation; the parser only relies on the
        // existing attribute syntax gate.)
        let src = r#"
            extern "C" {
                #[extern_symbol(= 5)]
                fn hew_x();
            }
        "#;
        let result = parse(src);
        assert!(
            !result.errors.is_empty(),
            "malformed `#[extern_symbol]` argument list must produce a parser error"
        );
    }

    // ---------------------------------------------------------------------------
    // Leading-dot variant patterns (`.Variant` implicit-enum form)
    // ---------------------------------------------------------------------------

    /// Extract the arm patterns of the first `match` expression in the first
    /// function's body. Panics if the shape does not match.
    fn first_match_arm_patterns(source: &str) -> Vec<Pattern> {
        let result = parse(source);
        assert!(result.errors.is_empty(), "errors: {:?}", result.errors);
        let Item::Function(f) = &result.program.items[0].0 else {
            panic!("expected function item");
        };
        // A value-producing `match` lands in the block's `trailing_expr`; a
        // statement-position `match` is a `Stmt::Match`. Check both.
        if let Some(tail) = &f.body.trailing_expr {
            if let Expr::Match { arms, .. } = &tail.0 {
                return arms.iter().map(|a| a.pattern.0.clone()).collect();
            }
        }
        for (stmt, _) in &f.body.stmts {
            if let Stmt::Match { arms, .. } = stmt {
                return arms.iter().map(|a| a.pattern.0.clone()).collect();
            }
            if let Stmt::Expression(e) | Stmt::Return(Some(e)) = stmt {
                if let Expr::Match { arms, .. } = &e.0 {
                    return arms.iter().map(|a| a.pattern.0.clone()).collect();
                }
            }
        }
        panic!("no match expression found in function body");
    }

    /// A leading-dot tuple-payload variant pattern parses to the SAME
    /// `Pattern::Constructor` a bare (unqualified) name would, with the short
    /// variant name and its payload sub-patterns.
    #[test]
    fn leading_dot_constructor_pattern_parses_as_bare_constructor() {
        let source = "fn f(e: E) -> i64 { match e { .Some(x) => x, _ => 0 } }";
        let patterns = first_match_arm_patterns(source);
        match &patterns[0] {
            Pattern::Constructor { name, patterns } => {
                assert_eq!(name, "Some");
                assert_eq!(patterns.len(), 1);
                assert!(matches!(&patterns[0].0, Pattern::Identifier(n) if n == "x"));
            }
            other => panic!("expected Pattern::Constructor, got {other:?}"),
        }
    }

    /// A leading-dot unit variant pattern parses to a bare-name
    /// `Pattern::Identifier`, matching the unqualified unit-variant spelling.
    #[test]
    fn leading_dot_unit_variant_pattern_parses_as_identifier() {
        let source = "fn f(e: E) -> i64 { match e { .None => 0, _ => 1 } }";
        let patterns = first_match_arm_patterns(source);
        assert!(matches!(&patterns[0], Pattern::Identifier(n) if n == "None"));
    }

    /// Leading-dot variants compose with or-patterns: `.A(x) | .B(x)` parses to
    /// a `Pattern::Or` over two bare-name constructor leaves.
    #[test]
    fn leading_dot_or_pattern_parses() {
        let source = "fn f(e: E) -> i64 { match e { .A(x) | .B(x) => x, _ => 0 } }";
        let patterns = first_match_arm_patterns(source);
        let Pattern::Or(left, right) = &patterns[0] else {
            panic!("expected Pattern::Or, got {:?}", patterns[0]);
        };
        assert!(matches!(&left.0, Pattern::Constructor { name, .. } if name == "A"));
        assert!(matches!(&right.0, Pattern::Constructor { name, .. } if name == "B"));
    }

    /// Adding the leading-dot arm does not disturb the existing qualified-name
    /// (`Type::Variant`) constructor-pattern path: both spellings still parse,
    /// and the qualified form keeps its fully-qualified name.
    #[test]
    fn leading_dot_arm_leaves_qualified_pattern_path_intact() {
        let source = "fn f(e: E) -> i64 { match e { E::Some(x) => x, .None => 0, _ => 1 } }";
        let patterns = first_match_arm_patterns(source);
        match &patterns[0] {
            Pattern::Constructor { name, .. } => assert_eq!(name, "E::Some"),
            other => panic!("expected qualified Pattern::Constructor, got {other:?}"),
        }
        assert!(matches!(&patterns[1], Pattern::Identifier(n) if n == "None"));
    }
} // mod tests
