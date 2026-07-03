//! Hand-written recursive-descent parser with Pratt precedence for operator expressions.

// The parser is split across sibling submodules by grammar area. Each area is a
// fresh `impl Parser` block (Rust permits multiple inherent impls within a crate)
// reaching the shared cursor/error primitives in `core` and the `Parser` fields
// defined below via `self.`. The AST types and `Token` are re-exported
// `pub(crate)` so each area submodule picks them up through `use super::*`.
pub(crate) use crate::ast::{
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
    WireFieldMeta, WireMetadata,
};
pub(crate) use hew_lexer::Token;
use serde::Serialize;
use std::cell::Cell;
use std::rc::Rc;

mod actor_machine_supervisor;
mod core;
mod expressions;
mod items;
mod patterns;
mod precedence;
mod statements;
mod types;
mod wire;

// Re-export the precedence free fns so the area submodules reach them through
// `use super::*` (the Pratt parser in `expressions` and the clone-prefix probe
// in `core` call them as bare names).
pub(crate) use precedence::{
    infix_bp, prefix_bp, token_begins_clone_operand, token_to_binop, CLONE_PREFIX_BP,
};

#[cfg(test)]
mod tests;

const EMBEDDED_NUL_STRING_MESSAGE: &str =
    "embedded NUL (\\0) in string literal is not supported by the null-terminated string ABI";

pub(crate) type ParsedTraitBoundArgs = (Option<Vec<Spanned<TypeExpr>>>, Vec<AssocTypeBinding>);
pub(crate) type StructInitFields = (Vec<(String, Spanned<Expr>)>, Option<Box<Spanned<Expr>>>);

/// Parse an integer literal string, returning both value and radix.
///
/// Handles hex (`0x`), octal (`0o`), binary (`0b`) prefixes and underscore separators.
/// Merges the old `parse_int_literal` + `detect_int_radix` to avoid scanning twice.
pub(crate) fn parse_int_literal(s: &str) -> Result<(i64, IntRadix), std::num::ParseIntError> {
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

/// Parse the digits following a unary `-` as a single negated integer literal.
///
/// Delegates to [`parse_int_literal`] and negates the result for every
/// magnitude that already fits a positive `i64`. Falls back to parsing
/// `-<digits>` directly (decimal only) so `i64::MIN`/`isize::MIN`'s
/// magnitude (`9223372036854775808`, one past `i64::MAX`) — which cannot be
/// tokenized as a positive `i64` at all — still parses when written with its
/// sign attached.
pub(crate) fn parse_negated_int_literal(
    s: &str,
) -> Result<(i64, IntRadix), std::num::ParseIntError> {
    match parse_int_literal(s) {
        Ok((val, radix)) => Ok((-val, radix)),
        Err(original_err) => {
            let cleaned: String = s.chars().filter(|c| *c != '_').collect();
            match format!("-{cleaned}").parse::<i64>() {
                Ok(v) => Ok((v, IntRadix::Decimal)),
                Err(_) => Err(original_err),
            }
        }
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

pub(crate) fn parse_duration_literal(s: &str) -> Option<i64> {
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
pub(crate) fn resolve_max_heap_args(args: &[AttributeArg]) -> Result<u64, String> {
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
pub(crate) fn unquote_str(s: &str) -> &str {
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
pub(crate) fn unescape_string(s: &str) -> (String, Vec<(usize, &'static str)>) {
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

pub(crate) fn embedded_nul_string_error(span: Span) -> ParseError {
    ParseError {
        message: EMBEDDED_NUL_STRING_MESSAGE.to_string(),
        span,
        hint: None,
        severity: Severity::Error,
        kind: ParseDiagnosticKind::InvalidLiteral,
    }
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
pub(crate) fn parse_string_parts(
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

            // Detect Python-style `{{` doubled-brace — invalid in Hew.
            //
            // Hew uses `\{` / `\}` for literal braces, not `{{` / `}}`.
            // When `expr_open` is `{` and the very next character is also `{`,
            // the user is most likely applying Python-style escaping.  Emit a
            // clear diagnostic and skip the entire interpolation so that no
            // cascade errors are produced from the block-expression parse.
            if expr_open == "{" {
                if let Some(&(_, '{')) = chars.get(idx) {
                    let abs_open = inner_offset + byte_pos;
                    errors.push(ParseError {
                        message: "f-string interpolation opened with `{{`; literal braces in Hew \
                             use `\\{` and `\\}`, not `{{` and `}}`"
                            .to_string(),
                        span: abs_open..abs_open + 2,
                        hint: Some(
                            "to include a literal `{` in an f-string, write `\\{`".to_string(),
                        ),
                        severity: Severity::Error,
                        kind: ParseDiagnosticKind::InvalidLiteral,
                    });
                    // Skip from the second `{` through the matching closing `}`.
                    let mut depth: u32 = 1;
                    while idx < chars.len() {
                        match chars[idx].1 {
                            '{' => depth += 1,
                            '}' => {
                                depth -= 1;
                                if depth == 0 {
                                    break;
                                }
                            }
                            _ => {}
                        }
                        idx += 1;
                    }
                    // Advance past the closing `}`.
                    if idx < chars.len() {
                        idx += 1;
                    }
                    continue;
                }
            }

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
                // Parse the sub-expression with its spans pre-shifted to
                // absolute source positions.  Every AST node span and every
                // diagnostic emitted by the sub-parser therefore refers to the
                // original source without any further rebasing.
                let base = inner_offset + expr_start_byte;
                let mut sub_parser = Parser::new_with_offset(expr_text, base);
                let parsed = sub_parser.parse_expr();
                errors.extend(sub_parser.errors);
                if let Some(spanned_expr) = parsed {
                    parts.push(StringPart::Expr(spanned_expr));
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

    if parts
        .iter()
        .any(|part| matches!(part, StringPart::Literal(text) if text.contains('\0')))
    {
        errors.push(embedded_nul_string_error(
            span_start..span_start + raw.len(),
        ));
    }

    parts
}

/// Maximum nesting depth for recursive parse functions.
pub(crate) const MAX_DEPTH: usize = 256;

/// RAII guard that decrements the parser recursion depth on drop.
///
/// Holds a cloned `Rc` of the depth counter so that `&mut self` reborrows
/// in parse methods cannot invalidate the reference — eliminating the
/// use-after-invalidation UB that a raw-pointer approach would introduce.
#[derive(Debug)]
pub(crate) struct RecursionGuard(Rc<Cell<usize>>);

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
pub(crate) struct NoStructLiteralGuard {
    cell: Rc<Cell<bool>>,
    prev: bool,
}

impl Drop for NoStructLiteralGuard {
    fn drop(&mut self) {
        self.cell.set(self.prev);
    }
}

/// Snapshot of parser position for speculative (backtracking) parses.
pub(crate) struct SavedPos {
    pos: usize,
    error_count: usize,
    angle_mutation_count: usize,
    /// Preserves `last_token_end` so a backtracking restore does not leave a
    /// stale end position that would corrupt EOF-based span calculations.
    last_token_end: usize,
}

/// Parser state wrapping a token stream.
#[derive(Debug)]
pub struct Parser<'src> {
    // Fields are `pub(crate)` so the per-grammar-area `impl Parser` blocks in the
    // sibling submodules can read them through `self.`. This is a visibility-only
    // promotion — the fields stay crate-internal and behaviour is unchanged.
    pub(crate) tokens: Vec<(Token<'src>, Span)>,
    pub(crate) pos: usize,
    pub(crate) errors: Vec<ParseError>,
    pub(crate) depth: Rc<Cell<usize>>,
    /// Stack of token mutations performed by `eat_closing_angle`, so they can
    /// be rolled back on speculative-parse backtrack.
    pub(crate) angle_mutations: Vec<(usize, (Token<'src>, Span))>,
    /// True while parsing an impl-method parameter list that accepts bare
    /// `self` as sugar for a `Self` receiver parameter.
    pub(crate) allow_implicit_self_params: bool,
    /// Number of enclosing `scope { ... }` expression bodies being parsed.
    pub(crate) scope_expr_depth: usize,
    /// Number of enclosing `fork { ... }` child-task block bodies being parsed.
    pub(crate) fork_block_depth: usize,
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
    pub(crate) no_struct_literal: Rc<Cell<bool>>,
    /// End byte of the most recently consumed token (absolute).
    ///
    /// For a full-program parse this starts at 0.  For an f-string sub-parser
    /// it is initialised to the byte offset of the extracted expression in the
    /// original source — so token spans are already absolute at construction —
    /// and then updated on every `advance()`.
    ///
    /// `peek_span()` falls back to `last_token_end..last_token_end` at EOF so
    /// that span-end calculations like `let end = self.peek_span().start` in
    /// `parse_primary` return the correct position even when no token follows —
    /// in particular in f-string sub-parsers that parse a short expression with
    /// no trailing delimiter.
    pub(crate) last_token_end: usize,
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
