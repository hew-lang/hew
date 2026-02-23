//! Syntax highlighting for Hew code using the hew-lexer.
//!
//! Produces HTML `<span>` elements with inline styles matching the Shiki
//! `dark-plus` theme as configured on the Hew website (`hew.sh`).

use hew_lexer::{Lexer, Span, Token};

// ── Hew website design-system colours (dark theme) ───────────────────────────

/// Keywords: cyan-400
const KW: &str = "#22d3ee";
/// Types and type names: teal-400
const TY: &str = "#2dd4bf";
/// String literals: green-400
const STR: &str = "#4ade80";
/// Comments: grey
const CMT: &str = "#8b949e";
/// Function names / calls: yellow-400
const FN: &str = "#facc15";
/// Numeric literals: orange-400
const NUM: &str = "#fb923c";
/// Operators and punctuation: slate-400
const OP: &str = "#94a3b8";
/// Boolean / constant literals: orange-400
const CONST: &str = "#fb923c";
/// Plain text / identifiers: light grey
const PLAIN: &str = "#e2e8f0";
/// Code background: gray-900
const BG: &str = "#111827";

/// Classify a token into a highlight colour.
#[expect(clippy::too_many_lines, reason = "exhaustive token→colour match")]
fn token_color(tok: &Token<'_>) -> &'static str {
    match tok {
        // ── Keywords ──────────────────────────────────────────────────
        Token::Let
        | Token::Var
        | Token::Const
        | Token::Fn
        | Token::If
        | Token::Else
        | Token::Match
        | Token::Loop
        | Token::For
        | Token::While
        | Token::Break
        | Token::Continue
        | Token::Return
        | Token::Import
        | Token::Pub
        | Token::Package
        | Token::Super
        | Token::Struct
        | Token::Enum
        | Token::Trait
        | Token::Impl
        | Token::Wire
        | Token::Actor
        | Token::Supervisor
        | Token::Child
        | Token::Restart
        | Token::Budget
        | Token::Strategy
        | Token::Scope
        | Token::Spawn
        | Token::Async
        | Token::Await
        | Token::Receive
        | Token::Init
        | Token::Type
        | Token::Dyn
        | Token::Move
        | Token::Try
        | Token::Reserved
        | Token::Optional
        | Token::Deprecated
        | Token::Default
        | Token::Unsafe
        | Token::Extern
        | Token::Foreign
        | Token::In
        | Token::Select
        | Token::Race
        | Token::Join
        | Token::From
        | Token::After
        | Token::Gen
        | Token::Yield
        | Token::Where
        | Token::Cooperate
        | Token::Catch
        | Token::Defer
        | Token::Pure
        | Token::As => KW,

        // ── Constants (strategy values + booleans) ────────────────────
        Token::Permanent
        | Token::Transient
        | Token::Temporary
        | Token::OneForOne
        | Token::OneForAll
        | Token::RestForOne
        | Token::True
        | Token::False => CONST,

        // ── String literals ───────────────────────────────────────────
        Token::StringLit(_)
        | Token::RawString(_)
        | Token::InterpolatedString(_)
        | Token::RegexLiteral(_) => STR,

        // ── Numeric literals ──────────────────────────────────────────
        Token::Integer(_) | Token::Float(_) | Token::Duration(_) => NUM,

        // ── Doc comments ──────────────────────────────────────────────
        Token::DocComment(_) | Token::InnerDocComment(_) => CMT,

        // ── Labels / lifetimes ────────────────────────────────────────
        Token::Label(_) => TY,

        // ── Operators ─────────────────────────────────────────────────
        Token::EqualEqual
        | Token::NotEqual
        | Token::FatArrow
        | Token::Arrow
        | Token::LeftArrow
        | Token::LessEqual
        | Token::GreaterEqual
        | Token::MatchOp
        | Token::NotMatchOp
        | Token::AmpAmp
        | Token::PipePipe
        | Token::DotDotEqual
        | Token::DotDot
        | Token::DoubleColon
        | Token::HashBracket
        | Token::PlusEqual
        | Token::MinusEqual
        | Token::StarEqual
        | Token::SlashEqual
        | Token::PercentEqual
        | Token::Plus
        | Token::Minus
        | Token::Star
        | Token::Slash
        | Token::Percent
        | Token::Equal
        | Token::Bang
        | Token::Less
        | Token::Greater
        | Token::Question
        | Token::Pipe
        | Token::Ampersand
        | Token::Caret
        | Token::Tilde
        | Token::LessLess
        | Token::GreaterGreater
        | Token::AmpEqual
        | Token::PipeEqual
        | Token::CaretEqual
        | Token::LessLessEqual
        | Token::GreaterGreaterEqual
        | Token::At
        | Token::Dot => OP,

        // ── Delimiters, punctuation, errors ──────────────────────────
        Token::LeftParen
        | Token::RightParen
        | Token::LeftBrace
        | Token::RightBrace
        | Token::LeftBracket
        | Token::RightBracket
        | Token::Semicolon
        | Token::Comma
        | Token::Colon
        | Token::Error
        | Token::_BlockComment
        | Token::_LineComment => PLAIN,

        // ── Identifiers ───────────────────────────────────────────────
        Token::Identifier(id) => classify_identifier(id),
    }
}

/// Classify an identifier as a type (`PascalCase`) or plain variable.
fn classify_identifier(id: &str) -> &'static str {
    // Built-in primitive types
    match id {
        "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" | "f32" | "f64" | "int"
        | "uint" | "byte" | "float" | "isize" | "usize" | "bool" | "char" | "string" | "bytes"
        | "void" | "Self" => TY,
        // PascalCase → type name
        _ if id.starts_with(|c: char| c.is_ascii_uppercase()) => TY,
        _ => PLAIN,
    }
}

/// Escape HTML special characters in source text.
fn html_escape(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '&' => out.push_str("&amp;"),
            '<' => out.push_str("&lt;"),
            '>' => out.push_str("&gt;"),
            '"' => out.push_str("&quot;"),
            _ => out.push(c),
        }
    }
    out
}

/// Check whether a gap between tokens looks like a comment and return the
/// colour to apply. Regular `//` and `/* */` comments are skipped by the lexer
/// so they appear as gaps.
fn classify_gap(text: &str) -> Option<&'static str> {
    let trimmed = text.trim();
    if trimmed.starts_with("//") || trimmed.starts_with("/*") {
        Some(CMT)
    } else {
        None
    }
}

/// Highlight Hew source code, returning an HTML string wrapped in
/// `<pre><code>` with inline-styled `<span>` elements and per-line wrappers.
///
/// The output structure mirrors Shiki: each line is a `<span class="line">`
/// and tokens are `<span style="color:#HEX">`.
#[must_use]
pub fn highlight_code(source: &str) -> String {
    use std::fmt::Write;

    let mut out = String::with_capacity(source.len() * 3);
    let _ = write!(
        out,
        "<pre class=\"shiki\" style=\"background-color:{BG};color:{PLAIN}\">"
    );
    out.push_str("<code>");

    // Collect tokens with spans
    let lexer = Lexer::new(source);
    let tokens: Vec<(Token<'_>, Span)> = lexer.collect();

    // Process source line-by-line, overlaying tokens
    let mut pos: usize = 0;

    // We'll walk through the source, emitting styled spans for tokens and
    // preserving gaps (whitespace, comments) between them.
    out.push_str("<span class=\"line\">");

    for (tok, span) in &tokens {
        // Emit any gap before this token (whitespace, comments)
        if span.start > pos {
            let gap = &source[pos..span.start];
            emit_gap(&mut out, gap);
        }

        // Emit the token
        let text = &source[span.start..span.end];
        let color = token_color(tok);
        // Check if the next identifier after `fn` should be colored as a function name
        emit_styled(&mut out, text, color);

        pos = span.end;
    }

    // Emit any trailing content
    if pos < source.len() {
        let gap = &source[pos..];
        emit_gap(&mut out, gap);
    }

    out.push_str("</span>"); // close last line
    out.push_str("</code></pre>");
    out
}

/// Highlight a single-line code signature for use in documentation headers.
///
/// Returns inline HTML with styled spans (no `<pre>` wrapper).
#[must_use]
pub fn highlight_signature(source: &str) -> String {
    let mut out = String::with_capacity(source.len() * 2);
    let lexer = Lexer::new(source);
    let tokens: Vec<(Token<'_>, Span)> = lexer.collect();
    let mut pos: usize = 0;

    for (tok, span) in &tokens {
        if span.start > pos {
            let gap = &source[pos..span.start];
            out.push_str(&html_escape(gap));
        }
        let text = &source[span.start..span.end];
        let color = token_color(tok);
        if color == PLAIN {
            out.push_str(&html_escape(text));
        } else {
            out.push_str("<span style=\"color:");
            out.push_str(color);
            out.push_str("\">");
            out.push_str(&html_escape(text));
            out.push_str("</span>");
        }
        pos = span.end;
    }

    if pos < source.len() {
        out.push_str(&html_escape(&source[pos..]));
    }

    out
}

/// Emit a gap (inter-token text) into the output, handling newlines to create
/// line spans and detecting comments.
fn emit_gap(out: &mut String, gap: &str) {
    if let Some(color) = classify_gap(gap) {
        // It's a comment — style the whole gap
        for (i, line) in gap.split('\n').enumerate() {
            if i > 0 {
                out.push_str("</span>\n<span class=\"line\">");
            }
            if !line.is_empty() {
                out.push_str("<span style=\"color:");
                out.push_str(color);
                out.push_str(";font-style:italic\">");
                out.push_str(&html_escape(line));
                out.push_str("</span>");
            }
        }
    } else {
        // Plain whitespace / text
        for (i, line) in gap.split('\n').enumerate() {
            if i > 0 {
                out.push_str("</span>\n<span class=\"line\">");
            }
            if !line.is_empty() {
                out.push_str(&html_escape(line));
            }
        }
    }
}

/// Emit a styled token span, splitting on newlines to maintain line structure.
fn emit_styled(out: &mut String, text: &str, color: &str) {
    for (i, line) in text.split('\n').enumerate() {
        if i > 0 {
            out.push_str("</span>\n<span class=\"line\">");
        }
        if !line.is_empty() {
            if color == PLAIN {
                out.push_str(&html_escape(line));
            } else {
                out.push_str("<span style=\"color:");
                out.push_str(color);
                out.push_str("\">");
                out.push_str(&html_escape(line));
                out.push_str("</span>");
            }
        }
    }
}

/// Post-process highlighted HTML to colour function names that follow the `fn`
/// keyword. This is a simple heuristic applied after initial tokenization.
///
/// We detect `<span style="color:#22d3ee">fn</span>` followed by whitespace and
/// an uncolored identifier, then wrap it with the function colour.
#[must_use]
pub fn apply_function_name_heuristic(html: &str) -> String {
    use std::fmt::Write;

    // Pattern: fn keyword span, then whitespace, then plain identifier
    let fn_span = format!("<span style=\"color:{KW}\">fn</span>");
    let mut result = String::with_capacity(html.len());
    let mut search_from = 0;

    while let Some(idx) = html[search_from..].find(&fn_span) {
        let abs_idx = search_from + idx;
        result.push_str(&html[search_from..abs_idx + fn_span.len()]);
        let after = &html[abs_idx + fn_span.len()..];

        // Skip whitespace
        let ws_len = after.len() - after.trim_start().len();
        if ws_len > 0 {
            result.push_str(&after[..ws_len]);
        }
        let rest = &after[ws_len..];

        // Check if next text is an uncoloured identifier (not wrapped in a span)
        if !rest.starts_with('<') {
            // Find the end of the identifier
            let id_end = rest
                .find(|c: char| !c.is_alphanumeric() && c != '_')
                .unwrap_or(rest.len());
            if id_end > 0 {
                let _ = write!(
                    result,
                    "<span style=\"color:{FN}\">{}</span>",
                    &rest[..id_end]
                );
                search_from = abs_idx + fn_span.len() + ws_len + id_end;
                continue;
            }
        }
        search_from = abs_idx + fn_span.len() + ws_len;
    }

    result.push_str(&html[search_from..]);
    result
}

/// Full highlight pipeline: tokenize → style → apply function-name heuristic.
#[must_use]
pub fn highlight(source: &str) -> String {
    apply_function_name_heuristic(&highlight_code(source))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_keyword_highlighting() {
        let html = highlight("fn main() {}");
        assert!(html.contains(&format!("color:{KW}\">fn</span>")));
        assert!(html.contains(&format!("color:{FN}\">main</span>")));
    }

    #[test]
    fn string_highlighting() {
        let html = highlight("let s = \"hello\";");
        assert!(html.contains(&format!("color:{STR}")));
        assert!(html.contains("hello"));
    }

    #[test]
    fn type_highlighting() {
        let html = highlight("let x: i32 = 42;");
        assert!(html.contains(&format!("color:{TY}\">i32</span>")));
        assert!(html.contains(&format!("color:{NUM}\">42</span>")));
    }

    #[test]
    fn signature_highlighting() {
        let html = highlight_signature("pub fn add(a: i32, b: i32) -> i32");
        assert!(html.contains(&format!("color:{KW}\">pub</span>")));
        assert!(html.contains(&format!("color:{KW}\">fn</span>")));
        assert!(html.contains(&format!("color:{TY}\">i32</span>")));
    }

    #[test]
    fn comment_in_gap() {
        let html = highlight("// comment\nfn main() {}");
        assert!(html.contains(&format!("color:{CMT}")));
    }

    #[test]
    fn pascal_case_types() {
        let html = highlight("let v: Vec<i32> = Vec::new();");
        assert!(html.contains(&format!("color:{TY}\">Vec</span>")));
    }

    #[test]
    fn multiline_preserves_lines() {
        let html = highlight("fn a() {\n    let x = 1;\n}");
        // Should have multiple line spans
        let line_count = html.matches("class=\"line\"").count();
        assert!(
            line_count >= 3,
            "expected at least 3 lines, got {line_count}"
        );
    }
}
