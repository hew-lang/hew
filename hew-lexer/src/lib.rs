//! Hew language lexer using logos derive macros.
//!
//! Tokenizes Hew source code into a stream of [`Token`] values with byte-offset
//! [`Span`] information. Built on the [`logos`] crate for compile-time DFA
//! generation.

use logos::Logos;

/// Byte-offset span within source text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    /// Start byte offset (inclusive).
    pub start: usize,
    /// End byte offset (exclusive).
    pub end: usize,
}

/// Lex `source` into a vector of (token, span) pairs, skipping whitespace and
/// comments. Invalid bytes produce [`Token::Error`].
#[must_use]
pub fn lex(source: &str) -> Vec<(Token<'_>, Span)> {
    let lexer = Token::lexer(source);
    let mut out = Vec::new();
    for (result, span) in lexer.spanned() {
        let tok = match result {
            Ok(t) => t,
            Err(()) => Token::Error,
        };
        out.push((
            tok,
            Span {
                start: span.start,
                end: span.end,
            },
        ));
    }
    out
}

/// Wrapper around the logos lexer iterator with span tracking.
pub struct Lexer<'src> {
    inner: logos::SpannedIter<'src, Token<'src>>,
}

impl std::fmt::Debug for Lexer<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Lexer").finish_non_exhaustive()
    }
}

impl<'src> Lexer<'src> {
    /// Create a new lexer over `source`.
    #[must_use]
    pub fn new(source: &'src str) -> Self {
        Self {
            inner: Token::lexer(source).spanned(),
        }
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = (Token<'src>, Span);

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(result, span)| {
            let tok = match result {
                Ok(t) => t,
                Err(()) => Token::Error,
            };
            (
                tok,
                Span {
                    start: span.start,
                    end: span.end,
                },
            )
        })
    }
}

// ---------------------------------------------------------------------------
// Block-comment callback (supports nesting)
// ---------------------------------------------------------------------------

fn line_comment<'s>(_lex: &mut logos::Lexer<'s, Token<'s>>) -> logos::Skip {
    logos::Skip
}

fn block_comment<'s>(lex: &mut logos::Lexer<'s, Token<'s>>) -> logos::FilterResult<(), ()> {
    let bytes = lex.remainder().as_bytes();
    let mut depth: u32 = 1;
    let mut i = 0;
    while i < bytes.len() {
        if i + 1 < bytes.len() && bytes[i] == b'/' && bytes[i + 1] == b'*' {
            depth += 1;
            i += 2;
        } else if i + 1 < bytes.len() && bytes[i] == b'*' && bytes[i + 1] == b'/' {
            depth -= 1;
            if depth == 0 {
                lex.bump(i + 2);
                return logos::FilterResult::Skip;
            }
            i += 2;
        } else {
            i += 1;
        }
    }
    // Unterminated — bump rest so we don't loop forever.
    lex.bump(bytes.len());
    logos::FilterResult::Error(())
}

fn skip_quoted(bytes: &[u8], mut i: usize, quote: u8, escapes: bool) -> usize {
    i += 1;
    while i < bytes.len() {
        if escapes && bytes[i] == b'\\' {
            i = (i + 2).min(bytes.len());
        } else if bytes[i] == quote {
            return i + 1;
        } else {
            i += 1;
        }
    }
    i
}

fn interpolated_string<'s>(lex: &mut logos::Lexer<'s, Token<'s>>) -> Result<&'s str, ()> {
    let bytes = lex.remainder().as_bytes();
    let mut i = 0;
    let mut brace_depth: usize = 0;
    while i < bytes.len() {
        match bytes[i] {
            b'\\' => i = (i + 2).min(bytes.len()),
            b'{' => {
                brace_depth += 1;
                i += 1;
            }
            b'}' if brace_depth > 0 => {
                brace_depth -= 1;
                i += 1;
            }
            b'"' if brace_depth == 0 => {
                lex.bump(i + 1);
                return Ok(lex.slice());
            }
            b'r' if brace_depth > 0 && matches!(bytes.get(i + 1), Some(b'"')) => {
                i = skip_quoted(bytes, i + 1, b'"', false);
            }
            b'"' | b'\'' if brace_depth > 0 => {
                i = skip_quoted(bytes, i, bytes[i], true);
            }
            _ => i += 1,
        }
    }
    lex.bump(bytes.len());
    Err(())
}

// ---------------------------------------------------------------------------
// Token enum
// ---------------------------------------------------------------------------

/// A single token produced by the Hew lexer.
#[derive(Logos, Debug, Clone, PartialEq, Eq, Hash)]
#[logos(skip r"[ \t\r\n]+")] // skip whitespace
                             // Line comments (non-doc) are handled by the _LineComment variant below.
pub enum Token<'src> {
    // ── Keywords ──────────────────────────────────────────────────────
    #[token("let")]
    Let,
    #[token("var")]
    Var,
    #[token("const")]
    Const,
    #[token("mut")]
    Mut,
    #[token("fn")]
    Fn,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("match")]
    Match,
    #[token("loop")]
    Loop,
    #[token("for")]
    For,
    #[token("while")]
    While,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("return")]
    Return,
    #[token("import")]
    Import,
    #[token("pub")]
    Pub,
    #[token("package")]
    Package,
    #[token("super")]
    Super,
    #[token("indirect")]
    Indirect,
    #[token("enum")]
    Enum,
    #[token("trait")]
    Trait,
    #[token("impl")]
    Impl,
    #[token("actor")]
    Actor,
    #[token("supervisor")]
    Supervisor,
    #[token("child")]
    Child,
    #[token("restart")]
    Restart,
    #[token("budget")]
    Budget,
    #[token("strategy")]
    Strategy,
    #[token("permanent")]
    Permanent,
    #[token("transient")]
    Transient,
    #[token("temporary")]
    Temporary,
    #[token("one_for_one")]
    OneForOne,
    #[token("one_for_all")]
    OneForAll,
    #[token("rest_for_one")]
    RestForOne,
    #[token("simple_one_for_one")]
    SimpleOneForOne,
    #[token("pool")]
    Pool,
    /// `brutal_kill` shutdown directive on a supervisor child: skip the
    /// graceful-stop deadline and terminate the child immediately.
    #[token("brutal_kill")]
    BrutalKill,
    #[token("scope")]
    Scope,
    #[token("fork")]
    Fork,
    #[token("spawn")]
    Spawn,
    #[token("async")]
    Async,
    #[token("await")]
    Await,
    #[token("await_restart")]
    AwaitRestart,
    #[token("receive")]
    Receive,
    #[token("init")]
    Init,
    #[token("record")]
    Record,
    #[token("type")]
    Type,
    #[token("this")]
    This,
    #[token("dyn")]
    Dyn,
    #[token("move")]
    Move,
    #[token("try")]
    Try,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("reserved")]
    Reserved,
    #[token("optional")]
    Optional,
    #[token("deprecated")]
    Deprecated,
    #[token("default")]
    Default,
    #[token("unsafe")]
    Unsafe,
    #[token("extern")]
    Extern,
    #[token("foreign")]
    Foreign,
    #[token("in")]
    In,
    #[token("select")]
    Select,
    #[token("race")]
    Race,
    #[token("join")]
    Join,
    #[token("from")]
    From,
    #[token("after")]
    After,
    #[token("gen")]
    Gen,
    #[token("yield")]
    Yield,
    #[token("where")]
    Where,
    #[token("cooperate")]
    Cooperate,
    #[token("catch")]
    Catch,
    #[token("defer")]
    Defer,
    #[token("as")]
    As,
    #[token("machine")]
    Machine,
    #[token("state")]
    State,
    #[token("event")]
    Event,
    #[token("on")]
    On,
    #[token("when")]
    When,
    #[token("entry")]
    Entry,
    #[token("exit")]
    Exit,
    #[token("emit")]
    Emit,
    #[token("is")]
    Is,

    // ── Multi-char operators (must precede single-char) ───────────────
    #[token("==")]
    EqualEqual,
    #[token("!=")]
    NotEqual,
    #[token("=>")]
    FatArrow,
    #[token("->")]
    Arrow,
    #[token("<<=")]
    LessLessEqual,
    #[token(">>=")]
    GreaterGreaterEqual,
    #[token("<<")]
    LessLess,
    #[token(">>")]
    GreaterGreater,
    #[token("<=")]
    LessEqual,
    #[token(">=")]
    GreaterEqual,
    #[token("&&")]
    AmpAmp,
    #[token("||")]
    PipePipe,
    #[token("&=")]
    AmpEqual,
    #[token("|=")]
    PipeEqual,
    #[token("^=")]
    CaretEqual,
    #[token("..=")]
    DotDotEqual,
    #[token("..")]
    DotDot,
    #[token("::")]
    DoubleColon,
    #[token("#[")]
    HashBracket,

    // ── Wrapping arithmetic operators ─────────────────────────────────
    // These are SINGLE tokens, not `&` followed by `+`/`-`/`*`.
    // Logos uses maximal munch: `&+` with no whitespace between the
    // characters lexes as `AmpPlus` (wrapping add), while `& +` (with
    // whitespace) lexes as two tokens `Ampersand` + `Plus`. This is the
    // disambiguation rule: spacing determines operator identity.
    #[token("&+")]
    AmpPlus,
    #[token("&-")]
    AmpMinus,
    #[token("&*")]
    AmpStar,

    // ── Compound assignment ───────────────────────────────────────────
    #[token("+=")]
    PlusEqual,
    #[token("-=")]
    MinusEqual,
    #[token("*=")]
    StarEqual,
    #[token("/=")]
    SlashEqual,
    #[token("%=")]
    PercentEqual,

    // ── Single-char operators ─────────────────────────────────────────
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("=")]
    Equal,
    #[token("!")]
    Bang,
    #[token("<")]
    Less,
    #[token(">")]
    Greater,
    #[token("?")]
    Question,
    #[token("|")]
    Pipe,
    #[token("&")]
    Ampersand,
    #[token("^")]
    Caret,
    #[token("~")]
    Tilde,
    #[token("@")]
    At,
    #[token(".")]
    Dot,

    // ── Delimiters ────────────────────────────────────────────────────
    #[token("(")]
    LeftParen,
    #[token(")")]
    RightParen,
    #[token("{")]
    LeftBrace,
    #[token("}")]
    RightBrace,
    #[token("[")]
    LeftBracket,
    #[token("]")]
    RightBracket,
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    /// Loop label, e.g. `@outer`.
    #[regex(r"@[a-zA-Z_][a-zA-Z0-9_]*")]
    Label(&'src str),

    // ── Literals ──────────────────────────────────────────────────────
    /// Duration literal, e.g. `100ms`, `5s`. Matched before plain integers.
    #[regex(r"[0-9][0-9_]*(ns|us|ms|[smh])", priority = 5)]
    Duration(&'src str),

    /// Floating-point literal with optional scientific notation.
    #[regex(r"[0-9][0-9_]*\.[0-9][0-9_]*([eE][+-]?[0-9]+)?", priority = 4)]
    Float(&'src str),

    /// Integer literal (decimal, hex, octal, or binary with optional `_` separators).
    #[regex(r"0[xX][0-9a-fA-F][0-9a-fA-F_]*", priority = 4)]
    #[regex(r"0[oO][0-7][0-7_]*", priority = 4)]
    #[regex(r"0[bB][01][01_]*", priority = 4)]
    #[regex(r"[0-9][0-9_]*", priority = 3)]
    Integer(&'src str),

    /// Interpolated string literal `f"..."`.
    #[regex(r#"f""#, interpolated_string)]
    InterpolatedString(&'src str),

    /// Raw string literal `r"..."` (no escape processing).
    #[regex(r#"r"[^"]*""#)]
    RawString(&'src str),

    /// Regex literal `re"..."`.
    #[regex(r#"re"([^"\\]|\\.)*""#)]
    RegexLiteral(&'src str),

    /// Byte string literal `b"..."`.
    #[regex(r#"b"([^"\\]|\\.)*""#)]
    ByteStringLit(&'src str),

    /// Regular string literal `"..."` with escape sequences.
    #[regex(r#""([^"\\]|\\.)*""#)]
    StringLit(&'src str),

    /// Character literal `'a'`, `'\n'`, `'\u{1F600}'`, `'\x41'`, etc.
    ///
    /// The escape body accepts the named escapes (`\.`), a `\u{H...}` Unicode
    /// escape (1–6 hex digits), and a `\xHH` byte escape, mirroring the string
    /// literal escape set so an invisible/confusable scalar can be written as a
    /// `\u{...}` escape rather than a raw codepoint.
    #[regex(r"'([^'\\]|\\u\{[0-9a-fA-F]{1,6}\}|\\x[0-9a-fA-F][0-9a-fA-F]|\\.)'")]
    CharLit(&'src str),

    /// Identifier. Keywords have higher priority via `#[token]`.
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifier(&'src str),

    // ── Block comments (nested) ───────────────────────────────────────
    #[regex(r"/\*", block_comment)]
    _BlockComment,

    // ── Line comments ─────────────────────────────────────────────────
    /// Regular line comment (skipped).
    #[regex(r"//[^\n]*", line_comment, priority = 1, allow_greedy = true)]
    _LineComment,

    /// Outer doc comment (`/// ...`).
    #[regex(r"///[^\n]*", priority = 5, allow_greedy = true)]
    DocComment(&'src str),

    /// Inner doc comment (`//! ...`).
    #[regex(r"//![^\n]*", priority = 5, allow_greedy = true)]
    InnerDocComment(&'src str),

    /// Invalid / unrecognised byte sequence.
    Error,
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(kw) = self.keyword_str() {
            return write!(f, "`{kw}`");
        }
        match self {
            // Delimiters and punctuation
            Token::LeftParen => f.write_str("`(`"),
            Token::RightParen => f.write_str("`)`"),
            Token::LeftBrace => f.write_str("`{`"),
            Token::RightBrace => f.write_str("`}`"),
            Token::LeftBracket => f.write_str("`[`"),
            Token::RightBracket => f.write_str("`]`"),
            Token::Semicolon => f.write_str("`;`"),
            Token::Comma => f.write_str("`,`"),
            Token::Colon => f.write_str("`:`"),
            Token::Dot => f.write_str("`.`"),
            Token::DotDot => f.write_str("`..`"),
            Token::DotDotEqual => f.write_str("`..=`"),
            Token::DoubleColon => f.write_str("`::`"),
            Token::HashBracket => f.write_str("`#[`"),
            Token::At => f.write_str("`@`"),
            // Operators
            Token::Plus => f.write_str("`+`"),
            Token::Minus => f.write_str("`-`"),
            Token::Star => f.write_str("`*`"),
            Token::Slash => f.write_str("`/`"),
            Token::Percent => f.write_str("`%`"),
            Token::Equal => f.write_str("`=`"),
            Token::EqualEqual => f.write_str("`==`"),
            Token::NotEqual => f.write_str("`!=`"),
            Token::Less => f.write_str("`<`"),
            Token::Greater => f.write_str("`>`"),
            Token::LessEqual => f.write_str("`<=`"),
            Token::GreaterEqual => f.write_str("`>=`"),
            Token::Bang => f.write_str("`!`"),
            Token::AmpAmp => f.write_str("`&&`"),
            Token::PipePipe => f.write_str("`||`"),
            Token::Pipe => f.write_str("`|`"),
            Token::Ampersand => f.write_str("`&`"),
            Token::Caret => f.write_str("`^`"),
            Token::Tilde => f.write_str("`~`"),
            Token::Question => f.write_str("`?`"),
            Token::FatArrow => f.write_str("`=>`"),
            Token::Arrow => f.write_str("`->`"),

            Token::LessLess => f.write_str("`<<`"),
            Token::GreaterGreater => f.write_str("`>>`"),
            // Wrapping arithmetic operators
            Token::AmpPlus => f.write_str("`&+`"),
            Token::AmpMinus => f.write_str("`&-`"),
            Token::AmpStar => f.write_str("`&*`"),
            // Compound assignment
            Token::PlusEqual => f.write_str("`+=`"),
            Token::MinusEqual => f.write_str("`-=`"),
            Token::StarEqual => f.write_str("`*=`"),
            Token::SlashEqual => f.write_str("`/=`"),
            Token::PercentEqual => f.write_str("`%=`"),
            Token::AmpEqual => f.write_str("`&=`"),
            Token::PipeEqual => f.write_str("`|=`"),
            Token::CaretEqual => f.write_str("`^=`"),
            Token::LessLessEqual => f.write_str("`<<=`"),
            Token::GreaterGreaterEqual => f.write_str("`>>=`"),
            // Literals and identifiers
            Token::Identifier(name) => write!(f, "identifier `{name}`"),
            Token::Integer(val) => write!(f, "integer `{val}`"),
            Token::Float(val) => write!(f, "float `{val}`"),
            Token::StringLit(_) => f.write_str("string literal"),
            Token::CharLit(_) => f.write_str("char literal"),
            Token::InterpolatedString(_) => f.write_str("interpolated string"),
            Token::RawString(_) => f.write_str("raw string"),
            Token::ByteStringLit(_) => f.write_str("byte string literal"),
            Token::RegexLiteral(_) => f.write_str("regex literal"),
            Token::Duration(val) => write!(f, "duration `{val}`"),
            Token::Label(name) => write!(f, "label `{name}`"),
            Token::DocComment(_) => f.write_str("doc comment"),
            Token::InnerDocComment(_) => f.write_str("inner doc comment"),
            Token::_BlockComment | Token::_LineComment => f.write_str("comment"),
            Token::Error => f.write_str("unrecognized token"),
            // Keywords are handled by the `keyword_str()` early return above.
            _ => unreachable!(),
        }
    }
}

// ---------------------------------------------------------------------------
// Keyword table — single source of truth
// ---------------------------------------------------------------------------
//
// To add a new keyword: add ONE entry to the `define_keywords!` invocation
// below. This generates `Token::keyword_str()`, `Token::is_keyword()`, and
// `ALL_KEYWORDS` automatically.
macro_rules! define_keywords {
    ($($variant:ident => $str:literal),* $(,)?) => {
        impl Token<'_> {
            /// If this token is a keyword, return its source string; otherwise `None`.
            #[must_use]
            pub fn keyword_str(&self) -> Option<&'static str> {
                match self {
                    $(Token::$variant => Some($str),)*
                    _ => None,
                }
            }

            /// Returns `true` if this token is a language keyword.
            #[must_use]
            pub fn is_keyword(&self) -> bool {
                self.keyword_str().is_some()
            }
        }

        /// All keyword strings recognised by the lexer, in definition order.
        ///
        /// This is the single source of truth for the set of Hew keywords —
        /// downstream consumers (e.g. the LSP) should use this instead of
        /// maintaining their own keyword list.
        pub const ALL_KEYWORDS: &[&str] = &[$($str),*];
    };
}

define_keywords! {
    Let        => "let",
    Var        => "var",
    Const      => "const",
    Mut        => "mut",
    Fn         => "fn",
    If         => "if",
    Else       => "else",
    Match      => "match",
    Loop       => "loop",
    For        => "for",
    While      => "while",
    Break      => "break",
    Continue   => "continue",
    Return     => "return",
    Import     => "import",
    Pub        => "pub",
    Package    => "package",
    Super      => "super",
    Indirect   => "indirect",
    Enum       => "enum",
    Trait      => "trait",
    Impl       => "impl",
    Actor      => "actor",
    Supervisor => "supervisor",
    Child      => "child",
    Restart    => "restart",
    Budget     => "budget",
    Strategy   => "strategy",
    Permanent  => "permanent",
    Transient  => "transient",
    Temporary  => "temporary",
    OneForOne        => "one_for_one",
    OneForAll        => "one_for_all",
    RestForOne       => "rest_for_one",
    SimpleOneForOne  => "simple_one_for_one",
    Pool             => "pool",
    BrutalKill       => "brutal_kill",
    Scope      => "scope",
    Fork       => "fork",
    Spawn      => "spawn",
    Async      => "async",
    Await      => "await",
    AwaitRestart => "await_restart",
    Receive    => "receive",
    Init       => "init",
    Record     => "record",
    Type       => "type",
    This       => "this",
    Dyn        => "dyn",
    Move       => "move",
    Try        => "try",
    True       => "true",
    False      => "false",
    Reserved   => "reserved",
    Optional   => "optional",
    Deprecated => "deprecated",
    Default    => "default",
    Unsafe     => "unsafe",
    Extern     => "extern",
    Foreign    => "foreign",
    In         => "in",
    Select     => "select",
    Race       => "race",
    Join       => "join",
    From       => "from",
    After      => "after",
    Gen        => "gen",
    Yield      => "yield",
    Where      => "where",
    Cooperate  => "cooperate",
    Catch      => "catch",
    Defer      => "defer",
    As         => "as",
    Machine    => "machine",
    State      => "state",
    Event      => "event",
    On         => "on",
    When       => "when",
    Entry      => "entry",
    Exit       => "exit",
    Emit       => "emit",
    Is         => "is",
}

impl Token<'_> {
    /// Returns `true` if this token is an operator (arithmetic, comparison,
    /// logical, bitwise, assignment, arrow, or range).
    #[must_use]
    pub fn is_operator(&self) -> bool {
        matches!(
            self,
            Token::Plus
                | Token::Minus
                | Token::Star
                | Token::Slash
                | Token::Percent
                | Token::Equal
                | Token::Bang
                | Token::Less
                | Token::Greater
                | Token::EqualEqual
                | Token::NotEqual
                | Token::LessEqual
                | Token::GreaterEqual
                | Token::AmpAmp
                | Token::PipePipe
                | Token::Arrow
                | Token::FatArrow
                | Token::DotDot
                | Token::DotDotEqual
                | Token::PlusEqual
                | Token::MinusEqual
                | Token::StarEqual
                | Token::SlashEqual
                | Token::PercentEqual
                | Token::LessLess
                | Token::GreaterGreater
                | Token::LessLessEqual
                | Token::GreaterGreaterEqual
                | Token::AmpEqual
                | Token::PipeEqual
                | Token::CaretEqual
                | Token::Pipe
                | Token::Ampersand
                | Token::Caret
                | Token::Tilde
                | Token::Question
        )
    }

    /// Returns `true` if this keyword introduces a named declaration —
    /// the identifier immediately following is a declaration site.
    #[must_use]
    pub fn is_decl_keyword(&self) -> bool {
        matches!(
            self,
            Token::Let
                | Token::Var
                | Token::Const
                | Token::Fn
                | Token::Receive
                | Token::Actor
                | Token::Enum
                | Token::Trait
                | Token::Supervisor
                | Token::Type
                | Token::Machine
        )
    }

    /// Returns `true` if this keyword introduces a type declaration —
    /// the identifier immediately following is a type name.
    #[must_use]
    pub fn is_type_decl_keyword(&self) -> bool {
        matches!(
            self,
            Token::Actor
                | Token::Enum
                | Token::Trait
                | Token::Supervisor
                | Token::Type
                | Token::Machine
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper: collect just the token variants from a source string.
    fn tokens(src: &str) -> Vec<Token<'_>> {
        lex(src).into_iter().map(|(t, _)| t).collect()
    }

    #[test]
    fn keywords() {
        assert_eq!(
            tokens("fn let var return if else match"),
            vec![
                Token::Fn,
                Token::Let,
                Token::Var,
                Token::Return,
                Token::If,
                Token::Else,
                Token::Match
            ]
        );
    }

    #[test]
    fn all_keywords() {
        let src = "let var const mut fn if else match loop for while break continue return \
                   import pub package super struct enum trait impl actor \
                   supervisor child restart budget strategy permanent transient temporary \
                   one_for_one one_for_all rest_for_one simple_one_for_one pool \
                   scope fork spawn async await receive \
                   init type this dyn move try true false reserved optional deprecated \
                   default unsafe extern foreign in select race join from after gen yield \
                   where cooperate catch defer is";
        let toks = tokens(src);
        assert_eq!(toks.len(), 70);
        // Spot-check first and last
        assert_eq!(toks[0], Token::Let);
        assert_eq!(toks[3], Token::Mut);
        assert_eq!(toks[69], Token::Is);
    }

    #[test]
    fn fork_keyword() {
        assert_eq!(tokens("fork"), vec![Token::Fork]);
    }

    #[test]
    fn all_keywords_matches_keyword_str() {
        // Lex every entry in ALL_KEYWORDS and verify keyword_str() round-trips.
        for &kw in ALL_KEYWORDS {
            let toks = tokens(kw);
            assert_eq!(
                toks.len(),
                1,
                "expected exactly one token for keyword `{kw}`"
            );
            assert_eq!(
                toks[0].keyword_str(),
                Some(kw),
                "keyword_str() mismatch for `{kw}`"
            );
        }
        // Verify ALL_KEYWORDS is exhaustive: lex all keywords as a single
        // string, confirm every resulting token has a keyword_str() that
        // appears in ALL_KEYWORDS.
        let all = ALL_KEYWORDS.join(" ");
        for tok in tokens(&all) {
            let s = tok
                .keyword_str()
                .expect("token from ALL_KEYWORDS should be a keyword");
            assert!(
                ALL_KEYWORDS.contains(&s),
                "`{s}` returned by keyword_str() but missing from ALL_KEYWORDS"
            );
        }
    }

    #[test]
    fn operators_multi_char() {
        assert_eq!(
            tokens("== != => -> <= >= && || ..= .. :: #["),
            vec![
                Token::EqualEqual,
                Token::NotEqual,
                Token::FatArrow,
                Token::Arrow,
                Token::LessEqual,
                Token::GreaterEqual,
                Token::AmpAmp,
                Token::PipePipe,
                Token::DotDotEqual,
                Token::DotDot,
                Token::DoubleColon,
                Token::HashBracket,
            ]
        );
    }

    #[test]
    fn compound_assignment() {
        assert_eq!(
            tokens("+= -= *= /= %="),
            vec![
                Token::PlusEqual,
                Token::MinusEqual,
                Token::StarEqual,
                Token::SlashEqual,
                Token::PercentEqual
            ]
        );
    }

    #[test]
    fn integer_literals() {
        assert_eq!(
            tokens("0 42 1_000_000"),
            vec![
                Token::Integer("0"),
                Token::Integer("42"),
                Token::Integer("1_000_000"),
            ]
        );
    }

    #[test]
    fn hex_literals() {
        assert_eq!(
            tokens("0xFF 0x1A_2B 0XDeadBeef"),
            vec![
                Token::Integer("0xFF"),
                Token::Integer("0x1A_2B"),
                Token::Integer("0XDeadBeef"),
            ]
        );
    }

    #[test]
    fn octal_literals() {
        assert_eq!(
            tokens("0o77 0o1_0 0O755"),
            vec![
                Token::Integer("0o77"),
                Token::Integer("0o1_0"),
                Token::Integer("0O755"),
            ]
        );
    }

    #[test]
    fn binary_literals() {
        assert_eq!(
            tokens("0b1010 0b1111_0000 0B10"),
            vec![
                Token::Integer("0b1010"),
                Token::Integer("0b1111_0000"),
                Token::Integer("0B10"),
            ]
        );
    }

    #[test]
    fn float_literals() {
        assert_eq!(
            tokens("3.14 0.5 1.0e10 2.5E-3"),
            vec![
                Token::Float("3.14"),
                Token::Float("0.5"),
                Token::Float("1.0e10"),
                Token::Float("2.5E-3"),
            ]
        );
    }

    #[test]
    fn string_literals() {
        assert_eq!(
            tokens(r#""hello" "with\nnewline" "esc\\aped""#),
            vec![
                Token::StringLit("\"hello\""),
                Token::StringLit("\"with\\nnewline\""),
                Token::StringLit("\"esc\\\\aped\""),
            ]
        );
    }

    #[test]
    fn char_literals() {
        assert_eq!(
            tokens(r"'a' '\n' '\t' '\0' '\\' '\''"),
            vec![
                Token::CharLit("'a'"),
                Token::CharLit(r"'\n'"),
                Token::CharLit(r"'\t'"),
                Token::CharLit(r"'\0'"),
                Token::CharLit(r"'\\'"),
                Token::CharLit(r"'\''"),
            ]
        );
    }

    #[test]
    fn raw_and_interpolated_strings() {
        assert_eq!(
            tokens(r#"r"raw\nstring" f"hello {name}""#),
            vec![
                Token::RawString(r#"r"raw\nstring""#),
                Token::InterpolatedString("f\"hello {name}\""),
            ]
        );
    }

    #[test]
    fn interpolated_string_allows_nested_string_literals() {
        assert_eq!(
            tokens(r#"f"x={func("a")}""#),
            vec![Token::InterpolatedString(r#"f"x={func("a")}""#)]
        );
    }

    #[test]
    fn interpolated_string_still_stops_at_outer_quote() {
        assert_eq!(
            tokens(r#"f"x={func("a")}" "tail""#),
            vec![
                Token::InterpolatedString(r#"f"x={func("a")}""#),
                Token::StringLit(r#""tail""#),
            ]
        );
    }

    #[test]
    fn regex_literals_tokenize() {
        // Basic regex literal with whitespace class
        assert_eq!(
            tokens(r#"re"\s+""#),
            vec![Token::RegexLiteral(r#"re"\s+""#)]
        );
        // Dot escape — a literal dot in regex
        assert_eq!(tokens(r#"re"\.""#), vec![Token::RegexLiteral(r#"re"\.""#)]);
        // Escaped quote inside regex literal
        assert_eq!(
            tokens(r#"re"^\"quoted\"$""#),
            vec![Token::RegexLiteral(r#"re"^\"quoted\"$""#)]
        );
        // Named capture group
        assert_eq!(
            tokens(r#"re"(?P<token>.+)""#),
            vec![Token::RegexLiteral(r#"re"(?P<token>.+)""#)]
        );
        // Multiple regex literals separated by whitespace
        assert_eq!(
            tokens(r#"re"^[0-9]+$" re"https?://""#),
            vec![
                Token::RegexLiteral(r#"re"^[0-9]+$""#),
                Token::RegexLiteral(r#"re"https?://""#),
            ]
        );
    }

    #[test]
    fn duration_literals() {
        assert_eq!(
            tokens("100ms 5s 1h 200ns 50us 30m"),
            vec![
                Token::Duration("100ms"),
                Token::Duration("5s"),
                Token::Duration("1h"),
                Token::Duration("200ns"),
                Token::Duration("50us"),
                Token::Duration("30m"),
            ]
        );
    }

    #[test]
    fn identifiers_vs_keywords() {
        assert_eq!(
            tokens("foo fn bar let _priv"),
            vec![
                Token::Identifier("foo"),
                Token::Fn,
                Token::Identifier("bar"),
                Token::Let,
                Token::Identifier("_priv"),
            ]
        );
    }

    #[test]
    fn delimiters() {
        assert_eq!(
            tokens("( ) { } [ ] ; , :"),
            vec![
                Token::LeftParen,
                Token::RightParen,
                Token::LeftBrace,
                Token::RightBrace,
                Token::LeftBracket,
                Token::RightBracket,
                Token::Semicolon,
                Token::Comma,
                Token::Colon,
            ]
        );
    }

    #[test]
    fn comments_are_skipped() {
        assert_eq!(
            tokens("a // line comment\nb /* block */ c"),
            vec![
                Token::Identifier("a"),
                Token::Identifier("b"),
                Token::Identifier("c")
            ]
        );
    }

    #[test]
    fn doc_comments_emitted() {
        let toks = tokens("/// doc line\na");
        assert_eq!(
            toks,
            vec![Token::DocComment("/// doc line"), Token::Identifier("a")]
        );
    }

    #[test]
    fn inner_doc_comments_emitted() {
        let toks = tokens("//! inner doc\na");
        assert_eq!(
            toks,
            vec![
                Token::InnerDocComment("//! inner doc"),
                Token::Identifier("a")
            ]
        );
    }

    #[test]
    fn regular_comments_still_skipped() {
        let toks = tokens("a // regular\nb");
        assert_eq!(toks, vec![Token::Identifier("a"), Token::Identifier("b")]);
    }

    #[test]
    fn nested_block_comments() {
        assert_eq!(
            tokens("x /* outer /* inner */ still comment */ y"),
            vec![Token::Identifier("x"), Token::Identifier("y")]
        );
    }

    #[test]
    fn unterminated_block_comment_emits_error_token() {
        assert!(tokens("/* unterminated").contains(&Token::Error));
    }

    #[test]
    fn span_tracking() {
        let result = lex("fn main");
        assert_eq!(result.len(), 2);
        assert_eq!(result[0], (Token::Fn, Span { start: 0, end: 2 }));
        assert_eq!(
            result[1],
            (Token::Identifier("main"), Span { start: 3, end: 7 })
        );
    }

    #[test]
    fn lexer_struct_iteration() {
        let mut lexer = Lexer::new("let x = 42;");
        assert_eq!(lexer.next().map(|(t, _)| t), Some(Token::Let));
        assert_eq!(lexer.next().map(|(t, _)| t), Some(Token::Identifier("x")));
        assert_eq!(lexer.next().map(|(t, _)| t), Some(Token::Equal));
        assert_eq!(lexer.next().map(|(t, _)| t), Some(Token::Integer("42")));
        assert_eq!(lexer.next().map(|(t, _)| t), Some(Token::Semicolon));
        assert!(lexer.next().is_none());
    }

    #[test]
    fn fibonacci_example() {
        let src = std::fs::read_to_string(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../examples/fibonacci.hew"
        ))
        .expect("fibonacci.hew should exist");
        let toks = lex(&src);
        // Should produce tokens without errors
        for (tok, _) in &toks {
            assert_ne!(
                *tok,
                Token::Error,
                "unexpected error token in fibonacci.hew"
            );
        }
        // Spot-check: skip any leading inner-doc-comment header (a v0.5
        // convention for example files documenting their status), then the
        // first significant token should be `fn`.
        let first_significant = toks
            .iter()
            .map(|(t, _)| t)
            .find(|t| !matches!(t, Token::InnerDocComment(_)))
            .expect("fibonacci.hew should contain a non-comment token");
        assert_eq!(*first_significant, Token::Fn);
    }

    #[test]
    fn wrapping_ops_single_token() {
        // `&+`, `&-`, `&*` with no whitespace each lex as a single wrapping-op
        // token, NOT as `Ampersand` followed by `Plus`/`Minus`/`Star`.
        assert_eq!(tokens("&+"), vec![Token::AmpPlus]);
        assert_eq!(tokens("&-"), vec![Token::AmpMinus]);
        assert_eq!(tokens("&*"), vec![Token::AmpStar]);
    }

    #[test]
    fn wrapping_ops_spaced_are_two_tokens() {
        // `& +` (with whitespace) is `Ampersand` + `Plus` — NOT a wrapping op.
        // This is the disambiguation rule: spacing determines operator identity.
        assert_eq!(tokens("& +"), vec![Token::Ampersand, Token::Plus]);
        assert_eq!(tokens("& -"), vec![Token::Ampersand, Token::Minus]);
        assert_eq!(tokens("& *"), vec![Token::Ampersand, Token::Star]);
    }

    #[test]
    fn wrapping_ops_in_expression_context() {
        // Verify that `a &+ b` lexes correctly in a typical use position.
        assert_eq!(
            tokens("a &+ b"),
            vec![
                Token::Identifier("a"),
                Token::AmpPlus,
                Token::Identifier("b"),
            ]
        );
        assert_eq!(
            tokens("x &- 1"),
            vec![Token::Identifier("x"), Token::AmpMinus, Token::Integer("1"),]
        );
        assert_eq!(
            tokens("m &* n"),
            vec![
                Token::Identifier("m"),
                Token::AmpStar,
                Token::Identifier("n"),
            ]
        );
    }

    #[test]
    fn unsafe_keyword_recognised() {
        // `unsafe` must lex to Token::Unsafe, not an identifier.
        assert_eq!(tokens("unsafe"), vec![Token::Unsafe]);
        // Verify keyword_str round-trip.
        assert_eq!(Token::Unsafe.keyword_str(), Some("unsafe"));
        // Identifiers that start with "unsafe" must not be mistaken for the keyword.
        assert_eq!(tokens("unsafe_ptr"), vec![Token::Identifier("unsafe_ptr")]);
    }

    #[test]
    fn is_decl_keyword_covers_named_declarations() {
        // Every keyword that introduces a named binding must return true.
        assert!(Token::Let.is_decl_keyword());
        assert!(Token::Var.is_decl_keyword());
        assert!(Token::Const.is_decl_keyword());
        assert!(Token::Fn.is_decl_keyword());
        assert!(Token::Actor.is_decl_keyword());
        assert!(Token::Enum.is_decl_keyword());
        assert!(Token::Trait.is_decl_keyword());
        assert!(Token::Supervisor.is_decl_keyword());
        assert!(Token::Type.is_decl_keyword());
        // `machine` introduces a named state-machine type — must be included.
        assert!(Token::Machine.is_decl_keyword());
        // A non-decl keyword must return false.
        assert!(!Token::If.is_decl_keyword());
        assert!(!Token::Return.is_decl_keyword());
    }

    #[test]
    fn is_type_decl_keyword_covers_type_declarations() {
        // Every keyword that introduces a named type must return true.
        assert!(Token::Actor.is_type_decl_keyword());
        assert!(Token::Enum.is_type_decl_keyword());
        assert!(Token::Trait.is_type_decl_keyword());
        assert!(Token::Supervisor.is_type_decl_keyword());
        assert!(Token::Type.is_type_decl_keyword());
        // `machine` declares a named type — must be included.
        assert!(Token::Machine.is_type_decl_keyword());
        // Non-type-decl keywords must return false.
        assert!(!Token::Let.is_type_decl_keyword());
        assert!(!Token::Fn.is_type_decl_keyword());
        assert!(!Token::If.is_type_decl_keyword());
    }

    #[test]
    fn syntax_data_json_covers_all_keywords() {
        let json_str = include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../docs/syntax-data.json"
        ));
        let data: serde_json::Value =
            serde_json::from_str(json_str).expect("syntax-data.json should be valid JSON");

        // Check all lexer keywords appear in syntax-data in order.
        let json_all: Vec<&str> = data["all_keywords"]
            .as_array()
            .expect("all_keywords should be an array")
            .iter()
            .map(|v| v.as_str().expect("keyword should be a string"))
            .collect();
        let json_lexer_keywords: Vec<&str> = json_all
            .iter()
            .copied()
            .filter(|kw| ALL_KEYWORDS.contains(kw))
            .collect();
        assert_eq!(
            json_lexer_keywords, ALL_KEYWORDS,
            "all lexer keywords must appear in syntax-data.json in lexer order"
        );

        // Verify the union of categorized keywords equals all_keywords.
        let categories = data["keywords"]
            .as_object()
            .expect("keywords should be an object");
        let mut categorized: Vec<&str> = Vec::new();
        for (_cat, arr) in categories {
            for v in arr.as_array().expect("each category should be an array") {
                categorized.push(v.as_str().expect("keyword should be a string"));
            }
        }
        categorized.sort_unstable();
        let mut all_sorted: Vec<&str> = json_all;
        all_sorted.sort_unstable();
        assert_eq!(
            categorized, all_sorted,
            "union of keyword categories must equal all_keywords"
        );
    }

    #[test]
    fn struct_lexes_as_identifier() {
        assert_eq!(tokens("struct"), vec![Token::Identifier("struct")]);
    }
}
