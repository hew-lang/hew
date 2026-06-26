//! Operator binding-power tables and token→operator mapping for the Pratt
//! expression parser.

#[allow(
    clippy::wildcard_imports,
    reason = "grammar-area submodules share the parent parser namespace via the split"
)]
use super::*;

// ── Precedence Functions ──

/// Get binding power for infix operators (left, right).
/// Higher numbers = tighter binding.
pub(crate) fn infix_bp(op: &Token) -> Option<(u8, u8)> {
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

pub(crate) fn prefix_bp(op: &Token) -> Option<u8> {
    match op {
        // `*expr` is a raw-pointer dereference.  v0.5 parses it only so
        // the type checker can reject it deterministically — no codegen
        // path is reached.  Same binding power as the other unary prefixes.
        Token::Bang
        | Token::Minus
        | Token::Tilde
        | Token::Await
        | Token::AwaitRestart
        | Token::Star => Some(25),
        _ => None,
    }
}

/// Right binding power of the contextual `clone <operand>` prefix.
///
/// Matches the other unary prefixes (`!`, `-`, `~`) so `clone a + b` parses as
/// `(clone a) + b` and a postfix chain binds into the operand: `clone x.f()`
/// clones the result of `x.f()`.
pub(crate) const CLONE_PREFIX_BP: u8 = 25;

/// Whether `tok` begins an operand for the contextual `clone` prefix.
///
/// Restricted to tokens that unambiguously start a fresh primary expression:
/// any identifier or literal. Deliberately excludes `(`, `[`, `.`, `?`, and
/// the infix/unary operator symbols so that `clone(args)` stays a call,
/// `clone.field` / `clone[i]` stay identifier postfixes, and `clone - x` stays
/// subtraction — `clone` remains a usable identifier in every position where
/// it is followed by a continuation rather than a new operand.
pub(crate) fn token_begins_clone_operand(tok: &Token) -> bool {
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

pub(crate) fn token_to_binop(token: &Token) -> Option<BinaryOp> {
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
