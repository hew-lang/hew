//! The comment-side Trojan-Source lints.
//!
//! Flags invisible Unicode codepoints in comments while leaving readable
//! non-ASCII prose alone. Bidirectional reordering controls are a deny-by-default
//! tier because they can make reviewed comment/source text appear in an order
//! different from the bytes the compiler consumes; other invisible/default-
//! ignorable scalars are warning-tier spoofing hazards.
//!
//! ## Precision over recall
//!
//! - Only scans comments extracted by the parser formatter's source scanner, so
//!   `//` inside string, char, and regex literals is never treated as a comment.
//! - The deny tier is exactly Unicode's nine explicit directional formatting
//!   controls: U+202A–U+202E and U+2066–U+2069.
//! - Readable non-ASCII (`é`, CJK, emoji, arrows, em dashes) is preserved by the
//!   same classifier the formatter uses for literal emission.

use hew_parser::ast::Span;

use crate::error::TypeError;

use super::{LintCtx, LintId, LintLevels};

/// Entry point: scan all comments in `source` and flag invisible codepoints.
pub(super) fn check(ctx: &LintCtx, levels: &LintLevels, source: &str, out: &mut Vec<TypeError>) {
    for comment in hew_parser::fmt::extract_comments(source, true) {
        for (offset, c) in comment.text.char_indices() {
            if c.is_ascii() {
                continue;
            }

            let span = comment.span.start + offset..comment.span.start + offset + c.len_utf8();
            if is_bidi_reorder_control(c) {
                emit(ctx, levels, &Kind::TEXT_DIRECTION, &span, c, out);
            } else if !hew_parser::fmt::is_printable_non_ascii(c) {
                emit(ctx, levels, &Kind::INVISIBLE, &span, c, out);
            }
        }
    }
}

/// The two comment lint tiers this pass emits, bundled because `id`,
/// `class`, and `suggestion` always vary together per tier — see
/// [`Kind::TEXT_DIRECTION`] and [`Kind::INVISIBLE`].
struct Kind {
    id: LintId,
    /// Noun phrase naming the codepoint class in the diagnostic message.
    class: &'static str,
    suggestion: &'static str,
}

impl Kind {
    /// Deny-by-default: bidirectional reordering controls.
    const TEXT_DIRECTION: Self = Self {
        id: LintId::TextDirectionCodepointInComment,
        class: "text-direction control",
        suggestion: "remove the codepoint or describe the directionality in visible text",
    };
    /// Warning-tier: other invisible/default-ignorable scalars.
    const INVISIBLE: Self = Self {
        id: LintId::InvisibleCodepointInComment,
        class: "invisible Unicode codepoint",
        suggestion: "remove the codepoint or spell it out visibly in the comment",
    };
}

fn emit(
    ctx: &LintCtx,
    levels: &LintLevels,
    kind: &Kind,
    span: &Span,
    c: char,
    out: &mut Vec<TypeError>,
) {
    ctx.emit(
        levels,
        kind.id,
        span,
        format!(
            "{}: comment contains {} U+{:04X}",
            kind.id.as_str(),
            kind.class,
            c as u32
        ),
        kind.suggestion.to_string(),
        out,
    );
}

fn is_bidi_reorder_control(c: char) -> bool {
    matches!(
        c as u32,
        0x202A..=0x202E | 0x2066..=0x2069
    )
}
