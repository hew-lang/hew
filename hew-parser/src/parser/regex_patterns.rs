//! Source names embedded in a regex match pattern.

use crate::ast::{Ident, RegexCapture};
use regex_syntax::ast::{Ast, GroupKind, RepetitionKind, RepetitionRange};

pub(super) fn named_captures(
    pattern: &str,
    token_start: usize,
    ident: impl Fn(&str) -> Ident,
) -> Vec<RegexCapture> {
    // The checker reports the regex engine's full diagnostic, including
    // compilation limits. A malformed pattern introduces no lexical names.
    let Ok(ast) = regex_syntax::ast::parse::Parser::new().parse(pattern) else {
        return Vec::new();
    };
    let mut groups = Vec::new();
    named_groups(&ast, true, &mut groups);
    // Each delimiter quote in the normalized pattern was escaped in the token.
    // Groups arrive in source order, so consume each prefix exactly once.
    let mut previous = 0;
    let mut escapes = 0;
    groups
        .into_iter()
        .map(|(group, required)| {
            let offset = group.span.start.offset;
            escapes += pattern[previous..offset]
                .bytes()
                .filter(|byte| *byte == b'"')
                .count();
            previous = offset;
            let start = token_start + 3 + offset + escapes;
            RegexCapture {
                name: ident(&group.name),
                index: group.index,
                required,
                span: start..start + group.name.len(),
            }
        })
        .collect()
}

/// A name occurs at one capture site. An enclosing choice or repetition
/// can skip that site. Repetition *inside* the group can produce a present,
/// empty capture.
fn named_groups<'a>(
    ast: &'a Ast,
    required: bool,
    out: &mut Vec<(&'a regex_syntax::ast::CaptureName, bool)>,
) {
    match ast {
        Ast::Group(group) => {
            if let GroupKind::CaptureName { name, .. } = &group.kind {
                out.push((name, required));
            }
            named_groups(&group.ast, required, out);
        }
        Ast::Repetition(repetition) => {
            let may_skip = matches!(
                repetition.op.kind,
                RepetitionKind::ZeroOrOne
                    | RepetitionKind::ZeroOrMore
                    | RepetitionKind::Range(
                        RepetitionRange::Exactly(0)
                            | RepetitionRange::AtLeast(0)
                            | RepetitionRange::Bounded(0, _)
                    )
            );
            named_groups(&repetition.ast, required && !may_skip, out);
        }
        Ast::Alternation(choice) => {
            for arm in &choice.asts {
                named_groups(arm, false, out);
            }
        }
        Ast::Concat(sequence) => {
            for part in &sequence.asts {
                named_groups(part, required, out);
            }
        }
        _ => {}
    }
}
