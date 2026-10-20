//! Editor navigation over the checker's declaration and binding identities.

use hew_parser::ParseResult;
use hew_types::check::scope::Resolution;
use hew_types::{DeclarationKind, TypeCheckOutput};

use crate::identity::{self, DeclarationTarget};
use crate::OffsetSpan;

/// Resolve an occurrence or an authored declaration in the current source.
#[must_use]
pub fn resolution_at(
    source: &str,
    parsed: &ParseResult,
    output: &TypeCheckOutput,
    offset: usize,
) -> Option<Resolution> {
    identity::resolution_at(output, 0, offset)
        .filter(|(span, resolution)| {
            !matches!(resolution, Resolution::Local(_))
                || identity::is_identifier_span(source, *span)
        })
        .or_else(|| identity::declaration_at(output, source, parsed, offset))
        .map(|(_, resolution)| resolution)
}

/// The nominal declaration of a value's checked type, or a named type itself.
#[must_use]
pub fn type_definition(
    source: &str,
    parsed: &ParseResult,
    output: &TypeCheckOutput,
    offset: usize,
) -> Option<DeclarationTarget> {
    let resolution = resolution_at(source, parsed, output, offset)?;
    if let Resolution::Nominal(_) = resolution {
        return identity::declaration_target(output, resolution);
    }
    let head = if let Resolution::Local(binding) = resolution {
        output.resolved_binding_types.get(&binding)?.head()?
    } else {
        output
            .expr_types
            .iter()
            .filter(|(span, _)| span.module_idx == 0 && span.start <= offset && offset <= span.end)
            .min_by_key(|(span, _)| (span.end - span.start, span.start))
            .and_then(|(_, ty)| ty.head())?
    };
    let nominal = head.declaration(&output.defs)?;
    identity::declaration_target(output, Resolution::Nominal(nominal))
}

/// Authored implementation methods selected by exact trait or receiver identity.
#[must_use]
pub fn implementations(
    source: &str,
    parsed: &ParseResult,
    output: &TypeCheckOutput,
    offset: usize,
) -> Vec<DeclarationTarget> {
    let Some(selected) = resolution_at(source, parsed, output, offset) else {
        return Vec::new();
    };
    let mut declarations = Vec::new();
    for signature in output.fn_sigs.values() {
        let Some(method) = &signature.impl_method else {
            continue;
        };
        let selected = match selected {
            Resolution::Def(id) if output.defs.kind(id) == DeclarationKind::Trait => {
                method.trait_method.is_some_and(|(owner, _)| owner == id)
            }
            Resolution::Member(id) | Resolution::Def(id) => {
                method.trait_method.is_some_and(|(_, target)| target == id)
                    || method.declaration == id
            }
            Resolution::Nominal(owner) => method.receiver == Some(owner.declaration()),
            _ => false,
        };
        if selected {
            declarations.push(method.declaration);
        }
    }
    declarations.sort();
    declarations.dedup();
    declarations
        .into_iter()
        .filter_map(|id| identity::declaration_target(output, Resolution::Member(id)))
        .collect()
}

/// Same-document occurrences of one checked identity, including its declaration.
#[must_use]
pub fn document_highlights(
    source: &str,
    parsed: &ParseResult,
    output: &TypeCheckOutput,
    offset: usize,
) -> Vec<OffsetSpan> {
    let Some(resolution) = resolution_at(source, parsed, output, offset) else {
        return Vec::new();
    };
    let mut spans = identity::reference_spans(output, 0, resolution);
    spans.extend(
        identity::all_shorthand_label_spans(output, resolution)
            .into_iter()
            .filter_map(|(module, span)| (module == 0).then_some(span)),
    );
    if let Some(target) = identity::declaration_target(output, resolution) {
        if target.occurrence.module() == output.defs.root_module() {
            if let Some(span) = identity::declaration_name_span(source, parsed, &target) {
                spans.push(span);
            }
        }
    }
    spans.retain(|span| identity::is_identifier_span(source, *span));
    spans.sort_by_key(|span| (span.start, span.end));
    spans.dedup();
    spans
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nominal_type_queries_distinguish_generic_values_fields_and_primitives() {
        let source = "type Box<T> { value: T; }\nfn accept(unused_param: Box<i64>) {}\nfn main() { let boxed = Box { value: 42 }; let unused = Box { value: 3 }; let (left, right) = (Box { value: 1 }, 2); println(boxed.value); }\n";
        let parsed = hew_parser::parse(source);
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let output = identity::source_identities(&parsed);
        assert!(output.errors.is_empty(), "{:?}", output.errors);
        for needle in [
            "Box<T>",
            "boxed =",
            "boxed.value",
            "unused =",
            "left,",
            "unused_param:",
        ] {
            let target = type_definition(source, &parsed, &output, source.find(needle).unwrap())
                .expect("nominal definition");
            assert_eq!(target.name, "Box");
            let span = identity::declaration_name_span(source, &parsed, &target).unwrap();
            assert_eq!(&source[span.start..span.end], "Box");
        }
        assert!(type_definition(source, &parsed, &output, source.find("42").unwrap()).is_none());
        assert!(
            implementations(source, &parsed, &output, source.find("boxed").unwrap()).is_empty()
        );
        assert!(document_highlights(source, &parsed, &output, source.len() + 1).is_empty());
    }
}
