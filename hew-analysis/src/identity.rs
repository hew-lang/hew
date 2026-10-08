//! Editor queries over the checker's published source resolutions.
//!
//! A [`hew_types::check::SpanKey`] includes the source module index.
//! Equal byte offsets in different files therefore never name the same
//! occurrence, and declaration identity comes from the checker rather than
//! from a rendered name.

use hew_parser::ast::{Item, RecordKind, TraitItem, TypeBodyItem};
use hew_parser::ParseResult;
use hew_types::check::scope::Resolution;
use hew_types::check::SpanKey;
use hew_types::TypeCheckOutput;
use hew_types::{DeclarationKind, DeclarationOccurrence};
use std::path::PathBuf;

use crate::OffsetSpan;

/// The checker resolution whose source segment contains `offset`.
///
/// Re-key the checker facts so the open document's own file is index 0.
///
/// Every analysis query reads the open buffer at index 0. A document that is
/// one file of a directory module is checked under its span index from
/// `ModuleGraph::file_span_indices`, so its facts move to 0 here and whatever
/// held 0 takes its index. Callers pass `file_span_indices().path_index(doc)`.
pub fn focus_file(output: &mut TypeCheckOutput, file: u32) {
    let swap = |key: &mut SpanKey| {
        if key.module_idx == file {
            key.module_idx = 0;
        } else if key.module_idx == 0 {
            key.module_idx = file;
        }
    };
    output.expr_types = std::mem::take(&mut output.expr_types)
        .into_iter()
        .map(|(mut key, ty)| {
            swap(&mut key);
            (key, ty)
        })
        .collect();
    output.resolutions = std::mem::take(&mut output.resolutions)
        .into_iter()
        .map(|(mut key, resolution)| {
            swap(&mut key);
            (key, resolution)
        })
        .collect();
    output.shorthand_field_labels = std::mem::take(&mut output.shorthand_field_labels)
        .into_iter()
        .map(|(mut key, field)| {
            swap(&mut key);
            (key, field)
        })
        .collect();
}

/// An offset on a boundary belongs to the segment on its left, as it does in
/// the parser's source spans. If malformed or synthesized spans overlap, the
/// narrowest segment wins so an inner member is preferred over its container.
#[must_use]
pub fn resolution_at(
    output: &TypeCheckOutput,
    module_idx: u32,
    offset: usize,
) -> Option<(OffsetSpan, Resolution)> {
    output
        .resolutions
        .iter()
        .filter(|(key, _)| {
            key.module_idx == module_idx
                && key.start < key.end
                && key.start <= offset
                && offset <= key.end
        })
        .min_by_key(|(key, _)| (key.end - key.start, key.start, key.end))
        .map(|(key, resolution)| (span_of(key), *resolution))
}

/// Every resolved source segment in one file that names the same declaration.
///
/// This includes the queried segment if it is present in the checker table.
/// Callers can add the declaration site when requested; they must not collect
/// more occurrences by comparing source spellings.
#[must_use]
pub fn reference_spans(
    output: &TypeCheckOutput,
    module_idx: u32,
    resolution: Resolution,
) -> Vec<OffsetSpan> {
    let mut spans: Vec<_> = output
        .resolutions
        .iter()
        .filter(|(key, value)| {
            key.module_idx == module_idx && key.start < key.end && **value == resolution
        })
        .map(|(key, _)| span_of(key))
        .collect();
    spans.sort_by_key(|span| (span.start, span.end));
    spans.dedup();
    spans
}

/// Resolved segments in every source file checked in this compilation.
#[must_use]
pub fn all_reference_spans(
    output: &TypeCheckOutput,
    resolution: Resolution,
) -> Vec<(u32, OffsetSpan)> {
    let mut spans: Vec<_> = output
        .resolutions
        .iter()
        .filter(|(key, value)| key.start < key.end && **value == resolution)
        .map(|(key, _)| (key.module_idx, span_of(key)))
        .collect();
    spans.sort_by_key(|(module, span)| (*module, span.start, span.end));
    spans.dedup();
    spans
}

/// Shorthand initializer labels (`Point { x }`) naming the field `resolution`,
/// in every source file checked in this compilation.
///
/// Each span is the shorthand token, which is also a use of the value binding
/// of the same spelling. A rename of the field keeps that binding by writing
/// the label out (`Point { y: x }`) instead of replacing the token.
#[must_use]
pub fn all_shorthand_label_spans(
    output: &TypeCheckOutput,
    resolution: Resolution,
) -> Vec<(u32, OffsetSpan)> {
    let Resolution::Field(owner, index) = resolution else {
        return Vec::new();
    };
    let mut spans: Vec<_> = output
        .shorthand_field_labels
        .iter()
        .filter(|(key, field)| key.start < key.end && **field == (owner, index))
        .map(|(key, _)| (key.module_idx, span_of(key)))
        .collect();
    spans.sort_by_key(|(module, span)| (*module, span.start, span.end));
    spans.dedup();
    spans
}

/// True when `span` in `module_idx` is a shorthand initializer token: a use
/// of a binding that also labels the field of the same spelling.
#[must_use]
pub fn is_shorthand_label(output: &TypeCheckOutput, module_idx: u32, span: OffsetSpan) -> bool {
    output.shorthand_field_labels.contains_key(&SpanKey {
        start: span.start,
        end: span.end,
        module_idx,
    })
}

/// Map a field declaration token to the same nominal/index identity used by
/// checked projections and initializer labels. Field declarations are not
/// expressions, so the checker need not duplicate these source tokens in its
/// resolution table.
#[must_use]
pub fn field_declaration_at(
    output: &TypeCheckOutput,
    source: &str,
    parsed: &ParseResult,
    offset: usize,
) -> Option<(OffsetSpan, Resolution)> {
    for (owner, definition) in &output.type_defs {
        for index in 0..definition.field_order.len() {
            let resolution = Resolution::Field(*owner, u32::try_from(index).ok()?);
            let Some(target) = declaration_target(output, resolution) else {
                continue;
            };
            if target.source.is_some() && target.occurrence.module() != output.defs.root_module() {
                continue;
            }
            let Some(span) = declaration_name_span(source, parsed, &target) else {
                continue;
            };
            if span.start <= offset && offset < span.end {
                return Some((span, resolution));
            }
        }
    }
    None
}

/// A checker-owned declaration, with its physical source when available.
/// The caller supplies that source's current parsed text, so unsaved editor
/// buffers take precedence over bytes on disk.
#[derive(Debug, Clone)]
pub struct DeclarationTarget {
    pub source: Option<PathBuf>,
    pub occurrence: DeclarationOccurrence,
    pub name: String,
    pub field_index: Option<u32>,
}

#[must_use]
pub fn declaration_target(
    output: &TypeCheckOutput,
    resolution: Resolution,
) -> Option<DeclarationTarget> {
    let (id, field_index) = match resolution {
        Resolution::Def(id) | Resolution::Member(id) => (id, None),
        Resolution::Nominal(id) => (id.declaration(), None),
        Resolution::Field(id, index) => (id.declaration(), Some(index)),
        Resolution::Param(_)
        | Resolution::Local(_)
        | Resolution::Module(_)
        | Resolution::Builtin(_)
        | Resolution::Variant(_, _) => return None,
    };
    let occurrence = output.defs.site(id)?;
    let source = occurrence
        .module()
        .and_then(|module| output.defs.module_source(module))
        .map(PathBuf::from);
    let name = if let Some(index) = field_index {
        output
            .type_defs
            .get(&hew_types::NominalId::of_declaration(id))?
            .field_order
            .get(index as usize)?
            .clone()
    } else {
        output.defs.name(id).to_string()
    };
    Some(DeclarationTarget {
        source,
        occurrence,
        name,
        field_index,
    })
}

/// Locate the exact token within the declaration selected by the checker.
#[must_use]
pub fn declaration_name_span(
    source: &str,
    parsed: &ParseResult,
    target: &DeclarationTarget,
) -> Option<OffsetSpan> {
    let item_span = target.occurrence.span();
    let (item, enclosing_span) = parsed
        .program
        .items
        .iter()
        .find(|(_, span)| span.start <= item_span.start && item_span.end <= span.end)?;
    let ordinal = target.occurrence.ordinal() as usize;
    let kind = target.occurrence.kind();
    if let Some(index) = target.field_index {
        let field_name = target.name.as_str();
        let range = match item {
            Item::TypeDecl(decl) => {
                let (_, ty) = decl
                    .body
                    .iter()
                    .filter_map(|part| match part {
                        TypeBodyItem::Field { name, ty, .. } => Some((name, ty)),
                        _ => None,
                    })
                    .nth(index as usize)?;
                enclosing_span.start..ty.1.start
            }
            Item::Record(decl) => {
                let RecordKind::Named(fields) = &decl.kind else {
                    return None;
                };
                let field = fields.get(index as usize)?;
                field.span.start..field.ty.1.start
            }
            Item::Actor(decl) => {
                let field = decl.fields.get(index as usize)?;
                field.span.start..field.ty.1.start
            }
            _ => return None,
        };
        return find_last_identifier(source, range, field_name);
    }
    let range = match (kind, item) {
        (DeclarationKind::Function, Item::Function(decl)) => decl.decl_span.clone(),
        (DeclarationKind::ImplMethod, Item::Impl(decl)) => decl
            .methods
            .iter()
            .find(|method| method.fn_span == item_span)
            .or_else(|| decl.methods.get(ordinal))?
            .decl_span
            .clone(),
        (DeclarationKind::TypeMethod, Item::TypeDecl(decl)) => {
            let methods: Vec<_> = decl
                .body
                .iter()
                .filter_map(|part| match part {
                    TypeBodyItem::Method(method) => Some(method),
                    _ => None,
                })
                .collect();
            methods
                .iter()
                .find(|method| method.fn_span == item_span)
                .copied()
                .or_else(|| methods.get(ordinal).copied())?
                .decl_span
                .clone()
        }
        (DeclarationKind::ActorMethod, Item::Actor(decl)) => decl
            .methods
            .iter()
            .find(|method| method.fn_span == item_span)
            .or_else(|| decl.methods.get(ordinal))?
            .decl_span
            .clone(),
        (DeclarationKind::ActorReceive, Item::Actor(decl)) => {
            decl.receive_fns.get(ordinal)?.span.clone()
        }
        (DeclarationKind::TraitMethod, Item::Trait(decl)) => decl
            .items
            .iter()
            .filter_map(|part| match part {
                TraitItem::Method(method) => Some(method),
                TraitItem::AssociatedType { .. } => None,
            })
            .nth(ordinal)?
            .span
            .clone(),
        (DeclarationKind::Variant, Item::TypeDecl(decl)) => decl
            .body
            .iter()
            .filter_map(|part| match part {
                TypeBodyItem::Variant(variant) => Some(variant),
                _ => None,
            })
            .nth(ordinal)?
            .span
            .clone(),
        _ => item_span,
    };
    find_identifier(source, range, &target.name)
}

fn find_identifier(source: &str, range: std::ops::Range<usize>, name: &str) -> Option<OffsetSpan> {
    let text = source.get(range.clone())?;
    for (offset, _) in text.match_indices(name) {
        let start = range.start + offset;
        let end = start + name.len();
        let identifier = |ch: char| ch.is_alphanumeric() || ch == '_';
        if !source[..start].chars().next_back().is_some_and(identifier)
            && !source[end..].chars().next().is_some_and(identifier)
        {
            return Some(OffsetSpan { start, end });
        }
    }
    None
}

fn find_last_identifier(
    source: &str,
    range: std::ops::Range<usize>,
    name: &str,
) -> Option<OffsetSpan> {
    let text = source.get(range.clone())?;
    text.match_indices(name)
        .filter_map(|(offset, _)| {
            let start = range.start + offset;
            find_identifier(source, start..start + name.len(), name)
        })
        .last()
}

fn span_of(key: &SpanKey) -> OffsetSpan {
    OffsetSpan {
        start: key.start,
        end: key.end,
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use hew_types::check::scope::Resolution;
    use hew_types::check::SpanKey;
    use hew_types::BuiltinType;
    use hew_types::TypeCheckOutput;

    use super::{all_reference_spans, reference_spans, resolution_at};

    #[test]
    fn source_resolution_keeps_module_indices_distinct() {
        let left = Resolution::Builtin(BuiltinType::Option);
        let right = Resolution::Builtin(BuiltinType::Result);
        let output = TypeCheckOutput {
            resolutions: HashMap::from([
                (
                    SpanKey {
                        start: 4,
                        end: 8,
                        module_idx: 0,
                    },
                    left,
                ),
                (
                    SpanKey {
                        start: 4,
                        end: 8,
                        module_idx: 1,
                    },
                    right,
                ),
                (
                    SpanKey {
                        start: 14,
                        end: 18,
                        module_idx: 0,
                    },
                    left,
                ),
            ]),
            ..TypeCheckOutput::default()
        };

        assert_eq!(resolution_at(&output, 0, 5).map(|(_, id)| id), Some(left));
        assert_eq!(resolution_at(&output, 1, 5).map(|(_, id)| id), Some(right));
        assert_eq!(reference_spans(&output, 0, left).len(), 2);
        assert_eq!(reference_spans(&output, 1, left).len(), 0);
        assert_eq!(all_reference_spans(&output, left).len(), 2);
        assert_eq!(all_reference_spans(&output, right)[0].0, 1);
    }
}
