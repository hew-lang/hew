//! Editor queries over the checker's published source resolutions.
//!
//! A [`hew_types::check::SpanKey`] includes the source module index.
//! Equal byte offsets in different files therefore never name the same
//! occurrence, and declaration identity comes from the checker rather than
//! from a rendered name.

use hew_types::check::scope::Resolution;
use hew_types::check::SpanKey;
use hew_types::TypeCheckOutput;

use crate::OffsetSpan;

/// The checker resolution whose source segment contains `offset`.
///
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

    use super::{reference_spans, resolution_at};

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
    }
}
