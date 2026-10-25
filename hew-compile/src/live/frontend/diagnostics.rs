//! Project compiler diagnostics back onto original live inputs.

use std::collections::BTreeMap;
use std::ops::Range;
use std::path::{Path, PathBuf};

use super::{Error, SourceView, State};
use crate::{FrontendDiagnostic, FrontendDiagnosticKind, FrontendFailure};

fn span(view: &SourceView, location: &Range<usize>) -> Option<Range<usize>> {
    if location.start < view.input_span.start || location.start > view.input_span.end {
        return None;
    }
    let start = location.start - view.input_span.start;
    let end = location
        .end
        .saturating_sub(view.input_span.start)
        .min(view.source.len());
    Some(start..end.max(start))
}

fn fixes(view: &SourceView, fixes: &mut Vec<hew_parser::diagnostic::DiagnosticFix>) {
    fixes.retain_mut(|fix| {
        let Some(edits) = fix
            .edits
            .iter()
            .map(|edit| {
                (edit.span.end <= view.input_span.end)
                    .then(|| span(view, &(edit.span.start..edit.span.end)))
                    .flatten()
            })
            .collect::<Option<Vec<_>>>()
        else {
            return false;
        };
        for (edit, location) in fix.edits.iter_mut().zip(edits) {
            edit.span = location.into();
        }
        true
    });
}

fn diagnostic(
    diagnostic: &mut FrontendDiagnostic,
    views: &BTreeMap<PathBuf, SourceView>,
) -> Option<()> {
    let filename = diagnostic.filename.as_deref()?;
    let view = views.get(Path::new(filename))?;
    match &mut diagnostic.kind {
        FrontendDiagnosticKind::Parse(error) => {
            error.span = span(view, &error.span)?;
            fixes(view, &mut error.fixes);
        }
        FrontendDiagnosticKind::Type(error) => {
            error.span = span(view, &error.span)?;
            for (index, (location, _, _)) in error.notes.iter_mut().enumerate() {
                let note = diagnostic
                    .note_sources
                    .get_mut(index)
                    .and_then(Option::as_mut);
                if let Some((source, filename)) = note {
                    if let Some(view) = views.get(Path::new(filename)) {
                        *location = span(view, location)?;
                        source.clone_from(&view.source);
                    }
                } else {
                    *location = span(view, location)?;
                }
            }
            fixes(view, &mut error.fixes);
        }
        FrontendDiagnosticKind::Hir(error) => {
            error.span = span(view, &error.span)?;
            for (location, _) in &mut error.secondary_spans {
                *location = span(view, location)?;
            }
        }
        FrontendDiagnosticKind::Message(message) => {
            if let Some(location) = &mut message.span {
                *location = span(view, location)?;
                message.source = Some(view.source.clone().into());
            }
            for note in &mut message.notes {
                if let Some(view) = views.get(Path::new(&note.filename)) {
                    note.span = span(view, &note.span)?;
                    note.source = view.source.clone().into();
                }
            }
        }
    }
    diagnostic.source = Some(view.source.clone());
    Some(())
}

pub(super) fn remap(
    mut failure: FrontendFailure,
    views: &BTreeMap<PathBuf, SourceView>,
) -> FrontendFailure {
    failure.diagnostics = remap_all(failure.diagnostics, views);
    failure
}

pub(super) fn remap_all(
    mut diagnostics: Vec<FrontendDiagnostic>,
    views: &BTreeMap<PathBuf, SourceView>,
) -> Vec<FrontendDiagnostic> {
    for original in &mut diagnostics {
        let mut mapped = original.clone();
        // Generated scaffolding keeps its complete diagnostic context. Never
        // pair original source with partially translated locations.
        if diagnostic(&mut mapped, views).is_some() {
            *original = mapped;
        }
    }
    diagnostics
}

pub(super) fn lowering(
    error: crate::SessionError,
    program: &hew_parser::ast::Program,
    source: &str,
    path: &Path,
    state: &State,
) -> Error {
    let message = error.to_string();
    let diagnostics = match error {
        crate::SessionError::Hir(diagnostics) => crate::hir_diagnostics_to_frontend(
            program,
            source,
            &path.to_string_lossy(),
            diagnostics,
            &state.options.documents,
        ),
        crate::SessionError::Ownership(diagnostics) => crate::ownership_diagnostics_to_frontend(
            program,
            source,
            &path.to_string_lossy(),
            diagnostics,
            &state.options.documents,
        ),
        error => return Error::Lowering(error),
    };
    Error::Frontend(remap(
        FrontendFailure {
            message,
            diagnostics,
        },
        &state.views,
    ))
}
