//! The checker-driven half of `hew fmt --migrate`: bare variants.
//!
//! Whether a bare `Some(x)` becomes `.Some(x)` or `Option.Some(x)` depends on
//! the type its context expects, so this pass asks the checker. Every check
//! unit is checked against the other files' migrated text, and the
//! `E_BARE_VARIANT_EXPR` fix-it the checker publishes at each site is applied.
//! Other diagnostics never block the pass: an ill-typed program still
//! migrates. A respelling is kept only when the re-check reports no
//! diagnostic its file did not have before.

use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Mutex;

use hew_compile::{DocumentSet, FrontendDiagnosticKind, FrontendOptions};
use hew_types::error::TypeErrorKind;

/// Respelling one site can expose another inside it (`Some(Ok(x))` checks
/// the payload once the outer variant has a type), so a file is re-checked
/// until no site remains, at most this many times.
const PASSES: usize = 4;

/// A file the pass refused to respell, at the first new diagnostic.
pub(crate) struct VariantRefusal {
    pub file: PathBuf,
    pub line: usize,
    pub column: usize,
    pub reason: String,
}

/// Respell the bare variants of every file in `files` (path, migrated text),
/// in place. Returns the refusals.
///
/// Respelling never changes what a file declares, so each check unit — a
/// directory module, or a file of its own — is checked against the other
/// units' syntax-migrated text, and units are respelled in parallel.
pub(crate) fn respell_bare_variants(files: &mut [(PathBuf, String)]) -> Vec<VariantRefusal> {
    let mut documents = DocumentSet::new();
    let mut units: Vec<(PathBuf, Vec<usize>)> = Vec::new();
    for (index, (path, text)) in files.iter().enumerate() {
        documents.insert(path.clone(), text.clone());
        let root = hew_compile::module_membership(path, &FrontendOptions::default())
            .filter(hew_types::module_registry::ModuleMembership::checks_as_directory_module)
            .map_or_else(|| path.clone(), |membership| membership.entry);
        match units.iter_mut().find(|(known, _)| *known == root) {
            Some((_, members)) => members.push(index),
            None => units.push((root, vec![index])),
        }
    }
    let next = AtomicUsize::new(0);
    let outcomes: Vec<Mutex<Vec<MemberOutcome>>> =
        units.iter().map(|_| Mutex::new(Vec::new())).collect();
    let workers = std::thread::available_parallelism()
        .map_or(1, std::num::NonZeroUsize::get)
        .min(units.len());
    std::thread::scope(|scope| {
        for _ in 0..workers {
            scope.spawn(|| loop {
                let unit = next.fetch_add(1, Ordering::Relaxed);
                let Some((root, members)) = units.get(unit) else {
                    break;
                };
                let members: Vec<_> = members
                    .iter()
                    .map(|&index| (index, files[index].0.as_path(), files[index].1.as_str()))
                    .collect();
                let outcome = respell_unit(root, &members, &documents);
                *outcomes[unit]
                    .lock()
                    .unwrap_or_else(std::sync::PoisonError::into_inner) = outcome;
            });
        }
    });
    let mut refusals = Vec::new();
    for outcome in outcomes {
        for (index, outcome) in outcome
            .into_inner()
            .unwrap_or_else(std::sync::PoisonError::into_inner)
        {
            match outcome {
                Ok(respelled) => files[index].1 = respelled,
                Err(refusal) => refusals.push(refusal),
            }
        }
    }
    refusals
}

/// Respell the members of one check unit until no bare variant remains or
/// [`PASSES`] run out. A member keeps its respelling only when the re-check
/// adds no diagnostic to it.
fn respell_unit(
    root: &Path,
    members: &[(usize, &Path, &str)],
    documents: &DocumentSet,
) -> Vec<MemberOutcome> {
    let mut documents = documents.clone();
    let before = unit_diagnostics(root, &documents);
    let mut current: Vec<String> = members
        .iter()
        .map(|(_, _, text)| text.to_string())
        .collect();
    let mut diagnostics = before.clone();
    for _ in 0..PASSES {
        let mut changed = false;
        for ((_, path, _), text) in members.iter().zip(current.iter_mut()) {
            let respelled = apply_bare_variant_fixes(text, &reported_in(&diagnostics, path));
            if respelled != *text {
                changed = true;
                *text = respelled;
                documents.insert(path.to_path_buf(), text.clone());
            }
        }
        if !changed {
            break;
        }
        diagnostics = unit_diagnostics(root, &documents);
    }
    members
        .iter()
        .zip(current)
        .map(|(&(index, path, text), respelled)| {
            let outcome = if respelled == text {
                Ok(respelled)
            } else {
                let after = reported_in(&diagnostics, path);
                match first_new_diagnostic(&reported_in(&before, path), &after) {
                    Some(new) => {
                        let (line, column) =
                            crate::diagnostic::offset_to_line_col(&respelled, new.span.start);
                        Err(VariantRefusal {
                            file: path.to_path_buf(),
                            line,
                            column,
                            reason: format!(
                                "respelling its bare variants would add `{}`: {}",
                                new.kind.as_kind_str(),
                                new.message
                            ),
                        })
                    }
                    None => Ok(respelled),
                }
            };
            (index, outcome)
        })
        .collect()
}

/// A unit member's index in the caller's files, and its respelled text or
/// refusal.
type MemberOutcome = (usize, Result<String, VariantRefusal>);

/// One checker diagnostic located in the file being respelled.
#[derive(Clone)]
struct FileDiagnostic {
    kind: TypeErrorKind,
    message: String,
    span: std::ops::Range<usize>,
    suggestions: Vec<String>,
}

/// The type diagnostics a check of `root` reports, with the file each is in.
fn unit_diagnostics(
    root: &Path,
    documents: &DocumentSet,
) -> Vec<(Option<PathBuf>, FileDiagnostic)> {
    let options = FrontendOptions {
        documents: documents.clone(),
        ..FrontendOptions::default()
    };
    let input = root.display().to_string();
    let diagnostics = match hew_compile::check_file(&input, &options) {
        Ok(output) => output.diagnostics,
        Err(failure) => failure.diagnostics,
    };
    diagnostics
        .into_iter()
        .filter_map(|diagnostic| match diagnostic.kind {
            FrontendDiagnosticKind::Type(error) => Some((
                diagnostic
                    .filename
                    .and_then(|file| std::fs::canonicalize(file).ok()),
                FileDiagnostic {
                    kind: error.kind,
                    message: error.message,
                    span: error.span,
                    suggestions: error.suggestions,
                },
            )),
            FrontendDiagnosticKind::Parse(_)
            | FrontendDiagnosticKind::Message(_)
            | FrontendDiagnosticKind::Hir(_) => None,
        })
        .collect()
}

/// The diagnostics located in `path`.
fn reported_in(
    diagnostics: &[(Option<PathBuf>, FileDiagnostic)],
    path: &Path,
) -> Vec<FileDiagnostic> {
    let path = std::fs::canonicalize(path).ok();
    diagnostics
        .iter()
        .filter(|(file, _)| path.is_some() && *file == path)
        .map(|(_, diagnostic)| diagnostic.clone())
        .collect()
}

/// Apply the checker's fix-it at every `E_BARE_VARIANT_EXPR` site.
fn apply_bare_variant_fixes(source: &str, diagnostics: &[FileDiagnostic]) -> String {
    let mut edits: Vec<_> = diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.kind == TypeErrorKind::BareVariantExpr)
        .flat_map(|diagnostic| {
            let info = hew_analysis::code_actions::DiagnosticInfo {
                kind: Some(diagnostic.kind.as_kind_str().to_string()),
                message: diagnostic.message.clone(),
                span: hew_analysis::OffsetSpan {
                    start: diagnostic.span.start,
                    end: diagnostic.span.end,
                },
                suggestions: diagnostic.suggestions.clone(),
            };
            hew_analysis::code_actions::build_code_actions(source, &[info])
        })
        .flat_map(|action| action.edits)
        .collect();
    edits.sort_by_key(|edit| std::cmp::Reverse(edit.span.start));
    edits.dedup_by_key(|edit| edit.span.start);
    let mut respelled = source.to_string();
    let mut floor = usize::MAX;
    for edit in edits {
        // A site nested in one already edited waits for the next pass.
        if edit.span.end > floor {
            continue;
        }
        respelled.replace_range(edit.span.start..edit.span.end, &edit.new_text);
        floor = edit.span.start;
    }
    respelled
}

/// The first diagnostic in `after` whose code occurs more often than in
/// `before`, ignoring the bare variants this pass removes. A diagnostic at
/// exactly a bare variant's site in `before` is a consequence of that site,
/// so it does not count: the respelled site must check without it. One
/// inside the site's payload stays, since the respelling keeps the payload.
fn first_new_diagnostic<'a>(
    before: &[FileDiagnostic],
    after: &'a [FileDiagnostic],
) -> Option<&'a FileDiagnostic> {
    let sites: Vec<_> = before
        .iter()
        .filter(|diagnostic| diagnostic.kind == TypeErrorKind::BareVariantExpr)
        .map(|diagnostic| diagnostic.span.clone())
        .collect();
    let before: Vec<_> = before
        .iter()
        .filter(|diagnostic| {
            diagnostic.kind == TypeErrorKind::BareVariantExpr || !sites.contains(&diagnostic.span)
        })
        .cloned()
        .collect();
    let count = |diagnostics: &[FileDiagnostic], kind: &TypeErrorKind| {
        diagnostics
            .iter()
            .filter(|diagnostic| diagnostic.kind == *kind)
            .count()
    };
    after.iter().find(|diagnostic| {
        diagnostic.kind != TypeErrorKind::BareVariantExpr
            && count(after, &diagnostic.kind) > count(&before, &diagnostic.kind)
    })
}
