use std::collections::{HashMap, HashSet, VecDeque};

use dashmap::DashMap;
use hew_analysis::util::compute_line_offsets;
use hew_hir::{
    lower_program_host_target, verify_hir, HirDiagnostic, HirDiagnosticKind, ResolutionCtx,
};
use hew_parser::ParseDiagnosticKind;
use hew_types::error::{Severity, TypeErrorKind};
use hew_types::{LintId, TypeCheckOutput};
use tower_lsp_server::lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, DiagnosticTag, Location,
    NumberOrString, Uri as Url,
};

use super::uri::{same_source_file, source_file_key, FileUriExt};
#[cfg(test)]
use super::UriParse;
use super::{DiagnosticMap, DiagnosticSource, DocumentState};

// ── In-memory module resolution ──────────────────────────────────────

/// Keep the URI under which the editor opened a source, even when the compiler
/// reports its canonical filesystem path. Exact spellings take precedence.
fn open_document_uri(uri: &Url, documents: &DashMap<Url, DocumentState>) -> Url {
    if documents.contains_key(uri) {
        return uri.clone();
    }
    documents
        .iter()
        .filter(|entry| same_source_file(uri, entry.key()))
        .map(|entry| entry.key().clone())
        .min_by(|left, right| left.as_str().cmp(right.as_str()))
        .unwrap_or_else(|| uri.clone())
}

/// Return the source text for a file, preferring open editor buffers over disk.
///
/// Checks the LSP document store first so that unsaved edits to an imported
/// module are immediately visible to type-checking in the importing file.
/// Falls back to `std::fs::read_to_string` for files that are not open.
pub(super) fn source_for_path(
    path: &std::path::Path,
    documents: &DashMap<Url, DocumentState>,
) -> Option<String> {
    // Prefer in-memory content if the file is currently open in the editor.
    if let Some(url) = Url::from_file_path(path) {
        if let Some(doc) = documents.get(&open_document_uri(&url, documents)) {
            return Some(doc.source.clone());
        }
    }
    // Fall back to on-disk content.
    std::fs::read_to_string(path).ok()
}

pub(super) fn build_module_source_map(
    program: &hew_parser::ast::Program,
    documents: &DashMap<Url, DocumentState>,
) -> HashMap<String, DiagnosticSource> {
    let Some(ref module_graph) = program.module_graph else {
        return HashMap::new();
    };

    let mut module_sources = HashMap::new();
    for module_id in &module_graph.topo_order {
        if *module_id == module_graph.root {
            continue;
        }
        let Some(module) = module_graph.modules.get(module_id) else {
            continue;
        };
        let Some(source_path) = module.source_paths.first() else {
            continue;
        };
        let Some(uri) = Url::from_file_path(source_path) else {
            continue;
        };
        let Some(source) = source_for_path(source_path, documents) else {
            continue;
        };
        module_sources.insert(
            module_id.path.join("."),
            DiagnosticSource {
                uri: open_document_uri(&uri, documents),
                line_offsets: compute_line_offsets(&source),
                source,
            },
        );
    }

    module_sources
}

pub(super) fn merge_diagnostics(into: &mut DiagnosticMap, from: &DiagnosticMap) {
    for (uri, diagnostics) in from {
        into.entry(uri.clone())
            .or_default()
            .extend(diagnostics.iter().cloned());
    }
}

pub(super) fn sort_and_dedup_diagnostics(diagnostics: &mut Vec<Diagnostic>) {
    diagnostics.sort_by(|left, right| {
        left.range
            .start
            .line
            .cmp(&right.range.start.line)
            .then(left.range.start.character.cmp(&right.range.start.character))
            .then(left.range.end.line.cmp(&right.range.end.line))
            .then(left.range.end.character.cmp(&right.range.end.character))
            .then(
                left.source
                    .as_deref()
                    .unwrap_or("")
                    .cmp(right.source.as_deref().unwrap_or("")),
            )
            .then(left.message.cmp(&right.message))
    });

    let mut seen = HashSet::new();
    diagnostics.retain(|diagnostic| seen.insert(format!("{diagnostic:?}")));
}

pub(super) fn collect_published_diagnostics(
    documents: &DashMap<Url, DocumentState>,
    publish_uris: HashSet<Url>,
) -> Vec<(Url, Vec<Diagnostic>)> {
    let mut diagnostics_by_uri = DiagnosticMap::new();
    for entry in documents {
        merge_diagnostics(&mut diagnostics_by_uri, &entry.value().diagnostics_by_uri);
    }

    for uri in publish_uris {
        diagnostics_by_uri.entry(uri).or_default();
    }

    let mut published: Vec<_> = diagnostics_by_uri.into_iter().collect();
    for (_, diagnostics) in &mut published {
        sort_and_dedup_diagnostics(diagnostics);
    }
    published.sort_by(|(left_uri, _), (right_uri, _)| left_uri.as_str().cmp(right_uri.as_str()));
    published
}

/// The shared frontend's view of one open document.
///
/// Every source read consults the open buffers first, so an unsaved edit to an
/// imported module is visible to the importing file, and the root document's
/// own text overrides whatever is on disk for it.
fn document_set(
    root_path: &std::path::Path,
    root_source: &str,
    documents: &DashMap<Url, DocumentState>,
) -> hew_compile::DocumentSet {
    let mut set = hew_compile::DocumentSet::new();
    for entry in documents {
        if let Some(path) = entry.key().to_file_path() {
            set.insert(path.into_owned(), entry.value().source.clone());
        }
    }
    set.insert(root_path.to_path_buf(), root_source.to_string());
    set
}

fn frontend_options(
    root_path: &std::path::Path,
    root_source: &str,
    documents: &DashMap<Url, DocumentState>,
    extra_pkg_paths: &[std::path::PathBuf],
) -> hew_compile::FrontendOptions {
    hew_compile::FrontendOptions {
        pkg_path: extra_pkg_paths.first().cloned(),
        documents: document_set(root_path, root_source, documents),
        ..hew_compile::FrontendOptions::default()
    }
}

/// The files the resolved program depends on, for the open-importer index.
/// `None` when resolution did not complete, so the set is unknown.
fn dependency_uris(program: &hew_parser::ast::Program) -> Option<Vec<Url>> {
    let module_graph = program.module_graph.as_ref()?;
    let mut uris: Vec<Url> = module_graph
        .modules
        .values()
        .flat_map(|module| module.source_paths.iter())
        .filter_map(Url::from_file_path)
        .collect();
    uris.sort_by(|left, right| left.as_str().cmp(right.as_str()));
    uris.dedup();
    Some(uris)
}

pub(super) fn analyze_document(
    uri: &Url,
    source: &str,
    documents: &DashMap<Url, DocumentState>,
    extra_pkg_paths: &[std::path::PathBuf],
) -> DocumentState {
    let line_offsets = compute_line_offsets(source);
    // An untitled buffer has no file to anchor module resolution, so it is
    // checked as a standalone source under the server's working directory.
    // Its own text is what the driver reads either way, so both kinds of
    // document take the same run.
    let root_path = uri.to_file_path().map_or_else(
        || std::path::PathBuf::from("./untitled.hew"),
        std::borrow::Cow::into_owned,
    );

    let options = frontend_options(&root_path, source, documents, extra_pkg_paths);
    let mut state =
        hew_compile::run_source_frontend(source, &root_path.display().to_string(), &options);
    let parse_result = state
        .parse_result
        .take()
        .expect("the document frontend parses the root buffer");

    let mut diagnostics_by_uri = build_frontend_diagnostics_by_uri(
        uri,
        source,
        &line_offsets,
        &state.diagnostics,
        state.stopped.as_ref(),
    );

    let type_output = state.typecheck_result.take().and_then(|result| result.tco);
    let module_sources = build_module_source_map(&state.program, documents);
    // HIR lowering and ownership-SIR verification run only on a program the
    // checker accepted: a module already rejected upstream is not worth
    // lowering, and the upstream errors are the actionable signal.
    if let Some(tco) = type_output.as_ref().filter(|tco| tco.errors.is_empty()) {
        let (hir_diagnostics, hir_module) = collect_hir_diagnostics(&state.program, tco);
        merge_diagnostics(
            &mut diagnostics_by_uri,
            &build_hir_lsp_diagnostics(
                uri,
                source,
                &line_offsets,
                &module_sources,
                &hir_diagnostics,
            ),
        );
        if hir_diagnostics.is_empty() {
            merge_diagnostics(
                &mut diagnostics_by_uri,
                &build_semantic_lsp_diagnostics(
                    uri,
                    source,
                    &line_offsets,
                    &state.program,
                    &hir_module,
                    tco,
                ),
            );
        }
    }

    // Apply editor identity after all compiler stages, including their related
    // locations, so source provenance stays intact while publication uses the
    // same URI as the open buffer.
    let editor_uri = |target: &Url| {
        if same_source_file(target, uri) {
            uri.clone()
        } else {
            open_document_uri(target, documents)
        }
    };
    let mut editor_diagnostics = DiagnosticMap::new();
    for (target, mut diagnostics) in diagnostics_by_uri {
        for diagnostic in &mut diagnostics {
            if let Some(notes) = &mut diagnostic.related_information {
                for note in notes {
                    note.location.uri = editor_uri(&note.location.uri);
                }
            }
        }
        editor_diagnostics
            .entry(editor_uri(&target))
            .or_default()
            .extend(diagnostics);
    }

    DocumentState {
        source: source.to_string(),
        line_offsets,
        parse_result,
        type_output,
        dependency_uris: dependency_uris(&state.program),
        diagnostics_by_uri: editor_diagnostics,
    }
}

/// Map each file to the open documents whose resolved module graph reaches it.
fn build_reverse_importer_index(documents: &DashMap<Url, DocumentState>) -> HashMap<Url, Vec<Url>> {
    let mut index: HashMap<Url, Vec<Url>> = HashMap::with_capacity(documents.len());

    for entry in documents {
        let importer_uri = entry.key().clone();
        let Some(dependencies) = entry.value().dependency_uris.as_ref() else {
            continue;
        };
        let importer_key = source_file_key(&importer_uri);
        for dependency_uri in dependencies {
            let dependency_key = source_file_key(dependency_uri);
            if dependency_key == importer_key {
                continue;
            }
            index
                .entry(dependency_key)
                .or_default()
                .push(importer_uri.clone());
        }
    }

    index
}

pub(super) fn refresh_open_importers(
    target_uri: &Url,
    documents: &DashMap<Url, DocumentState>,
    publish_uris: &mut HashSet<Url>,
    extra_pkg_paths: &[std::path::PathBuf],
) {
    // BFS over the open-document importer graph so that transitive importers
    // (e.g. A -> B -> C when C changes) are also re-analysed.
    // `visited` tracks every URI we have already queued or processed so that
    // diamond imports and import cycles don't cause infinite loops.
    let reverse_importer_index = build_reverse_importer_index(documents);
    let mut visited: HashSet<Url> = HashSet::from([target_uri.clone()]);
    let mut queue: VecDeque<Url> = VecDeque::from([target_uri.clone()]);

    // A document whose frontend stopped has no resolved module graph to index,
    // so this edit may be the one that resolves it.
    let mut unresolved: Vec<Url> = documents
        .iter()
        .filter(|entry| entry.value().dependency_uris.is_none())
        .map(|entry| entry.key().clone())
        .collect();

    while let Some(current) = queue.pop_front() {
        let dependents: Vec<_> = reverse_importer_index
            .get(&source_file_key(&current))
            .into_iter()
            .flat_map(|uris| uris.iter())
            .cloned()
            .chain(std::mem::take(&mut unresolved))
            .filter_map(|importer_uri| {
                if visited.contains(&importer_uri) {
                    return None;
                }
                let importer = documents.get(&importer_uri)?;
                Some((
                    importer_uri.clone(),
                    importer.source.clone(),
                    importer
                        .diagnostics_by_uri
                        .keys()
                        .cloned()
                        .collect::<Vec<_>>(),
                ))
            })
            .collect();

        for (importer_uri, importer_source, previous_diagnostic_uris) in dependents {
            visited.insert(importer_uri.clone());
            publish_uris.insert(importer_uri.clone());
            publish_uris.extend(previous_diagnostic_uris);
            let document =
                analyze_document(&importer_uri, &importer_source, documents, extra_pkg_paths);
            documents.insert(importer_uri.clone(), document);
            // Enqueue this importer so its own importers are refreshed too.
            queue.push_back(importer_uri);
        }
    }
}

pub(super) fn refresh_document_and_dependents(
    uri: &Url,
    source: &str,
    documents: &DashMap<Url, DocumentState>,
    extra_pkg_paths: &[std::path::PathBuf],
) -> Vec<(Url, Vec<Diagnostic>)> {
    let mut publish_uris = HashSet::from([uri.clone()]);
    if let Some(previous) = documents.get(uri) {
        publish_uris.extend(previous.diagnostics_by_uri.keys().cloned());
    }

    let document = analyze_document(uri, source, documents, extra_pkg_paths);
    documents.insert(uri.clone(), document);

    refresh_open_importers(uri, documents, &mut publish_uris, extra_pkg_paths);

    collect_published_diagnostics(documents, publish_uris)
}

pub(super) fn close_document_and_dependents(
    uri: &Url,
    documents: &DashMap<Url, DocumentState>,
    extra_pkg_paths: &[std::path::PathBuf],
) -> Vec<(Url, Vec<Diagnostic>)> {
    let Some((_removed_uri, removed_document)) = documents.remove(uri) else {
        return vec![];
    };

    let mut publish_uris = HashSet::from([uri.clone()]);
    publish_uris.extend(removed_document.diagnostics_by_uri.keys().cloned());

    refresh_open_importers(uri, documents, &mut publish_uris, extra_pkg_paths);

    collect_published_diagnostics(documents, publish_uris)
}

// ── Diagnostics ──────────────────────────────────────────────────────

pub(super) fn insert_diagnostic(
    diagnostics_by_uri: &mut DiagnosticMap,
    uri: Url,
    diagnostic: Diagnostic,
) {
    diagnostics_by_uri.entry(uri).or_default().push(diagnostic);
}

/// Where a shared-frontend diagnostic points: its own module's file when it
/// carries one, otherwise the document being analyzed.
struct DiagnosticTarget {
    uri: Url,
    source: String,
    line_offsets: Vec<usize>,
}

impl DiagnosticTarget {
    fn range(&self, span: &hew_parser::ast::Span) -> tower_lsp_server::lsp_types::Range {
        super::span_to_range(&self.source, &self.line_offsets, span)
    }
}

fn diagnostic_target(
    filename: Option<&str>,
    text: Option<&str>,
    root_uri: &Url,
    root_source: &str,
    root_line_offsets: &[usize],
) -> DiagnosticTarget {
    let mut uri = filename
        .map(std::path::Path::new)
        .and_then(Url::from_file_path)
        .unwrap_or_else(|| root_uri.clone());
    if same_source_file(&uri, root_uri) {
        uri = root_uri.clone();
    }
    match text {
        Some(text) if uri != *root_uri => DiagnosticTarget {
            uri,
            line_offsets: compute_line_offsets(text),
            source: text.to_string(),
        },
        _ => DiagnosticTarget {
            uri,
            source: root_source.to_string(),
            line_offsets: root_line_offsets.to_vec(),
        },
    }
}

fn type_related_information(
    diagnostic: &hew_types::TypeError,
    note_sources: &[Option<(String, String)>],
    target: &DiagnosticTarget,
) -> Option<Vec<DiagnosticRelatedInformation>> {
    (!diagnostic.notes.is_empty()).then(|| {
        diagnostic
            .notes
            .iter()
            .enumerate()
            .map(|(index, (note_span, note_msg, _))| {
                let note_target = note_sources.get(index).and_then(Option::as_ref).and_then(
                    |(text, filename)| {
                        let uri = Url::from_file_path(std::path::Path::new(filename))?;
                        Some(DiagnosticTarget {
                            uri,
                            line_offsets: compute_line_offsets(text),
                            source: text.clone(),
                        })
                    },
                );
                let note_target = note_target.as_ref().unwrap_or(target);
                DiagnosticRelatedInformation {
                    location: Location {
                        uri: note_target.uri.clone(),
                        range: note_target.range(note_span),
                    },
                    message: note_msg.clone(),
                }
            })
            .collect()
    })
}

fn parse_lsp_diagnostic(error: &hew_parser::ParseError, target: &DiagnosticTarget) -> Diagnostic {
    Diagnostic {
        range: target.range(&error.span),
        severity: Some(match error.severity {
            hew_parser::Severity::Error => DiagnosticSeverity::ERROR,
            hew_parser::Severity::Warning => DiagnosticSeverity::WARNING,
        }),
        code: Some(NumberOrString::String(error.kind.as_kind_str().to_string())),
        source: Some("hew-parser".to_string()),
        message: error.hint.as_ref().map_or_else(
            || error.message.clone(),
            |hint| format!("{}\n\nhint: {hint}", error.message),
        ),
        data: Some(parse_diagnostic_data(&error.kind)),
        ..Default::default()
    }
}

fn type_lsp_diagnostic(
    error: &hew_types::TypeError,
    note_sources: &[Option<(String, String)>],
    target: &DiagnosticTarget,
) -> Diagnostic {
    Diagnostic {
        range: target.range(&error.span),
        severity: Some(severity_to_lsp(error.severity)),
        code: Some(NumberOrString::String(error.kind.as_kind_str().to_string())),
        tags: unnecessary_diagnostic_tags(&error.kind),
        source: Some("hew-types".to_string()),
        message: if error.suggestions.is_empty() {
            error.message.clone()
        } else {
            format!(
                "{}\n\nDid you mean: {}",
                error.message,
                error.suggestions.join(", ")
            )
        },
        related_information: type_related_information(error, note_sources, target),
        data: Some(diagnostic_data(&error.kind, &error.suggestions)),
        ..Default::default()
    }
}

fn message_lsp_diagnostic(
    message: &hew_compile::FrontendMessageDiagnostic,
    target: &DiagnosticTarget,
    root_uri: &Url,
    root_source: &str,
    root_line_offsets: &[usize],
) -> Diagnostic {
    Diagnostic {
        range: message
            .span
            .as_ref()
            .map_or_else(zero_range, |span| target.range(span)),
        severity: Some(DiagnosticSeverity::ERROR),
        code: Some(NumberOrString::String(message.code.clone())),
        source: Some("hew-compile".to_string()),
        message: if message.help.is_empty() {
            message.message.clone()
        } else {
            format!("{}\n\n{}", message.message, message.help.join("\n"))
        },
        related_information: (!message.notes.is_empty()).then(|| {
            message
                .notes
                .iter()
                .map(|note| {
                    let note_target = diagnostic_target(
                        Some(&note.filename),
                        Some(&note.source),
                        root_uri,
                        root_source,
                        root_line_offsets,
                    );
                    DiagnosticRelatedInformation {
                        location: Location {
                            uri: note_target.uri.clone(),
                            range: note_target.range(&note.span),
                        },
                        message: note.message.clone(),
                    }
                })
                .collect()
        }),
        ..Default::default()
    }
}

/// Route the shared frontend's diagnostics to the files they belong to.
///
/// The LSP publishes exactly what `hew check` reports for the same source,
/// including the message-level import and manifest failures that stop the
/// pipeline before the checker runs.
fn build_frontend_diagnostics_by_uri(
    root_uri: &Url,
    root_source: &str,
    root_line_offsets: &[usize],
    diagnostics: &[hew_compile::FrontendDiagnostic],
    stopped: Option<&hew_compile::FrontendFailure>,
) -> DiagnosticMap {
    use hew_compile::FrontendDiagnosticKind;

    let mut diagnostics_by_uri = DiagnosticMap::new();

    for diagnostic in diagnostics {
        let inline_source = match &diagnostic.kind {
            FrontendDiagnosticKind::Message(message) => message.source.as_deref(),
            _ => diagnostic.source.as_deref(),
        };
        let target = diagnostic_target(
            diagnostic.filename.as_deref(),
            inline_source,
            root_uri,
            root_source,
            root_line_offsets,
        );

        let lsp_diagnostic = match &diagnostic.kind {
            FrontendDiagnosticKind::Parse(error) => parse_lsp_diagnostic(error, &target),
            FrontendDiagnosticKind::Type(error) => {
                type_lsp_diagnostic(error, &diagnostic.note_sources, &target)
            }
            FrontendDiagnosticKind::Message(message) => {
                message_lsp_diagnostic(message, &target, root_uri, root_source, root_line_offsets)
            }
            FrontendDiagnosticKind::Hir(_) => continue,
        };

        insert_diagnostic(&mut diagnostics_by_uri, target.uri, lsp_diagnostic);
    }

    // A stage failure with nothing to point at still has to reach the editor.
    if let Some(failure) = stopped.filter(|failure| failure.diagnostics.is_empty()) {
        insert_diagnostic(
            &mut diagnostics_by_uri,
            root_uri.clone(),
            Diagnostic {
                range: zero_range(),
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(NumberOrString::String("E_FRONTEND".to_string())),
                source: Some("hew-compile".to_string()),
                message: failure.message.clone(),
                ..Default::default()
            },
        );
    }

    diagnostics_by_uri
}

/// Lower to HIR once and return both the HIR diagnostics and the lowered
/// module, so a later consumer can reuse the same lowering instead of
/// repeating it.
fn collect_hir_diagnostics(
    program: &hew_parser::ast::Program,
    type_output: &TypeCheckOutput,
) -> (Vec<HirDiagnostic>, hew_hir::HirModule) {
    let lower_output = lower_program_host_target(program, type_output, &ResolutionCtx);
    let diagnostics =
        dedup_hir_diagnostics(lower_output.diagnostics, verify_hir(&lower_output.module));
    (diagnostics, lower_output.module)
}

/// Render shared semantic-boundary errors for an editor document.
/// Target-independent ownership checks use the same resolved roots as native
/// compilation; source-lint presentation remains separate.
fn build_semantic_lsp_diagnostics(
    root_uri: &Url,
    root_source: &str,
    root_line_offsets: &[usize],
    program: &hew_parser::ast::Program,
    hir_module: &hew_hir::HirModule,
    tco: &TypeCheckOutput,
) -> DiagnosticMap {
    let mut diagnostics_by_uri = DiagnosticMap::new();

    // The LSP has no target selector, so it uses the same native target facts
    // as a default `hew build`. The session fixes the check set to Build.
    let session = hew_compile::Session::new(
        hew_compile::SessionTarget::native(),
        hew_compile::DiagnosticPolicy::default(),
    );
    let result = hew_compile::Session::source_roots(program, tco)
        .and_then(|roots| session.lower_hir_module(hir_module, tco, &roots));
    if let Err(error) = result {
        insert_diagnostic(
            &mut diagnostics_by_uri,
            root_uri.clone(),
            Diagnostic {
                range: super::span_to_range(root_source, root_line_offsets, &(0..0)),
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(NumberOrString::String("E_SIR_VERIFY".to_string())),
                source: Some("hew-sir".to_string()),
                message: error.to_string(),
                ..Default::default()
            },
        );
    }

    diagnostics_by_uri
}

fn dedup_hir_diagnostics(
    mut lower_diagnostics: Vec<HirDiagnostic>,
    verifier_diagnostics: Vec<HirDiagnostic>,
) -> Vec<HirDiagnostic> {
    for diagnostic in verifier_diagnostics {
        let already_present = lower_diagnostics
            .iter()
            .any(|existing| existing.kind == diagnostic.kind && existing.span == diagnostic.span);
        if !already_present {
            lower_diagnostics.push(diagnostic);
        }
    }
    lower_diagnostics
}

fn build_hir_lsp_diagnostics(
    root_uri: &Url,
    root_source: &str,
    root_line_offsets: &[usize],
    module_sources: &HashMap<String, DiagnosticSource>,
    hir_diagnostics: &[HirDiagnostic],
) -> DiagnosticMap {
    let mut diagnostics_by_uri = DiagnosticMap::new();

    for diagnostic in hir_diagnostics {
        let unavailable_source = diagnostic
            .source_module
            .as_ref()
            .filter(|module_name| !module_sources.contains_key(*module_name));
        let target = diagnostic
            .source_module
            .as_ref()
            .and_then(|module_name| module_sources.get(module_name));
        let (target_uri, target_source, target_line_offsets) = if let Some(target) = target {
            (
                target.uri.clone(),
                target.source.as_str(),
                target.line_offsets.as_slice(),
            )
        } else {
            (root_uri.clone(), root_source, root_line_offsets)
        };
        let base_message = hir_diagnostic_message(&diagnostic.kind, &diagnostic.note);
        let message = if let Some(module_name) = unavailable_source {
            format!("[module '{module_name}' source unavailable] {base_message}")
        } else {
            base_message
        };
        let range = if unavailable_source.is_some() {
            zero_range()
        } else {
            super::span_to_range(target_source, target_line_offsets, &diagnostic.span)
        };
        let related_information = if diagnostic.secondary_spans.is_empty() {
            None
        } else {
            Some(
                diagnostic
                    .secondary_spans
                    .iter()
                    .map(|(span, label)| DiagnosticRelatedInformation {
                        location: Location {
                            uri: target_uri.clone(),
                            range: if unavailable_source.is_some() {
                                zero_range()
                            } else {
                                super::span_to_range(target_source, target_line_offsets, span)
                            },
                        },
                        message: label.clone(),
                    })
                    .collect(),
            )
        };
        insert_diagnostic(
            &mut diagnostics_by_uri,
            target_uri,
            Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                code: Some(NumberOrString::String(diagnostic.kind.kind_string())),
                source: Some("hew-hir".to_string()),
                message,
                related_information,
                data: Some(hir_diagnostic_data(&diagnostic.kind)),
                ..Default::default()
            },
        );
    }

    diagnostics_by_uri
}

fn hir_diagnostic_message(kind: &HirDiagnosticKind, note: &str) -> String {
    let base = match kind {
        HirDiagnosticKind::NotYetImplemented {
            construct,
            owning_pass,
        } => format!("not yet implemented: {construct} (planned: {owning_pass})"),
        _ => kind.kind_string(),
    };

    if note.is_empty() {
        base
    } else {
        format!("{base}\n\nnote: {note}")
    }
}

fn hir_diagnostic_data(kind: &HirDiagnosticKind) -> serde_json::Value {
    serde_json::json!({
        "kind": kind.kind_string(),
        "source": "hir",
    })
}

fn zero_range() -> tower_lsp_server::lsp_types::Range {
    tower_lsp_server::lsp_types::Range::new(
        tower_lsp_server::lsp_types::Position::new(0, 0),
        tower_lsp_server::lsp_types::Position::new(0, 0),
    )
}

/// Convert a type-checker `Severity` to the corresponding LSP `DiagnosticSeverity`.
///
/// This is the authoritative severity mapping: the `TypeError` struct carries the
/// severity that was decided at emit time, so the LSP should honour it directly
/// rather than re-deriving it from the `TypeErrorKind`.
pub(super) fn severity_to_lsp(severity: Severity) -> DiagnosticSeverity {
    match severity {
        Severity::Error => DiagnosticSeverity::ERROR,
        Severity::Warning => DiagnosticSeverity::WARNING,
    }
}

/// Encode a `TypeErrorKind` discriminant and suggestions as JSON for `Diagnostic.data`.
pub(super) fn diagnostic_data(kind: &TypeErrorKind, suggestions: &[String]) -> serde_json::Value {
    serde_json::json!({
        "kind": kind.as_kind_str(),
        "suggestions": suggestions,
    })
}

/// Encode a `ParseDiagnosticKind` discriminant as JSON for `Diagnostic.data`.
///
/// The shape mirrors `diagnostic_data` so editor extensions can handle both
/// parse and type errors with the same `data` consumer.
pub(super) fn parse_diagnostic_data(kind: &ParseDiagnosticKind) -> serde_json::Value {
    serde_json::json!({
        "kind": kind.as_kind_str(),
    })
}

fn unnecessary_diagnostic_tags(kind: &TypeErrorKind) -> Option<Vec<DiagnosticTag>> {
    match kind {
        TypeErrorKind::UnusedVariable
        | TypeErrorKind::UnusedMut
        | TypeErrorKind::UnusedImport
        | TypeErrorKind::UnreachableCode
        | TypeErrorKind::Lint(LintId::DeadCode) => Some(vec![DiagnosticTag::UNNECESSARY]),
        _ => None,
    }
}

#[cfg(test)]
pub(super) mod tests {
    use hew_hir::{HirItem, HirModule, HirNodeId};
    use hew_parser::ast::Span;
    use tower_lsp_server::lsp_types::Position;

    use super::*;

    #[test]
    fn hir_root_diagnostics_render_to_root_uri() {
        let source = "fn main() {}\n";
        let line_offsets = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let diagnostics = vec![HirDiagnostic::new(
            HirDiagnosticKind::TaskCannotEscape,
            3..7,
            "task handles must stay inside fork bodies",
        )];

        let by_uri =
            build_hir_lsp_diagnostics(&uri, source, &line_offsets, &HashMap::new(), &diagnostics);
        let rendered = by_uri
            .get(&uri)
            .expect("root diagnostics must stay on the root URI");
        let diagnostic = rendered.first().expect("expected one HIR diagnostic");

        assert_eq!(diagnostic.source.as_deref(), Some("hew-hir"));
        assert_eq!(diagnostic.severity, Some(DiagnosticSeverity::ERROR));
        assert_eq!(diagnostic.range.start, Position::new(0, 3));
        assert_eq!(diagnostic.range.end, Position::new(0, 7));
        assert_eq!(
            diagnostic
                .data
                .as_ref()
                .and_then(|data| data.get("kind"))
                .and_then(serde_json::Value::as_str),
            Some("TaskCannotEscape")
        );
    }

    #[test]
    fn hir_non_root_diagnostics_route_to_imported_uri() {
        let root_uri = Url::parse("file:///project/main.hew").unwrap();
        let helper_uri = Url::parse("file:///project/helper.hew").unwrap();
        let root_source = "fn main() {}\n";
        let helper_source = "fn helper() { bogus }\n";
        let diagnostics = vec![HirDiagnostic::new(
            HirDiagnosticKind::UnresolvedSymbol {
                name: "bogus".to_string(),
            },
            14..19,
            "imported helper body references an unresolved name",
        )
        .with_source_module(Some("helper".to_string()))];
        let module_sources = HashMap::from([(
            "helper".to_string(),
            DiagnosticSource {
                uri: helper_uri.clone(),
                source: helper_source.to_string(),
                line_offsets: compute_line_offsets(helper_source),
            },
        )]);

        let by_uri = build_hir_lsp_diagnostics(
            &root_uri,
            root_source,
            &compute_line_offsets(root_source),
            &module_sources,
            &diagnostics,
        );
        let helper_diags = by_uri
            .get(&helper_uri)
            .expect("imported-module HIR diagnostics must route to the imported URI");
        let diagnostic = helper_diags
            .first()
            .expect("expected one imported-module diagnostic");

        assert_eq!(diagnostic.range.start, Position::new(0, 14));
        assert_eq!(diagnostic.range.end, Position::new(0, 19));
    }

    #[test]
    fn hir_source_map_miss_fails_closed_to_root_zero_range() {
        let root_uri = Url::parse("file:///project/main.hew").unwrap();
        let root_source = "fn main() {}\n";
        let diagnostics = vec![HirDiagnostic::new(
            HirDiagnosticKind::UnresolvedSymbol {
                name: "phantom".to_string(),
            },
            99..120,
            "phantom import body references an unresolved name",
        )
        .with_source_module(Some("phantom".to_string()))];

        let by_uri = build_hir_lsp_diagnostics(
            &root_uri,
            root_source,
            &compute_line_offsets(root_source),
            &HashMap::new(),
            &diagnostics,
        );
        let root_diags = by_uri
            .get(&root_uri)
            .expect("source-map miss must still publish a diagnostic on the root URI");
        let diagnostic = root_diags
            .first()
            .expect("expected one fail-closed diagnostic");

        assert_eq!(diagnostic.range, zero_range());
        assert!(diagnostic
            .message
            .starts_with("[module 'phantom' source unavailable]"));
    }

    #[test]
    fn hir_secondary_spans_render_as_related_information() {
        let root_uri = Url::parse("file:///project/main.hew").unwrap();
        let root_source = "fn main() { abc }\nfn helper() {}\n";
        let diagnostic = HirDiagnostic::new(
            HirDiagnosticKind::UnresolvedSymbol {
                name: "abc".to_string(),
            },
            12..15,
            "identifier has no binding in resolved HIR",
        )
        .with_secondary_spans(vec![(21..27, "declared here".to_string())]);

        let by_uri = build_hir_lsp_diagnostics(
            &root_uri,
            root_source,
            &compute_line_offsets(root_source),
            &HashMap::new(),
            &[diagnostic],
        );
        let rendered = by_uri
            .get(&root_uri)
            .and_then(|diagnostics| diagnostics.first())
            .expect("expected one root diagnostic");
        let related = rendered
            .related_information
            .as_ref()
            .expect("secondary spans should render as related information");

        assert_eq!(related.len(), 1);
        assert_eq!(related[0].location.uri, root_uri);
        assert_eq!(related[0].location.range.start, Position::new(1, 3));
    }

    #[test]
    fn verifier_only_hir_diagnostics_are_rendered() {
        let source = "fn main() -> i64 { 1 }\n";
        let line_offsets = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let span = 19..20;

        let combined = dedup_hir_diagnostics(
            Vec::new(),
            verify_hir(&duplicate_node_module(span.clone()).0),
        );
        assert_eq!(combined.len(), 1, "expected one verifier-only diagnostic");

        let by_uri =
            build_hir_lsp_diagnostics(&uri, source, &line_offsets, &HashMap::new(), &combined);
        let rendered = by_uri
            .get(&uri)
            .and_then(|diagnostics| diagnostics.first())
            .expect("verifier-only HIR diagnostic should render to LSP");

        assert_eq!(rendered.source.as_deref(), Some("hew-hir"));
        assert_eq!(
            rendered
                .data
                .as_ref()
                .and_then(|data| data.get("kind"))
                .and_then(serde_json::Value::as_str),
            Some("DuplicateNodeId")
        );
    }

    #[test]
    fn duplicate_verifier_hir_diagnostics_are_suppressed_by_kind_and_span() {
        let span = 19..20;
        // `DuplicateNodeId` carries the id inside the kind, so the id
        // participates in the suppression key. Take it from the module that
        // duplicated it rather than pinning a lowering-internal counter value.
        let (module, duplicated) = duplicate_node_module(span.clone());
        let lower_diagnostic = HirDiagnostic::new(
            HirDiagnosticKind::DuplicateNodeId { id: duplicated },
            span.clone(),
            "lowering already reported duplicate HIR node id",
        );

        let combined = dedup_hir_diagnostics(vec![lower_diagnostic.clone()], verify_hir(&module));

        assert_eq!(
            combined.len(),
            1,
            "duplicate verifier diagnostic should be suppressed"
        );
        assert_eq!(combined[0], lower_diagnostic);
    }

    #[test]
    fn analyze_document_skips_hir_when_typecheck_fails() {
        let uri = Url::parse("file:///test.hew").unwrap();
        let document = analyze_document(&uri, "fn main() -> i32 { missing }", &DashMap::new(), &[]);

        let root_diags = document
            .diagnostics_by_uri
            .get(&uri)
            .expect("root document should always have a diagnostics entry");

        assert!(
            root_diags
                .iter()
                .all(|diagnostic| diagnostic.source.as_deref() != Some("hew-hir")),
            "HIR diagnostics must not run when type checking reports errors: {root_diags:?}"
        );
    }

    #[test]
    fn analyze_document_publishes_hir_diagnostics_after_successful_typecheck() {
        let main_uri = Url::parse("file:///project/main.hew").unwrap();
        let document = analyze_document(
            &main_uri,
            "fn main() { let xs: Vec<()> = []; let _: () = xs[0]; }\n",
            &DashMap::new(),
            &[],
        );
        let root_diags = document
            .diagnostics_by_uri
            .get(&main_uri)
            .expect("root document should always have a diagnostics entry");

        assert!(
            root_diags
                .iter()
                .any(|diagnostic| diagnostic.source.as_deref() == Some("hew-hir")),
            "expected HIR diagnostics after successful typecheck: {root_diags:?}"
        );
    }

    #[test]
    fn analyze_document_reports_comment_text_direction_lint() {
        let uri = Url::parse("file:///project/main.hew").unwrap();
        let document = analyze_document(&uri, "// \u{202E}\nfn main() {}\n", &DashMap::new(), &[]);
        let root_diags = document
            .diagnostics_by_uri
            .get(&uri)
            .expect("root document should always have diagnostics");

        assert!(
            root_diags.iter().any(|diagnostic| {
                diagnostic.source.as_deref() == Some("hew-types")
                    && diagnostic.severity == Some(DiagnosticSeverity::ERROR)
                    && diagnostic.code
                        == Some(NumberOrString::String(
                            "text_direction_codepoint_in_comment".to_string(),
                        ))
            }),
            "expected text_direction_codepoint_in_comment diagnostic: {root_diags:?}"
        );
    }

    #[test]
    fn analyze_document_honours_comment_lint_allow_directive() {
        let uri = Url::parse("file:///project/main.hew").unwrap();
        let source =
            "// hew:allow(text_direction_codepoint_in_comment)\n// \u{202E}\nfn main() {}\n";
        let document = analyze_document(&uri, source, &DashMap::new(), &[]);
        let root_diags: &[Diagnostic] = document
            .diagnostics_by_uri
            .get(&uri)
            .map_or(&[], Vec::as_slice);

        assert!(
            !root_diags.iter().any(|diagnostic| {
                diagnostic.code
                    == Some(NumberOrString::String(
                        "text_direction_codepoint_in_comment".to_string(),
                    ))
            }),
            "allow directive must suppress the source lint: {root_diags:?}"
        );
    }

    fn duplicate_node_module(span: Span) -> (HirModule, HirNodeId) {
        let source = "fn main() -> i64 { 1 }\n";
        let parse_result = hew_parser::parse(source);
        let mut checker =
            hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let type_output = checker.check_program(&parse_result.program);
        let mut module =
            lower_program_host_target(&parse_result.program, &type_output, &ResolutionCtx).module;
        let HirItem::Function(function) = module.items.first_mut().expect("expected one function")
        else {
            panic!("expected a function item")
        };
        function.body.span = span.clone();
        if let Some(tail) = function.body.tail.as_mut() {
            tail.node = function.body.node;
            tail.span = span;
        }
        let duplicated = function.body.node;
        (module, duplicated)
    }

    // ── Diagnostic.code tests ────────────────────────────────────────────

    /// Analyze an in-memory buffer through the shared frontend and return the
    /// diagnostics published for the document itself.
    fn analyzed_diagnostics(source: &str) -> Vec<Diagnostic> {
        let uri = test_document_uri();
        let mut document = analyze_document(&uri, source, &DashMap::new(), &[]);
        document.diagnostics_by_uri.remove(&uri).unwrap_or_default()
    }

    fn test_document_uri() -> Url {
        #[cfg(windows)]
        let path = std::path::PathBuf::from("C:/hew-lsp-test/main.hew");
        #[cfg(not(windows))]
        let path = std::path::PathBuf::from("/hew-lsp-test/main.hew");
        Url::from_file_path(path).expect("test path is absolute")
    }

    #[test]
    fn type_diagnostic_code_is_set_to_kind_string() {
        // A type error (UndefinedVariable) should produce a diagnostic whose
        // `code` field is `Some(NumberOrString::String("UndefinedVariable"))`.
        let diagnostics = analyzed_diagnostics("fn main() { missing_name }\n");
        let type_diag = diagnostics
            .iter()
            .find(|d| d.source.as_deref() == Some("hew-types"))
            .expect("expected at least one hew-types diagnostic");

        assert!(
            type_diag.code.is_some(),
            "type diagnostic must carry a code; got None"
        );
        let code_str = match &type_diag.code {
            Some(NumberOrString::String(s)) => s.as_str(),
            other => panic!("expected NumberOrString::String, got {other:?}"),
        };
        assert_eq!(code_str, "UndefinedVariable");
    }

    #[test]
    fn parse_diagnostic_code_is_set_to_kind_string() {
        // A parse error should produce a diagnostic whose `code` field is
        // `Some(NumberOrString::String(<kind-string>))`.
        // missing expression after `=`
        let diagnostics = analyzed_diagnostics("fn main() { let x = ; }\n");
        let parse_diag = diagnostics
            .iter()
            .find(|d| d.source.as_deref() == Some("hew-parser"))
            .expect("expected at least one hew-parser diagnostic");

        assert!(
            parse_diag.code.is_some(),
            "parse diagnostic must carry a code; got None"
        );
        assert!(
            matches!(&parse_diag.code, Some(NumberOrString::String(_))),
            "parse diagnostic code must be a string variant"
        );
    }

    #[test]
    fn hir_diagnostic_code_is_set_to_kind_string() {
        // An HIR diagnostic should produce a Diagnostic whose `code` field
        // is `Some(NumberOrString::String("TaskCannotEscape"))`.
        let source = "fn main() {}\n";
        let lo = compute_line_offsets(source);
        let uri = Url::parse("file:///test.hew").unwrap();
        let diagnostics = vec![HirDiagnostic::new(
            HirDiagnosticKind::TaskCannotEscape,
            3..7,
            "",
        )];

        let by_uri = build_hir_lsp_diagnostics(&uri, source, &lo, &HashMap::new(), &diagnostics);
        let rendered = by_uri.get(&uri).expect("root diagnostic expected");
        let diag = rendered.first().expect("expected one HIR diagnostic");

        assert!(
            diag.code.is_some(),
            "HIR diagnostic must carry a code; got None"
        );
        match &diag.code {
            Some(NumberOrString::String(s)) => {
                assert_eq!(s, "TaskCannotEscape");
            }
            other => panic!("expected NumberOrString::String, got {other:?}"),
        }
    }

    // ── New v0.5 surface coverage (imported-stdlib method tooling) ───────
    //
    // These tests exercise the LSP coverage that depends on std-module
    // inlining in `populate_user_module_imports`: hover, inlay hints,
    // signature help, completion, and the absence of false "no method"
    // diagnostics for method surfaces on imported stdlib types. Before the
    // inlining fix, imported stdlib types (regex.Pattern, net.Listener, …)
    // were never registered in `type_defs`/`fn_sigs`, so completion and
    // signature help silently returned nothing and the checker's narrower
    // `ModuleRegistry` handle-method table even raised false diagnostics for
    // valid method calls (e.g. `regex.Pattern.captures`).

    /// Anchor a synthetic document at the repository root so tier-2 module
    /// search resolves the in-worktree `std/` tree (no env mutation, so the
    /// test is parallel-safe).
    fn analyze_repo_rooted(file_stem: &str, source: &str) -> DocumentState {
        let repo_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("hew-lsp has a parent (repo root)")
            .to_path_buf();
        let path = repo_root.join(format!("{file_stem}.hew"));
        let uri = Url::from_file_path(&path).expect("repo-rooted path is absolute");
        let docs: DashMap<Url, DocumentState> = DashMap::new();
        analyze_document(&uri, source, &docs, &[])
    }

    fn surface_hover(doc: &DocumentState, source: &str, needle: &str) -> String {
        let off = source.find(needle).expect("needle present") + needle.len() - 1;
        hew_analysis::hover::hover(
            &doc.source,
            &doc.parse_result,
            doc.type_output.as_ref(),
            off,
        )
        .map(|h| h.contents)
        .unwrap_or_default()
    }

    fn surface_sighelp(doc: &DocumentState, source: &str, call: &str) -> Vec<String> {
        let cp = source.find(call).expect("call present");
        let paren = cp + source[cp..].find('(').expect("call has open paren") + 1;
        hew_analysis::signature_help::build_signature_help(
            &doc.source,
            doc.type_output.as_ref().expect("type output present"),
            paren,
        )
        .map(|s| s.signatures.into_iter().map(|x| x.label).collect())
        .unwrap_or_default()
    }

    fn surface_completions(doc: &DocumentState, source: &str, receiver_dot: &str) -> Vec<String> {
        let cp = source.find(receiver_dot).expect("receiver present");
        let dot = cp + source[cp..].find('.').expect("receiver has dot") + 1;
        hew_analysis::completions::complete(
            &doc.source,
            &doc.parse_result,
            doc.type_output.as_ref(),
            dot,
        )
        .into_iter()
        .map(|i| i.label)
        .collect()
    }

    fn surface_inlays(doc: &DocumentState) -> Vec<String> {
        hew_analysis::inlay_hints::build_inlay_hints(
            &doc.source,
            &doc.parse_result,
            doc.type_output.as_ref().expect("type output present"),
        )
        .into_iter()
        .map(|h| h.label)
        .collect()
    }

    fn hard_type_diagnostic(doc: &DocumentState) -> Option<String> {
        doc.type_output.as_ref().and_then(|tc| {
            tc.errors
                .iter()
                .find(|e| e.severity == Severity::Error)
                .map(|e| e.message.clone())
        })
    }

    #[test]
    fn coverage_regex_captures_surface() {
        // `captures` returns the stdlib `CaptureMatches` type and lives only in
        // the impl block (NOT the ModuleRegistry handle table), so it is the
        // sharpest regression guard for the inlining fix.
        let source = "import std.text.regex;\n\
                      fn probe(s: string) {\n\
                      \x20   let one = regex.new(\"a\");\n\
                      \x20   let caps = one.captures(s);\n\
                      \x20   one.close();\n\
                      }\n";
        let doc = analyze_repo_rooted("lsp_cov_regex", source);

        assert_eq!(
            hard_type_diagnostic(&doc),
            None,
            "regex.Pattern.captures must not raise a hard type diagnostic",
        );
        assert!(
            surface_hover(&doc, source, "one.captures").contains("CaptureMatches"),
            "hover over captures() should report the CaptureMatches return type",
        );
        let sig = surface_sighelp(&doc, source, "one.captures");
        assert!(
            sig.iter()
                .any(|s| s.contains("captures(input: string)") && s.contains("CaptureMatches")),
            "signature help should label captures(input: string) -> CaptureMatches, got: {sig:?}",
        );
        let completions = surface_completions(&doc, source, "one.captures");
        for expected in [
            "captures",
            "capture",
            "find_all",
            "find_all_submatch",
            "find",
        ] {
            assert!(
                completions.contains(&expected.to_string()),
                "completion on regex.Pattern should include `{expected}`, got: {completions:?}",
            );
        }
        assert!(
            surface_inlays(&doc)
                .iter()
                .any(|l| l.contains("CaptureMatches")),
            "inlay hint for `caps` should show CaptureMatches",
        );
    }

    #[test]
    fn coverage_http_accept_surface() {
        // The HTTP async server's `listener.accept()` carrier surface
        // (NEW-2): receiver type is the imported `net.Listener`.
        let source = "import std.net;\n\
                      fn probe(addr: string) {\n\
                      \x20   let ln = match net.listen(addr) { .Ok(value) => value, .Err(_) => panic(\"network setup failed\"), };\n\
                      \x20   let conn = ln.accept();\n\
                      }\n";
        let doc = analyze_repo_rooted("lsp_cov_net", source);

        assert_eq!(hard_type_diagnostic(&doc), None);
        assert!(
            surface_hover(&doc, source, "ln.accept").contains("Connection"),
            "hover over accept() should report the Connection return type",
        );
        let sig = surface_sighelp(&doc, source, "ln.accept");
        assert!(
            sig.iter()
                .any(|s| s.contains("accept()") && s.contains("Connection")),
            "signature help should label accept() -> Connection, got: {sig:?}",
        );
        let completions = surface_completions(&doc, source, "ln.accept");
        for expected in ["accept", "close"] {
            assert!(
                completions.contains(&expected.to_string()),
                "completion on net.Listener should include `{expected}`, got: {completions:?}",
            );
        }
        assert!(
            surface_inlays(&doc)
                .iter()
                .any(|l| l.contains("Connection")),
            "inlay hint for `conn` should show the Connection type",
        );
    }

    #[test]
    fn coverage_tls_ffi_result_surface() {
        // BUG-NET-3 std/net/tls FFI-result surface (free-function form, the
        // shape the accept fixture exercises: `tls.read(stream, n)`).
        let source = "import std.net.tls;\n\
                      fn probe() {\n\
                      \x20   let stream = tls.connect(\"h\", 443);\n\
                      \x20   let chunk = tls.read(stream, 16);\n\
                      }\n";
        let doc = analyze_repo_rooted("lsp_cov_tls", source);

        assert_eq!(hard_type_diagnostic(&doc), None);
        let hover = surface_hover(&doc, source, "tls.read");
        assert!(
            hover.contains("Result<bytes,") && hover.contains("NetError"),
            "hover over tls.read should report Result<bytes, net.NetError>, got: {hover}",
        );
        let sig = surface_sighelp(&doc, source, "tls.read");
        assert!(
            sig.iter()
                .any(|s| s.contains("read(") && s.contains("Result<bytes,")),
            "signature help should label tls.read -> Result<bytes, net.NetError>, got: {sig:?}",
        );
        assert!(
            surface_inlays(&doc)
                .iter()
                .any(|l| l.contains("Result<bytes,") && l.contains("NetError")),
            "inlay hint for `chunk` should show the Result<bytes, net.NetError> type",
        );
    }

    #[test]
    fn coverage_text_template_surface() {
        let source = "import std.text.template;\n\
                      fn probe(ctx: template.Ctx) {\n\
                      \x20   let out = template.render(\"hi\", ctx);\n\
                      }\n";
        let doc = analyze_repo_rooted("lsp_cov_template", source);

        assert_eq!(hard_type_diagnostic(&doc), None);
        let sig = surface_sighelp(&doc, source, "template.render");
        assert!(
            sig.iter()
                .any(|s| s.contains("render(") && s.contains("Result<string,")),
            "signature help should label template.render -> Result<string, ...>, got: {sig:?}",
        );
        assert!(
            surface_inlays(&doc)
                .iter()
                .any(|l| l.contains("Result<string,")),
            "inlay hint for `out` should show the Result<string, string> type",
        );
    }

    #[test]
    fn coverage_text_unicode_surface() {
        let source = "import std.text.unicode;\n\
                      fn probe(cp: i64) {\n\
                      \x20   let up = unicode.is_upper(cp);\n\
                      }\n";
        let doc = analyze_repo_rooted("lsp_cov_unicode", source);

        assert_eq!(hard_type_diagnostic(&doc), None);
        let sig = surface_sighelp(&doc, source, "unicode.is_upper");
        assert!(
            sig.iter()
                .any(|s| s.contains("is_upper(") && s.contains("bool")),
            "signature help should label unicode.is_upper -> bool, got: {sig:?}",
        );
        assert!(
            surface_inlays(&doc).iter().any(|l| l.contains("bool")),
            "inlay hint for `up` should show the bool type",
        );
    }

    #[test]
    fn coverage_typed_stream_recv_surface() {
        // NEW-7 typed streams: `stream.recv()` over Stream<bytes>. The
        // Stream/Sink surface is builtin, so it needs no std inlining — this
        // test pins the suspending primitive coverage alongside the
        // imported surfaces.
        let source = "fn probe(s: Stream<bytes>) {\n\
                      \x20   let item = s.recv();\n\
                      }\n";
        let doc = analyze_repo_rooted("lsp_cov_stream", source);

        assert_eq!(hard_type_diagnostic(&doc), None);
        assert!(
            surface_hover(&doc, source, "s.recv").contains("Option<bytes>"),
            "hover over s.recv() should report Option<bytes>",
        );
        let sig = surface_sighelp(&doc, source, "s.recv");
        assert!(
            sig.iter()
                .any(|s| s.contains("recv()") && s.contains("Option<bytes>")),
            "signature help should label recv() -> Option<bytes>, got: {sig:?}",
        );
        let completions = surface_completions(&doc, source, "s.recv");
        for expected in ["recv", "try_recv"] {
            assert!(
                completions.contains(&expected.to_string()),
                "completion on Stream<bytes> should include `{expected}`, got: {completions:?}",
            );
        }
        assert!(
            surface_inlays(&doc)
                .iter()
                .any(|l| l.contains("Option<bytes>")),
            "inlay hint for `item` should show Option<bytes>",
        );
    }

    #[test]
    fn coverage_channel_recv_surface() {
        let source = "import std.channel;\n\
                      actor Worker {\n\
                      \x20   receive fn run(unused: i64) {\n\
                      \x20       let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = match channel.new(4) { .Ok(pair) => pair, .Err(error) => panic(error), };\n\
                      \x20       tx.send(\"ping\");\n\
                      \x20       tx.close();\n\
                      \x20       let item = rx.recv();\n\
                      \x20   }\n\
                      }\n";
        let doc = analyze_repo_rooted("lsp_cov_channel_recv", source);

        assert_eq!(hard_type_diagnostic(&doc), None);
        assert!(
            surface_hover(&doc, source, "rx.recv").contains("Option<string>"),
            "hover over rx.recv() should report Option<string>",
        );
        let sig = surface_sighelp(&doc, source, "rx.recv");
        assert!(
            sig.iter()
                .any(|s| s.contains("recv()") && s.contains("Option<string>")),
            "signature help should label recv() -> Option<string>, got: {sig:?}",
        );
        let completions = surface_completions(&doc, source, "rx.recv");
        for expected in ["recv", "try_recv", "close"] {
            assert!(
                completions.contains(&expected.to_string()),
                "completion on channel.Receiver<string> should include `{expected}`, got: {completions:?}",
            );
        }
        assert!(
            surface_inlays(&doc)
                .iter()
                .any(|l| l.contains("Option<string>")),
            "inlay hint for `item` should show Option<string>",
        );
    }

    #[test]
    fn coverage_remote_ask_surface() {
        let source = "type Job {\n\
                      \x20   n: i32,\n\
                      }\n\
                      actor Worker {\n\
                      \x20   receive fn run(job: Job) -> i64 { 21 }\n\
                      }\n\
                      impl ActorMsg for Worker {\n\
                      \x20   type Msg = Job;\n\
                      \x20   type Reply = i64;\n\
                      }\n\
                      actor Caller {\n\
                      \x20   receive fn run(peer: RemotePid<Worker>) {\n\
                      \x20       let result = peer.ask(Job { n: 9 }, 250);\n\
                      \x20   }\n\
                      }\n";
        let doc = analyze_repo_rooted("lsp_cov_remote_ask", source);

        assert_eq!(hard_type_diagnostic(&doc), None);
        let hover = surface_hover(&doc, source, "peer.ask");
        assert!(
            hover.contains("Result<i64,") && hover.contains("ActorError"),
            "hover over RemotePid.ask should report Result<i64, ActorError>, got: {hover}",
        );
        let sig = surface_sighelp(&doc, source, "peer.ask");
        assert!(
            sig.iter()
                .any(|s| s.contains("ask(") && s.contains("Result<Worker::Reply, ActorError<Never, Never>>")),
            "signature help should label ask(...) -> Result<Worker::Reply, ActorError<Never, Never>>, got: {sig:?}",
        );
        let completions = surface_completions(&doc, source, "peer.ask");
        for expected in ["ask", "send"] {
            assert!(
                completions.contains(&expected.to_string()),
                "completion on RemotePid<Worker> should include `{expected}`, got: {completions:?}",
            );
        }
        assert!(
            surface_inlays(&doc)
                .iter()
                .any(|l| l.contains("Result<i64,") && l.contains("ActorError")),
            "inlay hint for `result` should show Result<i64, ActorError>",
        );
    }

    #[test]
    fn coverage_scanner_surface() {
        let source = "import std.io.scanner;\n\
                      fn probe() {\n\
                      \x20   var sc = scanner.from_string(\"one two\");\n\
                      \x20   sc = scanner.with_split(sc, SplitMode.SplitWords);\n\
                      \x20   sc = scanner.scan(sc);\n\
                      \x20   let ok = scanner.has_next(sc);\n\
                      \x20   let token = scanner.text(sc);\n\
                      \x20   let words = scanner.words(\"one two\");\n\
                      }\n";
        let doc = analyze_repo_rooted("lsp_cov_scanner", source);

        assert_eq!(hard_type_diagnostic(&doc), None);
        assert!(
            surface_hover(&doc, source, "scanner.from_string").contains("Scanner"),
            "hover over scanner.from_string should report Scanner",
        );
        let sig = surface_sighelp(&doc, source, "scanner.with_split");
        assert!(
            sig.iter()
                .any(|s| s.contains("with_split(") && s.contains("Scanner")),
            "signature help should label with_split(...) -> Scanner, got: {sig:?}",
        );
        let completions = surface_completions(&doc, source, "scanner.from_string");
        for expected in [
            "from_string",
            "scan",
            "has_next",
            "text",
            "words",
            "SplitWords",
        ] {
            assert!(
                completions.contains(&expected.to_string()),
                "completion on std::io::scanner should include `{expected}`, got: {completions:?}",
            );
        }
        let inlays = surface_inlays(&doc);
        for expected in ["bool", "string", "Vec<string>"] {
            assert!(
                inlays.iter().any(|l| l.contains(expected)),
                "scanner inlays should include {expected}, got: {inlays:?}",
            );
        }
    }

    // ── LSP-vs-compiler resolver agreement (fail-closed on ambiguity) ────

    /// Build a uniquely-named temporary workspace from (relative-path,
    /// content) pairs and return its root. Mirrors the env-mutation-free,
    /// parallel-safe pattern used by the navigation tests.
    pub(in crate::server) fn make_temp_workspace_dir(files: &[(&str, &str)]) -> std::path::PathBuf {
        static COUNTER: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
        let (secs, nanos) = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map_or((0u64, 0u32), |d| (d.as_secs(), d.subsec_nanos()));
        let root = std::env::temp_dir().join(format!(
            "hew-lsp-ambig-{:x}-{:x}-{}-{}",
            secs,
            nanos,
            std::process::id(),
            COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
        ));
        for (rel, content) in files {
            let path = root.join(rel);
            if let Some(parent) = path.parent() {
                std::fs::create_dir_all(parent).expect("create workspace dir");
            }
            std::fs::write(&path, content).expect("write workspace file");
        }
        root
    }

    /// An untitled buffer has no file behind it, and it still gets checked:
    /// the editor offers types, hovers and errors before the first save.
    #[test]
    fn an_untitled_buffer_is_still_type_checked() {
        let uri = Url::parse("untitled:Untitled-1").expect("untitled uri parses");
        let source = "fn main() {\n    let wrong: i32 = \"text\";\n    println(wrong);\n}\n";

        let document = analyze_document(&uri, source, &DashMap::new(), &[]);
        let type_output = document
            .type_output
            .as_ref()
            .expect("an untitled buffer still reaches the checker");
        assert!(
            !type_output.errors.is_empty(),
            "the deliberate mismatch must be reported"
        );
        assert!(
            document
                .diagnostics_by_uri
                .get(&uri)
                .is_some_and(|diagnostics| diagnostics
                    .iter()
                    .any(|d| d.severity == Some(DiagnosticSeverity::ERROR))),
            "the error must be published against the buffer's own uri, got: {:?}",
            document.diagnostics_by_uri
        );
    }

    /// The editor and `hew check` report the same thing for the same file.
    ///
    /// Both sides run the shared driver, so what this pins is that the LSP
    /// hands it the same configuration the CLI does - project discovery from
    /// the manifest, the same module search paths, the same lint levels - and
    /// that the conversion to LSP diagnostics loses no code, position or
    /// severity. The fixture carries a regex literal (the implicit
    /// `std.text.regex` import), a type error and a lint warning so both
    /// severities and a resolved import are in the compared set.
    #[test]
    fn editor_diagnostics_equal_hew_check_for_the_same_file() {
        const SOURCE: &str = "fn probe() -> i32 {\n\
             let unused = 1;\n\
             let pattern = re\"a+\";\n\
             if pattern.is_match(\"aaa\") { 1 } else { 0 }\n\
             }\n\
             \n\
             fn main() {\n\
             let wrong: i32 = \"text\";\n\
             println(probe());\n\
             println(wrong);\n\
             }\n";

        let root = make_temp_workspace_dir(&[
            (
                "hew.toml",
                "[package]\nname = \"fixture\"\nversion = \"0.1.0\"\nedition = \"2026\"\n",
            ),
            ("main.hew", SOURCE),
        ]);
        let path = root.join("main.hew");
        let uri = Url::from_file_path(&path).expect("workspace path is absolute");

        let failure = hew_compile::check_file(
            &path.display().to_string(),
            &hew_compile::FrontendOptions::default(),
        )
        .expect_err("the fixture has a deliberate type error");

        let line_offsets = compute_line_offsets(SOURCE);
        let expected = build_frontend_diagnostics_by_uri(
            &uri,
            SOURCE,
            &line_offsets,
            &failure.diagnostics,
            Some(&failure),
        );
        let published = analyze_document(&uri, SOURCE, &DashMap::new(), &[]).diagnostics_by_uri;

        let identities = |map: &DiagnosticMap| {
            let mut rows: Vec<String> = map
                .iter()
                .flat_map(|(uri, diagnostics)| {
                    diagnostics.iter().map(move |diagnostic| {
                        format!(
                            "{uri:?} {:?} {:?} {:?}",
                            diagnostic.code, diagnostic.range, diagnostic.severity
                        )
                    })
                })
                .collect();
            rows.sort();
            rows
        };

        let expected_rows = identities(&expected);
        assert!(
            !expected_rows.is_empty(),
            "the fixture must produce diagnostics for the comparison to mean anything"
        );
        assert!(
            expected
                .values()
                .flatten()
                .any(|diagnostic| diagnostic.severity == Some(DiagnosticSeverity::ERROR))
                && expected
                    .values()
                    .flatten()
                    .any(|diagnostic| diagnostic.severity == Some(DiagnosticSeverity::WARNING)),
            "the fixture must exercise both severities, got: {expected_rows:?}"
        );
        assert_eq!(
            identities(&published),
            expected_rows,
            "editor diagnostics must match `hew check` for the same file"
        );

        let _ = std::fs::remove_dir_all(&root);
    }

    /// Diagnostics the LSP publishes for `uri` after analyzing `source`.
    fn published_for(
        uri: &Url,
        source: &str,
        extra_pkg_paths: &[std::path::PathBuf],
    ) -> Vec<Diagnostic> {
        let mut document = analyze_document(uri, source, &DashMap::new(), extra_pkg_paths);
        document.diagnostics_by_uri.remove(uri).unwrap_or_default()
    }

    fn messages(diagnostics: &[Diagnostic]) -> Vec<&str> {
        diagnostics.iter().map(|d| d.message.as_str()).collect()
    }

    #[cfg(unix)]
    #[test]
    fn missing_import_diagnostics_preserve_open_symlink_uri() {
        let source = "import missing.widgets;\nfn main() {}\n";
        let root = make_temp_workspace_dir(&[("main.hew", source)]);
        let alias = root.join("open.hew");
        std::os::unix::fs::symlink(root.join("main.hew"), &alias).unwrap();
        let uri = Url::from_file_path(&alias).unwrap();
        let diagnostics = published_for(&uri, source, &[]);
        assert!(
            diagnostics.iter().any(|d| d.message.contains("not found")),
            "the editor must receive the missing import on its open URI: {:?}",
            messages(&diagnostics)
        );
        std::fs::remove_dir_all(root).unwrap();
    }

    /// A dotted import that resolves to more than one module fails closed in
    /// the compiler, so the editor must show that refusal rather than offer
    /// tooling for a module `hew check` will not bind.
    #[test]
    fn ambiguous_import_fails_closed_like_compiler() {
        let lib = "pub fn val() -> i64 { 1 }\n";
        let main_src = "import a.b;\n\nfn main() -> i64 { 0 }\n";
        let root = make_temp_workspace_dir(&[
            ("a/b/b.hew", lib),
            ("a/b.hew", lib),
            ("main.hew", main_src),
        ]);
        let main_uri =
            Url::from_file_path(root.join("main.hew")).expect("workspace path is absolute");

        let diagnostics = published_for(&main_uri, main_src, &[]);
        assert!(
            diagnostics
                .iter()
                .any(|d| d.message.contains("ambiguous") && d.message.contains("a.b")),
            "expected a fail-closed ambiguity diagnostic for `a.b`, got: {:?}",
            messages(&diagnostics)
        );

        let _ = std::fs::remove_dir_all(&root);
    }

    /// Negative control for the rule above: one candidate form is unambiguous
    /// and must analyze clean.
    #[test]
    fn unambiguous_import_resolves_without_failing_closed() {
        let lib = "pub fn val() -> i64 { 1 }\n";
        let main_src = "import a.b;\n\nfn main() -> i64 { 0 }\n";
        let root = make_temp_workspace_dir(&[("a/b.hew", lib), ("main.hew", main_src)]);
        let main_uri =
            Url::from_file_path(root.join("main.hew")).expect("workspace path is absolute");

        let diagnostics = published_for(&main_uri, main_src, &[]);
        assert!(
            !diagnostics.iter().any(|d| d.message.contains("ambiguous")),
            "a single-candidate import must not be flagged ambiguous, got: {:?}",
            messages(&diagnostics)
        );

        let _ = std::fs::remove_dir_all(&root);
    }

    /// String-literal file imports resolve local-only. A file that exists only
    /// under a search root must stay unfound, exactly as `hew check` reports
    /// it, instead of the editor pretending it resolved.
    #[test]
    fn file_import_resolves_local_only_not_from_search_root() {
        let lib = "pub fn val() -> i64 { 1 }\n";
        let main_src = "import \"lib.hew\";\n\nfn main() -> i64 { 0 }\n";
        let local = make_temp_workspace_dir(&[("main.hew", main_src)]);
        let root = make_temp_workspace_dir(&[("lib.hew", lib)]);
        let main_uri =
            Url::from_file_path(local.join("main.hew")).expect("workspace path is absolute");

        let diagnostics = published_for(&main_uri, main_src, std::slice::from_ref(&root));
        assert!(
            diagnostics
                .iter()
                .any(|d| d.message.contains("imported file not found")),
            "a file import missing locally must be reported unfound, got: {:?}",
            messages(&diagnostics)
        );

        let _ = std::fs::remove_dir_all(&local);
        let _ = std::fs::remove_dir_all(&root);
    }

    /// Complement of the rule above: a file import present both locally and
    /// under a search root binds the local file with no ambiguity.
    #[test]
    fn file_import_local_shadow_is_not_ambiguous() {
        let local_lib = "pub fn val() -> i64 { 1 }\n";
        let root_lib = "pub fn val() -> i64 { 2 }\n";
        let main_src = "import \"lib.hew\";\n\nfn main() -> i64 { val() }\n";
        let local = make_temp_workspace_dir(&[("main.hew", main_src), ("lib.hew", local_lib)]);
        let root = make_temp_workspace_dir(&[("lib.hew", root_lib)]);
        let main_uri =
            Url::from_file_path(local.join("main.hew")).expect("workspace path is absolute");

        let diagnostics = published_for(&main_uri, main_src, std::slice::from_ref(&root));
        assert!(
            diagnostics.is_empty(),
            "a local file import shadowing a search-root copy must analyze clean, got: {:?}",
            messages(&diagnostics)
        );

        let _ = std::fs::remove_dir_all(&local);
        let _ = std::fs::remove_dir_all(&root);
    }

    /// `hew.pkgPath` resolves an import the default search roots do not carry,
    /// mirroring `hew check --pkg-path DIR`.
    #[test]
    fn extra_pkg_path_resolves_import_from_added_search_root() {
        let pkg_source = "pub fn widget_fn() -> i64 { 42 }\n";
        let main_src = "import acme.widgets;\nfn main() -> i64 { widgets.widget_fn() }\n";
        let pkg_dir = make_temp_workspace_dir(&[("acme/widgets.hew", pkg_source)]);
        let project_dir = make_temp_workspace_dir(&[("main.hew", main_src)]);
        let main_uri =
            Url::from_file_path(project_dir.join("main.hew")).expect("project path is absolute");

        // Negative control: without the package path there is nowhere to find it.
        let without = published_for(&main_uri, main_src, &[]);
        assert!(
            without.iter().any(|d| d.message.contains("not found")),
            "without a package path the import must not resolve, got: {:?}",
            messages(&without)
        );

        let with = published_for(&main_uri, main_src, std::slice::from_ref(&pkg_dir));
        assert!(
            with.is_empty(),
            "with the package path `acme.widgets` must resolve, got: {:?}",
            messages(&with)
        );

        let _ = std::fs::remove_dir_all(&pkg_dir);
        let _ = std::fs::remove_dir_all(&project_dir);
    }

    /// `import hew.template` resolves against a package path by stripping the
    /// `hew` segment, the same candidate `hew check --pkg-path DIR` tries.
    #[test]
    fn extra_pkg_path_resolves_hew_prefixed_import_with_prefix_stripping() {
        let template_src = "pub fn apply() -> i64 { 1 }\n";
        let main_src = "import hew.template;\nfn main() -> i64 { template.apply() }\n";
        let pkg_dir = make_temp_workspace_dir(&[("template/template.hew", template_src)]);
        let project_dir = make_temp_workspace_dir(&[("main.hew", main_src)]);
        let main_uri =
            Url::from_file_path(project_dir.join("main.hew")).expect("project path is absolute");

        let without = published_for(&main_uri, main_src, &[]);
        assert!(
            without.iter().any(|d| d.message.contains("not found")),
            "without a package path there is no local template tree, got: {:?}",
            messages(&without)
        );

        let with = published_for(&main_uri, main_src, std::slice::from_ref(&pkg_dir));
        assert!(
            with.is_empty(),
            "with the package path `hew.template` must resolve, got: {:?}",
            messages(&with)
        );

        let _ = std::fs::remove_dir_all(&pkg_dir);
        let _ = std::fs::remove_dir_all(&project_dir);
    }

    /// A textbook accumulator loop where every store is read.
    const SEMANTIC_AGREEMENT_SOURCE: &str = "fn sum(n: i64) -> i64 {\nvar total = 0;\nfor i in 0..n {\ntotal = total + i;\n}\ntotal\n}\nfn main() {\nlet _ = sum(3);\n}\n";

    #[test]
    fn lsp_and_build_session_report_the_same_semantic_diagnostics() {
        let uri = Url::parse("file:///session_agreement.hew").unwrap();
        let parse_result = hew_parser::parse(SEMANTIC_AGREEMENT_SOURCE);
        let mut checker =
            hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
        let tco = checker.check_program(&parse_result.program);
        let (hir_diagnostics, module) = collect_hir_diagnostics(&parse_result.program, &tco);
        assert!(hir_diagnostics.is_empty(), "fixture must lower cleanly");

        let build = hew_compile::Session::new(
            hew_compile::SessionTarget::native(),
            hew_compile::DiagnosticPolicy::default(),
        )
        .lower_hir_module(&module, &tco, &[]);
        let lsp = build_semantic_lsp_diagnostics(
            &uri,
            SEMANTIC_AGREEMENT_SOURCE,
            &compute_line_offsets(SEMANTIC_AGREEMENT_SOURCE),
            &parse_result.program,
            &module,
            &tco,
        );
        let lsp_codes = lsp
            .get(&uri)
            .into_iter()
            .flatten()
            .filter_map(|diagnostic| match &diagnostic.code {
                Some(NumberOrString::String(code)) => Some(code.clone()),
                _ => None,
            })
            .collect::<Vec<_>>();
        let build_codes = if build.is_err() {
            vec!["E_SIR_VERIFY".to_string()]
        } else {
            Vec::new()
        };

        assert_eq!(lsp_codes, build_codes);
    }
}
