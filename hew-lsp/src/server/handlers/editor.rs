//! Editor providers over the shared, checked document snapshot.

use std::collections::{HashMap, HashSet};
use tower_lsp_server::jsonrpc::{Error, Result};
use tower_lsp_server::ls_types::request::{
    GotoImplementationParams, GotoImplementationResponse, GotoTypeDefinitionParams,
    GotoTypeDefinitionResponse,
};
use tower_lsp_server::ls_types::{
    Diagnostic, DocumentDiagnosticParams, DocumentDiagnosticReport, DocumentDiagnosticReportResult,
    DocumentHighlight, DocumentHighlightParams, FullDocumentDiagnosticReport,
    GotoDefinitionResponse, RelatedFullDocumentDiagnosticReport,
    RelatedUnchangedDocumentDiagnosticReport, SelectionRange, SelectionRangeParams,
    UnchangedDocumentDiagnosticReport, Uri,
};

use super::super::uri::FileUriExt;
use super::super::{offset_range_to_lsp, position_to_offset, HewLanguageServer};

pub(crate) fn type_definition(
    server: &HewLanguageServer,
    params: &GotoTypeDefinitionParams,
) -> Option<GotoTypeDefinitionResponse> {
    let position = &params.text_document_position_params;
    let uri = &position.text_document.uri;
    let doc = server.documents.get(uri)?;
    let offset = position_to_offset(&doc.source, &doc.line_offsets, position.position);
    let target = hew_analysis::editor::type_definition(
        &doc.source,
        &doc.parse_result,
        doc.type_output.as_ref()?,
        offset,
    )?;
    super::super::navigation::declaration_location(uri, &doc, &target, &server.documents)
        .map(GotoDefinitionResponse::Scalar)
}

pub(crate) fn implementations(
    server: &HewLanguageServer,
    params: &GotoImplementationParams,
) -> Option<GotoImplementationResponse> {
    let position = &params.text_document_position_params;
    let uri = &position.text_document.uri;
    let doc = server.documents.get(uri)?;
    let offset = position_to_offset(&doc.source, &doc.line_offsets, position.position);
    let targets = hew_analysis::editor::implementations(
        &doc.source,
        &doc.parse_result,
        doc.type_output.as_ref()?,
        offset,
    );
    let mut locations: Vec<_> = targets
        .iter()
        .filter_map(|target| {
            super::super::navigation::declaration_location(uri, &doc, target, &server.documents)
        })
        .collect();
    locations.sort_by(|a, b| {
        a.uri
            .as_str()
            .cmp(b.uri.as_str())
            .then(a.range.start.cmp(&b.range.start))
            .then(a.range.end.cmp(&b.range.end))
    });
    locations.dedup();
    (!locations.is_empty()).then_some(GotoDefinitionResponse::Array(locations))
}

pub(crate) fn document_highlights(
    server: &HewLanguageServer,
    params: &DocumentHighlightParams,
) -> Option<Vec<DocumentHighlight>> {
    let position = &params.text_document_position_params;
    let doc = server.documents.get(&position.text_document.uri)?;
    let offset = position_to_offset(&doc.source, &doc.line_offsets, position.position);
    let spans = hew_analysis::editor::document_highlights(
        &doc.source,
        &doc.parse_result,
        doc.type_output.as_ref()?,
        offset,
    );
    let highlights: Vec<_> = spans
        .into_iter()
        .map(|span| DocumentHighlight {
            range: offset_range_to_lsp(&doc.source, &doc.line_offsets, span.start, span.end),
            kind: None,
        })
        .collect();
    (!highlights.is_empty()).then_some(highlights)
}

pub(crate) fn selection_ranges(
    server: &HewLanguageServer,
    params: &SelectionRangeParams,
) -> Option<Vec<SelectionRange>> {
    let doc = server.documents.get(&params.text_document.uri)?;
    Some(
        params
            .positions
            .iter()
            .filter_map(|position| {
                let offset = position_to_offset(&doc.source, &doc.line_offsets, *position);
                hew_analysis::selection::selection_ranges(&doc.source, &doc.parse_result, offset)
                    .into_iter()
                    .rev()
                    .fold(None, |parent, span| {
                        Some(SelectionRange {
                            range: offset_range_to_lsp(
                                &doc.source,
                                &doc.line_offsets,
                                span.start,
                                span.end,
                            ),
                            parent: parent.map(Box::new),
                        })
                    })
            })
            .collect(),
    )
}

/// One retained report per requested document; result IDs are opaque and never
/// reused for a different diagnostic set during this server session.
#[derive(Debug, Default)]
pub(in super::super) struct DiagnosticReports {
    next_id: u64,
    reports: HashMap<Uri, (String, Vec<Diagnostic>)>,
}

impl DiagnosticReports {
    pub(super) fn remove(&mut self, uri: &Uri) {
        self.reports.remove(uri);
    }

    fn report(
        &mut self,
        uri: &Uri,
        items: Vec<Diagnostic>,
        previous: Option<&str>,
    ) -> DocumentDiagnosticReportResult {
        let unchanged = self.reports.get(uri).is_some_and(|(_, old)| *old == items);
        if !unchanged {
            self.next_id += 1;
            self.reports
                .insert(uri.clone(), (self.next_id.to_string(), items));
        }
        let (result_id, items) = &self.reports[uri];
        let report = if previous == Some(result_id.as_str()) {
            DocumentDiagnosticReport::Unchanged(RelatedUnchangedDocumentDiagnosticReport {
                related_documents: None,
                unchanged_document_diagnostic_report: UnchangedDocumentDiagnosticReport {
                    result_id: result_id.clone(),
                },
            })
        } else {
            DocumentDiagnosticReport::Full(RelatedFullDocumentDiagnosticReport {
                related_documents: None,
                full_document_diagnostic_report: FullDocumentDiagnosticReport {
                    result_id: Some(result_id.clone()),
                    items: items.clone(),
                },
            })
        };
        DocumentDiagnosticReportResult::Report(report)
    }
}

pub(crate) async fn diagnostic(
    server: &HewLanguageServer,
    params: &DocumentDiagnosticParams,
) -> Result<DocumentDiagnosticReportResult> {
    // A pull can follow didChange before the debounce expires. Wait for the
    // whole dependency refresh, not merely insertion of the edited document.
    let _analysis = loop {
        let analysis = server.analysis_jobs.lock().await;
        let pending = server.analysis_versions.iter().any(|entry| {
            server
                .completed_analysis_versions
                .get(entry.key())
                .is_none_or(|completed| *completed != *entry.value())
        });
        if !pending {
            break analysis;
        }
        drop(analysis);
        tokio::time::sleep(std::time::Duration::from_millis(10)).await;
    };
    let uri = &params.text_document.uri;
    let mut items = super::super::analysis::collect_published_diagnostics(
        &server.documents,
        HashSet::from([uri.clone()]),
    )
    .into_iter()
    .find(|(target, _)| target == uri)
    .map_or_else(Vec::new, |(_, items)| items);
    if !server.documents.contains_key(uri) {
        // Closed files can be pulled too. Analyze them against current open
        // dependency buffers without retaining them as open documents.
        let source = uri
            .to_checked_file_path()
            .and_then(|path| std::fs::read_to_string(path).ok())
            .ok_or_else(|| Error::invalid_params("document source is unavailable"))?;
        let document = super::super::analysis::analyze_document(
            uri,
            &source,
            &server.documents,
            &server.extra_pkg_paths,
        );
        items = document
            .diagnostics_by_uri
            .get(uri)
            .cloned()
            .unwrap_or_default();
        super::super::analysis::sort_and_dedup_diagnostics(&mut items);
    }
    if let Some(failures) = server.test_diagnostics.get(uri) {
        items.extend(failures.iter().cloned());
    }
    server
        .diagnostic_reports
        .write()
        .map_err(|_| super::super::internal_error("diagnostic report lock poisoned"))
        .map(|mut reports| reports.report(uri, items, params.previous_result_id.as_deref()))
}
