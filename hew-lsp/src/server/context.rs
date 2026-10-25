//! Explicit generated sources share the ordinary open-document frontend.
//! They are analysis inputs, never authored documents or filesystem writes.

use std::collections::{HashMap, HashSet};

use serde::Deserialize;
use tower_lsp_server::jsonrpc::{Error, Result};
use tower_lsp_server::ls_types::{TextDocumentIdentifier, Uri};

use super::analysis::{
    analyze_document, analyze_document_with_batch, collect_published_diagnostics,
};
use super::uri::{source_file_key, FileUriExt};
use super::HewLanguageServer;

#[derive(Clone, Debug, Deserialize)]
pub struct ContextSource {
    uri: Uri,
    text: String,
}

/// Replace one entry document's generated context; an empty list removes it.
#[derive(Debug, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct AnalysisContextParams {
    text_document: TextDocumentIdentifier,
    sources: Vec<ContextSource>,
}

pub(super) type AnalysisContexts = HashMap<Uri, Vec<ContextSource>>;
type ContextSources = HashMap<Uri, (Uri, String)>;

impl HewLanguageServer {
    pub(super) fn context_source(&self, uri: &Uri) -> Option<(Uri, String)> {
        let key = source_file_key(uri);
        self.analysis_contexts
            .read()
            .ok()?
            .values()
            .flatten()
            .find(|source| source_file_key(&source.uri) == key)
            .map(|source| (source.uri.clone(), source.text.clone()))
    }

    pub(super) fn suspend_context_source(&self, uri: &Uri) {
        if let Some((generated, _)) = self.context_source(uri) {
            if generated != *uri {
                self.documents.remove(&generated);
            }
        }
    }

    fn replace_context(&self, params: AnalysisContextParams) -> Result<(Vec<Uri>, ContextSources)> {
        let mut contexts = self
            .analysis_contexts
            .write()
            .map_err(|_| Error::internal_error())?;
        let mut replacement = contexts.clone();
        if params.sources.is_empty() {
            replacement.remove(&params.text_document.uri);
        } else {
            replacement.insert(params.text_document.uri, params.sources);
        }
        let mut sources = HashMap::new();
        for source in replacement.values().flatten() {
            let key = source_file_key(&source.uri);
            if let Some((_, text)) = sources.insert(key, (source.uri.clone(), source.text.clone()))
            {
                if text != source.text {
                    return Err(Error::invalid_params(
                        "generated contexts disagree on source content",
                    ));
                }
            }
        }
        let previous: Vec<_> = contexts
            .values()
            .flatten()
            .map(|source| source.uri.clone())
            .collect();
        *contexts = replacement;
        Ok((previous, sources))
    }

    /// Set SDK-authored source overlays, then refresh diagnostics and navigation.
    /// Open editor buffers, including edits awaiting debounce, always win.
    ///
    /// # Errors
    /// Rejects non-file source identities and conflicting generated contexts.
    pub async fn analysis_context(&self, params: AnalysisContextParams) -> Result<()> {
        if params.text_document.uri.to_checked_file_path().is_none()
            || params
                .sources
                .iter()
                .any(|source| source.uri.to_checked_file_path().is_none())
        {
            return Err(Error::invalid_params(
                "analysis context sources require file URIs",
            ));
        }
        let _analysis = self.analysis_jobs.lock().await;
        let (previous, sources) = self.replace_context(params)?;
        let open = self
            .open_documents
            .read()
            .map_err(|_| Error::internal_error())?
            .clone();
        let authored: HashSet<_> = open.keys().map(source_file_key).collect();
        let mut publish: HashSet<_> = previous.iter().cloned().collect();
        for uri in previous {
            if !open.contains_key(&uri) {
                self.documents.remove(&uri);
            }
        }
        for (key, (uri, text)) in sources {
            publish.insert(uri.clone());
            if !authored.contains(&key) {
                self.documents.insert(
                    uri.clone(),
                    analyze_document(&uri, &text, &self.documents, &self.extra_pkg_paths),
                );
            }
        }
        // One immutable frontend snapshot includes every pending authored edit.
        // Never pair new source text with an older document's parsed facts.
        let mut sources = hew_compile::SourceSet::new();
        for entry in self.documents.iter() {
            if let Some(path) = entry.key().to_checked_file_path() {
                sources.insert(path.into_owned(), entry.source.clone());
            }
        }
        for (uri, document) in &open {
            if let Some(path) = uri.to_checked_file_path() {
                sources.insert(path.into_owned(), document.source.clone());
            }
        }
        let mut batch = hew_compile::SourceAnalysisBatch::new(hew_compile::FrontendOptions {
            pkg_path: self.extra_pkg_paths.first().cloned(),
            documents: sources.clone(),
            ..hew_compile::FrontendOptions::default()
        });
        for (uri, document) in &open {
            let version = {
                let mut version = self.analysis_versions.entry(uri.clone()).or_insert(0);
                *version += 1;
                *version
            };
            let state = analyze_document_with_batch(
                uri,
                &document.source,
                &self.documents,
                &self.extra_pkg_paths,
                Some((&mut batch, &sources)),
            );
            publish.extend(state.diagnostics_by_uri.keys().cloned());
            publish.insert(uri.clone());
            self.documents.insert(uri.clone(), state);
            self.completed_analysis_versions
                .insert(uri.clone(), version);
        }
        for (uri, diagnostics) in super::with_test_diagnostics(
            collect_published_diagnostics(&self.documents, publish),
            &self.test_diagnostics,
        ) {
            self.client
                .publish_diagnostics(uri, diagnostics, None)
                .await;
        }
        Ok(())
    }
}
