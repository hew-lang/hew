//! Unit and integration tests for [`crate::server`].

mod code_actions;
mod completion;
mod diagnostics;
mod hierarchy;
mod language_features;
mod navigation;
mod positions;
mod rename;
mod rename_disk;
mod semantic_tokens;
mod v05_fixtures;
mod workspace;

use std::time::{SystemTime, UNIX_EPOCH};

use hew_analysis::references::count_all_references;
use hew_analysis::util::compute_line_offsets;
use hew_analysis::CompletionKind;
use hew_parser::ast::Item;
use hew_types::error::TypeErrorKind;
use hew_types::Checker;
use tower_lsp_server::ls_types::{
    CodeActionContext, CodeActionKind, CodeActionOrCommand, CompletionItemKind, DiagnosticSeverity,
    DocumentSymbol, InlayHintTooltip, InsertTextFormat, OneOf, PartialResultParams, SemanticToken,
    SemanticTokenModifier, SymbolKind, WorkDoneProgressParams,
};

use super::analysis::tests::make_temp_workspace_dir;
use super::analysis::{analyze_document, diagnostic_data};
use super::convert::analysis_symbol_kind_to_lsp;
use super::handlers::language_features::{
    code_action_response, lsp_code_actions_for_diagnostic, lsp_inlay_hint_from_analysis,
    lsp_signature_help_from_analysis, remove_unused_imports_kind,
};
use super::handlers::navigation::rename_error_to_jsonrpc;
use super::handlers::text_sync::build_initialize_result_from_caps_json;
use super::handlers::workspace::extract_run_test_name;
use super::navigation::{
    build_prepare_rename_response, build_workspace_edit, plan_workspace_rename,
};
use super::test_runner::build_run_test_invocation;
use super::testing::TestFailureBatch;
use super::workspace::{build_code_lenses, collect_workspace_symbols};
use super::*;

struct TestDir {
    path: PathBuf,
}

impl TestDir {
    fn new(label: &str) -> Self {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join(".test-artifacts")
            .join(format!("{label}-{}-{unique}", std::process::id()));
        std::fs::create_dir_all(&path).unwrap();
        Self { path }
    }

    fn path(&self) -> &Path {
        &self.path
    }
}

impl Drop for TestDir {
    fn drop(&mut self) {
        let _ = std::fs::remove_dir_all(&self.path);
    }
}

fn make_doc(source: &str) -> DocumentState {
    let parse_result = hew_parser::parse(source);
    let lo = compute_line_offsets(source);
    DocumentState {
        expanded_program: None,
        source: source.to_string(),
        line_offsets: lo,
        parse_result,
        type_output: None,
        dependency_uris: None,
        diagnostics_by_uri: HashMap::new(),
    }
}

/// Diagnostics the LSP publishes for `uri`, through the shared frontend
/// driver, which is the path a real client exercises.
fn analyzed_diagnostics(uri: &Url, source: &str) -> Vec<Diagnostic> {
    let mut document = analyze_document(uri, source, &DashMap::new(), &[]);
    document.diagnostics_by_uri.remove(uri).unwrap_or_default()
}

fn make_typed_doc(source: &str) -> DocumentState {
    let parse_result = hew_parser::parse(source);
    let lo = compute_line_offsets(source);
    let mut checker = Checker::new(hew_types::module_registry::ModuleRegistry::new(vec![]));
    let type_output = checker.check_program(&parse_result.program);
    DocumentState {
        expanded_program: None,
        source: source.to_string(),
        line_offsets: lo,
        parse_result,
        type_output: Some(type_output),
        dependency_uris: None,
        diagnostics_by_uri: HashMap::new(),
    }
}

/// Returns a file URI for `posix_path` that is valid on the current
/// platform. On Windows, the POSIX-shaped fixture path is placed on `C:`
/// before the platform-aware path conversion.
fn make_test_uri(posix_path: &str) -> Url {
    #[cfg(windows)]
    let path = PathBuf::from(format!("C:{posix_path}"));
    #[cfg(not(windows))]
    let path = PathBuf::from(posix_path);
    Url::from_checked_file_path(path).expect("test path should be an absolute file path")
}

/// Whether analyzing `uri` left an error anywhere it published.
///
/// The frontend stops at the first import it cannot use and locates the
/// diagnostic in the file that carries the problem: the importer for a
/// missing module, the imported file for one that does not parse. Both
/// land in the analyzing document's own map.
fn has_unresolved_import(documents: &DashMap<Url, DocumentState>, uri: &Url) -> bool {
    documents.get(uri).is_some_and(|document| {
        document
            .diagnostics_by_uri
            .values()
            .flatten()
            .any(|diagnostic| diagnostic.severity == Some(DiagnosticSeverity::ERROR))
    })
}

/// Returns the `ImportDecl`s extracted from the parsed program, mirroring
/// the extraction done in `goto_definition` before the cross-file search.
fn collect_imports(source: &str) -> Vec<hew_parser::ast::ImportDecl> {
    let pr = hew_parser::parse(source);
    pr.program
        .items
        .into_iter()
        .filter_map(|(item, _)| match item {
            Item::Import(i) => Some(i),
            _ => None,
        })
        .collect()
}

/// Error-severity diagnostics the LSP publishes for `uri`; lints and
/// warnings are not what these module-resolution tests are about.
fn published_errors(document: &DocumentState, uri: &Url) -> Vec<String> {
    document
        .diagnostics_by_uri
        .get(uri)
        .into_iter()
        .flatten()
        .filter(|diagnostic| diagnostic.severity == Some(DiagnosticSeverity::ERROR))
        .map(|diagnostic| diagnostic.message.clone())
        .collect()
}
