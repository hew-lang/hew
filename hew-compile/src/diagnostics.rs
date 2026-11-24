//! Frontend diagnostic types, warning policy, and admitted source attribution.

use std::collections::HashMap;
use std::fmt;
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use hew_parser::ast::Program;

use crate::frontend_options::FrontendOptions;
use crate::source_set::{read_source, SourceSet};

#[derive(Debug, Clone)]
pub enum FrontendDiagnosticKind {
    Message(FrontendMessageDiagnostic),
    Parse(hew_parser::ParseError),
    Type(hew_types::TypeError),
    Hir(hew_hir::HirDiagnostic),
}

/// A frontend failure that has no parser/type/HIR payload but still needs a
/// stable machine-readable identity (for example package-module resolution).
///
/// `span`/`source` are `None` for the ordinary message-only sites (the code
/// carries the identity, but there is nowhere in the source to point at —
/// `E_IMPORT_AMBIGUOUS` spans two files, `E_PACKAGE_ROOT_MISSING` names a
/// missing file on disk). A site that *does* have a location — today just
/// `E_MODULE_NOT_FOUND`, pointing at the offending `import` — sets both so
/// the renderers can locate it instead of falling back to a zero span.
#[derive(Debug, Clone)]
pub struct FrontendMessageDiagnostic {
    pub code: String,
    pub message: String,
    pub span: Option<Range<usize>>,
    pub source: Option<Arc<str>>,
    /// Secondary locations, each with its own file and span — an import
    /// cycle's remaining edges, one per module on the path. Empty for every
    /// other message-only site.
    pub notes: Vec<FrontendMessageNote>,
    /// `= help:` lines rendered after the primary location and its notes.
    pub help: Vec<String>,
}

/// One secondary location on a [`FrontendMessageDiagnostic`] that points into
/// a *different* file than the primary span — the shape `hew-cli`'s
/// diagnostic renderer already expects for a primary/note split, but that
/// [`FrontendMessageDiagnostic`] had no way to carry until the import-cycle
/// diagnostic needed one note per remaining cycle edge.
#[derive(Debug, Clone)]
pub struct FrontendMessageNote {
    pub message: String,
    pub span: Range<usize>,
    pub source: Arc<str>,
    pub filename: String,
}

#[derive(Debug, Clone)]
pub struct FrontendDiagnostic {
    pub source: Option<Arc<str>>,
    pub filename: Option<String>,
    /// Per-note source text and filename when a secondary span belongs to a
    /// different module than the primary diagnostic.
    pub note_sources: Vec<Option<(Arc<str>, String)>>,
    pub kind: FrontendDiagnosticKind,
}

impl FrontendDiagnostic {
    pub(crate) fn message(message: impl Into<String>) -> Self {
        Self {
            source: None,
            filename: None,
            note_sources: Vec::new(),
            kind: FrontendDiagnosticKind::Message(FrontendMessageDiagnostic {
                code: "E_MESSAGE".to_string(),
                message: message.into(),
                span: None,
                source: None,
                notes: Vec::new(),
                help: Vec::new(),
            }),
        }
    }

    pub(crate) fn coded_message(code: &str, message: impl Into<String>) -> Self {
        Self {
            source: None,
            filename: None,
            note_sources: Vec::new(),
            kind: FrontendDiagnosticKind::Message(FrontendMessageDiagnostic {
                code: code.to_string(),
                message: message.into(),
                span: None,
                source: None,
                notes: Vec::new(),
                help: Vec::new(),
            }),
        }
    }

    /// A coded message that also locates itself in source — the one shape
    /// [`FrontendMessageDiagnostic`] needs to render like a `Parse`/`Type`
    /// diagnostic instead of a zero-span message. `filename` lands on the
    /// existing outer field (the same one `parse`/`type_` already populate)
    /// rather than a new one, so the renderers' `(source, filename)` pattern
    /// keeps working unchanged. See [`FrontendFailure::coded_message_at`],
    /// its only caller.
    pub(crate) fn coded_message_at(
        code: &str,
        message: impl Into<String>,
        span: Range<usize>,
        source: impl Into<Arc<str>>,
        filename: &str,
    ) -> Self {
        Self {
            source: None,
            filename: Some(filename.to_string()),
            note_sources: Vec::new(),
            kind: FrontendDiagnosticKind::Message(FrontendMessageDiagnostic {
                code: code.to_string(),
                message: message.into(),
                span: Some(span),
                source: Some(source.into()),
                notes: Vec::new(),
                help: Vec::new(),
            }),
        }
    }

    /// [`Self::coded_message_at`], with secondary same-diagnostic locations
    /// (each carrying its own file) and trailing `= help:` lines. Used only by
    /// the import-cycle diagnostic, whose remaining edges each live in a
    /// different module's source file.
    pub(crate) fn coded_message_with_notes(
        code: &str,
        message: impl Into<String>,
        span: Range<usize>,
        source: impl Into<Arc<str>>,
        filename: &str,
        notes: Vec<FrontendMessageNote>,
        help: Vec<String>,
    ) -> Self {
        Self {
            source: None,
            filename: Some(filename.to_string()),
            note_sources: Vec::new(),
            kind: FrontendDiagnosticKind::Message(FrontendMessageDiagnostic {
                code: code.to_string(),
                message: message.into(),
                span: Some(span),
                source: Some(source.into()),
                notes,
                help,
            }),
        }
    }

    pub(crate) fn parse(
        source: impl Into<Arc<str>>,
        filename: &str,
        diagnostic: hew_parser::ParseError,
    ) -> Self {
        Self {
            source: Some(source.into()),
            filename: Some(filename.to_string()),
            note_sources: Vec::new(),
            kind: FrontendDiagnosticKind::Parse(diagnostic),
        }
    }

    pub(crate) fn type_(
        source: Arc<str>,
        filename: &str,
        diagnostic: hew_types::TypeError,
        module_source_map: &SourceSnapshot,
    ) -> Self {
        let note_sources = diagnostic
            .notes
            .iter()
            .map(|(_, _, source_module)| {
                source_module
                    .as_deref()
                    .and_then(|module| module_source_map.source(module))
            })
            .collect();
        Self {
            source: Some(source),
            filename: Some(filename.to_string()),
            note_sources,
            kind: FrontendDiagnosticKind::Type(diagnostic),
        }
    }

    pub(crate) fn hir(
        source: Option<Arc<str>>,
        filename: Option<&str>,
        diagnostic: hew_hir::HirDiagnostic,
    ) -> Self {
        Self {
            source,
            filename: filename.map(str::to_string),
            note_sources: Vec::new(),
            kind: FrontendDiagnosticKind::Hir(diagnostic),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FrontendFailure {
    pub message: String,
    pub diagnostics: Vec<FrontendDiagnostic>,
    /// Inputs observed before this frontend request stopped.
    pub inputs: Vec<hew_types::compile_time::CompilationInput>,
}

impl FrontendFailure {
    pub(crate) fn new(message: impl Into<String>, diagnostics: Vec<FrontendDiagnostic>) -> Self {
        Self {
            message: message.into(),
            diagnostics,
            inputs: Vec::new(),
        }
    }

    pub(crate) fn message_only(message: impl Into<String>) -> Self {
        Self::new(message, Vec::new())
    }

    pub(crate) fn coded_message(code: &str, message: impl Into<String>) -> Self {
        let message = message.into();
        Self::new(
            message.clone(),
            vec![FrontendDiagnostic::coded_message(code, message)],
        )
    }

    /// [`FrontendFailure::coded_message`], but the diagnostic also carries a
    /// span and the source it indexes into, so a renderer can point at it
    /// instead of showing a bare message. Used only by `E_MODULE_NOT_FOUND`,
    /// which can locate the offending `import` statement.
    pub(crate) fn coded_message_at(
        code: &str,
        message: impl Into<String>,
        span: Range<usize>,
        source: &str,
        filename: &str,
    ) -> Self {
        let message = message.into();
        Self::new(
            message.clone(),
            vec![FrontendDiagnostic::coded_message_at(
                code, message, span, source, filename,
            )],
        )
    }

    /// [`Self::coded_message_at`], with secondary cross-file locations and
    /// help lines. See [`FrontendDiagnostic::coded_message_with_notes`].
    pub(crate) fn coded_message_with_notes(
        code: &str,
        message: impl Into<String>,
        span: Range<usize>,
        source: &str,
        filename: &str,
        notes: Vec<FrontendMessageNote>,
        help: Vec<String>,
    ) -> Self {
        let message = message.into();
        Self::new(
            message.clone(),
            vec![FrontendDiagnostic::coded_message_with_notes(
                code, message, span, source, filename, notes, help,
            )],
        )
    }
}

impl fmt::Display for FrontendFailure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.message.fmt(f)
    }
}

impl std::error::Error for FrontendFailure {}

pub(crate) fn is_warning_diagnostic(d: &FrontendDiagnostic) -> bool {
    match &d.kind {
        FrontendDiagnosticKind::Type(e) => e.severity == hew_types::error::Severity::Warning,
        FrontendDiagnosticKind::Parse(e) => e.severity == hew_parser::Severity::Warning,
        FrontendDiagnosticKind::Message(_) | FrontendDiagnosticKind::Hir(_) => false,
    }
}

/// Compare source paths across canonical and editor-provided spellings.
#[must_use]
pub fn paths_name_same_file(left: &Path, right: &Path) -> bool {
    left == right
        || match (std::fs::canonicalize(left), std::fs::canonicalize(right)) {
            (Ok(left), Ok(right)) => left == right,
            _ => false,
        }
}

pub(crate) fn configured_stdlib_roots(options: &FrontendOptions) -> Vec<PathBuf> {
    options
        .std_roots()
        .into_iter()
        .map(|root| root.join("std"))
        .collect()
}

pub(crate) fn path_is_below(path: &Path, root: &Path) -> bool {
    match (std::fs::canonicalize(path), std::fs::canonicalize(root)) {
        (Ok(path), Ok(root)) => path.starts_with(root),
        _ => path
            .strip_prefix(".")
            .unwrap_or(path)
            .starts_with(root.strip_prefix(".").unwrap_or(root)),
    }
}

/// Remove diagnostics owned by an imported standard-library source before a
/// user-facing pipeline returns them. Users cannot act on compiler-shipped
/// implementation sites. A direct check of a file below `std/` deliberately
/// retains its diagnostics so the stdlib source gate remains authoritative.
pub(crate) fn retain_user_facing_diagnostics(
    root_filename: &str,
    stdlib_roots: &[PathBuf],
    diagnostics: &mut Vec<FrontendDiagnostic>,
) {
    diagnostics
        .retain(|diagnostic| !is_stdlib_owned_diagnostic(root_filename, stdlib_roots, diagnostic));
}

/// Whether a diagnostic belongs to an imported standard-library source rather
/// than the file being checked.
pub(crate) fn is_stdlib_owned_diagnostic(
    root_filename: &str,
    stdlib_roots: &[PathBuf],
    diagnostic: &FrontendDiagnostic,
) -> bool {
    let Some(filename) = diagnostic.filename.as_deref() else {
        return false;
    };
    let diagnostic_path = Path::new(filename);
    !paths_name_same_file(Path::new(root_filename), diagnostic_path)
        && stdlib_roots
            .iter()
            .any(|stdlib_root| path_is_below(diagnostic_path, stdlib_root))
}

/// If `options.warnings_as_errors` is set and `diagnostics` contains any
/// warning-severity entry, return a `FrontendFailure` that includes all
/// accumulated diagnostics.  Otherwise return `Ok(())`.
///
/// Call this in the success arm of every top-level pipeline function
/// (`check_file`, `check_program`) so the behaviour is uniform across all
/// public entry points.
pub(crate) fn fail_on_warning_diagnostics(
    diagnostics: Vec<FrontendDiagnostic>,
    options: &FrontendOptions,
) -> Result<Vec<FrontendDiagnostic>, FrontendFailure> {
    if options.warnings_as_errors && diagnostics.iter().any(is_warning_diagnostic) {
        return Err(FrontendFailure::new(
            "warnings treated as errors",
            diagnostics,
        ));
    }
    Ok(diagnostics)
}

#[derive(Debug, Clone, Default)]
pub struct CheckOutput {
    pub inputs: Vec<hew_types::compile_time::CompilationInput>,
    pub diagnostics: Vec<FrontendDiagnostic>,
    /// Diagnostic-only stack-allocation hints emitted by the checker's
    /// escape-analysis pass. Surfaced behind `hew check --show-stack-hints`.
    /// Empty when type-checking failed before the walker ran.
    pub stack_hints: Vec<hew_types::check::StackHint>,
    /// Source content of the checked file, used for line/column mapping in
    /// `--explain-cow` output. Empty when type-checking is skipped.
    /// Source text of the checked file, retained so the CLI can render
    /// `--show-stack-hints` / `--explain-cow` lines with `file:line:col` attribution.
    /// Empty when the input could not be loaded.
    pub source: String,
}

/// Source text and routing from one frontend admission, shared by diagnostic stages.
#[derive(Debug)]
pub struct SourceSnapshot {
    pub(crate) root_source: Arc<str>,
    pub(crate) root_filename: String,
    pub(crate) modules: HashMap<String, (Arc<str>, String)>,
    pub(crate) documents: SourceSet,
}

impl SourceSnapshot {
    /// Input fingerprints observed in this admission, including failed reads.
    #[must_use]
    pub fn inputs(&self) -> Vec<hew_types::compile_time::CompilationInput> {
        self.documents.inputs()
    }

    /// The checked root text and its diagnostic label.
    #[must_use]
    pub fn root(&self) -> (&str, &str) {
        (&self.root_source, &self.root_filename)
    }

    /// Authored module and per-file aliases, all referring to admitted text.
    #[must_use]
    pub fn modules(&self) -> &HashMap<String, (Arc<str>, String)> {
        &self.modules
    }

    pub(crate) fn source(&self, origin: &str) -> Option<(Arc<str>, String)> {
        self.modules.get(origin).cloned().or_else(|| {
            self.documents
                .admitted_diagnostic_text(Path::new(origin))
                .map(|source| (source, display_path(Path::new(origin))))
        })
    }
}

#[must_use]
pub fn line_map_from_source(source: &str) -> Vec<usize> {
    let mut map = vec![0usize];
    let bytes = source.as_bytes();
    for (i, &byte) in bytes.iter().enumerate() {
        if byte == b'\n' {
            map.push(i + 1);
        }
    }
    map
}

pub(crate) fn merge_prior_diagnostics(
    mut prior: Vec<FrontendDiagnostic>,
    mut failure: FrontendFailure,
) -> FrontendFailure {
    prior.extend(failure.diagnostics);
    failure.diagnostics = prior;
    failure
}

/// Strip Windows' extended-length verbatim prefix (`\\?\`, or `\\?\UNC\` for
/// a UNC share) from a path's rendered form (#3416).
///
/// `Path::canonicalize()` returns this form on Windows so a map key dedupes
/// reliably against symlinks and relative spellings, but a user never typed
/// it and a diagnostic must not show it. This only ever changes the display
/// string a caller renders; the canonical path itself remains the identity
/// key everywhere it is used for routing or lookup.
pub(crate) fn display_path(path: &Path) -> String {
    let text = path.display().to_string();
    text.strip_prefix(r"\\?\UNC\")
        .map(|rest| format!(r"\\{rest}"))
        .or_else(|| text.strip_prefix(r"\\?\").map(str::to_string))
        .unwrap_or(text)
}

pub(crate) fn build_module_source_map(
    program: &Program,
    root_source: &str,
    root_filename: &str,
    documents: &SourceSet,
) -> SourceSnapshot {
    let root_source = documents
        .read_text(Path::new(root_filename))
        .unwrap_or_else(|_| Arc::from(root_source));
    let mut snapshot = SourceSnapshot {
        root_source,
        root_filename: root_filename.to_string(),
        modules: HashMap::new(),
        documents: documents.clone(),
    };
    let Some(ref module_graph) = program.module_graph else {
        return snapshot;
    };

    let map = &mut snapshot.modules;
    for mod_id in &module_graph.topo_order {
        if *mod_id == module_graph.root {
            continue;
        }
        let Some(module) = module_graph.modules.get(mod_id) else {
            continue;
        };
        let Some(path) = module.source_paths.first() else {
            // A prelude module attached without a search path carries its
            // compiled-in source instead.
            if let [std, leaf] = mod_id.segments.as_slice() {
                if let Some((_, text)) = hew_types::stdlib_authority::IMPLICIT_PRELUDE_STD_SOURCES
                    .iter()
                    .find(|(name, _)| std.as_str() == "std" && *name == leaf.as_str())
                {
                    map.insert(
                        mod_id.dotted(),
                        (Arc::from(*text), format!("std/{leaf}.hew")),
                    );
                }
            }
            continue;
        };
        if let Ok(text) = read_source(documents, path) {
            map.insert(mod_id.dotted(), (text, display_path(path)));
        }
        // Per-file routing entries (rc1-F1 stage C): a directory module's
        // item spans are file-relative offsets, so the checker routes a
        // diagnostic on a peer-file item by the file's own path token. Every
        // source file of every module resolves under that token. The map key
        // stays the canonical spelling (identity, joined against elsewhere);
        // only the rendered label is stripped.
        for path in &module.source_paths {
            let key = path.display().to_string();
            if map.contains_key(&key) {
                continue;
            }
            if let Ok(text) = read_source(documents, path) {
                map.insert(key, (text, display_path(path)));
            }
        }
    }
    snapshot
}

pub(crate) fn macro_type_origin(
    program: &Program,
    definitions: &hew_types::DefTable,
    mut diagnostic: hew_types::TypeError,
) -> hew_types::TypeError {
    let original_file = diagnostic_source_file(definitions, diagnostic.source_module.as_deref());
    // Expansion attribution may fall back to the whole invocation. Such an
    // origin is useful for reporting, but is not an exact editable range.
    diagnostic.fixes.retain(|fix| {
        fix.edits.iter().all(|edit| {
            original_file
                .and_then(|file| {
                    program
                        .expansions
                        .source_origin(file, &(edit.span.start..edit.span.end))
                })
                .is_none()
        })
    });
    if let Some((path, span)) =
        original_file.and_then(|file| program.expansions.source_origin(file, &diagnostic.span))
    {
        diagnostic.fixes.clear();
        if let Some((invocation_path, invocation_span)) = original_file
            .and_then(|file| program.expansions.invocation_origin(file, &diagnostic.span))
        {
            diagnostic.notes.push((
                invocation_span,
                "in this macro invocation".into(),
                Some(invocation_path.display().to_string()),
            ));
        }
        diagnostic.span = span;
        diagnostic.source_module = Some(path.display().to_string());
    }
    for (span, _, source) in &mut diagnostic.notes {
        let file = source.as_deref().map_or(original_file, |source| {
            diagnostic_source_file(definitions, Some(source))
        });
        if let Some((path, original)) =
            file.and_then(|file| program.expansions.source_origin(file, span))
        {
            *span = original;
            *source = Some(path.display().to_string());
        }
    }
    diagnostic
}

fn diagnostic_source_file<'a>(
    definitions: &'a hew_types::DefTable,
    origin: Option<&str>,
) -> Option<&'a Path> {
    let module = match origin {
        Some(origin) => definitions
            .module_for_source(Path::new(origin))
            .or_else(|| definitions.module_for_path(origin)),
        None => definitions.root_module(),
    }?;
    definitions.module_source(module)
}

pub(crate) fn type_diagnostic_to_frontend(
    diagnostic: hew_types::TypeError,
    sources: &SourceSnapshot,
) -> FrontendDiagnostic {
    let (source, filename) = diagnostic
        .source_module
        .as_deref()
        .and_then(|origin| sources.source(origin))
        .unwrap_or_else(|| {
            (
                Arc::clone(&sources.root_source),
                sources.root_filename.clone(),
            )
        });
    FrontendDiagnostic::type_(source, &filename, diagnostic, sources)
}

fn hir_diagnostic_to_frontend(
    diagnostic: hew_hir::HirDiagnostic,
    sources: &SourceSnapshot,
) -> FrontendDiagnostic {
    let source = match diagnostic.source_module.as_deref() {
        None => Some((
            Arc::clone(&sources.root_source),
            sources.root_filename.clone(),
        )),
        Some(module) => sources.source(module),
    };
    match source {
        Some((source, filename)) => {
            FrontendDiagnostic::hir(Some(source), Some(&filename), diagnostic)
        }
        None => FrontendDiagnostic::hir(None, None, diagnostic),
    }
}

/// Route HIR diagnostics through the exact source context admitted by the frontend.
/// Unknown foreign origins remain unavailable rather than using the root file.
#[must_use]
pub fn hir_diagnostics_to_frontend(
    diagnostics: Vec<hew_hir::HirDiagnostic>,
    sources: &SourceSnapshot,
) -> Vec<FrontendDiagnostic> {
    diagnostics
        .into_iter()
        .map(|diagnostic| hir_diagnostic_to_frontend(diagnostic, sources))
        .collect()
}

/// Attribute ownership findings using the same admitted source text as type checking.
#[must_use]
pub fn ownership_diagnostics_to_frontend(
    diagnostics: Vec<hew_sir::SirDiagnostic>,
    sources: &SourceSnapshot,
) -> Vec<FrontendDiagnostic> {
    let source_for = |origin: &hew_sir::FunctionSourceOrigin| match origin {
        hew_sir::FunctionSourceOrigin::RootUnit => Some((
            Arc::clone(&sources.root_source),
            sources.root_filename.clone(),
        )),
        hew_sir::FunctionSourceOrigin::Foreign(module) => sources.source(module),
        hew_sir::FunctionSourceOrigin::Unknown => None,
    };
    diagnostics
        .into_iter()
        .map(|diagnostic| {
            if let hew_sir::SirDiagnosticKind::InvalidGeneratorSend { message, site } =
                &diagnostic.kind
            {
                return source_for(&site.source).map_or_else(
                    || FrontendDiagnostic::coded_message("InvalidSend", message.clone()),
                    |(source, filename)| FrontendDiagnostic {
                        source: Some(source),
                        filename: Some(filename),
                        note_sources: Vec::new(),
                        kind: FrontendDiagnosticKind::Type(hew_types::TypeError::new(
                            hew_types::error::TypeErrorKind::InvalidSend,
                            site.span.clone(),
                            message.clone(),
                        )),
                    },
                );
            }
            let hew_sir::SirDiagnosticKind::UnconsumedLinear {
                binding,
                span,
                source_origin,
                ..
            } = diagnostic.kind
            else {
                unreachable!("SessionError::Ownership contains source ownership findings");
            };
            let message = binding.map_or_else(
                || "linear value must be consumed before this normal exit".to_string(),
                |binding| {
                    format!("linear value `{binding}` must be consumed before this normal exit")
                },
            );
            source_for(&source_origin).map_or_else(
                || FrontendDiagnostic::coded_message("E_MUST_CONSUME", message.clone()),
                |(source, filename)| {
                    FrontendDiagnostic::coded_message_at(
                        "E_MUST_CONSUME",
                        message.clone(),
                        span,
                        source,
                        &filename,
                    )
                },
            )
        })
        .collect()
}
