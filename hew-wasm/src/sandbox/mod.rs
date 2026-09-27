//! WASM bindings for compiling Hew source into sandbox VM bytecode packages.
//!
//! The shared frontend produces verified ownership SIR. The VM admits the
//! resulting package before executing any instruction.

pub mod sir_emit;

/// The shipped standard-library sources, as `("std/<relative path>", source)`.
mod std_sources {
    include!(concat!(env!("OUT_DIR"), "/std_sources.rs"));
}

use serde::{Deserialize, Serialize};

pub const DEFAULT_PROFILE_ALIAS: &str = "sandbox-vm-export";
pub const DEFAULT_PROFILE_CANONICAL: &str = "sandbox.sandbox-vm-export.v0";

/// Normalize the historical browser API's target name; this is not a source
/// capability gate. Runtime capability admission belongs to the VM loader.
///
/// # Errors
/// Returns a diagnostic for an unknown target name.
#[expect(
    clippy::result_large_err,
    reason = "the browser uses one diagnostic envelope"
)]
pub fn canonical_profile(profile: Option<&str>) -> Result<String, Diagnostic> {
    match profile.unwrap_or(DEFAULT_PROFILE_ALIAS).trim() {
        "" | DEFAULT_PROFILE_ALIAS | DEFAULT_PROFILE_CANONICAL => {
            Ok(DEFAULT_PROFILE_CANONICAL.to_string())
        }
        other => Err(Diagnostic::profile_error(
            0..0,
            "unknown_sandbox_profile",
            format!("unknown sandbox target `{other}`"),
        )),
    }
}
pub use sir_emit::Package as SandboxBytecodePackageV1;

/// Names the editor buffer in frontend diagnostics and anchors its imports.
const SANDBOX_BUFFER_LABEL: &str = "playground.hew";

/// The standard library as a document set.
///
/// Import resolution builds candidate paths relative to the buffer and reads
/// them through this set before the filesystem, so the browser resolves the
/// shipped sources it has no filesystem to read. Natively the files are on
/// disk and win, so this changes nothing there.
///
/// The working-directory spelling is the one recorded, because it is the
/// candidate both shapes reach: the buffer's own import tries it after its
/// relative form, and a standard-library module importing a sibling tries it
/// after a form relative to that module's own directory. Recording both
/// spellings would make every import ambiguous, since a candidate the
/// filesystem cannot canonicalize stays the distinct path it was written as.
#[must_use]
pub fn embedded_standard_library() -> &'static [(&'static str, &'static str)] {
    std_sources::STD_SOURCES
}

/// The paths of [`embedded_standard_library`], as resolution spells them.
#[must_use]
pub fn embedded_standard_library_paths() -> Vec<&'static str> {
    std_sources::STD_SOURCES
        .iter()
        .map(|(path, _)| *path)
        .collect()
}

fn embedded_standard_library_documents() -> hew_compile::DocumentSet {
    let mut documents = hew_compile::DocumentSet::new();
    for (path, source) in std_sources::STD_SOURCES {
        documents.insert(*path, *source);
    }
    documents
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DiagnosticSpan {
    pub start: usize,
    pub end: usize,
}

impl DiagnosticSpan {
    fn from_span(span: &std::ops::Range<usize>) -> Self {
        Self {
            start: span.start,
            end: span.end,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DiagnosticNote {
    pub span: DiagnosticSpan,
    pub start_offset: usize,
    pub end_offset: usize,
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source_module: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Diagnostic {
    pub severity: String,
    pub phase: String,
    pub message: String,
    pub span: DiagnosticSpan,
    pub start_offset: usize,
    pub end_offset: usize,
    pub kind: String,
    pub notes: Vec<DiagnosticNote>,
    pub suggestions: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source_module: Option<String>,
}

impl Diagnostic {
    fn profile_error(span: std::ops::Range<usize>, kind: &str, message: impl Into<String>) -> Self {
        let diag_span = DiagnosticSpan::from_span(&span);
        Self {
            severity: "error".to_string(),
            phase: "profile".to_string(),
            message: message.into(),
            span: diag_span.clone(),
            start_offset: diag_span.start,
            end_offset: diag_span.end,
            kind: kind.to_string(),
            notes: Vec::new(),
            suggestions: Vec::new(),
            source_module: None,
        }
    }
}

/// One verified-SIR bytecode format is emitted by the browser compiler.
pub type SandboxPackage = SandboxBytecodePackageV1;

impl SandboxPackage {
    /// The compiler version recorded in fixture provenance.
    #[must_use]
    pub fn compiler_version(&self) -> &str {
        &self.compiler_version
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CompileOutput {
    pub diagnostics: Vec<Diagnostic>,
    pub bytecode: Option<SandboxPackage>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CompileError {
    pub message: String,
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for CompileError {}

/// Compile through the shared frontend and verified ownership SIR.
///
/// Parse, type and SIR errors are returned as diagnostics. The VM's package
/// validator owns runtime capability admission.
///
/// # Errors
/// Returns an internal error if verified semantics cannot be serialized.
pub fn compile_to_sandbox_bytecode(
    source: &str,
    profile: Option<&str>,
) -> Result<CompileOutput, CompileError> {
    compile_from_semantics(
        source,
        profile,
        SANDBOX_BUFFER_LABEL,
        hew_compile::FrontendOptions::default(),
        &mut Vec::new(),
    )
}

/// Compile one test file through the shared frontend and project an executable
/// package for each selected test. Type checking and SIR lowering happen once;
/// each package differs only by its checked entry callable.
///
/// # Errors
/// Returns compiler diagnostics in the output, or an internal projection error.
pub fn compile_tests_to_sandbox_bytecode(
    source: &str,
    source_path: &std::path::Path,
    selections: &[hew_types::DeclarationOccurrence],
    companion: Option<&std::path::Path>,
    project_dir: &std::path::Path,
) -> Result<TestCompileOutput, CompileError> {
    let label = source_path.to_string_lossy();
    let mut entries = Vec::new();
    let output = compile_from_semantics(
        source,
        Some(DEFAULT_PROFILE_ALIAS),
        &label,
        hew_compile::FrontendOptions {
            project_dir: Some(project_dir.to_path_buf()),
            test_entry_selections: selections.to_vec(),
            deterministic_admission: hew_compile::DeterministicAdmission::Tests(
                selections.to_vec(),
            ),
            companion: companion.map(std::path::Path::to_path_buf),
            ..Default::default()
        },
        &mut entries,
    )?;
    let bytecodes = output.bytecode.map_or_else(Vec::new, |package| {
        entries
            .into_iter()
            .map(|entry| {
                let mut selected = package.clone();
                selected.entry = Some(entry);
                selected
            })
            .collect()
    });
    Ok(TestCompileOutput {
        diagnostics: output.diagnostics,
        bytecodes,
    })
}

#[derive(Debug, Clone)]
pub struct TestCompileOutput {
    pub diagnostics: Vec<Diagnostic>,
    pub bytecodes: Vec<SandboxBytecodePackageV1>,
}

/// Compile every test in one editor buffer once, returning one selected VM
/// package per discovered declaration. The browser chooses the package by its
/// returned identity and runs it with `runBytecode` in the sandbox VM.
#[must_use]
#[wasm_bindgen::prelude::wasm_bindgen(js_name = compileTestsToSandboxBytecode)]
pub fn compile_tests_to_sandbox_bytecode_js(source: &str, file: &str) -> String {
    let parsed = hew_parser::parse(source);
    let tests = hew_analysis::test_discovery::discover_tests(&parsed.program);
    let parse_diagnostics = convert_parse_diagnostics(&parsed.errors);
    if parse_diagnostics
        .iter()
        .any(|diagnostic| diagnostic.severity == "error")
    {
        return serde_json::json!({"diagnostics": parse_diagnostics, "tests": []}).to_string();
    }
    if tests.is_empty() {
        return serde_json::json!({"diagnostics": parse_diagnostics, "tests": []}).to_string();
    }
    let eligible = tests
        .iter()
        .filter(|test| !test.real_time)
        .collect::<Vec<_>>();
    let selections = eligible
        .iter()
        .map(|test| {
            hew_types::DeclarationOccurrence::new_with_synthetic_ordinal(
                None,
                &test.span,
                test.item_ordinal,
                hew_types::DeclarationKind::Function,
                0,
            )
        })
        .collect::<Vec<_>>();
    let path = std::path::Path::new(file);
    let project_dir = path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| std::path::Path::new("."));
    if eligible.is_empty() {
        return serde_json::json!({
            "diagnostics": parse_diagnostics,
            "tests": tests.iter().map(|test| serde_json::json!({
                "identity": format!("{file}::{}", test.name),
                "name": test.name,
                "range": {"start": test.span.start, "end": test.span.end},
                "ignored": test.ignored,
                "ignore_reason": test.ignore_reason,
                "should_panic": test.should_panic,
                "should_panic_message": test.should_panic_message,
                "timeout_ns": test.timeout_ns.map(|value| value.to_string()),
                "real_time": test.real_time,
                "bytecode": null,
            })).collect::<Vec<_>>(),
        })
        .to_string();
    }
    match compile_tests_to_sandbox_bytecode(source, path, &selections, None, project_dir) {
        Ok(output) => {
            if output.bytecodes.len() != eligible.len() {
                if output
                    .diagnostics
                    .iter()
                    .any(|diagnostic| diagnostic.severity == "error")
                {
                    return serde_json::json!({"diagnostics": output.diagnostics, "tests": []})
                        .to_string();
                }
                return serde_json::json!({"error": "compiler emitted an incomplete test package set"}).to_string();
            }
            let mut bytecodes = output.bytecodes.into_iter();
            let packages = tests
                .into_iter()
                .map(|test| {
                    let bytecode = if test.real_time {
                        None
                    } else {
                        bytecodes.next()
                    };
                    serde_json::json!({
                        "identity": format!("{file}::{}", test.name),
                        "name": test.name,
                        "range": {"start": test.span.start, "end": test.span.end},
                        "ignored": test.ignored,
                        "ignore_reason": test.ignore_reason,
                        "should_panic": test.should_panic,
                        "should_panic_message": test.should_panic_message,
                        "timeout_ns": test.timeout_ns.map(|value| value.to_string()),
                        "real_time": test.real_time,
                        "bytecode": bytecode,
                    })
                })
                .collect::<Vec<_>>();
            serde_json::json!({"diagnostics": output.diagnostics, "tests": packages}).to_string()
        }
        Err(error) => {
            serde_json::json!({"error": format!("internal compiler error: {}", error.message)})
                .to_string()
        }
    }
}

/// Compile Hew source into a sandbox bytecode package and return the result
/// as a JSON-encoded string.
///
/// This is the browser entry point exported via wasm-bindgen as
/// `compileToSandboxBytecode`. The TS consumer in
/// `hew-sandbox-vm/src/interpreter/run-program.ts` calls this function and
/// JSON-parses the result as a `CompileOutput` object.
///
/// `profile` should be `"sandbox-vm-export"` for the educational sandbox VM.
/// An unrecognised profile is reported as a diagnostic with `bytecode: null`.
#[must_use]
#[wasm_bindgen::prelude::wasm_bindgen(js_name = compileToSandboxBytecode)]
pub fn compile_to_sandbox_bytecode_js(source: &str, profile: &str) -> String {
    compile_to_sandbox_bytecode_json(source, profile)
}

/// Serialize the result of [`compile_to_sandbox_bytecode`] as a JSON string.
///
/// Used by [`compile_to_sandbox_bytecode_js`] (the wasm-bindgen export) and by
/// native tests that verify the serialization path without a WASM runtime.
#[must_use]
pub fn compile_to_sandbox_bytecode_json(source: &str, profile: &str) -> String {
    match compile_to_sandbox_bytecode(source, Some(profile)) {
        Ok(output) => serde_json::to_string(&output)
            .unwrap_or_else(|e| format!("{{\"error\":\"serialization failed: {e}\"}}")),
        Err(err) => format!(
            "{{\"error\":\"internal compiler error: {}\"}}",
            err.message.replace('"', "\\\"")
        ),
    }
}

fn has_error_diagnostics(diagnostics: &[Diagnostic]) -> bool {
    diagnostics
        .iter()
        .any(|diagnostic| diagnostic.severity == "error")
}

fn compile_from_semantics(
    source: &str,
    profile: Option<&str>,
    source_label: &str,
    mut options: hew_compile::FrontendOptions,
    test_entries: &mut Vec<sir_emit::Entry>,
) -> Result<CompileOutput, CompileError> {
    let canonical_profile = match canonical_profile(profile) {
        Ok(profile) => profile,
        Err(diagnostic) => {
            return Ok(CompileOutput {
                diagnostics: vec![diagnostic],
                bytecode: None,
            })
        }
    };

    // The shared frontend, not a second one: it resolves this buffer's imports
    // into the module graph, so an imported declaration reaches HIR with a
    // body instead of an unresolved binding.
    options.documents = embedded_standard_library_documents();
    let state = hew_compile::run_source_frontend(source, source_label, &options);
    let mut diagnostics = state
        .parse_result
        .as_ref()
        .map(|parse| convert_parse_diagnostics(&parse.errors))
        .unwrap_or_default();
    if has_error_diagnostics(&diagnostics) {
        return Ok(CompileOutput {
            diagnostics,
            bytecode: None,
        });
    }

    let Some(type_output) = state
        .typecheck_result
        .as_ref()
        .and_then(|result| result.tco.as_ref())
    else {
        diagnostics.push(Diagnostic::profile_error(
            0..0,
            "frontend_stopped",
            state
                .stopped
                .as_ref()
                .map_or("type checking did not run", |failure| {
                    failure.message.as_str()
                }),
        ));
        return Ok(CompileOutput {
            diagnostics,
            bytecode: None,
        });
    };
    diagnostics.extend(convert_type_diagnostics(type_output));
    if has_error_diagnostics(&diagnostics) {
        return Ok(CompileOutput {
            diagnostics,
            bytecode: None,
        });
    }
    let program = &state.program;

    let session = hew_compile::Session::new(
        hew_compile::SessionTarget::browser(),
        hew_compile::DiagnosticPolicy::default(),
    );
    let semantics = match session.lower_program(program, type_output) {
        Ok(output) => output,
        Err(error) => {
            diagnostics.push(semantic_diagnostic(&error));
            return Ok(CompileOutput {
                diagnostics,
                bytecode: None,
            });
        }
    };

    let module = &semantics.semantics().module;
    let package = sir_emit::emit_package(
        module,
        &canonical_profile,
        env!("CARGO_PKG_VERSION"),
        &format!("hew-wasm-{}", env!("CARGO_PKG_VERSION")),
    )
    .map_err(|error| CompileError {
        message: error.message,
    })?;
    for test in &module.test_entries {
        let index = module
            .functions
            .iter()
            .position(|function| function.callable == test.callable)
            .ok_or_else(|| CompileError {
                message: "checked test entry is absent from sandbox functions".into(),
            })?;
        let function = u32::try_from(index).map_err(|_| CompileError {
            message: "sandbox function table exceeds u32".into(),
        })?;
        let exit = match test.action {
            hew_types::EntryExitAction::Unit => "unit",
            hew_types::EntryExitAction::Integer(_) | hew_types::EntryExitAction::Result { .. } => {
                "status"
            }
        };
        test_entries.push(sir_emit::Entry {
            function,
            exit: exit.into(),
        });
    }

    Ok(CompileOutput {
        diagnostics,
        bytecode: Some(package),
    })
}

/// Render a semantic boundary failure the way the browser analysis surface
/// does, so an editor and a run report the same thing.
fn semantic_diagnostic(error: &hew_compile::SessionError) -> Diagnostic {
    Diagnostic {
        severity: "error".to_string(),
        phase: "sir".to_string(),
        message: error.to_string(),
        span: DiagnosticSpan { start: 0, end: 0 },
        start_offset: 0,
        end_offset: 0,
        kind: "E_SIR_VERIFY".to_string(),
        notes: Vec::new(),
        suggestions: Vec::new(),
        source_module: None,
    }
}

fn convert_parse_diagnostics(parse_errors: &[hew_parser::ParseError]) -> Vec<Diagnostic> {
    parse_errors.iter().map(parse_error_to_diagnostic).collect()
}

fn parse_error_to_diagnostic(err: &hew_parser::ParseError) -> Diagnostic {
    let severity = match err.severity {
        hew_parser::Severity::Warning => "warning",
        hew_parser::Severity::Error => "error",
    };
    let span = DiagnosticSpan::from_span(&err.span);
    Diagnostic {
        severity: severity.to_string(),
        phase: "parse".to_string(),
        message: err.message.clone(),
        span: span.clone(),
        start_offset: span.start,
        end_offset: span.end,
        kind: err.kind.as_kind_str().to_string(),
        notes: Vec::new(),
        suggestions: err.hint.clone().into_iter().collect(),
        source_module: None,
    }
}

fn convert_type_diagnostics(type_output: &hew_types::TypeCheckOutput) -> Vec<Diagnostic> {
    type_output
        .errors
        .iter()
        .chain(type_output.warnings.iter())
        .map(type_error_to_diagnostic)
        .collect()
}

fn type_error_to_diagnostic(err: &hew_types::error::TypeError) -> Diagnostic {
    let severity = match err.severity {
        hew_types::error::Severity::Warning => "warning",
        hew_types::error::Severity::Error => "error",
    };
    let span = DiagnosticSpan::from_span(&err.span);
    Diagnostic {
        severity: severity.to_string(),
        phase: "typecheck".to_string(),
        message: err.message.clone(),
        span: span.clone(),
        start_offset: span.start,
        end_offset: span.end,
        kind: err.kind.as_kind_str().to_string(),
        notes: err
            .notes
            .iter()
            .map(|(span, message, source_module)| {
                let note_span = DiagnosticSpan::from_span(span);
                DiagnosticNote {
                    span: note_span.clone(),
                    start_offset: note_span.start,
                    end_offset: note_span.end,
                    message: message.clone(),
                    source_module: source_module.clone(),
                }
            })
            .collect(),
        suggestions: err.suggestions.clone(),
        source_module: err.source_module.clone(),
    }
}
