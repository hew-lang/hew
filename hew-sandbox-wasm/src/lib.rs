//! WASM bindings for compiling Hew source into sandbox VM bytecode packages.
//!
//! This crate is separate from `hew-wasm`: `hew-wasm` remains the browser
//! analysis-only crate, while this crate owns the sandbox bytecode export tier.

mod bytecode;
mod emit;
mod profile;

use bytecode::{Block, Capability, Instruction, Local, Operand, StdlibSymbol, Terminator};
use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;

pub use bytecode::SandboxBytecodePackage;
pub use profile::{canonical_profile, DEFAULT_PROFILE_ALIAS, DEFAULT_PROFILE_CANONICAL};

const SANDBOX_STDIN_HELPER: &str = "__hew_sandbox_stdin_read_line";
const SANDBOX_STDIN_SYMBOL: &str = "sym:core.stdin.read_line";
const SANDBOX_STDIN_CAPABILITY: &str = "core.stdin";

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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CompileOutput {
    pub diagnostics: Vec<Diagnostic>,
    pub bytecode: Option<SandboxBytecodePackage>,
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

/// Compile Hew source into a sandbox bytecode package.
///
/// Parse, type-check, and sandbox profile failures are reported as diagnostics
/// in the returned [`CompileOutput`]. `bytecode` is populated only when all
/// three gates complete without error-severity diagnostics.
///
/// # Errors
///
/// Returns [`CompileError`] only for internal serialization/emission failures.
pub fn compile_to_sandbox_bytecode(
    source: &str,
    profile: Option<&str>,
) -> Result<CompileOutput, CompileError> {
    let rewritten_source = source_with_sandbox_stdin_helper(source);
    let source_text = rewritten_source.as_deref().unwrap_or(source);
    let canonical_profile = match canonical_profile(profile) {
        Ok(profile) => profile,
        Err(diagnostic) => {
            return Ok(CompileOutput {
                diagnostics: vec![diagnostic],
                bytecode: None,
            });
        }
    };

    let parse_result = hew_parser::parse(source_text);
    let mut diagnostics = convert_parse_diagnostics(&parse_result.errors);
    if has_error_diagnostics(&diagnostics) {
        return Ok(CompileOutput {
            diagnostics,
            bytecode: None,
        });
    }

    let mut checker = hew_types::Checker::new(hew_types::module_registry::ModuleRegistry::new(
        hew_types::module_registry::build_module_search_paths(),
    ));
    let type_output = checker.check_program(&parse_result.program);
    diagnostics.extend(convert_type_diagnostics(&type_output));
    if has_error_diagnostics(&diagnostics) {
        return Ok(CompileOutput {
            diagnostics,
            bytecode: None,
        });
    }

    let profile_report =
        profile::check_program(&parse_result.program, &type_output, &canonical_profile);
    diagnostics.extend(profile_report.diagnostics);
    if has_error_diagnostics(&diagnostics) {
        return Ok(CompileOutput {
            diagnostics,
            bytecode: None,
        });
    }

    let mut bytecode = emit::emit_package(
        source_text,
        &canonical_profile,
        &parse_result.program,
        &type_output,
    )?;
    if rewritten_source.is_some() {
        install_sandbox_stdin_helper(&mut bytecode);
    }
    Ok(CompileOutput {
        diagnostics,
        bytecode: Some(bytecode),
    })
}

/// WASM-bindgen JSON entry point for browser callers.
///
/// JavaScript name: `compileToSandboxBytecode(source, profile)`.
///
/// # Errors
///
/// Throws only when the Rust result cannot be serialized to JSON or an internal
/// emitter invariant fails.
#[wasm_bindgen(js_name = compileToSandboxBytecode)]
#[expect(
    clippy::needless_pass_by_value,
    reason = "wasm-bindgen optional string arguments are owned at the ABI boundary"
)]
pub fn compile_to_sandbox_bytecode_json(
    source: &str,
    profile: Option<String>,
) -> Result<String, JsValue> {
    let output = compile_to_sandbox_bytecode(source, profile.as_deref())
        .map_err(|err| JsValue::from_str(&err.message))?;
    serde_json::to_string(&output)
        .map_err(|err| JsValue::from_str(&format!("serialize compile output: {err}")))
}

fn has_error_diagnostics(diagnostics: &[Diagnostic]) -> bool {
    diagnostics
        .iter()
        .any(|diagnostic| diagnostic.severity == "error")
}

fn source_with_sandbox_stdin_helper(source: &str) -> Option<String> {
    if !source.contains("io.read_line()") {
        return None;
    }
    let mut body = String::new();
    for line in source.lines() {
        if line.trim() == "import std::io;" {
            continue;
        }
        body.push_str(line);
        body.push('\n');
    }
    let body = body.replace("io.read_line()", &format!("{SANDBOX_STDIN_HELPER}()"));
    Some(format!(
        "fn {SANDBOX_STDIN_HELPER}() -> string {{ \"\" }}\n{body}"
    ))
}

fn install_sandbox_stdin_helper(bytecode: &mut SandboxBytecodePackage) {
    if !bytecode
        .capabilities
        .iter()
        .any(|capability| capability.id == SANDBOX_STDIN_CAPABILITY)
    {
        bytecode.capabilities.push(Capability {
            id: SANDBOX_STDIN_CAPABILITY.to_string(),
            disposition: "allowed".to_string(),
            reason: "stdin is supplied by the educational sandbox page input buffer".to_string(),
            required_by: vec![SANDBOX_STDIN_SYMBOL.to_string()],
        });
    }
    if !bytecode
        .stdlib_symbols
        .iter()
        .any(|symbol| symbol.id == SANDBOX_STDIN_SYMBOL)
    {
        bytecode.stdlib_symbols.push(StdlibSymbol {
            id: SANDBOX_STDIN_SYMBOL.to_string(),
            module: "core.stdin".to_string(),
            name: "read_line".to_string(),
            params: Vec::new(),
            result: "type:string".to_string(),
            capability: Some(SANDBOX_STDIN_CAPABILITY.to_string()),
            admission: "allowed".to_string(),
        });
    }
    let Some(function) = bytecode
        .functions
        .iter_mut()
        .find(|function| function.name == SANDBOX_STDIN_HELPER)
    else {
        return;
    };
    let local = "local:stdin.line".to_string();
    function.locals = vec![Local {
        id: local.clone(),
        name: Some("stdin_line".to_string()),
        ty: "type:string".to_string(),
        mutable: false,
        span: function.span.clone(),
    }];
    function.blocks = vec![Block {
        id: "block:entry".to_string(),
        params: Vec::new(),
        instructions: vec![Instruction {
            op: "call.stdlib".to_string(),
            dst: Some(local.clone()),
            args: vec![Operand::symbol(SANDBOX_STDIN_SYMBOL)],
            span: function.span.clone(),
            metadata: None,
        }],
        terminator: Terminator::ret(vec![Operand::local(local)], function.span.clone()),
        span: function.span.clone(),
    }];
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
            .map(|(span, message)| {
                let note_span = DiagnosticSpan::from_span(span);
                DiagnosticNote {
                    span: note_span.clone(),
                    start_offset: note_span.start,
                    end_offset: note_span.end,
                    message: message.clone(),
                }
            })
            .collect(),
        suggestions: err.suggestions.clone(),
        source_module: err.source_module.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fixture(name: &str) -> String {
        set_test_hewpath();
        let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../hew-sandbox-vm/fixtures")
            .join(name)
            .join("main.hew");
        std::fs::read_to_string(path).expect("fixture source should be readable")
    }

    fn set_test_hewpath() {
        let repo_root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .expect("crate should have a workspace parent");
        std::env::set_var("HEWPATH", repo_root);
    }

    #[test]
    fn profile_alias_is_canonicalized() {
        assert_eq!(
            canonical_profile(Some("sandbox-vm-export")).unwrap(),
            DEFAULT_PROFILE_CANONICAL
        );
        assert_eq!(
            canonical_profile(Some(DEFAULT_PROFILE_CANONICAL)).unwrap(),
            DEFAULT_PROFILE_CANONICAL
        );
        assert!(canonical_profile(Some("analysis-only")).is_err());
    }

    #[test]
    fn hello_world_exports_bytecode() {
        let output =
            compile_to_sandbox_bytecode(&fixture("01-hello-world"), Some("sandbox-vm-export"))
                .expect("compile should not throw");
        assert!(
            output.diagnostics.iter().all(|d| d.severity != "error"),
            "unexpected diagnostics: {:#?}",
            output.diagnostics
        );
        let bytecode = output.bytecode.expect("bytecode should be emitted");
        assert_eq!(bytecode.profile, DEFAULT_PROFILE_CANONICAL);
        assert_eq!(bytecode.schema_version, "hew.sandbox.bytecode.v0");
        assert!(bytecode
            .stdlib_symbols
            .iter()
            .any(|symbol| symbol.id == "sym:core.stdout.println"));
    }

    #[test]
    fn arithmetic_fixture_uses_checked_integer_ops() {
        let output = compile_to_sandbox_bytecode(
            &fixture("02-arithmetic-checked"),
            Some("sandbox-vm-export"),
        )
        .expect("compile should not throw");
        assert!(
            output.diagnostics.iter().all(|d| d.severity != "error"),
            "unexpected diagnostics: {:#?}",
            output.diagnostics
        );
        let bytecode = output.bytecode.expect("bytecode should be emitted");
        let ops = all_instruction_ops(&bytecode);
        assert!(ops.contains(&"i64.checked_add"));
    }

    #[test]
    fn literal_forms_export_constant_opcodes() {
        set_test_hewpath();
        let source = r#"
fn main() {
    let flag = true;
    let ratio = 1.5;
    let letter = 'h';
    println("literal coverage");
}
"#;
        let output = compile_to_sandbox_bytecode(source, Some("sandbox-vm-export"))
            .expect("compile should not throw");
        assert!(
            output.diagnostics.iter().all(|d| d.severity != "error"),
            "unexpected diagnostics: {:#?}",
            output.diagnostics
        );
        let bytecode = output.bytecode.expect("bytecode should be emitted");
        let ops = all_instruction_ops(&bytecode);
        assert!(ops.contains(&"const.bool"));
        assert!(ops.contains(&"const.f64"));
        assert!(ops.contains(&"const.string"));
    }

    #[test]
    fn record_fixture_emits_record_layout_and_access() {
        let output =
            compile_to_sandbox_bytecode(&fixture("04-record-fields"), Some("sandbox-vm-export"))
                .expect("compile should not throw");
        assert!(
            output.diagnostics.iter().all(|d| d.severity != "error"),
            "unexpected diagnostics: {:#?}",
            output.diagnostics
        );
        let bytecode = output.bytecode.expect("bytecode should be emitted");
        assert!(
            bytecode
                .layouts
                .records
                .iter()
                .any(|record| record.name == "Point"),
            "Point record layout missing: {:#?}",
            bytecode.layouts.records
        );
        let ops = all_instruction_ops(&bytecode);
        assert!(ops.contains(&"record.new"));
        assert!(ops.contains(&"record.get"));
    }

    #[test]
    fn enum_match_fixture_emits_tag_payload_and_branching() {
        let output =
            compile_to_sandbox_bytecode(&fixture("05-enum-match"), Some("sandbox-vm-export"))
                .expect("compile should not throw");
        assert!(
            output.diagnostics.iter().all(|d| d.severity != "error"),
            "unexpected diagnostics: {:#?}",
            output.diagnostics
        );
        let bytecode = output.bytecode.expect("bytecode should be emitted");
        assert!(
            bytecode
                .layouts
                .enums
                .iter()
                .any(|enum_layout| enum_layout.name == "Shape"),
            "Shape enum layout missing: {:#?}",
            bytecode.layouts.enums
        );
        let ops = all_instruction_ops(&bytecode);
        assert!(ops.contains(&"enum.new"));
        assert!(ops.contains(&"enum.tag"));
        assert!(ops.contains(&"enum.payload"));
        assert!(bytecode
            .functions
            .iter()
            .flat_map(|function| &function.blocks)
            .any(|block| block.terminator.op == "br_if"));
    }

    #[test]
    fn generic_call_exports_bytecode_without_rejecting_type_params() {
        set_test_hewpath();
        let source = r"
fn id<T>(value: T) -> T {
    value
}

fn main() {
    println(id(42));
}
";
        let output = compile_to_sandbox_bytecode(source, Some("sandbox-vm-export"))
            .expect("compile should not throw");
        assert!(
            output.diagnostics.iter().all(|d| d.severity != "error"),
            "unexpected diagnostics: {:#?}",
            output.diagnostics
        );
        let bytecode = output.bytecode.expect("bytecode should be emitted");
        let ops = all_instruction_ops(&bytecode);
        assert!(ops.contains(&"call.direct"));
    }

    #[test]
    fn direct_call_with_return_exports_call_and_return() {
        set_test_hewpath();
        let source = r"
fn double(value: i64) -> i64 {
    return value * 2;
}

fn main() {
    println(double(21));
}
";
        let output = compile_to_sandbox_bytecode(source, Some("sandbox-vm-export"))
            .expect("compile should not throw");
        assert!(
            output.diagnostics.iter().all(|d| d.severity != "error"),
            "unexpected diagnostics: {:#?}",
            output.diagnostics
        );
        let bytecode = output.bytecode.expect("bytecode should be emitted");
        let ops = all_instruction_ops(&bytecode);
        assert!(ops.contains(&"call.direct"));
        assert!(bytecode
            .functions
            .iter()
            .flat_map(|function| &function.blocks)
            .any(|block| block.terminator.op == "return"));
    }

    #[test]
    fn vector_fixture_emits_new_push_len_and_get() {
        let output =
            compile_to_sandbox_bytecode(&fixture("06-vector-basics"), Some("sandbox-vm-export"))
                .expect("compile should not throw");
        assert!(
            output.diagnostics.iter().all(|d| d.severity != "error"),
            "unexpected diagnostics: {:#?}",
            output.diagnostics
        );
        let bytecode = output.bytecode.expect("bytecode should be emitted");
        let ops = all_instruction_ops(&bytecode);
        assert!(ops.contains(&"vector.new"));
        assert!(ops.contains(&"vector.push"));
        assert!(ops.contains(&"vector.len"));
        assert!(ops.contains(&"vector.get"));
    }

    #[test]
    fn string_methods_emit_len_and_slice() {
        set_test_hewpath();
        let source = r#"
fn main() {
    let value = "sandbox";
    println(value.len());
    println(value.slice(1, 4));
}
"#;
        let output = compile_to_sandbox_bytecode(source, Some("sandbox-vm-export"))
            .expect("compile should not throw");
        assert!(
            output.diagnostics.iter().all(|d| d.severity != "error"),
            "unexpected diagnostics: {:#?}",
            output.diagnostics
        );
        let bytecode = output.bytecode.expect("bytecode should be emitted");
        let ops = all_instruction_ops(&bytecode);
        assert!(ops.contains(&"string.len"));
        assert!(ops.contains(&"string.slice"));
    }

    #[test]
    fn regex_fixture_maps_to_reserved_regex_shim() {
        let output =
            compile_to_sandbox_bytecode(&fixture("08-regex-match"), Some("sandbox-vm-export"))
                .expect("compile should not throw");
        assert!(
            output.diagnostics.iter().all(|d| d.severity != "error"),
            "unexpected diagnostics: {:#?}",
            output.diagnostics
        );
        let bytecode = output.bytecode.expect("bytecode should be emitted");
        let ops = all_instruction_ops(&bytecode);
        assert!(ops.contains(&"regex.compile"));
        assert!(ops.contains(&"regex.find"));
        assert!(bytecode
            .capabilities
            .iter()
            .any(|cap| cap.id == "std.text.regex.compile" && cap.disposition == "reserved"));
    }

    #[test]
    fn panic_fixture_emits_panic_opcode() {
        let output =
            compile_to_sandbox_bytecode(&fixture("11-runtime-panic"), Some("sandbox-vm-export"))
                .expect("compile should not throw");
        assert!(
            output.diagnostics.iter().all(|d| d.severity != "error"),
            "unexpected diagnostics: {:#?}",
            output.diagnostics
        );
        let bytecode = output.bytecode.expect("bytecode should be emitted");
        let ops = all_instruction_ops(&bytecode);
        assert!(ops.contains(&"panic"));
    }

    #[test]
    fn divide_by_zero_fixture_emits_checked_division_trap_point() {
        let output = compile_to_sandbox_bytecode(
            &fixture("12-divide-by-zero-trap"),
            Some("sandbox-vm-export"),
        )
        .expect("compile should not throw");
        assert!(
            output.diagnostics.iter().all(|d| d.severity != "error"),
            "unexpected diagnostics: {:#?}",
            output.diagnostics
        );
        let bytecode = output.bytecode.expect("bytecode should be emitted");
        let ops = all_instruction_ops(&bytecode);
        assert!(ops.contains(&"i64.checked_div"));
    }

    #[test]
    fn filesystem_fixture_is_profile_rejected() {
        let output = compile_to_sandbox_bytecode(
            &fixture("10-sandbox-reject-filesystem"),
            Some("sandbox-vm-export"),
        )
        .expect("compile should not throw");
        assert!(output.bytecode.is_none());
        assert!(
            output
                .diagnostics
                .iter()
                .any(|diagnostic| diagnostic.phase == "profile"
                    && diagnostic.kind == "sandbox_profile_rejected"),
            "expected filesystem profile rejection, got {:#?}",
            output.diagnostics
        );
    }

    #[test]
    fn native_ffi_is_profile_rejected() {
        assert_profile_rejection(
            r#"
extern "rt" {
    fn hew_datetime_now_ms() -> i64;
}

fn main() {
    println("ffi declaration");
}
"#,
            "Unsupported::NATIVE_ONLY",
        );
    }

    #[test]
    fn unsafe_block_is_profile_rejected() {
        assert_profile_rejection(
            r#"
fn main() {
    unsafe {
        println("unsafe");
    }
}
"#,
            "unsafe_rejected",
        );
    }

    #[test]
    fn user_resource_type_is_profile_rejected_w3030() {
        // W3.030 V15 — `#[resource]` types carry an implicit drop contract
        // that dispatches `<T>::close` on every scope-exit path through the
        // unified W3.021 `ScopeExitPlan` stream. The sandbox-WASM bytecode
        // export does not yet model that drop scheduling (the W3.021
        // `defer_rejected` follow-up tracks the same gap). The profile
        // checker must emit a named `user_resource_close_not_yet_admitted_sandbox`
        // diagnostic at the `#[resource]` declaration so the sandbox never
        // silently misses a resource close.
        assert_profile_rejection(
            r#"
#[resource]
type Conn {
    fd: i64
}
fn main() {
    println("done");
}
"#,
            "user_resource_close_not_yet_admitted_sandbox",
        );
    }

    #[test]
    fn while_let_is_admitted_by_profile() {
        // while-let is no longer reserved; the profile checker walks the body
        // and the emitter lowers Constructor patterns.
        set_test_hewpath();
        let output = compile_to_sandbox_bytecode(
            r"
fn main() {
    var current = Some(1);
    while let Some(value) = current {
        println(value);
        current = None;
    }
}
",
            Some("sandbox-vm-export"),
        )
        .expect("compile should not throw");
        assert!(
            output
                .diagnostics
                .iter()
                .all(|d| d.kind != "reserved_control_flow"),
            "while-let should no longer produce a reserved_control_flow diagnostic; got {:#?}",
            output.diagnostics
        );
        assert!(
            output.bytecode.is_some(),
            "while-let should produce bytecode now; diagnostics: {:#?}",
            output.diagnostics
        );
    }

    #[test]
    fn broader_string_methods_remain_catalogued_divergence() {
        assert_profile_rejection(
            r#"
fn main() {
    println("aba".replace("a", "x"));
}
"#,
            "unknown_method_symbol",
        );
    }

    #[test]
    fn machine_generics_are_reserved_runtime_feature() {
        assert_profile_rejection(
            r#"
machine Boxed<T> {
    event Store;
    state Idle;
    state Full;
    on Store: Idle -> Full;
    on Store: Full -> Full;
}

fn main() {
    println("machine generic");
}
"#,
            "reserved_runtime_feature",
        );
    }

    #[test]
    fn is_operator_is_reserved_runtime_feature() {
        assert_profile_rejection(
            r#"
type Node {
    value: i64;
}

fn same(left: Node, right: Node) -> bool {
    left is right
}

fn main() {
    println("identity compare");
}
"#,
            "reserved_runtime_feature",
        );
    }

    #[test]
    fn bytecode_output_is_stable_for_same_source() {
        let source = fixture("01-hello-world");
        let first = compile_to_sandbox_bytecode(&source, Some("sandbox-vm-export"))
            .expect("first compile should not throw")
            .bytecode
            .expect("first compile should emit bytecode");
        let second = compile_to_sandbox_bytecode(&source, Some("sandbox-vm-export"))
            .expect("second compile should not throw")
            .bytecode
            .expect("second compile should emit bytecode");
        assert_eq!(
            serde_json::to_string(&first).expect("first bytecode should serialize"),
            serde_json::to_string(&second).expect("second bytecode should serialize")
        );
    }

    #[test]
    fn parse_or_type_errors_do_not_throw() {
        let output = compile_to_sandbox_bytecode(
            "fn main() { let x: i64 = \"not an int\"; }",
            Some("sandbox-vm-export"),
        )
        .expect("compile diagnostics should be returned, not thrown");
        assert!(output.bytecode.is_none());
        assert!(output.diagnostics.iter().any(|d| d.phase == "typecheck"));
    }

    // Source matches hew-lsp/tests/fixtures/v05_impl_where_clause.hew.
    const IMPL_WHERE_SOURCE: &str = r"
type Holder<T> {
    value: T;
}

impl<T> Holder<T> where T: Display {
    fn show(holder: Holder<T>) {
        println(holder.value);
    }
}

fn main() {
    let holder = Holder { value: 7 };
    holder.show();
}
";

    #[test]
    fn impl_where_clause_is_reserved_runtime_feature() {
        assert_profile_rejection(IMPL_WHERE_SOURCE, "reserved_runtime_feature");
    }

    fn assert_profile_rejection(source: &str, expected_kind: &str) {
        set_test_hewpath();
        let output = compile_to_sandbox_bytecode(source, Some("sandbox-vm-export"))
            .expect("compile should not throw");
        assert!(
            output.bytecode.is_none(),
            "profile rejection should not emit bytecode; got {:#?}",
            output.bytecode
        );
        assert!(
            output.diagnostics.iter().any(|diagnostic| {
                diagnostic.phase == "profile" && diagnostic.kind == expected_kind
            }),
            "expected profile diagnostic kind {expected_kind:?}, got {:#?}",
            output.diagnostics
        );
    }

    fn all_instruction_ops(bytecode: &SandboxBytecodePackage) -> std::collections::BTreeSet<&str> {
        bytecode
            .functions
            .iter()
            .flat_map(|function| &function.blocks)
            .flat_map(|block| &block.instructions)
            .map(|instruction| instruction.op.as_str())
            .collect()
    }

    #[test]
    fn simple_for_range_lowers_to_bytecode() {
        set_test_hewpath();
        let output = compile_to_sandbox_bytecode(
            r"
fn main() {
    for i in 0..3 {
        println(i);
    }
}
",
            Some("sandbox-vm-export"),
        )
        .expect("compile should not throw");
        assert!(
            output.diagnostics.iter().all(|d| d.severity != "error"),
            "unexpected error diagnostics: {:?}",
            output.diagnostics
        );
        let bc = output
            .bytecode
            .as_ref()
            .expect("for-range should produce bytecode");
        let main_fn = bc
            .functions
            .iter()
            .find(|f| f.name == "main")
            .expect("main");
        assert!(
            main_fn.blocks.len() > 1,
            "for-loop should produce multiple blocks, got: {:?}",
            main_fn.blocks.iter().map(|b| &b.id).collect::<Vec<_>>()
        );
    }
}
