//! WASM bindings for compiling Hew source into sandbox VM bytecode packages.
//!
//! This crate is separate from `hew-wasm`: `hew-wasm` remains the browser
//! analysis-only crate, while this crate owns the sandbox bytecode export tier.

mod bytecode;
mod emit;
mod profile;

use bytecode::{Block, Capability, Instruction, Local, Operand, StdlibSymbol, Terminator};
use serde::{Deserialize, Serialize};

pub use bytecode::SandboxBytecodePackage;
pub use profile::{canonical_profile, DEFAULT_PROFILE_ALIAS, DEFAULT_PROFILE_CANONICAL};

/// The required native↔sandbox parity-case names — the teeth of the parity
/// ratchet (see `tests/parity.rs` and `tests/parity_ratchet.rs`).
///
/// A construct is "runnable in the sandbox" iff it is pinned to a name in this
/// set with a green stdout+exit parity case under `HEW_SEED=42`. This is the
/// single source of truth so the parity harness and the allowlist-coverage
/// ratchet cannot drift: both test binaries reference this `const`.
///
/// The set only grows. Every graduation lane that admits a new construct adds
/// its name here in the same commit as its parity case.
pub const REQUIRED_PARITY_TEST_NAMES: &[&str] = &[
    "hello_world",
    "fibonacci",
    "function_composition",
    "pattern_matching",
    "collections",
    "record_types",
    "structural_records",
    "counter_actor",
    "actor_pipeline",
    "supervisor",
    "traffic_light",
    "arithmetic_operators",
    "array_indexing",
    "string_slicing",
    "while_loop",
    "wildcard_match",
    // Float arithmetic now runs through the type-directed f64.* opcode family;
    // statement-position if/match/if-let now lower and run. Each name is backed
    // by a `parity.rs` case asserting full stdout+exit parity under HEW_SEED=42.
    "float_arithmetic",
    "float_division",
    "float_nonfinite_compare",
    "mixed_numeric",
    "stmt_if",
    "stmt_match",
    "stmt_if_let",
    // Value-position if-let now joins the matched/else arm values on a result
    // local, so `let v = if let .. { x } else { y }` yields the matched value
    // (not unit). Backed by the if_let_value parity case.
    "if_let_value",
    // Fieldless-enum `==`/`!=`: the emitter routes `BinaryOp::Equal`/`NotEqual`
    // on enum operands through `cmp.eq`/`cmp.ne`; the VM's `compare` handler
    // uses `canonicalComparable` which serialises the enum tag + empty payload
    // to JSON for equality testing. Admitted by the checker in #1987.
    "fieldless_enum_eq",
    "match_guard_parity",
    "match_guard_catch_all_fallthrough",
    // Structural `==`/`!=` on user-defined record types and payload enums.
    // Profile now admits these; canonicalComparable handles records structurally.
    "record_equality",
    // `clone expr` produces an independent deep copy via local.set cloneValue.
    "clone_value",
    // Compound assignment (`+=`, `-=`, `*=`, `/=`, `%=`) for i64 and f64.
    "compound_assign",
    // Non-finite f64 rendering: overflow → `inf` / `-inf`, 0/0 → `nan`.
    // Pins the sandbox VM's renderF64 path against native printf %g.
    "f64_nonfinite_render",
    // Finite f64 rendering: negative zero, %g fixed/scientific thresholds,
    // 6-sig-fig rounding. Pins renderF64's %g-equivalent finite path.
    "f64_finite_render",
    // Tuple construction and positional let-destructure lowered as anonymous
    // records with fields _0, _1, … using record.new / record.get.
    "tuple_values",
    // `==` on generic records and aggregates containing Vec<T>: the checker
    // resolves eq eligibility after generic substitution; the emitter routes
    // through cmp.eq; the VM's canonicalComparable handles records recursively.
    "generic_aggregate_eq",
    // Option/Result marker methods: is_some/is_none/is_ok/is_err/unwrap/unwrap_or.
    // The emitter now lowers these via enum.tag/enum.payload sequences.
    "option_result_methods",
    // f-string interpolation for all canonical integer and char primitives that
    // gained Display impls: i8/i16/i32, u8/u16/u32, u64, isize, usize, char.
    "display_scalars",
    // `#[wire]` type declaration with tagged fields now uses the sole canonical
    // surface (`#[wire]` attribute on TypeDecl; bare `wire` keyword removed).
    // Verifies the sandbox profile and emitter treat `#[wire]` TypeDecl as a
    // plain struct without rejecting the attribute or the optional-field tag.
    "wire_types_declaration",
    // Vec<T>::contains (linear equality scan) and v[start..end] (range slice).
    // Both map to new sandbox VM opcodes `vector.contains` / `vector.range_slice`
    // added in this parity sweep.
    "vec_operations",
    // v[start..=end] inclusive range slice: the emitter computes the exclusive
    // end (`end + 1`) and delegates to the existing `vector.range_slice` opcode,
    // so no new VM opcode is needed.  Pins end-inclusive semantics against native.
    "vec_inclusive_slice",
    // `rec.clone()` method call on a user-defined record type: the emitter lowers
    // it as `local.set` which calls `cloneValue` in the VM (deep recursive copy).
    // Verifies independence: the clone and the original do not alias.
    "record_clone",
    // `(rec.f)(args)` fn-field call: a record field holding a function value is
    // callable via `call.indirect`.  The emitter materialises the function via
    // `const.function` and retrieves it from the record via `record.get`.
    "fn_field_call",
    // Vec<f64>::contains with NaN and +-Infinity follows native fcmp-OEQ: NaN is
    // never found, +Inf != -Inf.  Uses the shared valuesEqual helper introduced
    // to align vector.contains with cmp.eq (was: collapsed to null via JSON).
    "vec_f64_nonfinite_contains",
    // CAP-16 sandbox graduation: boolean not, scalar literal match, record
    // functional update/pattern destructure, builtin Option construction, and
    // const item references now lower and execute at native parity.
    "bool_not",
    "scalar_match_int",
    "scalar_match_string",
    "bool_match",
    "struct_functional_update",
    "struct_pattern_match",
    "option_some_none",
    "const_reference",
    "logical_binary_operators",
    "bitwise_binary_operators",
    "compound_bitwise_assign",
    "shift_out_of_range",
    "struct_destructure_let",
    "record_shorthand_destructure_let",
    "nested_tuple_destructure_let",
    "wrapping_binary_operators",
    // `.clone()` method-call syntax on Vec/String/Array/Slice — an allowlist
    // gap: the emitter's generic `local.set` → `cloneValue` clone arm already
    // handled these receiver types, but the profile only admitted `clone` on
    // user-defined records. Verifies independence (mutating the original
    // after clone does not affect the copy) across all four types.
    "method_clone",
    // `regex.Pattern::clone()` — same allowlist gap, pinned separately since
    // its source lives outside the curated playground manifest scope (see
    // `regex_clone` in tests/parity.rs).
    "regex_clone",
    // AST-emitter residuals: array repeat, checker-admitted scalar casts, and
    // Result/Option postfix-try now execute at native parity.
    "trap_residual",
];

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
    fn match_arm_guard_gates_arm_body() {
        set_test_hewpath();
        let source = r#"
enum Score { High(i64); Low(i64); Zero; }

fn classify(s: Score) -> string {
    match s {
        High(n) if n > 90 => "excellent",
        High(_) => "good",
        Low(n) if n < 10 => "very low",
        Low(_) => "low",
        Zero => "zero",
    }
}

fn main() {
    println(classify(High(95)));
    println(classify(High(70)));
    println(classify(Low(5)));
    println(classify(Low(40)));
    println(classify(Zero));
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
        let classify = bytecode
            .functions
            .iter()
            .find(|function| function.name.contains("classify"))
            .expect("classify function should be emitted");
        let guard_blocks = classify
            .blocks
            .iter()
            .filter(|block| block.id.contains("match_guard_"))
            .count();
        assert_eq!(
            guard_blocks, 2,
            "guarded arms must lower to explicit guard blocks: {:#?}",
            classify.blocks
        );
        let br_if_count = classify
            .blocks
            .iter()
            .filter(|block| block.terminator.op == "br_if")
            .count();
        assert!(
            br_if_count > 5,
            "guarded match must branch for arm guards as well as arm patterns: {:#?}",
            classify.blocks
        );
    }

    #[test]
    fn guarded_catch_all_guard_failure_falls_through_to_next_check() {
        set_test_hewpath();
        let source = r#"
enum Score { High(i64); Low(i64); }

fn classify(s: Score) -> string {
    match s {
        High(n) if n > 90 => "excellent",
        _ if false => "never",
        _ => "fallback",
    }
}

fn main() {
    println(classify(Low(5)));
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
        let classify = bytecode
            .functions
            .iter()
            .find(|function| function.name.contains("classify"))
            .expect("classify function should be emitted");
        let guard_block = classify
            .blocks
            .iter()
            .find(|block| block.id.contains("match_guard_1"))
            .expect("guarded catch-all arm should emit a guard block");

        assert_ne!(
            guard_block.terminator.else_target.as_deref(),
            Some(guard_block.id.as_str()),
            "guard failure must not loop back to the same guard block: {:#?}",
            classify.blocks
        );
        assert!(
            guard_block
                .terminator
                .else_target
                .as_deref()
                .is_some_and(|target| target.contains("match_check_2")),
            "guard failure must fall through to the next check block: {:#?}",
            classify.blocks
        );
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
    fn crypto_random_bytes_is_rejected_with_platform_limitation() {
        // crypto.random_bytes(n) — the call form — depends on a native-only entropy
        // source absent from the wasm32 link set.  The sandbox profile gate
        // (Expr::MethodCall) emits a PlatformLimitation diagnostic for the call
        // form; the value-position form (FieldAccess) is covered by a separate test.
        set_test_hewpath();
        let source = "import std::crypto::crypto;\nfn main() { let _ = crypto.random_bytes(32); }";
        let output = compile_to_sandbox_bytecode(source, Some("sandbox-vm-export"))
            .expect("compile should not throw");
        assert!(
            output.bytecode.is_none(),
            "crypto.random_bytes must not produce bytecode in the sandbox; got {:#?}",
            output.bytecode
        );
        assert!(
            output.diagnostics.iter().any(|d| {
                d.severity == "error"
                    && d.kind == "PlatformLimitation"
                    && d.message.contains("random_bytes")
            }),
            "expected PlatformLimitation error for crypto.random_bytes, got {:#?}",
            output.diagnostics
        );
    }

    #[test]
    fn sandbox_rejects_value_position_crypto_random_bytes() {
        // `let f = crypto.random_bytes` is a FieldAccess (not a MethodCall) and
        // must be rejected in the browser sandbox with a PlatformLimitation
        // diagnostic, matching the call-form rejection and the native wasm32
        // target guard.  Without the profile gate fix the FieldAccess arm would
        // silently pass through, producing bytecode that the VM cannot execute
        // securely (the entropy source is absent from the wasm32 link set).
        set_test_hewpath();
        let source = "import std::crypto::crypto;\nfn main() { let f = crypto.random_bytes; }";
        let output = compile_to_sandbox_bytecode(source, Some("sandbox-vm-export"))
            .expect("compile should not throw");
        assert!(
            output.bytecode.is_none(),
            "value-position crypto.random_bytes must not produce bytecode; got {:#?}",
            output.bytecode
        );
        assert!(
            output.diagnostics.iter().any(|d| {
                d.severity == "error"
                    && d.kind == "PlatformLimitation"
                    && d.message.contains("random_bytes")
            }),
            "expected PlatformLimitation error for value-position crypto.random_bytes, got {:#?}",
            output.diagnostics
        );
    }

    #[test]
    fn sandbox_rejects_value_position_net_module() {
        // `let f = net.connect` is a value-position FieldAccess on the native-only
        // `net` stdlib module.  The sandbox must reject with PlatformLimitation,
        // matching the wasm32 target guard in the type checker and maintaining
        // native/wasm parity for the full native-only module set.
        set_test_hewpath();
        let source = "import std::net::net;\nfn main() { let f = net.connect; }";
        let output = compile_to_sandbox_bytecode(source, Some("sandbox-vm-export"))
            .expect("compile should not throw");
        assert!(
            output.bytecode.is_none(),
            "value-position net.connect must not produce bytecode; got {:#?}",
            output.bytecode
        );
        assert!(
            output.diagnostics.iter().any(|d| {
                d.severity == "error" && d.kind == "PlatformLimitation" && d.message.contains("net")
            }),
            "expected PlatformLimitation error for value-position net.connect, got {:#?}",
            output.diagnostics
        );
    }

    #[test]
    fn sandbox_admits_value_position_user_record_field_access() {
        // `let v = rec.x` where `rec` is a user-defined record must NOT be
        // rejected: the native-only guard is gated on the object being one of the
        // specific stdlib module short-names and NOT a user-declared type, so
        // legitimate field access on user records must continue to compile.
        let source = r"
type Point { x: i64; y: i64; }

fn main() {
    let p = Point { x: 3, y: 7 };
    let v = p.x;
    println(v);
}
";
        let output = compile_to_sandbox_bytecode(source, Some("sandbox-vm-export"))
            .expect("compile should not throw");
        assert!(
            output.diagnostics.iter().all(|d| d.severity != "error"),
            "user record field access must not produce error diagnostics; got {:#?}",
            output.diagnostics
        );
        assert!(
            output.bytecode.is_some(),
            "user record field access must emit bytecode; diagnostics {:#?}",
            output.diagnostics
        );
    }

    #[test]
    fn sandbox_admits_local_binding_named_net() {
        // A local let-binding whose name collides with the native-only module
        // short-name `net` must NOT be rejected: the guard fires only when the
        // object resolves to a stdlib module identifier (ty_for_expr returns None),
        // not when it resolves to a typed local variable.
        //
        // Over-reject regression: the previous `is_user_local` guard only tracked
        // top-level declarations and would have falsely rejected `net.connect` here.
        let source = r"
type Conn { connect: i64; }

fn main() {
    let net = Conn { connect: 42 };
    println(net.connect);
}
";
        let output = compile_to_sandbox_bytecode(source, Some("sandbox-vm-export"))
            .expect("compile should not throw");
        assert!(
            output.diagnostics.iter().all(|d| d.severity != "error"),
            "local binding named `net` must not trigger PlatformLimitation; got {:#?}",
            output.diagnostics
        );
        assert!(
            output.bytecode.is_some(),
            "local binding named `net` must emit bytecode; diagnostics {:#?}",
            output.diagnostics
        );
    }

    #[test]
    fn sandbox_admits_local_binding_named_stream() {
        // Same regression check for `stream` (another common collision name).
        let source = r"
type Packet { value: i64; }

fn process(stream: Packet) -> i64 {
    stream.value
}

fn main() {
    let pkt = Packet { value: 7 };
    println(process(pkt));
}
";
        let output = compile_to_sandbox_bytecode(source, Some("sandbox-vm-export"))
            .expect("compile should not throw");
        assert!(
            output.diagnostics.iter().all(|d| d.severity != "error"),
            "function param named `stream` must not trigger PlatformLimitation; got {:#?}",
            output.diagnostics
        );
        assert!(
            output.bytecode.is_some(),
            "function param named `stream` must emit bytecode; diagnostics {:#?}",
            output.diagnostics
        );
    }

    #[test]
    fn sandbox_admits_local_binding_named_os() {
        // Same regression check for `os`.
        let source = r"
type Cfg { name: i64; }

fn main() {
    let os = Cfg { name: 1 };
    println(os.name);
}
";
        let output = compile_to_sandbox_bytecode(source, Some("sandbox-vm-export"))
            .expect("compile should not throw");
        assert!(
            output.diagnostics.iter().all(|d| d.severity != "error"),
            "local binding named `os` must not trigger PlatformLimitation; got {:#?}",
            output.diagnostics
        );
        assert!(
            output.bytecode.is_some(),
            "local binding named `os` must emit bytecode; diagnostics {:#?}",
            output.diagnostics
        );
    }

    #[test]
    fn sandbox_native_only_module_set_drift_guard() {
        // Drift guard: every module in the authoritative NATIVE_ONLY_WASM_MODULES
        // const must be rejected by the sandbox in value-position.  This test
        // fails if a new module is added to the const but not handled by the
        // profile gate, or vice-versa.
        //
        // Structural drift is architecturally impossible: the profile gate uses
        // `hew_types::NATIVE_ONLY_WASM_MODULES.contains()` directly, so any change
        // to the const is automatically reflected.  This test verifies:
        //   1. The const has the expected number of entries (catches silent additions).
        //   2. Each expected short-name appears in the const.
        //   3. End-to-end rejection for the three modules with well-known import
        //      paths (net, tls, dns) as a representative sample.
        let expected: &[&str] = &[
            "stream",
            "http",
            "net",
            "process",
            "tls",
            "quic",
            "dns",
            "os",
            "encrypt",
            "sign",
            "http_client",
            "smtp",
        ];
        assert_eq!(
            expected.len(),
            hew_types::NATIVE_ONLY_WASM_MODULES.len(),
            "NATIVE_ONLY_WASM_MODULES length changed; update this expected list"
        );
        for name in expected {
            assert!(
                hew_types::NATIVE_ONLY_WASM_MODULES.contains(name),
                "expected `{name}` to be in NATIVE_ONLY_WASM_MODULES"
            );
        }
        // End-to-end: sample three modules with well-known import paths.
        set_test_hewpath();
        let sample: &[(&str, &str, &str)] = &[
            ("net", "std::net::net", "connect"),
            ("tls", "std::net::tls", "connect"),
            ("dns", "std::net::dns", "resolve"),
        ];
        for (module, import_path, func) in sample {
            let source = format!("import {import_path};\nfn main() {{ let f = {module}.{func}; }}");
            let output = compile_to_sandbox_bytecode(&source, Some("sandbox-vm-export"))
                .expect("compile should not throw");
            let has_platform_error = output
                .diagnostics
                .iter()
                .any(|d| d.severity == "error" && d.kind == "PlatformLimitation");
            assert!(
                has_platform_error,
                "value-position `{module}.{func}` must produce PlatformLimitation; got {:#?}",
                output.diagnostics
            );
        }
    }

    #[test]
    fn clone_prefix_is_admitted_and_emits_bytecode() {
        // `clone x` is now admitted by the profile and lowered by the emitter.
        // The emitter writes the operand into a fresh temp via `local.set`, which
        // calls `cloneValue` in the VM — a deep recursive copy.
        let output = compile_to_sandbox_bytecode(
            r#"
fn main() {
    let original = "hi";
    let duplicate = clone original;
    println(duplicate);
}
"#,
            Some("sandbox-vm-export"),
        )
        .expect("compile_to_sandbox_bytecode should not throw");
        let no_errors = output.diagnostics.iter().all(|d| d.severity != "error");
        assert!(
            no_errors,
            "clone prefix should not produce error diagnostics; got:\n{:#?}",
            output.diagnostics
        );
        assert!(
            output.bytecode.is_some(),
            "clone prefix should emit bytecode; diagnostics:\n{:#?}",
            output.diagnostics
        );
    }

    #[test]
    fn structural_eq_is_admitted_and_emits_bytecode() {
        // Structural `==`/`!=` on records and payload enums is now admitted.
        // `lower_binary` emits `cmp.eq`/`cmp.ne`; the VM's `canonicalComparable`
        // handles these structurally.
        let record_source = r"
type Point { x: i64; y: i64; }

fn main() {
    let a = Point { x: 1, y: 2 };
    let b = Point { x: 1, y: 2 };
    println(a == b);
}
";
        let output = compile_to_sandbox_bytecode(record_source, Some("sandbox-vm-export"))
            .expect("compile_to_sandbox_bytecode should not throw");
        let no_errors = output.diagnostics.iter().all(|d| d.severity != "error");
        assert!(
            no_errors,
            "record equality should not produce error diagnostics; got:\n{:#?}",
            output.diagnostics
        );
        assert!(
            output.bytecode.is_some(),
            "record equality should emit bytecode; diagnostics:\n{:#?}",
            output.diagnostics
        );

        let enum_source = r"
enum Shape {
    Circle(i64);
    Empty;
}

fn main() {
    let a: Shape = Circle(1);
    let b: Shape = Circle(1);
    println(a == b);
}
";
        let output = compile_to_sandbox_bytecode(enum_source, Some("sandbox-vm-export"))
            .expect("compile_to_sandbox_bytecode should not throw");
        let no_errors = output.diagnostics.iter().all(|d| d.severity != "error");
        assert!(
            no_errors,
            "payload enum equality should not produce error diagnostics; got:\n{:#?}",
            output.diagnostics
        );
        assert!(
            output.bytecode.is_some(),
            "payload enum equality should emit bytecode; diagnostics:\n{:#?}",
            output.diagnostics
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
    events {
        Store;
    }
    state Idle;
    state Full;
    on Store: Idle => Full;
    on Store: Full => Full;
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
    fn open_ended_slice_ranges_are_profile_rejected() {
        // The emitter only lowers a slice when both endpoints are present. These
        // forms must fail at the profile gate, not emit an unsupported opcode.
        assert_profile_rejection(
            r"
fn main() {
    let v: Vec<i64> = Vec::new();
    let all = v[..];
    let tail = v[1..];
    let prefix = v[..2];
    println(all.len() + tail.len() + prefix.len());
}
",
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

    /// Collect the first-argument `Value` of every `const.i64` instruction.
    fn const_i64_operand_values(bytecode: &SandboxBytecodePackage) -> Vec<serde_json::Value> {
        bytecode
            .functions
            .iter()
            .flat_map(|function| &function.blocks)
            .flat_map(|block| &block.instructions)
            .filter(|instruction| instruction.op == "const.i64")
            .filter_map(|instruction| instruction.args.first())
            .map(|operand| operand.value.clone())
            .collect()
    }

    #[test]
    fn large_i64_literal_is_string_encoded_to_survive_json_transport() {
        // Any i64 outside the JS safe-integer range (|n| > 2^53 - 1) must be
        // emitted as a decimal *string* operand, otherwise it loses precision
        // when the JS VM re-parses the bytecode via JSON.parse (doubles).
        set_test_hewpath();
        let source = r#"
fn main() {
    let big = 9223372036854775807;
    let neg = -9223372036854775808;
    println("large literals");
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
        let values = const_i64_operand_values(&bytecode);
        assert!(
            values
                .iter()
                .any(|v| v == &serde_json::json!("9223372036854775807")),
            "i64::MAX must be string-encoded, got: {values:#?}"
        );
        assert!(
            values
                .iter()
                .any(|v| v == &serde_json::json!("-9223372036854775808")),
            "i64::MIN must be string-encoded, got: {values:#?}"
        );
        // Regression guard: the raw JSON must not contain the imprecise
        // number-encoded form of i64::MAX.
        let serialized = serde_json::to_string(&bytecode).expect("serialize");
        assert!(
            !serialized.contains(":9223372036854775807,")
                && !serialized.contains(":9223372036854775807}"),
            "i64::MAX must not appear as a bare JSON number operand"
        );
    }

    #[test]
    fn small_i64_literal_stays_numeric() {
        // In-safe-range values stay compact JSON numbers so common-case
        // bytecode remains small and diff-friendly.
        set_test_hewpath();
        let source = r#"
fn main() {
    let small = 42;
    let boundary = 9007199254740991;
    println("small literals");
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
        let values = const_i64_operand_values(&bytecode);
        assert!(
            values.iter().any(|v| v == &serde_json::json!(42)),
            "small literal must stay numeric, got: {values:#?}"
        );
        assert!(
            values
                .iter()
                .any(|v| v == &serde_json::json!(9_007_199_254_740_991_i64)),
            "safe-int boundary must stay numeric, got: {values:#?}"
        );
        assert!(
            !values.iter().any(|v| v == &serde_json::json!("42")),
            "small literal must not be string-encoded, got: {values:#?}"
        );
    }

    #[test]
    fn supervisor_i64_bounds_are_tagged_in_serialized_bytecode() {
        set_test_hewpath();
        let source = r"
actor Bounds {
    let max: i64;
    let min: i64;
}

supervisor BoundsTree {
    strategy: one_for_one;
    intensity: 1 within 60s;
    child bounds: Bounds(max: 9223372036854775807, min: -9223372036854775808);
}

fn main() {
    let tree = spawn BoundsTree;
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
        let serialized = serde_json::to_string(&bytecode).expect("bytecode should serialize");
        let decoded: SandboxBytecodePackage =
            serde_json::from_str(&serialized).expect("serialized bytecode should deserialize");
        let args = &decoded.layouts.supervisors[0].children[0].start_spec.args;
        assert_eq!(
            args,
            &[
                serde_json::json!({ "kind": "i64", "value": "9223372036854775807" }),
                serde_json::json!({ "kind": "i64", "value": "-9223372036854775808" }),
            ],
            "supervisor child i64 bounds must retain their tagged decimal form"
        );
        assert!(
            !serialized.contains(":9223372036854775807")
                && !serialized.contains(":-9223372036854775808"),
            "supervisor i64 bounds must not be serialized as bare JSON numbers"
        );
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

    // --- Browser WASM export ---

    #[test]
    fn compile_to_sandbox_bytecode_wasm_export_returns_valid_json() {
        // The wasm-bindgen export `compile_to_sandbox_bytecode_js` must
        // return a JSON-serialized CompileOutput. The TS consumer in
        // hew-sandbox-vm/src/interpreter/run-program.ts calls
        // `compiler(source, profile)` and parses the result as CompileOutput.
        // This test exercises the JSON serialization path on native so CI can
        // verify the bridge code compiles and produces the expected schema
        // without requiring a full wasm-pack + Node.js environment.
        set_test_hewpath();
        let json = compile_to_sandbox_bytecode_json(
            "fn main() { println(\"hello\"); }",
            "sandbox-vm-export",
        );
        assert!(!json.is_empty(), "JSON output must not be empty");
        let output: serde_json::Value =
            serde_json::from_str(&json).expect("output must be valid JSON");
        assert!(
            output.get("diagnostics").is_some(),
            "output must have a 'diagnostics' field; got: {output}"
        );
    }

    // --- Fail-closed: indirect calls must ALL be rejected at the profile
    //     level regardless of argument count. ---

    #[test]
    fn unknown_module_call_with_args_is_profile_rejected() {
        // A call through a module-qualified name that is NOT in the sandbox
        // allowlist must be rejected regardless of how many arguments it has.
        // This covers the general case that any non-admitted call form is blocked
        // at the profile layer before bytecode emission.
        // Note: the `_` fallthrough in check_call (triggered by callee expressions
        // that are not Identifier or FieldAccess) previously had an `args.is_empty()`
        // guard, meaning non-zero-arg indirect calls slipped past. After removing
        // that guard, all non-admitted call forms are rejected. In v0.5 the `_` arm
        // with args is hard to reach through a type-checked program (callable types
        // are not supported), but the guard removal ensures defense-in-depth for
        // any future callable-type surface.
        set_test_hewpath();
        // `fs.read("path.txt")` with `import std::fs` — uses a native-only stdlib
        // function so the call type-checks and reaches the profile gate.
        // The native-only surface is rejected at the profile level.
        assert_profile_rejection(
            r#"
import std::fs;

fn main() {
    fs.read("path.txt");
}
"#,
            "sandbox_profile_rejected",
        );
    }

    // --- Statement-position if/match/if-let are admitted and lower to bytecode. ---
    //
    // These side-effecting control-flow statements (branches that return unit)
    // were previously trapped by a blanket profile rejection. They now lower
    // through `lower_stmt_if` / `lower_stmt_match` / `lower_stmt_if_let`, so the
    // profile must admit them and the emitter must produce bytecode with branch
    // structure (more than the single entry block).

    fn assert_admits_with_branches(source: &str) {
        set_test_hewpath();
        let output = compile_to_sandbox_bytecode(source, Some("sandbox-vm-export"))
            .expect("compile should not throw");
        assert!(
            output.diagnostics.iter().all(|d| d.severity != "error"),
            "statement-position control flow must be admitted, got error diagnostics: {:#?}",
            output.diagnostics
        );
        let bc = output
            .bytecode
            .as_ref()
            .expect("admitted statement-position control flow should emit bytecode");
        let main_fn = bc
            .functions
            .iter()
            .find(|f| f.name == "main")
            .expect("main");
        assert!(
            main_fn.blocks.len() > 1,
            "statement-position control flow should lower to multiple blocks, got: {:?}",
            main_fn.blocks.iter().map(|b| &b.id).collect::<Vec<_>>()
        );
    }

    #[test]
    fn statement_position_if_is_admitted() {
        // The `if` is a non-tail statement (the `return` after it prevents the
        // parser from promoting it to a `trailing_expr`), so it stays `Stmt::If`
        // and exercises the statement-position lowering path.
        assert_admits_with_branches(
            r#"
fn main() {
    let x: i64 = 1;
    if x == 1 {
        println("yes");
    }
    return;
}
"#,
        );
    }

    #[test]
    fn statement_position_match_is_admitted() {
        // A statement after the `match` keeps it in statement position.
        assert_admits_with_branches(
            r#"
fn main() {
    let x: i64 = 2;
    match x {
        1 => println("one"),
        _ => println("other"),
    }
    return;
}
"#,
        );
    }

    #[test]
    fn statement_position_if_let_is_admitted() {
        // A statement after `if let` keeps it in statement position. The pattern
        // is a payload-carrying constructor (`Value(n)`), which is the shape
        // `lower_stmt_if_let` lowers into a tag check + branch.
        assert_admits_with_branches(
            r"
enum Wrapped { Value(i64); Empty; }
fn main() {
    let w: Wrapped = Value(7);
    if let Value(n) = w {
        println(n);
    }
    return;
}
",
        );
    }

    #[test]
    fn early_return_in_receive_handler_is_admitted() {
        // The sandbox scheduler's trampoline fallback (scheduler.ts `stepActor`)
        // delivers the reply after `invoke()` returns, so early `return` in a
        // receive handler is now semantically safe and must be admitted by the
        // profile.  Mirrors native: the native dispatch trampoline always calls
        // `hew_reply()` after the handler function returns regardless of where
        // the return landed (llvm.rs:24730-24784).
        //
        // This test uses a handler with an unconditional early `return` (the
        // simplest case that previously triggered `return_in_receive_handler_rejected`)
        // to confirm the gate is gone without triggering other profile gates.
        set_test_hewpath();
        let source = r"
actor Echo {
    receive fn echo(n: i64) -> i64 {
        return n;
    }
}
fn main() {
    let e = spawn Echo;
    let r = match await e.echo(42) {
        Ok(v) => v,
        Err(_) => -1,
    };
    println(r);
}
";
        let output = compile_to_sandbox_bytecode(source, Some("sandbox-vm-export"))
            .expect("compile should not throw");
        assert!(
            output.diagnostics.iter().all(|d| d.severity != "error"),
            "early return in receive handler must be admitted; got diagnostics: {:#?}",
            output.diagnostics
        );
        assert!(
            output.bytecode.is_some(),
            "early return in receive handler must emit bytecode"
        );
        // Verify no `return_in_receive_handler_rejected` diagnostic — this gate
        // is intentionally absent; the trampoline fallback carries the guarantee.
        assert!(
            output
                .diagnostics
                .iter()
                .all(|d| d.kind != "return_in_receive_handler_rejected"),
            "return_in_receive_handler_rejected must not fire after trampoline-fallback fix"
        );
    }

    /// A stateful actor whose receive handler early-returns a value that differs
    /// from the current state must reply with that value AND preserve its state.
    ///
    /// Without the emitter fix, `bump`'s bytecode contains no `actor.reply` — the
    /// scheduler falls back to using the return value as BOTH reply and next-state,
    /// silently corrupting the actor's `count` field.
    ///
    /// With the fix, the emitter mirrors the normal-exit path: it emits
    /// `actor.reply(reply_token, expr)` followed by `ret(<state record>)`.
    #[test]
    fn stateful_early_return_handler_emits_actor_reply_in_bytecode() {
        set_test_hewpath();
        let source = r"
actor Counter {
    let count: i64;
    receive fn bump(n: i64) -> i64 { return n; }
    receive fn get() -> i64 { return count; }
}
fn main() {
    let c = spawn Counter(count: 100);
    println(match await c.bump(5) { Ok(v) => v, Err(_e) => 0-1 });
    println(match await c.get()   { Ok(v) => v, Err(_e) => 0-1 });
}
";
        let output = compile_to_sandbox_bytecode(source, Some("sandbox-vm-export"))
            .expect("compile should not throw");
        assert!(
            output.diagnostics.iter().all(|d| d.severity != "error"),
            "stateful early-return must be admitted; diagnostics: {:#?}",
            output.diagnostics
        );
        let bytecode = output
            .bytecode
            .expect("stateful early-return must emit bytecode");

        // The `bump` handler must contain an `actor.reply` instruction.
        // Before the emitter fix, early-return lowers to `ret([n])` only —
        // no `actor.reply` is emitted — so this assertion will fail.
        let bump_fn = bytecode
            .functions
            .iter()
            .find(|f| f.name.contains("bump"))
            .expect("Counter.bump function must exist in emitted bytecode");
        let has_actor_reply = bump_fn
            .blocks
            .iter()
            .flat_map(|b| b.instructions.iter())
            .any(|instr| instr.op == "actor.reply");
        assert!(
            has_actor_reply,
            "Counter.bump early-return path must emit `actor.reply`; \
             got blocks: {:#?}",
            bump_fn.blocks
        );

        // The `bump` handler must also return the state record (a single field
        // for a one-field actor), NOT the reply expression, as its function
        // return value.  Before the fix the terminator carries `n` (the early-
        // return expr); after the fix it carries the state local (`count`).
        //
        // We verify indirectly: the return value must NOT be one of the
        // message-param locals.  The bump handler params are
        // [reply_token, count, n]; the state return must be the `count` local.
        // Inspect that no block terminator returns a local named after "n" or
        // carrying the same local id as the param `n`.
        let param_n_id = bump_fn
            .params
            .last()
            .expect("bump should have at least one param beyond the reply token");
        let returns_param_n = bump_fn.blocks.iter().any(|b| {
            b.terminator
                .args
                .iter()
                .any(|a| a.value.as_str() == Some(param_n_id.as_str()))
        });
        assert!(
            !returns_param_n,
            "Counter.bump must return state local (not the param `n`) after emitter fix; \
             bump params: {param_n_id:?}, blocks: {:#?}",
            bump_fn.blocks
        );
    }

    /// Multi-field actor with pre-return mutation: the mutation must survive the
    /// early return.  Before the fix, `updateStateFromReturn` receives a scalar
    /// (the reply value) and silently skips the multi-field update path, leaving
    /// state stale.  After the fix, the state record is returned explicitly.
    #[test]
    fn stateful_early_return_multi_field_preserves_pre_return_mutations() {
        set_test_hewpath();
        let source = r"
actor Pair {
    var a: i64;
    let b: i64;
    receive fn set_a_return_b(x: i64) -> i64 {
        a = x;
        return b;
    }
    receive fn get_a() -> i64 { return a; }
}
fn main() {
    let p = spawn Pair(a: 1, b: 99);
    println(match await p.set_a_return_b(42) { Ok(v) => v, Err(_e) => 0-1 });
    println(match await p.get_a()            { Ok(v) => v, Err(_e) => 0-1 });
}
";
        let output = compile_to_sandbox_bytecode(source, Some("sandbox-vm-export"))
            .expect("compile should not throw");
        assert!(
            output.diagnostics.iter().all(|d| d.severity != "error"),
            "multi-field early-return must be admitted; diagnostics: {:#?}",
            output.diagnostics
        );
        let bytecode = output
            .bytecode
            .expect("multi-field early-return must emit bytecode");

        // The `set_a_return_b` handler must contain an `actor.reply` instruction.
        let handler_fn = bytecode
            .functions
            .iter()
            .find(|f| f.name.contains("set_a_return_b"))
            .expect("Pair.set_a_return_b function must exist");
        let has_actor_reply = handler_fn
            .blocks
            .iter()
            .flat_map(|b| b.instructions.iter())
            .any(|instr| instr.op == "actor.reply");
        assert!(
            has_actor_reply,
            "Pair.set_a_return_b early-return path must emit `actor.reply`; \
             got blocks: {:#?}",
            handler_fn.blocks
        );

        // Must also contain a `record.new` for the state record on the early-return
        // path — the multi-field state record is only built after the fix.
        let has_record_new = handler_fn
            .blocks
            .iter()
            .flat_map(|b| b.instructions.iter())
            .any(|instr| instr.op == "record.new");
        assert!(
            has_record_new,
            "Pair.set_a_return_b with two state fields must emit `record.new` for \
             the state record on early-return; got blocks: {:#?}",
            handler_fn.blocks
        );
    }

    #[test]
    fn bare_unit_enum_variant_as_call_arg_emits_enum_new() {
        // Regression: bare unit-variant identifiers in expression position (e.g.
        // `apply(Double, 5)` where Double is a unit variant) were previously
        // lowered as `const.unit` instead of `enum.new`, causing a runtime trap.
        // Verify that passing a unit variant as a call argument emits `enum.new`.
        set_test_hewpath();
        let source = r#"
enum Op { Add; Sub; }
fn apply(op: Op, x: i64, y: i64) -> i64 {
    match op {
        Add => x + y,
        Sub => x - y,
    }
}
fn main() {
    println(f"{apply(Add, 10, 3)}");
    println(f"{apply(Sub, 10, 3)}");
}
"#;
        let output = compile_to_sandbox_bytecode(source, Some("sandbox-vm-export"))
            .expect("compile should not throw");
        assert!(
            output.diagnostics.iter().all(|d| d.severity != "error"),
            "unexpected diagnostics: {:#?}",
            output.diagnostics
        );
        let bytecode = output.bytecode.expect("bytecode must be emitted");
        let ops = all_instruction_ops(&bytecode);
        // `enum.new` must appear: the bare `Add` and `Sub` identifiers used as
        // call arguments must be lowered as unit-variant constructions, not const.unit.
        assert!(
            ops.contains(&"enum.new"),
            "bare unit-variant as call arg must emit enum.new; ops: {ops:?}"
        );
    }

    #[test]
    fn other_profile_gates_still_reject_after_early_return_fix() {
        // Smoke-check: relaxing the receive-handler `return` gate must not
        // accidentally disable any adjacent profile gate.  Uses the same
        // sources as `native_ffi_is_profile_rejected` and
        // `unsafe_block_is_profile_rejected`.
        set_test_hewpath();
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
}
