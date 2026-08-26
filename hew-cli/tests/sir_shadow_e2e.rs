//! End-to-end contracts for the experimental Semantic IR lane.
//!
//! `--sir-shadow` must exercise and verify the temporary SIR → raw-MIR
//! candidate while leaving the established raw-MIR dump byte-for-byte
//! authoritative. `--sir-lower` is intentionally stronger: it must compile a
//! closed SIR call graph without constructing legacy function bodies.

mod support;

use std::fs;
use std::path::Path;
use std::process::{Command, Output};

use support::{assert_success, describe_output, hew_binary, repo_root, require_codegen};

const SCALAR_DIAMOND: &str = r"
fn sir_scalar_add(x: i64, y: i64) -> i64 {
    x + y
}

fn sir_scalar_diamond(x: i64) -> i64 {
    if x > 0 {
        42
    } else {
        23
    }
}
";

const CLOSED_DIRECT_CALLS: &str = r"
fn main() -> i64 {
    if twice(40) == 42 {
        0
    } else {
        1
    }
}

fn twice(value: i64) -> i64 {
    increment(increment(value))
}

fn increment(value: i64) -> i64 {
    value + 1
}
";

const SHORT_CIRCUIT_INSPECTION: &str = r"
fn rhs() -> bool {
    true
}

fn sir_and(flag: bool) -> bool {
    flag && rhs()
}

fn sir_or(flag: bool) -> bool {
    flag || rhs()
}
";

const SHORT_CIRCUIT_EXECUTABLE: &str = r"
fn sir_and_value(flag: bool) -> i64 {
    if flag && false {
        1
    } else {
        0
    }
}

fn sir_or_value(flag: bool) -> i64 {
    if flag || false {
        1
    } else {
        0
    }
}

fn main() -> i64 {
    let total = sir_and_value(false) + sir_and_value(true) + sir_or_value(false) + sir_or_value(true);
    if total == 1 { 0 } else { 1 }
}
";

const REACHABLE_UNSUPPORTED_CALL: &str = r"
fn main() -> i64 {
    effectful()
}

fn effectful() -> i64 {
    println(42);
    0
}
";

const RECURSIVE_DIRECT_CALLS: &str = r"
fn main() -> i64 {
    countdown(5)
}

fn countdown(value: i64) -> i64 {
    if value == 0 {
        0
    } else {
        countdown(value - 1)
    }
}
";

const UNREACHABLE_UNSUPPORTED_BODY: &str = r"
fn main() -> i64 {
    selected()
}

fn selected() -> i64 {
    0
}

fn unrelated_effectful() -> i64 {
    println(42);
    0
}
";

fn raw_mir_dump(source: &Path, sir_flag: Option<&str>) -> Output {
    let mut command = Command::new(hew_binary());
    command.arg("compile").arg("--dump-mir").arg("raw");
    if let Some(flag) = sir_flag {
        command.arg(flag);
    }
    command.arg(source).current_dir(repo_root());
    support::run_bounded_command(command, format!("raw MIR dump for {}", source.display()))
}

fn sir_dump(source: &Path) -> Output {
    let mut command = Command::new(hew_binary());
    command
        .arg("compile")
        .arg("--dump-sir")
        .arg(source)
        .current_dir(repo_root());
    support::run_bounded_command(command, format!("SIR dump for {}", source.display()))
}

fn function_section<'a>(dump: &'a str, name: &str) -> &'a str {
    let header = format!("fn {name}(");
    let start = dump
        .find(&header)
        .unwrap_or_else(|| panic!("raw MIR dump must contain `{header}`:\n{dump}"));
    let rest = &dump[start..];
    let end = rest[1..]
        .find("\nfn ")
        .map_or(rest.len(), |index| index + 1);
    &rest[..end]
}

fn scalar_fixture() -> (tempfile::TempDir, std::path::PathBuf) {
    let dir = support::tempdir();
    let source = dir.path().join("sir_scalar_diamond.hew");
    fs::write(&source, SCALAR_DIAMOND).expect("write scalar SIR fixture");
    (dir, source)
}

/// Shadow mode constructs, verifies, and backend-front validates the SIR
/// candidate, but emits the established raw MIR unchanged.  This protects the
/// initial migration against accidentally selecting the candidate in the
/// default/shadow lane.
#[test]
fn sir_shadow_keeps_established_raw_mir_authoritative() {
    let (_dir, source) = scalar_fixture();
    let baseline = raw_mir_dump(&source, None);
    let shadow = raw_mir_dump(&source, Some("--sir-shadow"));

    assert_success(&baseline, "baseline raw MIR dump must succeed");
    assert_success(&shadow, "SIR shadow raw MIR dump must succeed");
    assert_eq!(
        baseline.stdout,
        shadow.stdout,
        "--sir-shadow must retain the established raw MIR output\n\
         baseline:\n{}\n\
         shadow:\n{}",
        String::from_utf8_lossy(&baseline.stdout),
        String::from_utf8_lossy(&shadow.stdout),
    );

    let stderr = String::from_utf8_lossy(&shadow.stderr);
    assert!(
        stderr.contains("SIR shadow: verified"),
        "shadow run must report that its SIR lane was verified:\n{}",
        describe_output(&shadow),
    );
    assert!(
        !stderr.contains("realized 0/"),
        "the eligible scalar arithmetic function must be realized through the SIR-to-raw-MIR adapter:\n{}",
        describe_output(&shadow),
    );

    let shadow_dump = String::from_utf8_lossy(&shadow.stdout);
    let established = function_section(&shadow_dump, "sir_scalar_add");
    assert!(
        established.contains("stmt: use"),
        "shadow output must still be the established statement-oriented lowering:\n{established}",
    );
    let established_diamond = function_section(&shadow_dump, "sir_scalar_diamond");
    assert!(
        established_diamond.contains("branch "),
        "the shadow fixture must exercise an established scalar CFG body:\n{established_diamond}",
    );
}

/// Lower mode owns a closed direct-call graph. It must construct fresh bodies
/// for `main`, `twice`, and `increment` from SIR, preserve call continuations
/// and checked arithmetic, and omit every legacy HIR-use statement.
#[test]
fn sir_lower_owns_a_closed_direct_call_graph() {
    let dir = support::tempdir();
    let source = dir.path().join("sir_closed_direct_calls.hew");
    fs::write(&source, CLOSED_DIRECT_CALLS).expect("write strict SIR fixture");
    let baseline = raw_mir_dump(&source, None);
    let lowered = raw_mir_dump(&source, Some("--sir-lower"));

    assert_success(&baseline, "baseline raw MIR dump must succeed");
    assert_success(&lowered, "SIR lower raw MIR dump must succeed");

    let baseline_dump = String::from_utf8_lossy(&baseline.stdout);
    let lowered_dump = String::from_utf8_lossy(&lowered.stdout);
    let baseline_fn = function_section(&baseline_dump, "increment");
    let lowered_fn = function_section(&lowered_dump, "increment");
    assert_ne!(
        baseline_fn, lowered_fn,
        "--sir-lower must replace legacy body lowering instead of only constructing SIR",
    );
    assert!(
        !lowered_fn.contains("stmt: use"),
        "SIR-realized raw MIR must not retain legacy HIR-use statements:\n{lowered_fn}",
    );
    assert!(
        lowered_fn.contains("branch "),
        "checked arithmetic must retain its raw-MIR trap CFG:\n{lowered_fn}",
    );
    assert!(
        lowered_fn.contains("add.checked.s"),
        "SIR arithmetic must legalize to checked raw-MIR arithmetic:\n{lowered_fn}",
    );
    assert!(
        lowered_fn.contains("trap(IntegerOverflow)"),
        "SIR arithmetic overflow must preserve raw-MIR trap semantics:\n{lowered_fn}",
    );
    let baseline_diamond = function_section(&baseline_dump, "main");
    let lowered_diamond = function_section(&lowered_dump, "main");
    assert_ne!(
        baseline_diamond, lowered_diamond,
        "--sir-lower must select SIR block arguments for an ordinary scalar if/join",
    );
    assert!(
        !lowered_diamond.contains("stmt: use"),
        "SIR-realized scalar CFG must not retain legacy HIR-use statements:\n{lowered_diamond}",
    );
    assert!(
        lowered_diamond.contains("branch "),
        "the source if must remain explicit CFG in SIR realization:\n{lowered_diamond}",
    );
    assert!(
        lowered_diamond.contains("call twice"),
        "the SIR caller must legalize its direct call as raw-MIR Call:\n{lowered_diamond}",
    );
    let lowered_twice = function_section(&lowered_dump, "twice");
    assert!(
        lowered_twice.matches("call increment").count() == 2,
        "nested direct SIR calls must stay explicit through raw MIR:\n{lowered_twice}",
    );

    let stderr = String::from_utf8_lossy(&lowered.stderr);
    assert!(
        stderr.contains("SIR lower: selected 3 verified callable(s)")
            && stderr.contains("no legacy MIR bodies were lowered"),
        "lower run must report a closed strict SIR lane:\n{}",
        describe_output(&lowered),
    );
}

/// A strict SIR-only call component must survive raw/checked MIR, LLVM, link,
/// and execution without a legacy caller or callee body.
#[test]
fn sir_lower_closed_direct_call_graph_compiles_and_runs() {
    require_codegen();

    let dir = support::tempdir();
    let source = dir.path().join("sir_lower_execution.hew");
    fs::write(&source, CLOSED_DIRECT_CALLS).expect("write executable SIR fixture");

    let mut compile = Command::new(hew_binary());
    compile
        .arg("compile")
        .arg("--sir-lower")
        .arg("--emit-dir")
        .arg(dir.path())
        .arg(&source)
        .current_dir(repo_root());
    let compiled = support::run_bounded_command(compile, "compile strict SIR direct-call graph");
    assert_success(
        &compiled,
        "strict SIR direct-call graph must compile through the established backend",
    );
    let compile_stderr = String::from_utf8_lossy(&compiled.stderr);
    assert!(
        compile_stderr.contains("SIR lower: selected 3 verified callable(s)")
            && compile_stderr.contains("no legacy MIR bodies were lowered"),
        "compile must select the complete direct SIR graph:\n{}",
        describe_output(&compiled),
    );

    let binary = hew_testutil::compiled_binary_path(dir.path(), "sir_lower_execution");
    assert!(
        binary.is_file(),
        "SIR-lowered compile did not produce expected native binary {}:\n{}",
        binary.display(),
        describe_output(&compiled),
    );
    let executed = support::run_bounded_command(
        Command::new(&binary),
        format!("run strict SIR direct-call graph {}", binary.display()),
    );
    assert_success(
        &executed,
        "native binary containing only SIR-lowered bodies must run successfully",
    );
}

/// Shadow intentionally cannot realize direct calls through its legacy raw-MIR
/// template, so strict direct-call behavior is compared directly with the
/// established compiler rather than inferred from a shadow success. This is
/// temporary migration evidence: once SIR becomes the normal path, the legacy
/// half of this comparison is deleted with its body lowerer.
#[test]
fn sir_lower_matches_established_execution_for_closed_direct_call_graph() {
    require_codegen();

    let dir = support::tempdir();
    let source = dir.path().join("sir_direct_call_parity.hew");
    let established_dir = dir.path().join("established");
    let strict_dir = dir.path().join("strict");
    fs::create_dir(&established_dir).expect("create established emit directory");
    fs::create_dir(&strict_dir).expect("create strict emit directory");
    fs::write(&source, CLOSED_DIRECT_CALLS).expect("write direct-call parity fixture");

    let mut established_compile = Command::new(hew_binary());
    established_compile
        .arg("compile")
        .arg("--emit-dir")
        .arg(&established_dir)
        .arg(&source)
        .current_dir(repo_root());
    let established_compiled = support::run_bounded_command(
        established_compile,
        "compile established direct-call parity graph",
    );
    assert_success(
        &established_compiled,
        "established direct-call graph must compile for parity",
    );

    let mut strict_compile = Command::new(hew_binary());
    strict_compile
        .arg("compile")
        .arg("--sir-lower")
        .arg("--emit-dir")
        .arg(&strict_dir)
        .arg(&source)
        .current_dir(repo_root());
    let strict_compiled = support::run_bounded_command(
        strict_compile,
        "compile strict SIR direct-call parity graph",
    );
    assert_success(
        &strict_compiled,
        "strict SIR direct-call graph must compile for parity",
    );

    let established_binary =
        hew_testutil::compiled_binary_path(&established_dir, "sir_direct_call_parity");
    let strict_binary = hew_testutil::compiled_binary_path(&strict_dir, "sir_direct_call_parity");
    let established = support::run_bounded_command(
        Command::new(&established_binary),
        format!(
            "run established direct-call parity graph {}",
            established_binary.display()
        ),
    );
    let strict = support::run_bounded_command(
        Command::new(&strict_binary),
        format!(
            "run strict direct-call parity graph {}",
            strict_binary.display()
        ),
    );

    assert_eq!(
        strict.status.code(),
        established.status.code(),
        "strict and established direct-call graphs must have the same exit status\nstrict:\n{}\nestablished:\n{}",
        describe_output(&strict),
        describe_output(&established),
    );
    assert_eq!(
        strict.stdout,
        established.stdout,
        "strict and established direct-call graphs must have the same stdout\nstrict:\n{}\nestablished:\n{}",
        describe_output(&strict),
        describe_output(&established),
    );
}

/// A closed SIR component may contain cycles.  The strict driver must select
/// the recursive callable once, preserve its direct raw-MIR edge, and let the
/// existing backend predeclare the cycle without falling back to HIR bodies.
#[test]
fn sir_lower_recursive_direct_call_graph_compiles_and_runs() {
    require_codegen();

    let dir = support::tempdir();
    let source = dir.path().join("sir_recursive_execution.hew");
    fs::write(&source, RECURSIVE_DIRECT_CALLS).expect("write recursive SIR fixture");

    let lowered = raw_mir_dump(&source, Some("--sir-lower"));
    assert_success(&lowered, "strict SIR recursive raw MIR dump must succeed");
    let lowered_dump = String::from_utf8_lossy(&lowered.stdout);
    let countdown = function_section(&lowered_dump, "countdown");
    assert!(
        countdown.contains("call countdown"),
        "the recursive SIR edge must remain an explicit raw-MIR call:\n{countdown}",
    );
    assert!(
        !countdown.contains("stmt: use"),
        "the recursive caller must not be a legacy HIR-derived body:\n{countdown}",
    );

    let mut compile = Command::new(hew_binary());
    compile
        .arg("compile")
        .arg("--sir-lower")
        .arg("--emit-dir")
        .arg(dir.path())
        .arg(&source)
        .current_dir(repo_root());
    let compiled = support::run_bounded_command(compile, "compile strict recursive SIR graph");
    assert_success(
        &compiled,
        "strict SIR recursive graph must compile through the existing backend",
    );
    assert!(
        String::from_utf8_lossy(&compiled.stderr)
            .contains("SIR lower: selected 2 verified callable(s)"),
        "strict recursion must select one closed two-callable component:\n{}",
        describe_output(&compiled),
    );

    let binary = hew_testutil::compiled_binary_path(dir.path(), "sir_recursive_execution");
    let executed = support::run_bounded_command(
        Command::new(&binary),
        format!("run strict recursive SIR graph {}", binary.display()),
    );
    assert_success(
        &executed,
        "native binary containing recursive SIR-only bodies must run successfully",
    );
}

/// Strict selection is closed over calls from `main`, not over every HIR body
/// in a source file.  An unsupported body outside that graph must neither
/// force a fallback nor leak into the emitted component.
#[test]
fn sir_lower_excludes_unreachable_unsupported_hir_bodies() {
    let dir = support::tempdir();
    let source = dir.path().join("sir_unreachable_unsupported.hew");
    fs::write(&source, UNREACHABLE_UNSUPPORTED_BODY)
        .expect("write unreachable unsupported SIR fixture");

    let output = raw_mir_dump(&source, Some("--sir-lower"));
    assert_success(
        &output,
        "an unrelated unsupported HIR body must not block strict SIR lowering",
    );
    let dump = String::from_utf8_lossy(&output.stdout);
    assert!(
        dump.contains("fn main(") && dump.contains("fn selected("),
        "the closed entry component must contain its selected bodies:\n{dump}",
    );
    assert!(
        !dump.contains("fn unrelated_effectful("),
        "strict SIR must not emit an unrelated legacy or unsupported body:\n{dump}",
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("unrelated HIR bodies remain outside the current semantic surface")
            && stderr.contains("were not compiled or used as fallbacks"),
        "strict SIR must describe the excluded HIR body rather than silently using it:\n{}",
        describe_output(&output),
    );
}

/// Target selection must happen after strict SIR owns the body graph.  LLVM
/// emission for WASM is a lightweight target-front proof that no native-only
/// facts leaked into the SIR component or its MIR realization.
#[test]
fn sir_lower_closed_graph_emits_wasm_llvm() {
    require_codegen();

    let dir = support::tempdir();
    let source = dir.path().join("sir_wasm_frontend.hew");
    let emit_dir = dir.path().join("emit");
    fs::create_dir(&emit_dir).expect("create strict SIR WASM emit directory");
    fs::write(&source, CLOSED_DIRECT_CALLS).expect("write strict SIR WASM fixture");

    let mut compile = Command::new(hew_binary());
    compile
        .arg("compile")
        .arg("--sir-lower")
        .arg("--target")
        .arg("wasm32-unknown-unknown")
        .arg("--emit-llvm")
        .arg("--emit-dir")
        .arg(&emit_dir)
        .arg(&source)
        .current_dir(repo_root());
    let compiled = support::run_bounded_command(compile, "emit strict SIR WASM LLVM");
    assert_success(
        &compiled,
        "strict SIR closed graph must reach the WASM LLVM frontend",
    );
    assert!(
        String::from_utf8_lossy(&compiled.stderr)
            .contains("SIR lower: selected 3 verified callable(s)"),
        "WASM frontend must receive the strict SIR component:\n{}",
        describe_output(&compiled),
    );

    let llvm = fs::read_to_string(emit_dir.join("sir_wasm_frontend.ll"))
        .expect("strict SIR WASM LLVM emission must produce an .ll artifact");
    assert!(
        llvm.contains("target triple = \"wasm32-unknown-unknown\""),
        "strict SIR LLVM must retain the requested WASM target:\n{llvm}",
    );
    assert!(
        llvm.contains("@main(") && llvm.contains("@twice(") && llvm.contains("@increment("),
        "the WASM frontend must receive definitions for the complete strict SIR component:\n{llvm}",
    );
}

/// A logical RHS must remain in the CFG block guarded by its left-hand side.
/// This drives the SIR inspector directly, so the test also proves `--dump-sir`
/// does not need to construct legacy MIR or validate a backend candidate.
#[test]
fn sir_dump_preserves_short_circuit_control_flow() {
    let dir = support::tempdir();
    let source = dir.path().join("sir_short_circuit_inspection.hew");
    fs::write(&source, SHORT_CIRCUIT_INSPECTION).expect("write short-circuit SIR fixture");

    let output = sir_dump(&source);
    assert_success(&output, "SIR short-circuit inspection must succeed");
    let dump = String::from_utf8_lossy(&output.stdout);
    let and_body = function_section(&dump, "sir_and");
    let or_body = function_section(&dump, "sir_or");

    assert!(
        and_body.contains("branch %0, bb1, bb2"),
        "&& must evaluate its RHS only on the true edge:\n{and_body}",
    );
    assert!(
        and_body.find("branch").is_some_and(|branch| {
            and_body.find("call").is_some_and(|call| branch < call)
                && and_body
                    .find("const false")
                    .is_some_and(|constant| branch < constant)
        }),
        "&& must materialize both RHS and false paths after its branch:\n{and_body}",
    );
    assert!(
        !and_body.contains("Binary { op: And"),
        "&& must not survive as an eager SIR binary operation:\n{and_body}",
    );

    assert!(
        or_body.contains("branch %0, bb2, bb1"),
        "|| must evaluate its RHS only on the false edge:\n{or_body}",
    );
    assert!(
        or_body.find("branch").is_some_and(|branch| {
            or_body.find("call").is_some_and(|call| branch < call)
                && or_body
                    .find("const true")
                    .is_some_and(|constant| branch < constant)
        }),
        "|| must materialize both RHS and true paths after its branch:\n{or_body}",
    );
    assert!(
        !or_body.contains("Binary { op: Or"),
        "|| must not survive as an eager SIR binary operation:\n{or_body}",
    );
}

/// Strict SIR lowering must preserve both truth-table sides of `&&` and `||`
/// through raw MIR, LLVM, and a native executable, including the direct calls
/// from the SIR-owned `main` body.
#[test]
fn sir_lower_short_circuit_truth_table_compiles_and_runs() {
    require_codegen();

    let dir = support::tempdir();
    let source = dir.path().join("sir_short_circuit_execution.hew");
    fs::write(&source, SHORT_CIRCUIT_EXECUTABLE).expect("write short-circuit execution fixture");

    let mut compile = Command::new(hew_binary());
    compile
        .arg("compile")
        .arg("--sir-lower")
        .arg("--emit-dir")
        .arg(dir.path())
        .arg(&source)
        .current_dir(repo_root());
    let compiled = support::run_bounded_command(compile, "compile strict SIR short-circuit CFG");
    assert_success(
        &compiled,
        "strict SIR short-circuit call graph must compile through the backend",
    );
    let compile_stderr = String::from_utf8_lossy(&compiled.stderr);
    assert!(
        compile_stderr.contains("no legacy MIR bodies were lowered"),
        "compile must select the strict short-circuit SIR graph:\n{}",
        describe_output(&compiled),
    );

    let binary = hew_testutil::compiled_binary_path(dir.path(), "sir_short_circuit_execution");
    let executed = support::run_bounded_command(
        Command::new(&binary),
        format!("run selected SIR short-circuit CFG {}", binary.display()),
    );
    assert_success(
        &executed,
        "native binary containing strict SIR short-circuit functions must run",
    );
}

/// Strict selection is a hard boundary: a reachable callable that has no
/// supported SIR body must fail rather than re-entering HIR→MIR lowering.
#[test]
fn sir_lower_rejects_reachable_unsupported_call_without_fallback() {
    let dir = support::tempdir();
    let source = dir.path().join("sir_reachable_unsupported.hew");
    fs::write(&source, REACHABLE_UNSUPPORTED_CALL).expect("write unsupported strict SIR fixture");

    let mut command = Command::new(hew_binary());
    command
        .arg("compile")
        .arg("--sir-lower")
        .arg("--dump-mir")
        .arg("raw")
        .arg(&source)
        .current_dir(repo_root());
    let output = support::run_bounded_command(command, "reject unsupported strict SIR call");

    assert!(
        !output.status.success(),
        "strict SIR lowering must fail instead of falling back:\n{}",
        describe_output(&output),
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("SIR strict lowering failed")
            && stderr.contains("requires one lowered body for `effectful`")
            && stderr.contains("main → effectful"),
        "failure must identify the closed SIR call-graph boundary:\n{}",
        describe_output(&output),
    );
    assert!(
        output.stdout.is_empty(),
        "strict failure must not emit a legacy raw-MIR dump:\n{}",
        describe_output(&output),
    );
}
