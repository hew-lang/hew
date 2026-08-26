//! End-to-end contracts for the experimental Semantic IR lane.
//!
//! `--sir-shadow` must exercise and verify the SIR → raw-MIR candidate while
//! leaving the established raw-MIR dump byte-for-byte authoritative.  Once a
//! function is eligible, `--sir-lower` must select that same candidate rather
//! than merely reporting that it was constructed.

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

const EXECUTABLE_SCALAR: &str = r"
fn sir_selected_add(x: i64, y: i64) -> i64 {
    x + y
}

fn main() -> i64 {
    println(sir_selected_add(19, 23));
    0
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
    println(sir_and_value(false) + sir_and_value(true) + sir_or_value(false) + sir_or_value(true));
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

/// Lower mode selects the verified candidate for the supported scalar
/// value/CFG subset.  Both straight-line arithmetic and a branch/join must be
/// value-oriented (no legacy `stmt: use` markers); checked arithmetic retains
/// its required raw-MIR trap CFG.
#[test]
fn sir_lower_selects_value_and_cfg_lowering() {
    let (_dir, source) = scalar_fixture();
    let baseline = raw_mir_dump(&source, None);
    let lowered = raw_mir_dump(&source, Some("--sir-lower"));

    assert_success(&baseline, "baseline raw MIR dump must succeed");
    assert_success(&lowered, "SIR lower raw MIR dump must succeed");

    let baseline_dump = String::from_utf8_lossy(&baseline.stdout);
    let lowered_dump = String::from_utf8_lossy(&lowered.stdout);
    let baseline_fn = function_section(&baseline_dump, "sir_scalar_add");
    let lowered_fn = function_section(&lowered_dump, "sir_scalar_add");
    assert_ne!(
        baseline_fn, lowered_fn,
        "--sir-lower must select the SIR candidate instead of only constructing it",
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
    let baseline_diamond = function_section(&baseline_dump, "sir_scalar_diamond");
    let lowered_diamond = function_section(&lowered_dump, "sir_scalar_diamond");
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

    let stderr = String::from_utf8_lossy(&lowered.stderr);
    assert!(
        stderr.contains("SIR lower: verified"),
        "lower run must report its verified SIR lane:\n{}",
        describe_output(&lowered),
    );
    assert!(
        !stderr.contains("realized 0/"),
        "the eligible scalar arithmetic function must be selected from SIR:\n{}",
        describe_output(&lowered),
    );
}

/// A selected SIR callee must survive all remaining established pipeline
/// stages and execute with the same observable result. The caller stays on
/// the legacy path because call realization is intentionally not part of this
/// first adapter slice; that mixed module is the intended incremental-cutover
/// topology.
#[test]
fn sir_lower_selected_scalar_callee_compiles_and_runs() {
    require_codegen();

    let dir = support::tempdir();
    let source = dir.path().join("sir_lower_execution.hew");
    fs::write(&source, EXECUTABLE_SCALAR).expect("write executable SIR fixture");

    let mut compile = Command::new(hew_binary());
    compile
        .arg("compile")
        .arg("--sir-lower")
        .arg("--emit-dir")
        .arg(dir.path())
        .arg(&source)
        .current_dir(repo_root());
    let compiled = support::run_bounded_command(compile, "compile selected SIR scalar callee");
    assert_success(
        &compiled,
        "SIR-lowered scalar callee must compile through the established backend",
    );
    let compile_stderr = String::from_utf8_lossy(&compiled.stderr);
    assert!(
        compile_stderr.contains("SIR lower: verified") && !compile_stderr.contains("realized 0/"),
        "compile must select at least one SIR body:\n{}",
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
        format!("run selected SIR scalar callee {}", binary.display()),
    );
    assert_success(
        &executed,
        "native binary containing SIR-lowered callee must run successfully",
    );
    assert_eq!(
        String::from_utf8_lossy(&executed.stdout),
        "42\n",
        "SIR-lowered scalar callee returned an unexpected result:\n{}",
        describe_output(&executed),
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

/// Selected SIR CFG lowering must preserve both truth-table sides of `&&` and
/// `||` through raw MIR, LLVM, and a native executable. The calling `main`
/// remains temporary legacy scaffolding until the direct-call SIR cutover.
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
    let compiled = support::run_bounded_command(compile, "compile selected SIR short-circuit CFG");
    assert_success(
        &compiled,
        "SIR-lowered short-circuit functions must compile through the backend",
    );
    let compile_stderr = String::from_utf8_lossy(&compiled.stderr);
    assert!(
        compile_stderr.contains("SIR lower: verified") && !compile_stderr.contains("realized 0/"),
        "compile must select the short-circuit SIR functions:\n{}",
        describe_output(&compiled),
    );

    let binary = hew_testutil::compiled_binary_path(dir.path(), "sir_short_circuit_execution");
    let executed = support::run_bounded_command(
        Command::new(&binary),
        format!("run selected SIR short-circuit CFG {}", binary.display()),
    );
    assert_success(
        &executed,
        "native binary containing SIR-lowered short-circuit functions must run",
    );
    assert_eq!(
        String::from_utf8_lossy(&executed.stdout),
        "1\n",
        "SIR short-circuit truth table produced an unexpected result:\n{}",
        describe_output(&executed),
    );
}
