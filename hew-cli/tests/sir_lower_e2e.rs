//! End-to-end contracts for the strict Semantic IR lane.
//!
//! `--sir-lower` must compile a closed SIR call graph without constructing
//! legacy function bodies: a reachable unsupported feature is a compilation
//! error, never a fallback onto the established MIR path.

mod support;

use std::fmt::Write as _;
use std::fs;
use std::path::Path;
use std::process::{Command, Output};

use support::{assert_success, describe_output, hew_binary, repo_root, require_codegen};

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

/// The smallest source-level vertical slice for Raw MIR virtual values. The
/// tuple must remain semantic/value-only through strict SIR lowering; only
/// its selected scalar field reaches the existing return ABI slot.
const VIRTUAL_TUPLE_PROJECTION: &str = r"
fn main() -> i64 {
    let pair = (0, 42);
    pair.0
}
";

/// Extends the virtual tuple proof to the only ABI shape admitted by the
/// first Raw-value slice: `BitCopy` scalar parameters. The tuple itself remains
/// internal and semantic; `pair_second` returns only a selected scalar.
const VIRTUAL_TUPLE_SCALAR_PARAMS: &str = r"
fn pair_second(x: i64, y: i64) -> i64 {
    let pair = (x, y);
    pair.1
}

fn main() -> i64 {
    pair_second(42, 0)
}
";

// SIR body lowering is demand-driven from the entry, so an inspection fixture
// has to be a program: `main` is what puts these two bodies in the dump.
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

fn main() -> i64 {
    if sir_and(true) {
        1
    } else if sir_or(false) {
        2
    } else {
        0
    }
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

/// The first SIR optimization proof: both semantic branch arms are initially
/// present, but the direct `true` condition lets SIR retain only the selected
/// CFG edge before any Raw MIR representation is chosen.
/// The unselected arm is `9`, not `1`: the condition `true` is realized as
/// `const.i64 1` in raw MIR, so a `1` there could not be told apart from a
/// surviving dead arm. The selected arm stays `0` so the compiled fixture
/// still exits successfully.
const CONSTANT_CFG_CANONICALIZATION: &str = r"
fn main() -> i64 {
    if true {
        0
    } else {
        9
    }
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

const GENERIC_DIRECT_CALLS: &str = r"
pub fn id<T>(value: T) -> T {
    value
}

pub fn relay<U>(value: U) -> U {
    id(id(value))
}

fn main() -> i64 {
    if relay(40) == 40 {
        0
    } else {
        1
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

fn llvm_function_section<'a>(llvm: &'a str, name: &str) -> &'a str {
    let symbol = format!("@{name}(");
    let start = llvm
        .match_indices(&symbol)
        .find_map(|(symbol_start, _)| {
            let line_start = llvm[..symbol_start]
                .rfind('\n')
                .map_or(0, |index| index + 1);
            llvm[line_start..symbol_start]
                .starts_with("define ")
                .then_some(line_start)
        })
        .unwrap_or_else(|| panic!("LLVM IR must define `{symbol}`:\n{llvm}"));
    let rest = &llvm[start..];
    let end = rest.find("\n}").map_or(rest.len(), |index| index + 2);
    &rest[..end]
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

/// A source tuple projection must select the strict SIR → virtual Raw MIR
/// path, cross Checked and Elaborated MIR as a no-drop body, and execute as a
/// native binary. The raw dump is part of the assertion: it rules out quietly
/// returning to the legacy `Place`/storage lowering path merely because the
/// observable result happens to be scalar.
#[test]
fn sir_lower_virtual_tuple_projection_compiles_and_runs_without_legacy_storage() {
    require_codegen();

    let dir = support::tempdir();
    let source = dir.path().join("sir_virtual_tuple_projection.hew");
    fs::write(&source, VIRTUAL_TUPLE_PROJECTION).expect("write virtual tuple SIR fixture");

    let lowered = raw_mir_dump(&source, Some("--sir-lower"));
    assert_success(&lowered, "strict SIR virtual tuple raw dump must succeed");
    let dump = String::from_utf8_lossy(&lowered.stdout);
    let main = function_section(&dump, "main");
    assert!(
        main.contains("tuple.make")
            && main.contains("tuple.get")
            && main.contains("materialize.return_abi"),
        "source tuple must lower through Raw MIR virtual values:\n{main}"
    );
    assert!(
        !main.contains("locals:") && !main.contains("tuple.construct"),
        "virtual tuple lowering must not reintroduce legacy storage operations:\n{main}"
    );
    let dump_stderr = String::from_utf8_lossy(&lowered.stderr);
    assert!(
        dump_stderr.contains("no legacy MIR bodies were lowered"),
        "strict tuple dump must report that no legacy body was used:\n{}",
        describe_output(&lowered)
    );

    let mut compile = Command::new(hew_binary());
    compile
        .arg("compile")
        .arg("--sir-lower")
        .arg("--emit-dir")
        .arg(dir.path())
        .arg(&source)
        .current_dir(repo_root());
    let compiled = support::run_bounded_command(compile, "compile strict SIR virtual tuple");
    assert_success(
        &compiled,
        "source tuple must reach LLVM through strict SIR virtual Raw MIR",
    );
    let compile_stderr = String::from_utf8_lossy(&compiled.stderr);
    assert!(
        compile_stderr.contains("no legacy MIR bodies were lowered"),
        "strict tuple compile must report no legacy fallback:\n{}",
        describe_output(&compiled)
    );

    let binary = hew_testutil::compiled_binary_path(dir.path(), "sir_virtual_tuple_projection");
    assert!(
        binary.is_file(),
        "virtual tuple compile did not produce expected native binary {}:\n{}",
        binary.display(),
        describe_output(&compiled),
    );
    let executed = support::run_bounded_command(
        Command::new(&binary),
        format!("run strict SIR virtual tuple {}", binary.display()),
    );
    assert_success(
        &executed,
        "native virtual tuple program must return the selected zero field",
    );
}

/// Scalar parameters of a virtual tuple body must map directly to LLVM
/// parameters, rather than acquiring legacy Raw-MIR locals or allocas. This
/// is source-driver coverage for the ABI contract also exercised by the
/// hand-built LLVM fixture.
#[test]
fn sir_lower_virtual_tuple_scalar_params_remain_value_only() {
    require_codegen();

    let dir = support::tempdir();
    let source = dir.path().join("sir_virtual_tuple_scalar_params.hew");
    let emit_dir = dir.path().join("emit");
    fs::create_dir(&emit_dir).expect("create virtual tuple scalar parameter emit directory");
    fs::write(&source, VIRTUAL_TUPLE_SCALAR_PARAMS)
        .expect("write virtual tuple scalar parameter SIR fixture");

    let lowered = raw_mir_dump(&source, Some("--sir-lower"));
    assert_success(
        &lowered,
        "strict SIR virtual scalar-parameter raw dump must succeed",
    );
    let dump = String::from_utf8_lossy(&lowered.stdout);
    let pair_second = function_section(&dump, "pair_second");
    assert!(
        pair_second.contains("param 0")
            && pair_second.contains("param 1")
            && pair_second.contains("tuple.make")
            && pair_second.contains("tuple.get")
            && pair_second.contains("materialize.return_abi"),
        "scalar tuple parameters must lower through Raw virtual values:\n{pair_second}"
    );
    assert!(
        !pair_second.contains("locals:")
            && !pair_second.contains("local_0")
            && !pair_second.contains("local_1")
            && !pair_second.contains("tuple.construct"),
        "virtual scalar parameters must not reintroduce storage or allocas:\n{pair_second}"
    );
    let dump_stderr = String::from_utf8_lossy(&lowered.stderr);
    assert!(
        dump_stderr.contains("no legacy MIR bodies were lowered"),
        "strict scalar-parameter dump must report no legacy fallback:\n{}",
        describe_output(&lowered)
    );

    let mut compile = Command::new(hew_binary());
    compile
        .arg("compile")
        .arg("--sir-lower")
        .arg("--emit-llvm")
        .arg("--emit-dir")
        .arg(&emit_dir)
        .arg(&source)
        .current_dir(repo_root());
    let compiled = support::run_bounded_command(
        compile,
        "compile strict SIR virtual tuple scalar parameters",
    );
    assert_success(
        &compiled,
        "source scalar parameters must reach LLVM through strict SIR virtual Raw MIR",
    );
    let compile_stderr = String::from_utf8_lossy(&compiled.stderr);
    assert!(
        compile_stderr.contains("no legacy MIR bodies were lowered"),
        "strict scalar-parameter compile must report no legacy fallback:\n{}",
        describe_output(&compiled)
    );

    let llvm = fs::read_to_string(emit_dir.join("sir_virtual_tuple_scalar_params.ll"))
        .expect("strict scalar-parameter LLVM emission must produce an .ll artifact");
    let pair_second_llvm = llvm_function_section(&llvm, "pair_second");
    assert!(
        pair_second_llvm.contains("insertvalue { i64, i64 }")
            && pair_second_llvm.contains("extractvalue { i64, i64 }"),
        "scalar tuple body must retain an LLVM aggregate value:\n{pair_second_llvm}"
    );
    assert!(
        !pair_second_llvm.contains("alloca { i64, i64 }")
            && !pair_second_llvm.contains("local_0")
            && !pair_second_llvm.contains("local_1"),
        "scalar tuple parameters must not acquire tuple or parameter local allocas:\n{pair_second_llvm}"
    );

    let binary = hew_testutil::compiled_binary_path(&emit_dir, "sir_virtual_tuple_scalar_params");
    assert!(
        binary.is_file(),
        "virtual scalar-parameter compile did not produce expected native binary {}:\n{}",
        binary.display(),
        describe_output(&compiled),
    );
    let executed = support::run_bounded_command(
        Command::new(&binary),
        format!(
            "run strict SIR virtual tuple scalar parameters {}",
            binary.display()
        ),
    );
    assert_success(
        &executed,
        "native virtual tuple scalar-parameter program must return the selected zero field",
    );
}

/// Generic direct calls use the same strict body path as monomorphic calls.
/// The concrete SIR instances must be emitted under their derived symbols;
/// neither the abstract generic template nor a legacy MIR body is permitted
/// to reach the selected component.
#[test]
fn sir_lower_generic_direct_call_graph_compiles_and_runs() {
    require_codegen();

    let dir = support::tempdir();
    let source = dir.path().join("sir_generic_direct_calls.hew");
    fs::write(&source, GENERIC_DIRECT_CALLS).expect("write generic SIR fixture");

    let lowered = raw_mir_dump(&source, Some("--sir-lower"));
    assert_success(&lowered, "strict generic SIR raw MIR dump must succeed");
    let dump = String::from_utf8_lossy(&lowered.stdout);
    assert!(
        dump.contains("fn relay$$i64(") && dump.contains("fn id$$i64("),
        "strict generic component must contain concrete instance bodies:\n{dump}",
    );
    assert!(
        !dump.contains("fn relay(") && !dump.contains("fn id("),
        "abstract generic templates must not reach raw MIR:\n{dump}",
    );

    let mut compile = Command::new(hew_binary());
    compile
        .arg("compile")
        .arg("--sir-lower")
        .arg("--emit-dir")
        .arg(dir.path())
        .arg(&source)
        .current_dir(repo_root());
    let compiled = support::run_bounded_command(compile, "compile strict generic SIR graph");
    assert_success(
        &compiled,
        "strict generic SIR graph must compile through the existing backend",
    );
    assert!(
        String::from_utf8_lossy(&compiled.stderr)
            .contains("SIR lower: selected 3 verified callable(s)"),
        "generic strict component must report exactly main, relay<i64>, and cached id<i64>:\n{}",
        describe_output(&compiled),
    );

    let binary = hew_testutil::compiled_binary_path(dir.path(), "sir_generic_direct_calls");
    let executed = support::run_bounded_command(
        Command::new(&binary),
        format!("run strict generic SIR graph {}", binary.display()),
    );
    assert_success(
        &executed,
        "native binary containing only generic SIR instance bodies must run",
    );
}

/// Strict direct-call behaviour is compared against the established compiler
/// on the same source, because nothing inside the strict lane can attest to
/// its own correctness here. This is temporary migration evidence: once SIR
/// becomes the normal path, the established half of the comparison is deleted
/// with its body lowerer.
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
        stderr.contains("were never reached from the entry")
            && stderr.contains("were not compiled or used as fallbacks"),
        "strict SIR must account for the excluded HIR body rather than silently using it:\n{}",
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

/// `--dump-sir` must account for every body it could not lower.
///
/// The inspection surface used to print at most six reasons and replace the
/// rest with a count, which is exactly the case where an inspector needs the
/// detail. This fixture has seven unsupported bodies for that reason.
#[test]
fn sir_dump_reports_every_unsupported_body_not_just_the_first_few() {
    const UNSUPPORTED_BODIES: usize = 7;

    let mut fixture = String::new();
    for index in 0..UNSUPPORTED_BODIES {
        // `var` bindings are outside the initial SIR surface.
        write!(
            fixture,
            "fn helper{index}(value: i64) -> i64 {{\n    var accumulator = value;\n    accumulator\n}}\n\n"
        )
        .expect("write to String");
    }
    fixture.push_str("fn main() -> i64 {\n    ");
    fixture.push_str(
        &(0..UNSUPPORTED_BODIES)
            .map(|index| format!("helper{index}({index})"))
            .collect::<Vec<_>>()
            .join(" + "),
    );
    fixture.push_str("\n}\n");

    let dir = support::tempdir();
    let source = dir.path().join("sir_unsupported_inventory.hew");
    fs::write(&source, &fixture).expect("write unsupported-body inventory fixture");

    let output = sir_dump(&source);
    assert_success(
        &output,
        "SIR inspection must succeed with unsupported bodies",
    );
    let dump = String::from_utf8_lossy(&output.stdout);
    for index in 0..UNSUPPORTED_BODIES {
        assert!(
            dump.contains(&format!("; fn helper{index}\n; unsupported: ")),
            "`helper{index}` must be reported with its reason:\n{dump}",
        );
    }
    assert!(
        dump.contains("fn main("),
        "the dump must still carry the IR it could lower:\n{dump}",
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

/// `--dump-sir` and strict lowering must consume the same canonical semantic
/// CFG. This proves the first actual SIR pass is not an inspector-only
/// transformation or a second compiler lane.
#[test]
fn sir_canonicalizes_direct_constant_cfg_before_strict_lowering() {
    require_codegen();

    let dir = support::tempdir();
    let source = dir.path().join("sir_constant_cfg.hew");
    fs::write(&source, CONSTANT_CFG_CANONICALIZATION).expect("write constant SIR CFG fixture");

    let inspected = sir_dump(&source);
    assert_success(&inspected, "canonical SIR inspection must succeed");
    let dump = String::from_utf8_lossy(&inspected.stdout);
    let main = function_section(&dump, "main");
    assert!(
        main.contains("goto") && !main.contains("branch"),
        "a direct constant branch must become one semantic edge:\n{main}",
    );
    assert!(
        !main.contains("const 9"),
        "the unreachable source arm must be gone from canonical SIR:\n{main}",
    );

    let lowered = raw_mir_dump(&source, Some("--sir-lower"));
    assert_success(&lowered, "canonical strict SIR raw dump must succeed");
    let lowered_stderr = String::from_utf8_lossy(&lowered.stderr);
    assert!(
        lowered_stderr.contains("no legacy MIR bodies were lowered"),
        "strict canonicalization must remain on the SIR body path:\n{}",
        describe_output(&lowered),
    );

    // The canonical CFG must reach the artifact the backend consumes, not stop
    // at the inspector: a compile that merely succeeds cannot distinguish a
    // canonicalized raw body from an un-canonicalized one.
    let lowered_dump = String::from_utf8_lossy(&lowered.stdout);
    let lowered_main = function_section(&lowered_dump, "main");
    assert!(
        lowered_main.contains("goto") && !lowered_main.contains("branch"),
        "strict raw MIR must carry the canonical single edge:\n{lowered_main}",
    );
    assert!(
        !lowered_main.contains("const.i64 9"),
        "the unselected arm must not be realized in strict raw MIR:\n{lowered_main}",
    );
    assert!(
        lowered_main.contains("const.i64 0"),
        "the selected arm must still be realized in strict raw MIR:\n{lowered_main}",
    );

    let mut compile = Command::new(hew_binary());
    compile
        .arg("compile")
        .arg("--sir-lower")
        .arg("--emit-dir")
        .arg(dir.path())
        .arg(&source)
        .current_dir(repo_root());
    let compiled = support::run_bounded_command(compile, "compile canonical constant SIR CFG");
    assert_success(
        &compiled,
        "canonical SIR CFG must compile through Raw/Checked/Elaborated MIR and LLVM",
    );

    let binary = hew_testutil::compiled_binary_path(dir.path(), "sir_constant_cfg");
    let executed = support::run_bounded_command(
        Command::new(&binary),
        format!("run canonical constant SIR CFG {}", binary.display()),
    );
    assert_success(
        &executed,
        "canonical SIR CFG executable must preserve behavior",
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
