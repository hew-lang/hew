//! End-to-end machine execution fixtures for `m.step(ev)` and `m.state_name()`.
//!
//! The CLI currently emits textual LLVM IR as part of `hew compile`; there is
//! no separate `--emit-llvm` flag. These tests invoke that public compile path,
//! read the emitted `.ll`, and then run the same fixtures through `hew run`.

use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::OnceLock;

struct MachineFixture {
    stem: &'static str,
    machine: &'static str,
    states: usize,
    /// When true the machine has a `default { state }` arm, so its dispatch
    /// fall-through returns `self` (no LLVM `unreachable` trap). The IR-shape
    /// assertions invert the trap/return check for these fixtures.
    has_default: bool,
    /// When true (and `has_default` is false) at least one transition in the
    /// machine carries a `when` guard, so the dispatch fall-through is a
    /// user-reachable condition (an all-guards-false step) rather than a
    /// compiler invariant. These fixtures' fall-through must lower to
    /// a real `TrapKind::ExhaustivenessFallthrough` trap call, never the
    /// bare LLVM `unreachable` a guard-free machine gets.
    has_guard: bool,
}

const FIXTURES: &[MachineFixture] = &[
    MachineFixture {
        stem: "run_traffic_light",
        machine: "TrafficLight",
        states: 3,
        has_default: false,
        has_guard: false,
    },
    MachineFixture {
        stem: "run_tcp_handshake",
        machine: "TcpHandshake",
        states: 3,
        has_default: false,
        has_guard: false,
    },
    MachineFixture {
        // `default { state }` machine: unhandled cells stay in the current
        // state. Exercises the `has_default` step-dispatch fall-through that
        // returns `self` unchanged instead of trapping.
        stem: "run_default_stay",
        machine: "Tank",
        states: 2,
        has_default: true,
        has_guard: false,
    },
    MachineFixture {
        // Depth-1 composite: `Connected` groups three substates which desugar
        // to flat states + concrete-source transitions. The flat machine has
        // four states (Disconnected + the three substates); the composite name
        // is dropped. Exercises the Harel entry/exit ordering at runtime via
        // the `.expected` oracle.
        stem: "run_connection_lifecycle",
        machine: "ConnectionLifecycle",
        states: 4,
        has_default: false,
        has_guard: false,
    },
    MachineFixture {
        // a false `when` guard must fall through to the sibling
        // guarded arm, not fire the first same-cell arm in source order.
        stem: "guard_gates_transition",
        machine: "Latch",
        states: 2,
        has_default: false,
        has_guard: true,
    },
    MachineFixture {
        // a true `when` guard fires its arm — proves the
        // guard is actually evaluated both ways, not just "always false".
        stem: "guard_true_fires",
        machine: "Latch",
        states: 2,
        has_default: false,
        has_guard: true,
    },
    MachineFixture {
        // an all-guards-false step with `default { state }` present
        // stays put (clean exit 0) — `default` still absorbs it regardless
        // of the guard.
        stem: "guard_default_stays",
        machine: "Latch",
        states: 2,
        has_default: true,
        has_guard: true,
    },
    MachineFixture {
        // a false `when` guard on the only explicit arm for an event must
        // fall through to a same-event `_` wildcard arm, not to the trap
        // block — a guarded explicit arm is conditional, and filtering the
        // wildcard out whenever "any" same-event arm existed (guarded or
        // not) made the wildcard permanently unreachable (#2390).
        stem: "guard_false_falls_through_to_wildcard",
        machine: "Latch",
        states: 3,
        has_default: false,
        has_guard: true,
    },
    MachineFixture {
        // unit-event `emit` delivered through the deliver-design ABI
        // (`hew_machine_emit_push` → keep-on-step-exit →
        // `hew_machine_emit_take`). Exercises the step-exit-keep wrapper
        // (never drains) alongside the standard dispatch-shape assertions.
        stem: "run_emit_signal",
        machine: "Signal",
        states: 2,
        has_default: false,
        has_guard: false,
    },
];

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-codegen-rs has a workspace parent")
        .to_path_buf()
}

fn target_dir(repo: &Path) -> PathBuf {
    std::env::var_os("CARGO_TARGET_DIR").map_or_else(
        || repo.join("target"),
        |dir| {
            let path = PathBuf::from(dir);
            if path.is_absolute() {
                path
            } else {
                repo.join(path)
            }
        },
    )
}

fn hew_bin(repo: &Path) -> PathBuf {
    target_dir(repo)
        .join("debug")
        .join(format!("hew{}", std::env::consts::EXE_SUFFIX))
}

fn hew_command(repo: &Path) -> Command {
    let bin = hew_bin(repo);
    if bin.exists() {
        return Command::new(bin);
    }

    let cargo = std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into());
    let mut command = Command::new(cargo);
    command
        .current_dir(repo)
        .args(["run", "--quiet", "-p", "hew-cli", "--bin", "hew", "--"]);
    command
}

fn ensure_hew_runtime_lib(repo: &Path) {
    let _ = repo;
    static BUILT: OnceLock<()> = OnceLock::new();
    BUILT.get_or_init(|| {
        hew_testutil::ensure_hew_lib_built().expect("build libhew.a");
    });
}

fn fixture_path(repo: &Path, stem: &str) -> PathBuf {
    repo.join("examples")
        .join("machine")
        .join(format!("{stem}.hew"))
}

fn compile_fixture(repo: &Path, fixture: &MachineFixture) -> String {
    ensure_hew_runtime_lib(repo);
    let out_dir = std::env::temp_dir().join(format!(
        "hew-machine-exec-{}-{}",
        fixture.stem,
        std::process::id()
    ));
    let _ = std::fs::remove_dir_all(&out_dir);
    std::fs::create_dir_all(&out_dir).expect("create compile output dir");

    let output = hew_command(repo)
        .arg("compile")
        .arg(fixture_path(repo, fixture.stem))
        .arg("--emit-dir")
        .arg(&out_dir)
        .output()
        .expect("spawn hew compile");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.success(),
        "hew compile {} exited non-zero (status={:?}); stderr:\n{}",
        fixture.stem,
        output.status,
        stderr
    );
    assert!(
        !stderr.contains("cutover")
            && !stderr.contains("E_NOT_YET_IMPLEMENTED")
            && !stderr.contains("E_HIR")
            && !stderr.contains("E_MIR"),
        "machine fixture {} must compile without cutover/HIR/MIR diagnostics; stderr:\n{}",
        fixture.stem,
        stderr
    );

    // Normalise line endings: LLVM's print_to_file emits CRLF on Windows
    // while read_to_string is binary (no translation).  The extractors below
    // search for "\n}\n" which would miss "\r\n}\r\n", turning the extracted
    // body into the entire file tail and breaking downstream assertions.
    std::fs::read_to_string(out_dir.join(format!("{}.ll", fixture.stem)))
        .unwrap_or_else(|e| {
            panic!(
                "read emitted LLVM IR for {} from {}: {e}",
                fixture.stem,
                out_dir.display()
            )
        })
        .replace("\r\n", "\n")
}

/// Compile and link a fixture to a standalone native binary via `hew build`,
/// returning the temp dir (the caller keeps it alive until after the run) and
/// the binary path. The compile+link step uses `.output()` — unbounded here and
/// backstopped by the nextest envelope, matching `compile_fixture` and
/// `machine_lifecycle` — so the per-run exec deadline in `run_prebuilt` covers
/// only native execution, which is contention-immune. Binding compile under a
/// tight wall-clock is exactly the defect this harness removes.
fn build_fixture_binary(repo: &Path, source: &Path, stem: &str) -> (tempfile::TempDir, PathBuf) {
    ensure_hew_runtime_lib(repo);
    let dir = tempfile::tempdir().expect("create machine-exec build dir");
    let bin = dir
        .path()
        .join(format!("{stem}{}", std::env::consts::EXE_SUFFIX));

    let output = hew_command(repo)
        .arg("build")
        .arg("-o")
        .arg(&bin)
        .arg(source)
        .output()
        .expect("spawn hew build");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        output.status.success(),
        "hew build {stem} exited non-zero (status={:?}); stderr:\n{stderr}",
        output.status,
    );
    assert!(
        !stderr.contains("cutover")
            && !stderr.contains("E_NOT_YET_IMPLEMENTED")
            && !stderr.contains("E_HIR")
            && !stderr.contains("E_MIR"),
        "machine fixture {stem} must build without cutover/HIR/MIR diagnostics; stderr:\n{stderr}",
    );
    (dir, bin)
}

/// Run a prebuilt fixture binary under the bounded-exec helper and assert it
/// exits zero with stdout matching the oracle. The wall-clock deadline guards
/// only native execution (compile+link already happened in
/// `build_fixture_binary`), so runner contention cannot trip it; the bound stays
/// in place to fail-fast on a genuinely hung or runaway fixture.
fn run_prebuilt(bin: &Path, label: &str, expected: &str) {
    let mut command = Command::new(bin);
    let output = hew_testutil::run_command_bounded(
        &mut command,
        label.to_string(),
        hew_testutil::DEFAULT_EXEC_TIMEOUT,
    )
    .unwrap_or_else(|err| panic!("{label}: {err}"));
    assert!(
        output.status.success(),
        "{label} exited non-zero (status={:?}); stderr:\n{}",
        output.status,
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8(output.stdout).expect("fixture stdout is utf-8");
    assert_eq!(stdout, expected, "{label} stdout mismatch");
}

fn step_function_body<'a>(ir: &'a str, machine: &str) -> &'a str {
    let marker = format!("define %{} @{}__step(", machine, machine);
    let start = ir
        .find(&marker)
        .or_else(|| ir.find(&format!("@{}__step(", machine)))
        .unwrap_or_else(|| panic!("missing step helper `{machine}__step` in IR:\n{ir}"));
    let tail = &ir[start..];
    let end = tail.find("\n}\n").unwrap_or(tail.len());
    &tail[..end]
}

fn main_function_body(ir: &str) -> &str {
    let start = ir
        .find("define i8 @main(")
        .unwrap_or_else(|| panic!("missing user main function in IR:\n{ir}"));
    let tail = &ir[start..];
    let end = tail.find("\n}\n").unwrap_or(tail.len());
    &tail[..end]
}

/// True when `body` contains a "bare" `unreachable` terminator — one NOT
/// immediately preceded by `call void @llvm.trap()`. Every real trap
/// (`emit_trap_with_code`) lowers to `hew_trap_with_code` + `llvm.trap()` +
/// `unreachable` as its three-instruction sequence (the `unreachable` is
/// LLVM block-well-formedness boilerplate after the noreturn-ish trap call,
/// present for EVERY trap kind including `ExhaustivenessFallthrough`). The
/// bare/UB form (`TrapKind::MachineDispatchUnreachable`'s
/// `build_unreachable()`) skips both calls entirely. A plain
/// `step_body.contains("unreachable")` cannot tell these apart because
/// every guard-bearing fixture also contains OTHER trap sites (e.g. the
/// `state_name`/field-access traps) whose `unreachable` IS trap-backed.
fn has_bare_unreachable(body: &str) -> bool {
    let lines: Vec<&str> = body.lines().collect();
    for (idx, line) in lines.iter().enumerate() {
        if line.trim() != "unreachable" {
            continue;
        }
        let prev_is_llvm_trap = idx > 0 && lines[idx - 1].trim() == "call void @llvm.trap()";
        if !prev_is_llvm_trap {
            return true;
        }
    }
    false
}

#[test]
fn run_machine_fixtures_compile_to_step_dispatch_and_state_table() {
    let repo = repo_root();
    ensure_hew_runtime_lib(&repo);
    for fixture in FIXTURES {
        let ir = compile_fixture(&repo, fixture);
        let step_body = step_function_body(&ir, fixture.machine);
        let main_body = main_function_body(&ir);

        assert!(
            ir.contains(&format!("@{}__step(", fixture.machine)),
            "{} LLVM must contain the synthesised step helper",
            fixture.stem
        );
        assert!(
            step_body.contains("switch") || step_body.contains("icmp eq"),
            "{} step helper must contain dispatch over state/event tags:\n{}",
            fixture.stem,
            step_body
        );
        if fixture.has_default {
            // `default { state }` machines stay on unhandled cells: the
            // fall-through returns `self` instead of trapping, so the step
            // body has no `unreachable` terminator.
            assert!(
                !step_body.contains("unreachable"),
                "{} default-arm step dispatch must NOT trap (stays in current state):\n{}",
                fixture.stem,
                step_body
            );
            assert!(
                !step_body.contains("@hew_trap_with_code(i32 208)"),
                "{} default-arm step dispatch must NOT trap (stays in current state):\n{}",
                fixture.stem,
                step_body
            );
        } else if fixture.has_guard {
            // a guard-bearing machine's fall-through is a
            // user-reachable condition (an all-guards-false step), never a
            // compiler invariant — it must lower to a real
            // `ExhaustivenessFallthrough` trap call, NEVER a bare
            // `unreachable` (which would be UB once a guard can evaluate
            // false at runtime).
            assert!(
                !has_bare_unreachable(step_body),
                "{} guard-bearing step dispatch fallthrough must NOT lower to bare \
                 `unreachable` (UB hazard once a guard can fail at runtime):\n{}",
                fixture.stem,
                step_body
            );
            assert!(
                step_body.contains("@hew_trap_with_code(i32 208)"),
                "{} guard-bearing step dispatch fallthrough must lower to a real \
                 ExhaustivenessFallthrough trap (code 208):\n{}",
                fixture.stem,
                step_body
            );
        } else {
            assert!(
                has_bare_unreachable(step_body),
                "{} step dispatch fallthrough must lower to bare LLVM unreachable:\n{}",
                fixture.stem,
                step_body
            );
        }
        assert!(
            !step_body.contains("@hew_trap_with_code(i32 207)"),
            "{} step dispatch fallthrough must not lower to MachineDispatchUnreachable trap:\n{}",
            fixture.stem,
            step_body
        );
        assert!(
            main_body.contains(&format!(
                "call %{} @{}__step(",
                fixture.machine, fixture.machine
            )),
            "{} call site must call the internal step helper and not expose its result:\n{}",
            fixture.stem,
            main_body
        );
        assert!(
            main_body.contains("store %")
                && main_body.contains("%call_result")
                && main_body.contains(&format!("store %{} %move_load", fixture.machine))
                && main_body.contains("ptr %local_"),
            "{} call site must store the step helper result back through MIR Move:\n{}",
            fixture.stem,
            main_body
        );

        let table = format!(
            "@__hew_state_name_table__{} = private constant [{} x ptr]",
            fixture.machine, fixture.states
        );
        assert!(
            ir.contains(&table),
            "{} LLVM must contain the static state_name table `{}`:\n{}",
            fixture.stem,
            table,
            ir
        );
    }
}

/// Build and run a single machine fixture by its `FIXTURES` stem, asserting the
/// prebuilt binary's stdout matches the committed `.expected` oracle. Driving
/// each per-fixture `#[test]` through this keeps `FIXTURES` the single source of
/// truth and isolates a contention retry to the one unlucky fixture rather than
/// the whole set.
fn execute_fixture(stem: &str) {
    let repo = repo_root();
    let fixture = FIXTURES
        .iter()
        .find(|fixture| fixture.stem == stem)
        .unwrap_or_else(|| panic!("no FIXTURES entry for stem `{stem}`"));
    let path = fixture_path(&repo, fixture.stem);
    let expected_path = path.with_extension("expected");
    let expected = std::fs::read_to_string(&expected_path)
        .unwrap_or_else(|e| panic!("read {}: {e}", expected_path.display()));

    let (_build_dir, bin) = build_fixture_binary(&repo, &path, fixture.stem);
    run_prebuilt(&bin, &format!("run {} fixture", fixture.stem), &expected);
}

#[test]
fn traffic_light_executes_with_expected_stdout() {
    execute_fixture("run_traffic_light");
}

#[test]
fn tcp_handshake_executes_with_expected_stdout() {
    execute_fixture("run_tcp_handshake");
}

#[test]
fn default_stay_executes_with_expected_stdout() {
    execute_fixture("run_default_stay");
}

#[test]
fn connection_lifecycle_executes_with_expected_stdout() {
    execute_fixture("run_connection_lifecycle");
}

#[test]
fn guard_gates_transition_executes_with_expected_stdout() {
    execute_fixture("guard_gates_transition");
}

#[test]
fn guard_true_fires_executes_with_expected_stdout() {
    execute_fixture("guard_true_fires");
}

#[test]
fn guard_default_stays_executes_with_expected_stdout() {
    execute_fixture("guard_default_stays");
}

#[test]
fn guard_false_falls_through_to_wildcard_executes_with_expected_stdout() {
    execute_fixture("guard_false_falls_through_to_wildcard");
}

#[test]
fn run_emit_signal_fixture_executes() {
    execute_fixture("run_emit_signal");
}

/// Two machine TYPES declaring the same-named event (`Trigger`, tag 0 in
/// both). Only `Alarm` emits. `b.take_emits(Trigger)` must report `0` —
/// proving `take_emits` filters by (machine-type id, event tag) and never
/// by tag alone. Without the machine-id filter this would misattribute
/// `Alarm`'s queued event to `Beacon` and wrongly report `1` (red).
#[test]
fn run_emit_two_machines_fixture_executes() {
    let repo = repo_root();
    ensure_hew_runtime_lib(&repo);
    let path = repo
        .join("examples")
        .join("machine")
        .join("run_emit_two_machines.hew");
    let expected_path = path.with_extension("expected");
    let expected = std::fs::read_to_string(&expected_path)
        .unwrap_or_else(|e| panic!("read {}: {e}", expected_path.display()));

    let (_build_dir, bin) = build_fixture_binary(&repo, &path, "run_emit_two_machines");
    run_prebuilt(&bin, "run run_emit_two_machines fixture", &expected);
}

/// an all-guards-false step with NO `default` arm must TRAP with a
/// real, documented exit code — never silently fire the wrong arm (the
/// pre-fix bug) and never lower to UB (the `unreachable`-is-not-a-trap
/// hazard). `hew run` catches the trap in main context and exits
/// non-zero with a clean diagnostic (mirrors `hashmap_index_exec`'s
/// `read_absent_key_traps` oracle style), rather than propagating a raw
/// signal — which is exactly what a bare LLVM `unreachable` could do
/// instead (crash with no diagnostic, or silently miscompile).
#[test]
fn guard_fallthrough_no_default_traps_not_ub() {
    let repo = repo_root();
    ensure_hew_runtime_lib(&repo);
    let path = fixture_path(&repo, "guard_fallthrough_traps");

    let mut cmd = hew_command(&repo);
    cmd.arg("run").arg(&path);
    let output = hew_testutil::run_command_bounded(
        &mut cmd,
        format!("hew run {}", path.display()),
        hew_testutil::DEFAULT_EXEC_TIMEOUT,
    )
    .unwrap_or_else(|e| panic!("{e}"));

    let code = output.status.code().unwrap_or_else(|| {
        panic!(
            "guard_fallthrough_traps was killed by signal (expected a clean trap exit, \
             not a raw signal — that would mean the fall-through is still UB); stderr:\n{}",
            String::from_utf8_lossy(&output.stderr)
        )
    });
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_ne!(
        code, 0,
        "an all-guards-false step with no `default` must trap (non-zero exit), got {code}; \
         stderr:\n{stderr}"
    );
    assert!(
        stderr.contains("trap in main context"),
        "the fall-through must surface a real trap diagnostic, not a silent/garbled exit; \
         exit={code}, stderr:\n{stderr}"
    );
}

#[test]
fn counter_machine_example_executes_self_field_increment() {
    let repo = repo_root();

    let machine_source = std::fs::read_to_string(
        repo.join("examples")
            .join("machine")
            .join("counter_machine.hew"),
    )
    .expect("read counter_machine example");
    let src_dir = tempfile::tempdir().expect("create counter machine tempdir");
    let path = src_dir.path().join("counter_machine_run.hew");
    std::fs::write(
        &path,
        format!(
            "{machine_source}\n\
             fn main() {{\n\
             \x20   var c = Zero;\n\
             \x20   c.step(Inc);\n\
             \x20   c.step(Inc);\n\
             \x20   println(c.state_name());\n\
             \x20   let value = match c {{\n\
             \x20       NonZero {{ value }} => value,\n\
             \x20       Zero => 0,\n\
             \x20   }};\n\
             \x20   println(value);\n\
             }}\n"
        ),
    )
    .expect("write counter machine runner");

    let (_build_dir, bin) = build_fixture_binary(&repo, &path, "counter_machine_run");
    run_prebuilt(&bin, "run counter_machine fixture", "NonZero\n2\n");
}
