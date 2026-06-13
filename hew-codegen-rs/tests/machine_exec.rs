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
}

const FIXTURES: &[MachineFixture] = &[
    MachineFixture {
        stem: "run_traffic_light",
        machine: "TrafficLight",
        states: 3,
        has_default: false,
    },
    MachineFixture {
        stem: "run_tcp_handshake",
        machine: "TcpHandshake",
        states: 3,
        has_default: false,
    },
    MachineFixture {
        // `default { state }` machine: unhandled cells stay in the current
        // state. Exercises the `has_default` step-dispatch fall-through that
        // returns `self` unchanged instead of trapping.
        stem: "run_default_stay",
        machine: "Tank",
        states: 2,
        has_default: true,
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
    static BUILT: OnceLock<()> = OnceLock::new();
    BUILT.get_or_init(|| {
        // On Windows MSVC the static library is hew.lib; on Unix it is libhew.a.
        let lib_name = if cfg!(windows) { "hew.lib" } else { "libhew.a" };
        let lib = target_dir(repo).join("debug").join(lib_name);
        if lib.exists() {
            return;
        }
        let cargo = std::env::var_os("CARGO").unwrap_or_else(|| "cargo".into());
        let status = Command::new(cargo)
            .current_dir(repo)
            .args(["build", "--quiet", "-p", "hew-lib"])
            .status()
            .expect("spawn cargo build -p hew-lib");
        assert!(
            status.success(),
            "cargo build -p hew-lib failed: {status:?}"
        );
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

    std::fs::read_to_string(out_dir.join(format!("{}.ll", fixture.stem))).unwrap_or_else(|e| {
        panic!(
            "read emitted LLVM IR for {} from {}: {e}",
            fixture.stem,
            out_dir.display()
        )
    })
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
        } else {
            assert!(
                step_body.contains("unreachable"),
                "{} step dispatch fallthrough must lower to LLVM unreachable:\n{}",
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

#[test]
fn run_machine_fixtures_execute_with_expected_stdout() {
    let repo = repo_root();
    for fixture in FIXTURES {
        let path = fixture_path(&repo, fixture.stem);
        let expected_path = path.with_extension("expected");
        let expected = std::fs::read_to_string(&expected_path)
            .unwrap_or_else(|e| panic!("read {}: {e}", expected_path.display()));

        let (_build_dir, bin) = build_fixture_binary(&repo, &path, fixture.stem);
        run_prebuilt(&bin, &format!("run {} fixture", fixture.stem), &expected);
    }
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
