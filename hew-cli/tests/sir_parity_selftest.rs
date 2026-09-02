//! Self-test for `scripts/sir-parity.sh`, the SIR-route versus legacy-route
//! execution parity harness.
//!
//! A parity mismatch can only come from a compiler defect, which no fixture
//! can promise to reproduce. The detection logic is exercised instead with a
//! stand-in `hew` that emits a script as the "binary" and varies its stdout
//! and exit status by whether `--sir-lower` was requested. One test drives the
//! real compiler over the committed fixture to prove the harness and the
//! compiler agree on where the binary lands.
#![cfg(unix)]

mod support;

use std::fs;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use support::{describe_output, hew_binary, repo_root, require_codegen};

const PROGRAM: &str = "fn main() -> i64 {\n    0\n}\n";
const LIBRARY: &str = "fn helper() -> i64 {\n    0\n}\n";

/// A `hew` stand-in. It understands only `compile [--sir-lower] --emit-dir
/// DIR FILE` and writes `DIR/<stem>` as a shell script whose stdout and exit
/// status come from the environment, per route. `SHIM_REFUSE_SIR=1` makes the
/// `--sir-lower` compile fail, which the harness must read as "not admitted".
const SHIM: &str = r#"#!/usr/bin/env bash
set -eu
mode=legacy
dir=""
file=""
while [ $# -gt 0 ]; do
    case "$1" in
        compile) shift ;;
        --sir-lower) mode=sir; shift ;;
        --emit-dir) dir="$2"; shift 2 ;;
        *) file="$1"; shift ;;
    esac
done
if [ "$mode" = sir ] && [ "${SHIM_REFUSE_SIR:-0}" = 1 ]; then
    echo "SIR strict lowering failed: shim refusal" >&2
    exit 1
fi
if [ "$mode" = sir ]; then
    text="${SHIM_SIR_STDOUT:-same}"
    code="${SHIM_SIR_EXIT:-0}"
else
    text="${SHIM_LEGACY_STDOUT:-same}"
    code="${SHIM_LEGACY_EXIT:-0}"
fi
stem="$(basename "$file" .hew)"
mkdir -p "$dir"
out="$dir/$stem"
printf '#!/usr/bin/env bash\nprintf %%s "%s"\nexit %s\n' "$text" "$code" > "$out"
chmod +x "$out"
"#;

fn write_executable(path: &Path, content: &str) {
    fs::write(path, content).expect("write script");
    fs::set_permissions(path, fs::Permissions::from_mode(0o755)).expect("chmod script");
}

struct Harness {
    dir: tempfile::TempDir,
    shim: PathBuf,
    corpus: PathBuf,
}

impl Harness {
    fn new(files: &[(&str, &str)]) -> Self {
        let dir = support::tempdir();
        let shim = dir.path().join("hew");
        write_executable(&shim, SHIM);
        let corpus = dir.path().join("corpus");
        fs::create_dir_all(&corpus).expect("create corpus dir");
        for (name, content) in files {
            fs::write(corpus.join(name), content).expect("write corpus file");
        }
        Self { dir, shim, corpus }
    }

    fn run(&self, env: &[(&str, &str)]) -> Output {
        self.run_with_ratchet(None, env)
    }

    fn run_with_ratchet(&self, ratchet: Option<&Path>, env: &[(&str, &str)]) -> Output {
        let mut command = Command::new("bash");
        command
            .arg(repo_root().join("scripts/sir-parity.sh"))
            .arg("--hew-bin")
            .arg(&self.shim)
            .arg("--workdir")
            .arg(self.dir.path().join("work"));
        if let Some(ratchet) = ratchet {
            command.arg("--ratchet").arg(ratchet);
        }
        command
            .arg(&self.corpus)
            .current_dir(repo_root())
            .env_remove("HEW_BIN");
        for (key, value) in env {
            command.env(key, value);
        }
        support::run_bounded_command(command, "scripts/sir-parity.sh with the shim compiler")
    }

    fn write_ratchet(&self, contents: &str) -> PathBuf {
        let path = self.dir.path().join("ratchet.txt");
        fs::write(&path, contents).expect("write ratchet");
        path
    }
}

fn stdout(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout).into_owned()
}

#[test]
fn identical_routes_reach_parity() {
    let harness = Harness::new(&[("prog.hew", PROGRAM)]);
    let output = harness.run(&[]);
    assert!(
        output.status.success(),
        "byte-identical stdout and equal exit status must pass:\n{}",
        describe_output(&output)
    );
    let text = stdout(&output);
    assert!(
        text.lines().any(|line| line.starts_with("PARITY ")),
        "the compared program must be listed as parity:\n{text}"
    );
    assert!(
        text.contains("sir-parity: 1 compared, 0 mismatch(es), 0 not admitted"),
        "{text}"
    );
}

#[test]
fn a_stdout_difference_is_a_mismatch() {
    let harness = Harness::new(&[("prog.hew", PROGRAM)]);
    let output = harness.run(&[("SHIM_SIR_STDOUT", "sir"), ("SHIM_LEGACY_STDOUT", "legacy")]);
    assert_eq!(
        output.status.code(),
        Some(1),
        "a stdout difference must fail the harness:\n{}",
        describe_output(&output)
    );
    let text = stdout(&output);
    assert!(
        text.lines()
            .any(|line| line.starts_with("MISMATCH ") && line.ends_with("prog.hew")),
        "the mismatching program must be named:\n{text}"
    );
    assert!(text.contains("stdout differs"), "{text}");
    assert!(
        text.contains("sir-parity: 1 compared, 1 mismatch(es), 0 not admitted"),
        "{text}"
    );
}

#[test]
fn an_exit_status_difference_is_a_mismatch_even_with_identical_stdout() {
    let harness = Harness::new(&[("prog.hew", PROGRAM)]);
    let output = harness.run(&[("SHIM_SIR_EXIT", "3")]);
    assert_eq!(
        output.status.code(),
        Some(1),
        "an exit status difference must fail the harness:\n{}",
        describe_output(&output)
    );
    let text = stdout(&output);
    assert!(
        text.contains("exit status differs: sir=3 legacy=0"),
        "both statuses must be quoted:\n{text}"
    );
    assert!(
        !text.contains("stdout differs"),
        "identical stdout must not be reported as differing:\n{text}"
    );
}

#[test]
fn a_program_the_sir_route_refuses_is_not_admitted_and_an_empty_comparison_fails() {
    let harness = Harness::new(&[("prog.hew", PROGRAM)]);
    let output = harness.run(&[("SHIM_REFUSE_SIR", "1")]);
    assert_eq!(
        output.status.code(),
        Some(1),
        "comparing nothing must not pass:\n{}",
        describe_output(&output)
    );
    let text = stdout(&output);
    assert!(
        text.contains("not admitted by the SIR route (1)"),
        "the refused program must be listed as not admitted:\n{text}"
    );
    assert!(
        text.contains("sir-parity: 0 compared, 0 mismatch(es), 1 not admitted"),
        "{text}"
    );
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("nothing was compared"),
        "{}",
        describe_output(&output)
    );
}

#[test]
fn ratchet_fails_when_the_recorded_count_is_higher_than_measured() {
    let harness = Harness::new(&[("prog.hew", PROGRAM)]);
    let ratchet = harness.write_ratchet("2\n");
    let output = harness.run_with_ratchet(Some(&ratchet), &[]);
    assert_eq!(
        output.status.code(),
        Some(1),
        "a drop below the recorded compared-count must fail:\n{}",
        describe_output(&output)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("ratchet dropped")
            && stderr.contains("records 2")
            && stderr.contains("measured 1"),
        "the failure must quote both sides:\n{stderr}"
    );
}

#[test]
fn ratchet_holds_when_equal_and_reports_a_rise_without_failing() {
    let harness = Harness::new(&[("prog.hew", PROGRAM)]);
    let ratchet = harness.write_ratchet("1\n");
    let equal = harness.run_with_ratchet(Some(&ratchet), &[]);
    assert!(
        equal.status.success(),
        "an equal ratchet must hold:\n{}",
        describe_output(&equal)
    );
    assert!(
        String::from_utf8_lossy(&equal.stderr).contains("ratchet holds at 1"),
        "{}",
        describe_output(&equal)
    );

    let ratchet = harness.write_ratchet("0\n");
    let rise = harness.run_with_ratchet(Some(&ratchet), &[("RATCHET_STRICT_RECOVERIES", "0")]);
    assert!(
        rise.status.success(),
        "a rise is a recovery to record, never a failure by default:\n{}",
        describe_output(&rise)
    );
    let stderr = String::from_utf8_lossy(&rise.stderr);
    assert!(
        stderr.contains("ratchet can rise") && stderr.contains("update the file to 1"),
        "the rise must say what to record:\n{stderr}"
    );

    let strict = harness.run_with_ratchet(Some(&ratchet), &[("RATCHET_STRICT_RECOVERIES", "1")]);
    assert_eq!(
        strict.status.code(),
        Some(1),
        "an unrecorded rise must fail under strict-recoveries accounting:\n{}",
        describe_output(&strict)
    );
}

#[test]
fn a_non_numeric_parity_ratchet_fails_closed() {
    let harness = Harness::new(&[("prog.hew", PROGRAM)]);
    let ratchet = harness.write_ratchet("not-a-number\n");
    let output = harness.run_with_ratchet(Some(&ratchet), &[]);
    assert_eq!(
        output.status.code(),
        Some(2),
        "a non-integer ratchet must fail closed, not be silently ignored:\n{}",
        describe_output(&output)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("must hold one non-negative integer"),
        "{stderr}"
    );
}

#[test]
fn files_without_main_are_not_programs_and_an_empty_selection_is_refused() {
    let harness = Harness::new(&[("lib.hew", LIBRARY)]);
    let output = harness.run(&[]);
    assert!(
        !output.status.success(),
        "a corpus with no program must be refused, not passed vacuously:\n{}",
        describe_output(&output)
    );
    assert!(
        String::from_utf8_lossy(&output.stderr).contains("selected nothing"),
        "{}",
        describe_output(&output)
    );
}

/// The committed fixture is a program the strict lane admits today; driving
/// the real compiler through the script proves the two agree on the emitted
/// binary's location and that the routes execute identically.
#[test]
fn the_committed_fixture_reaches_parity_with_the_real_compiler() {
    require_codegen();
    let dir = support::tempdir();
    let mut command = Command::new("bash");
    command
        .arg(repo_root().join("scripts/sir-parity.sh"))
        .arg("--hew-bin")
        .arg(hew_binary())
        .arg("--workdir")
        .arg(dir.path().join("work"))
        .arg(repo_root().join("hew-cli/tests/fixtures/sir-parity"))
        .current_dir(repo_root());
    let output = support::run_bounded_command(command, "scripts/sir-parity.sh over the fixture");
    assert!(
        output.status.success(),
        "the admitted fixture must compile through both routes and agree:\n{}",
        describe_output(&output)
    );
    let text = stdout(&output);
    assert!(
        text.contains("PARITY ") && text.contains("closed_direct_calls.hew"),
        "{text}"
    );
    assert!(text.contains("0 mismatch(es)"), "{text}");
}
