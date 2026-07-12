"""Unit tests for scripts/check-sandbox-parity-coverage.py.

Primary regression this guards: a VM-dependent hew-sandbox-wasm test file
must be classified as VM-dependent as a WHOLE BINARY, not per-test. An
earlier version of this checker built a same-file call graph and only
flagged the `#[test]` functions it could statically prove reached a
VM-spawning helper, on the theory that a marker function with no unproven
caller must mean the rest of the binary's tests are safe. That was
unsound: proving that test A reaches a marker via the call graph says
nothing about whether some OTHER test B in the same file also reaches it
through a path the graph does not model -- a dispatch table keyed at
runtime, a closure captured and invoked elsewhere, a trait object. Test B
would keep running under generic (unprovisioned) nextest and only fail on
a platform without `hew-sandbox-vm` set up. This is exactly the
"dynamically dispatched second test evades classification" failure mode:
fixed by dropping per-test/call-graph attribution entirely and requiring
that ANY marker anywhere in a file condemns the WHOLE binary.

Also covers REQUIRED_PROFILES itself: profile.default and profile.ci are
not the only generic (unprovisioned) nextest entry points -- `make
test-lane`, `make test-lane-all`, and `make test-fast` all run `cargo
nextest run --profile lane`, so profile.lane must be checked identically
or a VM-dependent binary can leak into that tier unnoticed.
"""

import contextlib
import importlib.util
import io
import re
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "check-sandbox-parity-coverage.py"

spec = importlib.util.spec_from_file_location("check_sandbox_parity_coverage", SCRIPT)
check_sandbox_parity_coverage = importlib.util.module_from_spec(spec)
assert spec.loader is not None
spec.loader.exec_module(check_sandbox_parity_coverage)


def run_script(*args: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [sys.executable, str(SCRIPT), *args],
        cwd=ROOT,
        check=False,
        capture_output=True,
        text=True,
    )


def _remove_binary_exclusion_in_profile(text: str, profile: str, binary: str) -> str:
    """Return `text` with `binary(<binary>)` dropped from `[profile.<profile>]`'s
    default-filter only -- other profiles' filters are left untouched."""
    profile_m = re.search(
        rf"^\[profile\.{re.escape(profile)}\]\s*$", text, re.MULTILINE
    )
    assert profile_m, f"[profile.{profile}] not found in nextest.toml"
    rest = text[profile_m.end() :]
    filter_m = re.search(r'^default-filter\s*=\s*"([^"]*)"', rest, re.MULTILINE)
    assert filter_m, f"default-filter not found in [profile.{profile}]"
    old_value = filter_m.group(1)
    new_value = old_value.replace(f" - binary({binary})", "", 1)
    assert new_value != old_value, (
        f"binary({binary}) not present in [profile.{profile}]"
    )
    old_line = filter_m.group(0)
    new_line = old_line.replace(old_value, new_value, 1)
    abs_start = profile_m.end() + filter_m.start()
    abs_end = profile_m.end() + filter_m.end()
    return text[:abs_start] + new_line + text[abs_end:]


@contextlib.contextmanager
def _sabotaged_nextest_toml(mutated_text: str):
    """Point the module's NEXTEST_TOML at `mutated_text` for the duration of
    the `with` block, then restore the real path."""
    with tempfile.TemporaryDirectory() as tmp_dir:
        sabotaged_path = Path(tmp_dir) / "nextest.toml"
        sabotaged_path.write_text(mutated_text)
        original_path = check_sandbox_parity_coverage.NEXTEST_TOML
        check_sandbox_parity_coverage.NEXTEST_TOML = sabotaged_path
        try:
            yield
        finally:
            check_sandbox_parity_coverage.NEXTEST_TOML = original_path


def _run_main_capturing_output(argv_tail: list[str]) -> tuple[int, str]:
    original_argv = sys.argv
    sys.argv = [str(SCRIPT), *argv_tail]
    buffer = io.StringIO()
    try:
        with contextlib.redirect_stdout(buffer), contextlib.redirect_stderr(buffer):
            exit_code = check_sandbox_parity_coverage.main()
    finally:
        sys.argv = original_argv
    return exit_code, buffer.getvalue()


def test_direct_marker_call_is_detected() -> None:
    source = """
#[test]
fn spawns_directly() {
    ensure_parity_runner_built();
}

fn ensure_parity_runner_built() {
    let vm_dir = repo_root().join("hew-sandbox-vm");
    assert!(vm_dir.join("node_modules").is_dir());
}
"""
    assert check_sandbox_parity_coverage.is_vm_dependent(source) is True


def test_file_with_no_marker_is_not_flagged() -> None:
    source = """
#[test]
fn purely_structural_test() {
    let required: BTreeSet<_> = REQUIRED_NAMES.iter().copied().collect();
    assert_eq!(required.len(), 3);
}

#[test]
fn another_structural_test() {
    assert_eq!(2 + 2, 4);
}
"""
    assert check_sandbox_parity_coverage.is_vm_dependent(source) is False


def test_indirect_call_chain_is_detected() -> None:
    # Reproduces the original hole this checker closed: the test never
    # mentions the spawn helper by name; it calls a plain assertion helper,
    # which is the only thing that calls the actual npm-spawning function.
    # Whole-file substring detection catches this regardless of call depth.
    source = """
#[test]
fn live_gate_style_test() {
    for construct in CONSTRUCTS {
        assert_admitted_runs_clean(construct);
    }
}

fn assert_admitted_runs_clean(construct: &Construct) {
    let sandbox = run_sandbox_inline(&construct.bytecode);
    assert_eq!(sandbox.status.code(), Some(0));
}

fn run_sandbox_inline(bytecode_json: &str) -> Output {
    assert_cmd::Command::new("npm")
        .arg("--prefix")
        .arg(repo_root().join("hew-sandbox-vm"))
        .output()
        .expect("spawn sandbox parity runner")
}
"""
    assert check_sandbox_parity_coverage.is_vm_dependent(source) is True


def test_dynamically_dispatched_second_test_cannot_evade_classification() -> None:
    # The exact regression report: one test (`statically_obvious_test`)
    # reaches the spawn marker through a call the static graph CAN trace.
    # A second test (`dynamically_dispatched_test`) reaches the very same
    # marker only by looking up a function pointer in a table at runtime --
    # something no static call-graph analysis can attribute to a specific
    # test. A per-test attribution scheme could clear
    # `dynamically_dispatched_test` as "not provably VM-dependent" while it
    # actually spawns the VM at runtime. Binary-level, marker-anywhere
    # detection cannot be fooled this way: the whole file (both tests, and
    # any other function in it) is classified VM-dependent unconditionally.
    source = """
#[test]
fn statically_obvious_test() {
    ensure_parity_runner_built();
}

#[test]
fn dynamically_dispatched_test() {
    let handlers: HashMap<&str, fn()> = build_handler_table();
    (handlers["run"])();
}

fn build_handler_table() -> HashMap<&'static str, fn()> {
    let mut table: HashMap<&str, fn()> = HashMap::new();
    table.insert("run", spawn_via_table);
    table
}

fn spawn_via_table() {
    assert_cmd::Command::new("npm")
        .arg("--prefix")
        .arg(repo_root().join("hew-sandbox-vm"))
        .output()
        .expect("spawn");
}

fn ensure_parity_runner_built() {
    let vm_dir = repo_root().join("hew-sandbox-vm");
    assert!(vm_dir.join("node_modules").is_dir());
}
"""
    # Classification is whole-file: it does not matter which test the
    # marker function is (or is not) statically attributable to.
    assert check_sandbox_parity_coverage.is_vm_dependent(source) is True

    # Confirm this is genuinely a binary-level, not per-test, decision: the
    # checker exposes no per-test analysis at all to evade -- there is no
    # function left in the module that computes "is THIS specific test
    # VM-dependent" for a file already known to contain a marker.
    assert not hasattr(check_sandbox_parity_coverage, "transitive_closure")
    assert not hasattr(check_sandbox_parity_coverage, "build_call_graph")


def test_excludes_binary_rejects_a_bare_test_level_filter() -> None:
    # A `test(some_name)` exclusion, with no `binary(...)` naming the
    # VM-dependent binary itself, must NOT satisfy the coverage contract --
    # per-test carve-outs inside a VM-touching binary are exactly what this
    # checker refuses to trust (see module docstring).
    filter_value = "not package(hew-wasm) - test(live_gate_matches_declared_coverage)"
    assert (
        check_sandbox_parity_coverage.excludes_binary(filter_value, "parity_ratchet")
        is False
    )


def test_excludes_binary_accepts_whole_binary_exclusion() -> None:
    filter_value = "not package(hew-wasm) - binary(parity_ratchet)"
    assert (
        check_sandbox_parity_coverage.excludes_binary(filter_value, "parity_ratchet")
        is True
    )


def test_real_parity_ratchet_file_is_whole_binary_vm_dependent() -> None:
    # End-to-end check against the actual repo file: confirms detection
    # doesn't depend on which test the marker is reachable from -- the file
    # is VM-dependent because `run_sandbox_inline` and
    # `ensure_parity_runner_built` exist in it at all, full stop.
    path = ROOT / "hew-sandbox-wasm" / "tests" / "parity_ratchet.rs"
    text = path.read_text()
    assert check_sandbox_parity_coverage.is_vm_dependent(text) is True

    # Even with every direct call to ensure_parity_runner_built() stripped
    # from the file, run_sandbox_inline's own body still names
    # hew-sandbox-vm, so the file remains classified VM-dependent -- proving
    # the classification does not depend on any single call site or test.
    stripped = text.replace(
        "fn live_gate_matches_declared_coverage() {\n    ensure_parity_runner_built();\n",
        "fn live_gate_matches_declared_coverage() {\n",
        1,
    )
    assert stripped != text, "expected replacement text not found in parity_ratchet.rs"
    assert check_sandbox_parity_coverage.is_vm_dependent(stripped) is True


def test_real_repo_state_passes_the_full_check() -> None:
    result = run_script("--verbose")
    assert result.returncode == 0, result.stdout + result.stderr
    assert "0 failed" in result.stdout
    for binary in ("parity", "parity_ratchet", "playground", "ios_subset"):
        assert f"excludes whole binary {binary}" in result.stdout


def test_lane_is_a_required_profile() -> None:
    # make test-lane / make test-lane-all / make test-fast all run generic
    # `cargo nextest run --profile lane`, so it must be checked exactly like
    # profile.default and profile.ci -- not just those two.
    assert "lane" in check_sandbox_parity_coverage.REQUIRED_PROFILES


def test_removing_binary_exclusion_from_profile_lane_fails_the_checker() -> None:
    # Sabotage proof: drop parity_ratchet's `binary(parity_ratchet)` from
    # [profile.lane]'s default-filter ONLY (profile.default and profile.ci
    # keep their real exclusions) and confirm the checker fails specifically
    # on profile.lane -- proving lane is actually enforced, not just listed.
    real_text = check_sandbox_parity_coverage.NEXTEST_TOML.read_text()
    sabotaged_text = _remove_binary_exclusion_in_profile(
        real_text, "lane", "parity_ratchet"
    )

    with _sabotaged_nextest_toml(sabotaged_text):
        default_filter = check_sandbox_parity_coverage.default_filter_line("default")
        ci_filter = check_sandbox_parity_coverage.default_filter_line("ci")
        lane_filter = check_sandbox_parity_coverage.default_filter_line("lane")
        assert (
            check_sandbox_parity_coverage.excludes_binary(
                default_filter, "parity_ratchet"
            )
            is True
        )
        assert (
            check_sandbox_parity_coverage.excludes_binary(ci_filter, "parity_ratchet")
            is True
        )
        assert (
            check_sandbox_parity_coverage.excludes_binary(lane_filter, "parity_ratchet")
            is False
        )

        exit_code, output = _run_main_capturing_output(["--verbose"])

    assert exit_code == 1
    assert (
        "FAIL [profile.lane] does not exclude VM-dependent binary `parity_ratchet`"
        in output
    )
    assert "FAIL [profile.default] does not exclude" not in output
    assert "FAIL [profile.ci] does not exclude" not in output

    # Restoring the real file must make the checker pass again -- confirms
    # the failure above was caused by the sabotage, not a leaked patch.
    exit_code_after_restore, output_after_restore = _run_main_capturing_output(
        ["--verbose"]
    )
    assert exit_code_after_restore == 0, output_after_restore


_TESTS = [
    test_direct_marker_call_is_detected,
    test_file_with_no_marker_is_not_flagged,
    test_indirect_call_chain_is_detected,
    test_dynamically_dispatched_second_test_cannot_evade_classification,
    test_excludes_binary_rejects_a_bare_test_level_filter,
    test_excludes_binary_accepts_whole_binary_exclusion,
    test_real_parity_ratchet_file_is_whole_binary_vm_dependent,
    test_real_repo_state_passes_the_full_check,
    test_lane_is_a_required_profile,
    test_removing_binary_exclusion_from_profile_lane_fails_the_checker,
]

if __name__ == "__main__":
    failures = 0
    for test in _TESTS:
        try:
            test()
            print(f"PASS {test.__name__}")
        except AssertionError as exc:
            print(f"FAIL {test.__name__}: {exc}")
            failures += 1
    if failures:
        raise SystemExit(f"{failures}/{len(_TESTS)} tests failed")
    print(f"All {len(_TESTS)} tests passed.")
