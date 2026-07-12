"""Unit tests for scripts/check-sandbox-parity-coverage.py.

The primary regression this guards: the coverage checker must detect a test
as VM-dependent even when it reaches the hew-sandbox-vm spawn only through a
chain of same-file helper functions (`test -> helper -> spawn`), not just
when the test's own body contains a literal call to the spawn helper. This
is exactly the shape of `parity_ratchet.rs`'s
`live_gate_matches_declared_coverage`, which calls `run_sandbox_inline`
(the actual npm spawn) only through `assert_admitted_runs_clean` /
`assert_admitted_traps` / `assert_admitted_but_fails`, never directly.
"""

import importlib.util
import subprocess
import sys
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


def vm_tests_in(source: str) -> tuple[list[str], bool]:
    """Run the file-level analysis against synthetic Rust source text."""
    functions = check_sandbox_parity_coverage.extract_functions(source)
    test_names = check_sandbox_parity_coverage.find_test_names(source, functions)
    markers = check_sandbox_parity_coverage.marker_functions(functions)
    graph = check_sandbox_parity_coverage.build_call_graph(functions)

    reachable_from_tests: set[str] = set()
    test_reaches: dict[str, set[str]] = {}
    for name in test_names:
        reached = check_sandbox_parity_coverage.transitive_closure(name, graph)
        test_reaches[name] = reached
        reachable_from_tests |= reached

    orphaned = markers - reachable_from_tests
    if orphaned:
        return list(test_names), True
    return [name for name in test_names if markers & test_reaches[name]], False


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
    vm_tests, fallback = vm_tests_in(source)
    assert vm_tests == ["spawns_directly"]
    assert fallback is False


def test_two_hop_indirect_marker_is_detected() -> None:
    # Reproduces the exact regression: the test never mentions the spawn
    # helper by name; it calls a plain assertion helper, which is the only
    # thing that calls the actual npm-spawning function. A checker that
    # only greps the test's own body for a literal spawn-helper name misses
    # this entirely.
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
        .arg("run")
        .arg("-s")
        .arg("parity:run")
        .output()
        .expect("spawn sandbox parity runner")
}
"""
    vm_tests, fallback = vm_tests_in(source)
    assert vm_tests == ["live_gate_style_test"]
    assert fallback is False


def test_non_vm_structural_test_is_not_flagged() -> None:
    source = """
#[test]
fn purely_structural_test() {
    let required: BTreeSet<_> = REQUIRED_NAMES.iter().copied().collect();
    assert_eq!(required.len(), 3);
}

#[test]
fn live_gate_style_test() {
    assert_admitted_runs_clean(&CONSTRUCTS[0]);
}

fn assert_admitted_runs_clean(construct: &Construct) {
    run_sandbox_inline(&construct.bytecode);
}

fn run_sandbox_inline(bytecode_json: &str) -> Output {
    assert_cmd::Command::new("npm")
        .arg("--prefix")
        .arg(repo_root().join("hew-sandbox-vm"))
        .output()
        .expect("spawn")
}
"""
    vm_tests, fallback = vm_tests_in(source)
    assert vm_tests == ["live_gate_style_test"]
    assert "purely_structural_test" not in vm_tests
    assert fallback is False


def test_orphaned_marker_triggers_binary_level_fallback() -> None:
    # `spawns_via_unseen_path` references hew-sandbox-vm but is never called
    # by any parsed #[test] in this synthetic file (e.g. it might be reached
    # only through a function pointer or trait dispatch this script can't
    # trace). The checker cannot trust per-test attribution for this file,
    # so it must fall back to marking every test VM-dependent.
    source = """
#[test]
fn looks_purely_structural() {
    assert_eq!(2 + 2, 4);
}

fn spawns_via_unseen_path() {
    assert_cmd::Command::new("npm")
        .arg("--prefix")
        .arg(repo_root().join("hew-sandbox-vm"))
        .output()
        .expect("spawn");
}
"""
    vm_tests, fallback = vm_tests_in(source)
    assert fallback is True
    assert vm_tests == ["looks_purely_structural"]


def test_real_parity_ratchet_file_detects_indirect_live_gate() -> None:
    # End-to-end check against the actual repo file: confirms
    # live_gate_matches_declared_coverage is detected as VM-dependent via
    # its real call chain (assert_admitted_runs_clean /
    # assert_admitted_traps / assert_admitted_but_fails -> run_sandbox_inline),
    # not merely because it also happens to call ensure_parity_runner_built
    # directly in the same body.
    path = ROOT / "hew-sandbox-wasm" / "tests" / "parity_ratchet.rs"
    text = path.read_text()
    functions = check_sandbox_parity_coverage.extract_functions(text)
    graph = check_sandbox_parity_coverage.build_call_graph(functions)
    reached = check_sandbox_parity_coverage.transitive_closure(
        "live_gate_matches_declared_coverage", graph
    )
    assert "run_sandbox_inline" in reached, (
        "live_gate_matches_declared_coverage must transitively reach "
        "run_sandbox_inline through assert_admitted_runs_clean/"
        "assert_admitted_traps/assert_admitted_but_fails -- if this fails, "
        "the call graph extraction regressed against the real file shape."
    )

    # Simulate the exact regression report: strip the direct
    # ensure_parity_runner_built() call from the test body and confirm the
    # indirect chain through run_sandbox_inline alone still marks it
    # VM-dependent.
    stripped = text.replace(
        "fn live_gate_matches_declared_coverage() {\n    ensure_parity_runner_built();\n",
        "fn live_gate_matches_declared_coverage() {\n",
        1,
    )
    assert stripped != text, "expected replacement text not found in parity_ratchet.rs"
    vm_tests, fallback = vm_tests_in(stripped)
    # Stripping the direct call also orphans ensure_parity_runner_built
    # itself in the real file (it has no other caller today), which
    # correctly trips the conservative binary-level fallback -- either way,
    # the test the regression is about must still land in the VM-dependent
    # set, which is the property this test exists to prove.
    assert "live_gate_matches_declared_coverage" in vm_tests


def test_real_repo_state_passes_the_full_check() -> None:
    result = run_script("--verbose")
    assert result.returncode == 0, result.stdout + result.stderr
    assert "0 failed" in result.stdout


_TESTS = [
    test_direct_marker_call_is_detected,
    test_two_hop_indirect_marker_is_detected,
    test_non_vm_structural_test_is_not_flagged,
    test_orphaned_marker_triggers_binary_level_fallback,
    test_real_parity_ratchet_file_detects_indirect_live_gate,
    test_real_repo_state_passes_the_full_check,
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
