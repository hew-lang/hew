import importlib.util
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "verify-ffi-symbols.py"

spec = importlib.util.spec_from_file_location("verify_ffi_symbols", SCRIPT)
verify_ffi_symbols = importlib.util.module_from_spec(spec)
assert spec.loader is not None
spec.loader.exec_module(verify_ffi_symbols)


def run_script(*args: str) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        [sys.executable, str(SCRIPT), *args],
        cwd=ROOT,
        check=False,
        capture_output=True,
        text=True,
    )


def test_classify_stable_outputs_sorted_names_only() -> None:
    result = run_script("--classify", "stable", "--validate")
    assert result.returncode == 0, result.stderr
    lines = result.stdout.splitlines()
    assert lines == sorted(lines)
    assert "hew_actor_spawn" in lines
    assert "hew_sched_init" not in lines


def test_classify_internal_outputs_sorted_names_only() -> None:
    result = run_script("--classify", "internal", "--validate")
    assert result.returncode == 0, result.stderr
    lines = result.stdout.splitlines()
    assert lines == sorted(lines)
    assert "hew_sched_init" in lines
    assert "hew_runtime_cleanup" in lines
    assert "hew_actor_spawn" not in lines


def test_validate_covers_every_runtime_export_exactly_once() -> None:
    runtime_exports = verify_ffi_symbols.extract_runtime_exports()
    classification = verify_ffi_symbols.load_jit_symbol_classification()
    assert (
        verify_ffi_symbols.validate_jit_symbol_classification(
            runtime_exports, classification
        )
        == []
    )


def test_validate_reports_missing_symbol_with_classification_file_path() -> None:
    errors = verify_ffi_symbols.validate_jit_symbol_classification(
        {"hew_zzz_test_symbol"},
        {"stable": set(), "internal": set()},
    )
    assert errors == [
        "unclassified runtime exports (1): "
        "hew_zzz_test_symbol "
        f"(update {verify_ffi_symbols.JIT_SYMBOL_CLASSIFICATION})"
    ]


_TESTS = [
    test_classify_stable_outputs_sorted_names_only,
    test_classify_internal_outputs_sorted_names_only,
    test_validate_covers_every_runtime_export_exactly_once,
    test_validate_reports_missing_symbol_with_classification_file_path,
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
