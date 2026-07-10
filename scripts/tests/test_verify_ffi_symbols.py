import contextlib
import importlib.util
import io
import re
import subprocess
import sys
from pathlib import Path
from unittest import mock

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "verify-ffi-symbols.py"
IO_RUNTIME_FFI_FILES = (
    "connection.rs",
    "stream.rs",
    "file_io.rs",
    "process.rs",
    "quic_transport.rs",
    "io_time.rs",
    "transport.rs",
)
CODEGEN_STABLE_IO_EXPORTS = {
    "hew_conn_await_read",
    "hew_listener_await_accept",
    "hew_tcp_read_raw",
}
C_UNWIND_MACHINE_EMIT_EXPORTS = {
    "hew_machine_emit_step_enter",
    "hew_machine_emit_step_exit",
    "hew_machine_emit_step_exit_keep",
    "hew_machine_emit_take",
}

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
    assert "hew_sched_init" not in lines
    assert "hew_runtime_cleanup" in lines
    assert "hew_actor_spawn" not in lines

    codegen_result = run_script("--classify", "codegen-stable", "--validate")
    assert codegen_result.returncode == 0, codegen_result.stderr
    assert "hew_sched_init" in codegen_result.stdout.splitlines()


def test_validate_covers_every_runtime_export_exactly_once() -> None:
    runtime_exports = verify_ffi_symbols.extract_runtime_exports()
    stdlib_exports = verify_ffi_symbols.extract_stdlib_exports()
    classification = verify_ffi_symbols.load_jit_symbol_classification()
    assert (
        verify_ffi_symbols.validate_jit_symbol_classification(
            runtime_exports, stdlib_exports, classification
        )
        == []
    )


def test_validate_reports_missing_symbol_with_classification_file_path() -> None:
    runtime_exports = verify_ffi_symbols.extract_runtime_exports()
    stdlib_exports = verify_ffi_symbols.extract_stdlib_exports()
    classification = verify_ffi_symbols.load_jit_symbol_classification()
    phantom = "hew_zzz_test_symbol"
    errors = verify_ffi_symbols.validate_jit_symbol_classification(
        runtime_exports | {phantom},
        stdlib_exports,
        classification,
    )
    assert errors == [
        "unclassified runtime exports (1): "
        f"{phantom} "
        f"(update {verify_ffi_symbols.JIT_SYMBOL_CLASSIFICATION})"
    ]


def test_validate_rejects_missing_stable_stdlib_export() -> None:
    runtime_exports = verify_ffi_symbols.extract_runtime_exports()
    stdlib_exports = verify_ffi_symbols.extract_stdlib_exports()
    classification = {
        tier: set(symbols)
        for tier, symbols in verify_ffi_symbols.load_jit_symbol_classification().items()
    }
    phantom = "hew_missing_stable_stdlib_export"
    classification["stable-stdlib"].add(phantom)
    stderr = io.StringIO()

    with (
        mock.patch.object(
            verify_ffi_symbols,
            "load_jit_symbol_classification",
            return_value=classification,
        ),
        contextlib.redirect_stderr(stderr),
    ):
        exit_code = verify_ffi_symbols.run_classification_mode(
            verify_ffi_symbols.parse_args(["--validate"]),
            runtime_exports,
            stdlib_exports,
        )

    assert exit_code != 0
    assert (
        f"stable-stdlib classification names not exported by hew-std (1): {phantom}"
    ) in stderr.getvalue()


def test_io_runtime_exports_are_jit_stable() -> None:
    classification = verify_ffi_symbols.load_jit_symbol_classification()
    pattern = re.compile(
        r"#\[no_mangle\]"
        r"(?:\s*#\[[^\]]*(?:\([^)]*\))?[^\]]*\])*"
        r'\s*(?:pub\s+)?(?:unsafe\s+)?extern\s+"C"\s+fn\s+'
        r"(hew_\w+)",
        re.DOTALL,
    )
    io_exports: set[str] = set()
    for file_name in IO_RUNTIME_FFI_FILES:
        source = (ROOT / "hew-runtime" / "src" / file_name).read_text()
        io_exports.update(pattern.findall(source))

    assert io_exports
    assert not (io_exports & classification["internal"])
    assert io_exports & classification["codegen-stable"] == CODEGEN_STABLE_IO_EXPORTS
    assert io_exports - CODEGEN_STABLE_IO_EXPORTS <= classification["stable"]
    assert "hew_shutdown_initiate" in classification["internal"]


def test_c_unwind_machine_emit_exports_are_classified() -> None:
    runtime_exports = verify_ffi_symbols.extract_runtime_exports()
    classification = verify_ffi_symbols.load_jit_symbol_classification()

    assert C_UNWIND_MACHINE_EMIT_EXPORTS <= runtime_exports
    assert {
        "hew_machine_emit_step_enter",
        "hew_machine_emit_step_exit_keep",
        "hew_machine_emit_take",
    } <= classification["codegen-stable"]
    assert "hew_machine_emit_step_exit" in classification["internal"]


_TESTS = [
    test_classify_stable_outputs_sorted_names_only,
    test_classify_internal_outputs_sorted_names_only,
    test_validate_covers_every_runtime_export_exactly_once,
    test_validate_reports_missing_symbol_with_classification_file_path,
    test_validate_rejects_missing_stable_stdlib_export,
    test_io_runtime_exports_are_jit_stable,
    test_c_unwind_machine_emit_exports_are_classified,
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
