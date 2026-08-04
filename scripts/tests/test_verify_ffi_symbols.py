import contextlib
import importlib.util
import io
import re
import subprocess
import sys
import tempfile
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
}
C_UNWIND_MACHINE_EMIT_EXPORTS = {
    "hew_machine_emit_step_enter",
    "hew_machine_emit_step_exit",
    "hew_machine_emit_step_exit_keep",
    "hew_machine_emit_take",
}
LOCAL_PID_STABLE_EXPORTS = {
    "hew_local_pid_actor_id",
    "hew_local_pid_ask",
    "hew_local_pid_ask_with_channel",
    "hew_local_pid_send",
    "hew_local_pid_supervisor_is_running",
    "hew_local_pid_supervisor_stop",
    "hew_local_pid_unlink",
    "hew_supervisor_direct_id",
}
# `link` and `monitor` are NOT user-declarable. Their raw two-argument form
# takes the destination from the caller, so an `extern "rt"` declaration could
# put a lifecycle signal on a third party's system queue. The user surface is
# one-argument -- `link(target)` / `monitor(target)` -- and the MIR lowering
# supplies `hew_actor_self()` as arg0, which makes the destination structurally
# the calling actor. Their `unlink` / `demonitor` siblings stay stable above
# because removing a registration produces no signal.
LOCAL_PID_NON_DECLARABLE_EXPORTS = {
    "hew_local_pid_link",
    "hew_local_pid_monitor",
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


def ownership_errors_for_source(source: str) -> list[str]:
    with tempfile.TemporaryDirectory() as directory:
        classification_path = Path(directory) / "jit-symbol-classification.toml"
        classification_path.write_text(source, encoding="utf-8")
        with mock.patch.object(
            verify_ffi_symbols,
            "JIT_SYMBOL_CLASSIFICATION",
            classification_path,
        ):
            classification = verify_ffi_symbols.load_jit_symbol_classification()
            return verify_ffi_symbols.validate_ownership_contracts(
                classification,
                verify_ffi_symbols.extract_runtime_exports()
                | verify_ffi_symbols.extract_stdlib_exports(),
                verify_ffi_symbols._extract_fn_param_counts(
                    [
                        verify_ffi_symbols.RUNTIME_SRC,
                        verify_ffi_symbols.STDLIB_SRC,
                    ]
                ),
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


def test_local_pid_runtime_surface_is_jit_stable() -> None:
    runtime_exports = verify_ffi_symbols.extract_runtime_exports()
    classification = verify_ffi_symbols.load_jit_symbol_classification()

    assert LOCAL_PID_STABLE_EXPORTS <= runtime_exports
    assert LOCAL_PID_STABLE_EXPORTS <= classification["stable"]
    assert not (LOCAL_PID_STABLE_EXPORTS & classification["codegen-stable"])
    assert not (LOCAL_PID_STABLE_EXPORTS & classification["internal"])

    # The withdrawn pair must stay out of reach of `extern "rt"`: they are real
    # runtime exports, but not user-declarable in any tier.
    assert LOCAL_PID_NON_DECLARABLE_EXPORTS <= runtime_exports
    assert not (LOCAL_PID_NON_DECLARABLE_EXPORTS & classification["stable"])
    assert not (LOCAL_PID_NON_DECLARABLE_EXPORTS & classification["stable-stdlib"])


def test_string_to_bytes_transfer_contract_is_exact() -> None:
    document = verify_ffi_symbols.toml_compat.loads(
        verify_ffi_symbols.JIT_SYMBOL_CLASSIFICATION.read_text(
            encoding=verify_ffi_symbols.SOURCE_ENCODING
        )
    )
    rows = {row["symbol"]: row for row in document["ownership"]["contracts"]}
    assert rows["hew_string_to_bytes"] == {
        "symbol": "hew_string_to_bytes",
        "result": "fresh",
        "params": ["borrow"],
        "release-symbol": "hew_bytes_drop",
        "discharge-depth": "shallow",
        "result-retention": "transferred",
    }


def test_malformed_string_to_bytes_retention_fails_verification() -> None:
    source = verify_ffi_symbols.JIT_SYMBOL_CLASSIFICATION.read_text(
        encoding=verify_ffi_symbols.SOURCE_ENCODING
    )
    good = (
        'symbol = "hew_string_to_bytes"\n'
        'result = "fresh"\n'
        'params = ["borrow"]\n'
        'release-symbol = "hew_bytes_drop"\n'
        'discharge-depth = "shallow"\n'
        'result-retention = "transferred"'
    )
    bad = good.replace(
        'result-retention = "transferred"',
        'result-retention = "callee-keeps-alias"',
    )
    assert source.count(good) == 1, "fixture must target exactly one contract row"

    with tempfile.TemporaryDirectory() as directory:
        malformed = Path(directory) / "jit-symbol-classification.toml"
        malformed.write_text(source.replace(good, bad), encoding="utf-8")
        with mock.patch.object(
            verify_ffi_symbols,
            "JIT_SYMBOL_CLASSIFICATION",
            malformed,
        ):
            classification = verify_ffi_symbols.load_jit_symbol_classification()
            errors = verify_ffi_symbols.validate_ownership_contracts(
                classification,
                verify_ffi_symbols.extract_runtime_exports()
                | verify_ffi_symbols.extract_stdlib_exports(),
                verify_ffi_symbols._extract_fn_param_counts(
                    [
                        verify_ffi_symbols.RUNTIME_SRC,
                        verify_ffi_symbols.STDLIB_SRC,
                    ]
                ),
            )
    assert any(
        "ownership contract for hew_string_to_bytes result-retention must be one of"
        in error
        for error in errors
    ), errors


def test_transferred_result_with_resource_basis_fails_verification() -> None:
    source = verify_ffi_symbols.JIT_SYMBOL_CLASSIFICATION.read_text(
        encoding=verify_ffi_symbols.SOURCE_ENCODING
    )
    transferred = (
        'symbol = "hew_string_to_bytes"\n'
        'result = "fresh"\n'
        'params = ["borrow"]\n'
        'release-symbol = "hew_bytes_drop"\n'
        'discharge-depth = "shallow"\n'
        'result-retention = "transferred"\n'
    )
    assert source.count(transferred) == 1
    malformed = transferred + 'result-retention-basis = "stray resource claim"\n'
    errors = ownership_errors_for_source(source.replace(transferred, malformed))
    assert any(
        "ownership contract for hew_string_to_bytes result-retention-basis is "
        "meaningful only for resource-transfer retention" in error
        for error in errors
    ), errors


def test_resource_transfer_without_body_basis_fails_verification() -> None:
    source = verify_ffi_symbols.JIT_SYMBOL_CLASSIFICATION.read_text(
        encoding=verify_ffi_symbols.SOURCE_ENCODING
    )
    basis = (
        'result-retention-basis = "accept inserts the newly accepted TcpStream under a '
        'fresh table handle; only that returned token selects its hew_tcp_close removal"\n'
    )
    assert source.count(basis) == 1
    errors = ownership_errors_for_source(source.replace(basis, ""))
    assert any(
        "ownership contract for hew_tcp_accept resource-transfer retention requires "
        "a non-empty result-retention-basis" in error
        for error in errors
    ), errors


def test_unmeasured_resource_result_is_accepted_without_mint_authority() -> None:
    source = verify_ffi_symbols.JIT_SYMBOL_CLASSIFICATION.read_text(
        encoding=verify_ffi_symbols.SOURCE_ENCODING
    )
    measured = (
        'result-retention = "resource-transfer"\n'
        'result-retention-basis = "accept inserts the newly accepted TcpStream under a '
        'fresh table handle; only that returned token selects its hew_tcp_close removal"\n'
    )
    assert source.count(measured) == 1
    errors = ownership_errors_for_source(source.replace(measured, ""))
    assert not any(
        "ownership contract for hew_tcp_accept" in error for error in errors
    ), errors


def test_malformed_resource_result_type_fails_verification() -> None:
    source = verify_ffi_symbols.JIT_SYMBOL_CLASSIFICATION.read_text(
        encoding=verify_ffi_symbols.SOURCE_ENCODING
    )
    good = (
        'symbol = "hew_stream_channel"\n'
        'result = "fresh"\n'
        'params = ["borrow"]\n'
        'resource-result-type = "std.stream.StreamPair"'
    )
    bad = good.replace(
        'resource-result-type = "std.stream.StreamPair"',
        'resource-result-type = "StreamPair"',
    )
    assert source.count(good) == 1, "fixture must target exactly one contract row"

    with tempfile.TemporaryDirectory() as directory:
        malformed = Path(directory) / "jit-symbol-classification.toml"
        malformed.write_text(source.replace(good, bad), encoding="utf-8")
        with mock.patch.object(
            verify_ffi_symbols,
            "JIT_SYMBOL_CLASSIFICATION",
            malformed,
        ):
            classification = verify_ffi_symbols.load_jit_symbol_classification()
            errors = verify_ffi_symbols.validate_ownership_contracts(
                classification,
                verify_ffi_symbols.extract_runtime_exports()
                | verify_ffi_symbols.extract_stdlib_exports(),
                verify_ffi_symbols._extract_fn_param_counts(
                    [
                        verify_ffi_symbols.RUNTIME_SRC,
                        verify_ffi_symbols.STDLIB_SRC,
                    ]
                ),
            )
    assert any(
        "ownership contract for hew_stream_channel resource-result-type must be a qualified nominal"
        in error
        for error in errors
    ), errors


def test_malformed_resource_param_types_fails_verification() -> None:
    source = verify_ffi_symbols.JIT_SYMBOL_CLASSIFICATION.read_text(
        encoding=verify_ffi_symbols.SOURCE_ENCODING
    )
    good = 'resource-param-types = ["std.fs.FileReadStream"]'
    bad = 'resource-param-types = ["FileReadStream"]'
    assert source.count(good) >= 1, "fixture must target at least one contract row"

    with tempfile.TemporaryDirectory() as directory:
        malformed = Path(directory) / "jit-symbol-classification.toml"
        malformed.write_text(source.replace(good, bad, 1), encoding="utf-8")
        with mock.patch.object(
            verify_ffi_symbols,
            "JIT_SYMBOL_CLASSIFICATION",
            malformed,
        ):
            classification = verify_ffi_symbols.load_jit_symbol_classification()
            errors = verify_ffi_symbols.validate_ownership_contracts(
                classification,
                verify_ffi_symbols.extract_runtime_exports()
                | verify_ffi_symbols.extract_stdlib_exports(),
                verify_ffi_symbols._extract_fn_param_counts(
                    [
                        verify_ffi_symbols.RUNTIME_SRC,
                        verify_ffi_symbols.STDLIB_SRC,
                    ]
                ),
            )
    assert any(
        "resource-param-types[0] must be a qualified nominal" in error
        for error in errors
    ), errors


_TESTS = [
    test_classify_stable_outputs_sorted_names_only,
    test_classify_internal_outputs_sorted_names_only,
    test_validate_covers_every_runtime_export_exactly_once,
    test_validate_reports_missing_symbol_with_classification_file_path,
    test_validate_rejects_missing_stable_stdlib_export,
    test_io_runtime_exports_are_jit_stable,
    test_c_unwind_machine_emit_exports_are_classified,
    test_local_pid_runtime_surface_is_jit_stable,
    test_string_to_bytes_transfer_contract_is_exact,
    test_malformed_string_to_bytes_retention_fails_verification,
    test_transferred_result_with_resource_basis_fails_verification,
    test_resource_transfer_without_body_basis_fails_verification,
    test_unmeasured_resource_result_is_accepted_without_mint_authority,
    test_malformed_resource_result_type_fails_verification,
    test_malformed_resource_param_types_fails_verification,
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
