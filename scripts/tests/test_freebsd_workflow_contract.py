"""Static checks for the FreeBSD workflow's platform contract."""

from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
WORKFLOW = ROOT / ".github" / "workflows" / "freebsd.yml"
RELEASE_GATE = ROOT / ".github" / "workflows" / "release-gate.yml"

WASI_FILTER = (
    "-E 'not ((test(eval_wasm) & not test(reject)) | "
    "(binary(wasi_run_e2e) & not test(native_)))'"
)


def test_freebsd_workflow_matches_release_gate_wasi_filter() -> None:
    workflow = WORKFLOW.read_text()
    release_gate = RELEASE_GATE.read_text()

    assert WASI_FILTER in release_gate
    assert workflow.count(WASI_FILTER) == 1
    assert "--no-fail-fast" in workflow


def test_freebsd_workflow_provisions_and_probes_wasi_tools() -> None:
    workflow = WORKFLOW.read_text()

    assert (
        "pkg install -y llvm22 rust cmake ninja git pkgconf libffi libxml2 wasmtime"
        in workflow
    )
    assert "ln -sf /usr/local/llvm22/bin/wasm-ld /usr/local/bin/wasm-ld" in workflow
    assert "command -v wasmtime && wasmtime --version" in workflow
    assert "command -v wasm-ld && wasm-ld --version" in workflow


def test_freebsd_workflow_keeps_exclusions_narrow() -> None:
    workflow = WORKFLOW.read_text()
    command = workflow.split("cargo nextest run --workspace", 1)[1].split("\n", 4)
    command_text = "\n".join(command)

    assert "--exclude hew-wasm --exclude hew-cabi" in command_text
    assert "--exclude hew-runtime" not in command_text
    assert "--exclude hew-cli" not in command_text
    assert "test(reject)" in command_text
    assert "test(native_)" in command_text


TESTS = (
    test_freebsd_workflow_matches_release_gate_wasi_filter,
    test_freebsd_workflow_provisions_and_probes_wasi_tools,
    test_freebsd_workflow_keeps_exclusions_narrow,
)


if __name__ == "__main__":
    failures = 0
    for test in TESTS:
        try:
            test()
            print(f"PASS {test.__name__}")
        except AssertionError as exc:
            print(f"FAIL {test.__name__}: {exc}")
            failures += 1
    if failures:
        raise SystemExit(f"{failures}/{len(TESTS)} tests failed")
    print(f"All {len(TESTS)} tests passed.")
