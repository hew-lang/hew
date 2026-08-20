"""Fail-closed contract for build-system tool pins and CI installers."""

import re
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
XTASK = ROOT / "xtask" / "src" / "build_system.rs"
CI = ROOT / ".github" / "workflows" / "ci.yml"
COVERAGE = ROOT / ".github" / "workflows" / "coverage-nightly.yml"
RELEASE_GATE = ROOT / ".github" / "workflows" / "release-gate.yml"
FREEBSD = ROOT / ".github" / "workflows" / "freebsd.yml"
SETUP_WASM_PACK = ROOT / ".github" / "actions" / "setup-wasm-pack" / "action.yml"

PINS = {
    "NEXTEST": ("cargo-nextest", "0.9.99"),
    "CARGO_DENY": ("cargo-deny", "0.19.6"),
    "CARGO_ABOUT": ("cargo-about", "0.9.0"),
    "LLVM_COV": ("cargo-llvm-cov", "0.8.7"),
    "WASM_PACK": ("wasm-pack", "0.13.1"),
    "WASMTIME": ("wasmtime", "47.0.2"),
}


def _xtask_pins(source: str) -> dict[str, tuple[str, str]]:
    return {
        name: (tool, version)
        for name, tool, version in re.findall(
            r'^const (\w+): &str = "([^@]+)@([^"]+)";$', source, re.MULTILINE
        )
    }


def _assert_pins(
    xtask: str,
    ci: str,
    coverage: str,
    release_gate: str,
    freebsd: str,
    setup_wasm_pack: str,
) -> None:
    assert _xtask_pins(xtask) == PINS

    assert "cargo install cargo-deny --locked --version 0.19.6" in ci
    assert "cargo install cargo-about --locked --version 0.9.0 --features cli" in ci
    assert "WASM_PACK_VERSION=0.13.1" in setup_wasm_pack

    for workflow in (ci, coverage, release_gate):
        assert 'WASMTIME_VERSION: "v47.0.2"' in workflow
    assert '$version = "${{ env.WASMTIME_VERSION }}"' in release_gate
    assert 'WASMTIME_VERSION: "v47.0.3"' not in release_gate

    for workflow in (ci, coverage, release_gate):
        for tools in re.findall(r"^\s*tools:\s*(.+)$", workflow, re.MULTILINE):
            for tool in tools.split(","):
                if tool.startswith("nextest"):
                    assert tool == "nextest@0.9.99"
                if tool.startswith("cargo-llvm-cov"):
                    assert tool == "cargo-llvm-cov@0.8.7"
    assert "tools: nextest@0.9.99" in ci
    assert "tools: nextest@0.9.99,cargo-llvm-cov@0.8.7" in coverage
    assert "tools: nextest@0.9.99" in release_gate
    assert "cargo install cargo-nextest --locked --version 0.9.99" in release_gate
    assert "cargo install cargo-nextest --locked --version 0.9.99" in freebsd


def test_workflow_pins_match_the_xtask_graph() -> None:
    _assert_pins(
        XTASK.read_text(),
        CI.read_text(),
        COVERAGE.read_text(),
        RELEASE_GATE.read_text(),
        FREEBSD.read_text(),
        SETUP_WASM_PACK.read_text(),
    )


def test_pin_contract_rejects_workflow_drift() -> None:
    ci = CI.read_text().replace("tools: nextest@0.9.99", "tools: nextest@0.9.98", 1)
    try:
        _assert_pins(
            XTASK.read_text(),
            ci,
            COVERAGE.read_text(),
            RELEASE_GATE.read_text(),
            FREEBSD.read_text(),
            SETUP_WASM_PACK.read_text(),
        )
    except AssertionError:
        return
    raise AssertionError("the tool-pin contract accepted a mismatched CI pin")


if __name__ == "__main__":
    test_workflow_pins_match_the_xtask_graph()
    test_pin_contract_rejects_workflow_drift()
