"""Fail-closed contract for build-system tool pins and CI installers."""

import re
import shlex
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]
XTASK = ROOT / "xtask" / "src" / "build_system.rs"
CI = ROOT / ".github" / "workflows" / "ci.yml"
COVERAGE = ROOT / ".github" / "workflows" / "coverage-nightly.yml"
RELEASE_GATE = ROOT / ".github" / "workflows" / "release-gate.yml"
FREEBSD = ROOT / ".github" / "workflows" / "freebsd.yml"
SETUP_WASM_PACK = ROOT / ".github" / "actions" / "setup-wasm-pack" / "action.yml"

PINS = {
    "NEXTEST": ("cargo-nextest", "0.9.120"),
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
    assert 'WASMTIME_TAG="${WASMTIME_VERSION}"' in coverage
    assert (
        'gh release download "$WASMTIME_TAG" --repo bytecodealliance/wasmtime'
        in coverage
    )
    assert '$version = "${{ env.WASMTIME_VERSION }}"' in release_gate
    assert 'WASMTIME_VERSION: "v47.0.3"' not in release_gate

    for workflow in (ci, coverage, release_gate):
        for tools in re.findall(r"^\s*tools:\s*(.+)$", workflow, re.MULTILINE):
            for tool in tools.split(","):
                if tool.startswith("nextest"):
                    assert tool == "nextest@0.9.120"
                if tool.startswith("cargo-llvm-cov"):
                    assert tool == "cargo-llvm-cov@0.8.7"
    assert "tools: nextest@0.9.120" in ci
    assert "tools: nextest@0.9.120,cargo-llvm-cov@0.8.7" in coverage
    assert "tools: nextest@0.9.120" in release_gate
    assert "cargo install cargo-nextest --locked --version 0.9.120" in release_gate
    assert "cargo install cargo-nextest --locked --version 0.9.120" in freebsd


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
    ci = CI.read_text().replace("tools: nextest@0.9.120", "tools: nextest@0.9.98", 1)
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


def test_pin_contract_rejects_installer_drift() -> None:
    coverage = COVERAGE.read_text().replace(
        'WASMTIME_TAG="${WASMTIME_VERSION}"', 'WASMTIME_TAG="v47.0.3"', 1
    )
    try:
        _assert_pins(
            XTASK.read_text(),
            CI.read_text(),
            coverage,
            RELEASE_GATE.read_text(),
            FREEBSD.read_text(),
            SETUP_WASM_PACK.read_text(),
        )
    except AssertionError:
        return
    raise AssertionError("the tool-pin contract accepted a mismatched installer pin")


# ── Pinned nextest vs. the flags our scripts actually pass ──────────────────
#
# A pin is only correct if the pinned binary understands every flag the build
# system hands it.  Pinning CI to a nextest older than a flag one of our
# scripts passes turns into a CI-only failure that no local run reproduces
# (`cargo nextest list --no-pager` under a 0.9.99 pin was exactly that).
#
# Every flag reachable from a `cargo nextest` invocation in scripts/ is keyed
# here to the nextest release that introduced it.  An unrecognised flag is a
# hard failure, not a pass: nobody has established its minimum version, so the
# contract refuses to certify the pin.
NEXTEST_BASELINE = (0, 9, 0)
NEXTEST_FLAG_MIN_VERSION: dict[str, tuple[int, int, int]] = {
    "-E": NEXTEST_BASELINE,
    "-p": NEXTEST_BASELINE,
    "--color": NEXTEST_BASELINE,
    "--exclude": NEXTEST_BASELINE,
    "--no-default-features": NEXTEST_BASELINE,
    "--no-fail-fast": NEXTEST_BASELINE,
    "--no-run": NEXTEST_BASELINE,
    "--profile": NEXTEST_BASELINE,
    "--test": NEXTEST_BASELINE,
    "--workspace": NEXTEST_BASELINE,
    "--list-type": (0, 9, 10),  # 0.9.10
    "--cargo-quiet": (0, 9, 58),  # 0.9.58
    "--run-ignored": (0, 9, 76),  # 0.9.76 (`all` / `only` values)
    "--message-format": (0, 9, 117),  # 0.9.117 for `list --message-format oneline`
    "--no-pager": (0, 9, 120),  # 0.9.120, alongside pager support
}


def _parse_version(version: str) -> tuple[int, int, int]:
    major, minor, patch = version.split(".")
    return int(major), int(minor), int(patch)


def _nextest_flags(source: str) -> set[str]:
    """Every flag passed to a `cargo nextest` invocation in a shell script.

    Invocations continue across backslash-continued lines, so the logical
    command is reassembled before tokenising.
    """
    flags: set[str] = set()
    lines = source.split("\n")
    for index, line in enumerate(lines):
        if "cargo nextest" not in line:
            continue
        chunk = [line[line.index("cargo nextest") :]]
        cursor = index
        while cursor + 1 < len(lines) and lines[cursor].rstrip().endswith("\\"):
            cursor += 1
            chunk.append(lines[cursor])
        command = " ".join(part.rstrip().rstrip("\\") for part in chunk)
        try:
            tokens = shlex.split(command)
        except ValueError:
            tokens = command.split()
        for token in tokens:
            if not token.startswith("-") or token == "-":
                continue
            # `--flag=value` and shell-string artefacts (a trailing quote from
            # an embedded command spelling) both normalise to the bare flag.
            flag = token.split("=", 1)[0].rstrip("\"'`")
            flags.add(flag)
    return flags


def _scripted_nextest_flags() -> dict[str, set[str]]:
    return {
        str(script.relative_to(ROOT)): _nextest_flags(script.read_text())
        for script in sorted((ROOT / "scripts").rglob("*.sh"))
        if "cargo nextest" in script.read_text()
    }


def _assert_flags_supported(
    per_script: dict[str, set[str]], pinned_version: str
) -> None:
    pinned = _parse_version(pinned_version)
    for script, flags in per_script.items():
        for flag in sorted(flags):
            minimum = NEXTEST_FLAG_MIN_VERSION.get(flag)
            assert minimum is not None, (
                f"{script} passes `{flag}` to cargo nextest, but the tool-pin "
                "contract has no minimum nextest version recorded for it. Add "
                "it to NEXTEST_FLAG_MIN_VERSION with the release that "
                "introduced it."
            )
            assert minimum <= pinned, (
                f"{script} passes `{flag}`, which nextest gained in "
                f"{'.'.join(str(part) for part in minimum)}, but the pin is "
                f"{pinned_version}."
            )


def test_pinned_nextest_accepts_every_scripted_flag() -> None:
    per_script = _scripted_nextest_flags()
    assert per_script, "no scripts/ shell script invokes cargo nextest"
    _assert_flags_supported(per_script, PINS["NEXTEST"][1])


def test_flag_contract_rejects_a_flag_newer_than_the_pin() -> None:
    try:
        _assert_flags_supported({"fixture.sh": {"--no-pager"}}, "0.9.99")
    except AssertionError:
        return
    raise AssertionError("the flag contract accepted a flag newer than the pin")


def test_flag_contract_rejects_an_unrecorded_flag() -> None:
    try:
        _assert_flags_supported(
            {"fixture.sh": {"--flag-from-the-future"}}, PINS["NEXTEST"][1]
        )
    except AssertionError:
        return
    raise AssertionError("the flag contract accepted a flag with no recorded minimum")


if __name__ == "__main__":
    test_workflow_pins_match_the_xtask_graph()
    test_pin_contract_rejects_workflow_drift()
    test_pin_contract_rejects_installer_drift()
    test_pinned_nextest_accepts_every_scripted_flag()
    test_flag_contract_rejects_a_flag_newer_than_the_pin()
    test_flag_contract_rejects_an_unrecorded_flag()
