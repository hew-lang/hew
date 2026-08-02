#!/usr/bin/env python3
"""Execute source-derived runtime and Wasm opaque-resource lifecycle evidence."""

from __future__ import annotations

import argparse
import copy
import json
import os
import subprocess
import tempfile
import tomllib
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
AUDIT = ROOT / "scripts/structural-authority-audit.py"
AST_GREP = ROOT / ".ast-grep/tool/bin/ast-grep"
MANIFEST = ROOT / "scripts/opaque-resource-lifecycle-evidence.json"
HEW = Path(os.environ.get("HEW_BIN", ROOT / "target/debug/hew"))


def fail(message: str) -> None:
    raise AssertionError(message)


def source_key(row: dict[str, object]) -> tuple[str, str]:
    return str(row["source_path"]), str(row["resource"])


def validate_rows(candidates: list[dict], rows: list[dict]) -> dict[str, dict]:
    candidate_by_source = {source_key(row): row for row in candidates}
    if len(candidate_by_source) != len(candidates):
        fail("AST lifecycle facts contain duplicate source identities")
    evidence_by_source = {source_key(row): row for row in rows}
    if len(evidence_by_source) != len(rows):
        fail("lifecycle evidence contains duplicate source identities")
    missing = sorted(set(candidate_by_source) - set(evidence_by_source))
    extra = sorted(set(evidence_by_source) - set(candidate_by_source))
    if missing or extra:
        fail(f"source/evidence mismatch: missing={missing}, extra={extra}")

    result = {}
    for identity, candidate in candidate_by_source.items():
        evidence = evidence_by_source[identity]
        if "carrier_key" in evidence:
            fail(
                "carrier keys must be derived from shipped AST facts, not stored in evidence"
            )
        if evidence.get("release_symbol") != candidate.get("release_symbol"):
            fail(f"{candidate['carrier_key']} release authority is stale")
        runtime = evidence.get("runtime", {})
        if runtime.get("valid_handle") is not True:
            fail(
                f"{candidate['carrier_key']} runtime evidence is not a valid-handle proof"
            )
        if not runtime.get("path") or not runtime.get("test"):
            fail(f"{candidate['carrier_key']} runtime evidence is incomplete")
        if runtime.get("execution_profile") not in {"local", "external-network"}:
            fail(f"{candidate['carrier_key']} has an invalid runtime execution profile")
        wasm = evidence.get("wasm", {})
        if wasm.get("profile") != "wasm32-wasi":
            fail(f"{candidate['carrier_key']} has no exact wasm32-wasi evidence")
        if wasm.get("disposition") not in {"accepted", "rejected"}:
            fail(f"{candidate['carrier_key']} has an invalid Wasm disposition")
        result[str(candidate["carrier_key"])] = evidence
    return result


def expect_counterfactual_failure(candidates: list[dict], rows: list[dict]) -> None:
    try:
        validate_rows(candidates, rows)
    except AssertionError:
        return
    fail("counterfactual unexpectedly passed")


def run_counterfactuals(candidates: list[dict], rows: list[dict]) -> None:
    expect_counterfactual_failure(candidates, rows[:-1])
    extra = copy.deepcopy(rows)
    extra.append({**copy.deepcopy(rows[0]), "source_path": "std/fake/fake.hew"})
    expect_counterfactual_failure(candidates, extra)
    expect_counterfactual_failure(candidates[1:], rows)
    source_added = copy.deepcopy(candidates)
    source_added.append(
        {**copy.deepcopy(candidates[0]), "source_path": "std/new/new.hew"}
    )
    expect_counterfactual_failure(source_added, rows)
    stale = copy.deepcopy(rows)
    stale[0]["runtime"]["test"] = ""
    expect_counterfactual_failure(candidates, stale)
    synthetic = copy.deepcopy(rows)
    synthetic[0]["source_path"] = "tests/synthetic_resource.hew"
    expect_counterfactual_failure(candidates, synthetic)
    false_unsupported = copy.deepcopy(rows)
    false_unsupported[0]["wasm"]["disposition"] = "unsupported"
    expect_counterfactual_failure(candidates, false_unsupported)
    invalid_profile = copy.deepcopy(rows)
    invalid_profile[0]["runtime"]["execution_profile"] = "synthetic"
    expect_counterfactual_failure(candidates, invalid_profile)

    try:
        assert_wasm_disposition("counterfactual", "rejected", 0, "")
    except AssertionError:
        pass
    else:
        fail("false Wasm rejection unexpectedly passed")

    # Same-leaf resources remain distinct because identity includes source path.
    collision_candidates = [
        {
            "source_path": "std/left/value.hew",
            "resource": "Value",
            "carrier_key": "left.Value",
            "release_symbol": "left_value_free",
        },
        {
            "source_path": "std/right/value.hew",
            "resource": "Value",
            "carrier_key": "right.Value",
            "release_symbol": "right_value_free",
        },
    ]
    collision_rows = [
        {
            "source_path": row["source_path"],
            "resource": "Value",
            "release_symbol": row["release_symbol"],
            "runtime": {
                "path": "x",
                "test": "x",
                "valid_handle": True,
                "execution_profile": "local",
            },
            "wasm": {"profile": "wasm32-wasi", "disposition": "accepted"},
        }
        for row in collision_candidates
    ]
    assert set(validate_rows(collision_candidates, collision_rows)) == {
        "left.Value",
        "right.Value",
    }


def cargo_test_command(anchor: dict[str, object]) -> list[str]:
    path = ROOT / str(anchor["path"])
    if not path.is_file():
        fail(f"stale runtime path: {path.relative_to(ROOT)}")
    source = path.read_text()
    if f"fn {anchor['test']}(" not in source:
        fail(f"stale runtime test: {anchor['test']} in {path.relative_to(ROOT)}")
    cargo_root = path.parent
    while cargo_root != ROOT and not (cargo_root / "Cargo.toml").is_file():
        cargo_root = cargo_root.parent
    cargo_toml = cargo_root / "Cargo.toml"
    if not cargo_toml.is_file():
        fail(f"runtime anchor has no Cargo package: {path.relative_to(ROOT)}")
    package = tomllib.loads(cargo_toml.read_text())["package"]["name"]
    command = ["cargo", "test", "-p", package]
    try:
        relative = path.relative_to(cargo_root)
    except ValueError as error:
        raise AssertionError(path) from error
    if len(relative.parts) >= 2 and relative.parts[0] == "tests":
        command += ["--test", path.stem]
    else:
        command += ["--lib"]
    command += [str(anchor["test"]), "--", "--nocapture"]
    return command


def run_runtime_evidence(evidence: dict[str, dict], profile: str) -> None:
    completed: dict[tuple[str, ...], subprocess.CompletedProcess[str]] = {}
    failures = []
    executed = 0
    deferred = 0
    for carrier, row in evidence.items():
        command = cargo_test_command(row["runtime"])
        if (
            profile == "local"
            and row["runtime"]["execution_profile"] == "external-network"
        ):
            deferred += 1
            continue
        executed += 1
        key = tuple(command)
        if key not in completed:
            env = os.environ.copy()
            env["RUSTC_WRAPPER"] = ""
            completed[key] = subprocess.run(
                command, cwd=ROOT, env=env, text=True, capture_output=True
            )
        result = completed[key]
        output = result.stdout + result.stderr
        if (
            result.returncode != 0
            or "test result: ok" not in output
            or "0 passed" in output
        ):
            failures.append(f"{carrier}: {' '.join(command)}\n{output}")
    if failures:
        fail("runtime lifecycle evidence failures:\n" + "\n".join(failures))
    print(
        f"runtime lifecycle evidence: {executed} rows executed, "
        f"{deferred} external-network rows deferred"
    )


def assert_wasm_disposition(
    carrier: str, expected: str, returncode: int, output: str
) -> None:
    actual = "accepted" if returncode == 0 else "rejected"
    if actual != expected:
        fail(f"{carrier}: expected {expected}, got {actual}\n{output}")
    if actual == "rejected":
        diagnostic = output.lower()
        if (
            "not supported on wasm32" not in diagnostic
            and "not available on target" not in diagnostic
        ):
            fail(f"{carrier}: rejection was not a platform disposition\n{diagnostic}")


def run_wasm_evidence(cases: list[dict], evidence: dict[str, dict], temp: Path) -> None:
    failures = []
    for index, case in enumerate(cases):
        carrier = str(case["carrier_key"])
        source = temp / f"{index:02}-wasm.hew"
        source.write_text(str(case["scope_exit_source"]))
        result = subprocess.run(
            [
                str(HEW),
                "compile",
                "--target",
                "wasm32-wasi",
                "--dump-mir",
                "checked",
                str(source),
            ],
            cwd=ROOT,
            text=True,
            capture_output=True,
        )
        expected = evidence[carrier]["wasm"]["disposition"]
        try:
            assert_wasm_disposition(
                carrier, expected, result.returncode, result.stdout + result.stderr
            )
        except AssertionError as error:
            failures.append(str(error))
    if failures:
        fail("Wasm lifecycle evidence failures:\n" + "\n".join(failures))


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--runtime-profile",
        choices=("local", "external-network"),
        default="local",
    )
    args = parser.parse_args()
    if not AST_GREP.is_file() or not HEW.is_file():
        raise SystemExit("bootstrap ast-grep and build `hew` before lifecycle matrix")

    with tempfile.TemporaryDirectory() as directory:
        temp = Path(directory)
        facts_path = temp / "facts.json"
        audit = subprocess.run(
            [
                "python3",
                str(AUDIT),
                "--ast-grep",
                str(AST_GREP),
                "--opaque-resource-facts",
                str(facts_path),
                "--opaque-resource-facts-only",
            ],
            cwd=ROOT,
            text=True,
            capture_output=True,
        )
        assert audit.returncode == 0, audit.stderr
        facts = json.loads(facts_path.read_text())
        manifest = json.loads(MANIFEST.read_text())
        assert manifest["schema_version"] == 2
        evidence = validate_rows(facts["candidates"], manifest["resources"])
        run_counterfactuals(facts["candidates"], manifest["resources"])
        run_wasm_evidence(facts["compiler_e2e_cases"], evidence, temp)
        run_runtime_evidence(evidence, args.runtime_profile)


if __name__ == "__main__":
    main()
