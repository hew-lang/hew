#!/usr/bin/env python3
"""Executable contracts for source-derived opaque lifecycle compiler cases."""

from __future__ import annotations

import json
import importlib.util
import os
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
AUDIT = ROOT / "scripts/structural-authority-audit.py"
AST_GREP = ROOT / ".ast-grep/tool/bin/ast-grep"
HEW = Path(os.environ.get("HEW_BIN", ROOT / "target/debug/hew"))
AUDIT_TIMEOUT_SECONDS = 60
COMPILER_TIMEOUT_SECONDS = 90

BOUNDED_SPEC = importlib.util.spec_from_file_location(
    "lifecycle_facts_bounded_subprocess",
    ROOT / "scripts/tests/bounded_subprocess.py",
)
assert BOUNDED_SPEC is not None and BOUNDED_SPEC.loader is not None
BOUNDED_MODULE = importlib.util.module_from_spec(BOUNDED_SPEC)
BOUNDED_SPEC.loader.exec_module(BOUNDED_MODULE)
bounded_run = BOUNDED_MODULE.run


def compile_dump(source: Path, stage: str) -> str:
    result = bounded_run(
        [str(HEW), "compile", "--dump-mir", stage, str(source)],
        cwd=ROOT,
        timeout_seconds=COMPILER_TIMEOUT_SECONDS,
    )
    assert result.returncode == 0, (
        f"{source.name} did not reach {stage} MIR\n"
        f"stdout:\n{result.stdout}\nstderr:\n{result.stderr}"
    )
    assert "checks: none" in result.stdout if stage == "checked" else result.stdout
    return result.stdout


def function_section(dump: str, name: str) -> str:
    marker = f"fn {name} ->"
    assert marker in dump, f"missing {name} in MIR dump"
    body = dump.split(marker, 1)[1]
    return body.split("\nfn ", 1)[0]


def main() -> None:
    if not AST_GREP.is_file():
        raise SystemExit("bootstrap pinned ast-grep before lifecycle fact test")
    if not HEW.is_file():
        raise SystemExit("build `hew` or set HEW_BIN before lifecycle compiler test")

    with tempfile.TemporaryDirectory() as temp:
        facts = Path(temp) / "facts.json"
        result = bounded_run(
            [
                "python3",
                str(AUDIT),
                "--ast-grep",
                str(AST_GREP),
                "--opaque-resource-facts",
                str(facts),
                "--opaque-resource-facts-only",
            ],
            cwd=ROOT,
            timeout_seconds=AUDIT_TIMEOUT_SECONDS,
        )
        assert result.returncode == 0, result.stderr
        payload = json.loads(facts.read_text())
        candidates = payload["candidates"]
        cases = payload["compiler_e2e_cases"]
        assert candidates and len(candidates) == len(cases)
        assert {row["carrier_key"] for row in candidates} == {
            row["carrier_key"] for row in cases
        }
        failures = []
        for index, row in enumerate(cases):
            assert "import std." in row["scope_exit_source"]
            assert ".{ " in row["scope_exit_source"]
            assert "fn scope_exit_case(consume value:" in row["scope_exit_source"]
            assert "value.close();" in row["explicit_close_source"]

            scope = Path(temp) / f"{index:02}-scope.hew"
            explicit = Path(temp) / f"{index:02}-explicit.hew"
            scope.write_text(row["scope_exit_source"])
            explicit.write_text(row["explicit_close_source"])

            # `checked` is the real parser/checker/HIR/raw-MIR/checked-MIR path;
            # `elab` then proves the implicit-vs-explicit close disposition.
            try:
                compile_dump(scope, "checked")
                scope_elab = function_section(
                    compile_dump(scope, "elab"), "scope_exit_case"
                )
                assert "kind=resource" in scope_elab, row["carrier_key"]
                assert "fn=user_close(" in scope_elab, row["carrier_key"]

                explicit_checked = function_section(
                    compile_dump(explicit, "checked"), "explicit_close_case"
                )
                assert "call " in explicit_checked and "::close(" in explicit_checked, (
                    row["carrier_key"]
                )
                explicit_elab = function_section(
                    compile_dump(explicit, "elab"), "explicit_close_case"
                )
                return_plan = explicit_elab.split("return[", 1)[-1]
                assert "kind=resource" not in return_plan, row["carrier_key"]
            except AssertionError as error:
                failures.append(f"{row['carrier_key']}: {error}")

        assert not failures, "opaque lifecycle compiler failures:\n" + "\n".join(
            failures
        )


if __name__ == "__main__":
    main()
