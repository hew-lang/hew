#!/usr/bin/env python3
"""Prove a clean structural bootstrap installs before parser-backed tests."""

from __future__ import annotations

import os
import shutil
import subprocess
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
MAKEFILE = ROOT / "Makefile"


def makefile_parse_inputs() -> tuple[Path, ...]:
    lines = iter(MAKEFILE.read_text(encoding="utf-8").splitlines())
    prefix = "MAKEFILE_PARSE_INPUTS :="
    for line in lines:
        if line.startswith(prefix):
            value = line.removeprefix(prefix)
            break
    else:
        raise AssertionError("Makefile must declare MAKEFILE_PARSE_INPUTS")

    items = []
    while True:
        continued = value.rstrip().endswith("\\")
        items.extend(value.rstrip().removesuffix("\\").split())
        if not continued:
            break
        value = next(lines)

    inputs = tuple(Path(item) for item in items)
    assert inputs, "MAKEFILE_PARSE_INPUTS must not be empty"
    return inputs


PARSE_INPUTS = makefile_parse_inputs()


def run_bootstrap(
    omitted: Path | None = None,
) -> tuple[subprocess.CompletedProcess[str], list[str]]:
    with tempfile.TemporaryDirectory() as temp:
        work = Path(temp)
        (work / "Cargo.toml").write_text('[workspace]\nresolver = "2"\n')
        shutil.copy2(MAKEFILE, work / "Makefile")

        for relative in PARSE_INPUTS:
            if relative == omitted:
                continue
            destination = work / relative
            destination.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(ROOT / relative, destination)

        # The real input enumerator needs the full workspace. This executable
        # double preserves its parse-time contract inside the minimal fixture.
        inputs = work / "scripts/libhew-inputs.py"
        if inputs.exists():
            inputs.write_text("#!/bin/sh\nprintf 'Makefile\\n'\n")
            inputs.chmod(0o755)

        events = work / "events"
        (work / "scripts/tests").mkdir(parents=True)
        bootstrap = work / "scripts/ast-grep-lint.sh"
        bootstrap.write_text(
            "#!/bin/sh\n"
            "set -eu\n"
            '[ "$1" = --bootstrap ]\n'
            "[ ! -e .ast-grep/tool/bin/ast-grep ]\n"
            "mkdir -p .ast-grep/tool/bin\n"
            ": > .ast-grep/tool/bin/ast-grep\n"
            'printf "bootstrap\\n" >> "$BOOTSTRAP_EVENT_LOG"\n'
        )
        bootstrap.chmod(0o755)

        checks = {
            "test_structural_authority_audit.py": "authority",
            "test_ast_grep_contract.sh": "archive-contract",
            "test_structural_lint_bootstrap.py": "bootstrap-contract",
        }
        for filename, event in checks.items():
            check = work / "scripts/tests" / filename
            if filename.endswith(".py"):
                check.write_text(
                    "import os\n"
                    "from pathlib import Path\n"
                    "assert Path('.ast-grep/tool/bin/ast-grep').exists()\n"
                    f"with Path(os.environ['BOOTSTRAP_EVENT_LOG']).open('a') as handle:\n"
                    f"    handle.write('{event}\\n')\n"
                )
            else:
                check.write_text(
                    "#!/bin/sh\n"
                    "set -eu\n"
                    "[ -e .ast-grep/tool/bin/ast-grep ]\n"
                    f'printf "{event}\\n" >> "$BOOTSTRAP_EVENT_LOG"\n'
                )
            check.chmod(0o755)

        result = subprocess.run(
            ["make", "structural-lint-bootstrap"],
            cwd=work,
            env={**os.environ, "BOOTSTRAP_EVENT_LOG": str(events)},
            text=True,
            capture_output=True,
        )
        event_log = events.read_text().splitlines() if events.exists() else []
        return result, event_log


result, order = run_bootstrap()
assert result.returncode == 0, result.stdout + result.stderr
assert order[0] == "bootstrap", order
assert set(order[1:]) == {
    "authority",
    "archive-contract",
    "bootstrap-contract",
}, order

for parse_input in PARSE_INPUTS:
    result, _ = run_bootstrap(omitted=parse_input)
    output = result.stdout + result.stderr
    assert result.returncode != 0, f"make parsed without {parse_input}"
    assert "Makefile:" in output and "***" in output, output

print("structural lint clean-bootstrap ordering: PASS")
