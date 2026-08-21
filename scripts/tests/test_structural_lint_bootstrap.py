#!/usr/bin/env python3
"""Prove a clean structural bootstrap installs before parser-backed tests."""

from __future__ import annotations

import os
import shutil
import subprocess
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]


def main() -> None:
    with tempfile.TemporaryDirectory() as temp:
        work = Path(temp)
        (work / "Cargo.toml").write_text('[workspace]\nresolver = "2"\n')
        shutil.copy(ROOT / "Makefile", work / "Makefile")
        (work / "scripts/tests").mkdir(parents=True)
        shutil.copy(
            ROOT / "scripts/cargo-output-dir.py", work / "scripts/cargo-output-dir.py"
        )
        (work / "scripts/lib").mkdir()
        shutil.copy(
            ROOT / "scripts/lib/toml_compat.py", work / "scripts/lib/toml_compat.py"
        )
        inputs = work / "scripts/libhew-inputs.py"
        inputs.write_text("#!/bin/sh\nprintf 'Makefile\\n'\n")
        inputs.chmod(0o755)
        events = work / "events"

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
        assert result.returncode == 0, result.stdout + result.stderr
        order = events.read_text().splitlines()
        assert order[0] == "bootstrap", order
        assert set(order[1:]) == set(checks.values()), order

    print("structural lint clean-bootstrap ordering: PASS")


if __name__ == "__main__":
    main()
