#!/usr/bin/env python3
"""Self-tests for parallel stdlib-user-build-clean execution and fail-closed gates."""

from __future__ import annotations

import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "stdlib-user-build-clean.py"

STUB_HEW = r"""#!/usr/bin/env python3
import json
import os
import sys
from pathlib import Path

subcmd = sys.argv[1]
target = Path(sys.argv[2])
std_dir = Path(os.environ["HEW_STD"])

if subcmd == "check" and target.is_file():
    text = target.read_text(encoding="utf-8")
    if "broken_variant_probe" in text:
        print(json.dumps([{
            "file": str(target),
            "code": "E_BARE_VARIANT_PATTERN",
            "severity": "error",
            "message": "bare variant",
        }]))
        sys.exit(1)
    if "inject_stdlib_error" in text:
        print(json.dumps([{
            "file": str(target),
            "code": "E_STDLIB_TEST",
            "severity": "error",
            "message": "stdlib error",
        }]))
        sys.exit(0)
    print("[]")
    sys.exit(0)

if subcmd in ("check", "build"):
    arena = std_dir / "arena.hew"
    if arena.is_file() and "broken_variant_probe" in arena.read_text(encoding="utf-8"):
        print(json.dumps([{
            "file": str(arena),
            "code": "E_BARE_VARIANT_PATTERN",
            "severity": "error",
            "message": "broken stdlib",
        }]))
        sys.exit(1)
    marker = os.environ.get("STUB_PACKAGE_DIRS_LOG")
    if marker and subcmd == "build":
        with open(marker, "a", encoding="utf-8") as fh:
            fh.write(f"{Path.cwd().name}\n")
    print("[]")
    sys.exit(0)

sys.exit(2)
"""


class StdlibUserBuildCleanTests(unittest.TestCase):
    def make_workspace(
        self, raw: str, *, break_beta: bool = False
    ) -> tuple[Path, Path, Path]:
        ws = Path(raw)
        stdlib = ws / "std"
        stdlib.mkdir()
        for name in ("arena", "alpha", "beta", "gamma"):
            body = f"pub fn {name}() -> i64 {{ 1 }}\n"
            if name == "beta" and break_beta:
                body += "// inject_stdlib_error\n"
            (stdlib / f"{name}.hew").write_text(body, encoding="utf-8")
        stub = ws / "hew"
        stub.write_text(STUB_HEW, encoding="utf-8")
        stub.chmod(0o755)
        return ws, stdlib, stub

    def test_parallel_workers_preserve_sorted_order_and_isolated_packages(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            ws, stdlib, stub = self.make_workspace(raw)
            pkg_log = ws / "packages.log"
            env = {"STUB_PACKAGE_DIRS_LOG": str(pkg_log), "PATH": "/usr/bin:/bin"}
            result = subprocess.run(
                [
                    sys.executable,
                    str(SCRIPT),
                    "--hew-bin",
                    str(stub),
                    "--stdlib-dir",
                    str(stdlib),
                    "--jobs",
                    "2",
                ],
                env=env,
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIn("Modules: 4 (jobs: 2)", result.stdout)
            order = [
                line.strip()
                for line in result.stdout.splitlines()
                if line.strip().startswith("[0")
            ]
            self.assertEqual(
                order,
                [
                    "[01/04] std.alpha",
                    "[02/04] std.arena",
                    "[03/04] std.beta",
                    "[04/04] std.gamma",
                ],
            )
            dirs = sorted(pkg_log.read_text(encoding="utf-8").splitlines())
            self.assertEqual(dirs, ["package-1", "package-2", "package-3", "package-4"])

    def test_stdlib_diagnostic_still_fails_under_parallel_execution(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            _, stdlib, stub = self.make_workspace(raw, break_beta=True)
            result = subprocess.run(
                [
                    sys.executable,
                    str(SCRIPT),
                    "--hew-bin",
                    str(stub),
                    "--stdlib-dir",
                    str(stdlib),
                    "--jobs",
                    "2",
                ],
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(result.returncode, 1)
            self.assertIn("E_STDLIB_TEST", result.stderr)

    def test_nonpositive_jobs_is_rejected(self) -> None:
        with tempfile.TemporaryDirectory() as raw:
            _, stdlib, stub = self.make_workspace(raw)
            result = subprocess.run(
                [
                    sys.executable,
                    str(SCRIPT),
                    "--hew-bin",
                    str(stub),
                    "--stdlib-dir",
                    str(stdlib),
                    "--jobs",
                    "0",
                ],
                capture_output=True,
                text=True,
                check=False,
            )
            self.assertEqual(result.returncode, 1)
            self.assertIn("--jobs must be at least 1", result.stderr)


if __name__ == "__main__":
    unittest.main()
