#!/usr/bin/env python3
"""Opt-in diagnostic for the README's build and lint prerequisites."""

import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tomllib


ROOT = Path(__file__).resolve().parents[1]


def main() -> int:
    rust = tomllib.loads((ROOT / "rust-toolchain.toml").read_text())["toolchain"][
        "channel"
    ]
    workflow = (ROOT / ".github/workflows/ci.yml").read_text()
    shellcheck = re.search(r'^  SHELLCHECK_VERSION: "([^"]+)"$', workflow, re.M).group(
        1
    )
    llvm_prefix = os.environ.get("LLVM_SYS_221_PREFIX")
    llvm = (
        str(Path(llvm_prefix) / "bin/llvm-config")
        if llvm_prefix
        else (shutil.which("llvm-config-22") or "llvm-config")
    )
    # Only tools with a project version requirement constrain the output.
    # LLVM patch releases share the supported 22.1 API.
    commands = [
        (
            f"Rust (requires {rust})",
            ["rustc", "--version"],
            rf"^rustc {re.escape(rust)}\b",
        ),
        (
            f"Cargo (requires {rust})",
            ["cargo", "--version"],
            rf"^cargo {re.escape(rust)}\b",
        ),
        ("LLVM (requires 22.1.x)", [llvm, "--version"], r"^22\.1\."),
        ("GNU Make", ["make", "--version"], r"^GNU Make "),
        ("Git", ["git", "--version"], None),
        ("Bash", ["bash", "--version"], None),
        ("C compiler", [shutil.which("cc") or "clang", "--version"], None),
        ("clang-format", ["clang-format", "--version"], None),
        ("actionlint", ["actionlint", "--version"], None),
        (
            f"ShellCheck (requires {shellcheck})",
            ["shellcheck", "--version"],
            rf"^version: {re.escape(shellcheck)}$",
        ),
    ]
    failed = sys.version_info < (3, 12)
    print(
        f"{'FAIL' if failed else 'OK'} Python: {sys.version.split()[0]} (requires 3.12+)"
    )
    for name, command, required in commands:
        try:
            result = subprocess.run(command, capture_output=True, text=True, timeout=30)
            output = (result.stdout + result.stderr).strip()
            good = result.returncode == 0 and bool(output)
            if required:
                good = good and re.search(required, output, re.M) is not None
            version = re.search(r"^version: (.+)$", output, re.M)
            summary = (
                version.group(1)
                if version
                else next(iter(output.splitlines()), "no version output")
            )
            print(f"{'OK' if good else 'FAIL'} {name}: {summary}")
            failed |= not good
        except (OSError, subprocess.TimeoutExpired) as error:
            print(f"FAIL {name}: {error}")
            failed = True
    if failed:
        print(
            "Install the versions listed in README.md under Prerequisites and Development tools."
        )
    else:
        print(
            "Build and lint prerequisites are ready. Workflow-specific test tools are listed separately in README.md."
        )
    return int(failed)


if __name__ == "__main__":
    raise SystemExit(main())
