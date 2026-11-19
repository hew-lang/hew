#!/usr/bin/env python3
"""Read the repository LLVM pin, or verify that everything agrees with it.

llvm-version.env is the one place the LLVM version is written down. Two other
places necessarily restate it in a form their tools require:

- hew-codegen-rs/Cargo.toml, as the llvm-sys version (series without the dot)
  and the inkwell `llvm<major>-<minor>` feature;
- the `LLVM_SYS_<ver>_PREFIX` variable name llvm-sys reads, wherever a script
  or build script sets or reads it.

`--check` fails when either drifts from llvm-version.env, so a bump that misses
one fails `make llvm-version-check` instead of a CI build an hour later.

Usage:
  llvm_version.py --check
  llvm_version.py KEY            print one value, e.g. LLVM_MAJOR
  llvm_version.py LLVM_SYS_PREFIX_VAR
"""

from __future__ import annotations

import re
import subprocess
import sys
import tomllib
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PIN = ROOT / "llvm-version.env"
CODEGEN = ROOT / "hew-codegen-rs" / "Cargo.toml"
# Historical records and the changelog describe the toolchain of their time.
HISTORICAL = ("CHANGELOG.md", "docs/releases/")


def load() -> dict[str, str]:
    values: dict[str, str] = {}
    for number, line in enumerate(PIN.read_text().splitlines(), 1):
        if not line.strip() or line.lstrip().startswith("#"):
            continue
        match = re.fullmatch(r"([A-Z][A-Z0-9_]*)=(\S*)", line)
        if not match:
            raise SystemExit(f"{PIN.name}:{number}: expected KEY=value, got {line!r}")
        values[match.group(1)] = match.group(2)
    for key in ("LLVM_MAJOR", "LLVM_SERIES", "LLVM_VERSION"):
        if key not in values:
            raise SystemExit(f"{PIN.name}: {key} is missing")
    values["LLVM_SYS_PREFIX_VAR"] = prefix_var(values)
    return values


def prefix_var(values: dict[str, str]) -> str:
    return f"LLVM_SYS_{values['LLVM_SERIES'].replace('.', '')}_PREFIX"


def check(values: dict[str, str]) -> list[str]:
    errors: list[str] = []
    major, series, version = (
        values["LLVM_MAJOR"],
        values["LLVM_SERIES"],
        values["LLVM_VERSION"],
    )
    if not re.fullmatch(r"\d+", major):
        errors.append(f"LLVM_MAJOR={major} is not a number")
    if not re.fullmatch(rf"{re.escape(major)}\.\d+", series):
        errors.append(f"LLVM_SERIES={series} is not a {major}.<minor> series")
    if not re.fullmatch(rf"{re.escape(series)}\.\d+", version):
        errors.append(f"LLVM_VERSION={version} is not a {series}.<patch> release")

    manifest = tomllib.loads(CODEGEN.read_text())["dependencies"]
    llvm_sys = series.replace(".", "")
    found = str(manifest["llvm-sys"]).split(".")[0]
    if found != llvm_sys:
        errors.append(
            f"hew-codegen-rs/Cargo.toml: llvm-sys = {manifest['llvm-sys']!r}, "
            f"expected {llvm_sys!r} for LLVM {series}"
        )
    feature = f"llvm{series.replace('.', '-')}"
    features = manifest["inkwell"].get("features", [])
    if feature not in features:
        errors.append(
            f"hew-codegen-rs/Cargo.toml: inkwell features {features} lack {feature!r}"
        )

    expected = prefix_var(values)
    tracked = (
        subprocess.run(
            ["git", "ls-files", "-z"], cwd=ROOT, capture_output=True, check=True
        )
        .stdout.decode()
        .split("\0")
    )
    pattern = re.compile(r"LLVM_SYS_(\d+)_PREFIX")
    for name in tracked:
        if not name or name.startswith(HISTORICAL) or name.startswith("vendor/"):
            continue
        path = ROOT / name
        try:
            text = path.read_text()
        except (UnicodeDecodeError, FileNotFoundError, IsADirectoryError):
            continue
        for number, line in enumerate(text.splitlines(), 1):
            for match in pattern.finditer(line):
                if match.group(0) != expected:
                    errors.append(
                        f"{name}:{number}: {match.group(0)} should be {expected}"
                    )
    return errors


def main() -> int:
    args = sys.argv[1:]
    values = load()
    if args == ["--check"]:
        errors = check(values)
        for error in errors:
            print(f"llvm-version: {error}", file=sys.stderr)
        if not errors:
            print(f"llvm-version: LLVM {values['LLVM_VERSION']} pin is consistent")
        return 1 if errors else 0
    if len(args) == 1 and args[0] in values:
        print(values[args[0]])
        return 0
    print(__doc__.strip().split("Usage:")[1], file=sys.stderr)
    return 2


if __name__ == "__main__":
    sys.exit(main())
