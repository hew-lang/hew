#!/usr/bin/env python3
"""Gate direct-call ownership diagnostics against an exact per-file baseline."""

from __future__ import annotations

import os
from pathlib import Path
import subprocess
import sys


ROOT = Path(__file__).resolve().parents[2]
CORPUS = ROOT / "tests" / "obligation-advisory"
BASELINE = ROOT / "tests" / "obligation-advisory" / "baseline.tsv"
HEW = Path(os.environ.get("HEW_BIN", ROOT / "target" / "debug" / "hew"))
RELEASE_HEW = Path(
    os.environ.get("HEW_RELEASE_BIN", ROOT / "target" / "release-lib" / "hew")
)


def read_baseline() -> dict[str, tuple[int, int, int]]:
    rows: dict[str, tuple[int, int, int]] = {}
    with BASELINE.open(encoding="utf-8") as stream:
        for line in stream:
            line = line.rstrip("\n")
            if not line or line.startswith("#"):
                continue
            name, advisories, blocking, exit_code = line.split("\t")
            rows[name] = (int(advisories), int(blocking), int(exit_code))
    return rows


def corpus_entries() -> set[str]:
    root_entries = {path.name for path in CORPUS.glob("*.hew")}
    module_entries = {
        str(path.relative_to(CORPUS)) for path in CORPUS.glob("*/main.hew")
    }
    return root_entries | module_entries


def run_fixture(
    compiler: Path, name: str, environment: dict[str, str]
) -> tuple[int, int, int]:
    result = subprocess.run(
        [str(compiler), "check", str(CORPUS / name)],
        cwd=ROOT,
        env=environment,
        capture_output=True,
        text=True,
        timeout=60,
        check=False,
    )
    under = result.stderr.count("MIR kind: ObligationUnderReleased")
    all_mir = result.stderr.count("MIR kind:")
    return under, all_mir - under, result.returncode


def main() -> int:
    compilers = (("debug", HEW), ("release", RELEASE_HEW))
    for profile, compiler in compilers:
        if not compiler.is_file():
            print(
                f"error: {profile} compiler binary not found: {compiler}",
                file=sys.stderr,
            )
            return 1
    expected = read_baseline()
    entries = corpus_entries()
    if not expected or set(expected) != entries:
        print(
            "error: baseline and corpus entry inventory differ: "
            f"baseline={sorted(expected)} corpus={sorted(entries)}",
            file=sys.stderr,
        )
        return 1

    inherited = os.environ.copy()
    scrubbed = {
        key: value for key, value in inherited.items() if not key.startswith("HEW_")
    }
    failures: list[str] = []
    totals = [0, 0]
    environments = (("inherited", inherited), ("no-HEW-env", scrubbed))
    for build_profile, compiler in compilers:
        for environment_profile, environment in environments:
            for name in sorted(expected):
                observed = run_fixture(compiler, name, environment)
                totals[0] += observed[0]
                totals[1] += observed[1]
                if observed != expected[name]:
                    failures.append(
                        f"{build_profile}/{environment_profile} {name}: "
                        f"expected {expected[name]}, observed {observed}"
                    )

    if failures:
        print("ownership-advisory baseline drift:", file=sys.stderr)
        for failure in failures:
            print(f"  {failure}", file=sys.stderr)
        return 1
    print(
        "ownership-advisory: "
        f"fixtures={len(expected)} profiles=4 advisories={totals[0]} "
        f"blocking_mir={totals[1]}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
