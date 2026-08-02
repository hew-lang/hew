#!/usr/bin/env python3
"""Fail closed when codegen grows an unclassified raw llvm.trap emitter.

Actor-reachable failures must go through emit_trap_with_code[_raw], which
stamps the canonical runtime discriminator before retaining llvm.trap only as
the non-actor fallback. A direct intrinsic is permitted only inside those two
single-sourced helpers or beside a local TRAP-DISPOSITION defense-only proof.
"""

from __future__ import annotations

import pathlib
import re
import sys


ROOT = pathlib.Path(__file__).resolve().parents[1]
CODEGEN = ROOT / "hew-codegen-rs" / "src"
RAW_TRAP = re.compile(r'(?:intrinsics::)?Intrinsic::find\("llvm\.trap"\)')
FUNCTION = re.compile(
    r"^\s*(?:pub(?:\([^)]*\))?\s+)?(?:unsafe\s+)?fn\s+([A-Za-z0-9_]+)"
)
CANONICAL = {"emit_trap_with_code", "emit_trap_with_code_raw"}


def enclosing_function(lines: list[str], index: int) -> str | None:
    for line in reversed(lines[: index + 1]):
        match = FUNCTION.match(line)
        if match:
            return match.group(1)
    return None


def main() -> int:
    raw_sites: list[tuple[pathlib.Path, int, str | None, bool]] = []
    for path in sorted(CODEGEN.rglob("*.rs")):
        lines = path.read_text(encoding="utf-8").splitlines()
        for index, line in enumerate(lines):
            if not RAW_TRAP.search(line):
                continue
            function = enclosing_function(lines, index)
            nearby = "\n".join(lines[max(0, index - 8) : index + 1])
            defense_only = "TRAP-DISPOSITION: defense-only(" in nearby
            raw_sites.append((path, index + 1, function, defense_only))

    errors: list[str] = []
    canonical_seen: set[str] = set()
    defense_sites = 0
    for path, line, function, defense_only in raw_sites:
        relative = path.relative_to(ROOT)
        if function in CANONICAL:
            canonical_seen.add(function)
        elif defense_only:
            defense_sites += 1
        else:
            errors.append(
                f"{relative}:{line}: raw llvm.trap in {function or '<unknown>'} "
                "has no canonical cooperative stamp or defense-only proof"
            )

    missing = CANONICAL - canonical_seen
    if missing:
        errors.append(
            "canonical trap emitter corpus disappeared: " + ", ".join(sorted(missing))
        )
    if defense_sites == 0:
        errors.append(
            "defense-only trap corpus disappeared; audit/gate may no longer be live"
        )

    if errors:
        print("codegen trap inventory failed:", file=sys.stderr)
        for error in errors:
            print(f"  - {error}", file=sys.stderr)
        return 1

    print(
        f"codegen trap inventory passed: {len(canonical_seen)} canonical emitters, "
        f"{defense_sites} structurally proved defense-only site(s)"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
