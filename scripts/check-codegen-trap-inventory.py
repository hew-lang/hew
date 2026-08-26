#!/usr/bin/env python3
"""Fail closed when codegen grows an unclassified terminating edge.

Actor-reachable failures must carry an explicit disposition. Raw ``llvm.trap``
intrinsics belong only in the two canonical cooperative-stamp helpers or beside
a local defense-only proof. Direct generated calls to ``hew_panic`` must carry
an actor-cooperative proof; unlike a Rust C-unwind callback, the generated call
crosses the plain-C runtime ABI and is process-fatal if no actor recovery frame
is active.
"""

from __future__ import annotations

from collections import Counter
from dataclasses import dataclass
import pathlib
import re
import sys


ROOT = pathlib.Path(__file__).resolve().parents[1]
CODEGEN = ROOT / "hew-codegen-rs" / "src"
RAW_TRAP = re.compile(
    r'(?:intrinsics\s*::\s*)?Intrinsic\s*::\s*find\s*\(\s*"llvm\.trap"\s*\)',
    re.MULTILINE,
)
HEW_PANIC_DECL = re.compile(
    r'\.\s*(?:get_function|add_function)\s*\(\s*"hew_panic"', re.MULTILINE
)
HEW_PANIC_CALL = re.compile(r"\.\s*build_call\s*\(\s*panic_fn\s*,", re.MULTILINE)
FUNCTION = re.compile(
    r"^\s*(?:pub(?:\([^)]*\))?\s+)?(?:unsafe\s+)?fn\s+([A-Za-z0-9_]+)"
)
CANONICAL = {"emit_trap_with_code", "emit_trap_with_code_raw"}
DEFENSE_MARKER = "TRAP-DISPOSITION: defense-only("
HEW_PANIC_MARKER = "TRAP-DISPOSITION: actor-cooperative(hew_panic)"
MARKER_LOOKBEHIND_LINES = 16

# There are deliberately no expected counts here.
#
# The RULE is the contract: a raw trap lives inside a canonical cooperative
# helper or carries a defense-only proof beside it, and a generated hew_panic
# edge carries an actor-cooperative proof. That rule rejects a real wrong
# program -- an unclassified terminating edge reachable from an actor -- and it
# survives refactoring.
#
# The four constants that used to sit here (canonical site counts, defense-only
# site count, hew_panic declaration and call counts) rejected no wrong program
# the rule does not already reject. They fired on a legal refactor that split a
# helper in two or inlined a call, and they stayed silent on a NET-ZERO
# relocation, which is the case that actually matters. A count is a
# change-detector wearing a contract's clothes; the enumeration below is
# reported, and floored at non-empty, and never asserted equal.


@dataclass(frozen=True)
class Site:
    path: pathlib.Path
    line: int
    function: str | None
    kind: str
    disposed: bool


def enclosing_function(lines: list[str], index: int) -> str | None:
    for line in reversed(lines[: index + 1]):
        match = FUNCTION.match(line)
        if match:
            return match.group(1)
    return None


def marker_nearby(lines: list[str], index: int, marker: str) -> bool:
    nearby = "\n".join(lines[max(0, index - MARKER_LOOKBEHIND_LINES) : index + 1])
    return marker in nearby


def match_line(source: str, offset: int) -> int:
    return source.count("\n", 0, offset) + 1


def discover_sites(path: pathlib.Path, source: str) -> tuple[list[Site], list[Site]]:
    lines = source.splitlines()
    raw_sites: list[Site] = []
    panic_sites: list[Site] = []

    for match in RAW_TRAP.finditer(source):
        line = match_line(source, match.start())
        index = line - 1
        raw_sites.append(
            Site(
                path,
                line,
                enclosing_function(lines, index),
                "raw llvm.trap",
                marker_nearby(lines, index, DEFENSE_MARKER),
            )
        )

    for pattern, kind in (
        (HEW_PANIC_DECL, "hew_panic declaration"),
        (HEW_PANIC_CALL, "hew_panic call"),
    ):
        for match in pattern.finditer(source):
            line = match_line(source, match.start())
            index = line - 1
            panic_sites.append(
                Site(
                    path,
                    line,
                    enclosing_function(lines, index),
                    kind,
                    marker_nearby(lines, index, HEW_PANIC_MARKER),
                )
            )

    return raw_sites, panic_sites


def classify_sites(raw_sites: list[Site], panic_sites: list[Site]) -> list[str]:
    errors: list[str] = []
    for site in raw_sites:
        if site.function in CANONICAL or site.disposed:
            continue
        errors.append(
            f"{site.path}:{site.line}: raw llvm.trap in "
            f"{site.function or '<unknown>'} has no canonical cooperative stamp "
            "or defense-only proof"
        )

    for site in panic_sites:
        if site.disposed:
            continue
        errors.append(
            f"{site.path}:{site.line}: {site.kind} in "
            f"{site.function or '<unknown>'} has no actor-cooperative disposition proof"
        )
    return errors


def run_counterfactual_self_tests() -> list[str]:
    failures: list[str] = []
    counterfactual = pathlib.Path("<counterfactual>")

    multiline_raw = """
fn stray_raw_trap() {
    let trap = Intrinsic::find(
        "llvm.trap"
    );
}
"""
    raw_sites, panic_sites = discover_sites(counterfactual, multiline_raw)
    errors = classify_sites(raw_sites, panic_sites)
    if len(raw_sites) != 1 or not any("raw llvm.trap" in error for error in errors):
        failures.append("multiline unclassified llvm.trap escaped the scanner")

    unclassified_panic = """
fn stray_panic(module: &Module, builder: &Builder) {
    let panic_fn = module.get_function(
        "hew_panic"
    );
    builder.build_call(
        panic_fn,
        &[],
        "panic",
    );
}
"""
    raw_sites, panic_sites = discover_sites(counterfactual, unclassified_panic)
    errors = classify_sites(raw_sites, panic_sites)
    kinds = Counter(site.kind for site in panic_sites)
    if kinds != Counter({"hew_panic declaration": 1, "hew_panic call": 1}):
        failures.append("multiline hew_panic declaration/call escaped the scanner")
    if len(errors) != 2 or not all("actor-cooperative" in error for error in errors):
        failures.append("unclassified hew_panic edge escaped disposition enforcement")

    return failures


def main() -> int:
    errors = run_counterfactual_self_tests()
    raw_sites: list[Site] = []
    panic_sites: list[Site] = []
    for path in sorted(CODEGEN.rglob("*.rs")):
        source = path.read_text(encoding="utf-8")
        discovered_raw, discovered_panic = discover_sites(path, source)
        raw_sites.extend(discovered_raw)
        panic_sites.extend(discovered_panic)

    errors.extend(classify_sites(raw_sites, panic_sites))

    canonical_counts = Counter(
        site.function for site in raw_sites if site.function in CANONICAL
    )
    defense_sites = sum(
        site.function not in CANONICAL and site.disposed for site in raw_sites
    )
    panic_counts = Counter(site.kind for site in panic_sites)

    # Anti-vacuity FLOOR, not a target (LESSONS.md enumeration-gate-floors).
    # The scanner walking the whole of hew-codegen-rs/src and finding nothing
    # means the regexes stopped matching, not that codegen stopped trapping --
    # and a gate that enforces nothing while reporting green is the exact
    # failure this file exists to prevent one layer down.
    if not raw_sites:
        errors.append(
            "no raw llvm.trap site found anywhere in hew-codegen-rs/src; the "
            "scanner is broken, because codegen does emit traps"
        )
    if not any(site.function in CANONICAL for site in raw_sites):
        errors.append(
            "no raw trap sits inside a canonical cooperative helper "
            f"({sorted(CANONICAL)}); the canonical rule has nothing to enforce"
        )
    if not panic_sites:
        errors.append(
            "no generated hew_panic site found; the hew_panic scanner is broken"
        )

    if errors:
        print("codegen trap inventory failed:", file=sys.stderr)
        for error in errors:
            print(f"  - {error}", file=sys.stderr)
        return 1

    print(
        "codegen trap inventory passed: "
        f"{sum(canonical_counts.values())} canonical raw-trap site(s), "
        f"{defense_sites} defense-only raw-trap site(s), "
        f"{panic_counts['hew_panic declaration']} hew_panic declaration site(s), "
        f"{panic_counts['hew_panic call']} hew_panic call site(s); "
        "counterfactual self-tests passed"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
