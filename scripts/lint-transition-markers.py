#!/usr/bin/env python3
"""Marker hygiene: TRANSITION/SHIM/WHY:/TODO markers must name a live lane.

Registry: docs/internal/marker-lanes.tsv (id, owner, state live|done|v07, ref).
Baseline: docs/internal/marker-baseline.tsv, a closed list of existing
violations keyed by file, rule and marker-text hash (not line number). It only
shrinks: a violation outside it fails, and a row whose violation is gone fails.
"""

from __future__ import annotations

import hashlib
import re
import subprocess
import sys
import tempfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
REGISTRY = "docs/internal/marker-lanes.tsv"
BASELINE = "docs/internal/marker-baseline.tsv"
EXTENSIONS = (".rs", ".hew", ".ts", ".sh", ".toml")
WINDOW = 6

TRANSITION = re.compile(r"\bTRANSITION\(([^)]*)\)")
SHIM = re.compile(r"\bSHIM\b(?:\(([^)]*)\))?")
WHY = re.compile(r"\bWHY:")
TODO = re.compile(r"(?<![A-Za-z-])(?:NATIVE-)?TODO\b(?:\(([^)]*)\))?")
WHEN = re.compile(r"\bWHEN\b|deleted (?:by|with)|obsolete when|dies with")
REAL_FIX = re.compile(r"\bWHAT\b|real fix|REAL solution", re.IGNORECASE)
ISSUE = re.compile(r"#\d+")
COMMIT_FORM = re.compile(r"^([A-Za-z][\w-]*?)(?:\s+commit\s+(\d+))?$")


def parse_ids(text: str) -> list[str]:
    """`A1 commit 3, B1` names lanes A1c3 and B1."""
    ids = []
    for part in text.split(","):
        part = part.strip()
        match = COMMIT_FORM.match(part)
        ids.append(f"{match[1]}c{match[2]}" if match and match[2] else part)
    return ids


def load_registry(text: str) -> dict[str, tuple[str, str]]:
    registry = {}
    for line in text.splitlines()[1:]:
        if line.strip():
            fields = line.split("\t")
            registry[fields[0]] = (fields[2], fields[3] if len(fields) > 3 else "")
    return registry


def id_problem(
    raw: str, registry: dict[str, tuple[str, str]], allow_issue: bool
) -> str | None:
    for lane in parse_ids(raw):
        if allow_issue and ISSUE.fullmatch(lane):
            continue
        if lane not in registry:
            return "unknown-id"
        state, ref = registry[lane]
        if state == "done":
            return "done-id"
        if state == "v07" and not ISSUE.search(ref):
            return "v07-no-issue"
    return None


def scan_text(path: str, text: str, registry: dict[str, tuple[str, str]]):
    """Yield (line_number, rule, marker_line) for each violation."""
    lines = text.splitlines()

    def near(index: int, pattern: re.Pattern[str]) -> bool:
        lo, hi = max(0, index - WINDOW), index + WINDOW + 1
        return any(pattern.search(l) for l in lines[lo:hi])

    for index, line in enumerate(lines):
        for match in TRANSITION.finditer(line):
            problem = id_problem(match[1], registry, False)
            if problem:
                yield index + 1, problem, line
            if not near(index, WHEN):
                yield index + 1, "no-when", line
        for match in SHIM.finditer(line):
            if not match[1]:
                yield index + 1, "no-id", line
            else:
                problem = id_problem(match[1], registry, False)
                if problem:
                    yield index + 1, problem, line
            if not near(index, WHEN):
                yield index + 1, "no-when", line
            if not near(index, REAL_FIX):
                yield index + 1, "no-real-fix", line
        if WHY.search(line) and not near(index, REAL_FIX):
            yield index + 1, "no-real-fix", line
        for match in TODO.finditer(line):
            problem = id_problem(match[1], registry, True) if match[1] else "no-id"
            if problem:
                yield index + 1, "todo-" + problem, line


def violation_rows(
    files: dict[str, str], registry
) -> list[tuple[str, str, str, str, str]]:
    """Rows (file, rule, hash, nth, excerpt); `nth` separates identical markers."""
    rows = []
    seen: dict[tuple[str, str, str], int] = {}
    for path in sorted(files):
        for _, rule, line in scan_text(path, files[path], registry):
            digest = hashlib.sha1(line.strip().encode()).hexdigest()[:10]
            nth = seen.get((path, rule, digest), 0)
            seen[(path, rule, digest)] = nth + 1
            excerpt = " ".join(line.split())[:80]
            rows.append((path, rule, digest, str(nth), excerpt))
    return rows


def check(files: dict[str, str], registry_text: str, baseline_text: str) -> list[str]:
    registry = load_registry(registry_text)
    current = {r[:4]: r for r in violation_rows(files, registry)}
    baseline = {}
    for line in baseline_text.splitlines()[1:]:
        if line.strip():
            fields = tuple(line.split("\t"))
            baseline[fields[:4]] = fields
    errors = []
    for key in sorted(current.keys() - baseline.keys()):
        errors.append(f"{key[0]}: {key[1]}: {current[key][4]}")
    for key in sorted(baseline.keys() - current.keys()):
        errors.append(
            f"{BASELINE}: fixed violation still baselined, delete the row: {' '.join(key)}"
        )
    return errors


def tracked_files() -> dict[str, str]:
    out = subprocess.run(
        ["git", "ls-files", "-z"],
        cwd=REPO_ROOT,
        check=True,
        capture_output=True,
        text=True,
    ).stdout
    files = {}
    for name in out.split("\0"):
        if name.endswith(EXTENSIONS) and (REPO_ROOT / name).is_file():
            files[name] = (REPO_ROOT / name).read_text(errors="replace")
    return files


def write_baseline(files, registry_text) -> None:
    rows = violation_rows(files, load_registry(registry_text))
    with open(REPO_ROOT / BASELINE, "w") as out:
        out.write("file\trule\thash\tnth\texcerpt\n")
        for row in rows:
            out.write("\t".join(row) + "\n")


def self_test() -> int:
    registry = (
        "id\towner\tstate\tref\nL1\tX\tlive\t-\nOLD\tX\tdone\t-\nLATE\tX\tv07\t-\n"
    )
    header = "file\trule\thash\tnth\texcerpt\n"
    good = "// TRANSITION(L1): old path\n// deleted by L1\n"
    cases = [
        ("good marker passes", {"a.rs": good}, header, []),
        (
            "missing WHEN fails",
            {"a.rs": "// TRANSITION(L1): old path\n"},
            header,
            ["no-when"],
        ),
        ("done id fails", {"a.rs": good.replace("L1", "OLD")}, header, ["done-id"]),
        (
            "unknown id fails",
            {"a.rs": good.replace("L1", "NOPE")},
            header,
            ["unknown-id"],
        ),
        (
            "v07 without issue fails",
            {"a.rs": good.replace("L1", "LATE")},
            header,
            ["v07-no-issue"],
        ),
        (
            "SHIM needs real fix",
            {"a.rs": "// SHIM(L1) WHEN L1 lands\n"},
            header,
            ["no-real-fix"],
        ),
        ("bare TODO fails", {"a.rs": "// TODO: later\n"}, header, ["todo-no-id"]),
        ("TODO with issue passes", {"a.rs": "// TODO(#12): later\n"}, header, []),
    ]
    failed = 0
    for name, files, baseline, expected in cases:
        rules = [e.split(": ")[1] for e in check(files, registry, baseline)]
        if rules != expected:
            print(f"self-test FAIL: {name}: expected {expected}, got {rules}")
            failed += 1
    files = {"a.rs": "// TRANSITION(L1): old path\n"}
    row = next(iter(violation_rows(files, load_registry(registry))))
    baseline = header + "\t".join(row) + "\n"
    if check(files, registry, baseline):
        print("self-test FAIL: baselined violation must pass")
        failed += 1
    if not any(
        "delete the row" in e for e in check({"a.rs": good}, registry, baseline)
    ):
        print("self-test FAIL: baseline row for a fixed violation must fail")
        failed += 1
    return 1 if failed else 0


def main() -> int:
    if "--self-test" in sys.argv:
        return self_test()
    registry_text = (REPO_ROOT / REGISTRY).read_text()
    files = tracked_files()
    if "--write-baseline" in sys.argv:
        write_baseline(files, registry_text)
        return 0
    errors = check(files, registry_text, (REPO_ROOT / BASELINE).read_text())
    for error in errors:
        print(error, file=sys.stderr)
    return 1 if errors else 0


if __name__ == "__main__":
    sys.exit(main())
