#!/usr/bin/env python3
"""Validate actionable WASM-TODO markers against the repository backlog."""

from __future__ import annotations

import argparse
import re
import subprocess
import sys
import tempfile
import tomllib
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable


TOKEN = "WASM-TODO"
ID_PATTERN = r"[a-z0-9]+(?:-[a-z0-9]+)*"
ID_RE = re.compile(rf"^{ID_PATTERN}$")
MARKER_RE = re.compile(rf"WASM-TODO\((?P<id>{ID_PATTERN})\):")
REQUIRED_BACKLOG_FIELDS = ("id", "gap", "blocker", "tracking_label")

# These files describe the convention or render the authority; occurrences in
# them are labels/examples rather than actionable source markers.
EXCLUDED_PATHS = {
    "CONTRIBUTING.md",
    "Makefile",
    "hew-capability-gen/README.md",
    "wasm-capability-manifest.toml",
    "scripts/lint-wasm-todo-issue-ref.sh",
    "scripts/lint-wasm-todo.py",
}
EXCLUDED_PREFIXES = (
    ".github/",
    ".tmp/",
    "docs/",
    "hew-capability-gen/src/",
    "hew-capability-gen/tests/",
)


class LintError(Exception):
    """A fail-closed manifest or marker validation error."""


@dataclass(frozen=True)
class Marker:
    path: str
    line: int
    backlog_id: str


def _required_string(row: object, field: str, index: int) -> str:
    if not isinstance(row, dict):
        raise LintError(f"backlog row {index} must be a TOML table")
    value = row.get(field)
    if not isinstance(value, str) or not value.strip():
        raise LintError(f"backlog row {index} has empty or missing `{field}`")
    return value


def parse_authority(path: Path) -> frozenset[str]:
    try:
        data = tomllib.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, tomllib.TOMLDecodeError) as err:
        raise LintError(f"cannot parse {path}: {err}") from err

    if data.get("manifest_version") != 1:
        raise LintError("wasm capability manifest must declare `manifest_version = 1`")

    backlog = data.get("backlog")
    if not isinstance(backlog, list) or not backlog:
        raise LintError(
            "wasm capability manifest must contain at least one [[backlog]] row"
        )

    seen: set[str] = set()
    for index, row in enumerate(backlog, start=1):
        values = {
            field: _required_string(row, field, index)
            for field in REQUIRED_BACKLOG_FIELDS
        }
        backlog_id = values["id"]
        if ID_RE.fullmatch(backlog_id) is None:
            raise LintError(
                f"backlog row {index} id `{backlog_id}` is not stable kebab-case"
            )
        if backlog_id in seen:
            raise LintError(f"duplicate backlog id `{backlog_id}`")
        expected_label = f"WASM-TODO({backlog_id}):"
        if values["tracking_label"] != expected_label:
            raise LintError(
                f"backlog `{backlog_id}` tracking_label must be `{expected_label}`"
            )
        seen.add(backlog_id)

    return frozenset(seen)


def tracked_files(repo: Path) -> list[str]:
    result = subprocess.run(
        ["git", "-C", str(repo), "ls-files", "-z"],
        check=False,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    if result.returncode != 0:
        detail = result.stderr.decode("utf-8", errors="replace").strip()
        raise LintError(f"cannot enumerate tracked files: {detail}")
    return [raw.decode("utf-8") for raw in result.stdout.split(b"\0") if raw]


def is_actionable_path(path: str) -> bool:
    return path not in EXCLUDED_PATHS and not path.startswith(EXCLUDED_PREFIXES)


def scan_markers(
    repo: Path, authority: frozenset[str], paths: Iterable[str]
) -> list[Marker]:
    markers: list[Marker] = []
    violations: list[str] = []

    for relative in paths:
        if not is_actionable_path(relative):
            continue
        path = repo / relative
        try:
            raw = path.read_bytes()
        except OSError as err:
            violations.append(f"{relative}: cannot read tracked file: {err}")
            continue
        if TOKEN.encode() not in raw:
            continue
        try:
            text = raw.decode("utf-8")
        except UnicodeDecodeError as err:
            violations.append(f"{relative}: contains WASM-TODO but is not UTF-8: {err}")
            continue

        for line_number, line in enumerate(text.splitlines(), start=1):
            offset = 0
            while True:
                token_offset = line.find(TOKEN, offset)
                if token_offset < 0:
                    break
                marker_match = MARKER_RE.match(line, token_offset)
                if marker_match is None:
                    violations.append(
                        f"{relative}:{line_number}: malformed marker; expected "
                        "`WASM-TODO(<stable-backlog-id>):`"
                    )
                    offset = token_offset + len(TOKEN)
                    continue
                backlog_id = marker_match.group("id")
                if backlog_id not in authority:
                    violations.append(
                        f"{relative}:{line_number}: unknown WASM backlog id "
                        f"`{backlog_id}`"
                    )
                markers.append(Marker(relative, line_number, backlog_id))
                offset = marker_match.end()

    if not markers:
        violations.append(
            "actionable WASM-TODO corpus is empty; refusing a vacuous green lint"
        )
    if violations:
        raise LintError("\n".join(violations))
    return markers


def lint(
    repo: Path, manifest: Path, paths: Iterable[str] | None = None
) -> list[Marker]:
    authority = parse_authority(manifest)
    return scan_markers(
        repo, authority, tracked_files(repo) if paths is None else paths
    )


def _manifest(*ids: str) -> str:
    rows = []
    for backlog_id in ids:
        rows.append(
            "\n".join(
                (
                    "[[backlog]]",
                    f'id = "{backlog_id}"',
                    f'gap = "gap for {backlog_id}"',
                    f'blocker = "blocker for {backlog_id}"',
                    f'tracking_label = "WASM-TODO({backlog_id}):"',
                )
            )
        )
    return "manifest_version = 1\n\n" + "\n\n".join(rows) + "\n"


def self_test() -> None:
    cases = (
        ("good", _manifest("channels"), "WASM-TODO(channels): live gap\n", True),
        (
            "malformed",
            _manifest("channels"),
            "WASM-TODO(channels) missing colon\n",
            False,
        ),
        (
            "malformed-manifest",
            "manifest_version = [\n",
            "WASM-TODO(channels): gap\n",
            False,
        ),
        (
            "legacy-issue",
            _manifest("channels"),
            "WASM-TODO(#1451): closed issue\n",
            False,
        ),
        (
            "unknown-id",
            _manifest("channels"),
            "WASM-TODO(not-authoritative): gap\n",
            False,
        ),
        (
            "duplicate-id",
            _manifest("channels", "channels"),
            "WASM-TODO(channels): gap\n",
            False,
        ),
        (
            "empty-authority",
            "manifest_version = 1\n",
            "WASM-TODO(channels): gap\n",
            False,
        ),
        ("empty-corpus", _manifest("channels"), "no markers here\n", False),
    )

    failures: list[str] = []
    with tempfile.TemporaryDirectory(prefix="hew-wasm-todo-selftest-") as temp:
        root = Path(temp)
        manifest_path = root / "wasm-capability-manifest.toml"
        source_path = root / "source.rs"
        for name, manifest_src, source_src, expected_ok in cases:
            manifest_path.write_text(manifest_src, encoding="utf-8")
            source_path.write_text(source_src, encoding="utf-8")
            try:
                lint(root, manifest_path, ["source.rs"])
                actual_ok = True
            except LintError:
                actual_ok = False
            if actual_ok != expected_ok:
                failures.append(
                    f"{name}: expected {'pass' if expected_ok else 'failure'}, "
                    f"got {'pass' if actual_ok else 'failure'}"
                )
            else:
                print(f"lint-wasm-todo self-test: {name}: ok", file=sys.stderr)
    if failures:
        raise LintError("\n".join(failures))


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--self-test",
        action="store_true",
        help="run synthetic manifest and marker contract tests",
    )
    args = parser.parse_args()

    try:
        if args.self_test:
            self_test()
            return 0
        repo = Path(
            subprocess.run(
                ["git", "rev-parse", "--show-toplevel"],
                check=True,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                text=True,
            ).stdout.strip()
        )
        markers = lint(repo, repo / "wasm-capability-manifest.toml")
    except (LintError, subprocess.CalledProcessError) as err:
        print(f"lint-wasm-todo: {err}", file=sys.stderr)
        return 1

    used = len({marker.backlog_id for marker in markers})
    print(
        f"lint-wasm-todo: ok ({len(markers)} markers, {used} backlog ids)",
        file=sys.stderr,
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
