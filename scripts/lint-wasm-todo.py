#!/usr/bin/env python3
"""Validate actionable WASM-TODO markers against the repository backlog."""

from __future__ import annotations

import argparse
import re
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable

sys.path.insert(0, str(Path(__file__).resolve().parent / "lib"))
import toml_compat


TOKEN = "WASM-TODO"
ID_PATTERN = r"[a-z0-9]+(?:-[a-z0-9]+)*"
ID_RE = re.compile(rf"^{ID_PATTERN}$")
MARKER_RE = re.compile(rf"WASM-TODO\((?P<id>{ID_PATTERN})\):")
REQUIRED_BACKLOG_FIELDS = ("id", "gap", "blocker", "tracking_label")
SOURCE_MARKER_DISPOSITION = "source"
NON_SOURCE_MARKER_DISPOSITION = "non-source"

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
)

# Exact descriptive occurrences inside otherwise actionable source trees.
#
# The whole files must remain scannable: adding an actionable marker beside
# this prose must still require an authority row. Matching the complete
# stripped line makes this allowlist narrow and reviewable without coupling it
# to line numbers that move during ordinary edits.
DESCRIPTIVE_LINES = {
    "hew-capability-gen/src/lib.rs": frozenset(
        {
            "/// WASM-TODO backlog rows — one per row of the backlog table.",
            "/// A WASM-TODO backlog row.",
            'CheckerDisposition::Todo => "WASM-TODO (not checker-gated)".to_string(),',
        }
    ),
    "hew-capability-gen/tests/authority.rs": frozenset(
        {
            '"WASM-TODO(changed-supervision):",',
        }
    ),
    "hew-capability-gen/tests/row_count.rs": frozenset(
        {
            '"## WASM-TODO backlog",',
            '"TOML backlog count ({}) does not match prose WASM-TODO backlog rows ({}).",',
        }
    ),
}


class LintError(Exception):
    """A fail-closed manifest or marker validation error."""


@dataclass(frozen=True)
class Marker:
    path: str
    line: int
    backlog_id: str


@dataclass(frozen=True)
class BacklogAuthority:
    """Parsed backlog identities and their required source-marker coverage."""

    ids: frozenset[str]
    source_marker_ids: frozenset[str]
    non_source_ids: frozenset[str]


def _required_string(row: object, field: str, index: int) -> str:
    if not isinstance(row, dict):
        raise LintError(f"backlog row {index} must be a TOML table")
    value = row.get(field)
    if not isinstance(value, str) or not value.strip():
        raise LintError(f"backlog row {index} has empty or missing `{field}`")
    return value


def parse_authority(path: Path) -> BacklogAuthority:
    try:
        data = toml_compat.loads(path.read_text(encoding="utf-8"))
    except (OSError, UnicodeError, toml_compat.TOMLDecodeError) as err:
        raise LintError(f"cannot parse {path}: {err}") from err

    manifest_version = data.get("manifest_version")
    if type(manifest_version) is not int or manifest_version != 1:
        raise LintError(
            "wasm capability manifest `manifest_version` must be the integer 1"
        )

    backlog = data.get("backlog")
    if not isinstance(backlog, list) or not backlog:
        raise LintError(
            "wasm capability manifest must contain at least one [[backlog]] row"
        )

    seen: set[str] = set()
    source_marker_ids: set[str] = set()
    non_source_ids: set[str] = set()
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
        marker_disposition = row.get("marker_disposition", SOURCE_MARKER_DISPOSITION)
        if not isinstance(marker_disposition, str) or marker_disposition not in {
            SOURCE_MARKER_DISPOSITION,
            NON_SOURCE_MARKER_DISPOSITION,
        }:
            raise LintError(
                f"backlog `{backlog_id}` marker_disposition must be "
                f"`{SOURCE_MARKER_DISPOSITION}` or "
                f"`{NON_SOURCE_MARKER_DISPOSITION}`"
            )
        if marker_disposition == NON_SOURCE_MARKER_DISPOSITION:
            _required_string(row, "non_source_reason", index)
            non_source_ids.add(backlog_id)
        else:
            if "non_source_reason" in row:
                raise LintError(
                    f"backlog `{backlog_id}` non_source_reason requires "
                    f'marker_disposition = "{NON_SOURCE_MARKER_DISPOSITION}"'
                )
            source_marker_ids.add(backlog_id)
        seen.add(backlog_id)

    return BacklogAuthority(
        ids=frozenset(seen),
        source_marker_ids=frozenset(source_marker_ids),
        non_source_ids=frozenset(non_source_ids),
    )


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
    repo: Path, authority: BacklogAuthority, paths: Iterable[str]
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
            if line.strip() in DESCRIPTIVE_LINES.get(relative, frozenset()):
                continue
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
                if backlog_id not in authority.ids:
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


def validate_backlog_coverage(
    authority: BacklogAuthority, markers: Iterable[Marker]
) -> None:
    """Enforce manifest identity and actionable source-marker coverage both ways."""
    used = {marker.backlog_id for marker in markers}
    violations: list[str] = []
    unused = sorted(authority.source_marker_ids - used)
    if unused:
        violations.append(
            "backlog id(s) without an actionable WASM-TODO marker: "
            + ", ".join(f"`{backlog_id}`" for backlog_id in unused)
        )
    non_source_markers = sorted(authority.non_source_ids & used)
    if non_source_markers:
        violations.append(
            "backlog id(s) declared non-source have an actionable "
            "WASM-TODO marker: "
            + ", ".join(f"`{backlog_id}`" for backlog_id in non_source_markers)
        )
    if violations:
        raise LintError("\n".join(violations))


def lint(
    repo: Path, manifest: Path, paths: Iterable[str] | None = None
) -> list[Marker]:
    authority = parse_authority(manifest)
    markers = scan_markers(
        repo, authority, tracked_files(repo) if paths is None else paths
    )
    validate_backlog_coverage(authority, markers)
    return markers


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


@dataclass(frozen=True)
class SelfTestCase:
    name: str
    manifest_src: str
    source_src: str
    expected_error: str | None
    source_path: str = "source.rs"
    expected_markers: int = 1


def self_test() -> None:
    cases = (
        SelfTestCase(
            "good",
            _manifest("channels"),
            "WASM-TODO(channels): live gap\n",
            None,
        ),
        SelfTestCase(
            "unused-backlog-row",
            _manifest("channels", "semaphore"),
            "WASM-TODO(channels): live gap\n",
            "backlog id(s) without an actionable WASM-TODO marker: `semaphore`",
        ),
        SelfTestCase(
            "explicit-non-source-row",
            _manifest("channels", "semaphore").replace(
                'tracking_label = "WASM-TODO(semaphore):"',
                'tracking_label = "WASM-TODO(semaphore):"\n'
                'marker_disposition = "non-source"\n'
                'non_source_reason = "no source registration site"',
            ),
            "WASM-TODO(channels): live gap\n",
            None,
        ),
        SelfTestCase(
            "non-source-row-with-marker",
            _manifest("channels", "semaphore").replace(
                'tracking_label = "WASM-TODO(semaphore):"',
                'tracking_label = "WASM-TODO(semaphore):"\n'
                'marker_disposition = "non-source"\n'
                'non_source_reason = "no source registration site"',
            ),
            "WASM-TODO(channels): live gap\nWASM-TODO(semaphore): stale marker\n",
            "backlog id(s) declared non-source have an actionable WASM-TODO "
            "marker: `semaphore`",
        ),
        SelfTestCase(
            "non-source-without-reason",
            _manifest("channels").replace(
                'tracking_label = "WASM-TODO(channels):"',
                'tracking_label = "WASM-TODO(channels):"\n'
                'marker_disposition = "non-source"',
            ),
            "no markers here\n",
            "empty or missing `non_source_reason`",
            expected_markers=0,
        ),
        SelfTestCase(
            "manifest-version-bool",
            _manifest("channels").replace(
                "manifest_version = 1", "manifest_version = true"
            ),
            "WASM-TODO(channels): gap\n",
            "manifest_version` must be the integer 1",
        ),
        SelfTestCase(
            "manifest-version-float",
            _manifest("channels").replace(
                "manifest_version = 1", "manifest_version = 1.0"
            ),
            "WASM-TODO(channels): gap\n",
            "manifest_version` must be the integer 1",
        ),
        SelfTestCase(
            "manifest-version-wrong-integer",
            _manifest("channels").replace(
                "manifest_version = 1", "manifest_version = 2"
            ),
            "WASM-TODO(channels): gap\n",
            "manifest_version` must be the integer 1",
        ),
        SelfTestCase(
            "malformed-manifest",
            "manifest_version = [\n",
            "WASM-TODO(channels): gap\n",
            "cannot parse",
        ),
        SelfTestCase(
            "malformed-mixed-corpus",
            _manifest("channels"),
            "WASM-TODO(channels): live gap\nWASM-TODO(channels) missing colon\n",
            "malformed marker",
        ),
        SelfTestCase(
            "legacy-issue-mixed-corpus",
            _manifest("channels"),
            "WASM-TODO(channels): live gap\nWASM-TODO(#1451): closed issue\n",
            "malformed marker",
        ),
        SelfTestCase(
            "unknown-id",
            _manifest("channels"),
            "WASM-TODO(not-authoritative): gap\n",
            "unknown WASM backlog id `not-authoritative`",
        ),
        SelfTestCase(
            "duplicate-id",
            _manifest("channels", "channels"),
            "WASM-TODO(channels): gap\n",
            "duplicate backlog id `channels`",
        ),
        SelfTestCase(
            "empty-authority",
            "manifest_version = 1\n",
            "WASM-TODO(channels): gap\n",
            "must contain at least one [[backlog]] row",
        ),
        SelfTestCase(
            "empty-corpus",
            _manifest("channels"),
            "no markers here\n",
            "actionable WASM-TODO corpus is empty",
            expected_markers=0,
        ),
        SelfTestCase(
            "generator-description-is-non-actionable",
            _manifest("channels"),
            "/// A WASM-TODO backlog row.\nWASM-TODO(channels): live gap\n",
            None,
            source_path="hew-capability-gen/src/lib.rs",
        ),
        SelfTestCase(
            "generator-hidden-marker-counterfactual",
            _manifest("channels"),
            "WASM-TODO(not-authoritative): hidden generator gap\n",
            "unknown WASM backlog id `not-authoritative`",
            source_path="hew-capability-gen/src/hidden.rs",
        ),
    )

    failures: list[str] = []
    with tempfile.TemporaryDirectory(prefix="hew-wasm-todo-selftest-") as temp:
        root = Path(temp)
        manifest_path = root / "wasm-capability-manifest.toml"
        for case in cases:
            manifest_path.write_text(case.manifest_src, encoding="utf-8")
            source_path = root / case.source_path
            source_path.parent.mkdir(parents=True, exist_ok=True)
            source_path.write_text(case.source_src, encoding="utf-8")
            try:
                markers = lint(root, manifest_path, [case.source_path])
            except LintError as err:
                if case.expected_error is None:
                    failures.append(f"{case.name}: unexpected failure: {err}")
                elif case.expected_error not in str(err):
                    failures.append(
                        f"{case.name}: expected diagnostic containing "
                        f"{case.expected_error!r}, got {str(err)!r}"
                    )
                else:
                    print(
                        f"lint-wasm-todo self-test: {case.name}: ok",
                        file=sys.stderr,
                    )
            else:
                if case.expected_error is not None:
                    failures.append(
                        f"{case.name}: expected failure containing "
                        f"{case.expected_error!r}, got pass"
                    )
                elif len(markers) != case.expected_markers:
                    failures.append(
                        f"{case.name}: expected {case.expected_markers} marker(s), "
                        f"got {len(markers)}"
                    )
                else:
                    print(
                        f"lint-wasm-todo self-test: {case.name}: ok",
                        file=sys.stderr,
                    )
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
