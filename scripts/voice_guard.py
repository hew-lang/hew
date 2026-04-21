#!/usr/bin/env python3

from __future__ import annotations

import argparse
import json
import re
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True)
class Finding:
    text_index: int
    match: str
    start: int
    end: int
    pattern_name: str


@dataclass(frozen=True)
class TextEntry:
    label: str
    text: str


FENCE_RE = re.compile(r"```.*?```", re.DOTALL)


def _compile(pattern: str, *, ignore_case: bool = True) -> re.Pattern[str]:
    flags = re.IGNORECASE if ignore_case else 0
    return re.compile(pattern, flags)


SCAN_PATTERNS: tuple[tuple[str, re.Pattern[str]], ...] = (
    ("model", _compile(r"\bgpt-\d(?:\.\d+)*\b")),
    ("model", _compile(r"\bclaude-?(?:opus|sonnet|haiku)\b")),
    ("model", _compile(r"\b(?:sonnet|haiku|opus)\b")),
    ("orchestration", _compile(r"\bwave\s*\d+\b")),
    ("orchestration", _compile(r"\borchestrat\w*\b")),
    ("orchestration", _compile(r"\bAPPROVE(?:D)?\b")),
    ("orchestration", _compile(r"\bPASS:", ignore_case=False)),
    ("orchestration", _compile(r"\bR\d+\s+F\d+\b")),
    ("orchestration", _compile(r"\bfallback lane\b")),
    ("orchestration", _compile(r"\bbounded lane\b")),
    ("orchestration", _compile(r"\bpreflight lane\b")),
    ("orchestration", _compile(r"\bthis lane does(?: not)?\b")),
    ("orchestration", _compile(r"\bwhat this lane does\b")),
    ("orchestration", _compile(r"\bReview PASS\b")),
    ("orchestration", _compile(r"\blane\b")),
)

PLAINTEXT_ONLY_PATTERNS: tuple[tuple[str, re.Pattern[str]], ...] = (
    ("path", _compile(r"\.claude/")),
)

ALLOWLIST_PATTERNS: tuple[re.Pattern[str], ...] = (
    _compile(r"\blane change in routing\b"),
    _compile(r"\bpass:\s+the test passes\b"),
)


def _strip_fenced_code_blocks(text: str) -> str:
    return FENCE_RE.sub("", text)


def _is_allowlisted(text: str, start: int, end: int) -> bool:
    for pattern in ALLOWLIST_PATTERNS:
        for match in pattern.finditer(text):
            if match.start() <= start and end <= match.end():
                return True
    return False


def _overlaps_existing(
    findings: list[Finding], text_index: int, start: int, end: int
) -> bool:
    for finding in findings:
        if finding.text_index != text_index:
            continue
        if start < finding.end and finding.start < end:
            return True
    return False


def scan(texts: list[str]) -> list[Finding]:
    findings: list[Finding] = []
    for text_index, text in enumerate(texts):
        for pattern_name, pattern in SCAN_PATTERNS:
            for match in pattern.finditer(text):
                if _is_allowlisted(text, match.start(), match.end()):
                    continue
                if _overlaps_existing(findings, text_index, match.start(), match.end()):
                    continue
                findings.append(
                    Finding(
                        text_index=text_index,
                        match=match.group(0),
                        start=match.start(),
                        end=match.end(),
                        pattern_name=pattern_name,
                    )
                )

        stripped = _strip_fenced_code_blocks(text)
        for pattern_name, pattern in PLAINTEXT_ONLY_PATTERNS:
            for match in pattern.finditer(stripped):
                findings.append(
                    Finding(
                        text_index=text_index,
                        match=match.group(0),
                        start=match.start(),
                        end=match.end(),
                        pattern_name=pattern_name,
                    )
                )

    findings.sort(
        key=lambda finding: (
            finding.text_index,
            finding.start,
            finding.end,
            finding.match.lower(),
        )
    )
    return findings


def _load_pr_entries(pr_number: int) -> list[TextEntry]:
    result = subprocess.run(
        ["gh", "pr", "view", str(pr_number), "--json", "title,body,commits"],
        check=True,
        capture_output=True,
        text=True,
    )
    payload = json.loads(result.stdout)
    entries = [
        TextEntry("title", payload.get("title") or ""),
        TextEntry("body", payload.get("body") or ""),
    ]
    for commit in payload.get("commits", []):
        sha = commit["oid"]
        headline = commit.get("messageHeadline") or ""
        body = commit.get("messageBody") or ""
        message = headline if not body else f"{headline}\n\n{body}"
        entries.append(TextEntry(f"commit {sha}", message))
    return entries


def _load_entries_from_args(args: argparse.Namespace) -> list[TextEntry]:
    if args.pr_number is not None:
        return _load_pr_entries(args.pr_number)

    entries = [
        TextEntry("title", args.title or ""),
        TextEntry("body", args.body or ""),
    ]
    if args.commits_file:
        entries.extend(_load_commits_file(Path(args.commits_file)))
    return entries


def _load_commits_file(path: Path) -> list[TextEntry]:
    raw = path.read_text(encoding="utf-8")
    try:
        payload = json.loads(raw)
    except json.JSONDecodeError:
        stripped = raw.strip()
        return [TextEntry("commit local", stripped)] if stripped else []

    if not isinstance(payload, list):
        raise SystemExit("--commits-file JSON must be a list")

    entries: list[TextEntry] = []
    for index, item in enumerate(payload):
        if isinstance(item, str):
            message = item
            sha = f"local-{index + 1}"
        elif isinstance(item, dict):
            sha = str(item.get("sha") or item.get("oid") or f"local-{index + 1}")
            headline = str(item.get("messageHeadline") or item.get("headline") or "")
            body = str(item.get("messageBody") or item.get("body") or "")
            message = str(
                item.get("message")
                or (headline if not body else f"{headline}\n\n{body}")
            )
        else:
            raise SystemExit("--commits-file JSON items must be strings or objects")
        entries.append(TextEntry(f"commit {sha}", message))
    return entries


def _parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Fail PR metadata that leaks internal orchestration voice."
    )
    parser.add_argument(
        "--pr-number", type=int, help="Pull request number to scan via gh pr view."
    )
    parser.add_argument("--title", help="PR title for local testing.")
    parser.add_argument("--body", help="PR body for local testing.")
    parser.add_argument(
        "--commits-file",
        help="Path to a JSON list of commit messages or commit objects, or a plain-text commit message.",
    )
    args = parser.parse_args(argv)

    if args.pr_number is None and not any([args.title, args.body, args.commits_file]):
        parser.error(
            "provide --pr-number or at least one of --title/--body/--commits-file"
        )

    return args


def main(argv: list[str] | None = None) -> int:
    args = _parse_args(argv or sys.argv[1:])
    entries = _load_entries_from_args(args)
    findings = scan([entry.text for entry in entries])

    for finding in findings:
        label = entries[finding.text_index].label
        print(
            f"voice-guard: forbidden token '{finding.match}' in {label}. "
            "See CONTRIBUTING for policy."
        )

    return 1 if findings else 0


if __name__ == "__main__":
    raise SystemExit(main())
