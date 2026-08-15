#!/usr/bin/env python3
"""Reject bare identifier keys in compiler string-keyed symbol tables.

The candidate and declaration inventories come from parsed Rust syntax via the
repository-pinned ast-grep executable.  Existing findings must be named in the
shared structural-authority inventory with an owner and a follow-on work ID.
"""

from __future__ import annotations

import argparse
import json
import re
import subprocess
import sys
from collections import Counter
from dataclasses import dataclass
from pathlib import Path


COMPILER_ROOTS = (
    "hew-types/src",
    "hew-hir/src",
    "hew-analysis/src",
    "hew-mir/src",
    "hew-codegen-rs/src",
)
KEYSPACE_NAME = re.compile(
    r"^[A-Za-z_][A-Za-z0-9_]*_(?:names|defs|layouts|sigs|registry)$"
)
BARE_IDENTIFIER = re.compile(r"^[A-Za-z_][A-Za-z0-9_]*$")
STRING_COLLECTION = re.compile(
    r"(?:^|[^A-Za-z0-9_])(?:std\s*::\s*collections\s*::\s*)?"
    r"Hash(?:Map|Set)\s*<\s*(?:std\s*::\s*string\s*::\s*)?String\b",
    re.DOTALL,
)
INFERRED_COLLECTION = re.compile(
    r"=\s*(?:std\s*::\s*collections\s*::\s*)?Hash(?:Map|Set)"
    r"\s*(?:::\s*<[^;=]+>)?\s*::\s*(?:new|default|with_capacity|with_hasher)\b",
    re.DOTALL,
)
DECLARED_NAME = re.compile(
    r"(?:^|\b(?:let\s+(?:mut\s+)?|pub(?:\s*\([^)]*\))?\s+))"
    r"([A-Za-z_][A-Za-z0-9_]*)\s*:",
    re.DOTALL,
)
INFERRED_NAME = re.compile(
    r"\blet\s+(?:mut\s+)?([A-Za-z_][A-Za-z0-9_]*)\s*=", re.DOTALL
)
ALLOW_PREFIX = "# canonical-keyspace-allow\t"
CANONICAL_ONLY_TABLES = {"type_defs", "machine_layout_names"}
CANDIDATE_RULE = (
    Path(__file__).resolve().parents[1]
    / "rules/rust/authority/canonical-keyspace-bare-insert.yml"
)
RUST_ONLY_CONFIG = Path(__file__).resolve().parents[1] / "tools/ast-grep-rust-only.yml"


@dataclass(frozen=True, order=True)
class Finding:
    path: str
    line: int
    table: str
    key: str


@dataclass(frozen=True)
class SyntaxRange:
    path: str
    start: int
    end: int

    def contains(self, path: str, offset: int) -> bool:
        return self.path == path and self.start <= offset < self.end


def query(
    ast_grep: Path,
    roots: list[Path],
    *,
    kind: str | None = None,
    pattern: str | None = None,
) -> list[dict[str, object]]:
    command = [
        str(ast_grep),
        "run",
        "--config",
        str(RUST_ONLY_CONFIG),
        "--lang",
        "rust",
        "--json=stream",
    ]
    if kind is not None:
        command.extend(("--kind", kind))
    elif pattern is not None:
        command.extend(("--pattern", pattern))
    else:
        raise AssertionError("ast-grep query needs a kind or pattern")
    command.extend(str(path) for path in roots)
    result = subprocess.run(
        command,
        cwd=roots[0].parents[2],
        check=False,
        capture_output=True,
        text=True,
    )
    if result.returncode not in (0, 1) or (result.returncode == 1 and result.stderr):
        if result.stderr:
            print(result.stderr, file=sys.stderr, end="")
        raise SystemExit("canonical keyspace ast-grep query failed closed")
    try:
        return [json.loads(line) for line in result.stdout.splitlines() if line]
    except json.JSONDecodeError as error:
        raise SystemExit(
            f"canonical keyspace ast-grep returned invalid JSON: {error}"
        ) from error


def relative_path(root: Path, row: dict[str, object]) -> str:
    target = Path(str(row["file"]))
    if not target.is_absolute():
        target = root / target
    return str(target.resolve().relative_to(root)).replace("\\", "/")


def candidate_rows(
    ast_grep: Path, root: Path, roots: list[Path]
) -> list[dict[str, object]]:
    command = [
        str(ast_grep),
        "scan",
        "--config",
        str(RUST_ONLY_CONFIG),
        "--rule",
        str(CANDIDATE_RULE),
        "--json=stream",
        *(str(path.relative_to(root)) for path in roots),
    ]
    result = subprocess.run(
        command,
        cwd=root,
        check=False,
        capture_output=True,
        text=True,
    )
    if result.returncode not in (0, 1) or (result.returncode == 1 and result.stderr):
        if result.stderr:
            print(result.stderr, file=sys.stderr, end="")
        raise SystemExit("canonical keyspace ast-grep rule failed closed")
    try:
        return [json.loads(line) for line in result.stdout.splitlines() if line]
    except json.JSONDecodeError as error:
        raise SystemExit(
            f"canonical keyspace ast-grep rule returned invalid JSON: {error}"
        ) from error


def is_test_path(path: str) -> bool:
    return "/tests/" in path or path.endswith(("_test.rs", "_tests.rs", "/tests.rs"))


def syntax_range(root: Path, row: dict[str, object]) -> SyntaxRange:
    source_range = row["range"]
    assert isinstance(source_range, dict)
    byte_offset = source_range["byteOffset"]
    assert isinstance(byte_offset, dict)
    return SyntaxRange(
        relative_path(root, row),
        int(byte_offset["start"]),
        int(byte_offset["end"]),
    )


def test_ranges(ast_grep: Path, root: Path, roots: list[Path]) -> list[SyntaxRange]:
    """Map parsed test attributes to the parsed Rust item they govern."""
    attributes = [
        syntax_range(root, row)
        for row in query(ast_grep, roots, kind="attribute_item")
        if re.fullmatch(
            r"\s*#\s*\[\s*(?:test|cfg\s*\(\s*test\s*\))\s*]\s*",
            str(row["text"]),
        )
    ]
    items: dict[str, list[SyntaxRange]] = {}
    for kind in (
        "mod_item",
        "function_item",
        "impl_item",
        "trait_item",
        "struct_item",
        "enum_item",
        "union_item",
        "type_item",
        "const_item",
        "static_item",
        "use_declaration",
        "extern_crate_declaration",
        "foreign_mod_item",
        "macro_definition",
        "macro_invocation",
        "let_declaration",
        "field_declaration",
        "enum_variant",
        "expression_statement",
    ):
        for row in query(ast_grep, roots, kind=kind):
            item = syntax_range(root, row)
            items.setdefault(item.path, []).append(item)
    excluded: set[SyntaxRange] = set()
    for attribute in attributes:
        candidates = [
            item
            for item in items.get(attribute.path, [])
            if item.start >= attribute.end
        ]
        if not candidates:
            raise SystemExit(
                f"parsed test attribute has no following item: {attribute.path}:{attribute.start}"
            )
        excluded.add(min(candidates, key=lambda item: (item.start, item.end)))
    return sorted(excluded, key=lambda item: (item.path, item.start, item.end))


def declaration_name(text: str) -> str | None:
    match = DECLARED_NAME.search(text)
    if match is not None:
        return match.group(1)
    match = INFERRED_NAME.search(text)
    return match.group(1) if match is not None else None


def is_string_collection_declaration(text: str) -> bool:
    colon = text.find(":")
    equals = text.find("=")
    if colon >= 0 and (equals < 0 or colon < equals):
        return (
            STRING_COLLECTION.search(text[colon + 1 : equals if equals >= 0 else None])
            is not None
        )
    return INFERRED_COLLECTION.search(text) is not None


def tracked_names(ast_grep: Path, roots: list[Path]) -> set[str]:
    names: set[str] = set()
    for kind in ("field_declaration", "parameter", "let_declaration"):
        for row in query(ast_grep, roots, kind=kind):
            text = str(row["text"])
            name = declaration_name(text)
            if (
                name is not None
                and KEYSPACE_NAME.fullmatch(name)
                and is_string_collection_declaration(text)
            ):
                names.add(name)
    return names


def receiver_name(text: str) -> str | None:
    compact = re.sub(r"\s+", "", text)
    match = re.search(r"(?:^|\.)([A-Za-z_][A-Za-z0-9_]*)$", compact)
    return match.group(1) if match is not None else None


def discover(ast_grep: Path, root: Path) -> list[Finding]:
    roots = [
        (root / item).resolve() for item in COMPILER_ROOTS if (root / item).is_dir()
    ]
    if not roots:
        raise SystemExit("canonical keyspace lint found no compiler source roots")
    declarations = tracked_names(ast_grep, roots)
    excluded = test_ranges(ast_grep, root, roots)
    findings: list[Finding] = []
    for row in candidate_rows(ast_grep, root, roots):
        path = relative_path(root, row)
        if is_test_path(path):
            continue
        source_range = row["range"]
        assert isinstance(source_range, dict)
        byte_offset = source_range["byteOffset"]
        start = source_range["start"]
        assert isinstance(byte_offset, dict) and isinstance(start, dict)
        if any(item.contains(path, int(byte_offset["start"])) for item in excluded):
            continue
        meta = row["metaVariables"]
        assert isinstance(meta, dict)
        single = meta["single"]
        assert isinstance(single, dict)
        table_meta, key_meta = single["TABLE"], single["KEY"]
        assert isinstance(table_meta, dict) and isinstance(key_meta, dict)
        table = receiver_name(str(table_meta["text"]))
        key = str(key_meta["text"])
        if table in declarations and BARE_IDENTIFIER.fullmatch(key):
            findings.append(Finding(path, int(start["line"]) + 1, table, key))
    return sorted(set(findings))


def load_allowlist(path: Path) -> dict[tuple[str, str], int]:
    allowed: dict[tuple[str, str], int] = {}
    for number, line in enumerate(path.read_text(encoding="utf-8").splitlines(), 1):
        if not line.startswith(ALLOW_PREFIX):
            continue
        fields = line.split("\t")
        if len(fields) != 7:
            raise SystemExit(
                f"invalid canonical keyspace allowlist row at {path}:{number}"
            )
        _, target, table, count, owner, follow_on_work, reason = fields
        if (
            not target.endswith(".rs")
            or not KEYSPACE_NAME.fullmatch(table)
            or not count.isdigit()
            or int(count) == 0
            or not owner
            or not follow_on_work
            or not reason
        ):
            raise SystemExit(
                f"invalid canonical keyspace allowlist row at {path}:{number}"
            )
        if table in CANONICAL_ONLY_TABLES:
            raise SystemExit(
                f"canonical-only keyspace cannot be allowlisted: {target} {table}"
            )
        key = (target, table)
        if key in allowed:
            raise SystemExit(
                f"duplicate canonical keyspace allowlist row: {target} {table}"
            )
        allowed[key] = int(count)
    return allowed


def check(
    ast_grep: Path, root: Path, inventory: Path
) -> tuple[list[Finding], list[str]]:
    findings = discover(ast_grep, root)
    actual = Counter((item.path, item.table) for item in findings)
    allowed = load_allowlist(inventory)
    failures: list[str] = []
    for key in sorted(set(actual) | set(allowed)):
        want, got = allowed.get(key, 0), actual.get(key, 0)
        if want != got:
            failures.append(f"{key[0]} {key[1]}: allowlisted {want}, found {got}")
    return findings, failures


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--root", type=Path, default=Path(__file__).resolve().parents[1]
    )
    parser.add_argument("--inventory", type=Path)
    parser.add_argument("--ast-grep", type=Path)
    args = parser.parse_args()
    root = args.root.resolve()
    inventory = args.inventory or root / "scripts/structural-authority-inventory.tsv"
    ast_grep = args.ast_grep or root / ".ast-grep/tool/bin/ast-grep"
    if not ast_grep.is_file():
        raise SystemExit(f"pinned ast-grep is absent: {ast_grep}")
    findings, failures = check(ast_grep.resolve(), root, inventory.resolve())
    if failures:
        print("canonical keyspace lint: FAIL", file=sys.stderr)
        for failure in failures:
            print(f"  - {failure}", file=sys.stderr)
        for item in findings:
            print(
                f"  - {item.path}:{item.line}: bare identifier `{item.key}` inserted into "
                f"`{item.table}`; use machine_layout_key(...), mangle_instantiation(...), "
                'or format!("{module_full_path}.{name}")',
                file=sys.stderr,
            )
        return 1
    print(
        f"canonical keyspace lint: PASS ({len(findings)} reviewed pre-existing bare inserts)"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
