#!/usr/bin/env python3
"""Fail closed on additions to reviewed compiler identity/ownership seams.

Discovery is performed by the repository's pinned ast-grep Rust parser. Raw
source text is never searched, so comments and literal contents cannot create
or hide authority findings.
"""

from __future__ import annotations

import argparse
import bisect
import csv
import json
import subprocess
import sys
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path

COMPILER_ROOTS = (
    "hew-types/src",
    "hew-hir/src",
    "hew-analysis/src",
    "hew-mir/src",
    "hew-codegen-rs/src",
)
PRESENTATION_CATEGORIES = {"debug-metadata"}
ALL_GROUPS = {
    "semantic-leaf-name",
    "string-method-identity",
    "legacy-heap-reader",
    "checker-hir-publication",
    "mir-ownership-sink",
}
ITEM_KINDS = (
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
)
DEBUG_CONTEXT_PATTERNS = {
    "debug-struct-type-argument": (
        "$R.create_struct_type($A0, $D1, $A2, $A3, $A4, $A5, "
        "$A6, $A7, $A8, $A9, $A10, $D11)",
        ("D1", "D11"),
    ),
    "debug-enumerator-argument": (
        "$R.create_enumerator($D0, $A1, $A2)",
        ("D0",),
    ),
    "debug-member-type-argument": (
        "$R.create_member_type($A0, $D1, $A2, $A3, $A4, $A5, $A6, $A7, $A8)",
        ("D1",),
    ),
}


@dataclass(frozen=True, order=True)
class Finding:
    group: str
    form: str
    path: str
    line: int
    column: int
    byte_start: int
    byte_end: int
    text: str


@dataclass(frozen=True, order=True)
class SyntaxRange:
    path: str
    byte_start: int
    byte_end: int

    def contains(self, finding: Finding) -> bool:
        return (
            self.path == finding.path
            and self.byte_start <= finding.byte_start
            and finding.byte_end <= self.byte_end
        )


def is_source_path(path: str) -> bool:
    return not (
        "/tests/" in path or path.endswith(("_tests.rs", "_test.rs", "/tests.rs"))
    )


def query_command(
    ast_grep: Path, *, pattern: str | None, kind: str | None
) -> list[str]:
    command = [str(ast_grep), "run", "--lang", "rust", "--json=stream"]
    if pattern is not None:
        command.extend(("--pattern", pattern))
    elif kind is not None:
        command.extend(("--kind", kind))
    else:
        raise AssertionError("query requires a pattern or kind")
    return command


def parser_sentinel(ast_grep: Path) -> None:
    """Prove the executable parses Rust and returns structured query output."""
    command = query_command(ast_grep, pattern="$F($$$ARGS)", kind=None)
    command.append("--stdin")
    result = subprocess.run(
        command,
        input="fn authority_sentinel() { sentinel_authority(); }\n",
        text=True,
        capture_output=True,
    )
    try:
        rows = [json.loads(line) for line in result.stdout.splitlines() if line]
    except json.JSONDecodeError as error:
        raise SystemExit(
            f"pinned ast-grep sentinel returned invalid JSON: {error}"
        ) from error
    valid = (
        result.returncode == 0
        and len(rows) == 1
        and rows[0].get("text") == "sentinel_authority()"
        and rows[0].get("metaVariables", {}).get("single", {}).get("F", {}).get("text")
        == "sentinel_authority"
    )
    if not valid:
        if result.stderr:
            print(result.stderr, file=sys.stderr, end="")
        raise SystemExit("pinned ast-grep parser/query sentinel failed closed")


def run_query(
    ast_grep: Path,
    root: Path,
    *,
    pattern: str | None = None,
    kind: str | None = None,
) -> list[dict[str, object]]:
    paths = [path for path in COMPILER_ROOTS if (root / path).exists()]
    if not paths:
        return []
    command = query_command(ast_grep, pattern=pattern, kind=kind)
    command.extend(paths)
    result = subprocess.run(command, cwd=root, text=True, capture_output=True)
    # ast-grep uses status 1 for a valid query with no matches. The mandatory
    # sentinel above proves that status 1 is not a dead/non-parser executable.
    if result.returncode not in (0, 1) or (result.returncode == 1 and result.stderr):
        print(result.stderr, file=sys.stderr, end="")
        raise SystemExit("pinned ast-grep Rust query failed")
    try:
        return [json.loads(line) for line in result.stdout.splitlines() if line]
    except json.JSONDecodeError as error:
        raise SystemExit(f"pinned ast-grep returned invalid JSON: {error}") from error


def node_range(match: dict[str, object]) -> SyntaxRange:
    offsets = match["range"]["byteOffset"]  # type: ignore[index]
    return SyntaxRange(
        str(match["file"]),
        int(offsets["start"]),  # type: ignore[index]
        int(offsets["end"]),  # type: ignore[index]
    )


def finding(group: str, form: str, match: dict[str, object]) -> Finding:
    start = match["range"]["start"]  # type: ignore[index]
    offsets = match["range"]["byteOffset"]  # type: ignore[index]
    return Finding(
        group,
        form,
        str(match["file"]),
        int(start["line"]) + 1,  # type: ignore[index]
        int(start["column"]) + 1,  # type: ignore[index]
        int(offsets["start"]),  # type: ignore[index]
        int(offsets["end"]),  # type: ignore[index]
        " ".join(str(match["text"]).split()),
    )


def single_meta(match: dict[str, object], name: str) -> str:
    meta = match.get("metaVariables", {})
    return str(meta.get("single", {}).get(name, {}).get("text", ""))  # type: ignore[union-attr]


def single_meta_range(match: dict[str, object], name: str) -> SyntaxRange | None:
    meta = match.get("metaVariables", {})
    value = meta.get("single", {}).get(name)  # type: ignore[union-attr]
    if not isinstance(value, dict) or "range" not in value:
        return None
    offsets = value["range"]["byteOffset"]
    return SyntaxRange(str(match["file"]), int(offsets["start"]), int(offsets["end"]))


class CfgPredicateParser:
    """Balanced parser for the small cfg predicate language we rely on."""

    def __init__(self, text: str):
        self.tokens = self.tokenize(text)
        self.position = 0

    @staticmethod
    def tokenize(text: str) -> list[str]:
        tokens: list[str] = []
        index = 0
        while index < len(text):
            char = text[index]
            if char.isspace():
                index += 1
            elif char in "(),=":
                tokens.append(char)
                index += 1
            elif char == '"':
                end = index + 1
                while end < len(text):
                    if text[end] == "\\":
                        end += 2
                    elif text[end] == '"':
                        end += 1
                        break
                    else:
                        end += 1
                tokens.append(text[index:end])
                index = end
            elif char.isalnum() or char == "_":
                end = index + 1
                while end < len(text) and (text[end].isalnum() or text[end] == "_"):
                    end += 1
                tokens.append(text[index:end])
                index = end
            else:
                tokens.append(char)
                index += 1
        return tokens

    def take(self, token: str | None = None) -> str | None:
        if self.position == len(self.tokens):
            return None
        value = self.tokens[self.position]
        if token is not None and value != token:
            return None
        self.position += 1
        return value

    def predicate(self) -> bool | None:
        name = self.take()
        if name is None or not (name[0].isalnum() or name[0] == "_"):
            return None
        if self.take("=") is not None:
            return False if self.take() is not None else None
        if self.take("(") is None:
            return name == "test"
        children: list[bool] = []
        if self.take(")") is None:
            while True:
                child = self.predicate()
                if child is None:
                    return None
                children.append(child)
                if self.take(")") is not None:
                    break
                if self.take(",") is None:
                    return None
        if name == "all":
            return any(children)
        if name == "any":
            return bool(children) and all(children)
        # Be conservative for `not` and unknown predicate functions.
        return False


def is_test_attribute(text: str) -> bool:
    stripped = text.strip()
    if not (stripped.startswith("#[") and stripped.endswith("]")):
        return False
    body = stripped[2:-1].strip()
    if body == "test":
        return True
    parser = CfgPredicateParser(body)
    if parser.take("cfg") is None or parser.take("(") is None:
        return False
    result = parser.predicate()
    return bool(result) and parser.take(")") is not None and parser.take() is None


def test_only_ranges(ast_grep: Path, root: Path) -> list[SyntaxRange]:
    """Map parsed cfg(test)/test attributes to the parsed item they govern."""
    attributes = [
        node_range(match)
        for match in run_query(ast_grep, root, kind="attribute_item")
        if is_source_path(str(match["file"])) and is_test_attribute(str(match["text"]))
    ]
    items_by_path: defaultdict[str, list[SyntaxRange]] = defaultdict(list)
    for kind in ITEM_KINDS:
        for match in run_query(ast_grep, root, kind=kind):
            if is_source_path(str(match["file"])):
                item = node_range(match)
                items_by_path[item.path].append(item)
    exclusions: set[SyntaxRange] = set()
    for attribute in attributes:
        candidates = [
            item
            for item in items_by_path[attribute.path]
            if item.byte_start >= attribute.byte_end
        ]
        if not candidates:
            raise SystemExit(
                f"parsed test attribute has no following item: {attribute.path}:{attribute.byte_start}"
            )
        exclusions.add(
            min(candidates, key=lambda item: (item.byte_start, item.byte_end))
        )
    return sorted(exclusions)


def excluded(finding: Finding, test_ranges: list[SyntaxRange]) -> bool:
    return not is_source_path(finding.path) or any(
        item.contains(finding) for item in test_ranges
    )


def discover(ast_grep: Path, root: Path) -> tuple[set[Finding], list[SyntaxRange]]:
    test_ranges = test_only_ranges(ast_grep, root)
    findings: set[Finding] = set()

    # Identifier nodes remain parsed inside Rust macro token trees. This closes
    # the call-expression blind spot without treating comments or string tokens
    # as code. Imports, declarations, local names, qualified calls, and macro
    # arguments all stay visible until their path/form inventory is retired.
    for match in run_query(ast_grep, root, kind="identifier"):
        forms = {
            "short_name": "short-name-identifier",
            "rsplit": "leaf-rsplit-identifier",
            "rsplit_once": "leaf-rsplit-once-identifier",
        }
        if str(match["text"]) in forms:
            findings.add(
                finding("semantic-leaf-name", forms[str(match["text"])], match)
            )
    for match in run_query(ast_grep, root, kind="field_identifier"):
        forms = {
            "short_name": "short-name-field",
            "rsplit": "leaf-rsplit-field",
            "rsplit_once": "leaf-rsplit-once-field",
        }
        if str(match["text"]) in forms:
            findings.add(
                finding("semantic-leaf-name", forms[str(match["text"])], match)
            )

    calls = run_query(ast_grep, root, pattern="$F($$$ARGS)")
    for match in calls:
        callee = single_meta(match, "F")
        leaf = callee.split("::")[-1]
        if leaf == "ty_owns_heap":
            findings.add(finding("legacy-heap-reader", "heap-reader-call", match))
        if leaf == "classify" and callee.rsplit("::", 1)[0].split("::")[-1] in {
            "ValueOwnership",
            "OwnershipDecision",
        }:
            findings.add(
                finding("mir-ownership-sink", "ownership-classify-call", match)
            )

    for match in run_query(ast_grep, root, pattern="$R.insert($$$ARGS)"):
        receiver = single_meta(match, "R").split(".")[-1]
        if receiver in {"expr_types", "resolved_calls", "call_targets"}:
            findings.add(
                finding("checker-hir-publication", "checker-insert-call", match)
            )

    literals_by_path: defaultdict[str, list[tuple[int, int, str]]] = defaultdict(list)
    for kind in ("string_literal", "raw_string_literal"):
        for literal in run_query(ast_grep, root, kind=kind):
            literal_range = node_range(literal)
            literals_by_path[literal_range.path].append(
                (literal_range.byte_start, literal_range.byte_end, str(literal["text"]))
            )
    literal_indexes: dict[str, tuple[list[int], list[tuple[int, int, str]]]] = {}
    for path, literals in literals_by_path.items():
        literals.sort()
        literal_indexes[path] = ([start for start, _, _ in literals], literals)

    for match in run_query(ast_grep, root, pattern="$M!"):
        if single_meta(match, "M").split("::")[-1] != "format":
            continue
        macro_range = node_range(match)
        starts, literals = literal_indexes.get(macro_range.path, ([], []))
        literal_index = bisect.bisect_left(starts, macro_range.byte_start)
        first = literals[literal_index] if literal_index < len(literals) else None
        if first is not None and first[1] <= macro_range.byte_end and "::" in first[2]:
            findings.add(
                finding("string-method-identity", "qualified-format-macro", match)
            )

    return {item for item in findings if not excluded(item, test_ranges)}, test_ranges


def presentation_candidates(
    ast_grep: Path, root: Path, findings: set[Finding], test_ranges: list[SyntaxRange]
) -> set[tuple[str, int, int, str, str]]:
    short_name_calls = {
        node_range(match)
        for match in run_query(ast_grep, root, pattern="short_name($X)")
    }
    contexts: list[tuple[str, SyntaxRange]] = []
    for context_form, (pattern, designated_names) in DEBUG_CONTEXT_PATTERNS.items():
        for match in run_query(ast_grep, root, pattern=pattern):
            receiver = single_meta(match, "R")
            if receiver.split(".")[-1] == "di_builder":
                for name in designated_names:
                    designated = single_meta_range(match, name)
                    if designated is not None and designated in short_name_calls:
                        contexts.append((context_form, designated))
    candidates = set()
    for item in findings:
        if item.form != "short-name-identifier" or excluded(item, test_ranges):
            continue
        for context_form, context in contexts:
            if (
                item.path == context.path
                and item.byte_start == context.byte_start
                and item.byte_end == context.byte_start + len("short_name")
            ):
                candidates.add(
                    (item.path, item.line, item.column, item.form, context_form)
                )
    return candidates


def split_top_level(text: str) -> list[str]:
    parts: list[str] = []
    start = depth = 0
    opens = {"<", "(", "["}
    closes = {">", ")", "]"}
    for index, char in enumerate(text):
        if char in opens:
            depth += 1
        elif char in closes:
            depth -= 1
        elif char == "," and depth == 0:
            parts.append(text[start:index].strip())
            start = index + 1
    parts.append(text[start:].strip())
    return parts


def generic_parts(text: str) -> tuple[str, list[str]] | None:
    stripped = text.strip()
    try:
        opening = stripped.index("<")
    except ValueError:
        return None
    if not stripped.endswith(">"):
        return None
    outer = "".join(stripped[:opening].split()).split("::")[-1]
    return outer, split_top_level(stripped[opening + 1 : -1])


def strip_type_indirection(text: str) -> str:
    """Remove parsed reference/raw-pointer prefixes without eating type names."""
    value = text.strip()
    while True:
        if value.startswith("&"):
            value = value[1:].lstrip()
            if value.startswith("'"):
                end = 1
                while end < len(value) and (value[end].isalnum() or value[end] == "_"):
                    end += 1
                value = value[end:].lstrip()
            if value == "mut" or value.startswith("mut "):
                value = value[3:].lstrip()
            continue
        pointer = next(
            (
                prefix
                for prefix in ("*const", "*mut")
                if value == prefix or value.startswith(prefix + " ")
            ),
            None,
        )
        if pointer is None:
            return value
        value = value[len(pointer) :].lstrip()


def base_type(text: str) -> str:
    compact = "".join(strip_type_indirection(text).split())
    return compact.split("::")[-1]


def scalar_site_value(text: str) -> bool:
    normalized = strip_type_indirection(text)
    if base_type(normalized) == "SiteId":
        return True
    parts = generic_parts(normalized)
    return bool(
        parts
        and parts[0] in {"Option", "Box", "Rc", "Arc", "Cell", "RefCell"}
        and len(parts[1]) == 1
        and scalar_site_value(parts[1][0])
    )


def scalar_span_site_type(text: str) -> bool:
    parts = generic_parts(text)
    if not parts:
        return False
    outer, args = parts
    if outer in {"HashMap", "BTreeMap", "IndexMap", "FxHashMap"} and len(args) >= 2:
        return base_type(args[0]) == "SpanKey" and scalar_site_value(args[1])
    if outer in {"HashSet", "BTreeSet", "IndexSet", "FxHashSet"} and len(args) == 1:
        item = args[0]
        if item.startswith("(") and item.endswith(")"):
            tuple_args = split_top_level(item[1:-1])
            return (
                len(tuple_args) == 2
                and base_type(tuple_args[0]) == "SpanKey"
                and scalar_site_value(tuple_args[1])
            )
    return False


def scalar_span_site_findings(
    ast_grep: Path, root: Path, test_ranges: list[SyntaxRange]
) -> list[Finding]:
    result = []
    for match in run_query(ast_grep, root, kind="generic_type"):
        item = finding("forbidden-span-site-scalar", "scalar-span-site-type", match)
        if scalar_span_site_type(str(match["text"])) and not excluded(
            item, test_ranges
        ):
            result.append(item)
    return sorted(set(result))


def canonical_stage(group: str, form: str, path: str) -> str:
    """Return the stage at which the plan can actually retire this seam."""
    if group == "checker-hir-publication":
        return "stage-2"
    if group == "mir-ownership-sink":
        return "stage-4"
    if group == "legacy-heap-reader":
        return (
            "stage-5"
            if path.startswith("hew-codegen-rs/") or path.endswith("model.rs")
            else "stage-4"
        )
    if group == "string-method-identity":
        if path.startswith(("hew-types/", "hew-hir/", "hew-analysis/")):
            return "stage-1"
        if path.endswith(("lower/drop_plan.rs", "lower/mod.rs")):
            return "stage-3"
        return "stage-5"
    if group != "semantic-leaf-name" or form not in {
        "short-name-identifier",
        "short-name-field",
        "leaf-rsplit-identifier",
        "leaf-rsplit-once-identifier",
        "leaf-rsplit-field",
        "leaf-rsplit-once-field",
    }:
        raise SystemExit(f"no canonical cutover stage for {group}/{form} at {path}")
    if path.startswith(("hew-types/", "hew-hir/", "hew-analysis/")):
        return "stage-1"
    if path.startswith("hew-codegen-rs/"):
        return "stage-5"
    if path.endswith(("lower/drop_plan.rs", "lower/mod.rs")):
        return "stage-3"
    if path.endswith(("model.rs", "state_clone.rs", "thunk_requirements.rs")):
        return "stage-5"
    return "stage-4"


def load_presentation(
    path: Path,
) -> dict[tuple[str, int, int, str, str], dict[str, str]]:
    rows: dict[tuple[str, int, int, str, str], dict[str, str]] = {}
    with path.open(newline="") as handle:
        source = (line for line in handle if line.strip() and not line.startswith("#"))
        for row in csv.DictReader(source, delimiter="\t"):
            required = (
                "path",
                "line",
                "column",
                "form",
                "context_form",
                "category",
                "retirement_stage",
                "reason",
            )
            if any(not row.get(field) for field in required):
                raise SystemExit(f"invalid presentation baseline row: {row}")
            if row["category"] not in PRESENTATION_CATEGORIES:
                raise SystemExit(f"invalid presentation category: {row['category']}")
            if row["retirement_stage"] != "post-stage-5":
                raise SystemExit(f"presentation retirement must follow Stage 5: {row}")
            if not row["line"].isdigit() or not row["column"].isdigit():
                raise SystemExit(f"invalid presentation location: {row}")
            key = (
                row["path"],
                int(row["line"]),
                int(row["column"]),
                row["form"],
                row["context_form"],
            )
            if key in rows:
                raise SystemExit(f"duplicate presentation baseline row: {key}")
            rows[key] = row
    return rows


def load_inventory(path: Path) -> dict[tuple[str, str, str], int]:
    expected: dict[tuple[str, str, str], int] = {}
    with path.open(newline="") as handle:
        source = (line for line in handle if line.strip() and not line.startswith("#"))
        for row in csv.DictReader(source, delimiter="\t"):
            group, form, target, count = (
                row["group"],
                row["form"],
                row["path"],
                row["count"],
            )
            if (
                group not in ALL_GROUPS
                or not form
                or not target
                or not count.isdigit()
                or int(count) == 0
            ):
                raise SystemExit(f"invalid authority inventory row: {row}")
            required_stage = canonical_stage(group, form, target)
            if row.get("retirement_stage") != required_stage:
                raise SystemExit(
                    "authority inventory retirement stage is not canonical: "
                    f"{group}/{form} {target} requires {required_stage}, found "
                    f"{row.get('retirement_stage')}"
                )
            if not row.get("reason"):
                raise SystemExit(f"authority inventory row lacks a reason: {row}")
            key = (group, form, target)
            if key in expected:
                raise SystemExit(f"duplicate authority inventory row: {key}")
            expected[key] = int(count)
    return expected


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--root", type=Path, default=Path(__file__).resolve().parents[1]
    )
    parser.add_argument("--inventory", type=Path)
    parser.add_argument("--presentation-baseline", type=Path)
    parser.add_argument("--ast-grep", type=Path)
    args = parser.parse_args()
    root = args.root.resolve()
    repo_root = Path(__file__).resolve().parents[1]
    ast_grep = (args.ast_grep or repo_root / ".ast-grep/tool/bin/ast-grep").resolve()
    if not ast_grep.is_file():
        raise SystemExit(f"pinned ast-grep is absent: {ast_grep}")
    parser_sentinel(ast_grep)

    inventory = args.inventory or root / "scripts/structural-authority-inventory.tsv"
    presentation_path = (
        args.presentation_baseline
        or root / "scripts/structural-authority-presentation.tsv"
    )
    expected = load_inventory(inventory)
    presentation = load_presentation(presentation_path)
    findings, test_ranges = discover(ast_grep, root)
    candidates = presentation_candidates(ast_grep, root, findings, test_ranges)
    stale_presentation = sorted(set(presentation) - candidates)
    exempt_locations = {
        (path, line, column, form) for path, line, column, form, _ in presentation
    }
    semantic = {
        item
        for item in findings
        if (item.path, item.line, item.column, item.form) not in exempt_locations
    }

    actual: defaultdict[tuple[str, str, str], int] = defaultdict(int)
    for item in semantic:
        actual[(item.group, item.form, item.path)] += 1
    failures = []
    for key in sorted(set(expected) | set(actual)):
        want, got = expected.get(key, 0), actual.get(key, 0)
        if want != got:
            failures.append(f"{key[0]}/{key[1]} {key[2]}: expected {want}, found {got}")
    for key in stale_presentation:
        failures.append(f"presentation AST context disappeared or drifted: {key}")
    forbidden = scalar_span_site_findings(ast_grep, root, test_ranges)
    for item in forbidden:
        failures.append(
            f"forbidden scalar SpanKey -> SiteId authority at "
            f"{item.path}:{item.line}:{item.column}: {item.text}"
        )
    if failures:
        print(
            "structural authority inventory changed; review every explicit form/path target:",
            file=sys.stderr,
        )
        print("\n".join(f"  - {item}" for item in failures), file=sys.stderr)
        return 1

    floor_rows = (root / "scripts/corpus-floors.tsv").read_text().splitlines()
    floor = next(
        (
            line.split("\t")[2]
            for line in floor_rows
            if line.startswith("structural-authority-inventory\t")
        ),
        None,
    )
    reviewed = len(expected) + len(presentation)
    if floor is None or not floor.isdigit() or int(floor) != reviewed:
        print(
            f"structural-authority-inventory corpus floor is stale or missing (expected {reviewed})",
            file=sys.stderr,
        )
        return 1
    semantic_leaf_count = sum(
        count
        for (group, _, _), count in actual.items()
        if group == "semantic-leaf-name"
    )
    print(
        "structural authority inventory: "
        f"{semantic_leaf_count} semantic leaf-name syntax nodes in "
        f"{sum(group == 'semantic-leaf-name' for group, _, _ in expected)} form/path rows; "
        f"{len(presentation)} exact presentation AST contexts; "
        f"{len(expected)} authority form/path rows; "
        f"{len(test_ranges)} parsed test-only item ranges; "
        "0 scalar SpanKey -> SiteId authorities"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
