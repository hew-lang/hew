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
import re
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
ALL_GROUPS = {
    "semantic-owner-shortening-sink",
    "string-method-identity",
    "legacy-heap-reader",
    "checker-hir-publication",
    "mir-ownership-sink",
    # RC1 authority carriers.  These inventories deliberately name parsed
    # syntax nodes rather than comments, strings, or a hand-maintained list of
    # source lines.  A new carrier/variant is therefore a failing addition,
    # even before a reviewer decides which retirement stage owns it.
    "checker-hir-fact-relation",
    "call-target-authority",
    "runtime-call-authority",
    "lifecycle-identity-authority",
    "suspend-authority",
    "owner-retirement-path",
    "monomorphic-enum-leaf-synthesis",
}
SEMANTIC_KEY_BUILDERS = {
    "scoped_module_item_name",
    "module_item_name",
    "qualified_item_name",
    "qualified_name",
}
RUNTIME_RESOLUTION_SINKS = {
    "resolve_runtime_symbol",
    "runtime_symbol_for_call_expr",
    "runtime_symbol_for_method",
}
CANONICAL_OWNER_NAMES = re.compile(
    r"^(?:canonical_owner|declaring_module|module_full_path|module_identity|resolved_module|source_module)$"
)
SEMANTIC_MAP_NAMES = {
    "by_module",
    "call_targets",
    "const_registry",
    "declarations",
    "direct_call_targets",
    "fn_registry",
    "fn_sigs",
    "import_spans",
    "machine_ctor_registry",
    "methods",
    "module_import_bindings",
    "modules",
    "nominal_ids",
    "opaque",
    "record_registry",
    "resolved_calls",
    "supers",
    "type_defs",
    "user_modules",
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


def generated_builtin_enum_leaves(root: Path) -> set[str]:
    """Derive audited enum leaves from the checked-in ABI authority table."""
    authority = root / "hew-types/src/stdlib_authority/codegen.rs"
    try:
        source = authority.read_text()
    except OSError as error:
        raise SystemExit(
            f"generated builtin enum ABI authority is unreadable: {error}"
        ) from error
    table = re.search(
        r"const\s+BUILTIN_ENUM_ABI\s*:\s*&\[BuiltinEnumAbi\]\s*=\s*&\[(.*?)\n\];",
        source,
        re.DOTALL,
    )
    if table is None:
        raise SystemExit(
            "generated builtin enum ABI authority table is absent or malformed"
        )
    body = table.group(1)
    entry_count = len(re.findall(r"\bBuiltinEnumAbi\s*\{", body))
    leaves = re.findall(r'\bname\s*:\s*"([A-Za-z_][A-Za-z0-9_]*)"', body)
    if entry_count == 0 or len(leaves) != entry_count:
        raise SystemExit(
            "generated builtin enum ABI authority has missing or malformed name fields"
        )
    if len(set(leaves)) != len(leaves):
        raise SystemExit(
            "generated builtin enum ABI authority has duplicate leaf names"
        )
    return set(leaves)


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
    ast_grep: Path,
    *,
    pattern: str | None,
    kind: str | None,
    lang: str = "rust",
    config: Path | None = None,
) -> list[str]:
    command = [str(ast_grep), "run"]
    if config is not None:
        command.extend(("--config", str(config)))
    command.extend(("--lang", lang, "--json=stream"))
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


def hew_parser_sentinel(ast_grep: Path, root: Path) -> None:
    """Prove the pinned custom Hew grammar is live, not just installed."""
    config = root / "sgconfig.yml"
    if not config.is_file():
        raise SystemExit("pinned ast-grep Hew config is absent")
    command = query_command(
        ast_grep,
        pattern="fn $F($$$ARGS) { $$$BODY }",
        kind=None,
        lang="hew",
        config=config,
    )
    command.append("--stdin")
    result = subprocess.run(
        command,
        cwd=root,
        input="fn opaque_resource_sentinel(value: i64) { value; }\n",
        text=True,
        capture_output=True,
    )
    try:
        rows = [json.loads(line) for line in result.stdout.splitlines() if line]
    except json.JSONDecodeError as error:
        raise SystemExit(
            f"pinned ast-grep Hew sentinel returned invalid JSON: {error}"
        ) from error
    valid = (
        result.returncode == 0
        and len(rows) == 1
        and rows[0].get("metaVariables", {}).get("single", {}).get("F", {}).get("text")
        == "opaque_resource_sentinel"
    )
    if not valid:
        if result.stderr:
            print(result.stderr, file=sys.stderr, end="")
        raise SystemExit("pinned ast-grep Hew parser/query sentinel failed closed")


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


def run_hew_query(
    ast_grep: Path, root: Path, *, pattern: str | None = None, kind: str | None = None
) -> list[dict[str, object]]:
    """Run a Hew query through the pinned grammar, scoped to shipped stdlib."""
    std = root / "std"
    if not std.is_dir():
        return []
    command = query_command(
        ast_grep,
        pattern=pattern,
        kind=kind,
        lang="hew",
        config=root / "sgconfig.yml",
    )
    command.append("std")
    result = subprocess.run(command, cwd=root, text=True, capture_output=True)
    if result.returncode not in (0, 1) or (result.returncode == 1 and result.stderr):
        print(result.stderr, file=sys.stderr, end="")
        raise SystemExit("pinned ast-grep Hew query failed")
    try:
        return [json.loads(line) for line in result.stdout.splitlines() if line]
    except json.JSONDecodeError as error:
        raise SystemExit(
            f"pinned ast-grep Hew query returned invalid JSON: {error}"
        ) from error


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


def range_contains(outer: SyntaxRange, inner: SyntaxRange) -> bool:
    return (
        outer.path == inner.path
        and outer.byte_start <= inner.byte_start
        and inner.byte_end <= outer.byte_end
    )


def semantic_map_receiver(receiver: str) -> bool:
    leaf = receiver.split(".")[-1].strip()
    return (
        leaf in SEMANTIC_MAP_NAMES
        or leaf.endswith("_registry")
        or leaf.endswith("_registries")
        or leaf.endswith("_layouts")
        or leaf.endswith("_definitions")
        or leaf.endswith("_declarations")
    ) and leaf not in {"diagnostics", "errors", "messages"}


def leaf_path_receiver(receiver: str) -> bool:
    compact = "".join(receiver.split())
    return bool(
        re.search(
            r"(?:^|[.])(?:path|segments|module_path|module_id|mod_id|owner)(?:$|[.])",
            compact,
        )
    )


def separator_is_qualification(separator: str) -> bool:
    return separator.strip() in {'"::"', "'::'", '"."', "'.'"}


def implicit_format_names(text: str) -> set[str]:
    """Return Rust captured-format identifiers, excluding escaped braces."""
    return set(re.findall(r"(?<!\{)\{([A-Za-z_][A-Za-z0-9_]*)[^{}]*\}(?!\})", text))


def semantic_owner_shortening_findings(
    ast_grep: Path, root: Path, test_ranges: list[SyntaxRange]
) -> set[Finding]:
    """Find leaf module spellings that flow into executable identity sinks.

    This is deliberately an intraprocedural parsed-AST taint pass.  It catches
    the common two-step escape from a direct ast-grep rule (`module_short`, then
    `key`, then `registry.insert(key, ...)`) without treating formatting,
    diagnostics, or ordinary collection `.last()` calls as identity authority.
    """
    identifiers = run_query(ast_grep, root, kind="identifier")
    literals = []
    for literal_kind in ("string_literal", "raw_string_literal"):
        literals.extend(run_query(ast_grep, root, kind=literal_kind))

    source_ranges: list[SyntaxRange] = []
    for match in run_query(ast_grep, root, pattern="short_name($X)"):
        source_ranges.append(node_range(match))
    for pattern in (
        "current_module_short()",
        "$X.current_module_short()",
        "module_short_name($X)",
        "$X.module_short_name()",
    ):
        source_ranges.extend(
            node_range(match) for match in run_query(ast_grep, root, pattern=pattern)
        )
    for match in run_query(ast_grep, root, pattern="$X.last()"):
        if leaf_path_receiver(single_meta(match, "X")):
            source_ranges.append(node_range(match))
    for pattern in ("$X.rsplit($SEP).next()", "$X.split($SEP).last()"):
        for match in run_query(ast_grep, root, pattern=pattern):
            if separator_is_qualification(single_meta(match, "SEP")):
                source_ranges.append(node_range(match))
    for match in run_query(ast_grep, root, pattern="$X.module_alias"):
        source_ranges.append(node_range(match))

    bindings: list[tuple[str, SyntaxRange]] = []
    for pattern in (
        "let $V = $E;",
        "let mut $V = $E;",
        "let $V: $T = $E;",
        "let mut $V: $T = $E;",
        "if let Some($V) = $E { $$$BODY }",
        "let Some($V) = $E else { $$$BODY };",
        "$V = $E",
    ):
        for match in run_query(ast_grep, root, pattern=pattern):
            name = single_meta(match, "V")
            expression = single_meta_range(match, "E")
            if re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*", name) and expression:
                bindings.append((name, expression))

    sink_matches: list[tuple[str, dict[str, object], SyntaxRange]] = []
    identifier_index: defaultdict[str, list[tuple[int, str, SyntaxRange]]] = (
        defaultdict(list)
    )
    for identifier in identifiers:
        identifier_range = node_range(identifier)
        identifier_index[identifier_range.path].append(
            (identifier_range.byte_start, str(identifier["text"]), identifier_range)
        )
    for items in identifier_index.values():
        items.sort(key=lambda item: item[0])
    identifier_starts = {
        path: [item[0] for item in items] for path, items in identifier_index.items()
    }
    for method in ("insert", "entry", "get", "get_mut", "contains_key", "remove"):
        for pattern in (
            f"$R.{method}($KEY)",
            f"$R.{method}($KEY, $$$REST)",
        ):
            for match in run_query(ast_grep, root, pattern=pattern):
                if semantic_map_receiver(single_meta(match, "R")):
                    key = single_meta_range(match, "KEY")
                    if key:
                        sink_matches.append(("registry-key", match, key))
    for pattern in ("$F($KEY)", "$F($KEY, $$$REST)"):
        for match in run_query(ast_grep, root, pattern=pattern):
            callee = "".join(single_meta(match, "F").split())
            leaf = callee.split("::")[-1]
            if callee.endswith("DefId::legacy_reconstruct_from_full_path"):
                form = "def-id"
            elif callee.endswith("NominalId::legacy_reconstruct_from_full_path"):
                form = "nominal-id"
            elif "CallTarget::" in callee:
                form = "call-target"
            elif leaf in RUNTIME_RESOLUTION_SINKS:
                form = "runtime-resolution"
            elif leaf in SEMANTIC_KEY_BUILDERS:
                form = "registry-key"
            else:
                continue
            key = single_meta_range(match, "KEY")
            if key:
                sink_matches.append((form, match, key))
    # A shortening source can cross one helper boundary before the actual map
    # operation (`visit_items(..., module_short, ..., methods)`). Treat a call
    # that also carries an explicitly named semantic registry/map as the sink;
    # diagnostic/display helpers do not carry one of these authorities.
    for match in run_query(ast_grep, root, pattern="$F($$$ARGS)"):
        call_range = node_range(match)
        callee = "".join(single_meta(match, "F").split())
        callee_leaf = callee.split("::")[-1]
        if "." in callee or not callee_leaf.startswith(
            (
                "build_",
                "collect_",
                "lower_",
                "register_",
                "resolve_",
                "scan_",
                "seed_",
                "visit_",
            )
        ):
            continue
        path_identifiers = identifier_index.get(call_range.path, [])
        start = bisect.bisect_left(
            identifier_starts.get(call_range.path, []), call_range.byte_start
        )
        if any(
            range_contains(call_range, identifier_range) and semantic_map_receiver(name)
            for _, name, identifier_range in path_identifiers[start:]
            if identifier_range.byte_start < call_range.byte_end
        ):
            sink_matches.append(("registry-key", match, call_range))

    function_ranges = [
        node_range(match) for match in run_query(ast_grep, root, kind="function_item")
    ]
    # Include a file-wide fallback for const/static initializers and future
    # generated authorities outside functions.
    paths = {str(match["file"]) for match in identifiers}
    file_scopes = {path: SyntaxRange(path, 0, 1 << 62) for path in paths}
    functions_by_path: defaultdict[str, list[SyntaxRange]] = defaultdict(list)
    for item in function_ranges:
        functions_by_path[item.path].append(item)
    function_starts: dict[str, list[int]] = {}
    for path, items in functions_by_path.items():
        items.sort(key=lambda item: item.byte_start)
        function_starts[path] = [item.byte_start for item in items]

    def enclosing_scope(item: SyntaxRange) -> SyntaxRange | None:
        candidates = functions_by_path.get(item.path, [])
        index = bisect.bisect_right(function_starts.get(item.path, []), item.byte_start)
        # The latest-starting enclosing function is the narrowest scope. Rust
        # permits nested function items, so walk backwards until containment.
        for candidate in reversed(candidates[:index]):
            if range_contains(candidate, item):
                return candidate
        return file_scopes.get(item.path)

    sink_scopes: list[tuple[str, dict[str, object], SyntaxRange, SyntaxRange]] = []
    for form, sink_match, key_range in sink_matches:
        scope = enclosing_scope(key_range)
        if scope is not None:
            sink_scopes.append((form, sink_match, key_range, scope))

    sources_by_scope: defaultdict[SyntaxRange, list[SyntaxRange]] = defaultdict(list)
    for item in source_ranges:
        if scope := enclosing_scope(item):
            sources_by_scope[scope].append(item)
    identifiers_by_scope: defaultdict[SyntaxRange, list[tuple[str, SyntaxRange]]] = (
        defaultdict(list)
    )
    for match in identifiers:
        item_range = node_range(match)
        if scope := enclosing_scope(item_range):
            identifiers_by_scope[scope].append((str(match["text"]), item_range))
    literals_by_scope: defaultdict[SyntaxRange, list[tuple[str, SyntaxRange]]] = (
        defaultdict(list)
    )
    for match in literals:
        item_range = node_range(match)
        if scope := enclosing_scope(item_range):
            literals_by_scope[scope].append((str(match["text"]), item_range))
    bindings_by_scope: defaultdict[SyntaxRange, list[tuple[str, SyntaxRange]]] = (
        defaultdict(list)
    )
    for name, expression in bindings:
        if scope := enclosing_scope(expression):
            bindings_by_scope[scope].append((name, expression))

    scope_inputs: dict[
        SyntaxRange,
        tuple[
            list[SyntaxRange],
            list[tuple[str, SyntaxRange]],
            list[tuple[str, SyntaxRange]],
            dict[str, list[SyntaxRange]],
        ],
    ] = {}
    for scope in {item[3] for item in sink_scopes}:
        scoped_sources = sources_by_scope[scope]
        scoped_bindings = bindings_by_scope[scope]
        scoped_identifiers = identifiers_by_scope[scope]
        scoped_literals = literals_by_scope[scope]
        bindings_by_name: defaultdict[str, list[SyntaxRange]] = defaultdict(list)
        for name, expression in scoped_bindings:
            bindings_by_name[name].append(expression)
        for expressions in bindings_by_name.values():
            expressions.sort(key=lambda item: item.byte_end)
        scope_inputs[scope] = (
            scoped_sources,
            scoped_identifiers,
            scoped_literals,
            dict(bindings_by_name),
        )

    results: set[Finding] = set()
    for form, sink_match, key_range, scope in sink_scopes:
        sink = finding("semantic-owner-shortening-sink", form, sink_match)
        if excluded(sink, test_ranges):
            continue
        (
            scoped_sources,
            scoped_identifiers,
            scoped_literals,
            scoped_bindings,
        ) = scope_inputs[scope]

        def name_is_tainted(
            name: str, before: int, visiting: set[tuple[str, int]]
        ) -> bool:
            candidates = [
                expression
                for expression in scoped_bindings.get(name, [])
                if expression.byte_end <= before
            ]
            if not candidates:
                return False
            expression = candidates[-1]
            marker = (name, expression.byte_end)
            if marker in visiting:
                return False
            return expression_is_tainted(expression, visiting | {marker})

        def expression_is_tainted(
            expression: SyntaxRange, visiting: set[tuple[str, int]]
        ) -> bool:
            if (
                form in {"def-id", "call-target", "nominal-id"}
                and any(
                    CANONICAL_OWNER_NAMES.fullmatch(name)
                    and range_contains(expression, item_range)
                    for name, item_range in scoped_identifiers
                )
                or (
                    form in {"def-id", "call-target", "nominal-id"}
                    and any(
                        range_contains(expression, literal_range)
                        and any(
                            CANONICAL_OWNER_NAMES.fullmatch(name)
                            for name in implicit_format_names(text)
                        )
                        for text, literal_range in scoped_literals
                    )
                )
            ):
                # Re-attaching an item leaf to a checker-resolved full owner is
                # canonicalization, not owner shortening. The full owner is the
                # authority consumed by the resulting structured ID.
                return False
            if any(range_contains(expression, item) for item in scoped_sources):
                return True
            for name, item_range in scoped_identifiers:
                if range_contains(expression, item_range) and name_is_tainted(
                    name, item_range.byte_start, visiting
                ):
                    return True
            for text, literal_range in scoped_literals:
                if not range_contains(expression, literal_range):
                    continue
                if any(
                    name_is_tainted(name, literal_range.byte_start, visiting)
                    for name in implicit_format_names(text)
                ):
                    return True
            return False

        if expression_is_tainted(key_range, set()):
            results.add(sink)
    return results


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


def contained_nodes(
    ast_grep: Path, root: Path, *, enum_name: str, node_kind: str
) -> list[dict[str, object]]:
    """Return parsed nodes physically declared by one named enum.

    ast-grep's JSON stream exposes byte ranges for every syntax node.  Joining
    those ranges lets this audit count enum variants without source-text
    searches and without accidentally treating a same-named variant elsewhere
    as part of the authority carrier.
    """
    enum_ranges = [
        node_range(match)
        for match in run_query(
            ast_grep, root, pattern=f"enum {enum_name} {{ $$$BODY }}"
        )
    ]
    enum_ranges.extend(
        node_range(match)
        for match in run_query(
            ast_grep, root, pattern=f"pub enum {enum_name} {{ $$$BODY }}"
        )
    )
    return [
        match
        for match in run_query(ast_grep, root, kind=node_kind)
        if any(
            range_contains(enum_range, node_range(match)) for enum_range in enum_ranges
        )
    ]


def enum_variant_name(match: dict[str, object]) -> str:
    """Extract the identifier prefix of an already-parsed enum-variant node."""
    return str(match["text"]).lstrip().split("{", 1)[0].split("(", 1)[0].strip()


def rc1_structural_authority_findings(
    ast_grep: Path, root: Path, test_ranges: list[SyntaxRange]
) -> set[Finding]:
    """Enumerate RC1 cross-phase/lifecycle authority syntax nodes.

    This is intentionally a structural inventory, not a heuristic semantic
    analysis.  Each marker is an executable Rust node and the inventory is
    exact by `(group, form, path)`, so additions (including a new enum variant
    or writer) cannot silently bypass review.
    """
    results: set[Finding] = set()

    # The checker owns these TypeCheckOutput facts/relations; HIR reads are the
    # publication boundary.  Count all declared fields so adding a new
    # checker-produced fact fails even if its first consumer has not landed.
    output_ranges = [
        node_range(match)
        for match in run_query(
            ast_grep, root, pattern="pub struct TypeCheckOutput { $$$BODY }"
        )
    ]
    output_fields = {
        str(match["text"]).split(":", 1)[0].removeprefix("pub ").strip()
        for match in run_query(ast_grep, root, kind="field_declaration")
        if any(range_contains(item, node_range(match)) for item in output_ranges)
    }
    for match in run_query(ast_grep, root, kind="field_declaration"):
        if any(range_contains(item, node_range(match)) for item in output_ranges):
            results.add(
                finding("checker-hir-fact-relation", "produced-fact-field", match)
            )
    for match in run_query(ast_grep, root, kind="field_identifier"):
        item = finding("checker-hir-fact-relation", "hir-publication-consumer", match)
        if item.path.startswith("hew-hir/") and str(match["text"]) in output_fields:
            results.add(item)

    def closed_enum(enum_name: str, group: str, form_prefix: str) -> set[str]:
        """Inventory one real closed carrier by variants and executable uses."""
        variants = contained_nodes(
            ast_grep, root, enum_name=enum_name, node_kind="enum_variant"
        )
        names = {enum_variant_name(match) for match in variants}
        for match in variants:
            results.add(finding(group, f"{form_prefix}-variant", match))
        # Rust parses tuple/unit variant paths as `scoped_identifier`, while
        # struct-shaped constructors/patterns use `scoped_type_identifier`.
        # Both are executable uses of the same closed carrier.
        for node_kind in ("scoped_identifier", "scoped_type_identifier"):
            for match in run_query(ast_grep, root, kind=node_kind):
                text = str(match["text"]).strip()
                if any(text == f"{enum_name}::{variant}" for variant in names):
                    results.add(finding(group, f"{form_prefix}-use", match))
        for match in run_query(ast_grep, root, kind="match_pattern"):
            text = str(match["text"]).lstrip()
            if any(text.startswith(f"{enum_name}::{variant}") for variant in names):
                results.add(finding(group, f"{form_prefix}-consumer", match))
        return names

    # These are the real checker and HIR ownership-relation carriers. Their
    # closed variant sets must remain congruent, and every explicit producer or
    # consumer is inventoried. This replaces the provisional `ProducedValue`
    # assumption used before the composition landed.
    checker_relations = closed_enum(
        "ProducedValueDependency", "checker-hir-fact-relation", "checker-relation"
    )
    hir_relations = closed_enum(
        "HirProducedValueRelation", "checker-hir-fact-relation", "hir-relation"
    )
    if checker_relations != hir_relations:
        raise SystemExit(
            "checker/HIR produced-value relation variants diverged: "
            f"checker={sorted(checker_relations)}, HIR={sorted(hir_relations)}"
        )

    # Exact executable identity is a two-level closed carrier: `CallTarget`
    # selects the edge class and `RuntimeCallFamily` selects compiler/runtime
    # semantics without parsing a linker label.
    closed_enum("CallTarget", "call-target-authority", "call-target")
    closed_enum("RuntimeCallFamily", "runtime-call-authority", "runtime-call-family")

    # The checker-owned lifecycle graph is not yet consumed by HIR, but its
    # candidate shape and conflict discriminator are already real authority.
    # Inventorying declarations now makes the later HIR cutover an explicit
    # reviewed addition rather than a new provisional carrier.
    lifecycle_ranges = [
        node_range(match)
        for match in run_query(
            ast_grep,
            root,
            pattern="pub struct OpaqueResourceLifecycleCandidate { $$$BODY }",
        )
    ]
    for match in run_query(ast_grep, root, kind="field_declaration"):
        if any(range_contains(item, node_range(match)) for item in lifecycle_ranges):
            results.add(
                finding(
                    "lifecycle-identity-authority", "lifecycle-candidate-field", match
                )
            )
    closed_enum(
        "OpaqueResourceLifecycleConflictKind",
        "lifecycle-identity-authority",
        "lifecycle-conflict",
    )

    # SuspendKind has a real side-table carrier.  Count declaration variants,
    # every executable path use (producer or consumer), and the canonical
    # side-table writer separately.  New variants and unpaired writers thus
    # cannot hide behind an existing broad match arm.
    suspend_variants = contained_nodes(
        ast_grep, root, enum_name="SuspendKind", node_kind="enum_variant"
    )
    for match in suspend_variants:
        results.add(finding("suspend-authority", "suspend-kind-variant", match))
    for variant in {enum_variant_name(match) for match in suspend_variants}:
        for match in run_query(
            ast_grep, root, pattern=f"SuspendKind::{variant} {{ $$$FIELDS }}"
        ):
            results.add(finding("suspend-authority", "suspend-kind-use", match))
        for match in run_query(ast_grep, root, kind="match_pattern"):
            if str(match["text"]).lstrip().startswith(f"SuspendKind::{variant}"):
                results.add(
                    finding("suspend-authority", "suspend-kind-consumer", match)
                )
    for match in run_query(ast_grep, root, pattern="$R.record_suspend_kind($K)"):
        results.add(finding("suspend-authority", "suspend-kind-writer", match))

    # Terminator variants serve the same lifecycle boundary.  The enum-node
    # join catches a newly declared suspending terminator; path uses catch all
    # result/source handling sites; finish_current_block is the source writer.
    terminator_variants = contained_nodes(
        ast_grep, root, enum_name="Terminator", node_kind="enum_variant"
    )
    suspending_terminator_names = set()
    for match in terminator_variants:
        name = enum_variant_name(match)
        if name == "Suspend" or name.startswith("Suspending"):
            suspending_terminator_names.add(name)
            results.add(
                finding("suspend-authority", "suspending-terminator-variant", match)
            )
    for variant in suspending_terminator_names:
        for match in run_query(
            ast_grep, root, pattern=f"Terminator::{variant} {{ $$$FIELDS }}"
        ):
            results.add(
                finding("suspend-authority", "suspending-terminator-use", match)
            )
        for match in run_query(ast_grep, root, kind="match_pattern"):
            if str(match["text"]).lstrip().startswith(f"Terminator::{variant}"):
                results.add(
                    finding(
                        "suspend-authority", "suspending-terminator-consumer", match
                    )
                )
        for match in run_query(
            ast_grep,
            root,
            pattern=f"$R.finish_current_block(Terminator::{variant} {{ $$$FIELDS }})",
        ):
            results.add(
                finding("suspend-authority", "suspending-terminator-writer", match)
            )

    # Owner retirement has three independent exit authorities.  These precise
    # executable markers avoid comment-driven matches while retaining Join,
    # abandon, and crash cleanup as separately reviewable classes.
    for match in run_query(ast_grep, root, pattern="Terminator::Join { $$$FIELDS }"):
        results.add(finding("owner-retirement-path", "join-owner-path", match))
    for match in run_query(ast_grep, root, kind="match_pattern"):
        if str(match["text"]).lstrip().startswith("Terminator::Join"):
            results.add(finding("owner-retirement-path", "join-owner-consumer", match))
    for kind in ("identifier", "field_identifier"):
        for match in run_query(ast_grep, root, kind=kind):
            if str(match["text"]) == "suspend_abandon_extra_drops":
                results.add(
                    finding("owner-retirement-path", "abandonment-owner-path", match)
                )
    for match in run_query(ast_grep, root, pattern="ActorHandlerKind::Crash"):
        results.add(finding("owner-retirement-path", "crash-cleanup-owner-path", match))

    return {item for item in results if not excluded(item, test_ranges)}


def reject_raw_codegen_call_dispatch(ast_grep: Path, root: Path) -> None:
    """Forbid symbol spelling from granting a `lower_terminator` intercept.

    The two allowed comparisons are the carrier's exact linkage assertions.
    All specialised lowering must branch on `CallAuthority`/its runtime family;
    ordinary `Direct` calls retain their open-set linkage semantics.
    """
    ranges = [
        node_range(match)
        for match in run_query(
            ast_grep, root, pattern="fn lower_terminator($$$ARGS) { $$$BODY }"
        )
    ]
    # Counterfactual audit fixtures intentionally contain only the syntax
    # under test; this production-only guard has no target there.
    if not ranges:
        return
    if len(ranges) != 1:
        raise SystemExit(
            "expected exactly one codegen lower_terminator authority boundary"
        )

    bad: list[SyntaxRange] = []
    for pattern, allowed_left in (
        ("$LEFT == callee", {"family.c_symbol()", "kind.expected_callee()"}),
        ("$LEFT != callee", {"family.c_symbol()", "kind.expected_callee()"}),
    ):
        for match in run_query(ast_grep, root, pattern=pattern):
            if not range_contains(ranges[0], node_range(match)):
                continue
            if single_meta(match, "LEFT") not in allowed_left:
                bad.append(node_range(match))
    for pattern in (
        "callee == $RIGHT",
        "callee != $RIGHT",
        "matches!(callee, $$$PATTERN)",
        "callee.as_str()",
    ):
        for match in run_query(ast_grep, root, pattern=pattern):
            if range_contains(ranges[0], node_range(match)):
                bad.append(node_range(match))
    if bad:
        sites = ", ".join(f"{item.path}:{item.line}:{item.column}" for item in bad)
        raise SystemExit(
            "raw callee spelling dispatch is forbidden in lower_terminator; "
            f"carry CallAuthority instead ({sites})"
        )


def discover(ast_grep: Path, root: Path) -> tuple[set[Finding], list[SyntaxRange]]:
    test_ranges = test_only_ranges(ast_grep, root)
    findings: set[Finding] = set()
    generated_enum_leaves = generated_builtin_enum_leaves(root)

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

    for pattern in (
        "Ty::Named { name: $NAME, $$$FIELDS }",
        "ResolvedTy::Named { name: $NAME, $$$FIELDS }",
        "hew_types::ResolvedTy::Named { name: $NAME, $$$FIELDS }",
    ):
        for match in run_query(ast_grep, root, pattern=pattern):
            literal = re.search(r'"([^"\\]+)"', single_meta(match, "NAME"))
            if literal is not None and literal.group(1) in generated_enum_leaves:
                findings.add(
                    finding(
                        "monomorphic-enum-leaf-synthesis",
                        "leaf-named-semantic-struct-literal",
                        match,
                    )
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
        macro_path = "".join(single_meta(match, "M").split())
        if macro_path.split("::")[-1] != "format":
            continue
        macro_range = node_range(match)
        starts, literals = literal_indexes.get(macro_range.path, ([], []))
        literal_index = bisect.bisect_left(starts, macro_range.byte_start)
        first = literals[literal_index] if literal_index < len(literals) else None
        if first is not None and first[1] <= macro_range.byte_end and "::" in first[2]:
            findings.add(
                finding("string-method-identity", "qualified-format-macro", match)
            )

    findings.update(semantic_owner_shortening_findings(ast_grep, root, test_ranges))
    reject_raw_codegen_call_dispatch(ast_grep, root)
    findings.update(rc1_structural_authority_findings(ast_grep, root, test_ranges))
    return {item for item in findings if not excluded(item, test_ranges)}, test_ranges


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


def forbidden_context_free_nominal_findings(
    ast_grep: Path, root: Path, test_ranges: list[SyntaxRange]
) -> list[Finding]:
    """Reject checker/codegen bypasses of the owner-aware nominal authority."""
    result = []
    for match in run_query(ast_grep, root, pattern="$F($$$ARGS)"):
        callee = single_meta(match, "F")
        path = str(match["file"])
        leaf = callee.rsplit("::", 1)[-1]
        item = finding("forbidden-context-free-nominal", leaf, match)
        if excluded(item, test_ranges):
            continue
        if leaf == "names_match_qualified":
            if path not in {
                "hew-types/src/unify.rs",
                "hew-types/src/check/resolution.rs",
            } and not path.startswith("hew-analysis/src/"):
                result.append(item)
        elif leaf == "unify":
            if path not in {
                "hew-types/src/unify.rs",
                "hew-types/src/check/coerce.rs",
            }:
                result.append(item)
    return sorted(set(result))


def canonical_stage(group: str, form: str, path: str) -> str:
    """Return the stage at which the plan can actually retire this seam."""
    if group == "semantic-owner-shortening-sink" and form in {
        "registry-key",
        "def-id",
        "nominal-id",
        "call-target",
        "runtime-resolution",
    }:
        if path.startswith(("hew-types/", "hew-hir/", "hew-analysis/")):
            return "stage-1"
        if path.startswith("hew-codegen-rs/"):
            return "stage-5"
        if path.endswith(("lower/drop_plan.rs", "lower/mod.rs")):
            return "stage-3"
        return "stage-4"
    if group == "checker-hir-publication":
        return "stage-2"
    if group == "checker-hir-fact-relation":
        return "stage-2"
    if group == "call-target-authority":
        return "stage-4"
    if group == "runtime-call-authority":
        return "stage-4"
    if group == "lifecycle-identity-authority":
        return "stage-2"
    if group in {"suspend-authority", "owner-retirement-path"}:
        return "stage-3"
    if group == "monomorphic-enum-leaf-synthesis":
        return "stage-3" if path.startswith("hew-mir/") else "stage-2"
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


def load_inventory_reasons(path: Path) -> dict[tuple[str, str, str], str]:
    """Reasons keyed by inventory row, so a regen never invents editorial text."""
    reasons: dict[tuple[str, str, str], str] = {}
    with path.open(newline="") as handle:
        source = (line for line in handle if line.strip() and not line.startswith("#"))
        for row in csv.DictReader(source, delimiter="\t"):
            reasons[(row["group"], row["form"], row["path"])] = row["reason"]
    return reasons


def render_inventory(
    path: Path,
    counts: dict[tuple[str, str, str], int],
    reasons: dict[tuple[str, str, str], str],
) -> str:
    """Rebuild the inventory from observed counts, preserving the prologue.

    Counts are derived, so they are rewritten. Reasons are editorial, so an
    authority form/path that has never been reviewed cannot be minted here --
    `write_inventory` refuses instead of authoring a placeholder.
    """
    lines = path.read_text().splitlines()
    prologue = [line for line in lines if line.startswith("#")]
    header = "group\tform\tpath\tcount\tretirement_stage\treason"
    rows = [
        "\t".join(
            (
                group,
                form,
                target,
                str(counts[(group, form, target)]),
                canonical_stage(group, form, target),
                reasons[(group, form, target)],
            )
        )
        for (group, form, target) in counts
    ]
    rows.sort()
    return "\n".join([*prologue, header, *rows]) + "\n"


def write_inventory(
    path: Path,
    counts: dict[tuple[str, str, str], int],
) -> int:
    """Re-record the inventory counts. A brand-new authority row is an error.

    Dropping to zero and shrinking are the drift this regen exists to absorb:
    they mean an authority was retired, which is the direction the cutover is
    supposed to move. A key with no prior row is the opposite -- new authority
    landed -- and it needs a human reason before it enters the baseline.
    """
    reasons = load_inventory_reasons(path)
    unreviewed = sorted(key for key in counts if key not in reasons)
    if unreviewed:
        print(
            "structural authority inventory: new authority form/path rows cannot be "
            "auto-recorded; add each row with its reason:",
            file=sys.stderr,
        )
        for group, form, target in unreviewed:
            print(
                f"  - {group}\t{form}\t{target}\t{counts[(group, form, target)]}"
                f"\t{canonical_stage(group, form, target)}\t<reason>",
                file=sys.stderr,
            )
        return 1
    path.write_text(render_inventory(path, counts, reasons))
    print(
        f"structural authority inventory: re-recorded {len(counts)} authority "
        "form/path rows"
    )
    return 0


@dataclass(frozen=True, order=True)
class OpaqueResourceFact:
    """AST-derived, qualified lifecycle fact for one shipped empty handle.

    `carrier_key` is a qualified display key for joining source facts to test
    evidence. Compiler authority remains the foundation's DefId carriers; this
    audit neither constructs nor authorizes compiler identity from the display.
    """

    carrier_key: str
    module: str
    resource: str
    source_path: str
    close_body_range: tuple[int, int]
    release_symbol: str
    producer_symbols: tuple[str, ...]


def _match_path(match: dict[str, object]) -> str:
    return str(match["file"])


def _match_offsets(match: dict[str, object]) -> tuple[int, int]:
    offsets = match["range"]["byteOffset"]  # type: ignore[index]
    return int(offsets["start"]), int(offsets["end"])


def _contained(outer: dict[str, object], inner: dict[str, object]) -> bool:
    if _match_path(outer) != _match_path(inner):
        return False
    outer_start, outer_end = _match_offsets(outer)
    inner_start, inner_end = _match_offsets(inner)
    return outer_start <= inner_start and inner_end <= outer_end


def _module_for_std_path(path: str) -> str:
    relative = Path(path).relative_to("std")
    parts = ["std", *relative.parent.parts]
    stem = relative.stem
    if parts[-1] != stem:
        parts.append(stem)
    return ".".join(parts)


def _meta_many(match: dict[str, object], name: str) -> list[str]:
    meta = match.get("metaVariables", {})
    values = meta.get("multi", {}).get(name, [])  # type: ignore[union-attr]
    return [str(value.get("text", "")) for value in values if isinstance(value, dict)]


def discover_opaque_resource_facts(
    ast_grep: Path, root: Path
) -> list[OpaqueResourceFact]:
    """Join Hew AST siblings/ranges into source lifecycle facts.

    There is deliberately no resource-name table here.  Declarations, their
    adjacent marker siblings, inherent consuming close bodies, direct release
    calls, and source extern producer/release signatures are independently
    parsed before being joined by source range and nominal identity.
    """
    declarations = run_hew_query(ast_grep, root, pattern="type $NAME { }")
    attributes = run_hew_query(ast_grep, root, kind="attribute")
    impls = run_hew_query(ast_grep, root, pattern="impl $TYPE { $$$BODY }")
    closes = run_hew_query(
        ast_grep, root, pattern="fn close(consuming self) { $$$BODY }"
    )
    calls = run_hew_query(ast_grep, root, pattern="$F($$$ARGS)")
    extern_blocks = run_hew_query(ast_grep, root, pattern='extern "C" { $$$BODY }')
    parser_error_nodes = run_hew_query(ast_grep, root, kind="ERROR")
    if parser_error_nodes:
        paths = sorted({_match_path(node) for node in parser_error_nodes})
        raise SystemExit(
            "pinned Hew grammar must parse the shipped std corpus without ERROR nodes: "
            + ", ".join(paths)
        )
    extern_functions = run_hew_query(
        ast_grep, root, pattern="fn $NAME($$$ARGS) -> $RET;"
    )
    consuming_extern_functions = run_hew_query(
        ast_grep, root, pattern="fn $NAME(consume $PARAM: $TYPE) -> $RET;"
    )
    consuming_extern_functions.extend(
        run_hew_query(ast_grep, root, pattern="fn $NAME(consume $PARAM: $TYPE);")
    )
    facts: list[OpaqueResourceFact] = []
    for declaration in declarations:
        path = _match_path(declaration)
        start, _ = _match_offsets(declaration)
        # Attribute nodes are siblings of the declaration in the program item;
        # constrain their adjacency with byte ranges so a distant marker cannot
        # authorize a different type.
        sibling_attrs = [
            attr
            for attr in attributes
            if _match_path(attr) == path
            and start - _match_offsets(attr)[1] < 96
            and _match_offsets(attr)[1] <= start
        ]
        marker_texts = {str(attr["text"]).strip() for attr in sibling_attrs}
        if marker_texts.isdisjoint({"#[resource]"}) or marker_texts.isdisjoint(
            {"#[opaque]"}
        ):
            continue
        resource = single_meta(declaration, "NAME")
        if not resource:
            raise SystemExit(f"opaque resource declaration has no AST name at {path}")
        module = _module_for_std_path(path)
        inherent_impls = [
            implementation
            for implementation in impls
            if _match_path(implementation) == path
            and single_meta(implementation, "TYPE") == resource
        ]
        close_matches = [
            close
            for close in closes
            if any(
                _contained(implementation, close) for implementation in inherent_impls
            )
        ]
        if len(close_matches) != 1:
            raise SystemExit(
                f"{module}.{resource}: expected exactly one inherent consuming close body, found {len(close_matches)}"
            )
        close = close_matches[0]
        close_calls = [call for call in calls if _contained(close, call)]
        release_calls = [
            call
            for call in close_calls
            if "self" in _meta_many(call, "ARGS") and single_meta(call, "F")
        ]
        if len(release_calls) != 1:
            raise SystemExit(
                f"{module}.{resource}: consuming close must have exactly one direct self release call, found {len(release_calls)}"
            )
        release_symbol = single_meta(release_calls[0], "F")
        module_externs = [
            block for block in extern_blocks if _match_path(block) == path
        ]
        release_declarations = [
            function
            for function in consuming_extern_functions
            if any(_contained(block, function) for block in module_externs)
            and single_meta(function, "NAME") == release_symbol
            and single_meta(function, "TYPE") == resource
        ]
        if len(release_declarations) != 1:
            raise SystemExit(
                f"{module}.{resource}: close release {release_symbol!r} lacks one consuming extern declaration"
            )
        producers = sorted(
            {
                single_meta(function, "NAME")
                for function in extern_functions
                if any(_contained(block, function) for block in module_externs)
                and single_meta(function, "RET") == resource
                and single_meta(function, "NAME") != release_symbol
            }
        )
        if not producers:
            raise SystemExit(
                f"{module}.{resource}: no source extern producer returns the handle"
            )
        facts.append(
            OpaqueResourceFact(
                carrier_key=f"{module}.{resource}",
                module=module,
                resource=resource,
                source_path=path,
                close_body_range=_match_offsets(close),
                release_symbol=release_symbol,
                producer_symbols=tuple(producers),
            )
        )
    keys = [fact.carrier_key for fact in facts]
    if not keys:
        raise SystemExit("opaque resource source discovery corpus is empty")
    if len(keys) != len(set(keys)):
        raise SystemExit(
            "opaque resource source discovery found duplicate qualified candidates"
        )
    return sorted(facts)


def opaque_fact_json(facts: list[OpaqueResourceFact]) -> dict[str, object]:
    return {
        "schema_version": 1,
        "display_key": "qualified source/evidence presentation only; compiler identity is DefId-authoritative",
        "candidates": [
            {
                "carrier_key": fact.carrier_key,
                "module": fact.module,
                "resource": fact.resource,
                "source_path": fact.source_path,
                "close_body_range": list(fact.close_body_range),
                "release_symbol": fact.release_symbol,
                "producer_symbols": list(fact.producer_symbols),
            }
            for fact in facts
        ],
        "compiler_e2e_cases": [
            {
                "carrier_key": fact.carrier_key,
                "release_symbol": fact.release_symbol,
                "close_symbol": f"{fact.carrier_key}::close",
                "scope_exit_source": (
                    f"import {fact.module}.{{ {fact.resource} }};\n"
                    f"fn scope_exit_case(consume value: {fact.resource}) {{ }}\n"
                ),
                "explicit_close_source": (
                    f"import {fact.module}.{{ {fact.resource} }};\n"
                    f"fn explicit_close_case(consume value: {fact.resource}) {{ value.close(); }}\n"
                ),
            }
            for fact in facts
        ],
    }


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--root", type=Path, default=Path(__file__).resolve().parents[1]
    )
    parser.add_argument("--inventory", type=Path)
    parser.add_argument("--ast-grep", type=Path)
    parser.add_argument(
        "--write-inventory",
        action="store_true",
        help="re-record inventory counts from the current tree (a baselines regen)",
    )
    parser.add_argument(
        "--opaque-resource-facts",
        type=Path,
        help="write AST-derived shipped opaque resource facts (use - for stdout)",
    )
    parser.add_argument(
        "--opaque-resource-facts-only",
        action="store_true",
        help="stop after the AST-derived lifecycle artifact is written",
    )
    args = parser.parse_args()
    root = args.root.resolve()
    repo_root = Path(__file__).resolve().parents[1]
    ast_grep = (args.ast_grep or repo_root / ".ast-grep/tool/bin/ast-grep").resolve()
    if not ast_grep.is_file():
        raise SystemExit(f"pinned ast-grep is absent: {ast_grep}")
    parser_sentinel(ast_grep)
    opaque_facts: list[OpaqueResourceFact] = []
    if (root / "std").is_dir():
        hew_parser_sentinel(ast_grep, root)
        opaque_facts = discover_opaque_resource_facts(ast_grep, root)
        if args.opaque_resource_facts:
            rendered = (
                json.dumps(opaque_fact_json(opaque_facts), indent=2, sort_keys=True)
                + "\n"
            )
            if str(args.opaque_resource_facts) == "-":
                print(rendered, end="")
            else:
                args.opaque_resource_facts.write_text(rendered)
        if args.opaque_resource_facts_only:
            return 0

    inventory = args.inventory or root / "scripts/structural-authority-inventory.tsv"
    expected = {} if args.write_inventory else load_inventory(inventory)
    findings, test_ranges = discover(ast_grep, root)

    actual: defaultdict[tuple[str, str, str], int] = defaultdict(int)
    for item in findings:
        actual[(item.group, item.form, item.path)] += 1

    if args.write_inventory:
        # A forbidden authority is not drift and is never re-recorded; it stops
        # the regen exactly as it stops the audit.
        blocked = [
            f"forbidden scalar SpanKey -> SiteId authority at "
            f"{item.path}:{item.line}:{item.column}: {item.text}"
            for item in scalar_span_site_findings(ast_grep, root, test_ranges)
        ] + [
            f"forbidden context-free nominal authority at "
            f"{item.path}:{item.line}:{item.column}: {item.text}"
            for item in forbidden_context_free_nominal_findings(
                ast_grep, root, test_ranges
            )
        ]
        if blocked:
            print("\n".join(f"  - {item}" for item in blocked), file=sys.stderr)
            return 1
        return write_inventory(inventory, dict(actual))

    failures = []
    for key in sorted(set(expected) | set(actual)):
        want, got = expected.get(key, 0), actual.get(key, 0)
        if want != got:
            failures.append(f"{key[0]}/{key[1]} {key[2]}: expected {want}, found {got}")
    forbidden = scalar_span_site_findings(ast_grep, root, test_ranges)
    for item in forbidden:
        failures.append(
            f"forbidden scalar SpanKey -> SiteId authority at "
            f"{item.path}:{item.line}:{item.column}: {item.text}"
        )
    for item in forbidden_context_free_nominal_findings(ast_grep, root, test_ranges):
        failures.append(
            f"forbidden context-free nominal authority at "
            f"{item.path}:{item.line}:{item.column}: {item.text}"
        )
    if failures:
        print(
            "structural authority inventory changed; review every explicit form/path target:",
            file=sys.stderr,
        )
        print("\n".join(f"  - {item}" for item in failures), file=sys.stderr)
        return 1

    print(
        "structural authority inventory: "
        f"{len(expected)} authority form/path rows; "
        f"{len(test_ranges)} parsed test-only item ranges; "
        f"{len(opaque_facts)} AST-derived opaque resource lifecycle candidates; "
        "0 scalar SpanKey -> SiteId authorities"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
