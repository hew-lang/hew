#!/usr/bin/env python3
"""Generate and verify the census-only C ABI manifest.

The manifest deliberately records source facts without supplying ownership
contracts. Its rows are an authority boundary: consumers that mint ownership
behaviour must reject every row until a later measured slice changes it.
"""

from __future__ import annotations

import argparse
import importlib.util
import json
import re
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Any

ROOT = Path(__file__).resolve().parent.parent
MANIFEST = ROOT / "scripts" / "cabi-surface.json"
HEADER = ROOT / "hew-cabi" / "include" / "hew_cabi_surface.h"
TARGETS = ("native", "wasm32-wasip1")
SOURCE_ENCODING = "utf-8"


def parse_args(argv: list[str] | None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument(
        "--write", action="store_true", help="regenerate checked-in outputs"
    )
    mode.add_argument(
        "--check", action="store_true", help="verify source and generated outputs"
    )
    mode.add_argument(
        "--validate", action="store_true", help="verify one manifest against source"
    )
    parser.add_argument("--manifest", type=Path, default=MANIFEST)
    parser.add_argument("--header", type=Path, default=HEADER)
    return parser.parse_args(argv)


def load_ffi_verifier() -> Any:
    path = ROOT / "scripts" / "verify-ffi-symbols.py"
    spec = importlib.util.spec_from_file_location("hew_verify_ffi_symbols", path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot load {path}")
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def line_at(text: str, offset: int) -> int:
    return text.count("\n", 0, offset) + 1


def balanced_end(text: str, open_offset: int, opener: str, closer: str) -> int:
    depth = 0
    for index in range(open_offset, len(text)):
        if text[index] == opener:
            depth += 1
        elif text[index] == closer:
            depth -= 1
            if depth == 0:
                return index
    raise ValueError("unbalanced source declaration")


def split_arguments(value: str) -> list[str]:
    arguments: list[str] = []
    depth = 0
    start = 0
    for index, char in enumerate(value):
        if char in "(<[":
            depth += 1
        elif char in ")>]":
            depth -= 1
        elif char == "," and depth == 0:
            arguments.append(value[start:index].strip())
            start = index + 1
    if value[start:].strip():
        arguments.append(value[start:].strip())
    return arguments


def normalized_signature(text: str, fn_offset: int) -> tuple[str, str]:
    open_paren = text.find("(", fn_offset)
    close_paren = balanced_end(text, open_paren, "(", ")")
    body_end = text.find("{", close_paren)
    semi_end = text.find(";", close_paren)
    end = min(point for point in (body_end, semi_end) if point != -1)
    signature = re.sub(r"\s+", " ", text[fn_offset:end]).strip()
    raw_signature = signature
    signature = re.sub(r"([,(])\s*(?:mut\s+)?[A-Za-z_]\w*\s*:\s*", r"\1 ", signature)
    signature = re.sub(r"(?:crate|super)(?:::\w+)*::", "", signature)
    signature = signature.replace("std::ffi::", "")
    return signature, raw_signature


def cfg_arguments(expression: str) -> list[str]:
    open_paren = expression.find("(")
    if open_paren == -1:
        return []
    close_paren = balanced_end(expression, open_paren, "(", ")")
    return split_arguments(expression[open_paren + 1 : close_paren])


def evaluate_cfg(expression: str, wasm: bool) -> bool:
    expression = expression.strip()
    if expression.startswith("cfg(") and expression.endswith(")"):
        expression = expression[4:-1].strip()
    if expression.startswith("not(") and expression.endswith(")"):
        return not evaluate_cfg(cfg_arguments(expression)[0], wasm)
    if expression.startswith("all(") and expression.endswith(")"):
        return all(
            evaluate_cfg(argument, wasm) for argument in cfg_arguments(expression)
        )
    if expression.startswith("any(") and expression.endswith(")"):
        return any(
            evaluate_cfg(argument, wasm) for argument in cfg_arguments(expression)
        )
    if expression == "test":
        return False
    if expression in {"unix", 'target_os = "macos"'}:
        return not wasm
    if expression == "windows":
        return False
    if expression in {
        'target_os = "wasi"',
        'target_arch = "wasm32"',
        'target_family = "wasm"',
    }:
        return wasm
    if expression.startswith("target_"):
        return not wasm
    # Feature and compiler-version cfgs are not target selectors. Their
    # declaration remains part of the default exported surface for this census.
    return True


def local_cfg_expressions(text: str, declaration_offset: int) -> list[str]:
    expressions: list[str] = []
    lines = text[:declaration_offset].splitlines()
    for line in reversed(lines):
        stripped = line.strip()
        if not stripped or stripped.startswith("//") or stripped.startswith("///"):
            continue
        match = re.fullmatch(r"#\[cfg\((.*)\)\]", stripped)
        if match is not None:
            expressions.append(match.group(1))
            continue
        if stripped.startswith("#["):
            continue
        break
    return expressions


def enclosing_cfg_expressions(text: str, declaration_offset: int) -> list[str]:
    expressions: list[str] = []
    pattern = re.compile(
        r"(?P<attrs>(?:\s*#\[[^\]]+\])+)"
        r"\s*(?:pub(?:\([^)]*\))?\s+)?mod\s+\w+\s*\{",
        re.DOTALL,
    )
    for match in pattern.finditer(text):
        block_expressions = re.findall(r"#\[cfg\((.*)\)\]", match.group("attrs"))
        if not any(
            all(evaluate_cfg(expression, wasm) for expression in block_expressions)
            for wasm in (False, True)
        ):
            continue
        block_open = match.end() - 1
        block_end = balanced_end(text, block_open, "{", "}")
        if block_open < declaration_offset < block_end:
            expressions.extend(block_expressions)
    return expressions


def module_cfg_expressions(path: Path) -> list[str]:
    for root_name in ("hew-runtime", "hew-std"):
        source_root = ROOT / root_name / "src"
        try:
            relative = path.relative_to(source_root)
        except ValueError:
            continue
        parts = list(relative.with_suffix("").parts)
        if parts == ["lib"]:
            return []
        parent = source_root / "lib.rs"
        expressions: list[str] = []
        for part in parts:
            source = parent.read_text(encoding=SOURCE_ENCODING)
            pattern = re.compile(
                r"(?P<attrs>(?:\s*#\[[^\]]+\])*)"
                r"\s*(?:pub(?:\([^)]*\))?\s+)?mod\s+" + r"(?P<name>\w+)\s*(?:;|\{)",
                re.DOTALL,
            )
            matches = list(pattern.finditer(source))
            match = next(
                (
                    candidate
                    for candidate in matches
                    if candidate.group("name") == part
                    or Path(
                        (
                            path_match.group(1)
                            if (
                                path_match := re.search(
                                    r'#\[path\s*=\s*"([^"]+)"\]',
                                    candidate.group("attrs"),
                                )
                            )
                            else candidate.group("name")
                        )
                    ).stem
                    == part
                ),
                None,
            )
            if match is None:
                break
            expressions.extend(re.findall(r"#\[cfg\((.*)\)\]", match.group("attrs")))
            path_match = re.search(r'#\[path\s*=\s*"([^"]+)"\]', match.group("attrs"))
            module_name = match.group("name")
            file_candidate = (
                parent.parent / path_match.group(1)
                if path_match is not None
                else parent.parent / f"{module_name}.rs"
            )
            mod_candidate = parent.parent / module_name / "mod.rs"
            if file_candidate.exists():
                parent = file_candidate
            elif mod_candidate.exists():
                parent = mod_candidate
            else:
                break
        return expressions
    return []


def declaration_targets(path: Path, text: str, declaration_offset: int) -> list[str]:
    expressions = (
        local_cfg_expressions(text, declaration_offset)
        + enclosing_cfg_expressions(text, declaration_offset)
        + module_cfg_expressions(path)
    )
    return [
        target
        for target, wasm in (("native", False), ("wasm32-wasip1", True))
        if all(evaluate_cfg(expression, wasm) for expression in expressions)
    ]


def source_files() -> list[tuple[Path, str]]:
    files: list[tuple[Path, str]] = []
    for source_root in (ROOT / "hew-runtime" / "src", ROOT / "hew-std" / "src"):
        for path in sorted(source_root.rglob("*.rs")):
            files.append((path, path.read_text(encoding=SOURCE_ENCODING)))
    return files


def macro_signatures(
    sources: list[tuple[Path, str]],
) -> list[tuple[Path, str, int, str, str]]:
    recovered: list[tuple[Path, str, int, str, str]] = []
    for path, source in sources:
        templates: dict[str, str | None] = {}
        for macro_match in re.finditer(r"\bmacro_rules!\s+(\w+)\s*\{", source):
            macro_end = balanced_end(source, macro_match.end() - 1, "{", "}")
            macro_text = source[macro_match.start() : macro_end]
            fn_match = re.search(r"\bfn\s+\$name\b", macro_text)
            if "no_mangle" not in macro_text:
                continue
            if fn_match is None:
                templates[macro_match.group(1)] = None
            else:
                template, _ = normalized_signature(
                    macro_text, macro_match.start() + fn_match.start()
                )
                templates[macro_match.group(1)] = template
        for macro_name, template in templates.items():
            invocation = re.compile(re.escape(macro_name) + r"!\s*(?P<opener>[\(\{])")
            for match in invocation.finditer(source):
                opener = match.group("opener")
                closer = ")" if opener == "(" else "}"
                invocation_end = balanced_end(source, match.end() - 1, opener, closer)
                arguments = source[match.end() : invocation_end]
                if template is None or opener == "{":
                    fn_match = re.search(r"\bfn\s+(hew_\w+)", arguments)
                    if fn_match is None:
                        continue
                    fn_offset = match.end() + fn_match.start()
                    signature, _ = normalized_signature(source, fn_offset)
                    recovered.append(
                        (path, source, match.start(), fn_match.group(1), signature)
                    )
                    continue
                concrete = re.search(
                    r"(?:#\[[^\]]+\]\s*)*\b(hew_\w+)\s*,\s*([^,\n)]+)",
                    arguments,
                    re.DOTALL,
                )
                if concrete is None:
                    continue
                symbol, concrete_type = concrete.groups()
                recovered.append(
                    (
                        path,
                        source,
                        match.start(),
                        symbol,
                        template.replace("$name", symbol).replace(
                            "$ty", concrete_type.strip()
                        ),
                    )
                )
    return recovered


def nul_and_extent(raw_signature: str) -> tuple[str, str]:
    has_pointer = any(marker in raw_signature for marker in ("*const", "*mut", "&mut"))
    has_length = re.search(r"\b(len|size|count|capacity|argc|out_len)\b", raw_signature)
    nul = "nul-terminated" if "c_char" in raw_signature else "not-applicable"
    if not has_pointer:
        extent = "not-applicable"
    elif has_length:
        extent = "length-or-count"
    else:
        extent = "no-in-signature-extent"
    return nul, extent


def classification(verifier: Any) -> dict[str, str]:
    classes = verifier.load_jit_symbol_classification()
    result: dict[str, str] = {}
    for tier, symbols in classes.items():
        for symbol in symbols:
            result[symbol] = tier
    return result


def function_source() -> dict[str, dict[str, dict[str, str]]]:
    functions = {target: {} for target in TARGETS}
    sources = source_files()
    direct_pattern = re.compile(
        r"#\[(?:no_mangle|cfg_attr\((?P<export_cfg>[^,]+),\s*no_mangle\))\]"
        r"(?:\s*(?:#\[[^\]]*(?:\([^)]*\))?[^\]]*\]|//[^\n]*))*"
        r'\s*(?:pub\s+)?(?:const\s+)?(?:unsafe\s+)?extern\s+"C(?:-unwind)?"\s+fn\s+'
        r"(?P<symbol>hew_\w+)",
        re.DOTALL,
    )
    candidates: list[tuple[Path, str, int, str, str, str, str | None]] = []
    for path, source in sources:
        for match in direct_pattern.finditer(source):
            signature, raw_signature = normalized_signature(
                source, match.start() + match.group().rfind("fn ")
            )
            candidates.append(
                (
                    path,
                    source,
                    match.start(),
                    match.group("symbol"),
                    signature,
                    raw_signature,
                    match.group("export_cfg"),
                )
            )
    for path, source, offset, symbol, signature in macro_signatures(sources):
        candidates.append((path, source, offset, symbol, signature, signature, None))

    for (
        path,
        source,
        offset,
        symbol,
        signature,
        raw_signature,
        export_cfg,
    ) in candidates:
        source_location = f"{path.relative_to(ROOT)}:{line_at(source, offset)}"
        nul, extent = nul_and_extent(raw_signature)
        for target in declaration_targets(path, source, offset):
            if export_cfg is not None and not evaluate_cfg(
                export_cfg, target == "wasm32-wasip1"
            ):
                continue
            row = {
                "signature": signature,
                "source": source_location,
                "nul": nul,
                "extent": extent,
            }
            existing = functions[target].get(symbol)
            if existing is not None and any(
                existing[field] != row[field]
                for field in ("signature", "nul", "extent")
            ):
                raise ValueError(
                    f"{symbol} has conflicting {target} source declarations: "
                    f"{existing['source']} and {source_location}"
                )
            if existing is None:
                functions[target][symbol] = row
    return functions


def static_source() -> dict[str, dict[str, dict[str, str]]]:
    statics = {target: {} for target in TARGETS}
    sources = source_files()
    map_path = ROOT / "hew-cabi" / "src" / "map.rs"
    map_text = map_path.read_text(encoding=SOURCE_ENCODING)
    declared_layouts = {
        match.group(1): match.group(2)
        for match in re.finditer(
            r"pub\s+static\s+(hew_layout_[A-Za-z0-9_]+)\s*:\s*([A-Za-z0-9_:]+);",
            map_text,
        )
    }
    definitions: dict[str, tuple[str, str]] = {}
    static_pattern = re.compile(
        r"#\[(?:unsafe\()?no_mangle\)?\]"
        r"(?:\s*#\[[^\]]+\])*"
        r"\s*pub\s+static\s+([A-Za-z_][A-Za-z0-9_]*)\s*:\s*([^=;]+)",
        re.DOTALL,
    )
    for path, source in sources:
        for match in static_pattern.finditer(source):
            symbol, type_name = match.groups()
            if not symbol.startswith(("hew_", "HEW_")):
                continue
            signature = f"static {symbol}: {re.sub(r'\\s+', ' ', type_name).strip()}"
            definitions[symbol] = (
                signature,
                f"{path.relative_to(ROOT)}:{line_at(source, match.start())}",
            )
            for target in declaration_targets(path, source, match.start()):
                statics[target][symbol] = {
                    "signature": signature,
                    "source": definitions[symbol][1],
                    "nul": "not-applicable",
                    "extent": "not-applicable",
                }
    for path, source in sources:
        templates: dict[str, str] = {}
        for macro_match in re.finditer(r"\bmacro_rules!\s+(\w+)\s*\{", source):
            macro_end = balanced_end(source, macro_match.end() - 1, "{", "}")
            macro_text = source[macro_match.start() : macro_end]
            static_match = re.search(
                r"#\[no_mangle\]\s*pub\s+static\s+\$name\s*:\s*([A-Za-z0-9_:]+)",
                macro_text,
            )
            if static_match is not None:
                templates[macro_match.group(1)] = static_match.group(1)
        for macro_name, type_name in templates.items():
            invocation = re.compile(re.escape(macro_name) + r"!\s*\(")
            for match in invocation.finditer(source):
                invocation_end = balanced_end(source, match.end() - 1, "(", ")")
                symbol_match = re.search(
                    r"\b(hew_\w+)\b", source[match.end() : invocation_end]
                )
                if symbol_match is None:
                    continue
                symbol = symbol_match.group(1)
                signature = f"static {symbol}: {type_name}"
                source_location = (
                    f"{path.relative_to(ROOT)}:{line_at(source, match.start())}"
                )
                definitions[symbol] = (signature, source_location)
                for target in declaration_targets(path, source, match.start()):
                    statics[target][symbol] = {
                        "signature": signature,
                        "source": source_location,
                        "nul": "not-applicable",
                        "extent": "not-applicable",
                    }
    missing = sorted(set(declared_layouts) - set(definitions))
    extra = sorted(
        symbol
        for symbol in definitions
        if symbol.startswith("hew_layout_") and symbol not in declared_layouts
    )
    if missing or extra:
        raise ValueError(
            f"layout static declarations differ: source-only={missing}, declaration-only={extra}"
        )
    return statics


def expected_manifest() -> dict[str, Any]:
    verifier = load_ffi_verifier()
    classes = classification(verifier)
    functions = function_source()
    statics = static_source()

    def rows_for(
        source: dict[str, dict[str, dict[str, str]]],
        static: bool,
    ) -> list[dict[str, Any]]:
        rows: list[dict[str, Any]] = []
        for symbol in sorted(set().union(*(set(rows) for rows in source.values()))):
            availability = [target for target in TARGETS if symbol in source[target]]
            record = source[availability[0]][symbol]
            rows.append(
                {
                    "symbol": symbol,
                    "signature": {
                        target: source[target][symbol]["signature"]
                        for target in availability
                    },
                    "availability": availability,
                    "nul": record["nul"],
                    "extent": record["extent"],
                    "classification": (
                        "layout-descriptor"
                        if static and symbol.startswith("hew_layout_")
                        else "circuit-breaker-constant"
                        if static
                        else classes.get(symbol, "unclassified-stdlib")
                    ),
                    "ownership": "unmeasured",
                }
            )
        return rows

    return {
        "version": 1,
        "functions": rows_for(functions, static=False),
        "statics": rows_for(statics, static=True),
    }


def canonical_json(document: dict[str, Any]) -> str:
    return json.dumps(document, indent=2, sort_keys=True) + "\n"


def c_string(value: str) -> str:
    return json.dumps(value)


def clang_format(source: str) -> str:
    try:
        result = subprocess.run(
            ["clang-format"],
            check=False,
            input=source,
            capture_output=True,
            text=True,
        )
    except OSError as error:
        raise RuntimeError(f"cannot run clang-format: {error}") from error
    if result.returncode:
        raise RuntimeError(f"clang-format failed: {result.stderr.strip()}")
    return result.stdout


def header_text(document: dict[str, Any]) -> str:
    def array(name: str, rows: list[dict[str, Any]]) -> str:
        entries = "".join(
            "    {"
            + ", ".join(
                c_string(value)
                for value in (
                    row["symbol"],
                    json.dumps(row["signature"], sort_keys=True),
                    ",".join(row["availability"]),
                    row["classification"],
                    row["nul"],
                    row["extent"],
                )
            )
            + ", HEW_CABI_OWNERSHIP_UNMEASURED},\n"
            for row in rows
        )
        return (
            f"static const struct hew_cabi_manifest_row {name}[{len(rows)}] = {{\n"
            + entries
            + "};\n"
        )

    unformatted = (
        "/* Generated by scripts/generate-cabi-surface.py; do not edit manually. */\n"
        "#ifndef HEW_CABI_SURFACE_H\n#define HEW_CABI_SURFACE_H\n\n"
        "#include <stddef.h>\n\n"
        "enum hew_cabi_manifest_ownership {\n"
        "    HEW_CABI_OWNERSHIP_UNMEASURED = 0\n};\n\n"
        "struct hew_cabi_manifest_row {\n"
        "    const char *symbol;\n    const char *signature;\n"
        "    const char *availability;\n    const char *classification;\n"
        "    const char *nul;\n    const char *extent;\n"
        "    enum hew_cabi_manifest_ownership ownership;\n};\n\n"
        f"#define HEW_CABI_MANIFEST_FUNCTION_COUNT {len(document['functions'])}u\n"
        f"#define HEW_CABI_MANIFEST_STATIC_COUNT {len(document['statics'])}u\n\n"
        + array("hew_cabi_manifest_functions", document["functions"])
        + "\n"
        + array("hew_cabi_manifest_statics", document["statics"])
        + "\n#endif\n"
    )
    return clang_format(unformatted)


def source_maps(
    expected: dict[str, Any],
) -> dict[str, dict[str, dict[str, str]]]:
    maps = {target: {"functions": {}, "statics": {}} for target in TARGETS}
    for kind in ("functions", "statics"):
        for row in expected[kind]:
            for target, signature in row["signature"].items():
                maps[target][kind][row["symbol"]] = signature
    return maps


def validate_manifest(document: dict[str, Any], expected: dict[str, Any]) -> list[str]:
    errors: list[str] = []
    if document.get("version") != 1:
        errors.append("manifest version must be 1")
    expected_maps = source_maps(expected)
    required_fields = {
        "symbol",
        "signature",
        "availability",
        "nul",
        "extent",
        "classification",
        "ownership",
    }
    for kind in ("functions", "statics"):
        rows = document.get(kind)
        if not isinstance(rows, list):
            errors.append(f"manifest {kind} must be an array")
            continue
        by_symbol: dict[str, dict[str, Any]] = {}
        for row in rows:
            if not isinstance(row, dict) or not isinstance(row.get("symbol"), str):
                errors.append(f"manifest {kind} row has no symbol")
                continue
            symbol = row["symbol"]
            if symbol in by_symbol:
                errors.append(f"manifest {kind} `{symbol}` is duplicated")
                continue
            by_symbol[symbol] = row
            if row.get("ownership") != "unmeasured":
                errors.append(
                    f"manifest {kind} `{symbol}` ownership must be unmeasured"
                )
            if "release_symbol" in row:
                errors.append(
                    f"manifest {kind} `{symbol}` is unmeasured and must not declare release_symbol"
                )
            unsupported = sorted(set(row) - required_fields)
            if unsupported:
                errors.append(
                    f"manifest {kind} `{symbol}` has unsupported manifest fields: "
                    + ", ".join(unsupported)
                )
        for target in TARGETS:
            source = expected_maps[target][kind]
            manifest = {
                symbol: row
                for symbol, row in by_symbol.items()
                if target in row.get("availability", [])
            }
            missing = sorted(set(source) - set(manifest))
            extra = sorted(set(manifest) - set(source))
            if missing or extra:
                pieces = []
                if missing:
                    pieces.append("source-only: " + ", ".join(missing))
                if extra:
                    pieces.append("manifest-only: " + ", ".join(extra))
                errors.append(f"{target} {kind} exports differ: " + "; ".join(pieces))
            for symbol in sorted(set(source) & set(manifest)):
                signature = manifest[symbol].get("signature", {}).get(target)
                if signature != source[symbol]:
                    errors.append(
                        f"{target} {kind} signature mismatch for `{symbol}`: "
                        f"expected {source[symbol]!r}, found {signature!r}"
                    )
        expected_by_symbol = {row["symbol"]: row for row in expected[kind]}
        for symbol in sorted(set(by_symbol) & set(expected_by_symbol)):
            row = by_symbol[symbol]
            for field in required_fields - {
                "symbol",
                "signature",
                "availability",
                "ownership",
            }:
                if row.get(field) != expected_by_symbol[symbol][field]:
                    errors.append(
                        f"manifest {kind} `{symbol}` {field} mismatch: "
                        f"expected {expected_by_symbol[symbol][field]!r}, found {row.get(field)!r}"
                    )
            if row.get("availability") != expected_by_symbol[symbol]["availability"]:
                errors.append(f"manifest {kind} `{symbol}` availability is stale")
    return errors


def compile_header(header: Path) -> list[str]:
    source = (
        '#include "hew_cabi_surface.h"\n'
        "int main(void) {\n"
        "    return (int)(HEW_CABI_MANIFEST_FUNCTION_COUNT + HEW_CABI_MANIFEST_STATIC_COUNT);\n"
        "}\n"
    )
    with tempfile.TemporaryDirectory() as directory:
        directory_path = Path(directory)
        source_path = directory_path / "manifest.c"
        object_path = directory_path / "manifest.o"
        source_path.write_text(source, encoding=SOURCE_ENCODING)
        result = subprocess.run(
            [
                "cc",
                "-std=c11",
                "-Werror",
                "-Wpedantic",
                "-I",
                str(header.parent),
                "-c",
                str(source_path),
                "-o",
                str(object_path),
            ],
            check=False,
            capture_output=True,
            text=True,
        )
    if result.returncode:
        return ["generated C header does not compile:\n" + result.stderr.strip()]
    return []


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    try:
        expected = expected_manifest()
    except (OSError, RuntimeError, ValueError) as error:
        print(f"ERROR: cannot generate source census: {error}", file=sys.stderr)
        return 1
    if args.write:
        args.manifest.parent.mkdir(parents=True, exist_ok=True)
        args.header.parent.mkdir(parents=True, exist_ok=True)
        args.manifest.write_text(canonical_json(expected), encoding=SOURCE_ENCODING)
        args.header.write_text(header_text(expected), encoding=SOURCE_ENCODING)
        return 0
    try:
        document = json.loads(args.manifest.read_text(encoding=SOURCE_ENCODING))
    except (OSError, json.JSONDecodeError) as error:
        print(f"ERROR: cannot read manifest: {error}", file=sys.stderr)
        return 1
    errors = validate_manifest(document, expected)
    if args.check:
        if canonical_json(document) != canonical_json(expected):
            errors.append(
                "manifest is stale; run scripts/generate-cabi-surface.py --write"
            )
        try:
            actual_header = args.header.read_text(encoding=SOURCE_ENCODING)
        except OSError as error:
            errors.append(f"cannot read generated C header: {error}")
        else:
            expected_header = header_text(expected)
            if actual_header != expected_header:
                errors.append(
                    "generated C header is stale; run scripts/generate-cabi-surface.py --write"
                )
            else:
                errors.extend(compile_header(args.header))
    for error in errors:
        print(f"ERROR: {error}", file=sys.stderr)
    return int(bool(errors))


if __name__ == "__main__":
    raise SystemExit(main())
