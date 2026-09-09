#!/usr/bin/env python3
"""Check that every std `extern "C"` declaration agrees with its Rust definition.

A `.hew` extern that the compiler does not intercept is lowered through the
generic extern path, where a `string` value is one managed `*HewString` handle
and a `bytes` value is a `*const BytesTriple`. A Rust definition that spells
either slot as a C character buffer reads the allocation header as text, so the
mismatch is a runtime fault rather than a link error: nothing else in the build
compares the two sides.

The three TCP entry points are the one exception: codegen rewrites them to the
`hew_checked_tcp_*` / `hew_async_tcp_*` adapters, which take the managed handle
and copy it for the raw transport call. Every other declaration, intercepted by
a runtime-call family or not, reaches its Rust definition with the managed
carrier.
"""

from __future__ import annotations

import re
import sys
from collections import defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
RUST_CRATES = ("hew-std", "hew-runtime", "hew-cabi", "hew-lib")
CARRIER = {"string": "HewString", "bytes": "BytesTriple"}

# Declarations codegen realizes through a managed-string adapter rather than a
# direct call to the named symbol, with the adapter that owns the conversion.
ADAPTED = {
    "hew_tcp_listen": "hew_checked_tcp_listen",
    "hew_tcp_connect": "hew_checked_tcp_connect / hew_async_tcp_connect",
    "hew_tcp_connect_timeout": "hew_checked_tcp_connect_timeout / hew_async_tcp_connect_timeout",
}

EXTERN_BLOCK = re.compile(r'extern\s+"C"\s*\{')
HEW_DECL = re.compile(r"fn\s+(\w+)\s*\((.*)\)\s*(->\s*[^;]+)?;")
RUST_DEF = re.compile(
    r"#\[(?:unsafe\()?no_mangle\)?\]\s*(?:(?:#\[[^\]]*\]|//[^\n]*)\s*)*"
    r'(?:pub\s+)?(?:unsafe\s+)?extern\s+"C"\s+fn\s+(\w+)\s*\(([^)]*)\)\s*(->\s*[^{]+)?\{',
    re.S,
)


def split_params(text: str) -> list[str]:
    """Split a parameter list on top-level commas."""
    parts: list[str] = []
    depth = 0
    current = ""
    for char in text:
        if char in "<([":
            depth += 1
        if char in ">)]":
            depth -= 1
        if char == "," and depth == 0:
            parts.append(current)
            current = ""
        else:
            current += char
    if current.strip():
        parts.append(current)
    return [part.strip() for part in parts if part.strip()]


def hew_declarations() -> dict[str, tuple[str, int, list[str], str]]:
    """Every extern declaration in `std/`, by symbol."""
    declarations: dict[str, tuple[str, int, list[str], str]] = {}
    for path in sorted((ROOT / "std").rglob("*.hew")):
        inside = False
        depth = 0
        buffered = ""
        first_line = 0
        for number, line in enumerate(path.read_text().splitlines(), 1):
            code = line.split("//")[0].strip()
            if not inside:
                if EXTERN_BLOCK.match(code):
                    inside = True
                    depth = 1
                continue
            depth += code.count("{") - code.count("}")
            if depth <= 0:
                inside = False
                continue
            if not code:
                continue
            if not buffered:
                first_line = number
            buffered += " " + code
            if not buffered.rstrip().endswith(";"):
                continue
            declared = HEW_DECL.search(buffered)
            if declared:
                params = [
                    param.split(":", 1)[-1].strip()
                    for param in split_params(declared.group(2))
                ]
                result = (declared.group(3) or "").replace("->", "").strip()
                declarations[declared.group(1)] = (
                    str(path.relative_to(ROOT)),
                    first_line,
                    params,
                    result,
                )
            buffered = ""
    return declarations


def rust_definitions() -> dict[str, tuple[str, list[str], str]]:
    """Every `#[no_mangle] extern "C"` definition in the linked crates."""
    definitions: dict[str, tuple[str, list[str], str]] = {}
    for crate in RUST_CRATES:
        for path in sorted((ROOT / crate / "src").rglob("*.rs")):
            source = path.read_text()
            for defined in RUST_DEF.finditer(source):
                params = [
                    param.split(":", 1)[-1].strip()
                    for param in split_params(" ".join(defined.group(2).split()))
                ]
                result = " ".join((defined.group(3) or "").replace("->", "").split())
                definitions[defined.group(1)] = (
                    str(path.relative_to(ROOT)),
                    params,
                    result,
                )
    return definitions


def main() -> int:
    declarations = hew_declarations()
    definitions = rust_definitions()

    failures: dict[str, list[str]] = defaultdict(list)
    for symbol, (hew_file, hew_line, params, result) in sorted(declarations.items()):
        carriers = [
            (index, CARRIER[param])
            for index, param in enumerate(params)
            if param in CARRIER
        ]
        result_carrier = CARRIER.get(result)
        if not carriers and not result_carrier:
            continue
        if symbol in ADAPTED:
            continue
        defined = definitions.get(symbol)
        if defined is None:
            failures["(no Rust definition found)"].append(
                f"{symbol}   [{hew_file}:{hew_line}]"
            )
            continue
        rust_file, rust_params, rust_result = defined
        wrong: list[str] = []
        for index, carrier in carriers:
            actual = rust_params[index] if index < len(rust_params) else "<missing>"
            if carrier not in actual:
                wrong.append(f"param{index} is {actual}, want *const {carrier}")
        if result_carrier and result_carrier not in rust_result:
            wrong.append(
                f"result is {rust_result or 'void'}, want *mut {result_carrier}"
            )
        if wrong:
            failures[rust_file].append(
                f"{symbol}: {'; '.join(wrong)}   [{hew_file}:{hew_line}]"
            )

    if not failures:
        print(
            f"verify-extern-string-abi: {len(declarations)} std extern declarations agree "
            f"with their Rust definitions ({len(ADAPTED)} reach them through an adapter)"
        )
        return 0

    print("verify-extern-string-abi: FAIL", file=sys.stderr)
    for rust_file in sorted(failures):
        print(f"  {rust_file}", file=sys.stderr)
        for row in failures[rust_file]:
            print(f"    {row}", file=sys.stderr)
    print(
        "\nA `string` slot is one managed `*HewString` handle and a `bytes` slot is a\n"
        "`*const BytesTriple`; see hew-std/src/uuid.rs for the shape.",
        file=sys.stderr,
    )
    return 1


if __name__ == "__main__":
    sys.exit(main())
