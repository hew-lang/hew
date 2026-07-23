#!/usr/bin/env python3
"""Classify and validate the stable JIT host ABI exported by hew-runtime.

Scans hew-runtime and hew-std Rust source files for #[no_mangle] extern "C"
and extern "C-unwind" fn exports and validates the classifications in
scripts/jit-symbol-classification.toml.

Three-tier model:
  stable         -- user-visible runtime surface; user `extern "rt"` blocks
                    and JIT hosts.
  codegen-stable -- compiler-emitted; JIT hosts must provide these alongside
                    stable, but users cannot name them in `extern "rt"`.
  internal       -- lifecycle/bootstrap; AOT-only, never JIT-reachable.

The codegen-coverage mode (--strict) is retained as a no-op for backward
compatibility now that the C++ codegen subtree has been retired; the
Rust IR ladder validates its own symbol references through the type
checker and inkwell.

Usage:
    python3 scripts/verify-ffi-symbols.py --classify stable --validate
    python3 scripts/verify-ffi-symbols.py --classify stable         # print stable JIT symbols
    python3 scripts/verify-ffi-symbols.py --classify codegen-stable # print codegen-stable symbols
    python3 scripts/verify-ffi-symbols.py --classify internal       # print internal JIT symbols
    python3 scripts/verify-ffi-symbols.py --emit-cpp-header path    # generate stable-symbol header
"""

from __future__ import annotations

import argparse
import re
import sys
import tomllib
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
RUNTIME_SRC = ROOT / "hew-runtime" / "src"
STDLIB_SRC = ROOT / "hew-std" / "src"
JIT_SYMBOL_CLASSIFICATION = ROOT / "scripts" / "jit-symbol-classification.toml"
FFI_OWNERSHIP_RATCHET = ROOT / "scripts" / "ffi-ownership-ratchet.toml"
SOURCE_ENCODING = "utf-8"
OWNERSHIP_RESULTS = {"fresh", "retained", "borrowed", "none"}
PARAM_OWNERSHIP = {"borrow", "consume", "retain"}
DISCHARGE_DEPTHS = {"shallow", "deep", "none"}

# Exact function names that are codegen-internal (intercepted/rewritten, never linked).
# For example, hew_log_debug is rewritten to hew_log_emit with a level argument.
CODEGEN_INTERNAL = {
    "hew_log_debug",
    "hew_log_error",
    "hew_log_info",
    "hew_log_init",
    "hew_log_trace",
    "hew_log_warn",
}

# Generic/unsuffixed names that codegen uses as base names — the actual emitted
# calls use type-suffixed variants (e.g. hew_vec_get_i64, hew_vec_push_str)
# or method-suffixed variants (e.g. hew_string_ + "len" → hew_string_len).
TEMPLATE_NAMES = {
    "hew_hashset_",
    "hew_stream_collect",
    "hew_stream_collect_bytes",
    "hew_vec_get",
    "hew_vec_set",
    "hew_vec_push",
    "hew_vec_pop",
    "hew_vec_remove",
    "hew_string_",
}


def parse_args(argv: list[str] | None = None) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--strict", action="store_true", help="fail if codegen refs are uncovered"
    )
    parser.add_argument(
        "--classify",
        choices=("stable", "codegen-stable", "internal"),
        help="print the sorted JIT host ABI symbols for the requested class",
    )
    parser.add_argument(
        "--validate",
        action="store_true",
        help="fail if any hew-runtime export is missing from the JIT classification file",
    )
    parser.add_argument(
        "--emit-cpp-header",
        type=Path,
        metavar="PATH",
        help="write a generated C++ header containing the stable JIT symbol list",
    )
    return parser.parse_args(argv)


def _find_no_mangle_macros(sources: list[tuple]) -> set[str]:
    """Return macro names whose bodies emit no-mangle C or C-unwind functions.

    Uses a simple balanced-brace walk so that macros with deeply nested bodies
    (e.g. vec_push_primitive!) are detected correctly even when the function
    name is a macro variable like ``$name``.
    """
    no_mangle_macros: set[str] = set()
    macro_start_re = re.compile(r"\bmacro_rules!\s+(\w+)\s*\{")
    for _path, source in sources:
        for m in macro_start_re.finditer(source):
            macro_name = m.group(1)
            brace_start = m.end() - 1  # points at the opening '{'
            # Walk balanced braces to find the end of the macro body.
            depth = 0
            body_end = len(source)
            for i in range(brace_start, len(source)):
                ch = source[i]
                if ch == "{":
                    depth += 1
                elif ch == "}":
                    depth -= 1
                    if depth == 0:
                        body_end = i + 1
                        break
            body = source[brace_start:body_end]
            if "#[no_mangle]" in body and re.search(
                r'extern\s+"C(?:-unwind)?"\s+fn', body
            ):
                no_mangle_macros.add(macro_name)
    return no_mangle_macros


def _extract_macro_generated_exports(
    sources: list[tuple], no_mangle_macros: set[str]
) -> set[str]:
    """Extract the function names produced by invocations of no-mangle-emitting macros.

    Handles both plain invocations::

        vec_push_primitive!(hew_vec_push_i32, i32);

    and invocations with outer attributes passed as the first argument::

        vec_contains_primitive!(
            #[expect(clippy::float_cmp, reason = "...")]
            hew_vec_contains_f64,
            f64
        );

    The first identifier after any leading ``#[...]`` blocks is taken as the
    generated function name.
    """
    exports: set[str] = set()
    for macro_name in no_mangle_macros:
        # Match: macro_name!( [optional #[...] attrs]* first_ident
        invoc_re = re.compile(
            re.escape(macro_name) + r"!\s*\(\s*"
            # Skip zero or more outer attribute blocks (#[...]).  The inner
            # content may contain parenthesised sub-expressions (e.g.
            # #[expect(reason = "...")]) but never an unescaped ']'.
            r"(?:#\[[^\]]*(?:\([^)]*\))?[^\]]*\]\s*)*"
            r"(\w+)",  # the first identifier = generated function name
            re.DOTALL,
        )
        for _path, source in sources:
            for m in invoc_re.finditer(source):
                exports.add(m.group(1))
    return exports


def _extract_native_exports(source_dir: Path) -> set[str]:
    """Scan Rust source files for #[no_mangle] function names.

    Two passes are performed:

    1. Direct scan — finds ``#[no_mangle] pub unsafe extern "C" fn name`` and
       ``#[no_mangle] pub unsafe extern "C-unwind" fn name`` where the name is
       a literal identifier in the source.

    2. Macro-expansion scan — finds ``macro_rules!`` definitions whose bodies
       contain ``#[no_mangle]`` plus a C or C-unwind function, then collects
       the first identifier from every invocation of those macros. This covers
       patterns like ``vec_push_primitive!(hew_vec_push_i64, i64)`` where the
       symbol name is the macro's ``$name`` parameter and is invisible to the
       regex in pass 1.
    """
    fn_pattern = re.compile(
        r"#\[no_mangle\]"
        # Skip interleaved attributes AND doc/line comments: several modules
        # (quic.rs among others) place `#[no_mangle]` BEFORE the doc comment,
        # and a scanner that stops at `///` silently drops those exports from
        # the candidate ABI (they can then never be classified or contracted).
        # `const` qualifiers (e.g. hew_stream_pair_is_valid) are accepted too.
        r"(?:\s*(?:#\[[^\]]*(?:\([^)]*\))?[^\]]*\]|//[^\n]*))*"
        r'\s*(?:pub\s+)?(?:const\s+)?(?:unsafe\s+)?extern\s+"C(?:-unwind)?"\s+fn\s+'
        r"(\w+)",
        re.DOTALL,
    )
    sources: list[tuple] = []
    exports: set[str] = set()
    for rs_file in source_dir.rglob("*.rs"):
        source = rs_file.read_text(encoding=SOURCE_ENCODING)
        sources.append((rs_file, source))
        for match in fn_pattern.finditer(source):
            exports.add(match.group(1))

    # Pass 2: pick up symbols defined inside macro_rules! bodies.
    no_mangle_macros = _find_no_mangle_macros(sources)
    exports.update(_extract_macro_generated_exports(sources, no_mangle_macros))
    return exports


# Release symbols that are not literal exports: type-directed drop thunks
# resolved through the element layout descriptor at codegen time.
DROP_THUNK_RELEASES = {"HewTypeLayout.drop_fn"}


def _extract_fn_param_counts(source_dirs: list[Path]) -> dict[str, set[int]]:
    """Map every literally-declared `fn name(...)` to its parameter count(s).

    Multiple counts per name are possible (cfg-gated native/wasm variants).
    Macro-generated exports have no literal signature and are absent from the
    map; the params-arity check skips them rather than guessing.
    """
    counts: dict[str, set[int]] = {}
    decl_re = re.compile(r"\bfn\s+(\w+)\s*\(")
    for source_dir in source_dirs:
        for rs_file in source_dir.rglob("*.rs"):
            source = rs_file.read_text(encoding=SOURCE_ENCODING)
            for match in decl_re.finditer(source):
                name = match.group(1)
                depth = 0
                end = match.end() - 1
                for index in range(end, len(source)):
                    if source[index] == "(":
                        depth += 1
                    elif source[index] == ")":
                        depth -= 1
                        if depth == 0:
                            end = index
                            break
                params_text = source[match.end() : end]
                parts: list[str] = []
                nest = 0
                current = ""
                for ch in params_text:
                    if ch in "<([":
                        nest += 1
                    elif ch in ">)]":
                        nest -= 1
                    if ch == "," and nest == 0:
                        parts.append(current.strip())
                        current = ""
                    else:
                        current += ch
                if current.strip():
                    parts.append(current.strip())
                arity = len(
                    [part for part in parts if part and not part.startswith("#")]
                )
                counts.setdefault(name, set()).add(arity)
    return counts


def extract_runtime_exports() -> set[str]:
    """Return native exports defined by hew-runtime."""
    return _extract_native_exports(RUNTIME_SRC)


def extract_stdlib_exports() -> set[str]:
    """Return native exports defined by hew-std."""
    return _extract_native_exports(STDLIB_SRC)


def classify(name: str, runtime_exports: set[str]) -> str:
    """Classify a codegen reference. Returns '' if covered, else 'UNCOVERED'."""
    if name in runtime_exports:
        return ""
    if name in CODEGEN_INTERNAL:
        return ""
    if name in TEMPLATE_NAMES:
        return ""
    # Check if it's a suffixed variant of a runtime function
    # e.g. hew_print_u32 → base hew_print is in runtime
    parts = name.rsplit("_", 1)
    if len(parts) == 2 and parts[0] in runtime_exports:
        return ""
    return "UNCOVERED"


def load_jit_symbol_classification() -> dict[str, set[str]]:
    text = JIT_SYMBOL_CLASSIFICATION.read_text(encoding=SOURCE_ENCODING)
    classification: dict[str, set[str]] = {}
    for key in ("stable", "stable-stdlib", "codegen-stable", "internal"):
        match = re.search(rf"(?ms)^{re.escape(key)}\s*=\s*\[(.*?)^\]", text)
        if match is None:
            raise ValueError(f"{JIT_SYMBOL_CLASSIFICATION}: missing {key} list")
        symbols = re.findall(r'"([^"\n]+)"', match.group(1))
        if len(symbols) != len(set(symbols)):
            raise ValueError(f"{JIT_SYMBOL_CLASSIFICATION}: duplicate entries in {key}")
        classification[key] = set(symbols)
    return classification


def validate_ownership_contracts(
    classification: dict[str, set[str]],
    all_exports: set[str],
    fn_param_counts: dict[str, set[int]],
) -> list[str]:
    errors: list[str] = []
    document = tomllib.loads(
        JIT_SYMBOL_CLASSIFICATION.read_text(encoding=SOURCE_ENCODING)
    )
    ownership = document.get("ownership")
    if not isinstance(ownership, dict):
        return [f"{JIT_SYMBOL_CLASSIFICATION}: missing [ownership] table"]
    contracts = ownership.get("contracts")
    if not isinstance(contracts, list):
        return [f"{JIT_SYMBOL_CLASSIFICATION}: ownership.contracts must be an array"]

    classified = set().union(*classification.values())
    contracted: set[str] = set()
    for index, contract in enumerate(contracts, start=1):
        location = f"{JIT_SYMBOL_CLASSIFICATION}: ownership contract #{index}"
        if not isinstance(contract, dict):
            errors.append(f"{location} must be a table")
            continue

        symbol = contract.get("symbol")
        if not isinstance(symbol, str) or not symbol:
            errors.append(f"{location} requires a non-empty symbol")
            continue
        location = f"{JIT_SYMBOL_CLASSIFICATION}: ownership contract for {symbol}"
        if symbol in contracted:
            errors.append(f"{location} is duplicated")
            continue
        contracted.add(symbol)
        if symbol not in classified:
            errors.append(f"{location} is not ABI-classified")

        result = contract.get("result")
        if result not in OWNERSHIP_RESULTS:
            errors.append(
                f"{location} result must be one of {sorted(OWNERSHIP_RESULTS)}"
            )

        params = contract.get("params")
        if not isinstance(params, list) or any(
            param not in PARAM_OWNERSHIP for param in params
        ):
            errors.append(
                f"{location} params must contain only {sorted(PARAM_OWNERSHIP)}"
            )
        elif symbol in fn_param_counts and len(params) not in fn_param_counts[symbol]:
            # Arity teeth: a contract whose params row no longer matches the C
            # signature of the implementation is stale and must fail here, not
            # silently mis-describe ownership positionally. Macro-generated
            # exports have no literal signature and are skipped.
            found = sorted(fn_param_counts[symbol])
            errors.append(
                f"{location} declares {len(params)} params but the FFI "
                f"declaration has {found} parameters"
            )

        release_symbol = contract.get("release-symbol")
        discharge_depth = contract.get("discharge-depth")
        if not isinstance(release_symbol, str):
            errors.append(f"{location} requires string release-symbol")
        elif (
            release_symbol
            and release_symbol not in all_exports
            and release_symbol not in DROP_THUNK_RELEASES
        ):
            # Release-symbol teeth: an owned result must name a REAL release
            # entry (or the documented drop-thunk spelling); a renamed or
            # deleted release function fails validation instead of leaving the
            # contract pointing at a phantom discharge path.
            errors.append(
                f"{location} release-symbol {release_symbol!r} is not an "
                "exported hew-runtime/hew-std symbol or known drop thunk"
            )
        if discharge_depth not in DISCHARGE_DEPTHS:
            errors.append(
                f"{location} discharge-depth must be one of {sorted(DISCHARGE_DEPTHS)}"
            )
        elif result in {"fresh", "retained"}:
            if not release_symbol:
                errors.append(f"{location} owned result requires release-symbol")
            if discharge_depth == "none":
                errors.append(
                    f"{location} owned result requires shallow or deep discharge"
                )
        elif release_symbol or discharge_depth != "none":
            errors.append(
                f"{location} borrowed/none result must use empty release-symbol "
                'and discharge-depth = "none"'
            )

    ratchet = tomllib.loads(FFI_OWNERSHIP_RATCHET.read_text(encoding=SOURCE_ENCODING))
    expected_unclassified = ratchet.get("unclassified")
    if not isinstance(expected_unclassified, int) or expected_unclassified < 0:
        errors.append(f"{FFI_OWNERSHIP_RATCHET}: unclassified must be non-negative")
    else:
        actual_unclassified = len(classified - contracted)
        if actual_unclassified != expected_unclassified:
            errors.append(
                f"unclassified ownership contracts: expected "
                f"{expected_unclassified}, found {actual_unclassified}; "
                f"update the exact count in {FFI_OWNERSHIP_RATCHET}"
            )
    return errors


def validate_jit_symbol_classification(
    runtime_exports: set[str],
    stdlib_exports: set[str],
    classification: dict[str, set[str]],
) -> list[str]:
    errors: list[str] = []
    stable = classification["stable"]
    stable_stdlib = classification["stable-stdlib"]
    codegen_stable = classification["codegen-stable"]
    internal = classification["internal"]
    # Pairwise overlap checks across all tiers.
    for tier_a, set_a, tier_b, set_b in [
        ("stable", stable, "stable-stdlib", stable_stdlib),
        ("stable", stable, "codegen-stable", codegen_stable),
        ("stable", stable, "internal", internal),
        ("stable-stdlib", stable_stdlib, "codegen-stable", codegen_stable),
        ("stable-stdlib", stable_stdlib, "internal", internal),
        ("codegen-stable", codegen_stable, "internal", internal),
    ]:
        overlap = sorted(set_a & set_b)
        if overlap:
            errors.append(
                f"symbols classified in both {tier_a} and {tier_b}: "
                + ", ".join(overlap)
                + f" (update {JIT_SYMBOL_CLASSIFICATION})"
            )
    # `stable-stdlib` is intentionally NOT unioned into the runtime completeness
    # check because those symbols come from hew-std rather than hew-runtime.
    classified = stable | codegen_stable | internal
    missing = sorted(runtime_exports - classified)
    extra = sorted(classified - runtime_exports)
    if missing:
        errors.append(
            f"unclassified runtime exports ({len(missing)}): "
            + ", ".join(missing)
            + f" (update {JIT_SYMBOL_CLASSIFICATION})"
        )
    if extra:
        errors.append(
            f"classification names not exported by hew-runtime ({len(extra)}): "
            + ", ".join(extra)
            + f" (remove from {JIT_SYMBOL_CLASSIFICATION})"
        )
    missing_stdlib_exports = sorted(stable_stdlib - stdlib_exports)
    if missing_stdlib_exports:
        errors.append(
            "stable-stdlib classification names not exported by hew-std "
            f"({len(missing_stdlib_exports)}): "
            + ", ".join(missing_stdlib_exports)
            + f" (remove from {JIT_SYMBOL_CLASSIFICATION})"
        )
    errors.extend(
        validate_ownership_contracts(
            classification,
            runtime_exports | stdlib_exports,
            _extract_fn_param_counts([RUNTIME_SRC, STDLIB_SRC]),
        )
    )
    return errors


def classified_symbols(kind: str, classification: dict[str, set[str]]) -> list[str]:
    return sorted(classification[kind])


def emit_cpp_header(path: Path, stable_symbols: list[str]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(
        "// Generated by scripts/verify-ffi-symbols.py; do not edit manually.\n"
        "#pragma once\n\n"
        "#include <array>\n"
        "#include <string_view>\n\n"
        "namespace hew::jit_detail {\n\n"
        f"inline constexpr std::array<std::string_view, {len(stable_symbols)}> kStableJitHostSymbols = {{\n"
        + "".join(f'    "{name}",\n' for name in stable_symbols)
        + "};\n\n} // namespace hew::jit_detail\n",
        encoding=SOURCE_ENCODING,
    )


def run_classification_mode(
    args: argparse.Namespace,
    runtime_exports: set[str],
    stdlib_exports: set[str],
) -> int:
    classification = load_jit_symbol_classification()
    if args.validate:
        errors = validate_jit_symbol_classification(
            runtime_exports, stdlib_exports, classification
        )
        if errors:
            for error in errors:
                print(f"ERROR: {error}", file=sys.stderr)
            return 1

    if args.classify is not None:
        print("\n".join(classified_symbols(args.classify, classification)))

    if args.emit_cpp_header is not None:
        emit_cpp_header(
            args.emit_cpp_header, classified_symbols("stable", classification)
        )
    return 0


def run_coverage_mode(strict: bool) -> int:
    """Codegen-coverage mode is a no-op now that the C++ codegen subtree has been retired.

    The historical behaviour walked `hew-codegen/src/**` for `hew_*` string
    references and reported any that lacked a matching runtime export. With
    the C++ tree gone, there are no codegen references to scan. Kept as a
    permissive entry point so existing tooling (`make verify-ffi`, IDE
    integrations) does not regress.
    """
    runtime_exports = extract_runtime_exports()
    print(f"Runtime exports: {len(runtime_exports)}")
    print(
        "Codegen-coverage mode is a no-op after the hew-codegen C++/MLIR "
        "retirement. Use --classify stable --validate for runtime-export "
        "classification checks."
    )
    _ = strict  # silence unused
    return 0


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    if args.classify is not None or args.validate or args.emit_cpp_header is not None:
        runtime_exports = extract_runtime_exports()
        stdlib_exports = extract_stdlib_exports()
        return run_classification_mode(args, runtime_exports, stdlib_exports)
    return run_coverage_mode(args.strict)


if __name__ == "__main__":
    sys.exit(main())
