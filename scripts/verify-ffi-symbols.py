#!/usr/bin/env python3
"""Classify and validate the stable JIT host ABI exported by hew-runtime.

Scans all hew-runtime Rust source files for #[no_mangle] extern "C" fn
exports and validates each one is classified (stable, codegen-stable, or
internal) in scripts/jit-symbol-classification.toml.

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
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
RUNTIME_SRC = ROOT / "hew-runtime" / "src"
JIT_SYMBOL_CLASSIFICATION = ROOT / "scripts" / "jit-symbol-classification.toml"
SOURCE_ENCODING = "utf-8"

# Function prefixes that live in separate stdlib packages, not hew-runtime.
# These are linked by the compiler driver when the user imports the module.
STDLIB_PACKAGE_PREFIXES = (
    "hew_http_",
    "hew_tls_",
    "hew_json_",
    "hew_regex_",
    "hew_sqlite_",
    "hew_postgres_",
    "hew_redis_",
    "hew_csv_",
    "hew_toml_",
    "hew_yaml_",
    "hew_mqtt_",
    "hew_grpc_",
)

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


def extract_runtime_exports() -> set[str]:
    """Scan all hew-runtime .rs files for #[no_mangle] function names."""
    exports = set()
    fn_pattern = re.compile(
        r"#\[no_mangle\]"
        r"(?:\s*#\[[^\]]*(?:\([^)]*\))?[^\]]*\])*"  # skip interleaved attrs
        r'\s*(?:pub\s+)?(?:unsafe\s+)?extern\s+"C"\s+fn\s+'
        r"(\w+)",
        re.DOTALL,
    )
    for rs_file in RUNTIME_SRC.rglob("*.rs"):
        source = rs_file.read_text(encoding=SOURCE_ENCODING)
        for match in fn_pattern.finditer(source):
            exports.add(match.group(1))
    return exports


def is_stdlib_package(name: str) -> bool:
    return any(name.startswith(prefix) for prefix in STDLIB_PACKAGE_PREFIXES)


def classify(name: str, runtime_exports: set[str]) -> str:
    """Classify a codegen reference. Returns '' if covered, else 'UNCOVERED'."""
    if name in runtime_exports:
        return ""
    if name in CODEGEN_INTERNAL:
        return ""
    if name in TEMPLATE_NAMES:
        return ""
    if is_stdlib_package(name):
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
    for key in ("stable", "codegen-stable", "internal"):
        match = re.search(rf"(?ms)^{re.escape(key)}\s*=\s*\[(.*?)^\]", text)
        if match is None:
            raise ValueError(f"{JIT_SYMBOL_CLASSIFICATION}: missing {key} list")
        symbols = re.findall(r'"([^"\n]+)"', match.group(1))
        if len(symbols) != len(set(symbols)):
            raise ValueError(f"{JIT_SYMBOL_CLASSIFICATION}: duplicate entries in {key}")
        classification[key] = set(symbols)
    return classification


def validate_jit_symbol_classification(
    runtime_exports: set[str], classification: dict[str, set[str]]
) -> list[str]:
    errors: list[str] = []
    stable = classification["stable"]
    codegen_stable = classification["codegen-stable"]
    internal = classification["internal"]
    # Pairwise overlap checks across all three tiers.
    for tier_a, set_a, tier_b, set_b in [
        ("stable", stable, "codegen-stable", codegen_stable),
        ("stable", stable, "internal", internal),
        ("codegen-stable", codegen_stable, "internal", internal),
    ]:
        overlap = sorted(set_a & set_b)
        if overlap:
            errors.append(
                f"symbols classified in both {tier_a} and {tier_b}: "
                + ", ".join(overlap)
                + f" (update {JIT_SYMBOL_CLASSIFICATION})"
            )
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


def run_classification_mode(args: argparse.Namespace, runtime_exports: set[str]) -> int:
    classification = load_jit_symbol_classification()
    if args.validate:
        errors = validate_jit_symbol_classification(runtime_exports, classification)
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
        return run_classification_mode(args, runtime_exports)
    return run_coverage_mode(args.strict)


if __name__ == "__main__":
    sys.exit(main())
