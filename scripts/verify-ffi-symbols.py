#!/usr/bin/env python3
"""Verify runtime/codegen FFI coverage and classify the stable JIT host ABI.

Scans all hew-runtime Rust source files for #[no_mangle] extern "C" fn exports,
then extracts all `hew_*` function name string references from C++ codegen source.
Reports any codegen reference that isn't covered by an actual runtime export.

Functions from separate stdlib packages (e.g. hew_http_*, hew_regex_*) are listed
in STDLIB_PACKAGE_PREFIXES — these are expected to be missing from hew-runtime and
are linked separately by the driver.

Usage:
    python3 scripts/verify-ffi-symbols.py                        # check and report
    python3 scripts/verify-ffi-symbols.py --strict               # exit 1 on uncovered refs
    python3 scripts/verify-ffi-symbols.py --classify stable      # print stable JIT symbols
    python3 scripts/verify-ffi-symbols.py --classify internal    # print internal JIT symbols
    python3 scripts/verify-ffi-symbols.py --classify stable --validate
    python3 scripts/verify-ffi-symbols.py --emit-cpp-header path # generate stable-symbol header
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
CODEGEN_SRC = ROOT / "hew-codegen" / "src"
RUNTIME_SRC = ROOT / "hew-runtime" / "src"
JIT_SYMBOL_CLASSIFICATION = ROOT / "scripts" / "jit-symbol-classification.toml"

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
        choices=("stable", "internal"),
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
        source = rs_file.read_text()
        for match in fn_pattern.finditer(source):
            exports.add(match.group(1))
    return exports


def extract_codegen_refs() -> set[str]:
    """Extract all hew_* function name strings from C++ codegen source."""
    refs = set()
    pattern = re.compile(r'"(hew_[a-zA-Z0-9_]+)"')
    for src_file in CODEGEN_SRC.rglob("*.[ch]*"):
        for match in pattern.finditer(src_file.read_text()):
            refs.add(match.group(1))
    return refs


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
    text = JIT_SYMBOL_CLASSIFICATION.read_text()
    classification: dict[str, set[str]] = {}
    for key in ("stable", "internal"):
        match = re.search(rf"(?ms)^{key}\s*=\s*\[(.*?)^\]", text)
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
    internal = classification["internal"]
    overlap = sorted(stable & internal)
    if overlap:
        errors.append(
            "symbols classified more than once: "
            + ", ".join(overlap)
            + f" (update {JIT_SYMBOL_CLASSIFICATION})"
        )
    classified = stable | internal
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
        + "};\n\n} // namespace hew::jit_detail\n"
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
    runtime_exports = extract_runtime_exports()
    codegen_refs = extract_codegen_refs()

    uncovered = []
    covered = 0
    skipped_internal = 0
    skipped_stdlib = 0
    skipped_template = 0

    for ref in sorted(codegen_refs):
        reason = classify(ref, runtime_exports)
        if not reason:
            if ref in CODEGEN_INTERNAL:
                skipped_internal += 1
            elif ref in TEMPLATE_NAMES:
                skipped_template += 1
            elif is_stdlib_package(ref):
                skipped_stdlib += 1
            else:
                covered += 1
        else:
            uncovered.append(ref)

    print(f"Runtime exports: {len(runtime_exports)}")
    print(f"Codegen references: {len(codegen_refs)}")
    print(f"  Covered by runtime exports: {covered}")
    print(f"  Stdlib packages (linked separately): {skipped_stdlib}")
    print(f"  Codegen-internal (intercepted): {skipped_internal}")
    print(f"  Template names (suffixed at emit): {skipped_template}")
    print(f"  Uncovered: {len(uncovered)}")

    if uncovered:
        print("\nUncovered functions (codegen references with no runtime export):")
        for name in uncovered:
            print(f"  - {name}")
        if strict:
            print("\nFAILED: uncovered symbols found (--strict mode)")
            return 1
    else:
        print("\nAll codegen references are covered.")

    return 0


def main(argv: list[str] | None = None) -> int:
    args = parse_args(argv)
    if args.classify is not None or args.validate or args.emit_cpp_header is not None:
        runtime_exports = extract_runtime_exports()
        return run_classification_mode(args, runtime_exports)
    return run_coverage_mode(args.strict)


if __name__ == "__main__":
    sys.exit(main())
