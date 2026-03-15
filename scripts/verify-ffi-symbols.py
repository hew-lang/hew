#!/usr/bin/env python3
"""Verify that C++ codegen runtime function references have matching exports.

Scans all hew-runtime Rust source files for #[no_mangle] extern "C" fn exports,
then extracts all `hew_*` function name string references from C++ codegen source.
Reports any codegen reference that isn't covered by an actual runtime export.

Functions from separate stdlib packages (e.g. hew_http_*, hew_regex_*) are listed
in STDLIB_PACKAGE_PREFIXES — these are expected to be missing from hew-runtime and
are linked separately by the driver.

Usage:
    python3 scripts/verify-ffi-symbols.py          # check and report
    python3 scripts/verify-ffi-symbols.py --strict  # exit 1 on any uncovered symbol
"""

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
CODEGEN_SRC = ROOT / "hew-codegen" / "src"
RUNTIME_SRC = ROOT / "hew-runtime" / "src"

# Function prefixes that live in separate stdlib packages, not hew-runtime.
# These are linked by the compiler driver when the user imports the module.
STDLIB_PACKAGE_PREFIXES = (
    "hew_http_",
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
    "hew_vec_get",
    "hew_vec_set",
    "hew_vec_push",
    "hew_vec_pop",
    "hew_vec_remove",
    "hew_string_",
}


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


def main():
    strict = "--strict" in sys.argv

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
            sys.exit(1)
    else:
        print("\nAll codegen references are covered.")

    return 0


if __name__ == "__main__":
    sys.exit(main())
