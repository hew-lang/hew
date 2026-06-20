#!/usr/bin/env python3
"""Generate and verify the repo-local playground manifest used by browser tooling."""

from __future__ import annotations

import argparse
import difflib
import json
import re
import sys
from pathlib import Path
from typing import TypedDict


class Capabilities(TypedDict):
    browser: str
    sandbox: str
    wasi: str


class ManifestEntry(TypedDict):
    id: str
    category: str
    name: str
    description: str
    source_path: str
    expected_path: str
    capabilities: Capabilities


ROOT = Path(__file__).resolve().parent.parent
PLAYGROUND_DIR = ROOT / "examples" / "playground"
OUTPUT_FILE = PLAYGROUND_DIR / "manifest.json"
HEADER_PATTERN = re.compile(r"^//!\s*@(?P<key>name|description)\s+(?P<value>.+?)\s*$")
CATEGORY_ORDER = ("basics", "concurrency", "language", "machines", "types")
# Keep the checked-in manifest aligned with the current curated snippet order.
EXAMPLE_ORDER = {
    "basics": (
        "hello_world",
        "fibonacci",
        "float_arithmetic",
        "float_division",
        "float_nonfinite_compare",
        "higher_order_functions",
        "if_let_value",
        "mixed_numeric",
        "stmt_control_flow",
        "stmt_if",
        "stmt_if_let",
        "stmt_match",
        "string_interpolation",
        "clone_value",
    ),
    "concurrency": (
        "actor_pipeline",
        "async_await",
        "counter_actor",
        "supervisor",
    ),
    # Construct-coverage snippets backing the sandbox parity ratchet: each
    # exercises a profile-admitted language construct end-to-end so it cannot
    # be admitted-but-unparited (the G7 hole). See hew-sandbox-wasm/tests.
    "language": (
        "arithmetic_operators",
        "array_indexing",
        "string_slicing",
        "while_loop",
        "wildcard_match",
        "match_guard",
        "compound_assign",
        "f64_nonfinite_render",
        "f64_finite_render",
    ),
    "machines": ("traffic_light",),
    "types": (
        "collections",
        "pattern_matching",
        "record_types",
        "wire_types",
        "structural_bounds",
        "record_equality",
    ),
}

# Capability contract for the browser/sandbox/WASI manifest slice.
#
# browser: always "analysis-only" — hew-wasm (Tier 1) exposes lex/parse/typecheck
#          only; no in-browser program execution exists today.
# sandbox: "runnable"                — the example compiles to sandbox bytecode.
#          "unsupported_native_only" — the example is native-only for the
#                                      sandbox VM playground slice.
# wasi:    "runnable"   — the example compiles and executes correctly under
#                         hew build --target=wasm32-wasi (Tier 2).
#          "unsupported" — the example triggers a known WASM32 diagnostic path;
#                          it will fail to compile or produce a non-zero exit code
#                          under WASI because one or more features are rejected or
#                          warned at the type-checker level (see
#                          docs/wasm-capability-matrix.md for the full table).
#
# Entries omitted from SANDBOX_CAPABILITY default to "unsupported_native_only".
SANDBOX_CAPABILITY: dict[str, str] = {
    "basics/hello_world": "runnable",
    "basics/fibonacci": "runnable",
    # Float and mixed numeric arithmetic lower to the type-directed i64/f64
    # opcode families; the sandbox VM executes them at native parity.
    "basics/float_arithmetic": "runnable",
    "basics/float_division": "runnable",
    # Non-finite f64 equality (NaN/±Infinity) executes at native parity now that
    # the sandbox VM compares f64 operands with IEEE OEQ/ONE semantics.
    "basics/float_nonfinite_compare": "runnable",
    "basics/mixed_numeric": "runnable",
    # Statement-position if/match/if-let lower to branch terminators in sandbox
    # bytecode; all branch bodies are sandbox-admitted side effects.
    "basics/stmt_control_flow": "runnable",
    "basics/stmt_if": "runnable",
    "basics/stmt_if_let": "runnable",
    "basics/stmt_match": "runnable",
    "basics/string_interpolation": "runnable",
    # Value-position if-let: the matched arm value flows into a binding or
    # trailing expression. The lowering now declares a result local, binds the
    # matched payload in the then-arm, joins arms on the result local, and yields
    # it — identical to value-position if. The sandbox VM executes this at native
    # parity following the fix in this PR.
    "basics/if_let_value": "runnable",
    "types/pattern_matching": "runnable",
    # Actor/supervisor/machine support was added in the sandbox VM; these now compile
    # to bytecode and execute in the educational browser sandbox.
    "concurrency/counter_actor": "runnable",
    "concurrency/actor_pipeline": "runnable",
    # async_await uses actor+await which is now supported in the sandbox VM.
    "concurrency/async_await": "runnable",
    # machines/traffic_light has a fn main() demonstration and the sandbox VM
    # implements machine.new/step/state.
    "machines/traffic_light": "runnable",
    # concurrency/supervisor: rewritten to remove supervisor_stop (native-only);
    # spawn + child method calls are sandbox-admitted via the actor/supervisor
    # profile extension added in this release.
    "concurrency/supervisor": "runnable",
    # basics/higher_order_functions: rewritten from lambda-based to enum dispatch;
    # unit-variant construction as call arguments is now supported via enum.new.
    "basics/higher_order_functions": "runnable",
    # types/collections: rewritten to use indexed for-loop (range-based) rather
    # than for-in Vec; sandbox bytecode emission verified via parity test.
    "types/collections": "runnable",
    # types/record_types: plain record types with enum dispatch (formerly
    # misnamed wire_types); all constructs are sandbox-admitted.
    "types/record_types": "runnable",
    # types/wire_types: #[wire] struct declarations emit sandbox bytecode
    # (the playground consistency test proves it both ways); the example's
    # body is declaration + prints, all sandbox-admitted.
    "types/wire_types": "runnable",
    # types/structural_bounds: rewritten from trait/impl/generic (profile-rejected)
    # to plain record types with conditional dispatch.
    "types/structural_bounds": "runnable",
    # language/*: construct-coverage snippets that back the sandbox parity
    # ratchet — each runs at native↔sandbox parity (verified by tests/parity.rs).
    "language/arithmetic_operators": "runnable",
    "language/array_indexing": "runnable",
    "language/string_slicing": "runnable",
    "language/while_loop": "runnable",
    "language/wildcard_match": "runnable",
    "language/match_guard": "runnable",
    # Compound assignment (+=, -=, *=, /=, %=) for i64 and f64.
    "language/compound_assign": "runnable",
    # Non-finite f64 rendering: inf, -inf, nan match native printf %g.
    "language/f64_nonfinite_render": "runnable",
    # Finite f64 rendering: negative zero, %g thresholds, 6-sig-fig rounding.
    "language/f64_finite_render": "runnable",
    # basics/clone_value: `clone expr` produces an independent deep copy.
    "basics/clone_value": "runnable",
    # types/record_equality: structural == / != on records and payload enums.
    "types/record_equality": "runnable",
}

# Entries omitted from WASI_CAPABILITY default to "runnable".
WASI_CAPABILITY: dict[str, str] = {
    # Actor examples currently require the native actor/coroutine runtime ABI;
    # the WASI runtime gap is tracked by #1821.
    "concurrency/actor_pipeline": "unsupported",
    "concurrency/async_await": "unsupported",
    "concurrency/counter_actor": "unsupported",
    "concurrency/supervisor": "unsupported",  # supervision trees → WASM-TODO
    # traffic_light now has fn main() but the machine runtime is not yet wired
    # into the WASI/LLVM path; remains unsupported in WASI until that lands.
    "machines/traffic_light": "unsupported",
    # language/string_slicing: `hew_string_slice` aborts under wasm32-wasi with a
    # wasm-ld function-signature mismatch ((i32,i64,i64)->i32); the example runs
    # at native↔sandbox parity but is not WASI-runnable until that ABI gap closes.
    "language/string_slicing": "unsupported",
}


def curated_source_paths() -> list[Path]:
    curated_categories = set(CATEGORY_ORDER)
    configured_categories = set(EXAMPLE_ORDER)

    missing_categories = sorted(curated_categories - configured_categories)
    if missing_categories:
        raise SystemExit(
            "error: curated category list missing EXAMPLE_ORDER entries for: "
            + ", ".join(missing_categories)
        )

    unexpected_categories = sorted(configured_categories - curated_categories)
    if unexpected_categories:
        raise SystemExit(
            "error: EXAMPLE_ORDER contains unexpected categories: "
            + ", ".join(unexpected_categories)
        )

    curated_paths: list[Path] = []
    for category in CATEGORY_ORDER:
        for example in EXAMPLE_ORDER[category]:
            curated_paths.append(PLAYGROUND_DIR / category / f"{example}.hew")

    return curated_paths


def parse_header_metadata(source_path: Path) -> dict[str, str]:
    metadata: dict[str, str] = {}

    for line in source_path.read_text().splitlines():
        stripped = line.strip()
        if not stripped:
            if metadata:
                break
            continue
        if not stripped.startswith("//!"):
            break

        match = HEADER_PATTERN.match(stripped)
        if match:
            metadata[match.group("key")] = match.group("value")

    missing = [key for key in ("name", "description") if key not in metadata]
    if missing:
        missing_str = ", ".join(missing)
        rel_path = source_path.relative_to(ROOT)
        raise SystemExit(f"error: {rel_path} missing header metadata: {missing_str}")

    return metadata


def fail_on_curated_scope_mismatch(curated_paths: list[Path]) -> None:
    curated_rel_paths = {
        source_path.relative_to(ROOT).as_posix() for source_path in curated_paths
    }
    actual_rel_paths = {
        source_path.relative_to(ROOT).as_posix()
        for source_path in PLAYGROUND_DIR.rglob("*.hew")
    }

    missing_sources = sorted(curated_rel_paths - actual_rel_paths)
    unexpected_sources = sorted(actual_rel_paths - curated_rel_paths)
    if not missing_sources and not unexpected_sources:
        return

    details = []
    if missing_sources:
        details.append(
            "missing curated playground snippets:\n  - "
            + "\n  - ".join(missing_sources)
        )
    if unexpected_sources:
        details.append(
            "unexpected playground snippets outside the curated "
            f"{len(curated_rel_paths)}:\n  - " + "\n  - ".join(unexpected_sources)
        )

    raise SystemExit(
        "error: examples/playground no longer matches the curated manifest scope\n"
        + "\n".join(details)
    )


def build_manifest_entries() -> list[ManifestEntry]:
    curated_paths = curated_source_paths()
    fail_on_curated_scope_mismatch(curated_paths)

    entries: list[ManifestEntry] = []
    for source_path in curated_paths:
        if not source_path.is_file():
            rel_path = source_path.relative_to(ROOT)
            raise SystemExit(f"error: missing curated playground snippet: {rel_path}")
        expected_path = source_path.with_suffix(".expected")
        category = source_path.parent.name
        entry_id = f"{category}/{source_path.stem}"
        wasi_capability = WASI_CAPABILITY.get(entry_id, "runnable")
        if wasi_capability == "runnable" and not expected_path.is_file():
            rel_path = expected_path.relative_to(ROOT)
            raise SystemExit(f"error: missing expected output file for {rel_path}")

        metadata = parse_header_metadata(source_path)

        entries.append(
            {
                "id": entry_id,
                "category": category,
                "name": metadata["name"],
                "description": metadata["description"],
                "source_path": source_path.relative_to(PLAYGROUND_DIR).as_posix(),
                "expected_path": expected_path.relative_to(PLAYGROUND_DIR).as_posix(),
                "capabilities": {
                    "browser": "analysis-only",
                    "sandbox": SANDBOX_CAPABILITY.get(
                        entry_id, "unsupported_native_only"
                    ),
                    "wasi": wasi_capability,
                },
            }
        )

    if not entries:
        raise SystemExit(
            "error: no playground snippets found under examples/playground"
        )

    return entries


def render_manifest(entries: list[ManifestEntry]) -> str:
    return json.dumps(entries, indent=2) + "\n"


def check_manifest(rendered: str) -> int:
    rel_output = OUTPUT_FILE.relative_to(ROOT).as_posix()
    if not OUTPUT_FILE.is_file():
        print(
            f"error: {rel_output} is missing; run python3 scripts/gen-playground-manifest.py",
            file=sys.stderr,
        )
        return 1

    existing = OUTPUT_FILE.read_text()
    if existing == rendered:
        print(f"{rel_output} is up to date.")
        return _verify_capability_fields(json.loads(existing), rel_output)

    diff = difflib.unified_diff(
        existing.splitlines(keepends=True),
        rendered.splitlines(keepends=True),
        fromfile=rel_output,
        tofile=f"{rel_output} (generated)",
    )
    sys.stdout.writelines(diff)
    print(
        "\nerror: playground manifest is stale; run python3 scripts/gen-playground-manifest.py",
        file=sys.stderr,
    )
    return 1


def _verify_capability_fields(entries: list[ManifestEntry], rel_output: str) -> int:
    """Verify every entry carries well-formed capability metadata."""
    valid_sandbox = {"runnable", "unsupported_native_only"}
    valid_wasi = {"runnable", "unsupported"}
    errors: list[str] = []

    for entry in entries:
        entry_id = entry.get("id", "<unknown>")
        caps = entry.get("capabilities")
        if caps is None:
            errors.append(f"  {entry_id}: missing 'capabilities' field")
            continue
        if not isinstance(caps, dict):
            errors.append(f"  {entry_id}: 'capabilities' must be an object")
            continue
        if caps.get("browser") != "analysis-only":
            errors.append(
                f"  {entry_id}: capabilities.browser must be 'analysis-only', "
                f"got {caps.get('browser')!r}"
            )
        sandbox = caps.get("sandbox")
        if sandbox not in valid_sandbox:
            errors.append(
                f"  {entry_id}: capabilities.sandbox must be one of "
                f"{sorted(valid_sandbox)}, got {sandbox!r}"
            )
        wasi = caps.get("wasi")
        if wasi not in valid_wasi:
            errors.append(
                f"  {entry_id}: capabilities.wasi must be one of "
                f"{sorted(valid_wasi)}, got {wasi!r}"
            )

    if errors:
        print(
            f"error: {rel_output} has invalid capability metadata:\n"
            + "\n".join(errors),
            file=sys.stderr,
        )
        return 1

    return 0


def main() -> int:
    parser = argparse.ArgumentParser(
        description=(
            "Generate or verify examples/playground/manifest.json for the "
            "repo-local browser/playground smoke path."
        )
    )
    parser.add_argument(
        "--check",
        action="store_true",
        help=(
            "Verify that examples/playground/manifest.json is up to date for "
            "repo-local browser tooling"
        ),
    )
    args = parser.parse_args()

    entries = build_manifest_entries()
    rendered = render_manifest(entries)

    if args.check:
        return check_manifest(rendered)

    OUTPUT_FILE.write_text(rendered)
    print(
        f"Generated {OUTPUT_FILE.relative_to(ROOT)} with {len(entries)} curated browser/tooling playground snippets."
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
