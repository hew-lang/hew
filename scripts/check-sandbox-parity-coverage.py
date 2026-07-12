#!/usr/bin/env python3
"""Assert every VM-dependent hew-sandbox-wasm test is covered coherently.

A test in hew-sandbox-wasm/tests/*.rs is "VM-dependent" if it spawns the
hew-sandbox-vm Node/npm toolchain (requires `node_modules` to be
provisioned) -- either directly, or transitively through local helper
functions it calls. Detection is call-graph based, not a single literal
string check on the test's own body: a test can reach the VM only through
a chain of same-file helpers (e.g. `test -> assert_admitted_runs_clean ->
run_sandbox_inline`, none of which is the test's own body) and must still
be caught.

Regression this guards against: a new VM-dependent test lands in the crate
but

  1. is NOT excluded from the generic nextest default-filter
     (.config/nextest.toml, profile.default and profile.ci), so plain
     `cargo nextest run --workspace` on any platform (macOS/Windows/FreeBSD/
     release-gate/`make test`) fails on an unprovisioned runner, OR
  2. IS excluded from the generic filter but its binary is never named in
     the Makefile's `sandbox-parity` recipe, so the dedicated provisioned
     gate silently never exercises its real native-vs-VM behavior.

This does not prescribe *how* a test is excluded (whole-binary
`binary(<name>)` vs a specific `test(<name>)`) -- both are established,
legitimate conventions in nextest.toml. It only asserts that some exclusion
covers every VM-dependent test in both generic profiles, and that
`make sandbox-parity` runs every binary that contains at least one.

Detection method
----------------
1. Parse every top-level `fn NAME(...) { ... }` in the file (not just
   `#[test]` ones), each function's full body text (nested braces included).
2. A function is a VM "marker" if its body contains the literal string
   `hew-sandbox-vm` -- the Node/npm toolchain directory every real spawn
   path names (`ensure_parity_runner_built`, `run_sandbox`,
   `run_sandbox_inline`, ...). This is a broad substring match on purpose:
   over-detecting a function as a marker is safe (worst case, a harmless
   function gets swept into the VM-dependent set); under-detecting is the
   actual bug class this script exists to catch.
3. Build a same-file call graph (function A "calls" function B if `B(`
   appears literally in A's body) and compute, for every `#[test]` fn,
   whether it transitively reaches any marker function.
4. Safety net: if any marker function in the file is not transitively
   reached by ANY `#[test]` in the same file, the static call graph above
   is known-incomplete for this file (e.g. a marker reached only through a
   function pointer, closure combinator, or trait dispatch this script does
   not parse). In that case every test in the binary is conservatively
   treated as VM-dependent and the binary-level exclusion this script
   requires falls back to needing `binary(<name>)` specifically (a
   per-test `test(<name>)` exclusion is not accepted, since per-test
   attribution cannot be trusted for this file).

Usage: python3 scripts/check-sandbox-parity-coverage.py [--verbose]
"""

from __future__ import annotations

import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
TESTS_DIR = REPO_ROOT / "hew-sandbox-wasm" / "tests"
NEXTEST_TOML = REPO_ROOT / ".config" / "nextest.toml"
MAKEFILE = REPO_ROOT / "Makefile"

VM_MARKER_STRING = "hew-sandbox-vm"


def extract_functions(text: str) -> dict[str, str]:
    """Return {fn_name: full_body_text} for every top-level `fn NAME(...) {...}`."""
    functions: dict[str, str] = {}
    for m in re.finditer(r"\bfn\s+(\w+)\s*(?:<[^>]*>)?\s*\(", text):
        name = m.group(1)
        rest = text[m.end() :]
        brace_idx = rest.find("{")
        # A `fn` with no `{` before the next `;` is a signature-only
        # declaration (trait method, extern fn) -- not present in these
        # test files, but skip defensively rather than mis-parse.
        semi_idx = rest.find(";")
        if brace_idx == -1 or (semi_idx != -1 and semi_idx < brace_idx):
            continue
        depth = 1
        i = brace_idx + 1
        while depth > 0 and i < len(rest):
            if rest[i] == "{":
                depth += 1
            elif rest[i] == "}":
                depth -= 1
            i += 1
        body = rest[brace_idx:i]
        # A name can appear more than once (e.g. cfg-gated overloads); keep
        # the first, real duplicate `fn` names are not valid Rust anyway.
        functions.setdefault(name, body)
    return functions


def find_test_names(text: str, known_functions: dict[str, str]) -> list[str]:
    """Return the fn names immediately following a `#[test]` attribute."""
    names = []
    for m in re.finditer(r"#\[test\]", text):
        fn_m = re.search(r"fn\s+(\w+)\s*\(", text[m.end() :])
        if fn_m and fn_m.group(1) in known_functions:
            names.append(fn_m.group(1))
    return names


def build_call_graph(functions: dict[str, str]) -> dict[str, set[str]]:
    """Return {fn_name: {names of other known fns called in its body}}."""
    graph: dict[str, set[str]] = {}
    names = list(functions.keys())
    for name, body in functions.items():
        called = set()
        for other in names:
            if other == name:
                continue
            if re.search(rf"\b{re.escape(other)}\s*\(", body):
                called.add(other)
        graph[name] = called
    return graph


def marker_functions(functions: dict[str, str]) -> set[str]:
    return {name for name, body in functions.items() if VM_MARKER_STRING in body}


def transitive_closure(start: str, graph: dict[str, set[str]]) -> set[str]:
    """All function names reachable from `start` via the call graph (inclusive)."""
    seen = {start}
    stack = [start]
    while stack:
        current = stack.pop()
        for callee in graph.get(current, ()):
            if callee not in seen:
                seen.add(callee)
                stack.append(callee)
    return seen


def analyze_file(path: Path, verbose: bool) -> tuple[list[str], bool]:
    """Return (vm_dependent_test_names, binary_level_fallback_triggered)."""
    text = path.read_text()
    functions = extract_functions(text)
    test_names = find_test_names(text, functions)
    markers = marker_functions(functions)
    graph = build_call_graph(functions)

    reachable_from_tests: set[str] = set()
    test_reaches: dict[str, set[str]] = {}
    for test_name in test_names:
        reached = transitive_closure(test_name, graph)
        test_reaches[test_name] = reached
        reachable_from_tests |= reached

    orphaned_markers = markers - reachable_from_tests
    fallback = bool(orphaned_markers)

    if fallback:
        if verbose:
            print(
                f"  NOTE {path.name}: marker function(s) "
                f"{sorted(orphaned_markers)} not reachable from any #[test] in "
                f"this file via the static call graph -- falling back to "
                f"whole-binary VM-dependent classification for {path.stem}."
            )
        vm_tests = list(test_names)
    else:
        vm_tests = [name for name in test_names if markers & test_reaches[name]]

    return vm_tests, fallback


def default_filter_line(profile: str) -> str:
    """Return the raw default-filter value for the given [profile.<profile>]."""
    text = NEXTEST_TOML.read_text()
    profile_m = re.search(
        rf"^\[profile\.{re.escape(profile)}\]\s*$", text, re.MULTILINE
    )
    if not profile_m:
        raise SystemExit(f"error: [profile.{profile}] not found in {NEXTEST_TOML}")
    rest = text[profile_m.end() :]
    filter_m = re.search(r'^default-filter\s*=\s*"([^"]*)"', rest, re.MULTILINE)
    if not filter_m:
        raise SystemExit(f"error: default-filter not found in [profile.{profile}]")
    return filter_m.group(1)


def excludes_binary(filter_value: str, binary: str) -> bool:
    return f"binary({binary})" in filter_value


def excludes_test(filter_value: str, binary: str, test_name: str) -> bool:
    return excludes_binary(filter_value, binary) or f"test({test_name})" in filter_value


def sandbox_parity_test_flags() -> str:
    makefile_text = MAKEFILE.read_text()
    m = re.search(
        r"^sandbox-parity:.*\n(?:.*\n)*?^\tcargo test -p hew-sandbox-wasm(.*)$",
        makefile_text,
        re.MULTILINE,
    )
    if not m:
        raise SystemExit(
            "error: could not find `sandbox-parity`'s `cargo test -p "
            "hew-sandbox-wasm` recipe line in Makefile."
        )
    return m.group(1)


def main() -> int:
    verbose = "--verbose" in sys.argv[1:]

    vm_dependent_binaries: dict[str, list[str]] = {}
    fallback_binaries: set[str] = set()
    for path in sorted(TESTS_DIR.glob("*.rs")):
        vm_tests, fallback = analyze_file(path, verbose)
        if vm_tests:
            vm_dependent_binaries[path.stem] = vm_tests
        if fallback:
            fallback_binaries.add(path.stem)

    if not vm_dependent_binaries:
        print(
            "error: no VM-dependent tests found under hew-sandbox-wasm/tests/ "
            "-- extraction likely broke; check analyze_file() against the "
            "current test file shapes.",
            file=sys.stderr,
        )
        return 1

    if verbose:
        print("==> VM-dependent tests (reach a hew-sandbox-vm spawn transitively):")
        for binary, names in vm_dependent_binaries.items():
            tag = " [binary-level fallback]" if binary in fallback_binaries else ""
            for name in names:
                print(f"  - {binary}::{name}{tag}")
        print()

    default_filter = default_filter_line("default")
    ci_filter = default_filter_line("ci")
    sandbox_parity_cmd = sandbox_parity_test_flags()

    fail = 0
    pass_count = 0
    for binary, names in vm_dependent_binaries.items():
        binary_level_required = binary in fallback_binaries
        for name in names:
            for profile, filter_value in (
                ("default", default_filter),
                ("ci", ci_filter),
            ):
                if binary_level_required:
                    ok = excludes_binary(filter_value, binary)
                    requirement = f"`binary({binary})` (fallback: per-test attribution is not trusted for this file)"
                else:
                    ok = excludes_test(filter_value, binary, name)
                    requirement = f"`binary({binary})` or `test({name})`"
                if ok:
                    pass_count += 1
                    if verbose:
                        print(f"  ok  [profile.{profile}] excludes {binary}::{name}")
                else:
                    fail += 1
                    print(
                        f"  FAIL [profile.{profile}] does not exclude VM-dependent test "
                        f"{binary}::{name} (need {requirement} in .config/nextest.toml's "
                        f"[profile.{profile}] default-filter)",
                        file=sys.stderr,
                    )

        if f"--test {binary}" in sandbox_parity_cmd:
            pass_count += 1
            if verbose:
                print(f"  ok  Makefile sandbox-parity runs --test {binary}")
        else:
            fail += 1
            print(
                f"  FAIL Makefile `sandbox-parity` recipe does not run `--test {binary}`, "
                f"so its VM-dependent test(s) never run anywhere "
                f"(add it to the `cargo test -p hew-sandbox-wasm` line)",
                file=sys.stderr,
            )

    print()
    print(
        f"==> sandbox-parity coverage: {pass_count} check(s) passed, {fail} failed, "
        f"across {len(vm_dependent_binaries)} VM-dependent binary(ies) "
        f"({len(fallback_binaries)} via binary-level fallback)."
    )

    if fail:
        print(
            "\nFAIL: a VM-dependent hew-sandbox-wasm test is either not excluded from "
            "generic nextest runs, or excluded but never run by `make sandbox-parity`.",
            file=sys.stderr,
        )
        return 1

    print(
        "     Every VM-dependent test is excluded from generic nextest runs "
        "(profile.default and profile.ci) and covered by `make sandbox-parity`."
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
