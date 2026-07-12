#!/usr/bin/env python3
"""Assert every VM-dependent hew-sandbox-wasm test is covered coherently.

A test in hew-sandbox-wasm/tests/*.rs is "VM-dependent" if its body calls
`ensure_parity_runner_built()`, meaning it spawns the hew-sandbox-vm Node/npm
toolchain (requires `node_modules` to be provisioned). Regression this
guards against: a new VM-dependent test lands in the crate but

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


def extract_tests(text: str) -> list[tuple[str, str]]:
    """Return [(test_name, body_text), ...] for every #[test] fn in text."""
    tests = []
    for m in re.finditer(r"#\[test\]", text):
        rest = text[m.end() :]
        fn_m = re.search(r"fn\s+(\w+)\s*\(", rest)
        if not fn_m:
            continue
        name = fn_m.group(1)
        brace_start = rest.index("{", fn_m.end())
        depth = 1
        i = brace_start + 1
        while depth > 0 and i < len(rest):
            if rest[i] == "{":
                depth += 1
            elif rest[i] == "}":
                depth -= 1
            i += 1
        tests.append((name, rest[brace_start:i]))
    return tests


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


def is_excluded(filter_value: str, binary: str, test_name: str) -> bool:
    return f"binary({binary})" in filter_value or f"test({test_name})" in filter_value


def main() -> int:
    verbose = "--verbose" in sys.argv[1:]

    by_binary: dict[str, list[tuple[str, bool]]] = {}
    for path in sorted(TESTS_DIR.glob("*.rs")):
        binary = path.stem
        tests = extract_tests(path.read_text())
        vm_tests = [
            (name, "ensure_parity_runner_built" in body) for name, body in tests
        ]
        by_binary[binary] = vm_tests

    vm_dependent_binaries = {
        binary: [name for name, is_vm in tests if is_vm]
        for binary, tests in by_binary.items()
    }
    vm_dependent_binaries = {b: t for b, t in vm_dependent_binaries.items() if t}

    if not vm_dependent_binaries:
        print(
            "error: no VM-dependent tests found under hew-sandbox-wasm/tests/ "
            "-- extraction likely broke; check extract_tests() against the "
            "current test file shapes.",
            file=sys.stderr,
        )
        return 1

    if verbose:
        print("==> VM-dependent tests (call ensure_parity_runner_built()):")
        for binary, names in vm_dependent_binaries.items():
            for name in names:
                print(f"  - {binary}::{name}")
        print()

    default_filter = default_filter_line("default")
    ci_filter = default_filter_line("ci")
    makefile_text = MAKEFILE.read_text()
    sandbox_parity_m = re.search(
        r"^sandbox-parity:.*\n(?:.*\n)*?^\tcargo test -p hew-sandbox-wasm(.*)$",
        makefile_text,
        re.MULTILINE,
    )
    if not sandbox_parity_m:
        print(
            "error: could not find `sandbox-parity`'s `cargo test -p hew-sandbox-wasm` "
            "recipe line in Makefile.",
            file=sys.stderr,
        )
        return 1
    sandbox_parity_cmd = sandbox_parity_m.group(1)

    fail = 0
    pass_count = 0
    for binary, names in vm_dependent_binaries.items():
        for name in names:
            for profile, filter_value in (
                ("default", default_filter),
                ("ci", ci_filter),
            ):
                if is_excluded(filter_value, binary, name):
                    pass_count += 1
                    if verbose:
                        print(f"  ok  [profile.{profile}] excludes {binary}::{name}")
                else:
                    fail += 1
                    print(
                        f"  FAIL [profile.{profile}] does not exclude VM-dependent test "
                        f"{binary}::{name} (need `binary({binary})` or `test({name})` "
                        f"in .config/nextest.toml's [profile.{profile}] default-filter)",
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
        f"across {len(vm_dependent_binaries)} VM-dependent binary(ies)."
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
