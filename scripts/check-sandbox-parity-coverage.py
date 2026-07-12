#!/usr/bin/env python3
"""Assert every VM-dependent hew-sandbox-wasm test binary is covered coherently.

A hew-sandbox-wasm/tests/*.rs file is "VM-dependent" if ANY function in it
spawns the hew-sandbox-vm Node/npm toolchain (requires `node_modules` to be
provisioned) -- directly, through a same-file helper, or through some
indirection (a function pointer, closure, trait dispatch) this script does
not and cannot reliably trace.

Classification is BINARY-LEVEL, not per-test. An earlier version of this
checker tried to attribute VM-dependence to individual `#[test]` functions
via a same-file call graph, excluding only the tests it could statically
prove reached a VM-spawning function and letting the rest of the binary
keep running under generic (unprovisioned) nextest. That was unsound: the
call graph can prove one test reaches a spawning function, but it cannot
prove that OTHER tests in the same binary do not also reach the same
function through a path the graph does not model (a dispatch table, a
trait object, a closure passed through a combinator, a name matched at
runtime). A test that evades the static graph while still calling into the
VM at runtime would silently keep running unprovisioned and fail on any
platform that doesn't have `hew-sandbox-vm` set up -- exactly the failure
mode this script exists to prevent. Once a single VM-spawning function is
known to exist in a file, nothing short of "the whole binary is
VM-dependent" can be trusted.

The contract this script enforces:

  1. Every binary containing a VM spawn marker (see below) must be excluded
     WHOLE, by name (`binary(<name>)`, never a narrower `test(<name>)`),
     from the generic nextest default-filter in .config/nextest.toml's
     [profile.default], [profile.ci], AND [profile.lane] -- so plain
     `cargo nextest run --workspace` on any platform (macOS/Windows/FreeBSD/
     release-gate/`make test`), and the local `make test-lane`/
     `make test-lane-all`/`make test-fast` developer-iteration tier (all of
     which run `--profile lane`), never touch it.
  2. Every such binary must be named in the Makefile's `sandbox-parity`
     recipe's `cargo test -p hew-sandbox-wasm --test <name> ...` line, so
     the dedicated provisioned gate still runs every test in it (including
     any non-VM structural tests that binary happens to also contain --
     moving those out of the generic tier is an acceptable, correctness-
     driven consequence of binary-level containment, not a coverage loss:
     `make sandbox-parity` still runs them, every time, right after
     `sandbox-vm-deps` provisions the toolchain).

Marker detection: a file is considered VM-dependent if it contains the
literal substring `hew-sandbox-vm` anywhere -- the Node/npm toolchain
directory every real spawn path names (`ensure_parity_runner_built`,
`run_sandbox`, `run_sandbox_inline`, ...). This is a whole-file substring
search, not scoped to any parsed function body or #[test]: broad and
over-inclusive by design, since the only question this script answers
is "does this binary touch the VM anywhere", not "which test does".
Over-detecting a file as VM-dependent is safe (worst case its non-VM
tests move to `make sandbox-parity`, which still runs them);
under-detecting is the actual bug class this script exists to catch, and
per-test attribution has already been shown above to be unsafe.

Usage: python3 scripts/check-sandbox-parity-coverage.py [--verbose]
"""

from __future__ import annotations

import re
import sys
import tomllib
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
TESTS_DIR = REPO_ROOT / "hew-sandbox-wasm" / "tests"
NEXTEST_TOML = REPO_ROOT / ".config" / "nextest.toml"
MAKEFILE = REPO_ROOT / "Makefile"

VM_MARKER_STRING = "hew-sandbox-vm"

REQUIRED_PROFILES = ("default", "ci", "lane")


def extract_functions(text: str) -> dict[str, str]:
    """Return {fn_name: full_body_text} for every top-level `fn NAME(...) {...}`.

    Used only for --verbose diagnostics (naming which function(s) in a
    VM-dependent file actually spawn the toolchain) -- NOT for the pass/fail
    classification itself, which is a plain whole-file substring search (see
    module docstring for why per-function/per-test attribution is unsound).
    """
    functions: dict[str, str] = {}
    for m in re.finditer(r"\bfn\s+(\w+)\s*(?:<[^>]*>)?\s*\(", text):
        name = m.group(1)
        rest = text[m.end() :]
        brace_idx = rest.find("{")
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
        functions.setdefault(name, rest[brace_idx:i])
    return functions


def marker_functions_for_diagnostics(text: str) -> list[str]:
    functions = extract_functions(text)
    return sorted(name for name, body in functions.items() if VM_MARKER_STRING in body)


def is_vm_dependent(text: str) -> bool:
    return VM_MARKER_STRING in text


def default_filter_line(profile: str) -> str:
    """Return the default-filter value for [profile.<profile>], or "" if that
    profile has no default-filter key at all.

    Parsed via tomllib against the whole document, then looked up through
    the parsed `profile.<name>` TABLE -- not a regex scan that starts after
    the profile's `[profile.<name>]` header line and reads to end-of-file.
    That regex shape was a real bug: if `[profile.default]`'s own
    `default-filter` key were ever deleted, the unbounded scan would run
    past the end of `[profile.default]`'s table and match the NEXT
    profile's `default-filter` line instead (e.g. `[profile.ci]`'s),
    silently reporting default's binaries as excluded when they are not --
    a false pass borrowed from a sibling profile. A proper TOML parse
    cannot do this: `document["profile"]["default"]` is bounded to exactly
    that table's own keys, nothing more.

    A profile with no `default-filter` key returns "" (the empty string
    excludes nothing), which the caller reports as an ordinary FAIL for
    every VM-dependent binary under that profile -- not a crash, and not a
    value silently sourced from a different profile.
    """
    try:
        document = tomllib.loads(NEXTEST_TOML.read_text())
    except tomllib.TOMLDecodeError as exc:
        raise SystemExit(f"error: failed to parse {NEXTEST_TOML} as TOML: {exc}")
    profile_table = document.get("profile", {}).get(profile)
    if profile_table is None:
        raise SystemExit(f"error: [profile.{profile}] not found in {NEXTEST_TOML}")
    return profile_table.get("default-filter", "")


def excludes_binary(filter_value: str, binary: str) -> bool:
    return f"binary({binary})" in filter_value


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

    vm_dependent_binaries: list[str] = []
    for path in sorted(TESTS_DIR.glob("*.rs")):
        text = path.read_text()
        if is_vm_dependent(text):
            vm_dependent_binaries.append(path.stem)
            if verbose:
                markers = marker_functions_for_diagnostics(text)
                where = f" (spawn marker in: {', '.join(markers)})" if markers else ""
                print(f"==> {path.name} is VM-dependent{where}")

    if not vm_dependent_binaries:
        print(
            "error: no VM-dependent binaries found under hew-sandbox-wasm/tests/ "
            "-- detection likely broke; check is_vm_dependent() against the "
            "current test file contents.",
            file=sys.stderr,
        )
        return 1

    if verbose:
        print()

    filters = {profile: default_filter_line(profile) for profile in REQUIRED_PROFILES}
    sandbox_parity_cmd = sandbox_parity_test_flags()

    fail = 0
    pass_count = 0
    for binary in vm_dependent_binaries:
        for profile in REQUIRED_PROFILES:
            filter_value = filters[profile]
            if excludes_binary(filter_value, binary):
                pass_count += 1
                if verbose:
                    print(f"  ok  [profile.{profile}] excludes whole binary {binary}")
            else:
                fail += 1
                print(
                    f"  FAIL [profile.{profile}] does not exclude VM-dependent binary "
                    f"`{binary}` as a WHOLE binary (need `binary({binary})` in "
                    f".config/nextest.toml's [profile.{profile}] default-filter -- a "
                    f"narrower `test(<name>)` exclusion is not sufficient; see this "
                    f"script's module docstring for why per-test attribution inside a "
                    f"VM-touching binary cannot be trusted)",
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
        f"across {len(vm_dependent_binaries)} VM-dependent binary(ies): "
        f"{', '.join(vm_dependent_binaries)}."
    )

    if fail:
        print(
            "\nFAIL: a VM-dependent hew-sandbox-wasm binary is either not wholly "
            "excluded from generic nextest runs, or excluded but never run by "
            "`make sandbox-parity`.",
            file=sys.stderr,
        )
        return 1

    print(
        "     Every VM-dependent binary is wholly excluded from generic nextest "
        "runs (profile.default, profile.ci, and profile.lane) and fully covered "
        "by `make sandbox-parity`."
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
