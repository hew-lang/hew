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


def _profile_table_span(text: str, profile: str) -> tuple[int, int]:
    """Return the (start, end) byte offsets of [profile.<profile>]'s OWN
    direct-key body in `text` -- the text strictly between its
    `[profile.<profile>]` header line and the next TOML header line of any
    kind (a sibling profile's `[profile.<other>]`, or that same profile's
    own `[[profile.<profile>.overrides]]` array-of-tables entries).

    This mirrors real TOML table semantics without needing a TOML parser:
    once a header line re-opens ANY table (nested or not), the keys that
    follow no longer belong to the table declared by the PRECEDING header --
    they belong to whatever the new header just opened. So a table's own
    direct keys (like `default-filter`) can only ever appear between its own
    header and the very next header line, full stop. In this repo's
    .config/nextest.toml, every `default-filter` line for every profile is
    declared before that profile's first `[[profile.<name>.overrides]]`
    block, so bounding at "the next header line of any kind" is exactly
    right and does not require distinguishing top-level headers from nested
    ones.

    Raises SystemExit if `[profile.<profile>]` is not found at all (an
    absent profile TABLE is a more severe, out-of-scope error than an absent
    `default-filter` key within an existing one).
    """
    header_re = re.compile(rf"^\[profile\.{re.escape(profile)}\][ \t]*$", re.MULTILINE)
    header_m = header_re.search(text)
    if header_m is None:
        raise SystemExit(f"error: [profile.{profile}] not found in {NEXTEST_TOML}")
    body_start = header_m.end()
    next_header_m = re.search(r"^\[", text[body_start:], re.MULTILINE)
    body_end = body_start + next_header_m.start() if next_header_m else len(text)
    return body_start, body_end


def default_filter_line(profile: str) -> str:
    """Return the default-filter value for [profile.<profile>], or "" if that
    profile has no default-filter key at all.

    Looked up via `_profile_table_span`, which bounds the search to exactly
    the text between `[profile.<profile>]`'s own header and the next header
    line of any kind -- NOT a regex scan that starts after the profile's
    header and reads to end-of-file. That unbounded shape was a real bug:
    if `[profile.default]`'s own `default-filter` key were ever deleted,
    the unbounded scan would run past the end of `[profile.default]`'s
    table and match the NEXT profile's `default-filter` line instead (e.g.
    `[profile.ci]`'s), silently reporting default's binaries as excluded
    when they are not -- a false pass borrowed from a sibling profile.
    Bounding the search to `_profile_table_span`'s (start, end) cannot do
    this: nothing past the next header line is ever visible to the search,
    so there is no text left to borrow from a sibling profile.

    This deliberately avoids a full TOML parser (e.g. the stdlib `tomllib`
    module): `tomllib` requires Python 3.11+, and this repo's tooling
    baseline is Python 3.10, with no dependency or interpreter-provisioning
    change in scope here. A section-bounded regex scan is sufficient for
    this file's shape (flat `default-filter = "..."` string keys, no
    multi-line strings or nested inline tables holding that key) and keeps
    the fix within a single self-contained script with no new requirement.

    A profile with no `default-filter` key returns "" (the empty string
    excludes nothing), which the caller reports as an ordinary FAIL for
    every VM-dependent binary under that profile -- not a crash, and not a
    value silently sourced from a different profile.
    """
    text = NEXTEST_TOML.read_text()
    body_start, body_end = _profile_table_span(text, profile)
    body = text[body_start:body_end]
    filter_m = re.search(r'^default-filter\s*=\s*"([^"]*)"', body, re.MULTILINE)
    return filter_m.group(1) if filter_m else ""


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
