#!/usr/bin/env python3
"""Self-test for scripts/sys-lane-closure.py.

The closure gate is only worth having if it still FAILS. A checker that has
quietly become a no-op -- because a waiver over-matched, because a cfg
predicate hid half the tree, or because the call-graph builder stopped finding
edges -- reads exactly like a clean run. Every case below therefore drives the
real `main()` over a synthetic crate and asserts the specific failure, not just
"some" failure.

The tree is synthetic on purpose: pinning these cases to real runtime symbols
would make the self-test churn on every refactor and tempt the next person to
delete it.
"""

from __future__ import annotations

import importlib.util
import io
import sys
import tempfile
import textwrap
from contextlib import redirect_stderr, redirect_stdout
from pathlib import Path

SCRIPT = Path(__file__).resolve().parent.parent / "sys-lane-closure.py"

_spec = importlib.util.spec_from_file_location("sys_lane_closure", SCRIPT)
assert _spec is not None and _spec.loader is not None
closure_tool = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(closure_tool)


FAILURES: list[str] = []


def check(name: str, condition: bool, detail: str = "") -> None:
    if condition:
        print(f"ok   {name}")
        return
    FAILURES.append(f"{name}: {detail}" if detail else name)
    print(f"FAIL {name} {detail}")


def run_gate(tree: Path, classification: Path) -> tuple[int, str]:
    out, err = io.StringIO(), io.StringIO()
    with redirect_stdout(out), redirect_stderr(err):
        code = closure_tool.main(
            ["--scan-dir", str(tree), "--classification", str(classification)]
        )
    return code, out.getvalue() + err.getvalue()


def write(root: Path, rel: str, body: str) -> None:
    path = root / rel
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(textwrap.dedent(body), encoding="utf-8")


# A stable symbol three calls away from the lane, plus the shapes that have
# historically been mistaken for lane operations or hidden lane operations.
CRATE = """
    #[cfg(test)]
    mod unit_tests;

    pub unsafe extern "C" fn hew_toy_free(a: *mut Actor) {
        toy_free_inner(a);
    }

    fn toy_free_inner(a: *mut Actor) {
        toy_mailbox_free(a);
    }

    fn toy_mailbox_free(a: *mut Actor) {
        (*a).sys_queue.drain_and_free(None);
    }

    pub unsafe extern "C" fn hew_toy_spawn() -> *mut Actor {
        Box::into_raw(Box::new(Actor { sys_dispatch: None }))
    }

    #[cfg(any(target_arch = "wasm32", test))]
    pub unsafe extern "C" fn hew_toy_free_wasm(a: *mut Actor) {
        toy_mailbox_free(a);
    }

    pub unsafe extern "C" fn hew_toy_inert() {
        // A comment naming sys_queue must not mint a root.
        let _ = "sys_dispatch";
    }
"""

TEST_MODULE = """
    fn unit_test_helper(a: *mut Actor) {
        (*a).sys_queue.push(node);
    }
"""

BASE_TOML = """
    stable = ["hew_toy_free", "hew_toy_spawn", "hew_toy_free_wasm", "hew_toy_inert"]
    stable-stdlib = []
    codegen-stable = []
    internal = []
"""


def main() -> int:
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        tree = root / "src"
        write(tree, "lib.rs", CRATE)
        write(tree, "unit_tests.rs", TEST_MODULE)
        toml = root / "classification.toml"

        # 1. The gate fails on an unwaived transitive reach, and names the
        #    witness path rather than just the symbol.
        toml.write_text(textwrap.dedent(BASE_TOML), encoding="utf-8")
        code, output = run_gate(tree, toml)
        check("transitive reach fails the gate", code == 1, output)
        check(
            "witness path is printed",
            "hew_toy_free -> toy_free_inner -> toy_mailbox_free" in output,
            output,
        )

        # 2. `#[cfg(any(target_arch = "wasm32", test))]` is production wasm
        #    code, not a test item. Reading it as a test item is exactly how a
        #    wasm-only teardown path disappears from the audit.
        check(
            "cfg(any(wasm32, test)) is not treated as a test item",
            "hew_toy_free_wasm" in output,
            output,
        )

        # 3. `#[cfg(test)] mod x;` puts a whole FILE behind cfg(test); the
        #    attribute is in the parent, so the file cannot see it itself.
        check(
            "cfg(test) module files are excluded",
            "unit_test_helper" not in output,
            output,
        )

        # 4. Comments, strings, and `sys_dispatch: None` are not lane
        #    operations.
        check("prose does not mint a root", "hew_toy_inert" not in output, output)
        check(
            "sys_dispatch: None does not mint a root",
            "hew_toy_spawn" not in output,
            output,
        )

        # 5. An authenticated edge clears exactly the path it names -- and only
        #    for the caller it names.
        toml.write_text(
            textwrap.dedent(BASE_TOML)
            + "\n[sys-lane-closure.authenticated-edges]\n"
            + '"toy_free_inner -> toy_mailbox_free" = "test reason"\n',
            encoding="utf-8",
        )
        code, output = run_gate(tree, toml)
        check("waived caller clears", "hew_toy_free:" not in output, output)
        check(
            "a different caller of the same callee still fails",
            code == 1 and "hew_toy_free_wasm" in output,
            output,
        )

        # 6. A waiver with no reason is rejected outright.
        toml.write_text(
            textwrap.dedent(BASE_TOML)
            + "\n[sys-lane-closure.authenticated-edges]\n"
            + '"toy_free_inner -> toy_mailbox_free" = ""\n',
            encoding="utf-8",
        )
        try:
            run_gate(tree, toml)
            check("unreasoned waiver is rejected", False, "no error raised")
        except ValueError as exc:
            check(
                "unreasoned waiver is rejected", "has no reason" in str(exc), str(exc)
            )

        # 7. A waiver that no longer matches the code fails instead of sitting
        #    there silently widening the tier.
        toml.write_text(
            textwrap.dedent(BASE_TOML)
            + "\n[sys-lane-closure.authenticated-edges]\n"
            + '"toy_free_inner -> toy_mailbox_free" = "test reason"\n'
            + '"hew_toy_inert -> toy_mailbox_free" = "stale"\n',
            encoding="utf-8",
        )
        code, output = run_gate(tree, toml)
        check(
            "stale authenticated edge fails",
            code == 1 and "is not a call" in output,
            output,
        )

        toml.write_text(
            textwrap.dedent(BASE_TOML)
            + "\n[sys-lane-closure.non-roots]\n"
            + '"hew_toy_inert" = "stale"\n',
            encoding="utf-8",
        )
        code, output = run_gate(tree, toml)
        check(
            "stale non-root fails",
            code == 1 and "no longer names system-lane state" in output,
            output,
        )

        # 8. A clean tree passes -- so a green run means something.
        toml.write_text(
            textwrap.dedent(BASE_TOML)
            + "\n[sys-lane-closure.authenticated-edges]\n"
            + '"toy_free_inner -> toy_mailbox_free" = "test reason"\n'
            + '"hew_toy_free_wasm -> toy_mailbox_free" = "test reason"\n',
            encoding="utf-8",
        )
        code, output = run_gate(tree, toml)
        check("fully waived tree passes", code == 0, output)

    if FAILURES:
        print(f"\n{len(FAILURES)} check(s) failed", file=sys.stderr)
        return 1
    print("\nall sys-lane closure self-tests passed")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
