"""Regression tests for `scripts/viz/actor-topo.py`'s `RE_RECEIVE` regex.

The MLIR `hew.receive` op carries multiple attributes since
`handler_return_types` was added to support pass-ordering-safe ownership-
transfer decisions in the codegen lowering. The actor-topo visualizer
previously anchored its regex to `{handlers=[...]}` immediately before `}`,
which silently matched nothing once any additional attribute appeared in
the attr-dict. These tests pin the relaxed regex so the visualizer keeps
collecting dispatch handlers regardless of attribute ordering.

We deliberately extract the `RE_RECEIVE` source text and re-compile it in
this test process rather than importing `actor-topo.py` as a module. The
`scripts/viz/` directory ships an `ast.py` helper that shadows the stdlib
`ast` module under Python 3.13+ (a pre-existing import-shadowing bug
unrelated to this lane), making `import actor_topo` unreliable on newer
interpreters. Source-extraction targets the exact invariant the codegen
reviewer wants pinned — that the regex matches `hew.receive` ops
regardless of how `handlers` and `handler_return_types` are ordered in the
MLIR attr-dict — without depending on the visualizer's full import graph.
"""

from __future__ import annotations

import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "viz" / "actor-topo.py"


def _extract_re_receive() -> re.Pattern[str]:
    """Pull the RE_RECEIVE pattern source out of actor-topo.py and compile
    it in-process. Matches the multi-line `RE_RECEIVE = re.compile(...)`
    block exactly once; raises if the surface drifts so the test fails
    loudly instead of silently passing on a stale pattern."""
    src = SCRIPT.read_text()
    m = re.search(
        r"RE_RECEIVE\s*=\s*re\.compile\(\s*(?:#[^\n]*\n\s*)*r\"([^\"]+)\"\s*\)",
        src,
    )
    assert m is not None, (
        "RE_RECEIVE definition not found in actor-topo.py; the visualizer's "
        "regex surface drifted and this test no longer exercises it."
    )
    return re.compile(m.group(1))


RE_RECEIVE = _extract_re_receive()

HANDLERS_ONLY = (
    "hew.receive(%state, %msg_type, %data, %dsize) "
    '{handlers = [@"Counter::increment", @"Counter::get_name"]} '
    ": (!llvm.ptr, !llvm.ptr, i64)"
)
HANDLERS_THEN_RETURN_TYPES = (
    "hew.receive(%state, %msg_type, %data, %dsize) "
    '{handlers = [@"Counter::increment", @"Counter::get_name"], '
    "handler_return_types = [none, !hew.string_ref]} "
    ": (!llvm.ptr, !llvm.ptr, i64)"
)
RETURN_TYPES_THEN_HANDLERS = (
    "hew.receive(%state, %msg_type, %data, %dsize) "
    "{handler_return_types = [none, !hew.string_ref], "
    'handlers = [@"Counter::increment", @"Counter::get_name"]} '
    ": (!llvm.ptr, !llvm.ptr, i64)"
)


def _match_handlers(snippet: str) -> str | None:
    m = RE_RECEIVE.search(snippet)
    return m.group("handlers") if m else None


def test_handlers_only_baseline() -> None:
    """Baseline: the existing single-attr form keeps matching (no regression)."""
    got = _match_handlers(HANDLERS_ONLY)
    assert got is not None, f"RE_RECEIVE failed baseline: {HANDLERS_ONLY!r}"
    assert '@"Counter::increment"' in got
    assert '@"Counter::get_name"' in got


def test_handlers_followed_by_return_types() -> None:
    """`handler_return_types` appearing AFTER `handlers` must not break
    handler collection (the prior `}`-anchored pattern would silently
    fail here and the visualizer would drop the entire dispatch entry)."""
    got = _match_handlers(HANDLERS_THEN_RETURN_TYPES)
    assert got is not None, (
        "RE_RECEIVE failed to match when handler_return_types follows handlers; "
        "actor-topo would silently stop collecting dispatch handlers."
    )
    assert '@"Counter::increment"' in got
    assert '@"Counter::get_name"' in got


def test_handlers_preceded_by_return_types() -> None:
    """Attr-dict order is not guaranteed; ensure the regex matches when
    `handler_return_types` precedes `handlers` as well."""
    got = _match_handlers(RETURN_TYPES_THEN_HANDLERS)
    assert got is not None, (
        "RE_RECEIVE failed to match when handler_return_types precedes handlers"
    )
    assert '@"Counter::increment"' in got
    assert '@"Counter::get_name"' in got


if __name__ == "__main__":
    # Standalone runnable so the test can be exercised without pytest
    # in the worktree (matches the pattern used by the other
    # `scripts/tests/*.py` helpers).
    for name, fn in list(globals().items()):
        if name.startswith("test_") and callable(fn):
            print(f"-- {name}")
            fn()
            print("   PASS")
