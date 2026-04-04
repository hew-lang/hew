"""Unit tests for the hew_lldb._dotted_name_pattern helper.

These tests exercise the false-positive class caught by the code review:
  - A type whose name is a *prefix* of the requested type (PairList vs Pair)
    must NOT match the Pair.sum pattern.
  - A method whose name is a *prefix* of the requested method (summary vs sum)
    must NOT match the Pair.sum pattern.

The tests run without an LLDB installation by stubbing out the ``lldb``
module before importing ``hew_lldb``.

Run directly:
    python3 scripts/debug/test_hew_lldb_patterns.py
"""

import re
import sys
import types

# Stub out ``lldb`` so this file can be executed outside of an LLDB session.
if "lldb" not in sys.modules:
    sys.modules["lldb"] = types.ModuleType("lldb")

# Ensure we import the sibling module, not any installed package.
import os

sys.path.insert(0, os.path.dirname(__file__))
import hew_lldb  # noqa: E402


# ---------------------------------------------------------------------------
# Pattern-shape tests
# ---------------------------------------------------------------------------


def test_pattern_uses_exact_type_length():
    """T-prefix must carry the exact byte length of the type name."""
    pattern = hew_lldb._dotted_name_pattern("Pair", "sum")
    assert pattern.startswith("T4Pair"), f"got: {pattern!r}"


def test_pattern_uses_exact_method_length():
    """F-prefix must carry the exact byte length of the method name."""
    pattern = hew_lldb._dotted_name_pattern("Pair", "sum")
    assert "F3sum$" in pattern, f"got: {pattern!r}"


def test_pattern_anchored_at_end():
    """Pattern must be end-anchored to prevent method-suffix false-positives."""
    pattern = hew_lldb._dotted_name_pattern("Pair", "sum")
    assert pattern.endswith("$"), f"got: {pattern!r}"


# ---------------------------------------------------------------------------
# False-positive exclusion tests (the blocker class)
# ---------------------------------------------------------------------------


def test_prefix_type_not_matched():
    """PairList.sum symbol must NOT match the Pair.sum pattern.

    Before the fix the regex used T[0-9]+Pair which would match both
    T4Pair (Pair) and T8PairList (PairList).  With exact lengths,
    T4Pair never appears in _HM*T8PairList*, so the match is rejected.
    """
    pattern = hew_lldb._dotted_name_pattern("Pair", "sum")
    # Mangled symbol for PairList.sum in a hypothetical module
    symbol = "_HM6moduleT8PairListF3sum"
    assert not re.search(pattern, symbol), (
        f"pattern {pattern!r} wrongly matched prefix-type symbol {symbol!r}"
    )


def test_prefix_method_not_matched():
    """Pair.summary symbol must NOT match the Pair.sum pattern.

    Before the fix the regex used F[0-9]+sum which would match both
    F3sum (sum) and F7summary (summary).  With exact lengths,
    F3sum never appears in *F7summary, so the match is rejected.
    """
    pattern = hew_lldb._dotted_name_pattern("Pair", "sum")
    symbol = "_HM6moduleT4PairF7summary"
    assert not re.search(pattern, symbol), (
        f"pattern {pattern!r} wrongly matched prefix-method symbol {symbol!r}"
    )


# ---------------------------------------------------------------------------
# Positive match test
# ---------------------------------------------------------------------------


def test_exact_symbol_matched():
    """The exact Pair.sum mangled symbol must match the Pair.sum pattern."""
    pattern = hew_lldb._dotted_name_pattern("Pair", "sum")
    symbol = "_HM19dwarf_linked_binaryT4PairF3sum"
    assert re.search(pattern, symbol), (
        f"pattern {pattern!r} did not match expected symbol {symbol!r}"
    )


# ---------------------------------------------------------------------------
# Runner
# ---------------------------------------------------------------------------

_TESTS = [
    test_pattern_uses_exact_type_length,
    test_pattern_uses_exact_method_length,
    test_pattern_anchored_at_end,
    test_prefix_type_not_matched,
    test_prefix_method_not_matched,
    test_exact_symbol_matched,
]

if __name__ == "__main__":
    failed = 0
    for t in _TESTS:
        try:
            t()
            print(f"  PASS  {t.__name__}")
        except AssertionError as exc:
            print(f"  FAIL  {t.__name__}: {exc}")
            failed += 1
    if failed:
        raise SystemExit(f"\n{failed}/{len(_TESTS)} test(s) failed.")
    print(f"\nAll {len(_TESTS)} tests passed.")
