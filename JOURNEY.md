# Journey

## 2026-04-09 — refactor/checker-collection-capability-dedup

- Chose to centralise the repeated structural `Ty` recursion in `hew-types/src/check/admissibility.rs` behind one helper that dispatches back into the existing Vec, HashSet, and HashMap validators at the named-collection boundary.
- Kept the lane checker-only on purpose: the duplication lived in admissibility checking, the behaviour contract already belonged to the checker, and widening the change into serializer or codegen would have mixed cleanup with unrelated ownership and lowering concerns.
- Preserved the existing aggregate validation shape by still running the concrete HashMap, HashSet, and Vec passes separately at the top level, because short-circuiting through one shared pass hid the more specific nested collection diagnostic that current behaviour expects.

## 2026-04-09 — fix/match-lowering-fail-closed

**Symptom.** `derefIndirectEnumScrutinee` used a redundant soft lookup (`resolvedTypeOf`) as a pre-gate before calling `requireResolvedTypeOf`, making the fail-closed boundary hard to read. Separately, the `ExprMatch` path through that function (reached via `generateMatchExpr → derefIndirectEnumScrutinee`) had no test; only the `StmtMatch` path was covered by `test_indirect_enum_match_missing_scrutinee_expr_type_fails_closed`.

**Root cause.** The original guard in `derefIndirectEnumScrutinee` was written as:
```
if (!resolvedTypeOf(span)) {
  requireResolvedTypeOf(span, "indirect enum scrutinee", location);
  return nullptr;
}
```
This calls `resolvedTypeOf` twice (once explicitly, once inside `requireResolvedTypeOf`). The pattern used everywhere else for fail-closed metadata is the single `requireResolvedTypeOf` call. The double-call form obscures the boundary intent and misleads a reader into thinking the outer check is optional.

**Decision.** Collapsed to a single `requireResolvedTypeOf` call, matching the canonical pattern. Added `test_indirect_enum_match_expr_missing_scrutinee_expr_type_fails_closed` exercising the `ExprMatch` (return-match) path with the scrutinee's `expr_types` entry absent.

**Discovered: two unrelated test failures.** `statement_style_match_arm_block_tail_if_fails_closed` and `statement_style_match_arm_block_tail_match_fails_closed` fail on main too. Noted in `.tmp/TODO.md` for follow-up; not in scope for this lane.
