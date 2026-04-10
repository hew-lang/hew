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

## 2026-05-xx — fix/bytes-stream-abi-authority

### Symptom

`for await item in <bytes-stream>` and `.map`/`.filter` on a bytes stream
could silently lower to the string ABI (`hew_stream_next` / `hew_stream_next`
/ `hew_stream_map_string`) when the `expr_types` map did not contain an
entry for the iterable or receiver span.  The bytes ABI requires
`hew_stream_next_bytes` / `hew_vec_free` and the bytes variants of map and
filter; using the string variants on a `Vec<u8>` buffer corrupts or leaks
it at runtime.

### Exact code path

`resolveStreamHandleInfo` (`hew-codegen/src/mlir/MLIRGen.cpp`) was the
single choke-point for stream ABI selection.  Its first action was:

```cpp
std::optional<StreamHandleInfo> resolvedInfo;
if (span) {
    if (const auto *resolvedType = resolvedTypeOf(*span))
        resolvedInfo = streamHandleInfoFromTypeExpr(*resolvedType);
}
```

`resolvedTypeOf` does a span-keyed lookup into `exprTypeMap`, which is
built from `program.expr_types` at codegen entry.  That map is populated
by the type checker, but only for expressions that the checker recorded —
aliased bindings, method-chain intermediates, and chained combinator calls
frequently had no entry.  When the lookup returned `nullptr`, `resolvedInfo`
was empty and the function fell through to tracked-binding or call-name
resolution.  When it returned a *stale or wrong* entry (e.g., a
`Stream<string>` type that leaked from an earlier function with the same
local name), `resolvedInfo` took priority over the explicitly tracked bytes
metadata.

The same pattern appeared in `resolveTuplePatternStreamHandleInfos` for the
tuple-value span, meaning `let (sink, input) = expr` could fail to mark
`input` as a bytes stream if `expr` had no `expr_types` entry and was not
a recognised `stream.bytes_pipe` call.

### Why the code was wrong

`resolvedTypeOf` is an *incidental* lookup — it works only when the checker
happened to record a type for that exact span.  Because span identity is
byte-offset-based and the checker does not guarantee coverage for every
sub-expression, the lookup is unreliable in three distinct ways:

1. **Absent:** The checker never wrote a type for that span → returns
   `nullptr` → no bytes metadata, silently falls to string ABI.
2. **Stale/wrong:** A type from a *different* function with the same
   span offsets occupies the map (possible when two functions are lowered
   with overlapping byte ranges after preprocessing) → wrong ABI.
3. **Contradictory:** The checker wrote `Stream<string>` for a span that
   MLIRGen already tracked as `Stream<bytes>` (from a `bytes_pipe` call
   earlier in the same function) → the `mergeInfo` helper used `resolvedInfo`
   as the *primary* source for function-call expressions, which could
   override correct tracked metadata.

### Design choice for the authority boundary

The fix removes `resolvedTypeOf` entirely from `resolveStreamHandleInfo` and
`resolveTuplePatternStreamHandleInfos`.  The authority hierarchy is now:

1. **Explicit type annotation** on the binding (`stmt.ty`) — highest trust,
   processed into tracked metadata via `rememberTrackedStreamHandleInfo`.
2. **Known C-ABI call name** — `resolveKnownCallStreamHandleInfo` recognises
   the bytes variants (`hew_stream_pair_stream_bytes`, `hew_stream_map_bytes`,
   `hew_stream_filter_bytes`) and string variants by name.
3. **Chained stream combinator** — `resolveChainedStreamHandleInfo` propagates
   element type from a tracked receiver through `.map`, `.filter`, `.take`.
4. **Tracked binding** — `lookupTrackedStreamHandleInfo` reads metadata
   recorded at the let/var site.
5. **No match → `std::nullopt`** — caller treats as string ABI (the safe
   default, since string streams are more common and their ABI never corrupts
   a bytes buffer by writing the wrong free function).

The `span` parameter was removed from the public signature because it had
no remaining purpose; removing it makes the contract explicit: "no
expression-type map lookups happen here."

The `mergeInfo` lambda was deleted; it existed solely to blend `resolvedInfo`
into the explicit results, and blending is no longer needed.

## 2026-04-10 — refactor/mlir-typehint-array-hashmap

- Goal: replace the array-literal and empty-`HashMap` MLIR lowering dependence on `pendingDeclaredType` with local explicit type hints. Fit: this stays inside the existing `MLIRGen` expression-lowering flow by threading optional MLIR type hints from let/var initializers into collection literal lowering instead of adding another side channel. Invariants preserved: typed `Vec` literals must still lower directly to `hew.vec`, empty typed `{}` must still lower to `hew.hashmap`, unresolved collection types must still fail closed, and unrelated sibling expressions must not inherit collection hints.
- Follow-up after review: collection lowerings still have to consume and clear `pendingDeclaredType` before recursively lowering nested expressions. Reason: explicit `typeHint` is only for the current collection expression; leaving the member state live lets nested builtins like `Vec::new()` capture an outer `Vec<Vec<T>>` hint instead of failing closed.
