# Journey

## 2026-04-11 — test/wasm-diagnostic-notes-suggestions

- Certification lane: verify that `WasmDiagnostic.notes` and `WasmDiagnostic.suggestions` (introduced by PR #967) are covered by serialization contract tests.
- Gap confirmed: the struct fields existed at the base commit but no test asserted their presence or population in the JSON output.
- Added three focused unit tests in `hew-wasm/src/lib.rs`: structural field presence (both parse-error and type-error paths), suggestions populated for a mutability error, notes populated (with correct named fields) for a duplicate definition.
- All 19 `hew-wasm` tests pass.

## 2026-04-11 — fix/field-assign-codegen-invariant

- Symptom: `MLIRGenStmt.cpp` still surfaced user-facing backend diagnostics for field-assignment states the checker already rejects (`missing field`, `non-struct value field assignment`, and immutable value-struct roots), so a corrupted or stale `assign_target_kinds` path looked like a frontend error instead of an internal invariant failure.
- Root cause: the earlier assignment-authority hardening only converted part of the field/index fallback surface, leaving several field-only fail-closed branches with old backend wording even though `hew-types` already owns the target classification and field validity.
- Decision: keep the lane bounded to codegen hardening; rewrite the remaining checker-owned field-assignment fallbacks as explicit invariant diagnostics and add focused MLIRGen regressions that synthesize stale field-assignment metadata without relying on the frontend CLI.

## 2026-04-11 — fix/task-scope-cancelled-worker-reclamation

- Symptom: `hew_task_scope_destroy()` stayed bounded after cancelling a live running task, but `hew_task_scope_join_all()` marked that task `detached_on_cancel` and dropped its join handle, so destroy returned early forever and leaked the entire `HewTaskScope` task list.
- Root cause: bounded cancellation had no ownership handoff. Once the running cancelled task was detached there was no remaining thread responsible for eventually joining the worker and reclaiming the scope, so every task-local allocation stayed live until process exit.
- Decision: keep `hew_task_scope_join_all()` bounded by leaving cancelled-running worker handles attached to their tasks, then have `hew_task_scope_destroy()` move any remaining detached handles plus the boxed scope into a background reaper thread that joins those workers before calling the normal task/scope free path.
- Follow-up after local review: when a cancelled worker races from `Running` to `Done` inside `join_all()`, the runtime must still take/drop its join handle immediately so the existing done-signal wait remains the synchronization barrier; otherwise destroy can free the scope before the worker finishes reading `done_signal`.

## 2026-04-11 — fix/astgen-u32-overflow-guards

- Symptom: `make astgen` regenerated `hew-codegen/src/msgpack_reader.cpp` without the exact `uint32_t` overflow guard, which made `test_schema_version_u32_overflow_rejects` fail even though the generated-reader freshness gate passed.
- Root cause: the source-of-truth generator had regressed to emitting `static_cast<uint32_t>(getUint(...))` for `RustType::U32`, and the special-cased `parseProgram` schema-version parser duplicated the same unchecked narrowing outside the generic path.
- Decision: restore one checked `getUint32` helper in the shared helper preamble, route generated `u32` field/vector parses through it, and make the special-cased schema-version parser use the same fail-closed helper before regenerating the checked-in reader.

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

## 2026-04-10 — fix/module-qualified-stdlib-rewrite-metadata

- Moved module-qualified stdlib C-symbol rewrite authority into `hew-types`: `check_method_call` now records direct-call rewrite metadata for imported registry-backed stdlib calls instead of leaving `hew-serialize` to re-resolve them from `ModuleRegistry`.
- Kept the new rewrite metadata distinct from receiver-injecting method rewrites so `hew-serialize::enrich_method_call` can rewrite `json.parse`, `os.pid`, and `fs.read_file` straight to `Expr::Call` without prepending the module identifier as an argument.
- Left the serializer's registry lookup in place only as a backwards-compat fallback, and added focused checker + serializer regressions so the boundary stays checker-authoritative.

## 2026-04-10 — refactor/mlir-typehint-array-hashmap

- Goal: replace the array-literal and empty-`HashMap` MLIR lowering dependence on `pendingDeclaredType` with local explicit type hints. Fit: this stays inside the existing `MLIRGen` expression-lowering flow by threading optional MLIR type hints from let/var initializers into collection literal lowering instead of adding another side channel. Invariants preserved: typed `Vec` literals must still lower directly to `hew.vec`, empty typed `{}` must still lower to `hew.hashmap`, unresolved collection types must still fail closed, and unrelated sibling expressions must not inherit collection hints.
- Follow-up after review: collection lowerings still have to consume and clear `pendingDeclaredType` before recursively lowering nested expressions. Reason: explicit `typeHint` is only for the current collection expression; leaving the member state live lets nested builtins like `Vec::new()` capture an outer `Vec<Vec<T>>` hint instead of failing closed.

## 2026-04-10 — docs/fix-wire-codec-methods

- Re-audited the wire codec method names against `hew-types/src/check/registration.rs` instead of preserving the older aspirational `encode_hbf` / `decode_hbf` / `encode_json_pretty` surface from the spec.
- Documented the current stream codec boundary in `docs/specs/HEW-SPEC.md` as fail-closed rather than runtime-available, because `hew-types/src/check/methods.rs` rejects `Stream.decode()` and `Sink.encode()` as unlowerable.
- Left RcFree and collection-surface doc cleanup out of this lane on purpose so the wire codec spec correction stays small and matches the separate planner lane split.

## 2026-04-10 — refactor/ty-subst-dedup

- Verified the checker-owned and method-resolution `substitute_named_param` walkers matched in variant coverage and replacement guards before deduplicating them.
- Moved named-parameter substitution and inference-variable detection onto `hew-types/src/ty.rs` so all structurally identical `Ty` recursion now has one owner in the type model.
- Kept the lane behaviour-preserving and `hew-types`-local: call sites now dispatch through `Ty` methods, but the recursive walk itself is unchanged.

## 2026-04-10 — fix/iflet-whilelet-pattern-contract

- Discovered that the checker accepted all pattern kinds (Struct, Tuple, Or, Literal) at the top level of `if let` / `while let`, but codegen only handled `PatConstructor`. The mismatch meant unsupported patterns silently fell through to the `else` branch, producing no output and no error.
- Added `reject_unsupported_iflet_pattern` to `hew-types/src/check/diagnostics.rs`: emits an `InvalidOperation` diagnostic for Struct, Tuple, Or, and Literal patterns; returns false (allowed) for Wildcard, Identifier, and Constructor.
- Called the guard at four call sites: `synthesize_iflet` in expressions.rs, and three `check_stmt`/`check_stmt_as_expr` sites in statements.rs for IfLet and WhileLet.
- Restructured `MLIRGenIfLet.cpp` and `MLIRGenWhileLet.cpp`: PatWildcard generates an unconditional scf.if/loop body; PatIdentifier generates the same plus binds the scrutinee via `declareVariable`; PatConstructor keeps the existing deref + tag-test logic (deref moved inside this branch only); the else arm now increments `errorCount_` and emits an explicit error instead of silently returning.
- Key insight: `derefIndirectEnumScrutinee` is only semantically required for the constructor tag-test path. Wildcard and identifier need the raw value, not the dereffed payload. Moving deref inside the constructor branch prevented a double-deref regression.
- Checker tests (9 cases) verify that Literal/Tuple/Or patterns are rejected and Wildcard/Identifier patterns produce no `InvalidOperation` error for both `if let` and `while let`.
- Codegen tests (7 cases) verify that Wildcard and Identifier patterns lower correctly for stmt, expr, and while-let stmt forms; two fail-closed tests inject a `PatTuple` into a type-checked AST to confirm that `module == nullptr` and `errorCount_ > 0` after generation.
