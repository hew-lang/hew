# Lessons Learned — Distributed Actor Infrastructure

## From the 2026-03-15 observe wiring pass

### 1. Complete tracing APIs are still dead code until the scheduler owns the boundaries

The tracing module already had a usable C ABI, but nothing meaningful happened until the
dispatch loop itself called begin/end around real message execution. Observability features
that span runtime components usually fail at the integration boundary, not in the leaf
module that implements the data structure or HTTP endpoint.

### 2. Trace context must ride with the mailbox payload, not with the worker thread

Worker threads are reused across unrelated actor activations, so thread-local context alone
is not enough for causal tracing. The durable handoff point is the mailbox node: capture
context when enqueuing, restore it when dequeuing, then derive the child dispatch span.

### 3. Profiler-backed UIs need activation semantics, not just endpoints

Serving `/api/traces` was not sufficient because tracing was still disabled by default.
For debugging tooling, tying trace activation to `HEW_PPROF` keeps the operator workflow
simple: one flag starts the HTTP server and turns on the event stream the UI expects.
## From the 2026-03-15 `hew test` hardening pass

### 1. Test discovery must never discard parser diagnostics

If a test runner ignores parse errors during discovery, a broken test file can be
misreported as "no tests found" and the command can exit 0. That is worse than a
visible failure because it creates false confidence in the suite.

### 2. Stable result order is part of test-runner trustworthiness

Even when the right tests run, a `HashMap` in the execution path makes output order
non-deterministic across files. Preserving discovery order keeps repeated runs easy
to compare and prevents flaky golden-output expectations.

### 3. Timeout behaviour needs a CLI seam to be testable

A hard-coded timeout works for users, but it makes automated regression tests slow
and awkward. Exposing `--timeout` made the feature easier to validate and improved
the real CLI at the same time.

## From the 2026-03-06 remediation passes

### 1. Validation targets must follow the real repo layout

If grammar validation only watches `examples/*.hew`, nested tutorial, benchmark, and
playground trees quietly fall out of coverage. Infra checks need to recurse over the
actual repository layout, not the layout that existed when the target was first added.

### 2. Generated-file drift checks must regenerate

`git diff --quiet` only answers whether a checkout is dirty; it does not answer whether
re-running the generator would change the output. A reliable `--check` path needs an
isolated snapshot, regeneration, and a real file comparison.

### 3. Serializer boundaries must fail closed

At the Rust→C++ msgpack boundary, `Ty` conversion must never fail open. If a type is
representable, serialize it explicitly (for example `()` as `TypeExpr::Tuple([])` and
supported builtin named forms like `Range` as `TypeExpr::Named`); if it is not
representable (`Ty::Var`, `Ty::Error`, generator shapes, or a nested unsupported path),
fail with context that identifies the span and nested conversion step instead of silently
omitting data.

## From the audit phase

### 1. Multi-model parallel analysis catches more than single-model review

Running 4-5 different frontier models on the same codebase with different audit angles
produces complementary findings that no single model catches alone. Each model has
characteristic strengths: Claude Opus excels at parser/AST structure and codegen
correctness; GPT Codex variants find type system subtleties and quantitative patterns;
Claude Sonnet catches codegen UB and memory safety; Gemini focuses on runtime safety
and code organization. Pragmatic code reviewers catch integration bugs that implementers
miss. Convergence between models on the same finding is a high-confidence triage signal.

### 2. Typed MLIR ops obsolete string-based dispatch

The work done in PRs #5-#6 (codegen type gaps) created typed VecType/HashMapType
MLIR types that make the older string-based type dispatch (e.g., `collStr.rfind("Vec<", 0)`)
dead code. ~300 LOC of string parsing can be safely removed.

### 3. C-ABI null-guard boilerplate is the largest duplication source

314 instances of `if ptr.is_null() { return ...; }` across 47 files. A `#[macro_export]`
guard macro would eliminate this, but care is needed: the return values differ
(some return -1, some return null, some return 0).

### 4. Wire send path triplication indicates a missing abstraction

The same ~90-line envelope encode+send logic appears in transport.rs, node.rs,
and connection.rs. This is a clear sign that "send an envelope to a remote actor"
should be a single function, not copy-pasted.

### 5. The msgpack schema boundary is the highest-risk seam

The Rust → C++ boundary via msgpack has no version, no sync mechanism, and
manually-mirrored types. Any AST change is a silent breakage. Adding a version
field and a CI test that round-trips a known blob is the minimum fix.

### 6. Static analysis tools missed the dangling pointer

Despite running clippy and extensive testing, the `connection.rs:334` UB was
only caught by manual audit. This suggests we need more targeted safety
analysis for code that spawns threads with pointers.

### 7. Agent-extracted code can re-introduce fixed bugs

When GPT-5.3-Codex extracted `generateBuiltinMethodCall` from the monolithic method-call
path, it re-introduced a `CmpIOp` for `contains_key` that had been explicitly removed in
PRs #5-#10. The agent didn't have context about the prior fix. Lesson: always run the full
test suite after agent-generated refactoring, and cross-check extracted code against recent
commit history for the same functions.

### 8. Integration layers beat rewrites

Phase 4 succeeded because HewNode was designed as an integration layer — it wires together
existing transport, cluster, connection, and registry modules rather than rewriting them.
Each sub-agent worked on a focused piece (handshake, routing, SWIM wiring) independently,
and they composed cleanly because the integration boundary was explicit.

### 9. Thread safety through ownership, not locks

The dangling pointer fix (Phase 1) and routing table (Phase 4) both succeeded by choosing
the right ownership model upfront: `Arc<AtomicU64>` for the heartbeat counter shared
between ConnectionActor and reader thread, `RwLock<HashMap>` for the routing table.
Fixing ownership is cheaper than adding locks after the fact.

### 10. Fixed-size wire formats eliminate parsing bugs

The 48-byte handshake format has zero variable-length fields, zero TLV parsing, and zero
ambiguity. It's trivially serializable/deserializable and can be validated in a single
comparison. This simplicity paid off immediately — the handshake tests are 100% reliable.

### 11. Background agent file changes don't always persist

When dispatching sub-agents in background mode, their SQL database changes persist but
file modifications may not survive across context boundaries. Always verify file changes
on disk after agent completion. Trust SQL status for tracking but verify file diffs with
`git status`.

### 12. Thread-local error APIs are essential for C-ABI libraries

Returning `-1` from 34 C-ABI functions with no diagnostic is useless to callers.
A `thread_local!` last-error string (accessible via `hew_last_error() -> *const c_char`)
gives callers actionable diagnostics without global lock contention. The pattern mirrors
`errno`/`GetLastError` but with richer messages.

### 13. Visibility refactors must touch serialization boundaries

Changing `is_pub: bool` → `Visibility` enum required updating not just Rust code but
the serde serialization format AND the C++ msgpack reader in the codegen. The Rust
`#[derive(Serialize)]` changes key names automatically, but the C++ reader has
hardcoded key lookups. Multi-language serialization boundaries are high-risk refactor
points.

### 14. Unsafe enforcement in a language with runtime FFI requires audit of all tests

Adding `unsafe { }` enforcement for extern calls broke 12 e2e tests that directly
called runtime extern functions. In a language with a C-ABI runtime, many "stdlib"
functions are thin wrappers around extern calls. The correct fix is to wrap the extern
calls in `unsafe` in both the stdlib wrappers AND any tests that bypass the wrappers.

### 15. Agent-generated code must be fixed for Rust borrow checker patterns

When multiple AI agents write Rust code in parallel, they often produce code that
conflicts with the borrow checker — holding `&self` borrows while calling `&mut self`
methods, or holding references into containers while modifying those containers.
The fix patterns are consistent: clone data before dropping the borrow, extract
values into locals, or restructure closures into if/else chains. This is a systematic
issue when agents don't have full context of surrounding code.

### 16. Cross-enum variant fallback is a type-safety hole

When match pattern resolution can't find a variant in the scrutinee's type, falling
back to a global search across all types is dangerous. It means `match colour { Shape::Circle => }`
silently type-checks. The fix is simple: when the scrutinee type is known, only search
that type's variants.

### 17. String getters returning internal pointers create invisible use-after-free

C-ABI string getters (HashMap.get_str, Vec.get_str) that return pointers into
internal storage create lifetime hazards invisible to both the compiler and the
generated code. Returning strdup'd copies is safer, though it adds allocation cost.
The codegen must be aware that returned strings need freeing.

### 18. Match expressions and match statements need identical checks

When a language has both match-as-expression and match-as-statement, the
type checker must apply the same validation to both. The expression handler
had exhaustiveness checking but the statement handler didn't — an easy
oversight when the two codepaths diverged early.

### 19. Drop ordering in return statements: evaluate first, then clean up

In a language with deterministic destruction, `return expr` must evaluate
`expr` while all locals are still alive, capture the result, THEN run
destructors. The natural code structure of "clean up, then return" causes
use-after-free when the return expression references locals. This is
especially subtle with method calls like `return vec.get(0)`.

### 20. Labeled break must deactivate ALL intermediate loops and their continue flags

When `break @outer` targets a non-adjacent loop, every loop between the current
position and the target must be deactivated — both the active flag and the continue
flag at each level. Only deactivating the innermost and outermost leaves intermediate
loops spinning, with middle loop bodies executing after inner loop exits. This requires
tracking loop depth indices, not just innermost/outermost references.

### 21. Self type in generic impls needs full type information

Storing `Self` as just a name string loses generic type parameters. In
`impl<T> Pair<T>`, `Self` must resolve to `Pair<T>`, not bare `Pair`.
The fix is storing `(name, args)` instead of just `name` — a lesson in
not discarding type information at storage boundaries.

### 22. Code review catches what tests miss at nesting depth

The labeled break test with 2-level nesting passed both with and without
the fix because normal scope cleanup handled both scopes. Only a
pragmatic code review identified that 3+ nesting was needed to exercise
the intermediate-scope drop logic. Tests should match the complexity of
the bug they're verifying.

### 23. Compound assignment must be checked everywhere assignment occurs

When adding compound assignment operators (`+=`, `-=`, etc.), every
assignment path must handle them: simple variable assignment, field
assignment, AND indexed assignment. The indexed case was missed because
VecSetOp takes the value directly — there's no separate "read-modify-write"
in the IR, so it must be manually synthesized.

### 24. Don't restrict core type coercions without understanding the language design

Adding an integer width restriction (i64→i32 rejected) seemed correct from
a type-safety perspective but broke the entire language because Hew's `int`
type is i64 and is used ubiquitously for array indices, loop counters, etc.
Understanding the language's design philosophy (convenience over strict
width typing) is critical before adding restrictions.

### 25. Double-evaluation in codegen is a category of bug, not a one-off

The HashSet arg double-evaluation is the same class of bug as any "generate
for type info, then generate again for the actual op" pattern. When codegen
inspects an expression to determine types, it must NOT call
generateExpression — use AST type info or pass the generated value through.

### 26. 100% test pass rate is achievable and meaningful

Going from 314/316 to 317/318 to 321/321 across sprints shows that
persistent, methodical quality work pays compound returns. Each sprint fixes
the bugs that the previous sprint's fixes revealed.

### 27. Lambda capture analysis must mirror AST traversal completeness

Every new expression type (ExprIfLet, ExprArrayRepeat) must be handled in
both `collectFreeVarsInExpr` and `collectFreeVarsInStmt`. Pattern-bound
variables must be added to the `bound` set before traversing sub-expressions.
Missing any expression type causes false captures (outer variables captured
when they shouldn't be) or missed captures (variables not captured when
they should be).

### 28. Type enrichment should return None for types the backend doesn't need

Mapping internal types (Unit, Generator, AsyncGenerator, Range) to
TypeExpr::Named causes "unresolved type" errors in C++ codegen. The correct
approach is to return None — the backend handles these via built-in type
logic, not through the expr_types map. Only map types that the C++ side
actually looks up.

### 29. Test all numeric type widths in lowering, not just the common ones

The PrintOpLowering and CastOpLowering both handled i32, i64, f64 but
missed f32. When adding a type-dispatching lowering pattern, check ALL
possible types in the language's type system, not just the most common ones.

### 30. Early returns must clean up all pending state

In MLIRGenStmt, `pendingDeclaredType` acts as a side-channel between
type annotation parsing and expression generation. If a `var` declaration
fails partway through, the early return must still reset this state —
otherwise the leaked type contaminates the next expression's type
resolution. Any codegen state stored in instance variables must be
cleaned up on all exit paths.

### 31. Exhaustive expression traversal prevents silent semantic gaps

`rewrite_builtin_calls_in_expr` only handled 9 of ~30 expression variants,
meaning built-in call rewriting (e.g., `len(x)` → `x.len()`) silently
failed inside interpolated strings, ranges, sends, timeouts, etc. When
adding a recursive expression visitor, enumerate ALL variants explicitly —
a `_ => {}` wildcard silently swallows new additions.

### 32. All loop codegen must use the same four-part pattern

Every loop in Hew codegen must: (1) AND its condition with `!returnFlag`,
(2) use `generateLoopBodyWithContinueGuards`, (3) use `MutableTableScopeT`,
(4) check for break/continue/return flow. The `loop {}` infinite loop
missed item 1, and stream/generator/for-await loops missed item 2.
Violating any item creates subtle flow control bugs.

### 33. Always verify AST node semantics before treating as "always true"

In `generateOrPatternCondition`, `PatIdentifier` can mean either a variable
binding (always matches) or an enum unit variant (needs tag comparison).
The initial fix blindly treated all PatIdentifier as always-true, which
made enum or-patterns like `Red | Blue` match unconditionally. The pragmatic
reviewer caught this before commit. When handling overloaded AST nodes,
always check contextual information (like `variantLookup`).

### 34. Symmetrical registration and cleanup prevents stale state

Every for-loop variant registered labeled loop flags but only `while` and
`loop` cleaned them up. The pattern of "register in setup, erase in cleanup"
must be systematically applied whenever a new loop codegen path is added.
A checklist pattern (setup label → use → cleanup label) prevents drift.

### 35. Every type-dispatching lowering must handle ALL supported types

ToStringOp, AssertOp, AssertEqOp, AssertNeOp, and VecNewOp all had if/else
chains that handled the "common" types (i32, i64, f64) but missed f32, i8,
and i16. When the dispatch chain doesn't match, values get passed to runtime
functions with the wrong ABI (f32 passed where f64 is expected, i8 passed
where i64 is expected). Every lowering pattern must enumerate ALL types:
i1, i8, i16, i32, i64, f32, f64, index, string_ref, pointer.

### 36. Capture analysis and builtin rewriting share the same completeness risk

Both `collectFreeVarsInExpr` (C++) and `rewrite_builtin_calls_in_expr` (Rust)
recursively traverse expression trees. Both had the same bug: a catch-all
`else` / `_ => {}` that silently skipped new expression variants. Any time
a new Expr variant is added to the AST, BOTH visitors must be updated.
Consider a compile-time assertion or exhaustive match to prevent silent gaps.

### 37. Suffix remapping creates a mismatch between value type and storage type

When `vecElemSuffixWithPtr` maps i1/i8/i16 to `_i32`, the inline fast path
must also remap the value and GEP element type to i32. The suffix determines
which runtime function is called (and thus the element stride), but the GEP
and load/store instructions use the MLIR value type. Both must agree on the
element size or pointer arithmetic will be wrong.

### 38. Parallel suffix functions must stay in sync

`vecElemSuffix` (for VecNew) and `vecElemSuffixWithPtr` (for push/get/set/pop)
are separate functions that map element types to runtime function suffixes.
When f32 support was added to `vecElemSuffixWithPtr` but not `vecElemSuffix`,
Vec<f32> was created with wrong element size. Any duplicated dispatch logic
must be updated in all copies simultaneously.

### 39. Pragmatic code reviewers catch integration bugs that unit tests miss

Sprint 13's pragmatic reviewer caught that Vec<f32> would crash even though
all 330 tests passed — because no test exercised Vec<f32>. Static analysis
by a reviewer found the inconsistency between two suffix functions that
unit tests couldn't cover. Always run a pragmatic reviewer after implementation
agents, especially for type-dispatch changes.

### 40. Build a coverage matrix, not an ad-hoc bug hunt

Sprints 5-13 found 60+ type-dispatch bugs reactively. Sprint 14 built a full
(operation × type) matrix by tracing every if/else chain in all 14 lowering
patterns, instantly revealing 4 critical bugs that 332 passing tests missed.
The matrix approach is exhaustive; the sprint approach has a long tail.

### 41. Operations without tests accumulate silent bugs

VecRemoveOp had zero type promotion logic across all sprints because no test
exercised `remove` with narrow types. Meanwhile, VecPush/Get/Set/Pop all got
fixed iteratively. Any operation not covered by a typed test will accumulate
the same class of bugs that other operations already had fixed.

### 42. Warnings for unhandled codegen cases should be errors

Silent warnings that skip match arms or fall through to default behaviour
cause miscompilation that's invisible at compile time and produces wrong
results at runtime. Use emitError + return failure, not emitWarning + skip.

### 43. Never/Error types must be excluded from match result type inference

When determining the overall type of a match expression, skip arms that
evaluate to Ty::Never (return, panic, break) or Ty::Error. Otherwise the
match is typed as Never, causing downstream inference to fail or accept
wrong types. Use the first non-diverging arm as the expected type.

### 44. Qualified type names need module-aware comparison

Two fully-qualified names from different modules (auth.User vs billing.User)
must NOT unify. Only bare-name vs qualified-name matching is allowed. This
prevents cross-module type confusion while still supporting imported types.

### 45. AST traversal functions must visit ALL child expressions

When adding enrichment/normalization traversals, every child expression of
every variant must be visited. Select arms have both source and body; both
must be traversed in enrich_expr, normalize_expr_types, and
rewrite_builtin_calls. Missing a child means that child's expressions
won't get their types normalized or their builtin calls rewritten.

### 46. HashMap.get() must return Option<T>, not raw T

Returning raw T from HashMap.get() and relying on match-time wrapping is
fragile — it only works for inline match expressions, not let-binding +
match patterns. Wrap at the expression level: check contains_key, then
construct Some(raw_get) or None via scf.IfOp. The internal values() method
can still use HashMapGetOp directly (raw value) since it iterates known keys.

### 47. Match fallthrough must trap, not return default values

When no match arm matches at runtime (non-exhaustive match), returning
zero/undef via createDefaultValue produces silently wrong results. Emit
hew.panic (or llvm.trap) instead. The createDefaultValue after the panic
is still needed for SSA type consistency but is unreachable code.

### 48. Exhaustiveness checking must cover ALL types

Integer, float, and string matches without a wildcard or binding pattern
should warn — not just enum/bool/Option/Result. The catch-all in
check_exhaustiveness must check for Pattern::Identifier (binding) as well
as Pattern::Wildcard, since both are catch-all patterns for non-enum types.

### 49. Vec push overflow requires checked arithmetic

All hew*vec_push\*\* functions compute len + 1 for the new length. If len
is usize::MAX, this silently wraps to 0, causing ensure_cap to not grow
and subsequent writes to corrupt memory. Use checked_add and abort on
overflow. Store old len for slot calculation before updating (*v).len.

### 50. Vec append requires type validation

hew_vec_append blindly memcpys src data into dst without checking that
elem_size and elem_kind match. Appending a Vec<f64> into a Vec<i32>
would silently corrupt the destination. Validate both fields match before
the copy, aborting if they differ.

### 51. Shared wire modifier logic prevents syntax drift

`#[wire] struct` and legacy `wire type` had duplicated field-modifier handling, which let
`since N` survive in one parser path but disappear in the other and in formatting. A tiny
shared parser helper plus shared formatter emission is cheaper than chasing AST drift after
downstream tools start depending on wire metadata.

### 52. Select losers need cancellation, not join-style destruction

`join` owns every reply channel all the way through `hew_reply_wait`, so it can destroy
each channel immediately. `select` only consumes one reply; destroying the losing channels
eagerly would risk use-after-free when a late actor reply arrives. The safe remediation is
to cancel non-winning channels explicitly (including timeout paths), then release only the
waiter-side reference and let the runtime's late-reply cleanup handle the final free.

### 53. Ask send failures need an explicit submit status

If an ask never makes it into a mailbox, `join`/`select` must learn that from the send
operation itself; otherwise they can wait forever on a reply that can never exist. An
explicit send status is safer than trying to infer failure later from reply payload shape.

### 54. Partial select/join setup needs prefix cleanup before panic

`select` and `join` build reply channels incrementally. When arm N fails to submit, arms
`0..N` already own live channels; panicking without canceling and destroying that prefix
still leaves leaked waiters and late-reply hazards. Clean up the full prefix first, then
panic.

### 55. Best-effort serializer omissions still need structured, deduplicated diagnostics

Some inferred `Ty` shapes (notably generator forms) are intentionally left implicit because
codegen tracks them through other mechanisms, but that does not justify a silent `None`
fallthrough. Carry span-tagged diagnostics out of enrichment/build passes and deduplicate
by span in the CLI so developers see the unsupported conversion exactly once.

### 56. Inferred type arguments must be persisted for downstream passes

The type checker successfully infers concrete type arguments via unification, but if those
resolved types are not written back into the AST (or a side-channel map), downstream
consumers like codegen only see `type_args: None` and cannot specialize. Storing inferred
type arguments in `TypeCheckOutput` and backfilling during enrichment keeps the codegen
simple — it only has to handle one path (explicit type args) instead of re-inferring
types at the MLIR level.
