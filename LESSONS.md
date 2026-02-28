# Lessons Learned — Distributed Actor Infrastructure

## From the audit phase

### 1. Multi-model consensus produces higher-quality analysis

Deploying 4 different LLM models (GPT-5.3-Codex, Gemini 3 Pro, GPT-5.2, Sonnet 4.6)
on the same codebase with different audit angles produced complementary findings.
GPT-5.3-Codex was strongest on quantitative unsafe/duplication counting, Sonnet 4.6
produced the most actionable API contract analysis, GPT-5.2 gave the best end-to-end
distributed systems gap analysis, and Gemini 3 Pro focused well on code organization.

### 2. Typed MLIR ops obsolete string-based dispatch

The work done in PRs #5-#6 (codegen type gaps) created typed VecType/HashMapType
MLIR types that make the older string-based type dispatch (e.g., `collStr.rfind("Vec<", 0)`)
dead code. ~300 LOC of string parsing can be safely removed.

### 3. C-ABI null-guard boilerplate is the largest duplication source

314 instances of `if ptr.is_null() { return ...; }` across 47 files. A `#[macro_export]`
guard macro would eliminate this, but care is needed: the return values differ
(some return -1, some return null, some return 0).

### 4. The connection.rs dangling pointer is a real UB risk

`connection.rs:334` passes a pointer to a field of a struct that is subsequently
moved. The spawned reader thread may dereference a dangling pointer. This must
be fixed before any distributed actor work.

### 5. Wire send path triplication indicates a missing abstraction

The same ~90-line envelope encode+send logic appears in transport.rs, node.rs,
and connection.rs. This is a clear sign that "send an envelope to a remote actor"
should be a single function, not copy-pasted.

### 6. The msgpack schema boundary is the highest-risk seam

The Rust → C++ boundary via msgpack has no version, no sync mechanism, and
manually-mirrored types. Any AST change is a silent breakage. Adding a version
field and a CI test that round-trips a known blob is the minimum fix.

### 7. Node::\* builtins exist in the type checker but not in codegen

The type checker registers `Node::start`, `Node::shutdown`, `Node::register`,
`Node::lookup` — but the compiler never generates code for them. The example
file explicitly comments them out. This is a clear gap that needs bridging.

### 8. SWIM membership is isolated from everything

The cluster.rs SWIM implementation works in isolation but has no integration with:

- Connection management (how do SWIM messages travel?)
- Service discovery (how do membership changes affect routing?)
- Supervision (how do node failures affect supervised actors?)

### 9. Static analysis tools missed the dangling pointer

Despite running clippy and extensive testing, the `connection.rs:334` UB was
only caught by manual audit. This suggests we need more targeted safety
analysis for code that spawns threads with pointers.

### 10. Pre-existing test failures mask real issues

The 5 HashMap test failures (contains_key returning bool instead of i32)
existed on main for an unknown duration. Regular CI runs that exclude
certain test categories can hide regressions.

### 11. Agent-extracted code can re-introduce fixed bugs

When GPT-5.3-Codex extracted `generateBuiltinMethodCall` from the monolithic method-call
path, it re-introduced a `CmpIOp` for `contains_key` that had been explicitly removed in
PRs #5-#10. The agent didn't have context about the prior fix. Lesson: always run the full
test suite after agent-generated refactoring, and cross-check extracted code against recent
commit history for the same functions.

### 12. Integration layers beat rewrites

Phase 4 succeeded because HewNode was designed as an integration layer — it wires together
existing transport, cluster, connection, and registry modules rather than rewriting them.
Each sub-agent worked on a focused piece (handshake, routing, SWIM wiring) independently,
and they composed cleanly because the integration boundary was explicit.

### 13. Thread safety through ownership, not locks

The dangling pointer fix (Phase 1) and routing table (Phase 4) both succeeded by choosing
the right ownership model upfront: `Arc<AtomicU64>` for the heartbeat counter shared
between ConnectionActor and reader thread, `RwLock<HashMap>` for the routing table.
Fixing ownership is cheaper than adding locks after the fact.

### 14. Fixed-size wire formats eliminate parsing bugs

The 48-byte handshake format has zero variable-length fields, zero TLV parsing, and zero
ambiguity. It's trivially serializable/deserializable and can be validated in a single
comparison. This simplicity paid off immediately — the handshake tests are 100% reliable.

### 15. Test expectations must track semantic changes across branches

The HashMap tests expected `1`/`0` (I32) for contains_key, but the codegen now produces
`true`/`false` (I1) on some branches. Test expectations are branch-sensitive — when
semantic changes like return type modifications are in flight across branches, expected
outputs must be updated in the same commit as the semantic change.

### 16. Dual-identity fields create silent routing failures

Having two identity concepts (`pid` as counter, `id` as location-transparent address)
in the same struct is a trap. The C-ABI function `hew_actor_self_pid()` returned the
counter, and `hew_pid_node(counter)` always returned 0, making every actor appear local.
This is undetectable without cross-node testing — single-node tests pass because local
routing works with either value.

### 17. Security features must be wired end-to-end to be meaningful

The Noise XX handshake was generating ephemeral keys per-connection but never using the
node's persistent identity key. The peer allowlist existed but was never called. Both
features passed unit tests because the tests only checked that the code _existed_, not
that it was _integrated_. Integration tests that verify actual authentication and
rejection are essential.

### 18. Connection lifecycle events need explicit cleanup contracts

When a TCP reader loop exits, the connection is "dead" but nothing cleaned up: routing
table still had stale entries, SWIM still considered the peer alive, connmgr still
tracked it. Each subsystem needs explicit `on_disconnect` callbacks, and these must be
tested with simulated drops, not just graceful shutdowns.

### 19. Background agent file changes don't always persist

When dispatching sub-agents in background mode, their SQL database changes persist but
file modifications may not survive across context boundaries. Always verify file changes
on disk after agent completion. Trust SQL status for tracking but verify file diffs with
`git status`.

### 20. Thread-local error APIs are essential for C-ABI libraries

Returning `-1` from 34 C-ABI functions with no diagnostic is useless to callers.
A `thread_local!` last-error string (accessible via `hew_last_error() -> *const c_char`)
gives callers actionable diagnostics without global lock contention. The pattern mirrors
`errno`/`GetLastError` but with richer messages.

### 21. Every send path must handle remote PIDs — local-only is a silent failure

`hew_actor_send_by_id` searched only `LIVE_ACTORS` (local set). When a remote PID
was passed, it returned -1 silently — no error, no network attempt, no diagnostic.
This is the most dangerous kind of bug: all local tests pass, the API looks correct,
but distributed sends are dead. The fix is a two-line check: if local lookup fails
AND `hew_pid_is_local() == false`, forward to `try_remote_send`. Every function that
accepts a PID must handle both local and remote cases explicitly.

### 22. Global node handles enable location-transparent routing

A `static AtomicPtr<HewNode>` set during `hew_node_start` allows any function in the
runtime to route messages to remote nodes without requiring callers to pass a node
handle. This mirrors Erlang's model where `!` (send) works transparently regardless
of whether the target PID is local or remote. The tradeoff is one-node-per-process,
which is acceptable for the actor model.

### 23. Multi-model parallel analysis catches more gaps than single-model review

Running 5 different frontier models (Claude Opus, GPT-5.1/5.2 Codex, Claude Sonnet,
Gemini Pro) each analyzing a different layer (parser, types, codegen, runtime, examples)
found 25 gaps in ~5 minutes. Each model caught things others missed — Claude Opus was
best at parser/AST analysis, GPT-5.1 Codex excelled at type system subtleties, Claude
Sonnet 4.5 was thorough on codegen lowering gaps.

### 24. Visibility refactors must touch serialization boundaries

Changing `is_pub: bool` → `Visibility` enum required updating not just Rust code but
the serde serialization format AND the C++ msgpack reader in the codegen. The Rust
`#[derive(Serialize)]` changes key names automatically, but the C++ reader has
hardcoded key lookups. Multi-language serialization boundaries are high-risk refactor
points.

### 25. Unsafe enforcement in a language with runtime FFI requires audit of all tests

Adding `unsafe { }` enforcement for extern calls broke 12 e2e tests that directly
called runtime extern functions. In a language with a C-ABI runtime, many "stdlib"
functions are thin wrappers around extern calls. The correct fix is to wrap the extern
calls in `unsafe` in both the stdlib wrappers AND any tests that bypass the wrappers.

### 26. TraitObject vec migration must update codegen's dyn dispatch resolution

Changing `Ty::TraitObject { trait_name, args }` → `Ty::TraitObject { traits: Vec<TraitObjectBound> }`
impacts every consumer: type checker, unification, serialization, AND the C++ codegen's
dynamic dispatch resolution. The C++ side uses the trait name to look up vtable entries,
so using `bounds[0].name` preserves backward compatibility for single-trait objects.

### 27. Agent-generated code must be fixed for Rust borrow checker patterns

When multiple AI agents write Rust code in parallel, they often produce code that
conflicts with the borrow checker — holding `&self` borrows while calling `&mut self`
methods, or holding references into containers while modifying those containers.
The fix patterns are consistent: clone data before dropping the borrow, extract
values into locals, or restructure closures into if/else chains. This is a systematic
issue when agents don't have full context of surrounding code.

### 28. Associated type resolution requires `Self::Type` syntax in the parser

`Self::Item` as a type expression requires special handling in `parse_type()` —
after parsing `Self` as a named type, the parser must check for `::` and combine
into `Self::TypeName`. Without this, trait methods returning `Self::Item` fail
to parse with "expected `;`, found `::`" errors.

### 29. Cross-enum variant fallback is a type-safety hole

When match pattern resolution can't find a variant in the scrutinee's type, falling
back to a global search across all types is dangerous. It means `match color { Shape::Circle => }`
silently type-checks. The fix is simple: when the scrutinee type is known, only search
that type's variants.

### 30. String getters returning internal pointers create invisible use-after-free

C-ABI string getters (HashMap.get_str, Vec.get_str) that return pointers into
internal storage create lifetime hazards invisible to both the compiler and the
generated code. Returning strdup'd copies is safer, though it adds allocation cost.
The codegen must be aware that returned strings need freeing.

### 31. Multi-model code review catches different bug classes

Claude Opus caught parser inconsistencies and operator precedence issues. GPT-5.1
Codex found type coercion and unification bugs. Gemini found runtime memory safety
issues. Claude Sonnet found codegen UB and missing verification. Each model has
blind spots the others compensate for.

### 32. Match expressions and match statements need identical checks

When a language has both match-as-expression and match-as-statement, the
type checker must apply the same validation to both. The expression handler
had exhaustiveness checking but the statement handler didn't — an easy
oversight when the two codepaths diverged early.

### 33. Struct literal parsing requires explicit empty-struct lookahead

When `{}` after an identifier can mean either "empty struct literal" or
"block expression", the parser needs explicit handling for the empty case.
The standard lookahead of `ident: expr` inside braces fails when there are
no fields. Adding `peek == RightBrace` as a struct-init condition is the
minimal fix.

### 34. Drop ordering in return statements: evaluate first, then clean up

In a language with deterministic destruction, `return expr` must evaluate
`expr` while all locals are still alive, capture the result, THEN run
destructors. The natural code structure of "clean up, then return" causes
use-after-free when the return expression references locals. This is
especially subtle with method calls like `return vec.get(0)`.

### 35. Labeled control flow must deactivate ALL intermediate loops

When `break @outer` targets a non-adjacent loop, every loop between the
current position and the target must be deactivated. Only deactivating the
innermost and outermost leaves intermediate loops spinning. This requires
tracking loop depth indices, not just innermost/outermost references.

### 36. Self type in generic impls needs full type information

Storing `Self` as just a name string loses generic type parameters. In
`impl<T> Pair<T>`, `Self` must resolve to `Pair<T>`, not bare `Pair`.
The fix is storing `(name, args)` instead of just `name` — a lesson in
not discarding type information at storage boundaries.

### 37. Code review catches what tests miss at nesting depth

The labeled break test with 2-level nesting passed both with and without
the fix because normal scope cleanup handled both scopes. Only a
pragmatic code review identified that 3+ nesting was needed to exercise
the intermediate-scope drop logic. Tests should match the complexity of
the bug they're verifying.

### 38. Compound assignment must be checked everywhere assignment occurs

When adding compound assignment operators (`+=`, `-=`, etc.), every
assignment path must handle them: simple variable assignment, field
assignment, AND indexed assignment. The indexed case was missed because
VecSetOp takes the value directly — there's no separate "read-modify-write"
in the IR, so it must be manually synthesized.

### 39. Don't restrict core type coercions without understanding the language design

Adding an integer width restriction (i64→i32 rejected) seemed correct from
a type-safety perspective but broke the entire language because Hew's `int`
type is i64 and is used ubiquitously for array indices, loop counters, etc.
Understanding the language's design philosophy (convenience over strict
width typing) is critical before adding restrictions.

### 40. Double-evaluation in codegen is a category of bug, not a one-off

The HashSet arg double-evaluation is the same class of bug as any "generate
for type info, then generate again for the actual op" pattern. When codegen
inspects an expression to determine types, it must NOT call
generateExpression — use AST type info or pass the generated value through.

### 41. 100% test pass rate is achievable and meaningful

Going from 314/316 to 317/318 to 321/321 across sprints shows that
persistent, methodical quality work pays compound returns. Each sprint fixes
the bugs that the previous sprint's fixes revealed.
