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
