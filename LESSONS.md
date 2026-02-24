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
