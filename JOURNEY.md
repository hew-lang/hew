# Distributed Actor Infrastructure — Journey Log

## Phase 7: Delete dead `hew-types` helpers (2026-03-24)

### Goal

Remove `hew-types` helper APIs that no production checker code calls anymore while
keeping the remaining error-formatting tests focused on observable behaviour.

### Decisions

- Verified the targeted `TypeError` convenience constructors with a caller search
  across `hew-types/src/`; every call site was inside `error.rs` tests, so the
  helpers were dead in production.
- Verified `ModuleRegistry::is_unqualified_handle_type` and
  `is_unqualified_drop_type` the same way; both methods were only exercised by
  tests that existed solely to prove those helpers themselves worked.
- Kept the display-oriented `TypeError` tests, but rewrote them to build errors
  through `TypeError::new(...)` so they still cover formatting behaviour instead
  of the removed helper APIs.

### Validation

- `cargo test -p hew-types`
- `cargo clippy -p hew-types -- -D warnings`

## Phase 7: C string helper deduplication (2026-03-24)

### Goal

Remove the WASM bridge's private lossy C-string helper if the shared ABI helper
crate can carry the same behaviour without adding new coupling.

### Findings

- `hew-runtime` already depends on `hew-cabi`, so this refactor does not add a
  new crate edge or create a cycle.
- The existing helpers were close but not identical: `hew-cabi::cstr_to_str`
  rejects invalid UTF-8 with `None`, while `hew-runtime::bridge::cstr_to_string`
  intentionally keeps lossy conversion and turns null pointers into empty
  strings.

### Decision

Add `hew_cabi::cstr_to_string_lossy` with the bridge's existing semantics and
switch the bridge metadata loader to call it. This removes the duplicate helper
without changing how actor metadata names and type strings are decoded.

## Phase 7: Deduplicate hew-wasm analysis scaffold (2026-03-24)

### Goal

Remove the repeated parse-plus-type-check setup in `hew-wasm/src/lib.rs`
without changing any analysis behaviour.

### Decisions

- Extracted a single `parse_and_type_check()` helper that always returns the
  parsed program plus optional type-check output.
- Kept the helper local to `hew-wasm/src/lib.rs` instead of creating a shared
  module because the duplication exists entirely within one file.
- Avoided a configuration flag for type-checking because every repeated call
  site already wanted the same "type-check only after a clean parse" behaviour.

## Phase 7: Prune dead CLI surface (2026-03-24)

### Goal

Remove CLI flags and subcommands that are parsed or advertised but have no live behaviour.

### Verification

- Read the cited `hew-cli` and `adze-cli` call sites before editing.
- Confirmed `hew --Werror` only set `CompileOptions.werror` and was never consumed.
- Confirmed `adze init --bin` was parsed but discarded before template selection.
- Confirmed `adze namespace list` was wired to a TODO placeholder instead of a real implementation.

### Decisions

- Delete the dead surface instead of keeping compatibility shims so help text, completions,
  and docs match the real CLI.
- Treat the Hew spec note about `--Werror` as stale documentation and update it to match
  current fatal type-error behaviour.

## Phase 8: adze-cli gitignore helper deduplication (2026-03-24)

### Goal

Remove the duplicated `.gitignore` append logic in `adze-cli` so init and install
share one helper and future changes only need to touch one code path.

### Decision

- Keep `write_init_gitignore()` as the higher-level init helper because it expresses
  the two-entry contract for new projects.
- Reuse `ensure_gitignore_entry()` for the install path instead of maintaining a
  second `.adze/`-specific helper.
- Retarget the focused tests at the shared helper and add coverage for the trimmed
  line match so the deduplication preserves existing behaviour.

## Phase 9: Slim stdlib packaging (2026-03-15)

### Goal

Shrink release distributions by removing runtime object duplication from every
stdlib static library before assembling tarballs.

### Rationale

- Cargo `staticlib` archives embed transitive object files, so every
  `libhew_std_*.a` was shipping runtime support objects that were already
  present in `libhew_runtime.a`.
- The linker flags that suppressed duplicate definitions were hiding that waste
  and could also mask real symbol-collision bugs.
- Repacking each stdlib package as a thin archive over only its non-runtime
  objects keeps runtime-first link order unchanged while making release
  artifacts much smaller.

## Phase 8: Runtime observe wiring (2026-03-15)

### Goal

Replace hew-observe demo-only Messages, Timeline, Supervisors, and Crashes data with
live runtime data from the actor scheduler and profiler HTTP server.

### Changes made

- Wired `hew_trace_begin`/`hew_trace_end` into the scheduler dispatch loop so each
  dispatched message now produces real span timing events.
- Captured trace context in mailbox nodes at enqueue time and restored it before
  dispatch so trace IDs survive actor-to-actor message hops.
- Emitted lifecycle events for actor spawn, crash, stop, and message send from the
  runtime paths that already own those transitions.
- Enabled tracing automatically when `HEW_PPROF` starts the profiler so
  `hew-observe --addr ...` shows data without an extra tracing flag.
- Added `/api/supervisors` and `/api/crashes` endpoints and taught hew-observe to
  fetch them instead of relying on demo placeholders.

### Validation

- `cargo fmt --all --check`
- `make lint`
- `make test`
- Built a temporary Hew workload with a supervised counter that crashes once, then:
  - confirmed `/api/traces` returns live events
  - confirmed `/api/supervisors` returns the real tree
  - confirmed `/api/crashes` returns the recorded crash
  - connected `hew-observe` and captured live Supervisors, Crashes, Messages, and
    Timeline panes showing runtime-backed data
## Phase 0.5: `hew test` hardening audit (2026-03-15)

### What I audited

- Read the full `hew-cli/src/test_runner/` implementation and `hew-cli/src/main.rs` wiring.
- Exercised `hew test` manually against passing, failing, compile-error, timeout,
  empty-directory, no-test-function, nested-directory, and malformed-source cases.
- Added focused Rust tests plus CLI end-to-end coverage for the test runner itself.

### Findings

- **Silent parse failures:** malformed `_test.hew` files were reported as
  "No test functions found." with exit code 0 because discovery discarded parser
  diagnostics.
- **Non-deterministic execution order:** discovered tests were grouped in a
  `HashMap`, so multi-file runs printed results in unstable order.
- **Ambiguous empty-suite reporting:** an empty directory and a valid file with no
  `#[test]` functions produced the same message.
- **Timeouts existed but were fixed at 30 seconds:** the runner supported timeouts
  internally, but there was no CLI control, which made the behaviour harder to
  exercise and slower to test.

### Decisions

- Treat parser errors during discovery as fatal test-runner diagnostics and exit
  non-zero before executing the suite.
- Preserve discovery order all the way through execution and reporting so repeated
  runs are stable and trustworthy.
- Distinguish "No test files found." from "No test functions found." for clearer UX.
- Add `--timeout <seconds>` so timeout behaviour is explicit, configurable, and
  practical to test end-to-end.

### Implementation summary

- `discover_tests_in_file()` now returns both discovered tests and parser diagnostics.
- `hew test` now renders parse diagnostics with source spans and exits 1 on parser errors.
- The runner now preserves file/test order instead of relying on `HashMap` iteration.
- Output formatting gained render helpers so unit tests can assert on exact text and `JUnit`.
- Added end-to-end CLI tests for passing, failing, mixed, parse-error, and timeout suites.

## Phase 0: Multi-Agent Audit (2026-02-24)

### Agents deployed

- **GPT-5.3-Codex** → Runtime quality audit (stability, duplication, unsafe patterns)
- **Gemini 3 Pro** → Codegen duplication and simplification audit
- **GPT-5.2** → Distributed actor infrastructure gap analysis
- **Sonnet 4.6** → API contracts and module boundary audit

### Key findings across all agents

**Runtime (33,851 LOC across 69 files):**

- 2,375 unsafe blocks; 796 without SAFETY documentation
- 314 instances of duplicated C-ABI null-guard pattern across 47 files
- Wire envelope send path duplicated in 3 modules (~90 lines each)
- Critical: `connection.rs:334` passes stack pointer to spawned thread → potential UB
- Noise XX encryption exists but has no key management, identity, or authorization
- SWIM cluster membership exists but is not wired to connection management or routing
- Local name registry exists but no cross-node propagation

**Codegen (C++):**

- ~200 LOC of duplicated string-based type dispatch (now superseded by typed MLIR ops)
- Large monolithic functions (generateCallExpr, generateForStmt) need decomposition
- ~300 LOC of dead string-parsing code removable

**Distributed actors:**

- Transport: TCP/Unix with length-prefixed framing, pluggable vtable
- Security: Noise XX only, no identity/auth model
- Discovery: SWIM gossip exists but isolated from routing
- Supervision: Local trees work; remote_sup.rs is a skeleton
- Node API: Type checker declares Node::\* builtins but codegen doesn't lower them
- Example explicitly marks distribution as "FUTURE FEATURE"

**API contracts:**

- 🔴 Unversioned msgpack schema between Rust serializer and C++ codegen
- 🔴 Hardcoded HewActor byte offsets (MAILBOX_OFFSET=48) in bridge.rs
- 🔴 ast_types.h manually mirrors ast.rs with no sync mechanism
- No formal ABI specification for stdlib FFI

### Design iteration 1: Scope definition

Based on agent consensus, the work divides into two tracks:

**Track A — Runtime Hardening (pre-production quality)**

1. Fix critical safety issues (connection.rs dangling pointer, undocumented unsafe)
2. Deduplicate C-ABI boilerplate with macros
3. Unify wire send/recv paths
4. Add safety documentation to all unsafe blocks
5. Version the msgpack schema

**Track B — Distributed Actor Infrastructure**

1. Unify node runtime (transport + connmgr + cluster + registry)
2. Connection handshake protocol (version, node ID, schema hash, features)
3. Node API codegen lowering (Node::start/register/lookup)
4. Security: key management, peer authorization, allowlisting
5. Remote actor lifecycle (spawn, monitor, restart)
6. Actor pools with routing strategies

### Design iteration 2: Architecture decisions

**Decision 1: Single Node struct as integration point**
Rather than keeping transport.rs, connection.rs, cluster.rs, and registry.rs as
independent modules, we unify them under a `HewNode` that owns all distributed
state. The Node is created by `Node::start(addr)` and torn down by `Node::shutdown()`.

**Decision 2: Connection handshake before actor traffic**
Every new connection exchanges:

```
HandshakeReq {
  protocol_version: u32,    // HBF version
  node_id: u16,             // PID node component
  schema_hash: u64,         // FNV hash of wire schema
  features: u32,            // bitfield: encryption, compression
  static_key: [u8; 32],     // Noise static public key (if encrypted)
}
HandshakeResp {
  protocol_version: u32,
  node_id: u16,
  schema_hash: u64,
  features: u32,
  static_key: [u8; 32],
  status: u8,               // 0=ok, 1=version_mismatch, 2=schema_mismatch
}
```

**Decision 3: Route by (node_id, actor_id), not by connection handle**
Actor references encode `(node_id, actor_id)`. The node maintains a routing table
`node_id → conn_id`. References survive reconnections.

**Decision 4: Security model — Noise XX + allowlist**

- Every peer has a static Noise keypair (generated or loaded from file)
- Nodes maintain an allowlist of trusted static public keys
- Noise XX handshake authenticates both sides
- Post-handshake: all traffic encrypted with ChaChaPoly
- Future: SPIFFE integration as an alternative identity source

**Decision 5: Service discovery via gossip**

- Extend SWIM gossip to carry service registrations
- `Node::register(name, actorRef)` publishes to local + gossip
- `Node::lookup(name)` checks local registry, then queries cluster
- Registrations have TTL; nodes must re-register periodically

### Design iteration 3: Implementation plan

The implementation is split into 7 phases, with each phase independently
testable and committable.

---

## Phase 1: Critical Safety Fixes

**Commit:** b5d7481

- **Dangling pointer UB fixed** in connection.rs: Replaced raw `AtomicU64` pointer passed
  to spawned reader thread with `Arc<AtomicU64>`. The original code captured a pointer to a
  stack-local field, then moved the owning struct, creating a dangling pointer. GPT-5.3-Codex
  agent identified and fixed this with minimal restructuring.
- **Hardcoded byte offsets replaced** in bridge.rs: `MAILBOX_OFFSET=48` and
  `ACTOR_STATE_OFFSET=56` replaced with `std::mem::offset_of!(HewActor, ...)` plus
  compile-time assertions. Gemini 3 Pro agent handled this cleanly.
- **Wire send path unified**: Extracted `wire_send_envelope()` in transport.rs, eliminating
  ~90 LOC of triplicated envelope encode+send logic from node.rs and connection.rs.
- **SAFETY documentation added** to critical unsafe blocks in string.rs, vec.rs, actor.rs.

## Phase 2: Code Deduplication

**Commit:** 8162756

- **`cabi_guard!` macro** created to replace null-check boilerplate: applied to 43+ sites
  across vec.rs, string.rs, hashmap.rs.
- **TCP/Unix accept loops** unified via `accept_with_optional_timeout` helper in transport.rs.
- **Tagged-union module** created (`tagged_union.rs`) factoring shared Option/Result
  construction/destruction patterns.
- Bonus: `cargo fmt` applied across workspace.

## Phase 3: Codegen Cleanup

**Commit:** cc1f1bc

- **Extracted `generateBuiltinMethodCall`** from monolithic method-call path in
  MLIRGenExpr.cpp. Clean function boundary using `std::optional<mlir::Value>`.
- **Split `generateForStmt`** into `generateForRange`, `generateForVec`,
  `generateForString`, `generateForHashMap` helpers in MLIRGenStmt.cpp.
- **Removed ~270 LOC dead string-based type dispatch** fallbacks superseded by typed MLIR ops.
- Caught and fixed a regression where the extraction agent re-introduced a `CmpIOp` for
  contains_key that changed the return type from i32 to i1.

## Phase 4: Unified Node Runtime

**Commit:** 5e00c71

- **HewNode struct** created in `hew_node.rs` — integrates transport, connection manager,
  SWIM cluster, and registry into a single coherent entry point.
- **48-byte connection handshake** protocol: magic/version/node_id/schema_hash/feature_flags/
  noise_key. Automatic Noise XX upgrade when both peers support encryption.
- **PID-based routing table** in `routing.rs`: thread-safe `RwLock<HashMap<u16, c_int>>`
  mapping node_id → connection. Messages routed by destination PID, surviving reconnections.
- **SWIM ↔ connection lifecycle bridge**: connection lost triggers suspect, connection
  established triggers alive. Membership callbacks for event-driven integration.

## Phases 5-6: Node API, Security & Identity

**Commit:** c03dd10

- **Runtime manifest updated**: 9 `hew_node_*` functions added for codegen integration.
- **Integration tests**: Node lifecycle, local registry, routing table, two-node connect.
- **Noise key management**: `hew_noise_keygen/key_save/key_load` with X25519 keypairs,
  raw 64-byte file format, 0o600 permissions on Unix.
- **Peer allowlist**: `HewPeerAllowlist` with Open/Strict modes, integrated into Noise
  handshake for post-authentication peer verification.

## Phase 7: Supervision & Pools

- **Actor pools**: Round-robin and random routing strategies via `HewPool`.
- **Remote supervisor**: `remote_sup.rs` wired to unified node lifecycle.
- **Distributed tests**: Multi-node spawn, supervision tree, pool routing.

## Phase 8: v2 Audit & Hardening (2026-02-25)

### Second-round multi-agent audit

Deployed 4 audit agents (GPT-5.3-Codex, GPT-5.2, Sonnet 4.6, Gemini 3 Pro) against
the Phase 7 codebase. Found critical issues the first round missed:

**Critical findings:**

- **PID/id dual identity**: `HewActor.pid` was a plain counter while `id` was
  location-transparent `(node_id<<48|serial)`. `hew_actor_self_pid()` returned the
  counter, breaking all cross-node routing. Fixed by unifying: `pid = id` at all 6
  spawn sites.
- **Noise used ephemeral keys only**: Persistent static key was never bound to the
  transport session, making Noise XX authentication meaningless. Fixed by threading
  persistent key through handshake and validating remote static against allowlist.
- **Allowlist never enforced**: The check function existed but was never called during
  handshake. Now wired into post-handshake validation.
- **Connection drops didn't clean up**: Reader loop exit didn't remove routes, notify
  SWIM, or clean connmgr. Now wired: drop → remove route → SWIM notify → connmgr
  cleanup.

**High-priority fixes:**

- Deleted orphan `node.rs` (317 LOC dead code with conflicting symbols).
- Added thread-local `set_last_error`/`hew_last_error` C-ABI error diagnostic API.
- Converted 20+ panic-in-runtime paths to proper error returns (actor_group, link,
  mailbox, registry).
- Synced `runtime_manifest.json` with actual exported functions.
- Schema hash validation in handshake (reject incompatible peers).
- Connection reconnection with exponential backoff.
- Bounded outbound message queue for backpressure.
- **Wired remote send routing**: `hew_actor_send_by_id` now falls through to
  `try_remote_send → hew_node_send` when target PID is non-local, completing the
  end-to-end distributed message path.
- Added `hew_node_unregister` C-ABI function.
- Expanded `cabi_guard!` to 15 more functions across supervisor/transport/wire/encryption.

### Decisions

- **PID unification over PID removal**: Kept both fields but made them identical,
  minimizing struct layout changes while fixing the routing bug.
- **Thread-local errors over global**: `set_last_error` uses `thread_local!` so
  concurrent C-ABI calls don't clobber each other's diagnostics.
- **Delete over deprecate**: `node.rs` had conflicting symbols with `hew_node.rs` —
  deprecation would have caused linker errors, deletion was the only safe option.
- **CURRENT_NODE global**: A single `AtomicPtr<HewNode>` set on `hew_node_start` and
  cleared on `hew_node_stop`. This allows `hew_actor_send_by_id` to route remote PIDs
  without requiring callers to pass a node handle. One active node per process — matches
  Erlang/OTP's single-node model.

---

## Feature Completeness Sprint — February 2026

Multi-model analysis (Claude Opus 4.6, GPT-5.1 Codex, Claude Sonnet 4.5, Gemini 3 Pro,
GPT-5.2 Codex) identified 25 gaps across parser, type system, codegen, and runtime.
Implementation proceeded in parallel batches using frontier models.

### Batch 1: Parser Foundations (6 features)

- **Char literals**: Added `CharLit` lexer token, parser production, and type checking.
  End-to-end from `'a'` through codegen.
- **Negative patterns**: `-1` and `-3.14` now work in match arms.
- **Self parameter sugar**: `fn method(self)` infers Self type from impl block.
- **For loop labels**: `@outer: for i in 0..N { break @outer; }` now works.
- **Struct enum variants**: `Variant { field: Type }` with named fields.
- **Array repeat**: `[0; 256]` syntax for initializing arrays.

### Batch 2: Type System & Codegen (6 features)

- **Visibility modifiers**: `pub(package)` and `pub(super)` replace boolean `is_pub`.
  Full refactor across parser, type checker, serialization, and C++ codegen.
- **Trait bound enforcement**: Generic function calls now verify concrete types
  implement required traits.
- **Multi-trait dyn**: `dyn (Printable + Measurable)` with method resolution across
  all specified traits.
- **Range values**: Ranges are first-class tuples; `for i in start..end` works with
  variable bounds.
- **Unsafe enforcement**: Extern FFI calls require `unsafe { }` wrapper. Type checker
  tracks `in_unsafe` state.
- **Test migration**: 8 e2e tests updated to use `unsafe { }` for extern calls.

### Current State

- 311/312 codegen e2e tests passing (scope_spawn MLIR verification needs work)
- Full workspace Rust tests passing (530+ tests)
- Clean builds across all crates and C++ codegen
- 26 feature gaps identified, 24 fully implemented, 2 partial (scope_spawn codegen, WASM ops)

### Features Implemented

All 20 planned features from the multi-model analysis are now in place:
char literals, negative patterns, self parameter sugar, for loop labels,
struct enum variants, array repeat, visibility modifiers, trait bound enforcement,
multi-trait dyn, range first-class, unsafe enforcement, timeout codegen, HashSet,
generic lambdas, if-let patterns, custom indexing, associated types, s.spawn syntax,
ARM coroutines, and WASM platform warnings.

## Quality & Correctness Sprint — February 2026

Five frontier models analyzed the codebase in parallel, identifying 25+ issues
across parser, type system, codegen, runtime, and tests. Priority: correctness
bugs over style concerns.

### Correctness Fixes

- **Cross-enum variant matching**: Match arms from wrong enums silently type-checked.
  Fixed `lookup_variant_types` to only search the scrutinee's enum.
- **Bare return in non-unit fns**: `return;` in `fn -> int` compiled silently.
  Added `ReturnTypeMismatch` error kind.
- **Trait object ordering**: `dyn (A + B)` != `dyn (B + A)` in unification.
  Changed to set-based comparison.

### Safety Fixes

- **Compound assign UB**: Three codegen switch statements had no default case,
  leaving `mlir::Value` uninitialized. Added error-emitting defaults.
- **HashMap/Vec string getters**: Returned internal pointers that become dangling
  after mutation. Now return `strdup` copies.

### Parser Quality

- Missing param type annotation: silent drop → error message
- `pub(invalid)`: silent promotion to pub → defaults to private
- String interpolation errors: silently lost → propagated to parent

### Test Coverage

- Registered 4 unregistered e2e test directories (ranges, chars, for labels, tail call)
- Added negative type checker tests (mutability, arity)
- Added trait object ordering unification test

### Current State

- 314/316 codegen e2e tests passing
- 870+ Rust workspace tests passing
- Clean builds across all crates

### Quality Sprint 2: Deeper Issues

Second round of multi-model analysis found 8 more issues after the initial
correctness fixes:

- **Match exhaustiveness in statements**: `match` in statement position never
  checked for missing variants. Now calls `check_exhaustiveness()` same as
  match expressions.
- **Empty struct literals**: `Foo {}` failed to parse for zero-field structs.
  Parser struct-init lookahead now recognizes `Name {}` pattern.
- **C++ AST deserialization crashes**: IfLet, ArrayRepeat, and generic lambda
  type_params had no C++ counterpart — codegen crashed on "unknown variant".
  Added structs and parse cases.
- **Zero warnings**: Cleaned all Rust compiler warnings (unused fields,
  imports, stale suggestion text).

### Quality Sprint 3: Deep Correctness

Third round used 5 analysis agents (Claude Opus 4.6, GPT-5.1/5.2 Codex,
Claude Sonnet 4/4.5) across codegen, type checker, parser, runtime, and
serialization. Found 8 correctness bugs, fixed all:

- **Return use-after-free**: `generateReturnStmt` dropped locals BEFORE
  evaluating the return expression. `return vec.get(0)` would free `vec`
  first. Fixed by evaluating expression first, then dropping.
- **Self type losing generics**: `Self` in `impl<T> Pair<T>` resolved to bare
  `Pair` with no args. Changed `current_self_type` from `Option<String>` to
  `Option<(String, Vec<Ty>)>` to carry generic params.
- **Trait-object dispatch ignoring bound args**: `dyn Iterator<int>.next()`
  returned `Option<T>` instead of `Option<int>`. Now substitutes bound args
  into method signatures.
- **Labeled break/continue with 3+ nesting**: Only deactivated innermost loop,
  leaving intermediate loops running. Now deactivates ALL loops between target
  and current position.
- **Labeled break/continue resource leaks**: Only dropped innermost scope
  resources. Added `loopDropScopeBase` tracking to drop all intermediate scopes.
- **Scope binding undeclared**: `scope |s| { s.spawn {...} }` never declared
  `s` in symbol table. Fixed scope_spawn test (was pre-existing failure).
- **Scope spawn capture**: Spawned tasks couldn't access outer mutable
  variables. Added heap-cell capture mechanism for scope-spawned tasks.
- **TypeExpr::Infer deserialization**: C++ codegen crashed on Infer variant.
  Added missing msgpack reader case.
- **Parser error recovery**: Three gaps fixed (char escape silent None, struct/
  enum keyword error, positional-after-named args producing malformed AST).

**Test results**: 317/318 codegen e2e (up from 314/316), 252 type checker,
111 parser tests pass. Only pre-existing bench stdlib parse error remains.

### Quality Sprint 4: Full Test Suite Green

Fourth round found 9 more issues, all fixed. Achieved **321/321 codegen
tests passing** (100%) for the first time.

- **Indexed compound assignment**: `v[i] += 1` was silently dropping the
  operator, becoming `v[i] = 1`. Added VecGetOp→compound-op→VecSetOp pattern
  matching the field-access handler. Also fixed for arrays.
- **HashSet double evaluation**: `s.insert(expr)` generated `expr` twice —
  once for type inference, once in the method call. Refactored to pass
  pre-generated values to `emitHashSetMethod`.
- **Vec<bool> suffix mismatch**: `vecElemSuffix` returned `""` for i1 but
  `vecElemSuffixWithPtr` returned `"_i32"`. Vec creation used wrong runtime
  function. Fixed to use consistent `_i32` suffix for bool.
- **Tuple pattern matching**: `PatTuple` fell through to catch-all warning
  in match codegen. Added full tuple destructuring with `TupleExtractOp`.
- **Lambda arity**: 1-param lambda silently passed as `fn(int,int)->int`.
  Added arity check in `check_lambda` before parameter processing.
- **OR-pattern exhaustiveness**: `Some(x) | None => ...` spuriously warned
  about missing arms because `Pattern::Or` wasn't decomposed. Added recursive
  OR-pattern handling in `check_exhaustiveness`.
- **Parser deduplication**: Extracted `parse_fn_with_modifiers` to eliminate
  ~160 lines of duplicated async/gen/pure modifier handling.
- **Numeric narrowing revert**: The i64→i32 width restriction broke the
  language (Hew's `int` = i64, used everywhere as array indices, etc.).
  Reverted to allow same-sign integer coercion — correct for Hew's design.
- **Bench stdlib fixed**: The above revert also fixed the bench stdlib's
  i64/i32 division, making `e2e_bench_bench_basic` pass.

**Test results**: 321/321 codegen e2e (100%), 259 type checker, 111 parser.

### Quality Sprint 5: String Ordering, If-Let Codegen, Array Repeat

Fifth round focused on implementing three codegen features that were parsed
and type-checked but emitted stubs or errors during code generation.

- **String ordering**: `<`, `<=`, `>`, `>=` on strings compared raw pointers
  instead of lexicographic content. Added `hew_string_compare` runtime
  function using `strcmp` with null safety, and updated all four ordering
  operators to call it. Added actor-pointer guards to prevent UB when
  accidentally ordering actor references.
- **If-let codegen**: `if let Some(x) = opt { ... }` was a dead stub in
  codegen. Created `MLIRGenIfLet.cpp` with full implementation: pattern test
  via `EnumGetTagOp`, binding via `EnumExtractPayloadOp`, scf::IfOp for
  branching. Works for both statement and expression forms.
- **Array repeat codegen**: `[value; count]` was a dead stub. Implemented as
  VecNewOp + loop of VecPushOp, matching the type checker's decision to type
  `[expr; n]` as `Vec<T>`.
- **Enrichment revert**: Mapping Unit/Generator/AsyncGenerator/Range to
  TypeExpr::Named caused "unresolved type" errors in C++ codegen. Reverted
  to returning None — these types are handled by built-in codegen logic.

### Quality Sprint 6: Match Safety, Capture Analysis, Exhaustiveness

Sixth round addressed critical correctness bugs found by multi-model analysis
(Claude Opus 4.6, GPT-5.2 Codex, Claude Sonnet 4.5, Gemini 3 Pro, GPT-5.1
Codex).

- **Struct variant match bypass**: Struct variant patterns as the last match
  arm skipped the tag check, executing unconditionally. This caused UB when
  a different variant was active. Added `!isStructVariantPattern` to the
  last-arm optimization condition.
- **If-let return guards**: `stmtMightContainReturn` and
  `stmtMightContainBreakOrContinue` were missing `StmtIfLet`. Code after an
  `if let` with a `return` would execute even after the return fired.
- **Lambda capture pattern binding**: `collectFreeVarsInExpr` didn't bind
  pattern variables before traversing match arm bodies. Pattern vars like
  `x` in `Ok(x)` were treated as free variables, causing spurious captures.
  Added `collectPatternBindings` helper and applied it to match, let, for,
  and if-let contexts. Also added `ExprIfLet` and `ExprArrayRepeat` handling
  in expression-level capture analysis.
- **Range inclusive types**: `..=` only accepted i64/index, rejecting i8,
  i16, i32, etc. Changed to `isIntOrIndex()`.
- **Strdup null checks**: HashMap's `strdup` calls had no null checks.
  Added `libc::abort()` on failure for all 8 strdup sites.
- **Exhaustiveness with guards**: Guarded wildcards (`_ if false => ...`)
  were treated as exhaustive. Now guarded arms are excluded from coverage.
- **Pragmatic review catch**: Reviewer found `ExprIfLet` and
  `ExprArrayRepeat` missing from `collectFreeVarsInExpr` (expression-level
  capture analysis) — fixed before commit.

**Test results**: 327/327 codegen e2e (100%), 1539 Rust tests, zero warnings.

### Quality Sprint 7: Float Types, Nested Patterns, Field Validation

Seventh round addressed LLVM lowering gaps and type checker safety:

- **f32 printing**: `PrintOpLowering` had no f32 case — values fell through
  to `hew_print_i32`, producing garbage output. Added f32→f64 promotion.
- **Float coercion**: `CastOpLowering` and `coerceType` had no f32↔f64
  conversion path. Added `ExtFOp`/`TruncFOp` lowering and MLIR-level coercion.
- **Nested patterns**: `Some((a, b))` silently skipped the tuple
  destructuring inside enum payloads. Root cause was two bugs working
  together — type checker passed unresolved `Ty::Var` to `bind_pattern`
  (fixed with `subst.resolve()`), and codegen only handled `PatIdentifier`
  sub-patterns in constructors (added `PatTuple` handling in match and
  if-let paths).
- **char_at bounds**: Sign extension (`ExtSIOp`) of i32 index to i64 could
  misinterpret unsigned values. Changed to zero extension (`ExtUIOp`).
- **Struct field errors**: Unknown fields in struct patterns were silently
  skipped. Added `UndefinedField` error reporting with suggestions.

**Test results**: 328/328 codegen e2e (100%), 1540 Rust tests, zero warnings.

### Quality Sprint 8: Stored Range Loops, Guard Pattern Binding

Eighth round found correctness issues in for-loop codegen for stored ranges
and constructor pattern guard binding:

- **For-loop stored ranges**: Four bugs in `generateForCollectionStmt`:
  (1) `IndexCastOp` used instead of `ExtSIOp` for type widening (invalid for
  non-index types), (2) missing `MutableTableScopeT` causing scope leaks,
  (3) `generateBlock` instead of `generateLoopBodyWithContinueGuards` (continue
  wouldn't work), (4) always signed comparison (should check unsigned flag).
  Fixed items 1-3; item 4 deferred as unsigned ranges are rare in practice.
- **Constructor guard PatTuple**: When a constructor pattern with a guard
  like `Some((a, b)) if a > 0` was used, the guard scope only bound
  `PatIdentifier` sub-patterns. Added `PatTuple` binding to match the arm
  body handling.

**Test results**: 328/328 codegen e2e (100%), 1540+ Rust tests, zero warnings.

### Quality Sprint 9: Loop Safety, Expression Coverage, Overflow Protection

Ninth round dispatched five analysis agents across different frontier models,
followed by seven implementation agents and a pragmatic code reviewer:

- **`loop {}` returnFlag**: `loop` body re-entered even after `return` set the
  returnFlag. Fixed by wrapping loop condition with `!returnFlag` check.
- **`var` pendingDeclaredType leak**: When `generateExpression` failed in a `var`
  declaration, the early return skipped `pendingDeclaredType.reset()`, leaking
  the type into subsequent expressions. Fixed by resetting before the early return.
- **Stream/generator/for-await continue**: Three loop types used `generateBlock`
  instead of `generateLoopBodyWithContinueGuards`, making `continue` statements
  silently skip to the next iteration incorrectly.
- **Integer overflow**: Added `checked_mul` in hashmap resize, `saturating_mul`
  in string repeat, and overflow checks in string replace_all to prevent UB.
- **Parser EOF safety**: `expect()` and `parse_identifier()` called `.unwrap()`
  on EOF tokens, causing panics. Replaced with proper error handling.
- **`rewrite_builtin_calls` completeness**: Only 9 of ~30 expression variants
  were traversed. Added 18 missing variants (InterpolatedString, PostfixTry,
  Await, Yield, Send, Range, Unsafe, Join, Timeout, ScopeLaunch, ScopeSpawn,
  Scope, SpawnLambdaActor, Match, Lambda, Spawn, StructInit, Select).
- **Log emit leak**: Temporary string created for `hew_log_emit` was not freed
  after the call, leaking memory on each log statement.

**Test results**: 329/329 codegen e2e (100%), 1540+ Rust tests, zero warnings.

### Quality Sprint 10: Double-Free, Label Cleanup, Normalization Gaps

Tenth round dispatched five analysis agents (Claude Opus 4.6, GPT-5.1 Codex,
Claude Sonnet 4.5, GPT-5.2 Codex, Gemini 3 Pro), followed by five
implementation agents and a pragmatic code reviewer:

- **Log emit double-free**: When `log.info(42)` (non-string) was called, the
  `ToStringOp` result was both `msgStr` and in `ownedTemps`. Both were freed
  independently, causing a double-free. Fixed with `temp != msgStr` guard.
- **Labeled loop cleanup**: Five for-loop variants (`ForRange`, `ForVec`,
  `ForHashMap`, `ForGenerator`, `ForStream`) registered labels but never erased
  them from `labeledActiveFlags`/`labeledContinueFlags` on exit. Added cleanup
  to all five, plus full label support for `ForStream` which had none.
- **Or-pattern PatIdentifier**: `generateOrPatternCondition` returned nullptr
  for `PatIdentifier`, which silently skipped or-patterns with variable bindings.
  The initial fix (always-true) was caught by the pragmatic reviewer as wrong
  for enum unit variants. Final fix checks `variantLookup` to distinguish
  variable bindings (always-true) from enum variants (tag comparison).
- **Vec strdup NULL checks**: Five strdup call sites in vec.rs now abort on
  NULL, matching hashmap.rs behaviour.
- **ExprRange fixes**: Inclusive range widened from `i64|index` to all integer
  types. Type mismatch between start and end now coerced instead of silently
  producing a broken tuple.
- **Serialization normalization**: Added normalization for `Item::Trait`,
  `TypeBodyItem::Variant`, `Item::Const`, and `Item::TypeAlias` — previously
  all skipped by the `_ => {}` wildcard.

**Test results**: 329/329 codegen e2e (100%), 1534+ Rust tests, zero warnings.

### Quality Sprint 11: Type-Dispatch Gaps, Capture Analysis Completeness

Eleventh round dispatched four deep-analysis agents targeting LLVM lowering,
control flow, expression codegen, and concurrency safety:

- **ToStringOp f32**: `hew_float_to_string` expects f64, but f32 values were
  passed without promotion. Added `ExtFOp` to widen f32→f64 before the call.
- **Assert lowering gaps**: AssertOp only handled i1/i32, AssertEqOp/AssertNeOp
  only handled i32/i64/f64/string/pointer. Added i8/i16 widening (ExtSIOp→i64)
  and f32 promotion (ExtFOp→f64) across all three assert operations.
- **VecNewOp struct sizing**: f32 struct fields fell through to the default
  (size=8, align=8) instead of correct (size=4, align=4). Added explicit f32
  case in the struct size computation loop.
- **Capture analysis completeness**: `collectFreeVarsInExpr` was missing 14
  expression variants. Added: ExprSpawn, ExprSpawnLambdaActor, ExprScope,
  ExprScopeLaunch, ExprScopeSpawn, ExprSelect, ExprJoin, ExprRange,
  ExprTimeout, ExprYield, ExprUnsafe. All legitimately skippable variants
  confirmed (ExprLiteral, ExprLambda, ExprScopeCancel, ExprRegexLiteral,
  ExprCooperate).

**Test results**: 329/329 codegen e2e (100%), 1534+ Rust tests, zero warnings.

### Quality Sprint 12: Vec Memory Corruption, Sleep Safety

Twelfth round fixed critical memory corruption and safety issues:

- **Vec<bool>/Vec<i8>/Vec<i16> inline path**: The `vecElemSuffixWithPtr`
  function maps narrow integer types to `_i32` suffix, routing through the
  inline fast path. But GEP used the original type (1-2 byte stride) while
  the runtime stores these as i32 elements (4-byte stride). Fixed by widening
  values to i32 before GEP/Store (push/set) and truncating after Load (get).
  Added `vec_bool` e2e test to verify correctness.
- **SleepOp truncation**: i64→i32 truncation without bounds check. Values
  > INT32_MAX silently wrapped. Added saturating clamp to INT32_MAX.
- **String interpolation**: Investigation found the `j > 1` guard is actually
  correct — `partValues[0]` is tracked separately in `ownedTemps` during
  the part-generation phase if it's a temporary.

**Test results**: 330/330 codegen e2e (100%, +1 new), 1534+ Rust tests.

### Quality Sprint 13: Vec<f32>, Trait Object, Capture Stmt, Runtime Safety

Thirteenth round found and fixed remaining type-dispatch gaps, capture
analysis holes, and runtime safety issues:

- **Vec<f32>**: `vecElemSuffix` didn't handle f32, so `Vec::new()` for f32
  used the wrong element size. `vecElemSuffixWithPtr` mapped f32→_f64, but
  `vecElemSuffix` (used by VecNew) and VecPop were missing. Fixed by adding
  f32→_f64 mapping to both suffix functions and adding f32↔f64 promotion/
  truncation to ALL Vec operation paths (inline + fallback).
- **Trait object default**: `createDefaultValue` used `ConstantIntOp(i32, 0)`
  for vtable pointer, but TraitObjectCreateOp requires `!llvm.ptr`. Changed
  to `LLVM::ZeroOp` null pointer.
- **collectFreeVarsInStmt**: Missing StmtMatch (scrutinee + arm bodies),
  StmtBreak (optional break value), and StmtDefer (deferred expression).
- **Runtime**: Added checked_add overflow in string concat, NULL check after
  malloc_cstring in string split, and u32 overflow guard in TCP framing.

**Test results**: 330/330 codegen e2e (100%), 1534+ Rust tests, zero warnings.

### Quality Sprint 14: Systematic Type Coverage Matrix

Fourteenth round took a systematic approach instead of ad-hoc analysis.
Built a full (operation × type) coverage matrix by tracing every if/else
chain in all 14 type-dispatching LLVM lowering patterns:

- **VecRemoveOpLowering**: Had zero promotion logic — passed narrow values
  (i1/i8/i16) directly to `hew_vec_remove_i32` without widening, and f32
  to nonexistent `hew_vec_remove_f64`. Added ExtUIOp/ExtSIOp for narrow
  ints, ExtFOp for f32, and `hew_vec_remove_f64` runtime function.
- **HashMapInsertOpLowering**: The `else` branch assumed i32 but received
  i1/i8/i16 without promotion and f32 without f64 conversion. Added f32→f64
  branch (routes to `hew_hashmap_insert_f64`) and narrow int promotion to i32.
- **HashMapGetOpLowering**: Declared the runtime function with the MLIR
  result type (e.g., i1) instead of the actual runtime return type (i32).
  Fixed to call `hew_hashmap_get_i32`/`hew_hashmap_get_f64` with correct
  return type, then TruncIOp/TruncFOp to narrow.
- **PrintOpLowering**: Replaced silent i32 fallback with explicit error.
- **Reviewer catch**: i8 was using ExtSIOp (sign-extend) instead of ExtUIOp
  (zero-extend), inconsistent with the rest of the codebase. Fixed in both
  VecRemove and HashMapInsert.

Added systematic type coverage e2e tests: `type_coverage` (int, i32, bool,
float, string through Vec ops) and `type_dispatch_narrow` (Vec<i32> remove,
HashMap<string, i32> insert/get).

**Test results**: 332/332 codegen e2e (100%, +2 new), 324+ Rust runtime tests.

### Quality Sprint 15: Control Flow Correctness, Match Guards

Fifteenth round dispatched three deep-analysis agents (Opus 4.6 on codegen
MLIR, GPT 5.1 Codex on type checker, Sonnet 4.5 on runtime) for systematic
analysis beyond the type dispatch matrix:

- **Return in loop body**: When `return` executed inside a while/for/loop
  body, the returnFlag was set but the continueFlag was not. Remaining
  statements in the same loop iteration would still execute (wrong side
  effects). Fixed by also setting the innermost continueFlag when setting
  returnFlag inside a loop.
- **Labeled break intermediate continue**: `break 'outer` through 3+
  nesting levels set the innermost and target continue flags but skipped
  intermediate loops. Middle loop body statements after inner loop exit
  would incorrectly execute. Fixed by setting continue flags for ALL
  intermediate loops.
- **Or-pattern failure**: If `generateOrPatternCondition` returned nullptr,
  the match arm was silently skipped with a warning. Changed to emit error
  instead (fail-fast, no silent miscompilation).
- **Vec append overflow**: `(*dst).len + src_len` could overflow before
  `ensure_cap`. Added `checked_add` with abort on overflow.

Analysis false positives: HashMap resize tombstone issue (line 153 already
checks `old.state == OCCUPIED` before `fnv1a`).

**Test results**: 333/333 codegen e2e (100%, +1 new), zero warnings.

### Quality Sprint 16: Type Checker, Serialization, Module Safety

Sixteenth round dispatched three analysis agents targeting the Rust type
checker (GPT 5.1 Codex), serialization boundary (Sonnet 4.5), and
remaining codegen lowering (GPT 5.2 Codex):

- **Match Never type**: `check_match_expr` cached the first arm's type
  even when it was `Ty::Never` (diverging arm). `match x { true => return,
false => 42 }` was typed as Never instead of I64. Fixed by skipping
  Never/Error arms when setting the expected type.
- **Tuple pattern arity**: `bind_pattern` for `Pattern::Tuple` used `zip`
  without checking element count, silently dropping excess bindings. Added
  arity validation with `ArityMismatch` error.
- **Module-qualified type names**: `names_match_qualified` stripped all
  module prefixes, so `auth.User` unified with `billing.User`. Fixed to
  only allow bare-vs-qualified matching (one name unqualified, other
  qualified), rejecting two different qualified names.
- **Select arm source**: `enrich_expr` and `normalize_expr_types` traversed
  `arm.body` but not `arm.source` in Select expressions. Module-qualified
  calls in select sources weren't rewritten. Fixed both functions.
- **Or-pattern fallback**: Match codegen emitted warning and silently
  skipped unhandled patterns. Changed to `emitError` for fail-fast.

**Test results**: 333/333 codegen e2e (100%), 1088+ Rust workspace tests, zero failures.

### Quality Sprint 17: Deep Hash/Vec Audit + Match Safety

Dispatched 5 parallel audit agents (Opus 4.6, GPT 5.1 Codex, Sonnet 4.5,
GPT 5.2 Codex, Gemini 3 Pro) for deep Hash/Vec correctness analysis across
all compiler layers (type checker → serialization → MLIR gen → LLVM lowering
→ runtime). Then dispatched 4 fix agents and 5 more audit agents for broader
coverage.

**HashMap.get() Option return** (breaking change):

- `HashMap.get()` previously returned raw `T` at MLIR level, with a fragile
  match-time wrapping hack that only worked for inline `match m.get("x")`.
  Using `let g = m.get("x"); match g { ... }` broke because the match
  couldn't trace back through the memref store to find the HashMapGetOp.
- Fixed by wrapping the raw value in `Option<T>` at the expression level:
  `contains_key` check → `scf.IfOp` → `EnumConstructOp(Some)` / `(None)`.
- Removed 40-line special-case hack from MLIRGenMatch.cpp.
- Updated all HashMap test code to use `match` with `Some/None` patterns.

**Non-exhaustive match trap**:

- Match fallthrough previously called `createDefaultValue` — silently
  returning zero/undef when no arm matched. Now emits `hew.panic`
  (traps at runtime).
- Exhaustiveness warnings now cover all types (int, float, string),
  not just enums/bool/Option/Result. The `_ => {}` catchall in
  `check_exhaustiveness` replaced with proper identifier-or-wildcard check.

**Vec runtime hardening**:

- Added `hew_vec_set_ptr` and `hew_vec_pop_ptr` for pointer-type vectors.
- All 6 `hew_vec_push_*` functions now use `checked_add` for overflow
  protection on `len + 1`.
- `hew_vec_append` now validates `elem_size` and `elem_kind` match between
  source and destination vectors before memcpy.

**Broader audit findings** (5 additional agents):

- Enum/match codegen (Opus 4.6): Found match trap + exhaustiveness bugs (fixed).
  Confirmed Option/Result tag indices consistent across all code paths.
- For-in iterator (GPT 5.1 Codex): Vec iteration captures length once — stale
  if body mutates Vec. Design tradeoff, not a bug (OOB crash is correct).
- String operations (Sonnet 4.5): No bugs found. All string ops properly owned.
- Struct/impl (GPT 5.2): Pointer-like field access scans all struct types by
  field name — collision risk if multiple structs share field names. Low priority.
- Closure/lambda (Gemini 3 Pro): Captured owned types shallow-copied without
  suppressing outer drop (potential use-after-free). Closure env uses null
  destructor (captured resources leaked). Design limitations, not crashes in
  typical use.

**Test results**: 335/335 codegen e2e (100%, +2 new), 324/324 runtime, zero warnings.

## 2026-03-06 — Audit remediation follow-ups

### Example/test infra cleanup

- `make grammar` now walks nested `examples/**.hew` trees, so algorithms, benchmarks,
  playground examples, and other subdirectories stay under grammar validation.
- `scripts/sync-syntax-downstream.sh --check` now regenerates downstream artifacts in
  temp snapshots and fails on real output drift instead of only reporting working tree dirtiness.
- Downstream repo paths are overridable with `HEW_SYNC_*` environment variables, which
  keeps the sync check usable from isolated worktrees and local fixture repos.

### Wire syntax remediation

- Unified wire field modifier handling in `hew-parser` so both `#[wire] struct` and legacy
  `wire type` preserve `since N` in wire metadata.
- Updated the formatter to emit `since N` for normalized wire output and added
  parser/formatter regressions covering positive and invalid-version cases.

### Serializer contract hardening

- Reworked `hew-serialize/src/enrich.rs` so `Ty` conversion either serializes the type
  explicitly or returns a contextual `TypeExprConversionError`; expr-type entries are no
  longer dropped by `filter_map`/`Option` fallthrough.
- `Ty::Unit` now becomes `TypeExpr::Tuple([])`, `Range` stays serializable as a named
  builtin, and unsupported inferred forms like `Generator`/`AsyncGenerator` are reported
  explicitly instead of disappearing.
- Added regression coverage for preserving unit entries and for rejecting nested unsupported
  types (for example `Option<Ty::Var>`) with span-aware diagnostics.
- Followed up by removing the last `lookup_type()` silent-`None` path: best-effort inferred
  binding/return enrichment now carries span-tagged diagnostics back to the CLI instead of
  quietly dropping unsupported conversions.
- Updated `hew-cli` to render one structured warning per unsupported inferred type and to
  deduplicate overlap between enrichment and expression-type-map passes, so generator-heavy
  builds stay working without hiding serializer gaps.
- Added regression coverage for unsupported inferred `let`/return paths plus a compile-side
  dedupe test that guards against double-reporting the same span.

### Select cleanup remediation

**Commit:** 67c071d

- Fixed the select/join cleanup asymmetry in `hew-codegen/src/mlir/MLIRGenExpr.cpp` by
  extracting shared reply-wait cleanup, preserving join's explicit destroy path, and
  canceling every non-winning select channel (including timeout paths) via
  `hew_reply_channel_cancel`.
- Added MLIR regression coverage in `hew-codegen/tests/test_mlirgen.cpp` to catch missing
  select channel cancellation and to ensure join keeps its explicit destroy-only cleanup.
- Updated the reply-channel runtime (`reply_channel.rs` and `reply_channel_wasm.rs`) with
  an explicit cancellation entry point plus sender/waiter refcounting so abandoned select
  arms self-clean on late reply without use-after-free.

### Ask send-failure remediation

- Changed `hew_actor_ask_with_channel` to return an explicit `HewError` status and to
  make submit failures explicit so select/join can fail fast instead of waiting on a
  request that never entered a mailbox.
- Updated MLIR select/join lowering to capture `hew.select.add` status, cancel/destroy
  every already-created reply channel on send failure, and panic explicitly on failed
  submission before any wait path can hang.
- Added regression coverage in the runtime and MLIR tests for failed `ask_with_channel`
  submission plus send-failure cleanup paths in both `select` and `join`.


## Structured Error Types — Error Handling Story

### Design rationale

Hew already has the infrastructure for proper error handling:

- `Result<T, E>` is a built-in generic enum
- The `?` operator propagates errors from `Result` (and `Option`)
- Enums with payload variants support pattern matching

What was missing was a **convention**: stdlib modules returned bare
`i32` error codes or `Result<T, String>` with unstructured messages.
Services need to match on error *kinds* (not found vs permission denied
vs timeout), not parse error strings.

### Approach: pure-enum error types (Option C)

We chose the simplest approach that requires **no language changes** —
define error enums as regular Hew enums in each stdlib domain:

```hew
enum IoError {
    NotFound(int);
    PermissionDenied(int);
    AlreadyExists(int);
    Other(int);
}
```

User code wraps existing operations in `Result<T, IoError>` and gains
structured matching via the `?` operator:

```hew
fn load_config(path: String) -> Result<String, IoError> {
    if fs.exists(path) {
        Ok(fs.read(path))
    } else {
        Err(IoError::NotFound(1))
    }
}

fn init() -> Result<String, IoError> {
    let cfg = load_config("app.toml")?;   // propagates IoError
    Ok(cfg)
}
```

### What shipped

- **`IoError` enum** in `std/fs.hew` — canonical error type for
  file-system operations (`NotFound`, `PermissionDenied`,
  `AlreadyExists`, `Other`), each carrying an `int` error code.
- **E2E test suite** (`e2e_structured_errors/structured_errors.hew`) —
  exercises custom `MathError` enum with the `?` operator: Ok-path
  propagation, first-error and second-error propagation through chained
  `?`, pattern matching on specific error variants, and qualified
  construction (`MathError::Overflow(999)`).

### Known limitation — `Result<T, UserEnum>` in imported modules

The codegen currently cannot lower `Result<T, E>` where `E` is a
user-defined enum when that type appears in a function signature inside
an imported module.  Standalone files work fine (the E2E test proves
this).  Once the MLIR monomorphisation handles heterogeneous Result
instantiations in module scope, `std/fs.hew` can export `try_*`
functions returning `Result<T, IoError>` directly.

### Future work

- Add `NetError`, `ParseError`, `CryptoError` enums to their respective
  stdlib modules following the same pattern.
- Lift the imported-module codegen restriction so stdlib functions can
  return `Result<T, IoError>` natively.
- Add an `Error` trait with `message()` and `kind()` methods once trait
  objects are fully supported in codegen.

### Implicit generic monomorphization

- Added implicit type-argument inference for generic function calls so that
  `identity(42)` works without requiring `identity<int>(42)`.
- **Root cause:** the type checker inferred concrete types via unification but never
  wrote them back into the AST's `call.type_args`; the C++ codegen only specialised
  when `type_args` was present, falling through to "undefined function" for implicit calls.
- **Fix (enrichment-stage):** added a `call_type_args` map to `TypeCheckOutput` that
  records inferred type arguments per call site. During enrichment, calls with missing
  `type_args` are backfilled from this map before serialization, so the codegen sees
  explicit type args and handles them through the existing monomorphization path.
- Added E2E tests: `generic_implicit_identity` (single-param, int + string) and
  `generic_implicit_multi` (multi-arg generic with int + string specializations).

### Single-binary codegen cutover

- **Decision:** Embed the C++ MLIR/LLVM codegen directly into the `hew` Rust binary
  instead of shipping a separate `hew-codegen` executable.  The Rust driver calls the
  C++ backend through a thin C API (`hew_codegen_compile_msgpack`) rather than spawning
  a child process and piping MessagePack over stdin.

- **Why:** Eliminates a process boundary (spawn + pipe + wait), removes a binary from
  packaging, and makes the compiler self-contained.  Users install one binary that does
  everything: parse, type-check, lower to LLVM IR, link.

- **Architecture:** `hew-cli/build.rs` invokes CMake to compile the C++ code into
  `libHewCodegenCAPI.a`.  CMake generates a `.cargo` file containing link directives
  that Cargo reads — zero parsing, CMake is the sole authority on what to link.

- **Static vs. shared:** `HEW_EMBED_STATIC=1` (release) statically links all of
  MLIR + LLVM + libstdc++ into a ~127MB self-contained binary.  Dev mode uses shared
  linking for ~2s incremental builds.  The `.cargo` file abstraction means the same
  Rust code handles both modes.

- **Key decisions:**
  - Glob all `libMLIR*.a` / `libLLVM*.a` with `--start-group`/`--end-group` instead
    of curating a library list (MLIR's cmake helpers miss transitive deps).
  - Prefer `${LLVM_PREFIX}/bin/clang` over PATH clang to avoid Apple Clang on macOS.
  - Reject cross-arch compilation with a clear error (host→target arch mismatch would
    silently embed wrong-arch code).
  - Clean cutover: no fallback to standalone binary, no compatibility paths.
  - Standalone `hew-codegen` executable deleted.  CMake now builds only libraries and
    test executables (test_mlirgen, test_mlir_dialect, test_translate).

- **Removed from shipping:** standalone hew-codegen binary stripped from release.yml,
  all installers (shell, PowerShell, Docker, Alpine, Debian, RPM, Arch, Nix, Homebrew),
  and the nightly sanitizer workflow was simplified to use the same build directory.

## Stream/Sink RAII Auto-Close and Drop Exclusion Fixes

- **What:** Three fixes for the stream/bytes API: (1) let-bound match return double-frees
  in bytes map, (2) Stream/Sink RAII auto-close on scope exit, (3) type guard for
  `lines()` on `Stream<bytes>`.

- **Why:** Without RAII auto-close, every Stream/Sink handle requires an explicit
  `.close()` call. Forgetting one causes consumer deadlocks (waiting for EOF that
  never arrives). The let-bound match fix prevents double-frees when match arms
  flow through let bindings to function returns.

- **RAII approach:** Each Stream/Sink variable gets a `closeAlloca` (`memref<llvm.ptr>`)
  storing the handle pointer. On scope exit, codegen loads from the alloca, null-checks
  via `scf::IfOp`, and calls close. Explicit `.close()`, consuming runtime functions,
  and user function calls null the alloca so scope-exit close is a no-op.

- **Key decisions:**
  - Ownership transfer heuristic: user functions receiving stream/sink args are assumed
    to take ownership (alloca nulled). Functions that only observe would leak, but
    double-frees are worse. Proper fix requires move semantics in the type system.
  - Block expression tail values need alloca null-out before scope pop, or the
    scope-exit close fires on a handle being returned to the outer scope.
  - Non-consuming function allowlist uses actual runtime names (`hew_sink_write_string`,
    not `hew_sink_write`) discovered by checking enricher rewriting.
  - The `funcLevelReturnVarNames` flat name set handles the depth mismatch caused by
    the enricher's `unsafe {}` wrapper (variables at depth 1, return expressions at
    depth 2+).

## RAII Phase 1: Parameter Drops and the Null-After-Move Prerequisite

- **What:** Investigated adding automatic drop calls for function parameters at scope
  exit, as the first step toward proper Rust-style RAII across the codegen.

- **Approach 1 (abandoned): dropValue in DropEntry.** Stored the raw `mlir::Value`
  from the parameter declaration site. Failed because `declareVariable` promotes
  `let` bindings to `alloca+store` when `returnFlag` is active — the raw SSA value
  lives inside an `scf.if(!returnFlag)` guard and doesn't dominate the drop site.
  `lookupVariable` is the correct mechanism because it creates a fresh `memref::LoadOp`
  from the alloca, which always dominates.

- **Approach 2 (double-free): pendingFunctionParamDrops.** Registered params for drop
  via the existing `registerDroppable` mechanism. All 522 tests passed on compile, but
  13+ tests hit double-frees at runtime. Root cause: the codegen has no ownership
  transfer tracking. When a param is consumed by match destructuring (`match t { ... }`
  frees the indirect enum), callee move (`sink.write(data)` frees `data` in the runtime),
  or return, the param drop at function exit frees already-freed memory.

- **Decision:** Param drops require **null-after-move** as a prerequisite. Without
  nulling out a variable's storage after consumption, any new drop registration is
  unsafe. This applies to all owned values, not just params.

- **Key discovery: declareVariable alloca promotion.** When `returnFlag` is active
  (non-void functions), `declareVariable` (MLIRGen.cpp:878-916) promotes `let` bindings
  to `memref` alloca+store in the function entry block. This means `lookupVariable`
  always returns a value that dominates any point in the function. This is the correct
  mechanism for drop emission and is the foundation for null-after-move (store null
  into the alloca after consumption).

- **Path forward:** Implement null-check-before-drop in `emitDropEntry`, then add
  null-after-move at consumption sites (match destructuring, callee args, return).
  Only then re-enable param drops.
