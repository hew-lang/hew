# Concurrency & Drop-Safety ast-grep rules — validation report

Domain: `rules/rust/concurrency-drop/` — CLAUDE.md §1 (Drop Safety) and §9 (Concurrency Safety).
Scope: `hew-runtime/**`, `hew-codegen-rs/**` (tests excluded). Validated against the tree at
authoring time with `ast-grep 0.44.0`.

Precision-first: this domain is FP-prone, so the bar was "few, sharp rules; downgrade or reject
anything that can't be made precise." Result: **2 shipped (advisory), 3 rejected.**

---

## Shipped rules

### 1. `lock-unwrap.yml` — severity `info`

- **Invariant:** CLAUDE.md §9 — poisoned-lock panics can mask the shutdown race that poisoned the
  lock. The codebase's dominant idiom is poison recovery via `PoisonSafe::access` /
  `.unwrap_or_else(PoisonError::into_inner)` (≈386 uses) rather than raw `.lock().unwrap()`.
- **Pattern:** `$M.lock().unwrap()` / `$M.lock().expect($_)`.
- **Precision work:** the naive pattern matched **96** sites, but **88 were inside in-file
  `#[cfg(test)] mod` modules** (test code, where unwrapping is fine). Test modules are excluded by
  matching the enclosing `mod_item` name (`tests` / `*_tests` / `test_support`) — robust against the
  many `cfg(...)` spellings used here (`cfg(all(test, ...))`, `cfg(not(test))`, etc.), where an
  attribute-regex would mis-handle `#[cfg(not(test))]` production code. Final: **10 production hits**.

| file:line | receiver | triage |
|---|---|---|
| `bridge.rs:63` | `OUTBOUND` (global `Mutex`) | **TP, strongest** — global shared state (§9), poison panics in a bridge accessor. |
| `bridge.rs:196` | `META_STATE` (global `Mutex`) | **TP, strongest** — same. |
| `alloc_tracker.rs:48,58,66` | `LIBC_ALLOC_SET` (global) | **TP** — global lock; `#[cfg(debug_assertions)]` only, so lower runtime risk. |
| `auto_mutex.rs:147,156,191` | `m.inner` / `m.guard_slot` | **TP-by-pattern, intentional** — the low-level `hew_auto_mutex` primitive; treats poison as a fail-closed trap with a documented rationale (`auto_mutex.rs:143-146`) and descriptive `.expect()` messages. Good `// JUSTIFIED:` candidate. |
| `xnode_serial.rs:924,925` | `THUNK_REGISTRY` / `REPLY_REGISTRY` (globals) | **TP, low-priority** — global registries, but inside the `pub(crate) fn clear_codec_registries_for_test()` test-support helper (compiled in all builds, so not test-mod-excluded). |

- **Precision:** 10/10 hits are genuine raw poisoned-lock unwraps in production concurrency code
  (0 false positives on the pattern; 0 test-module noise). Actionability ranges from "should fix"
  (the two `bridge.rs` globals) to "intentional, annotate `// JUSTIFIED:`" (`auto_mutex` primitive).
- **Severity rationale:** `info` (advisory, non-gating). The set skews toward intentional/low-urgency
  sites, so `warning` would overstate; `info` flags them for §9 review and points at the poison-safe
  idiom. Raise to `warning` if the team wants more visibility on the `bridge.rs` globals.
- **Limitations:** does not honor a trailing/leading `// JUSTIFIED:` comment (tree-sitter treats
  comments as "extras", so `follows`/`precedes` on comments is not reliable in ast-grep). Annotated
  sites are still flagged — call them out in review rather than suppressing.

### 2. `explicit-leak-review.yml` — severity `hint`

- **Invariant:** CLAUDE.md §1 — every heap/resource escape needs a cleanup path for sync return,
  async cancel, and actor shutdown. `mem::forget` / `Box::leak` are explicit escapes from RAII and are
  exactly the sites a drop-safety audit must re-examine.
- **Pattern:** `{std,core,}::mem::forget($X)`, `Box::leak($X)` / `Vec::leak($X)` / `String::leak($X)`.
- **Hits: 3** (all production; tests excluded via the same `mod_item`-name filter).

| file:line | code | triage |
|---|---|---|
| `quic_mesh.rs:1412` | `std::mem::forget(arc)` | Intentional leak when `Arc::try_unwrap` fails (avoid tearing down a runtime we don't fully own). **Not** `// JUSTIFIED:`-annotated — worth a glance / annotation. |
| `quic_transport.rs:756` | `std::mem::forget(arc)` | Same pattern; **already `// JUSTIFIED:`-annotated** (`quic_transport.rs:752-755`). Re-flagged (see limitation). |
| `actor_registry.rs:94` | `Box::leak(type_name.into_boxed_str())` | Deliberate string-interning leak with a documented paired reclaim (`clear_dispatch_registry` via `Box::from_raw`). |

- **Why ship at all:** all three are currently intentional, but they are the complete inventory of
  explicit RAII escapes in the runtime — precisely what §1 review should re-verify when teardown logic
  changes. Three `hint`s is not noise; it is a small, durable audit list. Shipped at `hint` (lowest
  severity) to signal "review", not "bug".
- **Limitations:** advisory only — ast-grep is single-file/syntactic and cannot prove the cleanup
  path, nor honor `// JUSTIFIED:` (see above). `$TY::leak` is restricted to the std `Box`/`Vec`/`String`
  set to avoid matching user-defined `leak` methods; a future `Rc::leak` etc. would be missed.

---

## Rejected candidates

### R1. `CURRENT_NODE` / `LIVE_ACTORS` global-access rule — **REJECTED (hotspot no longer a raw global)**

The §9-named hotspots have been **deglobalized**, so a rule keyed on them has no valid target:

- `CURRENT_NODE`: **no `static` definition and no code access** anywhere in the crate — all ~60
  occurrences are stale comments. It is now a `RuntimeInner` field reached via `rt_current()`
  (`hew_node.rs:139`, `runtime.rs:84`). A literal-identifier rule would match only comments (ast-grep
  ignores those) → **0 actionable hits**.
- `LIVE_ACTORS`: now the **module-private** `static LIVE_ACTORS_WASM: PoisonSafe<…>`
  (`lifetime/live_actors.rs:104`), reached only through same-file accessors (`with_live_actors` /
  `.access`). Rust **module privacy already enforces** "no raw access outside the owner module," so the
  proposed `files`/`ignores` cross-module rule is redundant with the compiler → **0 hits possible**.

Honest conclusion: the precise version of this rule is "enforce something the language already
enforces," so it adds nothing. (The CLAUDE.md §9 text naming these globals is now historical.)

### R2. `Box::into_raw` without nearby `from_raw` — **REJECTED (too numerous, unpairable single-file)**

`Box::into_raw($X)` = **96 production hits** (and 365 `from_raw`). It is the standard FFI
ownership-transfer idiom throughout this runtime; the matching `from_raw` is routinely in a different
function/file (e.g. a `Drop` impl or C-ABI free). ast-grep is single-file and cannot prove pairing, so
the rule would be ~96 lines of pure noise. A noisy rule is worse than none.

### R3. `ManuallyDrop::new` without paired drop/`into_inner` — **REJECTED (structural, correct-by-design)**

`ManuallyDrop::new` = 6–8 production sites, all structural manual-drop-control:
`inner: ManuallyDrop::new(actor)` struct fields (`lambda_actor.rs`, `duplex.rs`) and the
`let me = ManuallyDrop::new(self)` consume-self pattern (`duplex.rs:459,490`). The paired
`ManuallyDrop::drop`/`into_inner` lives in the type's `Drop` impl or a consuming method — invisible to
single-file ast-grep. These are the normal, correct way to build manual-drop types here, so a rule
would re-flag deliberate patterns → noise.

---

## Methodology

For each candidate: wrote the `.yml`; ran `ast-grep scan --rule …`; inspected **every** hit;
classified production vs in-file-test and TP vs FP; tuned for precision (the decisive move was
excluding in-file `#[cfg(test)] mod`s by enclosing-`mod_item` name, which cut `lock-unwrap` from 96 →
10); kept only precise/meaningful rules and downgraded both survivors to advisory severities
(`info`/`hint`); rejected the rest with the counts above. Both shipped rules parse cleanly and scan
without error.
