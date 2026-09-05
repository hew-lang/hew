# Core integration

## Initial checkpoint

- Preserve the profiler port-only address fix; focused profiler unit tests pass.
- Remove the unused LLVM metadata scraper and the help renderer's
  implementation-derived expectation test. Retain actual runner failure and
  malformed-input coverage.
- `make test-build-harness`, `make help` and `git diff --check` pass.
- Native ownership cutover and whole-compiler acceptance remain unfinished.

## Owned SSA availability

- Add a finite CFG fixed-point check for owned SSA availability, including
  incoming parameters, consuming operations, edge transfers, call/unwind
  results and loop-local dynamic definitions.
- Negative lifetime tests failed before implementation; all SIR crate unit
  and integration tests pass with the verifier hook enabled.
- Borrow-region and place-initialization checks remain separate unfinished
  components. Ownership operations remain closed in the relation table until
  those checks and the executable lowering are ready.

## Core acceptance manifest

- Added the initial six audited native value-semantics cases and their exact
  O0/O2 output and exit outcomes. `make core-acceptance` builds the native
  compiler once, then asks `xtask core-acceptance` to compile and execute each
  case at both profiles.
- The runner reports compiler version, host, profile and instrumentation and
  distinguishes source diagnostics, compiler/program crashes, timeouts, wrong
  exit/output and environment failures. It intentionally has no green safety
  or native-smoke placeholder while those manifest cases are absent.
- `make test-core-acceptance-runner`, the full `make core-acceptance`, and the
  focused `--case entry-return` recipe all pass with the isolated target tree.
- Repaired the existing `quick-xml` text-event inference ambiguity in the
  nextest ratchet without changing its whitespace or role checks; this lets
  the xtask runner compile on the current dependency graph.
- Factored the existing Rust format and JSON Clippy invocation into
  `make lint-rust`; `make lint` still runs that target and every pre-existing
  lint gate. A focused `make lint-rust CLIPPY_ARGS='-p hew-sir'` run passes.
- Removed the duplicate whole-workspace Clippy pass from the formatter hook;
  it still formats and restages staged Rust. `make lint-rust` and CI remain
  the mandatory Clippy authority.
- Consolidated each case to one expected outcome shared by O0 and O2. The
  value cases now print computed values, bytes mutation uses a `var` binding,
  and the runner uses RAII temporary directories with Unicode-safe summaries
  and explicit output-capture read failures.

## Call-result visibility

- Represent call results as normal-edge definitions in the SSA verifier,
  distinct from block-entry values and ordinary operation results. A result
  must be forwarded to a continuation argument before the body can use it.
- The new negative test first demonstrated acceptance of a call result as
  its own input. It now rejects operation, input, unwind and direct
  continuation uses while retaining valid normal-edge forwarding.
- The integrated native acceptance cases pass at O0 and O2; these exercise
  the existing native route, not a completed ownership cutover or sanitizers.

## Borrowed-input boundary

- Guaranteed inputs may be read or copied explicitly, but cannot be consumed,
  transferred into an owned block argument or returned without a copy.
  A call's borrow operand remains permitted; its operation contract must
  independently establish synchronous, non-retaining use.
- The consumption and return negatives failed before the check. Focused SIR
  verification also covers an owned copy and normal/unwind call borrowing.
- This does not admit local borrow regions, suspension or new ownership
  operations. Their operation contracts and executable lowering remain work
  for the first native owned-value slice.

## Result consumer intake

- Restacked the complete 19-commit Result consumer migration from PR #3349
  onto integration checkpoint `6892e1017` without textual conflicts.
- Regenerated the C ABI surface after the integration base and PR both added
  `hew_ask_error_translate_for_public_result`; the generator removed only the
  duplicate manifest row. `make cabi-surface-check test-cabi-surface` passes.
- `make test-vertical-slice` reproduced the stale transition after
  `vec_iter_free_fold_unwind`: `fs.read` now returned structured `IoError`,
  while its old fixture still forced a generic `Result.unwrap` panic.
- Replaced that panic assertion with an executable `IoError.NotFound` branch
  and removed the obsolete `os.args(index)` panic case. The replacement
  `os.args() -> Vec<string>` surface is already exercised on its success path,
  and ordinary Vec out-of-bounds traps have dedicated fixtures.
- The complete vertical-slice run reaches its final fixture after passing the
  replacement Result case. Existing #3127 and #3226 runtime failures remain
  explicitly classified by the harness rather than being migration failures.
- `make checked-mir-run` passes. `make checked-mir-verify` reports composed
  dump drift: the Result migration removes the obsolete `AskError` "no error"
  display arm, while the integration base changes ownership classes for actor
  handles and aggregates. Leave these generated files for one regeneration on
  the final combined integration base rather than recapturing them here.
- `make hew-fmt-check` and `make lint` pass with an isolated target directory,
  sccache and an eight-job build cap.

## Canonical loaded-module view

- Expose active modules as a deterministic borrowed view over the registry's
  retained declarations. Parse-cache and configured backing maps are not a
  second HIR source surface.
- Activate compiler stdlib modules only through the compiler distribution root
  selected by the existing installed/development resolver. Project and
  environment search paths cannot confer authority; an already-active
  lookalike is refused without mutation.
- Counterfactual runs proved the focused tests detect both reversed module
  order and accidental user-search-path selection. The focused
  `module_registry` nextest selection and package-scoped `lint-rust` pass.
- An exploratory full `hew-types` run with `test-artifacts` deliberately omitted
  stopped on the existing `std.channel` import setup in two channel tests; the
  failures occur before the changed registry APIs. The two import-bearing tests
  now use the existing repo-root test registry locally, preserving the default
  import-free helper policy while exercising the real `std.channel` module.
- Checker, HIR and compile-session consumers remain untouched for the combined
  S10 continuation after the entry-identity work integrates.

## Checked-MIR golden retirement

- Retired the exact raw/elaborated dump snapshots and their manifest. The
  executable checked-MIR corpus, transcript expectations, timeout, actor-leak
  counterfactual and shared structured-refusal authority remain the gate.
- Replaced the only dump-only concrete Vec ABI matrix coverage with a
  source-to-MIR test that checks typed runtime family selection and call arity
  while allowing incidental MIR presentation changes.
- The known `actor_link_monitor` refusal remains reported by
  `make checked-mir-run`; it is not converted into a passing transcript.

## Entry and test-root checkpoint

- Reused the canonical file compiler for selected Hew tests and carried the
  exact root occurrence plus its deterministic production peer through normal
  compile options.
- Removed the alternate frontend-to-native finishing route. A selected test no
  longer deletes an authored `main`; HIR assigns that exact declaration a
  stable internal callable symbol while the process adapter retains `main`.
- Carried process exit policy through HIR, SIR and MIR. LLVM lookup now uses
  complete callable keys for both the selected entry and its `Display` target.
- A concrete specialized `Display` impl remains a declared callable; a blanket
  generic impl is seeded into the normal HIR monomorphisation registry with its
  concrete arguments.
- Focused entry, selected-test, companion and callable-identity tests pass via
  `make test-strict`.
- A selected test can call an authored `main`, while the authored function is
  not executed as the process root. Directory-module entry insertion preserves
  the selection because real source occurrences are keyed by span, not item
  position.

## Explicit logical-failure transport

- Add a uniquely owned opaque fault for the private native status ABI, with
  paired allocation/release and borrowed reporting. Reporting preserves the
  canonical logical reason and returns I/O failure without unwinding across C.
- Unknown codes do not masquerade as recoverable hardware signals. Native-only
  exports carry explicit ownership contracts and are classified as internal.
- Reporting regressions fail against an empty implementation; a false success
  for an absent fault is also detected. Generated-call propagation and cleanup
  still require the physical backend's executable acceptance.

## Concrete type-fact authority

- Replaced SIR's empty-context ownership fallback with a checker-created
  `TypeFactService`. The service retains the declaration, trait-marker and
  collection-eligibility context that produced accepted rows, so later
  concrete specializations use the same authority and unknown types fail
  closed.
- A source-to-SIR generic tuple regression exercises a specialized tuple that
  had no row. The service publishes its concrete facts before SIR creates
  the value.
- Hand-built SIR fixtures now publish explicit rows for the structural types
  they model. This is fixture setup for the production boundary, not a restored
  fallback or a source-text oracle.

## Executable owned SIR

- String and bytes callable parameters now borrow ordinary inputs. Returning
  or storing a borrowed input emits an explicit `copy_value`; owned literals,
  call results and replacements are destroyed on normal, unwind and early
  exits.
- Mutable owned bindings remain SSA values. Conditional reassignment merges
  them through block arguments, while loop headers carry the current values
  explicitly across back edges and scoped locals are cleaned before leaving
  their block.
- Integer arithmetic now ends its block with `checked.binary`, an explicit
  normal result edge and the exact failure edges required by its type and
  operator. Every produced failure path destroys live owners before reaching
  the matching typed trap; the verifier rejects wrong kinds, non-trapping
  cycles and legacy checked arithmetic hidden in ordinary `Binary` operations.
- The old SIR-to-MIR bridges fail closed on this new terminator. Physical MIR
  realization remains the next layer and must preserve the explicit edges and
  ownership operations rather than inferring copies or cleanup.
- Ordinary `let` and `var` aliases now copy owned string and bytes bindings,
  preserving the source for later uses; explicit return and block-tail exits
  remain ownership transfers. Direct-call cleanup records only owners created
  while evaluating that call's arguments, so a nested later argument cannot
  destroy an earlier argument belonging to its enclosing call.
- Discarding a bare owned binding is now a read with no ownership discharge.
  A discarded block result receives copy semantics at its tail, then destroys
  only that new result; the source binding remains live. The same scoped
  distinction lets a block used by a return move its tail while a block used by
  an ordinary value expression copies it.
- Every HIR block expression now enters the same scoped lowering path. Its
  yielded value survives according to the surrounding copy or move context,
  while owned locals are destroyed before control leaves the lexical block.

## LLVM AddressSanitizer helper

- Added the crate-private LLVM 22 AddressSanitizer pass helper for the paired
  physical emitter. It marks only defined generated functions, rejects a
  missing target triple before mutation, runs the `asan` module pipeline and
  re-verifies the module.
- Focused tests prove emitted load/store checks and the ASan module constructor,
  retain runtime declarations as uninstrumented inputs, and link/run a local
  one-byte allocation overflow at both O0 and O2. Each execution requires an
  AddressSanitizer diagnostic rather than accepting a generic non-zero exit.
- The O2 sentinel initially disappeared as dead code because LLVM recognized
  the libc allocation/free pair. The fixture now uses the LLVM `nobuiltin`
  declaration attribute so the test observes generated instrumentation rather
  than an optimizer-elided access.
- The helper now consumes the exact target machine that built the physical
  module and rejects a triple mismatch before marking anything. The linked
  probe requires `ERROR: AddressSanitizer: heap-buffer-overflow` and `WRITE of
size 1`; matching clean in-bounds O0/O2 controls must exit without a report.
  Removing the `asan` pass makes the invalid O0 probe exit successfully, so
  linking the ASan runtime alone cannot satisfy the test.
- The ASan execution test resolves clang from `llvm-config --bindir` exported
  by the crate build script, the same LLVM authority selected by `llvm-sys`.
  It therefore needs no ambient `LLVM_SYS_221_PREFIX` at test runtime.

## Physical MIR checkpoint

- Added one target-realized physical module beneath ownership SIR. It carries
  exact callable identity, target-derived layouts, concrete storage, private
  status/result/fault call edges, and an immutable verified wrapper.
- Explicit SIR copy and destroy operations resolve once to typed bitwise,
  string, or bytes actions. Physical CFG edges transfer storage and never hide
  an implicit clone decision.
- Physical lowering reruns SIR verification and fails closed on missing target
  layouts, unsupported runtime wrappers, snapshot boundaries, and arithmetic
  operations whose failure cleanup is not yet explicit in SIR.
- Focused `hew-mir` physical tests cover scalar result-out lowering, concrete
  copy selection, missing-layout refusal, and malformed callable identity.

## Owned aggregate semantics

- Owned tuples and named records now use one semantic operation family:
  construction consumes ordered fields, ordinary projection creates an
  explicit independent copy, and destruction consumes the whole aggregate.
  The existing no-drop tuple make/get operations remain a separate bit-copy
  specialization.
- Demanded named record shapes retain exact checker declaration and concrete
  nominal-instance identity. Field recipes are derived once from the
  checker-published type-fact rows and shared with physical lowering; missing
  shape, type or clone facts refuse verification.
- Source regressions cover retained source fields, whole-record aliases,
  declaration order independent of initializer order, repeated projections,
  and a counterfactual missing-copy recipe.
- Direct aggregate calls now borrow owned parameters and return independent
  owners. Callable admission and verification both require the same exact
  tuple or nominal record shape, and admitted headers publish their shape even
  when demand does not reach the body.
- A source-to-SIR regression retains the caller's record after the call and
  reads fields from both owners. Removing the nominal descriptor makes the
  verifier reject the callable header, demonstrating that presentation names
  cannot substitute for the exact shape contract.
- Canonical `string.len()` now carries a typed `StringLen` runtime family from
  the trusted `std.string` declaration into SIR. Its semantic contract borrows
  one string, returns a bit-copy `i64`, and has no logical failure edge; an
  identical user extern remains outside this family.

## Legacy ownership pipeline retirement

- Removed the compiled HIR-to-MIR body pipeline and its ownership-inferencing
  LLVM consumer. Physical MIR retains only the shared target and LLVM mechanics
  required to emit the verified semantic program.
- Removed checker/HIR produced-value ledgers, their graph resolution and
  verification, non-executable source anchors, and exclusive release adapters.
  TypeFacts, declaration identities, extern contracts, and source mutability
  and consume diagnostics remain in their existing authorities.
- Preserved source-level checker and HIR coverage while retiring graph-shape
  assertions. Expression-lowering fixtures with non-entry return types now use
  ordinary named helpers, keeping the selected process-entry contract intact.
- Native build, workspace consumer checks, retained native execution tests,
  checker source tests, and HIR suites passed after the retirement.
- Irrefutable tuple and record patterns now remain one ordered, typed HIR
  destructure group instead of becoming unrelated synthetic projection lets.
  Nested aggregate fields form a second group through their resolved binding;
  HIR carries no ownership decision, leaving copy-versus-consume to SIR.
- SIR validates each destructure group against that aggregate's exact ordered
  descriptor, copies an ordinary owned source before consuming it, and binds
  every field result as a separately tracked SSA value. Wildcard and nested
  fields therefore remain explicit cleanup obligations; a malformed result
  type is rejected by the verifier rather than reclassified downstream.

### Local optional and error recovery

- Added lazy Option defaults and expression-local Result handlers with a named
  error binding. They normalize to ordinary typed variant matches; the semantic
  backend remains responsible for payload transfer and cleanup.
- Preserved lexical returns, loop control and closure captures in handler
  bodies. Required optional let-else exposes only the success payload, while
  postfix propagation now rejects Option/Result conflation.
- Formatter round trips preserve grouping around recovery expressions. Editor
  binding lookup now uses live lexical scopes, including handler-local errors.
- Frontend and native compiler builds validate the surface; source execution
  awaits composition with the semantic match continuation implementation.

- Fallible function clauses preserve the exact success and error types while
  reusing Result callable signatures. Ordinary returns, including tuple and
  Result-valued successes, select Ok; explicit error returns select Err.
  Checked return-site selections guide HIR construction without ownership facts.
- Nested closure and generator bodies retain their own return contexts.
  Generator `fails` composition and sandbox execution remain explicitly
  unadmitted pending their respective semantic consumers.
