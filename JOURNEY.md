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

## Owned variant semantics

- SIR variant descriptors retain an exact concrete enum type, indirectness,
  and declaration-ordered payload fields without choosing tag width, payload
  layout or allocation. Payload ownership recipes come only from the module's
  checker-published type facts.
- Variant construction consumes every payload field. An exhaustive variant
  switch consumes the enum and defines the active payload fields only on that
  arm's edge; missing arms, repeated tags and cross-arm payload forwarding are
  verifier errors.
- A runtime tag outside the verified descriptor is corrupt representation.
  It has no language-visible trap or unwind edge and must become a process-fatal
  backend trap without reading or dropping the unknown payload.
- HIR constructors and unguarded exhaustive matches now produce those semantic
  operations for exact user enums and concrete Option/Result instances. An
  ordinary enum argument remains with the caller, while the callee switches a
  copy; selected payload fields move into their arm and unbound fields are
  destroyed explicitly before the join.
- The same descriptors also carry checker-classified bit-copy records and
  enums such as `Point` and `Option<i64>`. Their operations have no owner glue,
  but retain exact shapes and field recipes instead of bypassing the semantic
  contract based on a scalar-only representation.
- Native enum storage now uses a target-measured tag and aligned payload area.
  Construction, consuming switches and recursive copy/drop execute the exact
  active variant's recipes; invalid tags terminate without touching payloads.
- Fixed-size scratch storage belongs in the callable prologue. Keeping it at
  a clone or drop site would accumulate stack space when that site runs in a
  loop, even with balanced heap ownership. The LLVM regression rejects that
  placement, and repeated native enum copies exercise the bounded lifetime.
- Native and paired generated/runtime sanitizer cases cover owned and scalar
  payloads, variant-size differences, repeated calls, and callee faults while
  the caller retains an enum owner.

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

### Uniform structural punctuation

- Structural data members now use commas across types, enums, actors,
  machines, wire records and supervisors. Executable statements and bodyless
  function declarations retain semicolons; block declarations remain
  self-delimiting. The parser rejects obsolete delimiters with precise spans.
- Parser and formatter tests cover strict rejection, trailing commas and the
  boundary between actor state declarations and executable local statements.
- Retained source fixtures are migrated by parser-confirmed delimiter spans.
  Concatenated and interpolated test-source builders require a follow-up pass;
  their failures remain fixture admission errors, not semantic test results.

- Composing later qualified-record and native enum coverage exposed structural
  semicolons in constructed sources. Migrated those declarations while retaining
  the original identity, visibility, payload and cleanup assertions. The affected
  checker and HIR suites now exercise their intended semantics again.

### Length-aware string execution

- Managed immutable strings preserve all valid UTF-8, including embedded NUL.
  Removed the parser's obsolete C-string restriction and replaced its rejection
  cases with parsed-value and formatter round-trip assertions.
- Native programs distinguish Unicode scalar length from encoded byte length.
  A string returned beyond its source scope retains its complete contents through
  copies, uppercase conversion and output; mutating derived bytes leaves the
  original string and byte value unchanged.
- The retained native suite and paired generated/runtime address-sanitizer suite
  exercise these behaviours at O0 and O2. Broader foreign API conversion and the
  source decoding functions remain separate unfinished work.

### Typed UTF-8 source operations

- Validating and lossy decoding are canonical source declarations with ordinary
  string and nominal Result types. A shared runtime contract checks signatures;
  module spelling alone cannot grant intrinsic authority.
- Module and named import aliases retain the checked runtime target. HIR uses
  that target directly and suppresses admitted floor bodies before attempting
  to lower a placeholder, while inconsistent signature facts report an error.
- Checker and HIR tests exercise canonical imports, user lookalikes, altered
  signatures, foreign error types and inconsistent compiler boundary facts.

- Validating and lossy decoding now execute through the shared native ownership
  pipeline. Native and paired sanitizer cases cover exact malformed/incomplete
  errors, Unicode and NUL preservation, source-scope exit, independent byte
  mutation, named import aliases and repeated decoding through both result arms.
- Byte-literal hex escapes preserve their raw byte values instead of re-encoding
  them as Unicode. Parser and formatter coverage retains every possible byte.
- Ordinary expressions rooted at an `error` binding take priority after return.
  Both statement and expression returns preserve member access, calls, indexing,
  propagation and operators; ambiguous explicit failure payloads use a named
  local or a block. This lets a handler return its error's formatted description.

### Explicit byte decoding

- Retired the implicit bytes-to-string method and its obsolete runtime symbol.
  TCP and QUIC text reads now validate UTF-8, with transport and decoding errors
  remaining distinct ordinary values.
- Hex and Base64 encoders construct their guaranteed-ASCII strings directly.
  URL decoding uses the shared UTF-8 validator, preserves decoded NUL and keeps
  its documented empty-string recovery. URL encoding no longer mistakes byte
  offsets after a multi-byte scalar for string positions.
- Byte-oriented examples and retained fixtures now decode explicitly and handle
  errors. The downloader collects bytes before decoding so a scalar can span
  network reads. Existing SIR resource/collection gaps still limit execution.

### One element protocol for vectors

Descriptor-backed vector operations now accept plain values without inventing
clone or drop callbacks. Owning elements retain their required callbacks;
copy-in, move-in, extraction, replacement and destruction keep distinct
ownership contracts. Runtime tests exercise independent copies and both plain
and recursive cleanup, including sanitizer execution.

The native corpus now retains nested-vector sibling extraction and nested
Result/enum extraction from application reports, plus independent vector copies
and strings that escape their parent. These remain explicit unsupported native
cases until semantic operations and their physical consumer are complete.

### Linear ASCII encoder construction

- Restored byte-buffer emission for Base64 and hex, avoiding repeated immutable
  string concatenation. Each encoder validates its completed ASCII output once
  with the canonical UTF-8 decoder and panics on an invariant violation.
- Kept URL byte/scalar/NUL fixes and compiler ownership boundaries unchanged.
  Special awaited text-read lowering remains with its compiler owner.

# Local Make configuration

The Makefile optionally loads the ignored `.env` before resolving build
configuration. Defined temporary-directory and Cargo settings are exported to
child processes, while command-line assignments retain precedence. Undefined
settings remain absent: exporting an empty `CARGO_TARGET_DIR` causes Cargo's
output-directory resolution to fail.

Validated missing-file handling, propagation of local settings to a child
process, and command-line precedence with Make. `make test-build-harness` passed
on Linux. Windows and macOS checks were not run for this change.

## Shared physical value emission

Extracted the existing concrete copy, destruction and variant-layout emission
into a function-independent value emitter. Ordinary bodies and container
callbacks can now use the same checked physical recipes without synthesizing a
function body or duplicating ownership decisions. Focused MIR/codegen tests,
including the LLVM sanitizer instrumentation control, pass after the extraction.

## Managed environment, path and stream boundaries

Environment, path, file-stream and stream error exports now borrow or return
managed UTF-8 strings. OS-facing environment values and paths reject interior
NUL before any operation, while stream data, callback arguments, collected file
contents and error messages preserve every byte. Empty string values use the
canonical null handle. The legacy file-read alias delegates to the same managed
file reader instead of allocating another string representation.

The stream layout regression exposed a remaining C-string encoder and decoder
in the shared channel envelope helper. String envelopes now copy the managed
value's complete bytes and validate UTF-8 before publishing a received owner.
The channel fixtures use managed slots; byte and aggregate envelope operations
retain their existing contracts. Callback tests exercise retained results and
independent input release, while filesystem probes prove rejected NUL paths
cannot open or truncate a valid prefix. Runtime tests and focused sanitizer
checks exercise the migrated ownership and error paths.

The HTTP stream-error test consumes the same managed error result through
`string_as_str` and `string_release`, preserving the runtime allocator contract
across the standard-library test boundary.

## Vector operation acceptance

Added executable scalar, optional extraction and mutation cases alongside
separate negative-index, invalid-replacement and empty-pop cleanup cases.
They exercise both borrowed and transferred receiver failure edges under the
same native and sanitizer suites. These cases remain pending until the physical
vector consumer is composed. The string lines helper now declares its vector
builder mutable, matching the checker-enforced mutation contract.

Regenerated the C ABI declarations after the managed OS and stream text
migration so exported string handles agree with the runtime signatures.

## Descriptor-backed native vector values

Vector construction, independent reads, receiver updates and removal now have
one typed runtime-operation contract through HIR, ownership SIR and physical
MIR. Physical descriptors reference the same element copy/drop recipes used by
ordinary aggregate values. LLVM emits one descriptor and any required callbacks
per concrete vector type; callbacks reuse the shared value emitter and never
free an inline element slot. Failed replacement and removal release the
transferred receiver before following the semantic cleanup edge.

The composed semantic and physical/codegen suites pass, including malformed
ownership/result/descriptor cases and the runtime C ABI layout comparison.
Zero-sized runtime elements preserve logical lengths while copying no payload
bytes; vector unit tests and unsuppressed owned-vector ASan/LSan pass. Complete
native execution and generated-code sanitizer acceptance follow this checkpoint.

The native vector cases now pass at both optimization levels, including paired
generated-code and runtime ASan/LSan. Initial failures were rejected fixture
patterns using bare option variants; migrating them to `.Some` and `.None`
allowed the intended zero-sized and retained-value behaviours to execute.
Full lint reached the remaining machine/actor dogfood coverage and the older MIR
measurement fixture, which still fail to compile through the new core. Those
failures remain visible while the native language implementation continues.

## Managed JSON and YAML text boundaries

JSON and YAML source-facing text now borrows the canonical managed string
handle. Getters, serialization and error exports return independent managed
owners, and the existing string-free exports release those owners. Empty text
uses the null handle throughout constructors, keys, values and parse input;
parsers receive the complete UTF-8 document and retain native format errors.
The existing resource boxes and child-transfer rules remain intact.

Direct Rust fixtures now allocate actual managed handles through a shared test
owner. Boundary tests cover empty and embedded-NUL keys and values through
builders, native serialization and reparsing. Retention tests release inputs,
sibling results and source containers while keeping other results alive, and
check error text after the slot changes or clears. Focused JSON/YAML checks,
address/leak sanitizers without suppressions, and standard-library JSON Clippy
pass. The full standard-library run identifies failures in the untouched
regex/DNS Vec-string consumers already recorded by the boundary audit; those
remain with their owners. Combined census and native/platform acceptance are
integration work outside this slice.

Windows validation found that the file-read compatibility entry point used a
managed string type whose import was still restricted to Unix platforms. The
import is now unconditional, matching the cross-platform function signature.
The integrated JSON/YAML tests pass, and the combined C ABI census reflects
their managed text arguments and results. A fresh Windows build verifies the
import repair; earlier Linux results do not substitute for that check.

## Shared map and vector value protocols

Map keys now add hash/equality callbacks to the same copy/drop descriptor used
by vector elements. The duplicate map-value descriptor is removed. Map copying
and key/value projections use the common callbacks, including owning composite
keys; the separate String-only key-copy path and composite projection refusals
are gone. Set copying inherits the same behaviour through its inner map.

The complete runtime/C ABI suite passes. New composite-key/value tests exercise
map growth, copying, extraction, removal and projections that outlive both source
maps; set copies and projected elements also survive source destruction. These
tests and the existing string-reference and recursive-container lifetime tests
pass under ASan/LSan without suppressions. The Make sanitizer target can select
integration tests while retaining its library-test default. Generated native
map lowering and platform validation remain subsequent work.

Borrowed map/set insertion now copies inputs before growth or replacement,
including inputs borrowed from the same collection. Callers retain their input
owners on either insertion outcome. Zero-sized value callbacks now track logical
owners during copying, replacement and destruction. The complete runtime/C ABI
suite and focused ownership integration tests pass, including unsuppressed
ASan/LSan checks and the generated C ABI census.

Descriptor-backed vector buffers now allocate, grow and release with the
descriptor's alignment and capacity. Legacy scalar buffers retain their matching
allocator. Descriptor sizes must preserve alignment between adjacent elements;
logical zero-sized vector elements retain an aligned backing address.

Aligned owning records exercise map/set projections and vector growth, copies,
slices, buffer transfer, source reuse and truncation. Aligned zero-sized vector
callbacks preserve both alignment and logical owner balance. Focused runtime and
unsuppressed ASan/LSan checks pass. The broader runtime/C ABI run passed all other
cases; its initial zero-sized test used alignment outside the existing map
contract, so aligned ZST vector coverage is now separate from map ZST coverage.
Cross-platform allocator validation follows integration.

## Managed regex and DNS text boundaries

Regex pattern, subject, replacement and capture-name arguments now borrow
managed strings; match/replacement/capture results own managed references.
Vector-producing regex and DNS paths retain each string on insertion and
release the producer's reference. Direct Vec getters release their returned
owners after reading, including canonical empty elements. The CABI String
push/get/set/pop/contains declarations now match the runtime's HewString
parameters and results, with corrected retention and transfer documentation.
Descriptor types, collection layouts and runtime operations are unchanged.

Capture presence stays encoded by vector length, preserving a present empty
capture separately from absence. The scalar compiler-facing capture helper
returns a string and therefore cannot distinguish empty from absent under
canonical-null representation; its contract now directs presence-sensitive
callers to the vector-returning capture API. No compiler route was added.
DNS rejects embedded NUL hostnames without resolving a valid prefix and keeps
existing empty/failure/deadline behaviour.

The full standard-library suite passes, including the formerly failing DNS
and regex Vec-string cases. Direct tests cover NUL-bearing text, multiple
matches, empty capture presence, getter/setter/pop ownership after parent
release, and DNS results after the input and runtime are dropped. Focused
address/leak sanitizers pass without suppressions; scoped CABI/stdlib JSON
Clippy passes. Combined census and platform/native acceptance remain with
the integration owner.

## Vector iteration semantic checkpoint

Confirmed the isolated iteration branch starts at d3cb33855. A scalar for-in
reproduced the missing VecIter aggregate contract. The checker type service now
supplies exact source record fields, including the canonical VecIter declaration,
and cursor copies use the shared recursive aggregate recipe. Removed the separate
synthetic vector cursor layout and iterator-only element read.

Cursor constructor/next rewrites retain operation identity while HIR consumes the
final checked receiver type; early inference had silently omitted iter rewrites.
For-in composes a while loop with the same next expansion as explicit next calls.
SIR field replacement uses destructure/reconstruction, and while-loop exits carry
current mutable values and destroy iteration-local owners on break/continue.
Nested range break/continue remains explicitly unsupported rather than selecting
an enclosing while's exit.

Make built the compiler after the record/field changes. Focused checker record
contracts and semantic cursor/for-in/cleanup/nested-field cases pass. Native O0/O2,
paired sanitizer acceptance and the broader semantic suite remain outstanding at
this checkpoint. The compiler cache client reported a server protocol error;
subsequent Make commands use an empty RUSTC_WRAPPER and the dedicated lane target.

Vector iteration validation found a consumed array-literal temporary being
carried into the cursor loop. The old for-in block hoist extended that temporary's
lexical lifetime; retaining the ordinary expression block fixes the producer.
Nested loop break/continue now verifies and executes. A focused malformed SIR
control removes the named local's break-edge destruction and remains rejected by
the ownership verifier. Return and bounds-fault edges have semantic coverage.

Validation through Make in the dedicated iteration target:

- `make hew-native RUSTC_WRAPPER=` and `make core-safety-build` succeeded.
- The temporary `iteration-matrix` Make target ran nine source cases at O0/O2;
  all native and paired generated/runtime ASan/LSan executions matched exact
  stdout, stderr and exit status. The safety driver also required generated LLVM
  address instrumentation. Cases cover scalar sums, explicit next/exhaustion,
  cursor copies, source mutation, strings, nested vectors, generic records,
  retained items, single evaluation, nested loops, early return and bounds faults.
- `make test-strict -o test-artifacts` selecting hew-types, hew-hir and hew-sir
  libraries/integration tests passed, including malformed ownership negatives.
- `make core-acceptance` and `make lint-rust RUSTC_WRAPPER=` passed.
- `make bench-mir -o hew` still fails: the cursor aggregate rejection is gone,
  but Vec.is_empty remains outside the current semantic vector operation set.
  A reduced nested record iteration passes with len() > 0. The benchmark shell
  additionally divides by zero after the rejected fixture; neither path was
  altered to manufacture a passing gate.

Limitations: native evidence is Linux only. Explicit next retains the existing
mutable-local receiver requirement. Ordinary aggregate field projection copies
owned values: the emitted next body consequently clones the cursor vector for
its length and item reads. This is safe but makes traversal copying quadratic;
a future common borrowed-projection contract should remove those copies without
introducing iterator-specific backend behaviour. MIR/backend/runtime, acceptance
fixtures and the other owner's stdlib catalog change remain outside this lane.

The combined integration retains the iteration examples in native acceptance
and paired generated/runtime sanitizer suites. These cover independent cursor
positions, nested value extraction, record replacement, source mutation and
single evaluation, plus cleanup on nested loop exits, return and fault. Managed
regex/DNS declarations are included in the regenerated C ABI surface.

Full lint caught missing ABI classifications for the new borrowed map/set
insertion functions. Their explicit borrowing contracts and generated surface
now agree. The retained native and paired sanitizer suites and the combined
runtime/C ABI/stdlib suite pass. Unsupported language coverage and the pinned
structural grammar remain separate full-lint failures.

Runtime semantic signatures now bind a canonical collection receiver and build
ordinary result types from its type arguments. Vector element, optional and
receiver-plus-element results use the same type templates that map/set operations
will consume, replacing the vector-only signature vocabulary. The full checker
suite passes with the existing vector contracts preserved.

Map and set semantic operation contracts now describe borrowed reads and
inputs, receiver replacement, independent projections and removal results.
The shared receiver templates derive nested Option/Vec/tuple results and reject
wrong keys, forged builtin names, incorrect arities and mismatched result types.
The full checker suite passes. Source producers, verified physical recipes and
native consumers remain the next layer; this checkpoint does not admit map/set
source programs through native lowering yet.

Permanent map/set acceptance now covers borrowed insertion, independent copies,
nested selected values, owned projections, scalar growth/removal, record keys,
membership results and indexing-fault cleanup. The first targeted map case
currently stops in SIR admission: the constructor still reaches aggregate
transfer lookup for a builtin map. The source/physical producers must consume
the new contracts before these positive cases can pass.

## Shared value layout naming

Based on `a38f3de3d`, move the shared value descriptor, ownership kind and
copy/drop thunks into `hew-cabi/src/value.rs`. Collections re-export the
canonical names without compatibility aliases. Preserve the physical layout,
callback signatures, clone rollback and drop semantics; keep the legacy
`HewTypeLayout` unchanged. Update runtime consumers, HTTP header-pair glue,
owning documentation and generated CABI census. Regeneration also refreshes
DNS/regex signatures whose implementations had already migrated in the base.

CABI and runtime suites, stdlib/testkit tests, format and scoped JSON Clippy
pass. The isolated compiler name substitutions were included for compilation
but are supplied separately for integration with the borrowed-projection work;
the main checkpoint excludes those files. No Hew source or behaviour changes.

CABI cross-checks pass for Windows MSVC and macOS ARM64; the wasm32-wasip1
CABI/runtime check also passes, exercising the retained 32-bit layout
assertions. Census regeneration, freshness and verifier self-tests pass.
Native platform execution and acceptance intake remain with integration.

Physical aggregate fields and vector elements now share the explicitly named
value recipe. Type-directed clone/drop selection uses one concrete identity
index, and the glue builder returns named tables rather than an expanding tuple.
Existing physical ownership, loan and native codegen checks pass unchanged.

## Common borrowed aggregate reads

Continued from the clean signed iteration checkpoint. Vec.is_empty now composes
Vector(Len) and scalar equality in HIR. The focused semantic suite and a native
nested record/vector case at O0/O2 pass without a new runtime primitive.

Inspection found BeginBorrow/EndBorrow and physical borrow operations dormant:
the semantic verifier still rejects local loans. The next bounded change will
activate local loan validation and shape-checked aggregate borrowing, keeping
ordinary escaping reads as independent copies. Argument evaluation order must
remain observable: a value needed across a later mutation still needs a snapshot.
The prior quadratic cursor read remains until this borrowing contract is complete.

The semantic contract now admits local whole-value loans and borrowed aggregate
fields with exact shape/type checks. The existing lifetime flow tracks local
loan availability and immediate parent dependencies: owners cannot be consumed
until children end, and normal, unwind, trap and loop edges must close loans.
Physical storage carries those SIR dependencies; LLVM extracts borrowed field
bits without a clone. Ordinary AggregateProjectCopy remains the owned extraction.

Contract checkpoint validation: `make test-strict -o test-artifacts` with
`NEXTEST_WORKSPACE_ARGS='-p hew-sir -p hew-mir -p hew-codegen-rs --lib --tests --no-fail-fast'`
passed, including malformed parent/field/ownership/cleanup negatives and LLVM
comparison against independent nested copies. Scoped `make lint-rust` passed.
An initial invocation used the wrong selection variable and launched the broader
workspace suite; it is not acceptance evidence and reports unrelated unsupported
language surfaces, including LocalPid aggregate transfer. No checks were weakened.
This checkpoint establishes the contract only: HIR-to-SIR call operands still
produce owned field copies, so cursor complexity and native safety acceptance
remain pending the scoped producer change.

## Map and Set value recipes

Map keys and values, Set elements, vectors and aggregate fields now share the
physical value recipe used for explicit copy and destruction. The type inventory
follows nested collections and the verifier checks exact collection identity,
component ownership, callback availability and target layout. Map and Set handles
use the target pointer carrier; their existing descriptor-backed runtime kernels
perform independent cloning and recursive destruction.

SIR's recursive collection-dependency check now covers all canonical collection
kinds, including collections nested inside one another, without adding a second
copyability classifier. Physical verifier controls reject mismatched key/value
recipes, missing destruction, foreign identities and forged carriers. Target
layout controls cover Linux, Windows and macOS; these are LLVM layout checks,
not native execution on those hosts. The affected SIR/MIR/codegen Make selection
passes. Source construction and Map/Set runtime-call emission remain the next
compiler boundary; this checkpoint does not yet execute the native map examples.

## Borrowed call-argument producers

HIR-to-SIR lowering now borrows aggregate field chains for read-only call
arguments through AggregateProjectBorrow. Tuple and record projections share
exact field validation with owned extraction. Each call ends its local loans
in reverse order on normal and failure paths before cleaning up argument owners.
No MIR, codegen, model, lifetime or verifier interface changed in this delta.

A later argument that can mutate or fail still requires earlier fields to be
captured as independent owners. Native validation exposed a related call cleanup
bug: argument evaluation could replace an outer binding, then temporary cleanup
destroyed its new owner. Both call producers now retain values still bound after
argument evaluation. Permanent controls read the replacement binding afterwards;
the missing-clone negative still exercises an owning extraction.

Validation through Make:

- `test-strict -o test-artifacts` selecting HIR/SIR/MIR/codegen libraries and
  integration tests passed. The final focused `lower_aggregates` test run also
  passed after adding runtime index-expression mutation coverage.
- Scoped `lint-rust` for HIR/SIR and `core-acceptance -o hew-native` passed.
- An external native matrix passed at O0/O2: nested record/tuple fields,
  temporary receivers, independent returned items, explicit next, loops,
  mutation snapshots, empty vectors, early exits and index/callee faults.
  The same cases passed with a paired compiler/runtime from `core-safety-build`,
  generated sanitize_address/\_\_asan_init evidence and ASan/LSan error exits.
- A linker-wrapped runtime counter proved that N=0,64,4096 traversals at O0/O2
  perform two setup vector clones, N+1 length reads and N item reads. Generated
  LLVM places the clones before traversal. A separate 4096-item case also passed
  under ASan/LSan. There are no per-step whole-vector clones.
- `bench-mir -o hew` remains red: `quote` has a call without a verified SIR
  contract. The benchmark then divides by zero on absent measurements. The
  earlier cursor and is_empty refusals are gone; no benchmark checks changed.

Native evidence is Linux only. Owned extraction and captures across later effects
remain copies by contract; this is a bounded call-argument borrowing change.
Map/Set source admission and the other owner's physical hash/equality work remain
outside this checkpoint. Native sources and runner logs are preserved for intake.

## Typed Map and Set operations

Physical collection calls now retain their exact map/set identity and optional,
pair or vector result descriptors. LLVM uses the shared value descriptor and
copy/drop callback emitter for vector elements and map values. Copy-in updates
preserve borrowed inputs; lookup and removal initialize owned outputs only on
their present paths. Entry projections use the target's tuple field offset.

The physical/LLVM boundary tests cover caller-supplied collections, including
optional extraction, receiver/presence pairs, projections and checked indexing.
Affected Make tests and scoped JSON Clippy pass. Native source construction is
still pending key-capability demand and descriptor emission. User-defined Hash
and Eq also need fault-aware callbacks through the existing logical-fault
transport; the direct-result callback ABI cannot silently turn those failures
into process aborts. This remains an implementation boundary, not an accepted
native-map milestone.

## Map/Set source producers: first composed checkpoint

Started the assigned Map lowering lane from b11433814 and retained the signed
borrow producer. Integrated the shared collection-admission/value-recipe commit;
both journal sections were preserved when their append points conflicted.

Canonical checker-selected Map/Set constructors and methods now become typed
semantic runtime calls. Map literal construction uses New/Insert and a mutable
internal receiver; the old layout-symbol and type-pattern lowering path is gone.
Call headers and ordinary binding/copy/return admission use the public canonical
collection helper. The producer publishes component facts and shapes; the shared
collection verifier owns recursive copy admissibility. Existing refusals for
unimplemented function values remain separate execution-domain checks.

The first composed Map copy/call/return, Set mutation/result and map literal
programs produce verified SIR. The full HIR/SIR Make selection passes, including
updated semantic projection assertions and meaningful ineligible-callable
negatives (Map headers are now valid; function-value headers remain refused).
No shared IR, verifier or physical producer changes belong to this checkpoint.
Map index, clone/emptiness composition and the complete permanent source intake
remain follow-up work. Native Map/Set operation emission and Hash/Eq capability
selection are still being implemented by their named owners.

## Recursive collection admission

Start from `6086afcf7`. Extend the existing constructor and iterator clone
walks with the container depth at each active declaration. A cycle may close
through an entry record after its Vec edge, while an unrelated outer buffer
cannot admit a later inline cycle. Keep leaf ownership and key capabilities.

Focused tests reproduce the original carrier rejection and cover generic entry
records, inline cycles below outer buffers, mixed/growing generic paths,
resource clone refusal and finite nested generic Copy values. The mixed-path
negative also exposed a stack overflow when Copy-layout parameter expansion
restarted its walk; reuse the existing declaration-termination authority before
that restart. No type-fact or TypeContext changes are needed.

Focused and full checker tests and scoped JSON Clippy pass. Native carrier
retry follows this checker checkpoint; fixture intake remains with integration.

The unchanged recursive carrier now executes at O0 and O2, including paired
compiler/runtime ASan with leak detection. Copies, replacement mutations and
children retained after parent destruction pass the embedded checks. The first
stdout comparison exposed a fixture oracle error: `.len()` counts Unicode
scalars, so the Unicode/NUL leaf length is seven, not its nine UTF-8 bytes.
The handoff preserves that first result and supplies the corrected expectation.
No further compiler boundary was encountered by this carrier; JSON/YAML
conversion and fallible Hash/Eq callback contracts remain separate work.

## Map/Set source producers: index, copies and composed projections

Map indexing now uses the typed semantic Index operation and its shared failure
cleanup. Explicit Map/Set clones copy a borrowed receiver with CopyValue; field
loans end after the copy. Set emptiness composes semantic Len with comparison
against zero. Neither copy nor emptiness adds an ABI operation. Diagnostics for
unsupported calls now identify the exact typed call target.

Focused source tests cover all permanent Map/Set cases, cloned map fields,
optional lookups/removals, independently owned projections, nested set mutations,
and set iteration through the semantic Elements operation. The missing-key
regression verifies that the field loan ends before its parent is destroyed.
The complete HIR/SIR Make selection builds and retains four failing positives:
Map is_empty has no checker-produced rewrite, owned Map values/entries hit the
checker projection allowlist, public Set to_vec lookup is absent, and
println_bool has no semantic runtime operation. Their named owners must supply
these contracts; no spelling-based substitute or ignored test was introduced.
All other selected tests pass, including malformed-IR ownership negatives.

Validation: cargo fmt --all; make test-strict -o test-artifacts with
NEXTEST_WORKSPACE_ARGS='-p hew-hir -p hew-sir --lib --tests --no-fail-fast';
make lint-rust with CLIPPY_ARGS='-p hew-hir -p hew-sir' passes. The native
compiler builds through make core-acceptance with
CORE_ACCEPTANCE_ARGS='--case map-value-copy', but both O0 and O2 stop at
E_PHYSICAL_LOWERING for Map(New), whose physical action is not in this lane.
An initial invocation used an unsupported --report option and was corrected.
Generated native code and sanitizer execution are therefore unproven for Map/Set
sources here. This is a buildable producer checkpoint, not full acceptance.
No type, shared IR, verifier, physical or backend files were changed.

## Checker-owned value capability selection

Published independent Hash/Eq plans carrying exact registered method identities
and impl binder arguments. Registration now retains normalized receiver patterns
and binder order in the immutable fact context; concrete specialization and
nominal lookup share the checker index. TypeFacts projects the same selection.

The focused record-contract suite compiles and passes, including user overrides,
derived defaults, reordered generic binders, concrete specialization and builtin
lookalikes. This is an incremental checker checkpoint; broader checker validation
and refusal controls remain in progress. It does not establish native execution.

## Boolean print calls

Boolean println endpoints now have a typed semantic contract and an exact
physical call to the existing Boolean runtime entry. Normalize the internal
Boolean carrier before crossing the C ABI. A permanent native case returns true
and false through an ordinary function and prints both at O0/O2.

The native print case passes. Combined semantic testing still reports the
known Map projection/emptiness/Set projection frontiers and newly exposed opaque
class-totality regressions in the selected-capability checkpoint; those remain
visible with their owners and are not print acceptance claims.

### Capability composition and applicability follow-up

Fixed the opaque declaration regression without weakening class publication or
its totality assertion: unsupported structural Hash returns no plan while the
ownership row remains available. Derived Eq and Hash now compose independently
through each member's selected method. The existing Eq container surface is
retained; Hash does not admit enums, tuples or Vec even when a user supplies a
Hash method. Canonical imported cycles terminate with no structural plan.

Concrete impl selection checks retained marker obligations through TraitRegistry.
Inline bounds, impl where clauses and method where clauses on impl binders are
retained. Custom traits, associated-type constraints and other predicates that
the immutable marker registry cannot prove explicitly refuse; no general trait
solver is duplicated. Concrete specializations keep an empty argument vector,
and generic fallback no longer depends on specialization registration order.

Expression comparisons now use exact specialization lookup. The remaining
name-only compatibility helper shares that index but cannot specialize without
receiver arguments. Its only consumers are the legacy HashSet and HashMap
CollectionMethodDispatch producers in check/methods.rs. Final-core producers must
replace those facts with capability_plan; that file belongs to the integration
work and was not changed here.

Validation: the complete hew-types nextest suite passes through Make, including
the opaque and imported-recursion class tests, selected-member composition,
marker-bound refusal, unprovable-bound refusal, and comparison/selector agreement.
Rust formatting and scoped make lint-rust pass. This is Linux checker evidence;
Windows/macOS and native callback execution were not run for this checker slice.

## Executable value capability demand

Map and Set operations now retain checker-selected Hash and Eq implementations
in ownership SIR. A user operation names its exact declaration, specialization
arguments and callable; requesting it demands the method body through the same
instance service as a source call. Derived operations recursively select each
field's implementation, preserving field-level overrides. Shared verification
rejects missing component plans and mismatched callable identities or signatures.
The compilation session requires callback bodies before admitting an executable.

The derived-record and user-override source tests pass. The generic-key positive
exposes an older checker layout admission failure and remains enabled. Combined
SIR/session testing also retains Map projection/emptiness/Set projection failures
and two stale mutable-collection source fixtures; their repairs are in progress.
This semantic checkpoint does not yet emit native key callbacks or change the
runtime fault ABI, so it is not native Map/Set acceptance.

Physical MIR now resolves selected value operations to exact scalar, sequence,
aggregate, variant or collection recipes, or to the selected private-ABI
callable. Its verifier checks component selections, method signature and body
presence, and refuses collection construction without both key capabilities.
The physical suite passes, including deliberate recipe/body/selection corruption.
Native callback emission and dynamic runtime fault propagation remain pending.

## Source mutability fixture alignment

Use a mutable stack in std.path.normalize. Keep privslot's private generic
Slot<T>, cross-module Store<T> impl identities and generation stamps, while
making add copy self into a mutable local and return the updated Store<T>.
The importer now rebinds its mutable store explicitly after each addition.

The focused hew-compile tests
imported_generic_impl_bodies_publish_each_checker_owned_declaration and
cross_module_span_key_collision_unary_minus_and_string_lit_do_not_collide
pass through Make. Native package-fixture validation is in progress.

Native validation: make hew-debug succeeded. Running the package fixture stops
before execution with E_SIR_UNSUPPORTED: the imported generic Store::add
callee is not an ordinary user-function call in the current SIR domain. The
private generic impl identity is preserved and both focused compiler tests
pass; generation output is not claimed as natively verified at this checkpoint.
The compiler limitation is outside this source-fixture-only change.

### Opaque value method selections

The public capability query now returns a ValueMethodSelection whose concrete
receiver, capability and selected plan are private. Read-only getters expose the
binding and plan; only TypeFactService mints the handle after successful selection.
The recursive selector continues to return ValueMethodPlan internally, preserving
the existing selection authority without a second table or public constructor.

The Types API tests retain the method identity, generic binder, specialization,
derived composition and refusal oracles. They additionally check the exact receiver
and capability retained by user and derived selections. The complete hew-types
suite passes through Make. This checkpoint is based on the checker-only precursor;
semantic and collection-method consumers require coordinated API migration.

Rust formatting and scoped make lint-rust pass. Validation is Linux Types-only;
full-workspace and native/platform execution checks were not run for this API slice.

### Checked capability provenance through semantic and physical lowering

Semantic plans now retain the immutable checker selection and only add the
executable callable demanded for a user method. Verification checks the exact
receiver, operation, declaration and specialization. Physical callables retain
that declaration and specialization so the physical verifier can enforce the
same selection after lowering. The callable metadata is integrated from the
compiled fault-ownership checkpoint.

New negative tests reject compatible substitute functions, missing user bodies,
structural selections with user callables, and selections transplanted across
operations or types. The corresponding semantic and physical tests pass. Borrowed
collection length reads now demand no key callbacks; construction remains the
point at which a key descriptor and its selected bodies are needed.

Combined SIR, physical MIR and compiler-session tests compile. The new missing
construction-plan oracle exposes the verifier check pending integration; existing
generic-key, map projection/emptiness and set projection frontiers remain enabled.
Scoped Rust lint passes. This is a compiled checkpoint, not native Map acceptance.

## Collection source admission: public set snapshots and mutation tracking

Started the disjoint checker lane from 82bd0c645 and retained the recursive
admission prerequisite abb4a9123, preserving both prior journal sections.
The baseline complete checker suite passes.

HashSet.to_vec now uses the existing ToVec dispatch and returns Vec<T> through
the shared collection signature table. It checks arity and inferred result
types without adding a runtime alias. Map and Set mutating methods now share
Vec's semantic UpdatedReceiver/UpdatedReceiverAndValue interpretation, so writes
count for mutability warnings and require var on the containing binding. Private
record parameter mutations retain the existing Vec refusal.

The complete checker Make selection passes after correcting older positive test
receivers to use var and retaining the private-parameter negative. New source
tests cover exact Set dispatch/results, invalid snapshot calls, direct and field
mutations, read-only controls and the permanent Map/Set mutation warnings.
Map projection admission and canonical Map emptiness dispatch remain follow-up
work; HIR/SIR lowering and runtime capability/fault producers remain root-owned.

## Collection source admission: semantic projections and Map emptiness

Removed the separate representation allowlist for Map projections and its stale
tests. Projection calls and both iterator producers now use common collection
admission, and Map value copying uses the recursive clone proof already shared
by collection iteration. Nested collection keys follow that same semantic proof;
the replaced Map-specific clone walk and layout-based key copy predicate are
deleted. Resource and function-bearing values remain refused by source tests.

Map is_empty now has canonical typed dispatch with a Boolean result and no
runtime endpoint. Its runtime-operation conversion explicitly returns None;
root owns the HIR composition from Map length. The linked-symbol tests still
check actual runtime endpoints and separately require this composition to have
no symbol. Public Map emptiness checks arity before recording dispatch.

The checker selection builds, but this intermediate checkpoint is not full
acceptance: the stricter shared clone proof exposes a forward-declaration timing
defect in mutual recursive Map values, and nested owned-key projections still
hit the old layout-key value-admission path. The next bounded step consumes the
selected capability API and removes that obsolete key-admission contract,
including the reported generic Key<i64> refusal. These failures remain visible;
the tests are neither ignored nor changed to expect rejection.

Map emptiness now composes the checked Map length operation with integer zero
in HIR, matching the existing Vec and Set path. The permanent emptiness, owned
Map projection and Set snapshot SIR controls pass after composing the checker
checkpoints. Generic key admission and construction-plan verification remain
pending their compiled follow-up checkpoints; native callbacks are still pending.

## Exact collection key admission

Replace named-layout Map/Set admission with independent Hash and Eq selection
from TypeFactService at the exact substituted semantic type. The source resolver
uses the same capability authority. Remove the obsolete layout tables, their
name-only method selector, geometry helpers, public exports and consumers. Keep
the existing scalar Set metadata and its malformed-output controls.

Map value-copy admission now retains forward declarations in the existing
admission queue until registration and inference finish. Preserve the owning
template scope in that obligation, and check its values even when the key is
abstract. Concrete resource/function values remain refused. Bare template Set
keys use their declared bounds and do not acquire concrete scalar metadata.

The exact generic Key<i64> Map/Set examples, owned generic keys with nested
projection results, both forward-declaration orders, recursive Map values,
resource and closure refusals, and bounded template controls are covered by
source tests. The prior owned-key nested-value and recursive-forward-value
failures are repaired. Literal-defaulting tests caught a missing conversion
before semantic capability lookup; use the existing materialization operation.

Validation runs through Make: the complete hew-types library/integration suite
and scoped hew-types/hew-analysis Clippy. Removed tests asserted the retired
geometry tables and are replaced by source capability and failure controls.
This checkpoint changes checker admission only; root owns SIR capability
construction, physical lowering and native callback acceptance. No new native,
Windows or macOS execution is claimed. runtime_call.rs remains handed back to
the fault-ABI owner after the earlier MapIsEmpty checkpoint.

Construction verification now requires both selected key operations at Map and
Set construction, integrating the completed semantic check from the fault lane.
Together with exact generic key admission, the combined checker, analysis,
SIR, physical MIR and compiler-session suites pass. This includes capability
provenance mutations, absent construction plans, borrowed read demand, generic
impl specialization, owned projections and source capability refusals. Native
key callback emission and runtime fault consumers remain the next boundary.

Added permanent native and paired safety fixtures for derived outer keys using
exact generic Hash/Eq overrides, composite equality across vector and active
variant/optional payloads, and Hash/Eq callback faults with live callback locals
and nested insertion payloads. Their SIR source controls pass and now require
every demanded callable to lower. Native callback and sanitizer execution remain
pending the emitter and fault-kernel composition.

### Selected physical key callback emitter

Added a child emitter that predeclares the selected capability graph and executes
physical Scalar, String, Bytes, Aggregate and User Hash/Eq recipes. Variant,
Vector, Map and Set recipes implement structural Eq; their derived Hash recipes
explicitly refuse because the checker does not admit them. User calls preserve
the selected private ABI, exact fault/status and borrowed carriers. Callback
results are staged until success. Fields exclude padding, byte hashing visits
only the active region, and floating keys use coherent total bitwise Hash/Eq.
Container comparisons borrow slots and release their local iterators on all exits.
Map/Set key descriptors reuse the existing value clone/drop descriptor generator
and require both selected key capabilities before emission.

The child compiles with the separately staged parent hook, and the existing
hew-codegen-rs suite and scoped make lint-rust pass. This is an initial compiled
checkpoint; direct callback execution and fault regression tests are the next
validation step. At this base the checker also refuses derived Map/Set Eq, so
those recipes are present for the physical interface but are not source-admitted.

### Key callback admission and execution validation

Removed the unreachable structural Map/Set Eq emitters after confirming the
checker explicitly refuses those capabilities. Map/Set physical recipes now fail
closed; this supersedes the initial checkpoint's broader recipe implementation.
Key descriptors are emitted only for concrete New operations, and a demanded
constructor with either key capability missing explicitly refuses. Borrowed
collections do not acquire descriptors just because another collection demands
the same key type.

New JIT tests execute callbacks at O0 and O2: total floating keys, padding-free
aggregate composition through selected user methods, counted strings with embedded
NUL, active byte regions, borrowed owned-user receivers, Vec elements, active
variant payloads, and exact nonzero status/fault propagation with untouched caller
outputs. Malformed physical tests reject missing components and unadmitted Map/Set
recipes. Windows x64 and macOS arm64 target emission LLVM-verifies, including key
descriptors, byte hashing and private borrowed calls; native execution is Linux-only.

The base's legacy collection admission still refuses Bytes and Vec/Option record
keys before SIR despite their capability-level support. Those recipe tests therefore
exercise the emitter directly with physical layouts and already-selected component
callbacks. They do not change admission or claim full collection-kernel integration.
The integration owner is composing the source-admission and fault-kernel consumers.

The complete hew-codegen-rs Make suite, scoped make lint-rust, Rust formatting and
diff checks pass. The parent integration hook is isolated in its own commit; the
child only consumes PhysicalValueMethod and PhysicalCallable, so the opaque
checker-selection migration requires no emitter-side API change.

## Map and Set callback fault ABI

Start from `13c4e9c09`. Hash and equality callbacks now return logical status
with scalar and opaque fault outputs. Map and Set operations migrate in place;
operations that do not invoke callbacks retain their ABI. The kernel forwards
fault status and ownership without inspecting or releasing the fault.

Resize builds borrowed slot copies in separate storage and commits only after
the insertion probe also succeeds. Failure frees that storage without dropping
its copied elements. Transfer-in failure preserves both inputs; copy-in failure
releases its staged key and value clones. Existing successful ownership paths
remain covered by the migrated descriptor tests and benchmark.

The runtime/CABI library and existing runtime/CABI tests pass. Scoped JSON
Clippy passes. Fault-specific output, atomicity and release-balance tests follow
this compiled interface checkpoint. Compiler consumers, ownership contracts and
generated census updates remain with integration.

Fault protocol acceptance now covers real logical fault owners from hash and
equality callbacks. Lookup, cloned lookup, contains, removal, moving removal,
transfer-in and copy-in insertion leave result sentinels untouched on failure.
Tests cover occupied and vacant probes, partially staged rehash, the incoming
hash after rehash, and equality after successful rehash. Copies borrowed from
the same Map or Set remain readable after a failed insertion. Every scenario
checks exact owner balance and verifies original contents after resetting the
callback state.

The full runtime/CABI suite, focused JSON Clippy and benchmark JSON Clippy pass.
The fault protocol, composite values, owned entries and string refcount cases
also pass ASan/LSan without suppressions. CABI compiles for Windows x64, macOS
arm64 and WASI; runtime execution and sanitizer validation were on Linux.
Compiler consumers and ownership/census integration remain with their owner.

### Collection fault lowering: semantic and callable checkpoint

Integrated the callback-status runtime and C ABI changes. SIR now requires selected Hash and Eq methods at collection construction and tracks active faults in its existing path-sensitive ownership flow, including functions with no owned values. Added controls for absent, abandoned, overwritten and mixed-predecessor faults. Physical callables retain their checker declaration and concrete instance for capability identity verification.

Validation: SIR and physical MIR unit suites pass. The full SIR run reaches four outstanding collection source-admission cases (generic key specialization, map emptiness, map projections and set projection), assigned to the parallel checker work. The key descriptor emitter and callback-failure compiler ABI remain pending integration; this checkpoint does not claim native Map execution.

### Collection callback status emission checkpoint

Native Map/Set callback kernels use status plus presence/fault outputs. Emission tests status before reading outputs and releases a semantically consumed receiver on callback failure before SIR cleanup. Map index absence initializes the bounds fault only on successful lookup; existing callback faults retain their status and owner. Regenerated the C ABI census and corrected clone-in ownership parameter arities.

Compiler checks and scoped SIR/MIR/codegen JSON Clippy pass. The Windows private status/result/fault ABI test passes. FFI classification and its regression tests pass; generated-surface regressions are running. This is an intermediate compiled checkpoint: source contracts and SIR/MIR callback-failure edges still require their coordinated file handback, and native Map execution awaits key descriptor emission.

### Collection callback faults through the compiler

The runtime semantic contract now identifies callback faults and determines when a cleanup edge carries an active fault owner. Map/Set hashing operations propagate that owner through SIR ResumeUnwind and physical fault state. Map index materializes a missing-key fault on the same edge only after a successful absent lookup; existing callback status and fault owners are preserved. Static vector and bytes bounds checks keep their static trap cleanup.

Source SIR tests cover all callback families and reject abandoned/replaced faults and failure-edge result use. Physical MIR rejects replaced and absent callback faults. The LLVM protocol oracle follows actual control flow and output operands to prove status-first reads, receiver cleanup and exact fault/status forwarding. Its collection ABI matrix verifies Linux, Windows x64 and macOS arm64 targets.

Validation: complete Types suite, SIR/MIR unit suites, focused collection fault tests, complete codegen suite and scoped JSON Clippy pass. Native selected-key and callback-fault acceptance is pending the key descriptor emitter and permanent source fixtures being integrated separately; no native Map acceptance is claimed here.

Removed the redundant User-only check inside generic direct-call resolution. Its sole caller continues to admit only checked User and ImplMethod targets, and resolution still uses the exact declaration and checker-resolved type arguments. Existing direct-call and generic free-function SIR tests pass. The package-level generic impl regression is being validated with the coordinated compiler-session fixture.

### Native collection callback acceptance

The composed selected-key emitter, runtime ABI and compiler fault consumers pass
the complete native and paired generated/runtime ASan/LSan acceptance suites at
O0 and O2 without suppressions. Generic selected methods, vector/variant/optional
key equality, copy isolation, owned projections and callback-local cleanup execute
successfully. Missing-key indexing retains its bounds failure.

The older field-lookup cleanup oracle expected a newly created static trap.
It now requires propagation of the active fault and explicitly rejects replacing
that fault, while retaining its loan-before-owner-cleanup assertions. A separate
native and paired safety indexing control preserves DivideByZero from a selected
Hash callback instead of replacing it with IndexOutOfBounds. Focused SIR source
and native/safety controls pass.

The generic impl call guard is repaired, but package execution now reaches the
next unsupported boundary: mutating a collection through a record field. The
shared receiver-place implementation is in progress; package acceptance remains
unproven and no source workaround is introduced.

## Logical zero-sized collection keys

Start from `d8000b815`. Empty record keys now pass the shared key-layout
validator with their logical size unchanged. Slot metadata already guarantees
positive stride and aligned, non-null key addresses. Clone and drop callbacks
continue to represent logical owners even when the key occupies no bytes.
An empty key in the last slot may use the aligned one-past address; callbacks
must not read payload bytes that do not exist.

The related entries projection now allocates nonzero scratch storage for an
empty pair while keeping its descriptor size zero, matching the existing Vec
allocation convention. No callback ABI or compiler admission changes are needed.

Focused tests cover replacement and tombstone reuse, aligned Map/Set keys,
empty projections and their copies, last-owner release, null-key rejection,
exact hash/equality fault propagation and staged-clone rollback. A private
resize test exercises successful ownership transfer and hash-fault rollback:
valid equal empty keys cannot naturally fill a table enough to trigger growth.
The descriptor/projection/fault regression suites and CABI library tests pass.
The new zero-sized-key cases also pass ASan/LSan without suppressions on Linux.
Windows and macOS execution was not run; allocation uses the same Rust Layout
and allocator APIs across targets. Permanent native empty-record coverage stays
with integration.

Scoped runtime/CABI JSON Clippy and Rust formatting pass after using explicit
raw descriptor pointers in the new fixtures. The final focused unit and FFI
boundary tests pass on the formatted source.

## Collection place and empty-key acceptance

Added permanent native and sanitizer cases for generic method mutation through a
record field, nested Vec/Map/Set updates with retained owning siblings and copied
parents, and a nested map callback failure. The checker accepts these programs;
SIR currently rejects field receivers at its local-binding-only move requirement.
These cases remain pending the shared receiver-place lowering implementation.

Added an empty-record-key case covering map replacement, copy independence,
projections after clear, a zero-sized entry pair and set snapshots. The source
passes semantic checking; native and sanitizer execution are in progress after
the runtime layout fix. The SIR regression requires the normal collection
operation contracts for the same source.

## Generic impl dispatch regression coverage

Start from the compiled integration checkpoint and preserve the package API.
The private generic record test now enters the shared Session and demands the
exact string specializations of both imported Store methods. A local control
requires distinct i64/string instances of one impl declaration and verifies
that repeated calls reuse their semantic callable IDs.

Add the private generic package to the existing native monomorphisation test
suite using its shared bounded command helpers. Compilation and execution must
succeed at O0 and O2, independently asserting the generation output `0\n1\n`.
The test creates no replacement package source or alternate compiler harness.

Both focused Session regressions compile and fail at the known SIR generic
resolver guard: an ImplMethod target is rejected as not an ordinary User call.
This is an intentional red checkpoint pending the separately owned resolver
fix. Existing malformed type-argument/signature tests are retained. Native
package execution remains required after composition; HIR identity alone is
not acceptance, and later compilation stages are not yet proven here.

Validation uses `make hew-native` (pass), focused `make test-strict
-o test-artifacts` selections for the two Session tests (both fail at that
guard) and the new CLI test (fails with E_SIR_UNSUPPORTED for Store::add at
O0). The CLI test uses the package-aware `hew build --pkg-path` command.
O2 and native execution have not been reached on this base. No SIR producer
or package fixture API was changed.
`cargo fmt --all`, `git diff --check`, and `make lint-rust
CLIPPY_ARGS='-p hew-compile -p hew-cli'` pass. The test checkpoint is ready to
compose with the resolver fix, but does not claim runtime acceptance.

### Mutable collection fields share aggregate assignment reconstruction

SIR resolves a mutable local place before evaluating runtime arguments, then takes the current root version apart after those arguments finish. Field assignment and Vec/Map/Set receiver transforms share the same checked record and tuple path, extraction and reconstruction. The runtime consumes the leaf; retained siblings remain in the existing owned-live relation until normal reconstruction or fault cleanup. Argument temporaries are classified before extraction so retained sibling owners are not mistaken for temporary arguments.

The focused runtime-place controls cover generic impl-local records, nested records and tuples, receiver reads and parent replacement in later arguments, returned Map/Set values, copy independence, bounds and callback cleanup, and rejected immutable or malformed places. Existing vector iteration and field assignment controls and the SIR/MIR/codegen unit suites pass. Native field fixtures and package-level generic impl acceptance are pending composition with the integration branch.

## Native collection milestone accepted locally

The combined compiler passes all native acceptance cases and paired generated
code/runtime ASan/LSan at both optimization levels after shared field mutation
lowering. This includes nested owning siblings, receiver-reading arguments,
independent parent copies, callback-fault propagation and logical zero-sized
keys. Imported private generic Store methods now lower through Session and
execute the package's original generation-stamp oracle at O0 and O2.

The combined SIR, physical MIR, LLVM and compiler-session suites pass, as do the
complete runtime/C ABI and standard-library suites. Full lint was rerun and
retains the known source-lowering gaps in JobState and starts_with plus the
pinned grammar mismatch; Rust lint is clean. This is a native collection
milestone, not completion of the remaining callable, resource, actor or sandbox
work. Ordinary selected equality is isolated in its own implementation branch.

## Selected value-call semantic contract

Added an explicit SIR ValueCall that executes the exact selected Hash or Eq
method from the existing module capability table. Both methods borrow their
inputs, define a scalar result only on the normal edge and require cleanup
that propagates the original fault. Standard call visitors, result dominance,
boundary ownership, CFG rewrites and fault lifetime checks cover the operation.
The module and function-in-module boundaries require and validate its selected
capability evidence. No second plan registry or C runtime endpoint is added.

Focused contract tests cover both methods, missing selections, operand arity,
exact types, forbidden ownership transfers, result typing and discarded faults.
The SIR library and contract tests pass; scoped SIR/MIR/codegen Rust lint passes.
Physical lowering currently refuses ValueCall explicitly, and there is no source
producer yet. Native selected equality remains pending those two layers. Scalar
float operators retain IEEE comparison; structural selected Eq retains the
existing documented bit-pattern rule for float members.

## Ordinary selected equality producer

Composite and bytes equality now demand the exact selected Eq plan and emit
ValueCall with borrowed inputs, a normal-edge bool result and fault cleanup.
Inequality negates only the successful result. Ordinary string comparison keeps
its existing counted-string operation; scalar numeric comparisons retain their
numeric semantics. Source controls cover independent vectors, owning tuples and
records, selected nested user Eq, active Option/Result payloads and faults.

A shared call-read helper now captures a whole binding before a later argument
can replace it. It preserves stable projection loans and avoids copying ordinary
stable inputs. A further source control exposed tree.children.push(tree): the
parent was consumed before its borrowed insertion input. The receiver transform
now snapshots that exact alias before decomposition and shares the snapshot
across repeated inputs. The prior SIR ownership-lifetime refusal is reproduced
by the regression; the corrected source passes verification.

All SIR library and integration tests and scoped Rust lint pass. LLVM emission
for ValueCall remains in the separately owned physical implementation; no new
native equality acceptance is claimed at this checkpoint.

## Selected equality acceptance sources

Added permanent native and sanitizer oracles for composite equality with generic
user Eq, Option/Result tag and payload distinctions, scalar versus wrapped float
semantics, callback cleanup through a borrowed field, argument evaluation order
across equality/direct/runtime calls and insertion of a parent into its own
collection field. Every source passes the shared SIR verifier. Native execution
is pending physical selected-call emission; these expected results are not yet
accepted as executable evidence.

## Ordinary equality uses selected capabilities

Ordinary non-numeric equality now records demands in the existing generic
instantiation obligation graph and resolves concrete demands after declaration
registration and inference. Both concrete comparisons and instantiated generic
comparisons ask TypeFactService for Eq, allowing nested selected user methods.
Top-level user dispatch and aggregate ordering retain their existing routes;
ordinary scalar float comparisons continue through the numeric path. The Eq
leaf admits bytes independently of Hash. The separate Vec.contains admission
contract is outside this change.

The checker layer compiles through Make and the existing eligible generic
comparison control passes. Focused selected-Eq regressions, obsolete bytes
rejection expectations and full affected checker validation remain pending at
this checkpoint. Native selected-operation lowering belongs to the integration
owner and has not been changed or validated here.

Selected Eq admission now covers independent owned values, bytes nested through
records, tuples, Vec, Option and Result, and user methods whose ignored members
have no derived Eq. Generic calls reuse the same demands after inference,
including composite Eq bounds forwarded through an abstract caller. Controls
reject no-Eq concrete instantiations and verify that an element's Eq does not
grant Eq to its containing Map. Forward declaration registration, exact generic
method identity, both Result payloads and aggregate ordering during inference
are covered. Unused abstract branches in the separate Vec.contains layout gate
have been removed along with the old ordinary-comparison eligibility consumer.

The complete hew-types unit and integration suites pass through
`make test-strict -o test-artifacts NEXTEST_WORKSPACE_ARGS='-p hew-types --no-fail-fast'`.
Scalar float comparisons retain their existing checker route; selected float
Eq remains a distinct capability. Native value-operation, failure cleanup and
runtime execution evidence remain the integration owner's next boundary. No
HIR, SIR, MIR, codegen or runtime source was changed.

`make lint-rust CLIPPY_ARGS='-p hew-types'`, Rust formatting and diff checks pass.

## Bytes equality source composition

The combined checker and SIR suites pass with selected Eq admission composed
with its source producer. The permanent bytes case covers independent
NUL-containing payloads, different lengths, nested owned data and a record key
whose user Hash pairs with derived Eq over bytes. Bare bytes gains no Hash
capability. Native execution still awaits the selected-call physical layer.

## Transparent argument transfer review fix

Review found that Vec.from(left) bypassed a required argument snapshot because
its transparent HIR wrapper hid the binding from the shared transfer helper.
The equivalent bare-binding control passed while the wrapped control failed the
SIR ownership-lifetime verifier after a later operand cleared the source.

The transfer helper now unwraps transparent expressions after checking that
concrete types agree, then applies the existing copy/move and protected-binding
rules. Paired bare, wrapped and nested-wrapper controls and the permanent native
read-order source cover the fix. The complete SIR suite and scoped Rust lint
pass; native wrapper execution remains part of selected-call integration.

## Selected value calls in physical MIR

Lower the semantic selected-value terminator to a distinct physical call using
its exact type and capability. Its operands borrow storage, its scalar result
exists only after success, and its required cleanup edge receives the original
fault. Ordinary and selected calls share argument transfer lowering and the
status/result/fault dataflow transfer. Physical verification checks selection,
arity, argument and result types, borrowed transfers and both CFG paths.

Focused verifier controls cover missing selections, malformed signatures,
forbidden transfers, failed-result visibility and discarded faults. The physical
MIR library tests and scoped MIR/codegen JSON Clippy pass. LLVM still refuses the
new terminator explicitly in this compiled layer; callback emission follows.

## LLVM execution of selected value calls

Ordinary value calls now use the same exact selected callback table as collection
keys. The table remains available while emitting function bodies; key descriptors
are still demanded only by Map/Set construction. Borrowed operands use their
aligned physical entry slots, including immediate scalars, with no ownership
copy. The existing callback wrappers adapt selected user methods and normalize
scalar outputs. Ordinary and selected calls share normal/fault dispatch.

The LLVM library tests and scoped MIR/codegen JSON Clippy pass. An O0/O2 JIT
control executes selected String equality and Hash through ordinary function
bodies, including counted embedded-NUL contents and signed integer hash bits.
Broader source equality controls follow the separate SIR producer prerequisite.

## Source selected equality and IEEE float execution

The selected-equality source oracles now execute through ordinary function bodies
at O0 and O2. They cover owned vectors, tuples, records, active Option/Result
payloads and nested generic user Eq. Callback failure preserves the exact status,
opaque fault owner and untouched caller result. A borrowed generic Eq probe also
leaves caller strings usable without cloning or dropping them on either path.

The scalar-versus-wrapped NaN oracle exposed a missing float execution path:
physical LLVM emission previously handled only comparisons. Emit admitted IEEE
arithmetic and floating negation, and use unordered inequality for bare NaNs.
Selected structural float equality retains its documented bit-pattern rule.

The MIR/codegen library suites and scoped JSON Clippy pass. Source composite and
fault modules verify before and after O2 optimization for Windows x64 and macOS
arm64; execution here is Linux JIT. Source fixtures and their SIR/Types producers
are supplied prerequisites. Native platform and combined safety acceptance stay
with integration.

## Selected value call validation complete

Bytes equality now has source-driven JIT controls for nested owned values and
record keys combining user Hash with derived Eq. A borrowed Bytes probe compares
only the active region, including embedded NULs and empty values, while retaining
the caller's original buffers. Bare Bytes still has no Hash selection. The same
source module verifies before and after O2 optimization for Windows x64 and macOS
arm64.

After applying the transparent-transfer prerequisite, the complete MIR/codegen
library suites and scoped Rust lint pass through Make. The native acceptance
runner passes at O0 and O2 for selected composite equality, callback fault
propagation, operand read order, collection parent arguments and Bytes equality.
The fault case preserves the expected DivideByZero status; read-order coverage
includes the transparent Vec.from wrapper.

Native execution here is Linux. Windows and macOS execution and combined
ASan/LSan acceptance remain with integration. Supplied SIR, checker and fixture
prerequisites are unchanged by this layer.

## Selected equality native integration — 2026-09-05

Ordinary composite and byte equality now demands the exact checker-selected Eq
plan, including concrete generic user methods. SIR records borrowed operands,
normal-only scalar results and owned fault cleanup. Physical MIR verifies that
contract and LLVM calls the shared selected callback graph. Scalar floating
arithmetic retains IEEE behaviour; selected composite float equality retains
its existing bitwise contract. Bytes Eq does not imply Bytes Hash.

Argument evaluation preserves the earlier value across later mutation, including
transparent Vec.from wrappers and a container supplied to its own field mutation.
The review finding for wrapped captures is fixed in the shared transfer producer.

The full native acceptance and paired generated/runtime ASan/LSan suites pass at
O0/O2, including composite equality, nested bytes, callback faults, read ordering
and parent-container arguments. Combined Types/SIR/MIR/codegen/Session tests pass;
the final additional byte-region JIT and target-verification controls pass in the
codegen library suite. No compiler implementation changed after those native runs.
Evidence: /tmp/hew-selected-equality-{native,safety,pipeline}-combined.log and
/tmp/hew-selected-equality-final-codegen-tests.log.

Full make -k lint was run and is not green. Rust JSON Clippy passes. The remaining
failures are JobState's missing transparent record contract, std.string.starts_with's
missing SIR operation contract, and the pinned grammar's std corpus parse failures.
Evidence: /tmp/hew-selected-equality-full-lint.log. Cross-target LLVM verification
passes for Windows x64 and macOS arm64; native platform execution of this equality
milestone remains pending. Their preceding collection milestone c7683edaf passed
native, runtime, C ABI and stdlib suites. Its bare-WASM C ABI libc compile failure
is retained separately for sandbox work.

## Explicit indirect-call operand model — 2026-09-05

SIR now carries the evaluated callable as the first boundary operand of an
IndirectCall, with its exact semantic signature, ordinary argument transfers,
normal-only result and cleanup edge. SSA visitors, operand rewriting, dominance
intervals, fault flow and CFG traversal retain that first operand independently
of the arguments and result edges. The model regression checks the callee's
rewrite slot and both continuation roles; the focused SIR suite passes.

This is a model checkpoint. The verifier and physical lowerer explicitly reject
indirect execution until the callable capability, environment and receiver
contracts are connected. No source closure or indirect native support is claimed.

## Shared callable capability vocabulary

Introduce the shared callable invocation and duplication capabilities and the
three independent capture axes: acquisition, private access and consumption.
This prerequisite defines the vocabulary only; parser and checker integration
follow separately. No callable behaviour changes in this checkpoint.

## Callable receiver ownership contract

SIR distinguishes exclusive borrowed receivers from consuming receivers. Borrowed
receivers retain the caller's obligation; a consuming parameter requires a concrete
owning type and enters the body's normal and fault cleanup set. Indirect-call
lifetime tests cover both continuations, reject reuse after consumption and retain
cleanup obligations after borrowing. Exclusive loans also cannot escape by return.

This is a contract checkpoint. Source receiver selection and physical receiver
storage remain explicit refusals until the closure environment implementation
connects them; no executable closure acceptance is claimed here.

## Callable surface and type boundary

Pipe closures now parse explicit private capture prefixes such as
`[var count] || { count = count + 1; count }`, including the ownership
acquisition prefix `move`. Written function types carry invocation and Clone
qualifiers, and formatting preserves both additions. The same capability
structure travels through source types, checker types and resolved HIR types.

This is a compiling boundary checkpoint. Parser acceptance and rejection tests,
including formatting round trips, pass. Capture ownership, callable coercion
and invocation checking remain the next checker layer; native closure execution
is not implemented by this checkpoint.

Validation of the callable field checkpoint: the full parser and type-checker
Make suite passed, and workspace Rust Clippy passed with tests enabled. Shared
structural substitution now uses the existing child mapper so callable
capabilities cannot diverge between duplicated substitution walkers. This is a
compiling syntax and field checkpoint; capture enforcement, directional
coercions, capability-derived ownership and native execution remain unfinished.

## Concrete callable environments

SIR now identifies a closure by its literal and enclosing instance, with one
ordered capture descriptor and an explicit receiver body. Construction and
capability weakening are semantic operations. Descriptor checks reject mismatched
capture types, receiver permissions and ownership-erasing coercions. Capture
places retain their environment identity; their execution and source production
are the next connected layer.

## Callable captures and receiver capabilities

Replaced the legacy Copy/Move/Borrow/BorrowMut capture modes and syntactic
mutation scan with binding-resolved acquisition, private access and invocation
consumption facts. Ordinary closure bodies now check independent environment
bindings. Private mutation requires the capture prefix and a mutable callee;
consuming a captured owner requires call-once invocation, including consuming
uses in diverging arms. Nested captures preserve lexical identity and source
parameters retain definition spans through HIR.

Callable duplication follows explicit Clone evidence and copies private state;
erased fn types no longer imply Copy or Send. Resource-bearing closures cannot
claim Clone. HIR rejects missing callable, capture and escape facts and no
longer selects capture metadata by name alone. Removed the replaced scanner and
its consumers while retaining malformed-boundary controls.

Validation: the complete parser/types/HIR Make suite passed, including resource
acquisition/consumption, private mutation, shadowing, nested captures, call-once
reuse and forged Clone controls. Workspace Rust Clippy passed. Directional
coercions, capability joins and canonical callable identity keys remain for the
next checkpoint; native/runtime acceptance belongs to integration.

## Directional callable coercion and type identity

Separated exact callable unification from directional value coercion. Parameter
and result signatures remain invariant; value coercion may weaken invocation
capabilities and forget Clone through bindings and aggregates. Control-flow
joins compute common guarantees independently of arm order. Explicit binding
annotations retain their erased destination type, while expressions preserve
their concrete source type for ownership lowering. Callable-field invocation
checks the selected place and explicit clone uses the existing typed clone
rewrite. Erasure cannot discard captured linear duties.

Canonical type strings, mangled specialization keys and storage congruence now
retain callable capabilities; closure keys also retain capture types. This
prevents distinct ownership-bearing instances from sharing an accidental key.
The full type unit suite and native compiler build passed through Make, as did
focused HIR key collision controls. Native closure execution and lifetime
acceptance remain integration work. Value-transfer consumption inference and
generic function values remain the next source component work.

## Capture access and indirect-call verification

Capture places now join the exact closure receiver and ordered field descriptor.
SIR admits checked field copies, field loans, private mutable assignment and
call-once extraction. Its path-sensitive lifetime relation tracks initialized
fields across branches, protects live field loans and permits destruction of a
partially consumed environment while refusing its copy, invocation or transfer.
Indirect calls verify exact signatures, receiver permissions, argument transfers
and fault propagation; exclusive calls reject overlapping loans and arguments.

The complete SIR Make suite passes with capture mutation, malformed receiver and
signature, branch-dependent extraction, cleanup and exclusive-loan controls.
This connects semantic verification; source production and physical invocation
remain under implementation, so it is not native closure acceptance.

## Typed function values and shared invocation lowering

Function references now demand their exact source bodies and construct owned
callable values. Direct and indirect user calls share argument evaluation and
normal/fault cleanup, with explicit borrowing or consuming receiver transfers.
Callable capability weakening is explicit at bindings, arguments and returns;
fresh function references avoid a spurious copy after capability erasure.
Plain erased callables retain their non-Clone transfer contract.

Source tests cover indirect-only body demand, nested repeated calls, returned
and annotated callable values and consuming invocation. Existing demand tests
now use an unsupported scalar header instead of newly admitted function types;
collection tests still reject copying non-Clone callable elements. Physical
callable execution remains the next integration layer.

## Demanded closure bodies and capture production

The existing instance service now demands each checked closure literal under its
exact enclosing specialization. Body lowering preserves the original HIR source
and gives its environment an explicit receiver and ordered capture places.
Construction copies or moves the checker-selected bindings; body operations
copy, borrow, consume or privately assign the corresponding fields. Consuming
receivers enter the body's normal and fault cleanup obligations.

Source tests verify escaped string snapshots, repeated captured calls and exact
literal/body identity alongside function values. The complete SIR Make suite
passes. Native execution and broader capture combinations remain integration
work; no native closure acceptance is claimed at this checkpoint.

## Final callable surface and transfer facts

Applied the approved surface: lowercase fn[clone] qualifiers and the contextual
capture(var name, ...) prefix. Removed the provisional capture and qualifier
spellings. Parser and formatter examples cover move acquisition, private
mutation, nested callable types, ordinary functions named capture, and rejected
aliases and malformed prefixes. Updated the language specification and syntax
metadata; downstream editor grammar propagation remains pending.

Value materialization now records non-duplicable capture consumption using the
shared type-class authority. Returns, locals, assignments, tuples, arrays and
record/enum payloads feed the same capture facts. Inferred lambda returns join
callable guarantees independently of return order. HIR retains checker-selected
join types and concrete callable return expressions; explicit var annotations
retain their destination type without overwriting initializer facts.

The conditional Result/Option example exposed missing builtin context in dotted
variant shorthand. Contextual constructors now reuse the existing proven-builtin
member contract, retaining shape and payload diagnostics and ordinary user enum
resolution. No synthetic prelude declarations were added.

The native compiler build and focused parser/type/HIR checks passed through
Make. The complete component suite found an older equality test for closure
erasure; it now tests directional coercion and preserves exact/reverse refusal.
Lexer syntax-metadata validation passed through Make.
Generic function-value instantiation and editor grammar propagation remain for
the next source checkpoint. Native invocation and runtime lifetime acceptance
remain integration work.

## Combined capture ownership paths

Source verification now covers the approved mutable-counter syntax, independent
callable copying, nested escaping environments, consuming captured callables
and borrowing a remaining field after another field has been consumed.
Argument evaluation retains its already-evaluated receiver loan through nested
calls and arithmetic checks; terminating argument paths end that loan before
environment cleanup. The focused callable source suite passes these combined
paths. Native invocation and sanitizer acceptance remain pending physical
integration.

### Callable carrier and environment ABI

Defined the shared two-pointer callable carrier, immutable descriptor, and erased borrowed/consuming invocation adapter signature. SIR retains call permission and capture ownership decisions; compiler-generated environment layouts own initialization masks and clone/drop behaviour. Once adapters dispose the environment on both outcomes. The runtime interface provides zeroed aligned allocation, independent cloning with failure-preserved output, and drop with carrier clearing.

CABI tests and layout assertions compile for native, Windows x64, macOS arm64 and WASI. This is the ABI checkpoint; runtime helper implementation and executable closure integration remain pending.

### Callable environment runtime

Implemented zeroed aligned environment allocation through the existing allocator, independent semantic clones with output publication only on success, and carrier-clearing drop. Compiler layout callbacks interpret capture masks and roll back partial clones; the runtime releases only outer storage after clone failure. Invocation remains in compiler adapters, with consuming adapters responsible for cleanup on either outcome.

Focused runtime tests cover independent captures, alignment and zeroing, partial initialization, rollback without duplicate drop, release-only captures, empty function invocation, captured zero-sized values, and consuming success/fault cleanup. The full native runtime suite, focused native ASan/LSan, scoped JSON Clippy, generated C ABI surface checks and the export ownership verifier pass. C ABI tests and cross-target layout assertions pass for Windows x64, macOS arm64 and WASI; these are compile checks, not native platform execution. Added only the callable helpers to the existing codegen ABI classification and ownership contracts.

### Callable composition through aggregate and control boundaries

Callable coercion now follows record and variant payload targets and conditional
result joins. Explicit callable cloning uses the existing semantic copy contract.
Boolean matching evaluates its scrutinee once and carries binding guards and
ownership cleanup through the common match continuation machinery. Mutable
callable projections retain the original stored environment through a checked
borrow chain.

Focused source checks validate callable composition with conditional nested
Result/Option payloads and record cloning. The stored mutable callback regression
still identifies the HIR temporary-copy rewrite, whose replacement is pending
composition. Native acceptance awaits physical integration.

## Physical callable receiver boundary

Callable values now use a target-measured pair of environment and descriptor
pointers. Physical calls preserve the explicit exclusive receiver mode and
pass its caller storage by address; the callee uses that storage directly.
Consuming parameters retain their owned obligation. Runtime operations still
require their existing argument contracts.

The MIR/codegen library suites pass, including O0/O2 execution of the exclusive
receiver ABI and rejection of an incompatible carrier or shared call argument.
This is a receiver-layer checkpoint. Closure environment recipes, capture masks,
independent copies and indirect invocation still await the semantic descriptor
contract.

## Masked callable environment storage

Physical target data now measures capture environments as an initialization mask
followed by aligned fields. Empty callables have no environment allocation;
zero-size captures still retain mask storage. Generated clone callbacks reset
copied mask bytes and mark each successfully copied field. Generated drop
callbacks clear each live bit before releasing its capture. Copy failure rolls
back only initialized destination fields through that same drop callback.

Focused codegen tests execute independent captured vector copies and partial-mask
cleanup at O0/O2 using the runtime callable helpers. Layout checks cover Linux,
Windows, macOS and WASI target data; native execution was run on Linux. Scoped
MIR/codegen Clippy passes. Source closure construction, indirect calls and capture
place transitions still require the incoming semantic producer contract.

## Nested callable value recipes

Callable captures use the shared value-copy and destruction recipe machinery.
A value-layout clone callback forwards the runtime helper's status so enclosing
environment clones can roll back their completed fields. A direct semantic copy
aborts on failure without inventing a language fault. Callable destruction clears
and releases its owning environment through the runtime helper.

The nested-copy failure control passes at O0/O2: a completed vector copy is
released, a failed nested capture is never dropped as an owner, the source stays
live, and the caller's output is unchanged. The focused callable suite and scoped
MIR/codegen Clippy pass. Semantic type facts must publish independent copying;
legacy retain-only callable facts remain refused.

## Physical callable construction and invocation

Physical MIR now retains demanded closure identities, canonical capture storage,
function and closure construction, capability weakening and typed indirect calls.
LLVM emits erased adapters over the private result/fault ABI. Mutable receivers
address their original heap fields; capture live bits follow assignment and take.
Concrete once bodies own their cleanup, while adapters for weakened borrowed
bodies dispose the environment after either outcome.

Verified SIR execution controls cover escaping and copied mutable counters,
consuming invocation and preservation of fault results at O0 and O2. Earlier
field-glue controls continue to cover partial initialization and failed nested
copies. Source producer composition remains the next integration milestone.

## Native callable source composition

Composed the demanded source bodies and final capture syntax. Source-to-LLVM
execution now covers ordinary function values, a snapshot that outlives its
factory, independent mutable counters copied through a binding, and once erasure
at O0 and O2. The existing verified semantic environment and fault controls pass
with that composition.

Explicit `clone first` currently stops in SIR with an unsupported HIR expression;
the equivalent ordinary binding copy executes and preserves private state. This
producer gap is reported to the source owner. The broader Make lint graph reaches
the pre-existing JobState transparent-record refusal in dogfood compilation;
workspace Clippy and the callable component execution checks pass.

## Native consumed captures and argument faults

Composed the capture-loan cleanup fix and extended source execution controls.
A detached nested string closure survives both creating environments, a captured
once-callable transfers into invocation, another field remains callable after a
take, and an argument evaluation fault releases the outstanding capture loan.
These paths pass through verified SIR, physical MIR and LLVM at O0 and O2. The
fault control also proves that failure leaves result-out storage untouched.

### Callable acceptance checkpoint

Added callable cases to both the native acceptance and paired ASan/LSan suites: owned snapshots and lexical shadowing, independently copied private counters, escaped closures owning closures, indirect-only function items, borrowed and consuming callback faults, and repeated once/uncalled cleanup. The existing runner checks exact stdout, fault status and stderr at O0/O2; its paired runtime/generated-code sanitizer build supplies the cleanup oracle. No counted-drop extension or new harness was added.

The cases pass the retained frontend-only `hew_compile::check_file` API using the parser/checker build for `312adcf59`; manifest membership is verified. Native and sanitizer execution remain pending composition of the closure producer and physical lowering. The CLI's `check` also enters SIR and currently reports unsupported callable transfer, so it is not reported as passing.

Generic indirect-only values remain a source-checker blocker: assigning `identity<T>` as a value to `fn[clone](i64) -> i64` or `fn[clone](string) -> string` leaves the generic `T` unresolved. That case is held out of this passing-source checkpoint pending the checker owner's fix.

### Generic function values and projected callable invocation

Function declarations used as values now instantiate through the existing
signature, bound and declaration-identity authorities. Explicit type arguments
parse at value boundaries, and inferred arguments settle from annotations or
later calls. HIR records value-site arguments in the existing monomorphisation
registry, including imported aliases and private helpers reached through generic
factories. The previous annotation-only module-function path is removed.
Missing or corrupt generic value facts remain HIR boundary errors.

A record callable field remains the direct indirect-call callee. Removing the
synthetic local binding prevents a mutable, cloneable environment from being
copied before invocation; SIR decides the selected projection's lifetime.
The focused HIR control checks the exact receiver binding, capabilities,
argument order and absence of a temporary callable binding.

Make validation: the complete parser/type/HIR component run passed every test
except the new imported private-helper value control. Canonical owner lookup
fixed that failure and its focused rerun passed. Generic value and projected
field controls passed; native compilation and workspace lint are checked for
this checkpoint. Native runtime behaviour remains an integration acceptance
item in the SIR/backend owners' tree. Downstream grammar propagation follows.

### Combined callable native acceptance

The combined native run passes stored mutable callbacks, independent explicit
clones, nested escaped closures, conditional Result/Option factories and callee
loan cleanup at O0/O2. Two fixtures remain failing: lexical shadowing leaves a
literal without its concrete closure descriptor, and a once-only callback
parameter is still borrowed when invocation needs ownership. These are compiler
gaps. Paired safety produces the same source failures; every compiled case
passes generated-code and runtime ASan/LSan at both optimization levels.

Added generic indirect-only and generic returned-function acceptance cases after
the checker value-site instantiation checkpoint. Both pass native and paired
safety at O0/O2 using the same compiler. D345/D346 forbid local partial moves:
once-field invocation requires explicit destructuring, with no implicit copy.
The declaration-only parameter rule likewise requires `consume callback` for
consuming invocation through an ordinary parameter. Those are the existing
contracts for the remaining diagnostic and producer work.

### Callable editor grammar propagation

The canonical generators keep `capture` and `once` contextual rather than
colouring unrelated identifiers. Nano highlights callable qualifier lists and
private capture prefixes. The spec records explicit and inferred generic
function values next to callable guarantees.

Tree-sitter, TextMate and Vim changes live in isolated downstream branches;
Studio receives the same tested TextMate grammar. Tree-sitter's complete corpus,
TextMate's tokenization and unit suites, Vim's actual syntax-group controls,
and Nano's view-mode syntax load pass through Make. Compiler-wide grammar
parity still fails on exactly the same file set at the pinned tree-sitter base
and the new grammar commit; no new parity failures were introduced.
Studio dependency installation is blocked by the existing ESLint 10 versus
react-hooks plugin peer requirement, so its full application build is unrun.
The current website and playground use compiler WASM semantic tokens, not a
separate TextMate grammar; updating those packages belongs with the integrated
compiler artefact. The compiler's tree-sitter lock is unchanged until the
new downstream commit is available to the integration publisher.

### Concrete identity for empty closure environments

Preserve `Ty::Closure` for every literal even when lexical shadowing removes all captures. HIR now receives the concrete environment type directly from the checker. Added source-to-HIR controls for plain, annotated and shadow-filtered literals, and updated the contextual-lambda assertion to the concrete representation. Make component tests initially exposed only that obsolete assertion; native integration remains the coordinating lane's validation.

### Diagnose forbidden once-callable consumption at the source boundary

Preserve explicit parameter consume declarations independently of local mutability. Once invocation through a borrowed parameter now reports `E_OWN_CONSUME_BORROWED` with a declaration fix; direct consumption of a callable field in a live local aggregate reports `E_OWN_PARTIAL_CONSUME` with an explicit-destructuring fix. Clone capability does not authorize implicit cloning for invocation. Approved closure environment captures retain their existing place contract. Focused Make callable tests cover borrowed and consumed parameters, reuse, ordinary non-Clone argument borrowing, both field-call spellings and destructuring. No ordinary-call transfer path was removed: the current shared argument checker already borrows.

### Pin the published callable grammar

Update the tree-sitter source lock to the published callable grammar revision `dc48c15f0f1ee91250e76377dedfba414658ed7d`. The focused Make corpus check passes with the pinned CLI in a separate parser cache. Reusing the earlier baseline cache loaded the baseline parser despite the new source checkout; the isolated cache rebuilt the intended grammar. The earlier full parity comparison retained the same pre-existing failures and is not claimed green. The Studio grammar diff remains uncommitted because its existing dependency conflict blocks its normal lint hook.

## Enforce consuming callable projection admission

SIR currently lacks a verified transfer for consuming one field of a live local
aggregate. It now refuses once-call receivers that are record or tuple projections before
projection lowering can copy them. Borrowed aggregate roots receive the distinct
borrowed-consumption refusal. Explicit environment capture places retain their
existing consuming contract.

Focused producer controls cover Clone+Once and non-Clone record and tuple
receivers, borrowed roots, and explicit record destructuring into an owned
once-callable plus a still-live sibling. The negative controls also exercise the
SIR guard when frontend diagnostics already report the invalid source. No local
partial-initialization state or record ABI change is introduced.

## Execute explicit callable destructuring

Native source controls now exercise record and tuple destructuring for both
Clone+Once and non-Clone callable fields. The sibling remains usable after the
once call. Record controls count environment destruction separately at O0 and
O2, including argument-evaluation and callable-body faults, and verify that a
fault leaves result-out storage untouched. These execute the existing whole-
aggregate ownership transfer; no local partial-move machinery is introduced.

The focused codegen execution controls pass. SIR ownership has been handed back
to integration after the consuming-projection guard checkpoint.

## Preserve declared cleanup boundaries for partial fields

The canonical type-fact service now exposes the exact nominal declaration marker
without inferring it from copyability or the value class. A plain generic record
containing a resource remains distinct from a resource-marked container. The
query validates declaration identity and arity but grants no field access.

The record-contract suite passes. An initial attempt to reuse transparent field
admission for the marker query incorrectly excluded resource declarations; the
final query reads their declaration metadata independently. Projected SIR will
retain this marker alongside its separately verified aggregate shape.

## Lower owned field access through projected places

Local record and tuple fields now use exact SIR places rooted in one owned SSA
value. Field takes preserve siblings; field assignment uses conditional cleanup
and reinitialization. Structural partitions are completed across CFG root
versions before ownership verification. Consumed aggregate captures transfer to
one local root at entry, and runtime field transforms restore that same place.
Owned temporary extraction retains its explicit destructuring path.

The complete Job source controls verify nested siblings, conditional takes,
field restoration, loops and argument/body faults. Captured partial records and
mutation of a remaining vector field also verify. These exposed and corrected
missing callable coercion on replacement and attempts to carry already consumed
variables into loops. Iterator adapters explicitly acquire retained callbacks.

Focused callable and aggregate-place controls pass. The full SIR run reaches the
new producer but still has tests tied to replaced projection/reconstruction
shapes; those assertions need migration while preserving their borrow, ordering
and fault obligations. Native partial storage remains deliberately refused until
physical realization is integrated; this is a producer checkpoint.

## Preserve aggregate ownership oracles after projected lowering

Aggregate tests now inspect canonical local field paths, root identities and
explicit loads. A temporary aggregate retains the genuine nested loan chain
needed to test parent-loan lifetime rules; local nested fields exercise direct
root loans and the same ended-loan, ownership and fault-cleanup refusals.

Runtime mutation checks follow the returned receiver into its existing field
before the next take. Failure checks require cleanup of the partially
initialized root and reject removal of that cleanup. Record copies still have
independent roots, while cursor reads borrow the exact vector field.

The affected test binaries and the full SIR component pass. This validates
semantic lowering and verification; native partial-field realization remains a
separate integration requirement.

## Verify definite private parameter replacement

Borrowed callable parameter provenance now follows replacements through the
existing source ownership snapshots. Replacement on every reaching branch, or
on the only branch that continues, permits private mutable invocation. A mutable
Clone+Once parameter consumes its entry copy while leaving the caller reusable.
The native fixture passes at O0/O2 with paired generated/runtime ASan/LSan.

The full type-checker suite passes. Full lint completed with passing Rust
diagnostics; the existing JobState record lowering, starts_with operation and
standard-library grammar gates remain unresolved. Ordinary partial-field
consumption is a separate implementation and is not claimed by this checkpoint.

## AOT evaluation

Removed the dormant CLI JIT modes and the unavailable ORCv2 adapter. Native
`hew eval` compiles each submission and runs it in a child process; WASI eval
continues to compile a module and execute it through wasmtime. The removed
`--jit` option now receives the ordinary unknown-option diagnostic.

Hew-managed JIT execution is outside the current product targets and roadmap.
Native AOT remains primary, with WASM runtime and browser sandbox execution
retained. LLVM execution engines remain useful compiler test oracles. Runtime
session/reset hooks, C ABI embedding support and the shared ABI descriptor
inventory serve AOT or embedding and remain independent of the removed CLI mode.

## Preserve duplicate resource-release refusals across the checker boundary

The combined Types/HIR run exposed an older HIR test that expected the checker
to accept a second release hidden in an `if`. Ownership snapshot joins now reject
that use of moved `self` before HIR. The control requires the source diagnostic
and still lowers the malformed body to prove it cannot acquire HIR automatic
cleanup authority. The focused two-boundary control passes.

## Preserve consuming callable owners at conditional joins

A unit `if` after consuming a mutable callback tried to forward the dead owner,
while an `if` result tried to copy a non-cloneable callback from either branch.
Unit branches now use the existing control-state join instead of their separate
merge implementation. Non-cloneable owned bindings retain move semantics inside
conditional expressions. The full SIR suite passes with both source regressions.

A consuming callback followed by `break` remains a separate loop-exit defect:
the zero-iteration path owns the callback while the taken path has consumed it.
The verifier correctly rejects the unconditional owner transfer at that join.

## Execute partially consumed aggregate storage

Physical MIR carries canonical aggregate paths, leaf partitions and exact CFG
initialization transfers. LLVM addresses fields inside their root allocation and
tracks only those verified leaves, including zero-sized and no-drop fields.
Subtree replacement releases initialized old leaves before storing the new
value. Loop edges snapshot payloads and leaf states together, and call results
become initialized only on success.

Real runtime string owners exercise mixed, live, dead and conditional subtree
replacement, loop permutations and fault cleanup at both optimization levels.
Generated-code checks require field aliases instead of independent payload
allocations. The Job source cases also execute through Make at O0 and O2,
preserving siblings and restoring callbacks while propagating argument and
callback-body faults. Full integration acceptance and sanitizer execution remain
separate checks.

## Captured partial ownership and mutable siblings

The job-history acceptance programs consume a once callable inside an owned
aggregate capture, retain its heap-owning siblings, and append owned strings to
a vector sibling after taking another job's callable. The fault program fails
after the captured field is consumed and the remaining siblings are read. Its
cleanup must therefore follow the partially consumed local root without also
releasing it through the closure environment.

The expected reports independently check callback results, sibling contents and
vector growth. Source checks pass; native and paired generated/runtime sanitizer
execution remains pending.

## Initialize aggregate runtime results on their normal edges

Paired generated/runtime sanitizers exposed a recursive-carrier leak after the
projection cutover. Collection indexing writes an owned record directly to an
output address; its payload was valid, but its leaf flags remained empty when
the normal edge copied their state. Cleanup therefore skipped owned fields.
All result-producing normal edges now publish result initialization before their
parallel transfers, independent of whether LLVM or the runtime wrote the payload.
Failure edges leave result storage uninitialized. The original recursive-carrier
safety case now passes at O0 and O2. Focused vector/map returned-record controls
and the full integrated safety rerun remain the next validation steps.

## Eval timeout recovery across hosts

The timeout-recovery test now uses the one-second worker budget demonstrated by
the macOS control, with a trillion-iteration loop that cannot finish during that
budget. Compilation finishes before the deadline begins; post-spawn child
startup and execution consume the budget. The previous 100 ms check timed out
even the following arithmetic expression on macOS. That is a reproduced test
failure; its latency cause and whether it predates the revision remain unproven.

The check requires exactly one timeout diagnostic, a successful REPL exit and
exactly `42` from the next submission. A separate bounded runner contains a
broken deadline. This verifies enforced termination and recovery without making
sub-second startup speed part of the contract. Runtime behaviour is unchanged.

## Count ownership of records returned by collection indexing

Focused native execution tests index a two-string record from real vectors and
hash maps, then read or destructure its fields. The runtime callback boundary
must copy each field once on a hit and zero times on a miss. Every original and
retained string reference must be released, while failed indexing leaves the
result untouched and returns the bounds fault.

Before normal-edge initialization was fixed, successful vector and map lookups
each leaked one reference to both fields at O0 and O2; the missing-element
controls already passed. With the fix, all paths pass at both optimization
levels. The complete physical MIR and codegen library suites pass through Make,
as does scoped Rust lint. These count-based runtime checks complement the paired
generated/runtime sanitizer acceptance cases.

## Validate partial aggregate ownership end to end

The full native acceptance suite now passes at O0 and O2, including captured
partial records, mutable sibling fields and callable branch joins. The full
paired generated/runtime ASan/LSan suite also passes after normal-edge result
initialization repaired the recursive-carrier leak. Focused vector and map
controls establish exact field copying and release on success and untouched
result storage on failure. Combined compiler-layer tests pass; the earlier HIR
assertion now preserves both source and HIR duplicate-release refusals.

Integration preserves the published callable and AOT evaluation history with an
identical compiler, runtime, test and specification tree to the validated partial
ownership checkpoint. A whole callable consumed before a loop exit still needs
function-owned local storage with lexical cleanup. Cross-platform execution of
this partial-ownership checkpoint and the revised AOT timeout control is pending.

## Checked-fault eval diagnostics

The divide-by-zero eval controls now verify the checked language fault: raw
evaluation returns the child's exit code 202 and names `DivideByZero (202)`,
with no program stdout. JSON evaluation returns CLI status 1 and records the
child's status 202, cause and empty program stdout in the result. Its compiler
diagnostics remain empty because the runtime already supplied the fault message.

The previous controls required hardware-signal wording and synthesized JSON
diagnostics. Both failed against the checked-fault output before their assertions
were migrated. The separate controls for silent workers, hardware signals and
Windows fault statuses remain unchanged, as does the product output.

## Admit the canonical string-prefix predicate

String prefix checks now have an exact stdlib declaration and typed runtime
contract: both strings are borrowed, the result is a boolean and the operation
has no logical failure edge. This fills the native contract missing from the
MIR benchmark's quoting helper. The existing runtime retains its length-aware
Unicode and embedded-NUL behaviour. Runtime-contract tests pass; physical
realization and source execution follow as separate validation steps.

## Realize the prefix predicate with its native boolean ABI

Physical MIR selects the prefix operation from its verified runtime family.
LLVM borrows both managed string handles and widens the runtime boolean to Hew's
boolean storage. String equality retains its distinct integer-returning ABI;
the common predicate emitter preserves that difference explicitly.

The physical MIR and codegen library suites pass. Targeted LLVM verification
checks the boolean result and pointer parameters on Linux x64, Windows x64 and
macOS arm64. Source execution and sanitizer checks remain pending.

## Preserve prefix inputs across argument evaluation

The native prefix fixture passes at O0 and O2 with allocated Unicode strings,
embedded NULs, empty strings and mismatching or longer prefixes. Both input
values remain usable. When a later argument replaces the receiver variable,
the comparison still uses the value evaluated first, including replacement by
an empty string. Paired generated/runtime sanitizer validation is next.

## Validate borrowed string prefix execution

The complete native acceptance and paired generated/runtime ASan/LSan suites
pass at O0 and O2 with the prefix operation. Prefix and equality predicates
preserve their distinct runtime result ABIs while sharing the boolean
normalization path. The full lint run has clean Rust diagnostics; its benchmark
now reaches the next unsupported string operation, `trim`. Machine lowering
and the pinned grammar remain separate unfinished capabilities.

## Copy encoding trees as independent values

JSON and YAML now expose runtime deep-copy and semantic equality operations over
their existing boxed serde carriers. Copies and extracted children remain usable
after parent release, and changing one tree leaves the others intact. Equality
retains each format's number, mapping and tag rules; invalid handles stay distinct
from live null values. The existing release primitives destroy each independent
owner without adding source-level cleanup requirements.

The JSON and YAML runtime suites pass, including nested mutation and release,
signed zero, integer-versus-float distinction, YAML tags and NaNs, and preservation
after operations rejected by the existing builders. Compiler integration and
source API changes remain separate from these runtime operations.

## Give encoding values canonical ownership facts

JSON and YAML values now receive distinct builtin identities from their exact
shipped declarations. Import aliases, reexported signatures and generic wrappers
preserve those identities; same-named user types and catalogue lookups cannot
acquire them. The checker publishes semantic deep-copy and automatic-drop
capabilities for the owned serde trees, with Send and Sync based on independent
ownership, immutable reads and exclusive mutation. Ordinary opaque resources
retain their existing ownership rules.

Equality requires the format's explicit Eq implementation. Neither empty opaque
fields nor pointer identity can supply it, and Hash remains unavailable. Types
tests cover these decisions and the source-provenance boundary. Public wrapper
migration, runtime operation descriptors and native clone/drop realization still
need integration; these checker facts alone do not admit native encoding values.

## Preserve unsigned encoding values and report encoding failures

JSON and YAML now construct and read exact unsigned integers through `u64::MAX`.
The existing signed integer status distinguishes wrong kinds, signed values and
unsigned values above `i64::MAX`; unsigned access also rejects negative values.
YAML reports tagged values as their own outer kind, keeping container checks
consistent with the existing lookup and insertion operations. Copy, equality
and encoding preserve tags and arbitrary mapping keys.

Both encoders clear their error slot on success and retain a diagnostic on
failure, including an invalid handle. YAML serializer errors now reach callers
instead of being discarded. The runtime suites pass normally and under address
and leak sanitizers, covering integer boundaries, tagged containers and scalars,
and an actual nested-tag serialization failure followed by a successful encode.
Source wrapper integration remains separate.
