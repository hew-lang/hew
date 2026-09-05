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
