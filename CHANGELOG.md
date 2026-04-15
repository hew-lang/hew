# Changelog

## [Unreleased]

## [0.4.0] - 2026-04-15

### Added

- **LSP transitive goto-definition:** `find_cross_file_definition` now follows
  one import hop with a cycle guard, so goto-definition works through re-export
  and glob chains (#1073).
- **Module search-path documentation:** `HEWPATH`, `HEW_STD`, the four-step
  resolution order, and the `hew.toml` non-role are documented across the
  user-facing module discovery docs (#1074).
- **Eval WASM + `--json` ok-path coverage:** integration coverage now exercises
  `hew eval --json --target wasm32-wasi` on the success path, and the WASM
  capability matrix documents the non-interactive eval contract (#1075).
- **stdlib URL percent-encoding proof surface:** `url.encode`, `url.decode`,
  and `url.encode_query` are now confirmed end-to-end on native and WASM, and
  `hew-runtime` exports bounded `hew_bytes_to_string` support so `url.decode`
  works correctly under `wasm32-wasip1` (#1077).

### Fixed

- **`fs.try_read_bytes` binary-safety:** `try_read_bytes` now calls
  `hew_file_read_bytes` directly with proper `hew_file_last_error` handling
  instead of routing through the UTF-8 string path, so non-UTF-8 and
  NUL-containing binary files round-trip correctly (#1076).

## [0.3.0] - 2026-04-06

### Added

- **Cross-target object emission (`--emit-obj`):** `hew build --target <triple> --emit-obj`
  now emits correctly-formatted object files for arm64-apple-darwin, x86_64-unknown-linux-gnu,
  and x86_64-pc-windows-gnu without requiring a separate `-o` flag (output name defaults to
  `<stem><target-object-suffix>`). Foreign native executable linking is rejected early with a
  clear error directing users to `--emit-obj`. Verified by e2e tests that inspect object-file
  format and architecture via the `object` crate (#730, closes phase-1 of #254).
- **HTTP client surface:** `std::net::http` now includes bounded client wrapper helpers plus
  request/response header accessors for Hew programs, making it easier to build clients without
  dropping into Rust glue (#722, #747, #750).

### Fixed

- **Non-root module typechecking:** module graph bodies are now typechecked end-to-end while
  keeping local non-root types visible inside their own module, preventing imported private helper
  leaks, preserving static type methods, and restoring preregistered QUIC handle method
  enrichment (#756).
- **HTTP request typing precision:** request-building header inputs now stay aligned with the
  tuple-based header model exposed elsewhere in the HTTP surface (#758).
- **Composite generic codegen fail-closed behavior:** the remaining composite type-argument
  struct-init path now rejects unsupported inputs instead of reaching a late build-time crash
  during code generation (#769).
- **Builtin collection clone support:** `clone()` now works correctly for `Vec`, `HashMap`, and
  `HashSet`, including chained receiver expressions such as `v.clone().len()` and the missing
  `hew_hashset_clone` runtime hook (#772).
- **FreeBSD embedded codegen builds:** FreeBSD now uses the correct static LLVM/MLIR embedded
  codegen path, and the CLI fails closed when embedded codegen was explicitly requested but could
  not be configured instead of linking undefined embedded symbols (#775).
- **Release smoke portability and reliability:** the Darwin release path keeps its deployment
  target pin scoped correctly, pre-release validation no longer depends on GNU-only `mktemp`
  behavior, sanitizer OOM tests are hardened, and the QUIC remote-service smoke gate is
  stabilized (#763, #764, #771, #773).

### Changed

- **Release gating:** the pre-tag release gate is stronger, and the rust-runtime TSan lane is now
  explicitly documented as advisory until upstream toolchain support is restored (#757, #765).
- **Runtime/WASM parity proof:** closed-mailbox send behavior plus mailbox/scheduler/reply-channel
  parity are now explicitly covered in the native and wasm test matrix (#754).
- **CLI/docs truth surfaces:** the CLI/playground docs, first-run examples, and `hew fmt`/`hew
  doc` guidance now match the actual user-facing behavior more closely (#720, #748, #752, #755).

### Known Limitations

- **Nightly C++ sanitizer advisory:** the nightly `Codegen C++ ASan+UBSan` lane still reports an
  MLIR/container-overflow false-positive candidate in `mlirgen` teardown. It is tracked as #774
  and carried as an explicit advisory for v0.3.0; ordinary release builds and focused validation
  are otherwise green.

### Fixed

- **Fail-closed type metadata boundaries:** reject unresolved type-checker output holes and serializer-side explicit `-> _` survivors instead of leaking unresolved inference variables or reconstructing missing type data downstream (#838, #848, #849)
- **Trait default `-> _` resolution:** default trait methods with bodies now resolve explicit `-> _` from checker signatures and fail closed when the return type remains unresolved (#849)
- **CLI E2E bootstrap hardening:** serialize shared `hew-lib` bootstrap in the CLI test harness so concurrent integration runs do not race codegen setup (#851)
- **Codegen metadata hardening:** MLIR lowering now fails closed on missing indirect-enum scrutinee metadata and preserves bytes-stream ABI selection through tracked stream-metadata fallbacks (#852, #853)

## [0.2.2] - 2026-03-29

### Added

- **hew-observe unix socket discovery:** profiler binds to a per-user unix domain socket when `HEW_PPROF=auto`, with auto-discovery so `hew-observe` finds running programs without specifying ports (#380)
- **hew-observe CLI modes:** `--list` prints discovered profilers, `--pid N` connects to a specific process, auto-reconnects when the observed program restarts (#380)
- **WebSocket server support:** `websocket.listen(addr)` → `server.accept()` → `Conn` in std::net::websocket, with the same send/recv/close API as the client (#379)
- **Observe showcase example:** `examples/observe_showcase.hew` exercises Overview, Actors, Cluster, Messages, and Timeline tabs (#380)

### Fixed

- **Reply channel convention rewrite:** route reply channels through `HewMsgNode.reply_channel` field instead of embedding in the data buffer, eliminating SIGSEGV when actors receive messages with arguments (#380, fixes #382)
- **Duplicate trait impl generation:** track type-defining module so imported impls use the correct mangled names, preventing duplicate functions that executed as static constructors before `main()` (#386, fixes #384)
- **Vec.remove() semantics:** `v.remove(value)` is always value-based removal; the new `VecRemoveAtOp` is reserved for index-based `remove_at()` (#379)
- **hew-observe connection status:** only the metrics probe sets the connection indicator; secondary endpoint failures no longer poison it (#380)
- **Supervisor label JSON escaping:** child names containing quotes or backslashes no longer produce malformed JSON in `/api/supervisors` (#380)
- **Orphaned reply channels:** `hew_msg_node_free` sends an empty reply for undispatched ask messages so callers don't deadlock (#380)
- **Crash recovery reply handling:** scheduler sends an empty reply when an actor crashes during an ask dispatch (#380)
- **Silent analysis skips:** fix bugs where AST analysis passes silently skipped nodes (#377)
- **Diagnostic quality:** use operator symbols in error messages, correct LSP severity levels (#378)
- **Codegen type validation:** validate `convertType` results for generic types and thunk lookup (#372)
- **Runtime aliasing violation:** eliminate aliasing violation in `hew_connmgr_add` (#371)

### Changed

- **Profiler server:** replace tiny_http with hyper 1.x on a single-threaded tokio runtime, enabling unix domain socket support and cleaner shutdown (#380)
- **Pre-commit hook:** fix Bash 3.2 compatibility (macOS default) — clippy now runs on commit (#380)
- **CLI:** migrate hew to clap derive with auto-generated shell completions (#373)
- **Clippy clean:** resolve all clippy warnings across the workspace (`-D warnings` clean) (#380)
- Remove the unused export-metadata toolchain (`hew-stdlib-gen`, `hew-export-macro`, `hew-export-types`, and the `export-meta` Cargo feature) now that `hew-types` loads canonical stdlib `.hew` sources directly
- Quality consolidation: DRY deduplication, dead code removal, YAGNI cleanup (#374, #376)
- Update workspace dependencies (#375)

## [0.2.1] - 2026-03-23

### Added

- Generic `Channel<T>` for String and int types (#271)
- `Stream<T>` and `Sink<T>` monomorphization for String and bytes (#265)
- Stream bytes API exposed in stdlib (#265)
- Actor terminate cleanup blocks — run teardown logic when an actor stops (#268)
- Temporary materialization for RAII drop of unbound heap values (#274)
- `Option<T>` return from Channel `try_recv` (#272)
- 17 new test modules covering wire protocol, file I/O, timers, routing, crypto, generators, HTTP, iterators, streams, and Result/Option FFI wrappers (#295–#350)
- DateTime and JWT E2E tests (#345), JSON/YAML/TOML encoding E2E tests (#342, #343)
- C++ unit tests for codegen_capi and msgpack_reader (#291)
- 562 E2E tests (up from 451 in v0.2.0)
- Pre-release cross-platform validation script for Linux, macOS, FreeBSD, and Windows (#360–#362)

### Fixed

- **Runtime hardening (memory safety):** prevent use-after-free in actor lifecycle cleanup (#311), cache actor data before supervisor trap in crash handler (#303), drain queued values on generator free (#317), make `HewTask.state` atomic to prevent data races (#314), sync WASM `HewActor` struct with native layout (#322)
- **Runtime hardening (concurrency):** correct atomic orderings and consolidate CStr conversion (#332), eliminate lock unwraps and panics from production code (#333), use poison-recovery for blocking pool, registry, and link RwLocks (#318, #320), route supervisor restarts through mailbox to prevent budget race (#313), make actor state copy fallible on allocation failure (#326)
- **Runtime hardening (shutdown):** harden profiler shutdown and timer cancel protocol (#353), add graceful ticker shutdown to prevent timer wheel UAF (#307), prevent shutdown self-deadlock and spawn-failure stall (#309), close shutdown lifecycle and env-lock gaps (#330), shutdown profiler thread before freeing node resources (#308), shutdown sockets before dropping to prevent reader thread deadlock (#292)
- **Runtime hardening (networking):** harden Noise crypto — zeroize handshake buffers and return full keypair (#329), clean up connection on decrypt failure (#300), harden stream buffering — cap line buffer and add buffer-reuse API (#328)
- **Runtime hardening (scheduling):** propagate worker spawn failures during scheduler init (#315), treat Stopping as non-terminal in scope wait (#316), synchronise environment variable access with RwLock (#302), auto-seed MT19937 from OS entropy (#304)
- **Runtime hardening (observability):** preserve trace context in mailbox MPSC dequeue (#301), add `set_last_error` calls to FFI functions that silently return null (#335), standardise FFI string ownership on `malloc_cstring` (#324)
- **Codegen:** RAII drop system hardening with null-guard and infrastructure (#273), align actor send drop semantics with deep-copy transport (#275), path-specific drops for struct return from nested scopes (#284), handle Result return type in actor receive functions (#267), systematic RAII memory leak audit
- **Analysis:** classify function calls as FUNCTION in semantic tokens (#354), classify type annotations as TYPE in semantic tokens (#269)
- **Types:** prevent non-trivial wrappers from being treated as pass-throughs (#293), resolve channel.hew standalone type-check failures (#294), default unresolved Range type variable to i64 (#266)
- **CLI:** use cross-platform home directory resolution in adze (#351), handle `--help` flag in hew-lsp before server initialisation (#349)
- **Build:** fix static link ordering for aarch64 Linux release builds — include libstdc++ and libgcc in archive group for GNU ld circular dependency resolution (#359, #363–#369), fix macOS build detection and Make 3.81 segfault (#355), register encoding/hex and fix crypto interface location in stdlib (#338)

### Changed

- Embed MLIR/LLVM codegen into single `hew` binary — `hew-codegen` is no longer a separate executable (#261)
- Decompose `compile()` into pipeline stages (#287)
- Decompose `synthesize_inner` into focused helpers (#285)
- Simplify type checker and enrich passes (#283)
- Simplify enrich.rs tree-walkers and eliminate duplication (#286, #299)
- Decompose method dispatch and extract helpers (#282)
- Unify let/var drop registration and extract helpers (#280)
- Extract shared overflow-policy logic from mailbox send functions (#334)
- Replace LIVE_ACTORS linear scan with HashMap index (#325)
- Auto-discover E2E tests and consolidate test categories
- Strip `hew_` prefix from internal connection helpers (#331)

## [0.2.0] - 2026-03-15

### Added

- QUIC transport for inter-node messaging in the Node mesh (#152, #163)
- Happy Eyeballs (RFC 8305) TCP connector in adze package manager (#162)
- Distributed reply channels for remote `await` across nodes (#169)
- Cross-node registry gossip for `Node::lookup` (#168)
- Remote actor dispatch through the Node mesh
- Two-process QUIC mesh demo with transport cleanup
- Node API builtins wired through typechecker and MLIR codegen
- Comprehensive doc comments added to stdlib modules; improved doc renderer template and markdown rendering (#170)
- Duration as a distinct primitive type (#104)
- `hew-analysis` crate extracted; `hew-wasm` brought to feature parity with `hew-lsp` (#111)
- Stdlib gap fill: `string.split`/`lines`/`join`, filesystem directory helpers, URL encoding, HTTP response accessors (#132)
- Schema version field added to MessagePack AST boundary (#124)
- Grammar fuzzer for TextMate and tree-sitter grammars (#120)
- Centralized downstream generator and sync script for editor integrations
- Duration type design document and implementation plan

### Fixed

- Allow `await` on void receive handlers (#156, #159)
- Mark module as used when spawning `module.Actor()` (#160)
- MLIR codegen lowering for `to_float()` builtin (#158)
- Forward direct SIGTERM/SIGINT to `hew run` child process (#151)
- Emit error instead of silently defaulting `None` to `Option<i32>` (#115)
- Eliminate silent type fallbacks in codegen (#117)
- Close type-checker inference gaps found in audit (#116)
- Improve integer literal type inference and coercion (#114)
- Array literal to `Vec` coercion for enum variant elements (#112)
- Ecosystem package resolution for `--pkg-path` and lib search; module resolution and actor field access in codegen (#146)
- Suppress unused-import warnings for sub-module imports
- Replace catch-all match arms in `enrich.rs` with exhaustive variants (#122)
- Eliminate silent fallbacks in deserializer, codegen, and parser (#129)
- Convert warnings to errors and remove dead string dispatch (#126)
- Scope `zlib`/`zstd` static link and `-static-libstdc++` to Linux only
- Static-link `zlib`/`zstd`/`libstdc++` in hew-codegen; simplify distro packaging
- Strip phantom z3 dependency from hew-codegen; add multi-distro test script
- Correct release packaging — stdlib sources, static libs, and codegen static build
- Resolve all pre-existing Clippy warnings in workspace (#133)
- Remove stale `isolated`/`and`/`or` keywords from tmLanguage generator (#131)
- Refactor `convertType` validation into `convertTypeOrError` helper (#119)
- Unused import fixes (#140, #141)

### Changed

- Migrate actor examples away from sleep-based synchronization (#157)
- Homogeneous module resolution and stdlib pure-Hew migration (#128)
- Observer TUI polish — theme, clamping, and UX fixes (#130)
- Deduplicate `Ty` mapping and add FFI symbol verification (#127)
- Replace raw `is_null()` + return boilerplate with `cabi_guard!` macro (#125)
- Unify `vecElemSuffix` and `vecElemSuffixWithPtr` into a single function (#123)
- Canadian English `-our` spellings adopted across codebase (#113)
- Sync spec and grammars to v0.9.0 against implementation (#103)
- Prune superseded lessons and consolidate duplicates (#121)
- Remove stale plan docs for completed features (#110)

## [0.1.9] - 2026-03-07

### Added

- FreeBSD platform support (x86_64-freebsd compilation target)
- FreeBSD CI and release pipelines
- kqueue-based I/O poller for FreeBSD and macOS (replaces epoll on those platforms)

### Fixed

- WASM runtime: fix HewError import path
- Runtime: handle ask send failures explicitly (prevents hangs on failed sends)
- Serializer: report unsupported inferred types instead of silently dropping them
- Serializer: harden type conversion diagnostics in enrich pass

### Changed

- Refactor: consolidate Linux/FreeBSD ELF linker flags into shared path
- Refactor: extract shared `exe_suffix()` helper across CLI
- Refactor: extract shared signal recovery logic
- Refactor: remove dead `linkExecutable` from hew-codegen

## [0.1.8] - 2026-03-06

### Added

- Numeric literal type coercion: integer and float literals automatically coerce to the expected type with range validation (e.g., `var t: i32 = 4` works without explicit cast)
- Untyped const coercion: `const N: Int = 10` can be used as i32, u8, etc.
- `indirect enum` for recursive data types (expression trees, linked lists, etc.) with automatic heap allocation and RAII cleanup
- Named supervisor child access via field syntax (`sup.child_name` resolves to `supervisor_child(sup, idx)` at compile time)
- WASM platform capability documentation
- `s.spawn {}` parallel task syntax for structured concurrency
- Custom type indexing via `get()` method (`obj[key]` desugars to `obj.get(key)`)
- HashSet data structure with insert, contains, remove, len operations
- Granular visibility modifiers: `pub(package)` and `pub(super)`
- Bare `self` parameter in methods (no type annotation required)
- Label support on `for` loops (@label: for ...)
- Char literal support (`'a'`, `'\n'`, escape sequences)
- Associated type declarations inside `impl` blocks, including trait defaults and `Self::Alias` resolution
- `Self::Type` syntax in type position for associated type references
- Negative literal patterns in match expressions (-1, -3.14)
- `if let` conditional pattern syntax
- Array repeat syntax `[value; count]` for initializing arrays
- Struct-like enum variants with named fields (Variant { field: Type })
- Trait bound enforcement at call sites for generic functions
- Unsafe block enforcement: extern FFI calls require `unsafe { }` wrapper
- Multi-trait dyn objects: `dyn (Trait1 + Trait2)`
- Range expressions as first-class values (variable-bound ranges in for loops)
- Timeout expression codegen support (`expr | after duration`)
- Generic lambda syntax support (<T>(x: T) => expr)
- Coroutine support for aarch64 (ARM64) architecture
- String predicate methods: `.is_digit()`, `.is_alpha()`, `.is_alphanumeric()`, `.is_empty()`
- `String.lines()` method: split string on newlines (strips `\r`) returning `Vec<String>`
- `Vec<String>.join(sep)` method: join elements with separator string
- `Vec<T>.map((x) => expr)` method: transform each element, returns new `Vec<U>`
- `Vec<T>.filter((x) => expr)` method: keep elements where closure returns true, returns new `Vec<T>`
- `Vec<T>.fold(init, (acc, x) => expr)` method: reduce to a single value
- LLVM coroutine-based `gen fn` codegen: `yield` inside `while`/`for`/`loop` now works correctly (loop variables preserved across yields)
- `HashMap.keys()` method: return `Vec<K>` of all keys (type checker; codegen already existed)
- `join` is now a contextual keyword, usable as a method name in dot-call position

### Fixed

- Type checker: reject match arms using variants from wrong enum type
- Type checker: bare `return;` in non-unit functions now produces error
- Type checker: `dyn (A + B)` now unifies with `dyn (B + A)` (order-independent)
- Type checker: match statements now check exhaustiveness (missing variants warn)
- Type checker: `Self` in generic impls now resolves to full type (e.g., `Pair<T>` not bare `Pair`)
- Type checker: `dyn Trait<Args>` method dispatch now substitutes bound type args into signatures
- Type checker: lambda arity mismatch now detected (1-param lambda can't pass as `fn(int,int)->int`)
- Type checker: OR-patterns (`Some(x) | None`) now counted in exhaustiveness checks
- Type checker: guarded wildcard/identifier patterns no longer count as exhaustive (`_ if false => ...` warns)
- Parser: missing parameter type annotation now reports error instead of silent drop
- Parser: `pub(invalid)` now defaults to private instead of silently promoting to pub
- Parser: string interpolation sub-parser errors now propagated to parent
- Parser: empty struct literal `Foo {}` now parses correctly for zero-field structs
- Parser: invalid escape sequences now report error instead of silent failure
- Parser: positional args after named args now skipped instead of producing malformed AST
- Codegen: string ordering operators (`<`, `<=`, `>`, `>=`) now use lexicographic comparison instead of pointer comparison
- Codegen: `if let` statements now fully implemented (pattern matching with variable binding)
- Codegen: array repeat expressions `[val; count]` now generate Vec with runtime loop
- Codegen: struct variant patterns as last match arm now check tag (fixes UB with wrong variant)
- Codegen: `if let` bodies with `return`/`break`/`continue` now correctly guard subsequent code
- Codegen: lambda capture analysis now respects pattern-bound variables in match/for/if-let
- Codegen: `ExprIfLet` and `ExprArrayRepeat` inside lambdas now correctly tracked for capture analysis
- Codegen: `..=` inclusive range now accepts all integer widths (was only i64/index)
- Codegen: ordering operators on actor pointers now emit error instead of calling string compare (prevents UB)
- Codegen: indexed compound assignment (`v[i] += 1`) now applies the operator (was silently dropping it)
- Codegen: HashSet insert/contains/remove no longer double-evaluate argument expressions
- Codegen: Vec<bool> now uses consistent runtime suffixes (fixes data corruption)
- Codegen: tuple patterns in match expressions now destructure correctly (was silently skipped)
- Codegen: return statements now evaluate expression before dropping locals (fixes use-after-free)
- Codegen: labeled break/continue now deactivates ALL intermediate loops (fixes infinite loop with 3+ nesting)
- Codegen: labeled break/continue now drops resources in ALL intermediate scopes (fixes leaks)
- Codegen: scope binding (`scope |s| { }`) now declares variable for body access (fixes scope_spawn)
- Codegen: scope spawn now captures mutable variables via heap cells (cross-task mutation works)
- Codegen: compound assignment switches now have default case (prevents UB)
- Codegen: IfLet, ArrayRepeat, generic lambda type_params now deserialize without crash
- Codegen: TypeExpr::Infer now deserializes in C++ (was crashing with unknown variant)
- Runtime: HashMap/Vec string getters return owned copies (prevents use-after-free)
- Runtime: HashMap strdup calls now abort on NULL (prevents silent corruption on OOM)
- Runtime: `hew_string_compare` added for correct lexicographic string ordering
- Codegen: f32 values now print correctly (promoted to f64, was falling through to i32 printer)
- Codegen: f32↔f64 float coercion now handled via ExtFOp/TruncFOp (was missing)
- Codegen: nested constructor patterns like `Some((a, b))` now destructure tuple payloads
- Codegen: `char_at` index extension uses zero-extend (prevents signed misinterpretation)
- Type checker: type variables resolved before pattern matching (fixes false mismatches on generics)
- Type checker: unknown fields in struct patterns now report `UndefinedField` error with suggestions
- Codegen: for-loop over stored ranges uses ExtSIOp instead of IndexCastOp (fixes MLIR verification)
- Codegen: for-loop over stored ranges now uses continue guards and MutableTableScopeT
- Codegen: constructor pattern guards now bind PatTuple sub-patterns (e.g., `Some((a,b)) if a > 0`)
- Codegen: `loop {}` now checks returnFlag before re-entering body (fixes infinite loop on return)
- Codegen: `var` declaration with failed expression no longer leaks pendingDeclaredType into subsequent expressions
- Codegen: stream/generator/for-await loops now respect `continue` via continue guards
- Codegen: log emit now drops temporary string after hew_log_emit call (fixes leak)
- Runtime: integer overflow checks in string replace_all, string repeat, and hashmap resize (prevents UB)
- Parser: `expect()` and `parse_identifier()` no longer panic on unexpected EOF (returns error)
- Serialization: `rewrite_builtin_calls` now traverses all expression variants (InterpolatedString, PostfixTry, Await, Yield, Send, Range, Unsafe, Join, Timeout, ScopeLaunch, ScopeSpawn, Scope, SpawnLambdaActor, Match, Lambda, Spawn, StructInit, Select)
- Zero compiler warnings across entire Rust workspace
- All 333 codegen e2e tests pass (up from 321)
- Codegen: log emit double-free fixed — non-string args no longer freed twice in ownedTemps cleanup
- Codegen: labeled loop flags (activeFlags/continueFlags) now cleaned up in all 5 for-loop variants
- Codegen: `for await` stream loops now support labeled break/continue
- Codegen: or-pattern with enum unit variants now generates correct tag comparison (was always-true)
- Codegen: inclusive range `..=` now accepts all integer widths (was only i64/index)
- Codegen: range type mismatch between start and end now coerced (was silently ignored)
- Runtime: Vec strdup calls abort on NULL (OOM safety, consistent with HashMap)
- Serialization: normalization now covers Trait, TypeBodyItem::Variant, Const, TypeAlias items
- Codegen: ToStringOp now promotes f32 to f64 before calling hew_float_to_string (fixes garbled output)
- Codegen: AssertOp/AssertEqOp/AssertNeOp now handle i8, i16, and f32 types (fixes ABI mismatch)
- Codegen: VecNewOp struct layout now correct for f32 fields (was using 8-byte size instead of 4)
- Codegen: lambda capture analysis now covers Spawn, SpawnLambdaActor, Scope, ScopeLaunch, ScopeSpawn, Select, Join, Range, Timeout, Yield, Unsafe expressions
- Codegen: Vec<bool>/Vec<i8>/Vec<i16> inline push/get/set now use correct i32 element stride (fixes memory corruption)
- Codegen: SleepOp saturates i64→i32 truncation at INT32_MAX (prevents silent wrap)
- Codegen: Vec<f32> now uses \_f64 runtime path with f32↔f64 promotion/truncation (fixes crash)
- Codegen: VecPop now handles f32 and narrow int return type conversion
- Codegen: Vec push/get/set/pop fallback paths now promote/truncate for f32 and narrow ints
- Codegen: trait object default value uses null pointer for vtable (was i32(0), type violation)
- Codegen: collectFreeVarsInStmt now handles StmtMatch, StmtBreak, StmtDefer
- Runtime: string concat overflow check (checked_add before malloc)
- Runtime: string split NULL check after malloc_cstring
- Runtime: TCP framing overflow check before u32 cast
- Codegen: VecRemoveOp now promotes i1/i8/i16 to i32 and f32 to f64 (fixes type mismatch in runtime calls)
- Codegen: HashMapInsertOp now promotes f32 to f64 and i1/i8/i16 to i32 (fixes silent miscompile for narrow types)
- Codegen: HashMapGetOp now declares correct return type and narrows result (i32→i1/i8/i16, f64→f32)
- Codegen: PrintOp now emits error for unhandled types instead of silent i32 fallback
- Runtime: added hew_vec_remove_f64 for Vec<f64>/Vec<f32> remove-by-value
- Codegen: return inside loop body now sets continueFlag (prevents side effects after return in same iteration)
- Codegen: labeled break across 3+ nesting levels now sets continue flags for ALL intermediate loops
- Codegen: AssertEqOp/AssertNeOp fallthrough replaced with explicit i64/index check + error for unknown types
- Codegen: unhandled match pattern now emits error instead of silent skip with warning
- Runtime: Vec append overflow check (checked_add before ensure_cap)
- Codegen: HashMap.get() now returns Option<T> at MLIR level (fixes let-binding + match pattern)
- Codegen: non-exhaustive match now traps at runtime instead of silently returning zero
- Type checker: non-exhaustive match warning now covers all types (int, float, string), not just enums
- Runtime: Vec push functions use checked_add for overflow protection
- Runtime: Vec append validates elem_size/elem_kind match before memcpy
- Runtime: added hew_vec_set_ptr and hew_vec_pop_ptr for pointer-type vectors
- All 335 codegen e2e tests pass (up from 321)
- Type checker: implicit integer narrowing (e.g., i64→i32) now rejected; only widening allowed
- Type checker: Vec/HashMap/HashSet/String method indices and lengths use int (i64) instead of i32
- Type checker: array/index expressions check index against int (i64) instead of i32
- Codegen: StmtReturn now included in stmtMightContainBreakOrContinue guard (fixes incorrect SCF yield)
- Codegen: LitChar uses char32_t for full Unicode codepoint preservation
- Codegen: msgpack char deserializer decodes multi-byte UTF-8 sequences
- Codegen: var reassignment now drops old owned value (prevents memory leak for String/Vec/HashMap)
- Codegen: function argument coercion now passes isUnsigned flag (u32→u64 uses extui not extsi)
- Codegen: `generateLiteral()` now emits literals at correct MLIR width from resolved type (was always i64/f64)
- All 338 codegen e2e tests pass (100%, up from 263/335 = 79%)

### Changed

- **BREAKING**: Collection indices and lengths are now `int` (i64) instead of `i32`
- Test files updated: 80+ test files changed from i32 to int for function signatures and variables

- Function call results can be silently discarded — no more `let _ =` required
- Parser: deduplicated function modifier handling (extracted `parse_fn_with_modifiers`)
- Improved WASM target error messages for unsupported concurrency operations
- **Breaking**: `HashMap.get(key)` now returns `Option<T>` instead of raw `T` — use `match` to unwrap

## v0.1.5 — 2026-02-28

### Added

- 112 algorithm and data structure examples (sorting, searching, graphs, trees, heaps, etc.)
- 57 Go-comparison benchmarks with ops/sec measurements
- Inline Vec get/set/len/push for primitive types (2–5× speedup on native)
- Inline string `char_at` (direct GEP+load with bounds check)
- While-loop invariant hoisting (pre-evaluates loop-invariant conditions)
- Inline Vec push fast path (store+len-increment when capacity allows)
- `<-` send operator now works correctly in codegen

### Fixed

- Break stack desync in `for..in` Vec/HashMap loops (loopBreakValueStack push/pop)
- `hew_actor_free` manifest ABI mismatch (void → i32)
- Vec `<String>` double-free (exclude VecGetOp/HashMapGetOp from temporary drop)
- HashMap.get() match wraps raw value in Option for Some/None patterns
- WASM32 inline lowering: skip on non-64-bit targets (struct layout mismatch)
- Windows `libc::write` type mismatch in string abort handler
- Silent codegen fallbacks replaced with warnings/errors
- Parser silent token skips replaced with error messages

### Changed

- Converted all counting `while` loops to idiomatic `for i in 0..N` range syntax
- Code audit: 25 Mutex poison-recovery fixes, 5 unsafe UTF-8 fixes, ~70 Clippy warnings
- Parser deduplication: extracted `contextual_keyword_name()` and `collect_doc_comments_with_prefix()`

## v0.1.4 — 2026-02-26

### Fixed

- Eliminate false 'function is never called' warnings
- Properly static-link MLIR/LLVM in hew-codegen release builds
- Resolve all codegen test failures and WASM build warnings
- Windows test compatibility and end-to-end build pipeline

### Changed

- Simplify over-engineering: parser, runtime, std libraries

## v0.1.3 — 2026-02-25

### Fixed

- Resolve all codegen test failures and WASM build warnings

### Changed

- Bump version to 0.1.3

## v0.1.2 — 2026-02-24

### Added

- Windows x86_64 build support (#13)
- Distributed actors v2: bridge, transport synchronization, cluster membership (#14)
- Distributed observer HTTP API endpoints
- Distributed TUI: Cluster, Messages, and Timeline tabs

### Fixed

- Runtime: actor sync hardening, free wait
- Runtime: transport connection slot synchronization
- Runtime: close transport before dropping connection actor
- Runtime: supervisor delayed restart safety
- Runtime: pool lifecycle and mailbox sys backlog
- Runtime: hew_node start cleanup and actor ID encoding
- Wire varint validation
- Cluster membership message handling hardening
- Allowlist strict mode and zeroize key copies
- Bridge.rs post-rebase build errors

## v0.1.1 — 2026-02-23

### Fixed

- Release packaging: adze completions, RPM spec, Docker image, macOS signing
- macOS notarization: switch from Apple ID to App Store Connect API keys
- Non-Linux builds: continue-on-error for cross-platform CI
- Secret checks: use job-level env vars for signing conditions

## v0.1.0 — 2026-02-22

**Hew** is a statically-typed, actor-oriented programming language for concurrent and distributed systems. It features Erlang-inspired supervision trees, first-class async/await, and message-passing concurrency — compiled to native code via MLIR and LLVM.

### Added

#### Language

- Full compilation pipeline: `.hew` → Rust frontend → MLIR → LLVM → native binary
- Core language: functions, variables (`let`/`var`), control flow (`if`/`else`, `while`, `for`, `loop`), match expressions, closures/lambdas (including mutable capture), generics, traits with vtable dispatch, tuples, string interpolation (f-strings), range expressions, `defer`
- Actors: `spawn`, `send`, `receive`, `ask`/`await`, lambda actors
- Supervision trees: `supervisor` keyword with `one_for_one`, `one_for_all`, `rest_for_one` strategies
- Fault propagation: `link`, `monitor`, `unlink`, `demonitor`
- Actor priorities and mailbox policies (`block`, `drop_new`, `drop_old`, `coalesce`)
- Collections: `Vec<T>`, `HashMap<K,V>`, `bytes`
- Structured concurrency with `scope` and `launch`
- Streams: `stream.channel()` returning `(Sink, Stream)` tuples, `stream.pipe()`, `for await` loops, file-backed streams with `Result` error handling

#### Compilation Targets

- x86_64-linux
- x86_64-macos
- aarch64-macos
- wasm32-wasi (single-threaded programs only)

#### Standard Library

- **Standard:** `std::fs`, `std::log`, `std::os`, `std::net`, `std::encoding::json`, `std::text::regex`, `std::process`, `std::misc::uuid`, `std::time::datetime`, `std::net::url`, `std::path`, `std::encoding::base64`, `std::encoding::hex`, `std::crypto::crypto`, `std::encoding::compress`, `std::stream`
- **Extended:** `std::net::http`, `std::crypto::jwt`, `std::encoding::yaml`, `std::encoding::toml`, `std::encoding::csv`, `std::encoding::msgpack`
- **Ecosystem (separate repo):** `db::postgres`, `db::redis`, `db::sqlite`, `misc::glob` — see [hew-lang/ecosystem](https://github.com/hew-lang/ecosystem)

#### Tooling

- **LSP**: diagnostics, completion, hover, symbols, semantic tokens
- **VS Code extension**: syntax highlighting and language support
- **REPL**: `hew eval` for interactive expression evaluation
- **Test runner**: `hew test` for `.hew` test files
- **Doc generator**: `hew doc` for generating documentation from source
- **Package manager**: `adze` — init, install, publish, search, with single-fallback registry resilience
- **Observability**: `hew-observe` TUI for live actor inspection (connects to runtime profiler endpoint)

#### Installation

- Shell installer (`curl | bash`)
- Homebrew
- Docker
- Debian, RPM, Arch, Alpine, and Nix packages
- Shell completions for bash, zsh, and fish

#### Developer Experience

- Clear, actionable compiler error messages for common mistakes

### Known Limitations

- WASM target is single-threaded: basic actors (spawn, send, ask/await) work, but supervision trees, link/monitor, and scoped concurrency are not supported
- No incremental compilation
- `unsafe` blocks are parsed but not yet enforced (treated as regular blocks)

### Getting Started

```bash
# Install Hew
curl -fsSL https://hew.sh/install.sh | bash

# Create and run your first program
echo 'fn main() { println("Hello from Hew!"); }' > hello.hew
hew run hello.hew

# Try the REPL
hew eval

# Start a new project
hew init my_project
```

Visit [hew.sh](https://hew.sh) for documentation and examples.
