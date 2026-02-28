# Changelog

## [Unreleased]

### Added

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
- All 329 codegen e2e tests pass (up from 321)

### Changed

- Function call results can be silently discarded — no more `let _ =` required
- Parser: deduplicated function modifier handling (extracted `parse_fn_with_modifiers`)
- Improved WASM target error messages for unsupported concurrency operations

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
adze init my_project
```

Visit [hew.sh](https://hew.sh) for documentation and examples.
