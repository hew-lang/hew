# Hew v0.4.0

Hew v0.4.0 is the first release in which the language draws a firm
boundary between Hew code and the C ABI underneath it, and the first in
which the developer toolchain — language server, formatter, JIT,
distributable binary — is good enough to use every day without
workarounds.

## From v0.3 to v0.4

Hew v0.3, released in early April, made the language usable. v0.4 makes
it durable. The release is organized around four themes that, taken
together, justify a deliberate minor-version step rather than a
patch-level accumulation of fixes.

**Resource ownership becomes explicit.** Six breaking changes tighten
the contract between Hew values and the system resources they wrap.
`http.Server`, `http.Request`, `regex.Pattern`, and `json.Value` no
longer auto-release on scope exit; programmers state when those
resources are torn down. `compress` decompression now requires a
maximum-output bound, turning an OOM hazard into a fail-closed call.
The standard library's user-facing integer surface migrates from `i32`
and `i64` to a single `int` type, leaving the ABI-typed widths visible
only inside `extern "C"` blocks where they belong. The result is that
Hew code reads like Hew code, and the seams to C are visible exactly
where they exist.

**The toolchain becomes daily-use quality.** The language server now
implements format-on-save, resolves goto-definition and find-references
across the standard library, and surfaces structured parse-error
classifications that editor extensions can route on. The formatter
preserves inline comments through enums, struct field lists, match
arms, and `else if` branches — places it used to silently strip them.
The release binary on macOS and Linux is genuinely standalone, with
static libc++ linkage and no Homebrew rpath dependency.

**The JIT becomes preview-quality.** `hew eval --jit=inprocess` and
`--jit=auto` are stabilised for preview use in this release. The inprocess mode no longer
crashes the host process on first emit — runtime symbols are exported
correctly, and a use-after-free in the search-generator lambda capture
is closed. The JIT is now appropriate for REPL and eval workflows, not
only for batch AOT.

**Language correctness in places it matters.** Trait method dispatch
works on primitive and builtin-generic receivers, opening user-defined
extensions over `int`, `String`, `Vec<T>`, and the rest. Cross-module
enum variant construction and `Ok(())` as a unit-payload pattern parse
correctly. Per-call deadlines on DNS, TCP, QUIC, and WebSocket I/O
turn indefinite-block failure modes into deterministic timeouts.

These changes set the foundation v0.5 can build on: monomorphization,
the move-checker prototype, and broader stdlib maturity all assume the
ownership contract that landed here.

## What you can now rely on

The v0.4.0 release commits to the following invariants. Code that
respects them will not be quietly broken by point releases on the
v0.4 line.

- **Resource ownership is explicit.** Hew values that wrap system
  resources release them when the program says so. There is no hidden
  `Drop` on `http.Server`, `http.Request`, `regex.Pattern`, or
  `json.Value`. This contract is the precondition for the move-checker
  and ownership work scheduled for v0.5.
- **`int` is the user-facing integer.** `i32` and `i64` appear in user
  code only at `extern "C"` ABI seams. The standard library never asks
  callers to think about ABI widths.
- **Decompression is bounded.** `gzip_decompress`, `deflate_decompress`,
  and `zlib_decompress` require an explicit `max_output_len`. There is
  no path through the standard library that admits a decompression
  bomb.
- **The LSP is feature-complete for daily use.** Format-on-save,
  cross-file stdlib navigation, hover, find-references, rename, code
  actions, and inlay hints all work against the current standard
  library without editor-side workarounds.
- **The release binary is standalone.** macOS and Linux tarballs run
  on a fresh machine with no Homebrew prerequisite and no dynamic
  libc++ dependency.
- **The JIT preview is non-crashing.** `hew eval --jit=inprocess` and
  `--jit=auto` complete reliably on programs that previously
  segfaulted, including the search-generator pattern.
- **Network calls are bounded by deadline.** DNS, TCP connect, QUIC,
  and WebSocket calls accept an optional deadline and fail closed when
  it is exceeded.

## Breaking changes

Six breaking changes ship in this release. Three are caught at compile
time. Three are silent behaviour changes that require a manual
call-site audit. The full migration steps with before/after examples
are in the
[v0.4.0 migration guide](https://github.com/hew-lang/hew/blob/main/docs/migrations/v0.4.0.md).

- **Stdlib `int` surface.** All public `std/**/*.hew` function
  signatures use `int` instead of `i32` or `i64`. Affects 22 modules
  including `channel`, `encoding/*`, `fs`, `net/*`, `path`,
  `semaphore`, `text/semver`, and `time/cron`. *Migration:* replace
  `i32`/`i64` in binding declarations and call-site argument positions
  with `int`; ABI seams (`extern "C"` blocks) are exempt. Compile-time
  error if not migrated. (#1218)

- **`compress` decompression requires `max_output_len`.**
  `gzip_decompress`, `deflate_decompress`, and `zlib_decompress` each
  gained a required second argument `max_output_len: int`. The
  function fails closed when decompressed output would exceed the
  limit. *Migration:* add `max_output_len` at every call site; use
  `64 * 1024 * 1024` if no tighter bound applies. Compile-time error
  if not migrated. (#1471)

- **Explicit `http.Server` and `regex.Pattern` teardown.** Both types
  no longer auto-release on scope exit. The `impl Drop` was removed
  because it double-freed when callers also called `close()`/`free()`
  explicitly. *Migration:* call `server.close()` before every
  `http.Server` binding goes out of scope; call `pat.free()` before
  every `regex.Pattern` binding goes out of scope. Silent resource
  leak if not migrated. (#1314)

- **Explicit `http.Request` and `json.Value` teardown.** Same pattern
  as above. *Migration:* call `req.free()` after handling each request;
  call `val.free()` (and sub-value `free()`) for every `json.Value`
  produced by `json.parse` or `val.get_field(...)`. Silent resource
  leak if not migrated. (#1500)

- **`http_client` string helpers return `Option<String>`.**
  `get_string`, `post_string`, and `request_string` changed return
  type from `String` to `Option<String>`. `None` indicates transport
  failure. *Migration:* pattern-match on the result at each call site.
  Compile-time error if not migrated. (#1030)

- **WASM empty-result encoding.** Optional scalar LSP exports
  (`hover`, `goto_definition`, `find_references`, `prepare_rename`,
  `signature_help`) now return `"null"` instead of `""` for the
  no-result case. Collection exports (`rename`, `inlay_hints`) are
  unchanged at `"[]"`. *Migration:* update empty-result guards from
  `result === ""` to `result === "null"` in JS/TS integrations.
  Silent behaviour change if not migrated; the previous `""` would
  throw on `JSON.parse`, while `"null"` parses to `null` and bypasses
  a string-equality guard. (#1506)

## What's new

### Compiler and runtime

- **Trait method dispatch on primitives and builtin generics.** Trait
  methods can now be called on `int`, `String`, `Vec<T>`, and similar
  receivers, enabling user-defined extension traits over standard-library
  types. (#1596)
- **Per-call network deadlines.** DNS lookup, TCP connect, QUIC
  connection setup, and WebSocket I/O accept an optional deadline.
  Calls that exceed the deadline fail closed rather than blocking
  indefinitely. (#1557)
- **Structured parse diagnostics.** A `ParseDiagnosticKind` discriminant
  is exported from the WASM surface and surfaced through LSP code-action
  routing, giving tooling consumers a machine-readable parse-error
  classification. (#1583, #1592)

### Language server and formatter

- **Format-on-save.** The language server implements
  `textDocument/formatting` by delegating to `hew_parser::fmt`. Editors
  format with the same code path that `hew fmt` uses, with no separate
  shell invocation. (#1614)
- **Cross-file stdlib navigation.** Goto-definition and find-references
  resolve standard-library symbols across file boundaries, including
  cross-file references that previously dead-ended at the import
  statement. (#1616)
- **Transitive goto-definition.** Goto-definition follows one import
  hop with a cycle guard, so it works through re-export and glob
  chains. (#1073)

### Standard library and examples

- **HTTP + JSON demo.** `examples/http_json_demo.hew` is a complete
  HTTP server that accepts JSON request bodies, parses them with
  `json.parse`, and responds with JSON — demonstrating the
  explicit-teardown pattern v0.4.0 requires. (#1618)
- **URL percent-encoding end-to-end.** `url.encode`, `url.decode`, and
  `url.encode_query` are confirmed working on native and WASM
  targets. (#1077)
- **Module search-path documentation.** `HEWPATH`, `HEW_STD`, the
  four-step resolution order, and the role of `hew.toml` are
  documented. (#1074)

### Build and distribution

- **Release-binary stability on macOS.** Three function-local
  `static const std::regex` declarations in MLIR codegen caused a
  libc++ ABI mismatch at process exit — the Homebrew-compiled locale
  object was freed by the system allocator, producing a SIGABRT on
  every `hew run` invocation. Each `std::regex` now lives only for the
  duration of the codegen call, destroyed before any mixed-ABI exit
  boundary is reached. (#1607)
- **linux-aarch64 pre-release gate.** ARM Linux builds are validated
  by a dedicated pre-release CI job before tagging. (#1608)

## Bug fixes

- **JIT inprocess no longer crashes.** The inprocess JIT mode
  (`hew run --jit`, `hew eval --jit`) no longer segfaults on programs
  that use the search-generator pattern. Two root causes were
  addressed: missing runtime-symbol export, and a use-after-free in
  the search-generator lambda capture. (#1613)
- **Cross-module enum variant construction.** Enum variants defined in
  one module can be constructed with a payload from another module
  without a parser error. (#1605)
- **`Ok(())` as a unit-payload pattern.** The type checker now accepts
  `Ok(())` as a valid pattern in match arms. (#1617)
- **Formatter preserves inline comments.** `hew fmt` no longer drops
  inline comments inside enum bodies, struct field lists, match arm
  bodies, or around `else if` branch headers. (#1535)
- **REPL piped-mode flush.** The REPL flushes stdout after each
  submission in piped mode, and `--jit` is forwarded correctly into
  interactive mode. (#1553)
- **`fs.try_read_bytes` binary safety.** Non-UTF-8 and NUL-containing
  binary files round-trip correctly through `try_read_bytes`. (#1076)

## Documentation

- The v0.4.0 migration guide at
  [`docs/migrations/v0.4.0.md`](https://github.com/hew-lang/hew/blob/main/docs/migrations/v0.4.0.md)
  walks through every breaking change with before/after examples.
- HEW-SPEC.md is re-audited for the v0.4.0 surface.
- Stdlib documentation is published to Cloudflare Pages via
  `make publish-docs`.

## Install

**One-line installer (macOS and Linux):**

```bash
curl -fsSL https://hew.sh/install.sh | bash
```

**Homebrew (macOS):**

```bash
brew install hew-lang/hew/hew
```

**Pre-built tarballs** for linux-x86_64, linux-aarch64, darwin-x86_64,
darwin-arm64, and windows-x86_64 are on the
[Releases page](https://github.com/hew-lang/hew/releases/tag/v0.4.0).

**From source:**

```bash
cargo install --git https://github.com/hew-lang/hew hew-cli
```

## Verify

```bash
hew version
# hew 0.4.0
```

If you are upgrading from v0.3.x, work through the
[v0.4.0 migration guide](https://github.com/hew-lang/hew/blob/main/docs/migrations/v0.4.0.md)
before recompiling existing projects. Three of the six breaking
changes will compile-error if missed; three will not.

## Acknowledgments

Thank you to everyone who filed issues, tested pre-release builds, and
helped pressure-test the explicit-teardown contract before it shipped.

## Full changelog

See [`CHANGELOG.md` — \[0.4.0\]](https://github.com/hew-lang/hew/blob/main/CHANGELOG.md#040---2026-05-03)
or compare:
[v0.3.0...v0.4.0](https://github.com/hew-lang/hew/compare/v0.3.0...v0.4.0).
