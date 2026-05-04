# Hew v0.4.0

v0.4.0 is a hardening and developer-experience release. The headline work is a
set of six breaking changes that tighten resource ownership, close security
gaps, and draw a hard line between Hew's user-facing integer type (`int`) and
the C ABI seams underneath. Alongside those breaks, the LSP gains document
formatting and stdlib navigation, the JIT mode is stabilized, and a new HTTP +
JSON example shows the v0.4.0 explicit-teardown pattern end-to-end.

## ⚠️ Breaking changes

Six breaking changes ship in this release. Three are caught at compile time.
Three are silent behaviour changes that require a manual call-site audit. The
full migration steps with before/after examples are in the
[v0.4.0 migration guide](https://github.com/hew-lang/hew/blob/main/docs/migrations/v0.4.0.md).

- **#1218 stdlib `int` surface** — All public `std/**/*.hew` function
  signatures now use `int` instead of `i32` or `i64`. Affects 22 modules
  including `channel`, `encoding/*`, `fs`, `net/*`, `path`, `semaphore`,
  `text/semver`, `time/cron`. **Migration:** replace `i32`/`i64` in binding
  declarations and call-site argument positions with `int`; ABI seams
  (`extern "C"` blocks) are exempt. Compile-time error if not migrated.

- **#1471 `compress` decompression requires `max_output_len`** —
  `gzip_decompress`, `deflate_decompress`, and `zlib_decompress` each gained a
  required second argument `max_output_len: int`. The function fails closed when
  decompressed output would exceed the limit (prevents decompression-bomb OOM).
  **Migration:** add `max_output_len` at every call site; use
  `64 * 1024 * 1024` if no tighter bound applies. Compile-time error if not
  migrated.

- **#1314 explicit `http.Server` / `regex.Pattern` teardown** — Both types no
  longer auto-release on scope exit. The `impl Drop` was removed because it
  double-freed when callers also called `close()`/`free()` explicitly.
  **Migration:** add `server.close()` before every `http.Server` binding goes
  out of scope; add `pat.free()` before every `regex.Pattern` binding goes out
  of scope. Silent resource leak if not migrated.

- **#1500 explicit `http.Request` / `json.Value` teardown** — Same pattern as
  #1314. Both types no longer auto-release. **Migration:** add `req.free()`
  after handling each request; add `val.free()` (and sub-value `free()`) for
  every `json.Value` produced by `json.parse` or `val.get_field(...)`. Silent
  resource leak if not migrated.

- **#1030 `http_client` string helpers return `Option<String>`** —
  `get_string`, `post_string`, and `request_string` changed return type from
  `String` to `Option<String>`. `None` indicates transport failure.
  **Migration:** pattern-match on the result at each call site. Compile-time
  error if not migrated.

- **#1506 WASM empty-result encoding** — Optional scalar LSP exports
  (`hover`, `goto_definition`, `find_references`, `prepare_rename`,
  `signature_help`) now return `"null"` instead of `""` for the no-result case.
  Collection exports (`rename`, `inlay_hints`) are unchanged at `"[]"`.
  **Migration:** update empty-result guards from `result === ""` to
  `result === "null"` in JS/TS integrations. Silent behaviour change if not
  migrated (`JSON.parse("")` would have thrown; now `JSON.parse("null")` →
  `null` succeeds but bypasses the guard).

## What's new

### Compiler and runtime

- **Trait method dispatch on primitives** (#1596) — Trait methods can now be
  called on primitive and builtin-generic receivers (`int`, `String`, `Vec<T>`
  etc.), enabling user-defined extension traits over stdlib types.
- **Per-call network deadlines** (#1557) — DNS lookup, TCP connect, QUIC
  connections, and WebSocket I/O now accept an optional deadline argument.
  Calls that exceed the deadline fail closed rather than blocking indefinitely.
- **Structured parse diagnostics** (#1583, #1592) — `ParseDiagnosticKind`
  discriminant is now available in WASM exports and LSP code-action routing,
  giving tooling consumers a machine-readable parse error classification.

### Developer experience (LSP)

- **Document formatting** (#1614) — The language server now implements
  `textDocument/formatting`, delegating to `hew_parser::fmt`. Format-on-save
  works in editors without a separate `hew fmt` shell invocation.
- **Stdlib navigation** (#1616) — Goto-definition and find-references now
  resolve symbols from the standard library across file boundaries, including
  cross-file references into stdlib modules.
- **Transitive goto-definition** (#1073) — Goto-definition follows one import
  hop with a cycle guard, so it works through re-export and glob chains.

### Stdlib and examples

- **HTTP + JSON demo** (#1618) — `examples/http_json_demo.hew` shows a
  complete HTTP server that accepts JSON request bodies, parses them with
  `json.parse`, and responds with JSON — demonstrating the explicit-teardown
  pattern required by v0.4.0.
- **URL percent-encoding** (#1077) — `url.encode`, `url.decode`, and
  `url.encode_query` are confirmed end-to-end on native and WASM.
- **Module search-path docs** (#1074) — `HEWPATH`, `HEW_STD`, the four-step
  resolution order, and the `hew.toml` non-role are documented.

### Build and release

- **linux-aarch64 pre-release gate** (#1608) — A linux-aarch64 pre-release CI
  lane now validates ARM Linux builds before tagging.
- **Static `libcxx` linkage** (#1607) — The release binary no longer has a
  dynamic `libc++` dependency on Linux; a static-init pattern in the regex
  module was reworked to remove the MLIR-path dependency.

## Bug fixes

- **JIT inprocess SIGSEGV** (#1613) — The inprocess JIT mode (`hew run --jit`,
  `hew eval --jit`) no longer crashes on programs that use the search-generator
  pattern. Root causes: missing runtime-symbol export and a use-after-free in
  the search-generator lambda capture.
- **Cross-module enum variant construction** (#1605) — Enum variants defined in
  one module can now be constructed with a payload from another module without a
  parser error.
- **Type checker `Ok(())` pattern** (#1617) — `Ok(())` is now accepted as a
  valid unit-payload variant pattern in match arms.
- **Formatter inline comments** (#1535) — `hew fmt` preserves inline comments
  inside enum bodies, struct field lists, match arm bodies, and around `else if`
  branch headers.
- **REPL piped-mode flush** (#1553) — The REPL flushes stdout after each
  submission in piped mode, and `--jit` is forwarded correctly to interactive
  mode.
- **`fs.try_read_bytes` binary-safety** (#1076) — Non-UTF-8 and NUL-containing
  binary files round-trip correctly through `try_read_bytes`.

## Documentation

- HEW-SPEC.md re-audited for v0.4.0 surface deltas (#1588).
- v0.4.0 migration guide at
  [`docs/migrations/v0.4.0.md`](https://github.com/hew-lang/hew/blob/main/docs/migrations/v0.4.0.md)
  with full before/after examples for all six breaking changes (#1590).
- Compiler-stack audit and admission finalization tests committed (#1591).
- `make publish-docs` wires stdlib doc output to Cloudflare Pages (#1555).

## Install

**One-line installer (macOS / Linux):**

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

If upgrading from v0.3.x, run through the
[v0.4.0 migration guide](https://github.com/hew-lang/hew/blob/main/docs/migrations/v0.4.0.md)
before compiling existing projects.

## Acknowledgments

Thank you to everyone who filed issues and tested pre-release builds.

## Full changelog

See [`CHANGELOG.md` — \[0.4.0\]](https://github.com/hew-lang/hew/blob/main/CHANGELOG.md#040---2026-05-03)
or compare:
[v0.3.0...v0.4.0](https://github.com/hew-lang/hew/compare/v0.3.0...v0.4.0)
