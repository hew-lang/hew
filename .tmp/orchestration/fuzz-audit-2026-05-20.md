# Hew v0.5 fuzz audit - 2026-05-20

## Target inventory before revision

| Target | Harness body | Input shape | Compiler layer |
| --- | --- | --- | --- |
| `fuzz_lex` | UTF-8 gate, `hew_lexer::lex`, span monotonicity/bounds assertions | Raw bytes interpreted as UTF-8 source text | Lexer |
| `fuzz_parse` | UTF-8 gate, `hew_parser::parser::parse` | Raw bytes interpreted as UTF-8 source text | Parser |
| `fuzz_machine` | UTF-8 gate, parses raw input and raw input wrapped in `machine Fuzz { ... } fn main() {}` | Raw bytes interpreted as top-level source and machine body fragment | Parser machine grammar |
| `fuzz_check` | Parse; skip parse errors; `Checker::check_program` | Raw UTF-8 Hew source | Parser + type checker |
| `fuzz_mir` | Parse; skip parse/type/HIR diagnostics; lower HIR to MIR | Raw UTF-8 Hew source | Parser + type checker + HIR + MIR lowering |
| `fuzz_structured` | `arbitrary` program generator emits a synthesized module and parses it | Structured `HewProgram` with imports/items/flavour | Parser, broad surface smoke |

Surveyed fuzz scaffolds: only `hew-parser/fuzz` is present at current tip. No `hew-types/fuzz`, `hew-hir/fuzz`, `hew-mir/fuzz`, or `hew-codegen-rs/fuzz` packages were found.

## Stale-surface flags

Closed in this revision:

| Target | Stale surface | Action |
| --- | --- | --- |
| `fuzz_structured` | `wire type FuzzWire { field: ty = tag; }` | Replaced with `#[wire] struct ... { field: ty @tag }`. |
| `fuzz_structured` | `gen fn ... { yield ... }` | Replaced with `gen { yield ...; tail }` blocks inside normal functions. |
| `fuzz_structured` | Sparse actor-lambda coverage | Added separate v0.5 lambda actor target using `actor |param: T| { ... }` and call/`.send` dispatch. |

Observed stale examples/fixtures to avoid using as seeds:

- Removed spawn-lambda syntax: `spawn |msg: T| { ... }` appears in `hew-lsp/tests/fixtures/v05_spawn_lambda_actor.hew` and is explicitly rejected by `tests/vertical-slice/reject/spawn_lambda_removed.hew`.
- Removed arrow-send syntax: `<-` is explicitly rejected by `tests/vertical-slice/reject/lambda_arrow_operator.hew`.
- Removed structured-concurrency API: `scope |s| { ... }` is rejected in parser diagnostics; use `scope { fork name = call(); }`.
- Stale scalar spelling: `Int` appears in `examples/machine/tcp_handshake.hew`; v0.5 examples and fuzzers should use fixed-width forms such as `i64`.

No stale corpus existed under `hew-parser/fuzz/corpus` before this revision; the directory was absent/empty.

## Missing v0.5 coverage before revision

- Machine declarations with payload-bearing states/events, tagged-union state layouts, `m.step(ev)`, and `m.state_name()`.
- `gen { yield X; Y }` blocks and explicit-return generator blocks.
- Lambda actors with `actor |param: T| { body }` / `actor |param: T| -> R { body }`, `Duplex<S,R>`, `Sink<T>`, `Stream<T>`, `LocalPid<T>`, and `RemotePid<T>` typed forms.
- Structured concurrency `scope { fork name = call(); }`.
- Supervisor declarations and child accessor calls.
- `#[wire] struct` and `wire enum` v0.5 wire declarations.
- Named records, tuple records, functional update.
- Associated type projections and trait-object syntax.
- Qualified enum constructors such as `fs.IoError::TimedOut(42)`.
- `is` operator, `#[max_heap(N)]`, `unsafe { ... }`.
- String, float, char, unit, and duration literal mixtures.

## Dictionary/corpus refresh notes

The refreshed seeds intentionally use v0.5 keywords and spellings: `machine`, `state`, `event`, `on`, `@reenter`, `gen`, `yield`, `actor |...|`, `Duplex`, `Sink`, `Stream`, `LocalPid`, `RemotePid`, `scope`, `fork`, `supervisor`, `strategy`, `max_restarts`, `window`, `#[wire]`, `wire enum`, `#[max_heap]`, `unsafe`, and fixed-width integer spellings (`i32`, `i64`, `u8`) instead of stale `int`/`Int`.
