# Hew v0.5 fuzz revise + run report - 2026-05-20

## 1. Target inventory

Audit details are in `.tmp/orchestration/fuzz-audit-2026-05-20.md`.

| Target | Before | After | Change |
| --- | --- | --- | --- |
| `fuzz_lex` | Existing | Existing | No source change; lexer span invariant target remains valid. |
| `fuzz_parse` | Existing | Existing | No source change; raw UTF-8 parser target remains valid. |
| `fuzz_machine` | Existing | Existing | No source change; parser-only machine-body wrapper remains valid. |
| `fuzz_check` | Existing | Existing | No source change; parser + checker raw-source target remains valid. |
| `fuzz_mir` | Existing | Existing | No source change; parser + checker + HIR + MIR raw-source target remains valid. |
| `fuzz_structured` | Existing structured parser generator | Updated | Dropped stale `wire type` and `gen fn` surfaces; added raw-source seed ingestion while keeping arbitrary structured generation. |
| `fuzz_machine_decls` | Missing | New | Structured + raw-source target for machine declarations, payload states, `m.step(ev)`, `m.state_name()`, HIR/MIR lowering. |
| `fuzz_gen_blocks` | Missing | New | Structured + raw-source target for `gen { yield ...; tail }` and return-only gen blocks through check/HIR/MIR. |
| `fuzz_record_types` | Missing | New | Structured + raw-source target for named records, tuple records, and functional update through check/HIR/MIR. |
| `fuzz_actor_decls` | Missing | New | Structured + raw-source target for `#[max_heap]` actor declarations, lambda actors, call/`.send`, and channel/Pid typed surfaces through check/HIR/MIR. |
| `fuzz_supervisor_decls` | Missing | New | Structured + raw-source target for supervisor declarations, child accessors, spawn, and `scope { fork ... }` through check/HIR/MIR. |
| `fuzz_qualified_ctor_resolution` | Missing | New | Structured + raw-source target for module-qualified enum constructors such as `fs.IoError::TimedOut(42)` through check/HIR/MIR. |

Only `hew-parser/fuzz` exists as a cargo-fuzz package at this tip; no `hew-types/fuzz`, `hew-hir/fuzz`, `hew-mir/fuzz`, or `hew-codegen-rs/fuzz` packages were present.

## 2. Stale-surface flags closed

| Surface | Where found | Resolution |
| --- | --- | --- |
| `wire type Name { field: T = tag; }` | `fuzz_structured` | Replaced with `#[wire] struct Name { field: T @tag }` seeds/generation. |
| `gen fn name() -> T { yield ... }` | `fuzz_structured` | Replaced with `let _g = gen { yield ...; tail };`. |
| Raw-source-only seed blind spot in structured fuzzing | `fuzz_structured` and new targets | Every structured target now also parses/checks/lower raw corpus bytes before interpreting the same bytes as arbitrary data. |
| Stale corpus/dictionary | No previous `hew-parser/fuzz/corpus` seeds existed | Added v0.5 seeds under each revised/new target corpus. |

Known stale repo fixtures were not used as seeds: removed `spawn |...|` lambda actor syntax, `<-` send syntax, `scope |s|` API, and stale `Int` spelling.

## 3. New v0.5 surface coverage

| Target | Surface now exercised |
| --- | --- |
| `fuzz_machine_decls` | `machine`, payload `state`, `event`, `on`, `@reenter`, `self.field`, tagged-union state layout shapes, `m.step(ev)`, `m.state_name()`. |
| `fuzz_gen_blocks` | `gen { yield X; Y }`, multi-yield gen blocks, explicit-return gen blocks, no-yield tail gen blocks. |
| `fuzz_record_types` | `type Name { ... }`, tuple records `type Pair(T, U);`, named construction, field access, functional update `{ field: value, ..base }`. |
| `fuzz_actor_decls` | `#[max_heap(N)]`, actor declarations, `actor |param: T| { ... }`, `actor |param: T| -> R { ... }`, call syntax, `.send`, `Duplex<S,R>`, `Sink<T>`, `Stream<T>`, `LocalPid<T>`, `RemotePid<T>`. |
| `fuzz_supervisor_decls` | `supervisor`, `strategy`, `max_restarts`, `window`, child declarations, `spawn Supervisor`, child accessor, `scope { fork name = call(); }`, bare `fork call();`. |
| `fuzz_qualified_ctor_resolution` | `import std::fs`, module-qualified enum construction, qualified constructor patterns. |
| `fuzz_structured` | Refreshed broad parser generation for v0.5 records, actors, gen blocks, `#[wire] struct`, `is`, `unsafe`, string/regex/bytes literals, intrinsics. |

Remaining gap: the requested tagged-union codegen GEP/alignment shape is indirectly covered through MIR machine layout production only. There is no `hew-codegen-rs/fuzz` package and the public codegen surface emits files through `emit_module`; a follow-up target in `hew-codegen-rs/fuzz` would be the right place to repeatedly call LLVM textual emission for machine layouts.

## 4. Fuzz-run findings

Timed run command shape:

```sh
RUST_BACKTRACE=1 cargo +nightly fuzz run <target> -- -max_total_time=300 -print_final_stats=1
```

Nightly was available (`nightly-aarch64-apple-darwin`). Total timed fuzzing runtime was 2,107 seconds (~35m07s), excluding build/smoke time. No crash artifacts were produced under `hew-parser/fuzz/artifacts`.

| Target | Runtime | Executed units | Avg exec/s | New units | Crash count |
| --- | ---: | ---: | ---: | ---: | ---: |
| `fuzz_structured` | 301s | 1,385,077 | 4,601 | 19,105 | 0 |
| `fuzz_machine_decls` | 301s | 152,845 | 507 | 4,286 | 0 |
| `fuzz_gen_blocks` | 301s | 160,420 | 532 | 3,675 | 0 |
| `fuzz_record_types` | 301s | 3,240,193 | 10,764 | 35,868 | 0 |
| `fuzz_actor_decls` | 301s | 140,802 | 467 | 4,191 | 0 |
| `fuzz_supervisor_decls` | 301s | 2,633,595 | 8,749 | 33,207 | 0 |
| `fuzz_qualified_ctor_resolution` | 301s | 91,500 | 303 | 3,098 | 0 |

Aggregate executed units: 7,804,432. Aggregate new corpus units: 103,430.

No `cargo fuzz tmin` runs were needed because no crashes/panics were found. Minimized input and diagnostic columns are therefore N/A.

## 5. Real compiler bugs surfaced

None in the 5-minute-per-target run. The runs surfaced no compiler panics, sanitizer crashes, malformed-IR panics, or harness crashes.

## 6. Recommendations

1. No crash follow-up dispatches are needed from this run.
2. Add a dedicated `hew-codegen-rs/fuzz` package for machine-layout/codegen verification so tagged-union payload padding and GEP paths are fuzzed at the LLVM textual/module verification boundary rather than only via MIR layout production.
3. Keep the new raw-source + structured-input pattern for future targets; it makes checked-in `.hew` seeds meaningful while preserving arbitrary generation.
4. Promote useful generated corpus units after longer overnight runs, especially for slower HIR/MIR targets (`fuzz_actor_decls`, `fuzz_machine_decls`, `fuzz_qualified_ctor_resolution`).

## 7. Branch + commit list

Branch: `feat/fuzz-revise-v05`

| Commit | Purpose |
| --- | --- |
| `d489d9ef` | Adds v0.5 fuzz targets, refreshes stale structured syntax, adds seed corpora, and records the audit. |

