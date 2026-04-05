# Hew Examples

A collection of examples demonstrating the Hew programming language.

## Running Examples

```sh
hew run examples/fibonacci.hew
```

Or compile and run separately:

```sh
hew build examples/fibonacci.hew -o fibonacci
./fibonacci
```

If an example fails to import, typecheck, or link, start with
[`../docs/troubleshooting.md`](../docs/troubleshooting.md).

## Multi-file / module roadmap

If you're trying to move from single-file snippets to a real module tree, use
this progression:

1. [`directory_module_demo/`](directory_module_demo/README.md) — the smallest
   directory-form module; `import greeting;` loads `greeting/greeting.hew` plus
   its peer file automatically.
2. [`multifile/README.md`](multifile/README.md) / `01_shapes/` — same
   directory-module pattern, but with peer files contributing types and trait
   impls.
3. [`multifile/README.md`](multifile/README.md) / `02_geometry/` — selective
   `import mod::{A, B}` from a shared module namespace.
4. [`multifile/README.md`](multifile/README.md) / `03_text_stats/` — nested
   modules where `import parent;` and `import parent::child;` are distinct.

Whichever layout you use, point `hew check`, `hew build`, and `hew run` at the
example's `main.hew` entry file; imports resolve the rest.

## Expected Output Files

Many examples — especially those under `ux/` and `progressive/` — ship with a sibling `.expected` file containing the exact stdout the program should produce. These files are used by the automated test suite, and you can use them locally to verify your build is correct:

```sh
# Run an example and diff its output against the expected file
hew run examples/ux/01_hello.hew | diff examples/ux/01_hello.expected -

# Run all ux examples and check output
for f in examples/ux/*.hew; do
    expected="${f%.hew}.expected"
    hew run "$f" | diff "$expected" - && echo "OK: $f" || echo "FAIL: $f"
done
```

If the diff is empty the output matches exactly. A non-empty diff means the program produced unexpected output or the binary is out of date.

## Directory Guide

### Learning Paths

- **progressive/** -- Numbered lessons (01-11) introducing core language features with expected output files
- **ux/** -- Quick-start lessons (01-15) covering arithmetic, actors, enums, vectors, and more; all lessons have expected output files and run as automated tests
- **directory_module_demo/** -- Quick proof that directory-form module merging works: two peer files compose into one `greeting` module ([README](directory_module_demo/README.md))
- **module_generic_boundaries/** -- Small two-file demo showing generic trait APIs crossing a module boundary (`main.hew` + `src/widgets.hew`)
- **multifile/** -- Progressive multi-file examples showing peer-file type contribution, selective imports, and two-level module hierarchies ([README](multifile/README.md)):
  - `01_shapes/` — directory module with peer-file types + trait impls; bare import + qualified access
  - `02_geometry/` — peer files sharing a type across functions; selective `import mod::{A, B}`
  - `03_text_stats/` — two-level hierarchy; `import parent;` vs `import parent::child;`

### From Examples to Stdlib

The learning paths here are mostly language-focused. When you want shipped library APIs, use [`../std/README.md`](../std/README.md) as the canonical index, then use this map to jump to the right modules quickly:

| After this part of `examples/` | Start with these modules | Why |
| --- | --- | --- |
| `ux/` and `progressive/` | `std::string`, `std::fmt`, `std::vec`, `std::option`, `std::result`, `std::math` | Next stop after the core syntax, collections, and expression lessons |
| Root-level utilities such as `file_reader`, `cli_argparse`, `hew_grep`, and `regex_demo` | `std::io`, `std::fs`, `std::path`, `std::os`, `std::string`, `std::text::regex` | CLI I/O, files, paths, env access, and text scanning |
| Root-level networking examples such as `http_server`, `static_server`, `curl_client`, and `chat_*` | `std::net`, `std::net::http`, `std::net::mime`, `std::net::url` | TCP, HTTP, content types, and URLs |
| Root-level async/concurrency examples such as `async_demo` and `scope_*` | `std::stream`, `std::channel::channel`, `std::semaphore` | Stream pipelines, MPSC channels, and coordination primitives |
| `benchmark_demo.hew` and `benchmarks/` | `std::bench`, `std::net::http` | Benchmark harness plus the HTTP surfaces used in the server comparison |

### Topic Collections

- **algos/** -- One-file algorithm examples covering search, sorting, graph traversal, dynamic programming, and string processing
- **datastruct/** -- One-file data-structure examples covering trees, heaps, maps, caches, graphs, and related utilities
- **playground/** -- Grouped by topic; [`manifest.json`](playground/manifest.json) is the curated source of truth for the downstream browser playground catalog. After editing files under `playground/`, refresh it with `make playground-manifest` (or `python3 scripts/gen-playground-manifest.py`), use `make playground-manifest-check` for the cheap freshness check, and use `make playground-check` for the repo-local preflight that also builds `hew-wasm`:
  - `basics/` -- Hello world, fibonacci, higher-order functions, string interpolation
  - `concurrency/` -- Actor pipelines, async/await, counters, supervisors
  - `types/` -- Collections, pattern matching, wire types

### Cross-Language Comparisons

- **benchmarks/** -- Cross-language benchmark fixtures: paired `bench_*` implementations under `hew/`, `go/`, and `rust/`, plus the HTTP server comparison files at the directory root ([README](benchmarks/README.md))
- **comparison/** -- Counter service implemented side-by-side in Hew, Go, and Rust ([README](comparison/README.md))

### Service and Distributed Patterns

- **services/** -- Distributed service patterns that showcase Hew's actor model ([README](services/README.md)):
  - Circuit breaker, rate limiter, worker pool, pub/sub broker, health monitor, distributed counter
- **quic_mesh/** -- Two-node QUIC/TLS mesh demo with separate `server.hew` and `client.hew`
- **Root-level network/distributed demos** -- `distributed_hello`, `sensor_mesh`, `http_server`, `static_server`, `mqtt_broker`, `chat_*`, `curl_client`, `actor_net_reader`, and `network_file_reader`

### Root-Level Examples

The `examples/` root also contains many focused single-file demos. The
current clusters are easiest to scan by filename:

- **Actors / concurrency** -- `actor_*`, `lambda_*`, `async_*`, `scope_*`, `fibonacci_actors`, `concurrent_counter`, `struct_spawn_test`, `backpressure_test`
- **Supervision / runtime stress** -- `supervisor_*` and `stress_*`
- **Language / type-system demos** -- `enum_test`, `enums_and_options`, `types_and_traits`, `type_inference`, `if_let`, `custom_indexing`, `method_*`, `string_*`, `result_option_test`, `vec_hashmap_test`
- **Regression-oriented samples** -- `test_*`, `comprehensive_test*`, `closures_test`, `syntax_test`, `full_validation`, `test_llvm_e2e`
- **Tooling / observability / showcases** -- `hew_grep`, `regex_demo`, `file_reader`, `cli_argparse`, `benchmark_demo`, `profiler_demo`, `observe_showcase`, `selfhost_lexer_v2`, `showcase`, `simple`, `demo_four_pillars`
