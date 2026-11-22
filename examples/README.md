# Hew Examples

From the Hew repository root, run the first lesson.

```sh
hew run examples/progressive/01_hello.hew
```

Follow the [12 numbered lessons](progressive/) in order, or pick a program by what you want to learn.

| Try this                                                             | What to look for                                                       |
| -------------------------------------------------------------------- | ---------------------------------------------------------------------- |
| [Track parcels with an actor](progressive/09_simple_actor.hew)       | The handler replies with the total it just updated.                    |
| [Find a value in a sorted list](algos/binary_search.hew)             | Each comparison halves the search, and a missing value returns `None`. |
| [Run a circuit breaker](services/circuit_breaker.hew)                | A machine decides when a supervised service can try a request again.   |
| [Group source files into a package](directory_module_demo/README.md) | A directory module puts related declarations together.                 |

To build a standalone binary, run `hew build examples/fibonacci.hew -o fibonacci`, then `./fibonacci`. Inside an application created with `hew init`, use `hew run` or `hew build` without a filename.

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
   `import mod.{A, B}` from a shared module namespace.
4. [`multifile/README.md`](multifile/README.md) / `03_text_stats/` — nested
   modules where `import parent;` and `import parent.child;` are distinct.

Each of these directories carries a small `hew.toml`: a directory module
exists only inside a package, and outside one every file is a module of its
own. Start a multi-file program with `hew init`.

In an application package, set `[package] main` when the entry is not
`main.hew`, then use bare `hew check`, `hew build`, and `hew run`; imports
resolve the rest. The explicit `main.hew` paths in these fixture collections
remain useful for selecting among several examples in one directory.

[`process_capture.hew`](process_capture.hew) runs a worker with a runtime limit
and a shared stdout/stderr byte budget, preserving its exit status and handling
structured capture failures.

[`lazy_values.hew`](lazy_values.hew) initializes configuration on first demand
and shows that later reads return independent values.

[`concurrent_pipeline.hew`](concurrent_pipeline.hew) composes bounded prefetch
and parallel mapping over a generator, preserving input order.

## Directory Guide

### Learning Paths

- **progressive/** -- Numbered lessons (01–12) introducing core language features with expected output files
- **ux/** -- Quick-start lessons (01-15) covering arithmetic, actors, enums, vectors, and more; all lessons have expected output files and run as automated tests
- **directory_module_demo/** -- Directory-form module merging: two peer files compose into one `greeting` module ([README](directory_module_demo/README.md))
- **native_c/** -- A package whose `extern "C"` functions are a C source with a header, a define and a per-OS system library, declared in `hew.toml`'s `[native]` section and linked with no `--link-lib` ([manifest reference](../docs/package-manifest.md#native))
- **module_generic_boundaries/** -- Small two-file demo showing generic trait APIs crossing a module boundary (`main.hew` + `src/widgets.hew`)
- **multifile/** -- Progressive multi-file examples showing peer-file type contribution, selective imports, and two-level module hierarchies ([README](multifile/README.md)):
  - `01_shapes/` — directory module with peer-file types + trait impls; bare import + qualified access
  - `02_geometry/` — peer files sharing a type across functions; selective `import mod.{A, B}`
  - `03_text_stats/` — two-level hierarchy; `import parent;` vs `import parent.child;`

### From Examples to Stdlib

The learning paths here are mostly language-focused. When you want shipped library APIs, use [`../std/README.md`](../std/README.md) as the canonical index, then use this map to jump to the right modules quickly:

| After this part of `examples/`                                                                                       | Start with these modules                                                            | Why                                                                        |
| -------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------- | -------------------------------------------------------------------------- |
| `ux/` and `progressive/`                                                                                             | `std.string`, `std.fmt`, `std.vec`, `std.option`, `std.result`, `std.math`          | Next stop after the core syntax, collections, and expression lessons       |
| Root-level utilities such as `file_reader`, `cli_argparse`, `hew_grep`, and `regex_demo`                             | `std.cli`, `std.io`, `std.fs`, `std.path`, `std.os`, `std.string`, `std.text.regex` | CLI I/O, files, paths, env access, and text scanning                       |
| Root-level networking examples such as `http_server`, `static_server`, `curl_client`, `http_json_demo`, and `chat_*` | `std.net`, `std.net.http`, `std.net.mime`, `std.net.url`, `std.encoding.json`       | TCP, HTTP, content types, URLs, and JSON client payloads                   |
| `smtp_client.hew` (requires a real SMTP server; see file header)                                                     | `std.net.smtp`                                                                      | Connecting via STARTTLS or implicit TLS, sending plain-text and HTML email |
| Root-level async/concurrency examples such as `async_demo` and `scope_*`                                             | `std.stream`, `std.semaphore`                                                       | Pipe and stream pipelines, and coordination primitives                     |
| `benchmark_demo.hew` and `benchmarks/`                                                                               | `std.bench`, `std.net.http`                                                         | Benchmark harness plus the HTTP surfaces used in the server comparison     |

### Topic Collections

- [Algorithms](algos/) and [data structures](datastruct/) include runnable implementations with boundary cases. Each program has a sibling `.expected` file showing its output.
- [Playground](playground/) holds the [catalogue](playground/manifest.json) behind the browser examples. Its entries identify the available execution targets.
- [Sandbox execution cases](sandbox-graduation/) and [historical examples](v05/surfaces/) preserve particular behaviours. Their names sometimes describe an earlier implementation.
- [Networking](net/) includes a [local HTTP service](net/http_await_service.hew) and a [TLS client](net/tls_client.hew) that requires internet access.

### Cross-Language Comparisons

- **benchmarks/** -- Cross-language benchmark fixtures: paired `bench_*` implementations under `hew/`, `go/`, and `rust/`, plus the HTTP server comparison files at the directory root ([README](benchmarks/README.md))
- **comparison/** -- Counter service implemented side-by-side in Hew, Go, and Rust ([README](comparison/README.md))

### Service and Distributed Patterns

- **services/** -- Distributed service patterns that showcase Hew's actor model ([README](services/README.md)):
  - Circuit breaker, rate limiter, worker pool, pub/sub broker, health monitor, distributed counter
- **quic_mesh/** -- Two-node QUIC/TLS demo that exports a `Counter` service over an authenticated peer session; run each side once to create keys, exchange credentials, then pass the peer credential on launch
- **quic_service/** -- Minimal QUIC client/server round-trip using only the public `std.net.quic` API
- **Root-level network/distributed demos** -- `distributed_hello`, `sensor_mesh`, `http_server`, `static_server`, `mqtt_broker`, `chat_*`, `curl_client`, `actor_net_reader`, `network_file_reader`, and `http_json_demo` (HTTP fetch + JSON parse pipeline; requires internet access)

### Root-Level Examples

The `examples/` root also contains many focused single-file demos. The
current clusters are easiest to scan by filename:

- **Actors / concurrency** -- `actor_*`, `lambda_*`, `async_*`, `scope_*`, `fibonacci_actors`, `concurrent_counter`, `struct_spawn_test`, `backpressure_test`
- **Supervision / runtime stress** -- `supervisor_*` and `stress_*`
- **Language / type-system demos** -- `enum_test`, `enums_and_options`, `types_and_traits`, `type_inference`, `if_let`, `custom_indexing`, `method_*`, `string_*`, `result_option_test`, `vec_hashmap_test`
- **Regression-oriented samples** -- `test_*`, `comprehensive_test*`, `closures_test`, `syntax_test`, `full_validation`, `test_llvm_e2e`
- **Tooling / observability / showcases** -- `hew_grep`, `regex_demo`, `file_reader`, `cli_argparse`, `benchmark_demo`, `profiler_demo`, `observe_showcase`, `selfhost_lexer_v2`, `showcase`, `simple`, `demo_four_pillars`
