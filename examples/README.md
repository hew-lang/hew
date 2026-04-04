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

## Expected Output Files

Many examples — especially those under `ux/` and `progressive/` — ship with a sibling `.expected` file containing the exact stdout the program should produce. These files are used by the automated test suite, and you can use them locally to verify your build is correct:

```sh
# Run an example and diff its output against the expected file
diff <(hew run examples/ux/01_hello.hew) examples/ux/01_hello.expected

# Run all ux examples and check output
for f in examples/ux/*.hew; do
    expected="${f%.hew}.expected"
    diff <(hew run "$f") "$expected" && echo "OK: $f" || echo "FAIL: $f"
done
```

If the diff is empty the output matches exactly. A non-empty diff means the program produced unexpected output or the binary is out of date.

## Directory Guide

### Learning Paths

- **progressive/** -- Numbered lessons (01-11) introducing core language features with expected output files
- **ux/** -- Quick-start lessons (01-15) covering arithmetic, actors, enums, vectors, and more; all lessons have expected output files and run as automated tests
- **directory_module_demo/** -- Quick proof that directory-form module merging works: two peer files compose into one `greeting` module ([README](directory_module_demo/README.md))
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

- **playground/** -- Grouped by topic:
  - `basics/` -- Hello world, fibonacci, higher-order functions, string interpolation
  - `concurrency/` -- Actor pipelines, async/await, counters, supervisors
  - `types/` -- Collections, pattern matching, wire types

### Cross-Language Comparisons

- **benchmarks/** -- HTTP server implementations in Hew, Rust, Go, and Python
- **comparison/** -- Counter service implemented side-by-side in Hew, Go, and Rust

### Service Patterns

- **services/** -- Distributed service patterns that showcase Hew's actor model:
  - Circuit breaker, rate limiter, worker pool, pub/sub broker, health monitor, distributed counter

### Standalone Examples

| Category      | Examples                                                                  |
| ------------- | ------------------------------------------------------------------------- |
| Actors        | `actor_fib`, `fibonacci_actors`, `lambda_actor*`, `concurrent_counter`    |
| Supervisors   | `supervisor_*` (6 examples covering crash budgets, nesting, worker pools) |
| Networking    | `http_server`, `mqtt_broker`, `chat_server`, `chat_client`, `curl_client` |
| Async/Streams | `async_demo`, `scope_demo`, `scope_minimal`                               |
| Stress Tests  | `stress_*` (8 examples for actors, mailboxes, scheduling, supervision)    |
| Self-Hosting  | `selfhost_lexer_v2`                                                       |
| Types         | `enum_test`, `enums_and_options`, `types_and_traits` (structs · traits · impl blocks · dyn dispatch), `type_inference` |
| Strings       | `string_escapes`, `string_ops_test`, `string_test`                        |
| Utilities     | `hew_grep`, `regex_demo`, `file_reader`, `cli_argparse`                   |
