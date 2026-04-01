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

## Directory Guide

### Learning Paths

- **progressive/** -- Numbered lessons (01-11) introducing core language features with expected output files
- **ux/** -- Quick-start lessons (01-15) covering arithmetic, actors, enums, vectors, and more; all lessons have expected output files and run as automated tests

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
| Types         | `enum_test`, `enums_and_options`, `types_and_traits`, `type_inference`    |
| Strings       | `string_escapes`, `string_ops_test`, `string_test`                        |
| Utilities     | `hew_grep`, `regex_demo`, `file_reader`, `cli_argparse`                   |
