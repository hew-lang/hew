# hew-cli

The Hew programming language compiler driver.

## Usage

```sh
hew build file.hew [-o output]    # Compile to executable
hew run file.hew [-- args...]     # Compile and run
hew run file.hew --profile        # Compile, run, and enable the built-in profiler
hew debug file.hew [-- args...]   # Build with debug info + launch gdb/lldb
hew check file.hew                # Parse + typecheck only
hew watch file.hew                # Watch for changes and re-check continuously
hew watch --run file.hew          # Watch and re-run on successful check
hew doc file.hew                  # Generate documentation
hew eval "expr"                   # Evaluate an expression
hew eval -f file.hew              # Evaluate a file in REPL context
hew eval --json "expr"            # Evaluate and emit a machine-readable JSON run contract
hew test file.hew                 # Run tests
hew wire check file.hew --against baseline.hew
                                  # Validate wire compatibility
hew fmt file.hew                  # Format source file in-place
hew fmt --stdin < file.hew       # Format source from stdin to stdout
hew fmt --check file.hew         # Check formatting (CI mode)
hew init [name]                   # Scaffold main.hew + README.md in new dir (no hew.toml)
hew init                          # Scaffold main.hew + README.md in current dir
hew init [name] --force           # Overwrite existing scaffold files
hew completions <shell>           # Generate shell completions
hew version                       # Print version info
```

`hew file.hew` is shorthand for `hew build file.hew`.

### WASI runner prototype

`hew run --target wasm32-wasi file.hew` now uses the existing WASI build path
to produce a `.wasm` module, then runs it with `wasmtime`. Compile failures
still exit through the compile path (including the existing WASM unsupported
diagnostics for supervision trees and similar features), while runtime failures
come from the `wasmtime` execution step. This prototype requires `wasmtime`
plus a `hew-runtime` build for `wasm32-wasip1` (for example via `make
wasm-runtime`).

## Formatting

`hew fmt` supports four common workflows:

```sh
hew fmt myapp/main.hew                        # Rewrite a file in-place
hew fmt --stdin < myapp/main.hew              # Read stdin, write formatted source to stdout
hew fmt --check myapp/main.hew                # Exit non-zero if a file needs formatting
hew fmt --check a.hew b.hew c.hew             # Check multiple files (batch)
hew fmt --check --stdin < myapp/main.hew      # Verify piped input without writing
```

Without flags, `hew fmt` rewrites each named file in-place and leaves
already-formatted files untouched. When a file is rewritten, `hew fmt` prints
`Formatted <file>` on stderr; already-formatted files produce no output.

Use `--stdin` for editor integrations or shell pipelines. It reads from stdin,
writes the formatted result to stdout, and cannot be combined with file
arguments.

Use `--check` when you want formatting verification without rewriting files.
For files, `hew fmt --check` prints `<file>: needs formatting` to stderr and
exits non-zero when any input needs changes, which makes it suitable for CI.
Combined with `--stdin`, it performs the same verification on piped input,
prints `<stdin>: needs formatting` on failure, and stays silent on success.

When multiple files are passed to `--check`, each file that needs formatting
gets its own `<file>: needs formatting` line on stderr. Files that are already
correctly formatted produce no output. The process exits 1 if **any** file
needs formatting (aggregate exit), and exits 0 only if all files pass. There
is no final summary count line.

For common import-resolution, type-checking, and build failures, see
[`../docs/troubleshooting.md`](../docs/troubleshooting.md).

## Eval

`hew eval` compiles and runs Hew code interactively via a compile-per-input
session model. Each evaluation builds a complete synthetic Hew program from
accumulated session state (prior top-level definitions and bindings) plus the
new input, then runs the native compilation pipeline against it. No persistent
JIT or interpreter state is maintained between evaluations — the output you
see is always the result of a fresh compile-and-run.

```sh
hew eval "1 + 2"            # Evaluate an inline expression; exits when done
hew eval -f script.hew      # Run a .hew file in REPL context
hew eval                    # Start the interactive REPL
hew eval --timeout 10       # Per-evaluation timeout of 10 seconds (default: 30)
```

### Session model

In REPL mode (interactive or piped) top-level items (`fn`, `struct`, `enum`,
`actor`, `trait`, `impl`) and bindings (`let`, `var`) accumulate across
evaluations and are re-emitted into each subsequent compile. Bare expressions
are wrapped in `println()` and auto-printed.

`--timeout <seconds>` applies **per evaluation** — it is the wall-clock limit
for a single compile-and-run, not for the whole session or file. The minimum
accepted value is 1 second.

### REPL commands

| Command | Description |
|---|---|
| `:help`, `:h` | Show available commands |
| `:quit`, `:q` | Exit the REPL |
| `:clear` | Reset the session — drops all accumulated definitions and bindings |
| `:type <expr>` | Show the inferred type of an expression without running it |
| `:load <file>` | Load a `.hew` file's top-level items into the session |

`:clear` is a hard session reset: every item and binding accumulated since
the REPL started (or since the last `:clear`) is discarded. Names that were
defined before `:clear` are no longer in scope and can be safely redefined
after it. The REPL prints `Session cleared.` to confirm the reset.

### JSON run contract (`--json`)

`hew eval --json` emits a single JSON object on stdout (and always exits 0)
suitable for downstream tooling, playground workers, and CI scripts that need
to inspect the outcome programmatically.

```sh
hew eval --json "1 + 2"          # inline expression
hew eval --json -f script.hew    # file
```

The JSON object always contains these fields:

| Field | Type | Description |
|---|---|---|
| `status` | string | `"ok"`, `"compile_error"`, or `"runtime_failure"` |
| `stdout` | string | Output the program wrote to stdout (may be empty) |
| `exit_code` | integer | Child process exit code; `0` on success or compile error |
| `diagnostics` | string | Compiler diagnostic text; non-empty only when `status == "compile_error"` |

**Examples:**

```json
{"status":"ok","stdout":"3\n","exit_code":0,"diagnostics":""}

{"status":"compile_error","stdout":"","exit_code":0,"diagnostics":"<eval>:1:1: error: unknown name ..."}

{"status":"runtime_failure","stdout":"partial output\n","exit_code":101,"diagnostics":""}
```

Key properties:
- The process **always exits 0** when `--json` is active; callers must
  inspect `status`, not the exit code.
- `stdout` is preserved even on `runtime_failure` (matches the non-JSON
  behaviour that surfaces pre-failure output).
- `diagnostics` contains the full rendered compiler diagnostic text,
  including source underlines.
- `--json` requires `-f <file>` or an inline expression; it is rejected for
  interactive REPL mode.



`hew watch` continuously monitors a `.hew` file (or directory) for changes
and re-runs type-checking automatically. It is the fastest inner-loop
workflow when iterating on types, signatures, or module structure.

```sh
hew watch myapp.hew               # Re-check on every save
hew watch --run myapp.hew         # Re-check and re-run on each successful check
hew watch --clear myapp.hew       # Clear terminal before each check
hew watch --debounce 500 myapp.hew  # Wait 500 ms after last event before re-checking
```

| Flag | Default | Description |
|---|---|---|
| _(none)_ | — | File or directory to watch (required) |
| `--run` | off | Build and run the program after each successful check |
| `--clear` | off | Clear the terminal before each re-check pass |
| `--debounce <ms>` | `300` | Milliseconds to wait after the last file-system event before re-checking; increase on slow disks or large trees |

When watching a **directory**, `hew watch` re-checks the directory's entry
file (the `.hew` file whose stem matches the directory name) whenever any
`.hew` file inside changes. When watching a **single file**, only changes
to that file trigger a re-check.

`hew watch` runs the same check pipeline as `hew check`, so diagnostics
appear in the same format. Use `--run` to also execute the program, which
is useful for quick feedback on output changes. Exit with `Ctrl-C`.

## Documentation

`hew doc` extracts doc comments from `.hew` source files and renders them as
static HTML pages or Markdown files.

```sh
hew doc mylib.hew                           # document a single file → doc/
hew doc src/                                # document all .hew files under src/
hew doc src/ --output-dir site/docs         # custom output directory
hew doc src/ --format markdown              # render Markdown instead of HTML
hew doc src/ --open                         # open index.html in the browser after generation
```

**File input** generates docs for that single module. **Directory input**
recursively collects every `.hew` file under the tree, derives fully-qualified
module names (e.g. `std::encoding::json`), and writes one page per module plus
an index.

**Output directory** defaults to `./doc` and is created automatically if it
does not exist. HTML output writes `index.html` and one `<module>.html` per
module. Markdown output writes `README.md` and one `<module>.md` per module.

**`--format html|markdown`** (default: `html`). `md` is accepted as a short
alias for `markdown`. `--open` opens `index.html` in the default browser after
HTML generation; it is a no-op when `--format markdown` is set.

**Parse-error behaviour**: if one or more input files fail to parse, their
errors are printed to stderr and those files are skipped. Documentation for
the remaining valid files is still written, and `hew doc` exits 1 after
generation completes. Fix parse errors (run `hew check <file.hew>` first) to
ensure all modules appear in the output.

For `hew doc` failure modes and troubleshooting steps, see
[`../docs/troubleshooting.md`](../docs/troubleshooting.md).

## Scaffolding a new project

`hew init` writes two files — `main.hew` and `README.md` — and nothing else.
It intentionally does **not** create `hew.toml`. For the manifest-first
bootstrap flow (including `hew.toml`, `.gitignore`, and dependency management)
use `adze init` instead.

**Create a new directory:**

```sh
hew init myapp        # creates myapp/main.hew + myapp/README.md
```

**Initialise the current directory:**

```sh
cd myapp
hew init              # writes main.hew + README.md here
```

**Overwrite existing scaffold files:**

```sh
hew init myapp --force        # overwrites main.hew and README.md if they exist
hew init       --force        # same, but in the current directory
```

`--force` only applies to the two scaffold files; it will not delete
unrelated files in the directory.

### After scaffolding

```sh
hew check main.hew    # parse and typecheck
hew run   main.hew    # compile and run
```

When you are ready to add dependencies or publish, run `adze init` in the same
directory to layer `hew.toml` on top of the existing source files.

## Multi-file projects

For `hew check`, `hew build`, `hew run`, and `hew debug`, pass a **single
entry-point file**. The compiler resolves imports recursively from that file,
so you never need to list every source file on the command line.

Given a project with this layout:

```
myapp/
├── main.hew
└── greeting/
    ├── greeting.hew
    └── greeting_helpers.hew
```

All three commands below operate on the whole project through `main.hew`:

```sh
hew check myapp/main.hew
hew run   myapp/main.hew
hew build myapp/main.hew -o myapp
```

The `greeting/` directory is a **directory-form module**: `greeting/greeting.hew`
is the entry file (its stem matches the directory name) and
`greeting/greeting_helpers.hew` is merged in automatically as a peer file.
See [§ 3.5.1 of HEW-SPEC.md](../docs/specs/HEW-SPEC.md) for the full rules.

For the current wildcard-import warning caveat, see the
[troubleshooting guide](../docs/troubleshooting.md).

## Testing

`hew test` discovers and runs test functions declared with `#[test]` in one or
more `.hew` files or directories. Each test is compiled to a native binary via
the standard `hew build` pipeline and executed in a child process for
isolation. A per-test timeout prevents hung tests from blocking the suite.

```sh
hew test tests/                          # run all tests under tests/
hew test mylib.hew                       # run tests in a single file
hew test tests/ --filter auth            # run only tests whose name contains "auth"
hew test tests/ --format junit           # emit JUnit XML to stdout (CI mode)
hew test tests/ --timeout 60            # per-test timeout in seconds (default: 30)
hew test tests/ --include-ignored        # also run #[ignore]-annotated tests
hew test tests/ --no-color               # disable coloured output
```

| Flag | Default | Description |
|---|---|---|
| `--filter <pattern>` | — | Run only tests whose name contains `pattern` |
| `--format text\|junit` | `text` | Human-readable output or JUnit XML |
| `--timeout <seconds>` | `30` | Wall-clock limit for test execution (run phase only, not compile) |
| `--include-ignored` | off | Also execute tests annotated with `#[ignore]` |
| `--no-color` | off | Suppress ANSI colour codes |

Exit code is **0** when all executed tests pass, **1** when any test fails or
times out. Discovery parse errors are reported as failures (the runner is
fail-closed on discovery errors).

## Debugging

`hew debug` compiles the program with full debug information (no optimisation,
no stripping) and immediately launches it under the system debugger (`gdb` on
Linux, `lldb` on macOS). If a Hew helper script (`hew-gdb.py` / `hew_lldb.py`)
is found next to the installed binary it is loaded automatically to improve
pretty-printing of Hew types.

```sh
hew debug myapp.hew -- arg1 arg2   # debug myapp, passing args to the program
```

## Profiling and observability

`hew run --profile` enables the built-in runtime profiler on the compiled
program. The value injected into `HEW_PPROF` is platform-dependent:

| Platform | `HEW_PPROF` value set | How to attach |
|---|---|---|
| Unix (Linux, macOS) | `auto` | `hew-observe` (auto-discovers unix socket) |
| Other (Windows, …) | `:6060` | `hew-observe --addr localhost:6060` |

If `HEW_PPROF` is already set in your environment, `--profile` is a no-op and
your value is used as-is.

```sh
# Unix — hew-observe auto-discovers the unix socket
# Terminal 1
hew run myapp.hew --profile

# Terminal 2
hew-observe

# Non-Unix — profiler binds TCP on :6060
# Terminal 1
hew run myapp.hew --profile

# Terminal 2
hew-observe --addr localhost:6060
```

You can also set `HEW_PPROF` directly on an already-compiled binary to choose
a specific TCP address:

```sh
HEW_PPROF=:6060 ./myapp          # bind profiler on 0.0.0.0:6060
hew-observe --addr localhost:6060
```

To write a profile file on exit, set `HEW_PROF_OUTPUT` to `pprof`, `flat`, or
`both`:

```sh
HEW_PPROF=auto HEW_PROF_OUTPUT=pprof ./myapp
# writes hew-profile.pb.gz
```
