# hew-cli

The Hew programming language compiler driver.

## Usage

```sh
hew build file.hew [-o output]    # Compile to executable
hew run file.hew [-- args...]     # Compile and run
hew run file.hew --profile        # Compile, run, and enable the built-in profiler
hew debug file.hew [-- args...]   # Build with debug info + launch gdb/lldb
hew check file.hew                # Parse + typecheck only
hew doc file.hew                  # Generate documentation
hew eval "expr"                   # Evaluate an expression
hew eval -f file.hew              # Evaluate a file in REPL context
hew test file.hew                 # Run tests
hew wire check file.hew --against baseline.hew
                                  # Validate wire compatibility
hew fmt file.hew                  # Format source file in-place
hew fmt --stdin < file.hew       # Format source from stdin to stdout
hew fmt --check file.hew         # Check formatting (CI mode)
hew init [name]                   # Scaffold main.hew + README.md only (no hew.toml)
hew completions <shell>           # Generate shell completions
hew version                       # Print version info
```

`hew file.hew` is shorthand for `hew build file.hew`.

## Formatting

`hew fmt` supports four common workflows:

```sh
hew fmt myapp/main.hew                    # Rewrite a file in-place
hew fmt --stdin < myapp/main.hew          # Read stdin, write formatted source to stdout
hew fmt --check myapp/main.hew            # Exit non-zero if a file needs formatting
hew fmt --check --stdin < myapp/main.hew  # Verify piped input without writing
```

Without flags, `hew fmt` rewrites each named file in-place and leaves
already-formatted files untouched. When a file is rewritten, `hew fmt` prints
`Formatted <file>` on stderr; already-formatted files produce no output.

Use `--stdin` for editor integrations or shell pipelines. It reads from stdin,
writes the formatted result to stdout, and cannot be combined with file
arguments.

Use `--check` when you want formatting verification without rewriting files.
For files, `hew fmt --check` prints `<file>: needs formatting` and exits non-zero
when any input needs changes, which makes it suitable for CI. Combined with
`--stdin`, it performs the same verification on piped input, prints
`<stdin>: needs formatting` on failure, and stays silent on success.

`hew eval` phase-1 runs each inline expression or buffered `-f` chunk through
the in-process native pipeline with a fresh bounded execution. Session
definitions persist between evaluations, but `--timeout <seconds>` applies to
each evaluation independently (30 seconds by default), not to the entire REPL
session or file.

For common import-resolution, type-checking, and build failures, see
[`../docs/troubleshooting.md`](../docs/troubleshooting.md).

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

For the canonical project bootstrap flow, start with `adze init [name]`. It
creates `hew.toml`, `main.hew`, and `.gitignore`, after which
`hew check main.hew` and `hew run main.hew` both operate on the same entry
file. `hew init [name]` remains the lighter source-only scaffold: it writes
`main.hew` plus `README.md`, but no `hew.toml`.

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
