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
hew test file.hew                 # Run tests
hew wire check file.hew --against baseline.hew
                                  # Validate wire compatibility
hew fmt file.hew                  # Format source code
hew init [name]                   # Initialize a new project
hew completions <shell>           # Generate shell completions
hew version                       # Print version info
```

`hew file.hew` is shorthand for `hew build file.hew`.

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

