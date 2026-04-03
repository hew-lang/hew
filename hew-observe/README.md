# hew-observe

TUI dashboard for debugging Hew actor systems in real time.

Connects to a running Hew program's built-in profiler endpoint and displays:

- Live actor count and message throughput
- Per-actor mailbox depth and processing latency
- Actor group hierarchy and supervisor trees
- Memory allocation stats

## Usage

```sh
# Start your Hew program with profiling enabled (auto unix-socket discovery)
hew run myapp.hew --profile

# In another terminal, attach the observer (auto-discovers on Unix)
hew-observe

# Or target a specific TCP address
hew-observe --addr localhost:6060

# List all discovered profiler processes (Unix only)
hew-observe --list
```

You can also run an already-compiled binary directly without `hew run`:

```sh
HEW_PPROF=auto ./myapp      # Unix: per-user socket, auto-discovered
HEW_PPROF=:6060 ./myapp     # TCP on port 6060
hew-observe                 # or: hew-observe --addr localhost:6060
```

## Part of the Hew compiler

This crate is an internal component of the [Hew](https://github.com/hew-lang/hew) compiler toolchain.
