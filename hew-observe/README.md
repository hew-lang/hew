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

# If multiple profilers are running, pick one explicitly (Unix only)
hew-observe --pid 12345
```

You can also run an already-compiled binary directly without `hew run`:

```sh
HEW_PPROF=auto ./myapp      # Unix: per-user socket, auto-discovered
HEW_PPROF=:6060 ./myapp     # TCP on port 6060
hew-observe                 # or: hew-observe --addr localhost:6060
```

## Part of the Hew compiler

This crate is an internal component of the [Hew](https://github.com/hew-lang/hew) compiler toolchain.

## Discovery order

On Unix, `hew-observe` resolves the profiler socket directory in this order:

1. `$XDG_RUNTIME_DIR/hew-profilers/` (Linux, if `$XDG_RUNTIME_DIR` is set)
2. `$TMPDIR/hew-profilers-{uid}/` (macOS / BSD, if `$TMPDIR` is set)
3. `/tmp/hew-profilers-{uid}/` (fallback)

The Hew runtime writes a JSON descriptor (`{pid}.json`) to whichever
directory it chooses.  `hew-observe` reads from the first directory that
exists and is owned by your UID.  Stale entries (processes that are no longer
alive) are pruned automatically on the next scan.

## Troubleshooting

**No profiler discovered at startup?**

The observer starts in *waiting mode* and will attach automatically as soon
as a compatible profiler appears in the discovery directory.  Run
`hew-observe --list` to check what is currently visible.

Make sure the Hew program was started with profiling enabled:

```sh
hew run myapp.hew --profile
# or
HEW_PPROF=auto ./myapp
```

**"Multiple profilers running" error?**

Use `hew-observe --list` to see all active profilers, then pick one:

```sh
hew-observe --list        # shows PID, program name, uptime, socket path
hew-observe --pid 12345   # attach to that specific PID
```

**"No profiler found for PID …" error?**

The process may have exited or profiling may not be enabled.  Check with
`hew-observe --list` to see what is currently available.

**On non-Unix platforms (Windows)?**

Automatic discovery is not available.  Use `--addr` to specify the profiler
address explicitly:

```sh
HEW_PPROF=:6060 myapp.exe
hew-observe --addr localhost:6060
```
