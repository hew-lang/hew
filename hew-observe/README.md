# hew-observe

TUI dashboard for debugging Hew actor systems in real time.

Connects to a running Hew program's built-in profiler endpoint and displays:

- Live actor count and message throughput
- Per-actor mailbox depth and processing latency
- Actor group hierarchy and supervisor trees
- Memory allocation stats

## Usage

```sh
# Start your Hew program with profiling enabled
hew run myapp.hew --profile

# In another terminal, attach the observer
hew-observe
```

## Part of the Hew compiler

This crate is an internal component of the [Hew](https://github.com/hew-lang/hew) compiler toolchain.
