# Unreleased

The current candidate is [v0.10.0-rc1](v0.10.0-rc1.md). Changes after its frozen
candidate commit belong here.

## Supervisor restart delays

A supervisor without `backoff` now restarts a crashed child immediately. The
previous implicit delay of 100 milliseconds, doubling to 30 seconds from the
second crash, stalled tight restart loops. Declare
`backoff: restart.exponential(initial: 100ms)` to keep a delay; `maximum`
defaults to 30 seconds, and the delay never drops below `initial`.
