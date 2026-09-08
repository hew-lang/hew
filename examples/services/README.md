# Service Pattern Examples

Service-pattern examples retained during the compiler cutover. Review each
example's current acceptance before using it as a starting point. Ordinary actor
calls wait for completion; concurrent fan-out needs `fork`, and explicit
one-way submission uses a `mailbox` view.

## Examples

| Example                                              | Pattern           | Key Features                                                 |
| ---------------------------------------------------- | ----------------- | ------------------------------------------------------------ |
| [`circuit_breaker.hew`](circuit_breaker.hew)         | Circuit Breaker   | Actor as state machine, failure tracking, state transitions  |
| [`rate_limiter.hew`](rate_limiter.hew)               | Token Bucket      | Ask/reply, shared limiter actor, periodic refill timer       |
| [`worker_pool.hew`](worker_pool.hew)                 | Scatter/Gather    | Supervision, `fork` fan-out, crash recovery                  |
| [`pub_sub.hew`](pub_sub.hew)                         | Publish/Subscribe | Topic routing, actor-to-actor completion calls               |
| [`health_monitor.hew`](health_monitor.hew)           | Health Check      | `select` with timeout, periodic monitoring, aggregate status |
| [`distributed_counter.hew`](distributed_counter.hew) | Replicated State  | Coordinator pattern, local reads, sync-on-demand             |

## Running

An application package normally names its service entry in `[package] main` and
runs it with `hew check`, `hew run`, or `hew build` from anywhere inside the
package. This directory is a catalogue of independent entry files, so select a
specific fixture explicitly:

```sh
hew run examples/services/circuit_breaker.hew
```

Or compile and run separately:

```sh
hew build examples/services/circuit_breaker.hew -o circuit_breaker
./circuit_breaker
```

## Why Actors?

Actors serialize access to their state through handlers. This keeps ordinary
service state local and lets callers express coordination through messages.
