# Service Pattern Examples

Real-world distributed service patterns implemented in idiomatic Hew.
Each example demonstrates why the actor model eliminates boilerplate
that Go and Rust developers write by hand.

## Examples

| Example | Pattern | Key Features |
|---------|---------|-------------|
| [`circuit_breaker.hew`](circuit_breaker.hew) | Circuit Breaker | Actor as state machine, failure tracking, state transitions |
| [`rate_limiter.hew`](rate_limiter.hew) | Token Bucket | Ask/reply, shared limiter actor, periodic refill timer |
| [`worker_pool.hew`](worker_pool.hew) | Scatter/Gather | Supervision, `join` fan-out, crash recovery |
| [`pub_sub.hew`](pub_sub.hew) | Publish/Subscribe | Topic routing, actor-to-actor fan-out, fire-and-forget |
| [`health_monitor.hew`](health_monitor.hew) | Health Check | `select` with timeout, periodic monitoring, aggregate status |
| [`distributed_counter.hew`](distributed_counter.hew) | Replicated State | Coordinator pattern, local reads, sync-on-demand |

## Running

```sh
hew run examples/services/circuit_breaker.hew
```

Or compile and run separately:

```sh
hew build examples/services/circuit_breaker.hew -o circuit_breaker
./circuit_breaker
```

## Why Actors?

Every pattern above would require mutexes, channels, or `Arc<RwLock>` in
Go or Rust. In Hew, actors serialise access to their state through their
mailbox — data races are impossible by construction. The compiler enforces
it; the runtime executes it.
