# Language Comparison: Hew vs Go vs Rust

These examples implement the same problems in Hew, Go, and Rust
to demonstrate where Hew's design produces better outcomes.

## Test 1: Actor-based Counter Service

A counter service where multiple clients increment a shared counter.

- **Hew**: Actor isolation is the language model. No locks, no races, compile-time safe.
- **Go**: Requires channels or mutexes. Data races possible if you forget.
- **Rust**: Requires Arc<Mutex<>>. Verbose. Safe but painful.

## Test 2: Supervision / Fault Recovery

A service that handles errors in child workers.

- **Hew**: `supervisor` is a language construct. Restart strategy in 3 lines.
- **Go**: No built-in supervision. Must hand-roll goroutine restart logic.
- **Rust**: No built-in supervision. Libraries exist but are not standard.

## Test 3: Wire Protocol Definition

Define a network message format.

- **Hew**: `wire struct` is a language type. Schema evolution rules enforced at compile time.
- **Go**: Requires protobuf/gRPC external toolchain. Code generation step.
- **Rust**: Requires serde + format crate. Derive macros. No compile-time evolution checking.

## Running

```bash
# Hew
hew build counter_service.hew -o counter && ./counter

# Go
go run counter_service.go

# Rust
cargo run --example counter_service
```
