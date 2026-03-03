# Hew `machine` Type Specification v0.1.0 (Draft)

A `machine` is a **value type** that defines a closed set of named states, a closed set of named events, and transition rules mapping `(State, Event)` pairs to new states. It compiles to a tagged union with a generated `step()` function. Machines are not actors — they are data, like enums with per-state fields and compiler-checked transition logic.

**Design pillars:**

- **Value semantics** — a machine is a tagged union (like `enum`), not a reference type.
- **Exhaustiveness** — the compiler verifies that every `(State, Event)` pair is handled.
- **Composability** — machines embed in actors, structs, collections, and function parameters.
- **Zero-cost** — compiles to an integer tag + union of structs. No allocations, no threads.

**Changes in v0.1.0:**

_Initial specification. Defines syntax, type system integration, generated API, exhaustiveness rules, transition semantics, wildcard transitions, events with data, and pattern matching._

---

## §1 Overview

A `machine` declaration introduces a new nominal type with three components:

1. **States** — a closed set of named variants, each with optional per-state fields.
2. **Events** — a closed set of named variants, each with optional payload fields.
3. **Transitions** — rules mapping `(SourceState, Event)` → `TargetState`, with a body that constructs the target state value.

A machine value is always in exactly one state. The only way to change state is via the generated `step()` method, which accepts an event and returns a new machine value. The original value is consumed (move semantics).

### §1.1 Non-goals (v0.1)

- No hierarchical/nested states.
- No guard conditions on transitions.
- No entry/exit hooks.
- No side effects in transition bodies (pure transformation only).
- No `initial` keyword (the caller constructs the initial state explicitly).

### §1.2 Relationship to enums

A `machine` is a strict superset of an `enum` at the type level — it defines a tagged union with named variants and optional fields. The key addition is the `event` + `on` transition syntax and the generated `step()` method. A machine value can be pattern-matched like an enum.

---

## §2 Syntax

### §2.1 Machine declaration

```hew
machine TcpState {
    // States with optional per-state data
    state Closed;
    state Listen { backlog: Int; }
    state Established { local_seq: Int; remote_seq: Int; }
    state FinWait;
    state TimeWait;

    // Events with optional payload data
    event Connect;
    event SynAck;
    event Data { payload: String; }
    event Close;
    event Timeout;

    // Transitions: on EventName: SourceState -> TargetState { body }
    on Connect: Closed -> Listen {
        Listen { backlog: 128 }
    }

    on SynAck: Listen -> Established {
        Established { local_seq: 0, remote_seq: 0 }
    }

    on Data: Established -> Established {
        Established { local_seq: state.local_seq + 1, remote_seq: state.remote_seq }
    }

    on Close: Established -> FinWait {
        FinWait
    }

    on Timeout: TimeWait -> Closed {
        Closed
    }

    // Wildcard: handle event in all unhandled states
    on Timeout: _ -> _ {
        self  // stay in current state (identity transition)
    }
}
```

### §2.2 States

A state declaration introduces a named variant of the machine type.

```
state Ident ;
state Ident { FieldDecl { FieldDecl } } ;
```

- State names MUST be `PascalCase` identifiers, unique within the machine.
- A machine MUST declare at least two states.
- Fields within a state follow struct field syntax: `name: Type;`.
- State fields are only accessible inside transition bodies where that state is the source.

### §2.3 Events

An event declaration introduces a named variant of the machine's event type.

```
event Ident ;
event Ident { FieldDecl { FieldDecl } } ;
```

- Event names MUST be `PascalCase` identifiers, unique within the machine.
- A machine MUST declare at least one event.
- Fields within an event are payload data delivered to transition bodies.
- The compiler generates a companion enum type `{MachineName}Event` for events.

### §2.4 Transitions

A transition declaration maps a `(SourceState, Event)` pair to a `TargetState` and provides a body that constructs the target state value.

```
on EventIdent : SourceIdent -> TargetIdent { Expr }
```

- `EventIdent` MUST name a declared event.
- `SourceIdent` MUST name a declared state, or `_` (wildcard source).
- `TargetIdent` MUST name a declared state, or `_` (wildcard target, meaning same as source).
- The body is an expression that MUST evaluate to a value of the target state variant.
- When `TargetIdent` is `_`, the body MUST evaluate to a value of the machine type (any variant).

---

## §3 Transition semantics

### §3.1 The `state` binding

Inside a transition body, `state` is bound to the source state's data:

- If the source state has no fields, `state` is the unit-like state value.
- If the source state has fields, `self.field_name` accesses each field.
- `state` is consumed by the transition (move semantics). After the transition body executes, the old state no longer exists.

```hew
// state.local_seq and state.remote_seq are accessible because
// the source state is Established { local_seq: Int; remote_seq: Int; }
on Data: Established -> Established {
    Established { local_seq: state.local_seq + 1, remote_seq: state.remote_seq }
}
```

### §3.2 Event payload access

When an event carries payload fields, those fields are accessible by name in the transition body:

```hew
event Data { payload: String; }

on Data: Established -> Established {
    // 'payload' is accessible directly from the event
    log(payload);
    Established { local_seq: state.local_seq + 1, remote_seq: state.remote_seq }
}
```

Event payload fields are bound as local immutable variables in the transition body. If an event field name conflicts with a state field name, the event field takes precedence and the state field is accessed via `self.field_name`.

### §3.3 Return value

The transition body MUST evaluate to a value of the machine type. Specifically:

- If `TargetState` is a concrete state name, the body MUST return that state's variant.
- If `TargetState` is `_`, the body MAY return any variant of the machine type.
- The compiler type-checks the body's return value against the target.

### §3.4 Purity

Transition bodies are **pure** — they compute a new state from the old state and event data. They MUST NOT:

- Perform I/O.
- Access mutable external state.
- Call `receive fn` methods on actors.
- Spawn actors or tasks.

_Implementation note: The compiler MAY enforce purity by restricting the set of callable functions in transition bodies to `pure fn` and built-in operators. For v0.1, the compiler SHOULD emit a warning (not an error) for impure calls, to allow incremental adoption._

### §3.5 No output values (Mealy restriction)

For v0.1, transitions produce **only** the new state — they do not produce output values. This is a Moore machine model. Side effects (sending messages, logging, I/O) happen in the actor or function that calls `step()`:

```hew
actor ConnectionManager {
    var tcp: TcpState = TcpState::Closed;

    receive fn handle(event: TcpStateEvent) {
        let old = self.tcp;
        self.tcp = self.tcp.step(event);

        // Side effects happen here, not in the machine
        match self.tcp {
            TcpState::Established { .. } => println("Connection established"),
            TcpState::Closed => println("Connection closed"),
            _ => {}
        }
    }
}
```

_Future versions MAY introduce Mealy semantics (`on Event: Source -> Target / Output`) where transitions can produce an output value alongside the new state._

---

## §4 Exhaustiveness

### §4.1 The exhaustiveness matrix

For a machine with _S_ states and _E_ events, there are _S × E_ cells in the exhaustiveness matrix. Each cell represents the `(State, Event)` pair. Every cell MUST be covered by exactly one transition rule (explicit or wildcard).

### §4.2 Coverage rules

A cell `(State, Event)` is **covered** if any of the following apply:

1. An explicit transition `on Event: State -> Target { ... }` exists.
2. A wildcard-source transition `on Event: _ -> _ { ... }` exists and no explicit transition for that `(State, Event)` exists.

Wildcard transitions act as defaults — they fill uncovered cells for a given event. An explicit transition always takes priority over a wildcard.

### §4.3 Compiler behavior

- **Error**: If any `(State, Event)` cell is uncovered (no explicit transition and no wildcard for that event), the compiler MUST emit an error listing the uncovered pairs.
- **Error**: If two explicit (non-wildcard) transitions cover the same `(State, Event)` cell, the compiler MUST emit a duplicate-transition error.
- **Warning**: If a wildcard transition is defined for an event but all states already have explicit transitions for that event, the compiler SHOULD emit a dead-code warning (the wildcard is unreachable).

### §4.4 Example: exhaustiveness error

```hew
machine Light {
    state Off;
    state On;

    event Toggle;
    event Dim;

    on Toggle: Off -> On { On }
    on Toggle: On -> Off { Off }
    // ERROR: missing transitions for (Off, Dim) and (On, Dim)
}
```

Fix with a wildcard:

```hew
    on Dim: _ -> _ { state }  // ignore Dim in all states
```

---

## §5 Wildcard transitions

### §5.1 Wildcard source (`_` as source state)

```hew
on Timeout: _ -> _ { state }
```

This transition applies to every state that does not have an explicit `on Timeout` transition. The body receives `state` as the current state (type: the machine type itself, not a specific variant). The body MUST return a value of the machine type.

### §5.2 Wildcard target (`_` as target state)

When the target is `_`, it means "the resulting state is determined by the body". The compiler does not constrain the return type to a specific variant — any variant of the machine type is valid.

```hew
on Reset: _ -> _ {
    Closed  // always go to Closed, regardless of source
}
```

### §5.3 Wildcard source with concrete target

```hew
on FatalError: _ -> Closed {
    Closed
}
```

This applies to all states without an explicit `on FatalError` transition, and the body MUST return the `Closed` variant.

### §5.4 Self-transition shorthand

The expression `state` in a wildcard body returns the current state unchanged (identity transition). This is the canonical way to ignore an event:

```hew
on Heartbeat: _ -> _ { state }
```

---

## §6 Type system integration

### §6.1 Machine as a type

A `machine` declaration introduces a nominal type. The type name is the machine identifier.

```hew
let state: TcpState = TcpState::Closed;
```

Machine types are:

- **Value types** — they are copied/moved like structs and enums.
- **`Send`** — if all state fields are `Send`, the machine type is automatically `Send`.
- **`Frozen`** — if all state fields are `Frozen`, the machine type is automatically `Frozen`.
- **Sized** — always. The size is `max(size of each variant) + tag size`.

### §6.2 Generated event type

The compiler generates a companion enum for events:

```hew
// For machine TcpState, the compiler generates:
enum TcpStateEvent {
    Connect;
    SynAck;
    Data { payload: String; };
    Close;
    Timeout;
}
```

The event enum name is `{MachineName}Event`. Event variants follow the same naming and field conventions as enum variants.

### §6.3 Generics

Machines MAY be parameterized by type parameters:

```hew
machine StateMachine<T> {
    state Empty;
    state Loaded { data: T; }

    event Load { item: T; }
    event Clear;

    on Load: Empty -> Loaded {
        Loaded { data: item }
    }

    on Clear: Loaded -> Empty {
        Empty
    }

    on Load: Loaded -> Loaded {
        Loaded { data: item }
    }

    on Clear: Empty -> Empty {
        Empty
    }
}
```

Type parameters follow the same rules as generic structs and enums (§3.8 of HEW-SPEC). Monomorphization applies.

### §6.4 Trait implementations

Machines MAY implement traits via `impl` blocks:

```hew
impl Display for TcpState {
    fn to_string(self) -> String {
        self.state_name()
    }
}
```

The compiler automatically derives `Debug` for all machine types.

### §6.5 No subtyping

Individual states are NOT types. `TcpState::Established` is a variant constructor, not a type. You cannot declare a variable of type `Established` — only of type `TcpState`.

---

## §7 Generated API

For a machine named `M` with event type `MEvent`, the compiler generates:

### §7.1 `M.step(event: MEvent) -> M`

The primary API. Accepts an event, applies the matching transition, returns the new state. The original value is consumed (move semantics).

```hew
let s1 = TcpState::Closed;
let s2 = s1.step(TcpStateEvent::Connect);
// s1 is consumed; s2 is TcpState::Listen { backlog: 128 }
```

_Implementation note: `step()` compiles to a nested switch on (tag, event_tag). The outer switch dispatches on the current state tag; the inner switch dispatches on the event tag. Each branch executes the corresponding transition body._

### §7.2 `M.state_name() -> String`

Returns the name of the current state as a string, for debugging and logging.

```hew
let s = TcpState::Established { local_seq: 42, remote_seq: 7 };
assert(s.state_name() == "Established");
```

_Implementation note: compiles to a switch on the tag returning a string literal._

### §7.3 Pattern matching

Machine values participate in pattern matching identically to enum values:

```hew
match tcp_state {
    Closed => println("closed"),
    Listen { backlog } => println(f"listening, backlog={backlog}"),
    Established { local_seq, remote_seq } => {
        println(f"established seq={local_seq}/{remote_seq}")
    },
    FinWait => println("fin-wait"),
    TimeWait => println("time-wait"),
}
```

Pattern matching on machines follows the same exhaustiveness rules as enum matching (§3.3 of HEW-SPEC). The compiler requires all variants to be covered, or a wildcard `_` arm.

Partial matching with `_` is supported:

```hew
match tcp_state {
    Established { local_seq, .. } => println(f"seq={local_seq}"),
    _ => println("not established"),
}
```

### §7.4 Variant constructors

Each state is a constructor for the machine type, qualified by the machine name:

```hew
let a = TcpState::Closed;
let b = TcpState::Listen { backlog: 64 };
let c = TcpState::Established { local_seq: 0, remote_seq: 0 };
```

Within the machine's own transition bodies, the machine name qualifier is optional:

```hew
on Connect: Closed -> Listen {
    Listen { backlog: 128 }    // OK: unqualified inside machine body
}
```

Outside the machine, the full qualifier is required:

```hew
let s = TcpState::Listen { backlog: 128 };  // Required outside machine body
```

### §7.5 Event constructors

Events follow the same pattern:

```hew
let e = TcpStateEvent::Data { payload: "hello" };
let s2 = s1.step(e);
```

Within transition bodies, event constructors are not used (the event is destructured automatically).

---

## §8 Compilation model

### §8.1 Pipeline integration

Machine declarations are parsed by the Rust frontend (`hew-parser`), type-checked by `hew-types`, serialized to MessagePack by `hew-serialize`, and lowered to MLIR by the C++ codegen (`hew-codegen`).

### §8.2 Representation

A machine compiles to a tagged union:

```
struct TcpState {
    tag: u8,           // discriminant (0 = Closed, 1 = Listen, ...)
    union {
        // variant 0: Closed — no fields, zero-sized
        // variant 1: Listen
        struct { backlog: i64; } listen;
        // variant 2: Established
        struct { local_seq: i64; remote_seq: i64; } established;
        // variant 3: FinWait — no fields, zero-sized
        // variant 4: TimeWait — no fields, zero-sized
    } data;
};
```

The tag type is `u8` for machines with ≤ 256 states, `u16` otherwise.

The size of the machine is `sizeof(tag) + max(sizeof(variant_i))`, aligned to the largest field alignment.

### §8.3 Event representation

The event type compiles identically — a tagged union of event variants:

```
struct TcpStateEvent {
    tag: u8,
    union {
        // variant 0: Connect — no fields
        // variant 1: SynAck — no fields
        // variant 2: Data
        struct { payload: String; } data;
        // variant 3: Close — no fields
        // variant 4: Timeout — no fields
    } data;
};
```

### §8.4 step() codegen

The `step()` method compiles to a function with a nested switch:

```
TcpState TcpState_step(TcpState self, TcpStateEvent event) {
    switch (self.tag) {
        case 0: /* Closed */
            switch (event.tag) {
                case 0: /* Connect */
                    return { .tag = 1, .data.listen = { .backlog = 128 } };
                case 4: /* Timeout (wildcard) */
                    return self;
                // ... other events from wildcards
            }
        case 2: /* Established */
            switch (event.tag) {
                case 2: /* Data */
                    return { .tag = 2, .data.established = {
                        .local_seq = self.data.established.local_seq + 1,
                        .remote_seq = self.data.established.remote_seq
                    }};
                // ...
            }
        // ...
    }
}
```

_Implementation note: The MLIR lowering SHOULD use `hew.machine.step` as a custom MLIR op, which is then lowered to `scf.switch` during progressive lowering._

### §8.5 MessagePack schema

The machine declaration serializes to MessagePack as:

```
{
  "kind": "machine",
  "name": "TcpState",
  "type_params": [],
  "states": [
    { "name": "Closed", "fields": [] },
    { "name": "Listen", "fields": [{ "name": "backlog", "type": "Int" }] },
    ...
  ],
  "events": [
    { "name": "Connect", "fields": [] },
    { "name": "Data", "fields": [{ "name": "payload", "type": "String" }] },
    ...
  ],
  "transitions": [
    { "event": "Connect", "source": "Closed", "target": "Listen", "body": <Expr> },
    { "event": "Timeout", "source": "_", "target": "_", "body": <Expr> },
    ...
  ]
}
```

---

## §9 Complete example: Circuit Breaker

```hew
machine CircuitBreaker {
    state Closed { failures: Int; }
    state Open { expires_at: Int; }
    state HalfOpen { successes: Int; }

    event Success;
    event Failure { timestamp: Int; }
    event Tick { now: Int; }

    // --- Success transitions ---
    on Success: Closed -> Closed {
        Closed { failures: 0 }
    }

    on Success: HalfOpen -> HalfOpen {
        if self.successes + 1 >= 3 {
            Closed { failures: 0 }
        } else {
            HalfOpen { successes: self.successes + 1 }
        }
    }

    on Success: Open -> Open {
        self  // ignored while open
    }

    // --- Failure transitions ---
    on Failure: Closed -> Closed {
        if self.failures + 1 >= 5 {
            Open { expires_at: timestamp + 10000 }
        } else {
            Closed { failures: self.failures + 1 }
        }
    }

    on Failure: HalfOpen -> Open {
        Open { expires_at: timestamp + 10000 }
    }

    on Failure: Open -> Open {
        self  // already open
    }

    // --- Tick transitions ---
    on Tick: Open -> Open {
        if now >= self.expires_at {
            HalfOpen { successes: 0 }
        } else {
            self
        }
    }

    on Tick: _ -> _ {
        self  // no-op in Closed and HalfOpen
    }
}
```

Usage in an actor:

```hew
actor ApiGateway {
    var breaker: CircuitBreaker = CircuitBreaker::Closed { failures: 0 };

    receive fn call(req: Request) -> Result<Response, String> {
        // Check circuit state
        match self.breaker {
            CircuitBreaker::Open { .. } => {
                return Err("circuit open");
            },
            _ => {}
        }

        let result = http::send(req);

        // Update machine based on outcome
        match result {
            Ok(resp) => {
                self.breaker = self.breaker.step(CircuitBreakerEvent::Success);
                Ok(resp)
            },
            Err(e) => {
                let now = time::now_ms();
                self.breaker = self.breaker.step(CircuitBreakerEvent::Failure { timestamp: now });
                Err(e)
            },
        }
    }
}
```

Testing:

```hew
test "circuit opens after 5 failures" {
    var breaker = CircuitBreaker::Closed { failures: 0 };

    // 5 failures should open the circuit
    for i in 0..5 {
        breaker = breaker.step(CircuitBreakerEvent::Failure { timestamp: i * 1000 });
    }

    match breaker {
        CircuitBreaker::Open { .. } => assert(true),
        _ => assert(false),
    }
}

test "half-open recovers after 3 successes" {
    var breaker = CircuitBreaker::HalfOpen { successes: 0 };

    for _ in 0..3 {
        breaker = breaker.step(CircuitBreakerEvent::Success);
    }

    match breaker {
        CircuitBreaker::Closed { failures } => assert(failures == 0),
        _ => assert(false),
    }
}
```

---

## §10 EBNF grammar

The following productions extend the Hew grammar (§10 of HEW-SPEC.md).

```ebnf
(* Machine declarations — added to Item production *)
Item           = ... | MachineDecl ;

MachineDecl    = "machine" Ident TypeParams? "{"
                   { StateDecl | EventDecl | TransitionDecl }
                 "}" ;

StateDecl      = "state" Ident ( "{" { StructFieldDecl } "}" )? ";" ;

EventDecl      = "event" Ident ( "{" { StructFieldDecl } "}" )? ";" ;

TransitionDecl = "on" Ident ":" TransitionSource "->" TransitionTarget Block ;

TransitionSource = Ident | "_" ;

TransitionTarget = Ident | "_" ;
```

Where `StructFieldDecl`, `TypeParams`, `Block`, and `Ident` are as defined in the base Hew grammar.

---

## §11 Future work

The following features are explicitly deferred to future versions:

1. **Mealy outputs** — transitions producing output values alongside new state: `on Event: S1 -> S2 / OutputType { ... }`.
2. **Hierarchical states** — states containing sub-machines.
3. **Guard conditions** — `on Event: S1 -> S2 when (condition) { ... }`.
4. **Entry/exit hooks** — `enter S1 { ... }`, `exit S1 { ... }`.
5. **History states** — returning to a previously active sub-state.
6. **Timeout events** — compiler-generated events triggered by elapsed time.
7. **Visualization** — generating state diagrams from machine declarations.

---

## §12 References

- [1] Pony Language Tutorial — [tutorial.ponylang.io](https://tutorial.ponylang.io)
- [2] Erlang Supervision Principles — [erlang.org](https://www.erlang.org/doc/design_principles/sup_princ.html)
- [3] Statecharts — David Harel, 1987
- [4] Hew Language Specification v0.9.0 — `hew-machine/docs/specs/HEW-SPEC.md`
- [5] RFC: First-Class State Machines in Hew — `examples/machine-design.md`
