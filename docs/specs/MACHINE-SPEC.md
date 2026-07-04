# Hew `machine` Surface Specification

A `machine` is a **value type** that defines a closed set of named states, a closed set of named events, and transition rules mapping `(State, Event)` pairs to new states. It compiles to a tagged union with a generated `step()` function. Machines are not actors — they are data, like enums with per-state fields and compiler-checked transition logic.

**Design pillars:**

- **Value semantics** — a machine is a tagged union (like `enum`), not a reference type.
- **Exhaustiveness** — the compiler verifies that every `(State, Event)` pair is handled.
- **Composability** — machines embed in actors, structs, collections, and function parameters.
- **Zero-cost** — compiles to an integer tag + union of structs. No allocations, no threads.

_This document specifies the machine surface: the `=>` routing
arrow, `events {}` / `emits {}` vocabulary headers, the `reenter` keyword,
optional `on E(payload):` head bindings, inline state `entry {}` / `exit {}`
blocks, the `default { state }` unhandled-stays arm, depth-1 composite state
blocks, exhaustiveness rules, and diagram-oriented structure._

---

## §1 Overview

A `machine` declaration introduces a new nominal type with three components:

1. **States** — a closed set of named variants, each with optional per-state fields.
2. **Events** — a closed set of named variants (declared in an `events {}` header), each with optional payload fields.
3. **Transitions** — rules mapping `(SourceState, Event)` → `TargetState` (written with `=>`), with a body that constructs the target state value.

A machine value is always in exactly one state. The only way to change state is via the generated `step()` method, which accepts an event and mutates the machine in place (like `Vec.push()`). It does not return a value.

### §1.1 Non-goals and feature notes

> Guard conditions (`when`), inline `entry {}` / `exit {}` hooks, and depth-1
> composite (hierarchical) state blocks are all implemented. The list below
> records the remaining non-goals.

- No nesting deeper than depth-1 (a substate may not itself contain substates) — reserved for v0.6.
- No side effects in transition bodies beyond `entry`/`exit`/`emit` (pure transformation otherwise).
- No `initial` keyword for the *machine's* start state (the caller constructs it explicitly); `initial` IS used inside a composite block to mark a substate.
- No union-source `A | B =>` shared events (write the rule per source or use a composite parent rule).
- No Mealy `/ Output` syntax on transitions (use `emit` for outputs).

### §1.2 Relationship to enums

A `machine` is a strict superset of an `enum` at the type level — it defines a tagged union with named variants and optional fields. The key addition is the `event` + `on` transition syntax and the generated `step()` method. A machine value can be pattern-matched like an enum.

---

## §2 Syntax

### §2.1 Machine declaration

```hew
machine TcpState {
    // Input-event vocabulary, declared up front.
    events {
        Connect;
        SynAck;
        Data { payload: string; }
        Close;
        Timeout;
    }

    // States with optional per-state data.
    state Closed;
    state Listen { backlog: i64; }
    state Established { local_seq: i64; remote_seq: i64; }
    state FinWait;
    state TimeWait;

    // Transitions: on EventName: SourceState => TargetState { body }
    on Connect: Closed => Listen {
        Listen { backlog: 128 }
    }

    on SynAck: Listen => Established {
        Established { local_seq: 0, remote_seq: 0 }
    }

    on Data: Established => Established reenter {
        Established { local_seq: self.local_seq + 1, remote_seq: self.remote_seq }
    }

    on Close: Established => FinWait {
        FinWait
    }

    on Timeout: TimeWait => Closed {
        Closed
    }

    // Wildcard: handle event in all unhandled states.
    on Timeout: _ => _ {
        state  // stay in current state (identity transition)
    }
}
```

### §2.2 The `events {}` header

Every machine declares its input-event vocabulary in a single `events {}` block
at the top of the body, before the states.

```
events { EventDecl { EventDecl } }
EventDecl = Ident ( ";" | "{" { FieldDecl } "}" [";"] )
```

- Event names MUST be `PascalCase` identifiers, unique within the machine.
- A machine MUST declare at least one event.
- Fields within an event are payload data delivered to transition bodies.
- The compiler generates a companion enum type `{MachineName}Event` for events.
- `events` is a contextual keyword, valid only inside a machine body.

### §2.3 The `emits {}` header (optional)

An optional `emits {}` block declares the Mealy-output vocabulary — the events a
transition body may produce via `emit`.

```
emits { Ident ";" { Ident ";" } }
```

- Each entry names a declared event the machine may `emit`.
- When the manifest is present, an `emit` of an event NOT listed is a compile error, keeping the output surface auditable.
- `emits` is a contextual keyword, valid only inside a machine body.

### §2.4 States

A state declaration introduces a named variant of the machine type.

```
state Ident ;
state Ident { { FieldDecl } [ "entry" Block ] [ "exit" Block ] } [";"]
```

- State names MUST be `PascalCase` identifiers, unique within the machine.
- A machine MUST declare at least two states.
- Fields within a state follow struct field syntax: `name: Type;`.
- State fields are accessible as `self.field` inside transition bodies where that state is the source.
- A state body may carry `entry {}` / `exit {}` lifecycle blocks (run on state change).

### §2.5 Transitions

A transition declaration maps a `(SourceState, Event)` pair to a `TargetState`
and provides a body that constructs the target state value.

```
on EventIdent [ "(" Ident { "," Ident } ")" ] : SourcePat "=>" TargetPat
   [ "reenter" ] [ "when" Expr ] TransitionBody
SourcePat = Ident | "_"
TargetPat = Ident | "_"
```

- `EventIdent` MUST name a declared event.
- The optional `(field, …)` head binding names the event's payload fields so the body can use the bare names instead of `event.field`.
- `SourcePat` MUST name a declared state, or `_` (wildcard source).
- `TargetPat` MUST name a declared state, or `_` (wildcard target, meaning same as source).
- `=>` (`Token::FatArrow`) is the state-routing arrow.
- `reenter` opts a self-transition into Mealy re-entry (runs the source `exit` and target `entry` even though the state does not change).
- The body is an expression that MUST evaluate to a value of the target state variant.
- When `TargetPat` is `_`, the body MUST evaluate to a value of the machine type (any variant).

---

## §3 Transition semantics

### §3.1 The `self` and `state` bindings

Inside a transition body two implicit bindings are in scope:

- `self.field` — accesses a field of the **source state** (named source only; a
  wildcard-source body has no single source state, so `self.field` is rejected
  there). Available when the source state declares the field.
- `state` — the whole current machine value, used for the wildcard-source
  passthrough (`=> _ { state }`).

```hew
// self.local_seq and self.remote_seq are accessible because the source
// state is Established { local_seq: i64; remote_seq: i64; }
on Data: Established => Established reenter {
    Established { local_seq: self.local_seq + 1, remote_seq: self.remote_seq }
}
```

### §3.2 Event payload access

When an event carries payload fields, those fields are accessible in the
transition body either via `event.field` or via an `on E(field):` head binding:

```hew
events {
    Data { payload: string; }
}

// Head binding: `payload` is named at the rule site.
on Data(payload): Established => Established reenter {
    log(payload);
    Established { local_seq: self.local_seq + 1, remote_seq: self.remote_seq }
}

// Equivalent without the head binding, reading `event.payload` directly.
on Data: Established => Established reenter {
    log(event.payload);
    Established { local_seq: self.local_seq + 1, remote_seq: self.remote_seq }
}
```

Both forms lower identically. The head binding is the recommended idiom because
the names in scope are declared at the rule site.

### §3.3 Return value

The transition body MUST evaluate to a value of the machine type. Specifically:

- If `TargetState` is a concrete state name, the body MUST return that state's variant.
- If `TargetState` is `_`, the body MAY return any variant of the machine type.
- The compiler type-checks the body's return value against the target.

### §3.4 Purity

Transition bodies are **pure** by intent — they compute a new state from the old state and event data. They MUST NOT:

- Perform I/O.
- Access mutable external state.
- Call `receive fn` methods on actors.
- Spawn actors or tasks.

_Implementation note: `gen {}` blocks (suspension expressions) inside a transition body are compile errors (`E_GENBLOCK_IN_MACHINE_TRANSITION`). Full effect analysis for I/O calls and actor messages is not yet enforced; those restrictions are the language invariant, not the current compiler gate._

### §3.5 Output values

Transitions produce a new state and may additionally emit named output events via `emit EventName { … }` statements (Mealy-style outputs), declared up front in the `emits {}` manifest (§2.3). Side effects unrelated to the machine — actor sends, I/O, logging — happen in the actor or function that calls `step()`:

```hew
actor ConnectionManager {
    var tcp: TcpState = TcpState::Closed;

    receive fn handle(event: TcpStateEvent) {
        tcp.step(event);

        // Side effects happen here, not in the machine body
        match tcp {
            TcpState::Established { local_seq, remote_seq } => println("Connection established"),
            TcpState::Closed => println("Connection closed"),
            _ => {}
        }
    }
}
```

_A dedicated `/ Output` arrow syntax is a declined non-goal — `emit` covers Mealy outputs. Unit-event `emit` lowers; non-unit payload `emit` is not yet wired in codegen (`emit EventName { field: value, ... }` with a non-empty payload fails closed at codegen)._

**Delivery semantics.** A unit `emit` pushes onto a thread-local emit queue tagged with the emitting machine's stable type id (a name-derived digest, not the instance). `step()`'s implicit queue wrapper keeps every pushed event queued — it never drains or discards. A machine method `m.take_emits(EventName) -> i64` removes every queued event matching (this machine's TYPE, `EventName`'s tag) and returns the count removed; unconsumed events — of other tags, or from other machine instances that share the same type — accumulate on the queue exactly like today's unbounded default mailbox. Delivery is per-thread, per-machine-TYPE: it is NOT per-instance and NOT per-actor. A machine stepped inside an actor's `receive fn` must call `take_emits` within that same handler activation — cross-activation delivery (routing an emit to a specific actor or scheduler slot) is deferred to the actor-scheduler integration slice and composes on top of this ABI without changing it.

```hew
var s = Signal::Idle;
s.step(Start);            // transitions to Active, emits Ready {}
s.take_emits(Ready);       // 1 — removes the queued Ready event
s.take_emits(Ready);       // 0 — nothing left to remove
```

---

## §4 Exhaustiveness

### §4.1 The exhaustiveness matrix

For a machine with _S_ states and _E_ events, there are _S × E_ cells in the exhaustiveness matrix. Each cell represents the `(State, Event)` pair. Every cell MUST be covered by exactly one transition rule (explicit or wildcard).

### §4.2 Coverage rules

A cell `(State, Event)` is **covered** if any of the following apply:

1. An explicit transition `on Event: State => Target { ... }` exists.
2. A wildcard-source transition `on Event: _ => _ { ... }` exists and no explicit transition for that `(State, Event)` exists.
3. A `default { state }` arm covers it (every otherwise-uncovered cell stays in the current state).

Wildcard transitions act as defaults — they fill uncovered cells for a given event. An explicit transition always takes priority over a wildcard.

### §4.3 Compiler behaviour

- **Error**: If any `(State, Event)` cell is uncovered (no explicit transition and no wildcard for that event), the compiler MUST emit an error listing the uncovered pairs.
- **Error**: If two explicit (non-wildcard) transitions cover the same `(State, Event)` cell, the compiler MUST emit a duplicate-transition error.
- **Warning**: If a wildcard transition is defined for an event but all states already have explicit transitions for that event, the compiler SHOULD emit a dead-code warning (the wildcard is unreachable).

### §4.4 Example: exhaustiveness error

```hew
machine Light {
    events {
        Toggle;
        Dim;
    }

    state Off;
    state On;

    on Toggle: Off => On { On }
    on Toggle: On => Off { Off }
    // ERROR: missing transitions for (Off, Dim) and (On, Dim)
}
```

Fix with a wildcard, or with a `default { state }` arm (§5.5):

```hew
    on Dim: _ => _ { state }  // ignore Dim in all states
```

---

## §5 Wildcard transitions

### §5.1 Wildcard source (`_` as source state)

```hew
on Timeout: _ => _ { state }
```

This transition applies to every state that does not have an explicit `on Timeout` transition. The body receives `state` as the current state (type: the machine type itself, not a specific variant). The body MUST return a value of the machine type.

### §5.2 Wildcard target (`_` as target state)

When the target is `_`, it means "the resulting state is determined by the body". The compiler does not constrain the return type to a specific variant — any variant of the machine type is valid.

```hew
on Reset: _ => _ {
    Closed  // always go to Closed, regardless of source
}
```

### §5.3 Wildcard source with concrete target

```hew
on FatalError: _ => Closed {
    Closed
}
```

This applies to all states without an explicit `on FatalError` transition, and the body MUST return the `Closed` variant.

### §5.4 Self-transition shorthand

The expression `state` in a wildcard body returns the current state unchanged (identity transition). This is the canonical way to ignore an event:

```hew
on Heartbeat: _ => _ { state }
```

### §5.5 The `default { state }` arm

A `default { state }` clause at the end of the machine body declares that every
unhandled `(state, event)` cell **stays in the current state**. It satisfies
exhaustiveness for all otherwise-uncovered cells:

```hew
machine Tank {
    events {
        Fill;
        Drain;
    }

    state Filling;
    state Draining;

    on Drain: Filling => Draining { Draining }

    default { state }  // any other cell stays put
}
```

`default` differs from a `_ => _ { state }` wildcard in that it applies after all
explicit and wildcard rules, covering every remaining cell at once. A machine
*without* `default` and without covering wildcards fails exhaustiveness at
compile time (fail-closed).

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
    Data { payload: string; };
    Close;
    Timeout;
}
```

The event enum name is `{MachineName}Event`. Event variants follow the same naming and field conventions as enum variants.

### §6.3 Generics

Machines MAY be parameterized by type parameters:

```hew
machine StateMachine<T> {
    events {
        Load { item: T; }
        Clear;
    }

    state Empty;
    state Loaded { data: T; }

    on Load(item): Empty => Loaded {
        Loaded { data: item }
    }

    on Clear: Loaded => Empty {
        Empty
    }

    on Load(item): Loaded => Loaded reenter {
        Loaded { data: item }
    }

    on Clear: Empty => Empty reenter {
        Empty
    }
}
```

Type parameters follow the same rules as generic structs and enums (§3.8 of HEW-SPEC). Monomorphization applies.

### §6.4 Trait implementations

Machines MAY implement traits via `impl` blocks:

```hew
impl Display for TcpState {
    fn to_string(s: TcpState) -> string {
        s.state_name()
    }
}
```

The compiler automatically derives `Debug` for all machine types.

### §6.5 No subtyping

Individual states are NOT types. `TcpState::Established` is a variant constructor, not a type. You cannot declare a variable of type `Established` — only of type `TcpState`.

---

## §7 Generated API

For a machine named `M` with event type `MEvent`, the compiler generates:

### §7.1 `M.step(event: MEvent)`

The primary API. Accepts an event, applies the matching transition, and mutates the machine in place. Does not return a value (like `Vec.push()`).

```hew
var s = TcpState::Closed;
s.step(TcpStateEvent::Connect);
// s is now TcpState::Listen { backlog: 128 }
```

_Implementation note: `step()` compiles to a nested switch on (tag, event_tag). The outer switch dispatches on the current state tag; the inner switch dispatches on the event tag. Each branch executes the corresponding transition body._

### §7.2 `M.state_name() -> string`

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
on Connect: Closed => Listen {
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
s1.step(e);
```

Within transition bodies, event constructors are not used (the event is destructured automatically).

---

## §8 Compilation model

### §8.1 Pipeline integration

Machine declarations are parsed by the Rust frontend (`hew-parser`), type-checked by `hew-types`, lowered through the Rust HIR/MIR ladder, and emitted through `hew-codegen-rs`'s direct Rust/Inkwell LLVM backend.

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
        struct { payload: string; } data;
        // variant 3: Close — no fields
        // variant 4: Timeout — no fields
    } data;
};
```

### §8.4 step() codegen

The `step()` method compiles to a function that computes the new state and stores it back into the receiver variable. At the call site, the compiler generates a call to the internal step function followed by a store to the machine variable's slot:

```
// Internal step function (returns new value)
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

// Call site: m.step(event) compiles to:
//   %new = call TcpState_step(%m, %event)
//   store %new, %m_slot
```

_Implementation note: The internal step function still returns the new machine value. The compiler handles the store-back at the call site, similar to how `Vec.push()` mutates through the binding._

### §8.5 AST serialisation shape

The machine AST node serialises via `serde_json` (not MessagePack). The `Item`
enum uses an untagged outer wrapper, so the JSON shape is:

```json
{
  "Machine": {
    "name": "TcpState",
    "type_params": [],
    "states": [
      { "name": "Closed", "fields": [] },
      { "name": "Listen", "fields": [{ "name": "backlog", "type": "i64" }] }
    ],
    "events": [
      { "name": "Connect", "fields": [] },
      { "name": "Data", "fields": [{ "name": "payload", "type": "string" }] }
    ],
    "transitions": [
      { "event": "Connect", "source": "Closed", "target": "Listen", "body": "…" },
      { "event": "Timeout", "source": "_", "target": "_", "body": "…" }
    ]
  }
}
```

The outer key is `"Machine"` (the variant name), not `"kind"`. This is an
illustrative pseudo-schema — the exact field set follows the `ast::Machine`
struct definition.

---

## §9 Complete example: Circuit Breaker

The machine itself compiles and is verified against the Hew compiler. The actor
usage snippet shows the intended integration pattern; embedding a machine type as
an actor field is a known in-progress MIR limitation (`ActorStateCloneClassificationFailed`
for user-defined record types used as actor state).

```hew
machine CircuitBreaker {
    events {
        Success;
        Failure { timestamp: i64; }
        Tick { now: i64; }
    }

    state Closed { failures: i64; }
    state Open { expires_at: i64; }
    state HalfOpen { successes: i64; }

    // --- Success transitions ---
    on Success: Closed => Closed reenter {
        Closed { failures: 0 }
    }

    // HalfOpen success: use wildcard target to avoid `state` in a conditional branch.
    // Three consecutive successes recover to Closed; fewer increment the counter.
    on Success: HalfOpen => _ {
        if self.successes + 1 >= 3 {
            Closed { failures: 0 }
        } else {
            HalfOpen { successes: self.successes + 1 }
        }
    }

    on Success: Open => Open reenter {
        state  // ignored while open
    }

    // --- Failure transitions ---
    // Closed failure: use wildcard target for the conditional open/stay branch.
    on Failure(timestamp): Closed => _ {
        if self.failures + 1 >= 5 {
            Open { expires_at: timestamp + 10000 }
        } else {
            Closed { failures: self.failures + 1 }
        }
    }

    on Failure(timestamp): HalfOpen => Open {
        Open { expires_at: timestamp + 10000 }
    }

    on Failure: Open => Open reenter {
        state  // already open
    }

    // --- Tick transitions ---
    // Open tick: use wildcard target to avoid `state` in a conditional else-branch.
    on Tick(now): Open => _ {
        if now >= self.expires_at {
            HalfOpen { successes: 0 }
        } else {
            Open { expires_at: self.expires_at }
        }
    }

    on Tick: _ => _ {
        state  // no-op in Closed and HalfOpen
    }
}
```

> **Note on `=> _` vs `=> X reenter { state }`:** `state` is valid as the sole
> expression of a `reenter` body (unconditional stay). It is not yet valid as a
> branch of an if/else inside a `reenter` body. Use `=> _` (wildcard target) for
> transitions whose body conditionally returns different variants.

Usage in an actor (pattern illustration — actor embedding of machine types is a
current MIR limitation, not yet fully lowered):

```hew
actor ApiGateway {
    var breaker: CircuitBreaker = CircuitBreaker::Closed { failures: 0 };

    receive fn call(req: Request) -> Result<Response, string> {
        // Check circuit state
        match breaker {
            CircuitBreaker::Open { expires_at } => {
                return Err("circuit open");
            },
            _ => {}
        }

        let result = http::send(req);

        // Update machine based on outcome
        match result {
            Ok(resp) => {
                breaker.step(CircuitBreakerEvent::Success);
                Ok(resp)
            },
            Err(e) => {
                let now = time::now_ms();
                breaker.step(CircuitBreakerEvent::Failure { timestamp: now });
                Err(e)
            },
        }
    }
}
```

Testing (verified to compile and pass `hew check`):

```hew
fn main() {
    // Test: circuit opens after 5 failures
    var breaker = CircuitBreaker::Closed { failures: 0 };
    for i in 0..5 {
        breaker.step(CircuitBreakerEvent::Failure { timestamp: i * 1000 });
    }
    match breaker {
        CircuitBreaker::Open { expires_at } => assert(expires_at > 0),
        _ => assert(false),
    }

    // Test: half-open recovers after 3 successes
    var b2 = CircuitBreaker::HalfOpen { successes: 0 };
    for i in 0..3 {
        b2.step(CircuitBreakerEvent::Success);
    }
    match b2 {
        CircuitBreaker::Closed { failures } => assert(failures == 0),
        _ => assert(false),
    }
}
```

---

## §10 EBNF grammar

The following productions extend the Hew grammar (§10 of HEW-SPEC-2026.md).

```ebnf
(* Machine declarations — added to Item production *)
Item           = ... | MachineDecl ;

MachineDecl    = "machine" Ident TypeParams? WhereClause? "{"
                   EventsHeader
                   [ EmitsHeader ]
                   { StateDecl }
                   { TransitionDecl }
                   [ DefaultArm ]
                 "}" ;

EventsHeader   = "events" "{" { EventDecl } "}" ;

EventDecl      = Ident ( ";" | "{" { StructFieldDecl } "}" ";"? ) ;

EmitsHeader    = "emits" "{" { Ident ";" } "}" ;

StateDecl      = "state" Ident ( "{"
                   { StructFieldDecl }
                   [ "entry" Block ]
                   [ "exit"  Block ]
                   { CompositeMember }       (* depth-1 composite only *)
                   { TransitionDecl }        (* parent-level rules *)
                 "}" )? ";"? ;

CompositeMember = [ "initial" ] StateDecl ;  (* exactly one "initial" required *)

TransitionDecl = "on" Ident [ "(" Ident { "," Ident } ")" ] ":"
                 TransitionSource "=>" TransitionTarget
                 [ "reenter" ] [ "when" Expr ] TransitionBody ;

DefaultArm     = "default" "{" Body "}" ;
                 (* Body is a brace-balanced expression; `state` is canonical,
                    `self` is also accepted by the parser *)

TransitionSource = Ident | "_" ;            (* dotted Composite.Leaf strips to Leaf *)

TransitionTarget = Ident | "_" ;

TransitionBody = ";" | "{" FieldInits "}" | Block ;
```

`events`, `emits`, `reenter`, and `initial` are contextual keywords valid only
inside a machine body; they do not reserve global identifiers. Where
`StructFieldDecl`, `TypeParams`, `WhereClause`, `Block`, `Expr`, and `Ident` are
as defined in the base Hew grammar.

A `state` whose body contains substate declarations (`CompositeMember`) is a
**depth-1 composite**. It desugars entirely at the parser/AST level to the flat
state and transition lists: members become flat states, a parent-level
`on E: _ => T` expands to one concrete-source transition per member, and
composite `entry`/`exit` hooks splice into boundary-crossing transition bodies
(Harel ordering). A substate that itself contains substates (depth > 1) is
rejected with a v0.6 diagnostic.

---

## §11 Future work

The following features are explicitly deferred to future editions:

1. **Depth > 1 nesting** — composites containing composites. Depth-1 composite
   state blocks are supported; deeper nesting (depth > 1) is reserved for a
   future edition.
2. **History states** — returning to a previously active sub-state. `history`
   may be reserved as a contextual keyword but is not yet lowered.
3. **Composite-scoped auto-fill** — auto-synthesised identity fill for uncovered
   intra-composite cells (use the `_ => _ { state }` one-liner in the interim).
4. **Union-source `A | B =>`** shared events — write the rule per source or use a
   composite parent rule.
5. **Timeout events** — compiler-generated events triggered by elapsed time.

Guard conditions (`when`), inline state `entry {}` / `exit {}` blocks, the
`default { state }` arm, depth-1 composite states, and the `=>` / `events {}` /
`emits {}` / `reenter` / head-binding surface are implemented. Visualization
is available via the CLI: `hew machine diagram` emits Mermaid by default,
`--format graphviz` emits Graphviz DOT, and `--format json` emits a tooling
schema — all three render depth-1 composite nesting.

---

## §12 References

- [1] Pony Language Tutorial — [tutorial.ponylang.io](https://tutorial.ponylang.io)
- [2] Erlang Supervision Principles — [erlang.org](https://www.erlang.org/doc/design_principles/sup_princ.html)
- [3] Statecharts — David Harel, 1987
- [4] Hew Language Specification (edition 2026) — `docs/specs/HEW-SPEC-2026.md`
- [5] RFC: First-Class State Machines in Hew — `examples/machine-design.md`
