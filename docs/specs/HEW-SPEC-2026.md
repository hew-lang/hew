# Hew Language Specification — Edition 2026

Hew is a **high-performance, network-native, machine-code compiled** language for building long-lived services. Its design is anchored in four proven pillars:

- **Actor isolation + compile-time data-race freedom** (Pony-style capability discipline) ([tutorial.ponylang.io][1])
- **Fault tolerance via supervision trees** (Erlang/OTP restart semantics) ([Erlang.org][2])
- **Structured concurrency with cooperative cancellation** (Swift-style model) ([docs.swift.org][3])
- **Wire contracts with enforced schema evolution rules** (Protobuf best practices) ([protobuf.dev][4])

This document is the **normative specification for edition 2026**. It
covers goals, core semantics, type/effects model, module and trait systems,
memory management, `machine` types, runtime state machines, compilation
model, and an EBNF grammar sufficient to implement a working compiler and
runtime.

### Editions and release alignment

Hew tracks **two version axes** that move independently:

- **Compiler version** is SemVer on the binary (`hew --version` → `hew
  0.5.x`). Patch releases for soundness and codegen fixes; minor releases
  for new stdlib surfaces and new language editions; the major release is
  the v1.0 stability event.
- **Spec edition** is a year-shaped identifier declared once per package in
  `Hew.toml` as `edition = "2026"`. The first stabilised edition is
  **2026**. Editions are cadence-free: the next edition lands when
  accumulated breaking changes are worth a migration, expected every two
  to three years.

A compiler binary will advertise its supported editions once this
surface ships (planned, tracked for post-rc1):

```
$ hew --supported-editions
2026
```

A package on edition 2026 continues to compile under future compiler
versions for as long as `2026` remains in the supported list. Inside a
single edition, the language is **additive** — new stdlib modules and new
type-system features that compile old code unchanged land in minor
compiler releases. Anything that would reject previously-accepted code is
an edition-breaking change and waits for the next edition.

Hew does not adopt per-file edition pragmas. The edition stamp is
package-level. Migration tooling (`hew migrate --edition <year>`) is
planned as the supported path between editions once a second edition
exists (tracked for post-rc1) — it is not yet implemented. Cross-edition
dependencies are a future-edition feature.

Surfaces that have been designed but are not normative in edition 2026
live in `HEW-FUTURE.md` with explicit version targets. See the non-normative
Changelog at the end of this document for historical context.

---

## 1. Design goals

### 1.1 Non-goals

- No reflection-based runtime metaprogramming.
- No global shared mutable state.
- No “ambient” threads; all concurrency is via actors and structured tasks.
- No user-defined operator overloading (keeps parsing/tooling simple).

### 1.2 Primary goals

1. **Safety without performance tax**: prevent data races by construction; compile to efficient native code.
2. **Resilience as a language feature**: supervision/restart is standard, not a framework. ([Erlang.org][2])
3. **Network-native by default**: wire types, compatibility checks, and backpressure are first-class. ([protobuf.dev][5])
4. **Operational correctness**: bounded queues, explicit overflow policy, cooperative cancellation. ([docs.swift.org][3])

---

## 2. Core programming model

### 2.1 Units of execution

- **Actor**: isolated, single-threaded state machine with a mailbox.
- **Task**: structured concurrent work _within_ an actor, cancellable via scope.

**Implementation note:** The Rust runtime (`hew-runtime`) provides actor mailboxes using pthread-based synchronization internally. The final link step requires `-lpthread`.

Rules:

- An actor processes **one message at a time** (no intra-actor races).
- Actors do not share mutable state. They communicate by sending **messages**.
- All actors are isolated by definition in Hew's actor model. No separate `isolated` modifier is needed — isolation is a fundamental property of all actors.

### 2.1.1 Actor Message Protocol

Actors expose message handlers using `receive fn`. Named actor `receive fn` methods are callable directly — no `.send()` or `.ask()` required.

Named actor `receive fn` methods are called directly — there is no `.send()` or `.ask()` call site. The distinguishing axis is the callee's `receive fn` signature: a `receive fn` without a return type produces a fire-and-forget call (type `()`); a `receive fn` with a return type `R` produces a request-reply call (type `Result<R, AskError>`). The checker derives the call kind from the callee's signature — no caller-side keyword is needed, because `LocalPid<T>` is a distinct nominal type that already encodes the mailbox boundary.

The token `ask` does not appear at actor call sites. Request-reply against a named actor is written `await <ref>.<method>(<args>)` and has result type `Result<R, AskError>`. Fire-and-forget is written `<ref>.<method>(<args>)` (no `await`) and has type `()`. `ask` is not lexer-recognised at any position in edition 2026 (reserved for a future syntactic marker; see §4.11.1 and HEW-FUTURE).

```hew
actor Counter {
    var count: i64 = 0;

    // Fire-and-forget: no return type, caller does not await
    receive fn increment(n: i64) {
        count += n;
    }

    // Request-response: has return type, caller must await
    receive fn get() -> i64 {
        count
    }

    // Internal method - not accessible to other actors
    fn validate(n: i64) -> bool {
        n >= 0
    }
}
```

- `receive fn` declares a message handler (entry point for actor messages)
- `fn` declares a private internal method
- **`receive fn` without return type** → fire-and-forget. The method call returns `()` and the caller does not need `await`. The message is enqueued and the caller continues immediately.
- **`receive fn` with return type** → request-response. The call produces `R` and waits for the reply. Inside `select`/`join`, the actor call is treated as an implicit concurrent reply source; writing `await` there is accepted but redundant.

**Calling named actors:**

```hew
let counter = spawn Counter(count: 0);

// Fire-and-forget: no return type, no await needed
counter.increment(10);

// Request-response: has return type, requires await
let n = await counter.get();
```

**Sending messages:**

Lambda actors receive messages via call-syntax. Named actors expose typed receive methods:

```hew
// Lambda actor: call the handle directly
let worker = actor |msg: i64| { println(msg * 2); };
worker.send(42);                // fire-and-forget

// Named actor: use the receive method
counter.increment(10);
```

Fire-and-forget sends enqueue the message and return `()`. Named-actor request-response
uses `await` on the receive method (see §2.1.4).

**Actor instantiation:**

Actors are instantiated using the `spawn` keyword with constructor arguments matching the actor's `init` block parameters:

```hew
// Spawn with named field arguments
let counter = spawn Counter(count: 0);

// Spawn with no arguments (if actor has no-arg init or no init block)
let worker = spawn WorkerActor();
```

> **Note:** Named actor spawn always uses parenthesized arguments, even when empty. This is distinct from lambda actor syntax, which uses `actor |params| { body }`.

Actor behaviours can also be defined via traits:

```hew
trait Pingable {
    fn ping() -> string;
}

actor Pinger: Pingable {
    receive fn ping() -> string {
        "pong"
    }
}
```

### 2.1.2 Periodic Receive Handlers

Receive handlers can be annotated with `#[every(duration)]` to create periodic timers that fire automatically at a fixed interval:

```hew
actor HealthChecker {
    let endpoint: string;
    var failures: i64;

    #[every(5s)]
    receive fn check() {
        // Called automatically every 5 seconds
        failures = failures + 1;
    }

    receive fn get_failures() -> i64 {
        failures
    }
}
```

**Rules:**
- The `#[every]` attribute takes a single duration literal argument (e.g. `5s`, `100ms`, `1m`)
- Periodic handlers must not have parameters (they receive no message payload)
- Periodic handlers must not have a return type (fire-and-forget)
- The timer starts when the actor is spawned and repeats until the actor stops
- Periodic handlers run within the actor's message loop, preserving single-threaded semantics

**Implementation:** The runtime uses a global timer wheel to schedule periodic self-sends. Each tick fires a zero-payload message to the actor's dispatch function at the handler's message index.

### 2.1.3 Lambda Actors

Lambda actors provide lightweight, inline actor definitions:

```hew
// Basic lambda actor
let worker = actor |msg: i64| {
    println(msg * 2);
};

// With state capture (move semantics)
let factor = 2;
let multiplier = actor move |x: i64| {
    println(x * factor);
};
```

**Syntax:**

```ebnf
LambdaActorExpr = "actor" "move"? "|" LambdaParams? "|" RetType? Block ;
ActorSpawn      = "spawn" Ident TypeArgs? "(" FieldInitList? ")" ;  (* spawn Counter(count: 0) *)
```

**Type system:**

A lambda actor expression evaluates to a `LambdaPid<M, R>` handle — a PID-like
handle in the same family as `LocalPid` / `RemotePid` ("a pid you ask, `M` in →
`R` out"), where:

- `M` is the message type (from the parameter list: a single param's type, a
  tuple of the param types for multiple params, or `()` for no params)
- `R` is the reply type (from the `-> R` annotation, or `()` when omitted)

`LambdaPid<M, ()>` is send-shaped (fire-and-forget); `LambdaPid<M, R>` with a
non-unit `R` is ask-shaped (request-response). `LambdaPid` is a move-only
resource handle: it is `Send`/`Sync` iff both `M` and `R` are `Send`, and the
runtime stops the actor when the last strong handle drops.

A lambda actor is an actor, not a channel: `LambdaPid` exposes only the actor
surface (`handle(msg)` call-syntax, `.send(msg)`, `.close()`). It deliberately
has no `.recv()` / `.send_half()` / `.recv_half()` surface — the caller never
reads the actor's mailbox, and an actor handle cannot be split in two. The reply
for an ask-shaped actor is delivered through the call-site `Result`, never a
separate receive.

**Spawning:**

```hew
// Named actor spawn returns LocalPid<ActorType>
let counter: LocalPid<Counter> = spawn Counter(count: 0);

// Lambda actor expression returns LambdaPid<M, R>
let worker: LambdaPid<i64, ()> = actor |msg: i64| { println(msg); };   // send
let adder: LambdaPid<i64, i64> = actor |x: i64| -> i64 { x + 1 };      // ask
```

**Capture semantics:**

- Variables from enclosing scope can be captured
- Captured values must implement the `Send` trait
- Use `move` keyword to transfer ownership of captures
- Without `move`, copyable values are copied, non-copyable values cause an error

**Operations:**

```hew
// Fire-and-forget send
worker.send(42);

// Named actor request-response
let result = await counter.get();
```

**Integration with `scope` blocks (normative):**

Lambda actors spawned within a `scope` block are **scope-owned** by that block, but are NOT integrated with the block's task cancellation or trap propagation:

```hew
scope {
    let worker = actor |x: i64| { ... };
    worker.send(1);
}  // worker stopped when the scope-block exits
```

Specifically:

- When a scope-block exits, all lambda actors spawned within it are sent a stop signal (equivalent to `actor_stop`).
- Sibling-cancellation triggered by a child failure does NOT cancel lambda actors — it only cancels structured child tasks (`fork name = expr`).
- A trap within a lambda actor does NOT propagate to sibling tasks or the enclosing scope-block — the actor fails independently.
- For failure propagation across actors, use supervision trees (Section 5), not structured concurrency.

**Limitations:**

- Lambda actors handle a single message type (use full `actor` declaration for multiple)
- Lambda actors cannot implement traits
- Lambda actors cannot be named children in supervisor declarations

### 2.2 Failure model

- Functions do not throw exceptions for control flow.
- Recoverable failure is modeled as `Result<T, E>`.
- Unrecoverable failure is modeled as **trap** (panic). A trap:
  - terminates the current actor
  - is observed by its supervisor
  - may trigger restart per policy

Actors may declare `#[max_heap(N)]` to cap their per-actor arena. If an arena allocation would exceed that cap, the runtime fails closed with the `ExitReason::HeapExceeded` crash variant and the `HEW_TRAP_HEAP_EXCEEDED` trap-kind discriminator. Supervisors receive that heap-exhaustion payload through the same crash-report routing path as other traps, so restart policy, escalation, and `#[on(crash)]` observation all see the cap breach as an unrecoverable actor failure rather than a recoverable `Result`.

> **Error propagation:** `Result<T, E>` and `Option<T>` are first-class. User functions may return either type and use `?` for propagation. The `?` operator is available in any function whose return type is `Result` or `Option` with a compatible error type.

### 2.2.1 Error Propagation

The `?` operator propagates errors from `Result` and `Option` types:

```hew
fn read_file(path: string) -> Result<string, string> {
    let handle = open(path)?;  // Early return on error
    let content = read(handle)?;
    Ok(content)
}
```

When a `receive fn` message handler returns `Err`, the error is:

1. Logged to the actor's supervision context
2. Returned to the caller (if request-response pattern)
3. May cause trap if unhandled and configured to do so

### 2.2.2 Bind-and-Propagate Sugar (`let r? = expr`)

The `?`-suffix on a `let` binding is syntactic sugar for placing `?` on the
right-hand side:

```
let r? = expr;          ≡  let r = expr?;
let r?: T = expr;       ≡  let r: T = expr?;
```

**Rules:**

- The binding name must be a simple identifier. Complex patterns such as
  `let (a, b)? = …` or `let Some(x)? = …` are not valid — the sugar requires
  a single name to anchor the unwrapped value.
- An initialiser is required. `let r?;` with no `= expr` is a parse error.
- The expression `expr` must evaluate to `Result<T, E>` or `Option<T>`.
  Any other type is a type error (`InvalidOperation`), identical to the
  diagnostic produced by a bare `expr?` on a non-Result/Option expression.
- The enclosing function must return `Result<_, E>` or `Option<_>` with a
  compatible error type. If it does not, the checker reports the same
  "`?` cannot be used in a function returning …`" diagnostic as for bare `?`.
- The type annotation `T` in `let r?: T = expr` describes the *unwrapped*
  Ok-payload (type of `r` after propagation), not the Result itself — the
  same convention as `let r: T = expr?;`.

**Desugaring is canonical.** The form `let r = expr?;` is the lowered
representation. A formatter may rewrite `let r? = expr;` to the desugared
form; both representations carry identical semantics.

**Motivation.** The common `let x = (await call())?;` pattern requires
disambiguating parentheses because `await call()?` would parse as
`await (call()?)`, yielding a doubly-wrapped type. The sugar eliminates the
paren cluster and places the propagation marker next to the binding name,
where the reader's eye is focused:

```hew
// Before
let reply = (await server.compute(input))?;

// After
let reply? = await server.compute(input);
```

Both `let r? = expr;` and `let r = expr?;` remain valid; existing code is
unaffected.

---

## 3. Types, ownership, and sendability

### 3.1 Type categories

- **Value types** (copy): integers, floats, bool, char, small fixed aggregates.
- **Owned heap types**: `string`, `bytes`, `Vec<T>`, `HashMap<K,V>`, user-defined types.
- **Shared immutable types**: `Frozen` values are the conceptual shared-immutable category. The runtime has internal `Arc`/ABI support, but no user-facing `Arc<T>` type is exposed (HEW-FUTURE §2.3).
- **Actor references**: `LocalPid<A>` is sendable.
- **I/O stream types**: `Stream<T>` (readable) and `Sink<T>` (writable) — move-only, `Send`, first-class sequential I/O handles (§6.5).

### 3.2 Mutability

- Bindings are immutable by default: `let`.
- Mutable bindings: `var`.
- Type fields have no mutability qualifier. Field mutation is governed by the binding: a `var`-bound value allows `p.x = …`; a `let`-bound value rejects it.

### 3.3 Sendability / isolation rule

A value may cross an actor boundary only if it satisfies **Send**.

`Send` is satisfied if one of the following holds:

- the value is a value type (integers, floats, bool, char), or
- the value is **owned** and transferred (move) with no remaining aliases, or
- the value is `Frozen` (deeply immutable), or
- the value is an actor reference (`LocalPid<A>`)

This is the central compile-time guarantee: **no data races without locks**, aligning with capability-based actor safety in Pony. ([tutorial.ponylang.io][1])

#### 3.3.1 Automatic Derivation Rules

The compiler automatically determines `Send` and `Frozen` for user-defined types. Users do NOT manually implement these traits.

**Send derivation:**

| Type                               | `Send` if...                               |
| ---------------------------------- | ------------------------------------------ |
| Value types (i32, f64, bool, char, isize, usize) | Always `Send`                |
| `string`                           | Always `Send` (immutable-shareable owned type; alias-shared by refcount retain on send — not deep-copied) |
| `LocalPid<A>`                      | Always `Send`                              |
| `type S { f1: T1; f2: T2; ... }`   | All fields are `Send`                      |
| `enum E { V1(T1), V2(T2), ... }`   | All variant payloads are `Send`            |
| `Vec<T>`                           | `T` is `Send`                              |
| `HashMap<K, V>`                    | `K` and `V` are both `Send`                |
| `Option<T>`                        | `T` is `Send`                              |
| `Result<T, E>`                     | `T` and `E` are both `Send`                |
| `(T1, T2, ...)`                    | All elements are `Send`                    |
| `[T; N]`                           | `T` is `Send`                              |

> **Note on array annotations:** Only fixed-size array annotations `[T; N]` are supported. Slice annotations `[T]` (unsized) are rejected by the type checker; unsized-slice lowering is not implemented. Use `Vec<T>` for dynamically-sized sequences.

**Frozen derivation:**

| Type                          | `Frozen` if...                            |
| ----------------------------- | ----------------------------------------- |
| Value types                   | Always `Frozen`                           |
| `string`                      | NOT `Frozen` (mutable content)            |
| `LocalPid<A>`                 | Always `Frozen` (identity reference only) |
| `type S` where all field types are `Frozen` | `Frozen` (recursive over field types) |
| `type S` where any field type is not `Frozen` | NOT `Frozen`                        |
| `enum E`                      | All variant payloads are `Frozen`         |
| `Arc<T>`                      | _Not currently surfaced in Hew source; runtime-only support today_ |
| `Vec<T>`, `HashMap<K,V>`      | NOT `Frozen` (mutable containers)         |

> **Soundness requirement:** The compiler MUST reject as non-`Send` any type whose `Send` status cannot be determined (e.g., opaque foreign types). Foreign types are non-`Send` by default. The runtime/ABI has internal escape hatches, but there is currently **no surfaced Hew `#[send]` attribute** for user code.

### 3.3.2 The `bytes` Type

`bytes` is a built-in compiler type with stdlib-registered methods: a mutable, heap-allocated byte buffer — semantically a `Vec<u8>` — but with a dedicated type name:

```hew
let buf: bytes = bytes::new();
buf.push(0x48);    // push a byte value (i64)
buf.push(72);      // same as 'H' in ASCII
let n = buf.len(); // i64
let b = buf.get(0); // i64 — first byte
buf.set(1, 0xFF);   // overwrite byte at index 1
let last = buf.pop(); // i64 — removes and returns last byte
println(buf.is_empty()); // bool
println(buf.contains(72)); // bool — linear scan
```

**Methods on `bytes`:**

| Method         | Signature          | Description                     |
| -------------- | ------------------ | ------------------------------- |
| `bytes::new()` | `() -> bytes`      | Create an empty byte buffer     |
| `.push(b)`     | `(i64) -> ()`      | Append a byte                   |
| `.pop()`       | `() -> i64`        | Remove and return the last byte |
| `.get(i)`      | `(i64) -> i64`     | Get the byte at index `i`       |
| `.set(i, b)`   | `(i64, i64) -> ()` | Overwrite the byte at index `i` |
| `.len()`       | `() -> i64`        | Number of bytes                 |
| `.is_empty()`  | `() -> bool`       | True if len is 0                |
| `.contains(b)` | `(i64) -> bool`    | True if the buffer contains `b` |

`bytes` is an owned heap type and follows the same ownership rules as `Vec<T>` — it is automatically freed when it goes out of scope. It satisfies `Send`. At the runtime level, owned `bytes` values are treated as **immutable-shareable**: the runtime alias-shares them by refcount retain rather than deep-copying on send. A COW write-barrier (`ensure_unique`) forks the backing buffer before any in-place mutation when the refcount is greater than one, so actor isolation is preserved even when two actors hold retained references to the same buffer.

### 3.4 Ownership and References

**Design Decision: No Intra-Actor Borrow Checker**

Hew does **not** have a borrow checker within an actor. This is a deliberate design choice, not a simplification to be added later.

#### 3.4.1 Rationale

The Rust borrow checker exists to prevent data races in concurrent code. Data races require:

1. Two or more threads accessing the same memory
2. At least one thread writing
3. No synchronization

In Hew, actors are **single-threaded** and process **one message at a time**. This means:

- There is only ever one thread of execution within an actor
- Mutable aliasing cannot cause data races
- Multiple mutable references to the same data are **safe**

Therefore, within an actor, values behave like a normal single-threaded language (Python, JavaScript, single-threaded C). You can have multiple references to mutable data without restriction.

#### 3.4.2 Comparison to Other Languages

| Language         | Approach                                                   | Why                                            |
| ---------------- | ---------------------------------------------------------- | ---------------------------------------------- |
| **Rust**         | Full borrow checker everywhere                             | Prevents races in arbitrary threaded code      |
| **Pony**         | Capability system (iso, ref, val, etc.)                    | Fine-grained control for lock-free concurrency |
| **Swift actors** | No borrow checker within actor, `Sendable` for cross-actor | Same rationale as Hew                          |
| **Hew**          | No borrow checker within actor, `Send` for cross-actor     | Single-threaded actors make it unnecessary     |

Rust's approach is overkill for single-threaded code. Pony's capability system is powerful but adds complexity that provides no benefit when each actor is single-threaded. Hew follows Swift's pragmatic approach: enforce safety only where it matters (actor boundaries).

##### Hew vs Rust Ownership

| Scenario                        | Rust                  | Hew                               |
| ------------------------------- | --------------------- | --------------------------------- |
| Two `&mut` to same data         | ❌ Compile error      | ✅ Allowed (within actor)         |
| Send non-Send across threads    | ❌ Compile error      | ❌ Compile error (actor boundary) |
| Borrow checker overhead         | Always on             | Only at actor boundaries          |
| Lifetime annotations            | Required              | Never needed                      |
| Passing `&mut` to helper fn     | Requires borrow rules | ✅ Unrestricted (same actor)      |
| Storing refs to state in locals | Lifetime constraints  | ✅ Unrestricted (same actor)      |

#### 3.4.3 Binding vs. Ownership

`let` and `var` are **binding modes**, not ownership annotations:

```hew
let x = 5;       // immutable binding - cannot reassign x
var y = 5;       // mutable binding - can reassign y
y = 10;          // ok
// x = 10;       // compile error: cannot reassign immutable binding
```

This is similar to JavaScript's `const`/`let` or Swift's `let`/`var`. It controls whether the _binding_ can be reassigned, not whether the underlying data is mutable.

For type fields:

```hew
type Point {
    x: i64;
    y: i64;
}

var p = Point { x: 0, y: 0 };
p.x = 10;        // OK — p is var-bound, so field mutation is allowed

let q = Point { x: 0, y: 0 };
// q.x = 10;     // compile error — q is let-bound
```

**Type field syntax:**

Type fields do NOT require a `let`/`var` prefix. Fields are immutable and use semicolons as terminators:

```hew
type Point {
    x: f64;          // field declaration
    y: f64;          // field declaration
    label: string;   // field declaration
}
```

**Actor field syntax:**

Actor fields require a `let` or `var` prefix and use semicolons as terminators:

```hew
actor Counter {
    var count: i64 = 0;     // mutable field with default
    let name: string;        // immutable field, set by init
}
```

A `let` (or bare) actor field is immutable after construction: it may be assigned only inside the `init { }` block, where its initial value is established. Any assignment to a `let` field from a `receive fn`, a plain actor method, or a lifecycle hook is rejected at check time with a diagnostic that names the field and suggests declaring it with `var`. A `var` field is mutable and may be assigned anywhere in the actor body.

This distinction exists because actor fields are stateful (they change over the actor's lifetime) and use initialization syntax similar to variable declarations, while type fields are data layout declarations.

#### 3.4.4 The Boundary Rule: Move on Send

The **only** ownership constraint is at actor boundaries. When a value crosses an actor boundary (via method call or `.send()`), it must be **moved** or **cloned**:

```hew
type Message { body: string }

actor Handler {
    receive fn process(message: Message) {
        println(message.body);
    }
}

actor Forwarder {
    receive fn forward(message: Message, target: LocalPid<Handler>) {
        target.process(message);  // message is MOVED to target's mailbox
    }
}

fn main() {
    let handler = spawn Handler();
    let forwarder = spawn Forwarder();
    forwarder.forward(Message { body: "hello" }, handler);
}
```

> **Note:** Throughout this specification, "sending a message" refers to invoking a `receive fn` method on an actor (for named actors) or calling `.send()` on a lambda actor handle.

> **Send semantics (language vs runtime):**
>
> - **Language level** (unchanged): A method call or `.send()` moves the value — the sender can no longer use it.
> - **Runtime level** (gated): the send mechanism is selected based on the value's admissibility class:
>   - **Immutable-shareable** types (`string`, `bytes`): alias-shared by **refcount retain** — the receiver gets a retained reference to the same backing buffer; no byte copy occurs; a COW write-barrier (`ensure_unique`) forks the buffer before any subsequent mutation, preserving actor isolation.
>   - **Mutable collections** (`Vec<T>`, `HashMap<K,V>`, `HashSet<T>`): **deep-copied** into the receiver's per-actor heap.
>   - **Consumed-linear (`iso`/Linear) values**: zero-copy **ownership move** (P6 — not yet surfaced in edition 2026).
> - The send-admissibility gate is `is_immutable_shareable || consumed-Linear || deep-copy`. Note: bare `copy` does **not** imply sendability — a type may derive `Copy` yet hold non-send internals (`copy ⊥ sendable`, B-INV-3). `View`/borrow (`&T`) is rejected across actor boundaries and is not `Send`.
> - From the programmer's perspective the gated model is indistinguishable from move-then-independent-value: the receiver observes an independent value; the sender cannot use the value after send. Alias-sharing is a runtime optimization valid precisely because immutable-shareable values are never mutated in place through shared aliases.

**Why move semantics?**

- The receiving actor may be on a different thread (in the runtime)
- Two actors cannot share mutable state (this is the core safety guarantee)
- Move ensures the sender relinquishes the value

**Cloning for continued use:**

Hew provides two syntactic forms for duplication:

- `val.clone()` — method call form; equivalent to invoking the `Clone` trait.
- `clone val` — prefix expression form; contextual keyword at unary precedence.
  Binds tighter than binary operators: `clone a + b` is `(clone a) + b`.
  Only acts as a prefix when followed by an operand token (identifier or
  literal); otherwise `clone` is a plain identifier, so `clone(args)` and
  `x.clone()` are unaffected.

```hew
type Message { body: string }

actor Handler {
    receive fn process(message: Message) {
        println(message.body);
    }
}

actor Broadcaster {
    receive fn broadcast(message: Message, first: LocalPid<Handler>, second: LocalPid<Handler>) {
        first.process(message.clone());
        second.process(message.clone());
    }
}

fn main() {
    let first = spawn Handler();
    let second = spawn Handler();
    let broadcaster = spawn Broadcaster();
    broadcaster.broadcast(Message { body: "hello" }, first, second);
}

// Lambda actor .send() uses the same move-on-send rule.
```

#### 3.4.5 Capturing Values in Lambda Actors

When spawning a lambda actor, captured variables follow these rules:

**Without `move` keyword:**

- Values implementing `Copy` are copied
- Non-`Copy` values cause a compile error

**With `move` keyword:**

- All captured values are moved into the actor
- Values must implement `Send` (see §3.3)

```hew
let config = load_config();        // Config is not Copy

// Without move: compile error (config cannot be copied)
// let worker = actor |msg: Msg| { use(config); };

// With move: config is moved into the actor
let worker = actor move |msg: Msg| {
    use(config);   // ok - config now owned by this actor
};
// config invalid here - it was moved

// Alternative: clone first
let config2 = config.clone();
let worker2 = actor move |msg: Msg| {
    use(config2);
};
```

**Non-Send values cannot be captured:**

```hew
let local_ref = get_local_resource();  // returns a non-Send reference

// Compile error: local_ref does not implement Send
// let worker = actor |msg: Msg| { use(local_ref); };
```

This is enforced at compile time: any captured value in a `spawn` expression must satisfy the `Send` trait.

#### 3.4.6 What IS Allowed (Within an Actor)

```hew
actor Example {
    var data: Vec<i64> = Vec::new();

    receive fn demo() {
        // Multiple mutable references - ALLOWED (single-threaded)
        let ref1 = data;
        let ref2 = data;
        ref1.push(1);
        ref2.push(2);     // ok - no data race possible

        // Aliasing mutable data - ALLOWED
        var x = data;
        var y = x;
        x.push(3);
        y.push(4);        // ok - same actor, sequential execution

        // Returning references to local data - ALLOWED
        let local = Vec::new();
        process(local);   // works because single-threaded
    }
}
```

#### 3.4.7 What is NOT Allowed

```hew
actor Example {
    var data: Vec<i64> = Vec::new();

    receive fn bad_examples(other: LocalPid<Other>) {
        // Sending without move - ERROR (implicit move makes source invalid)
        other.process(data);
        data.push(1);     // compile error: data was moved

        // Using value after send - ERROR
        let msg = Message::new();
        other.process(msg);
        println(msg.content);  // compile error: msg was moved

        // Capturing non-Send value - ERROR
        let local_handle: RawPointer = get_handle();
        let worker = actor |x: i64| {
            use(local_handle);  // compile error: RawPointer is not Send
        };
    }
}
```

#### 3.4.8 Summary

| Context       | Aliasing     | Mutation     | Borrow checking |
| ------------- | ------------ | ------------ | --------------- |
| Within actor  | Unrestricted | Unrestricted | None            |
| Across actors | Not allowed  | N/A          | Move required   |

**Hew's guarantee:** No data races between actors, enforced at compile time through `Send` and move semantics. No runtime overhead, no borrow checker complexity for local code.

---

### 3.5 Module System

Hew uses a file-based module system inspired by Rust:

- **File = module**: Each `.hew` file is a module. The file name is the module name.
- **Directory = namespace**: Directories create nested namespaces.
- **Visibility**: All declarations are private by default. Use `pub` to export.

```hew
// src/network/tcp.hew
// This is module network::tcp

pub type Connection {
    address: string;       // public fields via pub keyword on type
    internal_state: i64;   // fields are named, terminated with semicolons
}

pub fn connect(addr: string) -> Result<Connection, Error> {
    // ...
}

fn helper() {  // private to this module
    // ...
}
```

**Import syntax:**

```hew
import network::tcp;                    // Import module
import network::tcp::Connection;        // Import specific symbol
import network::tcp::{Connection, connect};  // Import multiple
import network::tcp::*;                 // Import all public symbols (discouraged)
```

**Module dot-syntax for standard library:**

When importing a standard library module, the **last segment** of the module path becomes the local alias for the module. All access uses this short name, not the full path:

```hew
import std::net::http;     // Available as "http", not "std::net::http"
import std::fs;            // Available as "fs"
import std::io;            // Available as "io"
import std::text::regex;   // Available as "regex"

// Call module functions with dot-syntax: module.function(args)
let server = http.listen("0.0.0.0:8080");   // Uses short name "http"
let content = fs.read("config.toml");
let exists = fs.exists("output.txt");       // Returns bool
let line = io.read_line();                  // Preferred stdin surface
let re = regex.new("[a-z]+");
let matched = regex.is_match(re, input);    // Returns bool
server.close();
re.close();
```

This provides clean, namespaced access to stdlib functionality. The module name acts as a qualifier, avoiding verbose function names like `hew_http_server_new()`.

| Module             | Example functions                                                                                                                                            |
| ------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `std::net::http`   | `http.listen`, `http.accept`, `http.path`, `http.method`, `http.body`, `http.header`, `http.respond`, `http.respond_text`, `http.respond_json`, `http.close` |
| `std::fs`          | `fs.read`, `fs.write`, `fs.append`, `fs.exists`, `fs.delete`, `fs.size`                                                                                      |
| `std::io`          | `io.read_line`, `io.write`, `io.write_err`, `io.read_all`                                                                                                    |
| `std::os`          | `os.args_count`, `os.args`, `os.env`, `os.set_env`, `os.has_env`, `os.cwd`, `os.home_dir`, `os.hostname`, `os.pid`                                           |
| `std::net`         | `net.listen`, `net.accept`, `net.connect`, `net.read`, `net.write`, `net.close`                                                                              |
| `std::text::regex` | `regex.new`, `regex.is_match`, `regex.find`, `regex.replace`                                                                                                 |
| `std::net::mime`   | `mime.from_path`, `mime.from_ext`, `mime.is_text`                                                                                                            |
| `std::process`     | `process.run`, `process.try_run`, `process.run_argv`, `process.try_run_argv`, `process.start`                                                               |

Predicate functions (`fs.exists`, `path.exists`, `regex.is_match`, `os.has_env`, `mime.is_text`) return `bool`.

**Visibility modifiers:**

Three-tier model — written as a prefix keyword before the item keyword:

| Syntax              | Meaning                                    |
|---------------------|--------------------------------------------|
| `fn foo()`          | Private — visible only within this module  |
| `package fn foo()`  | Package — visible within the same package  |
| `pub fn foo()`      | Public — visible to all modules            |

The same prefix applies to any item kind: `package type`, `package const`, `package actor`, etc.

> **Implementation note:** both boundaries are enforced at every cross-module
> reference site: a private item referenced from outside its module is
> rejected with `E_VISIBILITY_PRIVATE`, and a `package` item referenced from
> outside its package is rejected with `E_VISIBILITY_PACKAGE`.

#### 3.5.1 Directory-form modules (peer-file composition)

A module may span multiple files inside a dedicated directory. When the
compiler resolves `import greeting;` it looks for either:

1. A single file `greeting.hew` beside the importing file (**single-file form**), or
2. A directory named `greeting/` that contains a file `greeting/greeting.hew`
   (**directory form** — the entry file's stem must match the directory name).

In directory form, **every other `.hew` file in that directory is a peer
file**. The compiler parses all peer files and merges their items into the same
module namespace, in deterministic (sorted) order.

```
myapp/
├── main.hew           ← import greeting;
└── greeting/
    ├── greeting.hew         ← entry (dir name == file stem)
    └── greeting_helpers.hew ← peer (merged automatically)
```

`main.hew`:

```hew
import greeting;

fn main() {
    println(greeting.hello() + " " + greeting.target());
}
```

`greeting/greeting.hew` (entry):

```hew
pub fn hello() -> string {
    "Hello"
}
```

`greeting/greeting_helpers.hew` (peer):

```hew
pub fn target() -> string {
    "from a merged directory module!"
}
```

Because both files are merged into the `greeting` namespace, `greeting.hello()`
and `greeting.target()` are both reachable without any extra re-export
statements.

**Rules:**

- The entry file is identified by `dir_name == file_stem` (e.g.
  `greeting/greeting.hew`). If no such file exists, import resolution fails.
- All other `.hew` files at the **top level** of the directory are peer files.
  Sub-directories are not automatically included; they must be imported
  explicitly.
- Peer files are merged in sorted filename order (deterministic across
  platforms).
- A peer file may itself contain `import` statements; those imports are
  resolved recursively.
- Duplicate `pub` names across the entry and its peers are a **compile error**.
  Each public symbol must have a unique name within the merged module.
- Peer files participate in the same module body. Private items remain private
  to the merged module from outside imports, but peer files are not isolated
  from one another.

A working example is at
[`examples/directory_module_demo/`](../../examples/directory_module_demo/README.md).

#### 3.5.2 Module search-path resolution

For imports outside the current source tree, Hew builds an ordered search-path
list and uses the first matching module root. The search order is:

1. In-worktree root — the enclosing Hew checkout root (identified by containing `std/builtins.hew`), when the source file being compiled is inside a checkout. This tier ensures that stdlib resolution is always scoped to the checkout that owns the file.
2. `HEWPATH` — a colon-separated list of module roots, where each entry is the
   parent directory that contains `std/`
3. `HEW_STD` — a direct path to the `std/` directory; Hew uses its parent as the
   module root
4. The installed FHS-style location `<prefix>/share/hew` relative to the `hew`
   binary
5. XDG user data: `~/.local/share/hew`
6. User home dotdir: `~/.hew`
7. System paths: `/usr/local/share/hew`, `/usr/share/hew`
8. A development fallback to the repo root when `std/` exists two levels above
   the binary

`HEWPATH` and `HEW_STD` are the supported user overrides. `hew.toml` does not
configure module search paths.

> **Scope note:** `HEWPATH` affects stdlib module resolution. User-module
> file-import paths use `HEW_STD` and `.adze/packages` instead; they are not
> governed by `HEWPATH`. Unification of `HEWPATH` across both stdlib and
> user-module file imports is planned for a future edition.

For stdlib discovery, the recommended workflow is to generate docs for the
stdlib tree itself:

```sh
hew doc std/
```

This produces an index plus per-module pages for the shipped `std/` sources.

#### 3.5.3 Per-module type namespacing

Types defined in different modules are distinct even if they share a name. A
`Point` defined in `geometry` and a `Point` defined in `graphics` are unrelated
types; the qualified names `geometry.Point` and `graphics.Point` disambiguate
them everywhere — in type annotations, `match` patterns, and aggregate literals.

```hew
import geometry;
import graphics;

let gp: geometry.Point = geometry.Point { x: 0.0, y: 0.0 };
let sp: graphics.Point = graphics.Point { x: 0,   y: 0   };
```

**Import aliasing** resolves ambiguity at the module level:

```hew
import geometry as geo;
import graphics  as gfx;

let p1: geo.Point = geo.Point { x: 1.0, y: 2.0 };
let p2: gfx.Point = gfx.Point { x: 10, y: 20 };
```

Within a module, all locally-defined types are in scope unqualified. The
compiler rejects ambiguous unqualified references when two imports export the
same name; qualifying with the module (or alias) resolves the conflict.

---

### 3.6 Trait System

Traits define shared behaviour that types can implement. Hew has built-in marker traits and supports user-defined traits.

**Trait declaration:**

```hew
trait Display {
    fn fmt(val: Self) -> string;
}

trait Iterator {
    type Item;
    fn next(iter: Self) -> Option<Self::Item>;
}

trait Clone {
    fn clone(val: Self) -> Self;
}
```

**Trait implementation:**

```hew
type Point { x: f64; y: f64 }

impl Display for Point {
    fn fmt(p: Point) -> string {
        f"({p.x}, {p.y})"
    }
}
```

**Built-in marker traits:**

- `Send` - Type can safely cross actor boundaries. Satisfied by:
  - Value types (integers, floats, bool, char)
  - Owned types transferred by move
  - `Frozen` types (deeply immutable)
  - `LocalPid<A>`

- `Sync` - Type is safe to share across concurrent actors without synchronisation. Derived structurally from field types; the compiler determines this automatically. A type is `Sync` if all its fields are `Sync`. Value types are always `Sync`; mutable containers (`Vec<T>`, `HashMap<K,V>`) are not.

- `Frozen` - Type is deeply immutable and thus safely shareable. Implies `Send`. `Frozen ⊇ Send` holds by structural coincidence — every type whose fields are all deeply immutable is also send-admissible — not via an explicit impl chain.
  - Runtime-internal shared immutable handles (the planned `Arc<T>` surface is not yet exposed in Hew source)
  - Types where all fields are `Frozen`

- `Copy` - Type is copied on assignment rather than moved.
  - Only value types (integers, floats, bool, char)
  - Small fixed-size aggregates

- `PartialOrd` - Type supports `<`, `<=`, `>`, `>=` comparisons. In edition 2026, satisfied only by primitive numeric types (`i8`–`i64`, `u8`–`u64`, `f32`, `f64`, `isize`, `usize`, `char`); user-defined types do not derive it automatically.

**Named receivers (Go-style):**

Hew uses **named receivers** instead of a `self` keyword. In trait declarations and `impl` blocks, the first parameter whose type matches the implementing type (or `Self` in traits) is the **receiver**. The receiver is what makes a function callable with dot-syntax:

```hew
trait Display {
    fn fmt(val: Self) -> string;       // val is the receiver (type Self)
}

impl Display for Point {
    fn fmt(p: Point) -> string {       // p is the receiver (type Point)
        f"({p.x}, {p.y})"
    }
}
```

The compiler identifies the receiver by matching the parameter type against the impl target. The parameter name is chosen by the programmer — use short, descriptive names (`p` for Point, `v` for Vec, `iter` for Iterator, etc.).

**Calling methods:**

```hew
let p = Point { x: 1.0, y: 2.0 };
p.fmt();    // desugars to Point::fmt(p) — p is consumed (by-value)
// p is no longer valid here
```

**In `impl` blocks for types/enums (by-value):**

The receiver is passed by value — the receiver is consumed (ownership transfer):

- `fn fmt(p: Point)` takes `p` by value in an `impl` for `Point`
- The caller gives up ownership of the value
- After calling a consuming method, the original binding is no longer usable

**In actor `receive fn` and `fn` methods (bare field access):**

Actors do not use receivers at all. Actor handler methods access fields by their bare names — no prefix, no receiver parameter. The actor's persistent state is implicitly available:

```hew
actor Counter {
    var count: i64 = 0;
    receive fn increment() {
        count += 1;  // bare field access — actor persists after handler returns
    }
}
```

There is no `&self` or `&mut self` syntax — Hew does not have references in its surface syntax (see §3.4.1). The by-value vs implicit-state distinction is determined by the context (`type`/`enum` `impl` vs actor body), not by annotation.

**The `this` keyword (actor self-reference):**

Inside an actor body, the `this` keyword provides a read-only handle to the actor itself:

- Available only inside actor bodies (`receive fn` and `fn` methods)
- Type `LocalPid<Self>` — a sendable handle to the enclosing actor
- Can be passed as a message argument or stored in a field
- Read-only — assignment to `this` is rejected at compile time
- Not valid in `type`/`enum` `impl` blocks or free functions

**Variable shadowing:**

Hew distinguishes three cases of variable shadowing:

- **Same-scope rebinding** — a **hard error**. Declaring a name that is already bound in the same scope is rejected outright:

  ```hew
  fn main() {
      let x = 1;
      let x = 2;  // compile error: variable `x` is already defined in this scope
  }
  ```

- **Outer-scope shadowing of an actor field** — a **hard error**. Actor fields must have unambiguous bare names; a parameter or local variable that shadows a field is rejected:

  ```hew
  actor Example {
      var count: i64 = 0;

      receive fn update(count: i64) {
          // compile error: variable `count` shadows a binding in an outer scope
      }
  }
  ```

- **Outer-scope shadowing of a local variable** — a **warning**. Reusing a name in a nested block is confusing but not ambiguous. The compiler emits a warning and the programmer is encouraged to choose a more descriptive name or prefix the new binding with `_` to suppress the diagnostic:

  ```hew
  fn main() {
      let x = 1;
      if condition {
          let x = 2;  // warning: variable `x` shadows a binding in an outer scope
          println(x);
      }
      println(x);  // still refers to the outer x
  }
  ```

**Shadowing exemptions:** Bindings whose name starts with `_` and for-loop induction variables are silently exempt from all shadowing diagnostics.

**Trait bounds on generics:**

```hew
type Message { body: string }

actor Receiver {
    receive fn accept(message: Message) {
        println(message.body);
    }
}

actor Broadcaster {
    receive fn broadcast(message: Message, first: LocalPid<Receiver>, second: LocalPid<Receiver>) {
        first.accept(message.clone());
        second.accept(message.clone());
    }
}

fn main() {
    let first = spawn Receiver();
    let second = spawn Receiver();
    let broadcaster = spawn Broadcaster();
    broadcaster.broadcast(Message { body: "hello" }, first, second);
}
```

**Trait objects (`dyn Trait`):**

> `dyn Trait` in type position is admitted by the parser and grammar.
> The syntax `dyn TraitBound` is valid and parses without error.
> Type-checking and codegen for trait objects are partial: basic
> dispatch through a `dyn` reference works for simple cases, but
> object-safety enforcement, associated-type bounds, and higher-ranked
> trait bounds remain incomplete. See HEW-FUTURE.md §2.2 for the
> full object-type roadmap. This status note is specific to trait objects;
> machine codegen is live as described in §3.11.

---

### 3.7 Memory Management

Hew uses **per-actor ownership** with RAII-style deterministic destruction. There is **no garbage collector** — no tracing GC, no generational GC, no GC pauses. Memory is managed through ownership, scopes, and reference counting.

#### 3.7.1 Ownership Model

**Principle 1: Actors own their heaps.**
Each actor has a private heap. No memory is shared between actors. When an actor terminates, its entire heap is freed in one operation.

**Principle 2: Ownership within actors.**
Within an actor, values follow Hew ownership semantics:

- Each value has exactly one owner
- When the owner goes out of scope, heap memory is freed automatically
- User-defined `impl Drop` is **not supported** — external-resource cleanup
  uses `#[resource]` + `close()` (see §3.7.3 and §3.7.8)

```hew
#[resource]
type Connection { fd: i64; }

impl Connection {
    fn open(host: string) -> Connection { Connection { fd: 0 } }
    fn close(c: Connection) {}
}

fn example(host: string) {
    let conn = Connection::open(host);
    // ... use conn ...
}  // conn.close() runs here automatically (implicit #[resource] drop)
```

**Principle 3: No garbage collection.**
Hew guarantees no GC pauses. Memory reclamation is entirely deterministic:

- Scope exit triggers drops
- Reference count reaching zero triggers drops
- Actor termination frees the actor's heap

#### 3.7.2 Message Passing Semantics

At the **language level**, `send()` **moves** the value — the sender loses access and cannot use it after sending (see §3.4.4). At the **runtime level**, the send mechanism is **gated** on the value's admissibility class (D355):

| Admissibility class | Runtime mechanism | Examples |
| ------------------- | ----------------- | -------- |
| **Immutable-shareable** (`is_immutable_shareable`) | **Alias-shared by refcount retain** — no byte copy | `string`, `bytes` |
| **Mutable owned collections** | **Deep-copied** into the receiver's per-actor heap | `Vec<T>`, `HashMap<K,V>`, `HashSet<T>` |
| **Consumed-linear (`iso`/Linear)** | Zero-copy **ownership move** (P6 — not surfaced in edition 2026) | — |

The gate is `is_immutable_shareable || consumed-Linear || deep-copy`. Bare `copy` does **not** imply sendability (`copy ⊥ sendable`, B-INV-3). `View`/borrow (`&T`) is rejected across actor boundaries and is not `Send`.

This hybrid gives the safety of Rust's move semantics (no use-after-send bugs) with actor isolation (no shared mutable state between actors).

> **Immutable-shareable alias sharing:** `string` and `bytes` are immutable-shareable owned heap types. When sent, the runtime retains a reference to the same backing buffer for the receiver rather than copying it. The sender's move and the receiver's retained alias both resolve to the same buffer with a shared refcount; the buffer is freed once the last reference drops. The backing buffer is never mutated in place through a shared alias — if a mutation targets a shared (`rc>1`) buffer, the COW write-barrier (`ensure_unique`) forks a private copy before the write, preserving actor isolation. An immutable `let`-bound sendable value is alias-shared with no barrier (never mutated in place).

> **Programmer indistinguishability preserved:** From the programmer's perspective the gated model is indistinguishable from move-then-independent-value. The receiver observes an independent value; the sender cannot use the value after send. Alias-sharing is a runtime optimization valid precisely because immutable-shareable values are never mutated in place through shared aliases.

**Move-on-send:**

- When a message is sent to an actor (via method call or `.send()`), the value is moved at the language level — the sender can no longer use it. At runtime, the mechanism is gated: immutable-shareable values (`string`, `bytes`) are alias-shared by retain; mutable collections are deep-copied; `iso`/Linear values are moved (P6).
- The receiver observes an independent value (alias-sharing is an optimization invisible to program semantics)
- No user-visible references or borrows cross actor boundaries — `&T` (borrow/`View`) is not `Send` and is rejected at the actor boundary entirely; the runtime retain optimization applies only to admissible immutable-shareable **owned** values, and the receiver always observes an independent owned value at the language level, never a borrow into the sender's heap

```hew
type Message { body: string }

actor Handler {
    receive fn process(message: Message) {
        println(message.body);
    }
}

actor Forwarder {
    receive fn forward(message: Message, target: LocalPid<Handler>) {
        target.process(message);  // message is MOVED by the gated send mechanism
    }
}

fn main() {
    let handler = spawn Handler();
    let forwarder = spawn Forwarder();
    forwarder.forward(Message { body: "hello" }, handler);
}
```

**The `Send` trait:**

`Send` is a **marker trait** — it has no methods. A type is `Send` if it is **send-admissible** (can safely cross actor boundaries). The compiler verifies `Send` bounds at compile time.

```hew
trait Send {}  // Marker trait — no methods
```

`Send` is satisfied if one of the following holds (see §3.3):

- The value is a value type (integers, floats, bool, char)
- The value is owned and transferred by move with no remaining aliases
- The value is `Frozen` (deeply immutable)
- The value is a `LocalPid<A>`
- The value is a type/enum where all fields/variants satisfy `Send`

> **Implementation note:** `Send` means send-admissible; the runtime selects the send mechanism based on the value's admissibility class (immutable-shareable → alias-shared by retain; mutable collections → deep-copied; `iso`/Linear → ownership move, P6). The `Send` marker tells the compiler that a type's structure is send-admissible; it does **not** mandate a particular runtime copy strategy. User-defined types do NOT need to implement `Send` explicitly — the compiler derives it automatically based on field types.

**Move semantics within actors:**
Within a single actor, values can be moved (ownership transferred) without copying:

```hew
fn process(data: Vec<u8>) {
    let owned = data;  // move, not copy
    // data is no longer valid
}
```

#### 3.7.3 Deterministic Cleanup

User-defined `impl Drop` is **not supported** and is rejected at compile time.
The checker emits a spanned error on any `impl Drop for T` block:

```
error: `impl Drop` is not supported (its `drop` method would not run);
       use `#[resource]` with a `close()` method for deterministic cleanup,
       or rely on automatic field-wise drop
```

Deterministic cleanup is provided by two mechanisms.

**`#[resource]` + `close()`** — for types that own an external resource and
require explicit release (file descriptors, network sockets, child processes,
etc.):

```hew
#[resource]
type FileHandle {
    fd: i32;
}

impl FileHandle {
    fn close(fh: FileHandle) {
        // release the file descriptor
    }
}
```

The `#[resource]` annotation marks the type as an owned handle. The compiler
calls `close()` at scope exit as the single release path, guaranteeing that the
resource is released exactly once.

**Automatic field-wise drop** — for plain data types, the compiler emits a
recursive drop that frees heap storage (strings, `Vec`, `HashMap`, nested
types) in **reverse declaration order (LIFO)**. No annotation or `impl`
is needed.

**Cleanup guarantees:**

- `close()` (resource types) or field-wise drop runs exactly once per value
- Cleanup runs at a predictable point (scope exit)
- Drop order: fields are released in reverse declaration order (LIFO) — last declared, first dropped
- Nested types are recursively dropped
- `Rc<T>` payloads are released when the refcount reaches zero; unsupported `Rc<T>` payload types are rejected during checking
- All owned values in an actor are cleaned up when the actor terminates; heap is freed after the last field

#### 3.7.4 Indirect Enums (Recursive Data Types)

Enum types cannot normally reference themselves because inline storage would require infinite size. The `indirect` modifier enables recursive data types by heap-allocating enum values behind a pointer.

```hew
indirect enum Expr {
    Lit(i64);
    Add(Expr, Expr);
    Neg(Expr);
}
```

**Semantics:**

- `indirect` applies to the entire enum declaration (not individual variants)
- All variant payloads are heap-allocated; the enum value itself is a pointer
- Construction and pattern matching syntax are identical to regular enums
- Memory is automatically freed when the value goes out of scope (RAII)
- The compiler generates a recursive drop function that walks the data structure

**Construction** works identically to regular enums:

```hew
let e = Expr::Add(Expr::Lit(1), Expr::Neg(Expr::Lit(2)));
```

**Pattern matching** works identically to regular enums:

```hew
fn eval(e: Expr) -> i64 {
    match e {
        Lit(n) => n,
        Add(l, r) => eval(l) + eval(r),
        Neg(inner) => 0 - eval(inner),
    }
}
```

**Restrictions:**

- `indirect` can only be used with `enum` declarations, not `type` declarations
- Only enums that contain self-referential variants benefit from `indirect`

#### 3.7.5 Reference Counting (Rc and Arc)

**`Rc<T>` — single-actor reference counting:**

- Non-atomic refcount (fast, single-threaded)
- Cannot cross actor boundaries (does not implement `Send`)
- Use for shared ownership within one actor
- Current compiler support is fail-closed: `Rc<T>` currently accepts `T: Copy`, `string`, `bytes`, and nested `Rc` of supported payloads until recursive drop lowering exists
- For collection admissibility, the checker currently enforces an internal `RcFree` boundary (current checker behaviour, not stable user-written trait syntax): a type is treated as RcFree only when its resolved structure contains no `Rc<_>` after recursively checking wrapper type arguments, tuples/arrays/slices, and registered named type/enum/actor members, including module-qualified and non-root private definitions seen during checking
- `LocalPid<A>` participates in that structural check through its actor type argument: if actor `A` stores non-RcFree state, `LocalPid<A>` is also non-RcFree for collection checks
- `HashMap` and `HashSet` reject non-RcFree key/value/element types during type checking; `Vec` rejects non-RcFree elements at collection method-call sites (`push`, `pop`, `get`, `remove`, `set`, `append`, `extend`, `map`, `filter`, `fold`) rather than as a bare annotation-level ban

```hew
let data: Rc<string> = Rc::new(expensive_computation());
let alias = data.clone();  // refcount++, no data copy
// data and alias share the same string
```

> See HEW-FUTURE.md §2.3 for the user-facing `Arc<T>` surface — targeted
> for v0.7. The runtime contains internal atomic-refcount machinery, but
> source code has no `Arc<T>` keyword in edition 2026. Cross-actor sharing
> is via owned messages and actor state; the intended invariant is that
> only deeply-immutable (`Frozen`) data is shareable.

**When to use which:**
| Type | Cross-actor? | Send mechanism | Use case |
|------|--------------|----------------|----------|
| Immutable-shareable `T` (`string`, `bytes`) | Yes | Alias-shared by refcount retain | Owned heap types with immutable backing; no byte copy on send |
| Mutable collection `T` (`Vec`, `HashMap`, `HashSet`) | Yes | Deep-copied | Receiver gets an independent copy; sender mutation cannot corrupt receiver |
| `iso`/Linear `T` | Yes (P6) | Zero-copy ownership move | Consumed-linear values; not yet surfaced in edition 2026 |
| `Rc<T>` | No | N/A | Shared within actor only |

#### 3.7.6 Compiler Optimizations (Implementation Details)

The compiler may apply memory optimizations that are **invisible to user
semantics**. Users always see RAII behaviour; optimizations affect only
performance.

- **Arena optimisation for message handlers.** The compiler may allocate
  message-handler temporaries in an arena and bulk-free them when the
  handler returns. The arena path applies only to values that do not
  carry a drop side effect (§3.7.8). For values that do, the destructor
  runs individually.
- **Copy elision.** When sending messages, the compiler may optimise away
  copies if the sender provably does not use the value after send.
- **Escape analysis.** Values that do not escape their scope may be stack-
  allocated rather than heap-allocated.

These optimisations do not change program behaviour. A correct program
produces identical results with or without them.

#### 3.7.7 Memory Safety Guarantees

| Guarantee         | How Hew ensures it                                             |
| ----------------- | -------------------------------------------------------------- |
| No use-after-free | Ownership + move semantics; compiler rejects use after move    |
| No double-free    | Single ownership; `drop()` runs exactly once                   |
| No data races     | No shared mutable state; `Send` requires `Frozen` for sharing  |
| No GC pauses      | No tracing GC; deterministic refcounting and scope-based drops |
| No memory leaks\* | RAII ensures cleanup; cycles in `Rc` can leak (use weak refs)  |

\*Reference cycles in `Rc<T>` can cause leaks. `Weak<T>` is the intended cycle-breaker but is **not yet implemented** — `Weak<T>` is not a registered builtin type, so `Weak::new(...)` is rejected during type-checking as an unknown type before it reaches HIR. Supervision trees naturally avoid cycles by keeping ownership parent-to-child only.

#### 3.7.8 Resource markers (`#[resource]` and `#[linear]`)

Hew provides two type annotations for resources whose lifecycle must be
visible in the type system. Both are single-owner; both interact with the
move-checker so use-after-consume becomes a compile-time error. They
differ in whether dropping the value at scope exit is an implicit,
infallible action.

##### 3.7.8.1 `#[resource]` — single-owner with drop side effect

`#[resource]` marks a type that carries an external resource (file
descriptor, socket, allocator handle, GPU context, libc pointer) and
**must** declare a `close` method in a sibling `impl` block:

```hew
#[resource]
type File { fd: i64 }

impl File {
    fn close(self) {}   // plain self, unit return, sibling impl
}
```

Semantics:

1. **Required `close` method in sibling `impl`.** The compiler errors at
   HIR if `#[resource] T` has no `fn close` in an inherent `impl` block
   (`ResourceMissingClose`). Declaring `close` inline in the type body
   is rejected (`ResourceCloseSourceUnsupported`). The `close` method
   must return unit (`ResourceCloseMustReturnUnit`).
2. **Implicit drop calls `close`.** When the value goes out of scope
   without an explicit close, the compiler emits a drop site that calls
   `close(value)` and discards the returned `Result`. The discard is
   intentional — there is nowhere for the error to propagate at drop time.
3. **Early close is a normal method call.** `f.close()?` consumes `f` and
   surfaces the error via `?` like any other method. Any subsequent use
   of `f` is a use-after-consume diagnostic from Checked MIR.
4. **Affine in the move-checker.** Sends, moves, and method calls with a
   consuming receiver all consume the value; the move-checker tracks the
   single live binding.

Typical example — file I/O with implicit cleanup (illustrative; no `File` type
exists in stdlib — for file reading use `fs::try_read`):

<!-- doctest: skip -->
```hew
fn read_config(path: string) -> Result<Config, IoError> {
    let f = File::open(path)?;
    let bytes = f.read_all()?;
    parse(bytes)
    // f drops at scope exit; the fd is closed automatically.
}
```

Early close, surfacing the I/O error to the caller (illustrative):

<!-- doctest: skip -->
```hew
fn read_and_process(path: string) -> Result<Summary, AppError> {
    let f = File::open(path)?;
    let bytes = f.read_all()?;
    f.close()?;                 // close early; the error is visible.
    Ok(crunch(bytes))           // do CPU work after the fd is released.
}
```

##### 3.7.8.2 `#[linear]` — single-owner with no implicit drop

`#[linear]` marks a type that **must be consumed** by one of its declared
consuming methods. There is no implicit drop. Letting a `#[linear]` value
go out of scope without consuming it is a compile error.

```hew
#[linear]
type Transaction {
    fn commit(consuming self) -> Result<(), DbError>
    fn rollback(consuming self) -> Result<(), DbError>
}
```

Semantics:

1. **No implicit drop.** The compiler does not synthesise a drop call.
   Scope exit with an unconsumed `@linear` value is a
   `MustConsumeAtScopeExit` diagnostic.
2. **Consumption discharges the obligation.** Calling any declared
   consuming method (one whose receiver is `consuming self`) is enough to
   satisfy the must-consume check.
3. **No canonical method name.** The type declares which methods are
   valid consumers. `Transaction` requires `commit` or `rollback`; a
   capability token might require `revoke`; a GPU command buffer might
   require `submit` or `discard`.
4. **Affine in the move-checker.** Same as `@resource`: the move-checker
   tracks the single live binding and rejects any use after the consuming
   method call.

A correct use (illustrative — `Database` and `Transaction` are hypothetical types
showing the `#[linear]` pattern; `&Database` uses reference syntax not in Hew):

<!-- doctest: skip -->
```hew
fn transfer(db: &Database, from: AccountId, to: AccountId, amount: Money)
    -> Result<(), DbError>
{
    let tx = db.begin_transaction()?;
    tx.debit(from, amount)?;
    tx.credit(to, amount)?;
    tx.commit()                 // tx is consumed here.
}
```

The compile error for forgetting to consume (illustrative):

<!-- doctest: skip -->
```hew
fn forgot_to_commit(db: &Database) -> Result<(), DbError> {
    let tx = db.begin_transaction()?;
    tx.debit(account, money)?;
    Ok(())
    // ERROR: `tx` of type Transaction (#[linear]) is not consumed at scope exit.
    //        #[linear] values must be consumed via one of: commit, rollback.
}
```

##### 3.7.8.3 Choosing between `#[resource]` and `#[linear]`

| Question                                                          | Pick           |
| ----------------------------------------------------------------- | -------------- |
| Is "close and discard the error" a sensible default at scope exit? | `#[resource]` |
| Must the caller surface the cleanup result, every time?            | `#[linear]`   |
| Are there multiple distinct ways to consume (commit / rollback / ...)? | `#[linear]`   |
| Is there exactly one cleanup action, and is it idempotent?         | `#[resource]` |

File descriptors, sockets, allocator handles, regex compiled patterns,
HTTP server/request handles — `#[resource]`. Database transactions,
capability tokens, response-body finish protocols where the success path
must be acknowledged, GPU command buffers — `#[linear]`.

##### 3.7.8.4 Interaction with cancellation and supervision

`#[resource]` and `#[linear]` differ in how their obligations interact with
each of the four teardown paths the language exposes. The rules below
are normative; the move-checker (for `#[linear]`) and drop elaboration
(for `#[resource]`) enforce them at the cited stage.

**Path 1 — Lexical task cancellation.** A `scope {}` child is cancelled
at a safepoint (§4.5) while holding the value:

- `#[resource]`: implicit `close()` runs on the cancellation-unwind edge
  of the CFG, in reverse construction order. The result is discarded as
  with any other implicit drop. The cancellation continues to propagate.
- `#[linear]`: the consuming method must appear on every reachable exit
  path including the cancellation-unwind edge. The move-checker reports
  the missing consume at the cancellation site (in practice, at
  definition time of the surrounding function), so the diagnostic fires
  before the program ever runs. There is no "cancellation forgives the
  obligation" rule.

**Path 2 — Graceful actor drain.** The actor's supervisor or
`actor.shutdown()` call runs the actor's terminating handler (`#[on(stop)]`
hooks, or the supervised shutdown handler):

- `#[resource]` fields: each field's implicit `close()` runs in
  **reverse declaration order** after the terminating handler returns. The
  handler may also close them explicitly via `f.close()?` to surface
  errors; an already-closed `#[resource]` is a use-after-consume
  diagnostic on any subsequent reference.
- `#[linear]` fields: the move-checker requires that every reachable exit
  path of the terminating handler consume each `#[linear]` field via one
  of its declared consuming methods. The check runs at actor-declaration
  time; an actor whose declared terminating handler cannot statically
  consume every `#[linear]` field is a compile error.

**Path 3 — Supervised actor crash (handler trap that bypasses the
terminating handler).** A trap raised by any handler restarts the
actor under the supervisor's policy. The terminating handler is *not*
guaranteed to run on this path; only heap teardown is:

- `#[resource]` fields: implicit `close()` runs as part of heap teardown
  during the restart. Errors are discarded per the `#[resource]` contract.
- `#[linear]` fields: a crash bypasses the consume path the move-checker
  relied on in Path 2. Edition 2026 resolves this conservatively: a
  `#[linear]` field is admitted on an actor only when the field's type
  *also* satisfies `#[resource]` semantics, so heap teardown invokes the
  implicit drop on the crash path. A bare `#[linear]` field whose consume
  path can be bypassed by a supervised restart is a compile error at
  actor-declaration time. A future edition may relax this with an
  explicit consuming-handler attribute that the runtime guarantees to
  invoke on crash (see HEW-FUTURE.md §1.7).

**Path 4 — Outer trap propagation.** A trap unwinds through stack
frames holding the value (distinct from cooperative cancellation,
which respects safepoints):

- `#[resource]`: implicit `close()` runs best-effort on the unwind edge;
  drop elaboration places the call on the trap-cleanup path the same
  way it places it on the normal cleanup path. Trap unwind continues.
- `#[linear]`: the consume obligation cannot be satisfied on a trap path
  because a trap is, by definition, unrecoverable. The move-checker
  does not require `#[linear]` consume on trap-only edges; instead, the
  value's storage is reclaimed by the runtime without invoking any
  consuming method. A `#[linear]` type whose author needs trap-safe
  cleanup must additionally satisfy `#[resource]` so its implicit close
  runs on the trap edge.

These four paths are the only teardown paths edition 2026 commits to.
Any new teardown surface added by a future edition (for example,
checkpoint snapshots, hot-swap upgrades) must extend this table before
it is admitted.

##### 3.7.8.5 `#[resource]` close discipline

Two HIR-boundary constraints govern the `close` method on a `#[resource]`
type, and fail closed before any subsequent stage can silently miss a drop:

1. **Inherent-impl-only `close` body.** The body of `close` on a
   `#[resource]` type must be declared in a sibling inherent `impl`
   block:

   ```hew
   #[resource]
   type Conn { fd: i64 }

   impl Conn {
       fn close(c: Conn) { /* release fd */ }
   }
   ```

   Declaring `close` as an inline method inside the type body is rejected
   at HIR with `ResourceCloseSourceUnsupported`. A future edition may
   relax this once inline-method lowering is wired.

2. **Unit return required.** The inherent-impl `close` body must return
   unit. A `close` declared to return `Result<(), E>` (or any non-unit
   type) is rejected at HIR with `ResourceCloseMustReturnUnit`. The
   implicit drop contract dispatches `close` on every scope-exit path
   including `Trap` and `Cancel`; propagating a value off those edges has
   no defined semantics. Fallible cleanup composes through `defer`, where
   the value can be inspected on the success path and surfaced via `?`.

A `#[resource]` declaration with no inherent-impl `close` is rejected at
HIR with `ResourceMissingClose` — the implicit drop contract has no method
to dispatch.

Once these constraints pass, codegen's typed `DropDispatch::{RuntimeSymbol,
UserFn}` dispatcher routes the drop through one of exactly two arms
(runtime substrate symbol or user inherent-impl method); a third path is
rejected by the codegen verifier. The `close` body runs on every exit path
that the unified `ScopeExitPlan` enumerates (per §3.7.8.4 above).

---

### 3.8 Generics and Monomorphization

Hew uses **monomorphization** as its primary strategy for generics, generating specialized code for each type instantiation (like Rust). This ensures zero runtime overhead for generic code while enabling compile-time verification of `Send` and `Frozen` constraints.

#### 3.8.1 Monomorphization Strategy

Generic functions and types are compiled to specialized versions at each call site:

```hew
fn max<T: Ord>(a: T, b: T) -> T {
    if a > b { a } else { b }
}

// Each call generates distinct machine code:
max(42, 17);           // max$i32
max("hello", "world"); // max$string
max(3.14, 2.71);       // max$f64
```

**Benefits:**

- Zero runtime overhead (no vtable dispatch)
- Full inlining and optimization per instantiation
- Compile-time verification of trait bounds

**Trade-offs:**

- Increased binary size (N instantiations → N copies)
- Longer compile times for heavily generic code

#### 3.8.2 Type-Erased Dispatch with `dyn Trait`

> See HEW-FUTURE.md §2.2 for `dyn Trait`, vtable layout, and object-safety
> rules — targeted for v0.6.

#### 3.8.3 Trait Bounds

**Inline bounds:**

```hew
type Message { body: string }

actor Receiver {
    receive fn accept(message: Message) {
        println(message.body);
    }
}

actor Broadcaster {
    receive fn broadcast(message: Message, first: LocalPid<Receiver>, second: LocalPid<Receiver>) {
        first.accept(message.clone());
        second.accept(message.clone());
    }
}

fn main() {
    let first = spawn Receiver();
    let second = spawn Receiver();
    let broadcaster = spawn Broadcaster();
    broadcaster.broadcast(Message { body: "hello" }, first, second);
}
```

**Where clauses for complex bounds:**

```hew
fn merge<K, V>(a: HashMap<K, V>, b: HashMap<K, V>) -> HashMap<K, V>
where
    K: Hash + Eq + Send,
    V: Clone + Send,
{
    // implementation
}
```

**Associated type bounds:**

> See HEW-FUTURE.md §2.2 for `where T::Item: Display`-style associated-
> type bounds — targeted for v0.6.

#### 3.8.4 Associated Types in Traits

Traits can declare associated types that implementors must specify:

```hew
trait Iterator {
    type Item;
    fn next(iter: Self) -> Option<Self::Item>;
}

impl Iterator for RangeIter {
    type Item = i32;

    fn next(iter: RangeIter) -> Option<i32> {
        if iter.current < iter.end {
            let value = iter.current;
            iter.current = iter.current + 1;
            Some(value)
        } else {
            None
        }
    }
}
```

Traits with associated types do not participate in structural trait satisfaction checks (the "E1 guard"). A type can only satisfy a trait with associated types through an explicit `impl` block that provides the required `type Item = ...` declaration. Multi-associated-type traits and the bound-projection surface (`where T: Iterator<Item = i64>` equality-binding works; projection form `where T::Item: Display` is deferred) land in the next edition. See HEW-FUTURE.md §2.2.

#### 3.8.5 Send/Frozen Specialization for Actors

The `Send` and `Frozen` marker traits have special rules for generic types:

**Automatic derivation:**

```hew
// Compiler derives: Point is Send + Frozen + Copy (all fields are)
type Point { x: f64; y: f64 }

// Compiler derives: Container<T> is Send if T is Send
type Container<T> { value: T; }

// MutableContainer has a mutable binding semantics determined by usage
type MutableContainer<T> {
    value: T;
}
```

**Conditional implementations:**

```hew
// Vec<T> is Send if T is Send
impl<T: Send> Send for Vec<T> {}

// Vec<T> is Frozen if T is Frozen
impl<T: Frozen> Frozen for Vec<T> {}
```

The runtime also has internal `Arc` support, but those `Send`/`Frozen` rules are not yet part of surfaced Hew source syntax.

**Actor boundary enforcement:**

```hew
// Error: T might not be Send
receive fn forward_unsafe<T>(message: T, target: LocalPid<Handler<T>>) {
    target.process(message);    // Compile error: T not bounded by Send
}

// Correct: T is bounded by Send
receive fn forward<T: Send>(message: T, target: LocalPid<Handler<T>>) {
    target.process(message);    // OK: T: Send verified at instantiation
}
```

#### 3.8.6 Type Inference

Hew employs **bidirectional type inference** to minimize explicit type annotations while maintaining compile-time type safety. Types flow from calling contexts into lambda expressions, making generic code elegant and natural to write.

**Core principle**: Hew remains **strongly typed** — all types are known at compile time. Inference simply reduces the annotation burden without sacrificing safety.

**Bidirectional inference strategy:**

- **Context flows inward**: Function signatures and explicit annotations provide typing context
- **Lambda parameters infer from context**: When a lambda appears where a specific function type is expected, parameter types are inferred
- **Explicit annotations when ambiguous**: If types cannot be inferred, the compiler requires explicit annotations

> **Note:** Return type inference via `-> _` is **not yet implemented**. A function annotated `fn f(...) -> _ { ... }` is rejected at HIR with `E_HIR: inferred type reached resolved HIR boundary (UnresolvedInferenceVar)`. All function return types must be written explicitly.

**Lambda inference examples:**

```hew
fn apply(f: fn(i64, i64) -> i64, a: i64, b: i64) -> i64 { f(a, b) }

// Closure parameters infer i64 from apply's signature
let sum = apply(|x, y| x + y, 3, 4);      // x: i64, y: i64 inferred
let product = apply(|x, y| x * y, 3, 4);  // types flow from apply's signature

// Method chaining with inference
numbers
    .filter(|x| x > 0)              // x: i64 inferred from Vec<i64>
    .map(|x| x * 2)                 // x: i64, result: i64
    .reduce(|a, b| a + b, 0)        // a: i64 (accumulator), b: i64 (element); seed last
```

`Vec::reduce` takes the combining closure first and the seed second
(`numbers.reduce(|acc, x| acc + x, 0)`). It is `fold` with the argument
order flipped for chain readability — `fold` takes the seed first
(`numbers.fold(0, |acc, x| acc + x)`); both fold left over the elements
with an explicit seed. A seedless `reduce` (first element as the
accumulator) is not provided: it would need an empty-vector answer, and
Hew refuses to invent one.

**Closure syntax:**

Hew uses pipe-delimited closure syntax for first-class function values:

```hew
let doubled = transform(|x| x * 2, 21);
let sum = numbers.reduce(|a, b| a + b, 0);
let checked = |x: i64| -> i64 { x + 1 };
```

**Untyped parameters when context provides types:**

```hew
fn map<T, U>(items: Vec<T>, transform: fn(T) -> U) -> Vec<U> { /* ... */ }

// T=i64, U=string inferred from usage
let strings = map([1, 2, 3], |x| x.to_string());  // x: i64 inferred
```

**Actor message type inference:**

Actor message handlers provide rich typing context:

```hew
actor Calculator {
    var result: i64 = 0;

    // receive fn signature provides context for message arguments
    receive fn apply_operation(op: fn(i64, i64) -> i64, value: i64) {
        result = op(result, value);
    }
}

let calc = spawn Calculator();
// Lambda types inferred from receive fn signature
calc.apply_operation(|a, b| a + b, 10);  // a: i64, b: i64 inferred
calc.apply_operation(|a, b| a * b, 5);   // also inferred
```

**Generic lambda constraints:**

> **Not yet implemented.** Type-parameterized closures (`<T: Bound>(params) => body`) are rejected by the compiler: `E_CLOSURE_PIPE_SYNTAX: '<T>(params) => body' has been removed; type-parameterized closures are not supported`. Lambdas infer their parameter types from calling context; explicit generic parameters on a lambda literal are not available in this edition.

**Ambiguous cases require annotations:**

```hew
// ERROR: Cannot infer types for lambda parameters
let f = |x, y| x + y;  // No context to determine x, y types

// Solution 1: Annotate the variable
let f: fn(i64, i64) -> i64 = |x, y| x + y;

// Solution 2: Annotate parameters
let f = |x: i64, y: i64| x + y;
```

**Constraint solving for complex bounds:**

The type system generates and solves constraints for complex generic hierarchies:

```hew
fn process<T: Send + Clone>(items: Vec<T>, transform: fn(T) -> T) -> Vec<T>
where
    T: Display,
{
    items.map(transform)
}

// All constraints automatically verified:
// - i64: Send ✓, Clone ✓, Display ✓
let results = process([1, 2, 3], |x| {
    print(f"Processing: {x}");  // Display bound allows this
    x * 2
});
```

**Error messages with inference context:**

When inference fails, the compiler provides clear, actionable errors:

```
error[E0282]: type annotations needed for lambda parameters
  --> src/main.hew:5:15
   |
5  |     let f = |x, y| x + y;
   |               ^^^^^^^^^^^^^ cannot infer types for `x` and `y`
   |
help: consider annotating the lambda variable type
   |
5  |     let f: fn(i64, i64) -> i64 = |x, y| x + y;
   |            ++++++++++++++++++
   |
help: or annotate the lambda parameters directly
   |
5  |     let f = |x: i64, y: i64| x + y;
   |                +++     +++
```

**Practical elegance**: This system achieves the design goal of "elegant simplicity" — minimal annotations paired with maximum type safety. Types propagate naturally from function signatures and calling contexts, while the monomorphization backend generates specialized, optimized code for each concrete instantiation.

---

### 3.9 Foreign Function Interface (FFI)

> **In-planning — deferred to v0.6+.** The full FFI surface described in this
> section (`extern "C"` blocks, `#[repr(C)]`, `#[export]`, C-string helpers,
> and safe-wrapper patterns) is not lowered in v0.5. The v0.5 compiler lowers
> only the fixed `hew_*` runtime allowlist (§8.4); any other foreign-call
> declaration reaches a cutover diagnostic during HIR lowering. This section
> documents the intended v0.6+ design so implementers and embedders can plan
> for it. No v0.5 code should rely on the constructs below.

Hew provides FFI capabilities for interoperating with C libraries and system calls.

#### 3.9.1 Extern Function Declaration

> **v0.6+.** `extern "C"` blocks with arbitrary foreign symbols are not lowered
> in v0.5. Declarations of foreign functions other than the runtime's own
> `hew_*` symbols produce a cutover diagnostic.

External C functions are declared in `extern` blocks:

<!-- doctest: skip -->
```hew
extern "C" {
    fn malloc(size: usize) -> *mut u8;
    fn free(ptr: *mut u8);
    fn printf(fmt: *const u8, ...) -> i32;
    fn open(path: *const u8, flags: i32) -> i32;
    fn read(fd: i32, buf: *mut u8, count: usize) -> isize;
    fn write(fd: i32, buf: *const u8, count: usize) -> isize;
    fn close(fd: i32) -> i32;
}
```

**Calling convention:**

- `extern "C"` specifies the C calling convention (default)
- Future: `extern "stdcall"`, `extern "fastcall"` for platform-specific conventions

#### 3.9.2 C-Compatible Struct Layout

> **v0.6+.** `#[repr(C)]` is not recognised by the v0.5 HIR lowering pass.
> Annotating a type with `#[repr(C)]` produces a cutover diagnostic; layout
> is controlled by the compiler for all v0.5 types.

Use `#[repr(C)]` to ensure C-compatible memory layout:

<!-- doctest: skip -->
```hew
#[repr(C)]
type Point {
    x: f64;
    y: f64;
}

#[repr(C)]
type FileInfo {
    size: u64;
    mode: u32;
    flags: u16;
    padding: u16;  // Explicit padding for alignment
}
```

**Additional layout attributes:**

- `#[repr(C)]` - C-compatible layout with C alignment rules
- `#[repr(C, packed)]` - C layout with no padding
- `#[repr(C, align(N))]` - C layout with minimum alignment N

#### 3.9.3 Type Mapping: Hew ↔ C

> **v0.6+.** This mapping becomes relevant when `extern "C"` blocks and
> `#[repr(C)]` land. In v0.5, raw pointer types (`*const T`, `*mut T`) are
> only usable within the runtime allowlist; the cross-language mapping
> below is informational for v0.6+ planning.

| Hew Type                  | C Type                      | Notes                      |
| ------------------------- | --------------------------- | -------------------------- |
| `i8`, `i16`, `i32`, `i64` | `int8_t`, `int16_t`, etc.   | Exact size match           |
| `u8`, `u16`, `u32`, `u64` | `uint8_t`, `uint16_t`, etc. | Exact size match           |
| `isize`                   | `intptr_t` / `ssize_t`      | Platform-dependent         |
| `usize`                   | `uintptr_t` / `size_t`      | Platform-dependent         |
| `f32`, `f64`              | `float`, `double`           | IEEE 754                   |
| `bool`                    | `_Bool` / `bool`            | C99 bool                   |
| `*const T`                | `const T*`                  | Immutable raw pointer      |
| `*mut T`                  | `T*`                        | Mutable raw pointer        |
| `*const u8`               | `const char*`               | C string (null-terminated) |
| `fn(...) -> T`            | Function pointer            | C function pointer         |

#### 3.9.4 Exporting Functions to C

> **v0.6+.** `#[export]` is not recognised by the v0.5 compiler. Annotating
> a function with `#[export]` produces a cutover diagnostic. Hew functions
> are not linkable from C in v0.5.

Use `#[export]` to make Hew functions callable from C:

```hew
#[export("hew_process_data")]
fn process_data(data: *const u8, len: usize) -> i32 {
    // Implementation accessible from C as hew_process_data()
}

#[export]  // Uses the function name as-is
extern "C" fn my_callback(value: i32) -> i32 {
    value * 2
}
```

#### 3.9.5 Safety Rules

> **v0.6+.** The `unsafe` block, C string helper (`to_c_string`), and
> `#[resource]` close pattern shown below are the intended v0.6+ surface.
> In v0.5, `unsafe {}` blocks are parsed but produce a cutover diagnostic
> during HIR lowering; there is no user-accessible unsafe escape hatch.

**All FFI calls are `unsafe`:**

<!-- doctest: skip -->
```hew
fn allocate_buffer(size: usize) -> *mut u8 {
    unsafe {
        malloc(size)
    }
}

fn safe_read(fd: i32, buf: *mut u8, count: usize) -> Result<usize, string> {
    let result = unsafe { read(fd, buf, count) };
    if result < 0 {
        Err("read failed")
    } else {
        Ok(result as usize)
    }
}
```

**Unsafe operations include:**

- Calling foreign functions
- Dereferencing raw pointers
- Casting between incompatible pointer types
- Accessing mutable statics
- Implementing unsafe traits

**Safe wrapper pattern:**

<!-- doctest: skip -->
```hew
// Raw FFI (internal, unsafe)
extern "C" {
    fn open(path: *u8, flags: i32) -> i32;
    fn close(fd: i32) -> i32;
}

// Safe wrapper (public API)
#[resource]
pub type File {
    fd: i32;
}

impl File {
    pub fn open(path: string) -> Result<File, string> {
        let c_path = path.to_c_string();
        let fd = unsafe { open(c_path.as_ptr(), O_RDONLY) };
        if fd < 0 {
            Err("open failed")
        } else {
            Ok(File { fd })
        }
    }

    fn close(f: File) {
        unsafe { close(f.fd); }
    }
}
```

---

### 3.10 Standard Library Architecture

Hew ships its standard library as Hew source under `std/`. Modules are imported
by path (`import std::math;`, `import std::fs;`) and most high-level APIs are
defined in those source modules rather than by a separate metadata system.

#### 3.10.1 Edition 2026 normative stdlib surface

Hew does **not** expose a user-visible `core`/`alloc`/`std` tier split in
source. The standard library is organised by module path. Edition 2026 makes
normative guarantees about a deliberately narrow core; broader modules exist
in `std/` and compile, but their surface is informative until promoted into a
future edition (see HEW-FUTURE.md §3).

Normative in edition 2026:

- Core types and builtins: `Option<T>`, `Result<T, E>`, `Vec<T>`, `string`,
  `HashMap<string, V>`, `print`, `println`, `panic`.
- Concurrency types: `Task<T>`, `Stream<T>`, `Sink<T>`, `ScopeError<E>`,
  `TaskError`, `select` (§4), `after` (§4.11.3).
- System and I/O: `std::fs`, `std::io`, `std::path`, `std::os`,
  `std::time`.
- Formatting: `std::fmt`.
- Encoding: `std::encoding::json`, `std::encoding::msgpack`.
- HTTP: `std::net::http` (server + client at the request/response level).
- Utilities: `std::math`, `std::testing`.

See HEW-FUTURE.md §3 for modules that exist in `std/` today but are not yet
normative — `std::net::dns`, `std::net::tls`, `std::net::quic`,
`std::net::websocket`, `std::encoding::xml`/`yaml`/`toml`/`csv`,
`std::text::regex`, `std::process`, `std::encoding::compress`.

#### 3.10.2 Core Traits

The language supports user-defined traits, associated types, and named
receivers. The current stdlib does **not** ship a full generic
iterator-trait hierarchy; modules such as `std::iter` expose concrete helper
functions instead.

The following traits are representative of the current trait style:

```hew
trait Display {
    fn fmt(val: Self) -> string;
}

trait Drop {
    fn drop(val: Self);
}
```

#### 3.10.3 Core Types and Error Handling

**Option and Result** are first-class generic enums:

```hew
enum Option<T> {
    Some(T),
    None,
}

enum Result<T, E> {
    Ok(T),
    Err(E),
}
```

User-authored functions may return `Result<T, E>` or `Option<T>` and use `?`
for propagation. Any error type `E` may be used with `Result<T, E>`. The recommended pattern is for each module to define its own structured error enum, as demonstrated by the canonical `std::fs::IoError`:

```hew
pub enum IoError {
    NotFound(i64);
    PermissionDenied(i64);
    AlreadyExists(i64);
    Other(i64);
}

pub fn io_error_from_message(message: string) -> IoError {
    IoError::Other(0)
}
```

This pattern is used across every `try_*` function in the `std::fs` module and pairs with the `?` operator for ergonomic error propagation. Each stdlib module defines its own error type following this shape; there is no single cross-module error enum. Future stdlib modules (under #1247) will adopt the same per-module structured error approach.

**`string` and Vec** are built-in generic/runtime-backed types with dot-syntax
methods:

```hew
type string {}
type Vec<T> {}

impl<T> Vec<T> {
    fn new() -> Vec<T>;
    fn push(v: Vec<T>, item: T);
    fn pop(v: Vec<T>) -> T;                     // traps on empty vec
    fn len(v: Vec<T>) -> i64;
    fn get(v: Vec<T>, index: i64) -> Option<T>;
    fn set(v: Vec<T>, index: i64, item: T);      // traps out of bounds
    fn contains(v: Vec<T>, item: T) -> bool;
    fn clear(v: Vec<T>);
    fn append(v: Vec<T>, other: Vec<T>);
}
```

**Current `Vec<T>` element restrictions** — the type checker rejects element types that the vec lowering cannot handle:

- Non-RcFree elements: `Vec<T>` operations reject element types that fail the internal RcFree boundary (for example direct `Rc<U>`, named wrappers/structs/enums that contain `Rc`, and `LocalPid<A>` when actor `A` stores `Rc` state); this remains enforced at method-call sites (`push`, `pop`, `get`, `remove`, `set`, `append`, `extend`, `map`, `filter`, `fold`) rather than as a bare annotation-level ban
- Element types that structurally contain a fixed-size array (`[T; N]`) are rejected; flatten such data before storing in a Vec

Commonly used string operations include `+`, `==`, `!=`, `.len()`,
`.contains()`, `.trim()`, `.replace()`, `.split()`, `.lines()`,
`.is_digit()`, `.is_alpha()`, and `.is_alphanumeric()`.

`HashMap<K, V>` is also built in. `HashMap.get()` returns `Option<V>`.

**Bracket indexing** — a `HashMap<K, V>` supports `m[k]` subscript syntax keyed
by the same `K: Hash + Eq` bound every HashMap method enforces. A read `m[k]`
returns `Option<V>` (it is sugar for `m.get(k)`, so a missing key yields `None`
rather than aborting), and an assignment `m[k] = v` inserts or overwrites the
entry (sugar for `m.insert(k, v)`). Indexing with a key of the wrong type is a
type error. This differs from `Vec<T>` indexing, where `v[i]` takes an `i64`
index and returns the bare element `T`.

```hew
var m: HashMap<string, i64> = HashMap::new();
m["answer"] = 42;        // insert/overwrite via index-assignment
let hit = m["answer"];   // Option<i64> — Some(42)
let miss = m["absent"];  // Option<i64> — None
```

**Current implementation boundary** — although the surface spelling is generic,
the shipped runtime/codegen ABI currently supports only `HashMap<string, V>`
where `V` is `string`, `bool`, `char`, any integer type, any float type, or
`duration`. Other `HashMap<K, V>` pairs are rejected during type checking.
Additionally, non-RcFree types in either the key or value position are rejected
regardless of the ABI key/value check; this is structural, not just a direct
`Rc<T>` ban.

**Map literal syntax** — a `HashMap<K, V>` can be constructed inline with
brace-colon syntax.  The parser disambiguates `{` as a map literal when the
first token after `{` is a `StringLit` followed by `:`:

```hew
// Inferred: HashMap<string, i64>
let scores = {"alice": 10, "bob": 20};

// Explicit type annotation drives checking; each value must match V
let env: HashMap<string, string> = {
    "HOST": "localhost",
    "PORT": "8080",
};

// Trailing comma is allowed
let flags = {"debug": true, "verbose": false,};

// Empty block {} coerces to HashMap<K,V> when the expected type is known
let empty: HashMap<string, i64> = {};
```

Rules:

- Keys must all have the same type; the key type is inferred from the first
  entry.
- Values must all have the same type; the value type is inferred from the
  first entry.
- The `{}` empty block coerces to `HashMap<K,V>` when the surrounding context
  supplies an expected `HashMap` type.
- Map literals compile to a `HashMap::new()` followed by one `insert` call per
  entry; no heap-coalescing is performed at compile time.

Available `HashMap` methods:

| Method                    | Returns         | Description                      |
| ------------------------- | --------------- | -------------------------------- |
| `HashMap::new()`          | `HashMap<K,V>`  | Create empty map                 |
| `m.get(key)`              | `Option<V>`     | Look up a key                    |
| `m.insert(key, value)`    | `()`            | Insert or overwrite              |
| `m.remove(key)`           | `Option<V>`     | Remove a key; `Some(value)` if present, else `None` |
| `m.contains_key(key)`     | `bool`          | Test membership                  |
| `m.len()`                 | `i64`           | Number of entries                |
| `m.is_empty()`            | `bool`          | True if no entries               |
| `m.keys()`                | `Vec<string>`   | Snapshot of all keys             |
| `m.values()`              | `Vec<V>`        | Snapshot of all values, same order as `keys()` |
| `m.clear()`               | `()`            | Remove all entries               |

Available `HashSet<T>` methods (supported element types: `i64` and
`string`):

| Method                    | Returns         | Description                      |
| ------------------------- | --------------- | -------------------------------- |
| `HashSet::new()`          | `HashSet<T>`    | Create empty set                 |
| `s.insert(item)`          | `()`            | Insert; duplicate inserts are a no-op |
| `s.contains(item)`        | `bool`          | Test membership                  |
| `s.remove(item)`          | `bool`          | Remove an item; true if present  |
| `s.len()`                 | `i64`           | Number of entries                |

#### 3.10.4 Collections, I/O, and Utility Modules

The standard library exposes concrete modules rather than a large trait
hierarchy. Representative APIs include:

```hew
import std::deque;
import std::fmt;
import std::io;
import std::iter;
import std::math;
import std::sort;
import std::testing;

let ints: Vec<i64> = Vec::new();
let set: HashSet<i64> = HashSet::new();
let dq = deque.new();

println(math.abs(-5));
println(fmt.to_hex(255));
println(iter.sum(ints));
testing.assert_true(true);
println(io.read_all());
```

Important current details:

- `std::io` currently provides plain functions (`read_line`, `write`,
  `write_err`, `read_all`), not `Read`/`Write`/`BufRead` traits
- Built-in `HashSet<T>` currently lowers the supported surface forms
  `HashSet<i64>` and `HashSet<string>` through the typed-layout runtime;
  unsupported `HashSet<T>` usages are rejected fail-closed during type
  checking, including nested annotations, function signatures, and `#[wire] enum`
  payloads; collection element admissibility also requires the internal RcFree
  boundary, so `HashSet<Rc<T>>`, named wrappers that contain `Rc`, and
  `LocalPid<A>` handles to actors with `Rc` state are rejected
- `std::iter` exposes lazy adapters (`map`, `filter`, `take`, `skip`) over
  any `Iterator`, driven by terminal helpers (`fold`, `count`, `collect`,
  `any`, `all`, `sum`, `sum_f64`, `product`, `product_f64`); drive a
  `Vec<T>` through it via `.iter()` or `.into_iter()`
- `std::sort` exposes concrete helpers like `sort_ints`, `sort_strings`,
  `sort_floats`, `reverse_ints`, `reverse_strings`, and `reverse_floats`
- `std::testing` is a pure-Hew assertion library layered on top of `panic()`

#### 3.10.5 Printing, Formatting, and Strings

`print` and `println` are builtins. The current compiler lowers them through
type-specific runtime intrinsics; that lowering is an implementation detail.

F-strings support arbitrary expressions inside `{}`:

```hew
let name = "world";
let x = 10;
let msg = f"hello {name}";
let computed = f"result: {x + 1}";
let nested = f"len: {name.len()}";
```

F-strings are the sole string interpolation syntax in Hew.

#### 3.10.6 Prelude (Automatically Imported)

The following are automatically available in every Hew module:

```hew
// Types
Option, Some, None
Result, Ok, Err
string, Vec, Box

// Traits
Clone, Copy, Drop
Send, Frozen
Debug, Display
Iterator, IntoIterator
Eq, Ord, Hash

// Functions
print, println
panic, assert, debug_assert
```

#### 3.10.7 Typed Handles

Standard library functions return opaque typed handle objects. Callers
**must** invoke `close()` explicitly to release the underlying
resource. Dropping without `close()` is a resource leak; the compiler
does not synthesise an implicit drop for these types in the current
implementation.

| Type             | Created by                                 | Methods                                                                                                                              |
| ---------------- | ------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------- |
| `http.Server`    | `http.listen(addr)`                        | `.accept()` → `http.Request`, `.close()`                                                                                              |
| `http.Request`   | `server.accept()` or `http.accept(server)` | `.path`, `.method`, `.body`, `.header(name)`, `.respond(status, body, len, type)`, `.respond_text(status, body)`, `.respond_json(status, body)`, `.close()` |
| `net.Listener`   | `net.listen(addr)`                         | `.accept()` → `net.Connection`, `await ln.accept() \| after d` → `Result<net.Connection, IoError>`, `.close()`                        |
| `net.Connection` | `listener.accept()` or `net.connect(addr)` | `.read()` → `bytes`, `.read_string()` → `string`, `await conn.read_string() \| after d` → `Result<string, IoError>`, `.write(data)`, `.write_string(data)`, `.close()` |
| `process.Child`  | `process.start(cmd)`                       | `.wait()`, `.kill()`                                                                                                                  |

Handle types are opaque — their internal representation is not accessible.
They can be stored in variables, passed as function arguments, and
returned from functions.

`net.Listener::accept()` and `net.Connection::read()`'s plain (non-`await`)
forms block the calling thread; inside an actor receive handler this stalls
the scheduler worker (`hew check`'s `BlockingCallInReceiveFn` warning) — use
the `await` form there instead. The plain forms remain the intended shape for
a `main()`-body call outside any receive handler.

#### 3.10.8 Regular Expressions

> See HEW-FUTURE.md §3 for `std::text::regex` — targeted for v0.6+
> alongside the stdlib port-forward. `regex.Pattern` is a
> `#[resource]`-annotated type with RAII handles (§3.7.8): `close()`
> releases early, and the implicit scope-exit drop covers the rest.

---

## 3.11 `machine` Types

> **Implementation status:** The front-end (lexer keywords, parser, AST, HIR
> lowering, static checks), the `hew machine diagram` visualisation subcommand,
> and native code generation are implemented. Machine values are executable:
> the compiler emits the tagged-union layout, companion event enum, `step()`,
> `state_name()`, and enum-like pattern matching support described below.

A `machine` is a **value type** that defines a closed set of named states, a
closed set of named events, and transition rules mapping `(State, Event)` pairs
to new states.  It compiles to a tagged union with a compiler-generated
`step()` method.  Machines are not actors — they are pure data, like enums
with per-state fields and compiler-checked transition logic.

> **Detailed specification:** See [`docs/specs/MACHINE-SPEC.md`](MACHINE-SPEC.md)
> for the full normative reference.

**Design pillars:**

- **Value semantics** — a machine is a tagged union (like `enum`), not a
  reference type.
- **Exhaustiveness** — the compiler verifies that every `(State, Event)` pair
  is handled (via an explicit transition, a wildcard, or a `default` handler).
- **Zero-cost** — compiles to an integer tag plus a C-style union of state
  structs. No heap allocations, no threads.

### 3.11.1 Declaration Syntax

```hew
machine Name {
    // Input-event vocabulary — declared up front (mandatory header)
    events {
        EventX;                            // event with no payload
        EventY { payload: Type; }          // event with payload
    }

    // Output vocabulary — optional; lists events this machine may `emit`
    emits {
        EventX;
    }

    // States — at least two required
    state StateA;                          // unit state (no fields)
    state StateB { field: Type; }         // state with data

    // Transitions: on Event: Source => Target { body }
    on EventX: StateA => StateB { StateB } // explicit body returns target value
    on EventY: StateA => StateB { StateB { field: event.payload } }

    // Head binding: name payload fields at the rule site
    on EventY(payload): StateB => StateA { StateA }

    // Self-transition with reenter (runs exit/entry even when state is unchanged)
    on EventX: StateB => StateB reenter { StateB { field: self.field } }

    // Wildcard — applies in all unhandled source states for this event
    on EventX: _ => _ { state }           // _ => _ means "stay in current state"

    // Depth-1 composite state (substate block; depth > 1 is reserved)
    state Parent {
        initial state Sub1;
        state Sub2 { value: i64; }
    }

    // Default handler — fallback for ALL unmatched (state, event) pairs
    default { state }
}
```

**Grammar (EBNF, from `docs/specs/grammar.ebnf`):**

```ebnf
MachineDecl    = "machine" Ident TypeParams? "{"
                   EventsHeader
                   [ EmitsHeader ]
                   { StateDecl }
                   { TransitionDecl }
                   [ DefaultArm ]
                 "}" ;

EventsHeader   = "events" "{" { EventDecl } "}" ;
EventDecl      = Ident ( ";" | "{" { Ident ":" Type (";" | ",") } "}" ";"? ) ;
EmitsHeader    = "emits" "{" { Ident ";" } "}" ;

StateDecl      = "state" Ident ( "{"
                   { Ident ":" Type (";" | ",") }  (* field declarations *)
                   [ "entry" Block ]                (* entry hook *)
                   [ "exit"  Block ]                (* exit hook  *)
                   { CompositeMember }              (* depth-1 composite only *)
                 "}" )? ";"? ;
CompositeMember = [ "initial" ] StateDecl ;         (* exactly one "initial" required *)

TransitionDecl = "on" Ident [ "(" Ident { "," Ident } ")" ] ":"
                 StatePattern "=>" StatePattern
                 [ "reenter" ] [ "when" Expr ] TransitionBody ;
TransitionBody = ";" | "{" FieldInitList "}" | Block ;
StatePattern   = Ident | "_" ;
DefaultArm     = "default" "{" "state" "}" ;

(* Emit expression (usable inside transition bodies and entry/exit blocks): *)
EmitExpr = "emit" Ident ( "{" FieldInitList "}" )? ;
```

> **Depth > 1 nesting is reserved** — a substate body may not itself contain
> substates.  Depth-1 composite state blocks are supported; deeper nesting
> (`depth > 1`) is a parse error: `nested composite states (depth > 1) are reserved`.

**Visualisation:** `hew machine diagram <file.hew>` renders any
`machine` declaration as a Mermaid state diagram, Graphviz DOT, or JSON
schema.  The command runs all HIR static checks before rendering, so it
doubles as a structural validator.

```
hew machine diagram traffic_light.hew                   # Mermaid (default)
hew machine diagram traffic_light.hew --format graphviz # Graphviz DOT
hew machine diagram traffic_light.hew --format json     # JSON schema
hew machine diagram traffic_light.hew --machine Name    # filter one machine
hew machine diagram traffic_light.hew --no-check        # skip HIR checks
```

### 3.11.2 Constraints

| Constraint                                  | Checked at  | Error if violated                             |
| ------------------------------------------- | ----------- | --------------------------------------------- |
| At least two states                         | Types/HIR   | `machine_one_state` negative test             |
| At least one event                          | Types/HIR   | `machine_no_events` negative test             |
| No `state` nesting deeper than depth 1; depth-1 composite states are supported | Parse | diagnostic: nested composite states (depth > 1) are reserved |
| No duplicate explicit transition per (S, E) | Parse/HIR   | `machine_dup_transition` negative test        |
| No duplicate wildcard for same event        | Parse/HIR   | `machine_dup_wildcard` negative test          |
| All referenced states/events must be declared | HIR       | `machine_unknown_state/event` negative tests  |
| All (S, E) pairs covered (exhaustiveness)   | HIR         | `MachineExhaustivenessViolation` diagnostic   |
| Effect parity: transition body writes ≡ entry writes | HIR  | `MachineEffectParityViolation` diagnostic     |
| No direct self-emit (`emit E` in transition for event E) | HIR | `MachineEmitCycle` diagnostic          |

Exhaustiveness can be satisfied by: explicit `on` rules, wildcard (`_`-source)
rules, or a `default` handler.  A `default` handler alone covers all pairs that
have no other matching rule.

**Effect parity:** when a transition body writes a state field (via
`self.field = …`) and the target state's `entry` block also writes that same
field, the compiler emits a `MachineEffectParityViolation` diagnostic.  This
prevents silent shadowing between transition-side and entry-side field
initialisation.

**Emit-cycle detection:** a transition handling event `E` may not directly
`emit E` — that would form an immediate re-entry cycle.  Indirect cycles
(A emits B, B emits A) are not checked.

### 3.11.3 Transition Bodies

Inside a transition body the compiler binds two implicit names:

| Binding     | Type              | Meaning                                         |
| ----------- | ----------------- | ----------------------------------------------- |
| `state`     | source state type | Fields of the current (source) state            |
| `event`     | event payload     | Payload fields of the incoming event (if any)   |

```hew
machine Elevator {
    state Stopped { floor: i64; }
    state Moving  { from: i64; to: i64; }

    event GoTo  { floor: i64; }
    event Arrive;

    on GoTo: Stopped => Moving {
        Moving { from: state.floor, to: event.floor }   // state.floor, event.floor
    }
    on Arrive: Moving => Stopped {
        Stopped { floor: state.to }
    }

    default { state }
}
```

**Elided target state name** — when the target state is unambiguous, the
`TargetState { ... }` wrapper may be omitted and only the field initialiser
list is written:

```hew
on Work: Active => Active { count: state.count + event.amount }
// equivalent to:
// on Work: Active => Active { Active { count: state.count + event.amount } }
```

**Body-less shorthand** — when a transition has no body, the compiler
constructs the target state's zero-field (unit) variant automatically:

```hew
on Toggle: Off => On;   // equivalent to: on Toggle: Off => On { On }
```

### 3.11.4 Guard Conditions (`when`)

A transition may carry a boolean guard expression after the target state name:

```hew
on Request: Allowing => Allowing when state.tokens > 1 {
    Allowing { tokens: state.tokens - 1 }
}
on Request: Allowing => Throttled when state.tokens <= 1;
```

Guards are evaluated in declaration order.  The first transition whose event
and source-state match *and* whose guard (if present) evaluates to `true` fires.
If no guarded transition matches, evaluation falls through to wildcard rules and
then to `default`.

### 3.11.5 Wildcard Transitions and Priority

`_` in the source position matches any state.  `_` in the target position means
"return a value of the machine type" (any variant, not a specific one).  The
conventional identity pattern `on E: _ => _ { state }` keeps the current state
unchanged.

Priority order (highest to lowest):

1. Explicit transitions (specific source state, no wildcard)
2. Wildcard/`_`-source transitions
3. `default` handler

Specific transitions always win over wildcards for the same event.

### 3.11.6 Generated API

The compiler generates the following for every `machine Name { ... }`:

| Generated item              | Usage / behaviour                                                  |
| ----------------------------| ------------------------------------------------------------------ |
| State constructors          | `Name::State` (unit) or `Name::State { field: val }` (with data)  |
| Companion event enum        | `NameEvent` with variants matching each `event` declaration        |
| Event constructors          | `NameEvent::EventName` (unit) or `NameEvent::EventName { f: v }`  |
| `m.step(event)`             | Mutates `m` in place; returns `()` — no return value              |
| `m.state_name()`            | Returns the current state name as `string`                        |
| Pattern-match support       | Machine values can be matched exactly like enum values             |

**Calling `step()`** — both unqualified and qualified event constructors are
accepted:

```hew
var light = Light::Off;
light.step(Toggle);                // unqualified (preferred for brevity)
light.step(LightEvent::Toggle);   // fully qualified (also valid)
```

**Pattern matching** — machine values can be destructured in `match`, `if let`,
`while let`, and function parameters exactly like enums:

```hew
match cb {
    Closed { failures } => println(f"failures = {failures}"),
    Open                => println("open"),
    HalfOpen            => println("half-open"),
}
```

### 3.11.7 Using Machines Inside Actors

Machines are values — they are commonly embedded as actor fields:

```hew
actor ConnectionManager {
    var tcp: TcpState = TcpState::Closed;

    receive fn handle(event: TcpStateEvent) {
        tcp.step(event);
        // React to the new state
        match tcp {
            Established { local_seq, remote_seq } => {
                println(f"established seq={local_seq}/{remote_seq}");
            },
            _ => {},
        }
    }
}
```

Because `machine` is a value type, assigning a machine variable copies it.
The `step()` method mutates the variable in place — it does not return a new
value.

**Implementation status (v0.5.1):** machine-typed actor state fields are
supported, including heap-payload states (the field rides the enum
clone/drop substrate; `step()` on a field stores back through the
state-field overwrite-release path). Generic machine instantiations other
than all-`i64` type arguments are refused at compile time (the machine
substrate keeps one bare-named layout per declaration). Machine-state
actors are not yet admitted as supervisor children (state constructors are
not literal child-init values), so supervisor restart-clone of machine
state is unreachable until that slice widens.

Because there is no shared machine instance, transition OBSERVATION is a
library pattern, not a language construct: the owner publishes
transitions into a `std/channel` `Sender` and observers select on the
receive arm with an `after` safety net. Machine values themselves can
also travel as channel elements (state snapshots) and pattern-match on
state variants at the receiver. See
`examples/machine/select_on_transition.hew` and
`examples/machine/transition_watch_baseline.hew`.

### 3.11.8 Type System Integration

- A machine type is a nominal type; it does not implicitly unify with any
  `enum` or other machine.
- Machines satisfy `Send` if all their state fields satisfy `Send`
  (same rule as structs).
- Machines can be used as type parameters wherever the bound permits.
- Generics over machines are not currently supported (non-goal for now).

---

## 4. Effects, IO, and Async Semantics

This section defines Hew's concurrency model within actors. Hew distinguishes between:

- **Inter-actor concurrency**: Actors communicate via asynchronous message passing (Section 2.1)
- **Intra-actor concurrency**: Tasks execute cooperatively within a single actor using structured concurrency

### 4.1 The Task Type

A `Task<T>` represents a concurrent computation that will produce a value of type `T`. Tasks execute within their spawning actor's single-threaded context.

```
Task<T>
```

**Type definition:**

| Property  | Description                                               |
| --------- | --------------------------------------------------------- |
| `T`       | The result type of the task                               |
| Ownership | Tasks are owned values, not `Send`                        |
| Lifetime  | A task lives until awaited, cancelled, or its scope exits |

**Task states:**

```
┌──────────┐   spawn    ┌─────────┐
│ Pending  │ ─────────► │ Running │
└──────────┘            └────┬────┘
                             │
           ┌─────────────────┼─────────────────┐
           │                 │                 │
           ▼                 ▼                 ▼
    ┌────────────┐    ┌────────────┐    ┌────────────┐
    │ Completed  │    │ Cancelled  │    │  Trapped   │
    │  (value)   │    │            │    │  (fault)   │
    └────────────┘    └────────────┘    └────────────┘
```

- **Pending**: Task created but not yet scheduled
- **Running**: Task is executing or ready to execute
- **Completed**: Task finished with a value of type `T`
- **Cancelled**: Task was cooperatively cancelled
- **Trapped**: Task encountered an unrecoverable error

**Task methods:**

| Method    | Signature                        | Description                                                 |
| --------- | -------------------------------- | ----------------------------------------------------------- |
| `is_done` | `fn is_done(t: Task<T>) -> bool` | Returns `true` if task has completed, cancelled, or trapped |

### 4.2 Scope: Structured Concurrency Boundary

> **Partially implemented in edition 2026 / v0.5.0.** The shipped surface is
> `scope { fork { call(); } }` in suspendable contexts (actor handlers,
> closures, task entries), where the forked call takes no arguments and the
> scope joins all children at its closing brace. The name-bound form
> `fork name = call(...)` described below, argument-bearing forks, awaiting a
> child's value, sibling cancellation on child failure, and the `?`
> propagation sugar are specified here but not yet accepted — each refuses
> with a named diagnostic rather than miscompiling.

A `scope` block creates a structured concurrency boundary. All child tasks
forked within the block must complete before the block returns.

**Syntax:**

```hew
scope {
    fork a = compute_a();   // child task: spawned + name-bound
    fork b = compute_b();   // sibling child task
    use_results(a?, b?);    // `?` propagates errors if a/b's return type is Result/Option;
                            // the scope itself joins children on exit — no `await` needed.
}
```

**Semantics:**

1. **Scope containment**: Child tasks cannot outlive their enclosing `scope` block.
2. **Automatic join**: The block waits for every child task before returning.
3. **Block value**: A `scope` block is **Unit-typed**. It is a statement, not a value-producer.
   `scope` is the scope bracket; the `fork name = expr` children carry the values.
   `scope` and `fork` are not synonyms — keeping them separate prevents confusing the
   bracket role with the child-start role. Use `await` inside the scope body
   to resolve child values, bind them to `let` or `var` bindings, and return them from the
   enclosing function directly.
4. **Nested scopes**: `scope` blocks may be nested; each manages its own children.
5. **First-failure-cancels-siblings**: If any child returns `Err(E)` or traps, the runtime
   cancels the remaining siblings at the next safepoint. The error surfaces via the `await`
   expression for that child: `?` on `await task` propagates `Err` to the enclosing function;
   an unhandled trap unwinds the scope and propagates to the enclosing context.

> **Design note.** `scope` is the scope bracket; `fork name = expr` is the child-start verb.
> These are deliberately separate keywords so neither can be confused for the other. See the
> Historical note in §4.9 for the earlier `scope |s| { s.spawn { … } }` surface.

**Child form:**

`fork name = expr` (or bare `fork expr`) is only legal dynamically inside a
`scope` block. `scope { ... }` opens the structured-concurrency block;
`fork` is exclusively the child-start verb. In v0.5.0 the accepted child
form is the block form `fork { call(); }` with a zero-argument callee; the
name-bound form parses but is rejected pending its type-checking slice.
Outside a scope-block, a child-form `fork` is a `ForkOutsideScopeBlock`
error.

### 4.3 Spawning Child Tasks

```hew
var result;
scope {
    fork a = compute_a();
    fork b = compute_b();
    result = combine(a?, b?);   // scope joins a and b on exit; `?` propagates Result/Option errors
};
result
```

**Syntax:**

```ebnf
Scope     = "scope" Block ;                        (* structured-concurrency block *)
ForkChild = "fork" ( Ident "=" )? Expr ;           (* child form, only inside a Scope block *)
```

**`fork name = expr` — structured child task:**

- Returns `Task<T>` where `T` is the type of `expr`.
- Spawned task runs concurrently with its siblings.
- Captured variables follow the same rules as actor sends and closures
  (move semantics by default; explicit `move` to force a moving capture).
- On `Err(E)` or trap, the enclosing `scope` block transitions to
  cancelling: siblings are cancelled at their next safepoint and the
  first error wins as `ScopeError::primary`.

**Capture rules for `@linear` and `@resource` values (normative):**

The interaction between ownership-discipline markers (§3.4) and child
tasks needs explicit rules, since a child may be cancelled at a
safepoint before its body reaches a consuming or close call.

1. **`@resource` capture.** A `@resource` value moved into a child task
   has its drop discipline run on whichever exit path the child takes:
   normal completion, recoverable `Err(E)`, trap, or cancellation. The
   declared `close(consuming self) -> Result<(), E>` is invoked through
   the child's stack-unwind path (§4.5), and its returned error is
   discarded per the §3.4 `@resource` semantics. This is the *same*
   discipline a `@resource` would receive in a non-task context; the
   child's cancellation safepoint is simply one more exit through which
   stack unwinding runs cleanup.

2. **`@linear` capture.** A `@linear` value moved into a child task
   transfers the must-consume obligation to the child body. The parent
   cannot use the value after the capture site; the child must reach a
   declared consuming method on every path that does not trap. Because
   edition-2026 cancellation is **scope-structural only** (no user
   cancellation tokens, no preemption), the checker rejects the capture
   if the child body has any cancel-reachable exit that does not pass
   through a consuming call. The rejection diagnostic is
   `LinearCaptureCancellable`, and it points at the binding, the
   capture site, and the cancel-reachable exit that proves the gap.

   The conservative rule keeps the language honest in edition 2026:
   without cancellation tokens, the programmer has no way to consume a
   `@linear` value along the cancellation path, so the only safe shape
   is for the child's *only* exits-to-completion to be ones the
   compiler can prove consume the value. Relaxation is tracked in
   HEW-FUTURE.md §1.2 alongside the broader cancellation-token
   vocabulary.

3. **`&` and `&mut` borrow into a child.** Shared `&` borrow into a
   child is admitted only when the borrow's referent is provably alive
   for the entire lexical scope-block and no mutable borrow of the same
   region is live while any child runs. Mutable `&mut` borrow into a
   child is rejected in edition 2026: the child runs on a substrate
   thread distinct from the parent's, and aliased mutable access cannot
   be made safe without synchronisation that the language deliberately
   does not auto-inject (HEW-FUTURE.md §1.6). The rejection diagnostic
   names the mutable borrow site and points at the fork child.

4. **Task<T> handle escape.** A `Task<T>` handle bound by `fork name =
   expr` is usable only within the lexical scope-block that introduced
   it. The handle cannot be returned from the scope-block, stored in a
   field, captured by a closure that outlives the block, nor moved into
   a sibling child unless that sibling is itself a `fork` form inside
   the same block. The rejection is structural: `Task<T>` is not a
   nameable type at the source level (§4.1) and the handle has no
   surface syntax to escape through.

**`fork expr` — bare child form:**

A degenerate single-child form: `fork expr` evaluates `expr` as a child
task. The enclosing scope block is still Unit-typed; to consume the child's
result, bind it with `fork name = expr` and then `await name` inside the
scope body, or simply fire-and-forget with the bare form when the value is
not needed.

**Substrate (informative):**

The β surface lowers each child's execution to an OS-thread-per-task
substrate (`hew-runtime/src/task_scope.rs`, `hew_task_spawn_thread`). The
parent's `await` of a child lowers onto the same unified `llvm.coro`
switched-resume continuation substrate (`hew-runtime/src/cont.rs`,
`hew_cont_*`) used by generator `yield` and actor `await`; the source
surface is a single `fork` child production whose scheduling discipline
is the runtime's concern. The earlier drafts exposed two child verbs
(`s.launch` for cooperative coroutines vs `s.spawn` for OS threads) on a
`scope |s| { ... }` handle; that surface was removed entirely in the 2026
edition — see "Historical note" at the end of §4.9.

**Yield points (normative):**

A child task MUST yield at:

- `await` expressions — suspends until the awaited task or actor is ready.
- compiler-inserted `cooperate` safepoints — reduction budget exhaustion;
  the compiler inserts checks at function entry and loop back-edges.
- IO operations — cancellation is observed at the syscall boundary.

Yield points are also where cooperative cancellation is delivered (§4.5).

### 4.4 Awaiting Tasks

The `await` operator blocks the current task until the awaited task completes, returning its result.

**Syntax:**

```hew
let result = await task;
```

**Semantics (normative):**

| Awaited Task State | `await` Behaviour                                           |
| ------------------ | ----------------------------------------------------------- |
| Completed          | Returns `Ok(value)` immediately                             |
| Running/Pending    | Suspends current task until completion, returns `Ok(value)` |
| Cancelled          | Returns `Err(...)`                                         |
| Trapped            | Propagates the trap to the awaiting task                    |

**Type:**

```
await : Task<T> -> Result<T, E>
```

Cancellation is an **expected** outcome (it is triggered automatically
when a sibling child fails, or implicitly at scope-block exit) and MUST be
modeled as a recoverable error, not a trap. The current release does not
expose a named `CancellationError` type in source; callers should handle
the `Err(...)` branch of the `await` result. Traps are reserved for
unexpected, unrecoverable failures (Section 2.2).

```hew
// Cancellation returns Err, not a trap:
let result = await task;
match result {
    Ok(v) => use_value(v),
    Err(_) => handle_cancellation(),
}

// Use ? to propagate cancellation errors:
let value = (await task)?;
```

> **Note:** Only traps (panics) propagate as unrecoverable. Cancellation is always catchable via the `Result` return type.

**Examples:**

```hew
// Simple await — bind result before the scope, assign inside
var value;
scope {
    fork x = expensive_compute();
    value = await x;
};

// Concurrent tasks with sequential await
var merged;
scope {
    fork a = fetch_user(id1);
    fork b = fetch_user(id2);

    // Both fetches run concurrently; await resolves them in order
    let user1 = await a;
    let user2 = await b;
    merged = merge_users(user1, user2);
};
merged
```

### 4.5 Cancellation

Cancellation in Hew is **automatic at safepoints**: when a scope-block is
cancelled, running children are interrupted at the next safepoint without
manual polling.

**Cancellation triggers:**

A scope-block transitions to cancelling when:

1. A child returns `Err(E)` — the first such `E` becomes `ScopeError::primary`.
2. A child traps — siblings are cancelled and the trap propagates after join.
3. An outer scope-block (or its enclosing actor) is itself cancelled.

There is no user-level `cancel()` call against a scope-block from inside its
own body; cancellation is event-driven from child outcomes.

**Cancellation is automatic at safepoints:**

The following points are safepoints where cancellation is checked automatically:

- `await` expressions
- compiler-inserted `cooperate` safepoints at function entry and loop back-edges
- IO operations (file read/write, network operations)

When cancellation fires at a safepoint, the runtime initiates **stack unwinding** with a `Cancelled` payload. All `defer` blocks and `Drop` implementations run during unwinding, ensuring deterministic resource cleanup.

> See HEW-FUTURE.md §1.2 for `#[noncancellable]` — targeted beyond
> edition 2026 alongside the broader cancellation-token vocabulary.

**Cancellation propagation:**

When a scope-block is cancelled:

1. Pending child tasks that haven't started are immediately marked `Cancelled`.
2. Running children are cancelled at their next safepoint (automatic — no polling needed).
3. Stack unwinding runs `defer`/`Drop` blocks for deterministic cleanup.
4. Nested scope-blocks receive the cancellation signal.

**Cancellation does NOT:**

- Forcibly terminate running code between safepoints.
- Affect tasks in other scope-blocks or other actors.

**Example with cleanup:**

```hew
receive fn download_files(urls: Vec<string>) -> Result<Vec<Data>, Error> {
    var results: Vec<Data> = Vec::new();
    scope {
        for url in urls {
            scope {
                let data = http::get(url)?;  // Safepoint — cancellation checked here
                results.push(process(data)); // If cancelled, stack unwinds; defer blocks run
            };
        }
    };
    Ok(results)
}
```

### 4.6 Error Handling in Tasks

Tasks can fail in two ways:

1. **Recoverable errors**: Return `Err(E)` from a `Result<T, E>`
2. **Unrecoverable errors**: Trap (panic)

**Recoverable errors:**

When a child task returns a `Result`, errors can be handled directly by the
awaiter inside the scope body:

```hew
scope {
    fork task = {
        fallible_operation()?;
        Ok(value)
    };

    match await task {
        Ok(v) => use_value(v),
        Err(e) => handle_error(e),
    }
}
```

The scope block is Unit-typed; `await task` resolves the child's
`Result<T, E>` inline. If multiple children can fail and you need to
aggregate their errors, collect the `await` results into a `Vec` inside the
scope body and inspect it after the scope block completes. `ScopeError<E>`
(see `std/concurrency/scope_error.hew`) is the layout for aggregated
per-child errors; it is produced by the `await` expressions, not by the
scope block itself. Propagating the first error with `?` is written as `?`
on the `await` expression for that child, which propagates to the enclosing
function — not to the scope block's "value."

**Traps (unrecoverable errors):**

When a child task traps:

1. The task transitions to `Trapped` state.
2. Sibling children in the same scope-block are cancelled.
3. The scope-block itself traps, propagating to its enclosing context.

**Trap in a `receive fn` (normative):**

When a trap propagates out of a `scope` block inside a `receive fn`:

1. The current message handler terminates immediately
2. The actor transitions to `Crashed` state (see §9.1)
3. The actor's supervisor is notified with the trap reason
4. The supervisor applies its restart policy (Section 5.1)

This means a trap within a forked child inside a `receive fn` causes the entire actor to crash — it does NOT silently discard the error and proceed to the next message. This matches Erlang's "let it crash" philosophy: unexpected failures are handled by the supervision tree, not by application-level error recovery.

**Trap propagation example:**

```hew
scope {
    fork a = compute();        // Running
    fork b = trap!("failed");  // Traps
    // Task 'a' is cancelled
    // Fork-block traps
}
// Code here never executes
```

**Isolating failures with nested scope-blocks:**

```hew
scope {
    fork results = {
        // Inner scope-block isolates failures; the fork child body is an
        // ordinary block (not a scope block) that can carry a value.
        var outcome: Result<Data, Error>;
        scope {
            fork task = risky_operation();
            outcome = await task;   // captures Ok or Err
        };
        outcome                     // fork child returns the Result
    };

    // Outer scope-block continues even if results returned Err;
    // inspect via `await results` inside this body.
}
```

### 4.7 IO and Effects

All IO operations in Hew are explicit and return `Result` types. Use
`fs::try_read` (not `fs::open`) for file reading; the stdlib has no `File`
handle type — reads and writes are free functions:

<!-- doctest: skip -->
```hew
fn read_config(path: string) -> Result<Config, string> {
    let content = fs::try_read(path)?;
    json::parse(content)
}
```

**IO operations are cancellation-aware:**

```hew
// If the enclosing scope-block is cancelled while waiting for response,
// http::get returns Err(Cancelled)
let response = http::get(url)?;
```

**Blocking operations:**

Hew runtime may offload blocking operations to a thread pool. From the task's perspective:

- The task suspends at the blocking call
- Other tasks in the actor may run
- The task resumes when the operation completes

**Actor isolation guarantees:**

Actor isolation is preserved because every fork-child runs on its own OS
thread (β substrate). Captured values must be sendable; `move` semantics
apply at the child-spawn site just as they do at every other actor send
boundary. The actor's own state remains owned by the actor's thread and
is not shared into child tasks.

### 4.8 Interaction with Actor Messages

Child tasks forked within a receive handler are isolated from the
actor's mutable state: each runs on its own OS thread, captured values
move (or clone) across the boundary, and the actor's fields are not
reachable from inside a child body.

> **§4.8 design unsettled** — the dynamic-fork-in-loop
> idiom shown below is illustrative only. The ratified `fork name = expr;`
> shape requires a binding name per child; collecting handles into
> `Vec<Task<T>>` contradicts the `Task<T>` non-nameability rule (§4.3),
> and `await t` is not a primitive. The settled idiom is one of:
> (a) `fork[]` array form yielding `[T; N]` on scope exit;
> (b) a `scope_par_map(items, |x| f(x))` stdlib op;
> (c) actor-mailbox accumulation via an anonymous `fork _ = …;`.
> The example below uses an indicative placeholder pending ratification.

<!-- doctest: skip -->
```hew
actor DataProcessor {
    var cache: HashMap<string, Data> = HashMap::new();

    receive fn process_batch(ids: Vec<string>) -> Vec<Data> {
        // Indicative shape; see §4.8 design-unsettled note above.
        // The scope joins all forks on exit; `data` is populated
        // via the (unsettled) accumulation mechanism.
        var data: Vec<Data> = Vec::new();
        scope {
            for id in ids {
                // Captures of `id` move into the child; actor fields
                // (e.g. `cache`) are not in scope inside the child body.
                fork _ = collect_into(&mut data, fetch_data(id));
            }
        };
        data
    }
}
```

**Message-task interaction rules:**

1. A `receive fn` handler executes on the actor's thread.
2. Child tasks spawned by `fork name = expr` run on their own OS threads and are isolated from actor state.
3. The actor does not process the next message until the current handler (and all its forked children) complete.
4. If a handler's child task traps, the actor may trap (per failure model).
5. Data captured into a child body must be moved or cloned (no implicit sharing of actor state).

**Yielding to the scheduler:**

For long-running computations, compiler-inserted `cooperate` safepoints yield the actor to the runtime scheduler. The compiler inserts these checks automatically at function entry and loop back-edges based on a reduction budget (see §9.0). `cooperate` is not a source-level expression:

```hew
fn heavy_computation() {
    for i in 0..1000000 {
        // cooperate is compiler-inserted on the loop back-edge
        process(i);
    }
}
```

### 4.9 Summary: Tasks vs Actors

| Aspect        | `fork` child task                                   | Actors                           |
| ------------- | --------------------------------------------------- | -------------------------------- |
| Communication | Explicit data passing on spawn + `await` for result | Message passing                  |
| Concurrency   | True parallelism (one OS thread per child)          | True parallelism (M:N scheduler) |
| Isolation     | Complete (no shared mutable state with parent)      | Complete (mailbox only)          |
| Failure       | First error becomes `ScopeError::primary`; siblings cancel | Traps isolated to actor   |
| Lifetime      | Bound to enclosing `scope` block                     | Independent                      |
| Cancellation  | Automatic at safepoints                             | Supervisor control               |
| Scheduling    | OS thread per child (execution); `await`/join suspension via the `llvm.coro` continuation substrate | M:N work-stealing scheduler |

**Design rationale:**

Hew combines Go's lightweight spawn ergonomics with Erlang's actor
isolation and Swift/Kotlin/Loom-grade structured concurrency:

- **Like Go**: a pair of short keywords — `scope { ... }` for the scope boundary and `fork name = call(...)` for child-start — with no nursery/scope object to pass around.
- **Like Erlang**: actors are isolated failure domains with supervisors; child tasks inside an actor cannot reach the actor's state.
- **Like Swift / Kotlin / Loom**: every child has a known parent block, the first failure cancels siblings, and no child error is silently dropped — `?` propagates `ScopeError::primary`.

**Historical note.**

`scope { ... }` is the scope boundary; `fork name = call(...);` inside a
scope is the child-start verb. These are not synonyms, and no
`s.launch / s.spawn / s.cancel` methods exist. Child execution runs on
the OS-thread-per-task runtime (`hew-runtime/src/task_scope.rs`); the
parent's `await` of a child suspends on the same unified `llvm.coro`
switched-resume continuation substrate (`hew-runtime/src/cont.rs`) used
by generator `yield` and actor `await`. The source-level choice between
spawn strategies is not user-visible.

### 4.10 Actor Await and Synchronization

> See HEW-FUTURE.md §1.3 for actor await, `await close(actor)`, and the
> read-after-send barrier — targeted for v0.6, gated on the I/O
> subsystem (#1236) settling and a re-audit of the mailbox protocol's
> failure modes.

### 4.11 Select and Join Expressions

Hew provides two built-in concurrency expressions for coordinating
multiple asynchronous operations. They are expressions — they produce
values — and integrate with structured concurrency and the actor model.

#### 4.11.1 `select` Expression

`select { }` is a **sealed compiler-known construct** in edition 2026. It
waits for the first of three named operation forms to complete, evaluates
the corresponding arm, and cancels the losing arms. There is no user-
implementable `Awaitable` trait — the three forms are exhaustive.

**Canonical syntax:**

```hew
select {
    reply   from worker.call(x)        => use(reply),     // actor ask
    item    from inbox.recv()          => use(item),      // channel receive
    after 5s                           => abort(),        // timer
}
```

The three arm-source discriminators are syntactic markers, recognised at
HIR lowering:

- `<actor-expr>.<method>(<args>)` — a method-call expression on an actor
  expression. The `ask` keyword is reserved for a future syntactic marker
  (see HEW-FUTURE) but is not lexer-recognised in edition 2026; the
  sealed-form discriminator is the method-call shape itself.
- `<receiver-expr>.recv()` — a std/channel receive on a `Receiver<T>`.
- `after <duration-expr>` — the timer arm; carries no binding.

> A stream-next arm (`<id> from <stream>.recv()` over a `Stream<T>`) and a
> task-await arm (`<id> from await <task>`) were specified in earlier drafts
> but are **not** part of edition 2026's sealed set: neither has a usable
> first-class substrate today (no `Stream<T>` handle is obtainable without
> aggregate-extraction that fails closed; `Task<T>` is unnameable and `fork`
> is parser-only). They return with their substrate — see HEW-FUTURE.

**The three forms (closed set).** Each form is fully specified by four
columns: what the winning arm binds, how the winning arm propagates a
non-success outcome at the source, how the runtime cleans up *that* arm
when a different arm wins (loser cleanup), and how the runtime cleans up
the arm when the enclosing scope is cancelled while the `select` is still
pending (outer-cancellation cleanup). Sources are the same in both
cleanup columns; the difference is which side initiates the teardown.

| Form                       | Winning bind / type             | Winning error or trap at the source                                                                                                                                                                                       | Loser cleanup (a different arm won)                                                                                                                                                                       | Outer-cancellation cleanup (enclosing scope cancelled, `select` still pending)                                                                              |
| -------------------------- | ------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `<id> from <actor>.<method>(<args>)` | `id: <reply-type>` per ask | `AskError` per HEW-DIST-SPEC §6 — `Partition`, `Timeout`, `Cancelled`, `LocalShutdown`, or `OrphanedAsk` as observed by the caller. Traps in the callee are isolated by the mailbox boundary and do not propagate through the ask. | If the envelope has **not yet been dispatched**, withdraw it from the target actor's mailbox by correlation id — no `OrphanedAsk` is observed on either side. If it **has been dispatched**, the reply sink is tombstoned; a late reply arriving at the tombstoned sink is classified as `OrphanedAsk` and discarded silently (no caller-visible failure). | Same as loser cleanup: withdraw-or-tombstone by correlation id, late reply classified as `OrphanedAsk` and discarded.                                       |
| `<id> from <rx>.recv()`    | `id: Option<T>` for `Receiver<T>` | `None` is a normal winning value indicating that the channel is closed; `Some(value)` carries the received item. Channel receive has no separate error surface in edition 2026.                              | Pending receive is withdrawn from the channel core; the receiver binding remains usable in the enclosing scope.                                                                                         | Same as loser cleanup: pending receive withdrawn, receiver binding remains usable for the cancellation handler.                                             |
| `after <duration>`         | no binding; arm type is `()`-shaped at the source | None. Timers cannot fail or trap in edition 2026.                                                                                                                                                                          | The timer is cancelled. No effect propagates.                                                                                                                                                            | The timer is cancelled. No effect propagates.                                                                                                              |

**Semantics:**

1. **Exhaustive arm set.** Each arm's source must be one of the three
   forms above. Anything else is `SelectArmInvalid` at parse or type-
   check time.
2. **First-completion wins.** The first arm whose source completes (or
   whose timer fires) wins. The bound identifier is in scope for that
   arm's `=>` expression.
3. **Loser cleanup is per-form.** The runtime applies the cleanup rule
   from the table above to every non-winning arm before the `select`
   expression returns. Cleanup runs synchronously from the `select`
   site's perspective; observable effects on other actors are
   asynchronous.
4. **Same-type arms.** All arm result expressions must have the same
   type `T`. The `select` expression has type `T`. There is no `T =
   Result<U, E>` flattening — if arms return `Result`, the `select`
   returns `Result`.
5. **Cancellation propagates outward.** If the enclosing `scope {}` is
   cancelled while a `select` is pending, every arm runs its loser-
   cleanup rule and the cancellation propagates through the `select`
   site as if it were any other safepoint.

**Type rule:**

```
select {
    p1 from act.call(x)      => r1,         where act.call(x): B, r1: T
    p2 from rx.recv()        => r2,         where rx: Receiver<D>, r2: T
    after d                  => r3,         where d: Duration, r3: T
} : T
```

The bound identifiers are in scope only inside their own `=>`
expression. Their static types follow the table above: `p1: B` for the
actor-ask arm, `p2: Option<D>` for the channel receive arm (so `None` is
a legitimate winning value indicating the channel observed EOF on that
call), and no binding for `after`.

**Why sealed?**

A user-implementable `Awaitable` trait would have to specify coherence
rules, cancellation hooks, fairness rules, pinning constraints, and a
loser-cleanup protocol — all unsettled in edition 2026. The three forms
above are the workloads `select` exists to serve. A user `Awaitable`
surface may land in a future edition once trait lowering and generator
cancellation are proven; see HEW-FUTURE.md.

**Implementation status (informative, not normative).** Edition 2026's
surface is the construct's contract. The three sealed arm forms — actor
ask, channel `recv()`, and `after` — type-check, lower, and reach live
codegen: the runtime substrate that decides the winner and runs each
form's loser-cleanup is wired (see the channel-receive and actor-ask
`select` vertical-slice fixtures, which compile and run). The arm set is
restricted by the **type checker**, not by codegen: a stream-next arm
(`<stream>.recv()` over `Stream<T>`) or a task-await arm
(`await <task>`) is rejected at check time with a structural diagnostic
("select arm source must be actor.method(args)"), because neither has a
usable first-class substrate in edition 2026 (see the note under
"Canonical syntax" above). They are not silently lowered and they never
reach codegen.

#### 4.11.2 `join` Expression

The `join` expression runs all branches concurrently and waits for all to complete, collecting results into a tuple.

**Static `join` (fixed number of branches):**

```hew
let (a, b, c) = join {
    actor1.compute(),
    actor2.compute(),
    actor3.compute(),
};
```

Each branch must be an actor receive handler call with a return type. An explicit `await` is accepted but is redundant inside `join`.

**Type rules:**

```
join {
    actor1.compute(),
    actor2.compute(),
    actor3.compute(),
} : (T1, T2, T3)
where actor1.compute(): T1, actor2.compute(): T2, actor3.compute(): T3
```

Each branch may have a different result type. The result is a tuple of all branch results, in declaration order.

Dynamic `join_all` remains reserved for future surface work; current Hew exposes static `join { ... }` for actor reply fan-out.

**Error propagation:** If any branch in a `join` traps, the remaining branches are cancelled and the trap propagates to the enclosing scope.

#### 4.11.3 `after` Timeout

The `after` keyword is used in two contexts:

**1. As an arm in `select` expressions** (see above):

```hew
select {
    result from server.fetch() => result,
    after 5s => default_value,
};
```

**2. As a timeout combinator with `|`** for individual await expressions:

```hew
let result = await counter.get_count() | after 1s;
// result: Result<i32, Timeout>
```

The `| after` combinator wraps the result in `Result<T, Timeout>`:

- If the operation completes before the deadline, returns `Ok(value)`.
- If the timeout expires first, the operation is cancelled and returns `Err(Timeout)`.

**Type rule:**

```
(e | after d) : Result<T, Timeout>
where e: Task<T>, d: Duration
```

#### 4.11.4 `scope`/`fork` and `select` Composition

Edition 2026 defines the legal compositions of `scope {}` and `select {}`
explicitly. Anything not listed is rejected by type checking, with the
diagnostic pointing at the offending position.

| Composition                                              | Legality        | Rationale                                                                                                                                                                                          |
| -------------------------------------------------------- | --------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `select {}` inside a `scope {}` body or child             | Legal           | The three `select` forms are single-await constructs and compose with the scope block's cancellation discipline at their safepoints.                                                                |
| `fork name = select { ... }`                             | Legal           | A child task's expression may be a `select` expression; the binding is the `select` expression's result type.                                                                                      |
| `scope {}` inside a `select` arm's `=>` result expression | Legal           | The arm has already won; its result expression runs in the surrounding scope as ordinary code that happens to contain a scope block.                                                               |
| `scope { ... }` as a `select` arm source                  | **Rejected**    | The three sealed arm sources are exhaustive (§4.11.1). A scope block is a *lexical region*, not a pending operation, and starting one as a `select` competitor would create children whose scope is unclear if the arm loses. Hint: wrap the fork in a child task and `await` the task instead. |

**Cancellation propagation across the composition (normative):**

- When a `scope {}` enters its cancelling state while a `select {}` in
  its body is still pending, every `select` arm runs its loser-cleanup
  rule (§4.11.1) and the cancellation then propagates through the
  `select` site as if the site were any other safepoint. The `select`
  does not return a value in this case; control unwinds.
- When a `select` arm wins inside a scope-block body, only the *losing*
  arms run their loser-cleanup. Sibling fork-children are not affected
  by the arm transition; their scope is bound to the enclosing scope
  block, not to the `select` site.
- A child task failing (typed `Err(E)` or trap) while a `select` in the
  scope-block body is still pending cancels the scope block; the
  outer-cancellation rule applies to the in-flight `select`.

The composition matrix is intentionally narrow in edition 2026. A
future edition may relax the `scope`-as-arm-source rejection once a
cancellation-token vocabulary (HEW-FUTURE.md §1.2) gives a scope block
a callable cancel handle that `select` can hold as a source.

### 4.12 Generators

> See HEW-FUTURE.md §1.6 for the remaining deferred generator forms (`async gen fn`,
> `receive gen fn`, `Lazy<T>`, `#[prefetch(N)]`). Scalar-parameter and fn-typed-parameter
> `gen fn` forms are live as of v0.5 (see §1.6 for detail).

---

## 5. Supervision (fault tolerance)

Hew's supervision is modeled after OTP concepts with first-class language syntax:

- Supervisor owns children; children fail independently.
- Restart classification: `permanent`, `transient`, `temporary`. ([Erlang.org][2])
- Supervisor strategy: `one_for_one`, `one_for_all`, `rest_for_one`, `simple_one_for_one`.
- Crash isolation via signal handling: SEGV/BUS/FPE/ILL in an actor is caught by the runtime, the actor is marked as Crashed, and the supervisor is notified for restart.

### 5.1 Supervisor Declaration

```hew
supervisor MyPool {
    strategy: one_for_one;
    intensity: 5 within 60s;

    child worker1: Worker(id: 1, count: 0);
    child worker2: Worker(id: 2, count: 0) restart: transient;
    child logger: Logger(level: 3) restart: temporary shutdown: 10s;
}
```

**Fields:**

- `strategy`: Restart strategy (`one_for_one`, `one_for_all`, `rest_for_one`,
  `simple_one_for_one`). Default `one_for_one`; the formatter always writes it
  explicitly so the restart contract is never silently defaulted.
- `intensity: N within <duration>`: the restart budget — at most `N` restarts
  within the rolling `<duration>` window. The window is a duration literal
  (`60s`, `5m`, `1h`), not a bare integer. Default `10 within 5s`.

**Child specifications:**

- `child <name>: <ActorType>(<field>: <expr>, ...)` — a static supervised child.
  Init args are named (positional args are rejected with a migration diagnostic).
- `pool <name>: <ActorType>(...)` — a dynamic pool child (only under
  `simple_one_for_one`); the args are the per-spawn template.
- Optional per-child suffix clauses, accepted in any order:
  - `restart: permanent | transient | temporary` (default `permanent`). This is
    the only restart spelling — bare `T permanent` and `with restart:` are not
    accepted.
  - `shutdown: <duration> | brutal_kill | infinity` — the graceful-stop
    deadline (default `5s`). `infinity` is accepted but not yet enforced (no
    per-child deadline wheel).
  - `wired_to: { <param>: <sibling>, ... }` — passes a sibling child's handle to
    this child's init param.
- Child actor types must be declared before the supervisor.

### 5.2 Restart Semantics (normative)

Let child exit reason be one of:

- `normal`
- `shutdown`
- `{shutdown, term}`
- `trap` (panic/abort or unrecovered error)

Then:

- `permanent`: always restart
- `temporary`: never restart
- `transient`: restart only if exit reason is not `normal`, `shutdown`, `{shutdown, term}` ([Erlang.org][6])

### 5.3 Restart Strategies

| Strategy              | Behaviour                                                           |
| --------------------- | ------------------------------------------------------------------- |
| `one_for_one`         | Only the crashed child is restarted.                                |
| `one_for_all`         | All children are stopped and restarted.                             |
| `rest_for_one`        | The crashed child and all children declared after it are restarted. |
| `simple_one_for_one`  | A pool-oriented strategy; only the specific crashed `pool` child instance is restarted. Required for supervisors that use `pool` declarations. |

### 5.4 Restart Budget and Escalation

The supervisor's `intensity: N within <window>` budget caps restarts; exceeding it escalates failure to the parent supervisor. The runtime tracks restarts in a sliding window.

**Exponential backoff** and **circuit breaker** are policy objects in `std::supervision`, not supervisor keywords — Hew leans on the standard library rather than the grammar for these tunable policies.

### 5.5 Nested Supervisors

A `child` declaration whose target is itself a supervisor with children is
implemented end-to-end. Dotted access to a nested child (`root.sub`) and
chained access through it (`root.sub.worker`) both lower through the
`hew_supervisor_nested_get` runtime call and return a fully typed
`LocalPid`.

The shape is:

```hew
supervisor Inner {
    strategy: one_for_one;
    intensity: 3 within 60s;

    child w1: Worker(id: 1, count: 0);
    child w2: Worker(id: 2, count: 0);
}

supervisor Root {
    strategy: one_for_one;
    intensity: 5 within 60s;

    child pool: Inner;
    child cache: CacheActor(capacity: 1000);
}
```

When a child supervisor's restart budget is exhausted, it escalates to its parent. The parent attempts to restart the entire child supervision subtree. If the parent's budget is also exhausted, the escalation propagates further up the tree.

### 5.6 Spawning and Accessing Supervised Children

```hew
fn main() {
    let pool = spawn MyPool;
    sleep(50ms);

    // Access children by declared name
    let w = pool.worker1;              // Typed: compiler knows w is a Worker
    w.tick();

    let w2 = pool.worker2;             // Dotted access is resolved statically
    w2.tick();

    supervisor_stop(pool);              // Graceful shutdown
}
```

- `spawn SupervisorName` — creates and starts the supervisor with all declared children
- `sup.child_name` — named child access via field syntax. The compiler resolves the child name to its index at compile time and returns a fully typed `LocalPid` for the child's actor type. The child name must match one of the `child` declarations in the supervisor definition.
- `supervisor_stop(sup)` — gracefully stops the supervisor and all its children

### 5.7 Crash Isolation

The runtime installs signal handlers for SEGV, SIGBUS, SIGFPE, and SIGILL. When an actor crashes:

1. The signal handler catches the signal on the worker's alternate signal stack
2. `siglongjmp` returns control to the scheduler's recovery point
3. The actor is marked as `Crashed` and a crash report is generated
4. The supervisor is notified and applies its restart strategy
5. The worker thread continues processing other actors

The `panic()` builtin triggers a controlled crash for testing.

---

## 6. Backpressure and bounded queues

Every actor mailbox has a bounded capacity and an overflow policy that determines behaviour when the mailbox is full.

### 6.1 Mailbox Declaration

<!-- doctest: skip -->
```hew
actor MyActor {
    mailbox 1024;                              // default: capacity=1024, overflow=block
    mailbox 100 overflow drop_new;             // explicit policy
    mailbox 100 overflow coalesce(request_id); // coalesce with key
    mailbox 100 overflow coalesce(request_id) fallback drop_new; // explicit fallback
}
```

> **Note:** The `coalesce` policy is planned for a future release and is not yet implemented.

### 6.2 Overflow Policies

| Policy               | Behaviour when mailbox is full                                       |
| -------------------- | -------------------------------------------------------------------- |
| `block`              | Sender suspends until space is available (cancellable). **Default.** |
| `drop_new`           | New message is silently discarded.                                   |
| `drop_old`           | Oldest message in the queue is evicted; new message is enqueued.     |
| `fail`               | Send returns an error to the sender.                                 |
| `coalesce(key_expr)` | Replace an existing message with a matching key (see §6.3).          |

Default:

- Actor mailbox: `capacity=1024`, `overflow_policy=block` for local sends, `drop_new` for network ingress unless overridden.

The compiler enforces that **all channels are bounded** (no accidental unbounded memory growth).

### 6.3 Coalesce Overflow Policy

The `coalesce(key_expr)` policy replaces an existing queued message that has the same coalesce key as the incoming message, rather than dropping or blocking.

**Syntax:**

```hew
actor PriceTracker {
    mailbox 100 overflow coalesce(symbol);

    receive fn update_price(symbol: string, price: f64) {
        prices.insert(symbol, price);
    }
}
```

**Semantics:**

The compiler generates a **coalesce key function** for each actor that uses the `coalesce` policy:

```
coalesce_key_fn: (msg_type: i32, data: *void, data_size: usize) -> u64
```

This function extracts the key expression value from the message payload and returns it as a `u64` hash.

When the mailbox is full and a new message arrives:

1. The runtime computes the coalesce key for the incoming message.
2. The runtime scans the queue for an existing message with the same `msg_type` AND the same coalesce key.
3. **If a match is found:** The existing message is replaced in-place (preserving its queue position). The old message data is freed and replaced with the new message data.
4. **If no match is found:** The **fallback policy** is applied. The default fallback is `drop_new`. An explicit fallback can be specified: `coalesce(key) fallback drop_old`.

**Key matching:**

- Keys are compared as `u64` integer equality.
- The coalesce key function is generated by the compiler based on the field expression in `coalesce(field_name)`.
- For integer fields, the key is the field value directly (zero-extended to u64).
- For string fields, the key is a hash of the string content.

**Evaluation timing:**

- The spec defines **observable semantics** only: messages with matching coalesce keys are replaced in-place; when no match exists, the fallback policy applies.
- The runtime MAY defer coalesce evaluation to message processing time (consumer-side) rather than send-time (producer-side). This permits lock-free mailbox implementations.
- The mailbox capacity is a **logical** bound. The runtime MAY temporarily accept messages beyond capacity if coalesce evaluation is deferred. After coalesce processing, the effective queue length MUST NOT exceed capacity.

> **Note:** Implementations using lock-free message queues MAY allow a transient overshoot of at most one message between producer enqueue and consumer coalesce scan. This transient state is not observable to the sending actor (the send succeeds or the fallback policy fires) and is resolved before the next message is dispatched to the receiving actor.

**Coalesce fallback policy:**

When the mailbox is full, no coalesce match exists, and the fallback must be applied:

| Fallback             | Behaviour                                            |
| -------------------- | ---------------------------------------------------- |
| `drop_new` (default) | Incoming message is discarded                        |
| `drop_old`           | Oldest message is evicted; incoming message enqueued |
| `fail`               | Error returned to sender                             |
| `block`              | Sender suspends until space is available             |

> **Note:** `coalesce` cannot be used as its own fallback. The fallback must be one of the non-coalesce policies.

### 6.5 First-Class Streams (`Stream<T>` and `Sink<T>`)

Hew provides two generic, move-only types for sequential I/O that can be passed between functions and stored in actor fields:

```
Stream<T>   // readable sequential source (analogous to an iterator that blocks)
Sink<T>     // writable sequential destination (blocks when backing buffer is full)
```

The contract frozen in this stage is intentionally small:

- `Stream<bytes>` / `Sink<bytes>` are the canonical first-class streaming foundation.
- `Stream<string>` / `Sink<string>` are convenience text ABI wrappers over the same bounded channel contract.
- Core `.recv()` / `.write()` operations are blocking and backpressured; this section makes no nonblocking promises.
- EOF means **end-of-stream only**. Zero-length `bytes` values and empty `string` values are valid data items.
- `sink.close()` or dropping a sink produces graceful EOF after buffered items drain.
- `stream.close()` or dropping a stream is local cancel/discard of unread items.
- Stage-1 errors cover constructor/open failures only. Transport/runtime read/write errors after open remain wrapper-specific and out of scope here.

Codec adapters are **not** part of this shipped stream runtime surface. The
compiler currently fails closed on `Stream.decode()` and `Sink.encode()` as an
unlowerable stream-codec boundary rather than exposing them as available
runtime features.

Both handle types are `Send` (safe to pass to other actors), opaque (backed by a vtable), and not `Clone`.

#### 6.5.1 Current `std::stream` surface

```hew
import std::stream;

// Canonical in-memory bounded bytes pipe
let (bytes_sink, bytes_stream) = stream.bytes_pipe(16);

// Convenience text pipe
let (text_sink, text_stream) = stream.pipe(16);

// Current file helpers remain text-only in this slice
let file_in  = stream.from_file("notes.txt")?;  // Result<Stream<string>, string>
let file_out = stream.to_file("out.txt")?;      // Result<Sink<string>, string>
```

`from_file()` / `to_file()` are intentionally unchanged in this slice: they
remain `Stream<string>` / `Sink<string>`. No file-adapter migration is implied
by this contract freeze.

#### 6.5.2 Current operations

```hew
// Pull items
match bytes_stream.recv() {
    Some(chunk) => { ... },
    None => { /* EOF only */ },
}

// Empty items are valid data, not EOF
text_sink.write("");
bytes_sink.write(b"");

// Close semantics
bytes_sink.close();   // graceful EOF for the paired reader
bytes_stream.close(); // local cancel / discard unread items
```

`for await` is the usual way to drain a stream. The only adapter point frozen in
this slice is that `lines()` remains `Stream<string> -> Stream<string>` today;
no `Stream<bytes>` `lines()` surface is promised here.

#### 6.5.3 Lifecycle Rules

- Closing or dropping a `Sink` signals graceful EOF to the paired `Stream`.
- Closing or dropping a `Stream` discards unread local data and releases the underlying handle.
- Streams and sinks implement `Resource`/`Drop` and **auto-close on scope exit** (RAII). Explicit `.close()` is available for early release but is not required.
- Types holding OS resources (streams, sinks, file handles) implement `Resource`/`Drop` and are **never arena-allocated**.

#### 6.5.4 Bidirectional connections

A bidirectional network connection (such as a TCP socket from `std::net`)
splits into a `(Stream<bytes>, Sink<bytes>)` pair via `.into_stream_sink()`:

<!-- doctest: skip -->
```hew
import std::net;

let conn = net.connect("127.0.0.1:8080")?;
let (rx, tx) = conn.into_stream_sink();
// rx: Stream<bytes>  — inbound data
// tx: Sink<bytes>    — outbound data
```

Accepted connections from a `TcpListener` expose the same method. The
`Stream<bytes>` and `Sink<bytes>` halves are independently move-able and
may be passed to separate actors. See `std/net/net.hew` for the full API.

#### 6.5.5 Relation to Actor Streams

`receive gen fn` produces a `Stream<Y>` backed by the actor mailbox protocol.
First-class `Stream<T>` values from `std::stream` are bounded handles that may
be moved across actor boundaries. Both are consumed with `for await`, but they
have different implementations:

|                     | Actor stream (`receive gen fn`) | `std::stream::Stream<T>`                   |
| ------------------- | ------------------------------- | ------------------------------------------ |
| Created by          | `receive gen fn` call           | `bytes_pipe()`, `pipe()`, `from_file()`, etc. |
| Backed by           | Actor mailbox protocol          | Bounded channel / wrapper-specific handle  |
| Passable as value   | No (tied to the spawned actor)  | Yes (move-only, `Send`)                    |
| Use in actor fields | No                              | Yes                                        |

---

## 7. Wire types and network contracts

Hew introduces `wire` definitions for network-serializable data.

### 7.1 Wire type requirements

A `#[wire] type` / `#[wire] enum`:

- has stable field tags (numeric IDs)
- has explicit optionality/defaults
- supports forward/backward compatibility checks

### 7.2 Compatibility rules (normative)

Hew adopts Protobuf-style invariants:

- **Field numbers (tags) must never be reused**. ([protobuf.dev][4])
- Deleted fields must have their tags **reserved** to prevent reuse. ([protobuf.dev][5])
- Changing a tag is treated as delete+add (breaking unless carefully managed). ([protobuf.dev][4])

Hew tooling provides:

- `hew wire check --against <schema>` to enforce these rules during builds or CI.

### 7.3 Encoding Formats

Hew supports multiple encoding formats. The runtime envelope (actor-to-actor transport) uses CBOR (ratified in R62; the CBOR path is the sole internode encoding, HBF retired and migration complete). The `std::encoding::*` surface provides user-level wire type serialization for cross-service and file I/O use cases.

#### 7.3.1 MessagePack — Default Binary Encoding

The MessagePack format is the primary shipped binary wire encoding for Hew wire types. MessagePack is a compact, language-agnostic serialization format that maps Hew types to MessagePack primitives efficiently.

Design goals: compact representation, fast encode/decode, language interoperability, forward/backward compatibility.

**Implementation reference:** The canonical type descriptor is defined in `hew-types/src/type_descriptor.rs` (`TypeDescriptor = ResolvedTy`). The `hew-wirecodec` crate was retired; wire codec consumers should use `TypeDescriptor::canonical_string()` and the wire-kind surface in `hew-types`.

##### 7.3.1.1 Wire Type–to–MessagePack Mapping

Hew wire types map to MessagePack formats as follows:

| Hew Type     | MessagePack Format      | Format Marker(s)           | Notes                            |
| ------------ | ----------------------- | -------------------------- | -------------------------------- |
| `bool`       | boolean                 | `0xc3` (true), `0xc2` (false) | Single-byte primitives           |
| `u8`–`u16`   | u64 (up to 16-bit)     | `0xcc`, `0xcd`             | Variable-length u64 encoding    |
| `u32`–`u64`  | u64 (up to 64-bit)     | `0xce`, `0xcf`             | Variable-length u64 encoding    |
| `i8`–`i16`   | i64 (signed, up to 16)  | `0xd0`, `0xd1`             | Variable-length signed encoding  |
| `i32`–`i64`  | i64 (signed, up to 64)  | `0xd2`, `0xd3`             | Variable-length signed encoding  |
| `f32`        | float32                 | `0xca`                     | IEEE 754 single precision        |
| `f64`        | float64                 | `0xcb`                     | IEEE 754 double precision        |
| `string`     | string                  | `0xa0`–`0xbf`, `0xd9`, ... | Length-prefixed UTF-8 string     |
| `bytes`      | binary                  | `0xc4`, `0xc5`, `0xc6`     | Length-prefixed raw bytes        |
| `#[wire] type` | map                     | `0x80`–`0x8f`, `0xde`, ... | Key–value pairs (field number keys) |
| `#[wire] enum`   | i64 + str (variant)     | Tag + variant index/name   | Encoded as MessagePack integer (variant index) or string (variant name) |
| Optional     | nil or value            | `0xc0` or type of Some(v)  | `None` encodes as MessagePack nil |
| List         | array                   | `0x90`–`0x9f`, `0xdc`, ... | Length-prefixed sequence         |

##### 7.3.1.2 Wire Type Map Encoding

A `#[wire]` type is encoded as a MessagePack **map**. Field numbers are used as map keys (as MessagePack integers), and values are encoded according to the table above.

```hew
#[wire]
type User {
    id: u64 @1;
    name: string @2;
    email: string @3 optional;
}

// User { id: 42, name: "alice", email: Some("alice@example.com") } encodes as:
// MessagePack map: {
//   1 (i64): 42 (u64),
//   2 (i64): "alice" (string),
//   3 (i64): "alice@example.com" (string)
// }

// User { id: 42, name: "alice", email: None } encodes as:
// MessagePack map: {
//   1 (i64): 42 (u64),
//   2 (i64): "alice" (string)
// }
// (optional field 3 omitted)
```

##### 7.3.1.3 Wire Enum Encoding

Wire enums are encoded as MessagePack **integers** representing the 0-based variant index:

`#[wire] enum` codec lowering is fully implemented: encode and decode are
emitted alongside the wire type codec path, unified on the CBOR body format.

```hew
#[wire]
enum Status { Pending; Active; Completed; }

// Status::Pending   -> CBOR integer: 0
// Status::Active    -> CBOR integer: 1
// Status::Completed -> CBOR integer: 2
```

##### 7.3.1.4 Optional Field Handling

Optional fields are represented using MessagePack **nil** for `None`:

```hew
#[wire]
type Config {
    timeout_ms: u64 @1;
    proxy_url: string @2 optional;
}

// Config { timeout_ms: 5000, proxy_url: None } encodes as:
// MessagePack map: { 1: 5000 }

// Config { timeout_ms: 5000, proxy_url: Some("http://proxy:8080") } encodes as:
// MessagePack map: { 1: 5000, 2: "http://proxy:8080" }
```

##### 7.3.1.5 List (Array) Encoding

Lists are encoded as MessagePack **arrays**. Each element is encoded according to the element type:

```hew
#[wire]
type Data {
    values: [i64] @1;
    tags: [string] @2;
}

// Data { values: [1, 2, 3], tags: ["a", "b"] } encodes as:
// MessagePack map: {
//   1: [1, 2, 3] (array of 3 ints),
//   2: ["a", "b"] (array of 2 strings)
// }
```

##### 7.3.1.6 Nested Structure Encoding

Nested `#[wire]` types are encoded recursively as MessagePack maps:

```hew
#[wire]
type Inner { x: i32 @1; }
#[wire]
type Outer { inner: Inner @1; nested_list: [Inner] @2; }

// Outer { inner: Inner { x: 150 }, nested_list: [Inner { x: 200 }] } encodes as:
// MessagePack map: {
//   1: { 1: 150 } (nested map),
//   2: [{ 1: 200 }] (array of nested maps)
// }
```

##### 7.3.1.7 Forward and Backward Compatibility

Unknown fields are preserved during round-trip encoding/decoding. When a `#[wire]` type carries fields unknown to a decoder, those fields are:

1. Decoded and stored in the decoder's internal unknown-fields store.
2. Re-encoded when the value is re-serialized.

This enables older code to accept and pass through messages containing fields added in newer schema versions.

##### 7.3.1.8 Field Ordering and Determinism

To enable deterministic encoding (important for hashing, signatures, and comparison):

- **Encoding:** Fields SHOULD be written in ascending field-number order.
- **Decoding:** Decoders MUST accept fields in any order.
- **Repeated fields:** If a field number appears multiple times in the wire, the last value wins (for scalars) or values are concatenated (for repeated fields).

##### 7.3.1.9 Versioning Guarantees

Wire types produced by the current compiler are compatible with any decoder that implements this §7.3.1 specification. Future versions may increment the descriptor version to signal breaking changes.

#### 7.3.2 JSON Encoding — External Interop

JSON encoding provides human-readable serialization for HTTP APIs, debugging, and external system integration.

##### 7.3.2.1 Mapping Rules

| Hew Type                               | JSON Representation                                         |
| -------------------------------------- | ----------------------------------------------------------- |
| `bool`                                 | JSON boolean                                                |
| `u8`, `u16`, `u32`, `i8`, `i16`, `i32` | JSON number                                                 |
| `u64`, `i64`                           | JSON string (to avoid precision loss)                       |
| `f32`, `f64`                           | JSON number (special: `"NaN"`, `"Infinity"`, `"-Infinity"`) |
| `string`                               | JSON string                                                 |
| `bytes`                                | JSON string (base64-encoded)                                |
| Lists                                  | JSON array                                                  |
| `#[wire] type`                         | JSON object with field names as keys                        |
| `#[wire] enum`                         | JSON string (variant name)                                  |
| Optional None                          | JSON `null` or field omitted                                |
| Optional Some(v)                       | JSON value of v                                             |

##### 7.3.2.2 Field Names

JSON field names are determined by the following rules, in priority order:

1. **Per-field override** — `json("name")` wire attribute sets the exact JSON key.
2. **Type-level convention** — `#[json(convention)]` attribute on the `#[wire] type` declaration transforms all field names. Valid conventions: `camelCase`, `PascalCase`, `snake_case`, `SCREAMING_SNAKE`, `kebab-case`.
3. **Default** — field name is used as-is (no transformation).

Per-field override always wins over the type-level convention.

```hew
#[json(camelCase)]
#[wire]
type User {
    user_name: string @1;                       // JSON: "userName"
    email_address: string @2;                   // JSON: "emailAddress"
    internal_id: string @3 json("id");          // JSON: "id"  (override wins)
}
```

JSON representation:

```json
{
  "userName": "alice",
  "emailAddress": "alice@example.com",
  "id": "u-42"
}
```

Without the type-level attribute, names are preserved exactly:

```hew
#[wire]
type User {
    user_name: string @1;
    email_address: string @2;
}
```

```json
{
  "user_name": "alice",
  "email_address": "alice@example.com"
}
```

##### 7.3.2.3 Enum Encoding

Wire enums encode as the string name of the variant:

```hew
#[wire]
enum Status { Pending; Active; Completed; }
```

```json
"Active"
```

For enums with associated data (future extension), encode as object:

```json
{ "Error": { "code": 500, "message": "Internal error" } }
```

##### 7.3.2.4 Unknown Fields in JSON

JSON decoders SHOULD ignore unknown fields (permissive parsing). This enables forward compatibility when newer services send fields unknown to older clients.

##### 7.3.2.5 Enum Variant Names in JSON

Enum variant names are used as-is by default. Apply `#[json(camelCase)]` (or another convention) to the `#[wire] enum` declaration to transform variant names consistently.

```hew
#[json(camelCase)]
#[wire]
enum Status { PendingReview; ActiveNow; Completed; }
```

```json
"activeNow"
```

#### 7.3.2a YAML Encoding

> See HEW-FUTURE.md §3 for `std::encoding::yaml` — targeted for v0.6+
> alongside the stdlib port-forward. Wire types may serialize as YAML
> via the same mapping rules JSON uses (§7.3.2); the normative YAML
> mapping waits for the next edition.

#### 7.3.4 Encoding Selection

Encoders select format based on context:

| Context                 | Default Format |
| ----------------------- | -------------- |
| Actor-to-actor (local)  | CBOR           |
| Actor-to-actor (remote) | CBOR           |
| HTTP API response       | JSON           |
| File storage            | user choice (`std::encoding::*`) |
| Debugging/logging       | JSON           |

Explicit format selection:

```hew
let msg = MyMessage { ... };
let binary = msg.encode();       // Hew Binary Format bytes
let json_str = msg.to_json();    // JSON string
let yaml_str = msg.to_yaml();    // YAML string
```

Decoding:

```hew
let msg1 = MyMessage.decode(binary);
let msg2 = MyMessage.from_json(json_str);
let msg3 = MyMessage.from_yaml(yaml_str);
```

Current shipped helper surface, as registered by the type checker:

- `#[wire] type` instance methods: `encode() -> bytes`, `to_json() -> string`,
  `to_yaml() -> string`
- `#[wire] type` static methods: `MyMessage.decode(bytes) -> MyMessage`,
  `MyMessage.from_json(string) -> MyMessage`,
  `MyMessage.from_yaml(string) -> MyMessage`
- unit-only `#[wire] enum` helpers are JSON/YAML-only:
  `to_json()`, `to_yaml()`, `from_json(string) -> Self`,
  `from_yaml(string) -> Self`

These constructors currently return the wire type directly rather than
`Result<Self, E>`.

---

## 8. Compilation model

Edition 2026 specifies the language. This section describes the
compiler's structural commitments at the level a language specification
needs to make — the names and responsibilities of the IR stages — and
leaves the implementation details (file paths, crate boundaries, dump
formats) to the compiler's own documentation.

### 8.1 The IR ladder

A Hew compiler accepts source files and produces native object code and
WASM modules. Between source and machine code, the compiler maintains
the following named intermediate representations:

```
Source (.hew)
    │  lex + parse
    ▼
AST                       — concrete syntax, no name resolution
    │  resolve
    ▼
Resolved HIR              — names, scopes, capabilities resolved;
    │  type check          stable BindingId / SiteId carriage
    ▼
THIR                      — every expression carries its concrete type;
    │  lower               monomorphisation and trait-dispatch done
    ▼
Raw MIR                   — real CFG, real Places, real terminators
    │  check
    ▼
Checked MIR               — fail-closed boundary: use-after-consume,
    │  elaborate           aliasing, init/use-after-move,
    ▼                       generator-borrow-across-yield,
Elaborated MIR              actor-send escape analysis
    │  emit                 (drops elaborated into the CFG)
    ▼
LLVM IR                   — emitted via inkwell; LLVM's own passes,
    │  llvm                 coroutine intrinsics, and target machine
    ▼
Native object / WASM
```

**What each stage guarantees:**

- **AST.** Concrete syntactic structure. No name resolution; no type
  information. Comments and whitespace stripped.
- **Resolved HIR.** Every name binding has a stable identifier; every
  use site resolves to a binding or to a `NameNotFound` diagnostic.
  Capabilities (Send, Frozen, Copy) attach here. Module structure is
  fully resolved.
- **THIR.** Every expression carries its concrete `Ty`. No `Ty::Var`
  survives this stage — the boundary is fail-closed. Generic functions
  are monomorphised at use sites; trait dispatch resolves to concrete
  implementations; closure signatures are explicit; aggregate
  initialiser type arguments are carried.
- **Raw MIR.** The function body is a control-flow graph of basic
  blocks with real terminators (`Goto`, `Branch`, `Return`, `Drop`,
  `Call`, `Unreachable`). Local variables are `Place`s. The
  `return;`-inside-an-`if` case has a CFG terminator, not a soft
  flag. Generator handles are typed `Place`s, not name-registry
  entries.
- **Checked MIR.** The semantic fail-closed boundary. Every program
  that survives this stage is guaranteed to be free of:
  - Use after consume (affine value moved and then used).
  - Aliasing violations (read-shared XOR mutate-unique).
  - Use after move.
  - Generator borrow across yield.
  - Actor-send escape (a value captured into an outgoing message that
    aliases live state in the sending actor).
  `@linear` must-consume obligations are discharged here; unconsumed
  `@linear` values surface as `MustConsumeAtScopeExit`.
- **Elaborated MIR.** Drops are first-class basic blocks in the CFG.
  Cleanup edges (panic, cancellation) are real edges. Every CFG exit
  runs the right destructor sequence in the right order. `@resource`
  types' implicit `close()` calls are emitted here.
- **LLVM IR.** Produced via the `inkwell` Rust binding to LLVM. LLVM's
  coroutine intrinsics handle generator state machines; LLVM's target
  machine handles native and WASM emission; LLVM's pass manager
  handles standard optimisations.

The compiler may collapse adjacent stages into a single in-memory
representation as an implementation detail, but the **responsibilities**
above are structural: a Hew compiler that skips a checked-MIR pass is
not a conforming compiler.

### 8.2 WASM target capabilities

The authoritative WASM capability matrix lives in
[`docs/wasm-capability-matrix.md`](../wasm-capability-matrix.md). The
specification tracks four target tiers:

- **Tier 1** (`wasm32-unknown-unknown` via `wasm-bindgen`, crate `hew-wasm`):
  analysis-only browser surface — lexer, parser, and type checker
  only. Powers the online playground and editor tooling. Does not
  execute Hew programs.
- **sandbox-vm-export** (`wasm32-unknown-unknown` via `wasm-bindgen`, crate
  `hew-sandbox-wasm`): deterministic bytecode package emission for the
  browser sandbox. Runs parse + type-check + explicit sandbox profile
  admission; does not execute programs directly.
- **sandbox-vm** (`hew-sandbox-vm` TypeScript worker): executes admitted
  sandbox bytecode in a browser Web Worker. Covers deterministic sequential
  code, actors (M4), channels + structured concurrency (M5), and supervision
  trees (M6). Almost all of Hew runs in a browser via this path; the only
  native-only feature class is OS-thread-dependent features (parallel
  work-stealing, production supervision restart policies, real-time network
  I/O). The full browser execution runtime for those thread-dependent
  features is the v0.6.0 browser-runtime lane.
- **Tier 2** (`wasm32-wasip1`): WASI execution runtime with a
  single-threaded cooperative actor scheduler.

Tier 2 surfaces are classified per feature as **Pass** (works as
implemented), **Warn** (works with documented semantic differences),
**Error** (compile-time rejected for lack of a coherent runtime path),
or **WASM-TODO** (backlog item not yet checker-gated). The capability
matrix is the source of truth for which feature falls into which bucket.

### 8.3 Linking

The compiler links emitted object code with `libhew_runtime` (the
runtime library), platform threading (e.g. `pthread`), and the math
library `-lm`, producing a standalone native executable. WASM
linking uses LLVM's WASM linker and a WASI libc; thread-dependent
runtime modules are gated out for Tier 2.

### 8.4 Runtime contract

`libhew_runtime` exports a stable C ABI consumed by every compiled Hew
program. It provides:

- An M:N work-stealing scheduler.
- Actor lifecycle (spawn, dispatch, stop, destroy) with the dispatch
  signature documented at §9.1.1.
- Bounded mailboxes with configurable overflow policies.
- Supervisor trees with restart strategies (§5).
- Built-in collection runtimes for `string`, `Vec<T>`, and
  `HashMap<string, V>`.
- Timer wheels and platform I/O integration (`epoll` / `kqueue` /
  `io_uring`).

The runtime ABI is committed to within a compiler major version. Edition
2026 does not specify the ABI's exact symbol set, but a compiler that
emits code calling a runtime symbol must link against a `libhew_runtime`
that provides it.

---

## 9. Runtime model

> **Detailed design:** The full M:N runtime architecture (C struct layouts, Chase-Lev deque,
> I/O poller integration, timer wheel, blocking pool, shutdown protocol) is documented in
> [`docs/dev/runtime-handle-api.md`](../dev/runtime-handle-api.md).

### 9.0 Scheduler Design

Hew uses an **M:N work-stealing scheduler** inspired by Go, Tokio, and BEAM:

**Thread model:**

- Worker threads (typically one per CPU core)
- Each worker has a local run queue of ready actors
- Idle workers steal from busy workers' queues
- Actors are scheduled as units (process messages until yield/await)

**Fairness guarantees (3-level preemption hierarchy):**

1. **Message budget (256 msgs/activation):** Coarse scheduler preemption — after processing 256 messages, the actor yields to the scheduler so other actors can run.
2. **Reduction budget (4000/dispatch):** The compiler inserts `cooperate` safepoints at function entry and loop back-edges. Each operation decrements a reduction counter; when exhausted, the actor yields to the scheduler.
3. **Cooperative task yield:** `await` and compiler-inserted `cooperate` safepoints suspend on the `llvm.coro` switched-resume continuation substrate (see §4.3 "Substrate" — internal to `hew-runtime`, not a source-level distinction), parking the current coroutine so the actor's executor can resume the next ready one.

- Round-robin within priority levels
- Starvation prevention through queue aging

**Memory management:**

- Per-actor heaps for isolation (no shared memory between actors)
- RAII with deterministic destruction (no garbage collector)
- Internal `Rc<T>` for single-actor shared ownership (runtime-only; not user-nameable); cross-actor sharing is expressed with owned messages / actor state rather than a surfaced Hew `Arc<T>` syntax
- Bulk deallocation on actor termination (entire heap freed)

**I/O integration:**

- Platform-specific event loops (epoll/kqueue/IOCP)
- io_uring support on Linux for high-performance I/O
- Separate thread pools for blocking operations
- Timer wheels for supervision windows and timeouts

### 9.1 Actor lifecycle state machine

> **Visual diagrams:** See [`docs/diagrams.md`](../diagrams.md) for state machine diagrams of actor lifecycle, supervisor, and distributed node states.

The actor state machine governs the lifecycle of an actor instance within the runtime scheduler. This is distinct from the **task state machine** (§4.1), which governs individual tasks spawned within an actor.

**Actor states** (discriminants match `HewActorState` in `hew-runtime/src/internal/types.rs`):

`Idle(0)`, `Runnable(1)`, `Running(2)`, `Suspended(3)`, `Stopping(4)`, `Crashed(5)`, `Stopped(6)`, `Sleeping(7)`, `Crashing(8)`

**Transitions:**

```
(spawn) ───► Idle           actor created, mailbox allocated, state initialized
Idle ──────► Runnable       message arrives in mailbox or timer fires
Runnable ──► Running        scheduler picks actor for execution on a worker thread
Running ───► Idle           message budget exhausted or no more messages; yields to scheduler
Running ───► Suspended      dispatch suspends at a non-final coro.suspend (slice-4 executor)
Suspended ─► Running        readiness source fires; executor resumes the continuation
Running ───► Stopping       supervisor requests shutdown, or actor calls stop()
Running ───► Sleeping       actor parks in the cooperative WASM sleep queue
Sleeping ──► Runnable       sleep timer fires
Stopping ──► Stopped        cleanup finished, normal exit
Running/Idle/Stopping ──► Crashing   unrecoverable trap caught; per-activation cleanup pending
Crashing ──► Crashed        cleanup complete; terminal state published via hew_actor_trap
Crashed ───► Stopped        crash finalized, supervisor notified
```

Actors start `Idle` after spawn. There is no separate `Blocked` state — actors waiting
for messages are `Idle` (or `Suspended` during a cooperative suspension) and become
`Runnable` when a message arrives.

**Key distinctions from task states (§4.1):**

| Aspect          | Actor State Machine                                                        | Task State Machine (§4.1)                     |
| --------------- | -------------------------------------------------------------------------- | --------------------------------------------- |
| **Entity**      | Entire actor instance                                                      | Individual task within an actor               |
| **Managed by**  | Runtime scheduler (Level 1)                                                | Actor-local coroutine executor (Level 2)      |
| **States**      | Idle/Runnable/Running/Suspended/Stopping/Crashing/Crashed/Stopped/Sleeping | Pending/Running/Completed/Cancelled/Trapped   |
| **Granularity** | One per actor                                                              | Many per actor (one per `fork` child)         |

Supervisor observes actor terminal states `Stopped` or `Crashed`.

#### 9.1.1 Actor Dispatch Interface

The runtime invokes actor message handlers through a **dispatch function pointer** with the following normative signature (from `hew-runtime/src/internal/types.rs` `HewDispatchFn`):

```c
void (*dispatch)(HewExecutionContext* ctx, void* state, i32 msg_type,
                 void* data, size_t data_size, i32 borrow_mode);
```

| Parameter     | Type                   | Description                                                                           |
| ------------- | ---------------------- | ------------------------------------------------------------------------------------- |
| `ctx`         | `HewExecutionContext*` | Pointer to the current execution context (scheduler, coroutine state)                 |
| `state`       | `void*`                | Pointer to the actor's private state (heap-allocated)                                 |
| `msg_type`    | `i32`                  | Integer discriminant identifying the message type (corresponds to `receive fn` index) |
| `data`        | `void*`                | Pointer to the serialized message payload                                             |
| `data_size`   | `size_t`               | Size in bytes of the message payload                                                  |
| `borrow_mode` | `i32`                  | Admissibility class for the payload (owned move, refcount retain, or deep copy)       |

**Requirements:**

- The dispatch function MUST be called with exactly 6 parameters. Implementations with fewer parameters are non-conforming.
- The `state` pointer MUST point to memory owned exclusively by the actor. No other actor or thread may access this memory during dispatch.
- The `data_size` parameter is REQUIRED for:
  - Safe deep-copy of message data into the actor's heap
  - Wire serialization (TLV encoding requires payload size)
  - Memory accounting per actor
- The `msg_type` value MUST correspond to the zero-based index of the `receive fn` declarations within the actor definition, in declaration order.
- `msg_type` is `i32`, not `i64`.

**Compiler-generated dispatch:**

For each actor, the compiler generates a dispatch function that switches on `msg_type` and deserializes `data` into the appropriate parameter types:

```c
// Generated for: actor Counter { receive fn increment(n: i32) { ... } receive fn get() -> i32 { ... } }
void Counter_dispatch(HewExecutionContext* ctx, void* state, i32 msg_type,
                      void* data, size_t data_size, i32 borrow_mode) {
    CounterState* self = (CounterState*)state;
    switch (msg_type) {
        case 0: Counter_increment(self, *(i32*)data); break;
        case 1: Counter_get(self, /* reply channel */); break;
    }
}
```

#### 9.1.2 Lifecycle Hooks

Actors expose user-defined startup, cleanup, crash-observation, and future
upgrade logic through **lifecycle hook annotations** on plain `fn`
declarations inside the actor body. The hook surface uses a single annotation
`on` with the hook kind as a positional argument, so additional hooks can be
added in later editions without growing the annotation vocabulary.

**Hooks defined in this edition:**

| Annotation       | Signature                                 | Runs when                                                                                       |
| ---------------- | ----------------------------------------- | ----------------------------------------------------------------------------------------------- |
| `#[on(start)]`   | `fn name()`                               | Once, after the actor's fields are initialized and before any message is dispatched.            |
| `#[on(stop)]`    | `fn name()`                               | Once per actor instance, on normal exit, cancellation by an enclosing `fork{}`, or supervisor `Shutdown`. |
| `#[on(crash)]`   | `fn name(info: CrashInfo) -> CrashAction` | After a child trap is classified and before restart-policy handling.                            |
| `#[on(upgrade)]` | reserved                                  | Reserved for future hot-upgrade machinery; not a usable hook in this edition.                   |

Unknown hook kinds (e.g. `#[on(restart)]`) are rejected with a diagnostic listing the valid set.

`#[on(crash)]` is a defined hook. The handler ABI is `(CrashInfo) -> CrashAction`;
the returned `CrashAction` is currently side-effects-only — supervisors honour each
child's `restart_policy` instead. `CrashAction` as a supervisor control surface is
reserved (HEW-FUTURE). `#[on(upgrade)]` is reserved for future hot-upgrade machinery
and is rejected during lowering.

**Signature rules (normative):**

1. A hook is a plain `fn` declaration inside an actor body carrying exactly one `#[on(...)]` annotation whose kind is `start`, `stop`, `crash`, or the reserved `upgrade`.
2. `#[on(start)]` and `#[on(stop)]` hooks take **no parameters**. Actor fields are in scope by bare name (the same convention as `init { }` and ordinary actor methods).
3. `#[on(crash)]` hooks take exactly one `CrashInfo` parameter and declare `CrashAction` as the return type. The return value is currently side-effects-only; `CrashAction` as a supervisor control surface is reserved (HEW-FUTURE).
4. `#[on(upgrade)]` is rejected during lowering; hot-upgrade machinery is reserved (HEW-FUTURE).
5. `#[on(start)]` and `#[on(stop)]` hooks return `()`.
6. A hook is **not** generic and has no `where` clause.
7. Hook functions are not invocable from message handlers; the runtime is the sole caller.
8. Multiple `#[on(stop)]` hooks are permitted and execute in **lexical order**.
   `#[on(start)]` and `#[on(crash)]` each appear **at most once** per actor.

**Cancellation and resource ordering (normative):**

9. Cancellation by an enclosing `fork{}` scope triggers `#[on(stop)]` for each cancelled actor before its task ends. This is the common path under structured concurrency, not an exceptional one.
10. The runtime sequence at terminal transition is:
   - (a) actor body exits or is cancelled;
   - (b) the `#[on(stop)]` hook runs with field access live, if present;
   - (c) `#[linear]` consumed-checks fire (unconsumed linear values surface as diagnostics);
   - (d) `#[resource]` field `close()` methods run in reverse declaration order.
   Hooks therefore run BEFORE `#[resource]` `close()`, so user logic in a hook can still use resources for goodbye flushes.
11. A panic in `#[on(start)]` aborts actor startup. The supervisor is notified; `#[on(stop)]` does NOT run, because the actor never reached the *started* state.
12. The default `#[on(stop)]` timeout budget is 5 seconds; future editions MAY introduce `#[on(stop, timeout = <duration>)]` to override per actor.

**Compilation:** `#[on(start)]` bodies are appended to the synthesized `_init`
function after any `init { ... }` block. `#[on(stop)]` lowers to the actor's
C-ABI `_terminate` function pointer. `#[on(crash)]` lowers to the crash hook
slot used by supervisor crash routing; its `CrashAction` result is currently
side-effects-only. `#[on(upgrade)]` is parsed as a reserved kind and fails
closed before codegen.

Cleanup logic is expressed as `#[on(stop)]` declarations; no free-standing
`terminate { }` block exists.

### 9.2 Supervisor state machine

States: `Healthy`, `Restarting`, `Escalating`, `Stopped`

Events:

- `ChildExit(child, reason)`
- `RestartBudgetExceeded`

Transitions:

- `Healthy --ChildExit--> Restarting` if policy says restart
- `Restarting -> Healthy` after successful restart
- `Healthy --RestartBudgetExceeded--> Escalating`
- `Escalating -> Stopped` if no parent; otherwise parent receives escalation

### 9.3 Channel send state machine

For a bounded channel with capacity `N`:

States: `HasSpace`, `Full`, `Closed`

Events:

- `Send(item)`
- `Recv()`
- `Close()`

Behaviour:

- In `Full`, overflow policy decides:
  - `block`: sender waits (cancellable)
  - `drop_new`: discard new item
  - `drop_old`: evict oldest then enqueue
  - `fail`: return error
  - `coalesce(field_name)`: replace existing message with same coalesce key (see §6.3 for full semantics)

**Coalesce syntax example:**

```hew
// Mailbox with coalescing based on request_id field
mailbox 100 overflow coalesce(request_id);

// When mailbox is full and a new message arrives:
// - If existing message has same request_id, replace it in-place
// - Otherwise apply fallback policy (default: drop_new)
```

> See §6.3 for the complete coalesce specification including key function generation, matching rules, and fallback policy configuration.

---

## 10. Debugging, profiling, and observability

> See HEW-FUTURE.md §4.1 for the tooling specification (`hew debug`,
> `HEW_PPROF`, `hew-observe`, LSP). Tooling tracks separately from the
> language edition; the implementations exist today, but their
> behavioural contracts are owned by `docs/observe.md`,
> `docs/troubleshooting.md`, and `docs/dev/lsp-editor-setup.md` rather
> than this document.

---

## 11. Distributed computing

The cross-node actor surface shipped across v0.5.x and is the default
runtime mode when a Hew program runs as a cluster node. The normative
specification is [`HEW-DIST-SPEC.md`](./HEW-DIST-SPEC.md); this section
summarises what is shipped and stable.

**Shipped cross-node surface:**

- Remote `send` and `ask` (`<- actor.method()` / `<id> from actor.method()`)
  with typed `SendError` and `AskError` result envelopes.
- Cross-node actor monitoring (`monitor` / `demonitor`) with exactly-once
  `DOWN` delivery; pruning on watcher-node death.
- Explicit cross-node links with `CrashLinked` cascade semantics.
- `PartitionPolicy::FailFast` and partition-detected-dead resolution for
  pending remote asks.
- SWIM-based membership with quarantine and incarnation-gated readmission.
- Two-process CI harness (13/13) as the distributed proving gate.

Authentication tokens and supervisor-capability enforcement are not yet
runtime-enforced; see `HEW-DIST-SPEC.md` §rc1-notes for the current
status. The SWIM quarantine/readmission surface is stable and in the
proving gate.

---

## 12. Syntax and EBNF (edition 2026)

The complete formal grammar is maintained in two files:

- **`docs/specs/grammar.ebnf`** — Authoritative ISO 14977 EBNF grammar (the canonical reference)
- **`docs/specs/Hew.g4`** — ANTLR4 grammar derived from the EBNF, validated against example programs

Both files cover the Edition 2026 grammar surface: modules, traits,
closures, pattern matching, control flow, `while let`, labelled loops,
structured concurrency, actor messaging operators, concurrency expressions,
generators, FFI, where clauses, f-string expressions, regex literals, match
operators, duration literals, `machine` declarations, and map literals.

When the grammar files and this specification disagree, the parser implementation (`hew-parser/src/parser.rs`) is the authoritative source of truth.

**Implementation note:** pipe closures lower through `Expr::Lambda`; captured closure environment records are the current substrate direction. Generic `<T>(...) => ...` is not a valid source syntax; type-parameterized lambdas are not supported in this edition (see §3.8.6).

### 12.1 Built-in Numeric Types

> Primitive integer annotations require explicit width (`i8`–`i64`, `u8`–`u64`) or
> platform width (`isize`/`usize`). The `int` and `uint` aliases are not valid types and
> are rejected. Integer literals default to `i64`; literal defaulting is not a
> user-nameable alias and does not affect wire shape or ABI.

| Type                      | Size          | Description             |
| ------------------------- | ------------- | ----------------------- |
| `i8`, `i16`, `i32`, `i64` | 1/2/4/8 bytes | Signed integers (fixed-width) |
| `u8`, `u16`, `u32`, `u64` | 1/2/4/8 bytes | Unsigned integers (fixed-width) |
| `isize`                   | platform      | Platform-sized signed integer: 32-bit on WASM32, 64-bit on native. Distinct from any fixed-width integer type. |
| `usize`                   | platform      | Platform-sized unsigned integer: 32-bit on WASM32, 64-bit on native. Distinct from any fixed-width integer type. |
| `f32`, `f64`              | 4/8 bytes     | IEEE 754 floating point |
| `bool`                    | 1 byte        | Boolean (true/false)    |
| `char`                    | 4 bytes       | Unicode scalar value    |

**Type aliases** (compile-time synonyms only — the aliased type is what the checker sees):

> **No active aliases.** The `byte` alias was retired in v0.5 and is now a compile-time error. Use `u8` directly.

Integer literals default to `i64`. Float literals default to `f64`.

**Mixed-width arithmetic requires an explicit cast.** The type checker rejects expressions
that mix distinct integer widths without a cast. Example:

```hew
let x: i32 = 1;
let y: i64 = x + 1;          // ERROR: i32 vs i64 width mismatch; use x as i64 + 1
let z: i64 = x as i64 + 1;   // OK
```

`isize` and `usize` are also distinct from each other and from any fixed-width type:

```hew
let n: usize = v.len();
let i: i32 = n as i32;       // explicit conversion required
let j: i64 = n as i64;       // explicit conversion required
```

All numeric types support `as` casts to every other numeric type:

```hew
// Integer → f64
let x: i32 = 42;
let f: f64 = x as f64;        // 42.0

// Float → integer (saturating)
let pi: f64 = 3.14;
let n: i32 = pi as i32;       // 3 (truncates toward zero for in-range values)

// Out-of-range and non-finite values saturate instead of producing poison:
let big: f64 = 1.0e30;
let clamped: i32 = big as i32;      // positive overflow clamps to 2147483647
let neg_big: f64 = -1.0e30;
let neg_clamped: i32 = neg_big as i32; // negative overflow clamps to -2147483648
let nan: f64 = 0.0 / 0.0;
let nan_as_int: i32 = nan as i32;       // NaN converts to zero
```

**`as` conversion semantics:**

| Conversion | Result |
| --- | --- |
| Integer → wider integer | Sign-extends signed sources and zero-extends unsigned sources. |
| Integer → narrower integer | Truncates to the target width's low bits. |
| Integer → float | Produces the nearest representable IEEE 754 value for the target float type. |
| Float → wider float | Extends precision. |
| Float → narrower float | Rounds to the nearest representable value for the target float type. |
| Float → integer | Truncates toward zero for in-range finite values, then saturates for out-of-range and non-finite values as specified below. |

**Float-to-integer `as` semantics:**

| Source value    | Signed result      | Unsigned result            |
| --------------- | ------------------ | -------------------------- |
| In-range finite | Truncated toward 0 | Truncated toward 0         |
| `+Inf` or > MAX | Integer `MAX`      | Integer `MAX` (`UINT_MAX`) |
| `-Inf` or < MIN | Integer `MIN`      | `0`                        |
| `NaN`           | `0`                | `0`                        |

These semantics are guaranteed on all Hew targets (x86_64, aarch64, wasm32). The underlying LLVM lowering uses `llvm.fptosi.sat` / `llvm.fptoui.sat`, which produce defined behaviour for all input values. Plain `fptosi` / `fptoui` (which produce LLVM poison for out-of-range inputs) are never emitted.

All numeric types also support exact fallible conversion methods:

```hew
let n: i64 = 2147483647;
let ok: Option<i32> = n.try_to_i32();        // Some(2147483647)

let past: i64 = 2147483648;
let too_large: Option<i32> = past.try_to_i32(); // None

let precise: i32 = 16777216;
let as_float: Option<f32> = precise.try_to_f32(); // Some(16777216.0)

let inexact: i32 = 16777217;
let not_exact: Option<f32> = inexact.try_to_f32(); // None
```

The methods `.try_to_i8()`, `.try_to_i16()`, `.try_to_i32()`, `.try_to_i64()`, `.try_to_u8()`, `.try_to_u16()`, `.try_to_u32()`, `.try_to_u64()`, `.try_to_isize()`, `.try_to_usize()`, `.try_to_f32()`, and `.try_to_f64()` return `Option<W>`. The result is `Some(w)` iff the source value round-trips through target type `W` exactly. The result is `None` for out-of-range values, negative values converted to unsigned targets, `NaN`, `+Inf`, `-Inf`, nonzero fractional parts in float-to-integer conversions, and inexact integer-to-float or float-to-float conversions.

### 12.2 Operator Precedence (highest to lowest)

1. Postfix: `?`, `.field`, `(args)`, `[index]`
2. Unary: `!` (logical NOT, bool-only), `-` (negate), `~` (bitwise complement, integer-only), `await`, `clone` (contextual prefix — see §3.4.4)
3. Multiplicative: `*`, `/`, `%`, `&*` (wrapping multiply)
4. Additive: `+`, `-` (`+` also concatenates strings), `&+` (wrapping add), `&-` (wrapping subtract)
5. Shift: `<<`, `>>`
6. Bitwise AND: `&`
7. Bitwise XOR: `^`
8. Bitwise OR: `|`
9. Relational: `<`, `<=`, `>`, `>=`
10. Equality: `==`, `!=`, `is` (`is` = reference identity on heap handles; `expr is TypeName` = static-only type check; rejected on string/scalars/records/tuples; regex matching is via `Pattern.is_match`)
11. Logical AND: `&&`
12. Logical OR: `||`
13. Range: `..`, `..=` (only lowered inside `for` loop iterables; standalone range value expressions are not lowered)
14. Timeout: `| after`
15. Assignment: `=`, `+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, `^=`, `<<=`, `>>=`

> **Overflow behaviour:** the plain `+`, `-`, `*` operators on integer types are checked — they lower to the `llvm.{s,u}{add,sub,mul}.with.overflow.iN` intrinsics and trap with `TrapKind::IntegerOverflow` on overflow. `&+`, `&-`, `&*` are the two's-complement **wrapping** versions of `+`, `-`, `*`: they lower directly to the plain `IntAdd`/`IntSub`/`IntMul` instructions (no overflow check; LLVM integers wrap by default) and exist as explicit source forms for opting into wraparound. All three wrapping operators have the same precedence as their plain counterparts. `.checked_*`/`.saturating_*`/`.wrapping_*` methods (see the language guide) provide the same three overflow policies as callable methods.

> **Comparison associativity:** comparison operators are left-associative. `a < b < c` parses as `(a < b) < c`, which compares a bool against an integer and is almost certainly a bug. Use `a < b && b < c` for chained comparisons.

> **Float context widening:** In a float-typed context, an integer literal widens to the contextual float type. `let f: f64 = 1;` is accepted and `1` is treated as `1.0`. This does not affect type annotations or wire shapes.

### 12.3 Duration Literals

Duration literals provide a concise syntax for time values. They compile to `i64` values representing nanoseconds:

```hew
let timeout = 100ms;     // i64: 100_000_000 nanoseconds
let interval = 5s;       // i64: 5_000_000_000 nanoseconds
let period = 1m;         // i64: 60_000_000_000 nanoseconds
let precise = 500us;     // i64: 500_000 nanoseconds
```

**Supported suffixes:**

| Suffix | Unit         | Conversion to nanoseconds |
| ------ | ------------ | ------------------------- |
| `ns`   | nanoseconds  | value (no conversion)     |
| `us`   | microseconds | value × 1_000             |
| `ms`   | milliseconds | value × 1_000_000         |
| `s`    | seconds      | value × 1_000_000_000     |
| `m`    | minutes      | value × 60_000_000_000    |
| `h`    | hours        | value × 3_600_000_000_000 |

**Type safety:**

`duration` is a distinct type — it does not implicitly convert to or from integers.

Duration arithmetic operators and accessor methods are fully implemented
(shipped in v0.5.4). Duration literals compile to `i64` nanosecond values;
arithmetic results are also `duration`.

```
duration + duration → duration
duration - duration → duration
duration * i64      → duration
duration / i64      → duration
duration % duration → duration
duration + i64      → COMPILE ERROR (type mismatch — enforced)
```

**Accessor methods** (return `i64`): `.nanos()`, `.micros()`, `.millis()`,
`.secs()`, `.mins()`, `.hours()`. **Instant arithmetic** is also
available via the builtin `instant` primitive (`instant::now()` — no
import required) — subtraction of two instants yields a `duration`. Both
`duration` and `instant` implement `Display`.

**Timeouts:**

Duration values are required for timeout expressions (`| after`) and `select` timeouts:

```hew
let result = await task | after 5s;        // Timeout after 5 seconds
```

```ebnf
DurationLit = IntLit ("ns" | "us" | "ms" | "s" | "m" | "h") ;
```

### 12.4 Labelled Loops

Loops (`loop`, `while`, `for`) may carry an optional **label** prefixed with `@`. Targeted `break @label` and `continue @label` are fully supported and transfer control to the enclosing loop that carries the matching label.

**Syntax:**

```hew
@outer: loop {
    @inner: while condition {
        if done {
            break @outer;
        }
        if skip {
            continue @outer;
        }
    }
}
```

Labels are scoped to the loop they annotate.

**Loops are statements, not expressions.**

`loop`, `while`, and `for` are statements — they do not produce a value. To carry a result out of a loop, declare a `var` binding before the loop and assign to it inside the body:

```hew
var result: i64 = 0;
loop {
    if found {
        result = computed_value;
        break;
    }
}
// use result here
```

`break` and `continue` are pure control-flow statements. A `break` may carry an expression (`break expr;`) — the parser accepts this syntax, but the expression is evaluated for side effects only and its value is **discarded**. Loop-as-expression (`let x = loop { break 42; }`) is not supported.

**Grammar:**

```ebnf
LabelledLoopStmt = Label? LoopStmt ;
LabelledWhileStmt = Label? WhileStmt ;
LabelledWhileLetStmt = Label? WhileLetStmt ;
LabelledForStmt = Label? ForStmt ;
Label          = "@" Ident ":" ;
LoopStmt       = "loop" Block ;
WhileStmt      = "while" Expr Block ;
WhileLetStmt   = "while" "let" Pattern "=" Expr Block ;
ForStmt        = "for" "await"? Pattern "in" Expr Block ;
BreakStmt      = "break" ("@" Ident)? Expr? ";" ;
ContinueStmt   = "continue" ("@" Ident)? ";" ;
```

The lexer tokenizes `@outer`-style labels as a dedicated label token; the EBNF
above shows their surface spelling.

### 12.5 `if let` and `while let`

`if let` and `while let` are first-class single-branch pattern-matching
constructs. They work on any type that supports pattern matching, including
`Option<T>`, `Result<T, E>`, enums, and `machine` values.

**`if let`** — execute a block only when a pattern matches, binding the
extracted value:

```hew
let opt: Option<string> = Some("hello");

if let Some(s) = opt {
    println(s);      // prints "hello"
}

// With else:
if let Some(s) = opt {
    println(s);
} else {
    println("nothing");
}
```

**`while let`** — loop as long as a pattern matches:

```hew
var m: HashMap<string, i64> = {"a": 1, "b": 2};

while let Some(v) = m.get("a") {
    println(f"{v}");
    break;
}
```

Both forms are semantically equivalent to the corresponding `match` form; they are compiled through dedicated IR paths rather than being desugared at the AST level. `if let P = expr { body }` corresponds to `match expr { P => { body }, _ => {} }`, but the lowering is a first-class HIR node, not a transformation.

> **Supported patterns:** Only payload-bearing constructor patterns (e.g. `Some(x)`, `Ok(v)`, `Err(e)`) and literal patterns work in `if let`/`while let`. Unit-variant, record, tuple, and or-patterns fail closed at HIR time.

```ebnf
IfLetExpr   = "if" "let" Pattern "=" Expr Block ("else" Block)? ;
WhileLetStmt = "while" "let" Pattern "=" Expr Block ;
```

---

## 13. Self-Hosting Roadmap

> See HEW-FUTURE.md §5.1 for the self-hosting roadmap — targeted for
> v1.0+. Bootstrap chain, minimum viable subset, kernel-language
> concept, and WASM-as-portable-bootstrap belong to the post-
> stability project.

---

## 14. "Researched outcomes" (what to build first to make Hew real)

1. **Actor + type safety baseline**: proven feasible and performant (Pony demonstrates the capability-typed actor approach can be implemented efficiently). ([tutorial.ponylang.io][1])
2. **Supervision semantics**: OTP restart categories are well-defined and battle-tested; encode them as primitives. ([Erlang.org][6])
3. **Cooperative cancellation**: structured concurrency with cooperative cancellation is a stable design pattern with clear semantics. ([docs.swift.org][3])
4. **Wire evolution invariants**: Protobuf’s “never reuse tags; reserve deleted tags” rules prevent real-world breaking changes and should be enforced by Hew tooling. ([protobuf.dev][5])

---

## 15. Minimum viable Hew (implementation plan aligned to this spec)

- **Phase A (compiler front-end)**: lexer/parser → AST → typecheck (Send/Frozen rules)
- **Phase B (runtime)**: scheduler, actor mailboxes, bounded channels, timers, TCP
- **Phase C (supervision)**: supervisor tree runtime + syntax lowering
- **Phase D (wire tooling)**: schema compiler + compatibility checker + encoder/decoder
- **Phase E (native codegen)**: LLVM backend + LTO + predictable allocation model

---

If you want this to be directly executable as an engineering project, the next most useful artifact is a “Hew Core IR” spec (the lowered form the compiler targets before LLVM), because it locks the semantics of actors, mailboxes, supervision, and cancellation independent of syntax.

[1]: https://tutorial.ponylang.io/index.html "Pony Tutorial"
[2]: https://www.erlang.org/docs/17/design_principles/sup_princ "Supervisor Behaviour - Restart Strategy"
[3]: https://docs.swift.org/swift-book/documentation/the-swift-programming-language/concurrency/ "Concurrency - Documentation | Swift.org"
[4]: https://protobuf.dev/programming-guides/proto3/ "Language Guide (proto 3) | Protocol Buffers Documentation"
[5]: https://protobuf.dev/best-practices/dos-donts/ "Proto Best Practices"
[6]: https://www.erlang.org/doc/apps/stdlib/supervisor.html "supervisor — stdlib v7.2"

---

## Changelog

> **Non-normative.** This section records what changed at the edition boundary
> for readers migrating existing code. For current invariants, see the spec body
> §N cited in each entry.

### Edition 2026 (this document)

- **Edition stamp.** Spec file renamed `HEW-SPEC-2026.md`; package descriptor
  declares `edition = "2026"`. Compiler version (`hew 0.5.x`) and edition
  track independently. See §1.3.
- **Resource markers.** `#[resource]` and `#[linear]` attribute annotations
  replace the v0.4.0 explicit-teardown carve-out (§3.7.8). `#[resource]` types
  declare a `close` method in a sibling `impl` block (unit return, plain
  receiver); the compiler synthesises an implicit drop that calls it.
  `#[linear]` types must be consumed via a declared consuming method and have
  no implicit drop.
- **Sealed `select{}`.** `select{}` widens from actor-receive-only to a
  three-form sealed construct over actor request-reply, channel receive
  (`pat from rx.recv()`), and timer (`after`) (§4.11). The earlier-drafted
  stream-`next` and task-`await` arms are deferred with their substrate
  (see HEW-FUTURE). Not user-extensible in this edition.
- **`scope{}` / `fork` split.** The `scope |s| { s.launch / s.spawn / s.cancel }`
  surface is removed entirely. `scope { }` is the structured-concurrency
  block (the scope boundary). `fork name = expr;` / `fork expr;`
  are the only child-start forms, and they are only legal inside a
  `scope { }` body. `scope` and `fork` are not synonyms.
  Historical note retained at §4.9.
- **Stdlib narrowing.** The edition 2026 normative stdlib is deliberately
  narrow (§3.10.1). Surfaces that exist in `std/` today but are not
  normative — `dns`, `tls`, `quic`, `websocket`, `xml`/`yaml`/`toml`/`csv`,
  `regex`, `process`, `compress` — move to HEW-FUTURE.md §3.
- **MIR ladder.** §8 (compilation model) is rewritten around the new IR
  ladder: AST → Resolved HIR → THIR → Raw MIR → Checked MIR → Elaborated
  MIR → LLVM IR via inkwell. The v0.4 Rust-frontend / C++ backend /
  MessagePack-AST pipeline is no longer the design.
- **Deferred to next edition.** Generators (`async gen fn` /
  `receive gen fn` / `Lazy<T>` / `#[prefetch(N)]`), closures with captured
  environment, user-facing `Arc<T>`, `dyn Trait`, `DoubleEndedIterator`,
  generic `HashMap<K, V>` over owned-aggregate/float keys, cancellation
  tokens, actor await + read-after-send barrier, and the self-hosting
  roadmap. See HEW-FUTURE.md for the surface and version targets.
  Channels (`std::channel`) and the rest of the `Iterator`/`IntoIterator`
  trait hierarchy shipped in this edition (§2.4, §2.5). Cross-node actor
  communication is shipped and normative — see §11 and HEW-DIST-SPEC.md.
