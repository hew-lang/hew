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
  0.6.0`). Patch releases for soundness and codegen fixes; minor releases
  for new stdlib surfaces and new language editions; the major release is
  the v1.0 stability event.
- **Spec edition** is a year-shaped identifier declared once per package in
  `hew.toml` as `edition = "2026"`. **2026** is the edition being developed;
  it has not yet stabilized. Editions are cadence-free: the next edition lands when
  accumulated breaking changes are worth a migration, expected every two
  to three years.

A compiler binary will advertise its supported editions once this
surface ships (planned, tracked for post-rc1):

```
$ hew --supported-editions
2026
```

Before stabilization, the native compiler cutover may make breaking changes
within edition 2026. Superseded syntax and APIs can be removed as the compiler,
standard library and documentation converge on the agreed design; selecting
edition 2026 is not yet a compatibility freeze.

After stabilization, the intended edition contract is compatibility for as
long as that edition remains supported. Additive features can land within an
edition; incompatible language changes require a later edition. This future
contract does not prevent the current pre-stability redesign.

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

The native runtime supplies platform-specific scheduling and synchronization;
the compiler links the libraries required by its target.

Rules:

- An actor processes **one message at a time** (no intra-actor races).
- Actors do not share mutable state. They communicate by sending **messages**.
- All actors are isolated by definition in Hew's actor model. No separate `isolated` modifier is needed — isolation is a fundamental property of all actors.

**Stopping an actor (normative).**

`close(actor)` requests cooperative stop and waits for terminal cleanup.
`closed(actor)` waits for termination without requesting it. `fork close(actor)`
starts the same operation concurrently and returns a `Task<()>`; it does not
remove the task's cleanup obligation. These calls return unit and are
idempotent for an actor that is already terminal (§4.10).

Inside a named actor body, bare `self` is the actor's own handle, so
`registry.register(self)` passes that identity. `self.field` still accesses
actor state. `this` is not a receiver token.

Acceptance of a mailbox submission is not proof that its handler completed.
Stopping can discard queued work. Use a completion call when the caller needs
to know whether a handler finished; handle its failure envelope (§2.1.1).

### 2.1.1 Actor Message Protocol

Actors expose message handlers using `receive fn`. Named actor `receive fn` methods are callable directly — no `.send()` or `.ask()` required.

**One actor identity (normative).** `Pid<A>` names an actor, local or remote.
There is no second surface handle for a remote actor: `RemotePid` is the wire
form the runtime uses to carry an identity between nodes, not a type a program
writes. `ChildRef<A>` stays distinct because it names a supervised *role*
rather than an incarnation, and re-resolves on every call (§5.6).

**Completion calls.** A call on a `receive fn` through an actor handle waits
for the handler to finish, exactly as a call on a function does.
`<pid>.<method>(<args>)` has type `Result<R, ActorError<E, Req>>`, where `R` is
the handler's return type and `()` when it declares none, and `E` is its
declared `fails` type and `Never` when it declares none. Written bare,
`ActorError` means `ActorError<Never, Never>`. `ActorError<E>` defaults only
`Req` to `Never`. A rejecting completion view infers a concrete sealed request
type; an ordinary waiting call uses `Never` because it cannot reject admission. `fork <pid>.<method>(<args>)` starts the same call concurrently
as a `Task<Result<R, ActorError<E, Req>>>` that `await` then joins. The call carries no
operator: an ordinary call waits, `fork` starts concurrent work, and `await`
joins a task. The handler does not run locally; the call returns when the
handler's turn has finished, which means processed, not durable.

One-way delivery is a separate, explicit surface: `mailbox(target, on_full:
...)` yields a view whose calls submit and return as soon as the message is
accepted, with the value `Result<Delivery, SendFailure<Req>>`. `Delivery`
reports `.Accepted` or an explicitly chosen `.Discarded`. A value-returning
handler cannot be called through a mailbox view; the diagnostic names
`fork target.m(..)` for concurrency. A `fails` handler that returns no value
may be submitted this way: with no caller to receive its declared error, an
`Err(e)` becomes the actor's own fault, carrying the error's `Display` text
into the diagnostic and reaching its supervisor. The error type must therefore
render — `string`, or a type with an `impl Display` body — or the submission
is refused.

A completion call chooses its own admission through the other view:
`policy(target, on_full: ...)` yields a view whose calls complete exactly as a
call on the handle does, with `Result<R, ActorError<E, Req>>`. `.Wait` is
the bare-handle behaviour and parks the caller while the destination mailbox is
full; `.Reject` refuses instead, and the call reports
`ActorError.Rejected(failure)`, with `failure.reason == SendError.Full`.
The owned request remains in `failure.message`; `.retry()` consumes it and
resubmits to the original actor, while `.to(other)` consumes it and resubmits
to a compatible handler. Both return the handler completion result. The
checker requires the same handler name, parameter types and reply contract.
Dropping the request releases its payload. Rejection is the only outcome
from which the same request may safely be resubmitted: every other variant means the request
was accepted or its fate is unknown. One view type per kind — `policy`
completes, `mailbox` submits — and both are immutable.

**Rejected requests.** A rejected completion or
submission returns `SendFailure<Req>`, which retains the unaccepted request
and exposes its `reason`. The payload stays sealed. Consuming retry resubmits
to the original target; consuming redirection checks the new target against
the request's handler and parameter types before resubmitting. A program
cannot manufacture or open a sealed request. This contract preserves an
affine payload on refusal. The request's type information survives storing
and later matching the failure, without relying on the original call site.

The outcome composes like any other `Result`: propagate with `?`, recover
with `handle` or `match`, or discard deliberately with `let _ = pid.m();`.
An accidentally discarded actor-call or submission Result is
`E_SEND_RESULT_DROPPED`. Neither an unbounded mailbox nor a unit-returning
handler removes the obligation to handle the outcome.

**Mailbox policy at the sender.** `mailbox(worker, on_full: .Wait)` yields an
immutable typed one-way view of the same actor and mailbox; a receive call
through that view submits under that policy. It mutates nothing and grants no
authority over other senders' work; it selects what *this* sender does when
the mailbox is full. The mailbox-view default is `.Reject`, which fails
immediately and hands the unaccepted payload back — transferred resources
included — so the caller can retry, redirect, or discard. `.Wait` parks until
the message is accepted, cancelled, closed, or timed out, and is the one
policy under which a submission suspends. `.DropNewest` drops the submitted
message and reports that disposition distinctly. Coalescing requires the
actor's own mailbox support for its key policy (§6.3). Capacity and queue-wide
eviction belong to the actor and its supervisor, never to a sender view.

No token marks an actor call site: an unmarked call on a handle waits for
completion, and the receiver type — handle or mailbox view — decides whether
the call waits or only submits. There is no actor-call operator or `send` keyword.

If the receiving handler faults before replying, the ask resolves to
`.Err(ActorError.Trapped)`. The receiving actor retains ownership of the
fault and its supervision policy; the caller may handle the error and continue.
Calling an actor does not join its lifetime to the caller's task scope. Caller
cancellation still follows the caller's own cancellation and cleanup edges.

```hew
actor Counter {
    var count: i64 = 0,

    // No return type: the call still waits until the handler has finished
    receive fn increment(n: i64) {
        count += n;
    }

    // Request-response: has a return type, the call waits for the reply
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
- **`receive fn` without return type** → completion. The call waits for the handler to finish and produces `Result<(), ActorError>`. Through a `mailbox(..)` view the same call submits instead, with the value `Result<Delivery, SendFailure<Req>>`.
- **`receive fn` with return type** → request-response. The call waits for the reply and produces `Result<R, ActorError>`. Inside a `select` arm the call is the arm's source, so the arm's `from` clause is what waits.

**Calling named actors:**

<!-- doctest: skip -->

```hew
let counter = spawn Counter(count: 0);

// No return type: the call waits until the handler has finished
counter.increment(10)?;

// One-way: submit without waiting
mailbox(counter, on_full: .Reject).increment(10)?;

// Request-response: has return type, the call waits for the reply
let n = counter.get()?;
```

**Sending messages:**

Lambda actors receive messages via call-syntax. Named actors expose typed receive methods:

<!-- doctest: skip -->

```hew
// Lambda actor: call the handle directly
let worker = actor |msg: i64| { println(msg * 2); };
let _ = worker(42);             // wait for completion

// Named actor: the call waits, and the outcome is not discardable
let _ = counter.increment(10);
```

**Message payloads (normative):**

A `receive fn` parameter is a **value** (snapshotted on send, §3.4.4), a **pid
handle** (`Pid`, `ChildRef` — copied, both sides address the
one actor), or an **opaque or resource handle** on a local send. A handle
payload is a move: the send consumes it and the sender's binding is dead
afterwards (`E_USE_AFTER_SEND`, §3.9.6). Such a handle may also be an actor's
init field, moved in at `spawn` and closed by the actor's drop glue at stop, so
one actor can own one connection and serve many requests over it. Across nodes
the rule is the wire rule: a remote payload must be CBOR-serializable, and a
handle is not (`E_OPAQUE_MESSAGE_PAYLOAD`). Counted handles (`Rc`, `Weak`,
`LambdaPid`) are never payloads, local or remote, because an actor's heap is its
own; closures, generators, and tasks are never payloads either
(`E_CALLABLE_MESSAGE_PAYLOAD`).

**Delivery outcomes (normative).**

A mailbox view returns `Result<Delivery, SendFailure<Req>>` for every
submission, whether the mailbox is bounded or unbounded. `Delivery.Accepted`
means the mailbox accepted responsibility for the message; it does not mean
that the handler has run. A policy that explicitly discards this submission
reports `Delivery.Discarded`. Refusal returns the unaccepted request and its
reason under the intended sealed-request contract above.

A direct handle call or a completion-policy view returns the completion
envelope. A handler's `fails E` result maps to `ActorError.Failed(E)`; a
handler without `fails` cannot produce that variant. A terminal destination
reports a lifecycle failure, never a successful no-op. Capacity and overflow
policy do not change these result types (§6).

**Current implementation limitations (informative).**

The following are gaps in the current native cutover, not alternative
language contracts:

- Actor identity unification is pending. Current source names local handles
  `LocalPid<A>` and remote handles `RemotePid<A>`; `Pid<A>` above is the
  intended unified identity.
- `close(sup)`, `fork close(sup)` and `closed(sup)` are decided supervisor
  forms, but native supervisor lowering has not adopted them. The current
  internal stop entry point is not the public language spelling (§5.6).
- Native `select` realizes task, timer and channel-receive sources. Actor-call
  registration remains pending (§4.11.1). Stream-next selection is not in the
  current classified source set.
- Stream codec adapters remain incomplete on the final path; ordinary
  Stream values and iteration are separate from that gap (§6.5).
- Native coalescing and ReplaceLatest realization require a checked key
  projection and remain pending (§6.3).
- Native supervision supports declared children; pools, sibling wiring and
  parts of the shutdown-deadline contract remain incomplete (§5).
- Captured WASI execution can still expose an internal trap tag as its exit
  status. This does not change the exit-1 rule for unrecovered faults (§5.8).
- Resource-close checking covers inherent methods; trait-method checking has
  not yet established the same blanket close rule in every form (§3.7.8).
- `into_iter()` consumes its vector, but the current cursor can clone
  cloneable elements. It is not a universal clone-free drain (§3.8.1).

These limitations do not establish sandbox or cross-platform execution parity.

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
    let endpoint: string,
    var failures: i64,

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
- Periodic handlers have unit success; the timer submits their turns without a reply consumer
- The timer starts when the actor is spawned and repeats until the actor stops
- Periodic handlers run within the actor's message loop, preserving single-threaded semantics

At runtime shutdown, periodic-timer admission closes. A periodic tick becomes
live work only when it is claimed for callback delivery. Shutdown waits for
callbacks already claimed and cancels all pending entries, including entries
that are due but not yet claimed; those entries are not delivered during
shutdown.

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
handle in the same family as `Pid` ("a pid you ask, `M` in →
`R` out"), where:

- `M` is the message type (from the parameter list: a single param's type, a
  tuple of the param types for multiple params, or `()` for no params)
- `R` is the reply type (from the `-> R` annotation, or `()` when omitted)

Both unit-returning and value-returning lambda actors use completion calls.
`handle(msg)` waits for the handler and yields the actor completion envelope
from §2.1.1. A unit-returning lambda can also receive one-way submissions via
`mailbox(handle, on_full: .Reject)(msg)`. `policy(handle, on_full: .Reject)(msg)`
selects completion admission. There is no lambda-specific `.send()` operation.

A lambda actor lowers to an ordinary actor declaration: captures become state
fields and its body becomes one receive handler. Its handle supports
`close(handle)` and `closed(handle)`. A handle can be stored in a record or
collection and called through that place; it cannot be split into channel
halves. Copies retain the same actor identity rather than duplicating its state.

A lambda that names the binding holding its own handle is refused. Use a named
actor for recursive protocols, keeping actor state separate from the handle
that addresses it.

**Spawning:**

```hew
actor Counter {
    var count: i64,
    receive fn value() -> i64 { count }
}

fn main() {
    // Spawn a named actor
    let counter = spawn Counter(count: 0);

    // Lambda actor expression returns LambdaPid<M, R>
    let worker: LambdaPid<i64, ()> = actor |msg: i64| { println(msg); };   // unit reply
    let adder: LambdaPid<i64, i64> = actor |x: i64| -> i64 { x + 1 };      // value reply
    close(counter);
    close(worker);
    close(adder);
}
```

**Capture semantics:**

Captures follow the value and transfer rules of §3.4.5. Ordinary data is
captured as an independent value; resource transfer consumes the source.
Captured values must satisfy the actor boundary's sendability requirements.
`move` requests transfer explicitly. It is not needed merely to capture a
string or another ordinary value.

**Operations:**

```hew
fn main() {
    let worker = actor |msg: i64| { println(msg); };
    let _ = worker(42);                              // wait for completion
    let _ = mailbox(worker, on_full: .Reject)(43);    // observe submission
    close(worker);                                  // wait for terminal cleanup
}
```

**Interaction with `scope`:**

An actor remains a separate failure domain. A lambda-actor fault does not
become a sibling task's fault merely because its handle was created in a
`scope`. A completion caller receives the actor error envelope. Structured
child tasks created with `fork` retain their own scope obligations.

Handles follow ordinary ownership cleanup. Use `close(worker)` when the code
requires the actor's terminal cleanup to complete at a particular point; use
`closed(worker)` to observe termination without requesting it.

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

> **Error propagation:** `Result<T, E>` and `Option<T>` are first-class. `?` propagates absence only into an enclosing `Option` return, and errors only into an enclosing `Result` return with a compatible error type (§2.2.1). It never converts absence into an error or discards an error as absence.

### 2.2.1 Error Propagation

**Fallible functions.** `fn read() -> T fails E` declares success type `T` and
one error type `E`; its call produces the same `Result<T, E>` value representation
used elsewhere. Ordinary returns and the function tail produce exactly `T`.
`return error problem;` returns an `E` failure. There is no implicit forwarding
of a complete Result at a success boundary: use `?` or `handle` explicitly.
This distinction also holds when `T` is itself a Result, tuple or record.

`error` remains an ordinary binding name. Member access, calls, indexing,
propagation and operators take priority after `return`: `return error.fmt();`
returns that method's result, and `return error + 1;` returns the sum. For an
ambiguous failure payload, use a named local or a block, such as
`return error { -1 };` or `return error { .Invalid };`.

```hew
fn pair() -> (i64, string) fails string {
    return (10, "hello");
}

fn unavailable() -> i64 fails string {
    return error "not available";
}
```

An error return targets its enclosing fallible function, not an enclosing
handler. A nested closure has its own return context. Error returns do not
trigger supervision or convert runtime faults and cancellation into Results.

**Local recovery.** `optional_value ?? fallback` evaluates its left operand
once. A present value supplies its payload; only absence evaluates `fallback`.
The fallback must have the payload type (or diverge). `??` accepts only
`Option`, never `Result`; it associates right and binds below logical operators.

`result_value handle problem { recovery }` evaluates its Result operand once.
Success supplies the success payload. Only an error runs the block, with
`problem` bound to the error payload. The block must produce the success
payload type or diverge. `handle` is contextual, and the error binder is
user-named and scoped to the block. It is not a surrounding exception handler:
an inner `?` in the operand retains its enclosing function's return edge.

Handler blocks are ordinary lexical blocks: `return`, `break`, `continue`,
`await`, captures and sends retain their usual contexts and contracts. There
is no implicit task, retry or asynchronous continuation. Errors produced in a
handler still require handling. Typed Result recovery does not intercept
runtime faults or clear cancellation. A handler attached directly to a scope
is the separate structured-failure boundary described in §4.2.

`let value = optional_value else { divergent_block };` requires a present
optional payload and binds it for the rest of the enclosing scope. A type
annotation describes that payload. The else block cannot see the new binding
and must diverge. Without `else`, an ordinary `let` preserves the Option value.
Explicit variant-pattern let-else remains available for other patterns.

The `?` operator propagates errors from `Result` and `Option` types:

```hew
fn read_file(path: string) -> Result<string, string> {
    let handle = open(path)?;  // Early return on error
    let content = read(handle)?;
    Ok(content)
}
```

A `receive fn ... -> T fails E` uses the same `return error e` and `?`
rules as a fallible function. A completion caller receives `Failed(e)` in its
actor envelope. If the handler's success type is unit and it is submitted
through a mailbox view, there is no reply consumer: its declared failure
becomes an actor fault with the error's `Display` text (§2.1.1). A handler
that explicitly returns `Result<T, E>` without `fails` returns that Result as
an ordinary reply value; it is not implicitly flattened or logged.

**`?` is exact (normative).** The error type of the operand must be the error
type of the enclosing function. Two concrete error enums never convert into one
another, implicitly or otherwise: there is no `From`-driven `?`, no `#[from]`
attribute, and no compiler-inserted conversion call. A `?` whose operand error
type differs from the function's is a type error.

**`dyn Error` is the one composition point.** `Error` is a prelude trait
declared in `std/builtins.hew`:

```text
trait Error: Display {}
```

Every public error enum in `std` and in a `hew.` package implements `Display`
and `Error`. When the enclosing function's error type is `dyn Error`, `?`
applies the ordinary `dyn Trait` coercion the language already performs in any
`dyn Trait` value position — the concrete error is erased into the trait
object, and nothing else about `?` changes. `dyn Error` is therefore the type a
function names when it composes errors from several modules, and a concrete
enum is what it names when it does not.

`Display` resolves through the supertrait on a `dyn Error` value, so
`f"{e}"` and `println(e)` work on one. Supertrait shapes the checker cannot yet
resolve through a trait object are refused with `E_DYN_SUPERTRAIT` (User
channel) rather than silently losing the method.

**Explicit conversion is a call.** A concrete error becomes another error, or
an absent value becomes an error, through methods the caller writes:

| Call | Meaning |
| --- | --- |
| `Result<T, E>.map_err(f)` | `f: fn(E) -> F` applied to the `Err` payload, yielding `Result<T, F>` |
| `Option<T>.ok_or(e)` | `Some(v)` becomes `Ok(v)`; `None` becomes `Err(e)` |
| `Result<T, E>.expect(reason)` | the `Ok` payload, or a trap carrying `reason` |

`expect(reason)` is the one deliberate crash-on-failure form. There is no
`unwrap()`: a crash whose message is the error text tells a reader what
happened but never why the author expected it not to. `expect` requires the
reason, so an invariant assertion is written as one.

**`main` returning a `Result`.** `fn main() -> Result<(), E>` requires
`E: Error`. On `Err(e)` the runtime writes `error: {e}` to stderr using the
error's `Display` text and exits with `user_code` 1 (§5.8). On `Ok(())` it
exits 0.

---

## 3. Types, ownership, and sendability

### 3.1 Type categories

#### Product type declaration

`type` is Hew's sole product-type declaration keyword. A named product uses
`type Name { field: Type, ... }`; a positional product uses
`type Name(Type, ...);`. Product values are immutable after construction
unless their binding is declared with `var`, and retain the structural
equality, hashing, construction, pattern, and tuple-constructor capabilities
previously associated with the removed `record` keyword. `record` is not a
keyword or declaration form; source using it is rejected with a migration hint
to use `type`.

- **Value types** (copy): integers, floats, bool, char, small fixed aggregates.
- **Owned heap types**: `string`, `bytes`, `Vec<T>`, `HashMap<K,V>`, user-defined types.
- **Shared immutable types**: `Frozen` values are the conceptual shared-immutable category. The runtime has internal `Arc`/ABI support, but no user-facing `Arc<T>` type is exposed (HEW-FUTURE §2.3).
- **Actor references**: `Pid<A>` is sendable.
- **I/O stream types**: `Stream<T>` (readable) and `Sink<T>` (writable) — move-only, `Send`, first-class sequential I/O handles (§6.5).

#### Variant spelling (normative)

One spelling serves both positions and every enum. A variant is written
`.Variant` when the expected type selects the enum, and `Type.Variant`
otherwise. This holds in pattern position and in expression position alike,
for user-declared enums and for the prelude variants `Some`, `None`, `Ok`,
and `Err` — there is no prelude exception, because a prelude exception is a
second rule to teach where one does the work.

The bare spelling — a variant name with neither the leading dot nor a type
qualifier — is **not** part of edition 2026:

- In pattern position it is a hard error, `E_BARE_VARIANT_PATTERN`, for every
  enum including `Option` and `Result`.
- In expression position it is `E_BARE_VARIANT_EXPR`, enforced for
  user-defined enums. The four builtin variants (`Some`, `None`, `Ok`, `Err`)
  still type-check bare in expression position at v0.6.0: the checker's
  builtin-name dispatch resolves them before the rule can fire. Closing that
  gap is tracked by issue #3240.

Both diagnostics carry a machine-applicable fix-it that replaces `X` with
`.X` where the context selects the enum, and with `Type.X` where it does not.
`hew fmt --migrate` applies those fix-its across a source tree, so a pre-2026
program is rewritten rather than hand-edited.

State names inside a `machine` declaration are not variants at the surface,
and this rule does not reach them (§3.11.3).

### 3.2 Mutability

- Bindings are immutable by default: `let`.
- Mutable bindings: `var`.
- Type fields have no mutability qualifier. Field mutation is governed by the binding: a `var`-bound value allows `p.x = …`; a `let`-bound value rejects it. The same wall covers every place rooted at the binding — `p.x = …`, `v[0] = …`, `m["k"] = …`, and `t.0 = …` — and calling a method declared `var self` (§3.7.1), which is how a value-category type is mutated.

**The binding wall (normative).** A `let` binding cannot be reassigned, and no
place rooted at it can be assigned. `p.x = …`, `v[0] = …`, `m["k"] = …`, and
`t.0 = …` on a `let` binding are all `MutabilityError` (User channel) carrying
the `let`→`var` fix-it. The wall is about the binding, not about the syntax
used to reach through it.

**The mutating receiver.** A method that mutates its receiver declares
`var self` (§3.6). Calling one needs a `var` binding: `let p = Counter { n: 0 };
p.bump()` is refused with "requires a mutable binding receiver". The mutating
builtin collection methods are declared the same way —
`Vec.push`/`pop`/`set`/`insert`/`remove`/`clear`/`sort`/`reverse`/`truncate`/`swap`,
`HashMap.insert`/`remove`/`clear`, `HashSet.insert`/`remove`, and `bytes.push`
all take `var self` — so `let v: Vec<i64> = Vec.new(); v.push(1)` is refused
and `var v` accepts it. There is no separate "interior mutability" rule for
collections.

Handle operations act on a resource or actor identity (§3.4.3). Their
declared receiver still controls access: a borrowing method may use a `let`
handle, a `var self` method requires mutable access, and `consume self`
ends the owner's lifetime. Actor calls do not mutate the handle binding.

The "declared mutable but never reassigned" warning reads "declared `var` but
never mutated; use `let`" and counts a `var self` call as a mutation, so
`var v: Vec<i64> = Vec.new(); v.push(1)` warns nothing.

A vector can own elements that have no copy operation, including generators.
`push` and `set` copy copyable elements and consume non-copyable elements.
`pop` transfers an element out. Indexing and `get` copy a cloneable element
or borrow a clone-free element; a borrowed read does not grant ownership.
A vector of non-copyable elements cannot itself be copied. Generic unbounded
elements are always borrowed (§3.8.1).

Replacing, clearing or dropping elements completes their cleanup before
execution continues. A failed bounds check leaves the receiver and any new
element owned by the caller's fault-cleanup path; neither is lost or transferred
into an invalid slot.

### 3.3 Sendability / isolation rule

A value may cross an actor boundary only if it satisfies **Send**.

`Send` is satisfied if one of the following holds:

- the value is a value type (integers, floats, bool, char), or
- the value is **owned** and transferred (move) with no remaining aliases, or
- the value is `Frozen` (deeply immutable), or
- the value is an actor reference (`Pid<A>`)

This is the central compile-time guarantee: **no data races without locks**, aligning with capability-based actor safety in Pony. ([tutorial.ponylang.io][1])

#### 3.3.1 Automatic Derivation Rules

The compiler automatically determines `Send` and `Frozen` for user-defined types. Users do NOT manually implement these traits.

**Send derivation:**

| Type                               | `Send` if...                               |
| ---------------------------------- | ------------------------------------------ |
| Value types (i32, f64, bool, char, isize, usize) | Always `Send`                |
| `string`                           | Always `Send` (immutable-shareable owned type; alias-shared by refcount retain on send — not deep-copied) |
| `Pid<A>`                      | Always `Send`                              |
| `type S { f1: T1; f2: T2; ... }`   | All fields are `Send`                      |
| `enum E { V1(T1), V2(T2), ... }`   | All variant payloads are `Send`            |
| `Vec<T>`                           | `T` is `Send`                              |
| `HashMap<K, V>`                    | `K` and `V` are both `Send`                |
| `Option<T>`                        | `T` is `Send`                              |
| `Result<T, E>`                     | `T` and `E` are both `Send`                |
| `(T1, T2, ...)`                    | All elements are `Send`                    |
| `[T; N]`                           | `T` is `Send`                              |

> **Note on array annotations:** `[T; N]` is a fixed-size array. The bare
> `[T]` spelling is accepted and is a synonym for `Vec<T>` — `let xs: [T] = ...`
> and `fn f(xs: [T])` both type-check and lower as `Vec<T>`, so an `[T; N]`
> value does not satisfy an `[T]` annotation (``expected `Vec<i64>`, found
> `[i64; 3]` ``). Prefer the explicit `Vec<T>` spelling for dynamically-sized
> sequences. Fixed arrays support `.len()`, indexing and replacement through a
> mutable binding. Their length remains part of the type through calls, returns
> and nested values. The compiler chooses storage; the source type does not
> promise stack allocation. `[seed; N]` evaluates `seed` once, even when `N` is
> zero, and requires a cloneable element when `N` exceeds one.

**Frozen derivation:**

| Type                          | `Frozen` if...                            |
| ----------------------------- | ----------------------------------------- |
| Value types                   | Always `Frozen`                           |
| `string`                      | NOT `Frozen` (mutable content)            |
| `Pid<A>`                 | Always `Frozen` (identity reference only) |
| `type S` where all field types are `Frozen` | `Frozen` (recursive over field types) |
| `type S` where any field type is not `Frozen` | NOT `Frozen`                        |
| `enum E`                      | All variant payloads are `Frozen`         |
| `Arc<T>`                      | _Not currently surfaced in Hew source; runtime-only support today_ |
| `Vec<T>`, `HashMap<K,V>`      | NOT `Frozen` (mutable containers)         |

> **Soundness requirement:** The compiler MUST reject as non-`Send` any type whose `Send` status cannot be determined (e.g., opaque foreign types). Foreign types are non-`Send` by default. The runtime/ABI has internal escape hatches, but there is currently **no surfaced Hew `#[send]` attribute** for user code.

### 3.3.2 The `bytes` Type

`bytes` is a built-in compiler type with stdlib-registered methods: a mutable, heap-allocated byte buffer — semantically a `Vec<u8>` — but with a dedicated type name:

```hew
fn main() {
    var buf: bytes = bytes.new();
    buf.push(0x48);    // push a byte value (i64)
    buf.push(72);      // same as 'H' in ASCII
    let n = buf.len(); // i64
    let b = buf.get(0); // Option<u8> — first byte, or None when out of range
    buf.set(1, 0xFF);   // overwrite byte at index 1
    let last = buf.pop(); // Option<u8> — removes and returns last byte, None when empty
    println(buf.is_empty()); // bool
    println(buf.contains(72)); // bool — linear scan
    println(n);
    println(last.unwrap_or(0));
    println(b.unwrap_or(0));
}
```

**Methods on `bytes`:**

| Method         | Signature          | Description                     |
| -------------- | ------------------ | ------------------------------- |
| `bytes.new()` | `() -> bytes`      | Create an empty byte buffer     |
| `.push(b)`     | `(i64) -> ()`      | Append a byte                   |
| `.pop()`       | `() -> Option<u8>` | Remove and return the last byte; `None` when empty |
| `.get(i)`      | `(i64) -> Option<u8>` | Byte at index `i`; `None` out of range |
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
- Mutation through an actor's `var` state remains serialized

Therefore, within an actor, values behave like a normal single-threaded
language (Python, JavaScript, single-threaded C). Actor state can be updated
through `var` bindings without reference or lifetime annotations.

#### 3.4.2 Comparison to Other Languages

| Language         | Approach                                                   | Why                                            |
| ---------------- | ---------------------------------------------------------- | ---------------------------------------------- |
| **Rust**         | Full borrow checker everywhere                             | Prevents races in arbitrary threaded code      |
| **Pony**         | Capability system (iso, ref, val, etc.)                    | Fine-grained control for lock-free concurrency |
| **Swift actors** | No borrow checker within actor, `Sendable` for cross-actor | Same rationale as Hew                          |
| **Hew**          | No borrow checker within actor, `Send` for cross-actor     | Single-threaded actors make it unnecessary     |

Rust's approach is overkill for single-threaded code. Pony's capability system is powerful but adds complexity that provides no benefit when each actor is single-threaded. Hew follows Swift's pragmatic approach: enforce safety only where it matters (actor boundaries).

##### Hew vs Rust Ownership

| Scenario                         | Rust                             | Hew                               |
| -------------------------------- | -------------------------------- | --------------------------------- |
| Aliasing mutable data            | `&mut` exclusivity is checked    | Value/COW semantics; no refs      |
| Send non-Send across threads     | ❌ Compile error                 | ❌ Compile error (actor boundary) |
| Borrow checker overhead          | Always on                        | No ordinary borrow checker        |
| Lifetime annotations             | Required                         | Never needed                      |
| Passing state to a helper fn     | Reference rules may be required  | Ordinary value parameter          |
| Keeping state-derived local data | Lifetime constraints may apply   | Owned/inferred value              |

#### 3.4.3 Binding vs. Ownership

`let` and `var` are **binding modes**, not ownership annotations:

```hew
let x = 5;       // immutable binding - cannot reassign x
var y = 5;       // mutable binding - can reassign y
y = 10;          // ok
// x = 10;       // compile error: cannot reassign immutable binding
```

The closest analogue is Swift's `let`/`var` paired with `mutating func`: the
binding mode controls both reassignment of the _binding_ and whether a mutating
method (`var self`, §3.6) may be called on it. It is not JavaScript's
`const`/`let`, which says nothing about the value's own methods.

For type fields:

```hew
type Point {
    x: i64,
    y: i64,
}

var p = Point { x: 0, y: 0 };
p.x = 10;        // OK — p is var-bound, so field mutation is allowed

let q = Point { x: 0, y: 0 };
// q.x = 10;     // compile error — q is let-bound
```

**Type field syntax:**

Type fields do NOT require a `let`/`var` prefix. Commas separate the fields:

```hew
type Point {
    x: f64,          // field declaration
    y: f64,          // field declaration
    label: string,   // field declaration
}
```

**Actor field syntax:**

Actor fields use `let` or `var` to distinguish immutable and mutable state.
Commas separate these structural members, just as in a type declaration:

```hew
actor Counter {
    var count: i64 = 0,     // mutable field with default
    let name: string,       // immutable field, set by init
}
```

A `let` (or bare) actor field is immutable after construction: it may be assigned only inside the `init { }` block, where its initial value is established. Any assignment to a `let` field from a `receive fn`, a plain actor method, or a lifecycle hook is rejected at check time with a diagnostic that names the field and suggests declaring it with `var`. A `var` field is mutable and may be assigned anywhere in the actor body.

**Who initializes a field (normative, D447).** Each state field has exactly one initializer, fixed at the declaration. A field with a default is initialized by that default, and `init` may replace it. A field without a default that `init` assigns is deferred to `init`: a `spawn` cannot name it, `init` must assign it on every path before it finishes (including every `return`), `init` may read it or call an actor method only after that assignment, and a branch that assigns it must do so in every arm (a loop body cannot be its first store). Every other field without a default is a required `spawn` argument. An `init` parameter with a field's name shadows the field inside `init`, so its assignments target the parameter. A fault inside `init` releases the deferred fields it has stored and the init arguments; the spawn releases the spawn-supplied fields and the unpublished state, and no handler or hook ever observes partial state.

This distinction exists because actor fields are stateful (they change over the actor's lifetime) and use initialization syntax similar to variable declarations, while type fields are data layout declarations.

**Value, handle, and callable categories (normative).**

Every type belongs to exactly one of the categories below. The category — not
the binding mode and not the spelling of the type — decides what a second
binding means, what a send does, whether `is` admits the type, and how the
value closes.

| Category | Examples | A second binding | Actor boundary | Cleanup |
| --- | --- | --- | --- | --- |
| ordinary data | scalars, strings, bytes, cloneable records/enums and collections | an independent logical value | snapshot when sendable | automatic recursive release |
| affine composite | an aggregate containing a non-copyable owner, or `dyn Trait` without a clone contract | transfers ownership | requires a valid transfer contract | automatic release of owned members |
| linear value | a type marked `#[linear]` | transfers ownership | transfers ownership when sendable | must be explicitly consumed |
| pid handle | `Pid`, `ChildRef` | names the same actor or role | copies the identity | use `close` to request actor termination |
| counted handle | `Rc`, `Weak`, `LambdaPid` | retains the same identity | subject to handle-specific sendability rules | releases a reference |
| opaque/resource handle | channel endpoints, sockets, user `#[resource]` types | transfers ownership | local transfer where admitted; no wire serialization | declared consuming close |
| callable | closure | copies independent state or transfers affine captures, according to its capabilities | subject to callable boundary restrictions | releases captures |
| task/generator | `Task<T>`, `Generator<Y, R>` | transfers ownership | not an actor message payload | structured completion or cooperative close |

A lambda handle retains the same synthesized actor; its capture storage follows
the ordinary actor lifetime. It is not a separate closure-runtime ownership
mechanism.

`#[resource]` and `#[linear]` are two disciplines, affine and linear, and both
stay (§3.7.8): a resource may be dropped and its drop glue closes it; a linear
value may not be dropped. `resource` is an ordinary identifier and never was a
token.

`is` admits handles only. It answers "are these two names the same actor,
count, or resource?" — see §12.2. There is no `expr is TypeName` form.

Ordinary data copies preserve both bindings. If a value contains an affine
owner without a copy contract, it must be transferred instead. Loans inferred
for collection reads prevent mutation or transfer of the borrowed owner while
the read remains live; no lifetime syntax is required.

#### 3.4.4 The Boundary Rule: Snapshot on Send

When ordinary data crosses an actor boundary through a completion call or
mailbox submission, the receiver observes a **logical snapshot** — an
independent value — and the sender's binding stays valid. An affine resource
instead transfers its sole ownership and cannot be reused by the sender:

<!-- doctest: skip -->

```hew
type Message { body: string }

actor Handler {
    receive fn process(message: Message) {
        println(message.body);
    }
}

actor Forwarder {
    receive fn forward(message: Message, target: Pid<Handler>) {
        let _ = target.process(message);  // target receives a snapshot of message
    }
}

fn main() {
    let handler = spawn Handler();
    let forwarder = spawn Forwarder();
    let _ = forwarder.forward(Message { body: "hello" }, handler);
}
```

Throughout this specification, sending describes transport across an actor
boundary. It is not a source keyword. Named actors use receive-method calls;
lambda actors use handle calls. A `mailbox(...)` view selects submission.

The runtime may copy, retain immutable storage, use copy-on-write, or transfer
unique storage where those choices preserve the language's value semantics.
Those optimizations do not change an ordinary argument into a consuming one.
Affine resources and explicitly consumed values follow their transfer contract.
Foreign views cannot escape in ordinary message payloads.

**Why snapshot semantics?**

- The receiving actor may be on a different thread (in the runtime)
- Two actors cannot share mutable state (this is the core safety guarantee)
- The snapshot gives the receiver an independent value while the sender keeps its own — actor isolation without a use-after-send rule to reason about

**Duplication syntax:**

Hew provides two syntactic forms for duplication:

- `val.clone()` — method call form; equivalent to invoking the `Clone` trait.
- `clone val` — prefix expression form; contextual keyword at unary precedence.
  Binds tighter than binary operators: `clone a + b` is `(clone a) + b`.
  Only acts as a prefix when followed by an operand token (identifier or
  literal); otherwise `clone` is a plain identifier, so `clone(args)` and
  `x.clone()` are unaffected.

Cloning is not required to keep using ordinary sendable data after a call or
submission. Fan-out to multiple receivers is ordinary code:

<!-- doctest: skip -->

```hew
type Message { body: string }

actor Handler {
    receive fn process(message: Message) {
        println(message.body);
    }
}

actor Broadcaster {
    receive fn broadcast(message: Message, first: Pid<Handler>, second: Pid<Handler>) {
        let _ = first.process(message);
        let _ = second.process(message);   // message still valid — each send snapshots
    }
}

fn main() {
    let first = spawn Handler();
    let second = spawn Handler();
    let broadcaster = spawn Broadcaster();
    let _ = broadcaster.broadcast(Message { body: "hello" }, first, second);
}

// Lambda handle calls use the same message value rules.
```

#### 3.4.5 Capturing Values in Lambda Actors

A lambda actor acquires its captures before it starts. Ordinary data is an
independent snapshot; capturing an affine resource transfers its owner. An
explicit `move` requests transfer. The parent cannot reuse a transferred
resource, and no capture creates shared mutable actor state.

```hew
fn main() {
    let prefix = "received: ";
    let worker = actor |message: string| {
        println(prefix + message);
    };
    println(prefix);                 // the ordinary value remains usable
    let _ = worker("hello");
    close(worker);
}
```

Captured types must satisfy the actor boundary's sendability rules. A borrowed
view cannot outlive its owner by being captured. See §3.8.6 for ordinary
closure capabilities and private mutable captures.

#### 3.4.6 What IS Allowed (Within an Actor)

```hew
actor Example {
    var data: Vec<i64> = Vec.new(),

    receive fn demo(incoming: Vec<i64>) {
        // Mutating the actor's own state - ALLOWED, no locks, no ceremony
        data.push(1);
        data.push(2);

        // A `var` local mutates in place; a `let` local does not (§3.2)
        var scratch: Vec<i64> = Vec.new();
        scratch.push(3);

        // Passing a value to a function borrows it - the binding stays valid
        process(incoming);
        process(incoming);   // ok - calls borrow, they do not consume
    }
}
```

A second binding of a value-category type is a **copy**, not an alias:
`var copy = incoming;` gives two independent values, and pushing to one does not
change the other (§3.4.3). There is no way to obtain two mutable names for one
value, so the question of aliased mutation does not arise; ordinary borrowing calls preserve their arguments. Consuming calls and live
collection loans still enforce ownership boundaries. Mutation uses `var`.

#### 3.4.7 What is NOT Allowed

<!-- doctest: skip -->

```hew
actor Example {
    receive fn bad_examples(other: Pid<Other>) {
        // Sending a non-Send value - ERROR
        let local_handle: RawPointer = get_handle();
        let _ = other.process(local_handle);  // compile error: RawPointer is not Send

        // Capturing non-Send value - ERROR
        let worker = actor |x: i64| {
            use(local_handle);  // compile error: RawPointer is not Send
        };
    }
}
```

Sending a non-`Send` value — `Rc<T>`, `Weak<T>`, raw handles, or any type
containing one — is a fail-closed compile error, never a silent copy.

#### 3.4.8 Summary

| Context       | Aliasing                        | Mutation                          | Boundary rule    |
| ------------- | ------------------------------- | --------------------------------- | ---------------- |
| Within actor  | Values copy; handles alias      | Through a `var` binding or `var self` | None         |
| Across actors | Not allowed                     | N/A                               | Snapshot on send |

**Hew's guarantee:** No data races between actors, enforced at compile time through `Send` and snapshot-on-send semantics. The compiler infers local loans and enforces their lifetime without source lifetime annotations.

---

### 3.5 Module System

Hew uses a file-based module system inspired by Rust:

- **File = module**: Each `.hew` file is a module. The file name is the module name.
- **Directory = namespace**: Directories create nested namespaces.
- **Visibility**: All declarations are private by default. Use `pub` to export.
- **Imports form a DAG (normative)**: a module may not import itself, directly or through other modules, whatever the imported declarations are. The compiler reports the cycle with every import on its path; break it by moving the shared declarations into a module both sides import.

```hew
// src/network/tcp.hew
// This is module network.tcp

pub type Connection {
    address: string,       // public fields via pub keyword on type
    internal_state: i64,   // named fields are separated by commas
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
import network.tcp;                    // Import module
import network.tcp.Connection;        // Import specific symbol
import network.tcp.{Connection, connect};  // Import multiple
```

Glob imports are rejected. Import the specific symbols required by the module
instead.

**Module dot-syntax for standard library:**

When importing a standard library module, the **last segment** of the module path becomes the local alias for the module. All access uses this short name, not the full path:

```hew
import std.net.http;     // Available as "http", not "std.net.http"
import std.fs;            // Available as "fs"
import std.io;            // Available as "io"
import std.text.regex;   // Available as "regex"

fn main() {
    // Call module functions with dot-syntax: module.function(args)
    match http.listen("127.0.0.1:0") { // Returns Result<Server, NetError>
        .Ok(server) => {
            println(f"HTTP server listening on port {http.server_port(server)}");
            server.close(); // Explicitly release the listener on every success path.
        },
        .Err(error) => println(error),
    }
    let content = fs.read("config.toml").expect("config.toml must be readable");
    let exists = fs.exists("output.txt");       // Returns bool
    let line = io.read_line();                  // Preferred stdin surface
    let re = regex.new("[a-z]+");
    let matched = re.is_match("example");      // Returns bool
    re.close();
}
```

This provides clean, namespaced access to stdlib functionality. The module name acts as a qualifier, avoiding verbose function names like `hew_http_server_new()`.

| Module             | Example functions                                                                                                                                            |
| ------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `std.net.http.server` | `http.listen`, `http.accept`, `http.path`, `http.method`, `http.body`, `http.header`, `http.respond`, `http.respond_text`, `http.respond_json`, `http.close` |
| `std.fs`           | `fs.read`, `fs.write`, `fs.append`, `fs.exists`, `fs.delete`, `fs.size`                                                                                     |
| `std.io`           | `io.read_line`, `io.write`, `io.write_err`, `io.read_all`                                                                                                   |
| `std.os`           | `os.args_count`, `os.args`, `os.env`, `os.set_env`, `os.has_env`, `os.cwd`, `os.home_dir`, `os.hostname`, `os.pid`                                          |
| `std.net`          | `net.listen`, `net.accept`, `net.connect`, `net.connect_timeout`, `net.parse_endpoint`, `net.read`, `net.write`, `net.close`                                 |
| `std.text.regex`   | `regex.new`, `regex.is_match`, `regex.find`, `regex.replace`                                                                                                |
| `std.net.mime`     | `mime.from_path`, `mime.from_ext`, `mime.is_text`                                                                                                           |
| `std.process`      | `process.run`, `process.run_argv`, `process.start`, `process.start_argv`                                                                                     |

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
> file-import paths use `HEW_STD` and `.hew/packages` instead; they are not
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
trait Renderable {
    fn fmt(self) -> string;
}

trait Sequence {
    type Item;
    fn next(var self) -> Option<Self.Item>;
}

trait Duplicable {
    fn clone(self) -> Self;
}
```

**Trait implementation:**

```hew
trait PointRenderer {
    fn fmt(self) -> string;
}

type Point { x: f64, y: f64 }

impl PointRenderer for Point {
    fn fmt(self) -> string {
        f"({self.x}, {self.y})"
    }
}
```

**Built-in marker traits:**

- `Send` - Type can safely cross actor boundaries. Satisfied by:
  - Value types (integers, floats, bool, char)
  - Owned types transferred by move
  - `Frozen` types (deeply immutable)
  - `Pid<A>`

- `Sync` - Type is safe to share across concurrent actors without synchronisation. Derived structurally from field types; the compiler determines this automatically. A type is `Sync` if all its fields are `Sync`. Value types are always `Sync`; mutable containers (`Vec<T>`, `HashMap<K,V>`) are not.

- `Frozen` - Type is deeply immutable and thus safely shareable. Implies `Send`. `Frozen ⊇ Send` holds by structural coincidence — every type whose fields are all deeply immutable is also send-admissible — not via an explicit impl chain.
  - Runtime-internal shared immutable handles (the planned `Arc<T>` surface is not yet exposed in Hew source)
  - Types where all fields are `Frozen`

- `Copy` - Type is copied on assignment rather than moved.
  - Only value types (integers, floats, bool, char)
  - Small fixed-size aggregates

**Comparison and hashing traits (normative):**

`Eq`, `Ord`, `PartialOrd`, and `Hash` are **derived by default**, and a user
`impl` is allowed and is what `==`, the ordering operators, and hashing dispatch
to for that type.

- Records and enums are `Eq` and `Hash` structurally: field by field, variant by
  variant, with no declaration.
- `Ord` and `PartialOrd` are derived lexicographically by field order when every
  field is itself ordered. Primitive numeric types (`i8`–`i64`, `u8`–`u64`,
  `f32`, `f64`, `isize`, `usize`, `char`) are ordered.
- Writing `impl Eq for T { … }` (likewise `Ord`, `PartialOrd`, `Hash`) is legal
  and **overrides** the derived implementation for `T`. A type with a real
  invariant — a case-insensitive key, a tolerance-based float comparison — states
  its own rule rather than being walled off from the operators. There is no
  `E_DERIVED_TRAIT_IMPL`: an `impl` body for these traits is never refused for
  being an "implementation of a derived trait".
- Derived comparison is structural, so it never observes handle identity; `is`
  is the operator that does (§12.2).

> **Limitation at edition 2026 (`E_LIMIT_DERIVED_ORD`, Limitation channel).**
> A record, enum, or tuple whose members derive ordering is legal, but its
> lexicographic comparison lowering is not implemented yet. Direct ordering
> reports `E_LIMIT_DERIVED_ORD`; use an explicit comparator (`sort_by`) or a
> user `impl Ord`/`impl PartialOrd`. A shape with an unordered member does not
> derive ordering and receives an ordinary type error instead.

**The three receivers (normative):**

A method's receiver is written `self`, and the receiver token set is fixed at
one member each:

| Receiver | Meaning | Caller's binding |
| --- | --- | --- |
| `self` | borrows the receiver | stays valid and unchanged |
| `var self` | mutates the receiver in place | must be declared `var` |
| `consume self` | consumes the receiver | is dead after the call |

```hew
type Point { x: f64, y: f64 }

trait Formattable {
    fn fmt(self) -> string;
}

impl Formattable for Point {
    fn fmt(self) -> string {
        f"({self.x}, {self.y})"
    }
}
```

`self` is the receiver's name inside the body; its type is the implementing type
(or `Self` in a trait declaration). There is no `&self` or `&mut self` — Hew has
no references in its surface syntax (§3.4.1) — and there is no implicit receiver
anywhere else in the language.

The **named-first-parameter** receiver form is not part of the language. A
method whose first parameter is spelled with a name and the target type
(`fn fmt(p: Point)`, `fn push(v: Vec<T>, value: T)`, the trait declaration
`fn fmt(val: Self)`) is rejected, with a fix-it that rewrites the parameter to
`self` — or to `var self` when the body assigns through it. This applies in
`impl` blocks and in trait declarations alike, including for the builtin
collection methods.

**Calling methods:**

```hew
let p = Point { x: 1.0, y: 2.0 };
p.fmt();    // `self` borrows: p is still valid
p.fmt();    // and may be called again
```

A `var self` method needs a `var` binding. `let c = Counter { n: 0 }; c.bump()`
is refused with "requires a mutable binding receiver" and the `let`→`var`
fix-it (§3.2). A `consume self` method takes the value: any later use of the
binding is a use-after-consume diagnostic.

The consuming receiver is spelled `consume self`, matching a consuming
parameter such as `consume value: T`. Both inherent and trait methods use the
same borrowing, mutable and consuming receiver contracts.

**In actor bodies (bare fields, `self` is the handle):**

An actor's own state is reached by **bare field name** — no prefix, no receiver
parameter — and the actor persists across handler invocations:

```hew
actor Counter {
    var count: i64 = 0,
    receive fn increment() {
        count += 1;  // bare field access — actor persists after handler returns
    }
}
```

Inside an actor body, `self` is the **actor handle**: a read-only value of type
`Pid<Self>` naming the enclosing actor (`LocalPid<Self>` on the current
native surface). A projection such as `self.count` still names the actor's
state field; bare `count` is the same state access.

- Available in `receive fn` and `fn` methods and in lifecycle hooks
- Sendable: it may be passed as a message argument or stored in a field
- Read-only: assignment to `self` is rejected at check time
- Actor termination uses the close/closed contract (§4.10); a completion
  call that waits on its own handler is a wait cycle, not a stop primitive.

`this` is not a keyword and carries no actor meaning; the handle is `self`
everywhere.

A plain `fn` in an actor body is an **actor method**: a helper over the actor's
own state with no receiver. Its body is checked against the actor's fields by
bare name like a handler's, and it is callable by bare name from that actor's
handlers, lifecycle hooks, `init`, and sibling methods. It takes no mailbox
slot: from outside the actor it is unreachable, and naming it there is
`E_ACTOR_METHOD_OUTSIDE` — send a message instead.

```hew
actor Counter {
    var count: i64 = 0,
    fn next() -> i64 { count + 1 }
    receive fn increment() { count = next(); }
}
```

**Variable shadowing:**

Hew distinguishes three cases of variable shadowing:

- **Same-scope rebinding** — a **hard error**. Declaring a name that is already bound in the same scope is rejected outright:

  ```hew
  fn main() {
      let x = 1;
      let x = 2;  // compile error: variable `x` is already defined in this scope
  }
  ```

- **Outer-scope shadowing of an actor field** — a **hard error**. Actor fields must have unambiguous bare names; a parameter, local variable, or loop variable that shadows a field is rejected:

  ```hew
  actor Example {
      var count: i64 = 0,

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

**Shadowing exemptions:** Bindings whose name starts with `_` are silently exempt from all shadowing diagnostics, and so are for-loop induction variables — except over an actor field. A field is in scope under its bare name for the whole actor body, and `self.count` is that same binding spelled through the receiver, so a loop variable that takes the field's name is rejected exactly as a parameter or a local is. Every other name a loop variable may shadow stays exempt.

**Trait bounds on generics:**

<!-- doctest: skip -->

```hew
type Message { body: string }

actor Receiver {
    receive fn accept(message: Message) {
        println(message.body);
    }
}

actor Broadcaster {
    receive fn broadcast(message: Message, first: Pid<Receiver>, second: Pid<Receiver>) {
        let _ = first.accept(message.clone());
        let _ = second.accept(message.clone());
    }
}

fn main() {
    let first = spawn Receiver();
    let second = spawn Receiver();
    let broadcaster = spawn Broadcaster();
    let _ = broadcaster.broadcast(Message { body: "hello" }, first, second);
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
Each actor owns its mutable state. Immutable backing storage may be shared
by reference count without sharing mutation. Terminal cleanup releases owned
resources and state before reclaiming their storage.

**Principle 2: Ownership within actors.**
Within an actor, values follow Hew ownership semantics:

- Each value has exactly one owner
- When the owner goes out of scope, heap memory is freed automatically
- User-defined `impl Drop` is **not supported** — external-resource cleanup
  uses `#[resource]` + `close()` (see §3.7.3 and §3.7.8)

```hew
#[resource]
type Connection { fd: i64, }

impl Connection {
    fn open(host: string) -> Connection { Connection { fd: 0 } }
    fn close(consume self) {}
}

fn main() {
    let conn = Connection { fd: 0 };
    // ... use conn ...
}  // conn.close() runs here automatically (implicit #[resource] drop)
```

**Principle 3: No garbage collection.**
Hew guarantees no GC pauses. Memory reclamation is entirely deterministic:

- Scope exit triggers drops
- Reference count reaching zero triggers drops
- Actor termination frees the actor's heap

#### 3.7.2 Message Passing Semantics

Message calls and mailbox submissions obey §3.4.4. Ordinary data arrives as
an independent logical value; affine resource payloads transfer ownership.
Actor identities still name the same destination. Physical copying, retaining
or moving is the runtime's concern and must preserve these contracts.

**The `Send` trait:**

`Send` is a **marker trait** — it has no methods. A type is `Send` if it is **send-admissible** (can safely cross actor boundaries). The compiler verifies `Send` bounds at compile time.

```hew
trait Send {}  // Marker trait — no methods
```

`Send` is satisfied if one of the following holds (see §3.3):

- The value is a value type (integers, floats, bool, char)
- The value is owned and transferred by move with no remaining aliases
- The value is `Frozen` (deeply immutable)
- The value is a `Pid<A>`
- The value is a type/enum where all fields/variants satisfy `Send`

`Send` describes whether a boundary is safe; it does not prescribe a copy
strategy. The compiler derives it from the type's fields and capabilities.

**Local value transfer:**

```hew
fn process(data: Vec<u8>) {
    let independent = data;     // ordinary value copy
    println(data.len());       // the borrowed argument is still usable
    println(independent.len());
}
```

A parameter declared `consume` instead accepts ownership. Non-copyable values
can only be transferred, never implicitly duplicated.

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
    fd: i32,
}

impl FileHandle {
    fn close(consume self) {
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
    Lit(i64),
    Add(Expr, Expr),
    Neg(Expr),
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
let e = Expr.Add(Expr.Lit(1), Expr.Neg(Expr.Lit(2)));
```

**Pattern matching** works identically to regular enums:

```hew
fn eval(e: Expr) -> i64 {
    match e {
        .Lit(n) => n,
        .Add(l, r) => eval(l) + eval(r),
        .Neg(inner) => 0 - eval(inner),
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
- `Rc.new(value)` consumes `value`; `.clone()` creates another strong owner
- `.get()` copies the payload and therefore requires `T: Copy`
- `.set(value)` consumes and replaces the entire shared payload; every strong alias observes the replacement
- `.downgrade()` creates a `Weak<T>`; `.strong_count()`, `.weak_count()`, and `.is_unique()` inspect the allocation
- Supported payloads include scalars, `string`, `bytes`, `Rc`, `Weak`, tuples,
  arrays, `Option`, `Result`, records, enums, and supported owned collections;
  clone/drop synthesis recursively follows aggregate fields
- `Vec<Rc<T>>`, `Vec<Weak<T>>`, and records containing these handles use
  semantic field clone/drop operations; map and set shapes retain their own
  independent key, value, `Eq`, `Hash`, and ABI restrictions

```hew
let data: Rc<string> = Rc.new(expensive_computation());
let alias = data.clone();  // refcount++, no data copy
// data and alias share the same string
```

**`Weak<T>` — non-owning cycle-breaking handle:**

- `rc.downgrade()` is the only constructor; there is no empty `Weak.new()`
- `weak.clone()` creates another weak owner
- `weak.upgrade()` returns exactly `Some(Rc<T>)` while a strong owner exists,
  and exactly `None` after the last strong owner is released
- `Weak<T>` is affine and is neither `Send` nor `Sync`
- A weak handle keeps the allocation header alive but does not keep its payload alive

```hew
type Node {
    label: string,
    parent: Option<Weak<Node>>,
}

fn main() {
    let root = Rc.new(Node { label: "root", parent: None });
    let weak = root.downgrade();
    root.set(Node { label: "child", parent: Some(weak.clone()) });
}
```

**Field reads through an `Rc`.** Reading one field of an `Rc<T>` payload —
`rc.field` where `T` is a record — is a `copy_value` through a borrow: the
payload is not moved and the refcount does not change. Until that lowering
lands, the checker refuses the projection with `E_LIMIT_RC_FIELD`
(Limitation channel); `.get()` on a `T: Copy` payload is the accepted form in
the meantime.

Strong `Rc` cycles leak because Hew does not run a tracing collector. Use weak
back-edges to break cycles. `Rc.new_cyclic`, dereference/borrow access to an Rc
payload, and cross-actor transfer are not supported in edition 2026.

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
| admitted affine/linear `T` | by its transfer contract | ownership move | the sender cannot reuse the consumed owner |
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

\*Strong reference cycles in `Rc<T>` can leak. Use `Weak<T>` back-edges; there
is deliberately no empty `Weak.new()` constructor.

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
    fn close(consume self) {}   // consuming receiver, unit return, sibling impl
}
```

Semantics:

1. **One consuming close.** A resource declares `fn close(consume self)` in
   an inherent `impl`. The return type is unit; a borrowing receiver or
   fallible return is rejected (§3.7.8.5).
2. **Automatic cleanup.** Scope exit calls close if the owner is still live.
   Normal return, structured cancellation and recoverable fault cleanup use
   the same obligation. Explicit process exit is different (§5.8).
3. **Early close.** `resource.close();` consumes the owner. There is no `?`
   on this unit result, and a later use of the resource is a compile error.
4. **Fallible completion is separate.** A resource may expose a `finish`,
   `flush` or `commit` operation returning `Result`. Call it explicitly when
   its outcome matters. Cleanup does not manufacture or silently discard a
   fallible completion result.

```hew
#[resource]
type Session { label: string, }

impl Session {
    fn close(consume self) {
        println("closing " + self.label);
    }
}

fn main() {
    let session = Session { label: "example" };
    session.close();            // early release; no second close at scope exit
}
```

For example, a file-like resource may have `finish() -> Result<(), E>` to
report a failed flush and a unit-returning close to release its descriptor.
That is a resource API contract, not a second language cleanup mechanism.

##### 3.7.8.2 `#[linear]` — single-owner with no implicit drop

`#[linear]` marks a type that **must be consumed** by one of its declared
consuming methods. There is no implicit drop. Letting a `#[linear]` value
go out of scope without consuming it is a compile error.

```text
#[linear]
type Transaction { ... }

impl Transaction {
    fn commit(consume self) -> Result<(), DbError> { ... }
    fn rollback(consume self) -> Result<(), DbError> { ... }
}
```

This declaration sketch separates the product's fields from its consuming
methods; bodies depend on the transaction API.

Semantics:

1. **No implicit drop.** The compiler does not synthesise a drop call.
   Scope exit with an unconsumed `#[linear]` value is a
   `MustConsumeAtScopeExit` diagnostic.
2. **Consumption discharges the obligation.** Calling any declared
   consuming method (one whose receiver is `consume self`) is enough to
   satisfy the must-consume check.
3. **No canonical method name.** The type declares which methods are
   valid consumers. `Transaction` requires `commit` or `rollback`; a
   capability token might require `revoke`; a GPU command buffer might
   require `submit` or `discard`.
4. **Affine in the move-checker.** Same single-owner tracking as `#[resource]`: the move-checker
   tracks the single live binding and rejects any use after the consuming
   method call.

A correct use (illustrative — `Database` and `Transaction` are hypothetical
types showing the `#[linear]` pattern):

<!-- doctest: skip -->
```hew
fn transfer(db: Database, from: AccountId, to: AccountId, amount: Money)
    -> Result<(), DbError>
{
    let tx = db.begin_transaction()?;
    // This example assumes debit/credit are infallible staging operations.
    tx.debit(from, amount);
    tx.credit(to, amount);
    tx.commit()                 // tx is consumed here.
}
```

The compile error for forgetting to consume (illustrative):

<!-- doctest: skip -->
```hew
fn forgot_to_commit(db: Database) -> Result<(), DbError> {
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
| Can a unit-returning close release the resource at scope exit? | `#[resource]` |
| Must the caller surface the cleanup result, every time?            | `#[linear]`   |
| Are there multiple distinct ways to consume (commit / rollback / ...)? | `#[linear]`   |
| Is there one automatic release obligation?                         | `#[resource]` |

File descriptors, sockets, allocator handles, regex compiled patterns,
HTTP server/request handles — `#[resource]`. Database transactions,
capability tokens, response-body finish protocols where the success path
must be acknowledged, GPU command buffers — `#[linear]`.

##### 3.7.8.4 Interaction with cancellation and supervision

Structured cleanup ends users of a resource before releasing it. Normal scope
exit waits for children; fault and cancellation paths cancel and drain them
before releasing parent-owned state. A resource transferred to a child is
released by that child, including when cancellation prevents an explicit close.

Graceful actor termination runs its stop hook before releasing live state
fields. A fault may bypass the stop hook, but does not turn resource ownership
into a second close obligation. Field and local cleanup follow the same
exactly-once rules as ordinary execution.

A linear value requires an explicit consuming operation on the relevant exit
paths. A transfer into cancellable work does not waive that obligation; code
whose consumption cannot be established is refused. `#[resource]` supplies
automatic release when that is the intended lifetime contract.

Deferred cleanup is block-scoped and LIFO, including each loop iteration. A
deferred action cannot escape with `return` or `?` or suspend. It may inspect a
fallible operation locally, but a result that must reach the caller belongs in
an explicit completion operation before cleanup. A secondary cleanup fault
must not replace the primary fault.

Explicit `exit(code)` and process abort are not graceful cleanup paths. They
do not promise deferred actions, consuming methods or actor stop hooks (§5.8).

##### 3.7.8.5 `#[resource]` close discipline

The resource close contract has three requirements:

1. **Inherent-impl-only `close` body.** The body of `close` on a
   `#[resource]` type must be declared in a sibling inherent `impl`
   block:

   ```hew
   #[resource]
   type Conn { fd: i64 }

   impl Conn {
       fn close(consume self) { /* release fd */ }
   }
   ```

   Declaring `close` as an inline method inside the type body is rejected
   with `ResourceCloseSourceUnsupported`.

2. **Unit return required.** The inherent-impl `close` body must return
   unit. A `close` declared to return `Result<(), E>` (or any non-unit
   type) is rejected at HIR with `ResourceCloseMustReturnUnit`. The
   implicit drop contract dispatches `close` on every scope-exit path
   including `Trap` and `Cancel`; propagating a value off those edges has
   no defined semantics. Report fallible completion separately before cleanup;
   a deferred body cannot propagate with `?`.

3. **Consuming receiver.** `close` on a `#[resource]` or `#[opaque]` type
   declares `consume self`. This ends its ownership exactly once; a borrowing
   `close(self)` is not the cleanup contract. See §2.1.1 for the current
   trait-method enforcement limitation.

A `#[resource]` declaration with no inherent-impl `close` is rejected at
HIR with `ResourceMissingClose` — the implicit drop contract has no method
to dispatch.

Ownership SIR records the release obligation and cleanup edges. Physical MIR
and codegen realize that checked contract. A consuming close body releases any
members it has not transferred; its caller must not release those members again.

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

**Generic collection elements.** In an unbounded generic `Vec<T>` body,
`for element in values` and `values[index]` borrow each element at every
instantiation. The body cannot move such a loan out, mutate its owner while
the loan is live, or assume an implicit clone merely because one particular
instantiation is copyable.

Ownership is obtained through `into_iter()` or an explicit `clone` under
`T: Clone`. `into_iter()` consumes the vector and owns its remaining elements,
including cleanup on early termination. The current clone-proven cursor path
can still clone elements; consuming the container does not yet promise
clone-free element extraction. A `Clone` bound also permits the current
compiler to select copying element reads (§2.1.1).

Borrowed `get` returns an optional loan. Let-bound reads keep the source
borrowed until their last permitted use. Loan ending is conservative across
branches and loops; mutation or draining is refused when the compiler cannot
prove the loan has ended. HashMap `get` similarly borrows clone-free values;
`remove` transfers a value out.

#### 3.8.2 Type-Erased Dispatch with `dyn Trait`

Single-trait `dyn Trait` dispatch is implemented. A concrete value whose type
implements the trait can be passed to a `dyn Trait` parameter, where calls to
the trait's instance methods dispatch through the runtime vtable. See
[`examples/types_and_traits.hew`](../../examples/types_and_traits.hew) for a
runnable example.

The current subset does not yet fully enforce object-safety rules or support
associated-type bounds and higher-ranked trait bounds in `dyn` position. See
[HEW-FUTURE.md §2.2](HEW-FUTURE.md) for those remaining object-type details.

`dyn Trait` works as a function parameter and as a `Vec` element. A
`Vec<dyn Trait>` type-checks, accepts `push` of any concrete implementor
(including a heterogeneous mix), and dispatches through the runtime vtable
when the elements are drained with `into_iter()`. Because a trait object has
no clone path, it cannot use a copying `iter()` snapshot. Borrowed reads and
plain iteration follow the clone-free element contract (§3.8.1); `into_iter()`
provides owning extraction. An enum of concrete variants, such as
`enum Shape { Circle(Circle), Square(Square), }`, is an alternative when the
program needs an exhaustive set of concrete cases.

#### 3.8.3 Trait Bounds

**Inline bounds:**

<!-- doctest: skip -->

```hew
type Message { body: string }

actor Receiver {
    receive fn accept(message: Message) {
        println(message.body);
    }
}

actor Broadcaster {
    receive fn broadcast(message: Message, first: Pid<Receiver>, second: Pid<Receiver>) {
        let _ = first.accept(message.clone());
        let _ = second.accept(message.clone());
    }
}

fn main() {
    let first = spawn Receiver();
    let second = spawn Receiver();
    let broadcaster = spawn Broadcaster();
    let _ = broadcaster.broadcast(Message { body: "hello" }, first, second);
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

An associated-type bound in a `where` clause — `where T.Item: Bound` — is
**refused**, with `E_ASSOC_BOUND_UNSUPPORTED` (Limitation channel, target
v0.7.0). The bound parses and type-checks today but never participates in
method resolution, so a program that writes one is admitted and then behaves
as though the bound were absent. A surface that does not act is not a surface:
until the bound reaches method resolution, writing it is an error rather than
a promise kept by nothing. See HEW-FUTURE.md §2.2.

#### 3.8.4 Associated Types in Traits

Traits can declare associated types that implementors must specify:

```hew
trait Iterator {
    type Item;
    fn next(var self) -> Option<Self.Item>;
}

impl Iterator for RangeIter {
    type Item = i32;

    fn next(var self) -> Option<i32> {
        if self.current < self.end {
            let value = self.current;
            self.current = self.current + 1;
            .Some(value)
        } else {
            .None
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
type Point { x: f64, y: f64 }

// Compiler derives: Container<T> is Send if T is Send
type Container<T> { value: T, }

// MutableContainer has a mutable binding semantics determined by usage
type MutableContainer<T> {
    value: T,
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

<!-- doctest: skip -->

```hew
// Error: T might not be Send
receive fn forward_unsafe<T>(message: T, target: Pid<Handler<T>>) {
    let _ = target.process(message);    // Compile error: T not bounded by Send
}

// Correct: T is bounded by Send
receive fn forward<T: Send>(message: T, target: Pid<Handler<T>>) {
    let _ = target.process(message);    // OK: T: Send verified at instantiation
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

`Vec.reduce` takes the combining closure first and the seed second
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

Captured values are independent immutable snapshots. To mutate private capture
state, name existing bindings in a `capture(var name, ...)` prefix:

```hew
let count: i64 = 0;
var next = capture(var count) || { count = count + 1; count };
var independent = next;
```

The original binding may be immutable. The prefix grants mutation only to the
closure's private field; invoking a mutable closure requires a mutable callable
place. Duplicate capture names, capture initializers, aliases and names that
conflict with lambda parameters are errors. `capture` remains an ordinary
identifier outside this prefix.

`move` precedes the capture prefix (`move capture(var count) || ...`) and
transfers captured ownership. It does not itself grant mutation or require
call-once invocation. A capture without an independent snapshot operation
requires `move`. Consuming a captured owner during invocation, including
returning it or transferring it into another value, requires call-once.

Written function types specify invocation and duplication guarantees:

| Type | Invocation | Independent copies |
|---|---|---|
| `fn(...) -> T` | Repeated, read-only | Not guaranteed |
| `fn[clone](...) -> T` | Repeated, read-only | Guaranteed |
| `fn[var](...) -> T` | Repeated, mutable place | Not guaranteed |
| `fn[var, clone](...) -> T` | Repeated, mutable place | Guaranteed |
| `fn[once](...) -> T` | Consumes the callable | Not guaranteed |
| `fn[once, clone](...) -> T` | Each copy is consumed by its call | Guaranteed |

`suspends` is the fourth qualifier and occupies the same bracket. A written
callable type is non-suspending unless it carries it: `fn[suspends](...) -> T`
accepts a callable whose body may suspend, and `fn[once, suspends](...) -> T`
combines the two. Suspension is the one qualifier that is inferred rather than
declared on a named function or a closure literal; the bracket exists so a data
boundary — a parameter, a return type, a field, an element type, an annotated
binding — can state the fact. The rules are §4.0.

Qualifiers are lowercase. `clone` refers to independent logical duplication,
not bitwise copying or shared mutable state. Resource-bearing captures cannot
claim it. Value coercion may weaken read-only invocation to mutable invocation
to consuming invocation, and may forget `clone`; it cannot invent guarantees.
A non-suspending callable coerces into a `fn[suspends]` slot; a callable whose
body suspends does not coerce into a slot written without the qualifier, and
the error is reported at the site that supplies the value, naming the call in
the body that suspends.
Parameter and result signatures remain invariant, and coercion cannot erase
linear ownership duties. Unannotated expressions retain proved guarantees.
Explicit annotations, assignments, arguments, returns and conditional joins
use the same directional rules, including nested `Option` and `Result` types.

Calling a once callable stored in an owned plain-record or tuple field consumes
that field and preserves its siblings. The same partial transfer applies to
other non-copyable fields. Remaining fields retain their usual cleanup rules:
automatic cleanup releases initialized contents, and linear fields retain their
explicit-consumption duties. A consumed field cannot be read again until assigned
a replacement through a mutable binding. Whole-value uses require every field to be initialized. Nested partial
moves may cross plain record and tuple fields, but not resource, linear or opaque
declaration boundaries. Borrowed aggregate parameters cannot supply a consuming
field; use an explicit `consume` parameter to acquire the owner.

Generic function declarations can be used as values with explicit type
arguments (`identity<i64>`) or arguments inferred from an expected function
type or a later call through the binding. Each reference is instantiated
independently and must satisfy the declaration's generic bounds. Function
values retain the read and clone guarantees of the selected declaration.

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
    var result: i64 = 0,

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

#### 3.8.7 Generic Actors and Supervisors

Actors and supervisors take type parameters like records. An instantiation is
keyed by its concrete type arguments: state layout, `init`, handlers, hooks,
methods, mailbox message types and reply types are monomorphized per
instantiation, and `LocalPid<Latest<i64>>` and `LocalPid<Latest<string>>` are
distinct handle types. Two instantiations never share a dispatch table, a
mailbox protocol or a restart budget.

```hew
actor Latest<T> {
    var value: Option<T> = .None,
    receive fn put(next: T) { value = .Some(next); }
    receive fn get() -> Option<T> { value }
}

fn main() {
    let numbers = spawn Latest<i64>();
    let names = spawn Latest<string>();
    numbers.put(41)?;
    names.put("hew")?;
    println(f"{(numbers.get())?.expect("set")} {(names.get())?.expect("set")}");
    close(numbers);
    close(names);
}
```

`spawn Latest<i64>()` instantiates explicitly. When the init arguments fix the
parameters, they are inferred the way record type arguments are:

```hew
actor Cache<K: Hash + Eq, V: Clone> {
    var entries: HashMap<K, V>,
    var hits: i64 = 0,
    receive fn insert(key: K, value: V) { entries.insert(key, value); }
    receive fn lookup(key: K) -> Option<V> {
        let found = entries.get(key);
        if found.is_some() { hits = hits + 1; }
        found
    }
}

fn main() {
    var seed: HashMap<string, i64> = HashMap.new();
    seed.insert("answer", 42);
    let cache = spawn Cache(entries: seed);   // K = string, V = i64
    let found = cache.lookup("answer") handle error { None };
    match found {
        .Some(v) => println(v),
        .None => println("miss"),
    }
    close(cache);
}
```

Bounds on actor parameters are ordinary bounds. A type argument that cannot
cross an actor boundary (an `Rc<T>`, a borrowed view) is refused at the
instantiation site, naming the argument and the bound it fails, never at a
later send. A message to the wrong instantiation is a type error at the send
or ask site.

A generic supervisor's child specs name the instantiated actor, so a restarted
child has the same instantiation and `workers.worker` has type
`ChildRef<Worker<Job>>`. Construction arguments can infer the supervisor's type
arguments and the type arguments of its children. Each instance retains its own
typed configuration and restart budget:

```hew
actor Worker<Job: Send> {
    var done: i64 = 0,
    receive fn run(job: Job) -> i64 { done = done + 1; done }
}

supervisor Pool<Job: Send> {
    strategy: one_for_one,
    intensity: 3 within 10s,
    child worker: Worker<Job>(done: 0),
}

fn main() {
    let workers = spawn Pool<string>();
    let completed = workers.worker.run("parse").expect("the example worker completes");
    println(completed);
    close(workers);
}
```

### 3.9 Foreign Function Interface (FFI)

> **Partially implemented.** `extern "C"` blocks, unsafe foreign calls,
> and native static-library linking with `hew build --link-lib` are shipped.
> Layout/export attributes and the higher-level C-string wrapper surface remain
> planned; their subsections are marked accordingly.

Hew provides FFI capabilities for interoperating with C libraries and system calls.

#### 3.9.1 Extern Function Declaration

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

**Symbol uniqueness:** an `extern "C"` symbol name is one C symbol program-wide,
across every module and file that declares it. The linker binds every call
site to the one implementation behind that symbol, so a second declaration of
the same symbol must repeat its established parameter types, return type,
variadic shape, and ownership (`consume`) modes exactly — one C symbol means
one ABI contract. This holds even if the conflicting declaration is never
called: the compiler rejects the drift at declaration time, not at the call
site. Identical re-declarations (for example, the same C header imported by
two modules) are accepted and resolve to the one established contract.

#### 3.9.2 C-Compatible Struct Layout

> **Not yet implemented.** `#[repr(C)]` is not recognised. Annotating a type
> with `#[repr(C)]` is rejected at parse time with
> `unrecognised type attribute '#[repr]' [E_UNKNOWN_TYPE_MARKER]`; layout is
> controlled by the compiler for all types today.

Use `#[repr(C)]` to ensure C-compatible memory layout:

<!-- doctest: skip -->
```hew
#[repr(C)]
type Point {
    x: f64,
    y: f64,
}

#[repr(C)]
type FileInfo {
    size: u64,
    mode: u32,
    flags: u16,
    padding: u16,  // Explicit padding for alignment
}
```

**Additional layout attributes:**

- `#[repr(C)]` - C-compatible layout with C alignment rules
- `#[repr(C, packed)]` - C layout with no padding
- `#[repr(C, align(N))]` - C layout with minimum alignment N

#### 3.9.3 Type Mapping: Hew ↔ C

> Primitive, raw-pointer, function-pointer, and immutable-view mappings apply
> to shipped `extern "C"` declarations. Aggregate layout remains contingent on
> the planned `#[repr(C)]` surface.

| Hew Type                  | C Type                      | Notes                      |
| ------------------------- | --------------------------- | -------------------------- |
| `i8`, `i16`, `i32`, `i64` | `int8_t`, `int16_t`, etc.   | Exact size match           |
| `u8`, `u16`, `u32`, `u64` | `uint8_t`, `uint16_t`, etc. | Exact size match           |
| `isize`                   | `intptr_t` / `ssize_t`      | Platform-dependent         |
| `usize`                   | `uintptr_t` / `size_t`      | Platform-dependent         |
| `f32`, `f64`              | `float`, `double`           | IEEE 754                   |
| `bool`                    | `_Bool` / `bool`            | C99 bool                   |
| `&T`                      | `const T*`                  | Non-owning immutable view  |
| `*const T`                | `const T*`                  | Immutable raw pointer      |
| `*mut T`                  | `T*`                        | Mutable raw pointer        |
| `*const u8`               | `const char*`               | C string (null-terminated) |
| `fn(...) -> T`            | Function pointer            | C function pointer         |

`&T` is legal only within an `extern` function's parameter or return type
tree. It is immutable, non-owning, and represented by one pointer. Foreign code
owns the pointee and guarantees its lifetime; Hew never retains or drops it.
Ordinary Hew declarations use `T`, and mutable foreign access uses `*mut T`.

#### 3.9.4 Exporting Functions to C

> **Accepted, not yet wired to codegen.** `#[export]` parses on a Hew function
> without error, but has no effect on the emitted binary today: the function
> keeps its original name and internal (non-exported) linkage, so it is not
> yet callable from C. `extern "C"` prefixing a Hew function body (as opposed
> to a foreign declaration in an `extern "C" { ... }` block, §3.9.1) is not
> valid syntax — `#[export]` applies directly to an ordinary `fn`.

Use `#[export]` to make Hew functions callable from C once codegen support lands:

```hew
#[export("hew_process_data")]
fn process_data(data: *const u8, len: usize) -> i32 {
    // Intended to be accessible from C as hew_process_data() once wired.
    0
}

#[export]  // Uses the function name as-is
fn my_callback(value: i32) -> i32 {
    value * 2
}
```

#### 3.9.5 Safety Rules

> Unsafe foreign calls are shipped. The C-string helper (`to_c_string`) and
> the complete `#[resource]` safe-wrapper pattern shown below remain planned.

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
    fd: i32,
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

    fn close(consume self) {
        unsafe { close(self.fd); }
    }
}
```

#### 3.9.6 `#[opaque]` handle types

`#[opaque]` marks a type that is a **handle to something the FFI owns**. Its
body is empty — an opaque handle has no fields and no record-literal
constructor, so values come only from a foreign function
(`E_OPAQUE_TYPE_SHAPE` rejects a non-empty body). Opaque handles are the
opaque-handle category of §3.4.3: a second binding is a second name for one
resource, `is` compares identity, and methods act through the handle regardless
of whether the binding is `let` or `var`.

**An opaque handle may live inside an actor (normative).** An `#[opaque]`
value, or the `#[resource]` wrapper around one, may be

- an actor's init field, moved in at `spawn`, owned by that actor's heap and
  closed by the actor's drop glue when the actor stops; and
- a `receive fn` parameter on a **local** send, where the send consumes the
  handle.

This is what lets one actor hold one connection and serve many requests over it
instead of re-opening the resource per message.

A use of the sender's binding after such a send is `E_USE_AFTER_SEND` (User).
This is a rule, not a limitation: a handle that is still live at the send cannot
be snapshotted, because there is nothing to snapshot but the resource itself.
It is the fourth wall of the ownership model, and it applies to handles only.

A handle is never `#[wire]`, so sending one to a remote `Pid` stays
`E_OPAQUE_MESSAGE_PAYLOAD` (User): a remote payload must be CBOR-serializable
and a handle has no serializable layout.

> **Limitation at edition 2026 (`E_LIMIT_OPAQUE_ACTOR`, Limitation channel).**
> The local case above is refused today: an `#[opaque]` type in a `receive fn`
> parameter or an actor init field is rejected with the message that a message
> payload must be CBOR-serializable, applied to a local send that never
> serializes. The refusal rides the Limitation channel under its own code so
> that one code has one channel, and it lifts when local sends carry the
> transfer-last-use move.

---

### 3.10 Standard Library Architecture

Hew ships its standard library as Hew source under `std/`. Modules are imported
by path (`import std.math;`, `import std.fs;`) and most high-level APIs are
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
- Concurrency: `Task<T>`, `Stream<T>`, `Sink<T>`, `ScopeFailure`, actor
  completion and submission envelopes (§2.1.1), `scope`, `fork`, `await`,
  `select` and `race` (§4); `after` is a select timer arm (§4.11.3).
- System and I/O: `std.fs`, `std.io`, `std.path`, `std.os`,
  `std.time`.
- Formatting: `std.fmt`.
- Encoding: `std.encoding.json`, `std.encoding.msgpack`.
- HTTP: `std.net.http.server` and `std.net.http.client`, at the
  request/response level.
- Utilities: `std.math`, `std.testing`.

See HEW-FUTURE.md §3 for modules that exist in `std/` today but are not yet
normative — `std.net.dns`, `std.net.tls`, `std.net.quic`,
`std.net.websocket`, `std.encoding.xml`/`yaml`/`toml`/`csv`,
`std.text.regex`, `std.process`, `std.encoding.compress`.

**Nothing is a grab-bag.** There is no `misc` namespace and no module whose
job is "the rest": a module that cannot be named for what it holds is deleted,
or its contents move to a module that can. `std.crypto.hash` names the hashing
module; `std.log` and `std.uuid` sit at the top level. `std.deque` and
`std.arena` are handle types (§3.4.3) rather than collections, so they stay at
the top level and never move under a `collections/` path. Modules with no
consumer are deleted rather than kept for symmetry. Retired spellings are
recorded in [`docs/migrations/v0.6.0.md`](../migrations/v0.6.0.md) and nowhere
else.

#### 3.10.2 Core Traits

The language supports user-defined traits, associated types, and the three
receivers of §3.6. The current stdlib does **not** ship a full generic
iterator-trait hierarchy; modules such as `std.iter` expose concrete helper
functions instead.

`Eq`, `Ord`, `PartialOrd`, and `Hash` are derived by default and may be
overridden by a user `impl`, which `==`, the ordering operators, and hashing
then dispatch to (§3.6).

The following traits are representative of the current trait style:

```hew
trait Printable {
    fn fmt(self) -> string;
}

trait Releasable {
    fn drop(consume self);
}
```

**`Error` (normative).** `Error` is a prelude trait declared in
`std/builtins.hew` with `Display` as its supertrait and no members of its own:

```text
trait Error: Display {}
```

Every public error enum in `std` and in a `hew.` package implements both
`Display` and `Error`. An error type that cannot print itself is not a
finished error type: `f"{e}"`, `println(e)`, a log line, and a JSON error body
all reach the same `Display` text, and `dyn Error` (§2.2.1) is the type that
composes errors across modules.

**`Display` style for errors (normative).** An error's `Display` text names its
variant first, then the detail, separated by `": "` — for example
`Partition: the route to the monitored peer is unavailable or the peer is
suspect`. A log line or a JSON error body is then searchable by the same name
the program matches on in a `match`. Prose-only text that omits the variant
name is not conforming.

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
for propagation. Any error type `E` may be used with `Result<T, E>`. Each module defines its own structured error enum, as demonstrated by the canonical `std.fs.IoError`:

```hew
pub enum IoError {
    NotFound(i64),
    PermissionDenied(i64),
    AlreadyExists(i64),
    TimedOut(i64),
    Cancelled(i64),
    Other(i64),
}
```

Each stdlib module defines its own error type following this shape; there is no single cross-module error enum. A function that composes errors from several modules names `dyn Error` as its error type (§2.2.1) rather than declaring a union enum.

**Fallible means `Result` (normative).** A standard-library function reports
failure in the type system or not at all. Three rules cover the whole surface:

1. **Fallible is `Result<T, E>` with `E: Error`.** The bare name carries the
   `Result`; there is no `try_`-prefixed twin beside it. A caller that wants a
   crash on failure writes `expect(reason)` at the call site, where the
   decision and its reason are both visible.
2. **Absence is `Option<T>`.** A lookup that can miss returns `None`, never a
   zero value, an empty string, or a designated "not found" variant of the
   success type.
3. **Nothing returns a status integer or a sentinel.** A negative `i64`, a
   zero-length string standing for "unset", and an error enum variant meaning
   "no error" are all fail-open shapes: the caller who forgets to test them
   runs on with wrong data. `ActorError` has no `NoError` variant.

The one stated exception is **indexing**. `v[i]`, `m[k]`, and `Vec.set` out of
bounds trap ("Bracket indexing" below), because a bounds failure is a
program error rather than a fallible operation. `Vec.get` and `HashMap.get`
remain the `Option`-returning forms for the case where a miss is expected.

Signatures the rules fix:

| Function | Signature |
| --- | --- |
| `os.env(name)` | `-> Option<string>` |
| `os.args()` | `-> Vec<string>` |
| `os.set_env(name, value)` | `-> Result<(), EnvError>` |
| `fs.read(path)` | `-> Result<string, IoError>` |
| `observe.read(key)` | `-> Option<i64>` |
| `observe.barrier()` | `-> Result<(), ObserveError>` |
| `http.Server.accept()` | `-> Result<Request, NetError>` |
| `Request.respond*(…)` | `-> Result<(), NetError>` |
| `wire.from_json(text)` | `-> Result<T, wire.DecodeError>` |

**No error is reported by a side channel.** A module does not expose a
`*_message(e) -> string` function beside its error type — `Display` is the one
rendering authority (§3.10.2). A module does not expose a `last_error()` poll
backed by a thread-local slot: an error that is returned cannot be missed,
while an error that must be fetched can. The `*_message` family is gone at
edition 2026; the remaining `last_error()` polls and the slots behind them are
implementation debt rather than an alternative error-return contract. Runtime
FFI may use internal status channels, but they are not the public recovery API.

**`string` and Vec** are built-in generic/runtime-backed types with dot-syntax
methods:

```hew
type string {}
type Vec<T> {}

impl<T> Vec<T> {
    fn new() -> Vec<T>;
    fn push(var self, item: T);
    fn pop(var self) -> T;                     // traps on empty vec
    fn len(self) -> i64;
    fn get(self, index: i64) -> Option<T>;
    fn set(var self, index: i64, item: T);      // traps out of bounds
    fn contains(self, item: T) -> bool;
    fn clear(var self);
    fn append(var self, other: Vec<T>);
}
```

**Collection element contracts.** A vector may own ordinary data, resource
handles, generators, tasks and supported callable values. Each admitted element
needs its exact layout and lifetime contract; a clone recipe is required only
for operations that copy it. A borrowed read and an owning removal are distinct
operations (§3.8.1).

Fixed-size arrays retain their exact element type and length on the native
path, including when stored in a collection. They use the element's ordinary
copy and cleanup contracts. Native storage currently uses a heap buffer;
allocation geometry must fit the target address space and runtime length
representation. There is no source size cap derived from a stack budget.

Commonly used string operations include `+`, `==`, `!=`, `.len()`,
`.contains()`, `.trim()`, `.replace()`, `.split()`, `.lines()`,
`.is_digit()`, `.is_alpha()`, and `.is_alphanumeric()`.

`HashMap<K, V>` is also built in. `HashMap.get()` returns `Option<V>`.

**Bracket indexing** — a `HashMap<K, V>` supports `m[k]` subscript syntax keyed
by the same `K: Hash + Eq` bound every HashMap method enforces. A read `m[k]`
returns the bare value `V`, and traps at runtime
(`hew: failure: IndexOutOfBounds (205)`, exit 1) when the key is
absent — it is NOT sugar for `m.get(k)`. Use `m.get(k)`, which returns
`Option<V>`, whenever the key may be missing. An assignment `m[k] = v` inserts
or overwrites the entry (sugar for `m.insert(k, v)`). Indexing with a key of
the wrong type is a type error. This matches `Vec<T>` indexing, where `v[i]`
takes an `i64` index and returns the bare element `T`.

```hew
var m: HashMap<string, i64> = HashMap.new();
m["answer"] = 42;        // insert/overwrite via index-assignment
let hit = m["answer"];   // i64 — 42
let miss = m.get("absent");  // Option<i64> — None (m["absent"] would trap)
```

**HashMap value ownership.** Keys must satisfy `Hash` and `Eq` as well as the
supported layout contract. Values need a release contract. `get` may borrow a
clone-free value, and `remove` transfers it; operations that return independent
copies require a clone contract. A map containing an affine value cannot be
implicitly copied. Admission is checked per operation, not by a fixed list of
scalar value types.

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
- Map literals compile to a `HashMap.new()` followed by one `insert` call per
  entry; no heap-coalescing is performed at compile time.

Available `HashMap` methods:

| Method                    | Returns         | Description                      |
| ------------------------- | --------------- | -------------------------------- |
| `HashMap.new()`           | `HashMap<K,V>`  | Create empty map                 |
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
| `HashSet.new()`           | `HashSet<T>`    | Create empty set                 |
| `s.insert(item)`          | `()`            | Insert; duplicate inserts are a no-op |
| `s.contains(item)`        | `bool`          | Test membership                  |
| `s.remove(item)`          | `bool`          | Remove an item; true if present  |
| `s.len()`                 | `i64`           | Number of entries                |

#### 3.10.4 Collections, I/O, and Utility Modules

The standard library exposes concrete modules rather than a large trait
hierarchy. Representative APIs include:

```hew
import std.deque;
import std.fmt;
import std.io;
import std.iter;
import std.math;
import std.sort;
import std.testing;

fn main() {
    let ints: Vec<i64> = Vec.new();
    let set: HashSet<i64> = HashSet.new();
    let dq = deque.new();

    println(math.abs(-5));
    println(fmt.to_hex(255));
    println(iter.sum(ints.into_iter()));
    testing.assert_true(set.len() == 0);
    println(io.read_all());
}
```

Important current details:

- `std.io` currently provides plain functions (`read_line`, `write`,
  `write_err`, `read_all`), not `Read`/`Write`/`BufRead` traits
- Built-in `HashSet<T>` currently lowers the supported surface forms
  `HashSet<i64>` and `HashSet<string>` through the typed-layout runtime;
  unsupported `HashSet<T>` usages are rejected fail-closed during type
  checking, including nested annotations, function signatures, and `#[wire] enum`
  payloads; admission follows the HashSet ABI and its independent element,
  `Hash`, and `Eq` requirements
- `std.iter` exposes lazy adapters (`map`, `filter`, `take`, `skip`) over
  any `Iterator`, driven by terminal helpers (`fold`, `count`, `collect`,
  `any`, `all`, `sum`, `sum_f64`, `product`, `product_f64`); drive a
  `Vec<T>` through it via `.iter()` or `.into_iter()`. These adapters and
  terminal helpers consume the iterator: constructors store it, and terminal
  operations finish or release it. The iterator cannot be reused after the
  call. Callable arguments retain their own declared consume/borrow contract
- `std.sort` generic helpers — one `sort<T: Ord>` and one
  `reverse<T>` over `Vec<T>` are the intended surface, preserving an
  independent input value. The current std source still contains specialized
  helpers; generic consolidation is pending, not an implemented API claim
- `std.testing` is a pure-Hew assertion library layered on top of `panic()`.
  Its whole surface is `assert(cond, msg)`, `assert_eq<T: Eq + Display>`, and
  `assert_ne<T: Eq + Display>`; the monomorphic per-type assertion family
  (`assert_true`, `assert_eq_int`, and the rest) is deleted. A generic
  assertion needs `Display` to report a mismatch, so comparing an `Option` or
  a `Result` is done by matching on it until `Display` for those two types
  lands at v0.7.0

**One form per operation (normative).** Where a generic form compiles, the
monomorphic twins beside it do not exist: `std.vec`, `std.option`,
`std.result`, `std.sort`, and `std.testing` expose the generic function and
nothing per element type. A module exposes an operation once — a method or a
free function, never both — and a `#[resource]` type's release is its `close`
method, so there is no `Closable` trait and no per-type `free` function
(`csv.Table.free`, `semver.Version.free`, `json.Value.free` are all deleted;
scope-exit drop glue and an early `close()` are the two ways a resource ends,
§3.7.8).

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

**One string surface (normative).** String operations are methods on `string`.
The `string_*` builtin family — the `string_*`-prefixed names, `substring`,
the free `len`, and the four `*_to_string` conversions — is deleted, together
with the free-function twins in `std.string` that shadowed the same methods
and the aliases in `std.fmt` that shadowed them again. `s.len()`,
`s.slice(a, b)`, `s.contains(t)`, and `f"{v}"` are the spellings; there is no
second name for any of them. A type renders itself through `Display`
(§3.10.2), never through a per-type `to_string` builtin.

**Indexing and slicing (normative).** `string`, `bytes` and `Vec<T>` share one
index and range-slice surface. `s[i]` reads the `i`th codepoint of a string,
the `i`th byte of a `bytes` value, and the `i`th element of a vector.
`x[a..b]`, `x[a..]`, `x[..b]` and `x[..]` select a range: a string slice is a
fresh owned string of codepoints, a bytes slice is an independent handle onto
the same buffer, and a `Vec<T>` slice is a fresh vector holding a copy of each
selected element. An index or endpoint outside the value reports
`IndexOutOfBounds` and releases the live owners on the way out. Because a Vec
slice copies its elements, a vector whose element type has no clone — a
`#[resource]` or `#[linear]` type, an opaque handle, a channel half, a
generator — cannot be range-sliced; an owning removal moves those elements out
instead. `for c in s` walks a string's codepoints and `for b in raw` walks a
bytes value's bytes, in each case yielding the same element `s[i]` would.

**Borrowed elements (normative).** When a `Vec<T>` element type has no clone,
`for x in v` binds each element as a borrow of the slot the vector still owns,
and `v[i]` reads one the same way. The body may read the element and call its
borrowing methods; consuming it — moving it into another binding, passing it
to a consuming parameter, calling a `consume self` method, or returning it —
is refused, as is mutating or draining the vector while the loop holds it. An
owning removal moves elements out. An element type with a clone keeps the
per-iteration independent copy, so existing loops are unchanged.

#### 3.10.6 Prelude (Automatically Imported)

The following are automatically available in every Hew module:

```text
// Types
Option, Some, None
Result, Ok, Err
string, Vec, Box

// Traits
Clone, Copy
Send, Frozen
Debug, Display, Error
Iterator, IntoIterator
Eq, Ord, Hash

// Functions
print, println
panic, assert, debug_assert
```

#### 3.10.7 Typed Handles

Standard library functions return opaque typed handle objects. A handle is
released by its `close()` method or by scope-exit drop glue, whichever comes
first: a `#[resource]` handle is closed at scope exit (§3.7.8), and `close()`
is the way to release one early. A handle that is neither `#[resource]` nor
closed is a leak, so a new handle type carries the attribute.

Every acquisition and every operation that can fail reports it as a `Result`
(§3.10.3); there is no `try_`-prefixed twin beside any of these names.

| Type             | Created by                                 | Methods                                                                                                                              |
| ---------------- | ------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------- |
| `http.Server`    | `http.listen(addr) -> Result<Server, NetError>` | `.accept()` → `Result<http.Request, NetError>`, `.close()`                                                                       |
| `http.Request`   | `server.accept()` or `http.accept(server)` | `.path`, `.method`, `.body`, `.header(name)`, `.respond(status, content_type, body)` → `Result<(), NetError>`, `.respond_text(status, body)` → `Result<(), NetError>`, `.respond_json(status, body)` → `Result<(), NetError>`, `.close()` |
| `net.Listener`   | `net.listen(addr) -> Result<Listener, NetError>` | `.accept()` → `Result<net.Connection, NetError>`, `.close()` |
| `net.Connection` | `listener.accept()` or `net.connect(addr)` | `.read()` → `Result<bytes, net.NetError>`, `.read_string()` → `Result<string, net.NetError>`, `.set_read_timeout(ms)`, `.set_write_timeout(ms)`, `.write(data)` → `Result<(), net.NetError>`, `.write_string(data)` → `Result<(), net.NetError>`, `.close()` |
| `process.Child`  | `process.start(cmd) -> Result<Child, ProcessError>`, `process.start_argv(cmd, argv) -> Result<Child, ProcessError>` | `.wait()`, `.kill()`                     |

Handle types are opaque — their internal representation is not accessible.
They can be stored in variables, passed as function arguments, and
returned from functions. Opaque resource handles are affine: a second binding
transfers ownership rather than creating another closer. `is` compares handle
identity, and each method's declared receiver controls borrowing, mutation or
consumption. The actor ownership rules of §3.9.6 still apply.

`net.Listener.accept()` and `net.Connection.read()` are plain suspending
calls (§4.0): they park the calling execution context rather than blocking
its thread, and they carry no `await`. A deadline on one of them is the
socket's own read and write timeouts, or a deadline on the enclosing scope;
there is no expression timeout combinator.

#### 3.10.8 Regular Expressions

`std.text.regex` is shipped. It compiles patterns and supports matching,
replacement, indexed and named captures, and multi-match capture tables.
`regex.Pattern` is a `#[resource]`-annotated type with RAII handles (§3.7.8):
`close()` releases early, and the implicit scope-exit drop covers the rest.
Pattern construction is currently fail-fast: `regex.new()` panics for invalid
syntax rather than returning a structured compile error.

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
- **Ordinary storage** — a state tag plus its payload, with normal ownership
  for heap values and output collections. A machine creates no thread.

### 3.11.1 Declaration Syntax

```hew
machine Name {
    // Input-event vocabulary — declared up front (mandatory header)
    events {
        EventX,                            // event with no payload
        EventY { payload: Type, },          // event with payload
        EventZ,
    }

    // Output vocabulary — optional and separate from input events
    emits {
        Changed { value: Type },
    }

    // States — at least one required
    state StateA,                          // unit state (no fields)
    state StateB { field: Type, },         // state with data

    // Transitions: on Event: Source => Target { body }
    on EventZ: StateB => StateA { .StateA } // explicit body returns target value
    on EventY: StateA => StateB { .StateB { field: event.payload } }

    // Head binding: name payload fields at the rule site
    on EventY(payload): StateB => StateA { Name.StateA }

    // Self-transition with reenter (runs exit/entry even when state is unchanged)
    on EventX: StateB => StateB reenter { .StateB { field: self.field } }

    // Wildcard — applies in all unhandled source states for this event
    on EventX: _ => _ { state }           // external transition, even to the same tag

    // Depth-1 composite state (substate block; depth > 1 is reserved)
    state Parent {
        initial state Sub1,
        state Sub2 { value: i64, }
    },

    // Default handler — fallback for ALL unmatched (state, event) pairs
    default { state }
}
```

**Surface spelling** (an illustration of what `hew-parser` accepts, not a
normative grammar — see §12):

```ebnf
MachineDecl    = "machine" Ident TypeParams? "{"
                   EventsHeader
                   [ EmitsHeader ]
                   { StateDecl }
                   { TransitionDecl }
                   [ DefaultArm ]
                 "}" ;

EventsHeader   = "events" "{" [ EventDecl { "," EventDecl } [ "," ] ] "}" ;
EventDecl      = Ident [ "{" FieldList "}" ] ;
FieldList      = [ Ident ":" Type { "," Ident ":" Type } [ "," ] ] ;
EmitsHeader    = "emits" "{" [ EventDecl { "," EventDecl } [ "," ] ] "}" ;

StateDecl      = "state" Ident ( "{"
                   { Ident ":" Type "," }          (* field declarations *)
                   [ "entry" Block ]                (* entry hook *)
                   [ "exit"  Block ]                (* exit hook  *)
                   { CompositeMember }              (* depth-1 composite only *)
                 "}" )? "," ;
CompositeMember = [ "initial" ] StateDecl ;         (* exactly one "initial" required *)

TransitionDecl = "on" Ident [ "(" Ident { "," Ident } ")" ] ":"
                 StatePattern "=>" StatePattern
                 [ "reenter" ] [ "when" Expr ] TransitionBody ;
TransitionBody = "," | "{" FieldInitList "}" | Block ;
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

A machine declares at least one state and one input event. Every state/input
pair needs an explicit rule, a source wildcard or `default { state }`.
Guarded rules require an unconditional fallback at the same or a lower
priority. Rules after an unconditional fallback at the same priority are
unreachable. A fixed target must be constructed on every normal path with
all payload fields initialized.

Machine evaluation is synchronous and pure: guards, transition bodies,
hooks and their transitive helpers may compute and mutate local value data,
but cannot perform I/O, interact with actors, suspend, access unsafe memory
or retain external resource identity. An unknown or indirect call has no
purity proof and is rejected. Checked computation faults remain possible.
Inputs, states and outputs must support independent value copies.

The native evaluator currently admits ordinary concrete machines. Const
parameters, composite states and unclassified generic payloads are not yet
admitted by this execution path. Parser support alone is not execution support.

### 3.11.3 Transition Bodies

Inside a transition body the compiler binds two implicit names:

| Binding     | Type              | Meaning                                         |
| ----------- | ----------------- | ----------------------------------------------- |
| `state`     | source state type | Fields of the current (source) state            |
| `event`     | event payload     | Payload fields of the incoming event (if any)   |

```hew
machine Elevator {
    state Stopped { floor: i64, },
    state Moving  { from: i64, to: i64, },

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
on Toggle: Off => On;   // equivalent to: on Toggle: Off => On { .On }
```

**State names are not variants (normative).** The name after `=>` in a
transition head is a state name in the machine's own namespace, resolved
against the machine's `state` declarations. It is not an enum variant in
expression position, so the variant-spelling rule of §3.1 does not reach it
and `on Toggle: Off => On;` is well formed as written. A `;` body is legal in
every transition form, guarded ones included. A machine's states desugar to
an enum below the surface, and that desugar — not the source spelling — owns
their identity.

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
unchanged as a value, but still runs its hooks.

A wildcard target is always an external transition: exit hook, transition
body, then the resulting state's entry hook. This also applies when the
body returns the source state's tag. `reenter` is allowed and redundant on
a wildcard target. A fixed same-state target skips exit and entry unless
it explicitly says `reenter`. Hook selection therefore never needs to
speculate about or repeat an unevaluated transition body.

Priority order (highest to lowest):

1. Explicit transitions (specific source state, no wildcard)
2. Wildcard/`_`-source transitions
3. `default` handler

Specific transitions always win over wildcards for the same event.

### 3.11.6 Generated API

Each `machine Name` becomes an ordinary state enum and methods, with these
companion types:

| Generated item | Behaviour |
| --- | --- |
| `NameEvent` | Typed input variants from `events` |
| `NameOutput` | Separate typed output variants from `emits` |
| `NameStepDisposition` | `Taken` for an explicit rule; `Ignored` for default fallback |
| `NameStep` | Must-use report with `outputs: Vec<NameOutput>` and `disposition: NameStepDisposition` |
| `m.step(event)` | Stages evaluation and returns `NameStep`, committing `m` only after success |
| `m.state_name()` | Returns the current state tag as a string |

`emit` appends output data in evaluation order; it never recursively feeds
an input event or performs the represented work. Without an `emits` header,
`NameOutput` is an empty enum and the report's vector is empty. There is no
dummy output variant or hidden queue.

The step copies the current owning value into staged evaluation. A checked
fault before commit leaves the caller's state unchanged and releases the
candidate and any collected outputs. Successful output values remain valid
independently of later state changes or the machine's lifetime.

```hew
var light: Light = .Off;
let report = light.step(.Toggle);
for output in report.outputs {
    // Interpret the typed output in the surrounding effectful application.
    handle_output(output);
}
```

**Pattern matching** — machine values can be destructured in `match`, `if let`,
`while let`, and function parameters exactly like enums:

```hew
match cb {
    .Closed { failures } => println(f"failures = {failures}"),
    .Open                => println("open"),
    .HalfOpen            => println("half-open"),
}
```

### 3.11.7 Using Machines Inside Actors

Machines are values — they are commonly embedded as actor fields:

```hew
actor ConnectionManager {
    var tcp: TcpState = TcpState.Closed,

    receive fn handle(event: TcpStateEvent) {
        let report = tcp.step(event);
        handle_outputs(report.outputs);
        // React to the new state
        match tcp {
            .Established { local_seq, remote_seq } => {
                println(f"established seq={local_seq}/{remote_seq}");
            },
            _ => {},
        }
    }
}
```

Because `machine` is a value type, assigning a machine variable copies it.
A successful `step()` updates its receiver and returns a report. Actor and
supervisor composition must satisfy their ordinary value ownership and
lifecycle contracts; it does not introduce a second machine runtime.

### 3.11.8 Type System Integration

- A machine type is a nominal type; it does not implicitly unify with any
  `enum` or other machine.
- Machines satisfy `Send` if all their state fields satisfy `Send`
  (same rule as structs).
- Machines can be used as type parameters wherever the bound permits.
- A machine declaration may itself be generic (`machine Lifecycle<T> { ... }`);
  see §3.11.7 for the type arguments the substrate admits.

---

## 4. Effects, IO, and Async Semantics

Actors are independent failure domains. Structured tasks run concurrent work
whose lifetime is bounded by its parent. Ordinary calls wait for their result;
`fork` starts concurrent work and `await` joins tasks.

### 4.0 Suspension (normative)

Suspension is an inferred callable effect. A named function or closure that
reaches a suspending operation suspends; the call is still written `f(x)`.
There is no `async fn` declaration or `await` on an ordinary call.

A written callable type specifies its effect at a data boundary. `fn(...) -> T`
is non-suspending; `fn[suspends](...) -> T` permits suspension. This qualifier
composes with the callable capabilities in §3.8.6. A non-suspending callable
can fill a suspending slot, but the reverse is rejected.

Parameters, return types, fields and explicitly annotated bindings carry these
contracts. A function calling a `fn[suspends]` parameter itself suspends; it
does not become effect-polymorphic.

Waiting for actor completion, task results, channel or stream input, timers
and suspending I/O uses the caller's execution context. For example,
`sleep(1s)`, `fs.read(path)` and `rx.recv()` are plain calls. The compiler
rejects suspension in a context that cannot support it, including a deferred
body. `await` is reserved for `Task<T>` and `Vec<Task<T>>` (§4.4).

### 4.1 The Task Type

`Task<T>` owns a concurrent computation whose ordinary result has type `T`.
It is affine and cannot be sent as an actor message. Its owner may join it
once. The task may be pending, running, completed with a value, cancelled or
faulted; cancellation and faults are structured outcomes, not extra variants
inserted into `T`.

Tasks do not share mutable parent state. Their captures must satisfy the
owning transfer contracts of §4.3. Scheduling is an implementation detail;
source code does not select an OS thread or a coroutine substrate.

### 4.2 Scope: Structured Concurrency Boundary

`scope { ... }` is an expression. Its tail supplies its value; no tail means
unit. Before delivering the value, the scope drains its children and completes
its cleanup. It can be used in a binding, return value or select-arm body.

```hew
fn square(n: i64) -> i64 { n * n }

fn main() {
    let total = scope {
        let first = fork square(3);
        let second = fork square(4);
        (await first) + (await second)
    };
    println(total);
}
```

Every callable also supplies an implicit task lifetime. An explicit `scope`
creates a narrower boundary; `fork` does not require a redundant explicit
scope around every function body. A task cannot escape the lifetime that owns
it, including through an aggregate returned as a scope's value.

Normal exit waits for unfinished children. A structured fault or cancellation
cancels and drains affected children before releasing parent-owned resources.
An ordinary child `Err(e)` is a value, not a scope-cancellation trigger.

A handler attached directly to a scope recovers a structured failure:

```hew
fn main() {
    let answer = scope within 20ms {
        sleep(1s);
        42
    } handle failure {
        0
    };
    println(answer);
}
```

The failure binding has type `ScopeFailure`, with `Deadline` and `Fault`
cases. The handler runs after the scope's children and cleanup have settled
and must produce the scope's value type or diverge. Cancellation inherited
from a parent continues outward; an inner handler cannot clear it. It does not intercept an ordinary `Err` returned by the scope body.
To handle that Result, first bind the scope's result and apply ordinary
Result `handle` to the binding (§2.2.1).

### 4.3 Spawning Child Tasks

```ebnf
Scope     = "scope" [ "within" Expr ] Block ;
ForkChild = "fork" ( CallExpr | Block | BatchCalls ) ;
```

`fork call(args)` starts a call and produces `Task<T>` for the call's result
`T`. `fork { ... }` starts a body with its own return context. Arguments and
captures are acquired before the child uses them: ordinary data gets an
independent value, while an affine owner transfers to the child. The child
cannot retain a borrowed parent resource or view beyond its borrow.

A resource transferred into a child receives automatic cleanup on normal,
fault and cancellation paths. Its `close(consume self)` returns unit. Linear
values retain their must-consume obligation; a child shape that cannot meet it
is rejected (§3.7.8). Actor state is not shared mutable capture storage.

An unbound `fork` gives up direct access to the result, but its parent still
drains the child. It is not detached work, and discarding its handle does not
suppress a structured fault. If the child returns an ordinary Result, inspect
that value when the application needs its error.

### 4.4 Awaiting Tasks

`await` preserves the result type exactly:

```text
await : Task<T> -> T
await : Vec<Task<T>> -> Vec<T>
```

For a pending task it suspends until completion; for a completed task it
extracts the value. It consumes the task's ownership. Vector await joins every
task and returns values in vector order, not completion order.

| Operand | Ordinary result |
| --- | --- |
| `Task<i64>` | `i64` |
| `Task<Result<T, E>>` | `Result<T, E>` |
| `Vec<Task<Result<T, E>>>` | `Vec<Result<T, E>>` |

There is no implicit `Ok` wrapper and no automatic Result flattening.
`(await task)?` is meaningful when `T` is an Option or Result accepted by
`?`. It is not a cancellation-handling operator. Task faults and cancellation
follow the structured scope path (§4.5).

```hew
fn square(n: i64) -> i64 { n * n }

fn main() {
    var tasks = [fork square(3)];
    tasks.push(fork square(4));
    let values = await tasks;
    for value in values { println(value); }
}
```

**Batch fork.** `fork [a(), b()]` starts homogeneous calls and returns
`Task<Vec<T>>`. `fork (a(), b())` starts calls with a tuple result and returns
one task over that tuple. `await fork [a(), b()]` joins that batch. This differs
from a vector containing separate task handles, but both preserve their
ordinary result values.

Actor handles, actor-call results and stream operations are not await operands.
A concurrent actor call is `fork worker.compute(x)`; the task result is the
actor completion envelope. Actor termination uses `close` or `closed`, and
stream iteration uses plain `for` (§4.10, §4.12).

### 4.5 Cancellation

Cancellation is cooperative. A scope cancels affected children when a child
faults, an enclosing lifetime is cancelled, or its `within` deadline expires.
A race also cancels its losing operands (§4.11.2). Ordinary `Err` values do not
trigger these transitions.

Cancellation is observed at supported safepoints, including suspending calls
and task waits. It is not preemption of an arbitrary instruction. The scope
waits for cancelled children to drain before returning or running its attached
failure handler. Nested lifetimes receive the cancellation; unrelated actors
do not become children of that scope.

Cleanup follows explicit ownership edges. Deferred actions and resource closes
run according to their contracts; Hew has no user-defined `Drop` implementation
(§3.7.3). A deferred action cannot return from its enclosing function, propagate
with `?`, or suspend. Secondary cleanup failures remain observable without
replacing the primary failure.

There is no user cancellation-token API or cancellation opt-out attribute in
this surface. Programs must not rely on prompt cancellation of code that never
reaches a supported safepoint.

### 4.6 Error Handling in Tasks

Application errors and structured failures are different:

- A child returning `Result<T, E>` completes normally with that value, even
  when it is `Err(e)`. `await` delivers it unchanged.
- A child fault initiates structured cleanup and cancellation of its siblings.
  It propagates unless a scope failure handler recovers it.
- Cancellation does not create an `Err` variant in an arbitrary task's result
  type. A scope handler can recover its own deadline or fault after cleanup;
  inherited parent cancellation still propagates (§4.2).

```hew
fn read_count(valid: bool) -> i64 fails string {
    if !valid { return error "count unavailable"; }
    7
}

fn main() {
    let result = scope {
        let task = fork read_count(false);
        await task
    };
    let count = result handle problem { 0 };
    println(count);
}
```

To collect application errors, return the joined Result values from the scope
and inspect them. The runtime does not synthesize a `ScopeError<E>` from child
application errors. `?` still targets its enclosing function or child body;
there is no scope-local implicit error return.

An unrecovered child fault escaping a receive handler faults that actor and
reaches its supervision policy. A handled actor-call error is an ordinary
Result in the caller; it does not transfer the callee's fault ownership.

### 4.7 IO and Effects

I/O uses ordinary calls and the operation's declared return type. For example,
`fs.read(path)` returns a Result; no `await` marks the call. Failure types belong
to each operation. Scope cancellation is not an implicit `Err(Cancelled)` added
to every I/O API.

```hew
import std.fs;

fn read_config(path: string) -> string fails fs.IoError {
    fs.read(path)?
}
```

A runtime may offload a blocking operation or park a continuation. This must
preserve the source suspension and cleanup contracts. An execution substrate
or readiness mechanism is not a separate public call spelling.

### 4.8 Interaction with Actor Messages

An actor processes one receive handler at a time. Forked work cannot mutate
its state through shared aliases. The handler's task lifetime drains before
the next message turn begins. Results returned to the handler can be used to
update its state after joining.

```hew
fn twice(n: i64) -> i64 { n * 2 }

actor Counter {
    var total: i64 = 0,
    receive fn add_twice(n: i64) {
        let work = fork twice(n);
        total += await work;
    }
    receive fn get() -> i64 { total }
}
```

A call from that child to another actor still crosses an actor boundary. Its
completion envelope follows §2.1.1; submission still requires a mailbox view.

### 4.9 Summary: Tasks vs Actors

| Aspect | Structured task | Actor |
| --- | --- | --- |
| Start | `fork call(...)` or `fork { ... }` | `spawn Actor(...)` or a lambda actor |
| Result | `await` yields the declared `T` | a completion call yields the actor envelope |
| Application error | an ordinary Result value | declared `fails` error reaches the completion envelope |
| Fault | propagates through the owning scope | belongs to the actor and its supervisor |
| Lifetime | bounded by its parent | independent actor lifetime |
| Termination wait | task join | `close(actor)` or `closed(actor)` |

**Historical note.** Earlier drafts exposed a scope handle with separate task
launch methods. Those drafts are not source syntax for this edition. There is
one `fork` operation; scheduling choices are not public aliases.

### 4.10 Actor Completion and Termination

`close(pid)` requests cooperative stop and waits for terminal cleanup.
`closed(pid)` waits for termination without requesting it. Both return unit;
closing an already terminal actor is idempotent. `fork close(pid)` returns a
`Task<()>` and starts the same work concurrently.

A call on a receive handler waits for that handler's completion, not the
actor's entire lifetime. `fork pid.method(args)` runs it concurrently; `await`
then joins that task. `for item in pid.stream()` waits per item using ordinary
iteration (§4.12).

A supervisor uses the same `close`/`closed` forms, with `close(sup)` waiting for
its children's terminal cleanup. That supervisor surface is decided but
pending implementation; see §2.1.1 and §5.6.

### 4.11 Select and Race Expressions

Hew provides two built-in concurrency expressions for coordinating
multiple asynchronous operations. They are expressions — they produce
values — and integrate with structured concurrency and the actor model.
The all-of counterpart is not a third construct: waiting for every operand
is batch `fork` (§4.4).

#### 4.11.1 `select` Expression

`select { }` is a **sealed compiler-known construct** in edition 2026. It
waits for the first of four named operation forms to complete, evaluates
the corresponding arm, and disarms the losing registrations. There is no
user-implementable `Awaitable` trait — the four forms are exhaustive.

**Canonical syntax:**

```hew
select {
    reply   from worker.call(x)        => use(reply),     // actor call
    item    from inbox.recv()          => use(item),      // channel receive
    value   from job                   => use(value),     // forked task
    after 5s                           => abort(),        // timer
}
```

The checker classifies each source by its resolved type and operation; HIR
consumes that classification:

- `<actor-expr>.<method>(<args>)` — a method-call expression on an actor
  expression. The receiver and handler identify a completion call; no `ask`
  marker is used.
- `<receiver-expr>.recv()` — a std/channel receive on a `Receiver<T>`.
- `<task-expr>` — an expression of type `Task<T>`, the handle `fork`
  produces (§4.4).
- `after <duration-expr>` — the timer arm; carries no binding.

An arm source never writes `await`: the `select` is what waits (§4.0). The
spelling is refused at check time with a fix-it that deletes it, and a
`select` with no arms at all is refused the same way.

A stream-next arm over `Stream<T>` is not in this sealed set. Streams remain
usable through ordinary calls and `for`; the absence of a select arm does not
make the Stream value itself unavailable. Current native realization of the
four specified forms is listed in §2.1.1.

**The four forms (closed set).** Each form is fully specified by four
columns: what the winning arm binds, how the winning arm propagates a
non-success outcome at the source, how the runtime cleans up *that* arm
when a different arm wins (loser cleanup), and how the runtime cleans up
the arm when the enclosing scope is cancelled while the `select` is still
pending (outer-cancellation cleanup). Sources are the same in both
cleanup columns; the difference is which side initiates the teardown.

| Form                       | Winning bind / type             | Winning error or trap at the source                                                                                                                                                                                       | Loser cleanup (a different arm won)                                                                                                                                                                       | Outer-cancellation cleanup (enclosing scope cancelled, `select` still pending)                                                                              |
| -------------------------- | ------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `<id> from <actor>.<method>(<args>)` | `id: Result<R, ActorError<E>>` for a reply type `R` | `ActorError` per HEW-DIST-SPEC §6 — `Partition`, `Timeout`, or `Dead` as observed by the caller. Traps in the callee are isolated by the mailbox boundary and do not propagate through the ask. | If the envelope has **not yet been dispatched**, withdraw it from the target actor's mailbox by correlation id — no `OrphanedAsk` is observed on either side. If it **has been dispatched**, the reply sink is tombstoned; a late reply arriving at the tombstoned sink is classified as `OrphanedAsk` and discarded silently (no caller-visible failure). | Same as loser cleanup: withdraw-or-tombstone by correlation id, late reply classified as `OrphanedAsk` and discarded.                                       |
| `<id> from <rx>.recv()`    | `id: Option<T>` for `Receiver<T>` | `None` is a normal winning value indicating that the channel is closed; `Some(value)` carries the received item. Channel receive has no separate error surface in edition 2026.                              | Pending receive is withdrawn from the channel core; the receiver binding remains usable in the enclosing scope.                                                                                         | Same as loser cleanup: pending receive withdrawn, receiver binding remains usable for the cancellation handler.                                             |
| `<id> from <task>`         | `id: T` for `Task<T>`             | The task's own outcome, exactly as `await` would deliver it.                                                                                                                                                | The handle is not consumed: the losing task keeps running and its handle stays owned by the enclosing scope, which must still join it. Its registration is disarmed, never cancelled.                     | The registration is disarmed; the task takes the enclosing scope's ordinary cancellation.                                                                  |
| `after <duration>`         | no binding; arm type is `()`-shaped at the source | Timer expiry selects this arm; evaluating its duration follows ordinary expression rules.                                                                                                                                                                          | The timer is cancelled. No effect propagates.                                                                                                                                                            | The timer is cancelled. No effect propagates.                                                                                                              |

**Semantics:**

1. **Exhaustive arm set.** Each arm's source must be one of the four
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
    p1 from act.call(x)      => r1,         where p1: Result<B, ActorError<E>>, r1: T
    p2 from rx.recv()        => r2,         where rx: Receiver<D>, r2: T
    p3 from job              => r3,         where job: Task<C>, p3: C, r3: T
    after d                  => r4,         where d: Duration, r4: T
} : T
```

The bound identifiers are in scope only inside their own `=>`
expression. Their static types follow the table above: `p1:
Result<B, ActorError<E>>` for the actor-call arm, because an actor call
completes with a `Result` whatever else happens; `p2: Option<D>` for the
channel receive arm (so `None` is a legitimate winning value indicating
the channel observed EOF on that call); `p3: C` for the task arm; and no
binding for `after`.

**Why sealed?**

A user-implementable `Awaitable` trait would have to specify coherence
rules, cancellation hooks, fairness rules, pinning constraints, and a
loser-cleanup protocol — all unsettled in edition 2026. The four forms
above are the workloads `select` exists to serve. A user `Awaitable`
surface may land in a future edition once trait lowering and generator
cancellation are proven; see HEW-FUTURE.md.

The source forms and cleanup table are the intended contract. The current
native actor-call and stream-selection gaps are recorded in §2.1.1.

#### 4.11.2 `race` Expression

`race { ... }` runs its operands concurrently and yields the first one to
complete. Completion is completion: an operand that returns an ordinary
`Err` wins the race exactly as an `Ok` does, because a result is a result.
Every loser is cancelled and drained before the expression returns.

<!-- doctest: skip -->

```hew
let fastest = race {
    primary.fetch(key),
    replica.fetch(key),
};
```

Operands are plain calls (§4.0). `await` is never written on a `race`
operand; the `race` is what waits.

**Type rule:**

```
race { e1, e2, ... } : T
where e1: T, e2: T, ...
```

All operands share one type and the expression has that type. There is no
`Result` flattening: if the operands yield `Result<U, E>`, so does the
`race`.

**Loser cleanup.** A losing operand is cancelled by the same discipline the
`select` table gives its form (§4.11.1): an in-flight ask is withdrawn from
the target mailbox or its reply sink is tombstoned, a pending receive is
withdrawn from the channel core, and a running child unwinds through its
`defer` blocks. The `race` expression does not return until every loser has
been drained. This does not retract work already dispatched to another actor
or undo an external effect; cancelling a caller is not a transaction rollback.

**Traps.** A trapping operand is not a completion. The remaining operands
are cancelled and the trap propagates to the enclosing context.

#### 4.11.3 `after` and Deadlines

`after` marks the timer arm of a `select`:

<!-- doctest: skip -->

```hew
select {
    result from server.fetch() => result,
    after 5s => default_value,
};
```

That is its only position. A deadline over a region of code is a `scope`
with a `within` clause:

<!-- doctest: skip -->

```hew
let total = scope within 1s {
    let count = fork counter.get_count();
    await count
} handle failure {
    return;
};
let value = total handle error { 0 };
```

`scope within d { ... }` cancels affected work when `d` elapses. Its attached
`handle failure` runs after structured cleanup and must produce the scope's
ordinary value type or diverge. An application `Err` remains an ordinary
result and is handled separately. A socket operation's own timeout is a
parameter of that API, not a wrapper around the call.

There is no timeout combinator. `expr | after d` is not Hew syntax; the
three shapes above are the whole surface, and `|` is only the bitwise
operator (§12.2).

#### 4.11.4 `scope`/`fork` and `select` Composition

Edition 2026 defines the legal compositions of `scope {}` and `select {}`
explicitly. Anything not listed is rejected by type checking, with the
diagnostic pointing at the offending position.

| Composition                                              | Legality        | Rationale                                                                                                                                                                                          |
| -------------------------------------------------------- | --------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `select {}` inside a `scope {}` body or child             | Legal           | The `select` forms are single-await constructs and compose with the scope block's cancellation discipline at their safepoints.                                                                |
| `let r = fork select { ... }`                             | Legal           | A child task's expression may be a `select` expression; the task's result type is the `select` expression's type.                                                                                  |
| `scope {}` inside a `select` arm's `=>` result expression | Legal           | The arm has already won; its result expression runs in the surrounding scope as ordinary code that happens to contain a scope block.                                                               |
| `scope { ... }` as a `select` arm source                  | **Rejected**    | A scope produces a value and owns a lexical lifetime; it is not a select registration. Fork the work and use the resulting task as the arm source, without `await`. |

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
- A child task faulting while a `select` in the
  scope-block body is still pending cancels the scope block; the
  outer-cancellation rule applies to the in-flight `select`.

The same distinction holds inside a select: ordinary Result values do not
cancel the scope. The arm bodies must agree on their result type; their source
values need not share an error or request type.

### 4.12 Generators

A generator is declared `gen fn`, or written inline as a `gen { ... }` block.
A `receive gen fn` inside an actor declares a stream producer whose consumer
pulls across the actor boundary (§4.8).

**The return type of a `gen fn` names the yield type (normative).** In
`gen fn f() -> Y`, `Y` is the type of each `yield` operand, not the type the
call produces. Spelling the handle instead — `gen fn f() -> Generator<Y, R>` —
is `E_GEN_RETURN_SPELLING` (User), with a fix-it that replaces the annotation
with `Y`. The handle type is what the caller receives; a declaration that
names it says the body yields handles. A generator yielding `i64` is declared
`gen fn counter() -> i64`, and its `yield` operands are `i64`.

There is no `async gen fn`. A plain `gen fn` body may suspend and is consumed
by `for` wherever its producer lives, so the word marked nothing; `async` is
not a keyword (§12) and `async gen fn` is `E_NO_ASYNC_GEN` (User) with a
fix-it that deletes it. `gen.next()` and `for x in gen` are plain calls:
pulling from a generator you own is a call into your own frame, and it carries
the generator's inferred suspension effect like any other call (§4.0). The
pull that crosses an actor boundary is written the same way:
`for x in pid.stream()` waits per item with no marker on the loop. There is no
`for await` spelling; `await` after `for` is an ordinary parse error where the
pattern belongs.

Generator construction snapshots every captured value into a heap-owned
environment before the body ramp reaches its first `yield`. Bit-copy values and
proven null-environment function values are copied directly. Strings, bytes,
`Rc`, `Weak`, supported collections, tuples, fixed arrays, records, and enums
are cloned structurally when every reachable leaf has both a total clone and
the matching inverse drop.

The caller, enclosing closure, or actor state remains the owner of the capture
source. The generator owns only its snapshot. Destroying a completed,
suspended, or never-resumed generator destroys the coroutine frame first,
drops environment fields in reverse declaration order, frees the environment,
drops any pending yielded value, and finally frees the companion.

Opaque handles, resources, IO handles, trait objects, and capturing closure
pairs are not cloneable generator captures. A whole owned value loaded from a
generator environment is an alias; it must be cloned explicitly before being
yielded or otherwise moved into another owner.

> See HEW-FUTURE.md §1.6 for the remaining deferred generator forms
> (`Lazy<T>`, `#[prefetch(N)]`). `gen fn`, `gen {}`, and `receive gen fn` use
> the snapshot semantics above. There is no `async gen fn` form: a generator
> whose body suspends carries no marker, because suspension is inferred from
> the body rather than declared on the signature (§12).

---

## 5. Supervision (fault tolerance)

Hew's supervision is modeled after OTP concepts with first-class language syntax:

- Supervisor owns children; children fail independently.
- Restart classification: `permanent`, `transient`, `temporary`. ([Erlang.org][2])
- Supervisor strategy: `one_for_one`, `one_for_all`, `rest_for_one`, `simple_one_for_one`.
- Crash isolation covers traps raised by the program — `panic`, an arithmetic
  or bounds fault the runtime detects, an exhausted `#[max_heap(N)]` arena.
  Synchronous hardware faults (SEGV, SIGBUS, SIGFPE, SIGILL) are process-fatal
  and are NOT converted into a supervised crash (§5.7).

### 5.1 Supervisor Declaration

```hew
supervisor MyPool {
    strategy: one_for_one,
    intensity: 5 within 60s,

    child worker1: Worker(id: 1, count: 0),
    child worker2: Worker(id: 2, count: 0) restart: transient,
    child logger: Logger(level: 3) restart: temporary shutdown: 10s,
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
- `pool <name>: <ActorType>(<field>: <expr>, ...) count: <N>,` — a pool of `N`
  fungible children (only under `simple_one_for_one`). The parenthesised args
  are the per-spawn template, exactly as for `child`; `count:` is the arity.
- Per-child suffix clauses, accepted in any order:
  - `restart: permanent | transient | temporary` (optional, default
    `permanent`). This is the only restart spelling — bare `T permanent` and
    `with restart:` are not accepted.
  - `shutdown: <duration> | brutal_kill | infinity` (optional) — the
    graceful-stop deadline (default `5s`). See §2.1.1 for current limitations.
  - `count: <N>` — pool arity. Required on a `pool` child, rejected on a
    `child` declaration; it has no default, because a pool with a guessed size
    is a guess about capacity.
  - `wired_to: { <param>: <sibling>, ... }` (optional) — passes a sibling
    child's handle to this child's init param.
- Child actor types must be declared before the supervisor.

**Pool arity is a clause, not an init field (normative).** `count:` sits
beside `restart:` and `shutdown:` in the child's clause namespace, and the
parenthesised argument list stays the actor's own field namespace. An actor
that happens to declare a field named `count` is therefore poolable like any
other, and its `count` field is set the same way every other field is. The
two namespaces never collide, so no diagnostic about pool arity can land on a
user's field.

### 5.2 Restart Semantics (normative)

Let child exit reason be one of:

- `normal`
- `shutdown`
- `{shutdown, term}`
- `trap` (a language panic or fault; process abort is not supervised recovery)

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

Backoff and circuit-breaker policies are outside this section's core restart
contract. Their presence in runtime code is not a claim that a corresponding
source API is implemented; see HEW-FUTURE.md for additional supervision policy.

### 5.5 Nested Supervisors

A child declaration can name another supervisor. Dotted access such as
`root.sub` addresses that supervisor, and `root.sub.worker` addresses the
leaf's stable `ChildRef<Worker>`. Each role resolves the currently supervised
incarnation rather than preserving a stale child address.

The shape is:

```hew
actor Worker {
    let id: i64,
    var count: i64,
    receive fn tick() { count += 1; }
}

actor CacheActor {
    let capacity: i64,
    receive fn size_limit() -> i64 { capacity }
}

supervisor Inner {
    strategy: one_for_one,
    intensity: 3 within 60s,

    child w1: Worker(id: 1, count: 0),
    child w2: Worker(id: 2, count: 0),
}

supervisor Root {
    strategy: one_for_one,
    intensity: 5 within 60s,

    child workers: Inner(),
    child cache: CacheActor(capacity: 1000),
}
```

When a child supervisor's restart budget is exhausted, it escalates to its parent. The parent attempts to restart the entire child supervision subtree. If the parent's budget is also exhausted, the escalation propagates further up the tree.

### 5.6 Spawning and Accessing Supervised Children

<!-- doctest: skip -->

```hew
fn main() {
    let pool = spawn MyPool();
    sleep(50ms);

    // Access children by declared name
    let w = pool.worker1;              // ChildRef<Worker>
    let _ = w.tick();

    let w2 = pool.worker2;             // ChildRef<Worker>
    let _ = w2.tick();

    close(pool);              // Graceful shutdown
}
```

- `spawn SupervisorName(...)` — starts a supervisor with its declared children
- `sup.child_name` — named actor-child access via field syntax. The compiler
  resolves the child name to a static slot and returns `ChildRef<Actor>`, which
  re-resolves the current incarnation on every ask or tell. The child name must
  match one of the `child` declarations in the supervisor definition.
- `close(sup)` — requests cooperative stop and waits for every child's terminal cleanup.
- `fork close(sup)` — starts that stop operation as a `Task<()>`.
- `closed(sup)` — waits for termination without requesting it.

These supervisor lifetime forms are decided but not implemented on the current
native path (§2.1.1). They are not aliases for an internal runtime entry point.

**Terminal destinations (normative).**

A `ChildRef` names a supervised role and resolves its current incarnation for
each call or submission. Once the role is permanently unavailable, such as
when its restart budget is spent, a completion call reports `ActorError.Dead`.
A mailbox submission reports a `SendFailure` with a lifecycle reason. Neither
can report successful acceptance of work that cannot be delivered.

Completion and submission retain their distinct envelopes (§2.1.1). Ordinary
`Err` recovery does not turn a dead target into a live one. Accidental Result
discard is rejected for both forms; `let _ = ...;` is the deliberate discard.
The current sealed-request limitation remains as recorded in §2.1.1.

### 5.7 Crash Isolation

On native unwind-capable targets, language-level actor panics unwind through the
runtime's `C-unwind` boundary. Owned locals are destroyed by LLVM cleanup pads
before the scheduler catches the panic, marks the actor `Crashed`, emits a crash
report, and notifies the supervisor. The worker may then continue processing
other actors. Production `wasm32-wasip1` remains `panic=abort`; actor-local panic
containment there is explicitly `WASM-TODO(actor-crash-containment)` in the
capability matrix and must reach parity before this target can claim this
native recovery contract.

Synchronous hardware faults (SEGV, SIGBUS, SIGFPE, and SIGILL) are process-fatal.
Signal handlers run on an alternate signal stack, record the terminal cause
using async-signal-safe operations, and terminate; the runtime never jumps out
of an arbitrary instruction or resumes a potentially corrupted process.

A `panic()` outside an actor unwinds the same way. The generated entry runs
beneath a runtime-owned catch boundary, so the same LLVM cleanup pads discharge
the same drop obligations - `#[resource]` closes included - before the process
ends with the panic status and the panic message on stderr. There is no
main-context carve-out: cleanup is not conditional on where the call sits. The
one stack with no boundary is a synchronous lifecycle hook, which runs on the
spawning thread with no recovery frame beneath it; a panic there is
process-fatal without cleanup, and the OS reclaims what is left.

The `panic()` builtin triggers the recoverable language-panic path for testing.

#### 5.7.1 Links and Monitors

`link(pid) -> Result<(), LinkError>` and
`monitor(pid) -> Result<MonitorRef, LinkError>` subscribe the caller to another
actor's exit. Both report failure in the type system, and both share one error
type. `LinkError` has three inhabitants — `Dead`, `Partition`, and `NoContext`
— and they cover local and remote pids alike: a cross-node link carries a
`PartitionPolicy`, so the remote form needs the typed failure as much as the
local one does. `monitor` on an already-dead pid is not a failure; it delivers
`DOWN` at once, which leaves `NoContext` as its only `Err`.

Both are actor-context operations, because only an actor can receive the `DOWN`
record or the linked exit the call subscribes to. A `link` or `monitor` written
directly in `main` is `E_ACTOR_CONTEXT_REQUIRED`, refused at compile time. The
same call reached through a free function that `main` happens to call is the
same fact decided at run time: `Err(LinkError.NoContext)`. It is decided at run
time deliberately — the execution context is dynamic (§4.2), and a static
"actor-only function" marker would colour every function that might one day
link. Neither form succeeds silently, which is the property that matters: a
subscription with no reader is always reported.

> **Implementation status.** Today `LinkError` carries ten variants and names
> the missing context `NoCurrentActor`, `monitor` returns
> `Result<MonitorRef, MonitorError>`, and a `link` reached through a free
> function called from `main` succeeds and prints. The `Dead` arm is never
> produced until dead-target resolution lands (§5.6). Tracked in
> hew-lang/hew#3255.

### 5.8 Process Exit Status (normative)

A Hew program's exit code is:

```text
final = user_code                    if user_code != 0
      = 1                            if a language fault went unrecovered
      = 0                            otherwise
```

`user_code` is the value `main` returns (0 for a unit `main`) or the argument to
`exit`. A non-zero code the program chose is never overwritten: it is already a
failure and carries more information than `1`. A zero never masks a fault. This
rule combines the chosen code with faults already recorded on termination.
An explicit `exit(0)` does not erase an already recorded unrecovered fault.

**Orderly return and explicit exit differ.** Returning from `main` completes
its structured task lifetime and cleanup. `exit(code)` terminates immediately:
it does not run lexical cleanup, deferred actions, child draining or actor
stop hooks. Use orderly return when that cleanup is required. Hardware faults
and process abort are also outside graceful cleanup (§5.7).

**Traps and panics are faults under this rule.** A trap or a `panic()` that no
supervisor recovers ends the process with status 1, wherever it was raised,
`main` included. It first writes one line to stderr: `hew: failure: ` followed
by the trap kind and its code, and, for a panic, `: ` and the panic text — for
example `hew: failure: IndexOutOfBounds (205)`. The code in that line is the
runtime's internal fault tag and names the failure for a reader; it is never
the process exit status.

**`fn main() -> Result<(), E>`** requires `E: Error` (§2.2.1). Returning
`Ok(())` sets `user_code` to 0. Returning `Err(e)` writes one line to stderr —
`error: ` followed by the error's `Display` text — and sets `user_code` to 1.
The error reaches the operator through the same `Display` a log line uses; a
program that returns `Err` and prints nothing is not conforming.

**Each supervised crash carries a record**, opened at the crash site and settled
by exactly ONE ruling. A crash is HANDLED only when a recovery took EFFECT — the
failed child is alive again. Not when one was intended: a non-null parent
pointer, a message that was sent, a timer that was requested are all plans that
can fail after they are made.

Two rulings do not settle a record; they TRANSFER it:

- **escalation** to a parent that ACCEPTS it (§5.5). The record stays open and
  the parent's own ruling settles it. A subtree the parent then restarts CLEARS
  the record — nested budget exhaustion followed by a recovery upward is not a
  fault of the run.
- **arming a delayed restart** (the backoff in §5.4). The record stays open and
  the restart's effect when the timer fires settles it.

**An actor fault is unrecovered** when it reaches a point with no recovery
authority left:

- an actor crashes with no supervisor attached — nothing owns the recovery
  decision; a top-level supervisor actor crashing is this case, since it has no
  supervisor of its own;
- a supervisor rules that it cannot recover the fault: the restart budget is
  exhausted (§5.4), the child is not restartable (`temporary`, or a tripped
  circuit breaker), the restart itself produces no child, an `#[on(crash)]` hook
  answers `Kill`, or it answers `Escalate` at a supervisor with no parent;
- a TRANSFER fails: the parent is stopped or closed and never receives the
  escalation, the restart timer cannot be armed, or shutdown CANCELS an armed
  timer before it fires. Handing a record to an authority that never receives
  it — or cancelling that authority before it acts — is not a recovery;
- a supervised crash whose supervisor never rules at all — its supervisor was
  already stopping, its mailbox was closed, or shutdown joined the workers
  before the queued decision ran. An undelivered decision is not a recovery.

A crash a supervisor HANDLES leaves the exit status successful. That, and only
that, keeps a crashed actor out of the exit status. A ruling settles only its own
record: one supervisor recovering its child does not clear a sibling
supervisor's unrecovered crash, and a later successful restart does not retract
an earlier unrecovered fault. A crash raised after shutdown has begun counts
exactly like one raised mid-run.

The rule does not depend on program shape. A program that contains a supervisor
and ALSO spawns an unsupervised actor that crashes exits non-zero — the
supervisor recovers what it supervises, and nothing else.

---

## 6. Backpressure and bounded queues

An actor with no `mailbox` declaration has unbounded capacity. `mailbox N`
sets a bounded capacity. Capacity and queue-wide overflow support belong to
the actor; a sender view selects admission for that sender's own request.
Neither changes a completion call into a submission or makes its result unit.

### 6.1 Mailbox Declaration

```hew
actor Worker {
    mailbox 1024,
    receive fn record(value: i64) { println(value); }
}
```

An actor declares at most one mailbox configuration. `mailbox N` defaults to
`overflow block`. Other declaration policies are `drop_new`, `drop_old`,
`fail` and `coalesce(key)` with its supported fallback. These describe the
queue's permitted behaviour; public delivery outcomes follow §2.1.1.

### 6.2 Admission and Overflow Policies

| Call surface | Full-mailbox behaviour | Result |
| --- | --- | --- |
| actor handle | wait for admission, then handler completion | completion envelope |
| `policy(actor, on_full: .Wait)` | same as the handle | completion envelope |
| `policy(actor, on_full: .Reject)` | refuse without accepting the request | completion envelope with rejection |
| `mailbox(actor, on_full: .Wait)` | wait for admission | submission envelope |
| `mailbox(actor, on_full: .Reject)` | refuse without accepting the request | submission envelope |
| `mailbox(actor, on_full: .DropNewest)` | explicitly discard this submission | `Delivery.Discarded` on that disposition |
| `mailbox(actor, on_full: .ReplaceLatest)` | use the actor's opted-in coalescing protocol | submission envelope |

Completion views admit only Wait and Reject: a caller cannot wait for the
completion of a request the policy intentionally discards. A sender cannot
unilaterally evict other senders' work. ReplaceLatest requires actor-declared
coalescing support. Write `on_full` explicitly when constructing a view.

Waiting for capacity suspends the calling execution context. It must not
silently become dropping or a blocking wait on a scheduler worker. A terminal
destination reports failure irrespective of capacity. Unbounded capacity
removes the Full condition, not lifecycle or transport failures.

### 6.3 Coalesce Overflow Policy

Coalescing is an actor-owned protocol for replacing queued work with a newer
message of the same handler and key. An example declaration is
`mailbox 100 overflow coalesce(request_id),`. The actor must define the key
on the relevant payload; a sender view cannot invent a key or replacement
policy for an unrelated protocol.

Matching work is replaced in its queue position and the old payload is
released. With no match, the declared fallback governs admission; the default
fallback is `drop_new`. `drop_old` and `fail` are explicit alternatives.
Replacement or discard must be represented as a policy disposition, not
misreported as handler completion. Submission and completion retain the
result envelopes defined in §2.1.1.

Current native coalescing requires a checked key-projection contract and is
not yet realized. See the implementation limitations in §2.1.1; this section
specifies its intended queue behaviour, not a passing execution claim.

### 6.4 Channels

Channels have explicit bounded capacity and separately owned sender and
receiver endpoints. `tx.send(value)` and `rx.recv()` are ordinary calls;
full send and empty receive can suspend. Receive returns `Option<T>`: `None`
means the sender side has closed and buffered values have drained. Empty
strings or bytes remain data.

Endpoints are affine. Transferring or closing one consumes its owner, and
scope cleanup releases a live endpoint. A failed transfer or cancellation must
not duplicate or lose the element's cleanup obligation. A receiver may be
used as a select source under §4.11.1's intended contract; current native
select registration remains a limitation (§2.1.1).

### 6.5 First-Class Streams (`Stream<T>` and `Sink<T>`)

Hew provides two generic, move-only types for sequential I/O that can be passed between functions and stored in actor fields:

```
Stream<T>   // readable sequential source
Sink<T>     // writable sequential destination with backpressure
```

The stream contract is intentionally small:

- `Stream<bytes>` / `Sink<bytes>` are the canonical first-class streaming foundation.
- `Stream<string>` / `Sink<string>` are convenience text ABI wrappers over the same bounded channel contract.
- Core `.recv()` / `.write()` calls wait for their operation and respect backpressure; they carry no `await`.
- EOF means **end-of-stream only**. Zero-length `bytes` values and empty `string` values are valid data items.
- `sink.close()` or dropping a sink produces graceful EOF after buffered items drain.
- `stream.close()` or dropping a stream is local cancel/discard of unread items.
- Each operation retains its declared error type. Current post-open error
  reporting still varies by wrapper and is not a uniform transport-error API.

Codec adapters remain a current implementation limitation (§2.1.1). No codec
method is presented here as an available streaming operation.

Both handle types are `Send` (safe to pass to other actors), opaque (backed by a vtable), and not `Clone`.

#### 6.5.1 `std.stream` surface

```hew
import std.stream;
import std.fs;

fn main() -> Result<(), fs.IoError> {
    // Canonical in-memory bounded bytes pipe
    let (bytes_sink, bytes_stream) = match stream.bytes_pipe(16) { .Ok(pair) => pair, .Err(error) => panic(error), };

    // Convenience text pipe
    let (text_sink, text_stream) = match stream.pipe(16) { .Ok(pair) => pair, .Err(error) => panic(error), };

    // Current file helpers remain text-only in this slice
    let file_in  = stream.from_file("notes.txt")?;  // Result<Stream<string>, fs.IoError>
    let file_out = stream.to_file("out.txt")?;      // Result<Sink<string>, fs.IoError>
    Ok(())
}
```

`from_file()` and `to_file()` currently return text endpoints. Their open
errors are `fs.IoError`; this does not promise a bytes-file adapter.

#### 6.5.2 Current operations

```hew
// Pull items
match bytes_stream.recv() {
    .Some(chunk) => { ... },
    .None => { /* EOF only */ },
}

// Empty items are valid data, not EOF
text_sink.write("");
bytes_sink.write(b"");

// Close semantics
bytes_sink.close();   // graceful EOF for the paired reader
bytes_stream.close(); // local cancel / discard unread items
```

`for` is the usual way to drain a stream. The text `lines()` adapter returns
a `Stream<string>`; it does not define an implicit bytes-to-text decoder.

#### 6.5.3 Lifecycle Rules

- Closing or dropping a `Sink` signals graceful EOF to the paired `Stream`.
- Closing or dropping a `Stream` discards unread local data and releases the underlying handle.
- Streams and sinks have affine release contracts and auto-close on scope exit.
  Explicit `.close()` consumes the endpoint for early release; there is no
  user `Resource` or `Drop` implementation to write.
- Resource users finish or are cancelled and drained before their owning
  endpoint is released (§3.7.8).

#### 6.5.4 Bidirectional connections

A bidirectional network connection (such as a TCP socket from `std.net`)
splits into a `(Stream<bytes>, Sink<bytes>)` pair via `.into_stream_sink()`:

<!-- doctest: skip -->
```hew
import std.net;

let conn = net.connect("127.0.0.1:8080")?;
let (rx, tx) = conn.into_stream_sink();
// rx: Stream<bytes>  — inbound data
// tx: Sink<bytes>    — outbound data
```

Accepted connections from a `TcpListener` expose the same method. The
`Stream<bytes>` and `Sink<bytes>` halves are independently move-able and
may be passed to separate actors. See `std/net/net.hew` for the full API.

#### 6.5.5 Relation to Actor Streams

`receive gen fn` produces a `Stream<Y>` through the actor's producer turn.
That stream is an owned value with the same move-only read/close contract as
other Stream values. Plain `for` waits per item. The producer's actor remains
a separate failure domain, and closing the stream does not promise rollback
of work the producer has already performed.

---

## 7. Wire types and network contracts

Hew introduces `wire` definitions for network-serializable data.

### 7.1 Wire type requirements

A `#[wire] type` / `#[wire] enum`:

- has stable field tags (numeric IDs)
- permits `optional` fields only when their declared type resolves to `Option<T>`
- records field presence independently from the field value's null encoding
- supports forward/backward compatibility checks

### 7.2 Compatibility rules (normative)

Hew adopts Protobuf-style invariants:

- **Field numbers (tags) must never be reused**. ([protobuf.dev][4])
- Deleted fields must have their tags **reserved** to prevent reuse. ([protobuf.dev][5])
- Changing a tag is treated as delete+add (breaking unless carefully managed). ([protobuf.dev][4])
- Changing an existing field between required and `optional` is rejected in
  either direction. It changes both map-key emission and missing-key decode
  behaviour; no directional compatibility is assumed.

Hew tooling provides:

- `hew wire check --against <schema>` to enforce these rules during builds or CI.

### 7.3 Encoding Formats

Hew supports multiple encoding formats. The runtime envelope (actor-to-actor transport) uses CBOR (ratified in R62; the CBOR path is the sole internode encoding, HBF retired and migration complete). The `std.encoding` surface provides user-level wire type serialization for cross-service and file I/O use cases.

#### 7.3.1 CBOR — Default Binary Encoding

CBOR is the shipped binary encoding for Hew wire types and actor transport.
`#[wire]` record bodies are maps keyed by unsigned field tags; enum bodies are
bare tags or map-of-one payloads. The exact schema is
`hew-runtime/schemas/wire-body.cddl`.

Design goals: compact representation, fast encode/decode, language interoperability, forward/backward compatibility.

**Implementation reference:** The canonical type descriptor is defined in `hew-types/src/type_descriptor.rs` (`TypeDescriptor = ResolvedTy`). Wire codec consumers use `TypeDescriptor::canonical_string()` and the wire-kind surface in `hew-types`.

##### 7.3.1.1 Wire Type–to–CBOR Mapping

Hew wire types map to CBOR values as follows:

| Hew type | CBOR representation | Notes |
| --- | --- | --- |
| integers, floats, `bool` | corresponding CBOR scalar | fixed-width integer decoders reject out-of-range values |
| `string` | text string | UTF-8 |
| `bytes` | byte string | raw bytes |
| `#[wire] type` | map | unsigned field-tag keys |
| `#[wire] enum` | unsigned tag or map-of-one | payload variants carry an array |
| `Option<T>` | null or `T` value | value shape only; field presence is separate |
| `Vec<T>` | array | element-wise encoding |

##### 7.3.1.2 Wire Type Map Encoding

A `#[wire]` type is encoded as a CBOR **map**. Field numbers are unsigned
integer keys, and values use the table above.

```hew
#[wire]
type User {
    id: u64 @1,
    name: string @2,
    email: Option<string> @3 optional,
}

// User { id: 42, name: "alice", email: Some("alice@example.com") } encodes as:
// CBOR map: {
//   1 (uint): 42 (uint),
//   2 (uint): "alice" (text),
//   3 (uint): "alice@example.com" (text)
// }

// User { id: 42, name: "alice", email: None } encodes as:
// CBOR map: {
//   1 (uint): 42 (uint),
//   2 (uint): "alice" (text)
// }
// (optional field 3 omitted)
```

##### 7.3.1.3 Wire Enum Encoding

Unit wire enum variants are encoded as unsigned CBOR tags. Payload variants
use a single-entry map from tag to positional payload array.

`#[wire] enum` codec lowering is fully implemented: encode and decode are
emitted alongside the wire type codec path, unified on the CBOR body format.

```hew
#[wire]
enum Status { Pending, Active, Completed, }

// Status.Pending   -> CBOR integer: 0
// Status.Active    -> CBOR integer: 1
// Status.Completed -> CBOR integer: 2
```

##### 7.3.1.4 Optional Field Handling

Field presence is independent of `Option<T>`'s null/value representation:

```hew
#[wire]
type Config {
    timeout_ms: u64 @1,
    proxy_url: Option<string> @2 optional,
}

// Config { timeout_ms: 5000, proxy_url: None } encodes as:
// CBOR map: { 1: 5000 }

// Config { timeout_ms: 5000, proxy_url: Some("http://proxy:8080") } encodes as:
// CBOR map: { 1: 5000, 2: "http://proxy:8080" }
```

| Field declaration | Encode | Absent key | Present null |
| --- | --- | --- | --- |
| required `T` | emit key and value | reject | reject |
| required `Option<T>` | emit key; `None` is null | reject | `None` |
| `optional Option<T>` | omit `None`; emit `Some` | `None` | `None` |

The text codecs use the same table. Their compiler descriptor carries explicit
required/optional presence for every field and is rejected if that metadata is
missing. It never derives presence from an `Option<T>` value descriptor.

##### 7.3.1.5 List (Array) Encoding

Lists are encoded as CBOR **arrays**. Each element is encoded according to the element type:

```hew
#[wire]
type Data {
    values: [i64] @1,
    tags: [string] @2,
}

// Data { values: [1, 2, 3], tags: ["a", "b"] } encodes as:
// CBOR map: {
//   1: [1, 2, 3] (array of 3 ints),
//   2: ["a", "b"] (array of 2 strings)
// }
```

##### 7.3.1.6 Nested Structure Encoding

Nested `#[wire]` types are encoded recursively as CBOR maps:

```hew
#[wire]
type Inner { x: i32 @1, }
#[wire]
type Outer { inner: Inner @1, nested_list: [Inner] @2, }

// Outer { inner: Inner { x: 150 }, nested_list: [Inner { x: 200 }] } encodes as:
// CBOR map: {
//   1: { 1: 150 } (nested map),
//   2: [{ 1: 200 }] (array of nested maps)
// }
```

##### 7.3.1.7 Forward and Backward Compatibility

Unknown fields are tolerated and discarded by the decoder. This lets an older
reader accept a newer sender without pretending that a decoded Hew value owns
data for fields absent from its type. Re-encoding that value does not preserve
unknown fields.

##### 7.3.1.8 Field Ordering and Determinism

To enable deterministic encoding (important for hashing, signatures, and comparison):

- **Encoding:** Fields SHOULD be written in ascending field-number order.
- **Decoding:** Decoders MUST accept fields in any order.
- **Duplicate fields:** Duplicate CBOR map keys are rejected as ambiguous.

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
| required `Option<T>` `None`            | present JSON key with `null`                                |
| `optional Option<T>` `None`            | field omitted                                                |
| any `Option<T>` `Some(v)`              | JSON value of `v`                                            |

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
    user_name: string @1,                       // JSON: "userName"
    email_address: string @2,                   // JSON: "emailAddress"
    internal_id: string @3 json("id"),          // JSON: "id"  (override wins)
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
    user_name: string @1,
    email_address: string @2,
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
enum Status { Pending, Active, Completed, }
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
enum Status { PendingReview, ActiveNow, Completed, }
```

```json
"activeNow"
```

#### 7.3.2a YAML Encoding

`std.encoding.yaml` is shipped for parsing, constructing, inspecting, and
stringifying YAML values. Wire types can also serialize to and from YAML using
the helper surface below.

YAML follows the JSON mapping and the same required/optional presence table in
§7.3.1.4. Missing required fields and null for bare required fields are errors;
an absent or explicitly-null `optional Option<T>` reconstructs `None`.

#### 7.3.4 Encoding Selection

Encoders select format based on context:

| Context                 | Default Format |
| ----------------------- | -------------- |
| Actor-to-actor (local)  | CBOR           |
| Actor-to-actor (remote) | CBOR           |
| HTTP API response       | JSON           |
| File storage            | user choice (`std.encoding`) |
| Debugging/logging       | JSON           |

Explicit format selection:

```hew
let msg = MyMessage { ... };
let binary = msg.encode();       // CBOR bytes
let json_str = msg.to_json();    // JSON string
let yaml_str = msg.to_yaml();    // YAML string
```

Decoding:

```hew
let msg1 = MyMessage.decode(binary);
let msg2 = MyMessage.from_json(json_str); // Result<MyMessage, string>
let msg3 = MyMessage.from_yaml(yaml_str); // Result<MyMessage, string>
```

Current shipped helper surface, as registered by the type checker:

- `#[wire] type` instance methods: `encode() -> bytes`, `to_json() -> string`,
  `to_yaml() -> string`
- `#[wire] type` static methods: `MyMessage.decode(bytes) -> MyMessage`,
  `MyMessage.from_json(string) -> Result<MyMessage, string>`,
  `MyMessage.from_yaml(string) -> Result<MyMessage, string>`
- unit-only `#[wire] enum` helpers are JSON/YAML-only:
  `to_json()`, `to_yaml()`, `from_json(string) -> Result<Self, string>`,
  `from_yaml(string) -> Result<Self, string>`

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

```text
source → lexer → parser → type checker → typed HIR
                                         ↓
                                    ownership SIR
                                     ↙         ↘
                          sandbox backend    physical MIR
                                                 ↓
                                                LLVM
                                                 ↓
                                         object + runtime
                                                 ↓
                                            executable
```

The sandbox consumes the same verified semantics before native layout. This
is the shared compiler architecture, not a statement that sandbox execution
parity is complete.

**What each stage guarantees:**

- **AST.** Concrete syntactic structure. No name resolution; no type
  information. Comments and whitespace stripped.
- **Typed HIR.** Every name binding has a stable identifier; every
  use site resolves to a binding or to a `NameNotFound` diagnostic.
  Capabilities (Send, Frozen, Copy) attach here. Module structure is
  fully resolved.
- **Ownership SIR.** Semantic SSA: typed values, effects, and the CFG.
  Concrete instances have resolved types. Generic functions are specialized
  under substitution; static trait dispatch resolves to concrete implementations,
  while dynamic dispatch retains its checked trait-object contract. Closure signatures are
  explicit; aggregate initialiser type arguments are carried.

  SIR is the ownership authority, and it is where the semantic
  fail-closed boundary sits. Every program that survives it is
  guaranteed to be free of:
  - Use after consume (affine value moved and then used).
  - Aliasing violations (read-shared XOR mutate-unique).
  - Use after move.
  - Invalid owner lifetimes across generator suspension.
  - Actor-send escape (a value captured into an outgoing message that
    aliases live state in the sending actor).
  `#[linear]` must-consume obligations are discharged here; unconsumed
  `#[linear]` values surface as `MustConsumeAtScopeExit`. No later stage
  re-derives an ownership decision; each one consumes SIR's facts.
- **Checked physical MIR.** The function body is a control-flow graph of
  basic blocks, concrete storage, call carriers and transfer operations.
  Cleanup edges (fault,
  cancellation) are real edges; every CFG exit runs the right destructor
  sequence in the right order, and `#[resource]` types' implicit `close()`
  calls are emitted here. MIR verifies its own contract against the
  ownership facts it is given rather than deciding ownership itself.
- **LLVM IR.** Produced via the `inkwell` Rust binding to LLVM. LLVM's
  coroutine intrinsics handle generator state machines; LLVM's target
  machine handles native and WASM emission; LLVM's pass manager
  handles standard optimisations.

The compiler may collapse adjacent stages into a single in-memory
representation as an implementation detail, but the **responsibilities**
above are structural: a Hew compiler that skips the ownership check is
not a conforming compiler. `hew tool compile --dump-sir` and
`--dump-mir physical` are the inspection points for the two middle
stages.

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
  single-threaded cooperative actor scheduler. Its production panic strategy is
  currently `abort`, so native actor-panic isolation does not apply; see
  `WASM-TODO(actor-crash-containment)` in the capability matrix.

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

**Fairness and suspension:**

The scheduler bounds an activation's message work and resumes parked
continuations when their readiness source fires. Suspending calls, task waits
and timer operations use this mechanism without a public yield keyword.
Compute-only cancellation and fairness depend on the safepoints actually
inserted; this specification does not promise a future reduction budget as a
current execution guarantee (§4.5).

**Memory management:**

- Per-actor ownership of mutable state; immutable storage may be retained safely
- RAII with deterministic destruction (no garbage collector)
- User-facing `Rc<T>` and `Weak<T>` provide single-actor shared ownership and
  cycle-breaking; neither can cross an actor boundary
- Terminal cleanup releases state and resources before their storage is reclaimed

**I/O integration:**

- Platform-specific event loops (epoll/kqueue/IOCP)
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
Running ───► Stopping       cooperative close or supervisor shutdown is requested
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

Cooperative termination is requested through `close(pid)` or supervision.
`closed(pid)` observes that transition without requesting it. A repeated
close of a terminal actor changes nothing and returns unit (§2.1).

**Key distinctions from task states (§4.1):**

| Aspect          | Actor State Machine                                                        | Task State Machine (§4.1)                     |
| --------------- | -------------------------------------------------------------------------- | --------------------------------------------- |
| **Entity**      | Entire actor instance                                                      | Individual task within an actor               |
| **Managed by**  | Runtime scheduler (Level 1)                                                | Actor-local coroutine executor (Level 2)      |
| **States**      | Idle/Runnable/Running/Suspended/Stopping/Crashing/Crashed/Stopped/Sleeping | Pending/Running/Completed/Cancelled/Trapped   |
| **Granularity** | One per actor                                                              | Many per actor (one per `fork` child)         |

Supervisor observes actor terminal states `Stopped` or `Crashed`.

#### 9.1.1 Actor Dispatch Interface

Actor dispatch is a compiler/runtime ABI, defined by `HewDispatchFn` in
`hew-runtime/src/internal/types.rs`. It carries execution context, actor state,
message identity, payload, size and transfer mode, and returns a continuation
pointer when execution suspends. It is not a Hew source-call signature.

The compiler's actor descriptor supplies dispatch and payload cleanup together.
State access is exclusive for the turn, and the message payload has one checked
lifetime. A resumed turn retains the same state and ownership contracts; the
actor cannot dispatch another message over that live state seat.

#### 9.1.2 Lifecycle Hooks

Actors expose user-defined startup, cleanup, and crash-observation logic
through **lifecycle hook annotations** on plain `fn`
declarations inside the actor body. The hook surface uses a single annotation
`on` with the hook kind as a positional argument, so additional hooks can be
added in later editions without growing the annotation vocabulary.

**Hooks defined in this edition:**

| Annotation       | Signature                                 | Runs when                                                                                       |
| ---------------- | ----------------------------------------- | ----------------------------------------------------------------------------------------------- |
| `#[on(start)]`   | `fn name()`                               | Once, after the actor's fields are initialized and before any message is dispatched.            |
| `#[on(stop)]`    | `fn name()`                               | Once per actor instance, on cooperative actor termination or supervisor shutdown. |
| `#[on(crash)]`   | `fn name(info: CrashInfo) -> CrashAction` | After a child trap is classified and before restart-policy handling.                            |

`#[on(exit)]` and `#[on(down)]` are the two further accepted kinds; they
deliver link and monitor notifications and their payload types are not
specified in this section.

Unknown hook kinds (e.g. `#[on(restart)]`, `#[on(upgrade)]`) are rejected with a diagnostic listing the valid set. `upgrade` is not among them: hot code upgrade is refused permanently, so the hook list holds no place for it.

`#[on(crash)]` runs on the crashing incarnation's last valid state, before
that state's cleanup. Completed changes from earlier turns and valid changes
made before the current turn's failure remain visible to the hook. A subsequent
restart constructs fresh state from the supervisor's configuration; the hook
does not run on that restart state.

`#[on(crash)]` is a defined hook. The handler ABI is `(CrashInfo) -> CrashAction`;
the returned `CrashAction` is currently side-effects-only — supervisors honour each
child's `restart_policy` instead. `CrashAction` as a supervisor control surface is
reserved (HEW-FUTURE).

**Signature rules (normative):**

1. A hook is a plain `fn` declaration inside an actor body carrying exactly one `#[on(...)]` annotation whose kind is `start`, `stop`, `crash`, `exit`, or `down`.
2. `#[on(start)]` and `#[on(stop)]` hooks take **no parameters**. Actor fields are in scope by bare name (the same convention as `init { }` and ordinary actor methods).
3. `#[on(crash)]` hooks take exactly one `CrashInfo` parameter and declare `CrashAction` as the return type. The return value is currently side-effects-only; `CrashAction` as a supervisor control surface is reserved (HEW-FUTURE).
4. `#[on(start)]` and `#[on(stop)]` hooks return `()`.
5. A hook is **not** generic and has no `where` clause.
6. Hook functions are not invocable from message handlers; the runtime is the sole caller.
7. Multiple `#[on(stop)]` hooks are permitted and execute in **lexical order**.
   `#[on(start)]` and `#[on(crash)]` each appear **at most once** per actor.

**Cancellation and resource ordering (normative):**

8. Task-scope cancellation does not itself make an independent actor a child task. Actor terminal cleanup follows its own stop or supervision protocol.
9. The runtime sequence at terminal transition is:
   - (a) the actor reaches cooperative termination;
   - (b) the `#[on(stop)]` hook runs with field access live, if present;
   - (c) cleanup follows the ownership plan, whose linear obligations were checked at compile time;
   - (d) `#[resource]` field `close()` methods run in reverse declaration order.
   Hooks therefore run BEFORE `#[resource]` `close()`, so user logic in a hook can still use resources for goodbye flushes.
10. A panic in `#[on(start)]` aborts actor startup. The supervisor is notified; `#[on(stop)]` does NOT run, because the actor never reached the *started* state.
11. A supervisor shutdown deadline belongs to its child specification (§5.1), not an invented hook argument. Current deadline limitations are listed in §2.1.1.

**Compilation:** `#[on(start)]` bodies are appended to the synthesized `_init`
function after any `init { ... }` block. `#[on(stop)]` lowers to the actor's
C-ABI `_terminate` function pointer. `#[on(crash)]` lowers to the crash hook
slot used by supervisor crash routing; its `CrashAction` result is currently
side-effects-only.

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

### 9.3 Actor mailbox delivery state machine

A bounded mailbox is open with space, open and full, or terminal. Dequeueing
work creates capacity and wakes registered admission waiters. Closure ends
admission; queued and in-flight requests retain their cleanup obligations.

- A handle call waits for admission and then completion.
- A completion-policy view may reject instead of waiting for admission.
- A mailbox view reports acceptance, an explicitly chosen discard or failure.
- A completed handler returns its success value or declared failure. A trapped
  handler produces the corresponding completion error without transferring
  its actor's fault ownership to the caller.

Capacity, coalescing and overflow follow §6. The result types and sealed-request
contract are defined once in §2.1.1; there is no unit-typed delivery variant of
this state machine.

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

The distributed contract is specified in
[`HEW-DIST-SPEC.md`](./HEW-DIST-SPEC.md). The intended source surface below
uses the same calls and error rules as local actors. Runtime transport support
does not by itself establish final-core source support or execution parity.

**Node setup (normative).** Starting a node is one call, and it reports failure
in the type system:

- `Node.start(config: NodeConfig) -> Result<(), NodeError>` is the only start.
  `NodeConfig { bind: string, transport: string, key: string, trust: string,
  peers: Vec<string>, seeds: Vec<string> }` is a prelude record, and
  `NodeConfig.at(addr)` fills the defaults around a bind address. No field is
  inert: `Node.start` pins every `peers` entry — the slot a peer occupies is
  its one-based position in that vector, so slot `0` stays reserved for local
  dispatch — dials every `seeds` entry once while skipping its
  own bind address, and admits `trust = "pinned"` only, answering
  `Err(NodeError.Config)` for anything else.
- `Node.set_transport`, `Node.load_keys`, and `Node.allow_peer` do not exist.
  Each carried one fact that is a field of `NodeConfig`, and a setup sequence
  whose steps can be reordered or skipped is a second configuration authority.
- `Node.connect(addr) -> Result<(), NodeError>` stays for explicit dials.
  `Node.shutdown()` stays `()`.

**The registry knows the actor's type (normative).**
`Node.register(name, pid: Pid<A>) -> Result<(), RegisterError>` records
`A`'s declaration identity beside the location, and `Node.lookup<A>(name)`
compares the two, answering `Err(LookupError.TypeMismatch)` when they
disagree. Without that record a lookup's type argument is the reader's wish and
the handle it produces is an unchecked cast.

`Node.register` is the one registration verb: it registers locally whether or
not a node has started, and publishes cluster-wide once one has.
`Node.unregister(name)` withdraws the name. `whereis<A>(name) ->
Result<Pid<A>, LookupError>` is the local view of the same registry, and
carries the same identity comparison.

**Distributed operations:**

- Remote actor completion calls and explicit mailbox submissions use the
  envelopes of §2.1.1; there is no remote-only call operator. In a select,
  `reply from actor.method()` is a completion source.
- Cross-node actor monitoring (`monitor` / `demonitor`) with exactly-once
  `DOWN` delivery; pruning on watcher-node death.
- Explicit cross-node links with `CrashLinked` cascade semantics.
- `PartitionPolicy.FailFast` and partition-detected-dead resolution for
  pending remote asks.
- SWIM-based membership with quarantine and incarnation-gated readmission.
- Two-process CI harness covering cross-node send/ask, monitor/link
  semantics, partition handling, and SWIM membership as the distributed
  proving gate.

Authentication tokens and supervisor-capability enforcement are not yet
runtime-enforced; see `HEW-DIST-SPEC.md` §rc1-notes for the current
status. The transport's own evidence must be distinguished from source-language
execution evidence.

**Current implementation limitation.** Node configuration and registry APIs
have not completed the single-configuration, typed-error and identity-checking
contracts above. Older runtime entry points and transport harnesses are not
canonical source examples for those contracts. Node startup, registration and
lookup must reach their intended types together; this specification does not
promise that the final native path already realizes every distributed operation.
Authentication and capability enforcement remain subject to the limitations
in HEW-DIST-SPEC.md.

---

## 12. Syntax (edition 2026)

**Accepted syntax and intended semantics.** `hew-parser` defines the syntax
accepted by the current compiler. This specification states the edition's
intended contracts; explicitly labelled implementation limitations, such as
supervisor close, remain pending even when their
design is settled. A parser limitation is not permission to implement a
superseded spelling as the language's permanent public surface.

Grammar fragments illustrate the source forms beside their semantic rules.
The parser and downstream grammars must converge on that same surface. They
do not establish separate language variants when an implementation lags.

### Structural punctuation

Commas separate structural data members: type and wire fields, enum variants
and variant fields, record values and patterns, actor state and mailbox config,
machine events, states and bodyless routes, and supervisor config and children.
A trailing comma is accepted before a closing brace. Adjacent members require
a comma; a newline is whitespace, not a separator.

Semicolons terminate statements and bodyless declarations, including trait
method signatures and extern function signatures. Function, method, lifecycle
and executable blocks do not acquire a terminator just because they occur
beside structural members. An array's `[T; N]` size syntax is not a member list
and retains its semicolon.

```hew
type Point { x: i64, y: i64 }
enum Reply { Ready, Value { label: string, count: i64 }, Failed(string) }

actor Counter {
    var count: i64 = 0,
    mailbox 64 overflow drop_new,
    receive fn bump() { count += 1; }
}

machine Switch {
    events { Toggle }
    state Off,
    state On,
    on Toggle: Off => On,
    on Toggle: On => Off,
}

supervisor App {
    strategy: one_for_one,
    intensity: 5 within 60s,
    child counter: Counter() restart: permanent,
}

trait Reader { fn read(self) -> i64; }
extern "C" { fn read_value() -> i64; }
```

Declaration context distinguishes an actor's `var count: i64 = 0,` state
member from an executable block's `var count = 0;` local statement. A machine
state with a body is still a structural member (`state Active { n: i64 },`);
its `entry` and `exit` blocks contain ordinary statements. A bodyless route
ends with a comma, whereas `on Toggle: Off => On { ... }` is self-delimiting.
The `events { ... }`, `emits { ... }` and `default { ... }` blocks do not take
an extra terminator. Supervisor child clauses such as `restart:` and
`shutdown:` remain parts of one child member, whose final separator is a comma.

**Implementation note:** pipe closures lower through `Expr::Lambda`; captured closure environment records are the current substrate direction. Generic `<T>(...) => ...` is not a valid source syntax; type-parameterized lambdas are not supported in this edition (see §3.8.6).

### Keywords

The keyword set is closed. A word in the reserved list below may not be used
as an identifier; every other word may. `docs/syntax-data.json` is exported
from the lexer and is the machine-readable form of this table — the
downstream highlighters generate from it, not from this section.

| Group | Keywords |
| --- | --- |
| Control flow | `if`, `else`, `match`, `loop`, `for`, `while`, `break`, `continue`, `return`, `in`, `yield`, `defer` |
| Declarations | `let`, `var`, `const`, `fn`, `gen`, `pub`, `import`, `package`, `extern`, `where`, `type`, `indirect`, `enum`, `trait`, `impl`, `as` |
| Actors and concurrency | `actor`, `supervisor`, `spawn`, `receive`, `init`, `scope`, `fork`, `move`, `select`, `race`, `after`, `from`, `await`, `await_restart` |
| Wire | `reserved`, `optional`, `deprecated` |
| Supervision | `child`, `restart`, `strategy`, `permanent`, `transient`, `temporary`, `brutal_kill`, `one_for_one`, `one_for_all`, `rest_for_one`, `simple_one_for_one` |
| Machines | `machine`, `state`, `event`, `on`, `when`, `entry`, `exit` |
| Literals | `true`, `false` |
| Other | `dyn`, `unsafe`, `is` |
| Reserved | `mut` (foreign pointer types, §3.9.3), `budget` |

**Contextual keywords** are words the lexer produces as identifiers and the
parser recognises only in the position that gives them meaning. Using one as
an ordinary name is legal everywhere else:

| Word | Position that gives it meaning |
| --- | --- |
| `default` | a machine's fallthrough transition arm |
| `emit` | a machine transition body's emit statement |
| `pool` | a supervisor body's pool clause |
| `events`, `emits`, `reenter`, `initial` | machine declaration headers and transition modifiers |
| `mailbox`, `overflow`, `intensity`, `within`, `shutdown`, `infinity` | actor and supervisor configuration clauses, and `within` a scope deadline (§4.11.3) |
| `handle` | the error-recovery and scope-failure clause (§2.2.1, §4.11.3) |
| `policy`, `on_full` | a sender's mailbox-policy view (§2.1.1) |
| `suspends` | the suspension qualifier in a written callable type (§4.0) |
| `self`, `consume` | receiver and transfer positions (§3.6, §3.9) |
| `clone` | the prefix-clone expression (§3.4.4) |
| `resource`, `linear`, `opaque`, `wire`, `json`, `yaml` | attribute names (§12.6) |
| `wired_to` | a supervisor child spec's sibling-handle clause (§5.1) |
| `export` | the `#[export]` attribute name (§3.9.4, §12.6) |

**Words that are not keywords.** `try`, `catch`, `join`, `cooperate`,
`foreign`, `super`, `send`, and `async` are ordinary identifiers. Each was reserved
against a surface that either shipped under another spelling or was refused:

- `async` marked nothing. Functions are colourless — suspension is inferred
  from the body and written only on a callable type (§4.0), and `await`
  joins a Task or vector of Tasks (§4.4). `async fn` is `E_NO_ASYNC_FN` (User) with a fix-it that
  deletes the word, and `async gen fn` is `E_NO_ASYNC_GEN` (User) with the
  same fix-it (§4.12).
- `try` and `catch` have no construct: fallible operations return `Result`
  and propagate with `?` (§2.2.1). `catch` never reached the parser at all.
- `join` is retired. Waiting for every operand is batch `fork`
  (`await fork [ .. ]`, `await fork ( .. )`, §4.4), which needs no keyword
  of its own.
- `cooperate` names a compiler-inserted safepoint (§4.7, §9.0), not a
  source-level expression.
- `foreign` is spelled `extern` (§3.9.1).
- `super` has no path form; an import names a module by its path from a
  module root (§3.5.2), never relative to the importing module.

`try`, `catch`, and `foreign` keep an identifier-position hint, so a program
written against the earlier reservation gets a diagnostic naming the
replacement rather than a bare parse error.

**`mut` stays reserved** because `*mut T` uses it in foreign declarations
(§3.9.3). `mut` is not a binding modifier: `let mut x = 0` is a single error
carrying the `var` fix-it, not a parse cascade. Mutable bindings are `var`
(§3.2).

**Current lexer limitation.** Some words intended as ordinary or contextual
identifiers remain reserved by the lexer, including `try`, `catch`, `default`,
`emit` and `pool`. This does not reinstate their retired constructs.
`send` has no keyword role, `this` has no receiver role, and neither `join` nor
`for await` is a language construct. Parser diagnostics for old spellings do
not make them recommended alternatives.

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

> **No active aliases.** There is no `byte` alias — using `byte` as a type name is a compile-time error. Use `u8` directly.

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
10. Equality: `==`, `!=`, `is` (`is` is **handle identity only** — it admits the pid, counted, opaque, and resource handle categories of §3.4.3 and answers whether two names denote the same actor, count, or resource. Every value-category and callable-category operand is rejected with `E_IS_VALUE_TYPE`, including scalars, `string`, `bytes`, tuples, records, enums, `Vec`, `HashMap`, and `HashSet`: these are copy-on-write values with no identity to compare, so `==` is their comparison. There is no `expr is TypeName` form; regex matching is via `Pattern.is_match`)
11. Logical AND: `&&`
12. Logical OR: `||`
13. Range: `..`, `..=` (only lowered inside `for` loop iterables; standalone range value expressions are not lowered)
14. Assignment: `=`, `+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, `^=`, `<<=`, `>>=`

> **Overflow behaviour:** the plain `+`, `-`, `*` operators on integer types are checked — they lower to the `llvm.{s,u}{add,sub,mul}.with.overflow.iN` intrinsics and trap with `TrapKind::IntegerOverflow` on overflow. `&+`, `&-`, `&*` are the two's-complement **wrapping** versions of `+`, `-`, `*`: they lower directly to the plain `IntAdd`/`IntSub`/`IntMul` instructions (no overflow check; LLVM integers wrap by default) and exist as explicit source forms for opting into wraparound. All three wrapping operators have the same precedence as their plain counterparts. `.checked_*`/`.saturating_*`/`.wrapping_*` methods (see the language guide) provide the same three overflow policies as callable methods.

> **Comparison associativity:** comparison operators are left-associative. `a < b < c` parses as `(a < b) < c`, which compares a bool against an integer and is almost certainly a bug. Use `a < b && b < c` for chained comparisons.

> **Float context widening:** In a float-typed context, an integer literal widens to the contextual float type. `let f: f64 = 1;` is accepted and `1` is treated as `1.0`. This does not affect type annotations or wire shapes.

### 12.3 Duration Literals

Duration literals have source type `duration`; their native carrier stores
nanoseconds in an `i64`:

```hew
fn main() {
    let timeout = 100ms;     // duration: 100_000_000 nanoseconds
    let interval = 5s;       // duration: 5_000_000_000 nanoseconds
    let period = 1m;         // duration: 60_000_000_000 nanoseconds
    let precise = 500us;     // duration: 500_000 nanoseconds
}
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

Duration arithmetic operators and accessor methods are fully implemented.
Duration literals compile to `i64` nanosecond values;
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
`.secs()`, `.mins()`, `.hours()`. `duration` implements `Display` (`5s`
prints as `5000000000ns`).

**`instant`:** a monotonic timestamp in nanoseconds read with
`instant.now()` (no import required). `instant + duration` and
`instant - duration` produce an `instant`, `instant - instant` produces a
`duration`, and `.elapsed()` and `.duration_since(earlier)` return the
`duration` since an earlier stamp.

**Suspension:** `sleep(d)` waits a span and `sleep_until(t)` waits until a
monotonic deadline; both suspend the calling task or actor handler instead of
blocking its worker, so sibling work continues while the timer runs and a
deadline already in the past returns immediately.

**Deadlines:**

A select timer and `scope within d` use `duration`. Socket timeout setters
use their declared API units; current `net.Connection` setters accept an
integer count of milliseconds. Neither form adds a timeout operator to calls.

<!-- doctest: skip -->

```hew
let result = scope within 5s {
    let task = fork calculate(input);
    await task
} handle failure {
    fallback
};
```

The scope and recovery body produce the same type (or recovery diverges).
The deadline covers the region; its structured failure is separate from an
ordinary Result returned by `calculate` (§4.2).

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

`break` and `continue` are pure control-flow statements. A `break` carries no
operand: `break expr;` is `E_BREAK_VALUE` (User), with the hint "assign to a
`var` before `break`". A loop never produces a value, so an operand on
`break` has nowhere to go — accepting it and discarding the value taught a
loop-as-expression model the language does not have. Loop-as-expression
(`let x = loop { break 42; }`) is not supported and has no reserved spelling.

This is orthogonal to where `break`, `continue`, and `return` may be *written*.
All three are `!`-typed (`Ty::Never`) and may appear wherever an expression is
expected — a match-arm body, an `else` block's tail, an operand of `&&`/`||` —
not only as the last statement of a block. A diverging arm's `!` type unifies
with any other arm's type, so `match d { 0 => break, _ => d }` type-checks
exactly like `match d { 0 => { break; } _ => d }`; the two spellings parse to
the identical tree. This does not make loops value-producing — `break`'s
`Never` type describes control flow leaving the match, not a value the loop
itself returns.

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
ForStmt        = "for" Pattern "in" Expr Block ;
BreakStmt      = "break" ("@" Ident)? ";" ;
ContinueStmt   = "continue" ("@" Ident)? ";" ;
```

The lexer tokenizes `@outer`-style labels as a dedicated label token; the
fragment above shows their surface spelling.

### 12.5 `if let` and `while let`

`if let` and `while let` are first-class single-branch pattern-matching
constructs. They work on any type that supports pattern matching, including
`Option<T>`, `Result<T, E>`, enums, and `machine` values.

**`if let`** — execute a block only when a pattern matches, binding the
extracted value:

```hew
let opt: Option<string> = .Some("hello");

if let .Some(s) = opt {
    println(s);      // prints "hello"
}

// With else:
if let .Some(s) = opt {
    println(s);
} else {
    println("nothing");
}
```

**`while let`** — loop as long as a pattern matches:

```hew
var m: HashMap<string, i64> = {"a": 1, "b": 2};

while let .Some(v) = m.get("a") {
    println(f"{v}");
    break;
}
```

Both forms are semantically equivalent to the corresponding `match` form; they are compiled through dedicated IR paths rather than being desugared at the AST level. `if let P = expr { body }` corresponds to `match expr { P => { body }, _ => {} }`, but the lowering is a first-class HIR node, not a transformation.

> **Supported patterns:** Only payload-bearing constructor patterns (e.g. `.Some(x)`, `.Ok(v)`, `.Err(e)`) and literal patterns work in `if let`/`while let`. Unit-variant, record, tuple, and or-patterns fail closed at HIR time. The variant spelling is the one rule of §3.1: a bare `Some(x)` pattern is `E_BARE_VARIANT_PATTERN`.

```ebnf
IfLetExpr   = "if" "let" Pattern "=" Expr Block ("else" Block)? ;
WhileLetStmt = "while" "let" Pattern "=" Expr Block ;
```

### 12.6 Attributes

**The attribute set is closed (normative).** The tables below list every
attribute of edition 2026 and the positions each one is legal in. An
attribute whose name is not in a table, or that appears in a position the
table does not list, is `E_UNKNOWN_ATTRIBUTE` (User) — on type declarations,
functions, parameters, fields, actor members, and `impl` blocks alike. There
is no position where an unrecognised attribute is ignored.

A silently ignored attribute is a fail-open in the shape the language least
tolerates: a misspelled `#[test]` produces a function nothing calls, the test
runner reports a green run over the tests that remain, and the program exits
0. The attribute a reader cannot see is the one that matters, so the compiler
refuses the name it does not know rather than dropping it.

**Attributes available to any program:**

| Attribute | Legal positions | Meaning |
| --- | --- | --- |
| `#[resource]` | type declaration | Affine handle: drop glue closes it at scope exit (§3.7.8). |
| `#[linear]` | type declaration | Linear value: must be consumed by a `consume self` method before scope exit (§3.7.8). Not combinable with `#[resource]`. |
| `#[opaque]` | type declaration | Opaque handle whose internal representation is not accessible (§3.10.7). |
| `#[wire]`, `#[wire(...)]` | type declaration, enum, field | Wire contract and per-field tag/naming metadata (§7.1, §7.3). |
| `#[json(...)]`, `#[yaml(...)]` | type declaration | Per-encoding field-naming case for a `#[wire]` type (§7.3.2, §7.3.2a). |
| `#[deprecated]` | type declaration | Accepted; no phase consumes it today. Wire field deprecation is the `deprecated` field modifier of §7.2, not this attribute. |
| `#[test]` | free function | Test entry point (the language guide's Testing chapter). Exempt from the dead-code lint. |
| `#[ignore]` | `#[test]` function | Discovered but not run. |
| `#[should_panic]` | `#[test]` function | The test passes only if the body traps. |
| `#[serial]` | `#[test]` function | Runs alone, never concurrently with another test. |
| `#[on(kind)]` | actor member `fn` | Lifecycle hook; `kind` is one of `start`, `stop`, `crash`, `exit`, `down` (§9.1.2). |
| `#[every(<duration>)]` | actor `receive fn` | Periodic receive handler (§2.1.2). |
| `#[max_heap(N)]` | actor declaration | Per-actor arena ceiling; a breach is an unrecoverable actor failure (§2.1). |
| `#[extern_symbol(name)]` | `fn` inside an `extern "C"` block or an `impl` block | Binds the declaration to a named C-ABI symbol (§3.9.1). Not legal on an actor member. |
| `#[export("...")]` | free `fn` | Makes the function callable from C (§3.9.4). |

**Substrate attributes** carry compiler-internal identity and are legal only
inside `std/`. A program that names one outside the standard library gets
`E_UNKNOWN_ATTRIBUTE` in the same way an invented name does:
`#[lang_item(...)]`, `#[intrinsic(...)]`, `#[diagnostic_item(...)]`,
`#[overload(...)]`, `#[runtime_capability(...)]`, `#[returns_receiver]`,
`#[abi(...)]`.

**Attributes removed in this edition:**

- `#[noncancellable]` no longer parses. Cancellation in edition 2026 is
  scope-structural (§4.5), and the cancellation-token vocabulary it reserved
  a place for is refused rather than deferred (HEW-FUTURE §1.2), so the
  attribute has no surface to be held for.
- `#[on(upgrade)]` is no longer a hook kind. Hot code upgrade is refused
  permanently, so the hook list stops holding a place for it (§9.1.2).

---

## 13. Self-Hosting Roadmap

> See HEW-FUTURE.md §6.1 for the self-hosting roadmap — targeted for
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

## 15. Historical minimum viable Hew plan

This early plan is retained as history, not current implementation sequencing.
The compiler architecture and source contracts above take precedence.

- **Phase A (compiler front-end)**: lexer/parser → AST → typecheck (Send/Frozen rules)
- **Phase B (runtime)**: scheduler, actor mailboxes, bounded channels, timers, TCP
- **Phase C (supervision)**: supervisor tree runtime + syntax lowering
- **Phase D (wire tooling)**: schema compiler + compatibility checker + encoder/decoder
- **Phase E (native codegen)**: LLVM backend + LTO + predictable allocation model

---

The original plan called for an IR specification before implementation. The
current compiler stages and their responsibilities are described in §8.1.

[1]: https://tutorial.ponylang.io/index.html "Pony Tutorial"
[2]: https://www.erlang.org/docs/17/design_principles/sup_princ "Supervisor Behaviour - Restart Strategy"
[3]: https://docs.swift.org/swift-book/documentation/the-swift-programming-language/concurrency/ "Concurrency - Documentation | Swift.org"
[4]: https://protobuf.dev/programming-guides/proto3/ "Language Guide (proto 3) | Protocol Buffers Documentation"
[5]: https://protobuf.dev/best-practices/dos-donts/ "Proto Best Practices"
[6]: https://www.erlang.org/doc/apps/stdlib/supervisor.html "supervisor — stdlib v7.2"

---

## Changelog

> **Historical, non-normative.** These entries preserve earlier edition
> checkpoints, including designs and implementation limits later superseded.
> They are not migration recipes or current feature-status claims. Use the
> reconciled spec body for current contracts and labelled implementation gaps.

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
  block (the scope boundary). `fork expr` is the only child-start form,
  and it is only legal inside a `scope { }` body. `scope` and `fork` are
  not synonyms. Historical note retained at §4.9.
- **Stdlib narrowing.** The edition 2026 normative stdlib is deliberately
  narrow (§3.10.1). Surfaces that exist in `std/` today but are not
  normative — `dns`, `tls`, `quic`, `websocket`, `xml`/`yaml`/`toml`/`csv`,
  `regex`, `process`, `compress` — move to HEW-FUTURE.md §3.
- **IR ladder.** §8 (compilation model) is rewritten around the new IR
  ladder: AST → typed HIR → ownership SIR → checked physical MIR → LLVM
  IR via inkwell. The v0.4 Rust-frontend / C++ backend / MessagePack-AST
  pipeline is no longer the design.
- **Deferred to next edition.** Generators (`Lazy<T>` / `#[prefetch(N)]`),
  closures with captured
  environment, user-facing `Arc<T>`, `dyn Trait`, `DoubleEndedIterator`,
  generic `HashMap<K, V>` over owned-aggregate/float keys, cancellation
  tokens, actor await + read-after-send barrier, and the self-hosting
  roadmap. See HEW-FUTURE.md for the surface and version targets.
  Channels (`std.channel`) and the rest of the `Iterator`/`IntoIterator`
  trait hierarchy shipped in this edition (§2.4, §2.5). Cross-node actor
  communication is shipped and normative — see §11 and HEW-DIST-SPEC.md.
