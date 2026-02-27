# Hew Language Specification v0.9.0 (Draft)

Hew is a **high-performance, network-native, machine-code compiled** language for building long-lived services. Its design is anchored in four proven pillars:

- **Actor isolation + compile-time data-race freedom** (Pony-style capability discipline) ([tutorial.ponylang.io][1])
- **Fault tolerance via supervision trees** (Erlang/OTP restart semantics) ([Erlang.org][2])
- **Structured concurrency with cooperative cancellation** (Swift-style model) ([docs.swift.org][3])
- **Wire contracts with enforced schema evolution rules** (Protobuf best practices) ([protobuf.dev][4])

This document specifies: goals, core semantics, type/effects model, module and trait systems, memory management, runtime state machines, compilation model, and an EBNF grammar sufficient to implement a working compiler and runtime.

**Changes in v0.9.0:**

_Task model unification, actor lifecycle fix, RAII streams, duration type, syntax cleanup._

- §4.3: Unified task model — `s.launch` for cooperative micro-coroutines, `s.spawn` for parallel OS threads
- §4.5: Cancellation is automatic at safepoints; removed manual `is_cancelled()` polling
- §4.7/§4.8: Fixed contradiction — cooperative tasks share actor state, parallel tasks do not
- §4.10: Added `await actorRef`, `await all()` for actor synchronization
- §6.5.3: Streams/sinks auto-close via RAII (Drop); explicit `.close()` optional
- §9.0: Documented 3-level preemption hierarchy (message budget, reduction budget, coroutine yield)
- §9.1: Actor lifecycle reduced from 8 states to 6 (removed Init and Blocked)
- §10.3: `duration` is now a distinct primitive type (i64 nanoseconds, type-safe arithmetic)
- Removed `isolated` keyword (all actors are isolated by definition)
- Removed template literal syntax (f-strings are the sole interpolation form)
- Removed `and`/`or` keyword operators (`&&`/`||` only)
- Removed `foreign` keyword alias for `extern` (`extern` is the sole FFI keyword)

**Changes in v0.8.0:**

_Spec accuracy — documented implemented features, removed stale aliases, clarified semantics._

- §3.5: Clarified module alias pattern — `import std::net::http;` makes the module available as `http` (short name)
- §4.12.1: Clarified that `gen fn` and `async gen fn` return types (`Generator<Y>`, `AsyncGenerator<Y>`) are compiler-inferred from the yield type annotation, not written explicitly
- §4.12.4: Clarified that `async` is only valid as a modifier on `gen fn`; standalone `async fn` has no specified semantics
- §10: Removed `ActorStream<Y>` from the `Type` production (no prior release, no backward compatibility needed)
- §10.3: Added Duration literals section documenting `100ms`, `5s`, `1m`, `1h` syntax
- Changelog: Added v0.8.0 entry

**Changes in v0.7.1:**

_First-class `Stream<T>` and `Sink<T>` types for sequential I/O._

- §6.5: Added `Stream<T>` and `Sink<T>` as first-class move-only types for sequential I/O
- §3 (Types): `Stream<T>` and `Sink<T>` added to the type grammar; both are `Send`
- §6.5.4: Documented difference between `ActorStream<Y>` (mailbox-backed) and `Stream<T>` (I/O-backed)
- `import std::stream;` makes channel, file, and byte-buffer stream constructors available
- Grammar: Added `Stream<T>` and `Sink<T>` to the `Type` production in §10 EBNF
- §4.12: `ActorStream<Y>` was a deprecated alias for `Stream<Y>` and has been removed as of v0.8.0. `receive gen fn` return type is `Stream<Y>`.

**Changes in v0.7.0:**

_Typed handles, regex literals, and match operators._

- §3.10.7: Added typed handle types (`http.Server`, `http.Request`, `net.Listener`, `net.Connection`, `regex.Pattern`, `process.Child`)
- §3.10.8: Added regex literals (`re"pattern"`), match operators (`=~`, `!~`), and regex methods
- §10.2: Updated operator precedence to include `=~` and `!~` at equality level
- §10 (EBNF): Added `RegexLiteral` production and updated `EqExpr`

**Changes in v0.6.4:**

_Module dot-syntax, string methods, and f-string expressions._

- §3.5: Added module dot-syntax for stdlib imports (`import std::net::http;` then `http.listen(addr)`)
- §3.10.3: Added string method syntax (`s.contains()`, `s.trim()`, etc.) and operators (`+`, `==`, `!=`)
- §3.10.5: Updated string interpolation to document expression support in f-strings
- §3.10.1: Updated stdlib tier with module names (`std::fs`, `std::net`, `std::text::regex`, etc.)
- §3.10.5: Documented `bool` return types for predicate functions
- §10: Updated EBNF with f-string expression grammar productions

**Changes in v0.6.3:**

_Spec accuracy — corrected compilation model and runtime references throughout._

- §8: Corrected to describe Rust frontend + C++ MLIR codegen pipeline (hew-codegen has no lexer/parser)
- §8.5: Clarified MLIRGen receives MessagePack AST from Rust frontend
- §8.7: Fixed library name to `libhew_runtime.a` from `hew-runtime/`
- §8.8: Corrected runtime from "pure C library" to "pure Rust staticlib"
- §2.1: Updated implementation note to remove C codegen references
- §7.1: Removed stale C codegen implementation note
- §10: Updated closure implementation note to reflect lambda lifting
- §11.3, §11.7: Updated bootstrap chain to reflect current architecture

**Changes in v0.6.2:**

_Spec accuracy — updated §8 to reflect the actual C++/MLIR compilation model._

- Rewrote §8 (Compilation Model) to describe the hew-codegen pipeline: lexer, parser, type checker, MLIRGen, codegen, linking
- Reorganized into §8.1–8.8: pipeline overview, lexical analysis, parsing, type checking, MLIR generation, code generation, linking, runtime
- Documented `--Werror`, `--no-typecheck`, `--emit-mlir`, `--emit-llvm`, `--emit-obj` compiler flags
- Removed old §8.1 (Rust Frontend as bootstrap) — Rust workspace is now noted as a separate tooling frontend

**Changes in v0.6.1:**

_Generator release — first-class generator/yield support for sync, async, and cross-actor streaming._

- Added §4.12 Generators with `gen fn`, `async gen fn`, and `receive gen fn` declarations
- `yield` expression produces values from generator bodies; `cooperate` remains scheduler yield
- `for await` loop syntax for consuming async generators and actor streams
- Generator types: `Generator<Y>` (sync), `AsyncGenerator<Y>` (async), `Stream<Y>` (cross-actor)
- `receive gen fn` enables cross-actor streaming via `Stream<Y>` over the mailbox protocol
- Generator trait hierarchy: `Generator<Y> : Iterator<Y>`, `AsyncGenerator<Y> : AsyncIterator<Y>`
- Grammar: added GenFnDecl, AsyncGenFnDecl, ReceiveGenFnDecl, YieldExpr, updated ForStmt with `await`

**Changes in v0.6.0:**

_Consolidation release — "one way to do it" principle applied to remove redundant syntax._

Syntax consolidations:

- Removed pipe lambda syntax `|x| ...`; arrow `(x) => ...` is the sole lambda form (R-1)
- Removed `race` keyword; `select` is the sole multi-branch completion primitive (R-2)
- `&&`/`||` are the sole boolean operators (R-3)
- Unified print to `print(dyn Display)` + `println(dyn Display)`; type-specific variants are implementation detail (R-4)
- Removed `foreign` keyword; `extern` is the sole FFI keyword (R-5)
- Removed `check_cancelled()` from grammar; `is_cancelled()` is the sole cancellation check (R-6)

Simplifications:

- Removed `async fn` from grammar (no specified semantics in actor model) (P-1)
- Removed top-level `let`/`var` from Item production (contradicts no-global-mutable-state) (P-2)
- Renamed cooperative yield to `cooperate`; reserved `yield` for generators (P-5)

Semantic clarifications:

- Scopes manage lambda actor lifetime only — no cancellation/trap propagation across actor boundary (C-1)
- Cancellation is a regular error: `await` returns `Result<T, CancellationError>`, not a trap (C-2)
- Trap in scoped `receive fn` causes actor crash → supervisor restart (C-5)
- Renamed `scope.spawn` to `s.launch` (inside `scope |s| { ... }`) — distinguishes task launching from actor spawning (S-2)
- Normative `self` semantics: by-value in struct impl, by-mutable-reference in actor methods (S-3)

Compilation model:

- Added §8.2 C++/MLIR Compiler with Hew MLIR dialect ops table, progressive lowering, runtime linkage (SA-6+TM-6)

**Changes in v0.5.2:**

- `Send` is now a marker trait; removed `clone_for_send` method (SA-2)
- Added Send/Frozen automatic derivation rules for user-defined types (TM-4)
- Strengthened §4.3 with normative two-level scheduling model for intra-actor tasks (TM-5)
- Documented `self` parameter handling in trait/impl methods; added SelfParam to FnSig grammar (SA-7)

**Changes in v0.5.1:**

- Standardized actor spawn syntax on parenthesized form: `spawn Counter(0)` (SA-1)
- Distinguished type field syntax (semicolons, no prefix) from actor field syntax (semicolons, let/var prefix) (SA-4)
- Extended MailboxDecl grammar with overflow policy syntax (SA-8)
- Added normative 4-parameter dispatch signature to §9.1 (TM-1)
- Expanded coalesce overflow policy with full semantics: key function, matching rules, fallback policy (TM-2)
- Distinguished actor state machine (§9.1) from task state machine (§4.1) with normative definitions (TM-3)

**Changes in v0.5.0:**

- Replaced `.send()` / `.ask()` messaging with direct method calls on named actors (Section 2.1.1)
- Added `<-` send operator for lambda actors and explicit message sending (Section 2.1.1, 2.1.2)
- `receive fn` without return type = fire-and-forget (no await needed)
- `receive fn` with return type = request-response (requires `await`)
- Added `select`, `race`, `join` concurrency expressions (Section 4.10)
- Added `after` keyword for timeouts in select/race arms
- Added `| after` timeout combinator for individual await expressions
- Updated EBNF with SendExpr, SelectExpr, RaceExpr, JoinExpr, TimeoutExpr (Section 10)

**Changes in v0.4.1:**

- Fixed move vs deep-copy contradiction: `send()` moves at language level, deep-copies at runtime level (Sections 3.4.4, 3.7.2)
- Removed redundant `isolated` keyword from examples (Section 3.4.6)
- Clarified `await` traps on cancellation; use `try` for graceful handling (Section 4.4)
- Documented await-point atomicity and state safety (Section 4.8)
- Added actor reference cycle leak strategy with `Weak<ActorRef>` (Section 3.7.8)
- Added arena `Drop` performance cliff warning (Section 3.7.6)
- Unified FFI keyword to `foreign` everywhere (Sections 3.9, 8.6, 10 EBNF)

**Changes in v0.3:**

- Added generics and monomorphization section (Section 3.8)
- Expanded memory management with per-actor ownership, allocators, and arenas (Section 3.7)
- Added FFI section with foreign functions and C interop (Section 3.9)
- Added standard library architecture section (Section 3.10)
- Added self-hosting roadmap section (Section 11)
- Updated EBNF with foreign fn, where clauses, and dyn Trait syntax

**Changes in v0.2:**

- Added module system (Section 3.5)
- Added trait system with marker traits (Section 3.6)
- Added memory management model (Section 3.7)
- Added closure syntax to EBNF
- Clarified actor message protocol with `receive fn`
- Enhanced async/await semantics with structured concurrency
- Added `?` operator for error propagation
- Expanded pattern matching
- Updated compilation and runtime models based on research

---

## 1. Design goals

### 1.1 Non-goals (v0.x)

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

Actors expose message handlers using `receive fn`. Named actor `receive fn` methods are callable directly — no `.send()` or `.ask()` required:

```hew
actor Counter {
    var count: i32 = 0;

    // Fire-and-forget: no return type, caller does not await
    receive fn increment(n: i32) {
        self.count += n;
    }

    // Request-response: has return type, caller must await
    receive fn get() -> i32 {
        self.count
    }

    // Internal method - not accessible to other actors
    fn validate(n: i32) -> bool {
        n >= 0
    }
}
```

- `receive fn` declares a message handler (entry point for actor messages)
- `fn` declares a private internal method
- **`receive fn` without return type** → fire-and-forget. The method call returns `()` and the caller does not need `await`. The message is enqueued and the caller continues immediately.
- **`receive fn` with return type** → request-response. The method call returns `Task<R>` and the caller must `await` the result.

**Calling named actors:**

```hew
let counter = spawn Counter(0);

// Fire-and-forget: no return type, no await needed
counter.increment(10);

// Request-response: has return type, requires await
let n = await counter.get();
```

**The `<-` operator (explicit send):**

For lambda actors and explicit message sending, the `<-` operator provides a concise send syntax:

```hew
// Lambda actor send
let worker = spawn (msg: i32) => { println(msg * 2); };
worker <- 42;                    // fire-and-forget

// The <- operator enqueues the message (fire-and-forget)
```

The `<-` operator is syntactic sugar for enqueueing a message and returning `()`. It is the primary way to send messages to lambda actors. For named actors, direct method calls are preferred.

**Actor instantiation:**

Actors are instantiated using the `spawn` keyword with constructor arguments matching the actor's `init` block parameters:

```hew
// Spawn with init arguments
let counter = spawn Counter(0);

// Spawn with no arguments (if actor has no-arg init or no init block)
let worker = spawn WorkerActor();
```

> **Note:** Named actor spawn always uses parenthesized arguments, even when empty. This is distinct from lambda actor spawn, which uses `spawn (params) => body` syntax.

Actor behaviors can also be defined via traits:

```hew
trait Pingable {
    receive fn ping() -> String;
}

actor Pinger: Pingable {
    receive fn ping() -> String {
        "pong"
    }
}
```

### 2.1.2 Lambda Actors

Lambda actors provide lightweight, inline actor definitions using lambda syntax:

```hew
// Basic lambda actor
let worker = spawn (msg: i32) => {
    println(msg * 2);
};

// With return type for request-response
let calc = spawn (x: i32) -> i32 => { x * x };

// With state capture (move semantics)
let factor = 2;
let multiplier = spawn move (x: i32) -> i32 => {
    x * factor
};
```

**Syntax:**

```ebnf
Spawn          = "spawn" ( LambdaActor | ActorSpawn ) ;
LambdaActor    = "move"? "(" LambdaParams? ")" RetType? "=>" (Expr | Block) ;
ActorSpawn     = Ident TypeArgs? "(" FieldInitList? ")" ;  (* Named fields: spawn Counter(count: 0) *)
```

**Type system:**

Lambda actors return `ActorRef<Actor<M>>` for fire-and-forget or `ActorRef<Actor<M, R>>` for request-response, where:

- `M` is the message type (from parameter)
- `R` is the return type (from annotation or inference)

**Spawning returns ActorRef:**

```hew
// Named actor spawn returns ActorRef<ActorType>
let counter: ActorRef<Counter> = spawn Counter(0);

// Lambda actor spawn returns ActorRef<Actor<M>> or ActorRef<Actor<M, R>>
let worker: ActorRef<Actor<i32>> = spawn (msg: i32) => { println(msg); };
let calc: ActorRef<Actor<i32, i32>> = spawn (x: i32) -> i32 => { x * x };
```

**Capture semantics:**

- Variables from enclosing scope can be captured
- Captured values must implement the `Send` trait
- Use `move` keyword to transfer ownership of captures
- Without `move`, copyable values are copied, non-copyable values cause an error

**Operations:**

```hew
// Fire-and-forget send (for ActorRef<Actor<M>>)
worker <- 42;

// Request-response (for ActorRef<Actor<M, R>>)
let result = await calc <- 5;
```

**Integration with scope (normative):**

Lambda actors spawned within a `scope` block have their **lifetime** managed by that scope, but are NOT integrated with the scope's task cancellation or trap propagation:

```hew
scope {
    let worker = spawn (x: i32) => { ... };
    worker <- 1;
}  // worker stopped when scope exits
```

Specifically:

- When a scope exits, all lambda actors spawned within it are sent a stop signal (equivalent to `actor_stop`)
- `s.cancel()` does NOT cancel lambda actors — it only cancels tasks spawned via `s.launch`
- A trap within a lambda actor does NOT propagate to sibling tasks or the enclosing scope — the actor fails independently
- For failure propagation across actors, use supervision trees (Section 5), not structured concurrency scopes

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

### 2.2.1 Error Propagation

The `?` operator propagates errors from Result types:

```hew
fn read_file(path: String) -> Result<String, IoError> {
    let handle = open(path)?;  // Early return on error
    let content = read(handle)?;
    Ok(content)
}
```

When a `receive fn` message handler returns `Err`, the error is:

1. Logged to the actor's supervision context
2. Returned to the caller (if request-response pattern)
3. May cause trap if unhandled and configured to do so

---

## 3. Types, ownership, and sendability

### 3.1 Type categories

- **Value types** (copy): integers, floats, bool, char, small fixed aggregates.
- **Owned heap types**: `String`, `Bytes`, `Vec<T>`, `HashMap<K,V>`, user-defined types.
- **Shared immutable types**: `Arc<T>` where `T: Frozen`.
- **Actor references**: `ActorRef<A>` is sendable.
- **I/O stream types**: `Stream<T>` (readable) and `Sink<T>` (writable) — move-only, `Send`, first-class sequential I/O handles (§6.5).

### 3.2 Mutability

- Bindings are immutable by default: `let`.
- Mutable bindings: `var`.
- Struct fields are immutable by default; mutation requires `var` field.

### 3.2.1 Pure Functions

A function can be marked `pure` to guarantee it is free of observable side effects.
The compiler statically verifies purity at type-check time.

```hew
pure fn add(a: i32, b: i32) -> i32 {
    a + b
}
```

**Purity rules.** Inside a `pure fn` or `pure receive fn`:

1. Only other `pure` functions may be called.
2. The `spawn`, `scope`, `select`, `join` expressions are forbidden.
3. Assignment to actor state (`self.field = …`) is forbidden.
4. Local `var` mutations _are_ allowed — they do not escape the function.

`pure` may appear before `fn`, `gen fn`, `receive fn`, or `receive gen fn`,
and in trait method signatures:

```hew
trait Math {
    pure fn square(self: Point) -> f64;
}

actor Calculator {
    pure receive fn add(a: i32, b: i32) -> i32 {
        a + b
    }
}
```

Pure receive handlers are guaranteed not to mutate the actor's state, which
makes it clear which handlers are stateless message transformations.

### 3.3 Sendability / isolation rule

A value may cross an actor boundary only if it satisfies **Send**.

`Send` is satisfied if one of the following holds:

- the value is a value type (integers, floats, bool, char), or
- the value is **owned** and transferred (move) with no remaining aliases, or
- the value is `Frozen` (deeply immutable), or
- the value is an actor reference (`ActorRef<A>`)

This is the central compile-time guarantee: **no data races without locks**, aligning with capability-based actor safety in Pony. ([tutorial.ponylang.io][1])

#### 3.3.1 Automatic Derivation Rules

The compiler automatically determines `Send` and `Frozen` for user-defined types. Users do NOT manually implement these traits.

**Send derivation:**

| Type                               | `Send` if...                               |
| ---------------------------------- | ------------------------------------------ |
| Value types (i32, f64, bool, char) | Always `Send`                              |
| `String`                           | Always `Send` (owned, deep-copied on send) |
| `ActorRef<A>`                      | Always `Send`                              |
| `type S { f1: T1; f2: T2; ... }`   | All fields are `Send`                      |
| `enum E { V1(T1), V2(T2), ... }`   | All variant payloads are `Send`            |
| `Vec<T>`                           | `T` is `Send`                              |
| `HashMap<K, V>`                    | `K` and `V` are both `Send`                |
| `Option<T>`                        | `T` is `Send`                              |
| `Result<T, E>`                     | `T` and `E` are both `Send`                |
| `(T1, T2, ...)`                    | All elements are `Send`                    |
| `[T; N]`                           | `T` is `Send`                              |

**Frozen derivation:**

| Type                          | `Frozen` if...                            |
| ----------------------------- | ----------------------------------------- |
| Value types                   | Always `Frozen`                           |
| `String`                      | NOT `Frozen` (mutable content)            |
| `ActorRef<A>`                 | Always `Frozen` (identity reference only) |
| `type S` with NO `var` fields | All fields are `Frozen`                   |
| `type S` with ANY `var` field | NOT `Frozen`                              |
| `enum E`                      | All variant payloads are `Frozen`         |
| `Arc<T>`                      | `T` is `Frozen`                           |
| `Vec<T>`, `HashMap<K,V>`      | NOT `Frozen` (mutable containers)         |

> **Soundness requirement:** The compiler MUST reject as non-`Send` any type whose `Send` status cannot be determined (e.g., opaque foreign types). Foreign types are non-`Send` by default; use `#[send]` attribute to override for types known to be safe.

### 3.3.2 The `bytes` Type

`bytes` is a standard library type (not a language primitive): a mutable, heap-allocated byte buffer — semantically a `Vec<u8>` — but with a dedicated type name and u8-typed API:

```hew
let buf: bytes = bytes::new();
buf.push(0x48);    // push a byte value (0–255)
buf.push(72);      // same as 'H' in ASCII
let n = buf.len(); // i32
let b = buf.get(0); // u8 — first byte
buf.set(1, 0xFF);   // overwrite byte at index 1
let last = buf.pop(); // u8 — removes and returns last byte
println(buf.is_empty()); // bool
println(buf.contains(72)); // bool — linear scan
```

**Methods on `bytes`:**

| Method         | Signature         | Description                     |
| -------------- | ----------------- | ------------------------------- |
| `bytes::new()` | `() -> bytes`     | Create an empty byte buffer     |
| `.push(b)`     | `(u8) -> ()`      | Append a byte                   |
| `.pop()`       | `() -> u8`        | Remove and return the last byte |
| `.get(i)`      | `(i32) -> u8`     | Get the byte at index `i`       |
| `.set(i, b)`   | `(i32, u8) -> ()` | Overwrite the byte at index `i` |
| `.len()`       | `() -> i32`       | Number of bytes                 |
| `.is_empty()`  | `() -> bool`      | True if len is 0                |
| `.contains(b)` | `(u8) -> bool`    | True if the buffer contains `b` |

`bytes` is an owned heap type and follows the same ownership rules as `Vec<T>` — it is automatically freed when it goes out of scope. It satisfies `Send` (deep-copied across actor boundaries).

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

For type (struct) fields:

```hew
type Point {
    x: i32;      // immutable field
    y: i32;      // immutable field
}

var p = Point { x: 0, y: 0 };
// p.x = 10;     // compile error - fields are immutable by default
```

**Type (struct) field syntax:**

Type fields do NOT require a `let`/`var` prefix. Fields are immutable and use semicolons as terminators:

```hew
type Point {
    x: f64;          // field declaration
    y: f64;          // field declaration
    label: String;   // field declaration
}
```

**Actor field syntax:**

Actor fields require a `let` or `var` prefix and use semicolons as terminators:

```hew
actor Counter {
    var count: i32 = 0;     // mutable field with default
    let name: String;        // immutable field, set by init
}
```

This distinction exists because actor fields are stateful (they change over the actor's lifetime) and use initialization syntax similar to variable declarations, while struct fields are data layout declarations.

#### 3.4.4 The Boundary Rule: Move on Send

The **only** ownership constraint is at actor boundaries. When a value crosses an actor boundary (via method call, `<-`, or `spawn`), it must be **moved** or **cloned**:

```hew
receive fn forward(message: Message, target: ActorRef<Handler>) {
    target.handle(message);  // message is MOVED to target's mailbox
    // message is now invalid - compile error if used
}

// Or for lambda actors:
worker <- message;           // message is MOVED via <- operator
```

> **Note:** Throughout this specification, "sending a message" refers to invoking a `receive fn` method on an actor (for named actors) or using the `<-` operator (for lambda actors). The internal runtime function `.send()` is an implementation detail not exposed in Hew syntax.

> **Send semantics (language vs runtime):**
>
> - **Language level**: A method call or `<-` moves the value — the sender can no longer use it
> - **Runtime level**: The value is deep-copied into the receiver's per-actor heap
> - This gives the safety of Rust's move semantics with the simplicity of Erlang's copy-on-send

**Why move semantics?**

- The receiving actor may be on a different thread (in the runtime)
- Two actors cannot share mutable state (this is the core safety guarantee)
- Move ensures the sender relinquishes the value

**Cloning for continued use:**

```hew
receive fn broadcast(message: Message, targets: Vec<ActorRef<Handler>>) {
    for target in targets {
        target.handle(message.clone());  // Each recipient gets a clone
    }
    // message still valid - we only sent clones
}

// Or for lambda actors:
for target in targets {
    target <- message.clone();
}
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
// let worker = spawn (msg: Msg) => { use(config); };

// With move: config is moved into the actor
let worker = spawn move (msg: Msg) => {
    use(config);   // ok - config now owned by this actor
};
// config invalid here - it was moved

// Alternative: clone first
let config2 = config.clone();
let worker2 = spawn move (msg: Msg) => {
    use(config2);
};
```

**Non-Send values cannot be captured:**

```hew
let local_ref = get_local_resource();  // returns a non-Send reference

// Compile error: local_ref does not implement Send
// let worker = spawn move (msg: Msg) => { use(local_ref); };
```

This is enforced at compile time: any captured value in a `spawn` expression must satisfy the `Send` trait.

#### 3.4.6 What IS Allowed (Within an Actor)

```hew
actor Example {
    var data: Vec<i32> = Vec::new();

    receive fn demo() {
        // Multiple mutable references - ALLOWED (single-threaded)
        let ref1 = self.data;
        let ref2 = self.data;
        ref1.push(1);
        ref2.push(2);     // ok - no data race possible

        // Aliasing mutable data - ALLOWED
        var x = self.data;
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
    var data: Vec<i32> = Vec::new();

    receive fn bad_examples(other: ActorRef<Other>) {
        // Sending without move - ERROR (implicit move makes source invalid)
        other.handle(self.data);
        self.data.push(1);     // compile error: data was moved

        // Using value after send - ERROR
        let msg = Message::new();
        other.handle(msg);
        println(msg.content);  // compile error: msg was moved

        // Capturing non-Send value - ERROR
        let local_handle: RawPointer = get_handle();
        let worker = spawn move (x: i32) => {
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
    address: String;       // public fields via pub keyword on type
    internal_state: i32;   // fields are named, terminated with semicolons
}

pub fn connect(addr: String) -> Result<Connection, Error> {
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
import std::text::regex;   // Available as "regex"

// Call module functions with dot-syntax: module.function(args)
let server = http.listen("0.0.0.0:8080");   // Uses short name "http"
let content = fs.read("config.toml");
let exists = fs.exists("output.txt");       // Returns bool
let re = regex.new("[a-z]+");
let matched = regex.is_match(re, input);    // Returns bool
```

This provides clean, namespaced access to stdlib functionality. The module name acts as a qualifier, avoiding verbose function names like `hew_http_server_new()`.

| Module             | Example functions                                                                                                                                            |
| ------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `std::net::http`   | `http.listen`, `http.accept`, `http.path`, `http.method`, `http.body`, `http.header`, `http.respond`, `http.respond_text`, `http.respond_json`, `http.close` |
| `std::fs`          | `fs.read`, `fs.write`, `fs.append`, `fs.exists`, `fs.delete`, `fs.size`, `fs.read_line`                                                                      |
| `std::os`          | `os.args_count`, `os.args`, `os.env`, `os.set_env`, `os.has_env`, `os.cwd`, `os.home_dir`, `os.hostname`, `os.pid`                                           |
| `std::net`         | `net.listen`, `net.accept`, `net.connect`, `net.read`, `net.write`, `net.close`                                                                              |
| `std::text::regex` | `regex.new`, `regex.is_match`, `regex.find`, `regex.replace`                                                                                                 |
| `std::net::mime`   | `mime.from_path`, `mime.from_ext`, `mime.is_text`                                                                                                            |
| `std::process`     | `process.run`, `process.spawn`, `process.wait`, `process.kill`                                                                                               |

Predicate functions (`fs.exists`, `regex.is_match`, `os.has_env`, `mime.is_text`) return `bool`.

**Visibility modifiers:**

- `pub` - public to all modules
- `pub(package)` - public within the same package
- `pub(super)` - public to parent module only
- (no modifier) - private to this module

---

### 3.6 Trait System

Traits define shared behavior that types can implement. Hew has built-in marker traits and supports user-defined traits.

**Trait declaration:**

```hew
trait Display {
    fn display(self) -> String;
}

trait Iterator {
    type Item;
    fn next(self) -> Option<Self::Item>;
}

trait Clone {
    fn clone(self) -> Self;
}
```

**Trait implementation:**

```hew
type Point { x: f64; y: f64 }

impl Display for Point {
    fn display(self) -> String {
        f"({self.x}, {self.y})"
    }
}
```

**Built-in marker traits:**

- `Send` - Type can safely cross actor boundaries. Satisfied by:
  - Value types (integers, floats, bool, char)
  - Owned types transferred by move
  - `Frozen` types (deeply immutable)
  - `ActorRef<A>`

- `Frozen` - Type is deeply immutable and thus safely shareable. Implies `Send`.
  - `Arc<T>` where `T: Frozen`
  - Structs where all fields are `Frozen`

- `Copy` - Type is copied on assignment rather than moved.
  - Only value types (integers, floats, bool, char)
  - Small fixed-size aggregates

**The `self` parameter:**

In trait declarations and `impl` blocks, the first parameter of a method MAY be `self`, which refers to the receiver value:

```hew
trait Display {
    fn display(self) -> String;       // self by value (consumes)
}

impl Display for Point {
    fn display(self) -> String {      // self: Point (inferred)
        f"({self.x}, {self.y})"
    }
}
```

`self` is syntactic sugar — the compiler treats it as a parameter with the implementing type. However, `self` has **two distinct semantics** depending on context:

**1. In `impl` blocks for structs/enums (by-value):**

`self` is passed by value — the receiver is consumed (ownership transfer):

- `fn display(self)` is equivalent to `fn display(self: Point)` in an `impl` for `Point`
- The caller gives up ownership of the value
- After calling a consuming method, the original binding is no longer usable

```hew
let p = Point { x: 1.0, y: 2.0 };
p.display();    // desugars to Point::display(p) — p is consumed
// p is no longer valid here
```

**2. In actor `receive fn` and `fn` methods (by-mutable-reference):**

`self` is an implicit mutable reference to the actor's persistent state — the actor is NOT consumed:

- `fn modify(self)` in a `receive fn` means the method can read and mutate the actor's fields
- The actor persists after the method returns; it continues processing messages
- `self` is always available in actor methods without explicit declaration

```hew
actor Counter {
    var count: i32 = 0;
    receive fn increment() {
        self.count += 1;  // self refers to actor state, actor persists
    }
}
```

There is no `&self` or `&mut self` syntax — Hew does not have references in its surface syntax (see §3.4.1). The by-value vs by-reference distinction is determined by the context (struct `impl` vs actor body), not by annotation.

**Trait bounds on generics:**

```hew
fn broadcast<T: Send>(message: T, recipients: Vec<ActorRef<Receiver>>) {
    for recipient in recipients {
        recipient.handle(message.clone());
    }
}

fn share<T: Frozen>(data: Arc<T>) -> Arc<T> {
    data  // Safe to share without copying
}
```

**Trait objects:**

```hew
fn log_anything(item: dyn Display) {
    print(item.display());
}
```

---

### 3.7 Memory Management

Hew uses **per-actor ownership** with RAII-style deterministic destruction. There is **no garbage collector** — no tracing GC, no generational GC, no GC pauses. Memory is managed through ownership, scopes, and reference counting.

#### 3.7.1 Ownership Model

**Principle 1: Actors own their heaps.**
Each actor has a private heap. No memory is shared between actors. When an actor terminates, its entire heap is freed in one operation.

**Principle 2: RAII within actors.**
Within an actor, values follow Rust-style ownership semantics:

- Each value has exactly one owner
- When the owner goes out of scope, the value is dropped
- Destructors (`Drop` trait) run deterministically at scope exit

```hew
fn example() {
    let file = File::open("data.txt");  // file owns the handle
    process(file);
}  // file.drop() runs here, closing the handle
```

**Principle 3: No garbage collection.**
Hew guarantees no GC pauses. Memory reclamation is entirely deterministic:

- Scope exit triggers drops
- Reference count reaching zero triggers drops
- Actor termination frees the actor's heap

#### 3.7.2 Message Passing Semantics

At the **language level**, `send()` **moves** the value — the sender loses access and cannot use it after sending (see §3.4.4). At the **runtime level**, the value is **deep-copied** into the receiver's per-actor heap, since actors may reside on different threads with separate heaps.

This hybrid gives the safety of Rust's move semantics (no use-after-send bugs) with the simplicity of Erlang's copy-on-send (no shared memory between actors).

> **Why not just "move" the bytes?** Actors have independent heaps. A pointer from one actor's heap is meaningless in another. The runtime deep-copies the value into the receiver's heap, then the sender's copy is dropped. From the programmer's perspective, this is indistinguishable from a move.

**Move-on-send:**

- When a message is sent to an actor (via method call or `<-`), the value is moved at the language level — the sender can no longer use it. At runtime, the value is deep-copied into the receiver's per-actor heap.
- The receiver owns an independent copy
- No references cross actor boundaries

```hew
receive fn forward(message: Message, target: ActorRef<Handler>) {
    target.handle(message);  // message is MOVED — runtime deep-copies to target's heap
    // message is now invalid — compile error if used
}
```

**The `Send` trait:**

`Send` is a **marker trait** — it has no methods. A type is `Send` if it can safely cross actor boundaries. The compiler verifies `Send` bounds at compile time.

```hew
trait Send {}  // Marker trait — no methods
```

`Send` is satisfied if one of the following holds (see §3.3):

- The value is a value type (integers, floats, bool, char)
- The value is owned and transferred by move with no remaining aliases
- The value is `Frozen` (deeply immutable)
- The value is an `ActorRef<A>`
- The value is a struct/enum where all fields/variants satisfy `Send`

> **Implementation note:** At runtime, messages are deep-copied into the receiver's per-actor heap. This deep-copy is performed by the runtime, not by a user-visible method. The `Send` marker trait tells the compiler that a type's structure permits this deep-copy. User-defined types do NOT need to implement `Send` explicitly — the compiler derives it automatically based on field types.

**Move semantics within actors:**
Within a single actor, values can be moved (ownership transferred) without copying:

```hew
fn process(data: Vec<u8>) {
    let owned = data;  // move, not copy
    // data is no longer valid
}
```

#### 3.7.3 Deterministic Destruction (Drop)

The `Drop` trait enables RAII-style resource cleanup:

```hew
trait Drop {
    fn drop(self);
}

type FileHandle {
    fd: i32;
}

impl Drop for FileHandle {
    fn drop(self) {
        close(self.fd);
    }
}
```

**Drop guarantees:**

- `drop()` runs exactly once per value
- `drop()` runs at a predictable point (scope exit or explicit drop)
- Drop order: fields are dropped in declaration order
- Nested drops: outer struct drops, then each field drops

**When drop runs:**
| Situation | Drop behavior |
|-----------|---------------|
| Variable goes out of scope | `drop()` called immediately |
| Value is moved | No drop at original location |
| `Rc<T>` refcount reaches zero | `drop()` called on inner `T` |
| Actor terminates | All owned values dropped, then heap freed |

#### 3.7.4 Reference Counting (Rc and Arc)

**`Rc<T>` — single-actor reference counting:**

- Non-atomic refcount (fast, single-threaded)
- Cannot cross actor boundaries (does not implement `Send`)
- Use for shared ownership within one actor

```hew
let data: Rc<LargeStruct> = Rc::new(expensive_computation());
let alias = data.clone();  // refcount++, no data copy
// data and alias share the same LargeStruct
```

**`Arc<T>` — atomic reference counting:**

- Uses atomic operations for refcount (thread-safe, has synchronization cost)
- Requires `T: Frozen` (deeply immutable)
- **Does implement `Send`** — can be shared across actors

```hew
let config: Arc<Config> = Arc::new(load_config());
worker1.configure(config.clone());  // Arc cloned (atomic refcount++)
worker2.configure(config.clone());  // Both workers share same Config
```

**Arc cost transparency:**

- Each `clone()` performs an atomic increment
- Each drop performs an atomic decrement
- When refcount reaches zero, `T` is dropped and memory freed
- This is cheaper than deep-copying large immutable data, but not free

**When to use which:**
| Type | Cross-actor? | Refcount cost | Use case |
|------|--------------|---------------|----------|
| Owned `T` | Copied on send | None | Default, small data |
| `Rc<T>` | No | Non-atomic | Shared within actor |
| `Arc<T>` | Yes (if `T: Frozen`) | Atomic | Large immutable shared data |

#### 3.7.5 Allocator Interface

Hew provides an explicit allocator interface for fine-grained memory control:

```hew
trait Allocator {
    fn alloc(self, size: usize, align: usize) -> Result<*var u8, AllocError>;
    fn dealloc(self, ptr: *var u8, size: usize, align: usize);
    fn realloc(self, ptr: *var u8, old_size: usize, new_size: usize, align: usize)
        -> Result<*var u8, AllocError>;
}
```

**Standard allocators:**

- `GlobalAllocator` — Default system allocator
- `ArenaAllocator` — Bump allocation with bulk deallocation
- `PoolAllocator` — Fixed-size object pools

**Collections accept allocators:**

```hew
let items = Vec::new();              // uses default allocator
let temp = Vec::new_in(arena);       // uses provided arena
```

#### 3.7.6 Compiler Optimizations (Implementation Details)

The compiler may apply memory optimizations that are **invisible to user semantics**. Users always see RAII behavior; optimizations affect only performance.

**Arena optimization for message handlers:**
The compiler may allocate message handler temporaries in an arena and bulk-free them when the handler returns. This is an optimization, not a semantic change:

- User code behaves as if each value is individually dropped
- `Drop::drop()` is still called for types that implement `Drop`
- The arena optimization applies only to types without custom `Drop`

> ⚠️ **Performance cliff: Adding `Drop` disables arena optimization.**
> Types with a `Drop` implementation have their destructors called individually instead of benefiting from arena bulk-free. This means adding `Drop` for debugging purposes (e.g., logging on destruction) can significantly impact performance in hot message handlers. Consider using explicit cleanup functions instead of `Drop` when arena performance matters.

**Copy elision:**
When sending messages, the compiler may optimize away copies in cases where the sender provably does not use the value after send. This is semantically equivalent to copy-then-drop-original.

**Escape analysis:**
The compiler may stack-allocate values that do not escape their scope, avoiding heap allocation entirely.

**Important:** These optimizations do not change program behavior. A correct program produces identical results with or without optimizations.

#### 3.7.7 Memory Safety Guarantees

| Guarantee         | How Hew ensures it                                             |
| ----------------- | -------------------------------------------------------------- |
| No use-after-free | Ownership + move semantics; compiler rejects use after move    |
| No double-free    | Single ownership; `drop()` runs exactly once                   |
| No data races     | No shared mutable state; `Send` requires `Frozen` for sharing  |
| No GC pauses      | No tracing GC; deterministic refcounting and scope-based drops |
| No memory leaks\* | RAII ensures cleanup; cycles in `Rc` can leak (use weak refs)  |

\*Reference cycles in `Rc<T>` can cause leaks. Use `Weak<T>` to break cycles.

#### 3.7.8 Actor Reference Cycles

Actor references (`ActorRef<A>`) use reference counting. This means cycles between actors can cause leaks:

```hew
// ⚠️ LEAK: A holds ref to B, B holds ref to A — neither can be freed
actor A {
    var peer: ActorRef<B>;
}
actor B {
    var peer: ActorRef<A>;
}
```

**Mitigation: Use `Weak<ActorRef<A>>` for back-references:**

```hew
actor B {
    var parent: Weak<ActorRef<A>>;  // weak ref — does not prevent A from being freed

    receive fn notify_parent() {
        if let Some(parent) = self.parent.upgrade() {
            parent.notify(Notification);
        }
    }
}
```

**Supervision trees naturally avoid cycles:** Parent supervisors hold strong `ActorRef` to children, but children do not hold ownership references back to parents. If a child needs to communicate with its parent, it should use a `Weak<ActorRef>` or an explicit message protocol.

#### 3.7.8 Defer Statements

The `defer` statement schedules an expression to execute when the enclosing function returns, regardless of the return path (normal exit or early `return`). Deferred expressions execute in **LIFO** (last-in, first-out) order — the most recently deferred expression runs first.

```hew
fn example() {
    defer println("cleanup");
    println("work");
}
// Output:
//   work
//   cleanup
```

**Semantics:**

1. **Function-scoped.** Deferred expressions are bound to the enclosing function, not to the enclosing block.
2. **LIFO execution order.** Multiple `defer` statements in the same function execute in reverse order of registration:

```hew
fn multi_defer() {
    defer println("third");
    defer println("second");
    defer println("first");
}
// Output:
//   first
//   second
//   third
```

3. **Runs before return.** Deferred expressions execute before the function returns, including on early `return` paths:

```hew
fn early_return() -> i32 {
    defer println("cleanup");
    if condition {
        return 42;  // "cleanup" prints before returning 42
    }
    return 0;       // "cleanup" prints before returning 0
}
```

4. **Interaction with drops.** Deferred expressions execute before RAII drop calls at function exit.
5. **Expression argument.** The `defer` keyword takes a single expression (typically a function call).

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
max("hello", "world"); // max$String
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

For cases where code size matters more than performance, Hew provides explicit type erasure via `dyn Trait`:

```hew
// Monomorphized (default) - static dispatch
fn render_static<T: Display>(item: T) {
    print(item.display());
}

// Type-erased (explicit) - dynamic dispatch via vtable
fn render_dynamic(item: dyn Display) {
    print(item.display());
}
```

**`dyn` implementation:**

- Fat pointer: (data pointer, vtable pointer)
- Vtable generated per-trait, per-concrete-type
- Object-safe traits only (no `Self` in return position, no generic methods)

**Object safety rules:**
A trait is object-safe if:

- All methods have `self` as the receiver
- No methods return `Self`
- No methods have generic type parameters
- No associated functions (only methods)

#### 3.8.3 Trait Bounds

**Inline bounds:**

```hew
fn broadcast<T: Send + Clone>(message: T, targets: Vec<ActorRef<Receiver>>) {
    for target in targets {
        target.handle(message.clone());
    }
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

```hew
fn process<C: Container>(c: C)
where
    C::Item: Display + Send,
{
    for item in c.items() {
        print(item.display());
    }
}
```

#### 3.8.4 Associated Types in Traits

Traits can declare associated types that implementors must specify:

```hew
trait Iterator {
    type Item;
    fn next(self) -> Option<Self::Item>;
}

trait Container {
    type Item;
    type Iter: Iterator[Item = Self::Item];

    fn iter(self) -> Self::Iter;
    fn len(self) -> usize;
}

impl Iterator for RangeIter {
    type Item = i32;

    fn next(self) -> Option<i32> {
        if self.current < self.end {
            let value = self.current;
            self.current = self.current + 1;
            Some(value)
        } else {
            None
        }
    }
}
```

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

// Arc<T> requires T: Frozen, and is always Send + Frozen
impl<T: Frozen> Send for Arc<T> {}
impl<T: Frozen> Frozen for Arc<T> {}
```

**Actor boundary enforcement:**

```hew
// Error: T might not be Send
receive fn forward_unsafe<T>(message: T, target: ActorRef<Handler<T>>) {
    target <- message;     // Compile error: T not bounded by Send
}

// Correct: T is bounded by Send
receive fn forward<T: Send>(message: T, target: ActorRef<Handler<T>>) {
    target <- message;     // OK: T: Send verified at instantiation
}
```

#### 3.8.6 Type Inference

Hew employs **bidirectional type inference** to minimize explicit type annotations while maintaining compile-time type safety. Types flow from calling contexts into lambda expressions, making generic code elegant and natural to write.

**Core principle**: Hew remains **strongly typed** — all types are known at compile time. Inference simply reduces the annotation burden without sacrificing safety.

**Bidirectional inference strategy:**

- **Context flows inward**: Function signatures and explicit annotations provide typing context
- **Lambda parameters infer from context**: When a lambda appears where a specific function type is expected, parameter types are inferred
- **Return type inference with `-> _`**: A function annotated with `-> _` has its return type inferred from the body's trailing expression
- **Explicit annotations when ambiguous**: If types cannot be inferred, the compiler requires explicit annotations

**Return type inference:**

The `-> _` annotation requests that the compiler infer the return type from the function body. This is distinct from omitting `->` entirely, which means the function returns void (unit):

```hew
fn add(a: i32, b: i32) -> _ { a + b }  // inferred: -> i32
fn greet(name: string) -> _ { "hello {name}" }  // inferred: -> string
fn noop() { }  // no -> at all: returns void
```

`-> _` requires a function body — it cannot be used on `extern fn` declarations or bodyless trait methods.

**Lambda inference examples:**

```hew
fn apply(f: fn(i32, i32) -> i32, a: i32, b: i32) -> i32 { f(a, b) }

// Lambda parameters infer i32 from apply's signature
let sum = apply((x, y) => x + y, 3, 4);      // x: i32, y: i32 inferred
let product = apply((x, y) => x * y, 3, 4);  // types flow from apply's signature

// Method chaining with inference
numbers
    .filter((x) => x > 0)           // x: i32 inferred from Vec<i32>
    .map((x) => x * 2)              // x: i32, result: i32
    .reduce((a, b) => a + b)        // a: i32, b: i32 from reduce signature
```

**Lambda syntax:**

Hew uses arrow syntax for all lambda expressions:

```hew
let doubled = transform((x) => x * 2, 21);
let sum = numbers.reduce((a, b) => a + b);
```

**Untyped parameters when context provides types:**

```hew
fn map<T, U>(items: Vec<T>, transform: fn(T) -> U) -> Vec<U> { /* ... */ }

// T=i32, U=String inferred from usage
let strings = map(vec![1, 2, 3], (x) => x.to_string());  // x: i32 inferred
```

**Actor message type inference:**

Actor message handlers provide rich typing context:

```hew
actor Calculator {
    var result: i32 = 0;

    // receive fn signature provides context for message arguments
    receive fn apply_operation(op: fn(i32, i32) -> i32, value: i32) {
        self.result = op(self.result, value);
    }
}

let calc = spawn Calculator();
// Lambda types inferred from receive fn signature
calc.apply_operation((a, b) => a + b, 10);  // a: i32, b: i32 inferred
calc.apply_operation((a, b) => a * b, 5);   // also inferred
```

**Generic lambda constraints:**

For standalone generic lambdas, explicit bounds are required:

```hew
// Generic lambda requires explicit type parameters and bounds
let generic_add = <T: Add>(x: T, y: T) => x + y;

// Can then be called with different types
generic_add(1, 2);        // i32
generic_add(1.0, 2.0);    // f64
```

**Ambiguous cases require annotations:**

```hew
// ERROR: Cannot infer types for lambda parameters
let f = (x, y) => x + y;  // No context to determine x, y types

// Solution 1: Annotate the variable
let f: fn(i32, i32) -> i32 = (x, y) => x + y;

// Solution 2: Annotate parameters
let f = (x: i32, y: i32) => x + y;
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
// - i32: Send ✓, Clone ✓, Display ✓
let results = process(vec![1, 2, 3], (x) => {
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
5  |     let f = (x, y) => x + y;
   |               ^^^^^^^^^^^^^ cannot infer types for `x` and `y`
   |
help: consider annotating the lambda variable type
   |
5  |     let f: fn(i32, i32) -> i32 = (x, y) => x + y;
   |            ++++++++++++++++++
   |
help: or annotate the lambda parameters directly
   |
5  |     let f = (x: i32, y: i32) => x + y;
   |                +++     +++
```

**Practical elegance**: This system achieves the design goal of "elegant simplicity" — minimal annotations paired with maximum type safety. Types propagate naturally from function signatures and calling contexts, while the monomorphization backend generates specialized, optimized code for each concrete instantiation.

---

### 3.9 Foreign Function Interface (FFI)

Hew provides FFI capabilities for interoperating with C libraries and system calls.

#### 3.9.1 Extern Function Declaration

External C functions are declared in `extern` blocks:

```hew
extern "C" {
    fn malloc(size: usize) -> *var u8;
    fn free(ptr: *var u8);
    fn printf(fmt: *u8, ...) -> i32;
    fn open(path: *u8, flags: i32) -> i32;
    fn read(fd: i32, buf: *var u8, count: usize) -> isize;
    fn write(fd: i32, buf: *u8, count: usize) -> isize;
    fn close(fd: i32) -> i32;
}
```

**Calling convention:**

- `extern "C"` specifies the C calling convention (default)
- Future: `extern "stdcall"`, `extern "fastcall"` for platform-specific conventions

#### 3.9.2 C-Compatible Struct Layout

Use `#[repr(C)]` to ensure C-compatible memory layout:

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

| Hew Type                  | C Type                      | Notes                      |
| ------------------------- | --------------------------- | -------------------------- |
| `i8`, `i16`, `i32`, `i64` | `int8_t`, `int16_t`, etc.   | Exact size match           |
| `u8`, `u16`, `u32`, `u64` | `uint8_t`, `uint16_t`, etc. | Exact size match           |
| `isize`                   | `intptr_t` / `ssize_t`      | Platform-dependent         |
| `usize`                   | `uintptr_t` / `size_t`      | Platform-dependent         |
| `f32`, `f64`              | `float`, `double`           | IEEE 754                   |
| `bool`                    | `_Bool` / `bool`            | C99 bool                   |
| `*T`                      | `const T*`                  | Immutable raw pointer      |
| `*var T`                  | `T*`                        | Mutable raw pointer        |
| `*u8`                     | `const char*`               | C string (null-terminated) |
| `fn(...) -> T`            | Function pointer            | C function pointer         |

#### 3.9.4 Exporting Functions to C

Use `#[export]` to make Hew functions callable from C:

```hew
#[export("hew_process_data")]
fn process_data(data: *u8, len: usize) -> i32 {
    // Implementation accessible from C as hew_process_data()
}

#[export]  // Uses the function name as-is
extern "C" fn my_callback(value: i32) -> i32 {
    value * 2
}
```

#### 3.9.5 Safety Rules

**All FFI calls are `unsafe`:**

```hew
fn allocate_buffer(size: usize) -> *var u8 {
    unsafe {
        malloc(size)
    }
}

fn safe_read(fd: i32, buf: *var u8, count: usize) -> Result<usize, IoError> {
    let result = unsafe { read(fd, buf, count) };
    if result < 0 {
        Err(IoError::from_errno())
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

```hew
// Raw FFI (internal, unsafe)
extern "C" {
    fn open(path: *u8, flags: i32) -> i32;
    fn close(fd: i32) -> i32;
}

// Safe wrapper (public API)
pub type File {
    fd: i32;
}

impl File {
    pub fn open(path: String) -> Result<File, IoError> {
        let c_path = path.to_c_string();
        let fd = unsafe { open(c_path.as_ptr(), O_RDONLY) };
        if fd < 0 {
            Err(IoError::from_errno())
        } else {
            Ok(File { fd })
        }
    }
}

impl Drop for File {
    fn drop(self) {
        unsafe { close(self.fd); }
    }
}
```

---

### 3.10 Standard Library Architecture

Hew's standard library follows a three-tier architecture, enabling use in contexts ranging from bare-metal to full OS environments.

#### 3.10.1 Library Tiers

```
┌─────────────────────────────────────────────┐
│                    std                       │
│  (Full OS integration: fs, net, io, env)     │
├─────────────────────────────────────────────┤
│                   alloc                      │
│    (Heap allocation: Vec, String, Box)       │
├─────────────────────────────────────────────┤
│                   core                       │
│  (No allocation: Option, Result, iterators)  │
└─────────────────────────────────────────────┘
```

**core (no allocation, no OS):**

- Works on bare metal (embedded, OS kernels)
- Primitive types and operations
- `Option<T>`, `Result<T, E>`
- Iterator trait and combinators
- Marker traits (`Send`, `Frozen`, `Copy`)
- Memory intrinsics (`mem::size_of`, `mem::align_of`)

**alloc (heap allocation, no OS):**

- Works anywhere with a heap
- `Vec<T>`, `String`, `Box<T>`
- `Arc<T>`, `Rc<T>`
- `HashMap<K, V>`, `HashSet<T>`
- Formatting infrastructure

**std (full OS integration):**

- Requires a full OS
- `std::fs` (file system: `fs.read`, `fs.write`, `fs.exists`, `fs.delete`, `fs.size`)
- `std::net` (networking: `net.listen`, `net.accept`, `net.connect`, `net.read`, `net.write`)
- `std::net::http` (HTTP: `http.listen`, `http.accept`, `http.respond`, `http.respond_json`)
- `std::io` (Read/Write traits, stdin/stdout)
- `std::os` (environment: `os.args`, `os.env`, `os.cwd`, `os.hostname`, `os.pid`)
- `std::process` (process spawning: `process.run`, `process.spawn`, `process.wait`)
- `std::text::regex` (regular expressions: `regex.new`, `regex.is_match`, `regex.find`, `regex.replace`)
- `std::net::mime` (MIME types: `mime.from_path`, `mime.from_ext`, `mime.is_text`)

#### 3.10.2 Core Traits

**Iterator protocol:**

```hew
trait Iterator {
    type Item;
    fn next(self) -> Option<Self::Item>;

    // Provided combinators
    fn map<B>(self, f: fn(Self::Item) -> B) -> Map<Self, B>;
    fn filter(self, pred: fn(Self::Item) -> bool) -> Filter<Self>;
    fn collect<C: FromIterator<Self::Item>>(self) -> C;
    fn fold<B>(self, init: B, f: fn(B, Self::Item) -> B) -> B;
}

trait IntoIterator {
    type Item;
    type IntoIter: Iterator[Item = Self::Item];
    fn into_iter(self) -> Self::IntoIter;
}
```

**Hashing:**

```hew
trait Hash {
    fn hash<H: Hasher>(self, state: H);
}

trait Hasher {
    fn write(self, bytes: [u8]);
    fn finish(self) -> u64;
}
```

**Display and Debug:**

```hew
trait Display {
    fn display(self) -> String;
}

trait Debug {
    fn debug(self) -> String;
}
```

**Clone and Copy:**

```hew
trait Clone {
    fn clone(self) -> Self;
}

// Marker trait - types are copied on assignment, not moved
trait Copy: Clone {}
```

**Allocator:**

```hew
trait Allocator {
    fn alloc(self, size: usize, align: usize) -> Result<*var u8, AllocError>;
    fn dealloc(self, ptr: *var u8, size: usize, align: usize);
}
```

#### 3.10.3 Core Types

**Option and Result:**

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

**String:**

```hew
// Owned, heap-allocated UTF-8 string
type String {
    // Internal: Vec<u8> guaranteed to be valid UTF-8
}

impl String {
    fn new() -> String;
    fn from(s: str) -> String;
    fn len(self) -> usize;
    fn push(self, c: char);
    fn push_str(self, s: str);
    fn as_str(self) -> str;
}

// String methods (called on string values with dot-syntax)
fn contains(self, sub: string) -> bool;
fn starts_with(self, prefix: string) -> bool;
fn ends_with(self, suffix: string) -> bool;
fn trim(self) -> string;
fn to_lower(self) -> string;
fn to_upper(self) -> string;
fn replace(self, old: string, new: string) -> string;
fn split(self, sep: string) -> Vec<string>;
fn find(self, sub: string) -> i32;          // Returns index or -1
fn slice(self, start: i32, end: i32) -> string;
fn len(self) -> i32;
fn repeat(self, n: i32) -> string;
fn char_at(self, i: i32) -> string;
fn index_of(self, sub: string) -> i32;      // Returns index or -1

// Built-in string functions (available in std::prelude)
fn string_length(s: String) -> i32;
fn string_char_at(s: String, index: i32) -> i32;  // Returns character code
fn string_equals(a: String, b: String) -> bool;
fn string_concat(a: String, b: String) -> String;
```

**String operators:**

The `+` operator concatenates strings. The `==` and `!=` operators compare strings by value.

```hew
let greeting = "hello" + " " + "world";   // String concatenation
let same = greeting == "hello world";       // true (value equality)
let diff = greeting != "goodbye";           // true
```

**String methods (dot-syntax):**

String values support method-call syntax for common operations:

```hew
let s = "Hello, World!";
let has_hello = s.contains("Hello");       // true (bool)
let upper = s.to_upper();                  // "HELLO, WORLD!"
let trimmed = "  hi  ".trim();             // "hi"
let parts = "a,b,c".split(",");            // Vec<string> ["a", "b", "c"]
let replaced = s.replace("World", "Hew");  // "Hello, Hew!"
let sub = s.slice(0, 5);                   // "Hello"
let n = s.len();                           // 13
```

Methods returning a yes/no answer (`contains`, `starts_with`, `ends_with`) return `bool`.

**Vec:**

```hew
type Vec<T> {
    // Internal: pointer, length, capacity
}

impl<T> Vec<T> {
    fn new() -> Vec<T>;
    fn with_capacity(cap: usize) -> Vec<T>;
    fn push(self, item: T);
    fn pop(self) -> Option<T>;
    fn len(self) -> usize;
    fn get(self, index: usize) -> Option<T>;
    fn truncate(self, len: usize);
    fn clone(self) -> Vec<T>;
    fn swap(self, a: usize, b: usize);
    fn sort(self) where T: Ord;
}
```

**HashMap:**

```hew
type HashMap<K: Hash + Eq, V> {
    // Internal: Robin Hood hashing
}

impl<K: Hash + Eq, V> HashMap<K, V> {
    fn new() -> HashMap<K, V>;
    fn insert(self, key: K, value: V) -> Option<V>;
    fn get(self, key: K) -> Option<V>;
    fn remove(self, key: K) -> Option<V>;
    fn contains_key(self, key: K) -> bool;
}
```

#### 3.10.4 IO Traits

```hew
trait Read {
    fn read(self, buf: [u8]) -> Result<usize, IoError>;

    // Provided methods
    fn read_exact(self, buf: [u8]) -> Result<(), IoError>;
    fn read_to_end(self, buf: Vec<u8>) -> Result<usize, IoError>;
    fn read_to_string(self, buf: String) -> Result<usize, IoError>;
}

trait Write {
    fn write(self, buf: [u8]) -> Result<usize, IoError>;
    fn flush(self) -> Result<(), IoError>;

    // Provided methods
    fn write_all(self, buf: [u8]) -> Result<(), IoError>;
}

trait BufRead: Read {
    fn fill_buf(self) -> Result<[u8], IoError>;
    fn consume(self, amt: usize);
    fn read_line(self, buf: String) -> Result<usize, IoError>;
}
```

#### 3.10.5 String Primitives and I/O

**Console output:**

The spec defines two polymorphic print functions that accept any type implementing `Display`:

```hew
fn print(value: dyn Display);    // Print value (no newline)
fn println(value: dyn Display);  // Print value with newline
```

These dispatch to `value.display()` to produce a string, then write it to stdout. All built-in types (`i32`, `f64`, `bool`, `String`, etc.) implement `Display`.

**Implementation note:** The current compiler provides type-specific intrinsics (`hew_print_i64`, `hew_print_str`, etc.) as the implementation of `print`/`println`. These are implementation details, not part of the language specification.

**File I/O:**

All IO APIs return `Result<T, IoError>` for error handling.

```hew
fn read_file(path: String) -> Result<String, IoError>;  // Read entire file to string
fn write_file(path: String, content: String) -> Result<(), IoError>;  // Write string to file
```

**String operations:**

```hew
fn string_length(s: String) -> i32;              // Get string length
fn string_char_at(s: String, index: i32) -> i32; // Get char code at index
fn string_equals(a: String, b: String) -> bool;  // Compare strings
fn string_concat(a: String, b: String) -> String; // Concatenate strings
fn string_from_char(code: i32) -> String;            // Create 1-char string from char code
```

These legacy functions remain available but the preferred style is to use string operators and methods:

```hew
// Preferred: operator and method syntax
let greeting = "hello" + " " + "world";   // + for concatenation
let eq = greeting == "hello world";        // == for equality
let n = greeting.len();                    // .len() method
let has = greeting.contains("world");      // .contains() returns bool
```

**String interpolation (f-strings):**

F-strings support arbitrary expressions inside `{}` delimiters, not just variable names:

```hew
let name = "world";
let x = 10;
let msg = f"hello {name}";                 // Variable reference
let computed = f"result: {x + 1}";         // Arithmetic expression
let nested = f"len: {name.len()}";         // Method call in interpolation
```

F-strings are the sole string interpolation syntax in Hew.

#### 3.10.6 Prelude (Automatically Imported)

The following are automatically available in every Hew module:

```hew
// Types
Option, Some, None
Result, Ok, Err
String, Vec, Box

// Traits
Clone, Copy, Drop
Send, Frozen
Debug, Display
Iterator, IntoIterator
Eq, Ord, Hash

// Functions
print, println
read_file
string_length, string_char_at, string_equals, string_concat, string_from_char
panic, assert, debug_assert
```

#### 3.10.7 Typed Handles

Standard library functions return typed handle objects instead of raw pointers.
These provide type-safe method access:

| Type             | Created by                                 | Methods/Properties                                                                                                                              |
| ---------------- | ------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------- |
| `http.Server`    | `http.listen(addr)`                        | `.accept()` → `http.Request`, `.close()`                                                                                                        |
| `http.Request`   | `server.accept()` or `http.accept(server)` | `.path`, `.method`, `.body`, `.header(name)`, `.respond(status, body, len, type)`, `.respond_text(status, body)`, `.respond_json(status, body)` |
| `net.Listener`   | `net.listen(addr)`                         | `.accept()` → `net.Connection`, `.close()`                                                                                                      |
| `net.Connection` | `listener.accept()` or `net.connect(addr)` | `.read()`, `.write(data)`, `.close()`                                                                                                           |
| `regex.Pattern`  | `re"pattern"` or `regex.new(pattern)`      | `.is_match(text)`, `.find(text)`, `.replace(text, replacement)`                                                                                 |
| `process.Child`  | `process.spawn(cmd)`                       | `.wait()`, `.kill()`                                                                                                                            |

Handle types are opaque — their internal representation is not accessible.
They can be stored in variables, passed as function arguments, and returned from functions.

#### 3.10.8 Regular Expressions

Regex is a first-class type in Hew. Regex patterns are compiled at runtime.

**Regex literals:**

```hew
let re = re"^hello\s+world$";
```

The `re"..."` syntax creates a `regex.Pattern` value. Standard regex escape sequences apply.

**Match operators:**

```hew
if text =~ re"pattern" { ... }   // matches
if text !~ re"pattern" { ... }   // does not match
```

The `=~` operator returns `true` if the string matches the pattern.
The `!~` operator returns `true` if the string does NOT match.

Both operators have the same precedence as `==` and `!=`.

**Regex methods:**

```hew
let re = re"[0-9]+";
re.is_match("abc123")              // true
re.find("hello 42 world")         // "42"
re.replace("hello 42", "NUM")     // "hello NUM"
```

**Regex as first-class values:**

Regex values can be stored, passed, and reused:

```hew
fn is_valid_email(s: string) -> bool {
    s =~ re"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$"
}
```

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

| Method    | Signature                   | Description                                                 |
| --------- | --------------------------- | ----------------------------------------------------------- |
| `is_done` | `fn is_done(&self) -> bool` | Returns `true` if task has completed, cancelled, or trapped |

### 4.2 Scope: Structured Concurrency Boundary

A `scope` block creates a structured concurrency boundary. All tasks spawned within a scope must complete before the scope exits.

**Syntax:**

```hew
scope {
    // Tasks spawned here are children of this scope
    // Block exits only when all child tasks complete
}
```

**Semantics:**

1. **Lifetime containment**: Tasks cannot outlive their enclosing scope
2. **Automatic join**: Scope block waits for all child tasks before returning
3. **Scope value**: A scope is an expression; its value is the final expression in the block
4. **Nested scopes**: Scopes may be nested; each manages its own children

**Scope type:**

Within a `scope` block, the implicit `scope` binding has type `Scope`. This binding is only valid inside the scope block.

| Method   | Signature                                      | Description                                         |
| -------- | ---------------------------------------------- | --------------------------------------------------- |
| `launch` | `fn launch<T>(&self, f: fn() -> T) -> Task<T>` | Launch a cooperative task (coroutine) in this scope |
| `spawn`  | `fn spawn<T>(&self, f: fn() -> T) -> Task<T>`  | Spawn a parallel task (OS thread) in this scope     |
| `cancel` | `fn cancel(&self)`                             | Request cancellation of all tasks in this scope     |

### 4.3 Spawning Tasks

Hew provides two task-spawning primitives within a scope:

```hew
scope |s| {
    // Cooperative micro-coroutine — runs on the actor's thread
    let task1 = s.launch { compute_a() };

    // Parallel OS thread — true parallelism, cannot access actor state
    let task2 = s.spawn { compute_b() };
}
```

**Syntax:**

```ebnf
Scope = "scope" ( "|" Ident "|" )? Block ;
(* Inside the block, the binding supports: s.launch { }, s.spawn { }, s.cancel() *)
```

**`s.launch { expr }` — cooperative micro-coroutine:**

- Returns `Task<T>` where `T` is the type of `expr`
- Runs on the actor's own thread as a cooperative micro-coroutine (8 KB pooled stacks, ~10 ns context switch)
- **CAN** access actor state safely — same thread, no data races, only one cooperative task runs at a time
- Captured variables follow the same rules as closures (move semantics by default)

**`s.spawn { expr }` — parallel OS thread:**

- Returns `Task<T>` where `T` is the type of `expr`
- Spawns a separate OS thread for true parallelism across CPU cores
- **Cannot** access actor state — data must be moved or cloned into the task body
- This prevents data races without requiring locks

**Scheduling model (normative):**

Tasks within an actor follow a **two-level scheduling** model:

- **Level 1 (Runtime scheduler):** The M:N work-stealing scheduler (§9) selects which actor to run next. Actors are scheduled across worker threads.
- **Level 2 (Actor-local coroutine executor):** Within a running actor, cooperative tasks (`s.launch`) are multiplexed on the actor's thread. Only one runs at a time; they yield at safepoints and the next ready coroutine resumes.

```
Level 1: Scheduler picks Actor A to run on Worker 3
    │
    Level 2: Actor A's coroutine executor runs cooperative tasks:
    ├─── Task 1 executes ──► yields at await (coro_switch)
    ├─── Task 2 executes ──► yields at cooperate
    ├─── Task 1 resumes  ──► completes
    └─── Task 2 resumes  ──► completes
    │
Level 1: Actor A yields; scheduler picks Actor B
    │
    (s.spawn tasks run independently on OS threads)
```

**Yield points (normative):**

Cooperative tasks (`s.launch`) MUST yield at:

- `await` expressions — suspends coroutine until awaited result is ready
- `cooperate` — reduction budget exhaustion; compiler inserts `cooperate` calls at loop headers and function call sites
- Tasks may opt out of safepoints in critical sections with `#[no_safepoint]`

Parallel tasks (`s.spawn`) run on OS threads and are not subject to cooperative yield points.

**Implementation note:** `s.launch` allocates an 8 KB stack from a pool and runs the body as a stackful coroutine via `coro_switch`. `s.spawn` outlines the body to a separate function and spawns it on a new OS thread. `await` suspends the calling coroutine (or blocks the calling thread for `s.spawn`). The runtime uses `Mutex`/`Condvar` for cross-thread completion notification. Scope exit calls `join_all` to wait for all tasks.

### 4.4 Awaiting Tasks

The `await` operator blocks the current task until the awaited task completes, returning its result.

**Syntax:**

```hew
let result = await task;
```

**Semantics (normative):**

| Awaited Task State | `await` Behavior                                            |
| ------------------ | ----------------------------------------------------------- |
| Completed          | Returns `Ok(value)` immediately                             |
| Running/Pending    | Suspends current task until completion, returns `Ok(value)` |
| Cancelled          | Returns `Err(CancellationError)`                            |
| Trapped            | Propagates the trap to the awaiting task                    |

**Type:**

```
await : Task<T> -> Result<T, CancellationError>
```

Cancellation is an **expected** outcome (it is explicitly requested via `s.cancel()`) and MUST be modeled as a recoverable error, not a trap. Traps are reserved for unexpected, unrecoverable failures (Section 2.2).

```hew
// Cancellation returns Err, not a trap:
let result = await task;
match result {
    Ok(v) => use_value(v),
    Err(e) => handle_cancellation(e),
}

// Use ? to propagate cancellation errors:
let value = (await task)?;
```

> **Note:** Only traps (panics) propagate as unrecoverable. Cancellation is always catchable via the `Result` return type.

**Examples:**

```hew
// Simple await
let value = scope |s| { await s.launch { expensive_compute() } };

// Concurrent tasks with sequential await
scope |s| {
    let a = s.launch { fetch_user(id1) };
    let b = s.launch { fetch_user(id2) };

    // Both fetches run concurrently
    // We wait for results in order
    let user1 = await a;
    let user2 = await b;

    merge_users(user1, user2)
}
```

### 4.5 Cancellation

Cancellation in Hew is **automatic at safepoints**: when a scope is cancelled, running tasks are interrupted at the next safepoint without manual polling.

**Requesting cancellation:**

```hew
s.cancel();  // Request cancellation of all tasks in scope
```

**Cancellation is automatic at safepoints:**

The following points are safepoints where cancellation is checked automatically:

- `await` expressions
- `cooperate` calls (compiler-inserted at loop headers and function calls)
- IO operations (file read/write, network operations)

When cancellation fires at a safepoint, the runtime initiates **stack unwinding** with a `Cancelled` payload. All `defer` blocks and `Drop` implementations run during unwinding, ensuring deterministic resource cleanup.

**`#[noncancellable]` for critical sections:**

```hew
#[noncancellable]
fn commit_transaction(tx: Transaction) -> Result<(), Error> {
    // This function will NOT be interrupted by cancellation.
    // Cancellation is deferred until after this function returns.
    tx.write_log()?;
    tx.commit()?;
    Ok(())
}
```

**Cancellation propagation:**

When a scope is cancelled:

1. Pending tasks that haven't started are immediately marked `Cancelled`
2. Running tasks are cancelled at their next safepoint (automatic — no polling needed)
3. Stack unwinding runs `defer`/`Drop` blocks for deterministic cleanup
4. Child scopes receive the cancellation signal

**Cancellation does NOT:**

- Forcibly terminate running code between safepoints
- Interrupt `#[noncancellable]` sections
- Affect tasks in other scopes or other actors

**Example with cleanup:**

```hew
receive fn download_files(urls: Vec<String>) -> Result<Vec<Data>, Error> {
    scope |s| {
        for url in urls {
            s.launch {
                let data = http::get(url)?;  // Safepoint — cancellation checked here
                process(data)                // If cancelled, stack unwinds; defer blocks run
            };
        }
    }
}
```

### 4.6 Error Handling in Tasks

Tasks can fail in two ways:

1. **Recoverable errors**: Return `Err(E)` from a `Result<T, E>`
2. **Unrecoverable errors**: Trap (panic)

**Recoverable errors:**

When a task returns a `Result`, errors can be handled by the awaiter:

```hew
scope |s| {
    let task = s.launch {
        fallible_operation()?;
        Ok(value)
    };

    match await task {
        Ok(v) => use_value(v),
        Err(e) => handle_error(e),
    }
}
```

**Traps (unrecoverable errors):**

When a task traps:

1. The task transitions to `Trapped` state
2. Sibling tasks in the same scope are cancelled
3. The scope itself traps, propagating to its enclosing context

**Trap in a `receive fn` (normative):**

When a trap propagates out of a `scope` block inside a `receive fn`:

1. The current message handler terminates immediately
2. The actor transitions to `Crashed` state (see §9.1)
3. The actor's supervisor is notified with the trap reason
4. The supervisor applies its restart policy (Section 5.1)

This means a trap within a scoped task inside a `receive fn` causes the entire actor to crash — it does NOT silently discard the error and proceed to the next message. This matches Erlang's "let it crash" philosophy: unexpected failures are handled by the supervision tree, not by application-level error recovery.

**Trap propagation example:**

```hew
scope |s| {
    let a = s.launch { compute() };        // Running
    let b = s.launch { trap!("failed") };  // Traps
    // Task 'a' is cancelled
    // Scope traps
}
// Code here never executes
```

**Isolating failures with nested scopes:**

```hew
scope |s| {
    let results = s.launch {
        // Inner scope isolates failures
        scope |inner| {
            let task = inner.launch { risky_operation() };
            await task
        }
    };

    // Outer scope continues even if inner scope trapped
    // (if using ? pattern)
}
```

### 4.7 IO and Effects

All IO operations in Hew are explicit and return `Result` types:

```hew
fn read_config(path: String) -> Result<Config, IoError> {
    let file = fs::open(path)?;
    let content = file.read_to_string()?;
    json::parse(content)
}
```

**IO operations are cancellation-aware:**

```hew
// If scope is cancelled while waiting for response,
// http::get returns Err(Cancelled)
let response = http::get(url)?;
```

**Blocking operations:**

Hew runtime may offload blocking operations to a thread pool. From the task's perspective:

- The task suspends at the blocking call
- Other tasks in the actor may run
- The task resumes when the operation completes

**Actor isolation guarantees:**

Actor isolation is preserved through the two-task model:

- **Cooperative tasks (`s.launch`):** Share the actor's mutable state. Only one cooperative task runs at a time within an actor (cooperative scheduling), so no data races occur on actor state.
- **Parallel tasks (`s.spawn`):** Do NOT share actor state. Data must be moved or cloned in. Multiple `s.spawn` tasks may execute simultaneously on different cores.

### 4.8 Interaction with Actor Messages

Tasks spawned within a receive handler interact with actor state differently depending on the spawn primitive:

- **`s.launch` (cooperative):** Runs on the actor's thread. CAN access actor state directly — only one cooperative task runs at a time, so no data races.
- **`s.spawn` (parallel):** Runs on a separate OS thread. Cannot access actor state — data must be moved or cloned into the task body.

```hew
actor DataProcessor {
    var cache: HashMap<String, Data> = HashMap::new();

    receive fn process_batch(ids: Vec<String>) -> Vec<Data> {
        scope |s| {
            var results: Vec<Task<Data>> = Vec::new();

            for id in ids {
                // s.spawn: data passed explicitly — task body cannot access self
                let task = s.spawn {
                    fetch_data(id)
                };
                results.push(task);
            }

            // Await all results (back on actor thread)
            results.iter().map(|t| await t).collect()
        }
    }
}
```

**Message-task interaction rules:**

1. A `receive fn` handler executes on the actor's thread
2. Cooperative tasks (`s.launch`) run on the actor's thread and can access actor state
3. Parallel tasks (`s.spawn`) run on separate OS threads and are isolated from actor state
4. The actor does not process the next message until the current handler (and all its tasks) complete
5. If a handler's tasks trap, the actor may trap (per failure model)
6. Data shared with `s.spawn` tasks must be moved or cloned (no implicit sharing)

**Yielding to the scheduler:**

For long-running computations, `cooperate` yields the actor to the runtime scheduler. The compiler inserts `cooperate` calls automatically at loop headers and function call sites based on a reduction budget (see §9.0):

```hew
fn heavy_computation() {
    for i in 0..1000000 {
        // cooperate is compiler-inserted at loop header
        process(i);
    }
}
```

### 4.9 Summary: Tasks vs Actors

| Aspect        | `s.launch` (cooperative)                    | `s.spawn` (parallel)                | Actors                           |
| ------------- | ------------------------------------------- | ----------------------------------- | -------------------------------- |
| Communication | Shared actor state + await                  | Explicit data passing + await       | Message passing                  |
| Concurrency   | Cooperative (one at a time on actor thread) | True parallelism (separate threads) | True parallelism (M:N scheduler) |
| Isolation     | Shares actor state (safe: single-threaded)  | Complete (no shared mutable state)  | Complete (mailbox only)          |
| Failure       | Traps propagate in scope                    | Traps propagate in scope            | Traps isolated to actor          |
| Lifetime      | Bound to scope                              | Bound to scope                      | Independent                      |
| Cancellation  | Automatic at safepoints                     | Automatic at safepoints             | Supervisor control               |
| Scheduling    | Actor-local coroutine (~10 ns switch)       | OS thread per task                  | M:N work-stealing scheduler      |

**Design rationale:**

Hew combines Go's lightweight concurrency with Erlang's actor isolation:

- **Like Go**: Lightweight tasks with `s.launch`/`s.spawn`, true parallelism via `s.spawn`
- **Like Erlang**: Actors as isolated failure domains, supervisors for fault tolerance, no shared mutable state between actors
- **Like Swift**: Structured concurrency with scope-bounded lifetimes, automatic cancellation at safepoints
- **Cooperative + parallel**: `s.launch` for actor-local work that needs state access; `s.spawn` for CPU-bound parallelism

This hybrid provides:

- Simple concurrent code within actors (cooperative coroutines)
- True parallelism when needed (OS threads via `s.spawn`)
- Strong isolation between actors (Erlang-style)
- Safe resource management via structured lifetimes (Swift-style)
- No data-race-by-design at all levels of concurrency

### 4.10 Actor Await and Synchronization

Hew provides deterministic actor synchronization primitives that replace polling patterns like `sleep_ms()`:

**Awaiting a single actor:**

```hew
let ref = spawn(MyActor::new());
// ... send messages ...
await ref;  // Blocks until ref reaches Stopped or Crashed
```

`await actorRef` installs a monitor on the target actor and blocks (via condvar) until the actor reaches a terminal state (`Stopped` or `Crashed`). This is event-driven — no polling or busy-waiting.

**Fan-out/fan-in with `await all()`:**

```hew
let a = spawn(Worker::new(1));
let b = spawn(Worker::new(2));
let c = spawn(Worker::new(3));

// Wait for all actors to terminate
await all(a, b, c);
```

`await all(a, b, c)` monitors all listed actors and returns when every actor has reached a terminal state. This is the idiomatic pattern for fork-join parallelism across actors.

**Drain barrier:**

```hew
actor.drain(ref);  // Stdlib barrier message — ensures all prior messages are processed
```

`actor.drain(ref)` sends a barrier message to the actor's mailbox. When the actor processes the barrier, it signals the caller. This guarantees all messages sent before the drain have been processed.

**Design rationale:**

These primitives replace `sleep_ms()` patterns with deterministic, event-driven synchronization. `await ref` is zero-cost when the actor has already stopped, and `await all()` enables clean fan-out/fan-in without manual bookkeeping.

### 4.11 Select and Join Expressions

Hew provides two built-in concurrency expressions for coordinating multiple asynchronous operations. These are expressions — they produce values — and integrate with Hew's structured concurrency and actor models.

#### 4.11.1 `select` Expression

The `select` expression waits for the first of several asynchronous operations to complete, then evaluates the corresponding arm. Remaining operations are cancelled.

**Syntax:**

```hew
let result = select {
    count from counter.get_count() => count * 2,
    data from worker.get_data() => data.len,
    after 1.seconds => -1,       // timeout arm
};
```

**Semantics:**

- Each arm starts an asynchronous operation.
- The first operation to complete wins; its binding is made available to the `=>` expression.
- All other operations are cancelled cooperatively.
- The `from` keyword binds the result of the operation to the identifier.
- An `after` arm provides a timeout — if no operation completes within the given duration, the timeout arm evaluates.
- All arm result expressions must have the same type `T`. The `select` expression has type `T`.

**Type rules:**

```
select {
    p1 from e1 => r1,
    p2 from e2 => r2,
    after d => r3,
} : T
where e1: Task<A>, e2: Task<B>, r1: T, r2: T, r3: T
```

The bound identifiers (`p1`, `p2`) have types `A`, `B` respectively within their arm expressions. The overall `select` has type `T` — the common type of all arm results.

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

**Type rules:**

```
join { e1, e2, e3 } : (T1, T2, T3)
where e1: Task<T1>, e2: Task<T2>, e3: Task<T3>
```

Each branch may have a different result type. The result is a tuple of all branch results, in declaration order.

**Dynamic `join_all` (variable number of branches):**

```hew
let results: Vec<i32> = join_all(actors, |a| a.compute());
```

`join_all` takes a collection and a closure, spawning a concurrent operation for each element. All branches must have the same result type. Returns `Vec<T>`.

**Type rules:**

```
join_all(collection: Iterable<A>, f: fn(A) -> Task<T>) : Vec<T>
```

**Error propagation:** If any branch in a `join` traps, the remaining branches are cancelled and the trap propagates to the enclosing scope.

#### 4.11.3 `after` Timeout

The `after` keyword is used in two contexts:

**1. As an arm in `select` expressions** (see above):

```hew
select {
    result from server.fetch() => result,
    after 5.seconds => default_value,
};
```

**2. As a timeout combinator with `|`** for individual await expressions:

```hew
let result = await counter.get_count() | after 1.seconds;
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

### 4.12 Generators

A **generator** is a function that produces a sequence of values lazily, suspending after each `yield` and resuming when the consumer requests the next value. Generators compile to state machines — each `yield` becomes a suspend point, and the generator's local state is preserved in a heap-allocated frame.

#### 4.12.1 Generator Functions

A generator function is declared with the `gen` modifier before `fn`:

```hew
gen fn fibonacci() -> i32 {
    var a = 0;
    var b = 1;
    loop {
        yield a;
        let temp = a;
        a = b;
        b = temp + b;
    }
}
```

**Syntax:**

```ebnf
GenFnDecl      = "gen" "fn" Ident TypeParams? "(" Params? ")" "->" Type WhereClause? Block ;
```

**Semantics:**

- The return type annotation after `->` specifies the **yield type** `Y` — the type of values produced by `yield` expressions.
- Calling a generator function does NOT execute its body. Instead, it returns a `Generator<Y>` value representing the suspended computation.
- The body executes incrementally: each call to `.next()` on the generator resumes execution until the next `yield` or until the function returns.
- When the function body completes (reaches the end or executes `return;`), the generator is exhausted — subsequent `.next()` calls return `None`.

```hew
let fib = fibonacci();           // Returns Generator<i32>, body does NOT run yet
let first = fib.next();          // Runs body until first yield → Some(0)
let second = fib.next();         // Resumes, runs until second yield → Some(1)
let third = fib.next();          // → Some(1)
```

**Generator type:**

```hew
type Generator<Y> { /* compiler-generated coroutine frame */ }

impl<Y> Iterator for Generator<Y> {
    type Item = Y;
    fn next(self) -> Option<Y>;
}
```

Because `Generator<Y>` implements `Iterator`, generators work everywhere iterators do:

```hew
// for-in loop (most common usage)
for n in fibonacci() {
    if n > 100 { break; }
    println(n);
}

// Iterator combinators
let squares = fibonacci()
    .map((n) => n * n)
    .filter((n) => n % 2 == 0)
    .take(10)
    .collect();
```

#### 4.12.2 Yield Expressions

The `yield` keyword within a generator function produces a value to the consumer and suspends the generator until the next `.next()` call.

**Syntax:**

```ebnf
YieldExpr      = "yield" Expr ;
```

**Semantics:**

- `yield expr` evaluates `expr`, produces the value to the consumer, and suspends the generator.
- When resumed, execution continues from the statement after the `yield`.
- `yield` is only valid inside a `gen fn` or `async gen fn` body. Using `yield expr` outside a generator function is a compile error.
- The keyword `cooperate` (§4.3) serves the distinct purpose of cooperative task scheduling. `yield` is reserved exclusively for generator value production.

**Generator state machine:**

Each `gen fn` compiles to a state machine (stackless coroutine). The compiler transforms the function body into states separated by `yield` points:

```
State 0 (Initial): Execute body until first yield → produce value, transition to State 1
State 1: Resume after first yield, execute until second yield → produce value, transition to State 2
...
State N (Terminal): Body completed → return None for all subsequent .next() calls
```

#### 4.12.3 Generator Parameters and Local State

Generator functions can take parameters and maintain mutable local state across yields:

```hew
gen fn range(start: i32, end: i32, step: i32) -> i32 {
    var i = start;
    while i < end {
        yield i;
        i += step;
    }
}

gen fn sliding_window(data: Vec<f64>, size: i32) -> Vec<f64> {
    for i in 0..(data.len() - size + 1) {
        yield data[i..i+size].to_vec();
    }
}
```

Parameters and local variables are stored in the generator's coroutine frame. The frame is heap-allocated within the owning actor's per-actor heap and freed when the generator is dropped (RAII).

#### 4.12.4 Async Generators

An **async generator** can both `yield` values and `await` asynchronous operations. This enables streaming from I/O sources, network calls, or other actors.

> **Note:** The `async` keyword is ONLY valid as a modifier on `gen fn`. Standalone `async fn` declarations have no specified semantics in the actor model (actors are inherently concurrent via message passing) and are not part of the Hew grammar. Use `async gen fn` to create async generators, or use actors and `receive fn` for async behavior.

```hew
async gen fn fetch_pages(base_url: String) -> Page {
    var page_num = 1;
    loop {
        let response = await http::get(f"{base_url}?page={page_num}");
        if response.items.is_empty() {
            return;  // Exhausts the generator
        }
        for item in response.items {
            yield item;
        }
        page_num += 1;
    }
}
```

**Syntax:**

```ebnf
AsyncGenFnDecl = "async" "gen" "fn" Ident TypeParams? "(" Params? ")" "->" Type WhereClause? Block ;
```

**Type:**

```hew
type AsyncGenerator<Y> { /* coroutine frame with async suspend points */ }

impl<Y> AsyncIterator for AsyncGenerator<Y> {
    type Item = Y;
    async fn next(self) -> Option<Y>;
}
```

**Consumption via `for await`:**

```hew
for await page in fetch_pages("https://api.example.com/users") {
    process(page);
}
```

```ebnf
ForStmt        = "for" "await"? Pattern "in" Expr Block ;
```

**Semantics:**

- `async gen fn` produces an `AsyncGenerator<Y>` — an async iterator.
- `for await item in async_gen { ... }` desugars to repeatedly calling `await async_gen.next()` until `None`.
- Between yields, the async generator can `await` other async operations. The generator suspends both when yielding a value AND when awaiting an external result.
- Async generators participate in structured concurrency — they are cancelled when their enclosing scope exits.

#### 4.12.5 Cross-Actor Generators (Streaming Receive)

A `receive gen fn` on an actor creates a **streaming message handler** — the actor lazily produces values that the caller consumes as a stream.

```hew
actor DatabaseActor {
    var db: Connection;

    init(conn_string: String) {
        self.db = connect(conn_string);
    }

    // Streaming receive: yields rows lazily to the caller
    receive gen fn query(sql: String) -> Row {
        let cursor = self.db.execute(sql);
        while let Some(row) = cursor.next() {
            yield row;
        }
    }
}
```

**Caller side:**

```hew
let db = spawn DatabaseActor("postgres://localhost/mydb");

// Streaming consumption — rows arrive lazily
for await row in db.query("SELECT * FROM users WHERE active = true") {
    process(row);
}

// Only fetches what's needed — generator is dropped when loop breaks
for await row in db.query("SELECT * FROM large_table") {
    if found_target(row) {
        break;  // Generator on DatabaseActor is cancelled
    }
}
```

**Semantics:**

Calling a `receive gen fn` returns a `Stream<Y>` — a first-class stream handle backed by the actor mailbox protocol.

```hew
// Stream<Y> implements AsyncIterator
```

**Protocol (normative):**

The cross-actor streaming protocol uses the existing mailbox infrastructure:

1. **Initiation:** The caller sends a "start stream" message to the actor. The actor begins executing the `receive gen fn` body.
2. **Yielding:** When the generator yields a value, the runtime sends it as a message to the caller's mailbox. The generator then suspends, waiting for a "next" request.
3. **Requesting:** When the caller calls `.next()` (or the `for await` loop iterates), a "next" message is sent to the producing actor, resuming the generator.
4. **Completion:** When the generator body completes, a "stream end" message is sent to the caller. Subsequent `.next()` calls return `None`.
5. **Cancellation:** If the caller drops the stream handle (e.g., `break` from a `for await` loop), a "cancel stream" message is sent to the producing actor, which cancels the generator coroutine.

**Backpressure:**

Cross-actor generators provide **natural backpressure**: the producer only runs when the consumer requests the next value. This is demand-driven — unlike push-based streaming, the producer cannot overwhelm the consumer's mailbox.

The streaming protocol MAY use a **prefetch window** to amortize message-passing overhead:

```hew
actor DataSource {
    #[prefetch(8)]
    receive gen fn stream_events() -> Event {
        for event in self.event_log {
            yield event;
        }
    }
}
```

This is an optimization hint — the observable semantics are identical to one-at-a-time request/yield.

**Network transparency:**

Cross-actor generators work identically for local and remote actors. The yielded values MUST satisfy `Send` (§3.3) since they cross actor boundaries.

#### 4.12.6 Generator Lifetime and Structured Concurrency

Generators participate in Hew's structured concurrency model:

**Scope-bound lifetime:**

```hew
scope {
    let gen = fibonacci();
    for n in gen {
        if n > 1000 { break; }
        println(n);
    }
}
// gen is dropped when scope exits (if not already exhausted)
```

**Cross-actor stream cancellation:**

When a stream handle is dropped (scope exit, break, or explicit drop), the runtime sends a cancellation message to the producing actor. The producer's generator coroutine is cancelled at its next yield/await point.

**Invariant:** A generator's lifetime MUST NOT exceed the lifetime of the actor that created it. If the producing actor is stopped or crashed, all its active stream handles become invalidated — `.next()` returns `None`.

#### 4.12.7 Type Inference for Generators

The compiler infers generator types using the bidirectional inference framework (§3.8.6):

**Yield type inference:**

The yield type `Y` is inferred from the types of all `yield expr` expressions in the generator body. All yield expressions MUST produce the same type. The wrapper type (`Generator<Y>` or `AsyncGenerator<Y>`) is inferred by the compiler — it is never written explicitly by the programmer:

```hew
gen fn example() -> i32 {    // Y = i32 (explicit annotation)
    yield 1;                 // Compiler infers return: Generator<i32>
    yield 2;
    yield 3;
}

async gen fn stream() -> i32 {  // Y = i32 (explicit annotation)
    yield 1;                    // Compiler infers return: AsyncGenerator<i32>
}
```

The `-> i32` annotation specifies the yield type, NOT the return type. The actual return type is always the appropriate generator wrapper:

- `gen fn foo() -> i32 { ... }` → returns `Generator<i32>`
- `async gen fn bar() -> i32 { ... }` → returns `AsyncGenerator<i32>`

> **Note:** The return type annotation on `gen fn` is REQUIRED. This makes the yield type visible at the call site and in documentation.

**Constraint generation:**

For each `yield expr` in the body, generate constraint: `typeof(expr) = Y`. The overall generator synthesizes type `Generator<Y>` (sync) or `AsyncGenerator<Y>` (async), implementing `Iterator<Item = Y>` or `AsyncIterator<Item = Y>` respectively.

#### 4.12.8 Generator Trait Hierarchy and Send Constraints

Generators integrate into the trait system:

```hew
// Synchronous iterator protocol (existing — §3.6)
trait Iterator {
    type Item;
    fn next(self) -> Option<Self::Item>;
}

// Asynchronous iterator protocol
trait AsyncIterator {
    type Item;
    async fn next(self) -> Option<Self::Item>;
}

// Generator — an Iterator backed by a coroutine
trait Generator: Iterator {
    fn resume(self) -> GeneratorState<Self::Item>;
}

enum GeneratorState<Y> {
    Yielded(Y),
    Complete,
}
```

**Send constraints:**

| Generator Kind      | `Send` if...                                     |
| ------------------- | ------------------------------------------------ |
| `Generator<Y>`      | `Y: Send` AND all captured/local state is `Send` |
| `AsyncGenerator<Y>` | `Y: Send` AND all captured/local state is `Send` |

Cross-actor generators (`receive gen fn`) enforce `Send` on the yield type at the declaration site.

---

## 5. Supervision (fault tolerance)

Hew's supervision is modeled after OTP concepts with first-class language syntax:

- Supervisor owns children; children fail independently.
- Restart classification: `permanent`, `transient`, `temporary`. ([Erlang.org][2])
- Supervisor strategy: `one_for_one`, `one_for_all`, `rest_for_one`.
- Crash isolation via signal handling: SEGV/BUS/FPE/ILL in an actor is caught by the runtime, the actor is marked as Crashed, and the supervisor is notified for restart.

### 5.1 Supervisor Declaration

```hew
supervisor MyPool {
    strategy: one_for_one
    max_restarts: 5
    window: 60
    child worker1: Worker(1, 0)
    child worker2: Worker(2, 0) transient
    child logger: Logger(3) temporary
}
```

**Fields:**

- `strategy`: Restart strategy (`one_for_one`, `one_for_all`, `rest_for_one`)
- `max_restarts`: Maximum restarts allowed within the time window (default: 5)
- `window`: Time window in seconds for restart budget tracking (default: 60)

**Child specifications:**

- `child <name>: <ActorType>(<init_args>)` — declares a supervised child actor
- Optional restart policy suffix: `permanent` (default), `transient`, `temporary`
- Child actor types must be declared before the supervisor

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

| Strategy       | Behavior                                                            |
| -------------- | ------------------------------------------------------------------- |
| `one_for_one`  | Only the crashed child is restarted.                                |
| `one_for_all`  | All children are stopped and restarted.                             |
| `rest_for_one` | The crashed child and all children declared after it are restarted. |

### 5.4 Restart Budget and Escalation

Supervisor has `(max_restarts, window)`; exceeding the restart budget escalates failure to its parent supervisor. The runtime tracks restarts in a sliding window.

**Exponential backoff:** After a crash, the runtime applies exponential backoff (starting at 100ms, doubling each crash, max 30s). If the backoff period hasn't elapsed when the next crash occurs, the restart is delayed — not abandoned.

**Circuit breaker:** Per-child circuit breaker transitions through CLOSED → OPEN → HALF_OPEN states to prevent cascading restart storms.

### 5.5 Nested Supervisors

Supervisors can contain other supervisors as children, forming supervision trees:

```hew
supervisor Inner {
    strategy: one_for_one
    max_restarts: 3
    window: 60
    child w1: Worker(1, 0)
    child w2: Worker(2, 0)
}

supervisor Root {
    strategy: one_for_one
    max_restarts: 5
    window: 60
    child pool: Inner
    child cache: CacheActor(1000)
}
```

When a child supervisor's restart budget is exhausted, it escalates to its parent. The parent attempts to restart the entire child supervision subtree. If the parent's budget is also exhausted, the escalation propagates further up the tree.

### 5.6 Spawning and Accessing Supervised Children

```hew
fn main() {
    let pool = spawn MyPool;
    sleep_ms(50);

    let w = supervisor_child(pool, 0);  // Typed: compiler knows w is a Worker
    w.tick();

    supervisor_stop(pool);              // Graceful shutdown
}
```

- `spawn SupervisorName` — creates and starts the supervisor with all declared children
- `supervisor_child(sup, index)` — compiler intrinsic that returns a typed reference to the child at the given index. The compiler resolves the child's actor type from the supervisor declaration, so the returned reference is fully typed without a cast.
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

Every actor mailbox has a bounded capacity and an overflow policy that determines behavior when the mailbox is full.

### 6.1 Mailbox Declaration

```hew
actor MyActor {
    mailbox 1024;                              // default: capacity=1024, overflow=block
    mailbox 100 overflow drop_new;             // explicit policy
    mailbox 100 overflow coalesce(request_id); // coalesce with key
    mailbox 100 overflow coalesce(request_id) fallback drop_new; // explicit fallback
}
```

### 6.2 Overflow Policies

| Policy               | Behavior when mailbox is full                                        |
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

    receive fn update_price(symbol: String, price: f64) {
        self.prices.insert(symbol, price);
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

| Fallback             | Behavior                                             |
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

Both types are `Send` (safe to pass to other actors), opaque (backed by a vtable), and not `Clone`. `Sink<bytes>` write blocks if the downstream consumer is not keeping up — this is the backpressure mechanism.

#### 6.5.1 Creation

```hew
import std::stream;

// In-memory bounded channel (for actor-to-actor communication)
let pair = hew_stream_channel(capacity);     // HewStreamPair* (opaque)
let sink: Sink<bytes>   = hew_stream_pair_sink(pair);
let stream: Stream<bytes> = hew_stream_pair_stream(pair);
hew_stream_pair_free(pair);

// File-backed
let src  = hew_stream_from_file_read("data.bin");   // Stream<bytes>
let dst  = hew_stream_from_file_write("out.bin");   // Sink<bytes>

// Byte-buffer (drains a byte slice)
let s = hew_stream_from_bytes(ptr, len, item_size); // Stream<bytes>
```

#### 6.5.2 Consumption and Production

```hew
// Iterate over all items
for await chunk in stream { ... }

// Get one item (method call, desugars to hew_stream_next)
let item = stream.next();

// Adapters (take ownership of the source stream)
let lines  = stream.lines();          // Stream<bytes> → Stream<string>
let chunks = stream.chunks(4096);     // Stream<bytes> → fixed-size chunks

// Sink operations
hew_sink_write(sink, data_ptr, size); // write raw bytes
sink.flush();                         // flush buffered writes
sink.close();                         // signal EOF to the reader
stream.close();                       // discard remaining items
```

#### 6.5.3 Lifecycle Rules

- Calling `.lines()` or `.chunks()` **transfers ownership** of the source stream; do not use the source after the adapter is created.
- Closing a `Sink` signals EOF to the paired `Stream`: subsequent `.next()` calls on the stream return `null` / EOF.
- Streams and sinks implement `Resource`/`Drop` and **auto-close on scope exit** (RAII). Explicit `.close()` is available for early release but is not required.
- Types holding OS resources (streams, sinks, file handles) implement `Resource`/`Drop` and are **never arena-allocated**.

#### 6.5.4 Relation to Actor Streams

`receive gen fn` produces a `Stream<Y>` backed by the actor mailbox protocol. First-class `Stream<T>` values (from `hew_stream_channel`, `hew_stream_from_file_read`, etc.) are backed by bounded channels or files. Both are consumed with `for await`, but they have different implementations:

|                     | Actor stream (`receive gen fn`) | `Stream<T>`                                             |
| ------------------- | ------------------------------- | ------------------------------------------------------- |
| Created by          | `receive gen fn` call           | `hew_stream_channel`, `hew_stream_from_file_read`, etc. |
| Backed by           | Actor mailbox protocol          | Bounded mpsc channel / file / bytes                     |
| Passable as value   | No (tied to the spawned actor)  | Yes (move-only, `Send`)                                 |
| Use in actor fields | No                              | Yes                                                     |

---

## 7. Wire types and network contracts

Hew introduces `wire` definitions for network-serializable data.

### 7.1 Wire type requirements

A `wire struct` / `wire enum`:

- has stable field tags (numeric IDs)
- has explicit optionality/defaults
- supports forward/backward compatibility checks

### 7.2 Compatibility rules (normative)

Hew adopts Protobuf-style invariants:

- **Field numbers (tags) must never be reused**. ([protobuf.dev][4])
- Deleted fields must have their tags **reserved** to prevent reuse. ([protobuf.dev][5])
- Changing a tag is treated as delete+add (breaking unless carefully managed). ([protobuf.dev][4])

Hew tooling provides:

- `hew wire check --against <schema>` to enforce these rules at build/CI time.

### 7.3 Encoding Formats

Hew supports multiple encoding formats for wire types. The primary internal format (HBF) is designed for efficiency; JSON encoding provides external interoperability.

#### 7.3.1 Hew Binary Format (HBF) — Default Internal Encoding

The Hew Binary Format is the primary wire encoding. Design goals: compact representation, fast encode/decode, zero-copy reads where possible, forward/backward compatibility.

##### 7.3.1.1 Message Structure

Every HBF message has the following structure:

```
+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+
|      Magic (4 bytes)     | Ver(1) | Flags(1)|       Message Length (4 bytes)          |
+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+
|                                    Payload (variable)                                   |
+-----------------------------------------------------------------------------------------+
```

**Header (10 bytes):**

| Offset | Size | Field   | Description                                 |
| ------ | ---- | ------- | ------------------------------------------- |
| 0      | 4    | Magic   | `0x48 0x45 0x57 0x31` (ASCII "HEW1")        |
| 4      | 1    | Version | Format version, currently `0x01`            |
| 5      | 1    | Flags   | Bit flags (see below)                       |
| 6      | 4    | Length  | Payload length in bytes (little-endian u32) |

**Flag bits:**

| Bit | Name       | Meaning                       |
| --- | ---------- | ----------------------------- |
| 0   | COMPRESSED | Payload is LZ4-compressed     |
| 1   | CHECKSUM   | 4-byte CRC32C follows payload |
| 2-7 | Reserved   | Must be 0                     |

##### 7.3.1.2 Field Encoding (TLV)

The payload consists of zero or more field encodings. Each field is encoded as:

```
+----------------+------------------+-------------------+
|   Tag (varint) |  Length (varint) |  Value (variable) |
+----------------+------------------+-------------------+
```

**Tag encoding:**

The tag is a varint encoding: `(field_number << 3) | wire_type`

- `field_number`: The field's numeric tag from the wire type definition (e.g., `@1`, `@2`)
- `wire_type`: 3-bit type indicator

**Wire types:**

| Value | Name             | Description             | Length field                  |
| ----- | ---------------- | ----------------------- | ----------------------------- |
| 0     | VARINT           | Variable-length integer | Not present (self-delimiting) |
| 1     | FIXED64          | 64-bit fixed-width      | Not present (always 8 bytes)  |
| 2     | LENGTH_DELIMITED | Length-prefixed bytes   | Present (varint length)       |
| 5     | FIXED32          | 32-bit fixed-width      | Not present (always 4 bytes)  |

Wire types 3, 4, 6, 7 are reserved for future use.

##### 7.3.1.3 Varint Encoding (Unsigned LEB128)

Varints encode unsigned integers in 1-10 bytes using unsigned LEB128 (Little Endian Base 128):

**Algorithm:**

```
encode_varint(value):
    while value >= 0x80:
        emit_byte((value & 0x7F) | 0x80)  // Set continuation bit
        value = value >> 7
    emit_byte(value & 0x7F)               // Final byte, no continuation

decode_varint():
    result = 0
    shift = 0
    loop:
        byte = read_byte()
        result = result | ((byte & 0x7F) << shift)
        if (byte & 0x80) == 0:
            return result
        shift = shift + 7
        if shift >= 64:
            error("varint too long")
```

**Examples:**

| Value | Encoded bytes    |
| ----- | ---------------- |
| 0     | `0x00`           |
| 1     | `0x01`           |
| 127   | `0x7F`           |
| 128   | `0x80 0x01`      |
| 300   | `0xAC 0x02`      |
| 16383 | `0xFF 0x7F`      |
| 16384 | `0x80 0x80 0x01` |

##### 7.3.1.4 ZigZag Encoding (Signed Integers)

Signed integers use ZigZag encoding to map negative values to positive values, enabling efficient varint encoding:

**Algorithm:**

```
zigzag_encode(n: i64) -> u64:
    return (n << 1) ^ (n >> 63)

zigzag_decode(n: u64) -> i64:
    return (n >> 1) ^ -(n & 1)
```

**Mapping:**

| Signed      | Unsigned   |
| ----------- | ---------- |
| 0           | 0          |
| -1          | 1          |
| 1           | 2          |
| -2          | 3          |
| 2           | 4          |
| -2147483648 | 4294967295 |
| 2147483647  | 4294967294 |

##### 7.3.1.5 Primitive Type Encodings

| Hew Type                  | Wire Type            | Encoding                       |
| ------------------------- | -------------------- | ------------------------------ |
| `bool`                    | VARINT (0)           | 0 = false, 1 = true            |
| `u8`, `u16`, `u32`, `u64` | VARINT (0)           | Unsigned LEB128                |
| `i8`, `i16`, `i32`, `i64` | VARINT (0)           | ZigZag then unsigned LEB128    |
| `f32`                     | FIXED32 (5)          | IEEE 754 single, little-endian |
| `f64`                     | FIXED64 (1)          | IEEE 754 double, little-endian |
| `string`                  | LENGTH_DELIMITED (2) | Length (varint) + UTF-8 bytes  |
| `bytes`                   | LENGTH_DELIMITED (2) | Length (varint) + raw bytes    |

##### 7.3.1.6 Composite Type Encodings

**Nested messages (wire struct):**

Encoded as LENGTH_DELIMITED. The value is the recursive HBF encoding of the nested message (payload only, no header).

```
wire struct Inner { x: i32 @1; }
wire struct Outer { inner: Inner @1; }

// Outer { inner: Inner { x: 150 } } encodes as:
// Tag: 0x0A (field 1, wire type 2)
// Length: 0x03 (3 bytes)
// Nested payload: 0x08 0x96 0x01 (field 1, varint, value 150 zigzag-encoded)
```

**Lists (repeated fields):**

Lists are encoded as: count (varint) followed by N elements.

```
wire struct Data { values: [i32] @1; }

// Data { values: [1, 2, 3] } encodes as:
// Tag: 0x0A (field 1, wire type 2)
// Length: 0x07 (total payload length)
// Count: 0x03 (3 elements)
// Element 1: 0x02 (zigzag of 1)
// Element 2: 0x04 (zigzag of 2)
// Element 3: 0x06 (zigzag of 3)
```

For primitive numeric types, elements are packed (no per-element tags). For nested messages, each element is length-prefixed.

**Enums (wire enum):**

Encoded as VARINT containing the 0-based variant index.

```
wire enum Status { Pending; Active; Completed; }

// Status::Active encodes as varint 1
```

**Optional fields:**

Optional fields use a presence byte followed by the value if present:

```
wire struct User { nickname: string? @3; }

// User { nickname: None } encodes as:
// Tag: 0x1A (field 3, wire type 2)
// Length: 0x01
// Presence: 0x00 (None)

// User { nickname: Some("alice") } encodes as:
// Tag: 0x1A (field 3, wire type 2)
// Length: 0x07
// Presence: 0x01 (Some)
// String length: 0x05
// String data: "alice"
```

##### 7.3.1.7 Unknown Fields

Decoders MUST preserve unknown fields encountered during decoding. When re-encoding a message, unknown fields MUST be included in their original encoded form. This enables forward compatibility: older code can decode, pass through, and re-encode messages containing fields added in newer versions.

Implementation: Store unknown fields as `Vec<(u32, Vec<u8>)>` mapping field numbers to raw encoded bytes.

##### 7.3.1.8 Field Ordering

**Encoding:** Fields SHOULD be written in ascending field number order for deterministic output.

**Decoding:** Decoders MUST accept fields in any order. If the same field number appears multiple times:

- For scalar fields: last value wins
- For repeated fields: values are concatenated

##### 7.3.1.9 Default Value Omission

Fields with default/zero values MAY be omitted from the encoding:

| Type          | Zero value |
| ------------- | ---------- |
| Integer types | 0          |
| Float types   | 0.0        |
| `bool`        | false      |
| `string`      | "" (empty) |
| `bytes`       | [] (empty) |
| Lists         | [] (empty) |
| Optional      | None       |

Decoders MUST treat missing fields as having their default value.

##### 7.3.1.10 Size Limits

- Maximum message size: 2^32 - 1 bytes (4 GiB)
- Maximum varint size: 10 bytes (sufficient for u64)
- Maximum nesting depth: 100 levels (implementation-defined)

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
| `wire struct`                          | JSON object with field names as keys                        |
| `wire enum`                            | JSON string (variant name)                                  |
| Optional None                          | JSON `null` or field omitted                                |
| Optional Some(v)                       | JSON value of v                                             |

##### 7.3.2.2 Field Names

JSON field names are determined by the following rules, in priority order:

1. **Per-field override** — `json("name")` wire attribute sets the exact JSON key.
2. **Struct-level convention** — `#[json(convention)]` attribute on the `wire struct` transforms all field names. Valid conventions: `camelCase`, `PascalCase`, `snake_case`, `SCREAMING_SNAKE`, `kebab-case`.
3. **Default** — field name is used as-is (no transformation).

Per-field override always wins over the struct-level convention.

```hew
#[json(camelCase)]
wire struct User {
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

Without the struct-level attribute, names are preserved exactly:

```hew
wire struct User {
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
wire enum Status { Pending; Active; Completed; }
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

Enum variant names are used as-is by default. Apply `#[json(camelCase)]` (or another convention) to the `wire enum` declaration to transform variant names consistently.

```hew
#[json(camelCase)]
wire enum Status { PendingReview; ActiveNow; Completed; }
```

```json
"activeNow"
```

#### 7.3.2a YAML Encoding — Configuration and Human-Readable Interop

YAML encoding provides human-readable serialization suitable for configuration files, Kubernetes manifests, CI pipelines, and other tooling that consumes YAML.

##### 7.3.2a.1 Mapping Rules

| Hew Type               | YAML Representation                               |
| ---------------------- | ------------------------------------------------- |
| `bool`                 | YAML boolean (`true` / `false`)                   |
| `u8`–`u64`, `i8`–`i64` | YAML integer                                      |
| `f32`, `f64`           | YAML float (`.nan`, `.inf`, `-.inf` for specials) |
| `string`               | YAML string (quoted if needed)                    |
| `bytes`                | YAML string (base64-encoded)                      |
| Lists                  | YAML sequence                                     |
| `wire struct`          | YAML mapping with field names as keys             |
| `wire enum`            | YAML string (variant name)                        |
| Optional None          | YAML `null` or key omitted                        |
| Optional Some(v)       | YAML value of v                                   |

##### 7.3.2a.2 Field Names

YAML field names follow the same priority rules as JSON:

1. **Per-field override** — `yaml("name")` wire attribute sets the exact YAML key.
2. **Struct-level convention** — `#[yaml(convention)]` attribute transforms all field names. Valid conventions: `camelCase`, `PascalCase`, `snake_case`, `SCREAMING_SNAKE`, `kebab-case`.
3. **Default** — field name is used as-is.

JSON and YAML naming can be configured independently:

```hew
#[json(camelCase)]
#[yaml(snake_case)]
wire struct DatabaseConfig {
    host_name: string @1;                       // JSON: "hostName",  YAML: "host_name"
    port_number: u16  @2;                       // JSON: "portNumber", YAML: "port_number"
    max_connections: u32 @3
        json("maxConns")                        // JSON: "maxConns" (override)
        yaml("max_conns");                      // YAML: "max_conns" (override)
}
```

YAML output:

```yaml
host_name: db.internal
port_number: 5432
max_conns: 100
```

##### 7.3.2a.3 Unknown Fields in YAML

YAML decoders SHOULD ignore unknown keys (permissive parsing), consistent with JSON behaviour.

##### 7.3.2a.4 Enum Variant Names in YAML

Same rules as JSON. Apply `#[yaml(convention)]` to the `wire enum` for bulk transformation; `yaml("name")` per-variant for individual overrides (future: variant-level overrides).

Hew provides bidirectional compatibility with external schema systems.

##### 7.3.3.1 Protocol Buffers Interop

**Generating .proto files:**

```hew
// hew.toml
[wire.export]
format = "protobuf"
output = "generated/schema.proto"
```

Or via CLI:

```bash
hew wire export --format protobuf --output schema.proto
```

**Mapping:**

| Hew                  | Protocol Buffers  |
| -------------------- | ----------------- |
| `wire struct`        | `message`         |
| `wire enum`          | `enum`            |
| `i32`                | `sint32` (ZigZag) |
| `u32`                | `uint32`          |
| `i64`                | `sint64` (ZigZag) |
| `u64`                | `uint64`          |
| `f32`                | `float`           |
| `f64`                | `double`          |
| `bool`               | `bool`            |
| `string`             | `string`          |
| `bytes`              | `bytes`           |
| `[T]`                | `repeated T`      |
| `T?`                 | `optional T`      |
| Nested `wire struct` | Nested `message`  |

**Importing .proto files:**

```hew
// Import protobuf schema and generate Hew wire types
wire import "external.proto" as external;

// Use imported types
wire struct MyMessage {
    user: external.User @1;
}
```

##### 7.3.3.2 JSON Schema Export

```bash
hew wire export --format json-schema --output schema.json
```

Generates JSON Schema (draft 2020-12) for each wire type, enabling validation in external systems.

##### 7.3.3.3 Avro Compatibility (Future)

Reserved for future implementation. Hew wire types can export to Avro schemas for integration with data processing systems.

#### 7.3.4 Encoding Selection

Encoders select format based on context:

| Context                 | Default Format |
| ----------------------- | -------------- |
| Actor-to-actor (local)  | HBF            |
| Actor-to-actor (remote) | HBF            |
| HTTP API response       | JSON           |
| File storage            | HBF            |
| Debugging/logging       | JSON           |

Explicit format selection:

```hew
let msg = MyMessage { ... };
let binary = msg.encode_hbf();      // Hew Binary Format
let json_str = msg.encode_json();   // JSON string
let json_pretty = msg.encode_json_pretty();  // Formatted JSON
```

Decoding:

```hew
let msg1 = MyMessage::decode_hbf(binary)?;
let msg2 = MyMessage::decode_json(json_str)?;
```

---

## 8. Compilation model

The Rust frontend processes source code into a typed AST, serializes it to MessagePack, and passes it to hew-codegen for MLIR generation, LLVM lowering, and native code emission. The Rust frontend is also compiled to WASM (via `hew-wasm/`) for in-browser diagnostics. Native WASM compilation is supported via `hew build --target=wasm32-wasi`, which compiles `hew-runtime` for `wasm32-wasip1` (thread-dependent modules gated out) and links with WASI libc. Actor and concurrency operations produce clear compile-time errors on WASM targets.

### 8.1 Pipeline Overview

```
Source (.hew) → hew (Rust: lex/parse/typecheck) → MessagePack AST → hew-codegen (C++: MLIRGen → MLIR → LLVM IR → native)
```

Each stage is invocable independently via compiler flags (`--no-typecheck`, `--emit-mlir`, `--emit-llvm`, `--emit-obj`).

### 8.2 Lexical Analysis

The lexer (`hew-lexer/src/lib.rs`) is implemented in Rust using the logos crate. It converts source text into a token stream. Tokens include keywords, identifiers, numeric and string literals (including raw and interpolated strings), operators, delimiters, and comments. Whitespace, newlines, and comments are filtered before parsing.

**Integer literal bases:**

Integer literals support four bases with optional `_` digit separators:

| Prefix      | Base         | Example                 | Value        |
| ----------- | ------------ | ----------------------- | ------------ |
| _(none)_    | 10 (decimal) | `255`, `1_000_000`      | 255, 1000000 |
| `0x` / `0X` | 16 (hex)     | `0xFF`, `0x1A_2B`       | 255, 6699    |
| `0o` / `0O` | 8 (octal)    | `0o377`, `0o755`        | 255, 493     |
| `0b` / `0B` | 2 (binary)   | `0b1111_1111`, `0b1010` | 255, 10      |

All bases produce the same `i64` value at parse time; the base is purely a source-level convenience.

### 8.3 Parsing

The parser (`hew-parser/src/parser.rs`) is implemented in Rust as a recursive-descent parser with Pratt precedence for expressions. It produces a typed AST (`hew-parser/src/ast.rs`) representing the full program structure: functions, actors, structs, enums, extern blocks, type aliases, and top-level expressions. The AST is serialized to MessagePack and passed to the C++ codegen backend.

### 8.4 Type Checking

Type checking is an optional pass enabled by default. The `TypeChecker` (`hew-types/src/`) walks the AST and produces a `TypeCheckOutput` containing inferred types, resolved names, and diagnostic errors. By default, type errors are reported as **warnings** and compilation proceeds. The `--Werror` flag promotes type errors to fatal errors. The `--no-typecheck` flag skips the pass entirely.

When type check output is available, it is provided to the MLIR generation stage for type-informed code generation.

### 8.5 MLIR Generation

`MLIRGen` (`hew-codegen/src/mlir/MLIRGen.cpp`) receives the MessagePack-encoded AST from the Rust frontend (deserialized by `msgpack_reader.cpp`) and translates it into MLIR using a combination of the Hew dialect and standard MLIR dialects.

**Hew dialect operations** (`hew.*`):

| Category | Operations                                                                                | Purpose                                      |
| -------- | ----------------------------------------------------------------------------------------- | -------------------------------------------- |
| Values   | `hew.constant`, `hew.global_string`, `hew.cast`                                           | Literals, string constants, type conversions |
| Structs  | `hew.struct_init`, `hew.field_get`, `hew.field_set`                                       | Struct construction and field access         |
| Actors   | `hew.actor_spawn`, `hew.actor_send`, `hew.actor_ask`, `hew.actor_stop`, `hew.actor_close` | Actor lifecycle and messaging                |
| I/O      | `hew.print`                                                                               | Polymorphic print                            |

**Standard dialects** reused: `func` (function declarations/calls), `arith` (integer/float arithmetic), `scf` (structured control flow: if, for, while), `memref` (stack allocation for mutable variables).

Enum construction uses LLVM dialect operations (`llvm.mlir.undef`, `llvm.insertvalue`) directly — no dedicated Hew dialect op is needed.

### 8.6 Code Generation

The codegen pipeline (`hew-codegen/src/codegen.cpp`) performs progressive lowering through multiple MLIR conversion passes:

1. **Hew → Standard/LLVM**: Actor ops expand to runtime function calls; struct ops become `llvm.insertvalue`/`llvm.extractvalue`; `hew.print` lowers to type-specific print calls.
2. **SCF → CF**: `scf.if`/`scf.for`/`scf.while` lower to `cf.br`/`cf.cond_br` basic blocks.
3. **Standard → LLVM**: `func.*` → `llvm.func`/`llvm.call`; `arith.*` → LLVM arithmetic; `memref.alloca` → `llvm.alloca`.
4. **LLVM dialect → LLVM IR**: Translation via `mlir::translateModuleToLLVMIR`.
5. **LLVM IR → Object**: LLVM machine code generation for the host target triple.

### 8.7 Linking

The compiler invokes the system C compiler (`cc`) to link the emitted object file with:

- `libhew_runtime.a` — the Hew runtime library from `hew-runtime/` (located automatically relative to the compiler binary, or via `--runtime-lib-dir`)
- `-lpthread` — POSIX threads (required by the runtime scheduler)
- `-lm` — math library

The result is a standalone native executable.

### 8.8 Runtime

`libhew_runtime` is a pure Rust staticlib (`hew-runtime/`) exporting C ABI functions via `#[no_mangle] extern "C"` linked into every compiled Hew program. It provides:

- **Scheduler**: M:N work-stealing scheduler with per-worker Chase-Lev deques
- **Actors**: Lifecycle management (spawn, dispatch, stop, destroy) with the dispatch signature `void (*dispatch)(void* state, int msg_type, void* data, size_t data_size)` (see §9.1.1)
- **Mailboxes**: Bounded message queues with configurable overflow policies
- **Supervision**: Supervisor trees for fault-tolerant actor hierarchies
- **Collections**: String, Vec, HashMap
- **I/O**: Timer wheels and I/O integration (epoll/kqueue/io_uring)

---

## 9. Runtime model

> **Detailed design:** See [docs/research/runtime-design.md](../research/runtime-design.md) for the
> complete M:N runtime architecture including C struct layouts, Chase-Lev deque pseudocode,
> I/O poller integration, timer wheel design, blocking pool, and shutdown protocol.

### 9.0 Scheduler Design

Hew uses an **M:N work-stealing scheduler** inspired by Go, Tokio, and BEAM:

**Thread model:**

- Worker threads (typically one per CPU core)
- Each worker has a local run queue of ready actors
- Idle workers steal from busy workers' queues
- Actors are scheduled as units (process messages until yield/await)

**Fairness guarantees (3-level preemption hierarchy):**

1. **Message budget (256 msgs/activation):** Coarse scheduler preemption — after processing 256 messages, the actor yields to the scheduler so other actors can run.
2. **Reduction budget (4000/dispatch):** The compiler inserts `cooperate` calls at loop headers and function call sites. Each operation decrements a reduction counter; when exhausted, the actor yields to the scheduler.
3. **Cooperative task yield:** When running inside a coroutine context (`s.launch`), `await` and `cooperate` trigger `coro_switch` to the next ready coroutine within the actor.

- Round-robin within priority levels
- Starvation prevention through queue aging

**Memory management:**

- Per-actor heaps for isolation (no shared memory between actors)
- RAII with deterministic destruction (no garbage collector)
- `Rc<T>` for single-actor shared ownership, `Arc<T>` for cross-actor immutable sharing
- Bulk deallocation on actor termination (entire heap freed)

**I/O integration:**

- Platform-specific event loops (epoll/kqueue/IOCP)
- io_uring support on Linux for high-performance I/O
- Separate thread pools for blocking operations
- Timer wheels for supervision windows and timeouts

### 9.1 Actor lifecycle state machine

The actor state machine governs the lifecycle of an actor instance within the runtime scheduler. This is distinct from the **task state machine** (§4.1), which governs individual tasks spawned within an actor.

**Actor states:** `Idle(0)`, `Runnable(1)`, `Running(2)`, `Stopping(3)`, `Stopped(4)`, `Crashed(5)`

**Transitions:**

```
(spawn) ───► Idle           actor created, mailbox allocated, state initialized
Idle ──────► Runnable       message arrives in mailbox or timer fires
Runnable ──► Running        scheduler picks actor for execution on a worker thread
Running ───► Idle           message budget exhausted or no more messages; yields to scheduler
Running ───► Stopping       supervisor requests shutdown, or actor calls self.stop()
Stopping ──► Stopped        cleanup finished, normal exit
Running/Idle/Stopping ──► Crashed   unrecoverable trap occurs
Crashed ───► Stopped        crash finalized, supervisor notified
```

Actors start `Idle` after spawn. There is no separate `Blocked` state — actors waiting for messages are simply `Idle` and become `Runnable` when a message arrives.

**Key distinctions from task states (§4.1):**

| Aspect          | Actor State Machine                            | Task State Machine (§4.1)                     |
| --------------- | ---------------------------------------------- | --------------------------------------------- |
| **Entity**      | Entire actor instance                          | Individual task within an actor               |
| **Managed by**  | Runtime scheduler (Level 1)                    | Actor-local coroutine executor (Level 2)      |
| **States**      | Idle/Runnable/Running/Stopping/Stopped/Crashed | Pending/Running/Completed/Cancelled/Trapped   |
| **Granularity** | One per actor                                  | Many per actor (one per `s.launch`/`s.spawn`) |

Supervisor observes actor terminal states `Stopped` or `Crashed`.

#### 9.1.1 Actor Dispatch Interface

The runtime invokes actor message handlers through a **dispatch function pointer** with the following normative signature:

```c
void (*dispatch)(void* state, int msg_type, void* data, size_t data_size);
```

| Parameter   | Type     | Description                                                                           |
| ----------- | -------- | ------------------------------------------------------------------------------------- |
| `state`     | `void*`  | Pointer to the actor's private state (heap-allocated)                                 |
| `msg_type`  | `int`    | Integer discriminant identifying the message type (corresponds to `receive fn` index) |
| `data`      | `void*`  | Pointer to the serialized message payload                                             |
| `data_size` | `size_t` | Size in bytes of the message payload                                                  |

**Requirements:**

- The dispatch function MUST be called with exactly 4 parameters. Implementations with fewer parameters are non-conforming.
- The `state` pointer MUST point to memory owned exclusively by the actor. No other actor or thread may access this memory during dispatch.
- The `data_size` parameter is REQUIRED for:
  - Safe deep-copy of message data into the actor's heap
  - Wire serialization (TLV encoding requires payload size)
  - Memory accounting per actor
- The `msg_type` value MUST correspond to the zero-based index of the `receive fn` declarations within the actor definition, in declaration order.

**Compiler-generated dispatch:**

For each actor, the compiler generates a dispatch function that switches on `msg_type` and deserializes `data` into the appropriate parameter types:

```c
// Generated for: actor Counter { receive fn increment(n: i32) { ... } receive fn get() -> i32 { ... } }
void Counter_dispatch(void* state, int msg_type, void* data, size_t data_size) {
    CounterState* self = (CounterState*)state;
    switch (msg_type) {
        case 0: Counter_increment(self, *(i32*)data); break;
        case 1: Counter_get(self, /* reply channel */); break;
    }
}
```

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

Behavior:

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

## 10. Syntax and EBNF (v0.9.0)

The complete formal grammar is maintained in two files:

- **`docs/specs/grammar.ebnf`** — Authoritative ISO 14977 EBNF grammar (the canonical reference)
- **`docs/specs/Hew.g4`** — ANTLR4 grammar derived from the EBNF, validated against example programs

Both files cover the full v0.9.0 syntax: modules, traits, closures, pattern matching, control flow, structured concurrency, actor messaging operators, concurrency expressions (select/join), generators (sync, async, cross-actor streaming), FFI, where clauses, f-string expressions, regex literals, match operators, and duration literals.

When the grammar files and this specification disagree, the parser implementation (`hew-parser/src/parser.rs`) is the authoritative source of truth.

**Implementation note:** closures use lambda lifting — captured variables are passed as extra parameters to the generated function. Full closure implementation with heap-allocated environment structs is future work.

### 10.1 Built-in Numeric Types

| Type                      | Size          | Description             |
| ------------------------- | ------------- | ----------------------- |
| `i8`, `i16`, `i32`, `i64` | 1/2/4/8 bytes | Signed integers         |
| `u8`, `u16`, `u32`, `u64` | 1/2/4/8 bytes | Unsigned integers       |
| `isize`, `usize`          | platform      | Pointer-sized integers  |
| `f32`, `f64`              | 4/8 bytes     | IEEE 754 floating point |
| `bool`                    | 1 byte        | Boolean (true/false)    |
| `char`                    | 4 bytes       | Unicode scalar value    |

**Type aliases:**

| Alias   | Resolves to | Description                   |
| ------- | ----------- | ----------------------------- |
| `int`   | `i64`       | Default integer type          |
| `uint`  | `u64`       | Default unsigned integer type |
| `byte`  | `u8`        | Single byte                   |
| `float` | `f64`       | Default floating-point type   |

Integer literals default to `int` (`i64`). Float literals default to `float` (`f64`).

All numeric types support explicit conversion methods:

```hew
// Integer → float
let x: i32 = 42;
let f: f64 = x.to_f64();      // 42.0

// Float → integer (truncates toward zero)
let pi: f64 = 3.14;
let n: i32 = pi.to_i32();     // 3

// usize ↔ i32
let len: usize = v.len();
let i: i32 = len.to_i32();
```

These are compiler intrinsics on all numeric types: `.to_i8()`, `.to_i16()`, `.to_i32()`, `.to_i64()`, `.to_u8()`, `.to_u16()`, `.to_u32()`, `.to_u64()`, `.to_f32()`, `.to_f64()`, `.to_usize()`, `.to_isize()`.

### 10.2 Operator Precedence (highest to lowest)

1. Postfix: `?`, `.field`, `(args)`, `[index]`
2. Unary: `!`, `-`, `~`, `await`
3. Multiplicative: `*`, `/`, `%`
4. Additive: `+`, `-` (`+` also concatenates strings)
5. Shift: `<<`, `>>`
6. Range: `..`, `..=`
7. Relational: `<`, `<=`, `>`, `>=`
8. Equality/Match: `==`, `!=`, `=~`, `!~` (value equality for strings; regex match)
9. Logical AND: `&&`
10. Bitwise AND: `&`
11. Bitwise XOR: `^`
12. Bitwise OR: `|`
13. Logical OR: `||`
14. Timeout: `| after`
15. Send: `<-`
16. Assignment: `=`, `+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, `^=`, `<<=`, `>>=`

### 10.3 Duration Literals

Duration literals provide a concise syntax for time values. They produce values of the `duration` type — a distinct primitive type representing time as `i64` nanoseconds:

```hew
let timeout = 100ms;     // duration: 100_000_000 nanoseconds
let interval = 5s;       // duration: 5_000_000_000 nanoseconds
let period = 1m;         // duration: 60_000_000_000 nanoseconds
let precise = 500us;     // duration: 500_000 nanoseconds (no truncation)
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

`duration` is a distinct type — it does not implicitly convert to or from integers:

```hew
let d = 5s;
let x = d + 100ms;        // OK: duration + duration → duration
let y = d * 3;             // OK: duration * int → duration
let z = d / 2;             // OK: duration / int → duration
let r = d / 500ms;         // OK: duration / duration → int
let err = d + 42;          // COMPILE ERROR: duration + int is not allowed
```

**Methods:**

| Method      | Signature            | Description                     |
| ----------- | -------------------- | ------------------------------- |
| `.nanos()`  | `fn nanos() -> i64`  | Total nanoseconds               |
| `.millis()` | `fn millis() -> i64` | Total milliseconds (truncates)  |
| `.secs()`   | `fn secs() -> f64`   | Total seconds as floating-point |

Duration literals are used with timeout expressions (`| after`), supervisor restart budgets, and any API accepting a duration:

```hew
let result = await task | after 5s;        // Timeout after 5 seconds
```

```ebnf
DurationLit = IntLit ("ns" | "us" | "ms" | "s" | "m" | "h") ;
```

---

## 11. Self-Hosting Roadmap

Hew is designed with self-hosting as a long-term goal. This section outlines the strategy and requirements for the Hew compiler to be written in Hew itself.

### 11.1 Minimum Viable Subset for Self-Hosting

The compiler requires only a subset of Hew's features. The following features are **essential**:

| Category         | Required Features                 | Used For                            |
| ---------------- | --------------------------------- | ----------------------------------- |
| **Data Types**   | i32, i64, u8, usize, bool, String | Token positions, flags, source code |
| **Collections**  | Vec<T>, HashMap<K, V>             | Token streams, symbol tables        |
| **Sum Types**    | enum with data variants           | AST nodes, IR instructions          |
| **Control Flow** | if, match, loop, for              | Dispatch, iteration                 |
| **Functions**    | First-class, closures             | Visitors, transformers              |
| **Generics**     | Basic type parameters             | Container types                     |
| **I/O**          | File read/write, stdout           | Source input, output                |
| **Memory**       | Heap allocation, Drop             | Dynamic structures                  |

The following Hew features are **NOT required** for self-hosting:

| Feature         | Why Not Needed                  |
| --------------- | ------------------------------- |
| Actors          | Compiler is single-threaded     |
| Message passing | No concurrency needed           |
| Supervisors     | No fault tolerance needed       |
| Async/await     | Synchronous processing suffices |
| Wire types      | No serialization needed         |
| Network I/O     | File-based operation            |

### 11.2 Kernel Language Concept

The "kernel language" is the minimal subset that can compile itself:

```hew
// Kernel language includes:
struct, enum, fn, impl, trait
let, var, const
if, else, match, loop, while, for, break, continue, return
// Standard operators and expressions

// Kernel does NOT include:
actor, supervisor, spawn, receive, await, wire
```

The kernel standard library includes:

- `Option<T>` and `Result<T, E>`
- `Vec<T>`, `String`, `Box<T>`
- `HashMap<K, V>`
- File I/O (`Read`, `Write`, `File`)
- Basic formatting

### 11.3 Bootstrap Chain

**Phase 1: Rust Frontend + C++ MLIR Codegen (Current)**

```
Source (.hew) → hew (Rust) → MessagePack → hew-codegen (C++/MLIR) → native
hew-codegen compiles Hew programs
```

**Phase 2: Hew Implementation (Kernel)**

```
hew-codegen (C++/MLIR) → hewcpp.hew (Hew source) → hewcpp2 (Hew binary)
hewcpp2 can compile full Hew, including itself
```

**Phase 3: Self-Sustaining**

```
hewcpp2 (Hew binary) → hewcpp.hew (Hew source) → hewcpp3 (Hew binary)
hewcpp2 and hewcpp3 should be identical (verified via hash)
```

### 11.4 Verification Strategy

**Diverse Double Compilation (DDC):**

To verify the self-hosted compiler hasn't been compromised:

1. Compile Hew compiler source with Rust compiler → Binary_R
2. Compile Hew compiler source with Hew compiler → Binary_H
3. Use Binary_R to compile Hew source → Binary_RR
4. Use Binary_H to compile Hew source → Binary_HH
5. Verify: Binary_RR == Binary_HH

If they match, neither compiler injected malicious code.

**Reproducible Builds:**

Requirements for verifiable builds:

- No timestamps embedded in binaries
- Deterministic linking order
- Fixed seeds for any "random" build decisions
- Sorted iteration over collections

### 11.5 Implementation Ordering

**Recommended porting order for compiler components:**

```
Phase 1: Foundation
├── Lexer (~1200 lines) - Pure transformation
└── AST definitions - Data structures only

Phase 2: Core Pipeline
├── Parser - Depends on Lexer + AST
└── Type definitions - Self-contained

Phase 3: Backend
├── IR definitions - Depends on AST + Types
├── IR lowering - Depends on IR + Parser
└── Code generation - Final stage

Phase 4: Driver
└── Compiler main - Ties everything together
```

### 11.6 Stdlib for Self-Hosting

Minimum standard library required (estimated ~2600 lines):

| Component       | Lines | Contents                        |
| --------------- | ----- | ------------------------------- |
| **core**        | ~500  | Option, Result, traits, mem ops |
| **alloc**       | ~800  | Vec, String, Box                |
| **collections** | ~600  | HashMap                         |
| **io**          | ~400  | Read, Write, File, BufReader    |
| **fmt**         | ~300  | Basic formatting                |

### 11.7 Backend Strategy for Bootstrap

**Recommended approach:**

1. **Phase 1 (Bootstrap):** Keep MLIR/LLVM as target
   - Already working in current compiler
   - Maximum portability
   - Leverages GCC/Clang optimization

2. **Phase 2 (Post-Bootstrap):** Consider QBE
   - Removes C compiler dependency
   - ~10K lines of C (vs millions for LLVM)
   - Designed for language bootstrapping

3. **Phase 3 (Long-term):** Optional LLVM
   - For maximum performance
   - Can be optional backend

### 11.8 WASM as Portable Bootstrap Format

Future consideration: compile the Hew compiler to WebAssembly for portable bootstrapping:

```
hewc.wasm (checked into repository)
    ↓ (runs on any WASM runtime)
hewc.wasm compiles hewc.hew → native hewc
    ↓
native hewc compiles everything
```

Benefits:

- Single artifact bootstraps all platforms
- Deterministic execution
- Auditable format
- No platform-specific trust requirements

---

## 12. "Researched outcomes" (what to build first to make Hew real)

1. **Actor + type safety baseline**: proven feasible and performant (Pony demonstrates the capability-typed actor approach can be implemented efficiently). ([tutorial.ponylang.io][1])
2. **Supervision semantics**: OTP restart categories are well-defined and battle-tested; encode them as primitives. ([Erlang.org][6])
3. **Cooperative cancellation**: structured concurrency with cooperative cancellation is a stable design pattern with clear semantics. ([docs.swift.org][3])
4. **Wire evolution invariants**: Protobuf’s “never reuse tags; reserve deleted tags” rules prevent real-world breaking changes and should be enforced by Hew tooling. ([protobuf.dev][5])

---

## 13. Minimum viable Hew (implementation plan aligned to this spec)

- **Phase A (compiler front-end)**: lexer/parser → AST → typecheck (Send/Frozen rules)
- **Phase B (runtime)**: scheduler, actor mailboxes, bounded channels, timers, TCP
- **Phase C (supervision)**: supervisor tree runtime + syntax lowering
- **Phase D (wire tooling)**: schema compiler + compatibility checker + encoder/decoder
- **Phase E (native codegen)**: LLVM backend + LTO + predictable allocation model

---

If you want this to be directly executable as an engineering project, the next most useful artifact is a “Hew Core IR” spec (the lowered form the compiler targets before LLVM), because it locks the semantics of actors, mailboxes, supervision, and cancellation independent of syntax.

[1]: https://tutorial.ponylang.io/index.html?utm_source=chatgpt.com "Pony Tutorial"
[2]: https://www.erlang.org/docs/17/design_principles/sup_princ?utm_source=chatgpt.com "Supervisor Behaviour - Restart Strategy"
[3]: https://docs.swift.org/swift-book/documentation/the-swift-programming-language/concurrency/?utm_source=chatgpt.com "Concurrency - Documentation | Swift.org"
[4]: https://protobuf.dev/programming-guides/proto3/?utm_source=chatgpt.com "Language Guide (proto 3) | Protocol Buffers Documentation"
[5]: https://protobuf.dev/best-practices/dos-donts/?utm_source=chatgpt.com "Proto Best Practices"
[6]: https://www.erlang.org/doc/apps/stdlib/supervisor.html?utm_source=chatgpt.com "supervisor — stdlib v7.2"

---

## Changelog

### v0.9.0

- **Added:** Cooperative task model. `s.launch` spawns micro-coroutines on actor thread; `s.spawn` for parallel OS threads.
- **Added:** `await actorRef`, `await all()` for event-driven actor synchronization.
- **Added:** RAII auto-close for streams/sinks via Drop.
- **Added:** `duration` as a proper type (i64 nanoseconds, type-safe arithmetic).
- **Fixed:** Task model spec contradiction (§4.3/§4.7/§4.8 unified).
- **Fixed:** Actor lifecycle states match runtime (6 states, not 8).
- **Fixed:** Cooperate described as actor-level reduction-based preemption.
- **Fixed:** TaskScope cancellation data race (bool → AtomicBool).
- **Removed:** `isolated` keyword (tautological — all actors are isolated).
- **Removed:** Template literal syntax (use f-strings only).
- **Removed:** `and`/`or` keyword operators (use `&&`/`||` only).
- **Removed:** Manual `is_cancelled()` (cancellation is automatic at safepoints).

### v0.8.0

- **`isolated` actor modifier**: `isolated actor Foo { }` declares an actor with no shared state dependencies
- **Duration literals section**: Documented `100ms`, `5s`, `1m`, `1h` syntax compiling to i64 milliseconds
- **Removed `ActorStream<Y>`**: Removed the deprecated alias; use `Stream<Y>` instead
- **Generator type inference**: Clarified that `Generator<Y>` and `AsyncGenerator<Y>` wrappers are compiler-inferred, not annotated
- **`async` keyword**: Clarified `async` is only valid as modifier on `gen fn`; standalone `async fn` is not part of the grammar
- **Module alias**: Clarified that `import std::net::http;` makes the module available as `http` (last path segment)

### v0.7.0

- **Typed handles**: `http.Server`, `http.Request`, `net.Listener`, `net.Connection`, `regex.Pattern`, `process.Child` — stdlib functions return typed handles with method/property access
- **Regex literals**: `re"pattern"` syntax creates first-class `regex.Pattern` values
- **Match operators**: `=~` and `!~` for regex matching at `==` precedence
- **Regex as first-class type**: regex values can be stored, passed as arguments, and used with `.is_match()`, `.find()`, `.replace()` methods

### v0.6.4

- **Module dot-syntax**: `http.listen()`, `fs.read()`, `os.args()` — clean module-qualified function calls
- **String methods**: `s.contains()`, `s.trim()`, `s.len()` etc. — method syntax on strings
- **String operators**: `+` for concatenation, `==`/`!=` for equality
- **Bool returns**: Predicate functions (`fs.exists`, `regex.is_match`) return `bool`
- **F-string expressions**: `f"result: {x + y}"` with full expression support
