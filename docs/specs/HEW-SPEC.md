# Hew Language Specification (audited for v0.2.0)

Hew is a **high-performance, network-native, machine-code compiled** language for building long-lived services. Its design is anchored in four proven pillars:

- **Actor isolation + compile-time data-race freedom** (Pony-style capability discipline) ([tutorial.ponylang.io][1])
- **Fault tolerance via supervision trees** (Erlang/OTP restart semantics) ([Erlang.org][2])
- **Structured concurrency with cooperative cancellation** (Swift-style model) ([docs.swift.org][3])
- **Wire contracts with enforced schema evolution rules** (Protobuf best practices) ([protobuf.dev][4])

This document specifies: goals, core semantics, type/effects model, module and trait systems, memory management, `machine` types, runtime state machines, compilation model, and an EBNF grammar sufficient to implement a working compiler and runtime.

**Release alignment note (v0.2.2):**

This document has been re-audited against the shipped compiler/runtime in
release **v0.2.0** and incrementally updated through **v0.2.2**. Where earlier
drafts described aspirational APIs, the text below now prefers what the
parser, type-checker, codegen, runtime, and shipped stdlib implement today.

Key corrections in this audit:

- `self` is no longer a Hew keyword; methods use named receivers and actors use
  bare field access plus `this` for actor self-reference
- `while let`, nested struct destructuring, generic `impl<T>`, labelled loops,
  enum-path constructors such as `Shape::Circle(5)`, and implicit generic
  monomorphization are all implemented and documented
- stdlib references now use the shipped module layout and current APIs
- speculative shared error types such as `IoError` have been removed in favour
  of the actual `Result<T, E>` model and current stdlib conventions

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
        count += n;
    }

    // Request-response: has return type, caller must await
    receive fn get() -> i32 {
        count
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
- **`receive fn` with return type** → request-response. The call produces `R` and waits for the reply. Inside `select`/`join`, the actor call is treated as an implicit concurrent reply source; writing `await` there is accepted but redundant.

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

Actor behaviours can also be defined via traits:

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

### 2.1.2 Periodic Receive Handlers

Receive handlers can be annotated with `#[every(duration)]` to create periodic timers that fire automatically at a fixed interval:

```hew
actor HealthChecker {
    let endpoint: String;
    let failures: int;

    #[every(5s)]
    receive fn check() {
        // Called automatically every 5 seconds
        failures = failures + 1;
    }

    receive fn get_failures() -> int {
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
fn read_file(path: String) -> Result<String, String> {
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
- **Shared immutable types**: `Frozen` values are the conceptual shared-immutable category. The runtime has internal `Arc`/ABI support, but v0.2.x does **not** yet expose a user-facing Hew `Arc<T>` type.
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
3. Assignment to actor fields (bare field names) is forbidden.
4. Local `var` mutations _are_ allowed — they do not escape the function.

`pure` may appear before `fn`, `gen fn`, `receive fn`, or `receive gen fn`,
and in trait method signatures:

```hew
trait Math {
    pure fn square(p: Point) -> f64;
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
| `Arc<T>`                      | _Not currently surfaced in Hew source; runtime-only support today_ |
| `Vec<T>`, `HashMap<K,V>`      | NOT `Frozen` (mutable containers)         |

> **Soundness requirement:** The compiler MUST reject as non-`Send` any type whose `Send` status cannot be determined (e.g., opaque foreign types). Foreign types are non-`Send` by default. The runtime/ABI has internal escape hatches, but there is currently **no surfaced Hew `#[send]` attribute** for user code.

### 3.3.2 The `bytes` Type

`bytes` is a standard library type (not a language primitive): a mutable, heap-allocated byte buffer — semantically a `Vec<u8>` — but with a dedicated type name:

```hew
let buf: bytes = bytes::new();
buf.push(0x48);    // push a byte value (i32)
buf.push(72);      // same as 'H' in ASCII
let n = buf.len(); // i64
let b = buf.get(0); // i32 — first byte
buf.set(1, 0xFF);   // overwrite byte at index 1
let last = buf.pop(); // i32 — removes and returns last byte
println(buf.is_empty()); // bool
println(buf.contains(72)); // bool — linear scan
```

**Methods on `bytes`:**

| Method         | Signature          | Description                     |
| -------------- | ------------------ | ------------------------------- |
| `bytes::new()` | `() -> bytes`      | Create an empty byte buffer     |
| `.push(b)`     | `(i32) -> ()`      | Append a byte                   |
| `.pop()`       | `() -> i32`        | Remove and return the last byte |
| `.get(i)`      | `(i64) -> i32`     | Get the byte at index `i`       |
| `.set(i, b)`   | `(i64, i32) -> ()` | Overwrite the byte at index `i` |
| `.len()`       | `() -> i64`        | Number of bytes                 |
| `.is_empty()`  | `() -> bool`       | True if len is 0                |
| `.contains(b)` | `(i32) -> bool`    | True if the buffer contains `b` |

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
    var data: Vec<i32> = Vec::new();

    receive fn bad_examples(other: ActorRef<Other>) {
        // Sending without move - ERROR (implicit move makes source invalid)
        other.handle(data);
        data.push(1);     // compile error: data was moved

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
pub fn hello() -> String {
    "Hello"
}
```

`greeting/greeting_helpers.hew` (peer):

```hew
pub fn target() -> String {
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

---

### 3.6 Trait System

Traits define shared behaviour that types can implement. Hew has built-in marker traits and supports user-defined traits.

**Trait declaration:**

```hew
trait Display {
    fn display(val: Self) -> String;
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
    fn display(p: Point) -> String {
        f"({p.x}, {p.y})"
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
  - Runtime-internal shared immutable handles (the planned `Arc<T>` surface is not yet exposed in Hew source)
  - Structs where all fields are `Frozen`

- `Copy` - Type is copied on assignment rather than moved.
  - Only value types (integers, floats, bool, char)
  - Small fixed-size aggregates

**Named receivers (Go-style):**

Hew uses **named receivers** instead of a `self` keyword. In trait declarations and `impl` blocks, the first parameter whose type matches the implementing type (or `Self` in traits) is the **receiver**. The receiver is what makes a function callable with dot-syntax:

```hew
trait Display {
    fn display(val: Self) -> String;       // val is the receiver (type Self)
}

impl Display for Point {
    fn display(p: Point) -> String {       // p is the receiver (type Point)
        f"({p.x}, {p.y})"
    }
}
```

The compiler identifies the receiver by matching the parameter type against the impl target. The parameter name is chosen by the programmer — use short, descriptive names (`p` for Point, `v` for Vec, `iter` for Iterator, etc.).

**Calling methods:**

```hew
let p = Point { x: 1.0, y: 2.0 };
p.display();    // desugars to Point::display(p) — p is consumed (by-value)
// p is no longer valid here
```

**In `impl` blocks for structs/enums (by-value):**

The receiver is passed by value — the receiver is consumed (ownership transfer):

- `fn display(p: Point)` takes `p` by value in an `impl` for `Point`
- The caller gives up ownership of the value
- After calling a consuming method, the original binding is no longer usable

**In actor `receive fn` and `fn` methods (bare field access):**

Actors do not use receivers at all. Actor handler methods access fields by their bare names — no prefix, no receiver parameter. The actor's persistent state is implicitly available:

```hew
actor Counter {
    var count: i32 = 0;
    receive fn increment() {
        count += 1;  // bare field access — actor persists after handler returns
    }
}
```

There is no `&self` or `&mut self` syntax — Hew does not have references in its surface syntax (see §3.4.1). The by-value vs implicit-state distinction is determined by the context (struct `impl` vs actor body), not by annotation.

**The `this` keyword (actor self-reference):**

Inside an actor body, the `this` keyword provides a read-only handle to the actor itself. Use `this` when you need to pass the actor's reference to another actor or store it:

```hew
actor Worker {
    var manager: ActorRef<Manager>;

    receive fn register(mgr: ActorRef<Manager>) {
        manager = mgr;
        mgr.add_worker(this);  // pass this actor's reference to the manager
    }
}
```

`this` is:

- Available only inside actor bodies (`receive fn` and `fn` methods)
- Read-only — you cannot assign to `this`
- Of type `ActorRef<Self>` — a sendable handle to the enclosing actor
- Not valid in struct `impl` blocks or free functions

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
      var count: i32 = 0;

      receive fn update(count: i32) {
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
fn broadcast<T: Send>(message: T, recipients: Vec<ActorRef<Receiver>>) {
    for recipient in recipients {
        recipient.handle(message.clone());
    }
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
    fn drop(val: Self);
}

type FileHandle {
    fd: i32;
}

impl Drop for FileHandle {
    fn drop(fh: FileHandle) {
        close(fh.fd);
    }
}
```

**Drop guarantees:**

- `drop()` runs exactly once per value
- `drop()` runs at a predictable point (scope exit or explicit drop)
- Drop order: fields are dropped in declaration order
- Nested drops: outer struct drops, then each field drops

**When drop runs:**
| Situation | Drop behaviour |
|-----------|---------------|
| Variable goes out of scope | `drop()` called immediately |
| Value is moved | No drop at original location |
| `Rc<T>` refcount reaches zero | `drop()` called on inner `T` |
| Actor terminates | All owned values dropped, then heap freed |

#### 3.7.4 Indirect Enums (Recursive Data Types)

Enum types cannot normally reference themselves because inline storage would require infinite size. The `indirect` modifier enables recursive data types by heap-allocating enum values behind a pointer.

```hew
indirect enum Expr {
    Lit(Int);
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
fn eval(e: Expr) -> Int {
    match e {
        Lit(n) => n,
        Add(l, r) => eval(l) + eval(r),
        Neg(inner) => 0 - eval(inner),
    }
}
```

**Restrictions:**

- `indirect` can only be used with `enum` declarations, not `type` (struct)
- Only enums that contain self-referential variants benefit from `indirect`

#### 3.7.5 Reference Counting (Rc and Arc)

**`Rc<T>` — single-actor reference counting:**

- Non-atomic refcount (fast, single-threaded)
- Cannot cross actor boundaries (does not implement `Send`)
- Use for shared ownership within one actor

```hew
let data: Rc<LargeStruct> = Rc::new(expensive_computation());
let alias = data.clone();  // refcount++, no data copy
// data and alias share the same LargeStruct
```

**Runtime note: internal `Arc` support exists, but Hew source does not expose `Arc<T>` yet.**

- The runtime contains ABI/runtime machinery for atomically reference-counted data
- The intended surfaced rule remains “only deeply immutable data is shareable”
- Until a language-level `Arc<T>` lands, user code should model cross-actor sharing via owned messages / actor state instead of `Arc` syntax

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
| Runtime-internal `Arc` | ABI-only today | Atomic | Internal/runtime implementation detail until surfaced |

#### 3.7.6 Allocation Surface (not yet public)

Earlier drafts of this spec described a user-visible allocator interface with
types such as `GlobalAllocator`, `ArenaAllocator`, and `PoolAllocator`, plus
collection constructors such as `Vec::new_in(arena)`. That surface is **not**
part of Hew v0.2.0.

Today, the public collection APIs use the default runtime allocator. In user
code, prefer the shipped `Vec::new()` API or collection literals such as
`[1, 2, 3]`. The arena discussion in the next section describes
compiler/runtime optimization strategies, not a stable user-facing allocator
API.

#### 3.7.7 Compiler Optimizations (Implementation Details)

The compiler may apply memory optimizations that are **invisible to user semantics**. Users always see RAII behaviour; optimizations affect only performance.

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

**Important:** These optimizations do not change program behaviour. A correct program produces identical results with or without optimizations.

#### 3.7.8 Memory Safety Guarantees

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
        if let Some(parent) = parent.upgrade() {
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
5. **Expression or block argument.** The `defer` keyword takes a single expression (typically a function call) or a block containing multiple statements:

```hew
// Single expression:
defer close(handle);

// Block with multiple cleanup steps:
defer {
    flush(handle);
    close(handle);
    println("resource released");
}
```

Block defers follow the same LIFO and early-return semantics as expression defers.

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

- All methods have a named receiver parameter of type `Self`
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
    fn next(iter: Self) -> Option<Self::Item>;
}

trait Container {
    type Item;
    type Iter: Iterator[Item = Self::Item];

    fn iter(c: Self) -> Self::Iter;
    fn len(c: Self) -> usize;
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
- **Return type inference with `-> _`**: A function annotated with `-> _` has its return type inferred from the checked body — first from the trailing expression when present, otherwise from the checker-resolved signature
- **Explicit annotations when ambiguous**: If types cannot be inferred, the compiler requires explicit annotations

**Return type inference:**

The `-> _` annotation requests that the compiler infer the return type from the checked function body. When a trailing expression is present, that expression supplies the return type; otherwise Hew uses the checker-resolved signature for that body. This also applies to trait default methods with bodies. If the checker cannot resolve a concrete return type where one is required, `-> _` is rejected. This is distinct from omitting `->` entirely, which means the function returns void (unit):

```hew
fn add(a: i32, b: i32) -> _ { a + b }  // inferred: -> i32
fn greet(name: string) -> _ { "hello {name}" }  // inferred: -> string
fn noop() { }  // no -> at all: returns void
```

`-> _` is allowed only on functions or methods with bodies, including trait default methods with bodies. It cannot be used on `extern fn` declarations or on bodyless trait method signatures.

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
let strings = map([1, 2, 3], (x) => x.to_string());  // x: i32 inferred
```

**Actor message type inference:**

Actor message handlers provide rich typing context:

```hew
actor Calculator {
    var result: i32 = 0;

    // receive fn signature provides context for message arguments
    receive fn apply_operation(op: fn(i32, i32) -> i32, value: i32) {
        result = op(result, value);
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
let results = process([1, 2, 3], (x) => {
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

fn safe_read(fd: i32, buf: *var u8, count: usize) -> Result<usize, String> {
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
    pub fn open(path: String) -> Result<File, String> {
        let c_path = path.to_c_string();
        let fd = unsafe { open(c_path.as_ptr(), O_RDONLY) };
        if fd < 0 {
            Err("open failed")
        } else {
            Ok(File { fd })
        }
    }
}

impl Drop for File {
    fn drop(f: File) {
        unsafe { close(f.fd); }
    }
}
```

---

### 3.10 Standard Library Architecture

Hew ships its standard library as Hew source under `std/`. Modules are imported
by path (`import std::math;`, `import std::net::dns;`,
`import std::collections::hashset;`) and most high-level APIs are defined in
those source modules rather than by a separate metadata system.

#### 3.10.1 Library Tiers

The current release does **not** expose a user-visible `core`/`alloc`/`std`
tier split in Hew source. Instead, the shipped library is organised by module
path.

Commonly used modules in v0.2.0 include:

- Core types and builtins: `Option<T>`, `Result<T, E>`, `Vec<T>`, `String`,
  `HashMap<K, V>`, `print`, `println`, `panic`
- Collections and helpers: `std::collections::hashset`, `std::deque`,
  `std::iter`, `std::sort`
- System and I/O: `std::fs`, `std::io`, `std::os`, `std::path`,
  `std::process`, `std::stream`
- Networking: `std::net`, `std::net::http`, `std::net::dns`,
  `std::net::tls`, `std::net::url`, `std::net::quic`, `std::net::websocket`
- Data encoding and formatting: `std::encoding::xml`, `std::encoding::json`,
  `std::encoding::yaml`, `std::encoding::toml`, `std::encoding::csv`,
  `std::encoding::msgpack`, `std::fmt`
- Utilities: `std::math`, `std::testing`, `std::time`, `std::text::regex`,
  `std::text::semver`

#### 3.10.2 Core Traits

The language supports user-defined traits, associated types, and named
receivers. The current stdlib in v0.2.0 does **not** ship a full generic
iterator-trait hierarchy yet; modules such as `std::iter` expose concrete helper
functions instead.

The following traits are representative of the current trait style:

```hew
trait Display {
    fn display(val: Self) -> String;
}

trait Drop {
    fn drop(val: Self);
}
```

#### 3.10.3 Core Types

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

Any error type `E` may be used with `Result<T, E>`. In practice, the current
stdlib mixes `Result<T, String>` with sentinel-value APIs (for example, empty
strings or `-1` on failure) depending on the module. There is no shared
stdlib-wide `IoError` or `AllocError` family in v0.2.0.

**String and Vec** are built-in generic/runtime-backed types with dot-syntax
methods:

```hew
type String {}
type Vec<T> {}

impl<T> Vec<T> {
    fn new() -> Vec<T>;
    fn push(v: Vec<T>, item: T);
    fn pop(v: Vec<T>) -> Option<T>;
    fn len(v: Vec<T>) -> i64;
    fn get(v: Vec<T>, index: i64) -> T;
}
```

Commonly used string operations include `+`, `==`, `!=`, `.len()`,
`.contains()`, `.trim()`, `.replace()`, `.split()`, `.lines()`,
`.is_digit()`, `.is_alpha()`, and `.is_alphanumeric()`.

`HashMap<K, V>` is also built in. In v0.2.0, `HashMap.get()` returns
`Option<V>`.

**Map literal syntax** — a `HashMap<K, V>` can be constructed inline with
brace-colon syntax.  The parser disambiguates `{` as a map literal when the
first token after `{` is a `StringLit` followed by `:`:

```hew
// Inferred: HashMap<String, i32>
let scores = {"alice": 10, "bob": 20};

// Explicit type annotation drives checking; each value must match V
let env: HashMap<String, String> = {
    "HOST": "localhost",
    "PORT": "8080",
};

// Trailing comma is allowed
let flags = {"debug": true, "verbose": false,};

// Empty block {} coerces to HashMap<K,V> when the expected type is known
let empty: HashMap<String, i32> = {};
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

Available `HashMap` methods in v0.2.0:

| Method                    | Returns         | Description                      |
| ------------------------- | --------------- | -------------------------------- |
| `HashMap::new()`          | `HashMap<K,V>`  | Create empty map                 |
| `m.get(key)`              | `Option<V>`     | Look up a key                    |
| `m.insert(key, value)`    | `()`            | Insert or overwrite              |
| `m.remove(key)`           | `bool`          | Remove a key; true if present    |
| `m.contains_key(key)`     | `bool`          | Test membership                  |
| `m.len()`                 | `i64`           | Number of entries                |
| `m.is_empty()`            | `bool`          | True if no entries               |

#### 3.10.4 Shipped Collections, I/O, and Utility Modules

The current release exposes concrete stdlib modules rather than a large trait
hierarchy. Representative APIs include:

```hew
import std::collections::hashset;
import std::channel::channel;
import std::deque;
import std::encoding::xml;
import std::fmt;
import std::io;
import std::iter;
import std::math;
import std::net::dns;
import std::net::tls;
import std::sort;
import std::testing;

let ints: Vec<int> = Vec::new();
let set: HashSet<int> = HashSet::new();
let dq = deque.new();
let (tx, rx) = channel.new(8);
let root = xml.parse("<root/>");

println(math.abs(-5));
println(fmt.to_hex(255));
println(iter.sum(ints));
testing.assert_true(true);
println(dns.lookup_host("localhost"));
let tls_stream = tls.connect("example.com", 443);
println(io.read_all());
```

Important current details:

- `std::io` currently provides plain functions (`read_line`, `write`,
  `write_err`, `read_all`), not `Read`/`Write`/`BufRead` traits
- `std::collections::hashset` currently lowers the supported surface forms
  `HashSet<int>` and `HashSet<String>` through type-specific runtime entry
  points; unsupported `HashSet<T>` usages are rejected fail-closed during
  type checking, including nested annotations, function signatures, and
  `wire enum` payloads
- `std::iter` is presently specialised to `Vec<int>` helpers such as
  `map_int`, `filter_int`, `fold_int`, `any`, `all`, and `sum`
- `std::sort` exposes concrete helpers like `sort_ints`, `sort_strings`,
  `sort_floats`, and `reverse`
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

## 3.11 `machine` Types

A `machine` is a **value type** that defines a closed set of named states, a
closed set of named events, and transition rules mapping `(State, Event)` pairs
to new states.  It compiles to a tagged union with a compiler-generated
`step()` method.  Machines are not actors — they are pure data, like enums
with per-state fields and compiler-checked transition logic.

> **Detailed specification:** See [`docs/specs/MACHINE-SPEC.md`](MACHINE-SPEC.md)
> for the full normative reference.  This section summarises the implemented
> surface in v0.2.0.

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
    // States — at least two required
    state StateA;                           // unit state (no fields)
    state StateB { field: Type; }          // state with data

    // Events — at least one required
    event EventX;                           // event with no payload
    event EventY { payload: Type; }        // event with payload

    // Transitions: on Event: Source -> Target { body }
    on EventX: StateA -> StateB;           // body-less: target constructed implicitly
    on EventX: StateB -> StateA { StateA } // explicit body returns target value
    on EventY: StateA -> StateB { StateB { field: event.payload } }

    // Wildcard — applies when no specific transition matches
    on EventX: _ -> _ { state }            // _ -> _ means "stay in current state"

    // Default handler — fallback for ALL unmatched (state, event) pairs
    default { state }
}
```

**Grammar (EBNF, from `docs/specs/grammar.ebnf`):**

```ebnf
MachineDecl   = "machine" Ident "{" { MachineItem } "}" ;
MachineItem   = MachineState | MachineEvent | MachineTransition | MachineDefault ;
MachineState  = "state" Ident ( "{" { Ident ":" Type (";" | ",") } "}" )? ";"? ;
MachineEvent  = "event" Ident ( "{" { Ident ":" Type (";" | ",") } "}" )? ";"? ;
MachineTransition = "on" Ident ":" StatePattern "->" StatePattern
                    ("when" Expr)? (Block | "{" FieldInitList "}" | ";") ;
MachineDefault = "default" (Block | ";") ;
```

### 3.11.2 Constraints

| Constraint                                  | Error if violated                          |
| ------------------------------------------- | ------------------------------------------ |
| At least two states                         | `machine_one_state` negative test          |
| At least one event                          | `machine_no_events` negative test          |
| No duplicate explicit transition per (S, E) | `machine_dup_transition` negative test     |
| No duplicate wildcard for same event        | `machine_dup_wildcard` negative test       |
| All referenced states/events must be declared | `machine_unknown_state/event` negative tests |
| All (S, E) pairs covered (exhaustiveness)   | `machine_exhaustive_fail` negative test    |

Exhaustiveness can be satisfied by: explicit `on` rules, wildcard `on` rules,
or a `default` handler.  A `default` handler alone covers all pairs that have
no other matching rule.

### 3.11.3 Transition Bodies

Inside a transition body the compiler binds two implicit names:

| Binding     | Type              | Meaning                                         |
| ----------- | ----------------- | ----------------------------------------------- |
| `state`     | source state type | Fields of the current (source) state            |
| `event`     | event struct      | Payload fields of the incoming event (if any)   |

```hew
machine Elevator {
    state Stopped { floor: Int; }
    state Moving  { from: Int; to: Int; }

    event GoTo  { floor: Int; }
    event Arrive;

    on GoTo: Stopped -> Moving {
        Moving { from: state.floor, to: event.floor }   // state.floor, event.floor
    }
    on Arrive: Moving -> Stopped {
        Stopped { floor: state.to }
    }

    default { state }
}
```

**Elided target state name** — when the target state is unambiguous, the
`TargetState { ... }` wrapper may be omitted and only the field initialiser
list is written:

```hew
on Work: Active -> Active { count: state.count + event.amount }
// equivalent to:
// on Work: Active -> Active { Active { count: state.count + event.amount } }
```

**Body-less shorthand** — when a transition has no body, the compiler
constructs the target state's zero-field (unit) variant automatically:

```hew
on Toggle: Off -> On;   // equivalent to: on Toggle: Off -> On { On }
```

### 3.11.4 Guard Conditions (`when`)

A transition may carry a boolean guard expression after the target state name:

```hew
on Request: Allowing -> Allowing when state.tokens > 1 {
    Allowing { tokens: state.tokens - 1 }
}
on Request: Allowing -> Throttled when state.tokens <= 1;
```

Guards are evaluated in declaration order.  The first transition whose event
and source-state match *and* whose guard (if present) evaluates to `true` fires.
If no guarded transition matches, evaluation falls through to wildcard rules and
then to `default`.

### 3.11.5 Wildcard Transitions and Priority

`_` in the source position matches any state.  `_` in the target position means
"return a value of the machine type" (any variant, not a specific one).  The
conventional identity pattern `on E: _ -> _ { state }` keeps the current state
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
| `m.state_name()`            | Returns the current state name as `String`                        |
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

### 3.11.8 Type System Integration

- A machine type is a nominal type; it does not implicitly unify with any
  `enum` or other machine.
- Machines satisfy `Send` if all their state fields satisfy `Send`
  (same rule as structs).
- Machines can be used as type parameters wherever the bound permits.
- Generics over machines are not yet supported in v0.2.0 (non-goal).

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

| Method   | Signature                                         | Description                                         |
| -------- | ------------------------------------------------- | --------------------------------------------------- |
| `launch` | `fn launch<T>(s: Scope, f: fn() -> T) -> Task<T>` | Launch a cooperative task (coroutine) in this scope |
| `spawn`  | `fn spawn<T>(s: Scope, f: fn() -> T) -> Task<T>`  | Spawn a parallel task (OS thread) in this scope     |
| `cancel` | `fn cancel(s: Scope)`                             | Request cancellation of all tasks in this scope     |

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

Cancellation is an **expected** outcome (it is explicitly requested via
`s.cancel()`) and MUST be modeled as a recoverable error, not a trap. The
current release does not expose a named `CancellationError` type in source;
callers should handle the `Err(...)` branch of the `await` result. Traps are
reserved for unexpected, unrecoverable failures (Section 2.2).

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
fn read_config(path: String) -> Result<Config, String> {
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
                // s.spawn: data passed explicitly — task body cannot access actor state
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

`await actor` installs a monitor on the target actor and blocks (via condvar) until the actor reaches a terminal state (`Stopped` or `Crashed`). This is event-driven — no polling or busy-waiting.

**Close and await in one step:**

```hew
await close(actor);
```

For finite actors, `await close(actor)` is shorthand for `close(actor); await actor;`: it closes the mailbox to new messages and then waits for the actor to finish draining its remaining work.

**Read-after-send barrier:**

```hew
counter.increment();
counter.increment();
let count = await counter.get_count();
```

For actor request/reply handlers, an awaited read acts as a barrier. The `get_count()` ask is enqueued after the earlier sends, so its reply observes all prior messages from the same sender.

**Design rationale:**

These primitives replace `sleep_ms()` patterns with deterministic, event-driven synchronization. `await actor` is zero-cost when the actor has already stopped, `await close(actor)` removes shutdown boilerplate for finite actors, and awaited reply handlers provide a simple mailbox barrier without polling.

### 4.11 Select and Join Expressions

Hew provides two built-in concurrency expressions for coordinating multiple asynchronous operations. These are expressions — they produce values — and integrate with Hew's structured concurrency and actor models.

#### 4.11.1 `select` Expression

The `select` expression waits for the first of several actor request/reply operations to complete, then evaluates the corresponding arm. Remaining operations are cancelled.

**Syntax:**

```hew
let result = select {
    count from counter.get_count() => count * 2,
    data from worker.get_data() => data.len,
    after 1.seconds => -1,       // timeout arm
};
```

**Semantics:**

- Each arm starts an actor request/reply operation.
- The first operation to complete wins; its binding is made available to the `=>` expression.
- All other operations are cancelled cooperatively.
- The `from` keyword binds the result of the operation to the identifier.
- An `after` arm provides a timeout — if no operation completes within the given duration, the timeout arm evaluates.
- All arm result expressions must have the same type `T`. The `select` expression has type `T`.
- Each non-timeout source must be an actor receive handler call with a return type. An explicit `await` is accepted for backward compatibility but is redundant inside `select`.

**Type rules:**

```
select {
    p1 from actor1.compute() => r1,
    p2 from actor2.compute() => r2,
    after d => r3,
} : T
where actor1.compute(): A, actor2.compute(): B, r1: T, r2: T, r3: T
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

Each branch must be an actor receive handler call with a return type. An explicit `await` is accepted for backward compatibility but is redundant inside `join`.

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

A **generator** is a function that produces a sequence of values lazily, suspending after each `yield` and resuming when the consumer requests the next value. Generators compile using LLVM coroutines — each `yield` becomes a suspend point, and the generator's entire local state (including loop variables) is automatically preserved in a heap-allocated coroutine frame. This means `yield` works correctly inside `while`, `for`, and `loop` constructs.

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
    fn next(g: Generator<Y>) -> Option<Y>;
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

> **Note:** The `async` keyword is ONLY valid as a modifier on `gen fn`. Standalone `async fn` declarations have no specified semantics in the actor model (actors are inherently concurrent via message passing) and are not part of the Hew grammar. Use `async gen fn` to create async generators, or use actors and `receive fn` for async behaviour.

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
    async fn next(g: AsyncGenerator<Y>) -> Option<Y>;
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
        db = connect(conn_string);
    }

    // Streaming receive: yields rows lazily to the caller
    receive gen fn query(sql: String) -> Row {
        let cursor = db.execute(sql);
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
        for event in event_log {
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
    fn next(iter: Self) -> Option<Self::Item>;
}

// Asynchronous iterator protocol
trait AsyncIterator {
    type Item;
    async fn next(iter: Self) -> Option<Self::Item>;
}

// Generator — an Iterator backed by a coroutine
trait Generator: Iterator {
    fn resume(g: Self) -> GeneratorState<Self::Item>;
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

| Strategy       | Behaviour                                                           |
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

    // Access children by declared name (preferred)
    let w = pool.worker1;              // Typed: compiler knows w is a Worker
    w.tick();

    // Or access by index (legacy)
    let w2 = supervisor_child(pool, 1);
    w2.tick();

    supervisor_stop(pool);              // Graceful shutdown
}
```

- `spawn SupervisorName` — creates and starts the supervisor with all declared children
- `sup.child_name` — named child access via field syntax. The compiler resolves the child name to its index at compile time and returns a fully typed `ActorRef` for the child's actor type. The child name must match one of the `child` declarations in the supervisor definition.
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

Every actor mailbox has a bounded capacity and an overflow policy that determines behaviour when the mailbox is full.

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

    receive fn update_price(symbol: String, price: f64) {
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
- `Stream<String>` / `Sink<String>` are convenience text ABI wrappers over the same bounded channel contract.
- Core `.next()` / `.write()` operations are blocking and backpressured; this section makes no nonblocking promises.
- EOF means **end-of-stream only**. Zero-length `bytes` values and empty `String` values are valid data items.
- `sink.close()` or dropping a sink produces graceful EOF after buffered items drain.
- `stream.close()` or dropping a stream is local cancel/discard of unread items.
- Stage-1 errors cover constructor/open failures only. Transport/runtime read/write errors after open remain wrapper-specific and out of scope here.

Both handle types are `Send` (safe to pass to other actors), opaque (backed by a vtable), and not `Clone`.

#### 6.5.1 Current `std::stream` surface

```hew
import std::stream;

// Canonical in-memory bounded bytes pipe
let (bytes_sink, bytes_stream) = stream.bytes_pipe(16);

// Convenience text pipe
let (text_sink, text_stream) = stream.pipe(16);

// Current file helpers remain text-only in this slice
let file_in  = stream.from_file("notes.txt")?;  // Result<Stream<String>, String>
let file_out = stream.to_file("out.txt")?;      // Result<Sink<String>, String>
```

`from_file()` / `to_file()` are intentionally unchanged in this slice: they
remain `Stream<String>` / `Sink<String>`. No file-adapter migration is implied
by this contract freeze.

#### 6.5.2 Current operations

```hew
// Pull items
match bytes_stream.next() {
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
this slice is that `lines()` remains `Stream<String> -> Stream<String>` today;
no `Stream<bytes>` `lines()` surface is promised here.

#### 6.5.3 Lifecycle Rules

- Closing or dropping a `Sink` signals graceful EOF to the paired `Stream`.
- Closing or dropping a `Stream` discards unread local data and releases the underlying handle.
- Streams and sinks implement `Resource`/`Drop` and **auto-close on scope exit** (RAII). Explicit `.close()` is available for early release but is not required.
- Types holding OS resources (streams, sinks, file handles) implement `Resource`/`Drop` and are **never arena-allocated**.

#### 6.5.4 Relation to Actor Streams

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

The Rust frontend processes source code into a typed AST, serializes it to MessagePack, and passes it to Hew's embedded C++ codegen for MLIR generation, LLVM lowering, and native code emission. The Rust frontend is also compiled to WASM (via `hew-wasm/`) for in-browser diagnostics. Native WASM compilation is supported via `hew build --target=wasm32-wasi`, which compiles `hew-runtime` for `wasm32-wasip1` (thread-dependent modules gated out) and links with WASI libc. Actor and concurrency operations produce clear compile-time errors on WASM targets.

### 8.0 WASM32 target capabilities

**Works on wasm32-wasi:**

- Basic actors (`spawn`, `send`, `receive`, `ask/await`) and message passing
- Generators/async streams plus pattern matching and algebraic data types
- Arithmetic, collections, and general-purpose stdlib modules
- HTTP/TCP clients and servers routed through WASI sockets

**Unavailable on wasm32-wasi (native-only features):**

- Supervision trees (`supervisor` declarations and `supervisor_*` helpers)
- Actor `link` / `monitor` fault-propagation APIs
- Structured concurrency scopes (`scope {}`, `scope.launch`, `scope.await`, `scope.cancel`)
- Scope-spawned `Task` handles that rely on scoped schedulers
- `select {}` expressions that wait on multiple mailboxes concurrently

These operations require preemptive OS threads, which the current WASM runtime does not expose. When you compile with `--target=wasm32-wasi`, the type checker emits warnings for these constructs and codegen fails with grouped diagnostics if they reach lowering. Prefer the basic actor primitives above or run the program on a native target when advanced supervision is required.

### 8.1 Pipeline Overview

> **Visual diagrams:** See [`docs/diagrams.md`](../diagrams.md) for Mermaid sequence diagrams and flowcharts of the compilation pipeline and MLIR lowering stages.

```
Source (.hew) → hew (Rust: lex/parse/typecheck) → MessagePack AST → embedded codegen (C++: MLIRGen → MLIR → LLVM IR → native)
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

The parser (`hew-parser/src/parser.rs`) is implemented in Rust as a recursive-descent parser with Pratt precedence for expressions. It produces a typed AST (`hew-parser/src/ast.rs`) representing the full program structure: functions, actors, structs, enums, extern blocks, type aliases, and top-level expressions. The AST is serialized to MessagePack and passed to the embedded C++ codegen backend.

### 8.4 Type Checking

Type checking is an optional pass enabled by default. The `TypeChecker` (`hew-types/src/`) walks the AST and produces a `TypeCheckOutput` containing inferred types, resolved names, and diagnostic errors. Type errors are fatal by default. The `--no-typecheck` flag skips the pass entirely.

When type check output is available, it is provided to the MLIR generation stage for type-informed code generation.

### 8.5 MLIR Generation

`MLIRGen` (`hew-codegen/src/mlir/MLIRGen.cpp`) receives the MessagePack-encoded AST from the Rust frontend (deserialized by `msgpack_reader.cpp` inside the embedded backend) and translates it into MLIR using a combination of the Hew dialect and standard MLIR dialects.

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
- `Rc<T>` for single-actor shared ownership; cross-actor sharing is currently expressed with owned messages / actor state rather than surfaced Hew `Arc<T>` syntax
- Bulk deallocation on actor termination (entire heap freed)

**I/O integration:**

- Platform-specific event loops (epoll/kqueue/IOCP)
- io_uring support on Linux for high-performance I/O
- Separate thread pools for blocking operations
- Timer wheels for supervision windows and timeouts

### 9.1 Actor lifecycle state machine

> **Visual diagrams:** See [`docs/diagrams.md`](../diagrams.md) for state machine diagrams of actor lifecycle, supervisor, and distributed node states.

The actor state machine governs the lifecycle of an actor instance within the runtime scheduler. This is distinct from the **task state machine** (§4.1), which governs individual tasks spawned within an actor.

**Actor states:** `Idle(0)`, `Runnable(1)`, `Running(2)`, `Stopping(3)`, `Stopped(4)`, `Crashed(5)`

**Transitions:**

```
(spawn) ───► Idle           actor created, mailbox allocated, state initialized
Idle ──────► Runnable       message arrives in mailbox or timer fires
Runnable ──► Running        scheduler picks actor for execution on a worker thread
Running ───► Idle           message budget exhausted or no more messages; yields to scheduler
Running ───► Stopping       supervisor requests shutdown, or actor calls stop()
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

Hew ships built-in tooling for debugging and profiling actor programs without
requiring external agents or separate SDK instrumentation.

### 10.1 `hew debug` — interactive debugger

`hew debug file.hew [-- args...]` compiles the program with full debug
information (no optimisation, no stripping) and launches it immediately under
the system debugger:

- **macOS / Linux with LLDB:** uses `lldb -- <binary> [args]`
- **Linux with GDB:** uses `gdb --args <binary> [args]`

If a Hew helper script (`hew_lldb.py` / `hew-gdb.py`) is found in
`share/hew/` next to the installed `hew` binary (or in `scripts/debug/`
relative to the development checkout), it is loaded automatically. These
scripts improve pretty-printing of Hew types inside the debugger.

```sh
hew debug myapp.hew -- --config prod.toml
```

### 10.2 Built-in runtime profiler (`HEW_PPROF`)

Every compiled Hew binary includes a dormant profiler that activates when the
`HEW_PPROF` environment variable is set:

| `HEW_PPROF` value | Behaviour |
|---|---|
| `auto` or `1` (Unix) | Binds a per-user unix domain socket; auto-discovered by `hew-observe` |
| `:6060` or `host:port` | Binds a TCP listener on the given address |
| *(unset)* | Profiler is disabled; zero overhead |

The profiler exposes a live HTTP JSON API with scheduler metrics, per-actor
mailbox depths, memory allocation stats, and time-series history. It runs on
a dedicated OS thread and does not interfere with the actor scheduler.

The easiest way to enable it is via `hew run --profile`:

```sh
hew run myapp.hew --profile
```

This injects `HEW_PPROF` into the compiled child process (no-op if already
set). The value is platform-dependent:

- **Unix:** `HEW_PPROF=auto` — per-user unix socket, auto-discovered by
  `hew-observe`
- **Non-Unix:** `HEW_PPROF=:6060` — TCP listener on port 6060 (`auto` is not
  supported on non-Unix; the runtime would silently skip the profiler)

For an already-compiled binary:

```sh
HEW_PPROF=auto ./myapp          # unix socket, auto-discovered
HEW_PPROF=:6060 ./myapp         # TCP on port 6060
```

### 10.3 Profile file output (`HEW_PROF_OUTPUT`)

Set `HEW_PROF_OUTPUT` to write a profile file on program exit:

| Value | File written |
|---|---|
| `pprof` | `hew-profile.pb.gz` (pprof protobuf, compatible with `go tool pprof`) |
| `flat` | `hew-profile.txt` (human-readable flat profile) |
| `both` | Both files |

```sh
HEW_PPROF=auto HEW_PROF_OUTPUT=pprof ./myapp
go tool pprof hew-profile.pb.gz
```

### 10.4 `hew-observe` — live TUI dashboard

`hew-observe` is a terminal UI that attaches to a running program's profiler
endpoint and displays real-time data:

- Live actor count and message throughput
- Per-actor mailbox depth and processing latency
- Actor group hierarchy and supervisor trees
- Memory allocation stats
- Crash logs

**Auto-discovery (Unix):** when `HEW_PPROF=auto`, `hew-observe` finds the
running program automatically via the unix socket discovery directory:

```sh
# Terminal 1
hew run myapp.hew --profile

# Terminal 2
hew-observe
```

**Explicit TCP address:**

```sh
hew-observe --addr localhost:6060
```

**List running profilers:**

```sh
hew-observe --list
```

**Multi-node observation:**

```sh
hew-observe --addr node1:6060 --node node2:6060 --node node3:6060
```

---

## 11. Distributed computing and the Node API

Hew provides built-in distributed computing through the `Node` API. Actors on different nodes communicate transparently — message sends to a remote actor use the same syntax as local sends. The runtime handles serialization, transport, and routing automatically.

**Design principles:**

- **Location transparency:** An actor reference obtained from `Node::lookup` behaves identically to a local `spawn` reference. The caller does not need to know whether the target actor is local or remote.
- **Pluggable transport:** TCP (default) or QUIC with TLS 1.3. Selected before the node starts.
- **Gossip-based registry:** Actor names propagate across the cluster via SWIM protocol piggybacking, so `Node::lookup` can resolve actors on any connected node.

### 10.1 Node lifecycle

A distributed node is started, used, and shut down within a single program:

```hew
fn main() {
    Node::start("127.0.0.1:9000");

    let counter = spawn Counter;
    Node::register("counter", counter);

    // ... interact with the cluster ...

    Node::shutdown();
}
```

**Node states:** `Starting` → `Running` → `Stopping` → `Stopped`

The runtime maintains a single implicit current node per process. All `Node::` calls operate on this current node.

### 10.2 API reference

#### 10.2.1 `Node::start(addr: string)`

Bind the current node to a network address and begin accepting connections.

```hew
Node::start("127.0.0.1:9000");   // fixed port
Node::start("127.0.0.1:0");      // ephemeral port (OS-assigned)
```

- Creates the transport listener (TCP or QUIC, depending on `Node::set_transport`)
- Initializes the SWIM cluster membership protocol
- Spawns a background accept loop for incoming peer connections
- Transitions node state to `Running`

#### 10.2.2 `Node::connect(addr: string)`

Connect the current node to a remote peer node.

```hew
Node::start("127.0.0.1:9000");
Node::connect("127.0.0.1:9001");  // join peer
```

Once connected, registry gossip and message routing flow between the two nodes. Connections are bidirectional — either side can send messages to actors on the other.

#### 10.2.3 `Node::register(name: string, actor)`

Register a spawned actor under a human-readable name in the distributed registry.

```hew
let counter = spawn Counter;
Node::register("counter", counter);
```

- Stores the name → PID mapping in the local registry
- Queues a gossip event so remote nodes learn about the actor
- The second parameter is generic (`T`) — any actor reference is accepted
- The runtime automatically removes the name when that actor is freed or when
  the owning node shuts down

#### 10.2.4 `Node::lookup(name: string) -> T`

Look up an actor by its registered name. Returns the actor reference if found, or a zero value if not found.

```hew
let found = Node::lookup("counter");
if found != 0 {
    found.increment(10);          // fire-and-forget to remote actor
    let n = await found.get_count();  // request-response across nodes
}
```

- Checks the local registry first, then the remote names learned via gossip
- The return type is generic (`T`) — assign to a typed binding at the call site
- Remote request-response (`await`) has a 5-second timeout; when the caller expects a reply value, timeout or other remote delivery failures surface as an explicit runtime failure instead of a synthesized zero/default reply

#### 10.2.5 `Node::shutdown()`

Shut down the current node, closing all connections and cleaning up resources.

```hew
Node::shutdown();
```

- Stops the accept loop and tears down transport connections
- Leaves the SWIM cluster (notifies peers via a graceful `Left` event)
- Unregisters this node's published actor names before tearing down the local
  registry state
- Frees all node-owned memory

#### 10.2.6 `Node::set_transport(transport: string)`

Select the network transport **before** calling `Node::start`. Supported values:

| Value    | Transport                                |
| -------- | ---------------------------------------- |
| `"tcp"`  | TCP with 4-byte length framing (default) |
| `"quic"` | QUIC with TLS 1.3 encryption             |

```hew
Node::set_transport("quic");
Node::start("127.0.0.1:9000");
```

If not called, TCP is used. Calling `set_transport` after `start` has no effect on the current node.

### 10.3 Remote message dispatch

Messages sent to a remote actor are routed transparently by the runtime:

```hew
// On node A
Node::start("127.0.0.1:9000");
let counter = spawn Counter;
Node::register("counter", counter);

// On node B
Node::start("127.0.0.1:9001");
Node::connect("127.0.0.1:9000");

let remote_counter = Node::lookup("counter");
remote_counter.increment(5);              // fire-and-forget (routed to node A)
let n = await remote_counter.get_count(); // request-response (routed to node A, awaits reply)
```

**Routing rules:**

- Each actor PID encodes a 16-bit node ID and a 48-bit actor index.
- When the target node ID matches the local node, the message is delivered directly via the local scheduler.
- When the target node ID differs, the message is serialized using HBF framing (4-byte little-endian length prefix + payload) and sent over the transport to the remote node.
- Remote request-response (`await`) assigns a unique request ID, sends the request, and blocks the caller until the reply arrives (5-second timeout). When a reply value is expected, remote ask failures surface as an explicit failure instead of fabricating a zero/default reply value.

### 10.4 Cross-node registry gossip

Actor name registrations propagate across the cluster using the SWIM protocol's gossip channel:

1. `Node::register("name", actor)` queues a registry-add event.
2. The event is piggybacked on the next SWIM ping or ack message sent to peers.
3. Receiving nodes update their remote name table.
4. `Node::lookup("name")` on any node can now resolve the actor.

Registry events have a bounded dissemination count (pruned after 8 gossips). Unregister events propagate the same way when an actor is removed.

**SWIM membership states:**

| State     | Meaning                                 |
| --------- | --------------------------------------- |
| `Alive`   | Node is responding to pings             |
| `Suspect` | Node missed a direct ping               |
| `Dead`    | Node confirmed unreachable              |
| `Left`    | Node departed gracefully via `shutdown` |

### 10.5 QUIC transport

When `Node::set_transport("quic")` is used, the node communicates over QUIC with TLS 1.3:

- **Self-signed certificates** are generated automatically by default.
- **Custom certificates** can be provided via environment variables:
  - `HEW_QUIC_CERT` — PEM server certificate chain
  - `HEW_QUIC_KEY` — PEM server private key
- Message framing is identical to TCP (4-byte little-endian length prefix), layered on QUIC bidirectional streams.

### 10.6 Complete example

```hew
actor Counter {
    let count: int;

    receive fn increment(n: int) {
        count = count + n;
    }

    receive fn get_count() -> int {
        count
    }
}

fn main() {
    // Start a local node on an ephemeral port.
    Node::start("127.0.0.1:0");

    // Spawn and register an actor.
    let counter = spawn Counter;
    counter.increment(10);
    counter.increment(5);
    Node::register("counter", counter);

    // Look up the actor by name and verify it works.
    let found = Node::lookup("counter");
    println(found != 0);               // true

    let result = await counter.get_count();
    println(result);                    // 15

    await close(counter);
    Node::shutdown();
}
```

---

## 12. Syntax and EBNF (audited for v0.2.0)

The complete formal grammar is maintained in two files:

- **`docs/specs/grammar.ebnf`** — Authoritative ISO 14977 EBNF grammar (the canonical reference)
- **`docs/specs/Hew.g4`** — ANTLR4 grammar derived from the EBNF, validated against example programs

Both files cover the currently documented v0.2.0 syntax: modules, traits,
closures, pattern matching, control flow, `while let`, labelled loops,
structured concurrency, actor messaging operators, concurrency expressions,
generators, FFI, where clauses, f-string expressions, regex literals, match
operators, duration literals, `machine` declarations, and map literals.

When the grammar files and this specification disagree, the parser implementation (`hew-parser/src/parser.rs`) is the authoritative source of truth.

**Implementation note:** closures use lambda lifting — captured variables are passed as extra parameters to the generated function. Full closure implementation with heap-allocated environment structs is future work.

### 11.1 Built-in Numeric Types

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

### 11.2 Operator Precedence (highest to lowest)

1. Postfix: `?`, `.field`, `(args)`, `[index]`
2. Unary: `!`, `-`, `~`, `await`
3. Multiplicative: `*`, `/`, `%`
4. Additive: `+`, `-` (`+` also concatenates strings)
5. Shift: `<<`, `>>`
6. Bitwise AND: `&`
7. Bitwise XOR: `^`
8. Bitwise OR: `|`
9. Relational: `<`, `<=`, `>`, `>=`
10. Equality/Match: `==`, `!=`, `=~`, `!~` (value equality for strings; regex match)
11. Logical AND: `&&`
12. Logical OR: `||`
13. Range: `..`, `..=`
14. Timeout: `| after`
15. Send: `<-`
16. Assignment: `=`, `+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, `^=`, `<<=`, `>>=`

### 11.3 Duration Literals

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

`duration` is a distinct type — it does not implicitly convert to or from integers:

```hew
let d = 5s;
let x = d + 100ms;        // OK: duration + duration → duration
let y = d - 1s;            // OK: duration - duration → duration
let z = d * 3;             // OK: duration * int → duration
let w = d / 2;             // OK: duration / int → duration
let r = d / 500ms;         // OK: duration / duration → i64 (ratio)
let n = -d;                // OK: negation → duration
let err1 = d + 42;         // COMPILE ERROR: duration + int
let err2 = d * 1.5;        // COMPILE ERROR: duration * float
let err3 = d * d;          // COMPILE ERROR: duration * duration
```

**Methods:**

| Method       | Signature              | Description                    |
| ------------ | ---------------------- | ------------------------------ |
| `.nanos()`   | `fn nanos() -> i64`    | Total nanoseconds              |
| `.micros()`  | `fn micros() -> i64`   | Total microseconds (truncates) |
| `.millis()`  | `fn millis() -> i64`   | Total milliseconds (truncates) |
| `.secs()`    | `fn secs() -> i64`     | Total seconds (truncates)      |
| `.mins()`    | `fn mins() -> i64`     | Total minutes (truncates)      |
| `.hours()`   | `fn hours() -> i64`    | Total hours (truncates)        |
| `.abs()`     | `fn abs() -> duration` | Absolute value                 |
| `.is_zero()` | `fn is_zero() -> bool` | True if zero nanoseconds       |

**Constructor:**

```hew
let d = duration::from_nanos(1_000_000);   // 1ms from raw nanoseconds
```

**Timeouts:**

Duration values are required for timeout expressions (`| after`) and `select` timeouts:

```hew
let result = await task | after 5s;        // Timeout after 5 seconds
```

```ebnf
DurationLit = IntLit ("ns" | "us" | "ms" | "s" | "m" | "h") ;
```

### 11.4 Labelled Loops and Break-with-Value

Loops (`loop`, `while`, `for`) may carry an optional **label** prefixed with `@`. Labels allow `break` and `continue` to target a specific enclosing loop in nested contexts.

**Syntax:**

```hew
@outer: loop {
    @inner: while condition {
        if done {
            break @outer;      // exits the outer loop
        }
        if skip {
            continue @outer;   // continues the outer loop
        }
    }
}
```

Labels are scoped to the loop they annotate. Using an undefined label is a compile-time error.

**Break-with-value:**

`break` may carry an expression whose value becomes the result of the loop when used in a value-producing position (e.g., the last statement in a block):

```hew
var result: int = 0;
loop {
    if found {
        break result;    // the loop "returns" result via the break value
    }
}
```

The break value is stored in a compiler-managed temporary and loaded after the loop exits.

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
BreakStmt      = "break" ("@" Ident)? Expr? ";" ;
ContinueStmt   = "continue" ("@" Ident)? ";" ;
```

The lexer tokenizes `@outer`-style labels as a dedicated label token; the EBNF
above shows their surface spelling.

---

## 13. Self-Hosting Roadmap

Hew is designed with self-hosting as a long-term goal. This section outlines the strategy and requirements for the Hew compiler to be written in Hew itself.

### 12.1 Minimum Viable Subset for Self-Hosting

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

### 12.2 Kernel Language Concept

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

### 12.3 Bootstrap Chain

**Phase 1: Rust Frontend + C++ MLIR Codegen (Current)**

```
Source (.hew) → hew (Rust) → MessagePack → embedded codegen (C++/MLIR) → native
hew compiles Hew programs through its embedded MLIR backend
```

**Phase 2: Hew Implementation (Kernel)**

```
embedded codegen (C++/MLIR) → hewcpp.hew (Hew source) → hewcpp2 (Hew binary)
hewcpp2 can compile full Hew, including itself
```

**Phase 3: Self-Sustaining**

```
hewcpp2 (Hew binary) → hewcpp.hew (Hew source) → hewcpp3 (Hew binary)
hewcpp2 and hewcpp3 should be identical (verified via hash)
```

### 12.4 Verification Strategy

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

### 12.5 Implementation Ordering

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

### 12.6 Stdlib for Self-Hosting

Minimum standard library required (estimated ~2600 lines):

| Component       | Lines | Contents                        |
| --------------- | ----- | ------------------------------- |
| **core**        | ~500  | Option, Result, traits, mem ops |
| **alloc**       | ~800  | Vec, String, Box                |
| **collections** | ~600  | HashMap                         |
| **io**          | ~400  | Read, Write, File, BufReader    |
| **fmt**         | ~300  | Basic formatting                |

### 12.7 Backend Strategy for Bootstrap

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

### 12.8 WASM as Portable Bootstrap Format

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

[1]: https://tutorial.ponylang.io/index.html?utm_source=chatgpt.com "Pony Tutorial"
[2]: https://www.erlang.org/docs/17/design_principles/sup_princ?utm_source=chatgpt.com "Supervisor Behaviour - Restart Strategy"
[3]: https://docs.swift.org/swift-book/documentation/the-swift-programming-language/concurrency/?utm_source=chatgpt.com "Concurrency - Documentation | Swift.org"
[4]: https://protobuf.dev/programming-guides/proto3/?utm_source=chatgpt.com "Language Guide (proto 3) | Protocol Buffers Documentation"
[5]: https://protobuf.dev/best-practices/dos-donts/?utm_source=chatgpt.com "Proto Best Practices"
[6]: https://www.erlang.org/doc/apps/stdlib/supervisor.html?utm_source=chatgpt.com "supervisor — stdlib v7.2"

---

## Changelog

### v0.2.2 (user-facing-docs-update)

- **Updated release alignment note** — bumped framing from v0.2.0 to v0.2.2
  to match the current `Cargo.toml` workspace version.
- No semantic changes to the language specification in this revision; all
  constructs documented below remain as specified in v0.2.1.

### v0.2.1 (spec-machine-map-alignment)

- **Added §3.11 `machine` Types** — syntax, states/events/transitions, guard
  conditions (`when`), wildcard rules, `default` handler, elided target state
  name shorthand, body-less transition shorthand, the `state`/`event` bindings
  in transition bodies, generated `step()` and `state_name()` API, pattern
  matching, and actor integration.  Grounded in `e2e_machine/` and
  `e2e_negative/machine_*` codegen tests and `hew-types/tests/machine_typecheck.rs`.
- **Added map literal documentation in §3.10.3** — `{"key": value, ...}`
  syntax, inference rules, trailing-comma and empty-block coercion, and
  `HashMap` method table.  Grounded in `e2e_collections/map_literal.hew`,
  `hew-parser/src/parser.rs` (`parse_map_literal_entries`), and
  `hew-types/src/check/expressions.rs` (`synthesize_map_literal`).
- **Updated §11 intro** — added `machine` declarations and map literals to the
  list of constructs covered by `grammar.ebnf` / `Hew.g4`.

### v0.2.0

- Audited the specification against the shipped compiler/runtime and stdlib
- Documented the removal of `self` and the use of named receivers plus `this`
- Added `while let` to the syntax chapter and aligned labelled-loop grammar
- Updated standard-library documentation to match the shipped module layout
- Removed speculative shared error-type references such as `IoError`

### v0.9.1

- **Removed:** `self` keyword — no longer a keyword or special identifier in Hew
- **Added:** Named receivers (Go-style) for trait and impl methods (`fn display(p: Point)`)
- **Added:** Bare field access in actors (`count += 1` instead of `self.count += 1`)
- **Added:** `this` keyword for actor self-reference (read-only `ActorRef<Self>` handle)
- **Changed:** Variable shadowing is now a hard error (prerequisite for bare field names)
- **Changed:** `Drop` trait uses `fn drop(val: Self)` named receiver
- **Changed:** Object safety requires a named receiver of type `Self` (not `self`)
- **Changed:** All stdlib trait and impl method signatures updated to named receivers
- **Unchanged:** `Self` (capital S) type alias remains valid in trait/impl blocks

### v0.9.0

- **Added:** Cooperative task model. `s.launch` spawns micro-coroutines on actor thread; `s.spawn` for parallel OS threads.
- **Added:** `await actor`, `await close(actor)`, and awaited actor reply barriers.
- **Added:** RAII auto-close for streams/sinks via Drop.
- **Added:** Duration literal suffixes (i64 nanoseconds); `duration` is a distinct primitive type.
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
- **Duration literals section**: Documented `100ms`, `5s`, `1m`, `1h` syntax compiling to i64 nanoseconds
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
