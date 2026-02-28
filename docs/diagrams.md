# Hew Compiler & Runtime Diagrams

Visual documentation of the Hew compilation pipeline, runtime architecture, and protocol formats using Mermaid diagrams. These diagrams reflect the actual codebase structure — function names, module names, constants, and data types are taken directly from source.

> **Rendering:** These diagrams use [Mermaid](https://mermaid.js.org/) syntax. GitHub renders them natively in Markdown. For local viewing, use a Mermaid-compatible Markdown previewer or the [Mermaid Live Editor](https://mermaid.live/).

---

## 1. Compilation Pipeline

The compiler has two process boundaries: a **Rust frontend** (lexer → parser → type checker → serializer) and a **C++ backend** (MLIR generation → lowering → LLVM → native code). They communicate via MessagePack over stdin.

```mermaid
sequenceDiagram
    participant Source as source.hew
    participant Lexer as Lexer<br/>(hew-lexer)
    participant Parser as Parser<br/>(hew-parser)
    participant TypeChecker as Type Checker<br/>(hew-types)
    participant Serializer as Serializer<br/>(hew-serialize)
    participant Codegen as hew-codegen<br/>(C++ process)
    participant LLVM as LLVM Backend
    participant Linker as Linker (cc)

    Source->>Lexer: source text
    Note over Lexer: logos crate tokenizer
    Lexer->>Parser: token stream
    Note over Parser: recursive-descent +<br/>Pratt precedence
    Parser->>TypeChecker: AST (Program)
    Note over TypeChecker: bidirectional HM inference<br/>TypeCheckOutput: types + diagnostics
    TypeChecker->>Serializer: typed AST + type map
    Note over Serializer: enrich.rs → msgpack.rs
    Serializer->>Codegen: MessagePack binary (stdin pipe)
    Note over Codegen: ── C++ process boundary ──
    Codegen->>Codegen: msgpack_reader.cpp → AST structs
    Codegen->>Codegen: MLIRGen.cpp → Hew MLIR dialect
    Codegen->>Codegen: lowerHewDialect (75+ patterns)
    Codegen->>Codegen: Lower std → LLVM dialect (11 passes)
    Codegen->>LLVM: translateModuleToLLVMIR
    LLVM->>LLVM: O3 optimization pipeline
    LLVM->>Linker: object file (.o)
    Linker->>Linker: .o + libhew_runtime.a + -lpthread -lm
    Note over Linker: native executable
```

**Compiler flags for partial pipeline:**

- `--no-typecheck` — skip type checking
- `--emit-mlir` — stop after MLIR generation
- `--emit-llvm` — stop after LLVM IR translation
- `--emit-obj` — stop after object file emission

---

## 2. Actor Lifecycle State Machine

Governs the lifecycle of each actor instance within the runtime scheduler. Defined in `hew-runtime/src/actor.rs` as `HewActorState`. This is distinct from the per-task state machine (§4.1 in the spec).

```mermaid
stateDiagram-v2
    [*] --> Idle: hew_actor_spawn()

    Idle --> Runnable: message arrives / timer fires
    Runnable --> Running: scheduler picks actor<br/>(worker thread)
    Running --> Idle: budget exhausted /<br/>no more messages
    Running --> Stopping: self.stop() /<br/>supervisor shutdown (SYS_MSG_SUPERVISOR_STOP)
    Stopping --> Stopped: cleanup complete

    Running --> Crashed: unrecoverable trap
    Idle --> Crashed: unrecoverable trap
    Stopping --> Crashed: unrecoverable trap

    Crashed --> Stopped: crash finalized,<br/>supervisor notified (SYS_MSG_CHILD_CRASHED)

    Stopped --> [*]
```

**Preemption budget (3-level hierarchy):**

| Level             | Budget                                       | Mechanism                                                          |
| ----------------- | -------------------------------------------- | ------------------------------------------------------------------ |
| Message budget    | `HEW_MSG_BUDGET = 256` messages/activation   | Coarse scheduler preemption — yield after 256 messages             |
| Reduction budget  | `HEW_DEFAULT_REDUCTIONS = 4000` per dispatch | Compiler-inserted `cooperate` calls at loop headers and call sites |
| Cooperative yield | Per-coroutine                                | `coro_switch` on `await` within `s.launch` tasks                   |

**Actor dispatch signature** (§9.1.1):

```c
void (*dispatch)(void* state, int msg_type, void* data, size_t data_size);
```

---

## 3. MLIR Lowering Pipeline

The codegen pipeline in `hew-codegen/src/codegen.cpp` performs progressive lowering through multiple stages. The Hew dialect defines ~50 custom operations across actor, collection, generator, scope, and trait categories.

```mermaid
flowchart TD
    A["MessagePack AST<br/>(from Rust frontend via stdin)"] --> B["MLIRGen.cpp<br/>→ Hew MLIR Dialect"]

    B --> PRE["<b>Pre-Lowering Optimization</b>"]

    PRE --> C1["Canonicalize"]
    PRE --> C2["DevirtualizeTraitDispatchPass<br/><i>direct-call when vtable is static</i>"]
    PRE --> C3["Canonicalize"]
    PRE --> C4["StackPromoteDynCoercionPass<br/><i>arena.malloc → alloca for confined ptrs</i>"]

    C1 --> C2 --> C3 --> C4

    C4 --> STAGE1["<b>Stage 1: Lower Hew Dialect</b><br/>75+ ConversionPatterns<br/>(applyPartialConversion)"]

    STAGE1 --> S1_DETAIL["Actor: SpawnOp, SendOp, AskOp, StopOp …<br/>Collections: VecNewOp, HashMapInsertOp …<br/>Generators: GenCtxCreateOp, GenYieldOp …<br/>Scope/Task: ScopeCreateOp, TaskSetResultOp …<br/>Select: SelectCreateOp, SelectFirstOp …<br/>Traits: TraitDispatchOp → func.CallOp<br/>Closures: ClosureCreateOp, ClosureGetFnOp …"]

    S1_DETAIL --> STAGE2["<b>Stage 2: Lower to LLVM Dialect</b><br/>(11 core passes)"]

    STAGE2 --> P1["1. Canonicalize + CSE"]
    P1 --> P2["2. SCFToControlFlow<br/><i>scf.if/for/while → cf.br/cond_br</i>"]
    P2 --> P3["3. ConvertFuncToLLVM<br/><i>func.func → llvm.func</i>"]
    P3 --> P4["4. VtableGlobalPass<br/><i>VtableRefOp → llvm.GlobalOp + AddressOfOp</i>"]
    P4 --> P5["5. InternalLinkagePass<br/><i>hide non-main funcs</i>"]
    P5 --> P6["6. SetTailCallsPass<br/><i>hew.tail_call → LLVM tail call</i>"]
    P6 --> P7["7. ArithToLLVM"]
    P7 --> P8["8. ConvertControlFlowToLLVM"]
    P8 --> P9["9. FinalizeMemRefToLLVM"]
    P9 --> P10["10. ReconcileUnrealizedCasts"]
    P10 --> P11["11. Canonicalize (cleanup)"]

    P11 --> STAGE3["<b>Stage 3: Translate to LLVM IR</b><br/>mlir::translateModuleToLLVMIR"]

    STAGE3 --> OPT["LLVM O3 Pipeline<br/>(skipped with --debug-info)"]
    OPT --> OBJ["Object File (.o)<br/>TargetMachine::addPassesToEmitFile"]
```

**Type converter mappings** (Hew → LLVM):

| Hew Type                                            | LLVM Type                                |
| --------------------------------------------------- | ---------------------------------------- |
| `ActorRef`, `StringRef`, `Vec`, `HashMap`, `Handle` | `!llvm.ptr`                              |
| `HewTuple`                                          | `!llvm.struct<...>`                      |
| `HewArray`                                          | `!llvm.array<N x T>`                     |
| `TraitObject`                                       | `!llvm.struct<ptr, ptr>` (data + vtable) |
| `Closure`                                           | `!llvm.struct<ptr, ptr>` (fn + env)      |
| `OptionEnum`                                        | `!llvm.struct<i32, T>`                   |
| `ResultEnum`                                        | `!llvm.struct<i32, OK, ERR>`             |

---

## 4. Actor Message Flow

Shows the internal mechanics of sending a message between actors. Deep-copy semantics ensure actor isolation — no shared memory between actors.

```mermaid
sequenceDiagram
    participant Sender as Sender Actor<br/>(Running)
    participant Runtime as Runtime<br/>(hew_actor_send_by_id)
    participant Mailbox as HewMailbox<br/>(dual-queue)
    participant MPSC as MpscQueue<br/>(lock-free)
    participant Scheduler as Scheduler<br/>(sched_enqueue)
    participant Worker as Worker Thread
    participant Receiver as Receiver Actor

    Sender->>Runtime: hew_actor_send_by_id(target_id, msg_type, data, size)
    Runtime->>Runtime: deep copy data into HewMsgNode<br/>(msg_type, data, data_size)

    alt Mailbox has capacity
        Runtime->>Mailbox: hew_mailbox_send(node)
        Mailbox->>MPSC: push(node) — lock-free CAS
    else Mailbox full
        Runtime->>Runtime: apply HewOverflowPolicy<br/>(Block / DropNew / DropOld / Fail / Coalesce)
    end

    MPSC-->>Mailbox: count++

    alt Receiver is Idle
        Runtime->>Runtime: CAS state: Idle → Runnable
        Runtime->>Scheduler: sched_enqueue(actor)
        Scheduler->>Scheduler: push to global_queue<br/>(crossbeam Injector)
        Scheduler->>Worker: sched_try_wake()<br/>(Condvar notify)
    end

    Worker->>Worker: pop from local deque (LIFO)<br/>or steal from peer (FIFO)<br/>or steal_batch_and_pop from global
    Worker->>Receiver: set state → Running
    Worker->>Mailbox: hew_mailbox_try_recv()
    Mailbox->>Worker: HewMsgNode
    Worker->>Receiver: dispatch(state, msg_type, data, data_size)

    loop up to HEW_MSG_BUDGET (256)
        Worker->>Mailbox: hew_mailbox_try_recv()
        Mailbox->>Worker: next HewMsgNode (or empty)
    end

    Worker->>Receiver: set state → Idle<br/>(budget exhausted or no messages)
```

**Mailbox internals** (`hew-runtime/src/mailbox.rs`):

- Dual-queue design: fast lock-free MPSC (Vyukov queue) + slow Mutex-guarded VecDeque
- Configurable `HewOverflowPolicy`: Block, DropNew, DropOld, Fail, Coalesce
- Optional `coalesce_key_fn` for in-place message replacement

---

## 5. Runtime Architecture

Layered architecture of `libhew_runtime.a` (`hew-runtime/src/`). All layers export C ABI functions via `#[no_mangle] extern "C"`.

```mermaid
block-beta
    columns 1

    block:L5["<b>L5: Networking & Distribution</b>"]
        columns 5
        wire["wire.rs<br/>HBF encode/decode<br/>HewWireEnvelope"]
        transport["transport.rs<br/>HewTransportOps vtable<br/>HewActorRef (local/remote)"]
        connection["connection.rs<br/>Connection Manager<br/>MAX_CONNS=64"]
        hew_node["hew_node.rs<br/>HewNode<br/>HewRegistry"]
        remote_sup["remote_sup.rs<br/>Remote Supervisor"]
    end

    block:L4["<b>L4: Async & Timers</b>"]
        columns 4
        task_scope["task_scope.rs<br/>HewTask / HewTaskScope<br/>Ready→Running→Suspended→Done"]
        timer_wheel["timer_wheel.rs<br/>2-level hierarchical wheel<br/>L0: 256×1ms, L1: 64×256ms"]
        coro["coro.rs<br/>Stackful coroutines<br/>coro_switch"]
        generator["generator.rs<br/>gen fn / async gen fn"]
    end

    block:L3["<b>L3: Actors & Supervision</b>"]
        columns 4
        actor["actor.rs<br/>HewActor<br/>HewActorState (6 states)"]
        scope["scope.rs<br/>HewScope<br/>MAX_ACTORS=64"]
        actor_group["actor_group.rs<br/>Actor Groups"]
        supervisor["supervisor.rs<br/>HewChildSpec<br/>OneForOne/OneForAll/RestForOne"]
    end

    block:L2["<b>L2: Scheduling & Messaging</b>"]
        columns 3
        mailbox["mailbox.rs<br/>HewMailbox (dual-queue)<br/>HewMsgNode / HewOverflowPolicy"]
        scheduler["scheduler.rs<br/>M:N work-stealing<br/>per-worker Parker"]
        blocking["blocking_pool.rs<br/>OS thread pool<br/>for blocking ops"]
    end

    block:L1["<b>L1: Lock-free Data Structures</b>"]
        columns 3
        mpsc["mpsc.rs<br/>Vyukov MPSC queue<br/>(lock-free CAS)"]
        deque["deque.rs<br/>Chase-Lev work deque<br/>(crossbeam)"]
        semaphore["semaphore.rs<br/>Counting semaphore"]
    end

    block:L0["<b>L0: Primitives & Collections</b>"]
        columns 5
        print["print.rs"]
        string["string.rs"]
        vec["vec.rs"]
        hashmap["hashmap.rs"]
        arena["arena.rs<br/>Per-actor heap"]
    end

    L5 --> L4
    L4 --> L3
    L3 --> L2
    L2 --> L1
    L1 --> L0
```

**Key constants:**

| Constant                 | Value  | Source         |
| ------------------------ | ------ | -------------- |
| `HEW_MSG_BUDGET`         | 256    | `actor.rs`     |
| `HEW_DEFAULT_REDUCTIONS` | 4000   | `actor.rs`     |
| `HEW_MAX_WORKERS`        | 256    | `actor.rs`     |
| `HEW_SCOPE_MAX_ACTORS`   | 64     | `scope.rs`     |
| `MAX_CONNS`              | 64     | `transport.rs` |
| `MAX_FRAME_SIZE`         | 16 MiB | `transport.rs` |
| `PARK_TIMEOUT`           | 10 ms  | `scheduler.rs` |

---

## 6. Distributed Node State Machine

Governs the lifecycle of a `HewNode` in distributed mode (`hew-runtime/src/hew_node.rs`). Each node has a `node_id: u16`, a bound address, and a transport vtable.

```mermaid
stateDiagram-v2
    [*] --> STARTING: hew_node_start()

    STARTING --> RUNNING: transport.listen() succeeds,<br/>accept_thread spawned
    STARTING --> STOPPED: bind/listen failure

    RUNNING --> STOPPING: hew_node_stop()
    RUNNING --> RUNNING: hew_node_send() /<br/>try_remote_send()

    STOPPING --> STOPPED: accept_thread joined,<br/>connections closed,<br/>transport.destroy()

    STOPPED --> [*]: hew_node_free()

    note right of STARTING
        NODE_STATE_STARTING = 0
        AtomicU8 state transitions
    end note

    note right of RUNNING
        NODE_STATE_RUNNING = 1
        Accepts connections,
        routes messages via
        HewRegistry (remote_names)
    end note

    note right of STOPPING
        NODE_STATE_STOPPING = 2
    end note

    note right of STOPPED
        NODE_STATE_STOPPED = 3
    end note
```

**Node components:**

- `HewRegistry` — maps string names to remote actor IDs
- `HewTransportOps` — vtable: `connect`, `listen`, `accept`, `send`, `recv`, `close_conn`, `destroy`
- `HewActorRef` — discriminated union: `ACTOR_REF_LOCAL(0)` or `ACTOR_REF_REMOTE(1)`
- `next_peer_node` — auto-incrementing connection ID counter

---

## 7. Wire Protocol Frame Format (HBF)

The Hew Binary Format is the internal wire encoding for distributed actor messaging. Defined in `hew-runtime/src/wire.rs` and specified in §7.3.1 of the language spec.

```mermaid
packet-beta
    0-7: "H (0x48)"
    8-15: "E (0x45)"
    16-23: "W (0x57)"
    24-31: "1 (0x31)"
    32-39: "Version (0x01)"
    40-47: "Flags"
    48-55: "Payload Length [0:7]"
    56-63: "Payload Length [8:15]"
    64-71: "Payload Length [16:23]"
    72-79: "Payload Length [24:31]"
    80-95: "Field 1: Tag (varint) | Wire Type (3 bits)"
    96-111: "Field 1: Value (variable)"
    112-127: "Field N: ..."
    128-143: "Optional: CRC32C (if CHECKSUM flag set)"
```

**Header structure** (`HBF_HEADER_LEN = 10` bytes):

| Offset | Size | Field   | Value                                      |
| ------ | ---- | ------- | ------------------------------------------ |
| 0      | 4    | Magic   | `HBF_MAGIC = "HEW1"` (0x48 0x45 0x57 0x31) |
| 4      | 1    | Version | `HBF_VERSION = 0x01`                       |
| 5      | 1    | Flags   | `HBF_FLAG_COMPRESSED = 0x01` (LZ4)         |
| 6      | 4    | Length  | Payload length (little-endian u32)         |

**Wire types** (3-bit field in tag):

| Value | Name             | Constant                     | Description                      |
| ----- | ---------------- | ---------------------------- | -------------------------------- |
| 0     | VARINT           | `WIRE_TYPE_VARINT`           | Unsigned LEB128, self-delimiting |
| 1     | FIXED64          | `WIRE_TYPE_FIXED64`          | Always 8 bytes                   |
| 2     | LENGTH_DELIMITED | `WIRE_TYPE_LENGTH_DELIMITED` | Varint length prefix + bytes     |
| 5     | FIXED32          | `WIRE_TYPE_FIXED32`          | Always 4 bytes                   |

**Tag encoding:** `(field_number << 3) | wire_type`

**Envelope structure** (`HewWireEnvelope`):

```mermaid
flowchart LR
    E["HewWireEnvelope"] --> A["target_actor_id<br/>(u64)"]
    E --> M["msg_type<br/>(u16, max 65535)"]
    E --> P["payload<br/>(HewWireBuf)"]

    P --> D["data: *mut u8"]
    P --> L["len: usize"]
    P --> C["cap: usize"]
    P --> R["read_pos: usize"]
```

**Signed integers** use zigzag encoding: `zigzag(n) = (n << 1) ^ (n >> 63)`

---

## 8. Supervisor State Machine

Supervisors manage child actor lifecycles with configurable restart strategies. Defined in `hew-runtime/src/supervisor.rs`.

```mermaid
stateDiagram-v2
    [*] --> Healthy: supervisor started

    Healthy --> Restarting: ChildExit(child, reason)<br/>if restart_policy ≠ TEMPORARY

    Restarting --> Healthy: restart succeeds<br/>(strategy applied)
    Restarting --> Escalating: RestartBudgetExceeded

    Healthy --> Stopped: all children stopped normally
    Escalating --> Stopped: no parent supervisor
    Escalating --> Escalating: escalate to parent<br/>(SYS_MSG_CHILD_CRASHED)

    Stopped --> [*]

    note right of Restarting
        Strategies:
        STRATEGY_ONE_FOR_ONE (0) — restart failed child only
        STRATEGY_ONE_FOR_ALL (1) — restart all children
        STRATEGY_REST_FOR_ONE (2) — restart failed + later children

        Backoff: INITIAL_RESTART_DELAY_MS = 100
        Max: DEFAULT_MAX_RESTART_DELAY_MS = 30,000
    end note
```

**Restart policies** (`HewChildSpec.restart_policy`):

| Policy              | Value | Behavior                                |
| ------------------- | ----- | --------------------------------------- |
| `RESTART_PERMANENT` | 0     | Always restart                          |
| `RESTART_TRANSIENT` | 1     | Restart only on crash (not normal stop) |
| `RESTART_TEMPORARY` | 2     | Never restart                           |

**System messages:**

| Message                   | Value | Trigger                     |
| ------------------------- | ----- | --------------------------- |
| `SYS_MSG_CHILD_STOPPED`   | 100   | Child stopped normally      |
| `SYS_MSG_CHILD_CRASHED`   | 101   | Child crashed               |
| `SYS_MSG_SUPERVISOR_STOP` | 102   | Supervisor shutdown command |
| `SYS_MSG_EXIT`            | 103   | Exit signal                 |
| `SYS_MSG_DOWN`            | 104   | Monitored actor down        |

---

## Cross-References

- **Language specification:** [`docs/specs/HEW-SPEC.md`](specs/HEW-SPEC.md) — §8 (Compilation Model), §9 (Runtime Model)
- **Runtime source:** [`hew-runtime/src/`](../hew-runtime/src/)
- **Codegen source:** [`hew-codegen/src/`](../hew-codegen/src/)
- **Wire format spec:** HEW-SPEC.md §7.3.1 (HBF encoding)
