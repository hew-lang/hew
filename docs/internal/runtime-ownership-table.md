# Runtime ownership contracts

Ownership decisions live in the compiler's checked contracts. This document
locates those authorities; it does not maintain a second symbol inventory.
The former ladder-era table is superseded by the native HIR → SIR → physical
MIR path and the generated ABI inventory.

## Owning stages

| Boundary | Authority | Contract consumed downstream |
| --- | --- | --- |
| Source values | [`TypeFacts`](../../hew-types/src/type_facts.rs) and [`ValueClass`](../../hew-types/src/value_class.rs) | The exact concrete type determines copy, clone, destruction and resource behaviour. |
| Ordinary callable parameters | [`SemParamPassing`](../../hew-sir/src/model.rs) | Read, borrow, mutable borrow and consuming parameters are explicit. |
| Aggregate fields | [`AggregateFieldRecipe`](../../hew-sir/src/ownership.rs) | Ordered fields and their ownership come from checked type facts and exact shape descriptors. |
| Runtime operations | [`RuntimeCallFamily`](../../hew-types/src/runtime_call.rs) and SIR lowering | A selected operation carries its typed arguments, result and transfer contract. |
| Foreign resource boundaries | [`ExternOwnershipContract`](../../hew-types/src/ffi_contracts.rs) | Parameter ownership, nominal resource identity, result retention and release obligations must agree. |
| Physical storage and calls | [`PhysicalModule`](../../hew-mir/src/physical.rs) | Checked operations acquire concrete layouts, carriers and cleanup actions before LLVM emission. |
| Published C ABI | [`cabi-surface.json`](../../scripts/cabi-surface.json) | Generated symbol signatures and classifications describe the exported surface. They do not establish source-language support. |

Missing ownership facts are not evidence of borrowing. An ABI word shared by
two resource types does not make those resources interchangeable: contracts
carry their qualified nominal identity. LLVM lowering consumes these facts;
it must not recover ownership from display names or pointer shapes.

## Foreign parameter and result dispositions

The maintained symbol rows are in
[`runtime-export-classification.toml`](../../scripts/runtime-export-classification.toml).
[`hew-types/build.rs`](../../hew-types/build.rs) projects their ownership data
into the shared [`ffi_contracts`](../../hew-types/src/ffi_contracts.rs) table.

| Parameter disposition | Meaning |
| --- | --- |
| `Borrow` | The callee reads or copies the value; the caller retains its owner. |
| `Consume` | The callee receives the owner and discharges the caller's obligation. |
| `Retain` | The callee acquires an additional reference. |

| Result disposition | Meaning |
| --- | --- |
| `Fresh` | A fresh result is returned under the declared release contract. |
| `Retained` | The result carries an acquired reference. |
| `Owned` | The result has an independent owner; this does not promise a new allocation. |
| `Borrowed` | The result does not carry an independent owner. |
| `None` | There is no result ownership obligation. |

Result ownership alone does not prove a valid release. The contract also names
the release symbol, shallow or deep discharge, and measured retention:
transferred allocation, shared reference, resource transfer or unspecified.
Opaque resource results additionally require an exact nominal resource and
consuming release edge. See the validation helpers in `ffi_contracts` for the
admission rules.

## Native lifecycle boundaries

| Operation | Ownership boundary |
| --- | --- |
| Actor publication | [`hew_actor_spawn_native`](../../hew-runtime/src/actor.rs) receives initialized state and its clone/drop callbacks together. Spawn consumes that state on success and failure. |
| Message admission | [`mailbox_native`](../../hew-runtime/src/mailbox_native.rs) retains the unaccepted request until admission transfers it. Cancellation and refusal release the owners still held by the operation. |
| Reply observation | [`reply_channel_native`](../../hew-runtime/src/reply_channel_native.rs) separates completion readiness from taking the reply. An abandoned observer does not transfer cleanup to its former caller. |
| Terminal observation | [`actor_native_close`](../../hew-runtime/src/actor_native_close.rs) publishes completion after terminal state cleanup. `closed` observes; `close` also requests termination. |
| Supervisor teardown | [`supervisor`](../../hew-runtime/src/supervisor.rs) owns child teardown and retained config destruction. Completion must follow that cleanup. |
| Parked I/O | [`async_io` contracts](../../hew-types/src/runtime_call/async_io.rs) describe buffer loans and resume operations. Cancellation must make borrowed buffers quiescent before their owners are released. |

A runtime export, a checker declaration or an inventory row is not an
execution test. Native support requires the source program to pass through the
active compiler and run with the expected behaviour. Sandbox parity requires
its own successful execution.

## Refresh and verification

Run `make cabi-surface` after changing runtime exports, then validate the
result with `make verify-ffi`. `make lint` checks the generated surface and
other source contracts before publication. Keep behavioural ownership coverage
in the relevant [core acceptance cases](../../tests/core-acceptance/cases),
including success, refusal, cancellation, fault cleanup and restart where those
boundaries apply. Use `make core-safety` for the selected sanitizer cases.

Do not regenerate the retired hand-maintained ladder inventory or treat its
historical admission proposals as current compiler rules. Git retains that
history.
