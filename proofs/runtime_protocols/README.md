# Runtime ownership and concurrent protocols

These validators check production Rust for eleven core contracts and seven
concurrent models. Run `make prove-runtime-protocols`; failures include
logs and concrete counterexamples. They supplement the readiness-gate and
request-deadline proofs.

The target uses Kani 0.68.0, Rust, Python 3, and Loom 0.7.2. The separate
daily/manual workflow runs `make prove-runtime`. Normal PRs and builds do not
run the validators or require these tools. No verification code runs in Hew.

## Eleven core contracts

Kani compiles the production files directly, including private timer-wheel and
arena state through proof-only child modules. There are sixteen required
Kani harnesses: three each for timers and arenas,
two each for the general heap and supervisor pin admission/release, and one
each for the remaining Kani contracts.
Semaphore conservation is checked with Loom. Supervisor, read-slot, and
managed-budget contracts combine Kani and Loom.
Missing harnesses, unsupported reachable operations, timeouts, and solver
failures fail the target.

| Contract                                 | Checked property                                                                                                                                                                                       | Quantification and bounds                                                                                                                                                                                                                                                                                                                             |
| ---------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Retained wakers                          | Each owner holds one retained reference; clone/wake/drop cannot access a logically released target                                                                                                     | Every four-operation sequence over two owned slots, including creator release; an independent callback monitor supplies the reference-count contract                                                                                                                                                                                                  |
| Cancellation publication                 | Zero normalizes to a nonzero reason; the first published reason survives; only one requester gains publication authority                                                                               | Every valid token phase, every existing signed 32-bit reason, two arbitrary signed 32-bit requests                                                                                                                                                                                                                                                    |
| Await settlement                         | A pending await selects one terminal outcome and its cleanup/wake policy; later requests preserve it                                                                                                   | Every valid await phase, every signed 32-bit cancellation input, completion choice, and wake flag                                                                                                                                                                                                                                                     |
| Timer registration and payload ownership | Registration preserves deadlines and bucket placement; consumes unique generations until exhaustion; removal respects generation and sibling links; removal and collection cannot both claim a payload | Production registration: two operations from an empty wheel, arbitrary 64-bit generation/cursor/clock/deadline/duration and absolute/relative choice. List removal: three nodes, every removal position and wrong generation. Handoff: one node, every 64-bit deadline/tick/nonzero generation                                                        |
| Actor arena allocation                   | Extents are aligned, nonempty, nonoverlapping and representable; quota cannot wrap; failed mapping leaves quota/cursors unchanged; reset reuses a retained chunk                                       | Arithmetic: every machine-width input, checked against wider arithmetic and independent postconditions. Real C ABI heap check: first eight-byte allocation, then every size 0–80 plus MAX, MAX−7, and ISIZE_MAX+1, at most two chunks. Failure/reset: one chunk with every valid cursor 0–32 and usage up to that cursor, mapping failure, then reuse |

| Contract                       | Checked property                                                                                                                                                                                                              | Quantification and bounds                                                                                                                                                                                                                                                        |
| ------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Semaphore permits              | Release, timeout, abandonment, and take conserve permits; abandoning a reserved permit hands it to a successor; operations retain backing after the original handle is freed; teardown detaches deadlines and releases wakers | Two queued operations, one release, one optional timer, three workers, two preemptions; take/abandon and timed/untimed cases. Completed results are taken only after observing a terminal status                                                                                 |
| Read-slot lifecycle            | A retained registration survives creator teardown; terminal status cannot be replaced; competing producers and cancellation have one wake authority, including through the real shared await registration                     | Kani: both cancellation/deposit orders, every signed 32-bit cancellation reason and readiness class, two references. Loom: two depositors and cancellation, two preemptions; both standalone and shared-await attachment                                                         |
| Supervisor reclamation gate    | Closure refuses new pins; live pins prevent a drained result; close and destructor admission have one winner; teardown handoff rollback does not reopen access                                                                | Kani: every native-word admission state and every nonzero pin release count, including closure and final-pin notification. Loom: two pin workers and two closers, two preemptions, exact-once claim and rollback. Uses the production gate that `SupervisorControl` delegates to |
| Managed-heap transactions      | Aliases charge once per account; refused admission leaves the account unchanged; acquisition and allocation/growth guard drops refund abandoned work; outstanding reservations count against quota                            | Kani: every 64-bit live/reserved/request/optional limit. Loom: two concurrent alias acquisitions at sizes/limits (0,0), (8,8), (9,8), (MAX,MAX), (MAX,MAX−1); concurrent reservation/growth rollback at an 8-byte live / 16-byte capped account, two preemptions                 |
| Root/host frame ownership      | Readiness authorizes resume; completed owners are destroyed and cleared before result observation; later polls access no retired frame; invalid boundary states reject                                                        | Every public outcome, frame-presence/completion/readiness combination and signed 32-bit result; two consecutive polls with real modeled allocation/deallocation. Generated frame operations supply explicit boundary contracts                                                   |
| General heap realloc ownership | Realloc preserves the prefix and transfers the exact owner; invalid growth leaves the old owner intact; shrink-to-empty frees; grow-from-null allocates; free/realloc use the original size/alignment                         | Kani: every native-word layout size/alignment; actual C ABI lifecycle with an eight-byte original block, every new size 0–16 and MAX, at alignments 1, 2, 4, and 8. An allocator monitor forces relocation, checks exact layout pairing, preserves bytes, and records release    |

The production kernels are `waker_target.rs`, `cancel_state.rs`,
`await_state.rs`, `timer_entry.rs`, and `arena_layout.rs`. The full production
`timer_wheel.rs` and `arena.rs` also participate directly. The managed ledger moved intact from `owned_value_budget.rs` to
`owned_value_budget/account.rs`; actor/context adapters retain the original
public API. `SupervisorControl` uses `lifetime/pin_state.rs` for the original
admission/closure atomics. Read slots use the original `BytesTriple` type,
shared through `bytes_value.rs`. These are not
parallel implementations maintained for the verifier.

The arena checks exposed unchecked offset/growth arithmetic and quota charged
before allocation success. The allocator now checks arithmetic and commits
state only after success. Those are runtime safety fixes; the other module
extractions introduce no extra runtime locks, atomics, or allocations.

## Root and host frame ownership

The shared root driver is the code that decides when a process root or
asynchronous host may resume and retire its coroutine. Kani compiles the
production [`poll_root`](../../hew-runtime/src/coro_root/poll.rs), moved intact
into a module used by both callers.

| Boundary state                                                                                           | Required behavior                                                             |
| -------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------- |
| Pending, live suspended frame, no readiness                                                              | Keep the frame; return pending                                                |
| Pending, live suspended frame, readiness consumed                                                        | Resume once, then keep the frame or finish according to the published outcome |
| Complete/fault/cancelled, complete frame                                                                 | Destroy once, clear the handle, then read the private result                  |
| Complete/fault/cancelled, null handle                                                                    | Return the result without any frame operation                                 |
| Pending with no frame or an already complete frame; yielded root; terminal outcome with unfinished frame | Abort before an unsafe resume or destruction                                  |

The harness chooses each public outcome, readiness/completion bit and frame
presence independently. Resume may publish any of the five public outcomes and
any signed 32-bit private result. Two consecutive polls use the same concrete
allocation: destruction actually deallocates it in Kani's memory model.
Seven required cover properties establish that waiting, resuming, completion,
rejection, and polling after real destruction are reachable. An unreachable
cover fails the runner. Nine implementation mutations test the guards, abort
policy, result routing, release, clearing and post-release access.

Abort is a checked terminal outcome. Its host stub first asserts that the
independent state table requires rejection, then terminates that model path.
Returning from a rejected case also fails. This checks both missing and
spurious aborts. A pending publication with a completed prefix immediately
after resume returns pending; a subsequent poll rejects it before another
resume. Immediate diagnosis of that producer defect is not claimed.

The boundary assumes an exclusively owned live handle on entry, stable state
reads between synchronous callbacks, storage retained across resume, and a
state object that outlives frame destruction. Modeled frame operations enforce
their documented contracts. The actual LLVM entry dispatch, adapter's returned
handle, generated cleanup bodies, host readiness acquisition, and simultaneous
polling are outside this proof. The separate LLVM handle-return obligation
therefore remains open. No runtime instrumentation is added.

## Concurrent models

The four new lifecycle models above supplement these three compositions.
Loom executes the actual cancellation-tree, observer, await, sleep, readiness
gate, and **complete production timer wheel**, including its bucket delegation,
locks and callback pipeline. `runtime_sync` selects standard types in Hew and
Loom mutex/publication atomics in the host. Standard `Arc`, `Weak`, and
`SmallVec` are used directly and their library contracts remain trusted.

| Composition                                 | Checked outcomes                                                                                                                                                                                   | Exploration bound                                                                                                                                         |
| ------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Hierarchical cancellation delivery          | Racing subscription/requesters cannot lose cancellation; detach releases its target; observers retain the parent chain after creator release; notification during a poll survives for another poll | Two tokens, three workers, two preemptions; attached and early-detached prefixes                                                                          |
| Completion/cancellation/timeout arbitration | One terminal authority for cleanup/wake; cleanup sees published status; reentrant completion loses; settled awaits detach timers, including cancellation without wake                              | One registration/token/timer, three workers, two preemptions; pending, precompleted, cancelled-without-wake prefixes                                      |
| Deadline sleep teardown                     | Inherited deadline rounds to a future tick; drop racing firing releases the target once; drop after collection cannot notify cancelled work                                                        | One sleep, two workers, no preemption bound; an additional two-entry batch forces collection before drop before callback; inherited deadline 1,000,001 ns |

There is no permutation/time sampling cutoff or checkpoint resumption. These
checks are exhaustive within the stated scenarios and bounds. The timer's
calendar and cascade behavior at arbitrary times, arbitrary thread counts,
scheduler fairness and real-time delivery are not proved.

## Trusted boundaries

The host supplies sampled clocks, a valid installed execution context, actor
incarnation lookup, and a wake-request counter. Cancellation/sleep callbacks
exercise the production readiness gate; actual actor dispatch and frame
access are outside these checks. FFI pointers and foreign callbacks must obey
their lifetime contracts, and wheels must outlive their operations.

The arena host replaces OS mappings with a fresh live 128-byte allocation or
mapping failure. It assumes the returned address has the OS's 4096-byte
alignment; requests larger than 128 bytes fail. No arena placement, accounting,
or cursor operation is replaced. Heap harnesses deliberately omit arena
teardown; they prove allocation and reset, not reclamation or all allocation
histories. The separate arithmetic harness has no allocation-size bound.

`boundaries.py` refuses drift in modeled ABI constants and closed state domains.
Unexpected Kani panics become failing assertions, never successful assumptions.
Kani's compiler/solver, Loom, Rust/LLVM, library contracts, and the environmental
boundaries above remain trusted. Passing these checks is not a whole-runtime
correctness proof.

The general-heap monitor models a successful allocator primitive for blocks
up to 32 bytes, enforcing the primitive's documented layout/lifetime contract.
The wrappers under verification are unchanged; the OOM abort path and 32-bit
WASM conversion behavior are not proved. The read-slot model carries readiness
signals, with null byte payloads. Trying to drop a real bytes payload in this
host fails explicitly. Bytes-buffer reclamation and external producer pointer
validity are outside its claim.

The supervisor proof establishes pin/close/drain authority. The OS wait loop,
condition-variable timing, actual destructor body, and every caller's adherence
to the reclamation precondition remain outside it. Managed-budget models call the admission and guard APIs used by Rc, byte/vector
backing changes, stream construction, and reply operations. They do not use the
utility-only `ValueOwner::transfer` method as evidence of production transfer
correctness. Models use canonical supplied storage lists; descriptor traversal and provider freeze/
rebind callbacks are outside those models. Standard `HashMap`, `Arc`, and
`Weak` implementations remain trusted. No model proves unbounded fairness.

## Counterexamples and maintenance

Logs are saved in `target/proof-logs` and uploaded by daily CI. Kani failures
rerun with concrete playback and name a `protocol:`, `wheel:`, or `arena:`
assertion. Rerun one contract with:

```sh
python3 proofs/runtime_protocols/run.py arena
```

Other contracts are `waker`, `cancellation`, `await`, `timer`, `heap`,
`read_slot`, `supervisor`, `managed_budget`, `root_driver`, and `semaphore`. Selecting a
lifecycle contract runs its Kani checks where applicable and its Loom model. For a Loom
counterexample, run:

```sh
LOOM_LOG=trace cargo run --manifest-path proofs/runtime_protocols/Cargo.toml \
  --locked --features loom --bin protocols -- await_race
```

Other model names are `cancellation_delivery`, `sleep_teardown`, `semaphore`,
`supervisor`, `read_slot`, and `managed_budget`.

The mutation runner requires thirty-six counterexamples: omitted waker retain,
replaced cancellation reason, reversed await admission, ignored timer
generation, wrapped arena extent, overlapping returned blocks, quota charged on mapping failure, reused
wheel generation, wrong wheel bucket, skipped production removal, skipped
production callback firing, missed cancelled subscription, attached settled
await timer, and notification from cancelled collected sleep. Thirteen further defects cover
lost permit handoff, stranded waiters despite available permits, fabricated timeout permits, post-close supervisor access,
false drain, lost final-pin drain notification, ignored quota reservations, abandoned acquisition/reservation
leaks, shrink-to-empty leaks, returning the freed realloc pointer, duplicate
readiness publication, and freeing a read slot with an outstanding owner. Nine root-driver
defects cover unready/completed resumption, spurious abort, accepted yield,
missing/premature destruction, a dangling slot, post-free access and incorrect
private-result routing.
Kani witnesses require a named contract or recognized memory-safety violation
and concrete playback. Tool errors do
not count as counterexamples. Mutated files exist only in a temporary tree.

Refactors should preserve these contracts. Intentional policy/state changes
need review of the assertions and bounds; mutation-site changes require an
updated witness. Mutation rejection demonstrates sensitivity to selected
bugs, not specification completeness. The assertions are maintained separately
from the implementation and are not regenerated to make it pass.
