# Verified scheduling readiness gate

Hew's tasks, host calls, inbound distributed calls, and monitor work use
`ReadyState` to coordinate notifications and polling. Its gate guards mutable
frame access and retains notifications that arrive during a poll. Freezing
holds new polls while a frame is inspected or transferred.

Verification adds no runtime checks, allocations, locks, or table lookups.
Production keeps the original atomic operations and memory orderings. The
small atomic interface is resolved statically for production and lets the
verification tools execute the same Rust source with modeled atomics.

## Running

Run `make prove-ready-gate` from the repository root. The supported CI environment
is Ubuntu 24.04 on x86-64. The tools are separate from Hew's normal build:

| Dependency                                   | Purpose                                                                              |
| -------------------------------------------- | ------------------------------------------------------------------------------------ |
| Repository Rust toolchain, including rustfmt | Compile the isolated verification crate and format the oracle                        |
| Lean 4.19.0                                  | Check the theorems and generate the verification oracle; no Mathlib or Lake packages |
| Kani 0.68.0                                  | Check Rust with symbolic inputs; its setup installs its own compiler and CBMC solver |
| Python 3, standard library only              | Check oracle freshness and run mutation witnesses                                    |
| Loom 0.7.2, fetched by Cargo                 | Explore concurrent executions; confined to the verification crate                    |

Install Lean 4.19.0 and put its `bin` directory on `PATH`. With an existing
elan installation, `elan toolchain install leanprover/lean4:v4.19.0` supplies
that version through the adjacent `lean-toolchain` file. Install Kani with:

```sh
cargo install --locked kani-verifier --version 0.68.0
cargo kani setup
make prove-ready-gate
```

After installation, the full command takes about a minute in the development
environment. First-time tool installation and dependency downloads take longer.
Normal builds and runtime tests need none of these additional verification
tools. This target builds neither the compiler nor the rest of the runtime,
and requires no Hew LLVM installation. Cargo pins the verification dependencies
in this directory's lockfile.

The separate [Formal verification workflow](../../.github/workflows/formal-verification.yml)
runs daily at 08:23 UTC on the default branch and on manual dispatch. Manual
runs can select a development branch. It has no pull-request, push, or merge-queue
trigger, and no other workflow depends on it. Failures remain visible as failed
runs and retain the verification log for 14 days; they do not gate normal PRs.

## Guarantees

Lean proves the transition rules satisfy these properties over arbitrary-length
legal executions, including repeated polling and freezing:

- Only one successful poll admission can be outstanding. Another poll is
  refused until the current poll relinquishes its permit.
- A notification stays pending until a poll consumes it or the work completes.
  Finishing a pending poll retains readiness received while it was running.
- Frozen work cannot acquire a poll permit. A frozen, quiescent gate has no
  outstanding poller. Unfreezing queued work requests scheduling again.
- Completion is permanent: late notifications and stale queue attempts cannot
  restart completed work.

## Structure and connection to production

| File                 | Responsibility                                                          |
| -------------------- | ----------------------------------------------------------------------- |
| `ReadyGate.lean`     | Executable rules, invariant proofs, and oracle generation               |
| `expected.rs`        | Generated verification oracle; never linked into Hew                    |
| `generate.py`        | Check theorems and compare regenerated oracle byte for byte             |
| `check.rs`           | Kani harness including the production gate directly                     |
| `concurrency.rs`     | Loom scenarios including that same production gate through `src/lib.rs` |
| `check_mutations.py` | Require rejection of seven deliberately broken variants                 |

Kani checks every valid encoded state and legal operation against the Lean
oracle using uncontended sequential atomic primitives, with unwinding assertions
enabled. The invariants live in Lean; there is no second handwritten contract
in Rust to keep in sync. The refinement harness validates the oracle's
entire input domain before assuming legality, preventing vacuous success from
missing cases.

Loom checks two successive notification/poll cycles, notification versus
completion, competing pollers, and freeze versus a finishing poller without a
preemption bound. The additional two-producer repeated-cycle scenario allows
at most two preemptions. These scenarios exercise actual CAS loops and memory
orderings, but do not establish every possible concurrent Rust execution.

The mutation witnesses drop notifications, duplicate poll admission, admit
frozen work, restart completed work, and empty the oracle's domain. Each must
produce a named Kani assertion failure and a concrete counterexample. Two
further witnesses change the Lean rules to discard notifications or admit
frozen work; the existing theorems must reject both. Compiler or solver errors
alone do not count as detected regressions.

## Working with failures

| Failure                | What to inspect                                                                                                                               |
| ---------------------- | --------------------------------------------------------------------------------------------------------------------------------------------- |
| Lean theorem fails     | The file, line, and remaining goal identify which invariant the rule change breaks                                                            |
| Oracle is stale        | Review the rule change, then regenerate with `python3 proofs/ready_gate/generate.py --write`                                                  |
| Kani assertion fails   | The named assertion and printed concrete input identify the state and operation; compare the runtime with the rule                            |
| Loom assertion fails   | Rerun the named scenario with `LOOM_LOG=trace cargo test --manifest-path proofs/ready_gate/Cargo.toml --locked --lib SCENARIO -- --nocapture` |
| Mutation witness fails | Check whether the mutation site moved, a guarantee weakened, or a tool failed; the script prints unexpected tool output                       |
| Tool setup fails       | Fix installation or version selection; this is not evidence of a runtime defect                                                               |

Kani prints concrete inputs in declaration order: `raw`, then `event`. Phases
are idle=0, queued=1, running=2, notified=3, complete=4, with bit 8 indicating
frozen. Events are notify=0, begin=1, freeze=2, unfreeze=3, pending=4, complete=5.
For example, deliberately dropping a notification during polling produces
`raw=2, event=0` and `ready gate: next state differs from Lean`: the gate
incorrectly stays running instead of becoming notified.

For an implementation change that preserves the contract, edit the Rust and
rerun the target; no Lean changes are needed. For an intentional contract
change, update the rules and affected theorems, review the guarantee, regenerate
the oracle, and verify the Rust. Regeneration only updates the expected
transitions; it cannot repair a failed theorem. CI never regenerates files to
silence a mismatch.

## Operating contract and limits

Callers access the mutable frame only after successful admission and stop
accessing it before `finish`. Poll completion must be reported by its owner;
an externally serialized host may also close an idle gate. The frame and
readiness allocation remain live while the gate is used. Freeze ownership and
the actual frame transfer are coordinated outside this gate.

The Lean owner count treats `finish` as relinquishing the poll permit. The
proof does not inspect frame accesses or establish that every Rust caller
actually stops using the frame before that call. Exclusive admission under
this caller contract is proved; concrete frame exclusivity across Hew is not.

Queue entries can become stale during freezing; admission, rather than the
number of entries, protects frame exclusivity. Notifications are readiness
hints, and callbacks must rescan operation state. Cancellation cleanup,
memory reclamation, actor coroutine parking, OS fairness, scheduling latency,
and network delivery are outside this result.

Lean's unbounded rule proofs, Kani's local implementation checks, and Loom's
finite concurrency scenarios are complementary results. They are not a Lean
proof of Rust semantics or a proof of the entire scheduler. The serializer,
Kani's translation and solver, atomic adapter, caller contracts, Rust/LLVM,
Loom's model, and hardware atomics remain trusted for their respective results.
