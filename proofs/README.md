# What Hew's formal corpus establishes

The corpus gives strong evidence for named rules. Whole-subsystem coverage is
still partial. `make proof-status` gives a short subsystem overview and
shows whether the latest proof run matches the current source tree.

| Area                  | Rules in everyday terms                                                                                 | Confidence and largest gap                                                                                                                      |
| --------------------- | ------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------- |
| Readiness             | One poll owner; wakes survive a poll; frozen/completed work stays stopped                               | Strong local gate proof. Queue/frame integration and scheduling fairness remain open                                                            |
| Time                  | Deadlines tighten and cannot wrap; timer payloads have one terminal owner                               | Strong full-width arithmetic. Arbitrary wheel cascades and real-time delivery remain open                                                       |
| Allocation            | Extents fit; failure consumes no arena space/quota; realloc preserves or releases its owner             | Strong arithmetic with bounded API lifecycle checks. Arbitrary histories and OS behavior remain open                                            |
| Managed ownership     | Waker references balance; aliases charge once; abandoned reservations refund quota                      | Bounded production protocol checks. Descriptor walks, provider transfers and all generated drops remain open                                    |
| Runtime lifecycle     | Await settlement happens once; permits reach waiters; in-flight slots stay live; closure drains pins    | Bounded concurrent compositions plus full-width gate arithmetic. Complete shutdown/destructor coverage remains open                             |
| Root/host frames      | Readiness authorizes resume; completion destroys and clears the frame owner before returning its result | Exhaustive guard combinations and two consecutive polls of the production driver. Generated handle lifetime and host readiness glue remain open |
| Generated LLVM        | After explicit frame release, generated code performs no more memory/call work or another release       | Kernel-checked local rule on O0/O2 artifacts, with no path-length bound. Returned handles and cross-entry lifetime remain open                  |
| Compiler lowering     | No semantic preservation contract maintained yet                                                        | HIR/SIR/MIR ownership preservation remains open                                                                                                 |
| Distributed transport | No maintained formal target yet                                                                         | Authentication, reconnect fencing and wire/delivery correctness remain open                                                                     |

“All inputs” means every value in the stated arithmetic domain. “Bounded”
means every execution represented by the listed finite scenario and scheduling
bound. “Kernel checked” means Lean checks the certificate for the actual
extracted artifact. These strengths describe a rule's evidence, rather than a
percentage of the subsystem proved correct.

The LLVM report separately lists syntactic post-release pointer-return paths
whose handle lifetime remains unproved. That is a concrete remaining obligation,
even when terminal cleanup certificates pass.

## Run and inspect

- `make prove`: runtime and emitted-LLVM targets, with source fingerprints.
- `make prove-runtime`: scheduling, deadlines and runtime protocols.
- `make prove-llvm-frames`: build the current compiler and certify the LLVM corpus.
- `make proof-status`: subsystem rules, confidence and missing coverage.
- `python3 proofs/coverage.py --rules`: expand the individual rules when needed.
- `python3 proofs/coverage.py --json`: machine-readable evidence and assessment.

A source change makes old passing evidence **stale**. Failed, missing and stale
runs remain distinct; the report never silently reuses an old green result.
Daily/manual CI saves logs, source fingerprints, LLVM graphs and certificates.
The two jobs are advisory and independent of normal PR checks. Proof tooling
adds no runtime instrumentation or dependencies.

Detailed contracts and reproduction commands:

- [Scheduling readiness](ready_gate/README.md)
- [Request deadlines](request_deadline/README.md)
- [Runtime ownership and lifecycle](runtime_protocols/README.md)
- [Emitted LLVM frame cleanup](llvm_frames/README.md)

To call an entire subsystem covered, we need its important failure classes
represented, its production callers and cleanup paths connected, the relevant
input/concurrency bounds justified, and the largest modeled boundaries removed
or independently validated. Today the corpus meets those criteria for narrow
rules within subsystems, with the remaining gaps shown above.
