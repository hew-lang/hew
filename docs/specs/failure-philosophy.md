# Hew failure-detection philosophy

> Status: living document. Scope: the cluster substrate
> (`hew-runtime/src/cluster.rs`) and the per-peer reachability decision
> that triggers `HEW_MEMBERSHIP_EVENT_NODE_SUSPECT`.

## Tenet recap

Hew's failure-handling principles ([HEW-SPEC-2026](./HEW-SPEC-2026.md))
hold the substrate to three commitments:

1. **Fail closed.** When the substrate cannot prove a peer is dead,
   it must not declare one dead.
2. **Disclose, don't dictate.** The substrate exposes typed events
   (`Partition`-shaped membership transitions); application policy lives
   above it.
3. **No new wire surface for local decisions.** The judgement of "is
   peer X alive?" is local-state; the wire protocol does not learn
   detector-level details.

## Request expiry and uncertain outcomes

Actor-call deadlines bound admission and queued execution. A request whose
budget expires before dispatch never starts; a handler already claimed belongs
to its actor and can finish after the caller stops waiting. Its nested calls
still inherit the exhausted request budget and return `ActorError.TimedOut`.
Caller expiry does not roll back the handler's mutations.

A timeout or partition therefore cannot tell an application whether a remote
effect happened. Use an application idempotency key, query durable state to
reconcile, or expose a job identifier with explicit status and cancellation
messages for work that must survive a request. Ordinary calls wait in order;
`fork` changes concurrency, not delivery certainty. Hew does not automatically
retry a timed-out request or supply exactly-once business effects.

## Failure detector

### Trigger: an unanswered probe, weighed by phi-accrual

Silence alone never raises suspicion: with round-robin probing, contact
with one peer is naturally a full probe cycle apart. Each protocol period
the substrate probes one member directly and stamps when it did; a re-probe
keeps the original stamp, so the deadline never slides. Any frame from the
member answers the probe, and so does a relay's report that the member
answered an indirect probe (see below). A member is suspected only when a
probe stays unanswered for longer than `ping_timeout_ms`.

Phi-accrual (Hayashibara et al., 2004) then decides whether the overdue
probe is anomalous for that peer. For each peer we maintain a sliding
window of heartbeat inter-arrival intervals and estimate the mean μ and
standard deviation σ. On each tick we compute

```
t   = now − last_heartbeat
φ   = −log₁₀(P(interval > t | learned distribution))
```

and a warm detector suspects the peer only when its probe is overdue and
`φ > 8.0` (the production-SOTA threshold used by Akka, Cassandra, and
Hashicorp memberlist — corresponding to roughly a 10⁻⁸ probability that
the observed silence is benign jitter). Phi only ever delays suspicion; it
never raises it on its own.

The math, including the closed-form normal-survival approximation, is
documented in `hew-runtime/src/phi_accrual.rs`. The module is exposed as
`hew_runtime::phi_accrual::PhiAccrualDetector`.

### Why phi-accrual over a fixed timeout?

A fixed timeout forces a single trade-off across all networks: too short
and a sleepy WAN link sees spurious SUSPECTs on healthy peers; too long
and a snappy LAN waits longer than necessary to react to a real failure.

Phi-accrual lets the trigger **adapt to actual conditions per peer**.
A peer whose heartbeats land like clockwork accrues phi quickly when it
falls silent; a peer on a jittery link is granted proportional slack
before the substrate calls it suspect. The threshold is a knob over
"how confident must we be?", not over "how long must we wait?", and the
wall-clock wait emerges from the data.

### Cold start (fail-closed)

A detector that has seen fewer than 10 intervals (`PHI_MIN_SAMPLES`)
returns `φ = 0`. In this state an overdue probe suspects on its own, so
brand-new and quiet peers are not blind-spots. Once the window warms, an
overdue probe must also exceed the phi threshold.

This is a deliberate fail-closed choice (tenet 1): a fresh detector
must not produce a high phi from a tiny sample (mean and stddev are
unreliable on `n < 10`), so we abstain rather than fabricate.

### Recovery (SUSPECT → ALIVE)

When a peer's first heartbeat after a SUSPECT period arrives, the
substrate records it as an **anchor only** — the recovery interval is
*not* pushed into the distribution. Folding a multi-second silence into
the window would teach the detector that long gaps are normal and dull
all future detections of real failures.

### SUSPECT → DEAD

A SUSPECT peer is declared DEAD once `suspect_timeout_ms` has passed since
the last contact with it, not since the suspicion was raised; the per-peer
detector is then pruned. A peer that went quiet long before its probe came
due is therefore buried no later than a peer suspected at once. This
escalation uses a fixed timeout because the SUSPECT window already exists
specifically to allow indirect-probe confirmation, and applying phi twice
would not improve confirmation latency.

### Detection time

Take `N` members, protocol period `P`, probe timeout `T = ping_timeout_ms`
and suspect timeout `S = suspect_timeout_ms`, with the defaults
`P = 1000`, `T = 500` and `S = 3000`. Round-robin probing reaches a silent
member within `(N-1)·P` of its last contact, and the tick that finds its
probe overdue is the first one strictly past `T`, `(⌊T/P⌋ + 1)·P` later. A
member is therefore suspected within

```
(N-1)·P + (⌊T/P⌋ + 1)·P     of its last contact (3000 ms at N = 3)
```

while its detector is cold. A warm detector also waits for `φ > 8`, that
is, for silence beyond roughly `μ + 5.2σ` of the learned contact intervals,
rounded up to the next period. When contacts arrive in bursts — a probe's
ACK and the member's own PING landing together, then a gap of two periods —
σ approaches μ and that wait dominates: a three-member mesh without
indirect probes, μ ≈ 1000 ms and σ ≈ 950 ms, is suspected about 6 s after
the last contact. Indirect probes add contacts and narrow σ. Gossip from
another member that already suspects the peer can raise the suspicion
sooner. A SUSPECT member becomes DEAD at the first period strictly past
`S` after its last contact, and never in the period that suspected it, so
detection completes within `max(S, suspicion time) + P` of the last contact.

### Indirect probes

Each period the prober also sends `PING_REQ` naming the probed member to up
to `indirect_ping_count` other connected peers. A relay pings the member
and, when the member's ACK arrives within `ping_timeout_ms`, forwards an ACK
naming the member to each requester, once. The requester counts that as
contact with the member, answering the overdue probe, only when it sent
that relay a `PING_REQ` about that member within `ping_timeout_ms`, and once
per request: a peer cannot keep a silent member alive with unsolicited ACKs. A black hole between
two live members is therefore bridged by any third member that reaches both,
and neither side is suspected. The forwarded ACK carries the relay's
incarnation, so it never readmits a buried member or re-stamps one.

### Burial notices and refutation

A member that sees itself suspected or declared dead at or above its own
incarnation refutes the claim by advancing its incarnation; the new ALIVE
wins everywhere. Gossip alone cannot guarantee the member hears the claim:
each event rides a bounded number of frames, and a member whose frames were
dropped rather than delayed can come back after every copy was lost. So
every SWIM frame a node sends to a member it holds DEAD leads with that
member's obituary. It takes one of the frame's `max_gossip_per_msg` slots
but not the retransmit budget of the gossip event queue, so it is sent
however many earlier copies were spent. A buried member that stays
connected learns of its burial from the next frame it receives from any
node that buried it — the ACK to its own PING, a relayed PING or a
`PING_REQ` — and refutes. The obituary rides frames the protocol already
sends, so a buried or flapping member never causes extra traffic.

Every PING, ACK and `PING_REQ` carries the sender's own incarnation, and
each counts as an ALIVE claim at it: a member that refuted its burial
returns through whichever frame it sends first, even when its refuting
gossip was lost. A claim at or below the buried incarnation is refused.

Two members that hold each other DEAD with no third member in contact with
both exchange no frames over a connection they already hold, since DEAD
members are never probed. When one of them reconnects at the same durable
session, the node admitting the buried member's connection opens it with a
PING that carries the obituary, so each learns of its burial, refutes and
readmits the other; a higher durable session is admitted afresh.

### ABI

Phi-accrual governs **when** the substrate emits
`HEW_MEMBERSHIP_EVENT_NODE_SUSPECT`; the membership-event ABI surface is
fixed — no new wire messages and no new event variants. Forwarded ACKs and
burial notices reuse the existing SWIM control payload. The threshold and
window size live as module-level constants in `cluster.rs` and are
deliberately not exposed through the `#[repr(C)] ClusterConfig` struct.

## References

- N. Hayashibara, X. Défago, R. Yared, T. Katayama,
  *"The φ Accrual Failure Detector"*, SRDS 2004.
- Akka cluster `PhiAccrualFailureDetector` (Apache-2.0; informed the
  closed-form normal-survival approximation used here).
- Cassandra `FailureDetector` (exponential-distribution variant; not
  used here — normal-CDF approximation matches our jitter profile
  better).
