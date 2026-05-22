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

## Failure detector

### Trigger: phi-accrual (Hayashibara et al., 2004)

SWIM's ALIVE → SUSPECT trigger is the **phi-accrual failure detector**.
For each peer we maintain a sliding window of heartbeat inter-arrival
intervals and estimate the mean μ and standard deviation σ. On each
tick we compute

```
t   = now − last_heartbeat
φ   = −log₁₀(P(interval > t | learned distribution))
```

and suspect the peer when `φ > 8.0` (the production-SOTA threshold used
by Akka, Cassandra, and Hashicorp memberlist — corresponding to roughly
a 10⁻⁸ probability that the observed silence is benign jitter).

The math, including the closed-form normal-survival approximation, is
documented in `hew-runtime/src/phi_accrual.rs`. The module is exposed as
`hew_runtime::phi_accrual::PhiAccrualDetector`.

### Why phi-accrual over a fixed timeout?

The previous trigger was a flat `ping_timeout_ms = 500` cutoff. That
choice forces a single trade-off across all networks: too short and a
sleepy WAN link sees spurious SUSPECTs on healthy peers; too long and a
snappy LAN waits seconds longer than necessary to react to a real
failure.

Phi-accrual lets the trigger **adapt to actual conditions per peer**.
A peer whose heartbeats land like clockwork accrues phi quickly when it
falls silent; a peer on a jittery link is granted proportional slack
before the substrate calls it suspect. The threshold is a knob over
"how confident must we be?", not over "how long must we wait?", and the
wall-clock wait emerges from the data.

### Cold start (fail-closed)

A detector that has seen fewer than 10 intervals (`PHI_MIN_SAMPLES`)
returns `φ = 0`, i.e. it never raises suspicion on its own. In this
state SWIM falls back to the legacy `ping_timeout_ms` so brand-new and
quiet peers are not blind-spots. Once the window warms, phi takes over.

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

Unchanged. A peer that has been SUSPECT for longer than
`suspect_timeout_ms` is declared DEAD; the per-peer detector is then
pruned. This escalation remains a fixed timeout because the SUSPECT
window already exists specifically to allow indirect-ping confirmation,
and applying phi twice would not help confirmation latency.

### ABI

Phi-accrual changes **when** the substrate emits
`HEW_MEMBERSHIP_EVENT_NODE_SUSPECT`, never **what** the consumer sees.
The membership-event ABI surface is preserved; no new wire messages and
no new event variants are introduced. The threshold and window size
live as module-level constants in `cluster.rs` and are deliberately not
exposed through the `#[repr(C)] ClusterConfig` struct.

## References

- N. Hayashibara, X. Défago, R. Yared, T. Katayama,
  *"The φ Accrual Failure Detector"*, SRDS 2004.
- Akka cluster `PhiAccrualFailureDetector` (Apache-2.0; informed the
  closed-form normal-survival approximation used here).
- Cassandra `FailureDetector` (exponential-distribution variant; not
  used here — normal-CDF approximation matches our jitter profile
  better).
