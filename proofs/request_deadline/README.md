# Request deadline correctness

Request deadlines constrain nested scopes, outgoing actor calls, and connection
admission. Extending a bound can let an operation outlive its caller; arithmetic
overflow must never turn a finite limit into the negative value meaning
unlimited time.

Run `make prove-request-deadline` with Kani 0.68.0 and Python 3 on `PATH`.
Use the Kani installation instructions in the [verification setup](../ready_gate/README.md#running).
This target requires neither Lean nor Loom and adds no new dependencies to the
existing proof toolchain. `make prove-runtime` runs all maintained targets.
The daily/manual workflow runs that aggregate target, independently of normal
PR and merge-queue checks.

## What is proved

Kani checks the actual [RequestDeadline implementation](../../hew-runtime/src/request_deadline.rs)
against these contracts for every representable input:

- **Composition:** `earlier` selects the minimum effective expiry and cannot
  extend either input. Absence is an identity. Reordering or regrouping merges
  preserves their meaning, and merging a deadline with itself preserves it.
- **Construction:** for any nonnegative clock reading and any signed 64-bit
  duration, `after` returns `min(now + max(duration, 0), i64::MAX)`. The sum in
  the specification uses 128-bit arithmetic. The result remains finite and
  never precedes `now`; nonpositive durations expire immediately.
- **Observation:** absence has no remaining duration and never expires. A
  finite expiry has the exact signed remaining duration and is expired exactly
  when `expiry <= now`, including equality. Remaining time cannot increase,
  and expiry cannot reverse, when the clock advances.

The merge specification embeds absence above the largest finite instant in an
`i128` ordering. This preserves the distinction between `i64::MAX`, a finite
deadline, and absence. The ABI's default absent value is checked to be `-1`;
other negative inputs are treated as absence by the implementation. Algebraic
equality compares the effective bounds, so equivalent negative encodings need
not have identical raw bits.

These are symbolic proofs over the complete integer domains, including their
endpoints, rather than sampled values or a shortened integer representation.
The verified methods and harnesses have no loops, and the checks impose no
loop-unwinding limit.

## Connection to production and failure diagnosis

`check.rs` includes the production source file directly. It supplies a clock
with arbitrary nonnegative readings; the execution-context accessor is a null
stub needed to compile the file. `current()` is not part of this proof. No
deadline implementation is copied, extracted, or changed for verification.

Kani reports a named `deadline:` assertion and prints concrete input values on
failure. The values appear in declaration order:

| Harness                                               | Inputs                                      |
| ----------------------------------------------------- | ------------------------------------------- |
| `merging_never_extends_deadlines`                     | Three raw deadline encodings: `a`, `b`, `c` |
| `durations_cannot_overflow_into_unlimited_time`       | `now`, `duration`                           |
| `expiry_is_exact_and_cannot_reverse_as_time_advances` | Raw deadline, `now`, `later`                |

The target also requires five deliberate defects to produce assertion failures
and concrete counterexamples: choosing the later bound, discarding an inherited
bound, wrapping addition, accepting a negative duration without clamping it,
and expiring strictly after the deadline instead of at it. Compilation errors
or a failed solver invocation alone do not count as detecting a defect.
If a refactor changes a mutation site, the witness script asks for its update.

For an implementation refactor, preserve the contracts and rerun the target.
An intentional change in deadline semantics requires reviewing the affected
assertions and their callers. The proof has no generated files to refresh.

## Boundary of the result

The clock model assumes `0 <= now <= later <= i64::MAX`, matching the
nonnegative nanosecond representation supplied by `hew_instant_now`. It does
not prove the OS clock, concurrent clock reads, timer scheduling, wake latency,
cancellation delivery, or propagation of deadlines through every call site or
network message. It proves the deadline value operations those paths use.
At the representation limit, construction clips to `i64::MAX`; it does not
promise a requested interval can extend beyond that instant.

Kani's Rust translation, its bundled compiler and solver, the clock boundary,
and the Rust arithmetic semantics remain trusted. No concurrency model is
needed for these value operations, and no verification code is linked into Hew.
