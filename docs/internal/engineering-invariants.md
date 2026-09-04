# Engineering Invariants

This document records durable engineering principles for Hew. It is a compact
design aid, not an incident log or a substitute for the language and runtime
specifications.

## Boundaries fail closed

- Every boundary must represent the complete supported form or stop with a
  useful diagnostic. This includes serialization, FFI, generated code, local
  and remote execution, and tool output.
- Never turn an unsupported shape into omission, a sentinel-shaped success, or
  an uncounted diagnostic.
- Keep capability and platform differences explicit in the relevant manifest,
  specification, or diagnostic contract.

## One semantic authority

- Each fact has one authoritative owner. Downstream stages consume that fact;
  they do not reconstruct it from weaker fallbacks.
- Preserve authoritative type, ownership, and diagnostic information across
  lowering and serialization boundaries.
- When two representations disagree, reject the ambiguity or resolve it at the
  owning layer before crossing the boundary.

## Lifecycle symmetry

- Every acquire, register, borrow, send, or spawn operation has a clearly
  defined release, unregister, return, or join path.
- Cleanup must cover success, error, cancellation, timeout, and partial
  initialization paths.
- Resource ownership is transferred at explicit boundaries; aliases and
  borrowed values must not outlive their owner.
- Cleanup operations are idempotent or are guarded so that each owned resource
  is released exactly once.

## Oracles test intent

- Tests and checkers should assert observable contracts, not implementation
  details or source-text arrangements.
- Negative tests must prove that invalid input is rejected for the intended
  reason, rather than merely failing later during linking or execution.
- A regression oracle should fail when the protected behaviour regresses and
  remain independent of incidental ordering, counts, or diagnostics wording.

## Parity or an explicit gap

- Supported behaviour should have equivalent coverage across supported targets
  and execution modes.
- An intentional difference is a documented, typed capability disposition with
  a focused diagnostic or behaviour check.
- Do not treat comments, platform accidents, or an absent test as a parity
  decision.

## Attribute regressions before acting

- Establish whether a failure is caused by the change, the current baseline,
  or the environment before assigning ownership.
- Use the narrowest reproduction that proves the contract and preserves the
  failure's intent.
- Fix the underlying implementation or contract; do not weaken a checker,
  oracle, or gate to hide the failure.
