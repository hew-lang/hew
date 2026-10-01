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

## Declaration and type identity

- Resolve source names in the checker. Later stages consume declaration,
  binding and type identities; rendered names describe those identities.
- Before resolution, source scopes, import catalogues and external symbol
  tables may index spellings. Publishing a semantic fact ends that boundary:
  later consumers must use its declaration or binding identity. Structural
  rules should protect those published facts, not reject every string map
  used to read source, render diagnostics or select an external ABI symbol.
- Generic parameters carry their declaring `DefId` and parameter index through
  checker types, HIR and substitution. Spelling is for source lookup and display.
  An impl receiver is instantiated from its resolved parameter pattern; the
  nominal type's parameter list does not identify an impl's binders. Trait
  default bodies substitute the declaring trait's receiver binder explicitly.
- Ownership markers, opacity and recursive value classification use nominal
  IDs. Structural marker derivation uses resolved type heads; changing a
  head's display spelling cannot change its fields, bounds or capabilities.
- Capability implementations are selected by the resolved receiver and the
  trait method's declaration ID, or the compiler's Hash/Eq operation enum.
  Concrete specializations precede the generic receiver entry. Alias rendering
  and wire facts retain declaration IDs rather than joining display names.
- HIR registers callable and constant bodies under checker declaration IDs.
  Import aliases and repeated inventory visits must reuse those body entries.
- HIR record and variant construction selects the checker-published result
  declaration. A variant spelling selects a member only within that owner;
  import aliases and short type names cannot select another declaration.
- Source annotations cross the checker boundary as resolved types keyed by
  their source file and span. HIR must reject a missing annotation fact rather
  than resolve its spelling again.
- A declaration ID belongs to its compilation's declaration table. When
  embedded source checking appends rows, publish the extended table with every
  fact that can refer to those rows. Never interpret an ID through a shorter
  table or an unrelated compilation.
- Private helper reachability follows checked declaration references, including
  function values and closure bodies. A local binding that shadows a function
  does not make that function reachable.

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

## Markers name a live lane

`make transition-marker-lint` checks every `TRANSITION(<id>)`, `SHIM`, `WHY:`
and `TODO` marker in tracked Rust, Hew, TypeScript, shell and TOML sources. A
`TRANSITION` or `SHIM` names a lane id with state `live` in
`docs/internal/marker-lanes.tsv` and carries a `WHEN` or `deleted by` line
within 6 lines; `SHIM` and `WHY:` also state the real fix; `TODO` names a
registry id or `#N`. To add a lane, append a row (`id owner state ref`); mark
it `done` when it lands so leftover markers fail. Existing violations live in
`docs/internal/marker-baseline.tsv`, which only shrinks: fix the marker and
delete its row.
