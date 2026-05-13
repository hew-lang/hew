# v0.5 Value-Model Calibration Corpus

This directory contains worked examples for the Hew v0.5 value-model surface.
Each `.hew` file is a hand-written corpus fixture.  Each `.ownership-plan.txt`
file is the hand-written expected output of `hew explain ownership` for that
fixture — checked in *before* the implementation exists so that the v0.5
value-model checker has a concrete, byte-level target to match.

## Status

These fixtures are **corpus / future** items.  They are not wired into the
current CTest suite and will not compile or run under the current compiler.
They become runnable tests once the v0.5 value-model checker/lowering
implementation lands; the CMakeLists entry and XFAIL→pass flips happen at
that point.

Fixtures whose name includes `_reject_` document code that the v0.5 checker
must *reject* — they are expected-failure cases.  The companion `.ownership-plan.txt`
for a `_reject_` fixture documents the expected diagnostic, not a passing report.

## Naming scheme

```
NN_descriptive_name[_reject_].hew               -- source fixture
NN_descriptive_name[_reject_].ownership-plan.txt -- expected report / diagnostic
```

## Value classes (user-facing names, per §8 decision 1)

| Internal name   | User-facing / diagnostic name | Marker         |
|-----------------|-------------------------------|----------------|
| BitCopy         | Copy                          | (structural)   |
| CowValue        | Value                         | `@value` (opt) |
| PersistentShare | Shareable                     | (stdlib types) |
| AffineResource  | Resource                      | `@linear` / `@resource` |
| View            | View                          | (compiler-only)|

## User-confirmed defaults

- User struct default: COW-by-fields (CowValue if all fields are Copy/Value;
  BitCopy if all fields are Copy; Resource requires `@linear`/`@resource`).
- Hidden materialize diagnostics: note-level by default; `copy(x)` silences.
- Refcount strategy: actor-local non-atomic RC with cross-actor promotion.
- `actor_scope` included as an opt-in structured actor cleanup primitive.
