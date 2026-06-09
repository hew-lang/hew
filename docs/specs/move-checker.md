# Move-checker analysis substrate

> **Status: historical — C++/MLIR was retired in v0.5 (commit 842842bd). Current implementation: hew-codegen-rs direct Rust/Inkwell LLVM emission.**
> The ownership-analysis vocabulary remains relevant, but the concrete
> codegen paths and DROP-TODO inventory below describe the retired C++ backend.

**Status:** derived substrate spec for issue #1399. This document defines the
analysis contract for the ownership-classification substrate that supports the
`--experimental-handle-safety` prototype.
**Audience:** checker implementers, codegen reviewers, stdlib handle reviewers,
and spec reviewers.
**Binding upstream authority:** [`handle-safety-and-resource-lifetime.md`](./handle-safety-and-resource-lifetime.md).
This document derives from that spec and does not re-decide handle ownership,
release semantics, or failure-mode ownership.

---

## 1. Scope

This document specifies the analysis substrate for the move-checker prototype:

- where ownership classification lives
- which ownership classifications are allowed
- which current `DROP-TODO` markers the substrate must explain
- which failure modes each marker maps to
- which diagnostics and rejection fixtures the prototype must publish
- how the prototype behaves while issue #1295 remains open

This document does **not**:

- redefine the language's owned-handle semantics
- change the per-handle migration order
- ship production move-checker behavior
- close issue #1399 on its own

The substrate exists to let the checker publish authoritative ownership facts
that codegen then consumes fail-closed.

## 2. Authority

[`handle-safety-and-resource-lifetime.md`](./handle-safety-and-resource-lifetime.md)
is the binding authority for:

- the prime invariant that owned handles release exactly once
- the consume-on-call direction for affine handle methods
- the single-oracle rule: the checker owns ownership facts and codegen consumes
  them
- the shared ownership vocabulary
- the failure-mode taxonomy, including FM-1, FM-2, FM-10, FM-11, and FM-12

This substrate spec narrows that upstream contract into implementable analysis
obligations:

- publish only the approved ownership categories
- serialize them through `handle_ownership_kinds`
- reject missing or contradictory ownership facts rather than re-deriving them
- cover every live `DROP-TODO` marker with a named classification path and
  rejection fixture

If a future handle migration cannot be expressed in the vocabulary below, the
spec response is to amend the shared vocabulary through cross-review with the
handle-safety spec. It is **not** to add a second ownership oracle.

## 3. Classification vocabulary

The substrate may publish only these classifications:

| Classification | Meaning | Typical use |
| --- | --- | --- |
| `Owned` | The binding or call edge transfers ownership and the caller may not use the value afterward. | Consuming calls; explicit ownership transfer. |
| `Borrowed` | The value is observed without transferring ownership. | Non-consuming methods and safe call arguments. |
| `FieldAlias` | The value aliases storage owned through another aggregate path. | `json.Value`/`String` field extraction and field-passing hazards. |
| `ClosureCapture` | The value is captured into closure storage whose cleanup is not equivalent to the current lexical scope. | Long-lived closure environments. |
| `YieldedAcrossSuspend` | The value crosses a generator suspend/resume boundary. | `yield` from a coroutine or generator. |
| `Temporary` | The value is an ephemeral expression result whose cleanup remains local to the current lowering boundary. | Anonymous call temporaries and short-lived rvalues. |

Rules:

1. The checker is the authoritative producer of these categories.
2. Codegen must not infer one of these categories from AST shape, drop-function
   name, or ad hoc exemptions when the checker did not publish it.
3. The prototype may become more precise inside these categories, but it may not
   mint a seventh category.

## 4. DROP-TODO inventory

Current in-tree reality is reproduced by:

```text
$ grep -n "DROP-TODO" retired C++ backend mlir/*.cpp
retired C++ backend MLIRGen.cpp:5268:    // DROP-TODO(D6): BLOCKED — generator coroutines suspend/resume across yield
retired C++ backend MLIRGenExpr.cpp:1870:    // DROP-TODO(D7): BLOCKED — no move-checker infrastructure exists. Current
retired C++ backend MLIRGenExpr.cpp:1877:    // DROP-TODO(D8): BLOCKED — String in closure env may alias an outer struct
```

Only three live markers exist in tree:

| Marker | Location | Current blocked condition | Substrate concern |
| --- | --- | --- | --- |
| D6 | `retired C++ backend MLIRGen.cpp:5268` | Generator bindings can survive across `yield` and later resume. | `YieldedAcrossSuspend` |
| D7 | `retired C++ backend MLIRGenExpr.cpp:1870` | Call lowering lacks a move-checker proof, so borrow-semantics exemptions remain conservative. | `Owned` / `Borrowed` / `FieldAlias` |
| D8 | `retired C++ backend MLIRGenExpr.cpp:1877` | Closure capture can alias outer aggregate storage without tracking. | `ClosureCapture` / `FieldAlias` |

### 4.1 Issue-body numbering reconciliation

Issue #1399 refers to D3 through D8, but the current codebase contains only D6,
D7, and D8 markers. The issue-body D3/D4/D5 citations are not distinct live
markers; they point at adjacent lines in the D6 coroutine pre-scan region rather
than separate `DROP-TODO(...)` comments.

For this substrate spec:

- **D6, D7, and D8 are the canonical in-tree marker names**
- the coroutine cluster previously described as D3/D4/D5 is treated as **one**
  live marker, D6
- fixture and prototype planning must key off the current in-tree markers, not
  the overcounted issue-body list

## 5. Failure-mode mapping

The substrate does not own the failure-mode taxonomy; it maps live markers onto
the failure modes already fixed by the handle-safety spec.

| Marker | Primary failure mode(s) | Why |
| --- | --- | --- |
| D6 | FM-12 | A yielded binding may be dropped too early or reused after resume unless the checker classifies the suspend boundary explicitly. |
| D7 | FM-10, FM-2 | Field-passed ownership can double-free through aliasing or permit use-after-consume if a consuming call is misclassified as borrow. |
| D8 | FM-11, FM-10 | Closure environments can retain aliased handle-backed storage beyond the lexical scope that appears to own it. |

Derived obligations:

- `Owned` vs. `Borrowed` classification is the guard for FM-2.
- `FieldAlias` is the guard for FM-10.
- `ClosureCapture` is the guard for FM-11.
- `YieldedAcrossSuspend` is the guard for FM-12.

FM-1 remains a downstream consequence the checker prevents through the same
facts: once a consuming edge or alias hazard is classified correctly, codegen
must not emit a second drop for the same ownership path.

## 6. Diagnostic vocabulary

The prototype should emit diagnostics in a vocabulary that matches the approved
classifications and failure modes. At minimum, the analysis surface should name
the following conditions:

| Diagnostic shape | When it fires | Related classification / failure mode |
| --- | --- | --- |
| consuming call invalidates binding | A binding is used after an `Owned` transfer. | `Owned`, FM-2 |
| field alias escapes owning aggregate | A field-backed value is passed or retained as though it were independent ownership. | `FieldAlias`, FM-10 |
| closure capture outlives owning scope | A captured value cannot satisfy affine cleanup from closure storage. | `ClosureCapture`, FM-11 |
| yielded value crosses suspend boundary unsafely | A generator resumes after a value was yielded or excluded incorrectly. | `YieldedAcrossSuspend`, FM-12 |
| missing ownership classification at lowering boundary | Codegen reaches a drop-sensitive lowering path without checker-published ownership metadata. | boundary invariant |

Guidance:

- Diagnostics should name the ownership action (`consumed`, `borrowed`,
  `captured`, `yielded`) rather than only the downstream crash class.
- Diagnostics should point at the user-visible binding or call site, not only
  an internal drop helper.
- Codegen invariant failures remain valid during the prototype, but they are a
  fallback surface; the intended steady state is checker-authored diagnostics.

## 7. Prototype flag: `--experimental-handle-safety`

The move-checker substrate prototype is gated by:

```text
--experimental-handle-safety
```

Requirements for the flag:

- default-off
- no behavioral change for existing programs when the flag is absent
- ownership classifications produced under the flag remain checker-authored and
  codegen-consumed; the flag does not authorize codegen-side inference
- rejection fixtures for the mappings in this spec must run under the flag

The flag name is shared with the per-handle migration work so the prototype and
the eventual handle migrations exercise one experimental surface, not parallel
switches.

## 8. Interaction with consume-on-call and issue #1295 fallback

The binding semantic direction comes from
[`handle-safety-and-resource-lifetime.md`](./handle-safety-and-resource-lifetime.md):

- methods on affine handle types are consume-on-call by default
- non-consuming methods are treated as borrow receivers
- a future `consume` modifier generalizes explicit consume semantics outside
  that default

Issue #1295 owns the parser, checker-surface, and codegen work for that public
syntax. This substrate spec only fixes how the analysis behaves around it:

1. If #1295 has landed, the checker classifies consume-marked call edges as
   `Owned` and non-consuming edges as `Borrowed`.
2. If #1295 has **not** landed, the prototype may fall back to
   runtime-visible-function-name classification for known consuming functions.
3. That fallback is a prototype compatibility bridge, not the final language
   design.
4. The fallback must stay narrow and auditable. It must not silently become a
   general-purpose second oracle.

The fallback exists so the prototype can exercise ownership classification
before the full consume-syntax change merges, while still deferring the
semantic authority to the handle-safety spec.

## 9. Rejection-fixture matrix

The prototype must carry at least one rejection fixture for each live marker and
its mapped failure mode.

| Marker | Failure mode(s) | Minimum rejection fixture intent |
| --- | --- | --- |
| D6 | FM-12 | A generator yields a handle-backed binding, resumes, and then uses or drops the same ownership path again. |
| D7 | FM-10, FM-2 | A field-backed or otherwise consuming call path passes a value as though it were borrow-safe, then reuses the binding or lets both paths drop it. |
| D8 | FM-11, FM-10 | A closure captures a handle-backed or aliased value whose cleanup can no longer be satisfied by the apparent outer scope. |

Recommended fixture names:

- `reject_yielded_handle_after_resume`
- `reject_field_alias_consumed_call`
- `reject_closure_capture_of_aliased_handle`

Additional prototype rules:

- each rejection fixture fails only when the experimental flag is enabled
- each failure should point at the source binding or call edge that triggered
  the ownership conflict
- fixture names may change, but the one-fixture-per-live-marker rule may not

## 10. Open questions

The following questions remain intentionally open for prototype and follow-on
work:

1. How much precision can the prototype add within `Temporary` before the
   classification stops being a stable wire contract?
2. Should D7 eventually split into finer internal reasoning paths while still
   publishing only the shared public vocabulary?
3. For D8, when a closure captures a field alias, should the primary diagnostic
   be phrased as alias escape, closure capture, or both?
4. For D6, what is the narrowest AST-level representation that proves
   `YieldedAcrossSuspend` without duplicating coroutine save/restore logic in
   codegen?
5. Once #1295 lands, which fallback-by-function-name cases can be deleted
   immediately, and which need a temporary compatibility test to ensure no drift
   between parser surface and checker output?

These are prototype-shaping questions. None of them reopen the upstream
authority on ownership semantics.

## 11. Downstream slices

This spec is the deliverable for the analysis-substrate spec milestone of
issue #1399. Two further slices follow, both held until v0.4.0 publication
completes:

**Slice 2 — TypedProgram `handle_ownership_kinds` seam.** Threads the
ownership-classification wire field across the msgpack boundary — producer
(`hew-types/src/check/`) → boundary validator
(`hew-types/src/check/admissibility.rs`) → serializer
(`hew-serialize/src/msgpack.rs`) → C++ reader
(`retired C++ backend msgpack_reader.cpp`) → codegen consumer
(`retired C++ backend MLIRGen.cpp`). The producer returns an empty map in
this slice; no existing program changes drop-emission shape. The reference
implementation is the analogous `method_call_receiver_kinds` field. The
soundness matrix row `handle_ownership_kinds` (`docs/specs/typedprogram-soundness-matrix.md`)
is graduated from *planned* to *on the wire* in the same diff.

**Slice 3 — Prototype move-checker analysis.** Populates ownership
classifications for one DROP-TODO site (recommended pilot: D8 —
`ClosureCapture` / `FieldAlias`) behind the `--experimental-handle-safety`
flag. New `e2e_move_checker/` acceptance and rejection fixtures exercise the
classifications; a calibration memo records agreement and divergence with the
existing field-alias scanner. The prototype is read-only relative to the live
oracles; it does not change drop-emission shape for any existing program.

Neither slice modifies the vocabulary or authority defined in this spec.
