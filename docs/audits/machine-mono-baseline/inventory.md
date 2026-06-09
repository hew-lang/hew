# Machine-mono baseline inventory (W3.033c Stage 0)

**Audit SHA**: `0b955839667bc0d31409c9dbe362457366f3ed88` (v05-int tip; base of `feat/machine-mono-foundation`)
**Date**: 2026-05-27
**Lane**: W3.033c (MachineLayout deferred-build redesign)
**Purpose**: Stage 0 confirmation re-run per
`.tmp/orchestration/plans/waves/w3/w3.033c-machinelayout-redesign-plan.md §Stage 0`.
Stages 1.5+ depend on these four invariants holding at the lane's base SHA.

---

## Verdict-A — HIR mono pass is FUNCTION-ONLY

`module.monomorphisations` is the function-mono output. At the audit SHA
there is exactly **one** writer reaching `MonoRegistry::insert`:

- `hew-hir/src/lower.rs:3193` — `self.mono_registry.insert(mono_key)`.
  Surrounding function records `(origin: ItemId, type_args: Vec<ResolvedTy>)`
  for `Call` lowering only. The `MonoKey` is constructed at
  `hew-hir/src/lower.rs:3188-3192` from a `Call` site's resolved type args.

No `HirItem::Machine` arm in any lowering walker writes to
`mono_registry`. Verified via:

```text
$ git grep -nF 'mono_registry.insert' hew-hir/src/
hew-hir/src/lower.rs:3193:        match self.mono_registry.insert(mono_key) {
```

Conclusion: **Verdict-A confirmed** at `0b955839`. The machine-mono
discovery surface does not yet exist; this lane creates it.

---

## Band-aid still load-bearing — `default_machine_type_params_to_i64`

Definition at `hew-mir/src/lower.rs:2415-2473`. The helper rewrites any
`ResolvedTy::Named { name, args: [] }` whose `name` is a member of the
enclosing machine's `type_params` set into `ResolvedTy::I64`, recursing
through compound types.

Call-graph at audit SHA (8 sites, all inside `hew-mir/src/lower.rs`):

| Line | Caller context |
|---|---|
| 1169 | `MachineLayout` state-field type build (Stage 3 deletes) |
| 1181 | `MachineLayout` event-field type build (Stage 3 deletes) |
| 2430 | Recursive — `Named.args` |
| 2437 | Recursive — `Tuple` elements |
| 2443 | Recursive — `Function` params |
| 2445 | Recursive — `Function` return |
| 2454 | Recursive — `Closure` params |
| 2456 | Recursive — `Closure` return |
| 2459 | Recursive — `Closure` captures |
| 2467 | Recursive — `Pointer.pointee` |
| 2470 | Recursive — `Task` inner |

Captured via:

```text
$ git grep -nF 'default_machine_type_params_to_i64' hew-mir/src/
hew-mir/src/lower.rs:1169:                                .map(|f| default_machine_type_params_to_i64(&f.ty, md))
hew-mir/src/lower.rs:1181:                                .map(|f| default_machine_type_params_to_i64(&f.ty, md))
hew-mir/src/lower.rs:2415:fn default_machine_type_params_to_i64(ty: &ResolvedTy, md: &HirMachineDecl) -> ResolvedTy {
hew-mir/src/lower.rs:2430:                .map(|arg| default_machine_type_params_to_i64(arg, md))
hew-mir/src/lower.rs:2437:                .map(|elem| default_machine_type_params_to_i64(elem, md))
hew-mir/src/lower.rs:2443:                .map(|param| default_machine_type_params_to_i64(param, md))
hew-mir/src/lower.rs:2445:            ret: Box::new(default_machine_type_params_to_i64(ret, md)),
hew-mir/src/lower.rs:2454:                .map(|param| default_machine_type_params_to_i64(param, md))
hew-mir/src/lower.rs:2456:            ret: Box::new(default_machine_type_params_to_i64(ret, md)),
hew-mir/src/lower.rs:2459:                .map(|capture| default_machine_type_params_to_i64(capture, md))
hew-mir/src/lower.rs:2467:            pointee: Box::new(default_machine_type_params_to_i64(pointee, md)),
hew-mir/src/lower.rs:2470:            ResolvedTy::Task(Box::new(default_machine_type_params_to_i64(inner, md)))
```

The two **load-bearing** sites are 1169 / 1181 (the layout-build call).
Stage 4 deletes both the call sites and the helper definition; the
remaining 9 entries are recursive self-calls inside the helper body.

---

## `MachineLayout` build site (single, pre-mono, name-keyed)

`hew-mir/src/lower.rs:1158-1185` — inside the pre-mono item loop. Each
`HirItem::Machine` produces exactly one `MachineLayout` entry keyed by
the bare declaration name. No `type_args` slot; no mangled-name slot;
no per-instantiation duplication. This matches Evidence §1+§2 of the
plan and is the surface Stage 3 replaces with deferred per-instantiation
build.

The build is unconditional on `type_params.is_empty()` — generic machine
decls produce one bare-name `MachineLayout` today, which is why
`default_machine_type_params_to_i64` is needed to keep field types
representable. Stage 3 adds the `if !type_params.is_empty() { continue; }`
guard that mirrors `hew-mir/src/lower.rs:493-503` (the existing
record/enum skip).

---

## `machine_layout_names: HashSet<String>` collision-unsafe

`hew-mir/src/lower.rs:895-906` — assembles a `HashSet<String>` of bare
machine names and `<Name>Event` synthetic names. Two `HirItem::Machine`
decls with the same bare name (legal under module-qualified shadowing —
the checker's `ItemId` substrate prevents true ambiguity but the set
intentionally drops that information) collapse into a single set entry.
Today this is benign because the set is consulted for `ValueClass`
disambiguation only, and the layout build is bare-name-keyed too — the
set's lossy keying matches the layout vec's lossy keying.

Stage 2/3 makes layouts keyed by `MachineMonoKey { machine_def: HirId, ... }`,
at which point the `HashSet<String>` must either become
`HashSet<HirId>` or be re-grounded against the new per-instantiation
registry. Recording the collision-unsafety here so the Stage 3 reviewer
remembers to walk this site when the layout-vec changes shape. **Not
fixed by Stage 0 or Stage 1.5** — this is a Stage 3 follow-on.

---

## `MachineMonoKey` is absent — no `machine_instantiations` table

```text
$ rg -n 'machine_instantiations|MachineMonoKey|MachineInstantiation' hew-hir/ hew-mir/
(no matches)
```

`HirModule` (`hew-hir/src/node.rs:46`) carries `monomorphisations:
Vec<MonomorphizedFn>` (function-mono only), `record_layouts`,
`enum_layouts` — no machine sibling. `MirModule` likewise has no
per-instantiation machine table; codegen reads bare-name
`machine_layouts` only. Stage 1 + Stage 2 introduce this table.

---

## Sibling pattern reference (for Stage 1.5 module shape)

- `MonoKey` / `MonomorphizedFn` / `MonoRegistry` — `hew-hir/src/monomorph.rs:56,80,222`
- `RecordMonoKey` / `RecordLayout` / `RecordLayoutRegistry` — `hew-hir/src/monomorph.rs:287,307,328`
- `EnumMonoKey` / `EnumLayout` / `EnumLayoutRegistry` — `hew-hir/src/monomorph.rs:500,532,552`
- `mangle()` helper — `hew-hir/src/monomorph.rs:101`
- `mangle_resolved_ty()` — `hew-hir/src/monomorph.rs:124` (private; Stage 1.5
  promotes to `pub(crate)` so `mono::mangle::mangle_instantiation` can reuse it)

All three existing key/layout/registry triples share an identical shape
(`{ origin: ItemId, origin_name: String, type_args: Vec<ResolvedTy> }`),
which is the structural justification for the Stage 1.5 parametric
`MonoKey<Kind>` foundation.

---

## Stage 0 verdict

> **Verdict-A confirmed at `0b955839`.** All four foundation
> preconditions (function-only mono pass, load-bearing band-aid, single
> pre-mono build site, absent `MachineMonoKey`) hold as documented in
> the plan. Stage 1.5 may dispatch.
