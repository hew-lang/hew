# `#[linear]` branch-sensitive fixtures

The CFG-construction lane that introduced the four-state move
lattice (`Uninit | Live | Consumed | MaybeConsumed`) pins its
`#[linear]` behavioural assertions as inline-source tests in
`hew-mir/tests/vertical.rs`.  The `.hew` fixture files below mirror
those cases end-to-end: they confirm that `hew fmt` round-trips
`#[linear]` and `consuming self` correctly (issue #1795, now resolved)
and that `hew compile` applies the move-checker to user-authored source.

## Fixtures

### `reject/` — must produce `E_MIR_CHECK: MustConsume` (exit non-zero)

| Fixture | Lattice condition | Mirrors |
|---|---|---|
| `linear_unconsumed_fall_through.hew` | Live → Return → MustConsume | `linear_unconsumed_single_exit_fires_must_consume` |
| `linear_consumed_only_some_branches.hew` | Consumed ⊓ Live = MaybeConsumed → Return → MustConsume | `linear_consumed_only_in_then_branch_rejects` |

### `accept/` — must compile cleanly (exit 0)

| Fixture | Lattice condition | Mirrors |
|---|---|---|
| `linear_consumed_both_branches.hew` | Consumed ⊓ Consumed = Consumed | `linear_consumed_in_both_branches_accepted` |
| `linear_consumed_via_rollback_on_err.hew` | Consumed ⊓ Consumed = Consumed (commit-or-rollback pattern) | demonstrates both exit paths must consume |

## Formatter round-trip

`hew fmt` preserves `#[linear]` (via `ResourceMarker::Linear`) and
`consuming self` (via `consuming_methods`) as of the #1795 fix.
Running `hew fmt` on any fixture in this directory is idempotent.

## Running the fixtures

```sh
# Reject cases — each must exit 1 and emit E_MIR_CHECK: MustConsume
hew compile examples/v05/linear/reject/linear_unconsumed_fall_through.hew
hew compile examples/v05/linear/reject/linear_consumed_only_some_branches.hew

# Accept cases — each must exit 0 and produce a binary
hew compile examples/v05/linear/accept/linear_consumed_both_branches.hew
hew compile examples/v05/linear/accept/linear_consumed_via_rollback_on_err.hew
```

## Deferred

- Calling a consuming method directly (`t.commit()`) to satisfy the
  must-consume obligation — deferred until method-call HIR lowering lands.
- `?`-bearing fixtures (`resource_early_close_propagates_err`) — deferred
  until the `?` operator and `Result`-propagation surface is added.
- Codegen: emitting a binary that calls `close(consuming self)` on scope
  exit — `Instr::Drop { drop_fn: Some(_) }` is still fail-closed.

The inline tests in `hew-mir/tests/vertical.rs` remain the
authoritative move-checker gate on the four-state lattice.
