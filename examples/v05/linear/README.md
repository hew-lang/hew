# `@linear` branch-sensitive fixtures

Each `.hew` source in this directory exercises the four-state move
lattice (`Uninit | Live | Consumed | MaybeConsumed`) introduced when
CFG construction landed real basic blocks for `if`/`else`. The
fixtures cover both arms of the lattice's decision surface:

| Fixture | Path | Expected | What the lattice says |
| --- | --- | --- | --- |
| `linear_consumed_both_branches.hew`        | accept | `Consumed ⊓ Consumed = Consumed` at the join — `@linear` is fully exhausted on every path reaching the function exit. |
| `linear_consumed_via_rollback_on_err.hew`  | accept | Same shape, but each arm uses a different consuming method. Meet still resolves to `Consumed`. |
| `linear_unconsumed_fall_through.hew`       | reject | Neither arm consumes the binding. `Live ⊓ Live = Live` at the Return; per-exit `MustConsume` fires. |
| `linear_consumed_only_some_branches.hew`   | reject | One arm consumes, the other leaves the binding `Live`. `Live ⊓ Consumed = MaybeConsumed` at the Return; per-exit `MustConsume` fires (the canary). |

The accept fixtures use a stand-in consume — `let _t = txn` — until
method-call lowering wires the `commit(consuming self)` /
`rollback(consuming self)` consume sites. The behavioural assertion
the move-checker makes about each fixture is identical either way:
"every path reaching the Return must exhaust the binding". A future
lane that lowers method calls swaps the consume site for a
`Terminator::Call` without touching the fixture's accept/reject
status.

Inline-source counterparts of these fixtures live in
`hew-mir/tests/vertical.rs` as `linear_consumed_in_both_branches_accepted`
and `linear_consumed_only_in_then_branch_rejects` — they are the
load-bearing regression tests. The `.hew` files in this directory
are documentation + a manual-run surface for inspecting the IR
shape via `hew compile-v05`.
