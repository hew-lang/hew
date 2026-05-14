# `@linear` branch-sensitive fixtures

The CFG-construction lane that introduced the four-state move
lattice (`Uninit | Live | Consumed | MaybeConsumed`) pinned its
`@linear` behavioural assertions as inline-source tests in
`hew-mir/tests/vertical.rs`:

- `linear_unconsumed_single_exit_fires_must_consume` — function-
  tail Return on a `Live` binding fires `MustConsume`.
- `linear_consumed_in_both_branches_accepted` —
  `Consumed ⊓ Consumed = Consumed`; no `MustConsume`.
- `linear_consumed_only_in_then_branch_rejects` — the canary;
  `Live ⊓ Consumed = MaybeConsumed` at a Return exit fires
  `MustConsume`.

These run on every `cargo test -p hew-mir` invocation and are the
load-bearing regression coverage.

`.hew` fixture files that exercise the same surface require
`#[linear]` markers and `consuming self` method modifiers, both of
which today's `hew fmt` does not round-trip cleanly — see
the formatter follow-up tracker. Once `hew fmt` preserves the
markers, this directory will gain accept/ + reject/ fixtures
mirroring `examples/v05/checked-mir/reject/`'s shape.

For now: the inline tests are the authoritative move-checker gate
on the four-state lattice; this README is a pointer.
