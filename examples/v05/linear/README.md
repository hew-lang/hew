# `#[linear]` branch-sensitive fixtures

The move checker uses a four-state lattice
(`Uninit | Live | Consumed | MaybeConsumed`). The `.hew` fixtures below test
those cases end to end: they confirm that `hew fmt` round-trips
`#[linear]` and `consume self` correctly and that `hew compile` applies the
move checker to user-authored source.

## Fixtures

### `reject/` — must produce `E_MUST_CONSUME` (exit 1)

| Fixture                                  | Lattice condition                                      |
| ---------------------------------------- | ------------------------------------------------------ |
| `linear_unconsumed_fall_through.hew`     | Live → Return → MustConsume                            |
| `linear_consumed_only_some_branches.hew` | Consumed ⊓ Live = MaybeConsumed → Return → MustConsume |

### `accept/` — must compile cleanly (exit 0)

| Fixture                                   | Lattice condition                                           |
| ----------------------------------------- | ----------------------------------------------------------- |
| `linear_consumed_both_branches.hew`       | Consumed ⊓ Consumed = Consumed                              |
| `linear_consumed_via_rollback_on_err.hew` | Consumed ⊓ Consumed = Consumed (commit-or-rollback pattern) |

## Formatter round-trip

`hew fmt` preserves `#[linear]` and `consume self`.
Running `hew fmt` on any fixture in this directory is idempotent.

## Running the fixtures

```sh
# Reject cases — each must exit 1 and emit E_MUST_CONSUME
hew compile examples/v05/linear/reject/linear_unconsumed_fall_through.hew
hew compile examples/v05/linear/reject/linear_consumed_only_some_branches.hew

# Accept cases — each must exit 0 and produce a binary
hew compile examples/v05/linear/accept/linear_consumed_both_branches.hew
hew compile examples/v05/linear/accept/linear_consumed_via_rollback_on_err.hew
```
