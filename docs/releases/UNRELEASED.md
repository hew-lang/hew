# Unreleased

The next 0.6 release replaces string scalar aliases with indexed access and
iteration, and the per-type sorting helpers with generic operations. Follow
the [v0.6.2 migration guide](../migrations/v0.6.2.md) when updating these calls
and rebuild native string consumers against the matching SDK.

Machines can be observed through `std.machine.watch.observe`. A single owner
steps an independent copy and returns its typed outputs; cloneable read-only
feeds create bounded subscriptions with an atomic state/revision snapshot.
Each committed input produces one ordered notice, including ignored inputs.
Slow subscribers receive explicit lag, and owner closure drains retained
notices before end-of-stream. Subscription cleanup detaches pending waits and
releases capacity. The wrapper uses the ordinary machine evaluator and actor
services.

A machine output without fields is emitted by its name alone, `emit Ready;`.
`hew fmt` no longer refuses files that use that spelling, and `emit Ready {}`
is now refused with `E_EMPTY_KEY_BRACES`, as an empty `spawn` key list is;
`hew fmt --migrate` rewrites it.
