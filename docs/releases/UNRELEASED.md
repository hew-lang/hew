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

A quoted import (`import "lib.hew";`) is now a module like any other. Private
functions, constants and a `fn main` in the imported file stay private to it,
so a runnable file can be imported for its public items, and two imported files
may each keep a private helper of the same name. Range slices inside a
quoted-imported file lower as they do at the root. A private actor or
supervisor in an imported module may call the module's private helpers and
read its own supervisor's children.
