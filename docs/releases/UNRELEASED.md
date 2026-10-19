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

A `var self` method can insert into and remove from `HashMap` and `HashSet`
fields in place: a fault from a key's `Hash` or `Eq` now leaves the collection
with its owner instead of releasing it. A lambda actor that captures a `var`
binding can assign and mutate it as state across messages. A `receive gen fn`
that writes one of its actor's state fields is refused at check time; such a
write previously landed on the stream's snapshot and was silently lost.
A `gen fn` can mutate its `var` parameters, and a `gen { ... }` block that
writes a captured binding is refused with a diagnostic instead of failing in
lowering.
