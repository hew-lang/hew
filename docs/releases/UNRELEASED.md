# Unreleased

The next 0.6 release replaces string scalar aliases with indexed access and
iteration, and the per-type sorting helpers with generic operations. Follow
the [v0.6.2 migration guide](../migrations/v0.6.2.md) when updating these calls
and rebuild native string consumers against the matching SDK.

Node identity files now use secure exclusive publication and durable writes. Existing
files must be regular, owned by the current user and private to that user; symbolic
links, reparse points, unsafe permissions and mismatched public/private keys refuse.
Concurrent first-time callers retain one identity. Tighten an existing file's mode or
ACL instead of deleting the key: regenerating it changes the node's identity and pins.
Mesh identity clones share zeroizing private-key storage and redact diagnostic output.

The obsolete `HEW_TRANSPORT=quic` node transport is removed. Use `tcp` or `quic-mesh`
with the existing peer authentication configuration. `std.net.quic` remains available
as the ordinary QUIC socket API.

Native applications can supply rooted `std.fs.ReadDir` and `WriteDir` resources
once when constructing an actor or component. Ordinary `read` and `write`
methods operate on relative child paths, `subdir` narrows the root, and
`open_read` or `open_write` returns an independently owned directional file.
Closing a parent does not close its narrowed roots or opened files.

Contained relative symlinks work; absolute paths, parent escapes and escaping
symlinks refuse without acquiring outside authority. Rooted handles retain the
opened object if its pathname changes. A host may refuse renaming an open root.
Directory acquisition remains ambient and is refused transitively inside a
`#[confined]` boundary; supplied directory operations are permitted.

The browser VM can use an in-memory implementation of the same ordinary service
interface. Native directory resources and hosted filesystem grants are not
available in the browser VM.

Machines can be observed through `std.machine.watch.observe`. A single owner
steps an independent copy and returns its typed outputs; cloneable read-only
feeds create bounded subscriptions with an atomic state/revision snapshot.
Each committed input produces one ordered notice, including ignored inputs.
Slow subscribers receive explicit lag, and owner closure drains retained
notices before end-of-stream. Subscription cleanup detaches pending waits and
releases capacity. The wrapper uses the ordinary machine evaluator and actor
services.
