# Cluster observation

`std.net.cluster` exposes this node's current membership and registration
knowledge. Node identity is the authenticated `NodeId`; `Member.session` is the
durable session incarnation. Receiver-local route slots are never exposed.

```hew
import std.net.cluster;

fn observe() {
    let observation = cluster.watch().expect("node running");
    println(observation.initial.members.len());
    while observation.changes.recv().is_some() {
        match observation.snapshot() {
            .Ok(current) => {
                for registration in current.registrations {
                    println(registration.name);
                }
            }
            .Err(.NotRunning) => break,
            .Err(.Limit) => panic("unexpected snapshot limit"),
        }
    }
    observation.wait_shutdown();
}
```

`snapshot()` copies the current local view. Its `local` member identifies the
running node, `members` includes local and authenticated peer sessions, and
`registrations` includes local and learned names. `local_registrations()` filters
those names by the local stable identity. These are local observations, not a
quorum or a guarantee of peer liveness. Membership and names are read from their
canonical owners; the snapshot does not promise an atomic transaction across
both collections.

`watch()` subscribes before taking `initial`. A racing mutation therefore
appears in the initial snapshot or leaves readiness queued, possibly both.
Readiness coalesces: several mutations can produce one item, so read a fresh
snapshot after receiving. This is a latest-view watch, not a lossless event log.
At most 64 subscriptions may be live on one node; additional watches fail with
`ObserveError.Limit`. Closing or dropping a subscription releases its slot.

`wait_registered(name)` returns the current exact `Location` when the name is
present. `wait_departed(member)` waits until that exact identity and session is
absent, dead or left. A higher-session rejoin cannot conceal the predecessor's
departure. A dropped or replaced connection is not a departure: the session stays
present until it reconnects or the failure detector declares it dead. Both
helpers subscribe before checking and compose with ordinary scope cancellation. Locations are discovery metadata; obtain callable actor
values through authenticated service grants.

A subscription remains bound to its original node instance across shutdown and
restart. Its snapshots fail with `ObserveError.NotRunning` after admission
closes. Its `changes` stream ends only after teardown completes; queued
readiness may precede EOF. `observation.wait_shutdown()` drains that stream to
terminal completion. It does not request shutdown, and scope cancellation can
release the wait. A new watch or global snapshot requires a running node.
