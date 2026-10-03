# Runtime export classification

This file defines the runtime export classification used by native linking,
source `extern "rt"` admission, generated-code hosts, and C ABI surface generation.
Source-declarable exports form the stable API; every other runtime export is
classified as non-declarable and remains available only to compiler-generated
code or AOT runtime support.

## Source of truth

Runtime methods marked `#[runtime(...)]` in the stdlib own their operation,
export classification and FFI ownership contracts. The compiler derives these
facts during its build; `make cabi-surface` writes the same projection to
`scripts/generated-runtime-declarations.toml` for the Python tooling. Remaining
exports and contracts are owned by `scripts/runtime-export-classification.toml`.
The two sources must not duplicate a symbol.

The combined classification feeds these consumers:

- the stable runtime-symbol set consumed by `hew-mir::runtime_symbols` and
  codegen-rs runtime lowering
- the C ABI surface generator (`make cabi-surface`) and the FFI ownership
  contract projection

## Two-tier model

### `stable`

Handle-oriented, user-visible runtime operations that `extern "rt"` declarations
in Hew source code may name. Generated-code hosts expose these symbols. The type-checker
enforces this boundary: a symbol named in an `extern "rt"` block must occur in
`stable` or `stable-stdlib`.

### `non-declarable` and `non-declarable-stdlib`

The suffix identifies exports implemented in `hew-std`; both tiers have the
same restriction on source declarations.

Every runtime export that user source cannot name through `extern "rt"`. This
tier combines compiler-emitted protocol functions such as safepoints,
task-scope wiring, actor-state locking, execution-context access, and scheduler
bootstrap with lifecycle, session, shutdown, reset, drain, and runtime-control
functions. The checker rejects all of them in user declarations.

Hosts that execute compiler-generated modules expose the runtime symbols
they link; AOT-only lifecycle and process-global functions remain outside the
generated-module symbol map. The shared tier name records the one property relevant to source
admission: user code cannot declare these exports.

## Classification decision flowchart

```
Is this a source-declarable runtime operation?
  Yes → stable (or stable-stdlib for a stdlib export)
  No  → non-declarable
```

## The system lane is not user-declarable

The system message queue is the privileged half of the sys/user channel split:
nodes dequeued with `Origin::Sys` are routed to the actor's `sys_dispatch`
entry point, which reclaims children, restarts them, and delivers `Exit` /
`Down`. The split makes provenance STRUCTURAL inside the queue — a user-queue
node can never be dispatched as a system message — but the queue is not the
only ingress. This classification table is the other one: a symbol in `stable`
can be named by an `extern "rt"` declaration and called directly from a Hew
program, so a privileged operation classified `stable` re-opens by symbol
exactly what the queue split closed by type.

The rule is therefore a first-class part of the provenance boundary and takes
precedence over the rest of the flowchart:

> No `stable` symbol may produce, install, mutate, observe, or destroy
> system-lane state.

The first audit of this table used the narrower property "mints a system node,
drains one, or installs a system dispatch pointer" and missed three symbols
because of it. OBSERVATION and DESTRUCTION are ingress in the same sense as
production: a caller that can distinguish an empty mailbox from one holding a
queued `Exit` has read privileged state (`hew_mailbox_has_messages`), and a
caller that can free the mailbox has silently discarded every pending lifecycle
signal before the scheduler dispatched it (`hew_mailbox_free`). A general
receive that happens to pop the system queue first is a drain even though its
name says nothing about the lane (`hew_mailbox_try_recv`).

Where the privileged and the legitimate question are separable, SPLIT rather
than remove: `hew_mailbox_has_user_messages` answers "is there work for me"
from the `stable` tier while the system-aware `hew_mailbox_has_messages` stays
`non-declarable`. Where they are not separable — destruction is not — the whole
symbol moves, and its constructors move with it *when the object would
otherwise be stranded*: a raw `hew_mailbox_new` mailbox is owned by nobody but
its holder, so a `stable` constructor with a `non-declarable` release symbol is a
leak factory. That is a test about tracking, not a reflex. `hew_actor_free`
moved to `non-declarable` for the same destruction reason and the spawn family stayed
`stable`, because a spawned actor is runtime-tracked — the live-actor registry,
the scheduler and the supervision tree all hold it, and `hew_runtime_cleanup`,
`hew_actor_group_destroy` and supervisor teardown reclaim it — so withholding
the raw destructor strands nothing.

Validating that a caller picked one of the seven `HewSysMsg` kinds checks the
VALUE, not the ORIGIN, and is not a substitute. The legitimate producers are
runtime paths whose event is authenticated by a transition they perform
themselves — `hew_actor_trap` CAS-transitions the child terminal before
notifying its supervisor — not entry points that accept a composed event.
`hew_actor_trap` is itself `non-declarable` for that reason: it took the subject
(`actor`) and the reason (`error_code`) from its own arguments, so as a
`stable` symbol it *was* an entry point that accepts a composed event. What
makes the remaining call sites authenticated is that none of them is
user-declarable.

Capability-scoped requests are NOT ingress: `hew_actor_stop` latches a stop
flag on an actor the caller already holds, and `link` / `monitor` install a
watcher whose `Exit` / `Down` is minted later by the runtime from a real death.
That is the general case, but not the whole of it. When the peer is ALREADY
terminal, installation has no later death to wait for, so it synthesizes the
signal the contract owes immediately — and the destination is the runtime
ABI's first argument. The raw 2-arg `hew_actor_link` / `hew_actor_monitor` are
therefore `non-declarable`, not `stable`: the user surface is 1-arg, and
`hew-mir/src/lower/actor.rs` synthesizes `hew_actor_self()` as arg0 for every
call it emits, so the destination is structurally the CALLING actor and a
program can only cause its own actor to receive a signal it just asked for.
`link_monitor_subject_is_always_the_self_handle` asserts that over every form
the lowering emits. The stable-pid forwarders `hew_local_pid_link` /
`hew_local_pid_monitor` moved to `non-declarable` with them; the `_unlink` /
`_demonitor` siblings stay `stable`, because removing a registration produces
no signal.

The test is whether the caller can put the system channel into a state the runtime
did not derive from an authenticated event, read it, or destroy it.

### The property is transitive

A symbol does not have to touch the lane itself to breach the invariant; it
only has to *call* something that does. `hew_actor_free` names no lane state
in its body, yet it reaches `hew_mailbox_free` four calls down and destroys the
lane there. A symbol is therefore disqualified from `stable` if it, or
**anything it can reach**, produces, installs, mutates, observes, or destroys
system-lane state. Judge a candidate by following its calls, not by reading
its own body.

## Host requirements

A compliant host exposes the stable symbols required by its generated module.
The non-declarable tier is never a source declaration surface; AOT-only symbols
remain outside a generated-module symbol map.
