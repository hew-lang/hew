# JIT host ABI classification

JIT session dylibs must only see the stable and codegen-stable Hew host ABI.
Runtime lifecycle and other process-global hooks stay out of the JIT session
allow-list so a JIT-compiled module cannot reinitialize, drain, or tear down
shared runtime state.

## Source of truth

The source of truth lives in `scripts/jit-symbol-classification.toml`.

We use an out-of-band allow-list instead of inline annotations because the
same reviewed classification file feeds three consumers:

- `scripts/verify-ffi-symbols.py --classify …`
- the required CI lint gate (`make verify-ffi`, which runs `--classify stable --validate`)
- the stable runtime-symbol set consumed by `hew-mir::runtime_symbols` and
  codegen-rs JIT/runtime lowering

That keeps the ABI review surface centralized while still failing closed: the
verifier rejects any `#[no_mangle] extern "C" fn` in `hew-runtime/src/` that
is missing from the file or classified more than once.

## Three-tier model

### `stable`

Handle-oriented, user-visible runtime operations that `extern "rt"` declarations
in Hew source code may name, and that JIT hosts must expose in their symbol map.
The type-checker enforces this boundary: any symbol named in an `extern "rt"`
block that is not in `stable` is a hard compile-time error.

### `codegen-stable`

Symbols the Hew compiler emits into generated LLVM IR for non-trivial actor
programs. Examples: cooperate safepoints (`hew_actor_cooperate`), task-scope
wiring (`hew_task_scope_set_current`), actor-state locking
(`hew_actor_state_lock_acquire` / `_release`), execution-context access
(`hew_require_execution_context`), and scheduler bootstrap (`hew_sched_init`).

These symbols are **NOT** user-callable via `extern "rt"`: the checker rejects
any attempt. They can only appear in compiler-emitted IR. JIT hosts must
provide them alongside the `stable` tier — a JIT host that loads only `stable`
will fail to link any non-trivial actor program.

### `internal`

Lifecycle, session/global-state, scheduler shutdown/reset/drain, and
conservative runtime-control hooks. These are **never JIT-reachable** — not
by user code and not by compiler-emitted IR. They are AOT-linkable only.

When a runtime export is ambiguous, prefer `internal` first and promote it
later with an explicit review.

## Classification decision flowchart

```
Does this symbol produce, install, mutate, observe, or destroy any
system-lane state?
  Yes → NOT stable (codegen-stable if the compiler emits it, else internal)
  No  →
    Is this symbol named by user extern "rt" blocks?
      Yes → stable
      No  →
        Is this symbol emitted by the Hew compiler into IR?
          Yes → codegen-stable
          No  → internal
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
`internal`. Where they are not separable — destruction is not — the whole
symbol moves, and its constructors move with it *when the object would
otherwise be stranded*: a raw `hew_mailbox_new` mailbox is owned by nobody but
its holder, so a `stable` constructor with an `internal` release symbol is a
leak factory. That is a test about tracking, not a reflex. `hew_actor_free`
moved to `internal` for the same destruction reason and the spawn family stayed
`stable`, because a spawned actor is runtime-tracked — the live-actor registry,
the scheduler and the supervision tree all hold it, and `hew_runtime_cleanup`,
`hew_actor_group_destroy` and supervisor teardown reclaim it — so withholding
the raw destructor strands nothing.

Validating that a caller picked one of the seven `HewSysMsg` kinds checks the
VALUE, not the ORIGIN, and is not a substitute. The legitimate producers are
runtime paths whose event is authenticated by a transition they perform
themselves — `hew_actor_trap` CAS-transitions the child terminal before
notifying its supervisor — not entry points that accept a composed event.

Capability-scoped requests are NOT ingress: `hew_actor_stop` latches a stop
flag on an actor the caller already holds, and `hew_actor_link` / `_monitor`
install a watcher whose `Exit` / `Down` is minted later by the runtime from a
real death. Both stay `stable`. The test is whether the caller can put the
system lane into a state the runtime did not derive from an authenticated
event, read it, or destroy it.

### The property is TRANSITIVE, and it is computed

Everything above is a property of what a symbol *does*, and every audit of it
read the symbols one at a time. That method enumerated this table four times
and got four different answers — 3 symbols, then 9, then 16, then 17 — because
a symbol does not have to touch the lane itself to breach the invariant. It
only has to *call* something that does. `hew_actor_free` names no lane state
anywhere in its body; it reaches `hew_mailbox_free` four calls down and
destroys the lane there.

So the rule is stated over the call graph:

> A symbol is disqualified from `stable` if it, or **anything it can reach**,
> produces, installs, mutates, observes, or destroys system-lane state.

and `scripts/sys-lane-closure.py` (`make verify-sys-lane-closure`, part of
`make lint`) computes it rather than asserting it:

1. **Roots** — every function in `hew-runtime/src` and `hew-std/src` whose own
   body names `sys_queue`, `sys_count`, `sys_dispatch`, `HewSysMsg` or
   `Origin::Sys`. Comments and string literals are blanked first so prose can
   neither mint nor hide a root; test-only items are dropped, including whole
   files behind a `#[cfg(test)] mod x;` in their parent. `#[cfg(any(target_arch
   = "wasm32", test))]` is production wasm code and is deliberately NOT dropped.
2. **Reachability** — reverse breadth-first search from the roots over call
   edges, so the result is everything that can reach a lane operation, however
   far away.
3. **Verdict** — the gate fails if any `stable` or `stable-stdlib` symbol is in
   that closure, and prints a witness path for each.

Escapes live in `[sys-lane-closure.authenticated-edges]` and
`[sys-lane-closure.non-roots]` in `scripts/jit-symbol-classification.toml`.
Each needs a written reason, each is checked for staleness, and an
authenticated edge clears exactly one caller→callee pair — a *new* caller of
the same callee still fails. `scripts/tests/test_sys_lane_closure.py` proves
the gate still fails on a transitive reach, so a green run means something.

This does not replace the judgement above; it replaces the enumeration. The
question "is this edge authenticated?" is still answered by a human, but the
question "which edges are there?" is no longer answered by reading.

An edge waiver has one limit worth naming, because the first draft of this
section ran into it. Cutting `free_actor_resources_with_options →
hew_mailbox_free` makes the gate green for *every* caller of that edge at once,
including `hew_actor_free` — the very symbol the transitive rule was written to
catch. What the waiver can honestly say is "the runtime, not the caller, chose
to reclaim this actor", and that sentence is false for a destructor a user
`extern "rt"` block may name and point at any actor it holds. So
`hew_actor_free` is `internal`, and the waiver covers only the routes where the
sentence is true: spawn rollback, `hew_exit` / runtime cleanup, and supervisor
and group teardown. Run `python3 scripts/sys-lane-closure.py --explain
hew_actor_free` after deleting the edge to see the witness path this reasoning
is about.

## JIT host requirements

A compliant JIT host **must** expose `stable ∪ codegen-stable`. The `internal`
tier must never appear in a JIT session symbol map.
