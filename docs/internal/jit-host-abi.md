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
symbol moves, and its constructors move with it so the tier never offers an
allocation whose release symbol it withholds.

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

## JIT host requirements

A compliant JIT host **must** expose `stable ∪ codegen-stable`. The `internal`
tier must never appear in a JIT session symbol map.
