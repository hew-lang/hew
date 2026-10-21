# Hew `machine` Specification

A machine is an ordinary value containing one state from a closed set. Its
`step` method evaluates an input against transition rules, collects typed
output values, and commits a new state after successful evaluation. Machines
have independent value copies and use the ordinary enum, collection and
ownership machinery. They do not own a thread, mailbox or output queue.

## Declaration and use

```hew
machine Gate {
    events {
        Open { token: string; }
        Close;
    }
    emits {
        Accepted { token: string; }
        Rejected { reason: string; }
    }

    state Closed;
    state Opened { token: string; }

    on Open: Closed => Opened when !event.token.is_empty() {
        emit Accepted { token: event.token };
        Opened { token: event.token }
    }
    on Open: Closed => Closed {
        emit Rejected { reason: "empty token" };
        Closed
    }
    on Close: Opened => Closed;
    default { state }
}

fn main() {
    var gate: Gate = .Closed;
    let saved = gate;
    let report = gate.step(.Open { token: "hello" });
    for output in report.outputs {
        match output {
            .Accepted { token } => println(token),
            .Rejected { reason } => println(reason),
        }
    }
    println(gate.state_name()); // Opened
    println(saved.state_name()); // Closed
}
```

A declaration requires at least one state and one input event. States and
inputs can carry named fields. The optional `emits` header declares a separate
output vocabulary with its own named fields. An output need not be an input,
and emitting an output with the same name as an input does not process that
input recursively.

State constructors and patterns follow ordinary enum rules. The caller
chooses the initial state explicitly. Ordinary assignment can replace a
machine value; `step` is the checked transition evaluator, not an exclusive
capability to construct state values.

## Generated API

For `machine M`, the compiler supplies:

| Item | Meaning |
| --- | --- |
| `M` | The state enum |
| `M.Event` | The input enum |
| `MOutput` | The independent output enum |
| `MStepDisposition` | `Taken` and `Ignored` variants |
| `MStep` | Report with `outputs: Vec<MOutput>`, `disposition: MStepDisposition`, `input: string` and `rules: Vec<i64>` |
| `step(var self, event: M.Event) -> MStep` | Staged evaluation with at most 1024 transition sets |
| `step_with_limit(var self, event: M.Event, limit: i64) -> Result<MStep, MachineStepError>` | Staged evaluation with an explicit positive bound |
| `state_name(self) -> string` | Current state tag |
| `MScope` | Unit variants naming the machine's compound states |
| `MSubtree` | Typed departure snapshots, one variant per compound state |
| `step_observed_with_limit(var self, event: M.Event, limit: i64, requested: Vec<MScope>)` | The same bounded evaluator, returning `Result<MachineObservedStep<MStep, MScope, MSubtree>, MachineStepError>` |

Each machine implements the compiler-generated `Machine` interface. Its
associated `Event` and `Step` types identify the input and report types;
`Scope` and `Subtree` identify the opt-in departure observation types;
`step_info(report)` returns the common `MachineStepInfo` view. Handwritten
implementations cannot replace the checked machine evaluator. A report's
`input` is the source event tag; `rules` contains the zero-based source rule
ordinals in body execution order. It is empty when no external or internal transition is taken.

`MStep` is must-use: discarding it as an expression produces the ordinary
must-use diagnostic. A caller can explicitly acknowledge deliberate discard
with `let _report = machine.step(event);`.

Without an output header, `MOutput` is an empty enum and every successful
report contains an empty vector. There is no dummy output or `take_emits`
method. Reports, states and outputs have independent value semantics,
including owning strings, bytes and collections.

## Rule selection and coverage

A rule has the form:

```hew,ignore
on Input: Source => Target reenter when condition { field: value }
```

`reenter` and `when condition` are optional. A rule with no body ends in `,`
and constructs a unit target; a rule to a fielded target writes the field list
with the target name elided (`=> Opened { token: event.token }`). An expression
body remains for a computed target, a rule that also emits, and the identity
`{ state }`. A body that is nothing but the target the head already named is
refused with `E_MACHINE_REDUNDANT_TARGET`. A fixed target requires that target
variant on every normal path, with every payload field initialized. `_` in the
target position permits any state variant.

Rules for a specific source state precede rules with wildcard source `_`.
Within the same priority, guards are tested in source order. The first passing
rule is selected. Each state/input pair needs an unconditional fallback at the
same or a lower priority; complementary-looking guards alone are not a proof
of coverage. An unconditional rule makes later rules at the same priority
unreachable.

`default { state }` explicitly ignores otherwise unmatched inputs, returns
`Ignored`, preserves the state and produces no outputs. It also covers future
unmatched inputs. The default is an identity fallback; computation belongs in
explicit rules. Every selected explicit rule returns `Taken`, including an
intentional same-state transition.

Inside a rule, `state.field` refers to the refined source payload and
`event.field` to the selected input payload; `state` alone denotes the current
machine value. These are the only implicit bindings: `self` is an actor or
method receiver and is refused here with `E_MACHINE_SELF`. Head bindings such
as `on Input { token }: ...` give the body a local alias for `event.token`.

## Hooks and wildcard targets

States can declare `entry { ... }` and `exit { ... }` blocks. For a fixed
changed-state target, evaluation order is exit, transition body, entry. A fixed
same-state target executes only its body unless it says `reenter`.

A wildcard target **always** executes exit, body and entry, including when the
body returns the source state's tag. `reenter` is allowed but redundant on a
wildcard target. This rule determines exit timing before the body runs;
evaluation never speculates about or repeats the body to discover its target.

Hooks operate on staged payload values. Source exit mutations are visible to
the transition body; destination entry mutations become part of the committed
state. Output order follows evaluation order across hooks and body.

## Purity, ownership and failure

Guards, hooks, transition bodies and their transitive source helpers are
synchronous value computations. Local mutation and allocation are allowed.
I/O, actor interaction, spawning, suspension, unsafe access and
execution-context reads are not admitted. Purity restricts what a transition
does, not the types its state holds: an `Rc`, an actor handle or a
`#[resource]` payload moves through transitions as an ordinary value. The
checker proves each selected call through its declaration identity or a closed
pure runtime operation; unknown and indirect calls have no such proof and are
rejected. Releasing a `#[resource]` runs its `close`, so the checker proves the
`close` of every resource a transition's values, its helpers' values or a
concrete instantiation's states and events can reach, the same way. The
compiler does not infer purity from function names.

A step stages an independent copy of its machine, so a state payload must
support one. A `#[resource]` held directly has none and is refused at the
transition that takes it, naming the machine, state, field and type; holding
it through `Rc` shares it across the staged copy and releases it once.
Computation can still fail with an ordinary checked fault, such as division by
zero. Evaluation stages the receiver, candidate and outputs; it commits only
after the complete report is constructed. A fault before commit leaves the
caller's original value intact and releases partial outputs and candidate
payloads through ordinary ownership cleanup. A successful report remains valid
after the machine changes or leaves scope.

The surrounding application interprets output data and performs effects.
Embedding the value in an actor does not move those effects into `step`.

## Hierarchy

A compound state groups leaf states or other compounds. Exactly one direct
child is chosen by `initial state Child` or an explicit `initial Child { ... };`
constructor. Targeting a compound follows those choices until a leaf is reached.
An explicit constructor supplies child payload using the newly supplied parent
payload; inherited parent fields carry through automatically. Each nested initial
constructor runs once on fresh descent, before entry hooks. Targeting a leaf
directly bypasses initial constructors and supplies its full inherited payload.
Compound fields are inherited by descendant leaves, and an
inherited field name cannot be redeclared. State names remain unique within a
machine; a qualified transition head such as `Connection.Handshake.Waiting`
must follow the declared parentage.

For each event the active leaf's rules precede its nearest ancestor's rules,
then successively broader ancestors, then machine-wide wildcards. A guarded
rule that does not match falls through in that order. An unconditional rule
ends fallback; narrower rules take priority regardless of their source order.

A named transition retains the source and target's nearest common ancestor.
It exits only the active descendants below that boundary, from leaf outwards,
evaluates the body once, and enters target descendants from outside in. A
transition to an active ancestor descends through that ancestor's initial
children. Targeting an active compound exits and enters its descendants even
when its initial leaf is already active; the compound itself runs hooks only
for `reenter`. A fixed leaf self-transition runs no hooks unless marked `reenter`;
reentry of an active compound also exits and enters that compound. Hook changes,
state and outputs still stage together before the ordinary commit.

A computed `_` target has the machine root as its transition domain. It exits
the full active branch before evaluating the body once, then enters the full
branch of the returned leaf. This includes same-leaf and sibling returns;
shared compounds exit and re-enter for a computed target. Named sibling targets
continue to retain their shared ancestors. Code that previously used `_` for
an intra-compound move and relied on skipped compound hooks should name the
target instead.

## Parallel regions

A parallel state holds one active configuration per named region. Regions may
contain exclusive compounds or further parallel states. State names remain unique
within the machine; region names are scoped to their containing parallel state.

```hew
machine Checkout {
    events { Advance; Cancel; }
    parallel state Running {
        region payment { initial state Waiting; state Paid; }
        region stock { initial state Empty; state Reserved; }
    }
    state Stopped;
    on Advance: Waiting => Paid;
    on Advance: Empty => Reserved;
    on Cancel: Running => Stopped;
    default { state }
}

fn main() {
    var checkout: Checkout = .Running { payment: .Waiting, stock: .Empty };
    let saved = checkout;
    let report = checkout.step(.Advance);
    assert(report.rules == [0, 1]);
    match checkout {
        .Running { payment: .Paid, stock: .Reserved } => {}
        _ => panic("both regions must advance"),
    }
    match saved {
        .Running { payment: .Waiting, stock: .Empty } => {}
        _ => panic("the saved configuration changed"),
    }
}
```

Each region field has an inferred closed enum type. Regional leaves are variants
of that field's type; they are not additional root variants. Parent payload is
stored once alongside the region fields. Ordinary constructors and destructuring
expose this structure, and ordinary copies remain independent. Construction does
not execute hooks.

Selection observes one pre-step configuration. Deeper sources have priority,
with source declaration order breaking ties. A shared ancestor candidate is
considered once. Candidates conflicting with an already selected transition are
skipped before their guards run; each eligible guard runs at most once. All
compatible disjoint regional transitions are selected before exits or bodies.
An enabled descendant therefore defeats a conflicting ancestor rule for the same
input, including a parent cancellation rule.

Exit hooks run deepest first, selected bodies run in source order, and entry
hooks run outermost first. Equal-depth hooks follow region declaration order.
Shared hooks run once. Regional rules and hooks may read shared ancestor payload
but cannot mutate it; an ancestor-owned rule can update that ancestor's payload.
State, outputs and the ordered report commit together after every selected body
and entry succeeds.

An in-region named destination retains its common ancestors and preserves other
regions. A named destination in a sibling region exits and re-enters the containing
parallel configuration, initializing its other regions. Its configuration-write
domain conflicts with transitions below that parallel state; unrelated outer
regions can still advance. A computed `_` destination exits the complete active
configuration and enters the returned root value. In such a body, `state` is the
complete staged root configuration.

A transition constructing a parallel state may omit regional fields to use their
initial configurations. A region with an initial payload can declare a separate
constructor:

```hew,ignore
region stock {
    state WaitingForStock { count: i64; }
    initial WaitingForStock { count: state.initial_stock };
}
```

The initializer runs on fresh descent inside the staged transaction. It may read
the enclosing payload, including newly supplied parent fields, but cannot read
an input event or another region, emit outputs, or perform effects. Explicit
region values bypass initial construction for those fields. `initial state Name`
remains the shorthand for an initial child. Separate initial constructor clauses
work in regions and exclusive compounds, including compounds nested in regions.

## Final states and internal reactions

A `final state` completes its containing exclusive state. A parallel state is
complete when every active region has reached a final descendant. Completion
propagates through nested compounds. A final state can carry payload and ordinary
entry and exit hooks.

`on completion: Compound => Target` reacts to a completed compound. An
`always: Source => Target` rule is eligible whenever its source is active and its
guard succeeds. Both use the ordinary target, guard, history and hook rules;
neither binds an input `event` or accepts event-head payload bindings. The exact
lower-case name `completion` is reserved in the input vocabulary; `Completion`
remains an ordinary event name.

```hew
machine Job {
    events { Start; }
    state Idle;
    state Running {
        initial state Working;
        final state Ready;
    }
    state Finished;
    on Start: Idle => Running;
    always: Working => Ready;
    on completion: Running => Finished;
    default { state }
}

fn main() {
    var job: Job = .Idle;
    let report = job.step_with_limit(.Start, 3).expect("stabilization fits the limit");
    assert(job == .Finished);
    assert(report.rules == [0, 1, 2]);
}
```

After the external transition set, the evaluator repeatedly selects completion
rules, then selects `always` rules only if no completion rule is enabled. Each
set uses deepest-owner priority, declaration order and the same compatibility
rules as an external set. Even an ignored external input initiates stabilization.
Construction by itself does not run internal reactions.

Each nonempty compatible set consumes one unit of the limit, including the
external set. Simultaneous transitions in independent regions count once. A
successful report combines outputs and rule ordinals from every set in execution
order, and keeps the original external input tag. `Taken` means at least one
external or internal transition was selected.

A nonpositive bound returns `.Err(.InvalidLimit)`. When another set is enabled
but the bound is exhausted, the call returns `.Err(.LimitExceeded)` before that
set's exit hooks. Both errors leave the original machine, saved history and
outputs unchanged. Ordinary hook or helper faults propagate as usual and also
roll back the whole call. `step(event)` uses a bound of 1024 and raises an ordinary
fault if stabilization exceeds it.

## Shallow history

A `history` target restores a compound's last active immediate child and that
child's own payload. The history is saved after exit hooks, before the transition
body. A compound child resumes through its initial descendants: shallow history
does not remember the child's deeper active branch. A parallel target restores
one immediate child per region. Normal entry hooks run on every restored branch.

```hew
machine Editor {
    events { Open; Review; Pause; Resume; }
    state Closed;
    state Editing {
        initial state Draft;
        state Reviewing;
    }
    on Open: Closed => Editing;
    on Review: Draft => Reviewing;
    on Pause: Editing => Closed;
    on Resume: Closed => Editing history;
    default { state }
}

fn main() {
    var editor: Editor = .Closed;
    let _opened = editor.step(.Open);
    let _reviewing = editor.step(.Review);
    let _paused = editor.step(.Pause);
    let _resumed = editor.step(.Resume);
    assert(editor.state_name() == "Reviewing");
}
```

The target must name a compound. When no history has been saved, its ordinary
initial constructors run. Parent payload comes from the transition's constructor;
history supplies the immediate child's payload. Explicit region values override
history for those regions. `history reenter` also exits and re-enters an already
active target compound, saving its current child before restoring it.

Saved history belongs to the machine value. Ordinary copies retain independent
history; equality and supported JSON/CBOR codecs include it. Constructing a new
state value starts with empty history. A failed exit, initializer, body or entry
commits neither the candidate configuration nor its saved history or outputs.

## Deep history as an ordinary library value

`std.machine.history` retains requested compound subtrees alongside a machine.
The owner is an ordinary value: copying it copies both the current machine and
its saved subtrees. `state()` returns the current machine; `saved(target)`
returns the latest departed subtree or `None` before its first departure.

```hew
import std.machine.history;

machine Document {
    events {
        Pause;
        Resume { title: string; saved: Option<DocumentSubtree>; }
    }
    state Closed;
    state Editing {
        title: string;
        state Review { text: string; }
        initial Review { text: "new draft" };
    }
    on Pause: Editing => Closed;
    on Resume: Closed => _ {
        match event.saved {
            .Some(.Editing { snapshot }) => snapshot.restore(event.title),
            .None => Editing { title: event.title },
        }
    }
    default { state }
}

fn main() {
    let initial: Document = .Closed;
    var owner = history.track(initial, [DocumentScope.Editing]);
    let _first = owner.step(.Resume { title: "first", saved: owner.saved(.Editing) });
    let _pause = owner.step(.Pause);
    let _resume = owner.step(.Resume { title: "reopened", saved: owner.saved(.Editing) });
    assert(owner.state() == .Review { title: "reopened", text: "new draft" });
}
```

Each requested departure saves the active descendants and their complete
payloads, including all nested parallel regions. It excludes the requested
compound's own payload, its ancestors' payloads and native shallow-history
slots. A `DocumentSubtree.Editing { snapshot }` contains a
`DocumentEditingSnapshot`; its variants name the possible active
configurations below `Editing`.

`snapshot.restore(...)` constructs an ordinary machine value. Its arguments
provide fresh enclosing payloads in outer-to-inner declaration order. When the
requested compound lies inside a parallel region, the arguments also provide
the sibling region configurations, in the enclosing state's field order.
Sibling parameters start with `Parent_region`; scalar parameters start with
their source field names. A numeric suffix disambiguates collisions. For example, restoring `Nested` inside `Work.left` with
`Work.seed`, sibling `Work.right` and `Nested.revision` takes
`restore(seed, Work_right, revision)`. The saved descendants remain intact.
Returning the constructed value from a computed transition runs normal entry
hooks and participates in the ordinary staged commit. Before the first saved
departure, the caller's `None` arm uses the normal initial constructors.

The low-level `step_observed_with_limit` returns `report` and an ordered
`departures` vector. Each departure has a typed `target` and `subtree`.
Capture occurs after the transition set's exit hooks and before its bodies;
within a set, deeper scopes precede ancestors, with source declaration order
breaking ties. Duplicate requests capture a scope once. Internal completion
and `always` sets use the same boundary, so a subtree entered and exited during
one call is observable even when absent from the final machine. Unrequested
subtrees are not captured. Ordinary `step` and `step_with_limit` request none.

The library stages its current machine and retained entries together. Its
`step_with_limit(event, limit)` returns the ordinary typed bound errors; its
`step(event)` uses the default bound of 1024 transition sets. A bound error or
an ordinary guard, exit, body or entry fault commits neither machine nor saved
subtrees. Saved values returned earlier remain independent of later departures.

## Observed ownership

`std.machine.watch` adds explicit observation around an independent machine
copy. The machine evaluator stays synchronous; an observed owner's `step`
uses ordinary actor service suspension. This example uses the threaded scheduler;
the documentation runner checks its compilation without deterministic execution.

```hew,no_run
import std.machine.watch;

machine Counter {
    events { Add { value: i64; } }
    state Ready { value: i64; }
    on Add: Ready => Ready { value: state.value + event.value }
    default { state }
}

fn main() {
    let initial: Counter = .Ready { value: 0 };
    let (owner, feed) = watch.observe(initial, capacity: 8, subscribers: 2)
        .expect("observe");
    var live = owner;
    let updates = feed.subscribe().expect("subscribe");
    assert(updates.initial().revision == 0);
    let _ = live.step(.Add { value: 4 });
    let notice = updates.recv().expect("history retained").expect("notice");
    assert(notice.revision == 1);
    live.close();
}
```

`Owner<M.Event, M.Step>` exclusively holds writable identity. Moving it transfers ownership;
it cannot be copied or cloned. `Feed<M>` is copyable and permits subscription
only. A subscription is an owning resource; closing or dropping it detaches
its cursor and readiness waiter. Neither feed nor subscription exposes the
private actor identity or a callable that can step it.

Subscribing atomically returns a snapshot at revision `r` and registers a
cursor for `r + 1`. Each successful input commits one notice containing the
post-step state, revision and `MachineStepInfo`, including ignored inputs and
same-state transitions. Step outputs belong to the returned report and do
not enter the observation history. A failed step publishes neither state nor
notice. Machine state and reports must satisfy `Send` to use this wrapper.

The owner retains at most `capacity` notices for at most `subscribers`
registered cursors. Readiness is coalesced; slow subscribers never block a
commit. A subscriber that falls behind receives `Lag { last_seen,
available_from }` and detaches instead of silently skipping notices. It may
subscribe again for a new atomic snapshot. Closing the owner rejects new
subscriptions, lets existing subscribers drain retained history, then returns
`None`; lag still takes precedence when the retained range was exceeded.
Creating another owner creates an independent revision sequence and identity.
Dropping a subscription detaches its cursor and pending receive before resource
cleanup completes. An ordinary `scope within` can bound a receive.

An ordinary receive call can also be selected repeatedly without consuming
the subscription. A timer win cancels and drains that receive before its arm
runs; the next iteration can receive from the same cursor. Ordinary-call
preparation can advance the cursor before selection; cancelling a completed
receive does not undo that consumption (§4.11.1 of the language specification):

```hew,ignore
loop {
    select {
        item from updates.recv() => {
            match item {
                .Ok(.Some(notice)) => println(notice.revision),
                .Ok(.None) => break,
                .Err(lag) => panic(f"history incomplete: {lag}"),
            }
        }
        after 1s => println("still waiting"),
    };
}
```

The wrapper uses ordinary Hew actors, bounded streams and resource cleanup.
It invokes the same generated evaluator as direct `M.step`; it has no second
transition evaluator or observer callback execution inside commit.

## Implementation scope

The native evaluator supports ordinary machines, owning value payloads,
guarded fallback, source wildcards, dynamic targets, hooks and typed outputs.
A generic machine is admitted; each concrete instantiation must support the
staged copy, so an argument with none is refused at the transition that takes
it. A direct `#[resource]` payload waits for a step that takes an affine
receiver and hands it back on a pre-commit fault. `usize` const parameters and
nested composite states are admitted: a const parameter's declared default is
its value. Compound parentage, initial descent and transition domains are
checked before generating ordinary state declarations. Additional
source forms are not yet admitted by this path. Parser or diagram support for
a form is not evidence of executable support.

Machine normalization precedes body checking and HIR. Generated state and
report declarations use the same checked type, call, ownership SIR, physical
MIR and native backend contracts as source-written enums and methods.
Sandbox execution remains a separate parity goal requiring successful
compilation and execution of the same behaviour.
