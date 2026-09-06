# Hew `machine` Specification

A machine is an ordinary value containing one state from a closed set. Its
`step` method evaluates an input against transition rules, collects typed
output values, and commits a new state after successful evaluation. Machines
have independent value copies and use the ordinary enum, collection and
ownership machinery. They do not own a thread, mailbox or output queue.

## Declaration and use

```hew
machine Gate {
    events { Open { token: string }, Close, }
    emits { Accepted { token: string }, Rejected { reason: string }, }

    state Closed,
    state Opened { token: string },

    on Open: Closed => Opened when event.token.len() > 0 {
        emit Accepted { token: event.token };
        .Opened { token: event.token }
    }
    on Open: Closed => Closed {
        emit Rejected { reason: "empty token" };
        .Closed
    }
    on Close: Opened => Closed { .Closed }
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
| `MEvent` | The input enum |
| `MOutput` | The independent output enum |
| `MStepDisposition` | `Taken` and `Ignored` variants |
| `MStep` | Report with `outputs: Vec<MOutput>` and `disposition: MStepDisposition` |
| `step(var self, event: MEvent) -> MStep` | Staged mutable evaluation |
| `state_name(self) -> string` | Current state tag |

`MStep` is must-use: discarding it as an expression produces the ordinary
must-use diagnostic. A caller can explicitly acknowledge deliberate discard
with `let _report = machine.step(event);`.

Without an output header, `MOutput` is an empty enum and every successful
report contains an empty vector. There is no dummy output or `take_emits`
method. Reports, states and outputs have independent value semantics,
including owning strings, bytes and collections.

## Rule selection and coverage

A rule has the form:

```hew
on Input: Source => Target reenter when condition { target_value }
```

`reenter` and `when condition` are optional. A body-less rule ending in `;`
constructs a unit target. A fixed target requires that target variant on every
normal path, with every payload field initialized. `_` in the target position
permits any state variant.

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

Inside a rule, `self.field` and `state.field` refer to the refined source
payload; `event.field` refers to the selected input payload. `state` denotes
the current machine value. Head bindings such as `on Input(token): ...` give
the body a local alias for `event.token`.

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
I/O, actor interaction, suspension, unsafe access, execution-context reads and
external resource identity are not admitted. The checker proves each selected
call through its declaration identity or a closed pure runtime operation;
unknown and indirect calls have no such proof and are rejected. The compiler
does not infer purity from function names.

Input, state and output payloads must support independent value copies.
Computation can still fail with an ordinary checked fault, such as division by
zero. Evaluation stages the receiver, candidate and outputs; it commits only
after the complete report is constructed. A fault before commit leaves the
caller's original value intact and releases partial outputs and candidate
payloads through ordinary ownership cleanup. A successful report remains valid
after the machine changes or leaves scope.

The surrounding application interprets output data and performs effects.
Embedding the value in an actor does not move those effects into `step`.

## Implementation scope

The native evaluator supports concrete ordinary machines, owning value
payloads, guarded fallback, source wildcards, dynamic targets, hooks and typed
outputs. Const parameters, composite state evaluation, unclassified generic
payloads and additional source forms are not yet admitted by this path.
Parser or diagram support for a form is not evidence of executable support.

Machine normalization precedes body checking and HIR. Generated state and
report declarations use the same checked type, call, ownership SIR, physical
MIR and native backend contracts as source-written enums and methods.
Sandbox execution remains a separate parity goal requiring successful
compilation and execution of the same behaviour.
