# Unreleased

## Native compilation

O0 compilation removes dead coroutine cleanup helpers after splitting, while
retaining dynamic, exported and explicitly retained function roots. Cancellation
and task-result cleanup preserve their existing behaviour.

## Distributed actors

Cluster admission uses one explicit trust root. Enrolled nodes can authenticate
discovered peers through Noise or TLS relays while actor grants retain control
over service access and revocation. Relayed calls preserve message framing,
origin identity and session lifetime across carrier loss and reconnection.

`Node.peer` now takes the discovered peer's typed `NodeId` instead of credential
text. Configure node enrolment separately, then use the discovered identity to
select its authenticated session. These changes use wire protocol 12; upgrade
communicating nodes together.

## Machines and diagrams

Parallel regions select compatible transitions from one configuration and commit
their state and outputs together. Child initializers support owned payloads.
The `history` target modifier restores the immediate child and its payload;
saved history follows ordinary machine copies, equality and JSON/CBOR encoding.

Final states enable `on completion` reactions, and `always` rules run without an
external event. Completion reactions take priority; selected sets stabilize
before the machine commits. Ordinary `step(event)` allows 1,024 transition sets.
Use `step_with_limit(event, limit)` for an explicit positive bound and a typed
`MachineStepError`; invalid or exhausted limits preserve the original state and
outputs. Internal reactions cannot refer to the external event.

Machine step reports use `rules: Vec<i64>` to retain the source ordinals of every
selected rule. Update consumers of the previous singular rule field.

`hew machine` exports Mermaid, Graphviz/DOT and JSON diagrams of the authored
chart, including nested states, parallel regions, initializers, guards, actions,
hooks, final states, internal reactions and history targets. Qualified names
select machines from imported modules.
