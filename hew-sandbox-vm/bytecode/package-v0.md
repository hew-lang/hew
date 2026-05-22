# Sandbox Bytecode IR package v0

**Status:** M1 encoding contract. This document defines the serialized package
shape that a future sandbox VM interpreter will consume. It does not define
execution behavior.

The sandbox bytecode package is an educational IR: it should be easy to inspect,
safe to reject, and stable enough for trace fixtures. It is not a compatibility
layer for native backend internals.

## Normative schema

The machine-readable package shape is
[`sandbox-bytecode-v0.schema.json`](sandbox-bytecode-v0.schema.json).

Every package contains:

- `schema_version`: `hew.sandbox.bytecode.v0`
- `package_id`: deterministic package identifier
- `hew_version` and `compiler_version`
- `profile`: sandbox profile name
- `source_map`: sources and byte span mappings
- `module_graph`: module IDs, import edges, and entry module
- `layouts`: type, record, enum, actor, supervisor, and machine layouts
- `stdlib_symbols`: symbolic references to canonical stdlib APIs
- `capabilities`: open-ended capability metadata used by fail-closed profile
  admission
- `functions`: locals, blocks, instructions, and terminators

## Package metadata

The profile name records the sandbox admission target, for example
`sandbox.educational.v0`. Profile identifiers are strings rather than enums in
v0 because the sandbox profile implementation is deferred.

`hew_version` identifies the language/toolchain version used for frontend
analysis. `compiler_version` identifies the compiler surface that emitted the
bytecode package. Consumers must reject packages whose schema version is not
exactly `hew.sandbox.bytecode.v0`.

## Source map

The source map preserves learner-facing paths and UTF-8 byte ranges. Browser or
host-local absolute paths must not appear in bytecode packages committed as
fixtures. Generated code may use `null` span references, but user-authored
instructions should carry a source span whenever possible.

Source positions use the same rules as the trace contract: one-based line and
column numbers plus zero-based byte offsets, with exclusive end positions.

## Module graph

The module graph records the entry module and every module admitted to the
package. Import edges are symbolic and deterministic. The bytecode package does
not perform module discovery; M2 frontend reuse owns that.

## Functions, locals, blocks, and terminators

Each function contains:

- stable function ID
- module ID
- display name
- parameter and result type IDs
- local slots
- basic blocks
- optional source span reference

Blocks contain ordered instructions and a required terminator. Instructions may
write to a destination local and read operands. Terminators own control-flow
edges such as branch, conditional branch, return, tail call, trap, and
unreachable.

Opcode names and capability metadata are registered in
[`opcodes-v0.md`](opcodes-v0.md).

The package shape deliberately separates `instructions` from `terminator` so
future validation can reject blocks with fallthrough ambiguity.

## Layouts

Layouts are compile-time metadata used by the future interpreter and trace
renderer:

- **Types:** scalar, unit, never, string, regex, vector, record, enum, actor,
  supervisor, machine, function, and opaque capability references.
- **Records:** field names, type IDs, and stable field indexes.
- **Enums:** variant names, tags, and payload type IDs.
- **Actors:** state fields and receive handlers.
- **Supervisors:** declared strategy, child actor layout references, and restart
  limits as metadata only.
- **Machines:** state names, event names, and transition source/target metadata.

These layouts do not execute constructors, mailbox receives, supervisor
decisions, or state transitions in M1.

## Stdlib symbol references

Stdlib calls use symbolic references instead of host function pointers. Each
symbol records:

- stable symbol ID
- module path and function name
- parameter/result type IDs
- optional capability ID
- admission disposition: `allowed`, `rejected`, or `reserved`

Fail-closed behavior requires unresolved or unsupported symbols to produce
structured rejection or runtime failure records, never silent success.

## Capability metadata

Capability IDs are open-ended strings such as `std.fs.read` or
`std.text.regex.compile`. A bytecode package may require capabilities that the
current educational sandbox cannot implement. The sandbox profile must reject
those packages with `sandbox.rejected` trace records before execution.

## Trap kinds

Trap kind names align with Trace Schema v0 so interpreter failures can flow
directly into runtime failure records. The v0 package admits trap metadata for:

- integer overflow
- divide by zero
- invalid local
- invalid block
- invalid call
- invalid enum tag
- invalid record field
- vector bounds
- string bounds
- regex compile
- missing capability
- budget exhaustion
- panic
- unsupported instruction
- internal error

## Reserved concurrent opcodes

M1 includes encoding slots for actor, mailbox, channel, task, select, virtual
sleep, supervisor, and machine operations because downstream bytecode packages
need stable names. These opcodes are encoding-only in this phase. Their
scheduler, mailbox, channel, task, supervisor, and machine semantics remain
deferred to later milestones.
