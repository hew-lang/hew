# Sandbox Bytecode opcode registry v0

**Status:** M1 opcode registry. This document names the instruction families
admitted by [`sandbox-bytecode-v0.schema.json`](sandbox-bytecode-v0.schema.json).
It is an encoding contract only; interpreter behavior is deferred.

All unsupported or unimplemented opcodes must fail closed when execution work
lands. A sandbox implementation must emit a structured rejection before
execution or a `runtime.failure` trace record with trap kind
`unsupported_instruction`; it must not silently skip an opcode.

## Instruction shape

Every instruction has:

- `op`: one registry name from this document
- `dst`: destination local ID or `null`
- `args`: ordered operands
- `span`: source span reference or `null`
- optional `metadata`: JSON value for educational annotations

Terminators are separate from instructions and are listed in
[control-flow terminators](#control-flow-terminators).

## Constants

| Opcode | Result | Operand shape | Notes |
| --- | --- | --- | --- |
| `const.unit` | unit local | none | Unit value. |
| `const.bool` | bool local | literal bool | Boolean value. |
| `const.i64` | i64 local | literal integer | Signed integer within i64 range. |
| `const.u64` | u64 local | literal integer | Unsigned integer represented within JSON safe range for fixtures. |
| `const.f64` | f64 local | literal number | Determinism rules must be ratified before execution. |
| `const.string` | string local | literal string | UTF-8 string. |
| `const.regex` | regex pattern local | literal pattern string | Encoding only; regex compilation may trap or reject later. |

## Locals and moves

| Opcode | Result | Operand shape | Notes |
| --- | --- | --- | --- |
| `local.get` | value local | local ID | Copy or read according to type ownership facts. |
| `local.set` | none | local ID, value | Writes mutable local. |
| `local.move` | value local | local ID | Transfers ownership when the type system allows it. |
| `local.borrow` | borrow local | local ID | Encoding for future borrow-aware lowering. |

The bytecode package records local mutability and type IDs. It does not re-run
borrow checking.

## Arithmetic and checked arithmetic

| Opcode | Result | Operand shape | Trap expectations |
| --- | --- | --- | --- |
| `i64.add` | i64 | lhs, rhs | Native mathematical add; overflow behavior must be explicit before execution. |
| `i64.sub` | i64 | lhs, rhs | Native mathematical subtract. |
| `i64.mul` | i64 | lhs, rhs | Native mathematical multiply. |
| `i64.div` | i64 | lhs, rhs | `divide_by_zero` on zero divisor. |
| `i64.rem` | i64 | lhs, rhs | `divide_by_zero` on zero divisor. |
| `i64.neg` | i64 | value | Negation. |
| `i64.checked_add` | i64 | lhs, rhs | `integer_overflow` on overflow. |
| `i64.checked_sub` | i64 | lhs, rhs | `integer_overflow` on overflow. |
| `i64.checked_mul` | i64 | lhs, rhs | `integer_overflow` on overflow. |
| `i64.checked_div` | i64 | lhs, rhs | `divide_by_zero` or `integer_overflow`. |
| `i64.checked_rem` | i64 | lhs, rhs | `divide_by_zero` or `integer_overflow`. |
| `f64.add` | f64 | lhs, rhs | IEEE-754 add; never traps. |
| `f64.sub` | f64 | lhs, rhs | IEEE-754 subtract; never traps. |
| `f64.mul` | f64 | lhs, rhs | IEEE-754 multiply; never traps. |
| `f64.div` | f64 | lhs, rhs | IEEE-754 divide; zero divisor yields ±inf/NaN, never traps. |
| `f64.rem` | f64 | lhs, rhs | IEEE-754 remainder (LLVM `frem` / C `fmod`); never traps. |
| `f64.neg` | f64 | value | IEEE-754 negation; never traps. |

The v0 registry started with i64 because fixture coverage can be educational
without committing the full scalar matrix. The f64 arithmetic family is
type-directed by the emitter: it selects `f64.*` when the operand type is a
float and `i64.*` otherwise. Unlike the checked i64 ops, the f64 ops follow
IEEE-754 exactly (matching native LLVM `fadd`/`fsub`/`fmul`/`fdiv`/`frem`/
`fneg`), so divide-by-zero produces `inf`/`-inf`/`NaN` rather than a trap.
Additional numeric widths require a schema revision or a documented v0
extension before use.

## Comparisons

Comparison opcodes are type-polymorphic: a single `cmp.*` opcode covers i64,
f64, and string operands. The interpreter dispatches on the runtime value kind
(`compareScalar`), so float comparisons reuse these opcodes directly.

| Opcode | Result | Operand shape |
| --- | --- | --- |
| `cmp.eq` | bool | lhs, rhs |
| `cmp.ne` | bool | lhs, rhs |
| `cmp.lt` | bool | lhs, rhs |
| `cmp.le` | bool | lhs, rhs |
| `cmp.gt` | bool | lhs, rhs |
| `cmp.ge` | bool | lhs, rhs |

Comparison semantics must match canonical Hew for admitted operand types. For
f64 operands, `cmp.eq` is ordered equal (`NaN == NaN` is false) and `cmp.ne` is
unordered not-equal (`NaN != NaN` and `NaN != inf` are true), matching native
LLVM `fcmp oeq`/`fcmp une`. For unsupported types, profile admission or runtime
execution must fail closed.

## Calls

| Opcode | Result | Operand shape | Notes |
| --- | --- | --- | --- |
| `call.direct` | call result | function ID, arguments | Calls a package function. |
| `call.indirect` | call result | callee value, arguments | Reserved for function values. |
| `call.stdlib` | call result | stdlib symbol ID, arguments | Requires symbol admission metadata. |

Stdlib calls must resolve through `stdlib_symbols`. Missing symbols trap as
`invalid_call` or reject by capability metadata before execution.

## Control-flow terminators

| Terminator | Operand shape | Notes |
| --- | --- | --- |
| `br` | target block, args | Unconditional branch. |
| `br_if` | condition, target block, else target, args | Conditional branch. |
| `return` | return values | Function return. |
| `tail_call` | function ID, arguments | Reserved tail-call encoding. |
| `trap` | trap kind | Emits runtime failure. |
| `unreachable` | none | Emits `internal_error` or `unsupported_instruction` until specified. |

Blocks never fall through. Every block has exactly one terminator.

## Records

| Opcode | Result | Operand shape | Notes |
| --- | --- | --- | --- |
| `record.new` | record value | type ID, field values | Field order follows record layout indexes. |
| `record.get` | field value | record, field index/name | Invalid field traps as `invalid_record_field`. |
| `record.set` | record value or none | record, field index/name, value | Requires mutable value semantics later. |

Record layouts live in `layouts.records`.

## Enums

| Opcode | Result | Operand shape | Notes |
| --- | --- | --- | --- |
| `enum.new` | enum value | type ID, variant tag, payload values | Variant tag follows enum layout. |
| `enum.tag` | integer tag | enum value | Used by match lowering. |
| `enum.payload` | payload value | enum value, payload index | Invalid tag traps as `invalid_enum_tag`. |

Enum layouts live in `layouts.enums`.

## Vectors

| Opcode | Result | Operand shape | Trap expectations |
| --- | --- | --- | --- |
| `vector.new` | vector value | element type ID | Empty vector. |
| `vector.push` | none or vector | vector, value | Mutability semantics deferred. |
| `vector.get` | element value | vector, index | `vector_bounds` on invalid index. |
| `vector.set` | none or vector | vector, index, value | `vector_bounds` on invalid index. |
| `vector.len` | integer | vector | Deterministic length. |

The registry names vector operations, not allocation strategy.

## Strings

| Opcode | Result | Operand shape | Trap expectations |
| --- | --- | --- | --- |
| `string.concat` | string | lhs, rhs | UTF-8 preserving concatenation. |
| `string.len` | integer | string | Length unit must be ratified before execution. |
| `string.slice` | string | string, start, end | `string_bounds` on invalid range. |

String behavior must match canonical Hew for admitted operations. Length and
slice units must be documented before interpreter work begins.

## Regex

| Opcode | Result | Operand shape | Trap expectations |
| --- | --- | --- | --- |
| `regex.compile` | regex pattern | pattern string | `regex_compile` on invalid pattern. |
| `regex.is_match` | bool | pattern, input string | Deterministic match answer. |
| `regex.find` | string | pattern, input string | Empty string when no match, matching stdlib contract. |
| `regex.replace` | string | pattern, input string, replacement | Deterministic replacement. |
| `regex.free` | none | pattern | Reserved for handle lifetime parity. |

Regex execution waits for the v0.5 regex substrate and sandbox shim decision.

## Panic and trap

| Opcode | Result | Operand shape | Trace result |
| --- | --- | --- | --- |
| `panic` | none | message value | `panic` runtime failure. |
| `trap` | none | trap kind | `trap` runtime failure. |

Trap kinds are shared with Trace Schema v0.

## Actor and mailbox operations

These opcodes reserve bytecode names for future actor execution. They do not
define mailbox ordering, delivery, fairness, cancellation, or supervision.

| Opcode | Result | Operand shape | Notes |
| --- | --- | --- | --- |
| `actor.spawn` | actor ID/value | actor layout ID, initial state values | Allocates an `actor:` ID when implemented. |
| `actor.send` | none | actor, message symbol, payload | Fire-and-forget send shape. |
| `actor.ask` | task/reply value | actor, message symbol, payload | Request/reply shape. |
| `actor.reply` | none | reply token, value | Reply token encoding reserved. |
| `mailbox.dequeue` | message value | actor/mailbox | Scheduler-owned; semantics deferred. |
| `actor.link` | none | actor, actor | Link relationship shape. |
| `actor.monitor` | monitor value | actor | Monitor relationship shape. |

## Channels

| Opcode | Result | Operand shape | Notes |
| --- | --- | --- | --- |
| `channel.new` | channel value | element type ID, capacity metadata | Allocates a `channel:` ID when implemented. |
| `channel.send` | none or bool | channel, value | Blocking/backpressure semantics deferred. |
| `channel.recv` | value | channel | Empty/closed semantics deferred. |
| `channel.close` | none | channel | Close semantics deferred. |

## Tasks and select

| Opcode | Result | Operand shape | Notes |
| --- | --- | --- | --- |
| `task.spawn` | task value | function ID, arguments | Allocates a `task:` ID when implemented. |
| `task.await` | value | task | Await scheduling semantics deferred. |
| `task.cancel` | none | task | Cancellation semantics deferred. |
| `select.poll` | selected arm value | arm descriptors | Selection fairness and readiness semantics deferred. |

`select.poll` is intentionally minimal in v0. Future scheduler work must define
arm descriptor metadata before execution.

## Virtual sleep

| Opcode | Result | Operand shape | Notes |
| --- | --- | --- | --- |
| `clock.sleep` | none | duration milliseconds | Uses virtual clock only; wall-clock sleep is forbidden. |

When implemented, virtual sleep must emit `clock.virtual_advance` or scheduler
wakeup records according to the conformance contract.

## Supervisors

| Opcode | Result | Operand shape | Notes |
| --- | --- | --- | --- |
| `supervisor.spawn` | supervisor value | supervisor layout ID, child specs | Allocates a `supervisor:` ID when implemented. |
| `supervisor.restart` | none | supervisor, child actor | Restart decision shape only. |
| `supervisor.stop` | none | supervisor | Stop decision shape only. |

Supervisor strategies are layout metadata in M1. Runtime restart semantics are
deferred.

## Machines

| Opcode | Result | Operand shape | Notes |
| --- | --- | --- | --- |
| `machine.new` | machine value | machine layout ID, initial state | Allocates a `machine:` ID when implemented. |
| `machine.step` | machine state | machine, event value | State transition shape. |
| `machine.state` | state name/tag | machine | State inspection shape. |

Machine layouts record states, events, and transitions. Executing transitions
is out of scope for M1.

## Capability metadata registry

Capability metadata is package-level and open-ended in v0.

| Field | Meaning |
| --- | --- |
| `id` | Stable capability name, for example `std.fs.read` or `std.text.regex.compile`. |
| `disposition` | `allowed`, `rejected`, or `reserved`. |
| `reason` | Human-readable explanation used by diagnostics and trace records. |
| `required_by` | Function IDs, stdlib symbols, or layout IDs that require the capability. |

Disposition meanings:

- `allowed`: the sandbox profile admits the capability.
- `rejected`: profile admission must emit `sandbox.rejected`.
- `reserved`: the bytecode can encode the symbol, but execution must fail closed
  until a sandbox-safe implementation lands.

Suggested initial capability names:

| Capability | Initial disposition | Notes |
| --- | --- | --- |
| `core.stdout` | `allowed` | Needed for educational output. |
| `core.stderr` | `allowed` | Needed for diagnostics and examples. |
| `core.virtual_clock` | `reserved` | Requires scheduler/runtime work. |
| `core.deterministic_random` | `reserved` | Requires replay integration. |
| `std.text.regex.compile` | `reserved` | Waits for regex substrate and shim. |
| `std.fs.read` | `rejected` | Host filesystem is not available. |
| `std.net.http` | `rejected` | Host network is not available. |
| `actor.scheduler` | `reserved` | Waits for actor scheduler milestone. |
| `channel.scheduler` | `reserved` | Waits for channel semantics milestone. |
| `task.scheduler` | `reserved` | Waits for task semantics milestone. |
| `supervisor.runtime` | `reserved` | Waits for supervisor execution milestone. |
| `machine.runtime` | `reserved` | Waits for machine execution milestone. |

## Trap kind registry

| Trap kind | Trigger |
| --- | --- |
| `integer_overflow` | Checked integer operation overflow. |
| `divide_by_zero` | Division or remainder by zero. |
| `invalid_local` | Local slot is missing, moved, or type-incompatible. |
| `invalid_block` | Branch target or block arguments are invalid. |
| `invalid_call` | Function or stdlib call cannot be resolved. |
| `invalid_enum_tag` | Enum value has no matching layout variant. |
| `invalid_record_field` | Field projection or update is invalid. |
| `vector_bounds` | Vector index is outside bounds. |
| `string_bounds` | String slice/index is outside bounds. |
| `regex_compile` | Regex pattern cannot compile in the sandbox. |
| `capability_missing` | Required capability was not admitted. |
| `budget_exhausted` | Step budget reaches zero. |
| `panic` | Hew panic surface. |
| `unsupported_instruction` | Reserved or unknown instruction reaches execution. |
| `internal_error` | Sandbox implementation bug. |
