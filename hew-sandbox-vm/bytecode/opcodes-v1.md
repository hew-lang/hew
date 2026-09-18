# Sandbox Bytecode opcode registry v1

The instruction and terminator set of
[`hew.sandbox.bytecode.v1`](sandbox-bytecode-v1.schema.json). Every entry is a
projection of one `hew_sir::SemOpKind` or `hew_sir::SemTerminator` variant, so
the registry is closed: an opcode exists because SIR produces it, and a SIR
variant with no opcode is an emitter gap, never a silent skip.

Read [`package-v1.md`](package-v1.md) first for the package shape, the operand
encoding and the admission contract.

## Shared shapes

- **operand** — a value id. It carries no mode; the op it feeds is what the use
  does (SIR §1.3).
- **boundary operand** — `{ "value": <id>, "decision": "borrow" | "borrow_mut" |
"copy" | "move" | "snapshot" }`, used only by calls, returns, panics and
  suspensions.
- **edge** — `{ "to": <block id>, "args": [<value id>, ...] }`.
- **result** — `null` for a unit result, `{ "value": <id>, "own": <own kind> }`
  for a value, and the terminator omits its normal edge entirely for a `never`
  result.
- **span** — `{ "start": <byte>, "end": <byte> }` or `null`.

Every instruction is `{ "op": <name>, "dst": <value id> | null, "span": ... }`
plus the fields named below. Every terminator is `{ "op": <name>, "span": ... }`
plus its fields.

## Constants

| Opcode           | `dst`    | Fields                                                                               | Notes                                                                                                     |
| ---------------- | -------- | ------------------------------------------------------------------------------------ | --------------------------------------------------------------------------------------------------------- |
| `const.int`      | integer  | `value` (decimal string)                                                             | SIR carries `i128`; the string keeps the full range through JSON. The VM narrows to the value's own type. |
| `const.bool`     | bool     | `value`                                                                              |                                                                                                           |
| `const.float`    | f64      | `value`, and `nonfinite` (`"nan"`, `"inf"`, `"-inf"`) when the literal is not finite | JSON has no NaN; the VM reads `nonfinite` first.                                                          |
| `const.char`     | char     | `value` (one scalar as a string)                                                     |                                                                                                           |
| `const.unit`     | unit     | —                                                                                    |                                                                                                           |
| `const.duration` | duration | `nanos` (decimal string)                                                             | `ConstDuration` is nanoseconds.                                                                           |
| `const.str`      | string   | `str` (index into `strings`)                                                         |                                                                                                           |
| `const.bytes`    | bytes    | `bytes` (index into `bytes`)                                                         |                                                                                                           |

## Ownership

These are the §1.2 obligations SIR proved. The VM executes them; it does not
decide them, and it does not skip one because a value "looks" scalar.

| Opcode                   | `dst`                 | Fields   | The VM does                                                                                        |
| ------------------------ | --------------------- | -------- | -------------------------------------------------------------------------------------------------- |
| `copy_value`             | the copy              | `source` | Deep clone.                                                                                        |
| `move`                   | the transferred value | `source` | Alias transfer; the source is invalidated.                                                         |
| `fork`                   | the duplicate         | `source` | Duplicate a value both paths will consume.                                                         |
| `destroy_value`          | —                     | `value`  | Release. A value holding no resource releases to nothing; the op still runs and still invalidates. |
| `begin_borrow`           | the loan              | `owner`  | Alias the owner for the loan's scope.                                                              |
| `end_borrow`             | —                     | `borrow` | End the loan.                                                                                      |
| `finish_linear_receiver` | —                     | —        | Discharge the body's terminal receiver obligation.                                                 |

## Places

A place is a mutable cell with no layout. The VM holds one slot per place per
activation.

| Opcode         | `dst`           | Fields           |
| -------------- | --------------- | ---------------- |
| `alloc_place`  | —               | `place`          |
| `store.init`   | —               | `place`, `value` |
| `store.assign` | —               | `place`, `value` |
| `load.copy`    | the copy        | `place`          |
| `load.take`    | the taken value | `place`          |
| `load.borrow`  | the loan        | `place`          |
| `end_lifetime` | —               | `place`          |

`load.borrow` yields a reference to the cell, not a snapshot of its contents: a
later `store.assign` through the same place is visible through the loan.

## Aggregates, tuples, arrays and variants

| Opcode                     | `dst`            | Fields                                |
| -------------------------- | ---------------- | ------------------------------------- |
| `tuple.make`               | the tuple        | `elements`                            |
| `tuple.get`                | the element      | `tuple`, `index`                      |
| `aggregate.make`           | the record       | `shape`, `fields`                     |
| `aggregate.project_copy`   | the field copy   | `shape`, `aggregate`, `field`         |
| `aggregate.project_borrow` | the field loan   | `shape`, `aggregate`, `field`         |
| `destructure`              | —                | `shape`, `aggregate`                  |
| `array.make`               | the array        | `elements`                            |
| `array.repeat`             | the array        | `value`                               |
| `variant.make`             | the enum value   | `shape`, `variant`, `fields`          |
| `variant.is`               | bool             | `shape`, `variant`, `source`          |
| `variant.project_copy`     | the payload copy | `shape`, `variant`, `source`, `field` |
| `variant.project_borrow`   | the payload loan | `shape`, `variant`, `source`, `field` |
| `variant.destructure`      | —                | `shape`, `variant`, `source`          |

`aggregate.project_borrow` and `variant.project_borrow` alias the field in
place. Writing through one must be visible in the container, so a projected
loan is a `(container, key)` reference, never the field's value.

`destructure` and `variant.destructure` consume their container and hand each
field to the values the following ops name; the container itself is spent.

## Operators

| Opcode     | `dst`      | Fields                    |
| ---------- | ---------- | ------------------------- |
| `unary`    | the result | `unary_op`, `value`       |
| `binary`   | the result | `binary_op`, `lhs`, `rhs` |
| `cast`     | the result | `value`, `from`, `to`     |
| `str.eq`   | bool       | `lhs`, `rhs`              |
| `bytes.eq` | bool       | `lhs`, `rhs`              |

`binary_op` and `unary_op` are the source operator names. `binary` is the
non-trapping family: wrapping arithmetic, bitwise, logical and comparison.
Arithmetic that can fail is the `checked.binary` **terminator**, because its
failures are CFG edges.

`ty` on `const.int`, `unary`, `binary` and `checked.binary` is the operand's
own scalar type, so wrapping, overflow and shift-range behaviour use the width
the program declared. The VM's value representation does not record integer
width, so it reads the width from the package rather than assuming 64 bits.

`cast` carries both the source and target scalar names for the same reason. Native width, truncation,
extension and saturation semantics apply.

## Callables

| Opcode            | `dst`                | Fields              |
| ----------------- | -------------------- | ------------------- |
| `function.make`   | the function value   | `callable`          |
| `closure.make`    | the closure          | `closure`, `fields` |
| `callable.coerce` | the coerced callable | `source`            |
| `dyn.make`        | the trait object     | `vtable`, `value`   |

## Defer

| Opcode           | `dst` | Fields                           |
| ---------------- | ----- | -------------------------------- |
| `register_defer` | —     | `defer`, `scope`, `dependencies` |

`enter_defer` and `finish_defer` are terminators.

## Concurrency

These have no producer in a package the VM accepts today: the emitter routes a
module that uses them to the concurrency path. They are listed so the registry
stays closed against `SemOpKind`.

| Opcode                  | Fields                        |
| ----------------------- | ----------------------------- |
| `generator.make`        | `closure`, `callable`         |
| `stream.pipe`           | `capacity`                    |
| `task_scope.enter`      | `scope`, `parent`, `duration` |
| `task_scope.close`      | `scope`                       |
| `task.spawn`            | `scope`, `callable`           |
| `actor.ingress_adapter` | `adapter`                     |

## Control-flow terminators

| Opcode           | Fields                                                          | Notes                                                                                                                       |
| ---------------- | --------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------- |
| `return`         | `value` (boundary operand or `null`)                            |                                                                                                                             |
| `goto`           | `edge`                                                          |                                                                                                                             |
| `branch`         | `condition`, `then`, `else`                                     |                                                                                                                             |
| `switch.variant` | `shape`, `scrutinee`, `arms`                                    | Each arm is `{ "variant": <tag>, "fields": [<result>...], "edge": <edge> }`. The arms cover every tag; there is no default. |
| `checked.binary` | `binary_op`, `lhs`, `rhs`, `ty`, `result`, `normal`, `failures` | Each failure is `{ "trap": <trap kind>, "edge": <edge> }`. Take the named edge; never trap implicitly.                      |
| `unreachable`    | —                                                               | A semantically unreachable endpoint, not a language-visible trap. Reaching it is an internal error.                         |

## Call terminators

Every call is a terminator because it has a normal successor and may have an
unwind successor.

| Opcode          | Fields                                                                         |
| --------------- | ------------------------------------------------------------------------------ |
| `call`          | `callee` (function id), `args`, `result`, `normal`, `unwind`                   |
| `indirect.call` | `callee` (boundary operand), `args`, `result`, `normal`, `unwind`              |
| `dyn.call`      | `receiver`, `slot`, `args`, `result`, `normal`, `unwind`                       |
| `value.call`    | `capability`, `args`, `result`, `normal`, `unwind`                             |
| `runtime.call`  | `family` (index into `runtime_families`), `args`, `result`, `normal`, `unwind` |
| `extern.call`   | `extern` (index into `externs`), `args`, `result`, `normal`, `unwind`          |
| `actor.call`    | `operation`, `args`, `result`, `normal`, `unwind`                              |
| `wire.codec`    | `direction`, `plan`, `args`, `result`, `normal`, `unwind`                      |

`result_shape` is the `variants` id of the demanded enum descriptor a runtime
or extern result is built against, or `null` when the result is not an enum. A
shim that returns `Option` or `Result` constructs the tag from that descriptor's
declaration order; it never assumes a variant order of its own.

`normal` is absent exactly when `result` is `never`. `unwind` is `null` when
the call cannot raise a fault; a C-ABI `extern.call` is always `null`.

`value.call` names a `plan`: an index into the package's `value_capabilities`
table, which is the checker's selection for one `(type, capability)` pair. An
entry with a `callable` is a user implementation and the VM calls that
function; an entry without one is the derived structural operation and the VM
performs it over its own value representation. The VM never decides which by
inspecting the receiver at run time.

## Faults and cleanup

| Opcode             | Fields                                                            | Notes                                                                                                                                        |
| ------------------ | ----------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------- |
| `panic`            | `message` (boundary operand), `cleanup`                           | Copy the message into a logical panic fault, then enter cleanup. Exit status 101.                                                            |
| `trap`             | `trap`                                                            | A reachable language-visible trap.                                                                                                           |
| `checked_raise`    | `trap`, `cleanup`                                                 | Raise a checked failure as a fault and enter cleanup.                                                                                        |
| `cleanup.dispatch` | `normal`, `fault`                                                 | Continue on the normal path, or resume unwinding, according to the activation's pending fault.                                               |
| `resume_unwind`    | —                                                                 | Continue unwinding after a cleanup block discharged its obligations. In the top frame this ends the program with the pending fault's status. |
| `enter_defer`      | `defer`, `park`, `body`                                           |                                                                                                                                              |
| `finish_defer`     | `defer`, `park`, `next`                                           |                                                                                                                                              |
| `recover_fault`    | `result`, `deadline_variant`, `fault_variant`, `normal`, `unwind` | Convert a pending fault into the result enum the arms name.                                                                                  |

### Trap kinds

SIR's five trap kinds map onto the trace schema's kinds and the process exit
statuses native execution uses:

| SIR `TrapKind`       | Trace `trap_kind`    | Exit |
| -------------------- | -------------------- | ---- |
| `IntegerOverflow`    | `integer_overflow`   | 201  |
| `SignedMinDivNegOne` | `integer_overflow`   | 201  |
| `DivideByZero`       | `divide_by_zero`     | 202  |
| `ShiftOutOfRange`    | `shift_out_of_range` | 204  |
| `IndexOutOfBounds`   | `vector_bounds`      | 205  |

A panic is `panic` / 101. The VM adds no trap kind of its own for a SIR fault:
a SIR fault with no mapping here is an emitter gap to fix, not a kind to invent.

## Suspension

| Opcode    | Fields                                                              |
| --------- | ------------------------------------------------------------------- |
| `suspend` | `kind`, `detail`, `inputs`, `result`, `resumes`, `cancel`, `unwind` |

A suspension parks the activation as `(function, block, args)`. `resumes` has
one edge per outcome — `await` one, `select` one per arm, a deadline form two,
`join` one. `cancel` is always present and its target's first op is the kind's
abandon op. `unwind` is the logical failure path taken after abandoning a
pending registration.

`kind` is the `SuspendKind` variant name and `detail` its payload. Every kind
the instruction stream reaches is also listed in the package's top-level
`suspend_kinds`, so load-time admission stays a walk of the manifest. The kinds
a sequential package may carry:

| `kind`       | The VM does                                                                                                           |
| ------------ | --------------------------------------------------------------------------------------------------------------------- |
| `ValueClose` | Release the named place or selection and take the first resume edge. A value holding no resource resumes immediately. |
| `Sleep`      | Advance the virtual clock by the input duration and take the first resume edge.                                       |
| `NativeIo`   | Reject at load: native I/O is not a sandbox capability.                                                               |

Every other kind belongs to the concurrency path.
