# Sandbox Bytecode package `hew.sandbox.bytecode.v1`

**Status:** the package the browser sandbox VM executes. It is a projection of
verified ownership SIR (`hew_sir::SemModule`, produced by
`hew_compile::Session::lower_hir_module`), not a lowering of the AST.

The compiler decides. The VM executes. Every fact in this package was proved by
SIR verification; the VM must never re-derive one from a type, a name or a
symbol spelling.

## What the VM owes the package

| Fact                        | Carried as                                                                   | The VM's obligation                                                                                           |
| --------------------------- | ---------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------- |
| Copies, transfers, releases | `copy_value`, `move`, `destroy_value`                                        | `copy_value` is a deep clone, `move` an alias transfer that invalidates its source, `destroy_value` a release |
| Borrow scopes               | `begin_borrow` / `end_borrow`, `load.borrow`                                 | A borrow aliases its referent; it is not a clone                                                              |
| Cleanup on the failing path | every call's `unwind` edge, `cleanup.dispatch`, `resume_unwind`              | Follow the edge the package names; never synthesize one                                                       |
| Checked arithmetic outcomes | `checked.binary` with `normal` and one edge per failure                      | Take the named failure edge; never trap implicitly                                                            |
| Enum dispatch               | `switch.variant` with one edge per tag plus `otherwise`                      | Dispatch on the tag the package records                                                                       |
| Runtime behaviour           | `runtime.call` by family identity, `extern.call` by declared symbol identity | Resolve to a shim by identity or reject the package at load                                                   |
| Suspension and resumption   | `suspend` with `resumes`, `cancel` and `unwind` edges                        | Park `(function, block, args)`; the scheduler owns readiness                                                  |
| Process exit                | `entry.exit`                                                                 | Publish the status the entry plan names                                                                       |

## Admission

Admission is a load-time decision, before any instruction runs. The VM's
package validator walks `runtime_families` and `externs`; an entry whose
`admission` is not `allowed` is reported as a `sandbox.rejected` trace event
with the package's own reason and the program does not start. A family or
symbol the validator does not know is rejected the same way: unknown is not
allowed.

The validator keys on the family identity and the declared extern symbol the
compiler recorded. It never inspects an instruction stream to guess a
capability, and it never matches a symbol prefix.

## Top-level shape

```jsonc
{
  "schema_version": "hew.sandbox.bytecode.v1",
  "hew_version": "0.6.0-rc4",
  "compiler_version": "hew-sandbox-wasm-0.6.0-rc4",
  "profile": "sandbox-vm-export",

  "entry": { "function": 0, "exit": "unit" },

  "strings": ["hello", ", world"],
  "bytes": [[104, 105]],
  "regex_patterns": ["^a+$"],

  "aggregates": [{ "id": 0, "name": "Point", "fields": ["x", "y"] }],
  "variants": [
    {
      "id": 0,
      "name": "Option",
      "cases": [
        { "name": "None", "fields": [] },
        { "name": "Some", "fields": ["0"] },
      ],
    },
  ],

  "runtime_families": [
    {
      "id": 0,
      "family": "Print",
      "detail": { "kind": "Str", "newline": true },
      "admission": "allowed",
    },
  ],
  "externs": [
    { "id": 0, "symbol": "hew_io_read_line", "admission": "allowed" },
  ],

  "functions": [
    /* see below */
  ],
}
```

### `entry`

`function` is the index into `functions` of the process entry SIR selected.
`exit` is `"unit"` when the entry returns nothing and `"status"` when it
returns the integer process status. A `Result` entry never reaches the package:
SIR consumes it in the entry adapter and publishes the integer the body
returns.

A package with no entry (a library compilation) omits `entry`; the VM rejects
it at load rather than choosing a function to run.

### Literal pools

`strings`, `bytes` and `regex_patterns` are dense arrays. `const.str`,
`const.bytes` and the regex externs index them. SIR interns them per module, so
two equal literals share one index.

### Shapes

`aggregates` are named record shapes; `fields` is the declaration-order field
name list, and a field index in an instruction is an index into it.
`variants` are enum shapes; `cases` is in declaration order, and a case index
is the tag. `fields` on a case names its payload fields (a tuple variant names
them `"0"`, `"1"`, ...).

Tuples are structural and carry no shape entry: `tuple.make` records its arity
and `tuple.get` its index.

### `runtime_families`

One entry per distinct `hew_types::RuntimeCallFamily` a `runtime.call` in this
package names. `family` is the variant name; `detail` carries the variant's
payload as JSON when it has one (`Print` carries `kind` and `newline`;
`Vector` carries its operation). A `runtime.call` instruction names the entry
by `id`, never by the spelling of `family`.

### `externs`

One entry per declared C-ABI symbol an `extern.call` names, with the declared
signature's per-operand ownership already applied to the instruction's
`boundary` args. `symbol` is the declared identity from the `extern`
declaration, not a name the emitter invented.

## Functions

```jsonc
{
  "id": 0,
  "name": "main",
  "params": [{ "value": 0, "own": "owned" }],
  "entry": 0,
  "places": [{ "id": 0 }],
  "blocks": [
    {
      "id": 0,
      "params": [],
      "ops": [{ "op": "const.str", "dst": 1, "str": 0, "span": null }],
      "term": {
        "op": "runtime.call",
        "family": 0,
        "args": [{ "value": 1, "decision": "borrow" }],
        "result": null,
        "normal": { "to": 1, "args": [] },
        "unwind": null,
        "span": null,
      },
    },
  ],
}
```

Bodies are SSA with block arguments, exactly as SIR carries them. A value is
defined once, by an op's `dst`, by a block parameter or by a call result on its
normal edge.

`own` on a parameter or block argument is the §1.2 ownership obligation SIR
proved: `"owned"` (exactly one consuming use per path), `"guaranteed"` (a loan
that must not outlive its borrow scope) or `"none"`.

`places` are the semantic storage locations this body addresses. A place is a
mutable cell: `alloc_place` creates it, `store.init` and `store.assign` write
it, `load.copy`, `load.take` and `load.borrow` read it, `end_lifetime` ends it.
A place has no layout; the VM holds one slot per place per activation.

### Operands

An ordinary operand is a value id. **An operand carries no mode**: what a use
does to its value is the op it feeds (SIR §1.3). `copy_value`, `move`,
`begin_borrow`, `load.*` and `store.*` are operations, not annotations on a
read.

A value crossing a call or suspension boundary is a `boundary operand` and does
carry the decision the boundary owns:

| `decision`   | The VM does                                    |
| ------------ | ---------------------------------------------- |
| `borrow`     | Pass an alias; the caller keeps the obligation |
| `borrow_mut` | Pass an alias the callee may write through     |
| `copy`       | Pass a deep clone                              |
| `move`       | Pass the value and invalidate the source       |
| `snapshot`   | Pass a deep clone taken at the boundary        |

### Edges

An edge is `{ "to": <block id>, "args": [<value id>, ...] }`. Its argument
count and ownership kinds match the target block's parameters exactly; SIR
verification proved that, and the VM binds them positionally without checking.

A call's `result` is defined on its `normal` edge, like a block parameter.

## Determinism

The package is a deterministic function of the verified SIR module: every table
is in SIR's own module-local id order, and no table is keyed by a name the
emitter chose. Regenerating a package from the same source and compiler
produces a byte-identical file.

## Pointer width

SIR is pointer-width neutral, so nothing in this package depends on a target
pointer size. `isize` and `usize` are 64-bit in the VM, matching native
execution, and the divergences document records that as a promise, not an
accident.
