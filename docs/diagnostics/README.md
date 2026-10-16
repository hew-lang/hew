# Diagnostic codes

Every compiler refusal carries a code and belongs to one of three channels.
The channel determines the exit code and prefix, so scripts can classify a
diagnostic without parsing its prose.

| Channel    | Exit | Prefix                     | Meaning                                                                                                                           |
| ---------- | ---- | -------------------------- | --------------------------------------------------------------------------------------------------------------------------------- |
| User       | 1    | none                       | The source violates a language rule; the diagnostic gives the location and available fix. |
| Limitation | 3    | `compiler limitation:`     | The source uses a specified form that this compiler build cannot lower. |
| Internal   | 4    | `internal compiler error:` | Compiler stages disagree; the diagnostic includes a bug-report line. |

Usage errors keep exit 2. These codes belong to `hew check`, `hew build`, and
the compile phase of `hew test`. `hew run` exits with the program's own code
once the program starts (HEW-SPEC-2026 §5.8), so a script that must distinguish
a limitation from a program's own `exit(3)` runs `hew check` first.

## Compiler-stage diagnostics

Compiler-stage codes use the same channels as other diagnostics. Most
`E_HIR` diagnostics report a source-level rule, while specific HIR boundary
violations report internal inconsistencies. `E_NOT_YET_IMPLEMENTED` reports a
compiler limitation. In each case, the message names the refused form and
includes a current alternative when one exists.

## The `E_LIMIT_*` family

| Code               | Channel    | Refuses                                                                                                                                    |
| ------------------ | ---------- | ------------------------------------------------------------------------------------------------------------------------------------------ |
| `E_LIMIT_RC_FIELD` | Limitation | A field read through an `Rc<T>` (§3.7.5). Use `.get()` when the payload implements `Copy`. |

## User-channel codes in the same family

These refuse a program the language does not accept, so no release removes
them.

| Code                                           | Refuses                                                                                                                                                                                                                                                                                |
| ---------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `E_BARE_VARIANT_PATTERN`                       | A bare variant in pattern position (`Ok(v)`). Write `.Ok(v)` when the scrutinee selects the enum, `Result.Ok(v)` otherwise.                                                                                                                                                            |
| `E_BARE_VARIANT_EXPR`                          | A bare variant in expression position, under the same rule.                                                                                                                                                                                                                            |
| `E_VISIBILITY_PRIVATE`, `E_VISIBILITY_PACKAGE` | A cross-module reference to a private or package item.                                                                                                                                                                                                                                 |
| `E_MODULE_NOT_FOUND`                           | An `import` no search path resolves (HEW-SPEC-2026 §3.5.3). A note names a directory module outside a package, or a module found in a parent directory of the importing file.                                                                                                          |
| `E_ENTRY_FILE_IMPORT`, `E_PEER_IMPORT`         | An `import` naming a directory module's entry file (`pkg.dir.dir`) or one of its peer files. A directory module has one spelling, its directory (HEW-SPEC-2026 §3.5.1); the message names it.                                                                                          |
| `E_NO_MAIN`                                    | `hew build` or `hew run` was given a file with no `fn main`. A library is checked with `hew check` and tested with `hew test`.                                                                                                                                                         |
| `E_GEN_RETURN_SPELLING`                        | A `gen fn` return type spelling the generator handle (`Generator<Y, R>`) instead of the yield type `Y` (HEW-SPEC-2026 §4.12).                                                                                                                                                          |
| `E_UNKNOWN_ATTRIBUTE`                          | An attribute whose name is not in the closed table of HEW-SPEC-2026 §12.6, or that appears in a position the table does not list for it — on a type declaration, function, parameter, field, actor member, or `impl` block alike. Supersedes the retired `E_UNKNOWN_TYPE_MARKER`.      |
| `E_BREAK_VALUE`                                | An operand on `break` (§12.4). A loop produces no value, so assign to a `var` before breaking.                                                                                                                                                                                         |
| `E_FOR_AWAIT`                                  | `for await` (HEW-SPEC-2026 §4.12). `for` waits for each item on its own, so the fix-it deletes `await`.                                                                                                                                                                                |
| `E_RESERVED_HANDLER_NAME`                      | A `receive fn stop()`. `stop` is the actor handle's own lifecycle method (HEW-SPEC-2026 §2.1); rename the handler, or call `self.stop()`.                                                                                                                                              |
| `E_BOUNDARY_RESOURCE_MUST_CONSUME`             | A `#[resource]`/`#[linear]` value parameter of an `extern` fn or a bodyless trait method without `consume`. No body shows whether the other side takes the value.                                                                                                                      |
| `E_RESULT_DROPPED`                             | A statement-position `Result` whose error is discarded (§2.1.1, §5.6). Handle it with `?`, `match` or `handle`, or write `let _ = call();` when the outcome is deliberately ignored.                                                                                                   |
| `E_ELEMENT_NO_COPY`                            | An operation that copies a collection element out — an array repeat, a `Vec` range slice, or a `HashMap` value projection — on an element whose value class has no copy operation. The message names the class; move the element out instead, or read it through a borrowing accessor. |
| `E_MACHINE_SELF`                               | `self` inside a machine body. A machine is not an actor: the source state's fields are `state`, the incoming payload is `event` (HEW-SPEC-2026 §3.11.3).                                                                                                                               |
| `E_MACHINE_REDUNDANT_TARGET`                   | A transition body that is nothing but the target the head already named (`=> Filled { Filled { … } }`, `=> Empty { .Empty }`). Write the field list with the target elided, or no body at all for a unit state.                                                                        |

This page lists codes emitted by the current compiler. Add a row when a new
stable code ships, and keep its wording aligned with the emitted diagnostic.
