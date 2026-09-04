# Diagnostic codes

Every refusal the compiler prints carries a code, and every code belongs to one
of three channels. The channel decides the exit code and the prefix, so a
script can tell "your program is wrong" from "the compiler cannot do this yet"
without parsing prose.

| Channel    | Exit | Prefix                     | Meaning                                                                                                                           |
| ---------- | ---- | -------------------------- | --------------------------------------------------------------------------------------------------------------------------------- |
| User       | 1    | none                       | The program is wrong. Fix the program.                                                                                            |
| Limitation | 3    | `compiler limitation:`     | The program is within the language as specified; this build cannot lower it. The code names the release that removes the refusal. |
| Internal   | 4    | `internal compiler error:` | Two compiler facts disagree. Never the program's fault; the message prints both facts and a bug-report line.                      |

Usage errors keep exit 2. These codes belong to `hew check`, `hew build`, and
the compile phase of `hew test`. `hew run` exits with the program's own code
once the program starts (HEW-SPEC-2026 §5.8), so a script that must distinguish
a limitation from a program's own `exit(3)` runs `hew check` first.

## No internal error reaches a user

`E_HIR`, `E_MIR_CHECK`, and `E_NOT_YET_IMPLEMENTED` are the compiler's own
channels. A program that reaches one of them was admitted by a checker that
should have refused it: the checker and the lowering disagree about what the
language accepts, which is a one-authority violation regardless of what the
program was trying to do.

So every known member of that set gets a refusal of its own, with a Limitation
code that names the release removing it, placed where the fact lives — at the
checker when the shape is syntactic, at the MIR diagnostic site when the
refusal needs a liveness fact only MIR holds. The refusal is deleted in the
same change that lands the mechanism.

## The `E_LIMIT_*` family

| Code                               | Channel    | Removed by | Refuses                                                                                                                                                                                                       |
| ---------------------------------- | ---------- | ---------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `E_LIMIT_COLLECTION_COPY`          | Limitation | v0.7.0     | A whole-binding copy of a value-category collection (`let b = a` on a `Vec`, then a use of `a`). The rule says retain; this lowering consumes. Relabelled at the MIR site, not re-derived in the checker.     |
| `E_LIMIT_GENERIC_RETURN_INFERENCE` | Limitation | v0.7.0     | A generic return type inferred through `?` or `.unwrap()`. The turbofish form is the accepted spelling.                                                                                                       |
| `E_LIMIT_SUPERVISED_STATE`         | Limitation | v0.7.0     | Supervised child state beyond scalars, `string`, and `bytes`. One code for the checker refusal, the init-arg MIR gap, and the init-thunk codegen backstop.                                                    |
| `E_LIMIT_SELF_NAMED_ALIAS`         | Limitation | v0.7.0     | An alias import of a file whose name repeats its directory (`import std.net.http.http as h;`). The unaliased file, the directory module, and a sibling file under the directory's name all compile.           |
| `E_LIMIT_RC_FIELD`                 | Limitation | v0.7.0     | A field read through an `Rc<T>` (§3.7.5). `.get()` on a `T: Copy` payload is the accepted form until the borrow-projection lowering lands.                                                                    |
| `E_LIMIT_DYN_SEND`                 | Limitation | v0.7.0     | Sending a `dyn Trait` value whose concrete type is not `#[wire]`.                                                                                                                                             |
| `E_LIMIT_MAIN_CONTEXT`             | Limitation | v0.7.0     | A suspension `main` cannot carry, because `main` has no execution context in this build.                                                                                                                      |
| `E_LIMIT_INHERENT_VAR_SELF`        | Limitation | v0.7.0     | A `var self` receiver on an inherent `impl` method. The trait form is accepted.                                                                                                                               |
| `E_LIMIT_OPAQUE_ACTOR`             | Limitation | v0.7.0     | An `#[opaque]` value, or a `#[resource]` wrapper around one, as an actor's init field.                                                                                                                        |
| `E_LIMIT_DERIVED_ORD`              | Limitation | v0.7.0     | A structurally derivable aggregate ordering comparison whose lexicographic lowering is not implemented.                                                                                                       |
| `E_ASSOC_BOUND_UNSUPPORTED`        | Limitation | v0.7.0     | An associated-type bound in a `where` clause (`where T.Item: Bound`, §3.8.3). It parses and type-checks but never reaches method resolution, so writing it is an error rather than a bound that does nothing. |

## User-channel codes in the same family

These refuse a program the language does not accept, so no release removes
them.

| Code                                           | Refuses                                                                                                                       |
| ---------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------- |
| `E_RANGE_VALUE`                                | A range in value position. Ranges are `for` iterables, by design.                                                             |
| `E_DYN_SUPERTRAIT`                             | A supertrait method reached through a trait object in a shape the checker cannot resolve (HEW-SPEC-2026 §2.2.1).              |
| `E_BARE_VARIANT_PATTERN`                       | A bare variant in pattern position (`Ok(v)`). Write `.Ok(v)` when the scrutinee selects the enum, `Result.Ok(v)` otherwise.   |
| `E_BARE_VARIANT_EXPR`                          | A bare variant in expression position, under the same rule.                                                                   |
| `E_VISIBILITY_PRIVATE`, `E_VISIBILITY_PACKAGE` | A cross-module reference to a private or package item.                                                                        |
| `E_MODULE_NOT_FOUND`                           | An `import` no search path resolves.                                                                                          |
| `E_GEN_RETURN_SPELLING`                        | A `gen fn` return type spelling the generator handle (`Generator<Y, R>`) instead of the yield type `Y` (HEW-SPEC-2026 §4.12). |
| `E_UNKNOWN_ATTRIBUTE`                          | An attribute whose name is not in the closed table of HEW-SPEC-2026 §12.6, or that appears in a position the table does not list for it — on a type declaration, function, parameter, field, actor member, or `impl` block alike. Supersedes the retired `E_UNKNOWN_TYPE_MARKER`. |
| `E_BREAK_VALUE`                                | An operand on `break` (§12.4). A loop produces no value, so assign to a `var` before breaking.                               |
| `E_SCOPE_IS_STATEMENT`                         | `scope { .. }` in a value position (§4.2). It brackets structured concurrency; `join { .. }` is the value-producing fan-out. |

The remaining v0.6.0 surface codes are named by the decision that introduces
them, and their rule lives in the spec section that decision writes — not
here. Each row is added by the lane that lands the refusal:
`E_IS_VALUE_TYPE`, `E_OPAQUE_MESSAGE_PAYLOAD`, `E_CALLABLE_MESSAGE_PAYLOAD`,
`E_USE_AFTER_SEND`, `E_UNKNOWN_ATTRIBUTE`, `E_RESERVED_HANDLER_NAME`,
`E_ACTOR_CONTEXT_REQUIRED`, `E_BREAK_VALUE`, `E_SCOPE_IS_STATEMENT`,
`E_GEN_RETURN_SPELLING`, `E_NO_ASYNC_FN`,
`E_NO_ASYNC_GEN`, `E_FOR_STREAM_NEEDS_AWAIT`, `E_AWAIT_NOT_STREAM`.

## Refusals with no code yet

Each of these is a known member of the internal-error set whose refusal has not
been written. The code is assigned in the change that adds the refusal, and it
is listed here then — not before, so this table never names a code the compiler
does not emit.

| Refusal                                                                   | Removed by |
| ------------------------------------------------------------------------- | ---------- |
| Skip-level (transitive) closure capture                                   | v0.7.0     |
| `if let` with a nested tuple pattern                                      | v0.7.0     |
| A guarded tuple pattern with a binding                                    | v0.7.0     |
| `for` over an iterator adapter                                            | v0.7.0     |
| `println` of an `Option<T>` or `Result<T, E>` — neither has `Display` yet | v0.7.0     |
| `duration.from_*` constructors                                            | v0.7.0     |

## Where this list is going

This file is written by hand. At v0.7.0 the per-code pages under
`docs/diagnostics/E_*.md` are generated from one registry table in the compiler,
`hew check --explain E_CODE` renders them, and a gate fails when a code is
emitted without a page or without a reject fixture that produces it. Until
then, a lane that adds a code adds its row here in the same change.
