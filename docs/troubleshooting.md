# Troubleshooting

This guide is a short checklist for common `hew` compiler, CLI, and source-build failures.

Start with the smallest command that still reproduces the problem:

```sh
hew check path/to/main.hew
hew build path/to/main.hew -o app
hew run path/to/main.hew
```

Source-aware commands such as `hew check`, `hew build`, `hew run`, `hew test`,
and `hew doc` operate on a single entry-point file and resolve imports
recursively from there. For multi-file projects, pass `main.hew` (or your real
top-level entry file), not every file in the tree.

## Build & linking

Common signs:

- `Error: clang not found. Install LLVM to link Hew programs.`
- raw linker output after `hew build <file.hew> [-o output]`
- `hew check <file.hew>` succeeds, but `hew build ...` or `hew run ...` fails

What to check:

- Use `hew check <file.hew>` first to separate frontend failures from
  link/toolchain failures.
- Hew forwards raw linker output. Fix missing libraries, bad `--link-lib`
  flags, or target mismatches before chasing compiler internals.
- For this repo, use `make`, `make release`, `make test`, and other Makefile
  targets instead of calling Cargo/CMake/ctest directly.
- Use the toolchain from the root README: Rust stable, LLVM/MLIR 22,
  CMake >= 3.20, Ninja, and clang/clang++.
- On Linux, build `hew-codegen` with clang/clang++ rather than GCC.
- On macOS, use Homebrew LLVM (`LLVM_PREFIX="$(brew --prefix llvm)"`) and
  follow [`cross-platform-build-guide.md`](cross-platform-build-guide.md) for
  bitcode, sysroot, and libc++ issues.
- After switching LLVM installs, `CC` / `CXX`, or `HEW_STATIC`, run
  `make clean` before rebuilding.
- If the program builds but you need a debugger, use
  `hew debug <file.hew> [-- args...]`.

## Type errors & type inference

Common signs:

- a `type mismatch` error such as `expected int, found String`
- `cannot infer type`
- `return type mismatch: expected ..., found ...`

What to check:

- Start with `hew check <file.hew>`.
- While iterating on the same file or directory,
  `hew watch <file_or_dir> [options]` can keep rerunning checks.
- Add explicit types to function parameters, return values, locals flowing into
  generic code, and any `_` placeholders.
- `type Foo = _;` currently fails closed with `cannot infer type for type alias`
  and a help suggestion to add a type annotation.
- When you hit a mismatch, fix the earliest ambiguous binding or return value,
  not only the final call site.

## Module & import resolution

Common signs:

- a module-not-found error for a local or package module
- a `cannot resolve import ...` diagnostic
- an `unused import: ...` warning while reorganizing imports

What to check:

- Run `hew check <file.hew>` or `hew build <file.hew> [-o output]` against the
  real entry file, not a peer file inside a module directory.
- For `import greeting;`, Hew looks for `greeting.hew` beside the importer or
  `greeting/greeting.hew` in a directory-form module. The entry file stem must
  match the directory name.
- Other top-level `.hew` files in that directory merge into the same module
  automatically. Subdirectories do not; import child modules explicitly, for
  example `import text_stats::words;`.
- Standard library imports are available under the last path segment:
  `import std::fs;` gives `fs`, and `import std::encoding::json;` gives `json`.
- If the missing module is a package dependency, run `adze install`. If it is
  undeclared in project metadata, add it with `adze add ...` first.
- Use the candidate-path list in the module-not-found error to confirm where Hew
  actually looked.
- `import mod::*;` works, but wildcard imports currently can emit a
  false-positive `unused import` warning when the module is only used through
  type references. Prefer bare (`import mod;`) or selective
  (`import mod::{Name}`) imports while reorganizing modules.

See also:
[`../examples/directory_module_demo/README.md`](../examples/directory_module_demo/README.md),
[`../examples/multifile/README.md`](../examples/multifile/README.md), and
[`specs/HEW-SPEC.md`](specs/HEW-SPEC.md).

## Pattern matching & exhaustiveness

Common signs:

- `non-exhaustive match: missing Blue`
- the same kind of error when a `match` on `Option<T>` skips `None`
- the same kind of error when a `match` on `Result<T, E>` skips `Ok` or `Err`

What to check:

- Matches over enums, `Option<T>`, `Result<T, E>`, machine states, and `bool`
  are fail-closed: cover every case or add `_ => ...`.
- For scalar or open-ended values such as `int`, a missing catch-all is only a
  warning.
- When a new variant or state lands, update old match sites before chasing
  downstream type errors.
- For machine-specific transition and exhaustiveness rules, see
  [`specs/MACHINE-SPEC.md`](specs/MACHINE-SPEC.md).

## Mutability & variable bindings

Common signs:

- a `cannot assign to immutable variable` error
- a `variable ... is already defined in this scope` error
- a warning that a variable shadows a binding in an outer scope

What to check:

- `let` bindings are immutable. Change a binding to `var` only when
  reassignment is actually intended.
- Same-scope rebinding is an error. Rename the second binding instead of
  treating it like reassignment.
- Nested-scope shadowing is a warning for locals, but it can be a hard error
  when it would shadow synthetic bindings such as actor fields.
- Prefix intentionally unused or intentionally shadowed names with `_` to
  suppress noise.

## Function calls & arity

Common signs:

- an `undefined function` error
- `cannot find value ... in this scope` / `cannot find type ... in this scope`
- `this function takes N argument(s) but M were supplied`

What to check:

- Fix spelling first. Hew can suggest close matches for values, functions,
  types, and fields.
- Re-check whether the call should stay qualified under a module
  (`utils.helper()`) or be brought into scope by a selective import.
- Recount arguments after refactors, especially when constructors or helper
  signatures changed.

## Wire type validation

Common signs:

- an `error: removed required field ...` compatibility failure
- `wire check: 1 error(s), 0 warning(s)`

What to check:

- Compare the current schema against a baseline with
  `hew wire check <file.hew> --against baseline.hew`.
- Treat field tags as stable IDs. Do not renumber or reuse them.
- Removing required fields, changing a wire declaration from struct to enum (or
  back), or setting `min_version` above the baseline version is breaking.
- Use the last released or otherwise agreed baseline schema, not another
  in-progress branch snapshot.

## Related docs

- [`../hew-cli/README.md`](../hew-cli/README.md) — CLI command reference
- [`../examples/README.md`](../examples/README.md) — runnable example entry
  points
- [`../examples/directory_module_demo/README.md`](../examples/directory_module_demo/README.md)
  — directory-form modules
- [`../examples/multifile/README.md`](../examples/multifile/README.md) —
  multi-file module patterns
- [`cross-platform-build-guide.md`](cross-platform-build-guide.md) — native
  toolchain setup
- [`specs/HEW-SPEC.md`](specs/HEW-SPEC.md) — language and module-system rules
- [`specs/MACHINE-SPEC.md`](specs/MACHINE-SPEC.md) — machine semantics
- [`diagrams.md`](diagrams.md) — compiler pipeline and runtime diagrams

If you still need help, include the exact `hew` command, the entry-point file
path, the full diagnostic output, `hew version`, and your OS/toolchain details.
