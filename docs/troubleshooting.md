# Troubleshooting

This guide is a narrow checklist for common `hew` compiler, CLI, and source-build failures.
Start with the smallest command that still reproduces the problem:

```sh
hew check path/to/main.hew          # parse + typecheck only
hew build path/to/main.hew -o app   # native build + link
hew run path/to/main.hew            # build and run
```

All `hew` CLI commands take a single entry-point file. For multi-file projects,
pass `main.hew` (or another real entry file) and let the compiler resolve
imports recursively from there.

## Import and module resolution failures

Common signs:

- a module-not-found error for a module such as `greeting`
- `cannot find value ... in this scope` immediately after adding an import
- confusing `unused import` warnings while reorganizing multi-file code

What to check:

- Run `hew check` against the entry file, not a peer file inside an imported
  module directory.
- For `import greeting;`, Hew looks for either `greeting.hew` beside the
  importer or `greeting/greeting.hew` inside a `greeting/` directory. In the
  directory form, the entry file stem must match the directory name.
- Peer files in the same module directory are merged automatically, but
  subdirectories are not. If you use a child module such as `text_stats::words`,
  import that child explicitly.
- Standard library imports use the last path segment as the local module name:
  `import std::fs;` gives `fs.read(...)`, and
  `import std::encoding::json;` gives `json.parse(...)`.
- If the missing import is a package dependency rather than a local module, run
  `adze install` so the package is available under `.adze/packages/`.
- The `module ... not found` error prints the exact paths Hew tried. Use that
  list to confirm whether the compiler is searching beside your entry file,
  under `.adze/packages/`, or under the installed standard library path.
- `import mod::*;` is supported, but the current unused-import checker can warn
  noisily when the import is used only through type references. Prefer bare
  (`import mod;`) or selective (`import mod::{Name}`) imports when that warning
  gets in the way.

See also:
[`../examples/directory_module_demo/README.md`](../examples/directory_module_demo/README.md),
[`../examples/multifile/README.md`](../examples/multifile/README.md), and
[`../hew-cli/README.md`](../hew-cli/README.md).

## Type inference and type mismatch diagnostics

Common signs:

- a `type mismatch` error between values such as `int` and `String`
- `cannot infer type for ...`
- `return type mismatch: expected ..., found ...`

What to try:

- Use `hew check` first. If `hew check` fails, `hew build` and `hew run` will
  fail too.
- Add explicit types where inference has too little context: function
  parameters, return types, locals passed into generic code, and `_`
  placeholders.
- Hew currently fails closed on unresolved type holes. For example,
  `type Foo = _;` produces a `cannot infer type for type alias` error with a
  help suggestion to add a type annotation.
- When a mismatch mentions two concrete types, fix the earlier source of the
  value rather than only the last call site. In practice that often means
  annotating the local or return value that first went ambiguous.

## Name lookup and arity mistakes

Common signs:

- an `undefined function` error for a misspelled name such as `calculate_sun`
- `cannot find value ... in this scope` / `cannot find type ... in this scope`
- `this function takes 2 argument(s) but 1 were supplied`

What to check:

- Fix spelling first. Hew can suggest close matches for values, functions,
  types, and fields.
- Re-check whether the name should be qualified. Bare module imports keep calls
  under the module name (`utils.helper()`), while selective imports bring only
  the listed names into scope.
- Confirm the current argument count after refactors. Arity errors often come
  from an outdated call site rather than the function you are editing now.

## Mutability and shadowing confusion

Common signs:

- a `cannot assign to immutable variable` error for a binding such as `count`
- a `variable ... is already defined in this scope` error
- a warning that a variable such as `count` shadows an outer binding

What to check:

- `let` bindings are immutable. If reassignment is intentional, change the
  binding to `var`.
- Reusing the same name in the same scope is a hard error. Rename the second
  binding instead of treating it like reassignment.
- Reusing a name in a nested scope is currently a warning, not an error. Rename
  it if the code reads ambiguously.
- If a binding is intentionally unused, or you want to suppress the shadowing
  warning explicitly, prefix it with an underscore such as `_count`.

## Exhaustiveness and pattern-match errors

Common signs:

- `error: non-exhaustive match: missing Blue`
- similar errors when a `match` on `Option` skips `None`
- similar errors when a `match` on `Result` skips `Ok` or `Err`

What to check:

- Matches over closed variant sets — user enums, `Option<T>`, and `Result<T, E>`
  — must cover every variant or include a catch-all arm.
- For scalar or open-ended values such as `int`, a missing catch-all is a
  warning rather than a hard error. Add `_ => ...` when you really mean
  "everything else".
- When a new enum variant lands, update old match sites before chasing unrelated
  downstream errors.

## Build, link, and toolchain issues

Start by separating frontend failures from toolchain failures:

- If `hew check path/to/main.hew` fails, stay in the sections above.
- If `hew check` passes but `hew build` fails, you are in codegen, linker, or
  host-toolchain territory.

For `hew build` problems:

- Hew uses the system linker toolchain and forwards raw linker output. Fix the
  reported missing libraries, bad `--link-lib` arguments, or target mismatch
  first.
- If the CLI reports `Error: clang not found. Install LLVM to link Hew
  programs.`, install LLVM/clang and make sure it is on `PATH`.

For source builds of this repo:

- Use the supported toolchain from the root README: Rust stable, LLVM/MLIR 22,
  CMake >= 3.20, Ninja, and clang/clang++.
- Use the supported entry points from the `Makefile`: `make`, `make release`,
  `make hew`, and `make test`.
- On Linux, build `hew-codegen` with clang/clang++ rather than GCC. LLVM's
  CMake setup passes Clang-only flags such as `-Wweak-vtables`.
- On macOS, use Homebrew LLVM (`LLVM_PREFIX="$(brew --prefix llvm)"`) for the
  compiler, linker, and libc++ paths when building `hew-codegen`. Errors such
  as `could not parse bitcode object file`, `unknown type name 'size_t'`, or
  unresolved `std::__1` symbols are covered in
  [`cross-platform-build-guide.md`](cross-platform-build-guide.md).
- After changing `HEW_STATIC`, switching LLVM installs, or changing C/C++
  compilers, run `make clean` before rebuilding.

## Related docs

- [`../hew-cli/README.md`](../hew-cli/README.md) — CLI command reference
- [`../examples/README.md`](../examples/README.md) — runnable example entry
  points
- [`../examples/directory_module_demo/README.md`](../examples/directory_module_demo/README.md)
  — directory-form modules
- [`../examples/multifile/README.md`](../examples/multifile/README.md) —
  multi-file module patterns
- [`cross-platform-build-guide.md`](cross-platform-build-guide.md) — deeper
  native toolchain setup
- [`specs/HEW-SPEC.md`](specs/HEW-SPEC.md) — language and module-system rules

If you still need help, include the exact `hew` command, the entry-point file
path, the full diagnostic output, `hew version`, and your OS/toolchain details.
