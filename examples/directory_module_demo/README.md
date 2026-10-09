# Directory-module demo

This tiny example proves Hew's existing directory-module merge path with three
Hew files: one consumer file plus a directory-form module made of two peer
files.

## Layout

```text
examples/directory_module_demo/
├── hew.toml
├── main.hew
└── greeting/
    ├── greeting.hew
    └── greeting_helpers.hew
```

- `main.hew` imports `greeting;`
- `greeting/greeting.hew` is the directory-form module entry because the
  directory basename matches the file stem
- `greeting/greeting_helpers.hew` is a peer file that Hew merges into the same
  module during import resolution

Because both `hello()` and `target()` are reached through `greeting.*`, this
example exercises peer-file merging instead of a simpler single-file import.
Directory modules belong to a package, so the example carries a small
`hew.toml`; without it `greeting/` would be a plain namespace and
`import greeting;` would not be found. The manifest leaves `[package] main`
unset for the conventional `main.hew` layout, so bare `hew check`, `hew run`
and `hew build` work inside the directory; the commands below name
`main.hew` so they also run from the repository root.

## Run from the repo root

```sh
hew check examples/directory_module_demo/main.hew
hew run examples/directory_module_demo/main.hew
hew build examples/directory_module_demo/main.hew -o directory_module_demo && ./directory_module_demo
```

Expected output:

```text
Hello from a merged directory module!
```

`hew check` passes in v0.5. `hew run` and `hew build` require string
concatenation lowering, which lands in the v0.5 codegen milestone. The
directory-module resolution path (`import greeting;`, peer-file merge, and
function-call lowering) is covered by the vertical-slice acceptance suite.

If you want the next step after this minimal layout, continue with
[`../multifile/README.md`](../multifile/README.md) for selective imports and
nested module hierarchies.
