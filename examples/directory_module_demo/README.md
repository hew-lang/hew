# Directory-module demo

This tiny example proves Hew's existing directory-module merge path with three
Hew files: one consumer file plus a directory-form module made of two peer
files.

## Layout

```text
examples/directory_module_demo/
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
Run CLI commands against `main.hew`; its `import greeting;` pulls in the
directory-form module and peer file automatically.

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
