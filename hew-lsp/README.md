# hew-lsp

Language Server Protocol implementation for Hew.

Shipped IDE/LSP features for Hew source files include:

- Real-time diagnostics (parse errors, type errors)
- Completions
- Hover information
- Go-to-definition, including struct field accesses at use sites
- Find references
- Rename (with prepare support)
- Document symbols
- Workspace symbols
- Signature help
- Inlay hints
- Semantic tokens for syntax highlighting
- Code actions / quick-fixes
- Call hierarchy
- Type hierarchy (supertypes / subtypes)
- Document links for imports
- Folding ranges
- Code lenses for reference counts and test execution
- Execute command support for running Hew tests (`hew.runTest`)

## Usage

The language server is the standalone `hew-lsp` binary crate. The `hew` CLI
does not currently expose a `hew lsp` subcommand.

From a source checkout, run:

```sh
cargo run -p hew-lsp --
```

For editor integrations, point your LSP client at the `hew-lsp` binary. You
can either build it in the workspace:

```sh
cargo build -p hew-lsp
# binary: target/debug/hew-lsp
```

or install it onto your `PATH`:

```sh
cargo install --path hew-lsp
```

See [`../editors/README.md`](../editors/README.md) for editor-specific setup
notes.

## Part of the Hew compiler

This crate is an internal component of the [Hew](https://github.com/hew-lang/hew) compiler toolchain.
