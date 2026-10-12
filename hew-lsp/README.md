# hew-lsp

Language Server Protocol implementation for Hew.

Shipped IDE/LSP features for Hew source files include:

- Real-time diagnostics (parse errors, type errors)
- Completions
- Hover information
- Go-to-definition, including record field accesses at use sites
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
also exposes `hew lsp` as a thin launcher that forwards all arguments to this
binary.

From a source checkout, run:

```sh
make hew-lsp
target/debug/hew-lsp
```

For editor integrations, point your LSP client at the built `hew-lsp` binary
or an installed Hew SDK.

See [`docs/dev/lsp-editor-setup.md`](../docs/dev/lsp-editor-setup.md) for the
build/launch/VS Code walkthrough, and [`../editors/README.md`](../editors/README.md)
for editor-specific setup notes.

## Part of the Hew compiler

This crate is an internal component of the [Hew](https://github.com/hew-lang/hew) compiler toolchain.
