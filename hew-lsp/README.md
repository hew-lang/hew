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

The language server is linked into the `hew` executable. Editors launch `hew`
with the `lsp` argument; no sibling executable is required.

From a source checkout, run:

```sh
make hew-native
target/debug/hew lsp
```

For editor integrations, use a matching installed Hew SDK and pass `lsp`
before language-server options such as `--pkg-path`.

See [`docs/dev/lsp-editor-setup.md`](../docs/dev/lsp-editor-setup.md) for the
build/launch/VS Code walkthrough, and [`../editors/README.md`](../editors/README.md)
for editor-specific setup notes.

## Part of the Hew compiler

This crate is an internal component of the [Hew](https://github.com/hew-lang/hew) compiler toolchain.
