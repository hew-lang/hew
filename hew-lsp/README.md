# hew-lsp

Language Server Protocol implementation for Hew.

Core IDE features for Hew source files include:

- Real-time diagnostics (parse errors, type errors)
- Go-to-definition
- Hover information
- Document symbols

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
