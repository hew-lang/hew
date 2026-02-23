# hew-lsp

Language Server Protocol implementation for Hew.

Provides IDE features for Hew source files via the LSP protocol:

- Real-time diagnostics (parse errors, type errors)
- Go-to-definition
- Hover information
- Document symbols

## Usage

The language server is built into the `hew` CLI:

```sh
hew lsp
```

Editor integrations (VS Code, Neovim, Emacs, etc.) can connect to this server using standard LSP client configuration.

## Part of the Hew compiler

This crate is an internal component of the [Hew](https://github.com/hew-lang/hew) compiler toolchain.
