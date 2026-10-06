# Editor setup: the Hew language server

`hew-lsp` is the Language Server Protocol implementation for Hew. It gives any
LSP-capable editor real-time diagnostics, completion, hover, go-to-definition,
find-references, rename, document/workspace symbols, signature help, inlay
hints, semantic highlighting, code actions, and call/type hierarchy for `.hew`
files.

This page covers building, launching, and pointing an editor (VS Code first) at
the server. For non-VS Code highlighting configs, see
[`editors/README.md`](../../editors/README.md).

## Build

From a source checkout:

```sh
cargo build -p hew-lsp
# binary: target/debug/hew-lsp
```

Release build (what CI smoke-tests and what editors should use day to day):

```sh
cargo build -p hew-lsp --release
# binary: target/release/hew-lsp
```

Or install it onto your `PATH`:

```sh
cargo install --path hew-lsp
# binary: ~/.cargo/bin/hew-lsp
```

## Launch

The server speaks LSP over stdin/stdout; editors spawn it, you don't run it
interactively. Two equivalent entry points:

```sh
hew-lsp                 # the standalone binary
hew lsp                 # thin launcher; forwards args to hew-lsp on the same PATH/dir
```

`--pkg-path <DIR>` mirrors `hew check --pkg-path`, adding package search roots so
package imports resolve the same way in the editor as on the command line.
`--version` and `--help` print and exit without starting a session.

Confirm the binary runs:

```sh
hew-lsp --version
```

## VS Code

Install **Hew Language** (`hew-lang.hew-lang`) from the
[VS Code Marketplace](https://marketplace.visualstudio.com/items?itemName=hew-lang.hew-lang),
or run `code --install-extension hew-lang.hew-lang`. Open a `.hew` file to
activate highlighting and the language server. The extension searches `PATH`
and includes a bundled `hew-lsp` for supported platforms.

To select a particular compiler installation, set `hew.lsp.serverPath` to the
absolute path of its `hew-lsp` binary and `hew.formatterPath` to its `hew`
binary. Use workspace settings in a workspace you trust, or user settings for
a trusted installed toolchain. For example, on macOS or Linux:

```jsonc
// .vscode/settings.json
{
  "hew.lsp.serverPath": "/absolute/path/to/hew/bin/hew-lsp",
  "hew.formatterPath": "/absolute/path/to/hew/bin/hew"
}
```

Use full paths rather than `${workspaceFolder}` or `~`; these settings are
passed to executable discovery without variable expansion. On Windows, use
paths to `hew-lsp.exe` and `hew.exe`, escaping backslashes in JSON.

A minimal client is just stdio transport with a `.hew` document selector — any
editor that can spawn a command works the same way:

```jsonc
{
  "command": "hew-lsp",
  "args": [],
  "filetypes": ["hew"],
  "rootPatterns": ["hew.toml", ".git"]
}
```

## Other editors

`hew-lsp` is a standard LSP server, so any client works. See
[`editors/README.md`](../../editors/README.md) for Vim/Neovim, Emacs, Sublime,
and nano. Crate-level feature notes live in
[`hew-lsp/README.md`](../../hew-lsp/README.md).

## Verifying a session

CI exercises the shipped binary end to end — `initialize` → `didOpen` →
`publishDiagnostics` — in `hew-lsp/tests/protocol_smoke.rs`. Run it locally to
confirm a build serves diagnostics, not just `--version`:

```sh
cargo nextest run -p hew-lsp --test protocol_smoke
```
