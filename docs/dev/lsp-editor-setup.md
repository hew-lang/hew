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
make hew-lsp
# binary: target/debug/hew-lsp
```

Build the optimized compiler toolchain:

```sh
make release
# binary: target/release/hew-lsp
```

After the release build, install the toolchain onto your `PATH`:

```sh
make install PREFIX="$HOME/.local"
# binary: ~/.local/bin/hew-lsp
```

## Launch

The server speaks LSP over stdin/stdout; editors spawn it, you don't run it
interactively. Two equivalent entry points:

```sh
hew-lsp                 # the standalone binary
hew lsp                 # thin launcher; forwards args to hew-lsp on the same PATH/dir
```

`--pkg-path <DIR>` mirrors `hew check --pkg-path`, selecting one package search
directory so package imports resolve the same way in the editor as on the
command line. Missing values and repeated `--pkg-path` options are rejected
before the server starts.
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

Source CI exercises the built server end to end — `initialize` → `didOpen` →
`publishDiagnostics` — in `hew-lsp/tests/protocol_smoke.rs`. This is separate
from checking the server in a packaged SDK. Run the protocol tests locally to
confirm a build serves diagnostics, not just `--version`:

```sh
make test NEXTEST_WORKSPACE_FILTER='package(hew-lsp) and binary(protocol_smoke)'
```
