# Editor setup: the Hew language server

`hew-lsp` is the Language Server Protocol implementation for Hew. It gives any
LSP-capable editor real-time diagnostics, completion, hover, go-to-definition,
find-references, rename, document/workspace symbols, signature help, inlay
hints, semantic highlighting, code actions, and call/type hierarchy for `.hew`
files.

This page covers building, launching, and pointing an editor (VS Code first) at
the server. For non-VS Code highlighting configs, see
[`editors/README.md`](../../editors/README.md).

## Build and install a matching SDK

The language server is linked into `hew`. Use the compiler and standard library
from the same build so editor diagnostics and command-line checks agree.

From a source checkout:

```sh
make hew-native
target/debug/hew lsp --version
```

To install an optimized toolchain:

```sh
make release
make install PREFIX="$HOME/.local"
```

## Launch

The server speaks LSP over stdin/stdout; editors launch `hew` with `lsp` as the
first argument:

```sh
hew lsp
hew lsp --help
```

`--pkg-path <DIR>` mirrors `hew check --pkg-path`, selecting one package search
directory. Missing, empty and repeated values are rejected before the server
starts. `--version` and `--help` exit without starting a session.

A minimal client configuration is:

```jsonc
{
  "command": "hew",
  "args": ["lsp"],
  "filetypes": ["hew"],
  "rootPatterns": ["hew.toml", ".git"]
}
```

## VS Code

The matching extension launches the same executable for language-server and
formatter operations. Set `hew.lsp.serverPath` and `hew.formatterPath` to the
absolute path of `hew` when selecting an installation:

```jsonc
{
  "hew.lsp.serverPath": "/absolute/path/to/hew/bin/hew",
  "hew.formatterPath": "/absolute/path/to/hew/bin/hew"
}
```

On Windows, use `hew.exe` and escape backslashes in JSON. Settings do not expand
`~` or `${workspaceFolder}`. Restricted Mode ignores workspace executable paths.
The extension and bundled SDK must be released together: older SDKs still
require a sibling language-server executable.

## Other editors

`hew lsp` is a standard LSP server, so any client works. See
[`editors/README.md`](../../editors/README.md) for Vim/Neovim, Emacs, Sublime,
and nano. Crate-level feature notes live in
[`hew-lsp/README.md`](../../hew-lsp/README.md).

## Verifying a session

Source CI exercises the built server end to end — `initialize` → `didOpen` →
`publishDiagnostics` — in `hew-cli/tests/lsp_protocol.rs`. This is separate
from checking the server in a packaged SDK. Run the protocol tests locally to
confirm a build serves diagnostics, not just `--version`:

```sh
make test-lsp-protocol
```
