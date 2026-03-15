# Editor Support

Syntax highlighting and editor configuration for editors other than VSCode.

For VSCode, see [vscode-hew](https://github.com/hew-lang/vscode-hew).

## Mobile editors (Android / iOS)

Shared mobile-facing editor data lives at `editors/mobile/hew.mobile-editor.json`.

It is generated from `docs/syntax-data.json` and is designed for app-bundled
use in Android and iOS editors:

- `completion_catalog.items`: shared language-level completions
- `lexical`: canonical lexical groups, operators, prefixes, suffixes, and comment styles
- `highlighting.textmate_scopes`: grouped tokens for lightweight highlighters
- `downstream.tree_sitter_sync`: grouped lists aligned with the existing tree-sitter sync tool

Regenerate it with:

```sh
node tools/downstream/generate-mobile-editor-assets.mjs
./scripts/sync-downstream.sh
```

Mobile clients should treat this file as shared static language data, then merge
it with document-local symbols or platform-specific editing behaviour in-app.

## Vim / Neovim

Install via any plugin manager from [hew-lang/vim-hew](https://github.com/hew-lang/vim-hew):

```vim
" vim-plug
Plug 'hew-lang/vim-hew'
```

```lua
-- lazy.nvim
{ 'hew-lang/vim-hew' }
```

## nano

Add to your `~/.nanorc`:

```
include "/path/to/hew/editors/nano/hew.nanorc"
```

Or copy it to nano's system syntax directory:

```sh
sudo cp editors/nano/hew.nanorc /usr/share/nano/hew.nanorc
```

## Sublime Text

Copy the files from `editors/sublime/` into a `Hew` package directory:

```sh
# macOS
mkdir -p ~/Library/Application\ Support/Sublime\ Text/Packages/Hew
cp editors/sublime/*.{json,tmPreferences} ~/Library/Application\ Support/Sublime\ Text/Packages/Hew/

# Linux
mkdir -p ~/.config/sublime-text/Packages/Hew
cp editors/sublime/*.{json,tmPreferences} ~/.config/sublime-text/Packages/Hew/
```

## Emacs

Add to your Emacs configuration (`~/.emacs.d/init.el` or `~/.emacs`):

```elisp
(add-to-list 'load-path "/path/to/hew/editors/emacs")
(require 'hew-mode)
```

This provides:

- Syntax highlighting for all Hew keywords, types, and literals
- `//` and `/* */` comment support (M-;)
- Block-based indentation (4 spaces)
- Automatic `.hew` file association

## Tree-sitter (Helix, Zed, Neovim)

A tree-sitter grammar is available at [hew-lang/tree-sitter-hew](https://github.com/hew-lang/tree-sitter-hew).

### Helix

Add to `~/.config/helix/languages.toml`:

```toml
[[language]]
name = "hew"
scope = "source.hew"
injection-regex = "hew"
file-types = ["hew"]
comment-token = "//"
indent = { tab-width = 4, unit = "    " }

[[grammar]]
name = "hew"
source = { git = "https://github.com/hew-lang/tree-sitter-hew", rev = "main" }
```

Then run `hx --grammar fetch && hx --grammar build` and copy the queries:

```sh
mkdir -p ~/.config/helix/runtime/queries/hew
cp ~/projects/tree-sitter-hew/queries/*.scm ~/.config/helix/runtime/queries/hew/
```

### Neovim (nvim-treesitter)

Add to your Neovim config:

```lua
local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
parser_config.hew = {
  install_info = {
    url = "https://github.com/hew-lang/tree-sitter-hew",
    files = { "src/parser.c" },
    branch = "main",
  },
  filetype = "hew",
}

vim.filetype.add({ extension = { hew = "hew" } })
```

Then run `:TSInstall hew` and copy highlight queries:

```sh
mkdir -p ~/.config/nvim/queries/hew
cp ~/projects/tree-sitter-hew/queries/*.scm ~/.config/nvim/queries/hew/
```

### Zed

Tree-sitter support for Zed can be added via a Zed extension. See the [Zed extension docs](https://zed.dev/docs/extensions/languages) for details.
