# Shell Completions

Tab-completion scripts for the Hew compiler CLI (`hew`) and the Adze package manager (`adze`).

## Quick Install

The `hew completions <shell>` and `adze completions <shell>` commands print the completion script to stdout:

### Bash

```bash
# Add to ~/.bashrc:
eval "$(hew completions bash)"
eval "$(adze completions bash)"
```

### Zsh

```bash
# Add to ~/.zshrc:
eval "$(hew completions zsh)"
eval "$(adze completions zsh)"
```

### Fish

```bash
hew completions fish > ~/.config/fish/completions/hew.fish
adze completions fish > ~/.config/fish/completions/adze.fish
```

## Manual Install

### Bash

Copy the completion scripts to your bash completions directory:

```bash
# System-wide:
sudo cp completions/hew.bash /etc/bash_completion.d/hew
sudo cp completions/adze.bash /etc/bash_completion.d/adze

# Per-user (if using bash-completion 2.x):
cp completions/hew.bash ~/.local/share/bash-completion/completions/hew
cp completions/adze.bash ~/.local/share/bash-completion/completions/adze
```

### Zsh

Copy completion files as `_hew` / `_adze` into a directory in your `$fpath`:

```bash
# System-wide:
sudo cp completions/hew.zsh /usr/local/share/zsh/site-functions/_hew
sudo cp completions/adze.zsh /usr/local/share/zsh/site-functions/_adze

# Per-user:
mkdir -p ~/.zsh/completions
cp completions/hew.zsh ~/.zsh/completions/_hew
cp completions/adze.zsh ~/.zsh/completions/_adze
# Then add to ~/.zshrc: fpath=(~/.zsh/completions $fpath)
```

### Fish

Copy completion files to the fish completions directory:

```bash
cp completions/hew.fish ~/.config/fish/completions/hew.fish
cp completions/adze.fish ~/.config/fish/completions/adze.fish
```

## What's Completed

### hew

- **Subcommands**: `build`, `run`, `check`, `doc`, `eval`, `test`, `wire`, `fmt`, `init`, `completions`, `version`, `help`
- **File arguments**: `.hew` files for `build`, `run`, `check`, `fmt`, and `wire check`
- **Options**: `-o`, `--Werror`, `--no-typecheck`, `--emit-mlir`, `--emit-llvm`, `--emit-obj`
- **Wire subcommands**: `wire check` with `--against`
- **Shorthand**: `hew file.hew` completes `.hew` files at the top level

### adze

- **Subcommands**: `init`, `add`, `install`, `publish`, `list`, `search`, `info`, `tree`, `update`, `remove`, `check`, `outdated`, `login`, `logout`, `key`, `namespace`, `yank`, `registry-key`, `deprecate`, `index`, `completions`
- **Nested subcommands**: `key {generate,list,info}`, `namespace {register,list,info}`, `index {sync,resolve,list}`
- **Per-command options**: `--version`, `--registry`/`-r`, `--locked`, `--category`, `--page`, `--per-page`, `--lib`, `--bin`, `--actor`, `--reason`, `--undo`, `--message`, `--successor`
