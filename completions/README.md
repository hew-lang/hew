# Shell Completions

Tab-completion scripts for the Hew compiler CLI (`hew`) and the Adze package manager (`adze`).

Completions are auto-generated from the CLI definitions using [`clap_complete`](https://docs.rs/clap_complete).

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

### PowerShell

```powershell
# Add to your PowerShell profile ($PROFILE):
hew completions powershell | Out-String | Invoke-Expression
adze completions powershell | Out-String | Invoke-Expression
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

### PowerShell

Copy the completion scripts to your PowerShell profile directory:

```powershell
cp completions/hew.ps1 $HOME/.config/powershell/hew.ps1
cp completions/adze.ps1 $HOME/.config/powershell/adze.ps1
# Then source them from your $PROFILE
```

## Regenerating

Completions are auto-generated from the CLI argument definitions:

```bash
hew completions bash > completions/hew.bash
hew completions zsh > completions/hew.zsh
hew completions fish > completions/hew.fish
hew completions powershell > completions/hew.ps1

adze completions bash > completions/adze.bash
adze completions zsh > completions/adze.zsh
adze completions fish > completions/adze.fish
adze completions powershell > completions/adze.ps1
```
