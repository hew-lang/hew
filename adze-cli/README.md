# adze

The Hew package manager.

## Installation

`adze` is built as part of the Hew compiler toolchain:

```bash
cargo install --path adze-cli
```

## Quick Start

```bash
# Create a manifest-first project
adze init myproject
cd myproject

# adze init creates hew.toml, main.hew, and .gitignore
hew check main.hew
hew run main.hew

# Add a dependency
adze add std::net::http --version "^1.0"

# Install dependencies
adze install
```

## Commands

### Project Setup

- `adze init [NAME]` — Create a manifest-first Hew project (`hew.toml` + scaffold source + `.gitignore`)
  - `--lib` — Library project template
  - `--actor` — Actor project template
- `adze check` — Validate your manifest

### Dependency Management

- `adze add <PACKAGE> [--version <VER>] [--registry <NAME>]` — Add a dependency
  - `--registry`, `-r` — Use a named registry from config
- `adze remove <PACKAGE>` — Remove a dependency
- `adze install [--locked] [--registry <NAME>]` — Install all dependencies
  - `--registry`, `-r` — Use a named registry from config
- `adze update [PACKAGE]` — Update dependency versions
- `adze outdated` — Show outdated dependencies

### Authentication

- `adze login` — Log in to the registry via GitHub
- `adze logout` — Log out from the registry
- `adze key generate` — Generate a new Ed25519 signing keypair
- `adze key list` — List registered signing keys
- `adze key info <FINGERPRINT>` — Look up a signing key by fingerprint

### Registry

- `adze publish [--registry <NAME>]` — Publish package to the registry
  - `--registry`, `-r` — Use a named registry from config
- `adze list` — List installed packages
- `adze search <QUERY> [--category <CATEGORY>] [--page <N>] [--per-page <N>] [--registry <NAME>]` — Search for packages
  - `--registry`, `-r` — Use a named registry from config
- `adze info <PACKAGE> [--registry <NAME>]` — Show package details
  - `--registry`, `-r` — Use a named registry from config
- `adze tree` — Show dependency tree
- `adze namespace register <PREFIX>` — Register a custom namespace prefix
- `adze namespace info <PREFIX>` — Show info about a namespace
- `adze yank <VERSION> [--reason <TEXT>] [--undo]` — Yank a published version or undo a yank
- `adze registry-key` — Show the registry's public signing key
- `adze deprecate [PACKAGE] [--message <TEXT>] [--successor <PACKAGE>] [--undo]` — Deprecate a package or undo deprecation
- `adze index sync` — Sync the local package index from the registry
- `adze index resolve <PACKAGE> [--version <VER>]` — Resolve a package version from the local index
- `adze index list <PACKAGE>` — List all versions of a package in the local index

### Developer Tools

- `adze completions <bash|zsh|fish|powershell>` — Generate shell completion scripts

## Manifest Format (hew.toml)

`adze init` writes a starter manifest like:

```toml
[package]
name = "my-project"
version = "1.0.0"
description = "A Hew project"
authors = ["Your Name"]
license = "MIT"
keywords = ["hew", "web"]
repository = "https://github.com/user/project"

[dependencies]
"std::net::http" = "^1.0"
"ecosystem::db::postgres" = "~2.0"
```

## Lock File (adze.lock)

`adze install` generates an `adze.lock` file pinning exact dependency versions
for reproducible builds. Use `adze install --locked` to enforce the lock file.

## Configuration (~/.adze/config.toml)

```toml
[defaults]
author = "Your Name"
license = "MIT"

[registry]
path = "~/.adze/packages"
```

## Version Requirements

adze supports semver version requirements:

| Syntax  | Meaning                       |
| ------- | ----------------------------- |
| `*`     | Any version                   |
| `1.0.0` | Exact version                 |
| `^1.0`  | Compatible (>=1.0.0, <2.0.0)  |
| `~1.0`  | Approximate (>=1.0.0, <1.1.0) |
| `>=1.0` | Greater or equal              |
