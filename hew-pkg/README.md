# hew-pkg

The Hew package manager, built into the `hew` CLI. This crate is a
library: the `hew` binary flattens its command surface into its own top
level, so every command below is a native `hew` subcommand and there is
no separate package-manager binary.

## Quick Start

```bash
# Create a manifest-first project (hew.toml, main.hew, .gitignore)
hew init myproject
cd myproject

hew check main.hew
hew run main.hew

# Add a dependency
hew add std::net::http --version "^1.0"

# Install dependencies
hew install
```

## Commands

### Project Setup

- `hew init [NAME]` — Create a manifest-first Hew project (`hew.toml` + scaffold source + `.gitignore`)
  - `--lib` — Library project template
  - `--actor` — Actor project template
- `hew check` — Validate your manifest (with no input file)
- `hew build` — Build and stage this package's `[native]` FFI library (with no input file)

### Dependency Management

- `hew add <PACKAGE> [--version <VER>] [--registry <NAME>]` — Add a dependency
  - `--registry`, `-r` — Use a named registry from config
- `hew remove <PACKAGE>` — Remove a dependency
- `hew install [--locked] [--registry <NAME>]` — Install all dependencies
  - `--registry`, `-r` — Use a named registry from config
- `hew update [PACKAGE]` — Update dependency versions
- `hew outdated` — Show outdated dependencies

### Authentication

- `hew login` — Log in to the registry via GitHub
- `hew logout` — Log out from the registry
- `hew key generate` — Generate a new Ed25519 signing keypair
- `hew key list` — List registered signing keys
- `hew key info <FINGERPRINT>` — Look up a signing key by fingerprint

### Registry

- `hew publish [--registry <NAME>]` — Publish package to the registry
  - `--registry`, `-r` — Use a named registry from config
- `hew list` — List installed packages
- `hew search <QUERY> [--category <CATEGORY>] [--page <N>] [--per-page <N>] [--registry <NAME>]` — Search for packages
  - `--registry`, `-r` — Use a named registry from config
- `hew info <PACKAGE> [--registry <NAME>]` — Show package details
  - `--registry`, `-r` — Use a named registry from config
- `hew tree` — Show dependency tree
- `hew namespace register <PREFIX>` — Register a custom namespace prefix
- `hew namespace info <PREFIX>` — Show info about a namespace
- `hew yank <VERSION> [--reason <TEXT>] [--undo]` — Yank a published version or undo a yank
- `hew key registry` — Show the registry's public signing key
- `hew deprecate [PACKAGE] [--message <TEXT>] [--successor <PACKAGE>] [--undo]` — Deprecate a package or undo deprecation
- `hew index sync` — Sync the local package index from the registry
- `hew index resolve <PACKAGE> [--version <VER>]` — Resolve a package version from the local index
- `hew index list <PACKAGE>` — List all versions of a package in the local index

### Developer Tools

- `hew completions <bash|zsh|fish|powershell>` — Generate shell completion scripts (covers every subcommand)

## Manifest Format (hew.toml)

`hew init` writes a starter manifest like:

```toml
[package]
name = "my-project"
edition = "2026"
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

The `edition` field selects the Hew language edition the package's sources
target. Currently only `"2026"` is accepted; the compiler refuses to build a
package that names an unsupported edition. When the field is omitted the
current edition is assumed, but new manifests should set it explicitly.

## Lock File (hew.lock)

`hew install` generates a `hew.lock` file pinning exact dependency versions
for reproducible builds. Use `hew install --locked` to enforce the lock file.

## Configuration (~/.hew/config.toml)

```toml
[defaults]
author = "Your Name"
license = "MIT"

[registry]
path = "~/.hew/packages"
```

## Version Requirements

The resolver supports semver version requirements:

| Syntax  | Meaning                       |
| ------- | ----------------------------- |
| `*`     | Any version                   |
| `1.0.0` | Exact version                 |
| `^1.0`  | Compatible (>=1.0.0, <2.0.0)  |
| `~1.0`  | Approximate (>=1.0.0, <1.1.0) |
| `>=1.0` | Greater or equal              |
