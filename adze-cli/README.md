# adze

The Hew package manager.

## Installation

`adze` is built as part of the Hew compiler toolchain:

```bash
cargo install --path adze-cli
```

## Quick Start

```bash
# Create a new project
adze init myproject
cd myproject

# Add a dependency
adze add std::net::http --version "^1.0"

# Install dependencies
adze install

# Build your project
hew build main.hew
```

## Commands

### Project Setup

- `adze init [NAME]` — Create a new Hew project
  - `--lib` — Library project template
  - `--bin` — Binary project template (default)
  - `--actor` — Actor project template
- `adze check` — Validate your manifest

### Dependency Management

- `adze add <PACKAGE> [--version <VER>]` — Add a dependency
- `adze remove <PACKAGE>` — Remove a dependency
- `adze install [--locked]` — Install all dependencies
- `adze update [PACKAGE]` — Update dependency versions
- `adze outdated` — Show outdated dependencies

### Registry

- `adze publish` — Publish package to the registry
- `adze list` — List installed packages
- `adze search <QUERY>` — Search for packages
- `adze info <PACKAGE>` — Show package details
- `adze tree` — Show dependency tree

## Manifest Format (hew.toml)

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
