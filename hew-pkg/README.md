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

hew check
hew build
hew run
```

Standard-library modules ship with the compiler. Use `import std.net.http;`
in your source to access one; do not add it as a registry dependency.

For third-party packages, first use `hew search <QUERY>` and
`hew info <PACKAGE>` to confirm that the package and version are published.
Check that the lookup succeeds against the registry: local-cache fallback does
not confirm publication.
Then use `hew add <PACKAGE> --version <VERSION>` and `hew install` in your
project. Packages found in a source repository may not yet be in the registry;
follow that repository's instructions for using a clone or a local path.

## Commands

### Project Setup

- `hew init [NAME]` — Create a manifest-first Hew project (`hew.toml` + scaffold source + `.gitignore`)
  - `--lib` — Library project template
  - `--actor` — Actor project template
- `hew check [FILE|DIR]` — Validate the manifest and type-check its entry point; omit the argument to use the enclosing package
- `hew build [FILE|DIR]` — Build the package entry point, writing `<package-root>/<package-name>` by default
- `hew run [FILE|DIR]` — Compile and run the package entry point

The package form is the idiomatic workflow. Hew finds the nearest `hew.toml`
from the current directory (or a directory argument), then compiles
`[package] main`. That field defaults to `main.hew` relative to the manifest.
For a dotted package name such as `hew.db.sqlite`, the default binary is named
`sqlite`; pass `hew build -o PATH` to choose another output. An explicit `.hew`
file keeps the file-oriented behaviour and names its output from the file stem.

When a manifest declares `[native]`, `hew build` and `hew run` build its Rust
crate, C and C++ sources and system libraries and link them into every program
that compiles the package, including through imports; the
[package manifest reference](../docs/package-manifest.md) lists the fields.
Publishing
materializes Cargo workspace settings and bundles local Rust path dependencies,
including workspace patches, into the archive. The installed native crate keeps
the workspace lockfile and builds without the publisher's checkout. The generated
`hew-native-deps/` directory is reserved for those bundled dependencies; include
rules must retain the native crate's `Cargo.toml`.

Published Rust static libraries can also provide signed native prebuilts.
`hew install` fetches the archive for the host target and `hew build` retains
its verified cache generation through linking. Selection requires the exact
Hew build version, native target and runtime Rust toolchain stamp. The signed
archive also binds the package name, version and source archive checksum.
These archives use the ordinary publisher key and SHA-256 checksum signature.
`hew.lock` pins each selected compatibility identity and native archive checksum
to its source package. After installation, a cached native archive matching that
pin can build without Cargo, rustc or registry access. `hew install --locked`
refuses changes to these pins; an ordinary install updates them.

Source compilation is used only when the registry has no artifact for that
compatibility identity. Invalid signatures, checksum or identity mismatches,
damaged caches and failed downloads of advertised artifacts refuse the build;
they never trigger a source build. C, C++ and assembly inputs and system library
requirements still follow the package manifest.

Publishers build the native archive before acquiring a short-lived publication
grant, then submit source and native archives through one publication plan:

```sh
hew native build --target x86_64-unknown-linux-gnu --output target/native.tar.zst
hew publish --native-archive target/native.tar.zst
```

Native publication uses the existing registry login and signing key, refuses
replacement of an existing source/runtime identity and verifies the stored
record and archive before reporting success.

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
- `hew key register [--registry <NAME>]` — Register the existing local signing key with the registry (requires `hew login`)
  - `--registry`, `-r` — Use a named registry from config
- `hew key list` — List registered signing keys
- `hew key info <FINGERPRINT>` — Look up a signing key by fingerprint

### Registry

- `hew publish [--registry <NAME>]` — Publish package to a remote registry (requires `hew login`; fails if the remote publish fails)
  - `--registry`, `-r` — Use a named registry from config
  - `--local` — Install into the local package registry only; never contacts a remote registry
  - `--native-archive <PATH>` — Attach a native archive to its selected source package; repeat for multiple targets or workspace members (remote publication only)
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

### Local publication

For an unpublished package, create a signing key with `hew key generate`, then
run `hew publish --local` in the package directory. A consumer with an ordinary
version requirement can select that cache entry using `hew install --offline`.
The resulting `source = "local"` lock can subsequently be used with
`hew install --locked`. Normal online installation selects registry-confirmed
packages and ignores local publications.

Local publication retains the exact source archive and a separate local-origin
record. Native code builds from those checked source bytes with the matching
Rust toolchain; it does not query or acquire registry-native archives. Local
locks cannot contain signed native archive pins. Missing or damaged provenance
refuses reuse. Package content cannot supply the reserved provenance files.

A local publish cannot replace a verified registry generation with different
content. Publishing the same tree preserves that verified generation and its
origin. Native archive attachments require remote publication; `--local` cannot
attach a binary to a verified remote generation.

### Developer Tools

- `hew completions <bash|zsh|fish|powershell>` — Generate shell completion scripts (covers every subcommand)

## Manifest Format (hew.toml)

Example application manifest:

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
local_dep = { version = "0.1.0", path = "../local_dep" }
```

This example expects a sibling library created with `hew init --lib local_dep`.
Use `hew install` to prepare the dependency, then `import local_dep;` in your
source. A registry dependency uses a published package name and version
requirement instead of `path`.

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

Authored package names, dependencies, cache entries and lockfiles use dotted
names such as `hew.math.stats`. The official registry uses slash-separated
names on the wire; Hew translates at that boundary without changing package
archives or signatures over their checksums.

Custom APIs keep dotted wire names by default. To use a custom API implementing
the official slash protocol, configure its named registry:

```toml
[registries.internal]
index = "https://git.example.com/hew-index"
api = "https://packages.example.com/api/v1"
wire-names = "slash"
```

Select it with `hew info hew.math.stats -r internal` or add a dependency with
`hew add hew.math.stats -r internal`. `wire-names = "dotted"` explicitly selects
the custom default. Under `[registry]`, `wire-names` selects the encoding for
the default API, including an API selected by `HEW_REGISTRY`.

Online installs through a custom `HEW_REGISTRY` currently fail because the
resolver retains the official default source identity; configure and select a
named registry for custom installs.

The built-in CDN and mirror belong only to the official primary API. A custom
default API uses an explicitly configured mirror, with an independent encoding:

```toml
[registry]
wire-names = "slash"
fallback-api = "https://mirror.example.com/api/v1"
fallback-wire-names = "slash"
```

The same `fallback-api` and `fallback-wire-names` settings under
`[registries.internal]` configure that named source's mirror. Named sources do
not inherit the default registry's fallback.

The mirror serves metadata below its API base and archives at its origin under
`/packages/{wire-name}/{version}.tar.zst`. Network errors and server errors may
use the mirror; HTTP 404 and other client errors remain authoritative. Supplied
publisher and registry signatures must verify even when archive bytes are
already cached. Git-index file paths remain unchanged.

## Version Requirements

The resolver supports semver version requirements:

| Syntax  | Meaning                       |
| ------- | ----------------------------- |
| `*`     | Any version                   |
| `1.0.0` | Exact version                 |
| `^1.0`  | Compatible (>=1.0.0, <2.0.0)  |
| `~1.0`  | Approximate (>=1.0.0, <1.1.0) |
| `>=1.0` | Greater or equal              |

Native archives attach to the same source or workspace publication plan with
repeatable `--native-archive PATH` inputs. They share its credentials or trusted
publishing grant, signing key and verified readback. A source version that
already exists still discovers missing requested native artifacts. Every
native descriptor must name a selected native source package and its exact
published source archive checksum; a same-tree source with different compression
retains the registry archive identity. Rebuild a mismatched native input from
the verified installed source archive, rather than changing its descriptor.
`--dry-run` validates and compares both kinds without authentication or uploads.
