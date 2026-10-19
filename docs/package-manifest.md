# Package manifest reference

Every Hew package has a `hew.toml` at its root. The manifest names the package,
lists its dependencies and declares the native code behind its `extern "C"`
functions. `hew build`, `hew run`, `hew check` and `hew test` find it by walking
up from the source file or directory they are given; a source file belongs to
the package whose `hew.toml` is nearest above it.

```toml
[package]
name = "meshcore.broker"
version = "0.3.0"
edition = "2026"
main = "broker_main.hew"

[dependencies]
"hew.db.sqlite" = "0.2"

[native]
sources = ["native/listener.c", "native/events.c"]
include-dirs = ["native/include"]
cflags = ["-std=c11", "-Wall", "-Wextra"]
pkg-config = ["openssl"]

[native.windows]
link-libs = ["libcrypto", "ws2_32"]
lib-dirs = ["C:/Program Files/OpenSSL/lib"]
```

## `[package]`

| Field                                                                                                              | Meaning                                                                                                                                                                                                                                                      |
| ------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `name`                                                                                                             | Required. Dotted lowercase segments (`acme.http.router`). The last segment names the binary. Each segment must be one import-path segment: it starts with a letter or `_` (`acme.three_d`, not `acme.3d`); keywords such as `type` and `actor` are accepted. |
| `version`                                                                                                          | Required. A semantic version.                                                                                                                                                                                                                                |
| `edition`                                                                                                          | The language edition, `"2026"`.                                                                                                                                                                                                                              |
| `main`                                                                                                             | The entry file relative to `hew.toml`; defaults to `main.hew`.                                                                                                                                                                                               |
| `description`, `authors`, `license`, `keywords`, `categories`, `homepage`, `repository`, `documentation`, `readme` | Registry metadata.                                                                                                                                                                                                                                           |
| `include`, `exclude`                                                                                               | Glob patterns that select the files `hew publish` packs.                                                                                                                                                                                                     |
| `hew`                                                                                                              | The compiler versions the package supports, as a semver range such as `">=0.6.1, <0.7.0"`.                                                                                                                                                                   |

`hew check` and `hew publish` refuse a name that an import cannot spell, so
every published package can be imported by its whole name. Packages already on
a registry under such a name still install.

`hew` is checked against the running compiler. `hew check` and `hew publish`
refuse a range that does not parse or that excludes the compiler, with
`E_PKG_COMPILER_RANGE`. `hew install` skips registry versions whose range
excludes the compiler and picks the highest version that admits it; when every
matching version is excluded, the error names each version and its range.

## Modules in a package

The directory holding `hew.toml` is the package's anchor. Every file below it
belongs to the module its place under the anchor names; the anchor
directory's own name is never read, so a checkout, a registry install and a
linked path dependency give each file the same module.

- The **root module** of package `acme.http` is the single file `http.hew`
  beside `hew.toml`, named by the last segment of `name`. `hew init --lib`
  writes it.
- Every other top-level file is a module of its own: `client.hew` is
  `acme.http.client`.
- A subdirectory `D` holding `D/D.hew` is a directory module: `D/D.hew` is its
  entry and the other top-level `.hew` files in `D`, except `*_test.hew`, are
  merged into it (HEW-SPEC-2026 §3.5.1).
- Any file in the package imports the package's modules by the package's whole
  name (`import acme.http;`, `import acme.http.client;`), by a path from the
  package root (`import client;`), or by a path relative to itself. Such imports
  need no `[dependencies]` entry. Distinct files answering the same import
  make it ambiguous.

```
my-http/                 ← the checkout; any name
├── hew.toml             [package] name = "acme.http"
├── http.hew             module acme.http
├── http_test.hew        tests acme.http
├── client.hew           module acme.http.client
├── router/
│   ├── router.hew       module acme.http.router (entry)
│   └── routes.hew       peer of acme.http.router
└── tests/public_api.hew import acme.http;
```

## `[dependencies]` and `[dev-dependencies]`

Each key is a module path; each value is a version requirement or a table:

```toml
[dependencies]
"hew.db.sqlite" = "0.2"
"acme.metrics" = { version = "1.4", features = ["prometheus"] }
"acme.local" = { path = "../local" }
```

A dependency's key also covers the modules inside it: declaring
`"acme.http"` permits `import acme.http.client;`.

Table fields are `version`, `optional`, `features`, `default-features`,
`registry` and `path`. A `path` dependency needs no `version`; it names the
directory of a package whose own `hew.toml` carries the dependency's name.
`hew install` links it into `.hew/packages/`, where the import finds it, and
its `[native]` code builds with the program. `[dev-dependencies]` is used by `hew test` and never
reaches a consumer of the package. `[features]` maps feature names to the
features they enable.

## Build paths

`hew build --trim-paths` removes the checkout prefix from source diagnostics
and compiled debug paths. Release builds enable it by default; use
`hew build --release --no-trim-paths` to retain original paths.

Project files use paths relative to `.`. Imported packages outside the
project use `packages/<name>/<version>`, and standard-library files use `std`.
The compiler still reads and tracks the original files: module identities,
package checksums and Make dependency files keep their usual meaning.

```sh
hew build --release -g
# After relocating the sources, in GDB:
# set substitute-path . /path/to/checkout
# In LLDB:
# settings set target.source-map . /path/to/checkout
```

Native C, C++ and Rust compilation uses the same prefix mappings. Existing
Cargo flags remain active. `hew native build` trims the native archive when
it is produced; a consumer keeps a signed prebuilt archive unchanged.

## `[native]`

`[native]` declares the native code behind the package's `extern "C"`
functions. Whenever a program compiles a module of the package, directly or
through any depth of imports, `hew build` and `hew run` build that code and link
it. Neither the package nor its consumers pass `--link-lib`.

A section may declare a Rust crate, C, C++ and assembly sources, system libraries, or any
combination. An empty `[native]` is refused.

### Rust crate

| Field   | Meaning                                                                         |
| ------- | ------------------------------------------------------------------------------- |
| `crate` | The crate directory relative to `hew.toml`. Declaring it declares a Rust crate. |

```toml
[native]
crate = "."
```

Cargo.toml owns the library's name and crate type. Its `[lib]` table must list
`"staticlib"` or `"cdylib"` in `crate-type`; `hew check` and `hew publish` read
`cargo metadata` and refuse a crate that builds neither with
`E_INVALID_NATIVE`, before anything builds. The build links the `staticlib`
Cargo reports, or the `cdylib` when the crate builds no `staticlib`. A
manifest that still names `lib` or `kind` in `[native]` is refused with
`E_INVALID_NATIVE`, naming the line to delete.

Cargo builds the crate with a non-LTO `release-lib` profile. The crate must be
built by the same rustc release as the Hew runtime, which a
`rust-toolchain.toml` in the crate pins; a mismatch is refused with
`E_NATIVE_TOOLCHAIN` before Cargo runs.

### Native prebuilts

Published Rust `staticlib` packages may provide native prebuilts beside their
source archives. `hew native build` packs the static library Cargo reports
and names that file in the archive's descriptor; a crate that builds no
`staticlib` has no prebuilt. `hew install` selects the host artifact and
`hew build` selects the requested native target. Compatibility requires the exact compiler
build version and runtime rustc release/host stamp. The Rust compiler host stamp
is independent of the selected native target, including for cross-built SDKs.
The package name, version,
source archive checksum and compatibility identity are inside the archive
covered by the ordinary publisher checksum signature. The registry countersigns
the source checksum, compatibility identity, archive checksum, publisher
signature and publication time using its existing signing key. Missing or
invalid countersignatures refuse installation. `hew.lock` records the
selected qualified archive checksum alongside the source package. Offline
linking verifies the cached archive against this pin.

Only an absent artifact record permits source compilation. Invalid signatures,
checksum or identity mismatches, damaged cached archives and failed downloads
of advertised artifacts refuse. Verified archives retain the existing immutable
cache generation through the final link and can be reused without registry
access. Native C/C++/assembly sources and system library requirements still
follow the fields below.

### C, C++ and assembly sources

| Field          | Meaning                                                                                                    |
| -------------- | ---------------------------------------------------------------------------------------------------------- |
| `sources`      | `.c` compiles as C; `.cc`, `.cpp`, `.cxx` as C++; `.s` as assembly; `.S` as assembly with C preprocessing. |
| `include-dirs` | Header search directories.                                                                                 |
| `defines`      | Preprocessor definitions, `NAME` or `NAME=VALUE`.                                                          |
| `cflags`       | Extra flags for C sources, such as `-std=c11` or `-Wall`.                                                  |
| `cxxflags`     | Extra flags for C++ sources, such as `-std=c++17`.                                                         |
| `asflags`      | Extra flags for assembly sources, using GNU driver spelling.                                               |

Source and include paths are relative to `hew.toml`, written with `/`, and stay
inside the package: a published package carries only what is below its
manifest.

Sources compile with the C driver Hew links with: `clang` when it is on `PATH`,
otherwise the system `cc`, or the driver `HEW_CC` names. Assembly uses explicit
`-x assembler` or `-x assembler-with-cpp`; C and C++ use their file extensions.
Every compile carries the target triple, the
build's optimization level (`-O0`, or `-O2` for `--release`), `-g` for debug
builds, and AddressSanitizer when the runtime is built with it. Flags in
`cflags`, `cxxflags` and `asflags` use the GNU driver spelling on every platform,
Windows included.

Assembly receives `include-dirs` and `asflags`, without C/C++-only flags.
Preprocessed assembly also receives `defines`. To preprocess an upstream lowercase `.s` file intentionally, set
`asflags = ["-x", "assembler-with-cpp"]`. Joined `-xassembler-with-cpp` also
works; `-x assembler` selects raw assembly, including for `.S`. Other language
overrides in `asflags` are refused. Assembly must match the selected target
and its assembler syntax; Hew does not translate instructions between CPUs.

C and C++ objects are cached under `target/native/` in the package. An object
is rebuilt when its source, a header it includes or any compile flag changes,
and a copy of
the package tree at another path builds its own objects. When the package
directory is read-only, objects are cached under `hew-native/` in the system
temporary directory instead. A compile error stops the build with
`E_NATIVE_COMPILE` and the compiler's own output.

Assembly is rebuilt on every Hew invocation: integrated assemblers do not
reliably report assembler `.include` dependencies. Preprocessed assembly
contributes its `#include` headers to `--emit-deps`; raw assembly contributes
the source itself. If an outer Make rule needs to track assembler `.include`
files, list those prerequisites explicitly or use `#include` in `.S` files.

A source listed twice in `sources`, in `[native]` or together with the matching
`[native.<os>]` table, is refused. Every other malformed `[native]` entry (a
wrong type, an unknown field, an unknown OS table, `crate` inside
`[native.linux]`) is refused with `E_INVALID_NATIVE` and the offending line. A
`hew.toml` without a `[native]` table is not read for native code, so a
workspace file or a dependency list beside a program never stops it building.

A program whose packages compile any C++ links the C++ standard library:
libstdc++ on Linux, libc++ on macOS and FreeBSD, and the MSVC STL on Windows.

### System libraries

| Field        | Meaning                                                                         |
| ------------ | ------------------------------------------------------------------------------- |
| `pkg-config` | Packages whose `--cflags` apply to `sources` and whose `--libs` join the link.  |
| `link-libs`  | Libraries by name (`"crypto"`), or library files by path (`"vendor/libfoo.a"`). |
| `lib-dirs`   | Library search directories: absolute, or relative to `hew.toml`.                |

`pkg-config` is the portable way to find a system library where the platform
provides it: Linux distributions, Homebrew on macOS and FreeBSD ports all ship
`.pc` files. Hew runs `pkg-config`, or the program `PKG_CONFIG` names. When it
cannot run, or does not know a package, the build stops with
`E_NATIVE_PKG_CONFIG`.

A `link-libs` name links as `-lNAME`: `libNAME.so` or `libNAME.a` on Linux and
FreeBSD, `libNAME.dylib` or `libNAME.a` on macOS, and `NAME.lib` on Windows. A
`link-libs` entry is a name, never a linker flag; write `"crypto"`, not
`"-lcrypto"`.

### Per-OS tables

`[native.linux]`, `[native.macos]`, `[native.freebsd]` and `[native.windows]`
take the C fields and the system-library fields. Each adds to the base
`[native]` table for builds targeting that operating system; none of them
replaces it. Windows rarely has `pkg-config`, so a portable package usually
finds its libraries with `pkg-config` in the base table and names them in
`[native.windows]`:

```toml
[native]
sources = ["native/tls.c"]

[native.linux]
pkg-config = ["openssl"]

[native.macos]
pkg-config = ["openssl"]

[native.freebsd]
pkg-config = ["openssl"]

[native.windows]
link-libs = ["libssl", "libcrypto", "ws2_32", "crypt32"]
lib-dirs = ["C:/Program Files/OpenSSL/lib"]
include-dirs = ["third_party/openssl/include"]
```

### Reserved symbol prefix

The `hew_` symbol prefix belongs to the Hew runtime. A package outside the
`hew.` namespace whose native code defines a global symbol starting with `hew_`
is refused with `E_RESERVED_NATIVE_SYMBOL`, and the diagnostic suggests the same
name under the package's own prefix:

```text
error[E_RESERVED_NATIVE_SYMBOL]: `native/listener.c` in package `meshcore.broker` defines
`hew_listener_now`, but the `hew_` symbol prefix is reserved for the Hew runtime
  help: rename it to `meshcore_broker_listener_now` here and in the `extern "C"` block that declares it
```

The check covers the compiled objects of C, C++ and assembly `sources` (functions, data,
thread-local variables and common symbols) and the symbols a `[native]` Rust
crate exports (`#[no_mangle] pub extern "C" fn hew_...`). Packages in the `hew.`
namespace are the runtime's own ecosystem and may define `hew_` symbols. A
library named in `link-libs` or `--link-lib` is not inspected.

Rename the definition and the `extern "C"` declaration together. A `static`
helper is local to its file and may use any name.

### Link order and `--link-lib`

The final link lists, in order: the program, the Hew runtime, every package's
Rust archive and C objects, each `--link-lib` value, then every package's
library directories and system libraries, then the runtime again.
`--link-lib` stays available for a one-off library or flag that no manifest
describes.

### Dependency files

`hew build --emit-deps FILE` writes a Makefile rule whose target is the built
binary and whose prerequisites are every Hew module the program compiled,
every owning package's `hew.toml` and `hew.lock`, and every native source,
header and Rust source its `[native]` code was built from. Standard-library
modules and system headers are left out. Each prerequisite also gets an empty
rule, so deleting a file does not break the next `make`. `hew run --emit-deps
FILE` writes the same rule with `FILE` itself as the target.

```make
build/broker: broker_main.hew
	hew build broker_main.hew --release -o $@ --emit-deps $@.d

-include build/broker.d
```

## Workspaces

A repository of several packages lists them in a root `hew.toml` that holds
`[workspace]` and no `[package]`:

```toml
[workspace]
members = ["dag", "db/*", "net/http"]
```

Each entry is a directory relative to the root, written with `/`. A `*`
segment matches exactly one directory level, skipping hidden directories.
Every literal entry and every match must hold a package `hew.toml`; otherwise
`E_WORKSPACE_MEMBER` names the entry. Nothing else is discovered, so
`.hew/packages`, `target/` and fixtures in nested checkouts never become
members. Two members with one name are refused with `E_WORKSPACE_MEMBER`, and
a `hew.toml` that declares both `[package]` and `[workspace]` is refused with
`E_WORKSPACE_MANIFEST`.

A package belongs to a workspace when the nearest ancestor `hew.toml` that
declares `[workspace]` lists it. Inside a workspace, a plain requirement on
another member resolves to that member's directory, for `hew install`,
`hew check` and `hew publish` alike. The requirement must admit the member's
version, or `E_WORKSPACE_REQUIREMENT` names both sides; it never falls back
to a registry copy. A dependency with an explicit `path` or `registry` keeps
that source. `hew.lock` records a member at a path relative to the package.
The manifest keeps its version requirement, so the published index entry
still says `"hew.db.sql" = "0.4.0"`.

`hew build`, `hew run` and the other package commands refuse a workspace
root; run them in a member.

## Publishing

`hew publish` publishes the package in the current directory.
`hew publish --workspace` publishes every member of the enclosing workspace.
Both run the same phases:

1. **Validate**, offline, every member before anything else: the required
   registry fields, an importable name, a semantic version, a `hew` range
   that admits this compiler, a `[native]` crate that builds a linkable
   library (read from `cargo metadata`; publishing never builds native
   code), and requirements on other members. Members are
   then ordered so each follows the members it depends on, the lowest name
   first among those ready; a cycle fails with `E_WORKSPACE_CYCLE`.
2. **Check and pack**, in that order: install each member's dependencies into
   its `.hew/packages` (members from their directories, other packages from
   the registry), run the frontend check, and pack the archive. Publishing
   writes neither `hew.lock` nor `.gitignore`. A member's own `hew.lock` is
   ignored by the check and left out of the archive: consumers resolve from
   the index entry, so a lock left behind by an earlier install can neither
   fail the publish nor change the archive.
3. **Query** the registry for every member. A version the registry already
   holds with the same archive, or with an archive that differs only in
   compression, is skipped. A version it holds with a different tree fails
   with `E_PUBLISH_VERSION_DRIFT`; every drifted member is reported and
   nothing uploads.
4. **Authenticate**, only when something will upload.
5. **Upload and read back**, one member at a time in order. The stored record
   must carry the uploaded checksum, key fingerprint and signature, and both
   the publisher signature and the registry counter-signature must verify.
   The served archive must hash to the checksum. A mismatch is
   `E_PUBLISH_READBACK`, and publishing stops there. A 409 from the upload
   goes back through the query, so a rerun after a partial failure converges.

`--dry-run` stops after the query. It loads no key, needs no credentials and
makes only anonymous reads. `--local` installs each package into the local
package cache instead of a registry.

### Credentials

`--auth credentials`, the default, uses the token `hew login` stored and the
key `hew key generate` wrote.

`--auth github-actions` is trusted publishing from a GitHub Actions workflow
with `permissions: id-token: write`. The publish process generates a signing
key in memory, requests an OIDC token whose audience is the registry's origin,
and exchanges the token and public key for a short-lived publish grant. The
grant must name the generated key, or publishing stops with
`E_AUTH_GRANT_KEY` before any upload. Nothing is read from or written to
`credentials.toml` or `keys/`. When the grant expires mid-run and the registry
answers 401, a fresh OIDC token is exchanged for the same key and the upload
is retried once.

```yaml
permissions:
  id-token: write
steps:
  - run: hew publish --workspace --auth github-actions
```

## Migrating a Makefile that compiles bridges by hand

A project that compiles C bridges with `$(CC)` and repeats them as `--link-lib`
values on every `hew build` line can declare them once instead. For a project
shaped like this:

```make
LINK = --link-lib build/listener_bridge.o --link-lib build/event_bridge.o \
       --link-lib -lcrypto --link-lib -l:libsodium.so.23

build/listener_bridge.o: listener_bridge.c event_bridge.h
	$(CC) -std=c11 -Wall -Wextra -Werror -g -O2 -c $< -o $@

build/hew-broker: broker_main.hew broker.hew hostlib/listener.hew build/listener_bridge.o
	$(HEW) build broker_main.hew --release -o $@ $(LINK)
```

1. Move each bridge beside the Hew module that declares its externs, and give
   that directory a manifest: `hostlib/native/listener_bridge.c`,
   `hostlib/native/event_bridge.c` and `hostlib/native/event_bridge.h`, with
   `hostlib/hew.toml` declaring them:

   ```toml
   [package]
   name = "hostlib"
   version = "0.1.0"
   edition = "2026"

   [native]
   sources = ["native/listener_bridge.c", "native/event_bridge.c"]
   include-dirs = ["native"]
   cflags = ["-std=c11", "-Wall", "-Wextra", "-Werror"]
   pkg-config = ["libcrypto", "libsodium"]
   ```

2. Rename every `hew_`-prefixed bridge symbol to the package's prefix, in the C
   source and the `extern "C"` block: `hew_listener_open` becomes
   `hostlib_listener_open`.

3. Drop the object rules and the `LINK` variable, and let `--emit-deps` keep the
   prerequisites current:

   ```make
   build/hew-broker:
   	$(HEW) build broker_main.hew --release -o $@ --emit-deps $@.d

   -include build/hew-broker.d
   ```

Every binary that imports `hostlib.listener` now builds and links the bridges
it needs, at the optimization level of that build. `-l:libsodium.so.23` becomes
the `libsodium` pkg-config package; where a specific file is required, name its
path in `link-libs` instead.
