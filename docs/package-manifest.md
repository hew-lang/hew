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

| Field                                                                                                              | Meaning                                                                                      |
| ------------------------------------------------------------------------------------------------------------------ | -------------------------------------------------------------------------------------------- |
| `name`                                                                                                             | Required. Dotted lowercase segments (`acme.http.router`). The last segment names the binary. |
| `version`                                                                                                          | Required. A semantic version.                                                                |
| `edition`                                                                                                          | The language edition, `"2026"`.                                                              |
| `main`                                                                                                             | The entry file relative to `hew.toml`; defaults to `main.hew`.                               |
| `description`, `authors`, `license`, `keywords`, `categories`, `homepage`, `repository`, `documentation`, `readme` | Registry metadata.                                                                           |
| `include`, `exclude`                                                                                               | Glob patterns that select the files `hew publish` packs.                                     |
| `hew`                                                                                                              | The minimum compiler version, such as `">=0.8.0"`.                                           |

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

## `[native]`

`[native]` declares the native code behind the package's `extern "C"`
functions. Whenever a program compiles a module of the package, directly or
through any depth of imports, `hew build` and `hew run` build that code and link
it. Neither the package nor its consumers pass `--link-lib`.

A section may declare a Rust crate, C and C++ sources, system libraries, or any
combination. An empty `[native]` is refused.

### Rust crate

| Field   | Meaning                                                               |
| ------- | --------------------------------------------------------------------- |
| `lib`   | The crate's `[lib]` name. Declaring it is what declares a Rust crate. |
| `crate` | The crate directory relative to `hew.toml`; defaults to `"."`.        |
| `kind`  | `"staticlib"` (default) or `"cdylib"`.                                |

Cargo builds the crate with a non-LTO `release-lib` profile. The crate must be
built by the same rustc release as the Hew runtime, which a
`rust-toolchain.toml` in the crate pins; a mismatch is refused with
`E_NATIVE_TOOLCHAIN` before Cargo runs.

### C and C++ sources

| Field          | Meaning                                                                 |
| -------------- | ----------------------------------------------------------------------- |
| `sources`      | `.c` files compile as C; `.cc`, `.cpp` and `.cxx` files compile as C++. |
| `include-dirs` | Header search directories.                                              |
| `defines`      | Preprocessor definitions, `NAME` or `NAME=VALUE`.                       |
| `cflags`       | Extra flags for C sources, such as `-std=c11` or `-Wall`.               |
| `cxxflags`     | Extra flags for C++ sources, such as `-std=c++17`.                      |

Source and include paths are relative to `hew.toml`, written with `/`, and stay
inside the package: a published package carries only what is below its
manifest.

Sources compile with the C driver Hew links with: `clang` when it is on `PATH`,
otherwise the system `cc`, or the driver `HEW_CC` names. The driver chooses the
language from the extension. Every compile carries the target triple, the
build's optimization level (`-O0`, or `-O2` for `--release`), `-g` for debug
builds, and AddressSanitizer when the runtime is built with it. Flags in
`cflags` and `cxxflags` use the GNU driver spelling on every platform,
Windows included.

Objects are cached under `target/native/` in the package. An object is rebuilt
when its source, a header it includes or any compile flag changes, and a copy of
the package tree at another path builds its own objects. When the package
directory is read-only, objects are cached under `hew-native/` in the system
temporary directory instead. A compile error stops the build with
`E_NATIVE_COMPILE` and the compiler's own output.

A source listed twice in `sources`, in `[native]` or together with the matching
`[native.<os>]` table, is refused. Every other malformed `[native]` entry (a
wrong type, an unknown field, an unknown OS table, `lib` inside
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

The check covers the compiled objects of C and C++ `sources` (functions, data,
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
