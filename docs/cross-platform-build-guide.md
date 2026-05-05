# Cross-Platform Build Guide

The C++ MLIR code generator (`hew-codegen`) is **embedded inside the `hew`
binary** via `build.rs`. It is not a separately shipped binary. The CMake
build in the `hew-codegen/` directory exists only to produce the C++ object
library that `build.rs` links into `hew`, and to build the C++ unit-test and
E2E ctest harness. This document captures the platform-specific issues
involved in building LLVM/MLIR and linking them into `hew`.

## Overview

The release build produces two binary artifacts per platform:

- `hew` — compiler driver (Rust), with the MLIR/LLVM codegen backend
  statically embedded (when built with `HEW_EMBED_STATIC=1` or `LLVM_PREFIX`
  set)
- `adze` — package manager (Rust)

Both Rust binaries build straightforwardly with `cargo build --release` once
LLVM/MLIR libraries are available. The embedded C++ codegen requires LLVM 22
development libraries and a compatible compiler toolchain, and this is where
the platform-specific complexity lives.

Use `make` / `make release` from the repository root rather than invoking
`cargo` or `cmake` directly — the Makefile wires up the detection and
embedding steps correctly across platforms.

## Linux x86_64

**Tested on:** Ubuntu 24.04 (GitHub Actions `ubuntu-24.04`)

### Prerequisites

```bash
# LLVM 22 from apt.llvm.org
sudo mkdir -p /etc/apt/keyrings
wget -qO- https://apt.llvm.org/llvm-snapshot.gpg.key \
  | sudo tee /etc/apt/keyrings/llvm.asc >/dev/null
echo "deb [signed-by=/etc/apt/keyrings/llvm.asc] \
  http://apt.llvm.org/noble/ llvm-toolchain-noble-22 main" \
  | sudo tee /etc/apt/sources.list.d/llvm.list >/dev/null
sudo apt-get update
sudo apt-get install -y cmake ninja-build \
  llvm-22-dev libmlir-22-dev mlir-22-tools clang-22
```

### Build

Use `make` from the repository root. It auto-detects LLVM/MLIR paths, invokes
CMake to build the C++ object library, and then triggers `cargo build` which
embeds that library into `hew`:

```bash
make           # debug build with embedded codegen
make release   # release build (sets HEW_EMBED_STATIC=1 automatically)
```

To drive CMake manually (e.g. for C++ unit tests only):

```bash
cd hew-codegen
cmake -B build -G Ninja \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_C_COMPILER=clang-22 \
  -DCMAKE_CXX_COMPILER=clang++-22 \
  -DHEW_STATIC_LINK=ON \
  -DLLVM_DIR=/usr/lib/llvm-22/lib/cmake/llvm \
  -DMLIR_DIR=/usr/lib/llvm-22/lib/cmake/mlir
cmake --build build --config Release
```

### Critical: use clang, not GCC

The `HandleLLVMOptions` CMake module (included from LLVM's cmake config)
propagates Clang-specific warning flags like `-Wweak-vtables` to all consumers.
GCC does not recognize these flags and will error. Always use `clang-22` /
`clang++-22` as the C/C++ compiler, never `gcc`/`g++`.

## Linux aarch64

**Tested on:** Raspberry Pi 5 (Debian 12 bookworm), GitHub Actions `ubuntu-24.04-arm`

### Prerequisites

Same as x86_64, plus ensure these libraries are installed (some minimal
installations miss them):

```bash
sudo apt-get install -y libzstd-dev zlib1g-dev
```

For Debian bookworm specifically, use `bookworm` instead of `noble` in the
apt.llvm.org repository URL:

```
deb [signed-by=/etc/apt/keyrings/llvm.asc] \
  http://apt.llvm.org/bookworm/ llvm-toolchain-bookworm-22 main
```

### Build

Identical to Linux x86_64.

### Critical: `c_char` is `u8` on aarch64

On x86_64 Linux, Rust's `std::ffi::c_char` is `i8`. On aarch64 Linux, it is
`u8`. Any Rust FFI code that uses `*mut i8` or `*const i8` where it means
"pointer to C char" will fail to compile on aarch64.

**Always use `std::ffi::c_char`** instead of `i8` in FFI signatures:

```rust
// Wrong - breaks on aarch64:
pub extern "C" fn my_func(s: *const i8) -> *mut i8 { ... }

// Correct - works everywhere:
use std::ffi::c_char;
pub extern "C" fn my_func(s: *const c_char) -> *mut c_char { ... }
```

This applies to all crates that define `extern "C"` functions: `hew-runtime`,
`hew-cabi`, and any stdlib FFI crates.

Similarly, when casting `libc::malloc` results or building C strings, use
`.cast::<c_char>()` instead of `.cast::<i8>()`.

## macOS (x86_64 and aarch64)

**Tested on:** macOS x86_64 (Homebrew LLVM 22.1.8), GitHub Actions `macos-13`
(x86_64) and `macos-14` (aarch64)

macOS is the most complex platform due to three interacting toolchain
components: the compiler, the linker, and the C++ standard library.

### Prerequisites

```bash
brew install llvm ninja cmake
```

Homebrew LLVM is keg-only (not symlinked into `/usr/local/bin`). You need the
prefix path:

```bash
LLVM_PREFIX="$(brew --prefix llvm)"
# x86_64: /usr/local/opt/llvm
# aarch64: /opt/homebrew/opt/llvm
```

### Build

Use `make` from the repository root with `LLVM_PREFIX` set:

```bash
LLVM_PREFIX="$(brew --prefix llvm)" make
LLVM_PREFIX="$(brew --prefix llvm)" make release
```

To drive CMake manually (e.g. for C++ unit tests only):

```bash
LLVM_PREFIX="$(brew --prefix llvm)"
cd hew-codegen
cmake -B build -G Ninja \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_C_COMPILER="${LLVM_PREFIX}/bin/clang" \
  -DCMAKE_CXX_COMPILER="${LLVM_PREFIX}/bin/clang++" \
  -DCMAKE_OSX_SYSROOT="$(xcrun --show-sdk-path)" \
  -DCMAKE_EXE_LINKER_FLAGS="-L${LLVM_PREFIX}/lib/c++ -Wl,-rpath,${LLVM_PREFIX}/lib/c++" \
  -DHEW_STATIC_LINK=ON \
  -DLLVM_DIR="${LLVM_PREFIX}/lib/cmake/llvm" \
  -DMLIR_DIR="${LLVM_PREFIX}/lib/cmake/mlir"
cmake --build build --config Release
```

### Why each flag is required

There are three flags beyond the standard LLVM/MLIR paths that are all
required. Removing any one of them causes a different build failure. Here is
what each solves:

**1. `CMAKE_C/CXX_COMPILER` = brew's clang (not Apple Clang)**

When `HEW_STATIC_LINK=ON`, CMake merges MLIR static library objects into our
archive. These MLIR objects were compiled by Homebrew's LLVM 22 and contain
LLVM 22 bitcode (from thin LTO). Apple's system linker uses its own LTO
implementation (based on LLVM 15-17 depending on Xcode version) and cannot
parse LLVM 22 bitcode:

```
ld: could not parse bitcode object file:
  'Unknown attribute kind (102) (Producer: 'LLVM22.1.0'
   Reader: 'LLVM APPLE_1_1700.6.3.2_0')'
```

Using brew's clang as the compiler means brew's linker toolchain handles the
bitcode, which is compatible with the MLIR objects.

**2. `CMAKE_OSX_SYSROOT` = Apple SDK path**

Brew's clang ships its own libc++ headers at
`${LLVM_PREFIX}/include/c++/v1/`. These headers define `size_t` as
`std::size_t` (with C++ namespacing). macOS SDK system headers (from Xcode)
expect plain `size_t` without the namespace. This causes compilation errors in
system headers like `malloc/_malloc_type.h`:

```
error: unknown type name 'size_t'; did you mean 'std::size_t'?
```

Setting `CMAKE_OSX_SYSROOT` tells brew's clang to resolve system headers from
Apple's SDK, fixing the namespace conflict.

**3. `CMAKE_EXE_LINKER_FLAGS` = brew's libc++ path**

With brew's clang and Apple's SDK sysroot, the compiler uses brew's libc++
headers (e.g. `std::__1::` ABI namespace) but the linker needs to find the
matching libc++ shared library. Without the explicit library path, the linker
finds Apple's system libc++ which has a different ABI:

```
ld: symbol(s) not found for architecture x86_64
  std::__1::__hash_table<...>::__emplace_unique_key_args<...>
```

The `-L` flag provides the library search path, and `-Wl,-rpath` embeds the
runtime search path so the binary can find libc++ when executed.

### Strip flag

On macOS, the `-s` linker flag is deprecated by Apple's `ld64`. The
CMakeLists.txt uses a post-build `strip` command instead:

```cmake
if(APPLE)
  add_custom_command(TARGET hew-codegen POST_BUILD
    COMMAND strip $<TARGET_FILE:hew-codegen>)
endif()
```

## Windows

**Status:** Supported for release builds when LLVM/MLIR 22 is installed locally
and the build uses the embedded-codegen environment (`LLVM_PREFIX` +
`HEW_EMBED_STATIC=1`). The runtime's low-level memory paths now have Windows
implementations (`VirtualAlloc` / `VirtualFree`), so the old
`mmap`/`munmap`-only limitation no longer applies. However, Windows is still
fail-soft / Tier 2 in `release.yml` today: the tag-release Windows job remains
`continue-on-error: true` until this validation path has proven itself through a
full release cycle.

### Prerequisites

The release workflow provisions LLVM/MLIR 22 into `C:\llvm-22` and builds with
MSVC's `cl`. The local Windows validator in `scripts/pre-release-validate.sh`
expects the same layout by default.

One-time bootstrap on the Windows host:

```powershell
choco install ninja cmake -y

git clone --depth 1 --branch llvmorg-22.1.0 `
  --filter=blob:none --sparse `
  https://github.com/llvm/llvm-project.git C:\llvm-src
Push-Location C:\llvm-src
git sparse-checkout set clang llvm mlir cmake third-party
Pop-Location

cmake -S C:\llvm-src\llvm -B C:\llvm-build -G Ninja `
  -DCMAKE_BUILD_TYPE=Release `
  -DCMAKE_INSTALL_PREFIX="C:\llvm-22" `
  -DCMAKE_C_COMPILER=cl `
  -DCMAKE_CXX_COMPILER=cl `
  -DLLVM_ENABLE_PROJECTS="clang;mlir" `
  -DLLVM_TARGETS_TO_BUILD="X86;AArch64" `
  -DLLVM_BUILD_TOOLS=ON `
  -DLLVM_BUILD_UTILS=ON `
  -DLLVM_INSTALL_UTILS=ON `
  -DLLVM_INCLUDE_TESTS=OFF `
  -DLLVM_INCLUDE_BENCHMARKS=OFF `
  -DLLVM_INCLUDE_EXAMPLES=OFF `
  -DLLVM_INCLUDE_DOCS=OFF `
  -DLLVM_ENABLE_BINDINGS=OFF `
  -DLLVM_ENABLE_ZLIB=OFF `
  -DLLVM_ENABLE_ZSTD=OFF `
  -DLLVM_ENABLE_DIA_SDK=OFF `
  -DLLVM_ENABLE_ASSERTIONS=OFF `
  -DLLVM_BUILD_LLVM_DYLIB=OFF `
  -DLLVM_LINK_LLVM_DYLIB=OFF `
  -DMLIR_BUILD_MLIR_C_DYLIB=OFF `
  -DMLIR_ENABLE_BINDINGS_PYTHON=OFF `
  -DBUILD_SHARED_LIBS=OFF

cmake --build C:\llvm-build --config Release
cmake --install C:\llvm-build --config Release

Test-Path 'C:\llvm-22\lib\cmake\mlir\MLIRConfig.cmake'
Test-Path 'C:\llvm-22\bin\clang.exe'
```

If the host cannot use MSVC, override the validator/compiler environment with
`HEW_WINDOWS_CC` / `HEW_WINDOWS_CXX` (for example `clang-cl`) and point
`HEW_WINDOWS_LLVM_PREFIX` at the matching install root.

### Build

Use a Developer PowerShell (or equivalent environment where your chosen
compiler is on `PATH`):

```powershell
$env:LLVM_PREFIX = 'C:\llvm-22'
$env:Path = 'C:\llvm-22\bin;' + $env:Path
$env:HEW_EMBED_STATIC = '1'
$env:CC = 'cl'
$env:CXX = 'cl'

cargo build -p hew-cli -p adze-cli -p hew-lsp --release
cargo build -p hew-lib --release
```

The important invariant is fail-closed embedded codegen: do not validate a
Windows release build without `LLVM_PREFIX` and `HEW_EMBED_STATIC=1`, or
`hew-cli/build.rs` may legitimately fall back to a frontend-only binary.

### Smoke test

```powershell
[System.IO.File]::WriteAllText(
    (Join-Path (Get-Location) '_smoke.hew'),
    'fn main() { println("smoke-ok") }',
    [System.Text.UTF8Encoding]::new($false)
)
.\target\release\hew.exe .\_smoke.hew -o .\_smoke.exe
.\_smoke.exe
Remove-Item -Force .\_smoke.hew, .\_smoke.exe
```

Expect `smoke-ok` on stdout. This verifies that the built `hew.exe` can still
compile and run a program with embedded codegen enabled, not just print
`--version`.

## Duplicate Symbol Errors When Linking stdlib Packages

When linking programs that use stdlib packages (e.g. `std::encoding::json`),
both `libhew_runtime.a` and `libhew_std_*.a` contain symbols from the shared
`hew-cabi` dependency. Cargo bakes all dependencies into each staticlib,
causing duplicate symbol errors at link time.

The linker is configured to tolerate this:

- **Linux:** `--allow-multiple-definition` (passed via `-Wl,` when stdlib
  packages are linked)
- **macOS:** `-multiply_defined,suppress` (passed via `-Wl,` when stdlib
  packages are linked)

This is handled automatically in `hew-cli/src/link.rs` when `extra_libs` is
non-empty.

## Quick Reference

The table below covers the flags needed when invoking CMake directly (e.g. to
run C++ unit tests). For a full build of `hew`, prefer `make` / `make release`
which handles all of these automatically.

| Platform      | Compiler                     | Sysroot                    | Linker flags                              | Extra apt packages       |
| ------------- | ---------------------------- | -------------------------- | ----------------------------------------- | ------------------------ |
| Linux x86_64  | `clang-22`                   | n/a                        | n/a                                       | (standard)               |
| Linux aarch64 | `clang-22`                   | n/a                        | n/a                                       | `libzstd-dev zlib1g-dev` |
| macOS x86_64  | `${LLVM_PREFIX}/bin/clang++` | `$(xcrun --show-sdk-path)` | `-L${LLVM_PREFIX}/lib/c++ -Wl,-rpath,...` | n/a                      |
| macOS aarch64 | `${LLVM_PREFIX}/bin/clang++` | `$(xcrun --show-sdk-path)` | `-L${LLVM_PREFIX}/lib/c++ -Wl,-rpath,...` | n/a                      |
| Windows       | `cl` (or override to `clang-cl`) | n/a                    | n/a                                       | `cmake`, `ninja`; LLVM/MLIR 22 under `C:\llvm-22` |
