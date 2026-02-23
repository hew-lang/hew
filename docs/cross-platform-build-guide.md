# Cross-Platform Build Guide

Building hew-codegen (the C++ MLIR code generator) for release requires careful
setup because it statically links against LLVM 21 and MLIR. This document
captures the platform-specific issues and their solutions.

## Overview

The release build produces three binary artifacts per platform:

- `hew` — compiler driver (Rust)
- `adze` — package manager (Rust)
- `hew-codegen` — MLIR code generator (C++, statically linked against LLVM/MLIR)

The Rust components build straightforwardly with `cargo build --release`. The
C++ component requires LLVM 21 development libraries and a compatible compiler
toolchain, and this is where the platform-specific complexity lives.

## Linux x86_64

**Tested on:** Ubuntu 24.04 (GitHub Actions `ubuntu-24.04`)

### Prerequisites

```bash
# LLVM 21 from apt.llvm.org
sudo mkdir -p /etc/apt/keyrings
wget -qO- https://apt.llvm.org/llvm-snapshot.gpg.key \
  | sudo tee /etc/apt/keyrings/llvm.asc >/dev/null
echo "deb [signed-by=/etc/apt/keyrings/llvm.asc] \
  http://apt.llvm.org/noble/ llvm-toolchain-noble-21 main" \
  | sudo tee /etc/apt/sources.list.d/llvm.list >/dev/null
sudo apt-get update
sudo apt-get install -y cmake ninja-build \
  llvm-21-dev libmlir-21-dev mlir-21-tools clang-21
```

### Build

```bash
cd hew-codegen
cmake -B build -G Ninja \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_C_COMPILER=clang-21 \
  -DCMAKE_CXX_COMPILER=clang++-21 \
  -DHEW_STATIC_LINK=ON \
  -DLLVM_DIR=/usr/lib/llvm-21/lib/cmake/llvm \
  -DMLIR_DIR=/usr/lib/llvm-21/lib/cmake/mlir
cmake --build build --config Release
```

### Critical: use clang, not GCC

The `HandleLLVMOptions` CMake module (included from LLVM's cmake config)
propagates Clang-specific warning flags like `-Wweak-vtables` to all consumers.
GCC does not recognize these flags and will error. Always use `clang-21` /
`clang++-21` as the C/C++ compiler, never `gcc`/`g++`.

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
  http://apt.llvm.org/bookworm/ llvm-toolchain-bookworm-21 main
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

**Tested on:** macOS x86_64 (Homebrew LLVM 21.1.8), GitHub Actions `macos-13`
(x86_64) and `macos-14` (aarch64)

macOS is the most complex platform due to three interacting toolchain
components: the compiler, the linker, and the C++ standard library.

### Prerequisites

```bash
brew install llvm@21 ninja cmake
```

Homebrew LLVM is keg-only (not symlinked into `/usr/local/bin`). You need the
prefix path:

```bash
LLVM_PREFIX="$(brew --prefix llvm@21)"
# x86_64: /usr/local/opt/llvm
# aarch64: /opt/homebrew/opt/llvm
```

### Build

```bash
LLVM_PREFIX="$(brew --prefix llvm@21)"
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
archive. These MLIR objects were compiled by Homebrew's LLVM 21 and contain
LLVM 21 bitcode (from thin LTO). Apple's system linker uses its own LTO
implementation (based on LLVM 15-17 depending on Xcode version) and cannot
parse LLVM 21 bitcode:

```
ld: could not parse bitcode object file:
  'Unknown attribute kind (102) (Producer: 'LLVM21.1.8'
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

**Status:** Not yet supported for release builds. The runtime uses POSIX
`mmap`/`munmap` in `arena.rs`, `coro.rs`, and `signal.rs`, which are not
available on Windows. The release workflow has `continue-on-error: true` for
Windows builds.

Fixing Windows support requires replacing mmap calls with `VirtualAlloc` /
`VirtualFree` or gating the affected modules behind `#[cfg(unix)]`.

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

| Platform      | Compiler                     | Sysroot                    | Linker flags                              | Extra apt packages       |
| ------------- | ---------------------------- | -------------------------- | ----------------------------------------- | ------------------------ |
| Linux x86_64  | `clang-21`                   | n/a                        | n/a                                       | (standard)               |
| Linux aarch64 | `clang-21`                   | n/a                        | n/a                                       | `libzstd-dev zlib1g-dev` |
| macOS x86_64  | `${LLVM_PREFIX}/bin/clang++` | `$(xcrun --show-sdk-path)` | `-L${LLVM_PREFIX}/lib/c++ -Wl,-rpath,...` | n/a                      |
| macOS aarch64 | `${LLVM_PREFIX}/bin/clang++` | `$(xcrun --show-sdk-path)` | `-L${LLVM_PREFIX}/lib/c++ -Wl,-rpath,...` | n/a                      |
| Windows       | N/A                          | N/A                        | N/A                                       | (not yet supported)      |
