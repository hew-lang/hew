# Cross-Platform Build Guide

The Hew compiler links against LLVM 22 via the `hew-codegen-rs` crate
(inkwell → llvm-sys-221). The previous C++/MLIR codegen subtree
(`hew-codegen/`) was retired; the Rust IR ladder is the sole compiler
backend. The platform-specific complexity captured below is now about
provisioning the LLVM development libraries that `llvm-sys` builds against.

## Overview

The release build produces two binary artifacts per platform:

- `hew` — compiler driver (Rust), linking LLVM 22 statically through
  `hew-codegen-rs` (inkwell)
- `adze` — package manager (Rust)

Both Rust binaries build with `cargo build --release` once LLVM 22
libraries and headers are available; set `LLVM_PREFIX` (or
`LLVM_SYS_221_PREFIX`) to point at the install.

Use `make` / `make release` from the repository root rather than invoking
`cargo` directly — the Makefile wires up symlinks and per-target lib
layouts correctly.

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
make           # debug build
make release   # release build
```

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

## Windows

**Status:** Supported for release builds when LLVM 22 is installed locally
and `LLVM_PREFIX` is set. The runtime's low-level memory paths have Windows
implementations (`VirtualAlloc` / `VirtualFree`). Windows is fail-soft /
Tier 2 in `release.yml` today: the tag-release Windows job remains
`continue-on-error: true` until this validation path has proven itself
through a full release cycle.

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

cargo build -p hew-cli -p adze-cli -p hew-lsp --release
cargo build -p hew-lib --release
```

`LLVM_PREFIX` (or `LLVM_SYS_221_PREFIX`) must point at an LLVM 22 install
so `llvm-sys` can locate `llvm-config` and the static libraries.

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
