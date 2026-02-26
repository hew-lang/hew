#!/bin/sh
# installers/alpine/build-alpine-codegen.sh
#
# Build hew-codegen natively on Alpine (musl) with LLVM+MLIR from source.
# This produces a musl-native binary that doesn't need gcompat.
#
# Usage:
#   docker run --rm -v $PWD:/src -v /tmp/alpine-out:/output alpine:edge \
#     sh /src/installers/alpine/build-alpine-codegen.sh
#
# Or via build-packages.sh which calls this automatically.
#
# The first build takes ~20-30 minutes (LLVM compile). Subsequent builds
# with a cached LLVM layer take ~2 minutes (hew-codegen only).

set -eu

LLVM_VERSION="${LLVM_VERSION:-21.1.8}"
LLVM_MAJOR="${LLVM_VERSION%%.*}"
SRC_DIR="${SRC_DIR:-/src}"
BUILD_DIR="${BUILD_DIR:-/tmp/hew-alpine-build}"
OUTPUT_DIR="${OUTPUT_DIR:-/output}"
NPROC=$(nproc)

info() { printf '\033[1;34m==> %s\033[0m\n' "$1"; }
die() {
    printf '\033[1;31mERROR: %s\033[0m\n' "$1" >&2
    exit 1
}

# ── Install build dependencies ───────────────────────────────────────────────
info "Installing build dependencies"
apk add --no-cache \
    cmake samurai build-base python3 clang \
    zlib-dev zlib-static zstd-dev zstd-static \
    libffi-dev linux-headers \
    git 2>&1 | tail -3

# ── Check for pre-built LLVM cache ───────────────────────────────────────────
LLVM_INSTALL="/opt/llvm-${LLVM_MAJOR}"
LLVM_SRC="/tmp/llvm-project"

if [ -d "${LLVM_INSTALL}/lib/cmake/mlir" ]; then
    info "Using cached LLVM+MLIR from ${LLVM_INSTALL}"
else
    # ── Download LLVM source ─────────────────────────────────────────────────
    info "Downloading LLVM ${LLVM_VERSION} source..."
    LLVM_TAG="llvmorg-${LLVM_VERSION}"

    # Shallow clone only the directories we need
    git clone --depth 1 --branch "${LLVM_TAG}" \
        --filter=blob:none --sparse \
        https://github.com/llvm/llvm-project.git "${LLVM_SRC}" 2>&1 | tail -3

    cd "${LLVM_SRC}"
    git sparse-checkout set llvm mlir cmake third-party 2>&1

    # ── Build LLVM + MLIR ────────────────────────────────────────────────────
    # Limit parallel jobs for LLVM build to avoid OOM
    LLVM_JOBS=$((NPROC > 4 ? 4 : NPROC))

    info "Building LLVM + MLIR (this takes ~20 min)..."
    cmake -S llvm -B build -G Ninja \
        -DCMAKE_BUILD_TYPE=Release \
        -DCMAKE_INSTALL_PREFIX="${LLVM_INSTALL}" \
        -DLLVM_ENABLE_PROJECTS="mlir" \
        -DLLVM_TARGETS_TO_BUILD="X86;AArch64" \
        -DLLVM_BUILD_TOOLS=OFF \
        -DLLVM_BUILD_UTILS=OFF \
        -DLLVM_INCLUDE_TESTS=OFF \
        -DLLVM_INCLUDE_BENCHMARKS=OFF \
        -DLLVM_INCLUDE_EXAMPLES=OFF \
        -DLLVM_INCLUDE_DOCS=OFF \
        -DLLVM_ENABLE_BINDINGS=OFF \
        -DLLVM_ENABLE_ZLIB=ON \
        -DLLVM_ENABLE_ZSTD=ON \
        -DLLVM_ENABLE_ASSERTIONS=OFF \
        -DLLVM_BUILD_LLVM_DYLIB=OFF \
        -DLLVM_LINK_LLVM_DYLIB=OFF \
        -DMLIR_BUILD_MLIR_C_DYLIB=OFF \
        -DMLIR_ENABLE_BINDINGS_PYTHON=OFF \
        -DBUILD_SHARED_LIBS=OFF \
        2>&1 | tail -5

    ninja -C build -j${LLVM_JOBS} 2>&1 | tail -5
    ninja -C build install 2>&1 | tail -3

    # mlir-tblgen is built during the MLIR build (used by tablegen) but not
    # installed when LLVM_BUILD_TOOLS=OFF.  Copy all built tools manually so
    # hew-codegen's cmake can find them via the MLIR/LLVM cmake config.
    mkdir -p "${LLVM_INSTALL}/bin"
    for tool in build/bin/*; do
        [ -f "$tool" ] && [ -x "$tool" ] &&
            install -Dm755 "$tool" "${LLVM_INSTALL}/bin/$(basename "$tool")"
    done

    info "LLVM+MLIR installed to ${LLVM_INSTALL}"
    cd /
    rm -rf "${LLVM_SRC}"
fi

# ── Build hew-codegen ────────────────────────────────────────────────────────
info "Building hew-codegen"
mkdir -p "${BUILD_DIR}"
cp -r "${SRC_DIR}/hew-codegen" /tmp/hew-codegen-src

cmake -S /tmp/hew-codegen-src -B "${BUILD_DIR}" -G Ninja \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_C_COMPILER=clang \
    -DCMAKE_CXX_COMPILER=clang++ \
    -DCMAKE_CXX_FLAGS="-fno-rtti" \
    -DHEW_STATIC_LINK=ON \
    -DLLVM_DIR="${LLVM_INSTALL}/lib/cmake/llvm" \
    -DMLIR_DIR="${LLVM_INSTALL}/lib/cmake/mlir" \
    2>&1 | tail -5

ninja -C "${BUILD_DIR}" -j${NPROC} 2>&1 | tail -5

# ── Verify the binary ───────────────────────────────────────────────────────
BINARY="${BUILD_DIR}/src/hew-codegen"
if [ ! -f "${BINARY}" ]; then
    die "hew-codegen binary not found at ${BINARY}"
fi

info "Binary info:"
file "${BINARY}"
ldd "${BINARY}" 2>&1 || true

# ── Strip and copy output ───────────────────────────────────────────────────
strip "${BINARY}"
mkdir -p "${OUTPUT_DIR}"
cp "${BINARY}" "${OUTPUT_DIR}/hew-codegen"

info "hew-codegen copied to ${OUTPUT_DIR}/hew-codegen"
ls -lh "${OUTPUT_DIR}/hew-codegen"
info "Done!"
