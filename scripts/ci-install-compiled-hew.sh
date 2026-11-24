#!/usr/bin/env bash
# Install the single PR compiler build where Make's source gates expect it.
set -euo pipefail

if [[ $# -ne 1 ]]; then
    echo "usage: $0 <compiled-hew-linux.tar.gz>" >&2
    exit 2
fi

archive="$1"
stage="$(mktemp -d)"
trap 'rm -rf "$stage"' EXIT
tar -xzf "$archive" -C "$stage"
mkdir -p target/debug target/wasm32-wasip1/debug
install -m 755 "$stage/compiled-hew/debug/hew" target/debug/hew
if [[ -f "$stage/compiled-hew/debug/xtask" ]]; then
    install -m 755 "$stage/compiled-hew/debug/xtask" target/debug/xtask
fi
install -m 644 "$stage/compiled-hew/debug/libhew_native.a" target/debug/libhew_native.a
install -m 755 "$stage/compiled-hew/debug/libhew_native.so" target/debug/libhew_native.so
# Keep the versioned link beside the shared runtime.
cp -P "$stage"/compiled-hew/debug/libhew.so.* target/debug/
install -m 644 "$stage/compiled-hew/debug/libhew_runtime.a" target/debug/libhew_runtime.a
install -m 644 "$stage/compiled-hew/wasm32-wasip1/debug/libhew_runtime.a" target/wasm32-wasip1/debug/libhew_runtime.a
install -m 644 "$stage/compiled-hew/wasm32-wasip1/debug/libhew_std.a" target/wasm32-wasip1/debug/libhew_std.a
