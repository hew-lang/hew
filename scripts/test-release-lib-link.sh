#!/usr/bin/env bash
# test-release-lib-link.sh — prove a shipped libhew archive links a real Rust
# native package through the public `hew build --link-lib` interface.
#
# Archive-member names are an implementation detail of rustc/ar and vary by
# toolchain. The release requirement is behavioural: a consumer staticlib
# built by the same Rust toolchain must link with libhew.a and run.

set -euo pipefail

usage() {
    echo "usage: $0 --hew <release-hew> --archive <libhew.a>" >&2
    exit 2
}

HEW=""
ARCHIVE=""
while [[ $# -gt 0 ]]; do
    case "$1" in
        --hew) HEW="${2:-}"; shift 2 ;;
        --archive) ARCHIVE="${2:-}"; shift 2 ;;
        *) usage ;;
    esac
done

[[ -x "$HEW" ]] || { echo "error: release hew binary is not executable: $HEW" >&2; exit 1; }
[[ -f "$ARCHIVE" ]] || { echo "error: libhew archive is missing: $ARCHIVE" >&2; exit 1; }
command -v rustc >/dev/null || { echo "error: rustc is required for native-package link validation" >&2; exit 1; }

WORK_DIR=$(mktemp -d "${TMPDIR:-/tmp}/hew-release-link-XXXXXX")
trap 'rm -rf "$WORK_DIR"' EXIT

mkdir -p "$WORK_DIR/release/bin" "$WORK_DIR/release/lib"
cp "$HEW" "$WORK_DIR/release/bin/hew"
cp "$ARCHIVE" "$WORK_DIR/release/lib/libhew.a"

cat >"$WORK_DIR/native.rs" <<'RS'
#[no_mangle]
pub extern "C" fn release_link_probe() -> i64 {
    // Force this consumer archive to carry the Rust standard library. This is
    // the ecosystem shape that used to expose libstd/personality collisions.
    String::from("release-link-ok").len() as i64
}
RS
rustc --crate-type staticlib --crate-name hew_release_link_probe --edition 2021 \
    -C panic=abort -C codegen-units=1 -o "$WORK_DIR/librelease_link_probe.a" \
    "$WORK_DIR/native.rs"

cat >"$WORK_DIR/main.hew" <<'HEW'
extern "C" { fn release_link_probe() -> i64; }

fn main() {
    let result: i64 = unsafe { release_link_probe() };
    if result != 15 {
        panic("native release link probe returned an unexpected value");
    }
    println("release-native-link-ok");
}
HEW

"$WORK_DIR/release/bin/hew" build "$WORK_DIR/main.hew" \
    --link-lib "$WORK_DIR/librelease_link_probe.a" -o "$WORK_DIR/release-link-probe"
OUTPUT=$("$WORK_DIR/release-link-probe")
if [[ "$OUTPUT" != *"release-native-link-ok"* ]]; then
    echo "error: native release-link probe output was: $OUTPUT" >&2
    exit 1
fi

echo "PASS: release libhew.a linked and ran a Rust native staticlib consumer"
