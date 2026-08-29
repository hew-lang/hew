#!/bin/sh
# Assemble the common contents of a Hew release archive.
#
# Platform jobs own stripping, signing, archive formats, and smoke tests. This
# script owns the tree they all ship and refuses to publish a partial stage.
set -eu

usage() {
    cat >&2 <<'EOF'
usage: scripts/stage-release-package.sh \
  --source-dir DIR --bin-dir DIR [--bin-suffix SUFFIX] \
  --native-lib FILE --native-triple TRIPLE --native-lib-name NAME \
  --wasi-lib-dir DIR --destination DIR --completion-shells "SHELL ..."
EOF
}

die() {
    printf 'stage-release-package: %s\n' "$*" >&2
    exit 1
}

source_dir=
bin_dir=
bin_suffix=
native_lib=
native_triple=
native_lib_name=
wasi_lib_dir=
destination=
completion_shells=

while [ "$#" -gt 0 ]; do
    option=$1
    case "$option" in
    --source-dir | --bin-dir | --bin-suffix | --native-lib | \
        --native-triple | --native-lib-name | --wasi-lib-dir | \
        --destination | --completion-shells)
        [ "$#" -ge 2 ] || die "$option requires a value"
        value=$2
        shift 2
        ;;
    --help | -h)
        usage
        exit 0
        ;;
    *)
        usage
        die "unknown option: $option"
        ;;
    esac

    case "$option" in
    --source-dir) source_dir=$value ;;
    --bin-dir) bin_dir=$value ;;
    --bin-suffix) bin_suffix=$value ;;
    --native-lib) native_lib=$value ;;
    --native-triple) native_triple=$value ;;
    --native-lib-name) native_lib_name=$value ;;
    --wasi-lib-dir) wasi_lib_dir=$value ;;
    --destination) destination=$value ;;
    --completion-shells) completion_shells=$value ;;
    esac
done

[ -n "$source_dir" ] || die "--source-dir must not be empty"
[ -n "$bin_dir" ] || die "--bin-dir must not be empty"
[ -n "$native_lib" ] || die "--native-lib must not be empty"
[ -n "$native_triple" ] || die "--native-triple must not be empty"
[ -n "$native_lib_name" ] || die "--native-lib-name must not be empty"
[ -n "$wasi_lib_dir" ] || die "--wasi-lib-dir must not be empty"
[ -n "$destination" ] || die "--destination must not be empty"
[ -n "$completion_shells" ] || die "--completion-shells must not be empty"

case "$native_triple" in
. | .. | */* | *\\*) die "native triple must be one path component: $native_triple" ;;
esac
case "$native_lib_name" in
. | .. | */* | *\\*) die "native library name must be one path component: $native_lib_name" ;;
esac
case "$destination" in
/ | . | ..) die "refusing unsafe destination: $destination" ;;
esac

for binary in hew hew-lsp hew-observe; do
    [ -f "$bin_dir/$binary$bin_suffix" ] ||
        die "missing release binary: $bin_dir/$binary$bin_suffix"
done
[ -f "$native_lib" ] || die "missing native release library: $native_lib"
[ -d "$source_dir/std" ] || die "missing standard library directory: $source_dir/std"
[ -d "$wasi_lib_dir" ] || die "missing portable WASI library directory: $wasi_lib_dir"
for wasi_lib in libhew_runtime.a libhew_std.a; do
    [ -f "$wasi_lib_dir/$wasi_lib" ] ||
        die "missing portable WASI library: $wasi_lib_dir/$wasi_lib"
done
for document in LICENSE-MIT LICENSE-APACHE NOTICE THIRD-PARTY-LICENSES README.md; do
    [ -f "$source_dir/$document" ] || die "missing release document: $source_dir/$document"
done

destination_parent=$(dirname "$destination")
mkdir -p "$destination_parent"
if [ -e "$destination" ] || [ -L "$destination" ]; then
    die "destination already exists: $destination"
fi
stage_dir=$(mktemp -d "$destination_parent/.hew-release-stage.XXXXXX") ||
    die "could not create staging directory under $destination_parent"
package_root=$stage_dir/package
cleanup() {
    rm -rf "$stage_dir"
}
trap cleanup EXIT HUP INT TERM

mkdir -p "$package_root/bin" "$package_root/lib/$native_triple" \
    "$package_root/lib/wasm32-wasip1" "$package_root/std" \
    "$package_root/completions"

for binary in hew hew-lsp hew-observe; do
    cp "$bin_dir/$binary$bin_suffix" "$package_root/bin/$binary$bin_suffix"
    chmod +x "$package_root/bin/$binary$bin_suffix"
done

cp "$native_lib" "$package_root/lib/$native_lib_name"
cp "$native_lib" "$package_root/lib/$native_triple/$native_lib_name"
for wasi_lib in libhew_runtime.a libhew_std.a; do
    cp "$wasi_lib_dir/$wasi_lib" "$package_root/lib/wasm32-wasip1/$wasi_lib"
done
cp -R "$source_dir/std/." "$package_root/std/"

completion_count=0
for completion_shell in $completion_shells; do
    case "$completion_shell" in
    *[!A-Za-z0-9_.-]*) die "invalid completion shell name: $completion_shell" ;;
    esac
    completion_path=$package_root/completions/hew.$completion_shell
    "$package_root/bin/hew$bin_suffix" completions "$completion_shell" >"$completion_path" ||
        die "completion generation failed for $completion_shell"
    [ -s "$completion_path" ] ||
        die "completion generation produced empty output for $completion_shell"
    completion_count=$((completion_count + 1))
done
[ "$completion_count" -gt 0 ] || die "no completion files were generated"

for document in LICENSE-MIT LICENSE-APACHE NOTICE THIRD-PARTY-LICENSES README.md; do
    cp "$source_dir/$document" "$package_root/$document"
done

mv "$package_root" "$destination"
printf 'Staged Hew release package at %s\n' "$destination"
