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
  --wasi-lib-dir DIR [--optional-wasi] [--live-runtime FILE] \
  --destination DIR --completion-shells "SHELL ..."
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
optional_wasi=false
live_runtime=

while [ "$#" -gt 0 ]; do
    option=$1
    case "$option" in
    --source-dir | --bin-dir | --bin-suffix | --native-lib | \
        --native-triple | --native-lib-name | --wasi-lib-dir | \
        --destination | --completion-shells | --live-runtime)
        [ "$#" -ge 2 ] || die "$option requires a value"
        value=$2
        shift 2
        ;;
    --optional-wasi)
        optional_wasi=true
        shift
        continue
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
    --live-runtime) live_runtime=$value ;;
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

[ -f "$bin_dir/hew$bin_suffix" ] ||
    die "missing release binary: $bin_dir/hew$bin_suffix"
[ -f "$native_lib" ] || die "missing native release library: $native_lib"
[ -d "$source_dir/std" ] || die "missing standard library directory: $source_dir/std"
for header in hew.h hew_host.h; do
    [ -f "$source_dir/hew-cabi/include/$header" ] ||
        die "missing public C header: $source_dir/hew-cabi/include/$header"
done
include_wasi=true
if [ ! -d "$wasi_lib_dir" ] && [ "$optional_wasi" = true ]; then
    include_wasi=false
    printf 'Standalone WASI libraries unavailable; staging the native toolchain.\n'
else
    [ -d "$wasi_lib_dir" ] || die "missing portable WASI library directory: $wasi_lib_dir"
    for wasi_lib in libhew_runtime.a libhew_std.a; do
        [ -f "$wasi_lib_dir/$wasi_lib" ] ||
            die "missing portable WASI library: $wasi_lib_dir/$wasi_lib"
    done
fi
for document in LICENSE-MIT LICENSE-APACHE NOTICE THIRD-PARTY-LICENSES README.md; do
    [ -f "$source_dir/$document" ] || die "missing release document: $source_dir/$document"
done
if [ -n "$live_runtime" ]; then
    [ -f "$live_runtime" ] || die "missing live shared runtime: $live_runtime"
    [ "$include_wasi" = true ] || die "live execution requires portable WASI libraries"
    for companion in hew-live-host hew-live-node; do
        [ -f "$bin_dir/$companion$bin_suffix" ] || die "missing live companion: $companion$bin_suffix"
    done
    [ -f "$source_dir/hew-live/README.md" ] || die "missing live SDK documentation"
fi

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
    "$package_root/std" \
    "$package_root/include" "$package_root/completions" "$package_root/share/hew"

cp "$bin_dir/hew$bin_suffix" "$package_root/bin/hew$bin_suffix"
chmod +x "$package_root/bin/hew$bin_suffix"

cp "$native_lib" "$package_root/lib/$native_lib_name"
cp "$native_lib" "$package_root/lib/$native_triple/$native_lib_name"
if [ "$include_wasi" = true ]; then
    mkdir -p "$package_root/lib/wasm32-wasip1"
    for wasi_lib in libhew_runtime.a libhew_std.a; do
        cp "$wasi_lib_dir/$wasi_lib" "$package_root/lib/wasm32-wasip1/$wasi_lib"
    done
fi
cp -R "$source_dir/std/." "$package_root/std/"
cp "$source_dir/scripts/debug/hew-gdb.py" "$source_dir/scripts/debug/hew_lldb.py" \
    "$package_root/share/hew/"
cp "$source_dir/hew-cabi/include/hew.h" "$source_dir/hew-cabi/include/hew_host.h" \
    "$package_root/include/"

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

if [ -n "$live_runtime" ]; then
    mkdir -p "$package_root/lib/hew" "$package_root/share/hew/live"
    for companion in hew-live-host hew-live-node; do
        cp "$bin_dir/$companion$bin_suffix" "$package_root/bin/"
        chmod +x "$package_root/bin/$companion$bin_suffix"
    done
    runtime_name=$(basename "$live_runtime")
    cp "$live_runtime" "$package_root/lib/hew/$runtime_name"
    cp "$source_dir/hew-live/README.md" "$package_root/share/hew/live/README.md"
    cp "$source_dir/hew-live/tests/fixtures/router.hew" "$package_root/share/hew/live/router.hew"
    "$package_root/bin/hew-live-node$bin_suffix" prepare-transport \
        --compiler "$package_root/bin/hew$bin_suffix" \
        --runtime "$package_root/lib/hew/$runtime_name" \
        --output "$package_root/lib/hew/live-node" >/dev/null ||
        die "live transport preparation failed"
fi

mv "$package_root" "$destination"
printf 'Staged Hew release package at %s\n' "$destination"
