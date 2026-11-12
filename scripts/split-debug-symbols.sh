#!/bin/sh
# Move a release compiler's debug info out of a staged binary.
#
# usage: scripts/split-debug-symbols.sh --triple TRIPLE --built-dir DIR \
#          --binary STAGED_BIN --destination DIR
#
# --built-dir is the Cargo output directory the staged binary was copied from;
# the Mach-O dSYM and the PDB are taken from there. --destination is created
# and receives the debug info laid out the way each platform's debuggers find
# it:
#   ELF      DESTINATION/.build-id/xx/yyyy.debug (extract under
#            /usr/lib/debug); the staged binary is stripped and names
#            `hew.debug` in its .gnu_debuglink beside its build-id
#   Mach-O   DESTINATION/hew.dSYM, matched to the binary by UUID; the
#            binary's debug map (build-host object paths) is stripped
#   PE       DESTINATION/hew.pdb, which the executable names by base name
set -eu

die() {
    printf 'split-debug-symbols: %s\n' "$*" >&2
    exit 1
}

triple=
built_dir=
binary=
destination=
while [ "$#" -gt 0 ]; do
    [ "$#" -ge 2 ] || die "$1 requires a value"
    case "$1" in
    --triple) triple=$2 ;;
    --built-dir) built_dir=$2 ;;
    --binary) binary=$2 ;;
    --destination) destination=$2 ;;
    *) die "unknown option: $1" ;;
    esac
    shift 2
done
[ -n "$triple" ] && [ -n "$built_dir" ] && [ -n "$binary" ] && [ -n "$destination" ] ||
    die "--triple, --built-dir, --binary and --destination are required"
[ -f "$binary" ] || die "missing staged binary: $binary"
if [ -e "$destination" ] || [ -L "$destination" ]; then
    die "destination already exists: $destination"
fi

case "$triple" in
*-apple-darwin)
    [ -d "$built_dir/hew.dSYM" ] ||
        die "missing $built_dir/hew.dSYM; build with CARGO_PROFILE_RELEASE_SPLIT_DEBUGINFO=packed"
    mkdir -p "$destination"
    # Cargo may expose the bundle as a symlink into deps/. Copy its contents
    # so the release archive contains the actual DWARF rather than that link.
    mkdir -p "$destination/hew.dSYM"
    cp -R "$built_dir/hew.dSYM/." "$destination/hew.dSYM/"
    strip -S "$binary" || die "strip -S failed on $binary"
    ;;
*-windows-msvc)
    [ -f "$built_dir/hew.pdb" ] || die "missing release PDB: $built_dir/hew.pdb"
    mkdir -p "$destination"
    cp "$built_dir/hew.pdb" "$destination/hew.pdb"
    ;;
*-linux-* | *-freebsd*)
    objcopy=${OBJCOPY:-objcopy}
    # The compiler reports its own build-id, on every ELF host alike.
    build_id=$("$binary" --version --verbose | sed -n 's/^build-id \([0-9a-f]*\)$/\1/p')
    [ "${#build_id}" -gt 2 ] || die "binary reports no GNU build-id: $binary"
    mkdir -p "$destination"
    debug_file=$destination/hew.debug
    # zlib rather than zstd: every gdb, lldb and elfutils reads it.
    "$objcopy" --only-keep-debug --compress-debug-sections=zlib "$binary" "$debug_file" ||
        die "$objcopy --only-keep-debug failed on $binary"
    # The debuglink records the base name and the debug file's CRC.
    "$objcopy" --strip-all --add-gnu-debuglink="$debug_file" "$binary" ||
        die "$objcopy could not strip $binary"
    id_dir=$destination/.build-id/$(printf '%s' "$build_id" | cut -c1-2)
    mkdir -p "$id_dir"
    chmod 644 "$debug_file"
    mv "$debug_file" "$id_dir/$(printf '%s' "$build_id" | cut -c3-).debug"
    ;;
*) die "no debug symbol layout for $triple" ;;
esac
