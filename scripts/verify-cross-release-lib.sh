#!/usr/bin/env sh
# Verify that a cross-built release archive matches its declared Rust target.

set -eu

if [ "$#" -ne 2 ]; then
    echo "usage: $0 <rust-target> <archive>" >&2
    exit 2
fi

target=$1
archive=$2

case "${target}" in
    x86_64-unknown-freebsd)
        expected_name=libhew.a
        expected_formats='elf64-x86-64'
        expected_arch='x86_64'
        expected_os_abi='FreeBSD'
        ;;
    aarch64-unknown-freebsd)
        expected_name=libhew.a
        expected_formats='elf64-littleaarch64'
        expected_arch='aarch64'
        expected_os_abi='FreeBSD'
        ;;
    x86_64-pc-windows-msvc)
        expected_name=hew.lib
        expected_formats='COFF-import-file-x86-64 COFF-x86-64'
        expected_arch='x86_64'
        expected_os_abi=''
        ;;
    *)
        echo "unsupported cross-release target: ${target}" >&2
        exit 2
        ;;
esac

if [ "$(basename "${archive}")" != "${expected_name}" ]; then
    echo "archive name does not match ${target}: ${archive}" >&2
    exit 1
fi
if [ ! -s "${archive}" ]; then
    echo "missing or empty cross-release archive: ${archive}" >&2
    exit 1
fi

archive_dir=$(dirname "${archive}")
archive_count=$(find "${archive_dir}" -maxdepth 1 -type f \
    \( -name libhew.a -o -name hew.lib \) | wc -l | tr -d ' ')
if [ "${archive_count}" != 1 ]; then
    echo "expected exactly one release archive in ${archive_dir}, found ${archive_count}" >&2
    exit 1
fi

for tool in llvm-readobj llvm-nm; do
    if ! command -v "${tool}" >/dev/null 2>&1; then
        echo "required archive inspection tool is unavailable: ${tool}" >&2
        exit 1
    fi
done

work_dir=$(mktemp -d)
trap 'rm -rf "${work_dir}"' EXIT HUP INT TERM
headers=${work_dir}/headers.txt
symbols=${work_dir}/symbols.txt

llvm-readobj --file-headers "${archive}" > "${headers}"
llvm-nm --defined-only --extern-only "${archive}" > "${symbols}"

formats=$(sed -n 's/^Format: //p' "${headers}" | sort -u | tr '\n' ' ' | sed 's/ $//')
if [ "${formats}" != "${expected_formats}" ]; then
    echo "unexpected object formats for ${target}: ${formats}" >&2
    exit 1
fi
if ! grep -q "^Arch: ${expected_arch}$" "${headers}"; then
    echo "expected ${expected_arch} objects in ${archive}" >&2
    exit 1
fi
if [ -n "${expected_os_abi}" ] && \
    ! grep -q "OS/ABI: ${expected_os_abi}" "${headers}"; then
    echo "expected ${expected_os_abi} object ABI in ${archive}" >&2
    exit 1
fi
if ! grep -Eq '[[:space:]]T[[:space:]]hew_alloc$' "${symbols}"; then
    echo "required hew_alloc export is missing from ${archive}" >&2
    exit 1
fi

echo "verified ${target}: formats=${formats}, arch=${expected_arch}"
