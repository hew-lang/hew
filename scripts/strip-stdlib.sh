#!/usr/bin/env bash

set -euo pipefail

RELEASE_DIR_INPUT="${1:-target/release}"
OUT_DIR_INPUT="${2:-${RELEASE_DIR_INPUT}/stripped-stdlib}"

if [ ! -d "${RELEASE_DIR_INPUT}" ]; then
    echo "error: release directory not found: ${RELEASE_DIR_INPUT}" >&2
    echo "hint: run 'make release' first" >&2
    exit 1
fi

mkdir -p "$(dirname "${OUT_DIR_INPUT}")"

RELEASE_DIR="$(cd "${RELEASE_DIR_INPUT}" && pwd)"
OUT_DIR_PARENT="$(cd "$(dirname "${OUT_DIR_INPUT}")" && pwd)"
OUT_DIR="${OUT_DIR_PARENT}/$(basename "${OUT_DIR_INPUT}")"
RUNTIME_ARCHIVE="${RELEASE_DIR}/libhew_runtime.a"

if [ ! -f "${RUNTIME_ARCHIVE}" ]; then
    echo "error: runtime archive not found: ${RUNTIME_ARCHIVE}" >&2
    echo "hint: run 'make release' first" >&2
    exit 1
fi

if command -v llvm-ar >/dev/null 2>&1; then
    AR='llvm-ar'
elif command -v ar >/dev/null 2>&1; then
    AR='ar'
else
    echo "error: neither llvm-ar nor ar is available" >&2
    exit 1
fi

if command -v llvm-nm >/dev/null 2>&1; then
    NM='llvm-nm'
elif command -v nm >/dev/null 2>&1; then
    NM='nm'
else
    echo "error: neither llvm-nm nor nm is available" >&2
    exit 1
fi

if command -v llvm-objcopy >/dev/null 2>&1; then
    OBJCOPY='llvm-objcopy'
elif command -v objcopy >/dev/null 2>&1; then
    OBJCOPY='objcopy'
else
    echo "error: neither llvm-objcopy nor objcopy is available" >&2
    exit 1
fi

size_bytes() {
    python3 - "$1" <<'PY'
import os
import sys

path = sys.argv[1]
if os.path.isdir(path):
    total = 0
    for root, _, files in os.walk(path):
        for name in files:
            total += os.path.getsize(os.path.join(root, name))
    print(total)
else:
    print(os.path.getsize(path))
PY
}

size_human() {
    python3 - "$1" <<'PY'
import sys

value = float(sys.argv[1])
units = ["B", "KiB", "MiB", "GiB", "TiB"]
for unit in units:
    if value < 1024.0 or unit == units[-1]:
        if unit == "B":
            print(f"{int(value)} {unit}")
        else:
            print(f"{value:.1f} {unit}")
        break
    value /= 1024.0
PY
}

shopt -s nullglob
stdlib_archives=("${RELEASE_DIR}"/libhew_std_*.a)
if [ "${#stdlib_archives[@]}" -eq 0 ]; then
    echo "error: no stdlib archives found in ${RELEASE_DIR}" >&2
    exit 1
fi

runtime_symbols_file="$(mktemp)"
trap 'rm -f "${runtime_symbols_file}"' EXIT

python3 - "${RUNTIME_ARCHIVE}" "${NM}" "${runtime_symbols_file}" <<'PY'
import subprocess
import sys
from pathlib import Path

archive = Path(sys.argv[1])
nm = sys.argv[2]
output_path = Path(sys.argv[3])

result = subprocess.check_output(
    [nm, "-g", "--defined-only", str(archive)],
    text=True,
    stderr=subprocess.DEVNULL,
)

symbols = set()
for line in result.splitlines():
    line = line.strip()
    if not line or line.endswith(":"):
        continue
    parts = line.split()
    if len(parts) >= 3:
        symbols.add(parts[-1])

output_path.write_text("".join(f"{symbol}\n" for symbol in sorted(symbols)))
PY

rm -rf "${OUT_DIR}"
mkdir -p "${OUT_DIR}"

echo "==> Using ${AR}"
echo "==> Runtime reference: ${RUNTIME_ARCHIVE}"
echo "==> Writing stripped stdlib archives to ${OUT_DIR}"

total_before=0
total_after_archive=0
total_after_payload=0

for archive in "${stdlib_archives[@]}"; do
    base_name="$(basename "${archive}")"
    crate_stem="${base_name%.a}"
    object_dir_name="${crate_stem}.objects"
    object_dir="${OUT_DIR}/${object_dir_name}"
    thin_archive="${OUT_DIR}/${base_name}"

    mkdir -p "${object_dir}"
    (cd "${object_dir}" && "${AR}" x "${archive}")

    counts="$(python3 - "${runtime_symbols_file}" "${object_dir}" "${NM}" "${OBJCOPY}" <<'PY'
import subprocess
import sys
from pathlib import Path

runtime_symbols_path = Path(sys.argv[1])
object_dir = Path(sys.argv[2])
nm = sys.argv[3]
objcopy = sys.argv[4]

runtime_symbols = {
    line.strip()
    for line in runtime_symbols_path.read_text().splitlines()
    if line.strip()
}

removed = 0
localized = 0

for member_path in sorted(object_dir.iterdir()):
    if not member_path.is_file():
        continue
    try:
        result = subprocess.check_output(
            [nm, "-g", "--defined-only", str(member_path)],
            text=True,
            stderr=subprocess.DEVNULL,
        )
    except subprocess.CalledProcessError:
        continue

    defined = set()
    for line in result.splitlines():
        line = line.strip()
        if not line or line.endswith(":"):
            continue
        parts = line.split()
        if len(parts) >= 3:
            defined.add(parts[-1])

    overlap = sorted(defined & runtime_symbols)
    if not overlap:
        continue

    unique = defined - runtime_symbols
    if not unique:
        member_path.unlink()
        removed += 1
        continue

    args = [objcopy]
    for symbol in overlap:
        args.extend(["--localize-symbol", symbol])
    args.append(str(member_path))
    subprocess.run(args, check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    localized += 1

print(f"{removed}\t{localized}")
PY
)"

    removed_count="${counts%%$'\t'*}"
    localized_count="${counts##*$'\t'}"

    kept_members=()
    for member_path in "${object_dir}"/*; do
        [ -f "${member_path}" ] || continue
        member_name="$(basename "${member_path}")"
        kept_members+=("${object_dir_name}/${member_name}")
    done

    if [ "${#kept_members[@]}" -eq 0 ]; then
        echo "error: ${base_name} has no members left after stripping" >&2
        exit 1
    fi

    rm -f "${thin_archive}"
    (
        cd "${OUT_DIR}"
        "${AR}" rcsT "${base_name}" "${kept_members[@]}"
    )

    before_bytes="$(size_bytes "${archive}")"
    after_archive_bytes="$(size_bytes "${thin_archive}")"
    after_payload_bytes="$(size_bytes "${object_dir}")"
    after_total_bytes=$((after_archive_bytes + after_payload_bytes))

    total_before=$((total_before + before_bytes))
    total_after_archive=$((total_after_archive + after_archive_bytes))
    total_after_payload=$((total_after_payload + after_total_bytes))

    printf '  %-36s %10s -> %10s thin (%10s total, removed %3d, localized %3d)\n' \
        "${base_name}" \
        "$(size_human "${before_bytes}")" \
        "$(size_human "${after_archive_bytes}")" \
        "$(size_human "${after_total_bytes}")" \
        "${removed_count}" \
        "${localized_count}"
done

printf '==> Totals: %s -> %s thin (%s total)\n' \
    "$(size_human "${total_before}")" \
    "$(size_human "${total_after_archive}")" \
    "$(size_human "${total_after_payload}")"
