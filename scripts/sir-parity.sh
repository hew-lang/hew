#!/usr/bin/env bash
# sir-parity.sh — SIR-route versus legacy-route execution parity.
#
# For every `.hew` program under the given paths that declares `main`, compile
# it twice with the same compiler: once through the strict SIR lane
# (`hew compile --sir-lower`) and once through the legacy HIR->MIR body
# lowerer (`hew compile`). Run both binaries and compare exit status and
# stdout byte for byte. Any difference is a mismatch and the harness fails.
#
# A program the strict lane refuses to compile is not admitted: the lane has
# no fallback, so a successful `--sir-lower` compile is the proof that every
# body the entry reaches went through SIR. Refused programs are counted and
# listed as "not admitted", never compared. An admitted program whose legacy
# compile fails is a mismatch too: the two routes disagree about the program.
#
# The harness refuses to pass when it compared nothing. An empty comparison
# proves nothing, so the make target always includes a fixture directory
# with at least one program the strict lane admits today.
#
# WHEN OBSOLETE: when the legacy lowerer is deleted (final-ladder P5) there is
# one route left and nothing to compare; delete this script with it.
#
# Usage:
#   scripts/sir-parity.sh [options] <path>...
#
# Options:
#   --hew-bin <path>     Compiler to drive. Default: $HEW_BIN, then Cargo's
#                        resolved debug output directory.
#   --workdir <dir>      Scratch directory for emitted binaries, logs, and
#                        TMPDIR. Default: <repo>/.tmp/sir-parity. Recreated
#                        on every run.
#   --timeout <seconds>  Per compile and per run budget. Default: 120.
#   --help               Print this text.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
# shellcheck source=scripts/lib/timeout.sh
# shellcheck disable=SC1091
source "$REPO_ROOT/scripts/lib/timeout.sh"
# shellcheck source=scripts/lib/cargo-output-dir.sh
# shellcheck disable=SC1091
source "$REPO_ROOT/scripts/lib/cargo-output-dir.sh"
# shellcheck source=scripts/lib/corpus-nonempty.sh
# shellcheck disable=SC1091
source "$REPO_ROOT/scripts/lib/corpus-nonempty.sh"

usage() {
    sed -n '2,/^$/p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
}

HEW_BIN="${HEW_BIN:-}"
WORKDIR="$REPO_ROOT/.tmp/sir-parity"
TIMEOUT_SECONDS=120
PATHS=()

while [[ $# -gt 0 ]]; do
    case "$1" in
    --hew-bin)
        [[ $# -ge 2 ]] || {
            echo "error: --hew-bin needs a path" >&2
            exit 2
        }
        HEW_BIN="$2"
        shift 2
        ;;
    --workdir)
        [[ $# -ge 2 ]] || {
            echo "error: --workdir needs a directory" >&2
            exit 2
        }
        WORKDIR="$2"
        shift 2
        ;;
    --timeout)
        [[ $# -ge 2 ]] || {
            echo "error: --timeout needs a number of seconds" >&2
            exit 2
        }
        TIMEOUT_SECONDS="$2"
        shift 2
        ;;
    --help | -h)
        usage
        exit 0
        ;;
    --)
        shift
        PATHS+=("$@")
        break
        ;;
    -*)
        echo "error: unknown option $1" >&2
        exit 2
        ;;
    *)
        PATHS+=("$1")
        shift
        ;;
    esac
done

if [[ ${#PATHS[@]} -eq 0 ]]; then
    echo "error: at least one file or directory is required" >&2
    usage >&2
    exit 2
fi
if [[ -z "$HEW_BIN" ]]; then
    HEW_BIN="$(cargo_debug_dir "$REPO_ROOT")/hew"
fi
if [[ ! -x "$HEW_BIN" ]]; then
    echo "error: hew binary is not executable: $HEW_BIN" >&2
    exit 2
fi
if [[ ! "$TIMEOUT_SECONDS" =~ ^[1-9][0-9]*$ ]]; then
    echo "error: --timeout must be a positive integer, got '$TIMEOUT_SECONDS'" >&2
    exit 2
fi

# Every `.hew` file under the paths, sorted so the report is a function of
# the corpus rather than of directory order.
enumerate_hew_files() {
    local path
    for path in "${PATHS[@]}"; do
        if [[ -d "$path" ]]; then
            find "$path" -type f -name '*.hew'
        elif [[ -f "$path" ]]; then
            printf '%s\n' "$path"
        else
            echo "error: $path is neither a file nor a directory" >&2
            return 2
        fi
    done | LC_ALL=C sort -u
}

# Programs are the files that declare `main`. This is a textual selection:
# a top-level `fn main` at column zero, optionally `pub`. The compiler is the
# authority on whether the declaration really is the entry, and a selected
# file the compiler refuses to build is reported as such, never hidden.
declares_main() {
    grep -qE '^(pub[[:space:]]+)?fn[[:space:]]+main[[:space:]]*\(' "$1"
}

PROGRAMS=()
while IFS= read -r file; do
    [[ -n "$file" ]] || continue
    if declares_main "$file"; then
        PROGRAMS+=("$file")
    fi
done < <(enumerate_hew_files)

corpus_nonempty_assert "sir-parity programs" "${#PROGRAMS[@]}" "files declaring main under: ${PATHS[*]}"

rm -rf "$WORKDIR"
mkdir -p "$WORKDIR/tmp"
export TMPDIR="$WORKDIR/tmp"

# The emitted native binary sits beside the other artefacts under the emit
# directory, named after the source stem (plus `.exe` on Windows).
emitted_binary() {
    local dir="$1"
    local stem="$2"
    if [[ -x "$dir/$stem" ]]; then
        printf '%s\n' "$dir/$stem"
    elif [[ -f "$dir/$stem.exe" ]]; then
        printf '%s\n' "$dir/$stem.exe"
    else
        return 1
    fi
}

# run_program <binary> <capture-prefix>: writes <prefix>.stdout, <prefix>.stderr,
# <prefix>.status. The exit status of the program is data, never a harness
# failure, so it is captured rather than propagated.
run_program() {
    local binary="$1"
    local prefix="$2"
    local status=0
    run_with_timeout "$TIMEOUT_SECONDS" "$binary" \
        </dev/null >"$prefix.stdout" 2>"$prefix.stderr" || status=$?
    printf '%s\n' "$status" >"$prefix.status"
}

compared=0
mismatches=0
not_admitted=0
MISMATCH_LINES=()
NOT_ADMITTED_LINES=()

for file in "${PROGRAMS[@]}"; do
    stem="$(basename "$file" .hew)"
    slug="${file//\//__}"
    slug="${slug%.hew}"
    case_dir="$WORKDIR/$slug"
    sir_dir="$case_dir/sir"
    legacy_dir="$case_dir/legacy"
    mkdir -p "$sir_dir" "$legacy_dir"

    if ! run_with_timeout "$TIMEOUT_SECONDS" "$HEW_BIN" compile --sir-lower \
        --emit-dir "$sir_dir" "$file" >"$case_dir/sir.compile.log" 2>&1; then
        not_admitted=$((not_admitted + 1))
        NOT_ADMITTED_LINES+=("$file")
        continue
    fi
    sir_bin="$(emitted_binary "$sir_dir" "$stem")" || {
        mismatches=$((mismatches + 1))
        MISMATCH_LINES+=("$file: --sir-lower compile succeeded but emitted no binary under $sir_dir")
        continue
    }

    if ! run_with_timeout "$TIMEOUT_SECONDS" "$HEW_BIN" compile \
        --emit-dir "$legacy_dir" "$file" >"$case_dir/legacy.compile.log" 2>&1; then
        mismatches=$((mismatches + 1))
        MISMATCH_LINES+=("$file: admitted by the SIR route but the legacy compile failed (see $case_dir/legacy.compile.log)")
        continue
    fi
    legacy_bin="$(emitted_binary "$legacy_dir" "$stem")" || {
        mismatches=$((mismatches + 1))
        MISMATCH_LINES+=("$file: legacy compile succeeded but emitted no binary under $legacy_dir")
        continue
    }

    run_program "$sir_bin" "$case_dir/sir"
    run_program "$legacy_bin" "$case_dir/legacy"
    compared=$((compared + 1))

    sir_status="$(cat "$case_dir/sir.status")"
    legacy_status="$(cat "$case_dir/legacy.status")"
    case_mismatch=0
    if [[ "$sir_status" != "$legacy_status" ]]; then
        MISMATCH_LINES+=("$file: exit status differs: sir=$sir_status legacy=$legacy_status")
        case_mismatch=1
    fi
    if ! cmp -s "$case_dir/sir.stdout" "$case_dir/legacy.stdout"; then
        MISMATCH_LINES+=("$file: stdout differs (see $case_dir/sir.stdout and $case_dir/legacy.stdout)")
        case_mismatch=1
    fi
    if [[ "$case_mismatch" -eq 1 ]]; then
        mismatches=$((mismatches + 1))
        echo "MISMATCH $file"
    else
        echo "PARITY   $file"
    fi
done

if [[ ${#NOT_ADMITTED_LINES[@]} -gt 0 ]]; then
    echo
    echo "not admitted by the SIR route (${#NOT_ADMITTED_LINES[@]}):"
    printf '  %s\n' "${NOT_ADMITTED_LINES[@]}"
fi
if [[ ${#MISMATCH_LINES[@]} -gt 0 ]]; then
    echo
    echo "mismatches (${#MISMATCH_LINES[@]}):"
    printf '  %s\n' "${MISMATCH_LINES[@]}"
fi

echo
echo "sir-parity: $compared compared, $mismatches mismatch(es), $not_admitted not admitted, workdir $WORKDIR"

if [[ "$mismatches" -gt 0 ]]; then
    exit 1
fi
if [[ "$compared" -eq 0 ]]; then
    echo "sir-parity: nothing was compared; an empty parity run proves nothing" >&2
    exit 1
fi
exit 0
