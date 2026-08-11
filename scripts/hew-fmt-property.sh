#!/usr/bin/env bash
# Prove that formatting every compilable file in the existing Hew corpus keeps
# it compilable and reaches a byte-stable fixed point on the second pass.

set -euo pipefail

if [[ "${1:-}" == "--worker" ]]; then
    action="$2"
    results_dir="$3"
    worker_hew_bin="$4"
    worker_repo_root="$5"
    worker_mirror_root="$6"
    rel="$7"
    result_file="$(mktemp "$results_dir/$action.result.XXXXXX")"

    case "$action" in
        discover)
            if "$worker_hew_bin" check --project-dir "$worker_repo_root" \
                "$worker_repo_root/$rel" >/dev/null 2>&1; then
                printf 'compilable\t%s\n' "$rel" > "$result_file"
            else
                printf 'not_compilable\t%s\n' "$rel" > "$result_file"
            fi
            ;;
        format)
            if "$worker_hew_bin" fmt "$worker_mirror_root/$rel" >/dev/null 2>&1; then
                printf 'formatted\t%s\n' "$rel" > "$result_file"
            else
                printf 'format_failure\t%s\n' "$rel" > "$result_file"
            fi
            ;;
        validate)
            : > "$result_file"
            if ! "$worker_hew_bin" check --project-dir "$worker_mirror_root" \
                "$worker_mirror_root/$rel" >/dev/null 2>&1; then
                printf 'check_failure\t%s\n' "$rel" >> "$result_file"
            fi
            if ! "$worker_hew_bin" fmt --check "$worker_mirror_root/$rel" >/dev/null 2>&1; then
                printf 'idempotence_failure\t%s\n' "$rel" >> "$result_file"
            fi
            ;;
        *)
            echo "error: unknown formatter property worker action: $action" >&2
            exit 2
            ;;
    esac
    exit 0
fi

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
# shellcheck source=scripts/lib/corpus-nonempty.sh
# shellcheck disable=SC1091
source "$REPO_ROOT/scripts/lib/corpus-nonempty.sh"
HEW_BIN="${HEW_BIN:-$REPO_ROOT/target/debug/hew}"

if [[ ! -x "$HEW_BIN" ]]; then
    echo "error: hew binary not found or not executable: $HEW_BIN" >&2
    exit 1
fi

CORPUS_FILES=()
repo_total=0
vertical_candidates=0
hew_test_candidates=0
example_candidates=0
core_matrix_candidates=0
stdlib_candidates=0
while IFS= read -r -d '' path; do
    repo_total=$((repo_total + 1))
    case "$path" in
        tests/vertical-slice/*)
            CORPUS_FILES+=("$path")
            vertical_candidates=$((vertical_candidates + 1))
            ;;
        tests/hew/*)
            CORPUS_FILES+=("$path")
            hew_test_candidates=$((hew_test_candidates + 1))
            ;;
        examples/*)
            CORPUS_FILES+=("$path")
            example_candidates=$((example_candidates + 1))
            ;;
        tests/core-matrix/cells/*)
            CORPUS_FILES+=("$path")
            core_matrix_candidates=$((core_matrix_candidates + 1))
            ;;
        std/*)
            CORPUS_FILES+=("$path")
            stdlib_candidates=$((stdlib_candidates + 1))
            ;;
    esac
done < <(git -C "$REPO_ROOT" ls-files -z -- '*.hew')

candidate_count=${#CORPUS_FILES[@]}
outside_count=$((repo_total - candidate_count))
if (( candidate_count == 0 )); then
    echo "error: formatter property corpus is empty" >&2
    exit 1
fi

jobs="${HEW_FMT_PROPERTY_JOBS:-8}"
if [[ ! "$jobs" =~ ^[1-9][0-9]*$ ]]; then
    echo "error: HEW_FMT_PROPERTY_JOBS must be a positive integer, got: $jobs" >&2
    exit 1
fi

RESULTS_DIR="$(mktemp -d "${TMPDIR:-/tmp}/hew-fmt-property-results.XXXXXX")"
MIRROR_ROOT="$(mktemp -d "${TMPDIR:-/tmp}/hew-fmt-property-corpus.XXXXXX")"
cleanup() {
    rm -rf -- "$RESULTS_DIR" "$MIRROR_ROOT"
}
trap cleanup EXIT
trap 'exit 130' HUP INT TERM

# Keep the original relative paths in a private mirror. Stdlib authority and
# module identity depend on those paths, so checking arbitrary sibling names
# would test the harness rather than the formatter.
cp -R "$REPO_ROOT/std" "$MIRROR_ROOT/std"
cp -R "$REPO_ROOT/examples" "$MIRROR_ROOT/examples"
cp -R "$REPO_ROOT/tests" "$MIRROR_ROOT/tests"

run_workers() {
    local action="$1"
    shift
    printf '%s\0' "$@" \
        | xargs -0 -n 1 -P "$jobs" bash "$0" --worker "$action" "$RESULTS_DIR" \
            "$HEW_BIN" "$REPO_ROOT" "$MIRROR_ROOT"
}

# Establish the antecedent dynamically: no list decides which files compile.
run_workers discover "${CORPUS_FILES[@]}"

compilable=0
not_compilable=0
vertical_compilable=0
hew_test_compilable=0
example_compilable=0
core_matrix_compilable=0
stdlib_compilable=0
COMPILED_FILES=()
for result_file in "$RESULTS_DIR"/discover.result.*; do
    IFS=$'\t' read -r kind rel < "$result_file"
    case "$kind" in
        compilable)
            compilable=$((compilable + 1))
            COMPILED_FILES+=("$rel")
            case "$rel" in
                tests/vertical-slice/*)
                    vertical_compilable=$((vertical_compilable + 1))
                    ;;
                tests/hew/*)
                    hew_test_compilable=$((hew_test_compilable + 1))
                    ;;
                examples/*)
                    example_compilable=$((example_compilable + 1))
                    ;;
                tests/core-matrix/cells/*)
                    core_matrix_compilable=$((core_matrix_compilable + 1))
                    ;;
                std/*)
                    stdlib_compilable=$((stdlib_compilable + 1))
                    ;;
            esac
            ;;
        not_compilable)
            not_compilable=$((not_compilable + 1))
            ;;
        *)
            echo "error: unknown formatter discovery result: $kind" >&2
            exit 1
            ;;
    esac
done

format_failures=0
FORMATTED_FILES=()
FAILURES=()
run_workers format "${COMPILED_FILES[@]}"
for result_file in "$RESULTS_DIR"/format.result.*; do
    IFS=$'\t' read -r kind rel < "$result_file"
    case "$kind" in
        formatted)
            FORMATTED_FILES+=("$rel")
            ;;
        format_failure)
            format_failures=$((format_failures + 1))
            FAILURES+=("$rel: first format failed")
            ;;
        *)
            echo "error: unknown formatter result: $kind" >&2
            exit 1
            ;;
    esac
done

# Validate only after the complete formatted mirror is stable, so imports see
# one coherent corpus rather than racing other formatter processes.
check_failures=0
idempotence_failures=0
run_workers validate "${FORMATTED_FILES[@]}"
for result_file in "$RESULTS_DIR"/validate.result.*; do
    while IFS=$'\t' read -r kind rel; do
        case "$kind" in
            check_failure)
                check_failures=$((check_failures + 1))
                FAILURES+=("$rel: formatted text failed parse/check")
                ;;
            idempotence_failure)
                idempotence_failures=$((idempotence_failures + 1))
                FAILURES+=("$rel: second format changed bytes")
                ;;
            *)
                echo "error: unknown formatter validation result: $kind" >&2
                exit 1
                ;;
        esac
    done < "$result_file"
done

echo "==> Hew formatter behaviour-preservation property"
echo "Total tracked .hew files:        $repo_total"
echo "Outside property roots:         $outside_count"
echo "Candidates discovered:          $candidate_count"
echo "Candidate roots: vertical-slice=$vertical_candidates tests/hew=$hew_test_candidates examples=$example_candidates core-matrix=$core_matrix_candidates std=$stdlib_candidates"
echo "Original files compilable:      $compilable"
echo "Compilable roots: vertical-slice=$vertical_compilable tests/hew=$hew_test_compilable examples=$example_compilable core-matrix=$core_matrix_compilable std=$stdlib_compilable"
echo "Original files not compilable:  $not_compilable"
echo "First-format failures:          $format_failures"
echo "Formatted parse/check failures: $check_failures"
echo "Idempotence failures:           $idempotence_failures"

floor_failed=0
corpus_nonempty_assert "hew-fmt-property-files" "$compilable" \
    "compilable files formatted, rechecked, and checked for idempotence" || floor_failed=1

if (( ${#FAILURES[@]} > 0 )); then
    echo ""
    echo "Formatter property failures:" >&2
    printf '  - %s\n' "${FAILURES[@]}" | sort >&2
fi

if (( floor_failed != 0 || ${#FAILURES[@]} > 0 )); then
    echo "==> Hew formatter behaviour-preservation property: FAILED" >&2
    exit 1
fi

echo "==> Hew formatter behaviour-preservation property: PASSED"
