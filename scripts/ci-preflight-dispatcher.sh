#!/usr/bin/env bash

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

DRY_RUN=0
BASE_REF=""
EXPLICIT_PATHS=0
LANE=""
LANE_REASON=""
CHANGED_FILES=()
COMMANDS=()

usage() {
    cat <<'EOF'
Usage: scripts/ci-preflight-dispatcher.sh [--dry-run] [--base <ref>] [--] [path...]

Dispatch a conservative local CI preflight based on changed files.

- Pass explicit paths to classify those files directly.
- With no paths, the script inspects committed, staged, unstaged, and untracked changes.
- If the first-slice routing is unclear, the script falls back to broader local checks.
EOF
}

die() {
    echo "error: $*" >&2
    exit 1
}

append_unique_path() {
    local path="$1"
    local existing
    if [[ ${CHANGED_FILES[0]+set} == set ]]; then
        for existing in "${CHANGED_FILES[@]}"; do
            if [[ "$existing" == "$path" ]]; then
                return 0
            fi
        done
    fi
    CHANGED_FILES+=("$path")
}

has_changed_files() {
    [[ ${CHANGED_FILES[0]+set} == set ]]
}

normalize_path() {
    local path="$1"
    if [[ "$path" == "$REPO_ROOT/"* ]]; then
        path="${path#"$REPO_ROOT"/}"
    fi
    while [[ "$path" == ./* ]]; do
        path="${path#./}"
    done
    printf '%s\n' "$path"
}

collect_paths_from_command() {
    local path=""
    while IFS= read -r path; do
        [[ -n "$path" ]] || continue
        append_unique_path "$(normalize_path "$path")"
    done < <("$@")
}

add_command() {
    COMMANDS+=("$1")
}

has_commands() {
    [[ ${COMMANDS[0]+set} == set ]]
}

is_docs_path() {
    case "$1" in
        docs/*|*.md|AUTHORS|LICENSE|LICENSE-*|NOTICE)
            return 0
            ;;
    esac
    return 1
}

is_grammar_path() {
    case "$1" in
        docs/specs/Hew.g4|docs/specs/grammar.ebnf)
            return 0
            ;;
    esac
    return 1
}

is_parser_path() {
    case "$1" in
        hew-parser/*|hew-lexer/*)
            return 0
            ;;
    esac
    return 1
}

is_types_path() {
    case "$1" in
        hew-types/*)
            return 0
            ;;
    esac
    return 1
}

is_cli_path() {
    case "$1" in
        hew-cli/*|adze-cli/*)
            return 0
            ;;
    esac
    return 1
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        --dry-run)
            DRY_RUN=1
            shift
            ;;
        --base)
            shift
            [[ $# -gt 0 ]] || die "--base requires a ref"
            BASE_REF="$1"
            shift
            ;;
        --help|-h)
            usage
            exit 0
            ;;
        --)
            shift
            EXPLICIT_PATHS=1
            while [[ $# -gt 0 ]]; do
                append_unique_path "$(normalize_path "$1")"
                shift
            done
            ;;
        -*)
            die "unknown option: $1"
            ;;
        *)
            EXPLICIT_PATHS=1
            append_unique_path "$(normalize_path "$1")"
            shift
            ;;
    esac
done

if [[ -n "$BASE_REF" ]] && ! git rev-parse --verify "$BASE_REF" >/dev/null 2>&1; then
    die "unknown base ref: $BASE_REF"
fi

if (( EXPLICIT_PATHS == 0 )); then
    if [[ -z "$BASE_REF" ]]; then
        if git rev-parse --verify origin/main >/dev/null 2>&1; then
            BASE_REF="origin/main"
        elif git rev-parse --verify main >/dev/null 2>&1; then
            BASE_REF="main"
        fi
    fi

    if [[ -n "$BASE_REF" ]]; then
        collect_paths_from_command git diff --name-only --diff-filter=ACMRD "$BASE_REF...HEAD"
    fi
    collect_paths_from_command git diff --cached --name-only --diff-filter=ACMRD
    collect_paths_from_command git diff --name-only --diff-filter=ACMRD
    collect_paths_from_command git ls-files --others --exclude-standard
fi

if ! has_changed_files; then
    echo "==> Hew CI preflight dispatcher"
    echo "No changed files detected."
    exit 0
fi

fallback_lane=0
has_grammar=0
has_parser=0
has_types=0
has_cli=0

for path in "${CHANGED_FILES[@]}"; do
    if is_grammar_path "$path"; then
        has_grammar=1
    elif is_docs_path "$path"; then
        continue
    elif is_parser_path "$path"; then
        has_parser=1
    elif is_types_path "$path"; then
        has_types=1
    elif is_cli_path "$path"; then
        has_cli=1
    else
        fallback_lane=1
    fi
done

bucket_count=$((has_grammar + has_parser + has_types + has_cli))

if (( fallback_lane == 1 )); then
    LANE="fallback"
    LANE_REASON="changed files extend beyond the first-slice targeted buckets"
elif (( bucket_count == 0 )); then
    LANE="docs"
    LANE_REASON="docs-only change"
elif (( bucket_count > 1 )); then
    LANE="fallback"
    LANE_REASON="multiple targeted buckets changed; keeping the first slice conservative"
elif (( has_grammar == 1 )); then
    LANE="grammar"
    LANE_REASON="grammar/spec inputs changed"
elif (( has_parser == 1 )); then
    LANE="parser"
    LANE_REASON="parser/frontend surface changed"
elif (( has_types == 1 )); then
    LANE="types"
    LANE_REASON="type-checker surface changed"
else
    LANE="cli"
    LANE_REASON="CLI surface changed"
fi

case "$LANE" in
    docs)
        ;;
    grammar)
        add_command "make grammar"
        ;;
    parser)
        add_command "cargo test -p hew-parser -p hew-lexer"
        ;;
    types)
        add_command "cargo test -p hew-types -p hew-parser -p hew-lexer"
        ;;
    cli)
        add_command "cargo test -p hew-cli -p adze-cli"
        ;;
    fallback)
        add_command "cargo fmt --all -- --check"
        add_command "make lint"
        add_command "make playground-check"
        add_command "make test"
        ;;
    *)
        die "unhandled lane: $LANE"
        ;;
esac

echo "==> Hew CI preflight dispatcher"
if (( EXPLICIT_PATHS == 1 )); then
    echo "Source: explicit paths"
else
    if [[ -n "$BASE_REF" ]]; then
        echo "Source: branch diff + working tree"
        echo "Base ref: $BASE_REF"
    else
        echo "Source: working tree"
    fi
fi
echo "Selected lane: $LANE"
echo "Reason: $LANE_REASON"
echo "Changed files:"
for path in "${CHANGED_FILES[@]}"; do
    echo "  - $path"
done

if ! has_commands; then
    echo "Commands: none (docs-only)"
    if (( DRY_RUN == 1 )); then
        echo "Dry run: no commands executed."
    fi
    exit 0
fi

echo "Commands:"
for cmd in "${COMMANDS[@]}"; do
    echo "  - $cmd"
done

if (( DRY_RUN == 1 )); then
    echo "Dry run: no commands executed."
    exit 0
fi

for cmd in "${COMMANDS[@]}"; do
    echo "==> $cmd"
    bash -lc "$cmd"
done
