#!/usr/bin/env bash
# Fail-closed release sanitizer evidence validator.
#
# Usage:
#   scripts/check-sanitizer-gate.sh <release-commit-sha> <asan-result-file> [waiver-file]
#
# The ASan result file must contain exactly:
#   asan=pass
#
# TSan and Miri are not hard-run in the release gate yet. Their absence is only
# acceptable when release-sanitizer-waiver.toml contains one commit-pinned,
# unexpired waiver row per axis.

set -euo pipefail

readonly REQUIRED_AXES=(tsan miri)

die() {
    echo "sanitizer-gate: ERROR: $*" >&2
    exit 1
}

usage() {
    die "usage: $0 <40-hex-release-commit-sha> <asan-result-file> [waiver-file]"
}

if [[ $# -lt 2 || $# -gt 3 ]]; then
    usage
fi

release_commit="$1"
asan_result_file="$2"
waiver_file="${3:-release-sanitizer-waiver.toml}"

if [[ ! "$release_commit" =~ ^[0-9a-fA-F]{40}$ ]]; then
    die "release commit must be the exact 40-hex commit SHA"
fi
release_commit="$(printf '%s' "$release_commit" | tr '[:upper:]' '[:lower:]')"

if [[ ! -f "$asan_result_file" ]]; then
    die "ASan result file is absent: ${asan_result_file}"
fi

asan_status=""
while IFS= read -r line || [[ -n "$line" ]]; do
    line="${line%%#*}"
    line="${line#"${line%%[![:space:]]*}"}"
    line="${line%"${line##*[![:space:]]}"}"
    [[ -z "$line" ]] && continue
    if [[ -n "$asan_status" ]]; then
        die "ASan result file is ambiguous: multiple result lines"
    fi
    asan_status="$line"
done < "$asan_result_file"

if [[ "$asan_status" != "asan=pass" ]]; then
    die "ASan hard gate did not pass unambiguously"
fi

if [[ ! -f "$waiver_file" ]]; then
    die "waiver file is absent: ${waiver_file}"
fi

today="$(date -u +%F)"

valid_tsan_count=0
valid_miri_count=0

current_axis=""
current_reason=""
current_tracking=""
current_commit=""
current_expires=""
row_number=0

trim() {
    local value="$1"
    value="${value#"${value%%[![:space:]]*}"}"
    value="${value%"${value##*[![:space:]]}"}"
    printf '%s' "$value"
}

strip_quotes() {
    local value="$1"
    if [[ "$value" =~ ^\"(.*)\"$ ]]; then
        printf '%s' "${BASH_REMATCH[1]}"
    else
        printf '%s' "$value"
    fi
}

reset_row() {
    current_axis=""
    current_reason=""
    current_tracking=""
    current_commit=""
    current_expires=""
}

axis_is_required() {
    local axis="$1"
    local required
    for required in "${REQUIRED_AXES[@]}"; do
        [[ "$axis" == "$required" ]] && return 0
    done
    return 1
}

validate_row() {
    [[ "$row_number" -eq 0 ]] && return 0

    [[ -n "$current_axis" ]] || die "waiver row ${row_number} missing axis"
    [[ -n "$current_reason" ]] || die "waiver row ${row_number} missing reason"
    [[ -n "$current_tracking" ]] || die "waiver row ${row_number} missing tracking"
    [[ -n "$current_commit" ]] || die "waiver row ${row_number} missing commit"
    [[ -n "$current_expires" ]] || die "waiver row ${row_number} missing expires"

    current_axis="$(printf '%s' "$current_axis" | tr '[:upper:]' '[:lower:]')"
    current_commit="$(printf '%s' "$current_commit" | tr '[:upper:]' '[:lower:]')"

    case "$current_axis" in
        "*"|"all")
            die "waiver row ${row_number} is a blanket waiver"
            ;;
    esac

    if ! axis_is_required "$current_axis"; then
        die "waiver row ${row_number} has unsupported axis: ${current_axis}"
    fi

    if [[ ! "$current_commit" =~ ^[0-9a-f]{40}$ ]]; then
        die "waiver row ${row_number} commit is not a 40-hex SHA"
    fi

    if [[ ! "$current_expires" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]]; then
        die "waiver row ${row_number} expires must be YYYY-MM-DD"
    fi

    if [[ "$current_commit" != "$release_commit" ]]; then
        return 0
    fi

    if [[ "$current_expires" < "$today" ]]; then
        die "waiver row ${row_number} for ${current_axis} expired on ${current_expires}"
    fi

    case "$current_axis" in
        tsan) valid_tsan_count=$((valid_tsan_count + 1)) ;;
        miri) valid_miri_count=$((valid_miri_count + 1)) ;;
    esac
}

while IFS= read -r raw_line || [[ -n "$raw_line" ]]; do
    line="${raw_line%%#*}"
    line="$(trim "$line")"
    [[ -z "$line" ]] && continue

    if [[ "$line" == "[[waiver]]" ]]; then
        validate_row
        row_number=$((row_number + 1))
        reset_row
        continue
    fi

    [[ "$row_number" -gt 0 ]] || die "waiver key outside [[waiver]] table: ${line}"
    [[ "$line" == *=* ]] || die "unparseable waiver line in row ${row_number}: ${line}"

    key="$(trim "${line%%=*}")"
    value="$(strip_quotes "$(trim "${line#*=}")")"

    case "$key" in
        axis) current_axis="$value" ;;
        reason) current_reason="$value" ;;
        tracking) current_tracking="$value" ;;
        commit) current_commit="$value" ;;
        expires) current_expires="$value" ;;
        *) die "unsupported waiver key in row ${row_number}: ${key}" ;;
    esac
done < "$waiver_file"

validate_row

for axis in "${REQUIRED_AXES[@]}"; do
    case "$axis" in
        tsan) count="$valid_tsan_count" ;;
        miri) count="$valid_miri_count" ;;
        *) die "internal error: unsupported required axis ${axis}" ;;
    esac
    case "$count" in
        0) die "missing valid ${axis} waiver for ${release_commit}" ;;
        1) ;;
        *) die "ambiguous ${axis} waivers for ${release_commit}" ;;
    esac
done

echo "sanitizer-gate: ASan passed; TSan and Miri waivers are valid for ${release_commit}"
