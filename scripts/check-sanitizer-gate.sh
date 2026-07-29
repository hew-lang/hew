#!/usr/bin/env bash
# Fail-closed release sanitizer evidence validator.
#
# Usage:
#   scripts/check-sanitizer-gate.sh <release-version> <asan-result-file> [ledger-file]
#
# ASan is an executed release gate and must contain exactly `asan=pass`.
# TSan and Miri are tracked as release-scoped behavioral limitations until
# their lanes can execute authoritatively. Each axis needs one bounded ledger
# row for this release with an observed behavior, rationale, tracking issue,
# owner, and expiry. Git identity is intentionally irrelevant: sanitizer
# behavior and the released version are the authority.

set -euo pipefail

readonly REQUIRED_AXES=(tsan miri)

die() {
    echo "sanitizer-gate: ERROR: $*" >&2
    exit 1
}

usage() {
    die "usage: $0 <release-version> <asan-result-file> [ledger-file]"
}

if [[ $# -lt 2 || $# -gt 3 ]]; then
    usage
fi

release_version="${1#v}"
asan_result_file="$2"
ledger_file="${3:-release-sanitizer-waiver.toml}"

if [[ ! "$release_version" =~ ^[0-9]+\.[0-9]+\.[0-9]+([.-][0-9A-Za-z][0-9A-Za-z.-]*)?$ ]]; then
    die "release version must be a concrete semantic version"
fi

[[ -f "$asan_result_file" ]] ||
    die "ASan result file is absent: ${asan_result_file}"

asan_status=""
while IFS= read -r line || [[ -n "$line" ]]; do
    line="${line%%#*}"
    line="${line#"${line%%[![:space:]]*}"}"
    line="${line%"${line##*[![:space:]]}"}"
    [[ -z "$line" ]] && continue
    [[ -z "$asan_status" ]] ||
        die "ASan result file is ambiguous: multiple result lines"
    asan_status="$line"
done < "$asan_result_file"

[[ "$asan_status" == "asan=pass" ]] ||
    die "ASan hard gate did not pass unambiguously"
[[ -f "$ledger_file" ]] ||
    die "sanitizer ledger is absent: ${ledger_file}"

today="$(date -u +%F)"
valid_tsan_count=0
valid_miri_count=0

current_axis=""
current_release=""
current_behavior=""
current_reason=""
current_tracking=""
current_owner=""
current_expires=""
current_keys=" "
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
    current_release=""
    current_behavior=""
    current_reason=""
    current_tracking=""
    current_owner=""
    current_expires=""
    current_keys=" "
}

axis_is_required() {
    local axis="$1"
    local required
    for required in "${REQUIRED_AXES[@]}"; do
        [[ "$axis" == "$required" ]] && return 0
    done
    return 1
}

require_substantive() {
    local field="$1"
    local value="$2"
    local minimum="$3"
    local normalized
    normalized="$(printf '%s' "$value" | tr '[:upper:]' '[:lower:]')"
    case "$normalized" in
        "*"|"all"|"n/a"|"na"|"none"|"unknown"|"tbd"|"todo"|"waived"|"not applicable")
            die "ledger row ${row_number} has vague ${field}: ${value}"
            ;;
    esac
    [[ "${#value}" -ge "$minimum" ]] ||
        die "ledger row ${row_number} ${field} is too vague"
}

validate_row() {
    [[ "$row_number" -eq 0 ]] && return 0

    [[ -n "$current_axis" ]] || die "ledger row ${row_number} missing axis"
    [[ -n "$current_release" ]] || die "ledger row ${row_number} missing release"
    [[ -n "$current_behavior" ]] || die "ledger row ${row_number} missing behavior"
    [[ -n "$current_reason" ]] || die "ledger row ${row_number} missing reason"
    [[ -n "$current_tracking" ]] || die "ledger row ${row_number} missing tracking"
    [[ -n "$current_owner" ]] || die "ledger row ${row_number} missing owner"
    [[ -n "$current_expires" ]] || die "ledger row ${row_number} missing expires"

    current_axis="$(printf '%s' "$current_axis" | tr '[:upper:]' '[:lower:]')"
    current_release="${current_release#v}"

    case "$current_axis" in
        "*"|"all") die "ledger row ${row_number} is a blanket entry" ;;
    esac
    axis_is_required "$current_axis" ||
        die "ledger row ${row_number} has unsupported axis: ${current_axis}"
    require_substantive behavior "$current_behavior" 16
    require_substantive reason "$current_reason" 16
    require_substantive owner "$current_owner" 3
    [[ "$current_release" =~ ^[0-9]+\.[0-9]+\.[0-9]+([.-][0-9A-Za-z][0-9A-Za-z.-]*)?$ ]] ||
        die "ledger row ${row_number} release is not a semantic version"
    [[ "$current_expires" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]] ||
        die "ledger row ${row_number} expires must be YYYY-MM-DD"
    [[ "$current_tracking" == https://* ]] ||
        die "ledger row ${row_number} tracking must be an https URL"

    # Historical or future versions may remain in the ledger. Only the
    # selected release is authoritative for this invocation.
    [[ "$current_release" == "$release_version" ]] || return 0

    [[ "$current_expires" > "$today" || "$current_expires" == "$today" ]] ||
        die "ledger row ${row_number} for ${current_axis} expired on ${current_expires}"

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

    [[ "$row_number" -gt 0 ]] ||
        die "ledger key outside [[waiver]] table: ${line}"
    [[ "$line" == *=* ]] ||
        die "unparseable ledger line in row ${row_number}: ${line}"

    key="$(trim "${line%%=*}")"
    value="$(strip_quotes "$(trim "${line#*=}")")"

    case "$current_keys" in
        *" ${key} "*) die "ledger row ${row_number} repeats key: ${key}" ;;
    esac
    current_keys="${current_keys}${key} "

    case "$key" in
        axis) current_axis="$value" ;;
        release) current_release="$value" ;;
        behavior) current_behavior="$value" ;;
        reason) current_reason="$value" ;;
        tracking) current_tracking="$value" ;;
        owner) current_owner="$value" ;;
        expires) current_expires="$value" ;;
        *) die "unsupported ledger key in row ${row_number}: ${key}" ;;
    esac
done < "$ledger_file"

validate_row

for axis in "${REQUIRED_AXES[@]}"; do
    case "$axis" in
        tsan) count="$valid_tsan_count" ;;
        miri) count="$valid_miri_count" ;;
    esac
    case "$count" in
        0) die "missing ${axis} behavioral ledger entry for release ${release_version}" ;;
        1) ;;
        *) die "ambiguous ${axis} behavioral ledger entries for release ${release_version}" ;;
    esac
done

echo "sanitizer-gate: ASan passed; TSan and Miri behavioral ledger entries are valid for v${release_version}"
