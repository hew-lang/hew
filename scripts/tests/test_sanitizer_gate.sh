#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$root"

version="$(sed -n 's/^version = "\(.*\)"/\1/p' Cargo.toml | head -1)"
fixture="scripts/fixtures/sanitizer-gate"
pass=0
fail=0

expect_reject() {
    local name="$1" asan_file="$2" waiver_file="$3"
    if scripts/check-sanitizer-gate.sh "$version" "$asan_file" "$waiver_file"; then
        echo "FAIL $name: expected reject"
        fail=$((fail + 1))
    else
        echo "ok $name: rejected"
        pass=$((pass + 1))
    fi
}

expect_accept() {
    local name="$1" asan_file="$2" waiver_file="$3"
    if scripts/check-sanitizer-gate.sh "$version" "$asan_file" "$waiver_file"; then
        echo "ok $name: accepted"
        pass=$((pass + 1))
    else
        echo "FAIL $name: expected accept"
        fail=$((fail + 1))
    fi
}

expect_reject "1 no ASan result" "$fixture/missing.result" "$fixture/waivers/valid.toml"
expect_reject "2 ASan red" "$fixture/asan-fail.result" "$fixture/waivers/valid.toml"
expect_reject "3 ASan ambiguous/skipped" "$fixture/asan-ambiguous.result" "$fixture/waivers/valid.toml"
expect_reject "4 missing TSan/Miri evidence" "$fixture/asan-pass.result" "$fixture/waivers/none.toml"
expect_reject "5 evidence for different release" "$fixture/asan-pass.result" "$fixture/waivers/different-release.toml"
expect_reject "6 expired evidence" "$fixture/asan-pass.result" "$fixture/waivers/expired.toml"
expect_reject "7 blanket evidence" "$fixture/asan-pass.result" "$fixture/waivers/blanket.toml"
expect_reject "8 missing behavior" "$fixture/asan-pass.result" "$fixture/waivers/missing-field.toml"
expect_reject "9 duplicate axis evidence" "$fixture/asan-pass.result" "$fixture/waivers/duplicate.toml"
expect_reject "10 untracked evidence" "$fixture/asan-pass.result" "$fixture/waivers/bad-tracking.toml"
expect_reject "11 duplicate ledger key" "$fixture/asan-pass.result" "$fixture/waivers/duplicate-key.toml"
expect_reject "12 vague behavioral evidence" "$fixture/asan-pass.result" "$fixture/waivers/vague.toml"
expect_accept "13 ASan green with bounded release evidence" "$fixture/asan-pass.result" "$fixture/waivers/valid.toml"

echo "$pass sanitizer gate cases passed, $fail failed"
test "$fail" -eq 0
