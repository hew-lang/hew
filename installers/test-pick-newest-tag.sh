#!/bin/sh
# Exercise the installer's numeric release ordering without network access.
set -eu

SCRIPT_DIR="$(cd -- "$(dirname -- "$0")" && pwd)"

# install.sh runs main unconditionally; load only its actual picker.
picker="$(awk '/^pick_newest_tag\(\) \{/,/^}/' "$SCRIPT_DIR/install.sh")"
[ -n "$picker" ] || {
    echo "FAIL: installer picker not found" >&2
    exit 1
}
eval "$picker"

check() {
    expected="$1"
    shift
    actual="$(printf '%s\n' "$@" | pick_newest_tag)"
    if [ "$actual" != "$expected" ]; then
        echo "FAIL: tags [$*]: expected $expected, got $actual" >&2
        exit 1
    fi
}

check 0.10.0 0.9.9 0.10.0
check 0.6.0 0.6.0 0.6.0-rc3
check 0.6.0-rc10 0.6.0-rc9 0.6.0-rc10
check 0.6.0-rc2 0.6.0-rc2 0.5.6
printf '%s\n' 'Installer release ordering: PASS'
