#!/bin/sh
# Fixture for install.sh's pick_newest_tag() semver picker and the --stable
# pre-release filter that feeds it.
#
# installers/docker/test-install.sh is the precedent for a standalone shell
# test living in this directory; this one exercises the tag-comparison logic
# in isolation instead of a full install, so a regression is caught without
# downloading a release or hitting the GitHub API.
set -eu

SCRIPT_DIR="$(cd -- "$(dirname -- "$0")" && pwd)"
INSTALL_SH="${SCRIPT_DIR}/install.sh"

# install.sh unconditionally runs `main "$@"` at end of file, so it cannot be
# sourced directly — extract just the pick_newest_tag() function body.
pick_newest_tag_def="$(awk '/^pick_newest_tag\(\) \{/,/^}/' "$INSTALL_SH")"
if [ -z "$pick_newest_tag_def" ]; then
    echo "FAIL: could not extract pick_newest_tag() from ${INSTALL_SH}" >&2
    exit 1
fi
eval "$pick_newest_tag_def"

fail=0

# Default (rc-inclusive) path: pick_newest_tag() over the raw tag list.
check() {
    description="$1"
    input="$2"
    expected="$3"
    actual="$(printf '%s\n' "$input" | pick_newest_tag)"
    if [ "$actual" != "$expected" ]; then
        echo "FAIL: ${description}"
        echo "  expected: ${expected}"
        echo "  got:      ${actual}"
        fail=1
    else
        echo "ok: ${description}"
    fi
}

# --stable path: resolve_version()'s own filter (grep -v -- '-') ahead of
# pick_newest_tag(), so this is a faithful negative control rather than a
# reimplementation of the filter.
check_stable() {
    description="$1"
    input="$2"
    expected="$3"
    actual="$(printf '%s\n' "$input" | grep -v -- '-' | pick_newest_tag)"
    if [ "$actual" != "$expected" ]; then
        echo "FAIL: ${description}"
        echo "  expected: ${expected}"
        echo "  got:      ${actual}"
        fail=1
    else
        echo "ok: ${description}"
    fi
}

# 0.10.0 beats 0.9.9 — catches lexical rather than numeric comparison, the
# reason the %06d padding exists.
check "numeric minor beats a lexically-larger patch" \
    "0.9.9
0.10.0" \
    "0.10.0"

# 0.6.0 beats 0.6.0-rc3 — catches the final-release rc=999999 sentinel being
# dropped or inverted.
check "final release beats an rc of the same version" \
    "0.6.0-rc3
0.6.0" \
    "0.6.0"

# 0.6.0-rc10 beats 0.6.0-rc9 — catches substr(\$4, 3) losing its + 0 numeric
# coercion and comparing "10" < "9" lexically.
check "rc10 beats rc9" \
    "0.6.0-rc9
0.6.0-rc10" \
    "0.6.0-rc10"

# 0.6.0-rc2 beats 0.5.6 — catches an over-eager stable filter leaking onto
# the default (rc-inclusive) path.
check "rc beats an older final release on the default path" \
    "0.5.6
0.6.0-rc2" \
    "0.6.0-rc2"

# --stable drops every rc and keeps the newest final — the negative control
# for the grep -v -- '-' filter.
check_stable "--stable returns the newest final, not the newer rc" \
    "0.5.6
0.6.0-rc2" \
    "0.5.6"

echo
if [ "$fail" -eq 0 ]; then
    echo "All pick_newest_tag() cases passed."
else
    echo "pick_newest_tag() regression detected." >&2
    exit 1
fi
