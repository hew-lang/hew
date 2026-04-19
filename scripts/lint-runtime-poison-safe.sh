#!/usr/bin/env bash
# lint-runtime-poison-safe.sh
#
# Fail if any raw .lock() / .read() / .write() (or try_* / *_or_recover
# variants) appears on a named runtime global that has been migrated to the
# PoisonSafe / PoisonSafeRw wrapper, and fail on the
# `if let Ok(g) = <STATIC>.lock()` anti-pattern (silent poison skip) anywhere
# in `hew-runtime/src/`.
#
# GLOBALS is the allowlist of migrated globals. Extend it — in one place —
# as each subsequent sweep lands (LIVE_ACTORS, MONITOR_TABLE,
# TOP_LEVEL_SUPERVISORS, ...). Never delete an entry from this list; once
# a global is wrapped it stays wrapped.
set -euo pipefail

# Capture the absolute path to this script before any cd happens.
_SELF=$(cd "$(dirname "$0")" && pwd)/$(basename "$0")

# ── Self-test mode ───────────────────────────────────────────────────────────
# Run with --self-test to verify that every pattern variant is detected.
# Creates a temporary directory, writes synthetic violations, and asserts this
# script exits non-zero for each one.  Exits 0 if all assertions pass.
if [ "${1-}" = "--self-test" ]; then
    _tmpdir=$(mktemp -d)
    trap 'rm -rf "$_tmpdir"' EXIT

    _pass=0
    _fail=0

    # Helper: assert the script exits non-zero when pointed at a dir containing
    # the given file content.
    assert_detects() {
        local desc="$1"
        local content="$2"
        mkdir -p "$_tmpdir/hew-runtime/src"
        printf '%s\n' "$content" > "$_tmpdir/hew-runtime/src/probe.rs"
        # Run the real lint from the temp dir so SRC resolves correctly.
        if (cd "$_tmpdir" && bash "$_SELF" 2>/dev/null); then
            echo "FAIL: $desc — expected non-zero exit but got zero"
            _fail=$((_fail + 1))
        else
            echo "PASS: $desc"
            _pass=$((_pass + 1))
        fi
        rm -f "$_tmpdir/hew-runtime/src/probe.rs"
    }

    # Pattern 1 — indexed-global branch (exercises the (\[[^]]*\])? part of the regex)
    assert_detects "P1 LINK_TABLE[i].lock()"    'let g = LINK_TABLE[i].lock();'
    # Pattern 1 — original verbs
    assert_detects "P1 LINK_TABLE.lock()"       'let g = LINK_TABLE.lock();'
    assert_detects "P1 LINK_TABLE.read()"       'let g = LINK_TABLE.read();'
    assert_detects "P1 LINK_TABLE.write()"      'let g = LINK_TABLE.write();'
    assert_detects "P1 ENV_LOCK.lock()"         'let g = ENV_LOCK.lock();'
    # Pattern 1 — extended verbs
    assert_detects "P1 LINK_TABLE.try_lock()"           'let g = LINK_TABLE.try_lock();'
    assert_detects "P1 LINK_TABLE.try_read()"           'let g = LINK_TABLE.try_read();'
    assert_detects "P1 LINK_TABLE.try_write()"          'let g = LINK_TABLE.try_write();'
    assert_detects "P1 LINK_TABLE.lock_or_recover()"    'let g = LINK_TABLE.lock_or_recover();'
    assert_detects "P1 LINK_TABLE.read_or_recover()"    'let g = LINK_TABLE.read_or_recover();'
    assert_detects "P1 LINK_TABLE.write_or_recover()"   'let g = LINK_TABLE.write_or_recover();'
    # Pattern 2 — silent-skip anti-pattern
    assert_detects "P2 if-let-Ok .lock()"   'if let Ok(g) = SOME_GLOBAL.lock() { }'
    assert_detects "P2 if-let-Ok .read()"   'if let Ok(g) = SOME_GLOBAL.read() { }'
    assert_detects "P2 if-let-Ok .write()"  'if let Ok(g) = SOME_GLOBAL.write() { }'
    assert_detects "P2 if-let-Ok .try_lock()"          'if let Ok(g) = SOME_GLOBAL.try_lock() { }'
    assert_detects "P2 if-let-Ok .try_read()"          'if let Ok(g) = SOME_GLOBAL.try_read() { }'
    assert_detects "P2 if-let-Ok .try_write()"         'if let Ok(g) = SOME_GLOBAL.try_write() { }'
    assert_detects "P2 if-let-Ok .lock_or_recover()"   'if let Ok(g) = SOME_GLOBAL.lock_or_recover() { }'
    assert_detects "P2 if-let-Ok .read_or_recover()"   'if let Ok(g) = SOME_GLOBAL.read_or_recover() { }'
    assert_detects "P2 if-let-Ok .write_or_recover()"  'if let Ok(g) = SOME_GLOBAL.write_or_recover() { }'

    echo ""
    echo "lint-runtime-poison-safe self-test: ${_pass} passed, ${_fail} failed"
    [ "$_fail" -eq 0 ]
    exit $?
fi
# ── End self-test mode ───────────────────────────────────────────────────────

SRC="hew-runtime/src"
# Migrated globals (must remain listed; never delete an entry):
#   LINK_TABLE, ENV_LOCK — landed in #1225
#   LIVE_ACTORS, DEFERRED_ACTOR_FREE_THREADS, MONITOR_TABLE — Stage 3
#   KNOWN_NODES, CURRENT_NODE — Stage 3 part 2
#   TOP_LEVEL_SUPERVISORS — Stage 5
#   TCP_API_STATE — Stage 5 continuation
#   ACTIVATE_PRE_REENQUEUE_HOOK — scheduler (#[cfg(test)] static)
# Note: Scheduler::worker_handles, HewConnMgr::connections, and
# HewConnMgr::reconnect_workers are struct fields, not named statics;
# they are not tracked here but are fully converted to PoisonSafe.
GLOBALS='LINK_TABLE|ENV_LOCK|LIVE_ACTORS|DEFERRED_ACTOR_FREE_THREADS|MONITOR_TABLE|KNOWN_NODES|CURRENT_NODE|TOP_LEVEL_SUPERVISORS|TCP_API_STATE|ACTIVATE_PRE_REENQUEUE_HOOK'

# All raw locking method variants that bypass PoisonSafe/PoisonSafeRw.
LOCK_METHODS='lock|read|write|try_lock|try_read|try_write|lock_or_recover|read_or_recover|write_or_recover'

fail=0

# Pattern 1: raw Mutex/RwLock method on a poison-wrapped global,
# including optional index expression (e.g. LINK_TABLE[i].read()).
# Catches plain .lock()/.read()/.write() and the try_* / *_or_recover variants.
if grep -rnE "(${GLOBALS})(\[[^]]*\])?\.(${LOCK_METHODS})\(\)" "$SRC"; then
    echo "lint-runtime-poison-safe: raw lock/read/write (or try_*/or_recover variant) on a PoisonSafe-wrapped global"
    echo "  use .access(|g| ..) / .read_access(|g| ..) / .try_access(|g| ..) instead"
    fail=1
fi

# Pattern 2: the silent-skip anti-pattern `if let Ok(_) = X.lock()` anywhere
# in the runtime source — match ANY named-global identifier, not only the
# allowlist, because this pattern is never acceptable.
# Also catches the try_* / *_or_recover variants of the same anti-pattern.
if grep -rnE 'if let Ok\([^)]+\)\s*=\s*[A-Z_][A-Z0-9_]*(\[[^]]*\])?\.('"${LOCK_METHODS}"')\(\)' "$SRC"; then
    echo "lint-runtime-poison-safe: if-let-Ok on a lock silently skips poison"
    echo "  wrap the global in PoisonSafe/PoisonSafeRw and use .access/.read_access"
    fail=1
fi

if [ "$fail" -ne 0 ]; then
    exit 1
fi

echo "lint-runtime-poison-safe: clean"
