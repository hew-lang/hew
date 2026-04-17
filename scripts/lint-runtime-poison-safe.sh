#!/usr/bin/env bash
# lint-runtime-poison-safe.sh
#
# Fail if any raw .lock() / .read() / .write() appears on a named runtime
# global that has been migrated to the PoisonSafe / PoisonSafeRw wrapper,
# and fail on the `if let Ok(g) = <STATIC>.lock()` anti-pattern (silent
# poison skip) anywhere in `hew-runtime/src/`.
#
# GLOBALS is the allowlist of migrated globals. Extend it — in one place —
# as each subsequent sweep lands (LIVE_ACTORS, MONITOR_TABLE,
# TOP_LEVEL_SUPERVISORS, ...). Never delete an entry from this list; once
# a global is wrapped it stays wrapped.
set -euo pipefail

SRC="hew-runtime/src"
GLOBALS='LINK_TABLE|ENV_LOCK'

fail=0

# Pattern 1: raw Mutex/RwLock method on a poison-wrapped global,
# including optional index expression (e.g. LINK_TABLE[i].read()).
if grep -rnE "(${GLOBALS})(\[[^]]*\])?\.(lock|read|write)\(\)" "$SRC"; then
    echo "lint-runtime-poison-safe: raw .lock()/.read()/.write() on a PoisonSafe-wrapped global"
    echo "  use .access(|g| ..) / .read_access(|g| ..) / .try_access(|g| ..) instead"
    fail=1
fi

# Pattern 2: the silent-skip anti-pattern `if let Ok(_) = X.lock()` anywhere
# in the runtime source — match ANY named-global identifier, not only the
# allowlist, because this pattern is never acceptable.
if grep -rnE 'if let Ok\([^)]+\)\s*=\s*[A-Z_][A-Z0-9_]*(\[[^]]*\])?\.(lock|read|write)\(\)' "$SRC"; then
    echo "lint-runtime-poison-safe: if-let-Ok on a lock silently skips poison"
    echo "  wrap the global in PoisonSafe/PoisonSafeRw and use .access/.read_access"
    fail=1
fi

if [ "$fail" -ne 0 ]; then
    exit 1
fi

echo "lint-runtime-poison-safe: clean"
