#!/usr/bin/env bash
# Ubuntu 24.04 installation verification script.
# Tests that all expected files are in place and the full compile+run pipeline works.
set -euo pipefail

echo "==> Verifying hew installation on Ubuntu 24.04"
echo ""

# 1. Binaries
echo "--- Binaries"
test -x /usr/local/bin/hew && echo "  hew: ok"

# 2. Combined library
echo ""
echo "--- Combined library"
test -f /usr/local/lib/libhew.a && echo "  libhew.a: ok"

# 3. Stdlib sources
echo ""
echo "--- Stdlib sources"
test -f /usr/local/share/hew/std/string.hew && echo "  std/string.hew: ok"
test -f /usr/local/share/hew/std/fs.hew && echo "  std/fs.hew: ok"
test -d /usr/local/share/hew/std/net && echo "  std/net/: ok"
hew_count=$(find /usr/local/share/hew/std -name "*.hew" | wc -l)
echo "  total .hew files: $hew_count"

# 4. Full compile + link + run via hew run
# This exercises: module resolution → type-check → hew-codegen (MLIR) → C linker → execute
echo ""
echo "--- Full compile + run (hew run)"
/usr/local/bin/hew run /work/test.hew
echo "  hew run: ok"

echo ""
echo "==> All checks passed on Ubuntu 24.04."
