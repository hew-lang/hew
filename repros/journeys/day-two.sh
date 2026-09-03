#!/usr/bin/env bash
# repros/journeys/day-two.sh
# Day two: a crash says where; a dependency installs from a clean cache and a
# wrong one is refused at the manifest; the release binary deploys as one
# static file.
# Source: hew-orchestration/plans/hew-platform-program.md §0.1 (the bash
# fence starting at that document's line 253), copied verbatim except for
# the step-reporting harness described below (V060-FD-1).
#
# Run: make test-journeys JOURNEY=day-two   (HEW_BIN on PATH, TMPDIR outside any hew root)
# The registry-dependent steps (search/add/install/info) run against a local
# registry started by scripts/registry-harness.sh, which exports HEW_REGISTRY;
# scripts/run-journeys.sh routes this script through that harness. When no
# registry is reachable, those steps fail closed instead of hanging (below).
# Day two on Windows is tier 2 until scripts/registry-harness.sh runs on the windows-2022 job (§0.1).
set -uo pipefail
HEW=${HEW_BIN:-hew}
WORK=$(mktemp -d "${TMPDIR:?TMPDIR must point outside the checkout}/journey-day-two-XXXXXX") || exit 1
trap 'rm -rf "$WORK"' EXIT
cd "$WORK" || exit 1

NAME=day-two
N=0
ok() {
    N=$((N + 1))
    echo "step ${NAME}.${N}: pass"
}
bad() {
    N=$((N + 1))
    echo "step ${NAME}.${N}: fail"
    echo "${NAME}: step ${N} FAIL: $*" >&2
}
# A registry-touching step that can't even attempt its command when no
# registry is reachable (avoids hanging on a dead endpoint).
no_registry() { bad "$* (HEW_REGISTRY not set: scripts/registry-harness.sh found no local registry)"; }
# Every assertion captures first and greps the capture (pipefail + `grep -q` is a false failure).

# 1. A trap names file, line, and function; HEW_BACKTRACE=1 lists frames;
#    release builds keep line tables. Exit code stays 1 (HEW-SPEC-2026 §5.8).
#    On Windows the reader is dbghelp over the PDB; the assertions are the same (§6.1).
cat >crash.hew <<'HEW'
fn boom(v: Vec<i64>, i: i64) -> i64 {
    v[i]
}
fn main() { let v: Vec<i64> = [1, 2, 3]; println(f"{boom(v, 5)}"); }
HEW
OUT=$("$HEW" run crash.hew 2>&1)
RC=$?
if [ "$RC" -eq 1 ]; then ok; else bad "trap exits 1 (got $RC)"; fi
if echo "$OUT" | grep -q 'IndexOutOfBounds'; then ok; else bad "trap kind is named: $OUT"; fi
if echo "$OUT" | grep -q 'crash.hew:2'; then ok; else bad "trap names file:line: $OUT"; fi
if echo "$OUT" | grep -q 'boom'; then ok; else bad "trap names the function: $OUT"; fi
OUT=$(HEW_BACKTRACE=1 "$HEW" run crash.hew 2>&1)
if echo "$OUT" | grep -q 'main (crash.hew:4)'; then ok; else bad "backtrace lists the caller frame: $OUT"; fi
if "$HEW" build --release crash.hew -o crash_rel >"$WORK/.log.build-crash" 2>&1; then ok; else bad "release build of crash.hew: $(cat "$WORK/.log.build-crash")"; fi
OUT=$(./crash_rel 2>&1)
if echo "$OUT" | grep -q 'crash.hew:2'; then ok; else bad "release trap still names file:line: $OUT"; fi

# 2. Add a dependency from a clean package cache; refuse a wrong one at the manifest.
export HEW_HOME="$WORK/hew-home"
if "$HEW" new app >"$WORK/.log.new-app" 2>&1 && cd app; then ok; else bad "hew new app: $(cat "$WORK/.log.new-app")"; fi
if [ -n "${HEW_REGISTRY:-}" ]; then
    OUT=$("$HEW" search stats 2>&1)
    if echo "$OUT" | grep -q 'hew.math.stats'; then ok; else bad "search prints the dotted name add accepts: $OUT"; fi
else
    no_registry "search prints the dotted name add accepts"
fi
if [ -n "${HEW_REGISTRY:-}" ]; then
    if "$HEW" add hew.math.stats >"$WORK/.log.add-stats" 2>&1; then ok; else bad "add resolves against the registry: $(cat "$WORK/.log.add-stats")"; fi
else
    no_registry "add resolves against the registry"
fi
if grep -q '"hew.math.stats"' hew.toml 2>/dev/null; then ok; else bad "add wrote the quoted dotted key"; fi
if [ -n "${HEW_REGISTRY:-}" ]; then
    "$HEW" add hew.does.not.exist >/dev/null 2>&1
    RC=$?
    if [ "$RC" -eq 1 ]; then ok; else bad "unknown package is refused at add with exit 1 (got $RC)"; fi
else
    no_registry "unknown package is refused at add with exit 1"
fi
if grep -q 'does.not.exist' hew.toml 2>/dev/null; then bad "unknown package must not reach the manifest"; else ok; fi
# An unquoted dotted key is a nested TOML table, which today reads as a dependency on `hew@*`
# (fable3/probes.md); the manifest refuses it and names the quoted form (§6.2).
printf '\n[dev-dependencies]\nhew.math.stats = "0.3"\n' >>hew.toml
OUT=$("$HEW" check 2>&1)
RC=$?
if [ "$RC" -eq 1 ]; then ok; else bad "unquoted dotted dependency key is refused (got $RC): $OUT"; fi
if echo "$OUT" | grep -q '"hew.math.stats"'; then ok; else bad "manifest refusal names the quoted form: $OUT"; fi
sed -i.bak '/dev-dependencies/,$d' hew.toml && rm -f hew.toml.bak
if [ -n "${HEW_REGISTRY:-}" ]; then
    if "$HEW" install >"$WORK/.log.install" 2>&1; then ok; else bad "install fetches from the registry: $(cat "$WORK/.log.install")"; fi
else
    no_registry "install fetches from the registry"
fi
if [ -f hew.lock ]; then ok; else bad "install writes hew.lock"; fi
cat >main.hew <<'HEW'
import hew.math.stats as stats;
fn main() -> Result<(), stats.StatsError> {
    let m = stats.mean([1.0, 2.0, 3.0])?;
    println(f"mean={m}");
    .Ok(())
}
HEW
if [ -n "${HEW_REGISTRY:-}" ]; then
    OUT=$("$HEW" run 2>&1)
    if echo "$OUT" | grep -q 'mean=2'; then ok; else bad "dependency is usable from main: $OUT"; fi
else
    no_registry "dependency is usable from main"
fi
cat >main_test.hew <<'HEW'
import std.testing;
import hew.math.stats as stats;
#[test]
fn mean_of_three() { testing.assert_eq(stats.mean([1.0, 2.0, 3.0]).unwrap(), 2.0); }
HEW
if [ -n "${HEW_REGISTRY:-}" ]; then
    OUT=$("$HEW" test 2>&1)
    if echo "$OUT" | grep -q '1 passed; 0 failed'; then ok; else bad "tests see dependencies: $OUT"; fi
else
    no_registry "tests see dependencies"
fi
if [ -n "${HEW_REGISTRY:-}" ]; then
    "$HEW" info hew.nope >/dev/null 2>&1
    RC=$?
    if [ "$RC" -eq 1 ]; then ok; else bad "info not-found is exit 1, a data answer, not usage exit 2 (got $RC)"; fi
else
    no_registry "info not-found is exit 1, a data answer, not usage exit 2"
fi

# 3. A main that returns Err prints the error and exits 1 (today: silent).
#    D4: main's error type implements Error; Display supplies the text.
cat >mainerr.hew <<'HEW'
enum AErr { Bad }
impl Display for AErr { fn fmt(self) -> string { "bad thing" } }
impl Error for AErr {}
fn f() -> Result<(), AErr> { .Err(AErr.Bad) }
fn main() -> Result<(), AErr> { f()?; .Ok(()) }
HEW
OUT=$("$HEW" run mainerr.hew 2>&1)
RC=$?
if [ "$RC" -eq 1 ]; then ok; else bad "main returning Err exits 1 (got $RC)"; fi
if echo "$OUT" | grep -q 'error: bad thing'; then ok; else bad "main returning Err prints the Display text: $OUT"; fi

# 4. Deploy: one static binary; cross-object emission in both modes; container smoke where docker exists.
BUILD_OUT=$("$HEW" build --release 2>&1)
if echo "$BUILD_OUT" | grep -q 'target/release/app'; then ok; else bad "build names its artifact: $BUILD_OUT"; fi
if command -v ldd >/dev/null 2>&1; then
    EXTRA=$(ldd target/release/app 2>/dev/null | grep -vE 'linux-vdso|libc\.so|libm\.so|libgcc_s|ld-linux|libpthread|libdl' || true)
    if [ -z "$EXTRA" ]; then ok; else bad "release binary links beyond libc/libm/libgcc: $EXTRA"; fi
else
    ok # ldd unavailable on this host; nothing to assert
fi
# `-o` is honoured from inside a package (today: dropped silently, main.o appears) ...
if "$HEW" build --release --target aarch64-unknown-linux-gnu --emit-obj -o app-aarch64.o >"$WORK/.log.obj-pkg" 2>&1; then ok; else bad "cross object emission: $(cat "$WORK/.log.obj-pkg")"; fi
if [ -f app-aarch64.o ]; then ok; else bad "cross object emission wrote the named file"; fi
if [ -f main.o ]; then bad "cross object emission must not also write main.o"; else ok; fi
if command -v file >/dev/null 2>&1; then
    if [ -f app-aarch64.o ] && file app-aarch64.o | grep -qi 'aarch64'; then ok; else bad "cross object is not aarch64: $(file app-aarch64.o 2>&1)"; fi
else
    ok # `file` unavailable on this host; nothing to assert
fi
# ... and in file mode (today: `--emit-obj hello.hew -o x.o` writes hello.o and drops -o; a binary -o works).
cd "$WORK" || true
printf 'fn main() { println("hi"); }\n' >obj.hew
if "$HEW" build --release --target aarch64-unknown-linux-gnu --emit-obj obj.hew -o obj-aarch64.o >"$WORK/.log.obj-file" 2>&1; then ok; else bad "file-mode cross object emission: $(cat "$WORK/.log.obj-file")"; fi
if [ -f obj-aarch64.o ]; then ok; else bad "file-mode -o was dropped"; fi
if [ -f obj.o ]; then bad "file-mode cross object emission must not write obj.o"; else ok; fi
cd app 2>/dev/null || true # a failed cd leaves the docker guard below false (no target/release), which skips cleanly
# Guard on target/release existing before invoking docker: a `-v` bind mount
# of a path that is not there yet is auto-created by the docker daemon as
# root (observed live during grounding), which a later `rm -rf "$WORK"`
# then cannot clean up. Skip cleanly instead of leaving root-owned garbage.
if [ -d target/release ] && command -v docker >/dev/null 2>&1 && docker info >/dev/null 2>&1; then
    OUT=$(docker run --rm -v "$PWD/target/release:/app:ro" debian:bookworm-slim /app/app 2>&1)
    if echo "$OUT" | grep -q 'mean=2'; then ok; else bad "container run: $OUT"; fi
else
    ok # docker unavailable, or target/release was never built; the static-link check above stands in
fi

echo "day-two: ${N} steps reported"
exit 0
