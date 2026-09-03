#!/usr/bin/env bash
# repros/journeys/day-one.sh
# Day one: a machine with `hew` and a C driver (the one dependency §6.1 names)
# gets a green, testable, deployable project in six commands.
# Source: hew-orchestration/plans/hew-platform-program.md §0.1 (the bash
# fence starting at that document's line 145), copied verbatim except for
# the step-reporting harness described below (V060-FD-1).
#
# Run: make test-journeys JOURNEY=day-one   (HEW_BIN on PATH, TMPDIR outside any hew root)
# On the windows-2022 CI job, which has no make, run: bash repros/journeys/day-one.sh
#
# Every assertion is a numbered step (`day-one.<n>`), printed to stdout as
# `step day-one.<n>: pass` or `step day-one.<n>: fail`. A failing step is
# recorded and the script continues; scripts/journeys-expected.tsv is the
# ratchet that decides whether a given failure is already known. No `-e`:
# a failure here must never abort the script before every step is reported.
set -uo pipefail
HEW=${HEW_BIN:-hew}
WORK=$(mktemp -d "${TMPDIR:?TMPDIR must point outside the checkout}/journey-day-one-XXXXXX") || exit 1
trap 'rm -rf "$WORK"' EXIT
cd "$WORK" || exit 1

NAME=day-one
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

# A hang is a failure, not a wait: run a command under a watchdog (bash only; no coreutils `timeout`).
with_timeout() {
    local secs=$1
    shift
    "$@" &
    local pid=$!
    (
        sleep "$secs"
        kill "$pid" 2>/dev/null
    ) &
    local wd=$!
    wait "$pid"
    local rc=$?
    kill "$wd" 2>/dev/null
    wait "$wd" 2>/dev/null || true
    return "$rc"
}
# `producer | grep -q` under pipefail fails whenever the producer writes after the
# match (SIGPIPE), so every assertion captures first and greps the capture.

# 1. Scaffold. `hew new <name>` is the verb; `hew init` stays for in-place. Like
#    `cargo new`, it initializes a repository unless it is already inside one.
if "$HEW" new svc >"$WORK/.log.new-svc" 2>&1; then ok; else bad "hew new: $(cat "$WORK/.log.new-svc")"; fi
cd svc 2>/dev/null || true # a failed cd leaves later checks in $WORK, where each still reports its own fail
if [ -f main_test.hew ]; then ok; else bad "scaffold ships a test file"; fi
if [ "$(git rev-parse --show-toplevel 2>/dev/null)" = "$PWD" ]; then ok; else bad "hew new initializes a repository of its own (TMPDIR under a repository defeats this step)"; fi
if grep -qx 'target/' .gitignore 2>/dev/null; then ok; else bad "artifact directory is ignored by the scaffold"; fi

# 2. Bare run / test / build are manifest-aware and green.
OUT=$("$HEW" run 2>&1)
if echo "$OUT" | grep -q 'Hello from svc'; then ok; else bad "hew run: $OUT"; fi
OUT=$("$HEW" test 2>&1)
if echo "$OUT" | grep -q '1 passed; 0 failed'; then ok; else bad "scaffold test is green: $OUT"; fi
# Build status lines go to stderr (§6.1), cargo's convention; stdout stays free for machine output.
BUILD_OUT=$("$HEW" build --release 2>&1)
if echo "$BUILD_OUT" | grep -q 'target/release/svc'; then ok; else bad "build names its artifact: $BUILD_OUT"; fi
if [ -x target/release/svc ]; then ok; else bad "artifact is where build said"; fi
OUT=$(./target/release/svc 2>&1)
if echo "$OUT" | grep -q 'Hello from svc'; then ok; else bad "release binary runs: $OUT"; fi

# 3. The binary never reaches git.
git add -A 2>/dev/null
if git ls-files 2>/dev/null | grep -q '^target/'; then bad "target/ was staged"; else ok; fi

# 4. Every scaffold variant compiles and tests green (a source-text pin cannot catch this).
#    The actor template's main sends three messages and exits (§6.1); a template that blocks fails here.
cd "$WORK" || true
if "$HEW" new --actor counter >"$WORK/.log.new-actor" 2>&1; then ok; else bad "actor scaffold: $(cat "$WORK/.log.new-actor")"; fi
if (cd counter 2>/dev/null && with_timeout 60 "$HEW" run >/dev/null 2>&1 && OUT=$("$HEW" test 2>&1) && echo "$OUT" | grep -q '1 passed; 0 failed'); then ok; else bad "actor scaffold is not green"; fi
if "$HEW" new --lib mylib >"$WORK/.log.new-lib" 2>&1; then ok; else bad "lib scaffold: $(cat "$WORK/.log.new-lib")"; fi
if (cd mylib 2>/dev/null && OUT=$("$HEW" test 2>&1) && echo "$OUT" | grep -q '1 passed; 0 failed'); then ok; else bad "lib scaffold is not green"; fi

# 5. The two most likely first mistakes get the right message, once.
cd "$WORK" || true
printf 'fn main() { let mut x = 1; println(f"{x}"); }\n' >letmut.hew
OUT=$("$HEW" check letmut.hew 2>&1)
RC=$?
if [ "$RC" -eq 1 ]; then ok; else bad "let mut exits 1 (got $RC)"; fi
if echo "$OUT" | grep -q 'var x'; then ok; else bad "let mut names the fix: $OUT"; fi
if [ "$(echo "$OUT" | grep -c 'error:')" -eq 1 ]; then ok; else bad "let mut is one error, not a cascade"; fi

# `hew check --format json` pretty-prints: the key/value pair carries a space.
printf 'import std.htp;\nfn main() { }\n' >badimport.hew
OUT=$("$HEW" check --format json badimport.hew 2>&1)
RC=$?
if [ "$RC" -eq 1 ]; then ok; else bad "module-not-found exits 1 (got $RC)"; fi
if echo "$OUT" | grep -q '"code": "E_MODULE_NOT_FOUND"'; then ok; else bad "module-not-found is a coded diagnostic: $OUT"; fi
if echo "$OUT" | grep -q '"start_line": 1'; then ok; else bad "module-not-found carries the import span: $OUT"; fi
if echo "$OUT" | grep -q 'std.net.http'; then ok; else bad "module-not-found suggests the nearest module"; fi

# 6. A missing C driver is named, with the package to install. On Windows the only driver is
#    clang.exe and the package is the LLVM release (link.rs:506-512, :544-547); `clang` matches there.
printf 'fn main() { println("hi"); }\n' >hello.hew
OUT=$(HEW_CC=/nonexistent/cc "$HEW" build hello.hew 2>&1)
RC=$?
if [ "$RC" -eq 1 ]; then ok; else bad "missing C driver exits 1 (got $RC)"; fi
if echo "$OUT" | grep -q '/nonexistent/cc'; then ok; else bad "missing C driver is named: $OUT"; fi
if echo "$OUT" | grep -qiE 'clang|gcc|build-essential|xcode'; then ok; else bad "missing C driver names the package: $OUT"; fi

# 7. The three diagnostic channels are distinguishable by exit code and prefix.
printf 'fn main() { let x: i64 = "s"; }\n' >user.hew
"$HEW" check user.hew >/dev/null 2>&1
RC=$?
if [ "$RC" -eq 1 ]; then ok; else bad "a user error exits 1 (got $RC)"; fi
# D9 freezes the rule; V060-DIAG ships the refusal as E_LIMIT_MAIN_CONTEXT, whose text
# names the actor-hosted loop (the Book's first service) and `join {}` (the fan-out from main).
printf 'fn main() { scope { fork { println("x"); } } }\n' >limit.hew
OUT=$("$HEW" check limit.hew 2>&1)
RC=$?
if [ "$RC" -eq 3 ]; then ok; else bad "a compiler limitation exits 3 (got $RC): $OUT"; fi
if echo "$OUT" | grep -q 'compiler limitation:'; then ok; else bad "limitation channel is prefixed: $OUT"; fi
if echo "$OUT" | grep -q 'E_LIMIT_MAIN_CONTEXT'; then ok; else bad "the refusal carries its code: $OUT"; fi
if echo "$OUT" | grep -q 'actor'; then ok; else bad "the refusal names the actor-hosted loop: $OUT"; fi
if echo "$OUT" | grep -q 'join'; then ok; else bad "the refusal names the fan-out that works from main: $OUT"; fi

echo "day-one: ${N} steps reported"
exit 0
