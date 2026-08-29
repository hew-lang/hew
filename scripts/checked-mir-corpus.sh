#!/usr/bin/env bash
# Golden MIR corpus driver for examples/v05/checked-mir/.
#
# The corpus pins the textual `--dump-mir` output (raw + elab stages) for
# one fixture per compiler-known runtime-call family cluster. It is the
# behavioural oracle for internal retyping work: a refactor that claims
# "zero behaviour change" must leave every golden dump byte-identical.
# An INTENTIONAL dump change (e.g. a MIR carrier gaining a typed field
# that the Debug rendering prints) regenerates the golden in the same
# commit, with the diff justified in the commit body.
#
# Regeneration visibility: `golden` never overwrites silently. It diffs
# every dump against the committed golden first and prints a per-file
# CHANGED/NEW report with line counts, then rewrites
# `golden/MANIFEST.sha256`. `verify` re-checks that manifest against the
# goldens on disk, so a regeneration cannot land without touching one
# central, small, line-per-golden file that shows a reviewer exactly
# which goldens moved and how many — even when the .mir diffs themselves
# are collapsed.
#
# Dumping is not running. A fixture can segfault on every execution while
# every golden stays byte-identical, so `run` builds and executes the
# corpus and diffs a rendered transcript — exit status plus verbatim
# stdout — against the fixture's committed `<name>.expected` sibling.
#
# Which fixtures run is decided by the compiler, not by a list: a fixture
# is runnable exactly when its raw MIR declares a `main` entry point.
# `<name>.expected` is required for every fixture that has one and
# forbidden for every fixture that has not, so a fixture can neither
# arrive without an expectation nor keep a stale non-runnable
# classification after it grows a `main`.
#
# Usage:
#   scripts/checked-mir-corpus.sh golden   # (re)capture golden dumps
#   scripts/checked-mir-corpus.sh verify   # re-dump and diff against golden
#   scripts/checked-mir-corpus.sh run      # build + execute, diff transcripts
#   scripts/checked-mir-corpus.sh expect   # (re)capture .expected transcripts
#
# Env:
#   HEW_BIN — compiler binary (default: target/debug/hew at the repo root).
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
# shellcheck source=scripts/lib/corpus-nonempty.sh
# shellcheck disable=SC1091
source "$ROOT/scripts/lib/corpus-nonempty.sh"
CORPUS="$ROOT/examples/v05/checked-mir"
GOLDEN="$CORPUS/golden"
MANIFEST="$GOLDEN/MANIFEST.sha256"
HEW_BIN="${HEW_BIN:-$ROOT/target/debug/hew}"
MODE="${1:-verify}"
STAGES=(raw elab)
# Wall-clock cap per fixture. A fixture that stops terminating must fail
# the gate rather than hang the build; 124/137 land in the transcript and
# mismatch the committed exit status.
RUN_TIMEOUT_SECS="${CHECKED_MIR_RUN_TIMEOUT_SECS:-60}"
# Per-execution environment overlay consumed by execute_fixture; empty for
# every ordinary fixture run and set only by the leak-oracle counterfactual.
EXTRA_RUN_ENV=()

# sha256 over stdin-named files; `sha256sum` on Linux, `shasum -a 256` on
# macOS. Both print `<hash>  <name>`, so the manifest format is identical.
sha256_of() {
    if command -v sha256sum >/dev/null 2>&1; then
        sha256sum "$@"
    else
        shasum -a 256 "$@"
    fi
}

# Manifest lines for every committed golden, sorted byte-wise so the file
# is stable across platforms and locales.
render_manifest() {
    (
        cd "$GOLDEN"
        shopt -s nullglob
        local names=(*.mir)
        [[ ${#names[@]} -eq 0 ]] && return 0
        sha256_of "${names[@]}" | LC_ALL=C sort -k2
    )
}

if [[ ! -x "$HEW_BIN" ]]; then
    echo "checked-mir-corpus: compiler binary not found at $HEW_BIN" >&2
    echo "build it first (make hew) or set HEW_BIN" >&2
    exit 2
fi

resolve_timeout() {
    if command -v timeout >/dev/null 2>&1; then
        command -v timeout
    elif command -v gtimeout >/dev/null 2>&1; then
        command -v gtimeout
    else
        echo "checked-mir-corpus: GNU timeout is required (install coreutils)" >&2
        exit 127
    fi
}

# Statuses that mean the fixture died rather than returned: the fault
# signals a crash raises (128 + signo) plus the two `timeout` statuses.
# A `main` returning a large i64 is not a crash — POSIX truncates the
# return value to 8 bits, so ordinary fixtures can and do exit above 128.
is_crash_status() {
    case "$1" in
    124 | 137) return 0 ;;                         # timeout expired / SIGKILL after --kill-after
    132 | 133 | 134 | 136 | 138 | 139) return 0 ;; # ILL TRAP ABRT FPE BUS SEGV
    *) return 1 ;;
    esac
}

# Runnability is a structural fact the compiler reports, not a curated
# name list: the raw MIR of a fixture that can be linked into a program
# declares a `main` function. A fixture that only defines library items
# has no `main` in its MIR and cannot be executed at all. Reading it back
# from `--dump-mir raw` means the classification tracks the compiler on
# every run, including a fixture that gains or loses `main`.
#
# Returns 0 (has main), 1 (no main), or 2 (the compiler could not dump the
# fixture at all). The third case matters: a compiler that crashes while
# dumping must not be read as "this fixture has no main", or a crashing
# fixture would classify itself out of the gate — the defect this gate
# exists to catch.
fixture_has_main() {
    local fixture="$1" dump="$2"
    if ! "$HEW_BIN" compile --dump-mir raw "$fixture" >"$dump" 2>"$dump.err"; then
        return 2
    fi
    grep -qE '^fn main\(' "$dump"
}

# The transcript a fixture must reproduce: exit status plus verbatim
# stdout, or the compiler's structured refusal to build it. stderr is
# deliberately not pinned — the runtime emits timing-dependent shutdown
# diagnostics that would make the gate flaky on a loaded machine, and
# stdout plus exit status already fail on a crash, a wrong answer, or
# silence.
render_transcript() {
    local compile_status="$1" compile_log="$2" run_status="$3" stdout_path="$4" out="$5"
    if [[ "$compile_status" -ne 0 ]]; then
        local code
        code="$(grep -oE '\bE_[A-Z0-9_]+' "$compile_log" | head -1 || true)"
        printf 'compile-error: %s\n' "${code:-unclassified}" >"$out"
        return 0
    fi
    printf 'exit: %s\nstdout:\n' "$run_status" >"$out"
    cat "$stdout_path" >>"$out"
}

# Compile a fixture and, if it links, execute it under a wall-clock cap in
# a scratch working directory (fixtures that persist node identity files
# must not write into the checkout). Fills the caller's transcript path.
#
# Every execution runs with the runtime's actor-box balance check armed
# (HEW_ACTOR_LEAK_CHECK=1). Exit status and stdout cannot see a leaked
# actor — a fixture that never reclaims one still prints the right thing
# and returns the right code — so without the check this gate is
# structurally blind to the exact defect issue #2817 reports. With it, an
# actor that outlives runtime cleanup lands in the transcript as
# `exit: 93` and mismatches the committed expectation. Extra environment
# for a single execution can be passed in EXTRA_RUN_ENV (name=value
# entries).
execute_fixture() {
    local fixture="$1" name="$2" workdir="$3" transcript="$4"
    local emit="$workdir/emit" scratch="$workdir/cwd"
    mkdir -p "$emit" "$scratch"
    local compile_log="$workdir/$name.compile.log"
    local compile_status=0
    "$HEW_BIN" compile --emit-dir "$emit" "$fixture" >"$compile_log" 2>&1 || compile_status=$?
    local run_status=0
    local stdout_path="$workdir/$name.stdout"
    : >"$stdout_path"
    if [[ "$compile_status" -eq 0 ]]; then
        # The fixture runs under an inner shell rather than `exec`, so a fault
        # is reaped and reported by that inner shell — its notice lands in the
        # fixture's stderr capture, where it is useful diagnostics, instead of
        # being printed by this script's shell across the gate's own output.
        # The status still arrives as 128+signo.
        # shellcheck disable=SC2016  # positional parameters expand in inner bash.
        env HEW_ACTOR_LEAK_CHECK=1 ${EXTRA_RUN_ENV[@]+"${EXTRA_RUN_ENV[@]}"} \
            "$TIMEOUT_BIN" --kill-after=5s "${RUN_TIMEOUT_SECS}s" \
            bash -c 'cd "$1" && "$2"; exit $?' _ "$scratch" "$emit/$name" \
            >"$stdout_path" 2>"$workdir/$name.stderr" || run_status=$?
    fi
    render_transcript "$compile_status" "$compile_log" "$run_status" "$stdout_path" "$transcript"
    LAST_COMPILE_STATUS="$compile_status"
    LAST_RUN_STATUS="$run_status"
}

# Exit status the runtime publishes when its actor-box balance check finds
# an actor still allocated after runtime cleanup. Must match
# `HEW_EXIT_ACTOR_LEAK` in hew-runtime/src/actor_balance.rs.
ACTOR_LEAK_EXIT=93

# Counterfactual for the leak check: accounting that has quietly stopped
# counting passes every fixture while proving nothing, so prove it can
# still fail before trusting it on any fixture.
#
# `HEW_ACTOR_LEAK_SELFTEST=skip-free` makes the runtime's shutdown sweep
# omit the free of exactly one actor it would otherwise reclaim — the same
# program, with the free left out. It must then exit ACTOR_LEAK_EXIT. The
# specific status matters: a counterfactual that merely exits non-zero
# could be failing for an unrelated reason and would prove nothing.
#
# Picks a runnable fixture that spawns an actor. Chosen by asking the
# runtime, not by trusting a name: the baseline run must first exit
# something OTHER than ACTOR_LEAK_EXIT, which is only possible if the
# fixture reaches runtime cleanup with a balanced count.
leak_oracle_selftest() {
    local workdir="$1"
    local fixture name baseline_status leaked_status
    for fixture in "${fixtures[@]}"; do
        name="$(basename "$fixture" .hew)"
        [[ -f "$CORPUS/$name.expected" ]] || continue
        grep -q '^actor ' "$fixture" || continue

        local sub="$workdir/selftest-$name"
        mkdir -p "$sub"
        EXTRA_RUN_ENV=()
        execute_fixture "$fixture" "$name" "$sub" "$sub/$name.baseline"
        [[ "$LAST_COMPILE_STATUS" -eq 0 ]] || continue
        baseline_status="$LAST_RUN_STATUS"
        if [[ "$baseline_status" -eq "$ACTOR_LEAK_EXIT" ]] || is_crash_status "$baseline_status"; then
            continue
        fi

        EXTRA_RUN_ENV=(HEW_ACTOR_LEAK_SELFTEST=skip-free)
        execute_fixture "$fixture" "$name" "$sub" "$sub/$name.leaked"
        EXTRA_RUN_ENV=()
        leaked_status="$LAST_RUN_STATUS"
        if [[ "$leaked_status" -ne "$ACTOR_LEAK_EXIT" ]]; then
            echo "LEAK ORACLE SELFTEST FAILED: $name with one actor free omitted exited $leaked_status, expected $ACTOR_LEAK_EXIT" >&2
            echo "  the actor-box balance check is not catching a leaked actor, so every PASS below is meaningless" >&2
            head -20 "$sub/$name.stderr" >&2
            return 1
        fi
        echo "SELFTEST $name (baseline exit $baseline_status; one free omitted -> exit $leaked_status)"
        return 0
    done
    echo "LEAK ORACLE SELFTEST FAILED: no runnable actor-spawning fixture available to run the counterfactual against" >&2
    return 1
}

fixtures=()
while IFS= read -r f; do
    fixtures+=("$f")
done < <(find "$CORPUS" -maxdepth 1 -name '*.hew' | sort)

if [[ ${#fixtures[@]} -eq 0 ]]; then
    echo "checked-mir-corpus: no fixtures under $CORPUS" >&2
    exit 2
fi

# The stale-golden check below catches a fixture deleted on its own, but a
# fixture deleted TOGETHER with its goldens leaves a smaller corpus that still
# verifies clean. Reject an empty enumeration before comparing the survivors.
corpus_nonempty_assert "checked-mir-fixtures" "${#fixtures[@]}" || exit 1

case "$MODE" in
golden)
    mkdir -p "$GOLDEN"
    tmpdir="$(mktemp -d)"
    trap 'rm -rf "$tmpdir"' EXIT
    changed=()
    added=()
    unchanged=0
    for f in "${fixtures[@]}"; do
        name="$(basename "$f" .hew)"
        for stage in "${STAGES[@]}"; do
            golden_file="$GOLDEN/$name.$stage.mir"
            fresh="$tmpdir/$name.$stage.mir"
            "$HEW_BIN" compile --dump-mir "$stage" "$f" >"$fresh"
            if [[ ! -f "$golden_file" ]]; then
                added+=("$name.$stage.mir")
            elif cmp -s "$golden_file" "$fresh"; then
                unchanged=$((unchanged + 1))
                continue
            else
                plus="$(diff "$golden_file" "$fresh" | grep -c '^>' || true)"
                minus="$(diff "$golden_file" "$fresh" | grep -c '^<' || true)"
                changed+=("$name.$stage.mir (+$plus -$minus)")
            fi
            cp "$fresh" "$golden_file"
        done
    done
    render_manifest >"$MANIFEST"
    # The regeneration report: a golden corpus that moves silently is a
    # corpus that cannot fail for the change regenerating it, so every
    # recapture states what moved before the commit is written.
    echo "checked-mir-golden: ${#changed[@]} changed, ${#added[@]} new, $unchanged unchanged"
    for entry in ${added[@]+"${added[@]}"}; do
        echo "  NEW     $entry"
    done
    for entry in ${changed[@]+"${changed[@]}"}; do
        echo "  CHANGED $entry"
    done
    if [[ ${#changed[@]} -gt 0 || ${#added[@]} -gt 0 ]]; then
        echo "checked-mir-golden: quote this report in the commit body." >&2
    fi
    ;;
verify)
    fail=0
    tmpdir="$(mktemp -d)"
    trap 'rm -rf "$tmpdir"' EXIT
    for f in "${fixtures[@]}"; do
        name="$(basename "$f" .hew)"
        for stage in "${STAGES[@]}"; do
            golden_file="$GOLDEN/$name.$stage.mir"
            if [[ ! -f "$golden_file" ]]; then
                echo "MISSING GOLDEN: $name.$stage.mir (run: make checked-mir-golden)" >&2
                fail=1
                continue
            fi
            "$HEW_BIN" compile --dump-mir "$stage" "$f" >"$tmpdir/$name.$stage.mir"
            if ! diff -u "$golden_file" "$tmpdir/$name.$stage.mir" >"$tmpdir/$name.$stage.diff"; then
                echo "DUMP DRIFT: $name ($stage stage) — first 40 diff lines:" >&2
                head -40 "$tmpdir/$name.$stage.diff" >&2
                fail=1
            fi
        done
    done
    # Stale goldens (golden exists, fixture removed) are an error too:
    # they silently shrink the oracle's coverage.
    for g in "$GOLDEN"/*.mir; do
        [[ -e "$g" ]] || continue
        base="$(basename "$g")"
        name="${base%.*.mir}"
        if [[ ! -f "$CORPUS/$name.hew" ]]; then
            echo "STALE GOLDEN: $base has no fixture $name.hew" >&2
            fail=1
        fi
    done
    # The manifest is the reviewer-visible record of which goldens a commit
    # regenerated. Checking it here keeps it honest in both directions: a
    # golden edited without recapturing, and a manifest edited without
    # moving the golden, both fail.
    if [[ ! -f "$MANIFEST" ]]; then
        echo "MISSING MANIFEST: $MANIFEST (run: make checked-mir-golden)" >&2
        fail=1
    else
        render_manifest >"$tmpdir/MANIFEST.sha256"
        if ! diff -u "$MANIFEST" "$tmpdir/MANIFEST.sha256" >"$tmpdir/manifest.diff"; then
            echo "MANIFEST DRIFT: golden/MANIFEST.sha256 does not match the goldens on disk" >&2
            echo "(run: make checked-mir-golden)" >&2
            head -40 "$tmpdir/manifest.diff" >&2
            fail=1
        fi
    fi
    if [[ $fail -ne 0 ]]; then
        echo "checked-mir-verify: FAILED" >&2
        exit 1
    fi
    echo "checked-mir-verify: OK (${#fixtures[@]} fixtures x ${#STAGES[@]} stages byte-identical, manifest in sync)"
    ;;
run)
    TIMEOUT_BIN="$(resolve_timeout)"
    fail=0
    ran=0
    nonrunnable=0
    tmpdir="$(mktemp -d)"
    trap 'rm -rf "$tmpdir"' EXIT
    # The leak check below is only evidence if it can still fail. Prove that
    # first, against a deliberately leaked actor, and refuse to report on the
    # corpus at all if the counterfactual comes back green.
    if ! leak_oracle_selftest "$tmpdir"; then
        echo "checked-mir-run: FAILED (leak oracle selftest)" >&2
        exit 1
    fi
    for f in "${fixtures[@]}"; do
        name="$(basename "$f" .hew)"
        expected="$CORPUS/$name.expected"
        classification=0
        fixture_has_main "$f" "$tmpdir/$name.raw.mir" || classification=$?
        if [[ "$classification" -eq 2 ]]; then
            echo "CANNOT CLASSIFY: $HEW_BIN failed to dump raw MIR for $name.hew" >&2
            head -20 "$tmpdir/$name.raw.mir.err" >&2
            fail=1
            continue
        fi
        if [[ "$classification" -eq 1 ]]; then
            # No `main` in the fixture's MIR: nothing to execute. The only
            # thing to assert is that no stale expectation claims otherwise.
            if [[ -f "$expected" ]]; then
                echo "STALE EXPECTATION: $name.expected exists but $name.hew declares no main" >&2
                fail=1
            else
                echo "NO-MAIN  $name (library-only fixture, nothing to execute)"
                nonrunnable=$((nonrunnable + 1))
            fi
            continue
        fi
        if [[ ! -f "$expected" ]]; then
            echo "MISSING EXPECTATION: $name.hew declares main but has no $name.expected" >&2
            echo "  (run: make checked-mir-expect, or hand-write it if the fixture does not build)" >&2
            fail=1
            continue
        fi
        execute_fixture "$f" "$name" "$tmpdir" "$tmpdir/$name.actual"
        if diff -u "$expected" "$tmpdir/$name.actual" >"$tmpdir/$name.transcript.diff"; then
            echo "PASS     $name"
            ran=$((ran + 1))
        else
            echo "TRANSCRIPT MISMATCH: $name" >&2
            head -40 "$tmpdir/$name.transcript.diff" >&2
            if is_crash_status "$LAST_RUN_STATUS"; then
                echo "  $name died rather than returned (status $LAST_RUN_STATUS)" >&2
            fi
            if [[ -s "$tmpdir/$name.stderr" ]]; then
                echo "  stderr:" >&2
                head -20 "$tmpdir/$name.stderr" >&2
            fi
            if [[ "$LAST_COMPILE_STATUS" -ne 0 ]]; then
                echo "  compiler output:" >&2
                head -20 "$tmpdir/$name.compile.log" >&2
            fi
            fail=1
        fi
    done
    # An expectation with no fixture is the mirror-image hole: it would let
    # a deleted fixture leave a passing-looking artefact behind.
    for e in "$CORPUS"/*.expected; do
        [[ -e "$e" ]] || continue
        base="$(basename "$e" .expected)"
        if [[ ! -f "$CORPUS/$base.hew" ]]; then
            echo "ORPHAN EXPECTATION: $base.expected has no fixture $base.hew" >&2
            fail=1
        fi
    done
    if [[ $fail -ne 0 ]]; then
        echo "checked-mir-run: FAILED" >&2
        exit 1
    fi
    echo "checked-mir-run: OK ($ran fixtures executed, $nonrunnable with no main)"
    ;;
expect)
    TIMEOUT_BIN="$(resolve_timeout)"
    tmpdir="$(mktemp -d)"
    trap 'rm -rf "$tmpdir"' EXIT
    changed=()
    added=()
    refused=()
    unchanged=0
    for f in "${fixtures[@]}"; do
        name="$(basename "$f" .hew)"
        expected="$CORPUS/$name.expected"
        classification=0
        fixture_has_main "$f" "$tmpdir/$name.raw.mir" || classification=$?
        if [[ "$classification" -eq 2 ]]; then
            echo "  REFUSED $name — the compiler cannot dump its raw MIR" >&2
            continue
        fi
        if [[ "$classification" -eq 1 ]]; then
            if [[ -f "$expected" ]]; then
                echo "  STALE   $name.expected (fixture declares no main; delete it)" >&2
            fi
            continue
        fi
        execute_fixture "$f" "$name" "$tmpdir" "$tmpdir/$name.actual"
        # Capture never blesses breakage. A fixture that fails to build, is
        # killed by the wall-clock cap, or dies on a fault signal is exactly
        # the failure this gate exists to catch, so recording it has to be a
        # deliberate hand-written act that shows up as authored content in
        # review, not a side effect of running the capture command. Only the
        # fault and timeout statuses are refused — a fixture whose `main`
        # legitimately returns a large value (POSIX truncates to 8 bits)
        # captures normally.
        if [[ "$LAST_COMPILE_STATUS" -ne 0 ]] || is_crash_status "$LAST_RUN_STATUS"; then
            refused+=("$name (compile exit $LAST_COMPILE_STATUS, run exit $LAST_RUN_STATUS)")
            continue
        fi
        # A leaked actor is a defect this gate exists to catch, so capturing
        # `exit: 93` as the expectation would bless it. Same rule as a crash:
        # recording one has to be a deliberate hand-written act.
        if [[ "$LAST_RUN_STATUS" -eq "$ACTOR_LEAK_EXIT" ]]; then
            refused+=("$name (leaked an actor: run exit $LAST_RUN_STATUS)")
            continue
        fi
        if [[ ! -f "$expected" ]]; then
            added+=("$name.expected")
        elif cmp -s "$expected" "$tmpdir/$name.actual"; then
            unchanged=$((unchanged + 1))
            continue
        else
            changed+=("$name.expected")
        fi
        cp "$tmpdir/$name.actual" "$expected"
    done
    echo "checked-mir-expect: ${#changed[@]} changed, ${#added[@]} new, $unchanged unchanged, ${#refused[@]} refused"
    for entry in ${added[@]+"${added[@]}"}; do
        echo "  NEW     $entry"
    done
    for entry in ${changed[@]+"${changed[@]}"}; do
        echo "  CHANGED $entry"
    done
    for entry in ${refused[@]+"${refused[@]}"}; do
        echo "  REFUSED $entry — does not build, died on a signal, or leaked an actor; write the expectation by hand" >&2
    done
    ;;
*)
    echo "usage: $0 {golden|verify|run|expect}" >&2
    exit 2
    ;;
esac
