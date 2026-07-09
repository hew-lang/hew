#!/usr/bin/env bash
# ll-identity-selftest.sh — Self-proof for scripts/ll-byte-identity.sh.
#
# Four independently-failable cases that prove the oracle's normaliser has
# the correct sensitivity for string-pool symbol changes:
#
#   Case 1 (content-change-caught):
#     Mutating @str_lit.4 from c"hello\00" to c" world\00" (and vice-versa)
#     changes which string literal is stored.  The oracle MUST report DIFFERS.
#     This is the canonical false-negative the old normaliser had: it collapsed
#     ALL @str_lit.N to @str_lit.N regardless of content, masking the change.
#
#   Case 2 (register-perturbation-caught):
#     Renaming %checked_lhs to %checked_lhs_perturbed inside a function body
#     MUST still be caught (confirming the old per-register sensitivity is
#     preserved by the rewrite).
#
#   Case 3 (baseline-identical):
#     Comparing a file against itself MUST be IDENTICAL (zero false positives
#     even with multiple pool symbols present).
#
#   Case 4 (pool-id-reordering-transparent):
#     Two files that store the SAME string literals but with pool-id numbers
#     swapped (run-to-run HashMap non-determinism) MUST be IDENTICAL.
#     The old normaliser would have passed this accidentally; the new one
#     proves it via content-based canonicalisation.
#
#   Case 5 (numeric-const-name-change-caught):
#     A function referencing @__hew_const__FOO__1 vs the same shape referencing
#     @__hew_const__BAR__99 MUST be reported as DIFFERS. gsub replacement
#     strings are literal in POSIX/gawk/mawk (they do not expand \1
#     backreferences), so the original
#     `gsub(/.../, "@__hew_const__\1__N", line)` collapsed both FOO and BAR
#     references to the literal text "@__hew_const__\1__N", erasing the NAME
#     and silently hiding a numeric constant identity change. The fix
#     reconstructs the substitution manually with match()/substr(), the same
#     portable idiom already used elsewhere in the normaliser, preserving the
#     captured NAME segment verbatim (gensub() would also expand the
#     backreference, but it is a gawk-only extension unavailable on the
#     default macOS /usr/bin/awk).
#
#   Case 6 (numeric-const-pool-id-reordering-transparent):
#     Same numeric constant NAME (FOO) but a different pool-id suffix
#     (@__hew_const__FOO__1 vs @__hew_const__FOO__7) MUST still be IDENTICAL
#     — only the NAME discriminates identity, not the run-to-run pool-id
#     number. Proves Case 5's fix does not over-correct into false positives.
#
# Exit codes:
#   0  all cases pass
#   1  one or more cases fail (details on stderr)

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ORACLE="$SCRIPT_DIR/ll-byte-identity.sh"

TMPDIR_BASE="$(mktemp -d /tmp/hew-ll-identity-selftest.XXXXXX)"
trap 'rm -rf "$TMPDIR_BASE"' EXIT

pass() { echo "PASS $1"; }
fail() { echo "FAIL $1: $2" >&2; exit 1; }

# ---------------------------------------------------------------------------
# Shared baseline fixture:
#   @str_lit.4 → c"hello\00"
#   @str_lit.5 → c" world\00"
#   The function stores .4 in local_42 and .5 in local_44.
# ---------------------------------------------------------------------------
write_baseline() {
  cat > "$1" << 'EOF'
@str_lit = private unnamed_addr constant [2 x i8] c"a\00", align 1
@str_lit.1 = private unnamed_addr constant [2 x i8] c"b\00", align 1
@str_lit.4 = private unnamed_addr constant [6 x i8] c"hello\00", align 1
@str_lit.5 = private unnamed_addr constant [7 x i8] c" world\00", align 1

define void @test_fn() {
  store ptr @str_lit.4, ptr %local_42, align 8
  store ptr @str_lit.5, ptr %local_44, align 8
  ret void
}
EOF
}

# Mutated: @str_lit.4 now carries c" world\00" and .5 carries c"hello\00",
# but the function STILL stores .4 in local_42 — so a different string is
# stored there.  The pool-id numbers are unchanged; only the content differs.
write_content_mutated() {
  cat > "$1" << 'EOF'
@str_lit = private unnamed_addr constant [2 x i8] c"a\00", align 1
@str_lit.1 = private unnamed_addr constant [2 x i8] c"b\00", align 1
@str_lit.4 = private unnamed_addr constant [7 x i8] c" world\00", align 1
@str_lit.5 = private unnamed_addr constant [6 x i8] c"hello\00", align 1

define void @test_fn() {
  store ptr @str_lit.4, ptr %local_42, align 8
  store ptr @str_lit.5, ptr %local_44, align 8
  ret void
}
EOF
}

# Pool-id reordered: @str_lit.4 carries c" world\00" and .5 carries c"hello\00",
# but the function stores .5 in local_42 and .4 in local_44 — SAME strings
# in the SAME locals, only the pool-id numbers shifted (run-to-run ordering).
write_pool_id_reordered() {
  cat > "$1" << 'EOF'
@str_lit = private unnamed_addr constant [2 x i8] c"a\00", align 1
@str_lit.1 = private unnamed_addr constant [2 x i8] c"b\00", align 1
@str_lit.4 = private unnamed_addr constant [7 x i8] c" world\00", align 1
@str_lit.5 = private unnamed_addr constant [6 x i8] c"hello\00", align 1

define void @test_fn() {
  store ptr @str_lit.5, ptr %local_42, align 8
  store ptr @str_lit.4, ptr %local_44, align 8
  ret void
}
EOF
}

# Register-perturbed: same string literals, but %checked_lhs renamed.
write_reg_perturbed() {
  cat > "$1" << 'EOF'
@str_lit.4 = private unnamed_addr constant [6 x i8] c"hello\00", align 1

define void @test_reg() {
  %checked_lhs_perturbed = load i64, ptr %x, align 8
  store ptr @str_lit.4, ptr %local_42, align 8
  ret void
}
EOF
}

write_reg_baseline() {
  cat > "$1" << 'EOF'
@str_lit.4 = private unnamed_addr constant [6 x i8] c"hello\00", align 1

define void @test_reg() {
  %checked_lhs = load i64, ptr %x, align 8
  store ptr @str_lit.4, ptr %local_42, align 8
  ret void
}
EOF
}

# Numeric-const fixture: references @__hew_const__FOO__1.
write_numeric_const_foo() {
  cat > "$1" << 'EOF'
@__hew_const__FOO__1 = private unnamed_addr constant i64 42, align 8

define i64 @test_numeric_const() {
  %v = load i64, ptr @__hew_const__FOO__1, align 8
  ret i64 %v
}
EOF
}

# Numeric-const fixture: SAME shape, but references @__hew_const__BAR__99 --
# a genuinely different named constant (not just a different pool-id
# suffix on the same name).
write_numeric_const_bar() {
  cat > "$1" << 'EOF'
@__hew_const__BAR__99 = private unnamed_addr constant i64 42, align 8

define i64 @test_numeric_const() {
  %v = load i64, ptr @__hew_const__BAR__99, align 8
  ret i64 %v
}
EOF
}

# Numeric-const fixture: SAME name (FOO) as write_numeric_const_foo, but a
# different pool-id suffix (__7 instead of __1) -- run-to-run
# HashMap-iteration-order non-determinism for the SAME semantic constant.
write_numeric_const_foo_reordered() {
  cat > "$1" << 'EOF'
@__hew_const__FOO__7 = private unnamed_addr constant i64 42, align 8

define i64 @test_numeric_const() {
  %v = load i64, ptr @__hew_const__FOO__7, align 8
  ret i64 %v
}
EOF
}

# ---------------------------------------------------------------------------
# Each case uses a dedicated pair of directories so failures are independent.
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Case 1: content-change-caught
# Changing which string literal a pool symbol points to MUST be detected.
# ---------------------------------------------------------------------------
echo "--- Case 1: content-change-caught ---"
C1_BASE="$TMPDIR_BASE/c1_base"
C1_HEAD="$TMPDIR_BASE/c1_head"
mkdir -p "$C1_BASE" "$C1_HEAD"
write_baseline "$C1_BASE/fixture.ll"
write_content_mutated "$C1_HEAD/fixture.ll"

rc=0
bash "$ORACLE" "$C1_BASE" "$C1_HEAD" > /dev/null 2>&1 || rc=$?
if [[ "$rc" -eq 1 ]]; then
  pass "content-change-caught"
elif [[ "$rc" -eq 0 ]]; then
  fail "content-change-caught" \
    "oracle exited 0 (IDENTICAL) but string content changed — false negative"
else
  fail "content-change-caught" "oracle exited $rc (expected 1)"
fi

# ---------------------------------------------------------------------------
# Case 2: register-perturbation-caught
# Renaming %checked_lhs inside a function body MUST still be detected.
# ---------------------------------------------------------------------------
echo "--- Case 2: register-perturbation-caught ---"
C2_BASE="$TMPDIR_BASE/c2_base"
C2_HEAD="$TMPDIR_BASE/c2_head"
mkdir -p "$C2_BASE" "$C2_HEAD"
write_reg_baseline "$C2_BASE/fixture.ll"
write_reg_perturbed "$C2_HEAD/fixture.ll"

rc=0
bash "$ORACLE" "$C2_BASE" "$C2_HEAD" > /dev/null 2>&1 || rc=$?
if [[ "$rc" -eq 1 ]]; then
  pass "register-perturbation-caught"
elif [[ "$rc" -eq 0 ]]; then
  fail "register-perturbation-caught" \
    "oracle exited 0 (IDENTICAL) but register name changed — false negative"
else
  fail "register-perturbation-caught" "oracle exited $rc (expected 1)"
fi

# ---------------------------------------------------------------------------
# Case 3: baseline-identical
# A file compared against itself MUST be IDENTICAL.
# ---------------------------------------------------------------------------
echo "--- Case 3: baseline-identical ---"
C3_BASE="$TMPDIR_BASE/c3_base"
C3_HEAD="$TMPDIR_BASE/c3_head"
mkdir -p "$C3_BASE" "$C3_HEAD"
write_baseline "$C3_BASE/fixture.ll"
write_baseline "$C3_HEAD/fixture.ll"

rc=0
bash "$ORACLE" "$C3_BASE" "$C3_HEAD" > /dev/null 2>&1 || rc=$?
if [[ "$rc" -eq 0 ]]; then
  pass "baseline-identical"
else
  fail "baseline-identical" \
    "oracle exited $rc (expected 0) — false positive on identical files"
fi

# ---------------------------------------------------------------------------
# Case 4: pool-id-reordering-transparent
# Same string literals stored in the same locals, but pool-id numbers shifted
# (run-to-run HashMap non-determinism).  MUST be IDENTICAL.
# ---------------------------------------------------------------------------
echo "--- Case 4: pool-id-reordering-transparent ---"
C4_BASE="$TMPDIR_BASE/c4_base"
C4_HEAD="$TMPDIR_BASE/c4_head"
mkdir -p "$C4_BASE" "$C4_HEAD"
write_baseline "$C4_BASE/fixture.ll"
write_pool_id_reordered "$C4_HEAD/fixture.ll"

rc=0
bash "$ORACLE" "$C4_BASE" "$C4_HEAD" > /dev/null 2>&1 || rc=$?
if [[ "$rc" -eq 0 ]]; then
  pass "pool-id-reordering-transparent"
else
  bash "$ORACLE" "$C4_BASE" "$C4_HEAD" 2>&1 || true
  fail "pool-id-reordering-transparent" \
    "oracle exited $rc (expected 0) — false positive on pool-id reordering"
fi

# ---------------------------------------------------------------------------
# Case 5: numeric-const-name-change-caught
# @__hew_const__FOO__1 vs @__hew_const__BAR__99 in the SAME structural
# position MUST be reported as DIFFERS -- a different named numeric constant
# is a real semantic change, not pool-id churn.
# ---------------------------------------------------------------------------
echo "--- Case 5: numeric-const-name-change-caught ---"
C5_BASE="$TMPDIR_BASE/c5_base"
C5_HEAD="$TMPDIR_BASE/c5_head"
mkdir -p "$C5_BASE" "$C5_HEAD"
write_numeric_const_foo "$C5_BASE/fixture.ll"
write_numeric_const_bar "$C5_HEAD/fixture.ll"

rc=0
bash "$ORACLE" "$C5_BASE" "$C5_HEAD" > /dev/null 2>&1 || rc=$?
if [[ "$rc" -eq 1 ]]; then
  pass "numeric-const-name-change-caught"
elif [[ "$rc" -eq 0 ]]; then
  fail "numeric-const-name-change-caught" \
    "oracle exited 0 (IDENTICAL) but the referenced numeric constant's NAME changed (FOO -> BAR) — false negative"
else
  fail "numeric-const-name-change-caught" "oracle exited $rc (expected 1)"
fi

# ---------------------------------------------------------------------------
# Case 6: numeric-const-pool-id-reordering-transparent
# @__hew_const__FOO__1 vs @__hew_const__FOO__7 -- SAME name, only the
# run-to-run pool-id suffix shifted.  MUST be IDENTICAL.
# ---------------------------------------------------------------------------
echo "--- Case 6: numeric-const-pool-id-reordering-transparent ---"
C6_BASE="$TMPDIR_BASE/c6_base"
C6_HEAD="$TMPDIR_BASE/c6_head"
mkdir -p "$C6_BASE" "$C6_HEAD"
write_numeric_const_foo "$C6_BASE/fixture.ll"
write_numeric_const_foo_reordered "$C6_HEAD/fixture.ll"

rc=0
bash "$ORACLE" "$C6_BASE" "$C6_HEAD" > /dev/null 2>&1 || rc=$?
if [[ "$rc" -eq 0 ]]; then
  pass "numeric-const-pool-id-reordering-transparent"
else
  bash "$ORACLE" "$C6_BASE" "$C6_HEAD" 2>&1 || true
  fail "numeric-const-pool-id-reordering-transparent" \
    "oracle exited $rc (expected 0) — false positive on numeric-const pool-id reordering"
fi

# ---------------------------------------------------------------------------
echo ""
echo "ll-identity-selftest: all 6 cases PASS"
