#!/usr/bin/env bash
# Negative-control regression test for doc_fence_extract_std's tri-state
# fence scanner (scripts/corpus-ratchet.sh).
#
# std/**/*.hew doc comments fence hew examples with a BARE ``` (no ```hew
# tag) and mix module-level `//!` with item-level `///` comments. A fence
# tagged with another language (```text) must never contribute a hew fence,
# and — the bug this test pins — its closing bare ``` must never be mistaken
# for the OPEN of the next real hew fence. Getting that wrong doesn't shrink
# the fence count (a stray fence still gets captured), it corrupts fence
# CONTENT: the fence after a ```text block ends up holding the prose between
# the two fences instead of its own code.
#
# WHEN OBSOLETE: superseded if doc_fence_extract_std is deleted or std moves
# off the bare-fence convention entirely.

set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
HARNESS="$REPO_ROOT/scripts/corpus-ratchet.sh"

PASSES=0
FAILURES=0

pass() {
    echo "PASS: $*"
    PASSES=$((PASSES + 1))
}

fail() {
    echo "FAIL: $*" >&2
    FAILURES=$((FAILURES + 1))
}

TMP_ROOT="$(mktemp -d "${TMPDIR:-/tmp}/hew-std-fence-test.XXXXXX")"
cleanup() {
    rm -rf "$TMP_ROOT"
}
trap cleanup EXIT

FIXTURE_STD_DIR="$TMP_ROOT/std-fixture"
mkdir -p "$FIXTURE_STD_DIR"

# One module-level (`//!`) fence, one item-level (`///`) fence, a `text`
# -tagged fence that must be excluded, a further `///` fence directly after
# it — the tooth that catches an in_other regression — and an indented `///`
# fence on a trait member — the tooth that catches an indented-marker
# regression (doc comments on trait/impl members are indented, unlike the
# column-0 module-level `//!` sources).
cat >"$FIXTURE_STD_DIR/example.hew" <<'FIXTURE_EOF'
//! Module doc.
//!
//! ```
//! let a = 1;
//! ```

/// Item doc with an example.
///
/// ```
/// let b = 2;
/// ```
pub fn noop() {}

/// A schema check example (not hew).
///
/// ```text
/// hew wire check <file.hew> --against <baseline.hew>
/// ```
///
/// Another example right after the text fence.
///
/// ```
/// let c = 3;
/// ```
pub fn another() {}

pub trait Greeter {
    /// A trait member doc with an indented example.
    ///
    /// ```
    /// let d = 4;
    /// ```
    fn greet();
}
FIXTURE_EOF

FAKE_HEW="$TMP_ROOT/hew"
cat >"$FAKE_HEW" <<'FAKE_HEW_EOF'
#!/usr/bin/env bash
[[ $# -eq 2 && "$1" == "check" ]] || exit 64
exit 0
FAKE_HEW_EOF
chmod +x "$FAKE_HEW"

OUTDIR="$TMP_ROOT/fences"
EXPECTED_EMPTY="$TMP_ROOT/expected-empty.txt"
touch "$EXPECTED_EMPTY"

DOC_FENCE_STD_DIR="$FIXTURE_STD_DIR" \
    "$HARNESS" doc-fences \
    --expected-failures "$EXPECTED_EMPTY" \
    --outdir "$OUTDIR" \
    --hew-bin "$FAKE_HEW" >"$TMP_ROOT/harness-output.txt" 2>&1 || true

mapfile -t FIXTURE_OUTFILES < <(find "$OUTDIR" -name '*example*.hew' | LC_ALL=C sort)

if ((${#FIXTURE_OUTFILES[@]} == 4)); then
    pass "fixture yields exactly 4 hew fences (the text-tagged fence is excluded)"
else
    fail "fixture yielded ${#FIXTURE_OUTFILES[@]} fences, expected 4 (got: ${FIXTURE_OUTFILES[*]:-none})"
fi

if ((${#FIXTURE_OUTFILES[@]} >= 1)) &&
    grep -qxF 'let a = 1;' "${FIXTURE_OUTFILES[0]}" 2>/dev/null; then
    pass "first fence (module //! doc) extracts its own content"
else
    fail "first fence content mismatch: $(cat "${FIXTURE_OUTFILES[0]:-/dev/null}" 2>/dev/null || echo MISSING)"
fi

if ((${#FIXTURE_OUTFILES[@]} >= 2)) &&
    grep -qxF 'let b = 2;' "${FIXTURE_OUTFILES[1]}" 2>/dev/null; then
    pass "second fence (item /// doc) extracts its own content"
else
    fail "second fence content mismatch: $(cat "${FIXTURE_OUTFILES[1]:-/dev/null}" 2>/dev/null || echo MISSING)"
fi

# The pinned bug: without the in_other tri-state, this third extracted fence
# holds the prose ("Another example...") swallowed between the ```text
# close and the real fence open, instead of `let c = 3;`.
if ((${#FIXTURE_OUTFILES[@]} >= 3)) &&
    grep -qxF 'let c = 3;' "${FIXTURE_OUTFILES[2]}" 2>/dev/null &&
    ! grep -q 'Another example' "${FIXTURE_OUTFILES[2]}" 2>/dev/null; then
    pass "fence after a text-tagged block extracts its own content, not the intervening prose"
else
    fail "fence after text-tagged block corrupted: $(cat "${FIXTURE_OUTFILES[2]:-/dev/null}" 2>/dev/null || echo MISSING)"
fi

if ! grep -rq 'hew wire check' "$OUTDIR"/*example*.hew 2>/dev/null; then
    pass "the text-tagged fence itself is never extracted as a hew fence"
else
    fail "the text-tagged fence's content leaked into an extracted hew fence"
fi

# The pinned bug this fixture tooth catches: without trimming leading
# whitespace before matching the `///` marker, an indented trait-member doc
# comment is never recognized as a doc comment at all, so its fence is
# silently dropped instead of extracted.
if ((${#FIXTURE_OUTFILES[@]} >= 4)) &&
    grep -qxF 'let d = 4;' "${FIXTURE_OUTFILES[3]}" 2>/dev/null; then
    pass "fourth fence (indented trait-member /// doc) extracts its own content"
else
    fail "fourth fence content mismatch: $(cat "${FIXTURE_OUTFILES[3]:-/dev/null}" 2>/dev/null || echo MISSING)"
fi

echo ""
echo "std doc-fence extraction self-test: $PASSES passed, $FAILURES failed"
((FAILURES == 0))
