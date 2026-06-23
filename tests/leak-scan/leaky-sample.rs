// tests/leak-scan/leaky-sample.rs
//
// Deliberately-leaky sample: every token that lint-orchestration-leak.sh
// must catch.  This file is intentionally excluded from the lint scan
// (via the ':!tests/leak-scan/' exclusion in lint-orchestration-leak.sh) so
// that `make leak-scan` exits 0 on a clean tree even though this file
// contains all the bad patterns.
//
// Proof-of-catch: to verify the rule fires, temporarily unstage this file
// exclusion (or add one of the patterns to an in-scope file) and run
// `scripts/lint-orchestration-leak.sh` — it exits 1.
// The `--self-test` mode does this automatically in an isolated temp repo.
//
// DO NOT remove this file — it is the evidence that each pattern is
// recognized by the gate.

// ── T1: PCA tracking IDs ────────────────────────────────────────────────────
// PCA-8 was an internal tracking ID that leaked into a committed comment.
// PCA-12 is a hypothetical future ID of the same class.
const PCA_8_NOTE: &str = "PCA-8 fix applied here";
// PCA-12: see orchestration state
const _PCA_DEMO: &str = "PCA-12";

// ── T2: wire-fleet orchestration term ───────────────────────────────────────
// "wire-fleet" is the multi-agent fleet term; it should never appear in shipped
// source as an identifier or string.
const FLEET_NOTE: &str = "wire-fleet dispatch";
// wire-fleet phase 2

// ── T3: wire-l<N> lane names ────────────────────────────────────────────────
// wire-l1 and wire-l2 are orchestration lane names that leaked into source.
const LANE_NAME: &str = "wire-l1";
// see wire-l2 for context

// ── T4: L7 / L8 / L9 high-numbered lane IDs ─────────────────────────────────
// L7 leaked from the wire-l1 lane as a brief-fed lane identifier.
// L8 and L9 are future-proofing the same class.
const LANE_ID: &str = "lane L7";
// L8 scope was defined in brief
// L9 follow-on

// ── T5: lowercase q-tags (3+ digits) ────────────────────────────────────────
// q185 and q023 are orchestration Q-tags that leaked into diagnostic strings.
fn diagnose_q185() -> &'static str {
    // q185 errors-as-values fix
    "q185 was the task ID"
}
// per q023

// ── T6: short Q-tags (Qa, Qb, …) ────────────────────────────────────────────
// Qa leaked from wire-l2 into a user-facing diagnostic string.
fn explain_qa() -> &'static str {
    "Qa was the root cause" // Qb also appeared
}

// ── T7: .tmp/(orchestration|plans|worktrees) in string literals ──────────────
// Orchestration path references compiled into binary are never appropriate.
fn bad_path() -> &'static str {
    ".tmp/orchestration/state.db"
}
fn bad_plan() -> &'static str {
    ".tmp/plans/lane-g8.md"
}
fn bad_wt() -> &'static str {
    ".tmp/worktrees/my-lane"
}
