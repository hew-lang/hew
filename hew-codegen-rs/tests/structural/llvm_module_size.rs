//! Line-count ratchet for the god-module `src/llvm.rs`.
//!
//! `llvm.rs` is the coordinating core of the backend: the public API surface,
//! the shared `FnCtx` / `FnSymbol` / `CoroState` context types, and the central
//! `lower_instruction` / `lower_terminator` / `lower_function` dispatch.
//! Cohesive free-function clusters live in sibling concern-modules
//! (`crate::wire`, `crate::suspend`, `crate::runtime_abi`, `crate::layout`,
//! `crate::thunks`, `crate::arith`, `crate::coro`, `crate::abi_class`) so the
//! merge-queue diff-review gate stays effective and future codegen lanes land
//! in a concern module rather than growing the core.
//!
//! This ratchet fails closed if `llvm.rs` grows past a hard ceiling. When it
//! trips, the fix is to carve a coherent concern cluster out into (or into a
//! new) sibling module — NOT to raise the ceiling. The `make ll-diff`
//! byte-identity oracle proves such a carve changes zero emitted IR.

use std::path::Path;

/// Hard ceiling for `src/llvm.rs`. Set to the post-carve line count plus a
/// small headroom band so ordinary in-core edits do not trip it, while a new
/// multi-thousand-line cluster does. This value MUST stay at or below 45,000:
/// a larger core defeats the diff-review gate this ratchet exists to protect.
const CEILING: usize = 44_765;

// Compile-time floor on the ceiling itself: it MUST stay at or below 45,000
// lines. Raising it past that defeats the merge-queue diff-review gate this
// ratchet exists to protect — carve a concern module out instead.
const _: () = assert!(CEILING <= 45_000);

#[test]
fn llvm_module_stays_under_line_ceiling() {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("src/llvm.rs");
    let source = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("failed to read {}: {e}", path.display()));
    let line_count = source.lines().count();

    assert!(
        line_count <= CEILING,
        "src/llvm.rs is {line_count} lines, over the {CEILING}-line ceiling. \
         Carve a concern module out; do not raise the ceiling without a split \
         plan. The `make ll-diff` byte-identity oracle proves a pure-move carve \
         emits identical IR."
    );
}
