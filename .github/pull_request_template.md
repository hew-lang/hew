## Why

<!-- What's broken, missing, or suboptimal? For fixes: the observable symptom.
     For features: what capability this adds. Delete this comment when filling in. -->

## What

<!-- What the change does. For multi-layer compiler changes, use a bullet list:
     - **lexer**: added `frobnicate` keyword
     - **parser**: parse frobnicate expressions
     - **codegen**: lower to MLIR FrobnicateOp
     For trivial PRs (typo, dep bump), delete this section — the title is enough. -->

## Test

<!-- Name specific new or modified tests. No pass counts — CI handles that.
     For test-only PRs, describe what's now covered that wasn't before.
     For changes with no new tests, explain why (e.g., "covered by existing E2E"). -->

## Quality Checklist
- [ ] No new `.ok()?` or `unwrap_or_default()` in codegen without `// JUSTIFIED` comment
- [ ] New allocations have cleanup paths for sync, async, and actor shutdown contexts
- [ ] Serialization changes include round-trip encode/decode tests
- [ ] New runtime features have WASM implementation or `// WASM-TODO` marker, and new `hew_*` exports are classified in `scripts/jit-symbol-classification.toml`
- [ ] No duplicated logic — checked for existing helpers before adding new ones
