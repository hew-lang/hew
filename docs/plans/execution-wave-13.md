# Execution Wave 13

**Date:** 2026-04-15
**Theme:** Runtime ABI hardening, parser/formatter parity, docs truth alignment

## PRs

| PR | Title | Status |
|----|-------|--------|
| #1065 | Runtime ABI coverage, WASM cmake parity, sleep codegen, scheduler consolidation | Open |
| #1066 | Parser/formatter parity tests and wire round-trip coverage | Open |
| #1067 | v0.3 CLI/docs alignment, DROP-TODO enrichment, hew doc test coverage | Open |
| #XXXX | Milestone docs, eval UX, v0.4 foundations | This PR |

## Discovery

### Scout A — Codegen DROP-TODO Classification

All 8 codegen DROP-TODOs classified as BLOCKED. Root causes:
- Alias-tracking infrastructure (D2, D5, D8)
- Move-checker / type-system move semantics (D4, D7)
- Stream handle ownership model (D3)
- Helper extraction prerequisite (D1)
- Coroutine-aware test harness (D6)
- D9 (QUIC): NO-ACTION — correctly handled

### Scout B — Parser/Formatter Parity

- 13+ Expr variants, 4 Item types had zero formatter tests → covered in PR #1066
- Wire parse duplicate @N silently absorbed → diagnostic added in PR #1066
- 10+ wire round-trip test categories missing → covered in PR #1066
- G1 (expr-type-map unresolved var): settled — defense-in-depth is correct, no action needed

### Scout C — LSP/Eval/Runtime

- LSP cross-file: already works for direct imports; transitive is future enhancement
- Reply-channel: fully implemented on native + WASM; no work needed
- Eval: fully shipped; UX/docs pass is the remaining slice
- Cooperative blocking recv: needs design doc; deferred

## Deferred to future waves

- All 8 DROP-TODOs (blocked on infrastructure: alias-tracking, move-checker, stream ownership)
- Actor receive param drops (blocked on same infrastructure as D1)
- LSP transitive import chains
- Cooperative blocking channel recv on WASM
- Compiler fail-closed tranche (v0.3 remaining)
- Runtime/stdlib proof-surface expansion (v0.3 remaining)
