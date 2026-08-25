# Hew Semantic IR

`hew-sir` is Hew's value-oriented semantic SSA intermediate representation.
It sits between resolved HIR and the established ownership/layout MIR ladder.

The initial implementation is intentionally shadow-only: it verifies a small,
pure subset of HIR and then leaves the existing HIR-to-MIR-to-LLVM pipeline as
the authoritative producer of compiler artifacts.
