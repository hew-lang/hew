# Hew Semantic IR

`hew-sir` sits between resolved HIR and physical MIR. It owns semantic values,
typed control flow, ownership and lifetime decisions, and explicit call and
cleanup boundaries. Physical MIR carries the resulting storage, layout and ABI
facts to LLVM emission.

```text
source → parser → type checker → HIR → SIR → physical MIR → LLVM → object
```

This is the native compiler body path. Unsupported language behaviour is an
implementation gap; it does not select a legacy lowering fallback. Sandbox
execution remains a parity goal using the same language semantics.
