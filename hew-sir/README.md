# Hew Semantic IR

`hew-sir` is Hew's value-oriented semantic SSA intermediate representation.
It sits between resolved HIR and the ownership/layout MIR ladder.

SIR owns semantic values, typed CFG, block arguments, direct-call identity,
effect classification, and Hew-aware optimization. It deliberately does not
own storage places, layout, ABI carriers, byte offsets, or LLVM operations.

Migration uses `--sir-shadow` only as temporary differential evidence. Closed
language domains selected with `--sir-lower` already produce Raw, Checked, and
Elaborated MIR without legacy function-body lowering. The end state is one
convergent body path:

```text
resolved + normalized HIR → SIR → Raw MIR → Checked MIR → Elaborated MIR → LLVM
```

Every SIR transformation verifies its input and output. Unsupported semantic
surface is a compiler implementation gap, never permission to silently route a
selected SIR body back through legacy lowering.
