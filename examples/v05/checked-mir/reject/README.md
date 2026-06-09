# Checked MIR reject fixtures

Each `.hew` source in this directory exercises one `MirCheck` variant.
`hew compile <fixture>` must:

- exit non-zero,
- emit a `MirDiagnostic` whose `kind` matches the fixture's variant,
- produce no native or wasm artefact.

| Fixture                            | `MirCheck` variant           | Status   |
| ---------------------------------- | ---------------------------- | -------- |
| `use_after_consume.hew`            | `UseAfterConsume`            | shipping |
| `use_after_move_into_tuple.hew`    | `UseAfterConsume`            | shipping |
| `init_before_use.hew`              | `InitialisedBeforeUse`       | shipping |
| `use_after_consume_in_block.hew`   | `UseAfterConsume`            | shipping |
| `init_before_use_in_block.hew`     | `InitialisedBeforeUse`       | shipping |
| `use_after_consume_in_if.hew`      | `UseAfterConsume`            | shipping |
| `aliasing.hew`                     | `Aliasing`                   | deferred |
| `generator_borrow.hew`             | `GeneratorBorrowAcrossYield` | deferred |
| `actor_send_escape.hew`            | `ActorSendEscape`            | deferred |

The `_in_block` and `_in_if` fixtures pin the recursion path through
expression-embedded statements: the block-expression lowering forwards
every nested `HirStmt` (not just `HirStmtKind::Expr`) into the
checker-authority stream, so a `let` or `return` inside a block or
inside an `if`/`struct-init`/`call` arm reaches the move-checker.

The deferred fixtures cannot be constructed against the current MIR
surface — the IR has no borrow-op `Instr`, no projection variant on
`Place`, and no construction site for `Terminator::Yield` /
`Terminator::Send`. The corresponding `MirCheck` variants are declared
on the enum so the dataflow passes have payload types to populate when
the construction surface arrives; the reject fixtures will land
alongside that surface.
