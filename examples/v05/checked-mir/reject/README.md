# Checked MIR reject fixtures

Each `.hew` source in this directory exercises one `MirCheck` variant.
`hew compile-v05 <fixture>` must:

- exit non-zero,
- emit a `MirDiagnostic` whose `kind` matches the fixture's variant,
- produce no native or wasm artefact.

| Fixture                  | `MirCheck` variant         | Status   |
| ------------------------ | -------------------------- | -------- |
| `use_after_consume.hew`  | `UseAfterConsume`          | shipping |
| `init_before_use.hew`    | `InitialisedBeforeUse`     | shipping |
| `aliasing.hew`           | `Aliasing`                 | deferred |
| `generator_borrow.hew`   | `GeneratorBorrowAcrossYield` | deferred |
| `actor_send_escape.hew`  | `ActorSendEscape`          | deferred |

The deferred fixtures cannot be constructed against the current MIR
surface — the IR has no borrow-op `Instr`, no projection variant on
`Place`, and no construction site for `Terminator::Yield` /
`Terminator::Send`. The corresponding `MirCheck` variants are declared
on the enum so the dataflow passes have payload types to populate when
the construction surface arrives; the reject fixtures will land
alongside that surface.
