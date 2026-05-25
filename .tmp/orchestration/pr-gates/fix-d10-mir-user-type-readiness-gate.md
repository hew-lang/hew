# PR Gate Report: fix/d10-mir-user-type-readiness-gate

## Scope

W4.012 Stage 1 only: MIR named-user type readiness gate and layout field-closure diagnostics.

## Verdict

ready-for-review

## Summary

- Replaced the MIR `UnknownType` gate's `type_classes.contains_key(name)` short-circuit with a codegen-readiness predicate over record, enum/machine, actor, supervisor, and machine-event layout names.
- Added a structured HIR named-type walker that preserves `ResolvedTy::Named.builtin`, and kept the legacy string-only walker backed by the same traversal.
- Added a module-level record/enum/machine field-type closure walk so unready nested user names are rejected at MIR.
- Removed the MIR `Generator` user-name carve-out; the remaining MIR classifier path accepts only `builtin: Some(BuiltinType::Generator)`.
- Completed the HIR builtin registration/lowering path for live builtin producers (`Vec`, `HashMap`, `HashSet`, `ActorRef`, `Actor`, lowercase `bytes`) so the MIR gate consumes carried discriminators rather than local string allowlists.

## Generator audit

`hew-hir` gen-block producers carry `builtin: Some(BuiltinType::Generator)`. No live HIR/MIR producer of `ResolvedTy::Named { name: "Generator", builtin: None, .. }` was found. Remaining `name == "Generator"` checks are HIR shape/verification checks, not MIR readiness bypasses.

## Stage boundaries

- Stage 2 codegen layout graph predeclaration was not implemented.
- Stage 3 `primitive_to_llvm` named-arm migration was not implemented.
- Codegen D10 sentinel behavior was not changed.

## Validation

- `cargo build -p hew-mir`: pass
- `cargo test -p hew-mir`: pass
- `cargo clippy -p hew-mir --all-targets -- -D warnings`: pass
- New/changed MIR tests 3x: pass
- `make ci-preflight`: pass

## Notes for reviewer

The HIR edits are producer-side discriminator plumbing required by the Stage 1 MIR gate: without them, existing parsed builtin annotations like `Vec<T>`, `ActorRef<T>`, `LocalPid<T>`, and `bytes` reached MIR without the W4.011 builtin discriminator and failed the new gate.
