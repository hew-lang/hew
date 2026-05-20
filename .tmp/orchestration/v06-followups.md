# v0.6 Follow-up Items

Items deferred from v0.5 multifile compile-lowering work.

## Visibility enforcement

**Item:** `pub(package)` / `pub(super)` restriction

v0.5 parses both restricted-visibility forms but treats them as fully public.
Enforcement is forward-compatible: callers already within the allowed scope are
unaffected when the restriction lands. Implement in v0.6 by threading scope
context through the visibility check in `hew-compile`.

See: `hew-parser/src/ast.rs` `Visibility::is_pub`, `docs/specs/HEW-SPEC-2026.md`
visibility section.

## HEWPATH unification

**Item:** Unify `HEWPATH` across stdlib and user-module file imports

`HEWPATH` currently governs stdlib module search only. User-module paths use
`HEW_STD` and `.adze/packages`. A unified search-path model covering both
contexts would simplify user configuration and documentation.

See: `hew-compile/src/lib.rs` `resolve_file_imports_internal`,
`docs/specs/HEW-SPEC-2026.md` §3.5.2 module search-path resolution.

## Resolver-backed callee routing (SHIM E2→checker)

**Item:** Reject user-defined function names that shadow registered builtins

`hew-mir/src/lower.rs:2826–2840` still classifies some callees by comparing the
source name against registered runtime symbols before final lowering. A user
function named `link`, `monitor`, `supervisor_stop`, `duplex_pair`, or
`channel` would be vulnerable to misrouting if checker registration ever drifted.
The checker currently prevents the collision in practice, but v0.6 should make
that rejection explicit at the checker boundary instead of relying on the MIR
shim comment.

Source: `v05-boundary-swallowing-audit-2026-05-20.md` §5.1.
