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

## CODEGEN-BYTES-001 (P1)

**Item:** ResolvedTy::Bytes LLVM value representation + bytes-intrinsic lowering

The `bytes` type today materialises through MIR but `hew-codegen-rs/src/llvm.rs:2498-2500`
returns `CodegenError::FailClosed("parity-or-tracked-gap")` for any `bytes`
local. The MIR producer already emits calls to `hew_bytes_index` and
`hew_bytes_slice` (landed in coll-S2, `9239294f`), so end-to-end works only
in scenarios where bytes values never reach codegen.

**Required work:**
1. Define `ResolvedTy::Bytes` LLVM value rep as `{ptr: ptr, offset: u32, len: u32}`
   struct alloca (matching the runtime triple).
2. Add `lower_call_runtime_abi` arms in `hew-codegen-rs/src/llvm.rs` for
   `hew_bytes_index` and `hew_bytes_slice` that splay the MIR-place into the
   3-arg runtime call.
3. Replace the `Unsupported` arm at `llvm.rs:2498-2500` with the real lowering.
4. Add bytes vertical-slice fixtures: `bytes_index`, `bytes_slice`,
   `bytes_slice_oob_panics`, `bytes_slice_overflow_aborts`.

**Trigger:** User code that materialises a `bytes` local and then accesses it.

**LESSONS:** Existing rows sufficient (boundary-fail-closed P0:49, parity-or-tracked-gap P0:46).

## OPEN-END-SLICE-001 (P2)

**Item:** Open-end slice syntax (`s[a..]`, `s[..]`, `b[a..]`, `b[..]`)

MIR producer emits `MirDiagnostic::NotYetImplemented` because the
`hew_string_char_count` runtime ABI returns `i32` and MIR has no `IntExtend`
instruction to widen to the `i64` slot the runtime slice entry expects.
Symmetric problem for bytes (`hew_bytes_len`).

**Options:**
1. New runtime helpers `hew_string_char_count_i64` / `hew_bytes_len_i64`.
2. Add MIR `IntExtend` instruction (more general, unlocks future widening needs).

**Preference:** Option 2 (MIR IntExtend); broader payoff.

**Trigger:** User writes `s[1..]` or `b[..]`.

## RECORDS-MUT-001 (post-v0.5 architectural review)

**Item:** `var` records allow field-write (Q296=B chosen for v0.5)

Q296 chose option B at the user level: records bound by `var` allow direct
field assignment (`r.field = value`) without explicit `mut` per-field.
Architectural review post-v0.5 should evaluate whether per-field `mut`
discipline (option A) provides better guarantees in larger codebases.

**Trigger:** Post-v0.5 retro on records mutability ergonomics.

## DISPATCH-INVARIANT-001 (P0 process)

**Item:** Every orchestrator dispatch brief MUST include the scope-fence
forbidding touches to `tests/vertical-slice/reject/machine_task_gate_fork_args.hew`
and other VS fixture comments.

**Snapshot:** `.tmp/orchestration/dispatch-boilerplate-scope-fence.md`.

**Trigger:** Author of a new dispatch brief.

**Status:** Active. All 7 W3+W4 lanes landed 2026-05-25/26 honored this fence.
