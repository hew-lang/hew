//! The declared-release adoption authority: which `#[resource]` record types
//! have a declared `close` that is their ENTIRE release plan.
//!
//! A `#[resource]` record with a declared `close` is released by codegen's
//! `__hew_record_drop_inplace_<R>` thunk calling `<R>::close(self)` FIRST and
//! only then tearing the fields down field-wise. When every declared field is
//! one that post-close teardown provably cannot free (a scalar leaf or an
//! `#[opaque]` handle declared in this module), the declared destructor is the
//! whole drop plan: constructing such a record is the program taking delivery
//! of a foreign handle and naming `close` as its release.
//!
//! Both IR layers consult this same question — HIR's produced-value fact
//! resolution (is a resource-record construction an adoption the program
//! owns?) and MIR's return-provenance / shell-drop authorities (is the
//! declared close the sole release authority for this payload?). Hosting the
//! computation here keeps it ONE authority; `hew-mir` re-exports it rather
//! than re-deriving the clauses.
//!
//! # Membership, and the clause that keeps it sound
//!
//! A type is admitted when all three hold:
//!
//! 1. it carries `ResourceMarker::Resource` in the module's
//!    [`TypeClassTable`](crate::TypeClassTable);
//! 2. that same table entry names its close method — the identical
//!    `(marker, close)` entry lowering admits into the typed lifecycle
//!    registry, so this authority and codegen cannot disagree about which
//!    types have a declared release;
//! 3. **every declared field is one the post-close field-wise teardown cannot
//!    free** — a scalar leaf, or an `#[opaque]` handle declared in this module
//!    (an `#[opaque]` decl is a pointer-width slot with no fields and no
//!    structural drop).
//!
//! Clause 3 is the whole soundness argument and it is why this is not simply
//! "`#[resource]` types are exempt". For a type that satisfies it the
//! field-wise second half frees nothing, so the type's entire release is the
//! one declared call and no compiler-generated free can reach an operand. A
//! `#[resource]` type with a heap-owning field — `#[resource] type Conn
//! { raw: Sock; log: string; }` — is NOT admitted: its `log` really is torn
//! down field-wise after `close`, so a foreign value in that position would be
//! freed by a plan the program never declared. That is the fail-closed
//! direction, and it costs a leak rather than a double release.

use std::collections::HashSet;

use hew_types::ResolvedTy;

/// The set of declaration identities (exact full paths) whose declared `close`
/// is the type's entire release plan — the three-clause admission above,
/// computed from the module's type declarations and its `#[resource]` close
/// registry. An adoption proof is nominal: a same-leaf declaration from
/// another owner must never inherit it.
#[must_use]
pub fn declared_release_type_names(module: &crate::HirModule) -> HashSet<String> {
    let opaque_handles: HashSet<&str> = module
        .items
        .iter()
        .filter_map(|item| match item {
            crate::HirItem::TypeDecl(decl) if decl.is_opaque => Some(decl.declaration.full_path()),
            _ => None,
        })
        .collect();
    let mut names = HashSet::new();
    for item in &module.items {
        let crate::HirItem::TypeDecl(decl) = item else {
            continue;
        };
        // Clauses 1 and 2 — the `#[resource]` marker AND a declared close,
        // read from the one table codegen's thunk synthesis reads.
        let declares_close = module
            .type_classes
            .get(decl.declaration.full_path())
            .is_some_and(|(marker, close)| {
                matches!(marker, crate::ResourceMarker::Resource) && close.is_some()
            });
        if !declares_close || decl.fields.is_empty() {
            continue;
        }
        // Clause 3 — the post-close field-wise teardown must free nothing.
        if !decl
            .fields
            .iter()
            .all(|field| field_is_released_only_by_the_declared_close(&field.ty, &opaque_handles))
        {
            continue;
        }
        names.insert(decl.declaration.full_path().to_string());
    }
    names
}

/// True for a field type the `#[resource]` record-drop thunk's post-close
/// field-wise teardown provably does not free: a scalar leaf, or a named
/// `#[opaque]` handle declared in this module.
///
/// Deliberately narrow. It answers from the field type's own spelling plus the
/// module's `#[opaque]` declaration set, so an unknown or unresolved named type
/// answers `false` and its declaring `#[resource]` type is simply not admitted.
/// Widening this to "not heap-owning under the layout registry" would admit more
/// types, but a layout registry that is absent or partial reads a composite as
/// non-heap, which is the permissive direction — the exact `Default`-shaped
/// fail-open the authority was hardened against.
#[must_use]
pub fn field_is_released_only_by_the_declared_close<S: std::hash::BuildHasher>(
    ty: &ResolvedTy,
    opaque_handles: &HashSet<&str, S>,
) -> bool {
    if ty_is_scalar_non_heap(ty) {
        return true;
    }
    let ResolvedTy::Named {
        name,
        args,
        is_opaque,
        ..
    } = ty
    else {
        return false;
    };
    if !args.is_empty() {
        return false;
    }
    *is_opaque || opaque_handles.contains(name.as_str())
}

/// True for a resolved type that is a scalar (or `unit`/`never`) leaf — a value
/// that provably owns no heap and therefore cannot alias any heap parameter.
///
/// Conservative on purpose: it fires ONLY for the primitive-scalar leaves the
/// type short-circuit needs without a layout registry (`semver`'s `maj/min/pat`
/// are `i64`). A composite whose fields are all scalar is NOT short-circuited
/// here — that needs the `ty_owns_heap` layout authority, threaded in at the
/// wiring site (S2); leaving it to the structural aggregate recursion is sound
/// (less precise, never unsound).
#[must_use]
pub fn ty_is_scalar_non_heap(ty: &ResolvedTy) -> bool {
    matches!(
        ty,
        ResolvedTy::I8
            | ResolvedTy::I16
            | ResolvedTy::I32
            | ResolvedTy::I64
            | ResolvedTy::U8
            | ResolvedTy::U16
            | ResolvedTy::U32
            | ResolvedTy::U64
            | ResolvedTy::Isize
            | ResolvedTy::Usize
            | ResolvedTy::F32
            | ResolvedTy::F64
            | ResolvedTy::Bool
            | ResolvedTy::Char
            | ResolvedTy::Duration
            | ResolvedTy::Unit
            | ResolvedTy::Never
    )
}
