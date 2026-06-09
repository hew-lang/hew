//! Per-actor state-field clone/drop classification (W2.002 Stage 1).
//!
//! This module is the MIR-layer substrate the W2.002 codegen lane consumes
//! to wire `hew_actor_set_state_clone` / `hew_actor_set_state_drop`
//! registration calls onto every actor spawn and every supervisor child
//! registration. Stage 1 produces the classifier output only; Stages 2-4
//! consume it to synthesize the per-actor `__hew_state_clone_<A>` /
//! `__hew_state_drop_<A>` C-ABI bodies and emit the registration setters.
//!
//! ## Why "classify" rather than "always memcpy"?
//!
//! The runtime's W2.001 ABI (`hew-runtime/src/actor.rs:760-790`) carries a
//! `state_clone_fn` slot whose contract is that the returned `*mut c_void`
//! owns an **independent** copy of the state graph. For trivially
//! bit-copyable states (only primitives or `#[repr(C)]` actor refs) a
//! single wrapper-level `malloc + memcpy` produces an independent copy —
//! no per-field action is required. For owned-heap fields (`String`,
//! `Vec<T>`, `Bytes`, nested user records), the byte-copy would alias the
//! source's heap allocation, violating the W2.001 invariant. The
//! classifier identifies which fields need a per-field clone helper and
//! which fields are bit-copy-correct already.
//!
//! ## Builtin-vs-user-record name shadowing (cross-eco review fix)
//!
//! `ResolvedTy::Named { name, args }` does not carry the checker's
//! builtin-vs-user discriminator across the resolved-ty boundary. A
//! user-declared `type Connection { ... }` and the runtime builtin
//! `net.Connection` both arrive at `classify_named` as `Named { name:
//! "Connection", args: [] }`. The classifier therefore looks
//! `record_layouts` up FIRST for every `Named` arm; only when the name
//! is not user-shadowed does it fall through to the builtin-name match
//! (`Connection` / `ActorRef` / `Vec` / `HashMap` / `HashSet` /
//! `string` / `bytes`).
//!
//! This is the local mitigation for dispatch-invariant #10
//! (`string-identifier-fragility`). The structural fix — propagating
//! a typed builtin discriminator past the checker boundary so MIR
//! never has to guess from a string — is tracked as **W4.011** (HIR
//! resolution gap: `ResolvedTy::Named` drops builtin discriminator).
//! Once W4.011 lands, the `record_layouts`-first guards in
//! `classify_named` become dead code; the lowercase `string`/`bytes`
//! fallback similarly becomes dead. Until then, every `Named { name }`
//! arm in this module MUST honour `record_layouts`-first — adding a
//! new builtin name without that guard re-opens the misclassification
//! bug.
//!
//! ## Visited-set rationale (reviewer P0 #4)
//!
//! `StateFieldCloneKind::UserRecord { name }` recurses into the record's
//! field types. The Hew type system does not currently permit
//! self-referential records (no boxed indirection — confirmed by the
//! audit at `.tmp/orchestration/audits/w2.002-state-clone-actor-
//! classification.md` §1), but the classifier carries a `visited:
//! HashSet<String>` keyed on record name as defence-in-depth: if a future
//! addition (e.g. `Box<T>` or boxed-recursive enums) introduces a cycle,
//! the classifier terminates with `ClassificationError::RecordCycle`
//! rather than recursing forever. String-keying is correct here because
//! record names are globally unique post-name-resolution; W4.007's typed
//! `RecordId` substrate would be a strict improvement and the classifier
//! should be migrated to it once that substrate lands. See dispatch-
//! invariant #10 — `visited: HashSet<String>` is by-design here.
//!
//! ## Drop safety (CLAUDE.md custom #1)
//!
//! Stage 1 allocates nothing on the synthesized-fn side — it only
//! produces classification metadata that lives in `IrPipeline`. The MIR
//! `IrPipeline` is owned by the caller of `lower_hir_module`. No new
//! heap acquisition is introduced in this stage. Sync-return: standard
//! RAII drop of the produced `Vec<StateFieldCloneKind>`. Async cancel:
//! N/A — `classify_actor_state_fields` is synchronous, no `.await`. Actor
//! shutdown: N/A — this is compile-time MIR construction, not runtime.
//! Stage 3's synthesized `__hew_state_clone_<A>` body re-confronts the
//! three-context table.

use std::collections::HashSet;

use hew_types::ResolvedTy;

use crate::model::RecordLayout;

/// One actor-state field's classification for the synthesized
/// `__hew_state_clone_<A>` / `__hew_state_drop_<A>` body.
///
/// The variant set is **closed** per Stage 0's `audits/w2.002-state-clone-
/// actor-classification.md` finding that the 110-actor corpus exhibits
/// only the shapes enumerated here. Any field type the classifier cannot
/// place into one of these arms returns `ClassificationError::Unsupported`
/// — Stage 1 is fail-closed (`no-silent-no-op-stubs`).
///
/// Plan refs: `.tmp/orchestration/plans/waves/w2/w2.002-state-clone-
/// codegen-plan.md` §6 Stage 1, §4.5.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StateFieldCloneKind {
    /// Field whose bits constitute the value: primitives, `bool`, `char`,
    /// `Duration`, `Unit`, `Never`, and `#[repr(C)]` bit-copy aggregates
    /// such as `HewActorRef` / `LocalPid<T>` / `Actor<T>` / `ActorRef<T>`.
    ///
    /// Plan §4.5 A: "`HewActorRef` is `#[repr(C)]` bit-copyable — NOT
    /// refcount-bump". The Stage 3 wholesale `memcpy(dst, src,
    /// state_size)` covers this arm; no per-field helper is required.
    ///
    /// `size_bytes` is best-effort metadata for codegen sizing. For
    /// target-dependent shapes (`Isize`/`Usize`, `HewActorRef`) the value
    /// is `0` and codegen recomputes from the LLVM type. Concrete sizes
    /// are populated for fixed-width primitives so Stage 2 / Stage 3 can
    /// emit IR golden expectations without re-deriving the size.
    BitCopy { size_bytes: u64 },

    /// `String` — `hew_string_clone` / `hew_string_drop` helpers.
    /// (`hew-runtime/src/string.rs:1112,1125`.)
    String,

    /// `Bytes` — refcounted; `hew_bytes_clone_ref` / `hew_bytes_drop`.
    /// (`hew-runtime/src/bytes.rs:238,254`.)
    Bytes,

    /// `Vec<T>` — actor-state clone/drop routes through the layout-managed
    /// witness pair `hew_vec_clone_managed` / `hew_vec_free_managed`, derived
    /// by codegen from `collection_layout_witness` (the sole clone/drop symbol
    /// authority, alongside the HashMap/HashSet `*_layout` family). The managed
    /// pair reads the `HewTypeLayout` descriptor stamped into the handle at
    /// construction and clones/frees layout-backed `Plain`/`String` Vecs,
    /// failing closed only on `LayoutManaged` elements. The legacy
    /// `hew_vec_clone` / `hew_vec_free` symbols are NOT used for state fields;
    /// they remain only for non-state Vec paths (locals, returns, user
    /// `Vec.clone()`) and their retirement is W5.003 scope. The inner element
    /// classification is carried for Stage 3 to decide whether the layout-absent
    /// fallback's generic byte-clone is sound (`BitCopy` element) or whether a
    /// per-element deep walk is required (owned-heap element).
    Vec { elem: Box<StateFieldCloneKind> },

    /// `HashMap<K, V>` — gated on `hew_hashmap_*` runtime helper
    /// availability. Corpus has zero users (audit §3); the variant exists
    /// so the synthetic-fixture path (plan §4.6) can be exercised once
    /// the runtime helper lands. Until then the classifier still
    /// produces this arm but Stage 3 emission must fail-closed at codegen
    /// time, not at runtime (`td-debt-not-runtime-surprise`).
    HashMap {
        key: Box<StateFieldCloneKind>,
        val: Box<StateFieldCloneKind>,
    },

    /// `HashSet<T>` — same caveat as `HashMap`.
    HashSet { elem: Box<StateFieldCloneKind> },

    /// IO handle whose underlying resource has no clone/dup runtime
    /// helper. Plan §4.5 B: direct-spawn byte-copy is sound (move
    /// semantics); supervisor-restart deep-clone is **codegen-time
    /// `FailClosed`**.
    IoHandle { kind: IoHandleKind },

    /// Nested user record. The recursion target's classification lives in
    /// `RecordLayout` and is computed lazily by `classify_state_field`
    /// with a `visited: HashSet<String>` guard.
    ///
    /// Stage 3 synthesizes a recursive `__hew_state_clone_<RecordName>`
    /// helper alongside `__hew_state_clone_<Actor>`; this variant carries
    /// only the record name so codegen can resolve the helper symbol by
    /// the existing `mangle_state_clone_fn(name)` pattern.
    UserRecord { name: String },
}

/// IO-handle subkind. Today the only inhabitant is `Connection`; the
/// audit at §6 push-back #3 documents the open question for other handle
/// types (`Listener`, `tls.Connection`, `quic.QUICConnection`). They are
/// not yet seen in actor state in the audited corpus, so they are not
/// modelled here; adding them must extend this enum AND extend
/// `classify_state_field`'s handle-arm match, which is fail-closed today.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IoHandleKind {
    /// `net.Connection` (`hew-runtime/src/transport.rs:2020`). No
    /// `hew_connection_dup` runtime helper exists. Direct spawn is
    /// move-semantic (byte-copy at spawn). Supervisor-restart clone path
    /// must `FailClosed` at codegen time per plan §4.5 B.
    Connection,
}

/// Errors the classifier returns when the field shape exceeds the closed
/// `StateFieldCloneKind` variant set, or when the recursion guard fires.
///
/// Every variant is consumed at MIR-construction time and surfaces as a
/// `MirDiagnostic` (caller-converted) — no `unreachable!` and no
/// runtime-deferred failure (CLAUDE.md custom #2 + A249).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClassificationError {
    /// A user-record field's recursion bottomed out on a name already
    /// visited on the active stack. The Hew type system does not permit
    /// self-referential records today (audit §1), so observing this in
    /// practice is a checker-invariant violation — the classifier still
    /// fails-closed rather than infinite-looping.
    RecordCycle { name: String },
    /// The recursion descended into a `UserRecord { name }` whose
    /// `RecordLayout` is absent from `IrPipeline.record_layouts`. This
    /// is a lowering-invariant violation; surfaced as a diagnostic so
    /// the MIR producer can attribute the missing layout to its source
    /// site.
    MissingRecordLayout { name: String },
    /// A field type was outside the closed variant set. The carried
    /// rendering uses `ResolvedTy::Debug` so diagnostics show what shape
    /// was rejected (an actor with `Function`-typed state, a closure
    /// capture, a `dyn Trait`, etc.).
    Unsupported { rendered: String },
}

impl std::fmt::Display for ClassificationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ClassificationError::RecordCycle { name } => write!(
                f,
                "actor-state classifier encountered record-name cycle on `{name}` \
                 (checker invariant: self-referential records are not permitted)",
            ),
            ClassificationError::MissingRecordLayout { name } => write!(
                f,
                "actor-state classifier could not resolve `RecordLayout` for nested \
                 user record `{name}` (lowering-invariant: record layouts must precede \
                 actor layouts in `lower_hir_module`)",
            ),
            ClassificationError::Unsupported { rendered } => write!(
                f,
                "actor-state classifier does not yet support field type `{rendered}` \
                 (Stage 0 audit enumerated the supported shapes; extending requires a \
                 new `StateFieldCloneKind` variant AND a runtime helper)",
            ),
        }
    }
}

/// Mangled symbol for an actor's per-actor synthesized clone fn. This
/// matches the format Stage 2/3 codegen looks up via `get_function`.
/// Kept in one place so the producer (MIR lowering) and the consumer
/// (LLVM emission) never drift on the naming scheme — drift here would
/// surface as a Stage 2 `CodegenError::FailClosed` ("clone fn missing")
/// at every spawn site.
#[must_use]
pub fn mangle_actor_state_clone_fn(actor_name: &str) -> String {
    format!("__hew_state_clone_{actor_name}")
}

/// Mangled symbol for an actor's per-actor synthesized drop fn. Same
/// drift-prevention reasoning as `mangle_actor_state_clone_fn`.
#[must_use]
pub fn mangle_actor_state_drop_fn(actor_name: &str) -> String {
    format!("__hew_state_drop_{actor_name}")
}

/// Classify every field of an actor's state in declaration order.
///
/// Returns one `StateFieldCloneKind` per element of `state_field_tys`, in
/// the same order. Fail-closed: any unsupported shape or recursion-guard
/// trip aborts the whole classification with `ClassificationError`. No
/// partial result is returned — Stage 2/3 must not be allowed to emit a
/// clone fn for an actor that has any unclassifiable field.
///
/// `record_layouts` must include every record reachable from the field
/// list (and transitively from nested user records). `lower_hir_module`
/// today populates `record_layouts` from `HirItem::Record` and from each
/// `HirItem::Actor`'s state-record-mirror in a first pass before
/// `actor_layouts` are built, so the lookup invariant holds.
///
/// # Errors
///
/// Returns `ClassificationError` if any field type falls outside the
/// closed `StateFieldCloneKind` variant set, or if the recursion guard
/// fires on a self-referential user record, or if a nested record's
/// layout is absent from `record_layouts`. No partial result is
/// returned — fail-closed per `no-silent-no-op-stubs`.
pub fn classify_actor_state_fields(
    state_field_tys: &[ResolvedTy],
    record_layouts: &[RecordLayout],
) -> Result<Vec<StateFieldCloneKind>, ClassificationError> {
    let mut visited: HashSet<String> = HashSet::new();
    state_field_tys
        .iter()
        .map(|ty| classify_state_field(ty, record_layouts, &mut visited))
        .collect()
}

/// Single-field classifier. Public so test fixtures can exercise the
/// per-shape arms without building a full `ActorLayout`. `visited` is
/// supplied by the caller so nested-record recursion can re-enter
/// `classify_state_field` with the active stack preserved.
///
/// # Errors
///
/// Same conditions as [`classify_actor_state_fields`].
#[allow(
    clippy::implicit_hasher,
    reason = "the substrate is internally-consumed; callers don't pick the hasher"
)]
pub fn classify_state_field(
    ty: &ResolvedTy,
    record_layouts: &[RecordLayout],
    visited: &mut HashSet<String>,
) -> Result<StateFieldCloneKind, ClassificationError> {
    match ty {
        // --- BitCopy primitives ---------------------------------------
        ResolvedTy::I8 | ResolvedTy::U8 | ResolvedTy::Bool => {
            Ok(StateFieldCloneKind::BitCopy { size_bytes: 1 })
        }
        ResolvedTy::I16 | ResolvedTy::U16 => Ok(StateFieldCloneKind::BitCopy { size_bytes: 2 }),
        ResolvedTy::I32 | ResolvedTy::U32 | ResolvedTy::F32 | ResolvedTy::Char => {
            Ok(StateFieldCloneKind::BitCopy { size_bytes: 4 })
        }
        ResolvedTy::I64 | ResolvedTy::U64 | ResolvedTy::F64 | ResolvedTy::Duration => {
            Ok(StateFieldCloneKind::BitCopy { size_bytes: 8 })
        }
        // `Isize` / `Usize` are target-dependent (32-bit on WASM32,
        // 64-bit on native). `Unit` and `Never` are zero-sized.
        // Carrying `0` here signals "codegen must compute the size from
        // the LLVM type" — Stage 2/3 will use the native pointer width
        // on the LLVM backend and the wasm32 width on the WASM backend
        // (Stage 4). Zero-sized actor state fields are exotic but
        // legal; the wholesale memcpy is a no-op for them. Substrate
        // stays target-clean.
        ResolvedTy::Isize | ResolvedTy::Usize | ResolvedTy::Unit | ResolvedTy::Never => {
            Ok(StateFieldCloneKind::BitCopy { size_bytes: 0 })
        }

        // --- Owned-heap primitives ------------------------------------
        ResolvedTy::String => Ok(StateFieldCloneKind::String),
        ResolvedTy::Bytes => Ok(StateFieldCloneKind::Bytes),
        // --- Container / handle / record arms ------------------------
        ResolvedTy::Named { name, args, .. } => classify_named(name, args, record_layouts, visited),

        // --- Closed-set rejection -------------------------------------
        // Pointer, Function, Closure, TraitObject, Tuple, Array, Slice,
        // and Task are not seen in any audited actor state field. The
        // checker rejects most of them at actor-state position already
        // (Q* rejects); reaching them here would be a checker bug, but
        // we fail-closed (`no-silent-no-op-stubs`) so the bug surfaces
        // as a MIR diagnostic at the actor-decl site rather than as a
        // crash in Stage 3 codegen.
        ResolvedTy::CancellationToken
        | ResolvedTy::Pointer { .. }
        | ResolvedTy::Function { .. }
        | ResolvedTy::Closure { .. }
        | ResolvedTy::TraitObject { .. }
        | ResolvedTy::Tuple(_)
        | ResolvedTy::Array(..)
        | ResolvedTy::Slice(_)
        | ResolvedTy::Task(_) => Err(ClassificationError::Unsupported {
            rendered: format!("{ty:?}"),
        }),
    }
}

fn classify_named(
    name: &str,
    args: &[ResolvedTy],
    record_layouts: &[RecordLayout],
    visited: &mut HashSet<String>,
) -> Result<StateFieldCloneKind, ClassificationError> {
    // ── record-layouts-first lookup (cross-eco review fix) ──────────
    //
    // `ResolvedTy::Named { name, args }` has already dropped the
    // checker's builtin discriminator (see `hew-types/src/resolved_ty.rs`
    // around the `from_ty` boundary): a user-declared `type Connection
    // { ... }` and the runtime builtin `net.Connection` both reach this
    // function as `Named { name: "Connection", args: [] }`. Routing on
    // name alone would silently misclassify a user record named after
    // a builtin (Connection / Vec / HashMap / HashSet / ActorRef /
    // string / bytes) — bypassing the recursive-record arm and the
    // owned-heap drop helpers, which would leak (Stage 3) or UAF
    // (post-Q185(c) lift, plan §8.8).
    //
    // Therefore: if `record_layouts` carries a user record under this
    // name, classify as `UserRecord` and recurse through the record's
    // fields. Only when the name is NOT a user record do we fall
    // through to the builtin-name arms below. This is the local
    // mitigation of dispatch-invariant #10 (`string-identifier-
    // fragility`); the structural fix — propagating a typed builtin
    // discriminator past the checker boundary — is tracked as W4.011
    // ("HIR resolution gap: ResolvedTy::Named drops builtin
    // discriminator"). Until W4.011 lands, every `Named { name }` arm
    // in this module MUST honour record_layouts-first.
    if record_layouts.iter().any(|r| r.name == name) {
        return classify_user_record(name, record_layouts, visited);
    }

    // ── Builtin name arms (only reached when not user-shadowed) ─────

    // Actor-reference handle types are bit-copyable per audit §1 +
    // plan §4.5 A. Codegen sizing of `HewActorRef` is target-dependent
    // (see Pointer note above) so `size_bytes` is `0`; Stage 3's
    // wholesale memcpy handles the bytes, and per-field synthesis is
    // nil for this arm.
    if matches!(name, "ActorRef" | "Actor" | "LocalPid") {
        return Ok(StateFieldCloneKind::BitCopy { size_bytes: 0 });
    }

    // `Connection` IO handle. Plan §4.5 B. No `hew_connection_dup` in
    // the runtime today; the classifier records the kind and Stage 2
    // emits a codegen-time `CodegenError::FailClosed` at the
    // supervisor-restart site. Direct-spawn byte-copy is sound.
    if name == "Connection" {
        return Ok(StateFieldCloneKind::IoHandle {
            kind: IoHandleKind::Connection,
        });
    }

    // Lowercase keyword fallbacks for HIR resolution gaps (see W4.011).
    // In practice the type-resolver maps `string` / `bytes` to
    // `ResolvedTy::String` / `ResolvedTy::Bytes`, but actor-state
    // fields occasionally surface them as `Named` instead. Classify
    // identically so we never silently route an owned-heap field
    // into BitCopy / UserRecord. Dead code once W4.011 lands.
    match name {
        "string" if args.is_empty() => return Ok(StateFieldCloneKind::String),
        "bytes" if args.is_empty() => return Ok(StateFieldCloneKind::Bytes),
        _ => {}
    }

    // Containers.
    match name {
        "Vec" => {
            let elem = args
                .first()
                .ok_or_else(|| ClassificationError::Unsupported {
                    rendered: format!("Named {{ name: \"Vec\", args: {args:?} }}"),
                })?;
            let elem_kind = classify_state_field(elem, record_layouts, visited)?;
            Ok(StateFieldCloneKind::Vec {
                elem: Box::new(elem_kind),
            })
        }
        "HashMap" => {
            let key = args
                .first()
                .ok_or_else(|| ClassificationError::Unsupported {
                    rendered: format!("Named {{ name: \"HashMap\", args: {args:?} }}"),
                })?;
            let val = args
                .get(1)
                .ok_or_else(|| ClassificationError::Unsupported {
                    rendered: format!("Named {{ name: \"HashMap\", args: {args:?} }}"),
                })?;
            let key_kind = classify_state_field(key, record_layouts, visited)?;
            let val_kind = classify_state_field(val, record_layouts, visited)?;
            Ok(StateFieldCloneKind::HashMap {
                key: Box::new(key_kind),
                val: Box::new(val_kind),
            })
        }
        "HashSet" => {
            let elem = args
                .first()
                .ok_or_else(|| ClassificationError::Unsupported {
                    rendered: format!("Named {{ name: \"HashSet\", args: {args:?} }}"),
                })?;
            let elem_kind = classify_state_field(elem, record_layouts, visited)?;
            Ok(StateFieldCloneKind::HashSet {
                elem: Box::new(elem_kind),
            })
        }
        // Not a builtin and not in record_layouts → still try
        // `classify_user_record` so the missing-layout diagnostic
        // surfaces with the correct field name (rather than a generic
        // Unsupported). `classify_user_record` returns
        // `MissingRecordLayout` here; lowering-invariant violation.
        _ => classify_user_record(name, record_layouts, visited),
    }
}

fn classify_user_record(
    name: &str,
    record_layouts: &[RecordLayout],
    visited: &mut HashSet<String>,
) -> Result<StateFieldCloneKind, ClassificationError> {
    if !visited.insert(name.to_string()) {
        return Err(ClassificationError::RecordCycle {
            name: name.to_string(),
        });
    }
    let layout = record_layouts
        .iter()
        .find(|r| r.name == name)
        .ok_or_else(|| ClassificationError::MissingRecordLayout {
            name: name.to_string(),
        })?;
    // Eagerly classify every field of the nested record. The result is
    // discarded here — the caller (Stage 3 codegen) re-runs the same
    // classification against the same `RecordLayout` to synthesize the
    // nested `__hew_state_clone_<name>` body, and the substrate-level
    // assertion is that the classification *succeeds*. If a nested
    // field is unsupported, this is where the error surfaces.
    for field_ty in &layout.field_tys {
        let _ = classify_state_field(field_ty, record_layouts, visited)?;
    }
    visited.remove(name);
    Ok(StateFieldCloneKind::UserRecord {
        name: name.to_string(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn no_records() -> Vec<RecordLayout> {
        Vec::new()
    }

    #[test]
    fn primitives_bitcopy_with_concrete_sizes() {
        let mut v = HashSet::new();
        assert_eq!(
            classify_state_field(&ResolvedTy::I64, &no_records(), &mut v).unwrap(),
            StateFieldCloneKind::BitCopy { size_bytes: 8 },
        );
        assert_eq!(
            classify_state_field(&ResolvedTy::I32, &no_records(), &mut v).unwrap(),
            StateFieldCloneKind::BitCopy { size_bytes: 4 },
        );
        assert_eq!(
            classify_state_field(&ResolvedTy::Bool, &no_records(), &mut v).unwrap(),
            StateFieldCloneKind::BitCopy { size_bytes: 1 },
        );
        assert_eq!(
            classify_state_field(&ResolvedTy::Duration, &no_records(), &mut v).unwrap(),
            StateFieldCloneKind::BitCopy { size_bytes: 8 },
        );
        // Isize/Usize sentinel-zero (target-dependent — codegen fills in).
        assert_eq!(
            classify_state_field(&ResolvedTy::Isize, &no_records(), &mut v).unwrap(),
            StateFieldCloneKind::BitCopy { size_bytes: 0 },
        );
    }

    #[test]
    fn string_and_bytes_route_to_owned_heap_arms() {
        let mut v = HashSet::new();
        assert_eq!(
            classify_state_field(&ResolvedTy::String, &no_records(), &mut v).unwrap(),
            StateFieldCloneKind::String,
        );
        assert_eq!(
            classify_state_field(&ResolvedTy::Bytes, &no_records(), &mut v).unwrap(),
            StateFieldCloneKind::Bytes,
        );
    }

    #[test]
    fn actor_ref_classifies_as_bitcopy_per_audit_section_1() {
        // Plan §4.5 A: `HewActorRef` is `#[repr(C)]` bit-copyable, NOT
        // refcount-bump. This test pins the contract — if a future
        // change reintroduces a dedicated `ActorRef` variant, this test
        // breaks and forces a re-read of plan §4.5 A.
        let mut v = HashSet::new();
        let ty = ResolvedTy::Named {
            name: "ActorRef".to_string(),
            args: vec![ResolvedTy::Named {
                name: "SomeActor".to_string(),
                args: vec![],
                builtin: None,
            }],
            builtin: None,
        };
        assert_eq!(
            classify_state_field(&ty, &no_records(), &mut v).unwrap(),
            StateFieldCloneKind::BitCopy { size_bytes: 0 },
        );
    }

    #[test]
    fn connection_classifies_as_iohandle_per_audit_section_6() {
        let mut v = HashSet::new();
        let ty = ResolvedTy::Named {
            name: "Connection".to_string(),
            args: vec![],
            builtin: None,
        };
        assert_eq!(
            classify_state_field(&ty, &no_records(), &mut v).unwrap(),
            StateFieldCloneKind::IoHandle {
                kind: IoHandleKind::Connection,
            },
        );
    }

    #[test]
    fn vec_of_primitive_recurses_to_bitcopy_element() {
        let mut v = HashSet::new();
        let ty = ResolvedTy::Named {
            name: "Vec".to_string(),
            args: vec![ResolvedTy::I32],
            builtin: None,
        };
        assert_eq!(
            classify_state_field(&ty, &no_records(), &mut v).unwrap(),
            StateFieldCloneKind::Vec {
                elem: Box::new(StateFieldCloneKind::BitCopy { size_bytes: 4 }),
            },
        );
    }

    #[test]
    fn vec_of_string_recurses_to_string_element() {
        let mut v = HashSet::new();
        let ty = ResolvedTy::Named {
            name: "Vec".to_string(),
            args: vec![ResolvedTy::String],
            builtin: None,
        };
        assert_eq!(
            classify_state_field(&ty, &no_records(), &mut v).unwrap(),
            StateFieldCloneKind::Vec {
                elem: Box::new(StateFieldCloneKind::String),
            },
        );
    }

    #[test]
    fn vec_of_connection_carries_iohandle_through() {
        // Exercises the mqtt_broker `Vec<Connection>` shape from the
        // audit. Stage 2 emission consumes the nested kind to detect
        // Connection-in-container and emit the codegen-time FailClosed
        // at supervisor sites.
        let mut v = HashSet::new();
        let ty = ResolvedTy::Named {
            name: "Vec".to_string(),
            args: vec![ResolvedTy::Named {
                name: "Connection".to_string(),
                args: vec![],
                builtin: None,
            }],
            builtin: None,
        };
        assert_eq!(
            classify_state_field(&ty, &no_records(), &mut v).unwrap(),
            StateFieldCloneKind::Vec {
                elem: Box::new(StateFieldCloneKind::IoHandle {
                    kind: IoHandleKind::Connection,
                }),
            },
        );
    }

    #[test]
    fn hashmap_recurses_into_both_key_and_value() {
        let mut v = HashSet::new();
        let ty = ResolvedTy::Named {
            name: "HashMap".to_string(),
            args: vec![ResolvedTy::String, ResolvedTy::I64],
            builtin: None,
        };
        assert_eq!(
            classify_state_field(&ty, &no_records(), &mut v).unwrap(),
            StateFieldCloneKind::HashMap {
                key: Box::new(StateFieldCloneKind::String),
                val: Box::new(StateFieldCloneKind::BitCopy { size_bytes: 8 }),
            },
        );
    }

    #[test]
    fn user_record_recurses_with_visited_set() {
        // Plan §4.6 synthetic fixture: `Workspace { entries: Vec<Entry>,
        // name: string }` where `Entry { id: string, payload: Vec<i32>
        // }`. Both records register in `record_layouts`; classifier
        // descends and the visited-set is empty on exit.
        let records = vec![
            RecordLayout {
                name: "Workspace".to_string(),
                field_tys: vec![
                    ResolvedTy::Named {
                        name: "Vec".to_string(),
                        args: vec![ResolvedTy::Named {
                            name: "Entry".to_string(),
                            args: vec![],
                            builtin: None,
                        }],
                        builtin: None,
                    },
                    ResolvedTy::String,
                ],
            },
            RecordLayout {
                name: "Entry".to_string(),
                field_tys: vec![
                    ResolvedTy::String,
                    ResolvedTy::Named {
                        name: "Vec".to_string(),
                        args: vec![ResolvedTy::I32],
                        builtin: None,
                    },
                ],
            },
        ];
        let ty = ResolvedTy::Named {
            name: "Workspace".to_string(),
            args: vec![],
            builtin: None,
        };
        let mut v = HashSet::new();
        let result = classify_state_field(&ty, &records, &mut v).unwrap();
        assert_eq!(
            result,
            StateFieldCloneKind::UserRecord {
                name: "Workspace".to_string(),
            },
        );
        // Termination invariant: the visited-set is empty after a
        // successful classification — `classify_user_record` removes
        // each name on the way out so sibling fields can reference the
        // same nested record without spurious cycle errors.
        assert!(v.is_empty(), "visited-set leaked after success: {v:?}");
    }

    #[test]
    fn user_record_diamond_is_not_a_cycle() {
        // A diamond — `Top { left: Leaf, right: Leaf }` — must succeed
        // even though `Leaf` is visited twice along *different* stacks.
        // This pins the contract that `visited` tracks the active
        // recursion stack, not the cumulative set of seen records.
        let records = vec![
            RecordLayout {
                name: "Top".to_string(),
                field_tys: vec![
                    ResolvedTy::Named {
                        name: "Leaf".to_string(),
                        args: vec![],
                        builtin: None,
                    },
                    ResolvedTy::Named {
                        name: "Leaf".to_string(),
                        args: vec![],
                        builtin: None,
                    },
                ],
            },
            RecordLayout {
                name: "Leaf".to_string(),
                field_tys: vec![ResolvedTy::I64],
            },
        ];
        let mut v = HashSet::new();
        let result = classify_state_field(
            &ResolvedTy::Named {
                name: "Top".to_string(),
                args: vec![],
                builtin: None,
            },
            &records,
            &mut v,
        )
        .unwrap();
        assert_eq!(
            result,
            StateFieldCloneKind::UserRecord {
                name: "Top".to_string(),
            },
        );
        assert!(v.is_empty());
    }

    #[test]
    fn user_record_self_cycle_fails_closed() {
        // Hypothetical `Node { next: Node, value: i32 }` — the
        // checker rejects this today (no boxed indirection), but the
        // classifier's visited-set defends as defence-in-depth. Stage
        // 1 pins the fail-closed behaviour so a future `Box<T>` /
        // boxed enum addition does not silently produce stack
        // overflow at MIR-construction.
        let records = vec![RecordLayout {
            name: "Node".to_string(),
            field_tys: vec![
                ResolvedTy::Named {
                    name: "Node".to_string(),
                    args: vec![],
                    builtin: None,
                },
                ResolvedTy::I32,
            ],
        }];
        let mut v = HashSet::new();
        let result = classify_state_field(
            &ResolvedTy::Named {
                name: "Node".to_string(),
                args: vec![],
                builtin: None,
            },
            &records,
            &mut v,
        );
        assert!(
            matches!(result, Err(ClassificationError::RecordCycle { ref name }) if name == "Node"),
            "expected RecordCycle on `Node`, got {result:?}",
        );
    }

    #[test]
    fn missing_record_layout_fails_closed() {
        // Lowering invariant: every record reachable from a state field
        // must be in `record_layouts`. Violating it surfaces here, not
        // at Stage 3.
        let mut v = HashSet::new();
        let result = classify_state_field(
            &ResolvedTy::Named {
                name: "Phantom".to_string(),
                args: vec![],
                builtin: None,
            },
            &no_records(),
            &mut v,
        );
        assert!(matches!(
            result,
            Err(ClassificationError::MissingRecordLayout { ref name }) if name == "Phantom"
        ));
    }

    #[test]
    fn unsupported_function_typed_state_fails_closed() {
        // `dispatch_invariants #3 no-silent-no-op-stubs` — the
        // classifier returns Err, not a default kind, for shapes
        // outside the closed set.
        let mut v = HashSet::new();
        let result = classify_state_field(
            &ResolvedTy::Function {
                params: vec![ResolvedTy::I32],
                ret: Box::new(ResolvedTy::Unit),
            },
            &no_records(),
            &mut v,
        );
        assert!(matches!(
            result,
            Err(ClassificationError::Unsupported { .. })
        ));
    }

    #[test]
    fn mangle_symbols_match_stage2_expected_format() {
        // The producer/consumer contract: Stage 2 codegen looks up
        // `__hew_state_clone_<Actor>` / `__hew_state_drop_<Actor>` via
        // `llvm_mod.get_function(&actor_layout.state_clone_fn_symbol)`.
        // If these formats change, Stage 2 fails-closed with
        // "synthesized clone fn missing" at every spawn site — but
        // pinning the format here catches the drift at unit-test time.
        assert_eq!(
            mangle_actor_state_clone_fn("Counter"),
            "__hew_state_clone_Counter",
        );
        assert_eq!(
            mangle_actor_state_drop_fn("Counter"),
            "__hew_state_drop_Counter",
        );
    }
}
