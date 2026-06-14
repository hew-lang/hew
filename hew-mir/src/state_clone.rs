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

use crate::model::{EnumLayout, RecordLayout};

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

    /// Tagged-union (`enum`) field: `Option<T>`, `Result<T, E>`, or any
    /// user-declared `enum`. Carries only the registry key of the
    /// `EnumLayout` (its `name` — the plain decl name for a monomorphic
    /// enum, or the `hew_hir::mangle`d name for a generic instantiation
    /// such as `Option$$string`). Codegen (W5.006 Slices 3/4) re-resolves
    /// the `EnumLayout` from the registry — the SINGLE authority — to
    /// synthesize the tag-aware clone/drop dispatch, so the clone and drop
    /// sides can never drift on which variant's payload they act over
    /// (W4.045 UAF class / `lifecycle-symmetry`). This mirrors the
    /// name-only `UserRecord` arm: the classifier carries no per-variant
    /// payload classification here — it only validates (during
    /// classification) that every variant payload field is itself
    /// classifiable, then defers symbol selection to the layout-driven
    /// codegen authority.
    Enum { name: String },

    /// Closure pair (`fn(...) -> T` surface type): a two-pointer
    /// `{ fn_ptr, env_ptr }` value whose non-null `env_ptr` points at the
    /// captures region of a heap env box carrying its own free thunk at
    /// `env_ptr - 8` (the heap-promotion substrate's sole-owner contract).
    ///
    /// **Drop**: wired. `emit_field_drop_step` loads the env slot, runs the
    /// planted free thunk when non-null, and null-stores the slot — the same
    /// shape as the standalone `DropKind::ClosurePair` binding drop, so the
    /// record's drop releases the env exactly once as the pair's sole owner.
    ///
    /// **Clone**: refused. The env box's size/align are baked into the
    /// per-closure free thunk and are unknowable at a record-clone site (any
    /// closure can inhabit the field), so no deep copy can be emitted; and
    /// the sole-owner model forbids aliasing the env without a refcount.
    /// Every reachable clone entry point is rejected at compile time (actor
    /// state refuses closure-valued fields; owned-Vec element harvesting
    /// skips closure-bearing records); the synthesised record-clone arm
    /// itself branches to the rollback/fail path as a defensive runtime
    /// backstop, returning the protocol's failure code instead of aliasing
    /// the env. A true closure-value clone (retain or deep env copy) is
    /// deferred until a concrete surface needs it.
    ClosurePair,

    /// `#[opaque]` runtime handle declared in a stdlib or user module (e.g.
    /// `json.Value`, `yaml.Value`, `cron.Expr`). The clone direction has no
    /// dup helper — supervisor-restart clone fails closed (`FailClosed`).
    ///
    /// **Drop**: the actor-state drop path is a **no-op / leak** for opaque
    /// handles. `drop_helper_for_kind(OpaqueHandle) => Ok(None)` and
    /// `emit_field_drop_step(OpaqueHandle) => Ok(())` both no-op in codegen.
    /// The handle is intentionally not freed by the actor state drop path:
    /// the user is responsible for calling `.free()` on the handle before the
    /// actor exits. A user that does not call `.free()` leaks the handle.
    /// This is the documented ownership contract for `#[opaque]` types
    /// (std/encoding/json/json.hew: "dropping without `free()` is a resource
    /// leak"). A future `#[resource]`/`#[linear]` upgrade would enforce the
    /// call discipline at type-check time.
    ///
    /// **Ownership semantics**: the handle is pointer-width. A byte-copy at
    /// enum construction moves (not aliases) the handle — exactly one owner
    /// holds it. No refcount; no dup.
    OpaqueHandle { name: String },
}

impl StateFieldCloneKind {
    /// True when this kind is, or transitively contains, an [`OpaqueHandle`].
    ///
    /// The `ResolvedTy`-level authority [`crate::ty_contains_unclonable_opaque`]
    /// already makes the MIR classifier fail closed before a
    /// `Vec`/`HashMap`/`HashSet` carrying an opaque handle is ever produced, so a
    /// container kind nesting an `OpaqueHandle` is unreachable in practice. This
    /// kind-level mirror is the codegen BACKSTOP: `collection_layout_witness`
    /// consults it so that if any future path synthesised such a kind and reached
    /// codegen, the managed-collection clone/free symbol is refused rather than
    /// silently shallow-copying the opaque pointer (double-free / UAF on restart).
    ///
    /// [`OpaqueHandle`]: StateFieldCloneKind::OpaqueHandle
    #[must_use]
    pub fn contains_opaque_handle(&self) -> bool {
        match self {
            StateFieldCloneKind::OpaqueHandle { .. } => true,
            StateFieldCloneKind::Vec { elem } | StateFieldCloneKind::HashSet { elem } => {
                elem.contains_opaque_handle()
            }
            StateFieldCloneKind::HashMap { key, val } => {
                key.contains_opaque_handle() || val.contains_opaque_handle()
            }
            // `UserRecord` / `Enum` carry only a registry key here; codegen
            // re-classifies their fields/payloads (which fail closed at the
            // `ResolvedTy` authority), so there is nothing to recurse into at the
            // kind level. Leaf bitcopy/heap kinds carry no opaque.
            StateFieldCloneKind::BitCopy { .. }
            | StateFieldCloneKind::String
            | StateFieldCloneKind::Bytes
            | StateFieldCloneKind::IoHandle { .. }
            | StateFieldCloneKind::UserRecord { .. }
            | StateFieldCloneKind::Enum { .. }
            | StateFieldCloneKind::ClosurePair => false,
        }
    }

    /// True when this kind is, or transitively contains, a [`ClosurePair`].
    ///
    /// Kind-level codegen backstop, mirroring [`contains_opaque_handle`]: the
    /// managed-collection clone/free witness must refuse a container whose
    /// element transitively carries a closure pair — the managed clone would
    /// shallow-copy the pair and create two owners of one environment box
    /// (double free at the two containers' releases). Closure-pair `Vec<fn>`
    /// handles have their own dedicated release
    /// (`hew_vec_free_closure_pairs`) and never ride the managed witness.
    ///
    /// [`ClosurePair`]: StateFieldCloneKind::ClosurePair
    /// [`contains_opaque_handle`]: StateFieldCloneKind::contains_opaque_handle
    #[must_use]
    pub fn contains_closure_pair(&self) -> bool {
        match self {
            StateFieldCloneKind::ClosurePair => true,
            StateFieldCloneKind::Vec { elem } | StateFieldCloneKind::HashSet { elem } => {
                elem.contains_closure_pair()
            }
            StateFieldCloneKind::HashMap { key, val } => {
                key.contains_closure_pair() || val.contains_closure_pair()
            }
            StateFieldCloneKind::BitCopy { .. }
            | StateFieldCloneKind::String
            | StateFieldCloneKind::Bytes
            | StateFieldCloneKind::IoHandle { .. }
            | StateFieldCloneKind::UserRecord { .. }
            | StateFieldCloneKind::Enum { .. }
            | StateFieldCloneKind::OpaqueHandle { .. } => false,
        }
    }

    /// True when a field of this kind is admissible to the owned-aggregate
    /// record value-class (the `__hew_record_{clone,drop}_inplace_<R>` in-place
    /// record spine).
    ///
    /// An owned-aggregate record admitted by the value-class gate is seeded for
    /// `DropKind::RecordInPlace`, which drives codegen to synthesise BOTH the
    /// clone AND the drop body for the record (`emit_state_clone_drop_synthesis`,
    /// llvm.rs ~6517). Every field of such a record must therefore have BOTH a
    /// clone and a drop helper:
    ///   - `BitCopy` — covered by the wholesale memcpy; no per-field helper.
    ///   - `String` / `Bytes` — `hew_string_*` / `hew_bytes_*` helpers.
    ///   - `Vec` / `HashMap` / `HashSet` — managed `collection_layout_witness`
    ///     clone/free pair, provided the element/key/value is itself supported
    ///     and the container carries no opaque handle (the managed clone would
    ///     shallow-copy an opaque pointer → double-free / UAF on a deep clone).
    ///   - `UserRecord` / `Enum` — recursively synthesised per-record / per-enum
    ///     helper (the codegen `emit_field_{clone,drop}_step` `UserRecord`/`Enum`
    ///     arms dispatch to the nested thunk).
    ///
    /// Returns `false` ONLY for `OpaqueHandle` (and any container transitively
    /// nesting one). An opaque handle (`json.Value`, `cron.Expr`) has no dup
    /// runtime helper, so the record's synthesised CLONE body would fail closed
    /// LATE at codegen clone-synthesis (`clone_helper_for_kind`, llvm.rs ~6254).
    /// A record carrying one must NOT be admitted to the owned-aggregate
    /// value-class: today it admits at the MIR gate (→ `CowValue`), is seeded for
    /// `RecordInPlace`, and only fails closed during clone-synthesis — a fragile
    /// MIR-admits/codegen-refuses seam. This predicate moves the refusal back to
    /// the W3.029 value-class gate (`fail-closed at the gate, not late`).
    ///
    /// `IoHandle` (`Generator`/`Stream`/`Sink`/`Connection`/`CancellationToken`)
    /// is admitted: a `ValueClass::AffineResource`/`Linear` field is NOT
    /// owned-aggregate-by-value, so a record carrying one is dropped field-wise
    /// through the resource-drop path (`hew_gen_free` / `*_close`) and is NEVER
    /// seeded for `RecordInPlace`, so its clone body is never synthesised. The
    /// proven `Holder { inner: Generator<i64,()> }` corpus (`generator_exec`)
    /// drops the handle exactly once with no record clone/drop thunk emitted —
    /// rejecting `IoHandle` here would over-reject that working shape at the
    /// value-class gate.
    ///
    /// The `UserRecord` / `Enum` arms carry only a registry key here; their
    /// nested fields/payloads are re-classified by the recursive classifier,
    /// which fails closed at the `ResolvedTy` authority
    /// (`ty_contains_unclonable_opaque`) before an opaque-bearing nested record
    /// or enum can be produced — so a `true` here for a nested record/enum is
    /// only reached when that record/enum itself classified cleanly.
    #[must_use]
    pub fn supports_value_class_drop_spine(&self) -> bool {
        match self {
            // ClosurePair joins the leaf-admitted set DROP-ONLY: the record's
            // synthesised drop body releases the env via the planted free
            // thunk; the clone arm is a protocol-conformant runtime refusal
            // (rollback + failure code) whose reachable entry points are all
            // rejected at compile time — see the variant doc. Same drop-wired
            // / clone-refused posture as `IoHandle`, except the record IS
            // seeded for `RecordInPlace` (the drop side is the entire point
            // of admitting fn-typed record fields).
            StateFieldCloneKind::BitCopy { .. }
            | StateFieldCloneKind::String
            | StateFieldCloneKind::Bytes
            | StateFieldCloneKind::IoHandle { .. }
            | StateFieldCloneKind::UserRecord { .. }
            | StateFieldCloneKind::Enum { .. }
            | StateFieldCloneKind::ClosurePair => true,
            // Containers: supported iff they carry no opaque handle (the managed
            // clone/free witness fails closed on an opaque-bearing container),
            // no closure pair (the managed clone would shallow-copy the pair
            // and alias its sole-owner environment; closure-pair Vecs ride the
            // dedicated `hew_vec_free_closure_pairs` release, not the managed
            // witness), and the element/key/value kind is itself supported.
            StateFieldCloneKind::Vec { elem } | StateFieldCloneKind::HashSet { elem } => {
                !self.contains_opaque_handle()
                    && !self.contains_closure_pair()
                    && elem.supports_value_class_drop_spine()
            }
            StateFieldCloneKind::HashMap { key, val } => {
                !self.contains_opaque_handle()
                    && !self.contains_closure_pair()
                    && key.supports_value_class_drop_spine()
                    && val.supports_value_class_drop_spine()
            }
            // Opaque handle: no dup runtime helper, so the synthesised record
            // clone body fails closed. Reject at the value-class gate.
            StateFieldCloneKind::OpaqueHandle { .. } => false,
        }
    }
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
    /// `Stream<T>` read handle. Its scope-exit release is `hew_stream_close`
    /// (the same symbol `Stream.close()` and the single-handle drop path use).
    /// No `hew_stream_dup` runtime helper exists, so the clone/restart path
    /// fails closed; only the drop direction is wired (W5.021 — owned-tuple
    /// drop spine). Pointer-backed handle: a byte-copy aliases the pointer with
    /// no refcount, so exactly one owner must close it.
    Stream,
    /// `Sink<T>` write handle. Scope-exit release is `hew_sink_close` (the same
    /// symbol `Sink.close()` and the single-handle drop path use). No dup
    /// helper; clone/restart fails closed, drop is wired (W5.021).
    Sink,
    /// `Generator<Y, R>` / `AsyncGenerator<Y>` runtime handle (`*mut HewGenCtx`).
    /// Scope-exit release is `hew_gen_free` (cancel-if-running, join the
    /// generator thread, drain unconsumed yields, free the context) — the same
    /// symbol the standalone-binding drop (`drop_kind_for`) uses. No dup helper
    /// exists, so the clone/restart direction fails closed, exactly like
    /// `Stream`/`Sink`. A composite carrying this handle (e.g. a returned
    /// `(Generator<i64, ()>, i64)`) must free it exactly once at the owner's
    /// scope exit; the exactly-once discipline rests on the move-checker plus the
    /// MIR drop-allow derivations, with the runtime's `hew_gen_free` null-guard
    /// as a backstop.
    Generator,
    /// `CancellationToken` runtime handle (`*mut HewCancellationToken`).
    /// Scope-exit release is `hew_cancel_token_release` (ref-count decrement,
    /// free at zero — null-tolerant) — the same symbol the standalone-binding
    /// drop uses. Ref-counted, so a clone could in principle retain, but no
    /// composite clone path is wired today; the clone direction fails closed
    /// alongside `Generator` for symmetry until a retain-on-clone seam exists.
    CancellationToken,
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
    /// A container/composite (`Vec<T>`, `Option<T>`, `Result<T, E>`,
    /// `HashMap<K, V>`, `HashSet<T>`, a record field, an enum payload, or any
    /// nesting thereof) transitively carries a `#[opaque]` runtime handle with
    /// no clone-dup helper. A bare opaque field already routes to
    /// `StateFieldCloneKind::OpaqueHandle` (clone fails closed per-kind); this
    /// variant fails the WHOLE composite closed at classification time because
    /// the container's managed clone / plain element layout would shallow-copy
    /// the opaque pointer and double-free / UAF on supervisor restart. The
    /// authoritative check is `model::ty_contains_unclonable_opaque`. `outer`
    /// names the container/composite shape; `opaque` names the handle.
    OpaqueInContainer { outer: String, opaque: String },
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
            ClassificationError::OpaqueInContainer { outer, opaque } => write!(
                f,
                "actor-state field `{outer}` transitively carries opaque handle \
                 `{opaque}`, which has no clone-dup runtime helper; cloning the \
                 container at supervisor restart would shallow-copy the handle and \
                 double-free / use-after-free. Remove the opaque handle from the \
                 container, or hold it directly (a direct opaque field is rejected with \
                 a dedicated diagnostic) and re-acquire it on restart.",
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
    classify_actor_state_fields_with_enum_layouts(state_field_tys, record_layouts, &[])
}

/// Enum-aware companion to [`classify_actor_state_fields`].
///
/// Identical to [`classify_actor_state_fields`] but additionally consults the
/// `enum_layouts` registry so a tagged-union field (`Option<T>`,
/// `Result<T, E>`, any user `enum`) classifies as
/// [`StateFieldCloneKind::Enum`] instead of failing closed as
/// `MissingRecordLayout`. The MIR producer's actor-state path
/// (`lower_hir_module`) calls this with the cumulative enum-layout list so
/// enum-typed actor-state fields are representable. The bare
/// [`classify_actor_state_fields`] shim retains the pre-W5.006 behaviour
/// (no enum registry → enum fields fail closed) for call sites that do not
/// carry an enum-layout registry.
///
/// # Errors
///
/// Same conditions as [`classify_actor_state_fields`].
pub fn classify_actor_state_fields_with_enum_layouts(
    state_field_tys: &[ResolvedTy],
    record_layouts: &[RecordLayout],
    enum_layouts: &[EnumLayout],
) -> Result<Vec<StateFieldCloneKind>, ClassificationError> {
    classify_actor_state_fields_with_opaque_handles(
        state_field_tys,
        record_layouts,
        enum_layouts,
        &[],
    )
}

/// Opaque-handle-aware companion to [`classify_actor_state_fields_with_enum_layouts`].
///
/// Identical to [`classify_actor_state_fields_with_enum_layouts`] but also
/// consults `opaque_handle_names` so a `#[opaque]`-typed field (e.g.
/// `json.Value`, `yaml.Value`) classifies as
/// [`StateFieldCloneKind::OpaqueHandle`] instead of failing as
/// `MissingRecordLayout`. `opaque_handle_names` is `IrPipeline::opaque_handle_names`
/// populated by `lower_hir_module` from `HirItem::TypeDecl { is_opaque: true }`.
///
/// # Errors
///
/// Same conditions as [`classify_actor_state_fields_with_enum_layouts`].
pub fn classify_actor_state_fields_with_opaque_handles(
    state_field_tys: &[ResolvedTy],
    record_layouts: &[RecordLayout],
    enum_layouts: &[EnumLayout],
    opaque_handle_names: &[String],
) -> Result<Vec<StateFieldCloneKind>, ClassificationError> {
    let mut visited: HashSet<String> = HashSet::new();
    state_field_tys
        .iter()
        .map(|ty| {
            classify_state_field_full(
                ty,
                record_layouts,
                enum_layouts,
                opaque_handle_names,
                &mut visited,
            )
        })
        .collect()
}

/// owned-string-record classifier for a direct user record with owned `string` fields.
///
/// This intentionally reuses the actor-state classifier, then narrows its
/// output to the first slice's closed surface: declaration-order fields must
/// be either [`StateFieldCloneKind::BitCopy`] or [`StateFieldCloneKind::String`],
/// and at least one field must be `String`. Nested records, enums, bytes,
/// collections, IO handles, and any unsupported type remain outside owned-string-record.
///
/// # Errors
///
/// Same conditions as [`classify_actor_state_fields_with_enum_layouts`].
pub fn classify_owned_string_record_fields(
    field_tys: &[ResolvedTy],
    record_layouts: &[RecordLayout],
    enum_layouts: &[EnumLayout],
) -> Result<Option<Vec<StateFieldCloneKind>>, ClassificationError> {
    let kinds =
        classify_actor_state_fields_with_enum_layouts(field_tys, record_layouts, enum_layouts)?;
    let mut has_string = false;
    for kind in &kinds {
        match kind {
            StateFieldCloneKind::BitCopy { .. } => {}
            StateFieldCloneKind::String => has_string = true,
            StateFieldCloneKind::Bytes
            | StateFieldCloneKind::Vec { .. }
            | StateFieldCloneKind::HashMap { .. }
            | StateFieldCloneKind::HashSet { .. }
            | StateFieldCloneKind::IoHandle { .. }
            | StateFieldCloneKind::UserRecord { .. }
            | StateFieldCloneKind::Enum { .. }
            | StateFieldCloneKind::ClosurePair
            | StateFieldCloneKind::OpaqueHandle { .. } => return Ok(None),
        }
    }
    Ok(has_string.then_some(kinds))
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
    classify_state_field_impl(ty, record_layouts, &[], visited)
}

/// Enum-aware companion to [`classify_state_field`]. Consults `enum_layouts`
/// so a tagged-union field classifies as [`StateFieldCloneKind::Enum`]
/// instead of failing closed. See
/// [`classify_actor_state_fields_with_enum_layouts`].
///
/// # Errors
///
/// Same conditions as [`classify_state_field`].
#[allow(
    clippy::implicit_hasher,
    reason = "the substrate is internally-consumed; callers don't pick the hasher"
)]
pub fn classify_state_field_with_enum_layouts(
    ty: &ResolvedTy,
    record_layouts: &[RecordLayout],
    enum_layouts: &[EnumLayout],
    visited: &mut HashSet<String>,
) -> Result<StateFieldCloneKind, ClassificationError> {
    classify_state_field_impl(ty, record_layouts, enum_layouts, visited)
}

/// Full classifier: enum-aware and opaque-handle-aware.
///
/// Opaque-handle-aware companion to [`classify_state_field_with_enum_layouts`].
/// `opaque_handle_names` is `IrPipeline::opaque_handle_names`.
///
/// # Errors
///
/// Same conditions as [`classify_state_field`].
#[allow(
    clippy::implicit_hasher,
    reason = "the substrate is internally-consumed; callers don't pick the hasher"
)]
pub fn classify_state_field_full(
    ty: &ResolvedTy,
    record_layouts: &[RecordLayout],
    enum_layouts: &[EnumLayout],
    opaque_handle_names: &[String],
    visited: &mut HashSet<String>,
) -> Result<StateFieldCloneKind, ClassificationError> {
    classify_state_field_full_impl(
        ty,
        record_layouts,
        enum_layouts,
        opaque_handle_names,
        visited,
    )
}

fn classify_state_field_impl(
    ty: &ResolvedTy,
    record_layouts: &[RecordLayout],
    enum_layouts: &[EnumLayout],
    visited: &mut HashSet<String>,
) -> Result<StateFieldCloneKind, ClassificationError> {
    classify_state_field_full_impl(ty, record_layouts, enum_layouts, &[], visited)
}

fn classify_state_field_full_impl(
    ty: &ResolvedTy,
    record_layouts: &[RecordLayout],
    enum_layouts: &[EnumLayout],
    opaque_handle_names: &[String],
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
        // --- Container / handle / record / enum arms -----------------
        ResolvedTy::Named {
            name,
            args,
            is_opaque,
            ..
        } => classify_named(
            name,
            args,
            *is_opaque,
            record_layouts,
            enum_layouts,
            opaque_handle_names,
            visited,
        ),

        // --- Closed-set rejection -------------------------------------
        // Pointer, Function, Closure, TraitObject, Tuple, Array, Slice,
        // and Task are not seen in any audited actor state field. The
        // checker rejects most of them at actor-state position already
        // (Q* rejects); reaching them here would be a checker bug, but
        // we fail-closed (`no-silent-no-op-stubs`) so the bug surfaces
        // as a MIR diagnostic at the actor-decl site rather than as a
        // crash in Stage 3 codegen.
        //
        // `TypeParam` joins this group: an abstract parameter has no
        // concrete clone strategy until monomorphisation substitutes it,
        // so an actor-state field typed by an unsubstituted parameter is
        // a producer bug that must fail closed here.
        // `CancellationToken` is a ref-counted runtime handle whose scope-exit
        // release is `hew_cancel_token_release`. Classify it as a drop-only
        // IO-handle so a composite carrying one (e.g. a returned tuple) frees it
        // exactly once at the owner's scope exit instead of leaking it.
        ResolvedTy::CancellationToken => Ok(StateFieldCloneKind::IoHandle {
            kind: IoHandleKind::CancellationToken,
        }),

        // Closure pair (`fn(...) -> T`): drop-only admission — the record
        // drop releases the env via the planted free thunk; the clone
        // direction is refused (see the `ClosurePair` variant doc). Actor
        // state keeps rejecting closure-valued fields via the ty-level
        // containment guard at the actor classification site, so this arm
        // is reached only for user-record value-class classification.
        ResolvedTy::Function { .. } | ResolvedTy::Closure { .. } => {
            Ok(StateFieldCloneKind::ClosurePair)
        }

        ResolvedTy::Pointer { .. }
        | ResolvedTy::Borrow { .. }
        | ResolvedTy::TraitObject { .. }
        | ResolvedTy::Tuple(_)
        | ResolvedTy::Array(..)
        | ResolvedTy::Slice(_)
        | ResolvedTy::Task(_)
        | ResolvedTy::TypeParam { .. } => Err(ClassificationError::Unsupported {
            rendered: format!("{ty:?}"),
        }),
    }
}

/// Fail-closed guard for a container element/key/value: if `elem` transitively
/// carries a `#[opaque]` runtime handle (no clone-dup helper), reject the WHOLE
/// container so a managed/plain clone can never shallow-copy the opaque pointer
/// and double-free / UAF on supervisor restart. Delegates the transitive walk to
/// the single `model::ty_contains_unclonable_opaque` authority so `Vec` /
/// `Option` / `Result` / `HashMap` / nested containers / record-with-opaque-field
/// / enum-payload all fail closed through one decision, not per-shape patches.
fn reject_unclonable_opaque_container(
    outer: &str,
    elem: &ResolvedTy,
    record_layouts: &[RecordLayout],
    enum_layouts: &[EnumLayout],
) -> Result<(), ClassificationError> {
    if crate::model::ty_contains_unclonable_opaque(elem, record_layouts, enum_layouts) {
        return Err(ClassificationError::OpaqueInContainer {
            outer: outer.to_string(),
            opaque: format!("{}", elem.user_facing()),
        });
    }
    Ok(())
}

#[allow(
    clippy::too_many_lines,
    reason = "dispatch function: each arm is a distinct named type; splitting would obscure the exhaustion pattern"
)]
fn classify_named(
    name: &str,
    args: &[ResolvedTy],
    is_opaque: bool,
    record_layouts: &[RecordLayout],
    enum_layouts: &[EnumLayout],
    opaque_handle_names: &[String],
    visited: &mut HashSet<String>,
) -> Result<StateFieldCloneKind, ClassificationError> {
    // Non-opaque user records/enums still shadow builtin names. Keep that
    // boundary before the `Connection` resource-handle exception below so a
    // user `type Connection { ... }` does not become a socket handle.
    if !is_opaque {
        if let Some(layout) = lookup_record_layout(name, args, record_layouts) {
            return classify_user_record(
                &layout.name,
                record_layouts,
                enum_layouts,
                opaque_handle_names,
                visited,
            );
        }
        if let Some(layout) = lookup_enum_layout(name, args, enum_layouts) {
            return classify_enum(
                layout,
                record_layouts,
                enum_layouts,
                opaque_handle_names,
                visited,
            );
        }
    }

    // `net.Connection` is a stdlib opaque handle, but unlike generic opaque
    // blobs it has actor-owned fd teardown semantics. Route it through the
    // existing IO-handle discriminator before the generic opaque fail-closed arm
    // so codegen can apply the restart-safe null-clone policy.
    if args.is_empty() && name == "Connection" {
        return Ok(StateFieldCloneKind::IoHandle {
            kind: IoHandleKind::Connection,
        });
    }

    // ── opaque-handle discriminator (authoritative, checked FIRST) ──────
    //
    // `is_opaque` is the type-identity discriminator stamped on
    // `ResolvedTy::Named` by `hew-hir::lower::lower_type` from the program's
    // `#[opaque]` type-decl set. When it is `true` this `Named` resolved to a
    // real opaque runtime handle (e.g. `json.Value`, `cron.Expr`) — a
    // pointer-width handle with no clone-dup helper, whose clone direction
    // MUST fail closed (a shallow `memcpy` would duplicate ownership of the
    // underlying allocation and double-free / UAF on supervisor restart).
    //
    // This check runs BEFORE the record-layouts and enum-layouts lookups
    // because the discriminator is authoritative: a real opaque handle whose
    // short name collides with a user `type Value` / `enum Value` arrives here
    // as `Named { name: "Value", is_opaque: true }`, while the user type
    // arrives as `Named { name: "Value", is_opaque: false }`. Routing on the
    // name string alone cannot tell them apart (both are `"Value"`), so the
    // earlier name-based ordering let a real opaque handle be captured by the
    // user record/enum layout and emit an unsafe shallow clone. Dispatching on
    // identity instead of name closes that false-negative for good.
    //
    // `indirect enum` types are also threaded through `opaque_handle_names`
    // (so codegen emits ptr-typed slots) but are NOT `#[opaque]` — they have
    // `is_opaque: false` here and an `EnumLayout`, so they fall through to the
    // enum-layouts arm and classify as `Enum`, not `OpaqueHandle`.
    if is_opaque {
        return Ok(StateFieldCloneKind::OpaqueHandle {
            name: name.to_string(),
        });
    }

    // ── record/enum-layout lookup provenance (handled above) ─────────
    //
    // `ResolvedTy::Named { name, args }` does not carry the checker's
    // builtin discriminator across this boundary: a user-declared `type
    // Connection { ... }` and the runtime builtin `net.Connection` both reach
    // this function as `Named { name: "Connection", args: [] }`. Routing on
    // name alone would silently misclassify a user record named after
    // a builtin (Connection / Vec / HashMap / HashSet / ActorRef /
    // string / bytes) — bypassing the recursive-record arm and the
    // owned-heap drop helpers, which would leak (Stage 3) or UAF
    // (post-Q185(c) lift, plan §8.8).
    //
    // Therefore: when `is_opaque == false`, the top of this function first
    // checks whether `record_layouts` or `enum_layouts` carries a user type
    // under this name and recurses through that layout. Only when the name is
    // not a user record/enum do we fall through to the builtin-name arms below.
    // This is the local
    // mitigation of dispatch-invariant #10 (`string-identifier-
    // fragility`); the structural fix — propagating a typed builtin
    // discriminator past the checker boundary — is tracked as W4.011
    // ("HIR resolution gap: ResolvedTy::Named drops builtin
    // discriminator"). Until W4.011 lands, every non-opaque `Named { name }`
    // arm in this module MUST honour record_layouts-first.
    //
    // The record lookup is keyed via `lookup_record_layout`, which resolves BOTH
    // a bare-name monomorphic record AND a generic INSTANTIATION mangled by
    // `hew_hir::mangle` (`Pair<i64, string>` → `Pair$$i64$string`). For the
    // generic case the resolved layout's `field_tys` are already SUBSTITUTED, so
    // `classify_user_record` recurses concrete types and carries the mangled key
    // in `UserRecord { name }` (the key codegen re-resolves the thunk by).
    // ── enum-layouts check ──────────────────────────────────────────
    //
    // `indirect enum` types are added to `opaque_handle_names` by
    // `lower_hir_module` so that codegen emits a bare `ptr` slot for
    // every variable of the type (heap-allocated tagged union). However,
    // an indirect enum IS a classified enum — it must route through
    // `classify_enum`, not be misidentified as a raw `OpaqueHandle`.
    //
    // The `is_opaque` discriminator already excluded real `#[opaque]`
    // handles above (they returned `OpaqueHandle`), so any `Named` reaching
    // here with an `EnumLayout` is a genuine enum (monomorphic, generic, or
    // indirect) and classifies as `Enum`.
    // Non-opaque enum layouts were handled before the `Connection` exception.

    // ── opaque-handle name fallback (defence-in-depth) ──────────────────
    //
    // The `is_opaque` discriminator above is the authoritative opaque check.
    // This name-set fallback remains ONLY as defence-in-depth for any path
    // that produces a `Named` for a `#[opaque]` decl without the discriminator
    // stamped (e.g. a `ResolvedTy` synthesised outside `lower_type`). It is
    // reached only after record-layouts-first and enum-layouts, so a user
    // record/enum that shadows the name still routes correctly. It never
    // captures a user type that has its own layout. `opaque_handle_names`
    // carries the unqualified decl name (`"Value"`); use sites may carry the
    // qualified form (`"json.Value"`), so match both the full name and the
    // short suffix.
    let short = name.rsplit_once('.').map_or(name, |(_, s)| s);
    if opaque_handle_names.iter().any(|n| n == name || n == short) {
        return Ok(StateFieldCloneKind::OpaqueHandle {
            name: name.to_string(),
        });
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

    // `Stream<T>` / `Sink<T>` pointer-backed IO handles (W5.021 — owned-tuple
    // drop spine). A tuple/record carrying these handles must close each one
    // exactly once at the owner's scope exit; the drop step routes to
    // `hew_stream_close` / `hew_sink_close` (the same symbols the single-handle
    // `.close()` and standalone-binding drop paths use). No dup helper exists,
    // so the clone/restart direction fails closed (handled in the codegen clone
    // step), matching the `Connection` posture.
    if name == "Stream" {
        return Ok(StateFieldCloneKind::IoHandle {
            kind: IoHandleKind::Stream,
        });
    }
    if name == "Sink" {
        return Ok(StateFieldCloneKind::IoHandle {
            kind: IoHandleKind::Sink,
        });
    }

    // `Generator<Y, R>` / `AsyncGenerator<Y>` pointer-backed runtime handle.
    // Same posture as Stream/Sink: drop routes to `hew_gen_free` (the standalone
    // generator-binding release symbol), no dup helper, clone/restart fails
    // closed. Without this arm a tuple/record/enum carrying a generator handle
    // falls through to `classify_user_record` and fails as `MissingRecordLayout`,
    // and (before this lane) the composite was mis-classified non-heap-owning so
    // its member-drop never ran — leaking the context + OS thread. A generator's
    // generic args (`i64`, `()`) never make the handle itself non-heap-owning, so
    // classification is by name alone, independent of `args`.
    if matches!(name, "Generator" | "AsyncGenerator") {
        return Ok(StateFieldCloneKind::IoHandle {
            kind: IoHandleKind::Generator,
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
            // Fail closed: a Vec whose element transitively carries an opaque
            // handle would clone as a managed vec with a plain element witness,
            // shallow-copying the opaque pointer (double-free / UAF on restart).
            // The single transitive authority covers Vec<json.Value>,
            // Vec<Option<json.Value>>, Vec<RecordWithOpaqueField>, etc.
            reject_unclonable_opaque_container("Vec", elem, record_layouts, enum_layouts)?;
            let elem_kind = classify_state_field_full_impl(
                elem,
                record_layouts,
                enum_layouts,
                opaque_handle_names,
                visited,
            )?;
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
            // Fail closed on either an opaque key or an opaque value.
            reject_unclonable_opaque_container("HashMap", key, record_layouts, enum_layouts)?;
            reject_unclonable_opaque_container("HashMap", val, record_layouts, enum_layouts)?;
            let key_kind = classify_state_field_full_impl(
                key,
                record_layouts,
                enum_layouts,
                opaque_handle_names,
                visited,
            )?;
            let val_kind = classify_state_field_full_impl(
                val,
                record_layouts,
                enum_layouts,
                opaque_handle_names,
                visited,
            )?;
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
            reject_unclonable_opaque_container("HashSet", elem, record_layouts, enum_layouts)?;
            let elem_kind = classify_state_field_full_impl(
                elem,
                record_layouts,
                enum_layouts,
                opaque_handle_names,
                visited,
            )?;
            Ok(StateFieldCloneKind::HashSet {
                elem: Box::new(elem_kind),
            })
        }
        // Not a builtin container and not a user record. Consult the
        // enum-layout registry BEFORE the `classify_user_record`
        // fall-through (closed-set invariant /
        // `exhaustive-traversal-and-lowering`): `Option<T>`,
        // `Result<T, E>`, and user `enum`s are registered as
        // `EnumLayout`s, never as `RecordLayout`s, so without this arm an
        // enum-typed field would fail closed as `MissingRecordLayout`.
        // Generic instantiations are keyed by the `hew_hir::mangle`d name
        // (e.g. `Option$$string`); monomorphic enums by their plain decl
        // name — mirroring `user_record_layout_key` in `lower.rs`.
        _ => {
            if let Some(layout) = lookup_enum_layout(name, args, enum_layouts) {
                return classify_enum(
                    layout,
                    record_layouts,
                    enum_layouts,
                    opaque_handle_names,
                    visited,
                );
            }
            // Resolve the record layout key the same mangling-aware way as the
            // record-layouts-first branch above so a generic instantiation that
            // reaches this fall-through (not a builtin, not an enum) recurses its
            // SUBSTITUTED fields and carries the mangled key. A genuine miss
            // passes the bare name to `classify_user_record`, which fails closed
            // as `MissingRecordLayout` naming the unresolved type.
            let record_key = lookup_record_layout(name, args, record_layouts)
                .map_or(name, |layout| layout.name.as_str());
            classify_user_record(
                record_key,
                record_layouts,
                enum_layouts,
                opaque_handle_names,
                visited,
            )
        }
    }
}

/// Resolve a `Named { name, args }` field type to its registered
/// [`EnumLayout`], if any. The lookup key mirrors `user_record_layout_key`
/// (`hew-mir/src/lower.rs`): the plain `name` for a monomorphic enum
/// (`args` empty), or the `hew_hir::mangle`d symbol for a generic
/// instantiation (e.g. `Option<string>` → `Option$$string`).
///
/// The generic arm shortens the WHOLE type-arg spine before mangling. The
/// registration side (`EnumLayoutRegistry::insert`, the `layout_mono` pass)
/// keys on the bare-normalised spine, so a use-site type carrying a
/// module-qualified payload (`Slot<lmonobox.Box>`) MUST shorten its args here
/// or the probe (`Slot$$lmonobox.Box`) diverges from the registered
/// `Slot$$Box`. Both the full-outer-name and short-outer-name candidates are
/// built from the SAME shortened args; only the outer name varies (to absorb a
/// qualified outer prefix). Nested qualified payloads are shortened at depth.
fn lookup_enum_layout<'a>(
    name: &str,
    args: &[ResolvedTy],
    enum_layouts: &'a [EnumLayout],
) -> Option<&'a EnumLayout> {
    let short = name.rsplit_once('.').map_or(name, |(_, s)| s);
    if args.is_empty() {
        enum_layouts
            .iter()
            .find(|el| el.name == name || el.name == short)
    } else {
        let short_args: Vec<ResolvedTy> = args
            .iter()
            .cloned()
            .map(hew_hir::shorten_named_arg_qualifiers)
            .collect();
        let full_mangled = hew_hir::mangle(name, &short_args);
        let short_mangled = hew_hir::mangle(short, &short_args);
        enum_layouts
            .iter()
            .find(|el| el.name == full_mangled || el.name == short_mangled)
    }
}

/// Resolve a `Named { name, args }` field type to its registered
/// [`RecordLayout`], if any. The lookup key mirrors `lookup_enum_layout` and
/// `user_record_layout_key` (`hew-mir/src/lower.rs`): the plain `name` for a
/// monomorphic record (`args` empty), or the `hew_hir::mangle`d symbol for a
/// generic INSTANTIATION (`Pair<i64, string>` → `Pair$$i64$string`). The
/// per-instantiation layout the producer registers under the mangled key
/// carries SUBSTITUTED field types (`[i64, string]`), so a caller that recurses
/// `layout.field_tys` recurses concrete types, never the bare `A`/`B` params.
/// Module-qualified names are matched both as the full name and the short
/// (post-`.`) suffix so imported records resolve. The generic arm shortens the
/// whole type-arg spine before mangling (identical discipline to
/// `lookup_enum_layout`): a record instantiated with a qualified payload
/// (`Holder<json.Value>`) registers under `Holder$$Value`, so the probe must
/// shorten its args to match. Nested qualified payloads are shortened at depth.
fn lookup_record_layout<'a>(
    name: &str,
    args: &[ResolvedTy],
    record_layouts: &'a [RecordLayout],
) -> Option<&'a RecordLayout> {
    let short = name.rsplit_once('.').map_or(name, |(_, s)| s);
    if args.is_empty() {
        record_layouts
            .iter()
            .find(|r| r.name == name || r.name == short)
    } else {
        let short_args: Vec<ResolvedTy> = args
            .iter()
            .cloned()
            .map(hew_hir::shorten_named_arg_qualifiers)
            .collect();
        let full_mangled = hew_hir::mangle(name, &short_args);
        let short_mangled = hew_hir::mangle(short, &short_args);
        record_layouts
            .iter()
            .find(|r| r.name == full_mangled || r.name == short_mangled)
    }
}

/// Classify a tagged-union (`enum`) field. Mirrors `classify_user_record`:
/// recurses into every variant's payload field types under the shared
/// `visited` recursion guard so an unclassifiable payload surfaces here as
/// a `ClassificationError` (fail-closed) rather than later in codegen, then
/// returns the name-only [`StateFieldCloneKind::Enum`] carrying the
/// registry key. Codegen (W5.006 Slices 3/4) re-resolves the `EnumLayout`
/// to drive tag-aware clone/drop — the single layout authority — so the
/// classifier need not carry per-variant payload classifications.
fn classify_enum(
    layout: &EnumLayout,
    record_layouts: &[RecordLayout],
    enum_layouts: &[EnumLayout],
    opaque_handle_names: &[String],
    visited: &mut HashSet<String>,
) -> Result<StateFieldCloneKind, ClassificationError> {
    if !visited.insert(layout.name.clone()) {
        // A name already in `visited` normally signals a value-type cycle
        // (infinite recursion without heap indirection). For `indirect` enums,
        // re-encountering the name is valid: the self-referential variant payload
        // is a heap-allocated pointer to the enum, not an inline embed. Return
        // `Enum` (ptr-shaped) instead of propagating `RecordCycle`.
        if layout.is_indirect {
            return Ok(StateFieldCloneKind::Enum {
                name: layout.name.clone(),
            });
        }
        return Err(ClassificationError::RecordCycle {
            name: layout.name.clone(),
        });
    }
    // Eagerly validate every variant payload field is classifiable. The
    // result is discarded — codegen re-runs classification against the
    // same `EnumLayout`. If any payload field is unsupported, this is
    // where the error surfaces (with the recursion stack preserved).
    for variant in &layout.variants {
        for field_ty in &variant.field_tys {
            let kind = classify_state_field_full_impl(
                field_ty,
                record_layouts,
                enum_layouts,
                opaque_handle_names,
                visited,
            )?;
            // Closure-pair enum payloads stay fail-closed: the tag-aware
            // enum clone/drop dispatch has no proven coverage for the
            // pair's sole-owner env discipline (the record spine carries
            // it; the enum spine does not yet). Transitive ty-level walk so
            // a payload of record-with-fn-field rejects too — the kind-level
            // `UserRecord` carries only a registry name and cannot see the
            // nested pair. Rejects here so an `Option<fn(...)>`-shaped enum
            // never reaches codegen with an env-owning payload the clone
            // walk would alias.
            if kind.contains_closure_pair()
                || crate::model::ty_contains_closure_value(field_ty, record_layouts, enum_layouts)
            {
                return Err(ClassificationError::Unsupported {
                    rendered: format!(
                        "enum `{}` variant `{}` carries a function-valued payload \
                         field ({field_ty:?}); closure values in enum payloads are \
                         not supported",
                        layout.name, variant.name
                    ),
                });
            }
        }
    }
    visited.remove(&layout.name);
    Ok(StateFieldCloneKind::Enum {
        name: layout.name.clone(),
    })
}

fn classify_user_record(
    name: &str,
    record_layouts: &[RecordLayout],
    enum_layouts: &[EnumLayout],
    opaque_handle_names: &[String],
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
    // field is unsupported, this is where the error surfaces. Enum-typed
    // record fields are threaded the enum registry so a record holding an
    // enum classifies, rather than failing closed.
    for field_ty in &layout.field_tys {
        let _ = classify_state_field_full_impl(
            field_ty,
            record_layouts,
            enum_layouts,
            opaque_handle_names,
            visited,
        )?;
    }
    visited.remove(name);
    Ok(StateFieldCloneKind::UserRecord {
        name: name.to_string(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::model::MachineVariantLayout;

    fn no_records() -> Vec<RecordLayout> {
        Vec::new()
    }

    /// Build a non-opaque `Named` type for test fixtures (user record/enum or
    /// builtin-by-name). Mirrors a `lower_type`-produced reference with
    /// `is_opaque: false`.
    fn named(name: &str, args: Vec<ResolvedTy>) -> ResolvedTy {
        ResolvedTy::named_user(name, args)
    }

    /// Build a `Named` type carrying the `#[opaque]` discriminator, as
    /// `lower_type` stamps for an opaque-handle reference (e.g. `json.Value`).
    fn named_opaque(name: &str, args: Vec<ResolvedTy>) -> ResolvedTy {
        ResolvedTy::named_opaque(name, args)
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
                is_opaque: false,
            }],
            builtin: None,
            is_opaque: false,
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
            is_opaque: false,
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
            is_opaque: false,
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
            is_opaque: false,
        };
        assert_eq!(
            classify_state_field(&ty, &no_records(), &mut v).unwrap(),
            StateFieldCloneKind::Vec {
                elem: Box::new(StateFieldCloneKind::String),
            },
        );
    }

    #[test]
    fn opaque_connection_classifies_as_iohandle() {
        let mut v = HashSet::new();
        let ty = ResolvedTy::Named {
            name: "Connection".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: true,
        };
        assert_eq!(
            classify_state_field_full(&ty, &no_records(), &[], &["Connection".to_string()], &mut v)
                .unwrap(),
            StateFieldCloneKind::IoHandle {
                kind: IoHandleKind::Connection,
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
                is_opaque: false,
            }],
            builtin: None,
            is_opaque: false,
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
            is_opaque: false,
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
                            is_opaque: false,
                        }],
                        builtin: None,
                        is_opaque: false,
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
                        is_opaque: false,
                    },
                ],
            },
        ];
        let ty = ResolvedTy::Named {
            name: "Workspace".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
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
                        is_opaque: false,
                    },
                    ResolvedTy::Named {
                        name: "Leaf".to_string(),
                        args: vec![],
                        builtin: None,
                        is_opaque: false,
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
                is_opaque: false,
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
                    is_opaque: false,
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
                is_opaque: false,
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
                is_opaque: false,
            },
            &no_records(),
            &mut v,
        );
        assert!(matches!(
            result,
            Err(ClassificationError::MissingRecordLayout { ref name }) if name == "Phantom"
        ));
    }

    /// Slice 2 — a generic record INSTANTIATION carrying an owned string field
    /// resolves its mangled layout (`Pair$$i64$string`), recurses its
    /// SUBSTITUTED fields `[i64, string]`, and classifies as a `UserRecord`
    /// whose carried name is the mangled key codegen re-resolves the thunk by.
    #[test]
    fn generic_record_instantiation_resolves_mangled_layout() {
        let key = hew_hir::mangle("Pair", &[ResolvedTy::I64, ResolvedTy::String]);
        let records = vec![RecordLayout {
            name: key.clone(),
            field_tys: vec![ResolvedTy::I64, ResolvedTy::String],
        }];
        let mut v = HashSet::new();
        let result = classify_state_field(
            &named("Pair", vec![ResolvedTy::I64, ResolvedTy::String]),
            &records,
            &mut v,
        )
        .expect("generic owned-record instantiation must classify");
        assert_eq!(
            result,
            StateFieldCloneKind::UserRecord { name: key },
            "the carried UserRecord name must be the mangled instantiation key"
        );
        assert!(
            v.is_empty(),
            "the recursion guard must be drained on success"
        );
    }

    /// Slice 2 negative control — an all-`BitCopy` generic instantiation still
    /// classifies as a `UserRecord` (the layout resolves; every field is
    /// `BitCopy`), so no owned-field drop is synthesised for it.
    #[test]
    fn generic_record_instantiation_all_bitcopy_classifies() {
        let key = hew_hir::mangle("Pair", &[ResolvedTy::I64, ResolvedTy::I64]);
        let records = vec![RecordLayout {
            name: key.clone(),
            field_tys: vec![ResolvedTy::I64, ResolvedTy::I64],
        }];
        let mut v = HashSet::new();
        let result = classify_state_field(
            &named("Pair", vec![ResolvedTy::I64, ResolvedTy::I64]),
            &records,
            &mut v,
        )
        .expect("all-BitCopy generic instantiation must classify");
        assert_eq!(result, StateFieldCloneKind::UserRecord { name: key });
    }

    /// Slice 2 — transitive fail-closed: a generic record whose substituted
    /// field is an opaque handle (`Box<json.Value>`) must fail closed, because
    /// the recursion classifies the substituted opaque field (`OpaqueHandle`) —
    /// there is no shallow clone. A regression here would be a UAF on supervisor
    /// restart (`unclonable-leaf-fails-closed-transitively`). The opaque field
    /// is `OpaqueHandle`, a recorded-but-unclonable kind: the record classifies
    /// structurally but the opaque leaf carries no clone helper, so the
    /// downstream owned-aggregate authority (which requires every field classify
    /// cleanly) excludes it.
    ///
    /// C1 symmetry: the registry shortens the qualified payload's prefix, so the
    /// layout is REGISTERED under the bare key `Box$$Value` (not the raw
    /// `Box$$json.Value`). The use-site probe still carries the qualified
    /// `Box<json.Value>`; `lookup_record_layout` must shorten its spine to
    /// resolve it. This pins both the bare-key registration discipline AND that
    /// the recursion descends into the SUBSTITUTED opaque field.
    #[test]
    fn generic_record_with_opaque_substituted_field_classifies_opaque_leaf() {
        // Registered under the SHORTENED spine, exactly as the real registry
        // (`RecordLayoutRegistry::insert` / `layout_mono`) keys it.
        let key = hew_hir::mangle("Box", &[ResolvedTy::named_opaque("Value", vec![])]);
        let records = vec![RecordLayout {
            name: key.clone(),
            // The substituted field is the opaque handle, NOT the bare `T`.
            field_tys: vec![ResolvedTy::named_opaque("json.Value", vec![])],
        }];
        let mut v = HashSet::new();
        let result = classify_state_field_full(
            // The use-site probe carries the QUALIFIED payload (import-use form).
            &named("Box", vec![ResolvedTy::named_opaque("json.Value", vec![])]),
            &records,
            &[],
            &[],
            &mut v,
        )
        .expect("the record itself classifies; the opaque leaf is recorded");
        // The record classifies as a UserRecord carrying the bare-spine mangled
        // key, and its substituted field is the opaque handle — confirm the
        // recursion reaches the opaque leaf rather than treating `T` as BitCopy.
        assert_eq!(result, StateFieldCloneKind::UserRecord { name: key });
        let mut fv = HashSet::new();
        let field_kind = classify_state_field_full(
            &ResolvedTy::named_opaque("json.Value", vec![]),
            &records,
            &[],
            &[],
            &mut fv,
        )
        .expect("opaque handle classifies as a recorded handle kind");
        assert!(
            matches!(field_kind, StateFieldCloneKind::OpaqueHandle { .. }),
            "the substituted opaque field must classify as OpaqueHandle (no \
             shallow clone), got {field_kind:?}"
        );
    }

    // ── C1 qualified-payload layout-key symmetry (actor-state clone) ────────
    //
    // The actor-state clone classifier resolves a generic field's layout via
    // `lookup_record_layout` / `lookup_enum_layout`. A field instantiated
    // through an import-use site carries a MODULE-QUALIFIED payload in its args
    // (`Pair<i64, json.Value>`), while the layout is REGISTERED under the bare
    // spine (`Pair$$i64$Value`). If the probe mangled the raw qualified spine it
    // would diverge from the registered key, fail closed as
    // `MissingRecordLayout`, and the actor would lose a clonable field — a
    // restart-time UAF or a spurious unclonable rejection. The lookup helpers
    // shorten the spine so the qualified use-site resolves to its bare key.

    /// A generic RECORD field whose payload is module-qualified at the use site
    /// resolves to its bare-key registered layout and classifies through.
    #[test]
    fn qualified_payload_record_field_resolves_to_bare_key() {
        // Registered under the bare spine (`Pair$$i64$Value`), as the registry keys it.
        let key = hew_hir::mangle(
            "Pair",
            &[ResolvedTy::I64, ResolvedTy::named_user("Value", vec![])],
        );
        let records = vec![
            RecordLayout {
                name: key.clone(),
                field_tys: vec![ResolvedTy::I64, ResolvedTy::named_user("Value", vec![])],
            },
            // The nested `Value` record so the field recursion classifies cleanly.
            RecordLayout {
                name: "Value".to_string(),
                field_tys: vec![ResolvedTy::I64],
            },
        ];
        let mut v = HashSet::new();
        // The probe carries the QUALIFIED payload (`json.Value`) as the import-use
        // MIR does. A raw `mangle("Pair", [i64, json.Value])` would key
        // `Pair$$i64$json.Value` and miss the registered `Pair$$i64$Value`.
        let result = classify_state_field_full(
            &named("Pair", vec![ResolvedTy::I64, named("json.Value", vec![])]),
            &records,
            &[],
            &[],
            &mut v,
        )
        .expect("qualified-payload record field must resolve to its bare-key layout");
        assert_eq!(
            result,
            StateFieldCloneKind::UserRecord { name: key },
            "the resolved layout carries the bare-spine mangled key"
        );
    }

    /// A generic ENUM field carrying a module-qualified payload at the use site
    /// resolves to its bare-key registered layout and classifies as `Enum`,
    /// driving the tag-aware actor-state clone — never a fail-closed miss.
    #[test]
    fn qualified_payload_enum_field_resolves_to_bare_key() {
        // Registered under the bare spine (`Slot$$Value`).
        let key = hew_hir::mangle("Slot", &[ResolvedTy::named_user("Value", vec![])]);
        let layouts = vec![EnumLayout {
            name: key.clone(),
            tag_width: 1,
            variants: vec![
                MachineVariantLayout {
                    name: "Full".to_string(),
                    field_tys: vec![ResolvedTy::String],
                },
                MachineVariantLayout {
                    name: "Empty".to_string(),
                    field_tys: vec![],
                },
            ],
            is_indirect: false,
        }];
        let mut v = HashSet::new();
        // Probe with the qualified payload `Slot<lmonobox.Value>`.
        let result = classify_state_field_with_enum_layouts(
            &named("Slot", vec![named("lmonobox.Value", vec![])]),
            &no_records(),
            &layouts,
            &mut v,
        )
        .expect("qualified-payload enum field must resolve to its bare-key layout");
        assert_eq!(
            result,
            StateFieldCloneKind::Enum { name: key },
            "the resolved layout carries the bare-spine mangled key"
        );
        assert!(v.is_empty(), "recursion guard must drain on success");
    }

    #[test]
    fn function_typed_field_classifies_as_drop_only_closure_pair() {
        // A `fn(...)` field classifies as `ClosurePair`: admitted to the
        // record value-class DROP-ONLY (the drop side releases the env via
        // the planted free thunk; the clone side is a structural refusal).
        // Actor state still rejects closure-valued fields — that guard
        // lives at the actor classification site via the transitive
        // `ty_contains_closure_value` walk, not here.
        let mut v = HashSet::new();
        let result = classify_state_field(
            &ResolvedTy::Function {
                params: vec![ResolvedTy::I32],
                ret: Box::new(ResolvedTy::Unit),
            },
            &no_records(),
            &mut v,
        );
        assert_eq!(result, Ok(StateFieldCloneKind::ClosurePair));
        assert!(StateFieldCloneKind::ClosurePair.supports_value_class_drop_spine());
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

    // ── W5.006 Slice 1: enum-layout consultation ──────────────────────

    /// Build the `Option$$string` layout the HIR mono pass would register:
    /// `Some(string)` (heap payload) + `None` (unit).
    fn option_string_layout() -> EnumLayout {
        EnumLayout {
            name: hew_hir::mangle("Option", &[ResolvedTy::String]),
            tag_width: 1,
            variants: vec![
                MachineVariantLayout {
                    name: "Some".to_string(),
                    field_tys: vec![ResolvedTy::String],
                },
                MachineVariantLayout {
                    name: "None".to_string(),
                    field_tys: vec![],
                },
            ],
            is_indirect: false,
        }
    }

    #[test]
    fn option_string_classifies_as_enum() {
        // `Option<string>` — the long-blocked heap-owning composite. The
        // enum-aware classifier must consult the registry (keyed by the
        // mangled `Option$$string`) and return `Enum`, NOT fail closed as
        // `MissingRecordLayout`.
        let layouts = vec![option_string_layout()];
        let ty = ResolvedTy::Named {
            name: "Option".to_string(),
            args: vec![ResolvedTy::String],
            builtin: None,
            is_opaque: false,
        };
        let mut v = HashSet::new();
        let result =
            classify_state_field_with_enum_layouts(&ty, &no_records(), &layouts, &mut v).unwrap();
        assert_eq!(
            result,
            StateFieldCloneKind::Enum {
                name: "Option$$string".to_string(),
            },
        );
        // Termination invariant: the recursion guard is empty on success.
        assert!(v.is_empty(), "visited-set leaked after success: {v:?}");
    }

    #[test]
    fn result_i64_string_classifies_as_enum() {
        // `Result<i64, string>`: `Ok(i64)` bitcopy payload + `Err(string)`
        // heap payload. Both payload field types must classify (the heap
        // arm proves the recursion validates owned-heap variants).
        let name = hew_hir::mangle("Result", &[ResolvedTy::I64, ResolvedTy::String]);
        let layouts = vec![EnumLayout {
            name: name.clone(),
            tag_width: 1,
            variants: vec![
                MachineVariantLayout {
                    name: "Ok".to_string(),
                    field_tys: vec![ResolvedTy::I64],
                },
                MachineVariantLayout {
                    name: "Err".to_string(),
                    field_tys: vec![ResolvedTy::String],
                },
            ],
            is_indirect: false,
        }];
        let ty = ResolvedTy::Named {
            name: "Result".to_string(),
            args: vec![ResolvedTy::I64, ResolvedTy::String],
            builtin: None,
            is_opaque: false,
        };
        let mut v = HashSet::new();
        let result =
            classify_state_field_with_enum_layouts(&ty, &no_records(), &layouts, &mut v).unwrap();
        assert_eq!(result, StateFieldCloneKind::Enum { name });
        assert!(v.is_empty());
    }

    #[test]
    fn monomorphic_user_enum_with_heap_variant_classifies_as_enum() {
        // A monomorphic user enum keyed by its plain decl name (`args`
        // empty), with a non-param heap-owning variant `Message(string)`
        // alongside a unit variant. This is the actor-state-field shape
        // (declared above the actor) that the in-loop classifier resolves.
        let layouts = vec![EnumLayout {
            name: "Envelope".to_string(),
            tag_width: 1,
            variants: vec![
                MachineVariantLayout {
                    name: "Message".to_string(),
                    field_tys: vec![ResolvedTy::String],
                },
                MachineVariantLayout {
                    name: "Empty".to_string(),
                    field_tys: vec![],
                },
            ],
            is_indirect: false,
        }];
        let ty = ResolvedTy::Named {
            name: "Envelope".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        };
        let mut v = HashSet::new();
        let result =
            classify_state_field_with_enum_layouts(&ty, &no_records(), &layouts, &mut v).unwrap();
        assert_eq!(
            result,
            StateFieldCloneKind::Enum {
                name: "Envelope".to_string(),
            },
        );
        assert!(v.is_empty());
    }

    #[test]
    fn enum_with_unsupported_payload_fails_closed() {
        // The recursion validates every variant payload: a function-typed
        // payload field is outside the closed set, so classification fails
        // closed at the enum arm rather than producing an `Enum` that
        // codegen could not lower.
        let layouts = vec![EnumLayout {
            name: "Bad".to_string(),
            tag_width: 1,
            variants: vec![MachineVariantLayout {
                name: "Holds".to_string(),
                field_tys: vec![ResolvedTy::Function {
                    params: vec![],
                    ret: Box::new(ResolvedTy::Unit),
                }],
            }],
            is_indirect: false,
        }];
        let ty = ResolvedTy::Named {
            name: "Bad".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        };
        let mut v = HashSet::new();
        let result = classify_state_field_with_enum_layouts(&ty, &no_records(), &layouts, &mut v);
        assert!(
            matches!(result, Err(ClassificationError::Unsupported { .. })),
            "expected Unsupported for function-typed enum payload, got {result:?}",
        );
    }

    #[test]
    fn enum_field_without_registry_still_fails_closed() {
        // The bare (non-enum-aware) entry point must retain the
        // pre-W5.006 behaviour: with no enum registry, an enum-typed field
        // falls through to `classify_user_record` and fails closed as
        // `MissingRecordLayout`. This pins that the additive enum-aware API
        // does not silently change the legacy call sites (llvm.rs record /
        // dyn-trait classification) that pass no enum layouts.
        let ty = ResolvedTy::Named {
            name: "Option".to_string(),
            args: vec![ResolvedTy::String],
            builtin: None,
            is_opaque: false,
        };
        let mut v = HashSet::new();
        let result = classify_state_field(&ty, &no_records(), &mut v);
        assert!(
            matches!(result, Err(ClassificationError::MissingRecordLayout { .. })),
            "expected MissingRecordLayout without enum registry, got {result:?}",
        );
    }

    #[test]
    fn record_holding_enum_field_classifies_through() {
        // A record whose field is an enum: the record recursion threads
        // the enum registry so `Holder { msg: Envelope }` classifies as
        // `UserRecord` (with the nested enum validated), not failing closed.
        let records = vec![RecordLayout {
            name: "Holder".to_string(),
            field_tys: vec![ResolvedTy::Named {
                name: "Envelope".to_string(),
                args: vec![],
                builtin: None,
                is_opaque: false,
            }],
        }];
        let enums = vec![EnumLayout {
            name: "Envelope".to_string(),
            tag_width: 1,
            variants: vec![MachineVariantLayout {
                name: "Message".to_string(),
                field_tys: vec![ResolvedTy::String],
            }],
            is_indirect: false,
        }];
        let ty = ResolvedTy::Named {
            name: "Holder".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        };
        let mut v = HashSet::new();
        let result = classify_state_field_with_enum_layouts(&ty, &records, &enums, &mut v).unwrap();
        assert_eq!(
            result,
            StateFieldCloneKind::UserRecord {
                name: "Holder".to_string(),
            },
        );
        assert!(v.is_empty());
    }

    // ── Collision-safety: record-layouts-first beats opaque set ──────────────

    /// A user-declared `type Value { x: i64 }` imported as `laneBmod.Value`
    /// arrives at the classifier as `Named { name: "Value", args: [] }` after
    /// `lower_type` strips the module prefix (W4.011 gap).  The same short name
    /// `"Value"` is also in `opaque_handle_names` because `json.Value` is an
    /// `#[opaque]` stdlib type.
    ///
    /// The record-layouts-first guard must win: `"Value"` is in `record_layouts`,
    /// so it classifies as `UserRecord`, NOT `OpaqueHandle`.  A false-positive
    /// `OpaqueHandle` here would later cause codegen to emit a bare `ptr` slot
    /// for the struct, triggering a `RecordFieldLoad non-struct slot type` error.
    #[test]
    fn user_record_named_value_beats_opaque_handle_in_name_set() {
        let records = vec![RecordLayout {
            name: "Value".to_string(),
            field_tys: vec![ResolvedTy::I64],
        }];
        // Simulates `json.Value` opaque handle — its bare decl name is `"Value"`.
        let opaque_names = vec!["Value".to_string()];
        let ty = ResolvedTy::Named {
            name: "Value".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        };
        let mut v = HashSet::new();
        let result = classify_state_field_full(&ty, &records, &[], &opaque_names, &mut v).unwrap();
        assert_eq!(
            result,
            StateFieldCloneKind::UserRecord {
                name: "Value".to_string(),
            },
            "record-layouts-first: user record `Value` must NOT be \
             misclassified as OpaqueHandle when json.Value shares the short name",
        );
        assert!(v.is_empty());
    }

    // ── Collision-safety: enum-layouts-first beats opaque set ───────────────

    /// An `indirect enum Expr { Lit(i64); Neg(Expr) }` has its name `"Expr"`
    /// added to `opaque_handle_names` by `lower_hir_module` so that codegen
    /// emits a bare `ptr` slot for every variable of the type.  However, an
    /// indirect enum IS a classified enum and must route through `classify_enum`,
    /// not be misidentified as a raw `OpaqueHandle`.
    ///
    /// The enum-layouts-first guard (inserted before the opaque-handle arm) must
    /// win: `"Expr"` is in `enum_layouts`, so it classifies as `Enum`, NOT
    /// `OpaqueHandle`.  Without the guard, the opaque-handle arm fires first and
    /// the enum-clone synthesis emits a trap body instead of real tag-switch code.
    #[test]
    fn indirect_enum_beats_opaque_handle_in_name_set() {
        let enums = vec![EnumLayout {
            name: "Expr".to_string(),
            tag_width: 1,
            variants: vec![
                MachineVariantLayout {
                    name: "Lit".to_string(),
                    field_tys: vec![ResolvedTy::I64],
                },
                MachineVariantLayout {
                    // Self-referential variant — the `Neg(Expr)` payload is the
                    // reason the enum is `indirect` in the first place.
                    name: "Neg".to_string(),
                    field_tys: vec![ResolvedTy::Named {
                        name: "Expr".to_string(),
                        args: vec![],
                        builtin: None,
                        is_opaque: false,
                    }],
                },
            ],
            is_indirect: true,
        }];
        // Simulates the `IrPipeline::opaque_handle_names` entry that
        // `lower_hir_module` adds for every `indirect` type decl.
        let opaque_names = vec!["Expr".to_string()];
        let ty = ResolvedTy::Named {
            name: "Expr".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        };
        let mut v = HashSet::new();
        let result =
            classify_state_field_full(&ty, &no_records(), &enums, &opaque_names, &mut v).unwrap();
        assert_eq!(
            result,
            StateFieldCloneKind::Enum {
                name: "Expr".to_string(),
            },
            "enum-layouts-first: indirect enum `Expr` must NOT be \
             misclassified as OpaqueHandle when its name appears in opaque_handle_names",
        );
        assert!(v.is_empty());
    }

    /// Corollary: when `"Value"` is NOT in `record_layouts` but IS in
    /// `opaque_handle_names`, it must classify as `OpaqueHandle`.  This is the
    /// stdlib `json.Value` case — no user record shadows it.
    #[test]
    fn opaque_handle_classifies_when_no_shadowing_record_present() {
        let opaque_names = vec!["Value".to_string()];
        let ty = ResolvedTy::Named {
            name: "Value".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        };
        let mut v = HashSet::new();
        let result = classify_state_field_full(&ty, &[], &[], &opaque_names, &mut v).unwrap();
        assert_eq!(
            result,
            StateFieldCloneKind::OpaqueHandle {
                name: "Value".to_string(),
            },
            "opaque handle must classify as OpaqueHandle when no user record shadows it",
        );
        assert!(v.is_empty());
    }

    // ── Discriminator-driven collision safety (round-3 memory-safety fix) ────
    //
    // The `is_opaque` type-identity discriminator (stamped by `lower_type`) is
    // authoritative and beats a colliding user record/enum LAYOUT. These pin
    // the false-negative fix: a real `#[opaque]` handle whose short name
    // collides with a user `type Value` / `enum Value` MUST classify as
    // `OpaqueHandle` (fail-closed clone), never as the user record/enum (which
    // would emit an unsafe shallow `memcpy` → double-free/UAF on supervisor
    // restart). The discriminator — not the name string — is what tells them
    // apart, since both arrive as `Named { name: "Value", .. }`.

    /// An opaque `json.Value` field (`is_opaque: true`) whose short name `Value`
    /// collides with a user `type Value { x: i64 }` present in `record_layouts`
    /// MUST classify as `OpaqueHandle`, NOT `UserRecord`. This is the round-2
    /// false-negative (item 2b struct): name-based resolution misrouted it to
    /// the user record and emitted a shallow clone.
    #[test]
    fn opaque_discriminator_beats_colliding_user_record() {
        // User record `Value { x: i64 }` is registered.
        let records = vec![RecordLayout {
            name: "Value".to_string(),
            field_tys: vec![ResolvedTy::I64],
        }];
        // The opaque handle field carries the identity discriminator.
        let ty = named_opaque("Value", vec![]);
        let mut v = HashSet::new();
        let result = classify_state_field_full(&ty, &records, &[], &[], &mut v).unwrap();
        assert_eq!(
            result,
            StateFieldCloneKind::OpaqueHandle {
                name: "Value".to_string(),
            },
            "opaque-discriminator field must classify as OpaqueHandle even when a \
             user record `Value` shares the short name (item 2b struct false-negative)",
        );
        assert!(v.is_empty());
    }

    /// An opaque `json.Value` field (`is_opaque: true`) whose short name `Value`
    /// collides with a user `enum Value { Foo; Bar }` present in `enum_layouts`
    /// MUST classify as `OpaqueHandle`, NOT `Enum`. This is the round-2
    /// false-negative (item 2b enum): name-based resolution misrouted it to the
    /// user enum and emitted a shallow clone + enum-clone-inplace call.
    #[test]
    fn opaque_discriminator_beats_colliding_user_enum() {
        let enums = vec![EnumLayout {
            name: "Value".to_string(),
            tag_width: 1,
            variants: vec![
                MachineVariantLayout {
                    name: "Foo".to_string(),
                    field_tys: vec![],
                },
                MachineVariantLayout {
                    name: "Bar".to_string(),
                    field_tys: vec![],
                },
            ],
            is_indirect: false,
        }];
        let ty = named_opaque("Value", vec![]);
        let mut v = HashSet::new();
        let result = classify_state_field_full(&ty, &no_records(), &enums, &[], &mut v).unwrap();
        assert_eq!(
            result,
            StateFieldCloneKind::OpaqueHandle {
                name: "Value".to_string(),
            },
            "opaque-discriminator field must classify as OpaqueHandle even when a \
             user enum `Value` shares the short name (item 2b enum false-negative)",
        );
        assert!(v.is_empty());
    }

    /// A user record `Value` referenced WITHOUT the opaque discriminator
    /// (`is_opaque: false`) must still classify as `UserRecord` even when the
    /// opaque-name fallback set carries `"Value"`. This pins that the
    /// discriminator does NOT over-fire: a user type sharing the name with an
    /// imported opaque handle is never captured as opaque (item 1 shadow case).
    #[test]
    fn non_opaque_user_record_with_colliding_opaque_name_stays_user_record() {
        let records = vec![RecordLayout {
            name: "Value".to_string(),
            field_tys: vec![ResolvedTy::I64],
        }];
        let opaque_names = vec!["Value".to_string()];
        let ty = named("Value", vec![]); // is_opaque: false
        let mut v = HashSet::new();
        let result = classify_state_field_full(&ty, &records, &[], &opaque_names, &mut v).unwrap();
        assert_eq!(
            result,
            StateFieldCloneKind::UserRecord {
                name: "Value".to_string(),
            },
            "a non-opaque user record must stay UserRecord even when its short \
             name is in the opaque-name fallback set (no over-fire)",
        );
        assert!(v.is_empty());
    }

    /// An opaque handle nested inside a user enum variant payload
    /// (`enum Wrap { V(json.Value) }`) must classify the payload as
    /// `OpaqueHandle` via the discriminator, so the enum-clone synthesis emits
    /// the fail-closed trap rather than a shallow clone (item 2c nested case).
    #[test]
    fn opaque_discriminator_propagates_into_enum_variant_payload() {
        let enums = vec![EnumLayout {
            name: "Wrap".to_string(),
            tag_width: 1,
            variants: vec![MachineVariantLayout {
                name: "V".to_string(),
                field_tys: vec![named_opaque("Value", vec![])],
            }],
            is_indirect: false,
        }];
        let ty = named("Wrap", vec![]); // the enum itself is a normal user enum
        let mut v = HashSet::new();
        let result = classify_state_field_full(&ty, &no_records(), &enums, &[], &mut v).unwrap();
        assert_eq!(
            result,
            StateFieldCloneKind::Enum {
                name: "Wrap".to_string(),
            },
            "the wrapper enum classifies as Enum; its opaque payload is detected \
             on recursion so codegen emits the fail-closed clone trap",
        );
        assert!(v.is_empty());
    }

    // ── Transitive opaque-in-container fail-close (round-4) ─────────────────
    // A `#[opaque]` handle inside a generic container has no per-element
    // clone-dup helper. The container's managed clone + plain element witness
    // would shallow-copy the opaque pointer and double-free / UAF on supervisor
    // restart, so the WHOLE container must fail closed at classification
    // (BLOCKER 1, round-3 hole-hunt probe 3f2). The single transitive authority
    // `model::ty_contains_unclonable_opaque` backs every arm below.

    fn assert_opaque_in_container(result: Result<StateFieldCloneKind, ClassificationError>) {
        match result {
            Err(ClassificationError::OpaqueInContainer { .. }) => {}
            other => panic!(
                "expected OpaqueInContainer fail-close, got {other:?} — a container \
                 carrying an unclonable opaque handle must fail closed, not classify"
            ),
        }
    }

    #[test]
    fn vec_of_opaque_handle_fails_closed() {
        // `Vec<json.Value>` in actor state (round-3 probe 3f2). Before the fix
        // this classified as `Vec { OpaqueHandle }` and codegen cloned the vec
        // handle with a plain element witness — the UAF.
        let mut v = HashSet::new();
        let ty = ResolvedTy::named_user("Vec", vec![named_opaque("Value", vec![])]);
        assert_opaque_in_container(classify_state_field_full(
            &ty,
            &no_records(),
            &[],
            &[],
            &mut v,
        ));
        assert!(v.is_empty(), "visited set must unwind on fail-close");
    }

    #[test]
    fn hashset_of_opaque_handle_fails_closed() {
        let mut v = HashSet::new();
        let ty = ResolvedTy::named_user("HashSet", vec![named_opaque("Value", vec![])]);
        assert_opaque_in_container(classify_state_field_full(
            &ty,
            &no_records(),
            &[],
            &[],
            &mut v,
        ));
    }

    #[test]
    fn hashmap_with_opaque_value_fails_closed() {
        let mut v = HashSet::new();
        let ty = ResolvedTy::named_user(
            "HashMap",
            vec![ResolvedTy::String, named_opaque("Value", vec![])],
        );
        assert_opaque_in_container(classify_state_field_full(
            &ty,
            &no_records(),
            &[],
            &[],
            &mut v,
        ));
    }

    #[test]
    fn hashmap_with_opaque_key_fails_closed() {
        let mut v = HashSet::new();
        let ty = ResolvedTy::named_user(
            "HashMap",
            vec![named_opaque("Value", vec![]), ResolvedTy::I64],
        );
        assert_opaque_in_container(classify_state_field_full(
            &ty,
            &no_records(),
            &[],
            &[],
            &mut v,
        ));
    }

    #[test]
    fn nested_vec_of_option_of_opaque_fails_closed() {
        // `Vec<Option<json.Value>>` — the opaque is two containers deep. The
        // transitive authority recurses through the `Option<T>` type-arg, so the
        // outer Vec fails closed even though its immediate element is an enum.
        let mut v = HashSet::new();
        let inner_opt = ResolvedTy::named_user("Option", vec![named_opaque("Value", vec![])]);
        let ty = ResolvedTy::named_user("Vec", vec![inner_opt]);
        assert_opaque_in_container(classify_state_field_full(
            &ty,
            &no_records(),
            &[],
            &[],
            &mut v,
        ));
    }

    #[test]
    fn vec_of_record_with_opaque_field_fails_closed() {
        // `Vec<Holder>` where `Holder { v: json.Value }`. The opaque is behind a
        // user record field; the authority looks up the record layout and
        // recurses into its fields, so the Vec fails closed.
        let records = vec![RecordLayout {
            name: "Holder".to_string(),
            field_tys: vec![named_opaque("Value", vec![])],
        }];
        let mut v = HashSet::new();
        let ty = ResolvedTy::named_user("Vec", vec![ResolvedTy::named_user("Holder", vec![])]);
        assert_opaque_in_container(classify_state_field_full(&ty, &records, &[], &[], &mut v));
    }

    #[test]
    fn vec_of_non_opaque_user_value_stays_clean() {
        // Negative control: `Vec<Value>` where `Value` is a plain user record
        // (`is_opaque: false`) MUST classify normally even when a same-short-name
        // opaque handle exists in the opaque-name fallback set. The authority
        // keys on the per-reference `is_opaque` discriminator, never the name, so
        // the fail-close does not over-fire on a colliding user type.
        let records = vec![RecordLayout {
            name: "Value".to_string(),
            field_tys: vec![ResolvedTy::I64],
        }];
        let opaque_names = vec!["Value".to_string()];
        let mut v = HashSet::new();
        let ty = ResolvedTy::named_user("Vec", vec![named("Value", vec![])]); // is_opaque: false
        let result = classify_state_field_full(&ty, &records, &[], &opaque_names, &mut v).unwrap();
        assert_eq!(
            result,
            StateFieldCloneKind::Vec {
                elem: Box::new(StateFieldCloneKind::UserRecord {
                    name: "Value".to_string(),
                }),
            },
            "a Vec of a non-opaque user record must classify normally (no over-fire)",
        );
        assert!(v.is_empty());
    }

    #[test]
    fn contains_opaque_handle_kind_backstop() {
        // The codegen backstop `collection_layout_witness` consults this kind-
        // level mirror. A bare opaque is opaque; a container nesting one is too;
        // a HashMap is opaque via either key or value; a clean container is not.
        let opaque = StateFieldCloneKind::OpaqueHandle {
            name: "Value".to_string(),
        };
        assert!(opaque.contains_opaque_handle());
        assert!(StateFieldCloneKind::Vec {
            elem: Box::new(opaque.clone()),
        }
        .contains_opaque_handle());
        assert!(StateFieldCloneKind::HashMap {
            key: Box::new(StateFieldCloneKind::String),
            val: Box::new(opaque.clone()),
        }
        .contains_opaque_handle());
        assert!(StateFieldCloneKind::HashSet {
            elem: Box::new(StateFieldCloneKind::Vec {
                elem: Box::new(opaque),
            }),
        }
        .contains_opaque_handle());
        // Negative: a clean Vec<string> carries no opaque.
        assert!(!StateFieldCloneKind::Vec {
            elem: Box::new(StateFieldCloneKind::String),
        }
        .contains_opaque_handle());
    }

    #[test]
    fn supports_value_class_drop_spine_admits_supported_rejects_opaque() {
        // Supported leaf kinds (clone + drop helper exists): BitCopy, String,
        // Bytes, UserRecord, Enum — and containers whose element/key/value is
        // itself supported and carries no opaque handle.
        assert!(StateFieldCloneKind::BitCopy { size_bytes: 8 }.supports_value_class_drop_spine());
        assert!(StateFieldCloneKind::String.supports_value_class_drop_spine());
        assert!(StateFieldCloneKind::Bytes.supports_value_class_drop_spine());
        assert!(StateFieldCloneKind::UserRecord {
            name: "Inner".to_string(),
        }
        .supports_value_class_drop_spine());
        assert!(StateFieldCloneKind::Enum {
            name: "Option$$string".to_string(),
        }
        .supports_value_class_drop_spine());
        assert!(StateFieldCloneKind::Vec {
            elem: Box::new(StateFieldCloneKind::String),
        }
        .supports_value_class_drop_spine());
        assert!(StateFieldCloneKind::HashMap {
            key: Box::new(StateFieldCloneKind::String),
            val: Box::new(StateFieldCloneKind::Bytes),
        }
        .supports_value_class_drop_spine());

        // IoHandle variants are ADMITTED: an AffineResource/Linear field is not
        // owned-aggregate-by-value, so a record carrying one drops field-wise via
        // the resource-drop path and is never seeded for RecordInPlace clone
        // synthesis. The proven `Holder { inner: Generator<i64,()> }` corpus
        // (generator_exec) drops the handle once with no record thunk; rejecting
        // it here would over-reject that working shape.
        assert!(StateFieldCloneKind::IoHandle {
            kind: IoHandleKind::Connection,
        }
        .supports_value_class_drop_spine());
        assert!(StateFieldCloneKind::IoHandle {
            kind: IoHandleKind::Generator,
        }
        .supports_value_class_drop_spine());
        assert!(StateFieldCloneKind::IoHandle {
            kind: IoHandleKind::Stream,
        }
        .supports_value_class_drop_spine());

        // OpaqueHandle (no dup runtime helper) must be REJECTED — the
        // value-class clone-synthesis would fail closed on it, so the record
        // must not admit at the gate.
        assert!(!StateFieldCloneKind::OpaqueHandle {
            name: "json.Value".to_string(),
        }
        .supports_value_class_drop_spine());

        // A container transitively carrying an opaque handle is rejected even
        // though `Vec` is otherwise supported — the managed clone would
        // shallow-copy the opaque pointer (double-free / UAF on a deep clone).
        assert!(!StateFieldCloneKind::Vec {
            elem: Box::new(StateFieldCloneKind::OpaqueHandle {
                name: "json.Value".to_string(),
            }),
        }
        .supports_value_class_drop_spine());
    }
}
