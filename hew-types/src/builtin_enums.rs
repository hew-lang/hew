//! Public catalog of monomorphic builtin enums declared in
//! `std/builtins.hew` whose layout must be visible to MIR and codegen
//! without appearing in any program's `HirProgram::items` list.
//!
//! ## Why this exists
//!
//! Generic builtin enums (`Option<T>`, `Result<T, E>`) carry their layout
//! into MIR via [`HirModule::enum_layouts`] — one entry per concrete
//! instantiation, registered by the HIR monomorphisation pass through
//! [`crate::monomorph::EnumLayoutRegistry`]. There is no bare-name layout
//! for these generics; an unused `Option<T>` declaration is a non-event.
//!
//! Monomorphic builtin enums (e.g. `LookupError`) are different: they
//! have no type parameters, so the HIR monomorphisation pass never
//! creates per-instantiation entries for them. MIR's `enum_layouts` and
//! `machine_layout_names` therefore have no path to learn the type from
//! the generic-enum lane.
//!
//! Earlier S4 prototypes synthesised a `HirItem::TypeDecl` for every
//! monomorphic builtin enum and pushed it into `HirProgram::items`. That
//! made the type visible to MIR but **leaked across every program** —
//! including ones that never reference the type — and changed the
//! downstream sandbox-VM bytecode descriptor table for every fixture
//! (new `type:LookupError` descriptor, churned `package_id` hash).
//!
//! The fix is to carry the layout **out-of-band**: MIR registers the
//! layout directly from this catalog (parallel to how
//! `hew_hir::builtin_type_classes::builtin_type_registrations` feeds
//! struct-shaped builtins into MIR's `record_layouts`). The
//! `HirProgram::items` list stays a faithful mirror of user source.
//!
//! ## Shape constraints
//!
//! Today every monomorphic builtin enum uses only unit variants (no
//! payload fields). Adding a payloaded variant would require extending
//! [`BuiltinMonomorphicEnumVariant`] with a `field_tys` slot and
//! propagating field types into the MIR layout registration. Keep the
//! API closed to that case until the first real use lands.

/// One monomorphic builtin enum.
///
/// `variants` is in declaration order — the index in this slice matches
/// the discriminant tag value assigned by the HIR ctor pre-pass and the
/// `MachineVariantLayout` index in the MIR `EnumLayout`. Codegen and
/// match-arm dispatch depend on this agreement.
#[derive(Debug, Clone, Copy)]
pub struct BuiltinMonomorphicEnum {
    /// Type name as written in `std/builtins.hew` (e.g. `"LookupError"`).
    pub name: &'static str,
    /// Variant names in declaration order. All payloads are empty (unit
    /// variants) by current substrate contract — see module docs.
    pub variants: &'static [BuiltinMonomorphicEnumVariant],
    /// Whether `hew-sandbox-wasm`'s eager `type_defs` sweep should
    /// suppress emission of this type when the program does not author
    /// it. Set `true` for types whose machine layout MIR carries
    /// out-of-band but which user programs never reference directly
    /// (e.g. `LookupError`, which only appears as `Result<_, LookupError>`
    /// payload via the `Node::lookup` codegen intercept and therefore
    /// would only land in fixtures as dead descriptor churn). Set
    /// `false` for types that DO surface in user-visible signatures and
    /// must remain in sandbox bytecode descriptor tables for fixture
    /// stability (e.g. `SendError`, the `Err` variant of every
    /// `Result<(), SendError>` returned by `.tell` / `.send` family
    /// methods declared in `std/builtins.hew`).
    pub suppress_from_sandbox_emit: bool,
}

/// One variant of a monomorphic builtin enum. Unit-only today.
#[derive(Debug, Clone, Copy)]
pub struct BuiltinMonomorphicEnumVariant {
    /// Variant name (e.g. `"NotFound"`).
    pub name: &'static str,
}

const LOOKUP_ERROR_VARIANTS: &[BuiltinMonomorphicEnumVariant] = &[
    BuiltinMonomorphicEnumVariant { name: "NotFound" },
    BuiltinMonomorphicEnumVariant { name: "Partition" },
    BuiltinMonomorphicEnumVariant { name: "Timeout" },
    BuiltinMonomorphicEnumVariant { name: "StaleRef" },
    BuiltinMonomorphicEnumVariant { name: "Cancelled" },
    BuiltinMonomorphicEnumVariant {
        name: "LocalShutdown",
    },
    BuiltinMonomorphicEnumVariant {
        name: "VersionMismatch",
    },
    BuiltinMonomorphicEnumVariant {
        name: "Unauthorized",
    },
];

const SEND_ERROR_VARIANTS: &[BuiltinMonomorphicEnumVariant] = &[
    BuiltinMonomorphicEnumVariant { name: "Full" },
    BuiltinMonomorphicEnumVariant { name: "Closed" },
    BuiltinMonomorphicEnumVariant {
        name: "NodeRoutingNotWired",
    },
    BuiltinMonomorphicEnumVariant { name: "Partition" },
    BuiltinMonomorphicEnumVariant { name: "StaleRef" },
    BuiltinMonomorphicEnumVariant {
        name: "LocalShutdown",
    },
    BuiltinMonomorphicEnumVariant { name: "Cancelled" },
    BuiltinMonomorphicEnumVariant {
        name: "VersionMismatch",
    },
    BuiltinMonomorphicEnumVariant {
        name: "Unauthorized",
    },
    BuiltinMonomorphicEnumVariant {
        name: "Backpressure",
    },
];

const ASK_ERROR_VARIANTS: &[BuiltinMonomorphicEnumVariant] = &[
    BuiltinMonomorphicEnumVariant { name: "NoError" },
    BuiltinMonomorphicEnumVariant {
        name: "NodeNotRunning",
    },
    BuiltinMonomorphicEnumVariant {
        name: "RoutingFailed",
    },
    BuiltinMonomorphicEnumVariant {
        name: "EncodeFailed",
    },
    BuiltinMonomorphicEnumVariant { name: "SendFailed" },
    BuiltinMonomorphicEnumVariant { name: "Timeout" },
    BuiltinMonomorphicEnumVariant {
        name: "ConnectionDropped",
    },
    BuiltinMonomorphicEnumVariant {
        name: "PayloadSizeMismatch",
    },
    BuiltinMonomorphicEnumVariant {
        name: "WorkerAtCapacity",
    },
    BuiltinMonomorphicEnumVariant {
        name: "ActorStopped",
    },
    BuiltinMonomorphicEnumVariant {
        name: "MailboxFull",
    },
    BuiltinMonomorphicEnumVariant {
        name: "OrphanedAsk",
    },
    BuiltinMonomorphicEnumVariant {
        name: "NoRunnableWork",
    },
    BuiltinMonomorphicEnumVariant {
        name: "DecodeFailure",
    },
    BuiltinMonomorphicEnumVariant { name: "Partition" },
    BuiltinMonomorphicEnumVariant { name: "StaleRef" },
    BuiltinMonomorphicEnumVariant { name: "Cancelled" },
    BuiltinMonomorphicEnumVariant {
        name: "LocalShutdown",
    },
    BuiltinMonomorphicEnumVariant {
        name: "VersionMismatch",
    },
    BuiltinMonomorphicEnumVariant {
        name: "Unauthorized",
    },
    BuiltinMonomorphicEnumVariant {
        name: "Backpressure",
    },
    BuiltinMonomorphicEnumVariant {
        name: "MonitorLost",
    },
];

const TIMEOUT_ERROR_VARIANTS: &[BuiltinMonomorphicEnumVariant] =
    &[BuiltinMonomorphicEnumVariant { name: "Timeout" }];

/// `LinkError` is the `Err` variant of `Result<(), LinkError>` returned by
/// `link()` in value position. It is prelude-declared in `std/builtins.hew`
/// (like `SendError` / `TimeoutError`), so programs name its variants without a
/// `type LinkError` declaration or an import. This out-of-band catalog entry
/// mirrors `SendError`'s: it sizes the dest local for the value-position
/// composite return that codegen constructs in place, in case the generic
/// `EnumLayoutRegistry` path has not already registered the layout.
const LINK_ERROR_VARIANTS: &[BuiltinMonomorphicEnumVariant] = &[
    BuiltinMonomorphicEnumVariant {
        name: "AlreadyLinked",
    },
    BuiltinMonomorphicEnumVariant { name: "TargetDead" },
];

/// `CrashAction` variants in declaration order (`std/failure.hew`). The tag
/// each maps to is its index: `Restart=0`, `Escalate=1`, `Kill=2`.
const CRASH_ACTION_VARIANTS: &[BuiltinMonomorphicEnumVariant] = &[
    BuiltinMonomorphicEnumVariant { name: "Restart" },
    BuiltinMonomorphicEnumVariant { name: "Escalate" },
    BuiltinMonomorphicEnumVariant { name: "Kill" },
];

/// `CrashKind` variants in declaration order (`std/failure.hew`): `Crashed=0`,
/// `HeapExceeded=1`, `PartitionDetected=2`. The `#[on(exit)]` prologue (M-7-R)
/// builds a `CrashKind` value from the delivered tag, so codegen needs its
/// tagged-union layout out-of-band like `CrashAction`.
const CRASH_KIND_VARIANTS: &[BuiltinMonomorphicEnumVariant] = &[
    BuiltinMonomorphicEnumVariant { name: "Crashed" },
    BuiltinMonomorphicEnumVariant {
        name: "HeapExceeded",
    },
    BuiltinMonomorphicEnumVariant {
        name: "PartitionDetected",
    },
];

/// Catalog of monomorphic builtin enums whose layout must be registered
/// out-of-band into MIR's `enum_layouts` and `machine_layout_names`.
///
/// MIR's lower pass calls this from a `register_builtin_monomorphic_enum_layouts`
/// helper, mirroring the precedent set by
/// `hew_hir::builtin_type_classes::builtin_type_registrations` for
/// struct-shaped builtins.
#[must_use]
pub fn monomorphic_builtin_enums() -> &'static [BuiltinMonomorphicEnum] {
    &[
        BuiltinMonomorphicEnum {
            name: "LookupError",
            variants: LOOKUP_ERROR_VARIANTS,
            suppress_from_sandbox_emit: true,
        },
        // `SendError` is the `Err` variant of `Result<(), SendError>`
        // returned by every `.tell` shaped surface: `LocalPid<T>::tell`,
        // `RemotePid<T>::tell`, `Duplex<…>::send`, and the channel
        // `SendHalf::send`/`try_send` family. Until S5, the only consumer
        // that needed the layout in MIR was the inline body of
        // `RemotePid<T>::tell` (which returned `Err(NodeRoutingNotWired)`
        // as a constant); the v0.5 inline-stdlib path lowered the literal
        // through ordinary enum-ctor lowering, so MIR learned the layout
        // from the generic `EnumLayoutRegistry`.
        //
        // S5 changes that contract: the checker rewrites
        // `pid.tell(msg)` on a `RemotePid<T>` receiver into a direct call
        // to the catalog-declared `hew_remote_pid_tell`. Codegen
        // intercepts that call and constructs the user-visible
        // `Result<(), SendError>` in place from the runtime rc, but MIR
        // still needs the `SendError` layout to size the dest local for
        // the Call terminator's dest place. Carry the layout out-of-band
        // here, parallel to `LookupError`.
        //
        // Unlike `LookupError`, `SendError` is surfaced in user-visible
        // trait signatures (`fn tell(...) -> Result<(), SendError>`), so
        // its bytecode descriptor IS present in baseline sandbox fixtures
        // and must stay there — hence `suppress_from_sandbox_emit: false`.
        BuiltinMonomorphicEnum {
            name: "SendError",
            variants: SEND_ERROR_VARIANTS,
            suppress_from_sandbox_emit: false,
        },
        BuiltinMonomorphicEnum {
            name: "AskError",
            variants: ASK_ERROR_VARIANTS,
            suppress_from_sandbox_emit: false,
        },
        // `TimeoutError` is the error arm of `await rx.recv() | after d` and
        // `await stream.recv() | after d`.  A unit enum with one variant
        // (`Timeout`) that distinguishes a deadline expiry from a closed channel
        // (`Ok(None)`).  Carried out-of-band here because user programs only
        // reference it as `Err(_)` in match arms — not as a standalone type
        // declaration — so the `EnumLayoutRegistry` generic path would never
        // register it. `suppress_from_sandbox_emit: true` because the sandbox
        // bytecode descriptor has no stable fixture baseline for `TimeoutError`.
        BuiltinMonomorphicEnum {
            name: "TimeoutError",
            variants: TIMEOUT_ERROR_VARIANTS,
            suppress_from_sandbox_emit: true,
        },
        // `LinkError` is the `Err` variant of `Result<(), LinkError>` returned
        // by `link()` in value position. Prelude-declared in `std/builtins.hew`,
        // so user programs name `LinkError::AlreadyLinked` / `TargetDead` in
        // match arms without an import (parity with `SendError`). Carried
        // out-of-band here for the same reason as `SendError`: codegen
        // constructs the `Result<(), LinkError>` in place for the value-position
        // `link()` call, and MIR needs the layout to size the dest local even on
        // a path the generic `EnumLayoutRegistry` has not seeded.
        // `suppress_from_sandbox_emit: true` because no stable sandbox-bytecode
        // fixture baseline for `LinkError` exists yet.
        BuiltinMonomorphicEnum {
            name: "LinkError",
            variants: LINK_ERROR_VARIANTS,
            suppress_from_sandbox_emit: true,
        },
        // `CrashAction` is the return type of every `#[on(crash)]` hook
        // (`std/failure.hew`). M-4 returns it by value across the `HewOnCrashFn`
        // ABI, so codegen must know its tagged-union layout to size the emitted
        // `__on_crash` return type — even for a panic()-diverging body that
        // never constructs a CrashAction value (the function SIGNATURE still
        // names the enum return type). Carried out-of-band here, parallel to
        // `LinkError`, because a crash hook may name `CrashAction` only as its
        // return type, so the generic `EnumLayoutRegistry` never seeds it.
        // `suppress_from_sandbox_emit: true` (no stable sandbox baseline).
        BuiltinMonomorphicEnum {
            name: "CrashAction",
            variants: CRASH_ACTION_VARIANTS,
            suppress_from_sandbox_emit: true,
        },
        // `CrashKind` (M-7-R) — the crash-class enum delivered in a
        // `CrashNotification` to an `#[on(exit)]` hook. Carried out-of-band so
        // codegen knows its tagged-union layout (the prologue builds a CrashKind
        // value from the runtime-delivered tag). `suppress_from_sandbox_emit:
        // true` (no stable sandbox baseline).
        BuiltinMonomorphicEnum {
            name: "CrashKind",
            variants: CRASH_KIND_VARIANTS,
            suppress_from_sandbox_emit: true,
        },
    ]
}
