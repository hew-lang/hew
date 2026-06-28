//! G3 — error-enum discriminant cross-crate parity.
//!
//! The runtime `#[repr(i32)]` enums (`hew_runtime::internal::types::AskError`,
//! `hew_runtime::duplex::{SendError, RecvError}`) are the canonical ABI
//! discriminant producers. Codegen projects the runtime ask error code directly
//! into the surface `AskError` enum (`emit_remote_ask_err_from_last_error`
//! stores the raw runtime i32 as the surface tag), and the lambda-actor ask path
//! hardcodes `ASKERR_*` discriminant literals plus a `SendError -> AskError`
//! mapping. The surface enum lives in three frontend catalogs
//! (`builtin_enum_specs()`, `BUILTIN_ENUM_VARIANT_BARE_NAMES`, and the
//! `hew-types` monomorphic catalog), which a sibling test in `hew-hir` already
//! pins to each other — but NOT to the runtime ABI producer.
//!
//! This is the cross-crate backstop the frontend test cannot be: `hew-hir`
//! does not (and should not) depend on `hew-runtime`, so the runtime <-> surface
//! discriminant identity was held only by convention. `AskError::DecodeFailure`
//! (runtime discriminant 13) had already drifted out of the surface enum,
//! producing an out-of-range surface tag whenever the runtime returned 13. This
//! test fails closed on that class of drift.
//!
//! `hew-codegen-rs` is the natural home: it already depends on both `hew-runtime`
//! and `hew-types`, and the codegen `ASKERR_*` literals being pinned live here.

use hew_runtime::duplex::{RecvError, SendError};
use hew_runtime::internal::types::AskError;

/// The surface `AskError`, in declaration (= discriminant) order, as the
/// frontend catalogs declare it. Read from the `hew-types` monomorphic catalog
/// so this is NOT a hand-copy: a frontend change moves the expectation.
fn surface_askerror_variants() -> Vec<&'static str> {
    let catalog = hew_types::builtin_enums::monomorphic_builtin_enums();
    let entry = catalog
        .iter()
        .find(|e| e.name == "AskError")
        .expect("hew-types monomorphic catalog must contain AskError");
    entry.variants.iter().map(|v| v.name).collect()
}

/// The runtime `AskError`, in discriminant order, paired with its `#[repr(i32)]`
/// value read off the live enum (NOT a literal copy — `as i32` reads the
/// producer).
fn runtime_askerror_variants() -> Vec<(&'static str, i32)> {
    use AskError::{
        ActorStopped, Backpressure, Cancelled, ConnectionDropped, DecodeFailure, EncodeFailed,
        LocalShutdown, MailboxFull, MonitorLost, NoRunnableWork, NodeNotRunning,
        None as RuntimeNone, OrphanedAsk, Partition, PayloadSizeMismatch, RoutingFailed,
        SendFailed, StaleRef, Timeout, Unauthorized, VersionMismatch, WorkerAtCapacity,
    };
    vec![
        ("None", RuntimeNone as i32),
        ("NodeNotRunning", NodeNotRunning as i32),
        ("RoutingFailed", RoutingFailed as i32),
        ("EncodeFailed", EncodeFailed as i32),
        ("SendFailed", SendFailed as i32),
        ("Timeout", Timeout as i32),
        ("ConnectionDropped", ConnectionDropped as i32),
        ("PayloadSizeMismatch", PayloadSizeMismatch as i32),
        ("WorkerAtCapacity", WorkerAtCapacity as i32),
        ("ActorStopped", ActorStopped as i32),
        ("MailboxFull", MailboxFull as i32),
        ("OrphanedAsk", OrphanedAsk as i32),
        ("NoRunnableWork", NoRunnableWork as i32),
        ("DecodeFailure", DecodeFailure as i32),
        ("Partition", Partition as i32),
        ("StaleRef", StaleRef as i32),
        ("Cancelled", Cancelled as i32),
        ("LocalShutdown", LocalShutdown as i32),
        ("VersionMismatch", VersionMismatch as i32),
        ("Unauthorized", Unauthorized as i32),
        ("Backpressure", Backpressure as i32),
        ("MonitorLost", MonitorLost as i32),
    ]
}

/// The runtime `None` sentinel is projected to the surface name `NoError` to
/// avoid colliding with `Option::None` in the bare-name variant registry (see
/// the `builtin-enum-variant-mirror-discipline` LESSONS row). Every other
/// position shares the canonical name.
fn runtime_to_surface_name(runtime_name: &str) -> &str {
    match runtime_name {
        "None" => "NoError",
        other => other,
    }
}

#[test]
fn surface_askerror_count_matches_runtime() {
    let surface = surface_askerror_variants();
    let runtime = runtime_askerror_variants();
    assert_eq!(
        surface.len(),
        runtime.len(),
        "surface AskError has {} variants but runtime AskError has {}. \
         A runtime variant absent from the surface (the ENUM-1 / DecodeFailure \
         drift) projects an out-of-range tag in emit_remote_ask_err_from_last_error. \
         Add the missing variant to all three frontend catalogs + std/builtins.hew.\n\
         surface: {surface:?}\nruntime: {runtime:?}",
        surface.len(),
        runtime.len(),
    );
}

#[test]
fn runtime_askerror_discriminants_are_dense_and_match_surface_position() {
    let surface = surface_askerror_variants();
    let runtime = runtime_askerror_variants();

    for (i, (runtime_name, discriminant)) in runtime.iter().enumerate() {
        // Dense: the i-th runtime variant has discriminant i. The surface
        // projection is index-based (the codegen stores the raw runtime code as
        // the surface tag), so a non-dense or reordered runtime enum silently
        // mis-selects a surface arm.
        assert_eq!(
            *discriminant,
            i32::try_from(i).unwrap(),
            "runtime AskError::{runtime_name} has discriminant {discriminant}, expected {i}. \
             The runtime->surface projection is positional; a gap or reorder mis-selects \
             a surface match arm."
        );
        // Name parity at each position (modulo the documented None->NoError
        // sentinel rename).
        let expected_surface_name = runtime_to_surface_name(runtime_name);
        assert_eq!(
            surface[i], expected_surface_name,
            "AskError position {i}: runtime `{runtime_name}` projects to surface \
             `{expected_surface_name}` but the surface catalog has `{}`. Runtime and \
             surface must agree by position.",
            surface[i]
        );
    }
}

#[test]
fn no_runtime_askerror_variant_exceeds_surface_count() {
    // The live ENUM-1 drift check: before DecodeFailure was added to the
    // surface, the runtime's max discriminant (13) was >= the surface count
    // (13), so emit_remote_ask_err_from_last_error could store tag 13 into a
    // 13-variant surface enum (0..=12) — an out-of-range tag. This is the
    // assertion that would have gone red on the realized drift.
    let surface_count = i32::try_from(surface_askerror_variants().len()).unwrap();
    let runtime = runtime_askerror_variants();
    let max_runtime = runtime
        .iter()
        .map(|(_, d)| *d)
        .max()
        .expect("runtime AskError is non-empty");
    assert!(
        max_runtime < surface_count,
        "runtime AskError max discriminant {max_runtime} is not below the surface \
         variant count {surface_count}; the runtime can produce a code with no surface \
         arm. Extend the surface enum (all three catalogs + std/builtins.hew)."
    );
}

#[test]
fn codegen_askerr_literals_match_runtime_discriminants() {
    // The lambda-actor ask path in llvm.rs (`emit_lambda_actor_ask`, the
    // `err_bb` SendError->AskError mapping) hardcodes these AskError tag
    // literals. They are function-local `const`s (not exportable), so the test
    // pins each literal to the live runtime discriminant of the same-named
    // variant. A runtime renumber moves the runtime side; this test then
    // demands the codegen literal follow.
    //
    // Keep in lock-step with llvm.rs:
    //   const ASKERR_SEND_FAILED:   u64 = 4;
    //   const ASKERR_ACTOR_STOPPED: u64 = 9;
    //   const ASKERR_MAILBOX_FULL:  u64 = 10;
    //   const ASKERR_ORPHANED_ASK:  u64 = 11;
    const ASKERR_SEND_FAILED: i32 = 4;
    const ASKERR_ACTOR_STOPPED: i32 = 9;
    const ASKERR_MAILBOX_FULL: i32 = 10;
    const ASKERR_ORPHANED_ASK: i32 = 11;

    assert_eq!(
        ASKERR_SEND_FAILED,
        AskError::SendFailed as i32,
        "codegen ASKERR_SEND_FAILED literal drifted from runtime AskError::SendFailed"
    );
    assert_eq!(
        ASKERR_ACTOR_STOPPED,
        AskError::ActorStopped as i32,
        "codegen ASKERR_ACTOR_STOPPED literal drifted from runtime AskError::ActorStopped"
    );
    assert_eq!(
        ASKERR_MAILBOX_FULL,
        AskError::MailboxFull as i32,
        "codegen ASKERR_MAILBOX_FULL literal drifted from runtime AskError::MailboxFull"
    );
    assert_eq!(
        ASKERR_ORPHANED_ASK,
        AskError::OrphanedAsk as i32,
        "codegen ASKERR_ORPHANED_ASK literal drifted from runtime AskError::OrphanedAsk"
    );

    // The same literals must agree with the surface position of the same-named
    // variant (closes ENUM-3: the codegen literal, runtime discriminant, and
    // surface index are one identity).
    let surface = surface_askerror_variants();
    for (name, literal) in [
        ("SendFailed", ASKERR_SEND_FAILED),
        ("ActorStopped", ASKERR_ACTOR_STOPPED),
        ("MailboxFull", ASKERR_MAILBOX_FULL),
        ("OrphanedAsk", ASKERR_ORPHANED_ASK),
    ] {
        let surface_pos = surface
            .iter()
            .position(|n| *n == name)
            .unwrap_or_else(|| panic!("surface AskError missing `{name}`"));
        assert_eq!(
            literal,
            i32::try_from(surface_pos).unwrap(),
            "codegen ASKERR literal for `{name}` ({literal}) != surface position ({surface_pos})"
        );
    }
}

#[test]
fn codegen_senderror_to_askerror_mapping_targets_are_valid_runtime_discriminants() {
    // llvm.rs maps SendError -> AskError (the `err_bb` select chain):
    //   Closed       (1) -> SendFailed   (4)
    //   Full         (2) -> MailboxFull  (10)
    //   ActorStopped (3) -> ActorStopped (9)
    //   DoubleClose  (4) -> SendFailed   (4)
    //   OrphanedAsk  (5) -> OrphanedAsk  (11)
    //   <unknown>        -> SendFailed   (4)  (catch-all)
    // Pin both the SendError source discriminants and the AskError targets to
    // the live runtime enums.
    assert_eq!(SendError::Closed as i32, 1);
    assert_eq!(SendError::Full as i32, 2);
    assert_eq!(SendError::ActorStopped as i32, 3);
    assert_eq!(SendError::DoubleClose as i32, 4);
    assert_eq!(SendError::OrphanedAsk as i32, 5);

    // Each mapping target is the runtime AskError discriminant the codegen
    // expects. Derived from the runtime enum so a renumber moves both sides.
    let mapping: [(SendError, AskError); 5] = [
        (SendError::Closed, AskError::SendFailed),
        (SendError::Full, AskError::MailboxFull),
        (SendError::ActorStopped, AskError::ActorStopped),
        (SendError::DoubleClose, AskError::SendFailed),
        (SendError::OrphanedAsk, AskError::OrphanedAsk),
    ];
    // Concrete codegen target literals, kept in lock-step with the llvm.rs
    // select chain; the assertion ties them to the runtime AskError producer.
    let expected_targets = [4_i32, 10, 9, 4, 11];
    for ((_src, ask), expected) in mapping.iter().zip(expected_targets) {
        assert_eq!(
            *ask as i32, expected,
            "SendError->AskError mapping target drifted from the runtime AskError discriminant"
        );
    }
}

#[test]
fn runtime_recv_error_discriminants_are_pinned() {
    // RecvError is the bounded-channel recv failure ABI. Pinned so a renumber
    // is caught (channel_wasm / native channel project these into surface arms
    // by discriminant).
    assert_eq!(RecvError::Ok as i32, 0);
    assert_eq!(RecvError::Closed as i32, 1);
    assert_eq!(RecvError::Empty as i32, 2);
    assert_eq!(RecvError::PartitionDetected as i32, 3);
}

#[test]
fn runtime_send_error_discriminants_are_pinned() {
    assert_eq!(SendError::Ok as i32, 0);
    assert_eq!(SendError::Closed as i32, 1);
    assert_eq!(SendError::Full as i32, 2);
    assert_eq!(SendError::ActorStopped as i32, 3);
    assert_eq!(SendError::DoubleClose as i32, 4);
    assert_eq!(SendError::OrphanedAsk as i32, 5);
}

/// The surface (user-visible) `SendError`, in declaration (= discriminant)
/// order, read from the `hew-types` monomorphic catalog (NOT a hand-copy).
fn surface_senderror_variants() -> Vec<&'static str> {
    let catalog = hew_types::builtin_enums::monomorphic_builtin_enums();
    let entry = catalog
        .iter()
        .find(|e| e.name == "SendError")
        .expect("hew-types monomorphic catalog must contain SendError");
    entry.variants.iter().map(|v| v.name).collect()
}

#[test]
fn remote_tell_stale_ref_mapping_is_pinned_cross_crate() {
    // `emit_remote_pid_tell_call` (llvm.rs) maps the runtime send rc to the
    // user-visible `SendError` discriminant:
    //   rc == HEW_ERR_STALE_REF (-16)  -> SendError::StaleRef          (surface 4)
    //   any other nonzero rc           -> SendError::NodeRoutingNotWired (surface 2)
    // The codegen-side literals are function-local consts; this test pins them
    // to (a) the runtime send-error rc constant and (b) the surface enum
    // positions, so a renumber on either side goes red here.

    // (a) The send-path StaleRef rc constant the codegen branches on must equal
    // the runtime producer `hew_node::HEW_ERR_STALE_REF`.
    const CODEGEN_HEW_ERR_STALE_REF: i32 = -16;
    assert_eq!(
        CODEGEN_HEW_ERR_STALE_REF,
        hew_runtime::hew_node::HEW_ERR_STALE_REF,
        "codegen send-path StaleRef rc literal drifted from runtime HEW_ERR_STALE_REF"
    );

    // (b) The surface SendError positions the codegen writes.
    const CODEGEN_SEND_ERR_STALE_REF: usize = 4;
    const CODEGEN_SEND_ERR_NODE_ROUTING: usize = 2;
    let surface = surface_senderror_variants();
    assert_eq!(
        surface.get(CODEGEN_SEND_ERR_STALE_REF).copied(),
        Some("StaleRef"),
        "surface SendError position {CODEGEN_SEND_ERR_STALE_REF} is not StaleRef; \
         the codegen tell-path writes that discriminant for a stale capture.\n\
         surface: {surface:?}"
    );
    assert_eq!(
        surface.get(CODEGEN_SEND_ERR_NODE_ROUTING).copied(),
        Some("NodeRoutingNotWired"),
        "surface SendError position {CODEGEN_SEND_ERR_NODE_ROUTING} is not \
         NodeRoutingNotWired; the codegen tell-path writes that discriminant for a \
         generic routing failure.\nsurface: {surface:?}"
    );
}
