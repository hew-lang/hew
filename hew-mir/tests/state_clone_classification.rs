//! W2.002 Stage 1 — actor-state clone classification integration tests.
//!
//! Pins the MIR substrate the W2.002 Stages 2-4 codegen lane consumes:
//!
//! - `ActorLayout.state_clone_fn_symbol` / `state_drop_fn_symbol` are
//!   populated for classifiable actors and `None` for unclassifiable
//!   ones (paired Some/None; substrate-first per dispatch-invariant #1).
//! - `ActorLayout.state_field_clone_kinds` matches the classifier's
//!   per-field output in declaration order.
//! - The Workspace/Entry synthetic fixture (plan §4.6) exercises the
//!   `UserRecord` recursion + visited-set machinery end-to-end through
//!   `parser → checker → HIR → MIR`.
//! - Representative samples of the audit's 110 classified actors (Counter,
//!   `ChatRoom`-shape, `mqtt_broker` `Router`, Connection-bearing actor)
//!   produce the expected classifications.

use std::collections::HashSet;

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::model::RecordLayout;
use hew_mir::{
    classify_actor_state_fields, lower_hir_module, ClassificationError, IoHandleKind,
    StateFieldCloneKind,
};
use hew_types::{module_registry::ModuleRegistry, Checker, ResolvedTy};

/// Pipe a `.hew` source through parser → checker → HIR → MIR, returning
/// the produced `IrPipeline`. Asserts no parser or HIR diagnostics;
/// per-test assertions handle MIR diagnostics.
fn lower_source(source: &str) -> hew_mir::IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let hir = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        hir.diagnostics.is_empty(),
        "HIR diagnostics: {:#?}",
        hir.diagnostics
    );
    lower_hir_module(&hir.module)
}

/// Find an actor layout by name. Panics with a helpful message if absent.
fn find_actor<'a>(pipeline: &'a hew_mir::IrPipeline, name: &str) -> &'a hew_mir::ActorLayout {
    pipeline
        .actor_layouts
        .iter()
        .find(|a| a.name == name)
        .unwrap_or_else(|| {
            panic!(
                "no actor `{name}` in pipeline; have: {:?}",
                pipeline
                    .actor_layouts
                    .iter()
                    .map(|a| &a.name)
                    .collect::<Vec<_>>()
            )
        })
}

// ─── Substrate shape ─────────────────────────────────────────────────

#[test]
fn trivial_state_actor_gets_paired_clone_and_drop_symbols() {
    // Audit row #6 / #8 / #9 / etc.: `actor Counter { let count: i64; }`
    // is the dominant (89/110) trivial-state shape. Classifier produces
    // a single `BitCopy { size_bytes: 8 }`; ActorLayout carries both
    // synthesized symbol names.
    let src = r"
        actor Counter {
            let count: i64;
        }
    ";
    let pipeline = lower_source(src);
    assert!(
        pipeline.diagnostics.is_empty(),
        "unexpected MIR diagnostics: {:#?}",
        pipeline.diagnostics
    );
    let counter = find_actor(&pipeline, "Counter");
    assert_eq!(
        counter.state_clone_fn_symbol.as_deref(),
        Some("__hew_state_clone_Counter"),
    );
    assert_eq!(
        counter.state_drop_fn_symbol.as_deref(),
        Some("__hew_state_drop_Counter"),
    );
    assert_eq!(
        counter.state_field_clone_kinds.as_deref(),
        Some(&[StateFieldCloneKind::BitCopy { size_bytes: 8 }][..]),
    );
}

#[test]
fn zero_state_actor_also_gets_paired_symbols() {
    // Plan §4.2: "Emit both `state_clone_fn` and `state_drop_fn`
    // registration unconditionally for every actor." Zero-state actors
    // get the synthesized wrapper too — Stage 3's body is a trivial
    // `malloc + memcpy(0)` / `libc::free`. Classifier returns an empty
    // kinds vec.
    let src = r"
        actor Marker {
            receive fn ping() {}
        }
    ";
    let pipeline = lower_source(src);
    assert!(
        pipeline.diagnostics.is_empty(),
        "unexpected MIR diagnostics: {:#?}",
        pipeline.diagnostics
    );
    let marker = find_actor(&pipeline, "Marker");
    assert_eq!(
        marker.state_clone_fn_symbol.as_deref(),
        Some("__hew_state_clone_Marker"),
    );
    assert_eq!(
        marker.state_drop_fn_symbol.as_deref(),
        Some("__hew_state_drop_Marker"),
    );
    assert_eq!(marker.state_field_clone_kinds.as_deref(), Some(&[][..]),);
}

#[test]
fn classification_is_paired_some_some_or_none_none() {
    // Substrate invariant the model.rs doc pins: clone/drop symbols
    // track each other. Either both Some (classification succeeded) or
    // both None (classification failed and diagnostic surfaced). Lane
    // §8.8 — emitting only one without the other converts a leak into a
    // UAF when Q185(c) is lifted.
    let src = r"
        actor A { let x: i64; }
        actor B { let s: string; }
    ";
    let pipeline = lower_source(src);
    for actor in &pipeline.actor_layouts {
        assert_eq!(
            actor.state_clone_fn_symbol.is_some(),
            actor.state_drop_fn_symbol.is_some(),
            "clone/drop symbol pairing broken on actor `{}`",
            actor.name,
        );
        assert_eq!(
            actor.state_clone_fn_symbol.is_some(),
            actor.state_field_clone_kinds.is_some(),
            "symbol/kinds pairing broken on actor `{}`",
            actor.name,
        );
    }
}

// ─── Per-actor representative classifications (audit §3) ────────────

#[test]
fn chatroom_shape_classifies_vec_of_string_and_vec_of_actor_ref() {
    // Audit row #1 of stdlib-supervision-relevant shapes:
    // `ChatRoom { handlers: Vec<ActorRef<ClientHandler>>, names: Vec<string> }`.
    // The classifier sees `Vec<ActorRef<ClientHandler>>` (Vec of
    // BitCopy because ActorRef is repr(C) bit-copy per audit §1) and
    // `Vec<string>` (Vec of String, needs per-element deep clone).
    let src = r"
        actor ClientHandler {
            receive fn msg(text: string) {}
        }
        actor ChatRoom {
            let handlers: Vec<ActorRef<ClientHandler>>;
            let names: Vec<string>;
        }
    ";
    let pipeline = lower_source(src);
    assert!(
        pipeline.diagnostics.is_empty(),
        "unexpected MIR diagnostics: {:#?}",
        pipeline.diagnostics
    );
    let chatroom = find_actor(&pipeline, "ChatRoom");
    let kinds = chatroom
        .state_field_clone_kinds
        .as_ref()
        .expect("classified");
    assert_eq!(kinds.len(), 2);
    match &kinds[0] {
        StateFieldCloneKind::Vec { elem } => match elem.as_ref() {
            // ActorRef collapses to BitCopy per plan §4.5 A.
            StateFieldCloneKind::BitCopy { .. } => {}
            other => panic!("Vec<ActorRef<...>> element should be BitCopy, got {other:?}"),
        },
        other => panic!("field 0 should be Vec, got {other:?}"),
    }
    assert_eq!(
        kinds[1],
        StateFieldCloneKind::Vec {
            elem: Box::new(StateFieldCloneKind::String),
        },
    );
}

#[test]
fn user_type_named_connection_classifies_as_user_record_not_iohandle() {
    // Cross-eco review regression: `ResolvedTy::Named { name }` does
    // not preserve the checker's builtin discriminator. A user-declared
    // `type Connection { ... }` and the runtime builtin `net.Connection`
    // both reach the classifier as `Named { name: "Connection", args:
    // [] }`. record_layouts-first guards in `classify_named` ensure the
    // user record wins — owned fields inside it (here: `payload:
    // Vec<i32>`) recurse properly through the UserRecord arm, so
    // Stage 3's synthesized drop walks them. Without the guard, the
    // whole record would be silently misclassified as `IoHandle {
    // Connection }`, bypassing per-field drop and leaking the Vec
    // (or, post-Q185(c) lift, UAFing). See module header on W4.011 for
    // the structural fix.
    let src = r"
        type Connection {
            id: i32;
            payload: Vec<i32>;
        }
        actor Handler {
            let conn: Connection;
            let label: string;
        }
    ";
    let pipeline = lower_source(src);
    assert!(
        pipeline.diagnostics.is_empty(),
        "unexpected MIR diagnostics: {:#?}",
        pipeline.diagnostics
    );
    let handler = find_actor(&pipeline, "Handler");
    let kinds = handler
        .state_field_clone_kinds
        .as_ref()
        .expect("classified");
    assert_eq!(kinds.len(), 2);
    assert_eq!(
        kinds[0],
        StateFieldCloneKind::UserRecord {
            name: "Connection".to_string(),
        },
        "user `type Connection` must classify as UserRecord, NOT IoHandle — \
         dispatch-invariant #10 string-identifier-fragility (W4.011)",
    );
    assert_eq!(kinds[1], StateFieldCloneKind::String);
    // The Connection record's own layout must be present so Stage 3's
    // recursive synthesis can walk its owned fields.
    assert!(
        pipeline
            .record_layouts
            .iter()
            .any(|r| r.name == "Connection"),
        "Connection RecordLayout missing — Stage 3 recursive synthesis would fail",
    );
}

#[test]
fn builtin_connection_without_record_layout_classifies_as_iohandle() {
    // Direct-classifier test (no parser involvement) pinning the
    // other half of the contract: when no user record shadows the
    // name, `Named("Connection", [])` classifies as the IoHandle
    // builtin. This is the surface Stage 2 will gate at supervisor-
    // restart sites per plan §4.5 B.
    let mut visited = HashSet::new();
    let result = hew_mir::classify_state_field(
        &ResolvedTy::Named {
            name: "Connection".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        },
        &[], // empty record_layouts → builtin path
        &mut visited,
    )
    .expect("classified");
    assert_eq!(
        result,
        StateFieldCloneKind::IoHandle {
            kind: IoHandleKind::Connection,
        },
    );
}

#[test]
fn user_type_named_vec_does_not_route_to_container_arm() {
    // Same dispatch-invariant #10 hazard: a user `type Vec { ... }`
    // (admittedly evil but legal at the MIR substrate layer) must
    // classify as `UserRecord`, not the runtime `Vec<T>` container
    // arm. record_layouts-first guards every Named name, not just
    // Connection.
    let mut visited = HashSet::new();
    let records = vec![hew_mir::model::RecordLayout {
        name: "Vec".to_string(),
        field_tys: vec![ResolvedTy::I64],
        field_names: vec![],
    }];
    let result = hew_mir::classify_state_field(
        &ResolvedTy::Named {
            name: "Vec".to_string(),
            args: vec![], // no args — a user record-named "Vec"
            builtin: None,
            is_opaque: false,
        },
        &records,
        &mut visited,
    )
    .expect("classified");
    assert_eq!(
        result,
        StateFieldCloneKind::UserRecord {
            name: "Vec".to_string(),
        },
        "user `type Vec` must classify as UserRecord, NOT container arm",
    );
}

#[test]
fn user_type_named_actor_ref_does_not_route_to_bitcopy_arm() {
    // Same hazard for the handle-typed builtins (ActorRef / Actor /
    // LocalPid). A user record shadowing one of these names must NOT
    // collapse to BitCopy — that would skip the per-field drop of any
    // owned fields the user declared.
    let mut visited = HashSet::new();
    let records = vec![hew_mir::model::RecordLayout {
        name: "ActorRef".to_string(),
        field_tys: vec![ResolvedTy::String],
        field_names: vec![],
    }];
    let result = hew_mir::classify_state_field(
        &ResolvedTy::Named {
            name: "ActorRef".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        },
        &records,
        &mut visited,
    )
    .expect("classified");
    assert_eq!(
        result,
        StateFieldCloneKind::UserRecord {
            name: "ActorRef".to_string(),
        },
    );
}

#[test]
fn router_shape_vec_of_user_connection_carries_user_record_through() {
    // Updated post-review: in the real corpus, `Connection` is a
    // user-declared `type Connection { ... }` in `std/net/net.hew`,
    // so `Vec<Connection>` classifies as `Vec<UserRecord("Connection")>`
    // — NOT `Vec<IoHandle{Connection}>`. Stage 2's supervisor gate
    // must pattern-match on the user record's owned-heap fields (or
    // detect by another channel — see W4.011), not on the IoHandle
    // arm. This test pins the post-fix behaviour so Stage 2's gate
    // design doesn't regress.
    let src = r"
        type Connection {
            handle: i64;
        }
        actor Client {
            receive fn ping() {}
        }
        actor Router {
            let conns: Vec<Connection>;
            let clients: Vec<ActorRef<Client>>;
            let qos: Vec<i32>;
        }
    ";
    let pipeline = lower_source(src);
    assert!(
        pipeline.diagnostics.is_empty(),
        "unexpected MIR diagnostics: {:#?}",
        pipeline.diagnostics
    );
    let router = find_actor(&pipeline, "Router");
    let kinds = router.state_field_clone_kinds.as_ref().expect("classified");
    assert_eq!(kinds.len(), 3);
    assert_eq!(
        kinds[0],
        StateFieldCloneKind::Vec {
            elem: Box::new(StateFieldCloneKind::UserRecord {
                name: "Connection".to_string(),
            }),
        },
    );
    match &kinds[1] {
        StateFieldCloneKind::Vec { elem } => assert!(
            matches!(elem.as_ref(), StateFieldCloneKind::BitCopy { .. }),
            "Vec<ActorRef<Client>> element should be BitCopy, got {elem:?}",
        ),
        other => panic!("field 1 should be Vec, got {other:?}"),
    }
    assert_eq!(
        kinds[2],
        StateFieldCloneKind::Vec {
            elem: Box::new(StateFieldCloneKind::BitCopy { size_bytes: 4 }),
        },
    );
}

// ─── Synthetic Workspace fixture — UserRecord recursion + visited set ──

#[test]
fn workspace_fixture_exercises_user_record_recursion_end_to_end() {
    // Plan §4.6 + dispatcher synthetic fixture requirement. This is the
    // ONLY exercise of the `UserRecord` arm + visited-set machinery in
    // the corpus; audit §1 confirms zero organic users.
    let source = include_str!("fixtures/state_clone_workspace.hew");
    let pipeline = lower_source(source);
    assert!(
        pipeline.diagnostics.is_empty(),
        "unexpected MIR diagnostics: {:#?}",
        pipeline.diagnostics
    );
    let server = find_actor(&pipeline, "Server");
    let kinds = server.state_field_clone_kinds.as_ref().expect("classified");
    assert_eq!(kinds.len(), 1);
    assert_eq!(
        kinds[0],
        StateFieldCloneKind::UserRecord {
            name: "Workspace".to_string(),
        },
    );
    // Stage 3 will descend into Workspace's RecordLayout at synthesis
    // time. Pin that the layouts are present so the lookup invariant
    // holds.
    assert!(
        pipeline
            .record_layouts
            .iter()
            .any(|r| r.name == "Workspace"),
        "Workspace RecordLayout missing — synthesis would fail at Stage 3",
    );
    assert!(
        pipeline.record_layouts.iter().any(|r| r.name == "Entry"),
        "Entry RecordLayout missing — synthesis would fail at Stage 3",
    );
}

#[test]
fn workspace_visited_set_terminates_classifier_directly() {
    // Defence-in-depth: even though the language doesn't permit
    // self-referential records today, the visited-set guard MUST fire
    // synthetically. This test bypasses the parser by constructing
    // RecordLayouts that mirror the Workspace shape AND adds a
    // hypothetical self-reference; the classifier must report
    // `RecordCycle` rather than recurse forever.
    //
    // Pinning the behaviour here means a future `Box<T>` /
    // boxed-recursive-enum addition cannot accidentally remove the
    // guard.
    let records = vec![RecordLayout {
        name: "Cyclic".to_string(),
        field_tys: vec![
            ResolvedTy::Named {
                name: "Cyclic".to_string(),
                args: vec![],
                builtin: None,
                is_opaque: false,
            },
            ResolvedTy::I64,
        ],
        field_names: vec![],
    }];
    let result = classify_actor_state_fields(
        &[ResolvedTy::Named {
            name: "Cyclic".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        }],
        &records,
    );
    assert!(
        matches!(result, Err(ClassificationError::RecordCycle { ref name }) if name == "Cyclic"),
        "expected RecordCycle on `Cyclic`, got {result:?}",
    );
}

// ─── Fail-closed surface (no silent no-op) ───────────────────────────

#[test]
fn missing_record_layout_surfaces_diagnostic_not_silent_none() {
    // dispatch-invariant #3 (`no-silent-no-op-stubs`): if classification
    // fails, the actor's clone/drop symbols are None AND a MIR
    // diagnostic surfaces. Constructing this case via Hew source is
    // not possible (the type-checker rejects references to undefined
    // types upstream), so we drive the classifier directly to pin the
    // surface contract.
    let mut visited = HashSet::new();
    let result = hew_mir::classify_state_field(
        &ResolvedTy::Named {
            name: "DoesNotExist".to_string(),
            args: vec![],
            builtin: None,
            is_opaque: false,
        },
        &[],
        &mut visited,
    );
    assert!(matches!(
        result,
        Err(ClassificationError::MissingRecordLayout { ref name }) if name == "DoesNotExist"
    ));
}

// ─── Per-actor classification coverage report ────────────────────────

#[test]
fn coverage_report_against_audit_representatives() {
    // The state-clone actor-classification audit enumerates 110 actors.
    // Re-running every one
    // through the parser in this test would couple it to the entire
    // examples/ tree (and to v0.5 examples that don't all parse
    // cleanly). Instead, we verify the classifier produces the audit-
    // claimed result for one representative per equivalence class:
    //
    //   trivial-state             → Counter      (89 actors in class)
    //   string + Vec<String>      → ChatRoom-like (covered in §1 test)
    //   Vec<Connection>           → Router-like   (covered in §1 test)
    //   ActorRef in state         → Router/Listener (covered in §1)
    //   nested user record        → Workspace fixture (covered above)
    //
    // The substrate-level claim is: "if Stage 0's 110-actor enumeration
    // is correct, then Stage 1 classifies every one without surfacing
    // an unsupported-shape diagnostic." We verify it directly for the
    // representatives; Stages 2-4's IR-goldens will pick up the rest.
    let src = r"
        actor Counter { let count: i64; }
        actor StringHolder { let s: string; }
        actor BytesHolder { let b: bytes; }
        actor BoolHolder { let flag: bool; }
        actor FloatHolder { let x: f64; }
        actor TupleOfPrimitives { let count: i64; let flag: bool; }
    ";
    let pipeline = lower_source(src);
    assert!(
        pipeline.diagnostics.is_empty(),
        "unexpected MIR diagnostics on the audit representatives: {:#?}",
        pipeline.diagnostics
    );
    for actor in &pipeline.actor_layouts {
        assert!(
            actor.state_clone_fn_symbol.is_some(),
            "audit representative `{}` failed classification",
            actor.name,
        );
    }
}

// ─── Opaque-in-container fail-close (round-4, end-to-end) ─────────────

/// `Vec<Widget>` actor state where `Widget` is `#[opaque]` MUST fail closed
/// end-to-end (parser → checker → HIR → MIR): the clone/drop symbols are
/// `None` and an `ActorStateCloneClassificationFailed` diagnostic surfaces.
/// Before the round-4 fix this classified as `Vec { OpaqueHandle }` and
/// codegen cloned the vec handle with a plain element witness, shallow-copying
/// the opaque pointer (double-free / UAF on supervisor restart). Uses a local
/// `#[opaque]` decl so the test is self-contained (no stdlib dependency).
#[test]
fn vec_of_opaque_handle_actor_state_fails_closed_end_to_end() {
    let src = r"
        #[opaque]
        type Widget {}

        actor Holder {
            let v: Vec<Widget>;
            receive fn ping() {}
        }
    ";
    let pipeline = lower_source(src);
    let holder = find_actor(&pipeline, "Holder");
    assert!(
        holder.state_clone_fn_symbol.is_none() && holder.state_drop_fn_symbol.is_none(),
        "Vec<#[opaque]> actor state must leave clone/drop symbols None (paired); got \
         clone={:?} drop={:?}",
        holder.state_clone_fn_symbol,
        holder.state_drop_fn_symbol,
    );
    assert!(
        pipeline.diagnostics.iter().any(|d| matches!(
            &d.kind,
            hew_mir::MirDiagnosticKind::ActorStateCloneClassificationFailed { actor, field_name, .. }
                if actor == "Holder" && field_name == "v"
        )),
        "expected ActorStateCloneClassificationFailed on Holder.v; got: {:#?}",
        pipeline.diagnostics
    );
}

/// `HashMap<string, Widget>` (opaque value) MUST also fail closed end-to-end,
/// confirming the transitive authority covers the map-value position, not just
/// Vec elements.
#[test]
fn hashmap_with_opaque_value_actor_state_fails_closed_end_to_end() {
    let src = r"
        #[opaque]
        type Widget {}

        actor Store {
            let m: HashMap<string, Widget>;
            receive fn ping() {}
        }
    ";
    let pipeline = lower_source(src);
    let store = find_actor(&pipeline, "Store");
    assert!(
        store.state_clone_fn_symbol.is_none(),
        "HashMap<_, #[opaque]> actor state must fail closed; got clone={:?}",
        store.state_clone_fn_symbol,
    );
    assert!(
        pipeline.diagnostics.iter().any(|d| matches!(
            &d.kind,
            hew_mir::MirDiagnosticKind::ActorStateCloneClassificationFailed { actor, .. }
                if actor == "Store"
        )),
        "expected classification-failed diagnostic on Store; got: {:#?}",
        pipeline.diagnostics
    );
}

/// Negative control: a `Vec<Point>` of a plain user record (non-opaque) MUST
/// classify clean end-to-end — the fail-close keys on the opaque discriminator,
/// never on container shape, so legitimate owned-element vecs are unaffected.
#[test]
fn vec_of_non_opaque_record_actor_state_classifies_clean() {
    let src = r"
        type Point {
            x: i64;
            y: i64;
        }

        actor Path {
            let points: Vec<Point>;
            receive fn ping() {}
        }
    ";
    let pipeline = lower_source(src);
    assert!(
        pipeline.diagnostics.is_empty(),
        "Vec<non-opaque record> must classify clean; got: {:#?}",
        pipeline.diagnostics
    );
    let path = find_actor(&pipeline, "Path");
    assert!(
        path.state_clone_fn_symbol.is_some(),
        "Vec<Point> actor state must classify (no over-fire)",
    );
}

// ─── Machine state fields (transition-watch lane, stage-0 pin) ────────────

/// Source fixture: a locally declared machine held as an actor state field.
const MACHINE_FIELD_SOURCE: &str = "
    machine Light {
        events {
            Flip;
        }
        state Off;
        state On;
        on Flip: Off => On { On }
        on Flip: On => Off { Off }
    }

    actor Holder {
        var light: Light = Light::Off;
        receive fn flip() {
            light.step(Flip);
        }
    }

    fn main() {
        let h = spawn Holder;
        h.flip();
    }
";

/// A machine-typed actor state field classifies through the enum
/// clone/drop authority (machines are enums at the value-classification
/// layer — the machine layouts project into enum-layout views). Was the
/// stage-0 fail-closed pin
/// (`machine_actor_state_field_currently_fails_classification`) before the
/// projection landed.
#[test]
fn machine_actor_state_field_classifies_as_enum() {
    let pipeline = lower_source(MACHINE_FIELD_SOURCE);
    assert!(
        pipeline.diagnostics.is_empty(),
        "machine state field must classify clean; got: {:#?}",
        pipeline.diagnostics
    );
    let holder = find_actor(&pipeline, "Holder");
    assert!(
        holder.state_clone_fn_symbol.is_some() && holder.state_drop_fn_symbol.is_some(),
        "machine state field must earn the paired clone/drop symbols; got \
         clone={:?} drop={:?}",
        holder.state_clone_fn_symbol,
        holder.state_drop_fn_symbol,
    );
    let kinds = holder
        .state_field_clone_kinds
        .as_ref()
        .expect("classified actor must carry per-field kinds");
    assert!(
        matches!(
            kinds.as_slice(),
            [StateFieldCloneKind::Enum { name }] if name == "Light"
        ),
        "machine field must classify as Enum {{ name: \"Light\" }}; got {kinds:?}",
    );
}

/// A machine with a heap-payload state (`Failed {{ reason: string }}`)
/// rides the same enum classification — the clone/drop thunks walk the
/// string payload exactly as an enum variant's.
#[test]
fn heap_payload_machine_state_field_classifies_as_enum() {
    let pipeline = lower_source(
        "
        machine Conn {
            events {
                Connect;
                Fail { reason: string; }
            }
            state Idle;
            state Failed { reason: string; }
            on Connect: _ => _ { state }
            on Fail: Idle => Failed { Conn::Failed { reason: event.reason } }
            on Fail: _ => _ { state }
        }

        actor Holder {
            var c: Conn = Conn::Idle;
            receive fn poke() {
                c.step(ConnEvent::Fail { reason: \"boom\" });
            }
        }

        fn main() {
            let h = spawn Holder;
            h.poke();
        }
    ",
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "heap-payload machine state field must classify clean; got: {:#?}",
        pipeline.diagnostics
    );
    let holder = find_actor(&pipeline, "Holder");
    assert!(
        holder.state_clone_fn_symbol.is_some() && holder.state_drop_fn_symbol.is_some(),
        "heap-payload machine field must earn the paired clone/drop symbols",
    );
    let kinds = holder
        .state_field_clone_kinds
        .as_ref()
        .expect("classified actor must carry per-field kinds");
    assert!(
        matches!(
            kinds.as_slice(),
            [StateFieldCloneKind::Enum { name }] if name == "Conn"
        ),
        "heap-payload machine field must classify as Enum {{ name: \"Conn\" }}; got {kinds:?}",
    );
}

// ─── Machine-handle field in a user record/type ──────────────────────
//
// A `machine` held as a field of a user `type` classifies through the
// same enum clone/drop authority that admits a machine as actor state:
// the machine projects into an enum-layout view and the field takes the
// `Enum` arm of the owned-aggregate record classifier. Before the view
// reached the user-function builder, every such record was rejected with
// `UnsupportedUserRecordValueClass` ("field `f` has value class Unknown").
// The honest oracle is the absence of that diagnostic — it fires on the
// unfixed path.

/// True when any pipeline diagnostic is an `UnsupportedUserRecordValueClass`
/// naming `record_name`.
fn has_unsupported_record_value_class(pipeline: &hew_mir::IrPipeline, record_name: &str) -> bool {
    pipeline.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            hew_mir::MirDiagnosticKind::UnsupportedUserRecordValueClass { name, .. }
                if name == record_name
        )
    })
}

/// A payload-free machine field in a user record classifies clean — the
/// record is admitted, mirroring a payload-free enum field.
#[test]
fn payload_free_machine_record_field_admits() {
    let pipeline = lower_source(
        "
        machine F {
            events { a; }
            state S1;
            state S2;
            on a: S1 => S2 { S2 }
            on a: _ => _ { state }
        }

        type P { f: F, x: i64 }

        fn main() {
            let p = P { f: F::S1, x: 0 };
            println(p.x);
        }
    ",
    );
    assert!(
        !has_unsupported_record_value_class(&pipeline, "P"),
        "payload-free machine record field must not be rejected; got: {:#?}",
        pipeline.diagnostics
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "payload-free machine record must lower clean; got: {:#?}",
        pipeline.diagnostics
    );
}

/// An i64-payload machine field in a user record classifies clean: the
/// payload is `BitCopy`, so the enum-view walk admits the record.
#[test]
fn i64_payload_machine_record_field_admits() {
    let pipeline = lower_source(
        "
        machine G {
            events { Step; }
            state Idle;
            state Running { count: i64; }
            on Step: Idle => Running { Running { count: 1 } }
            on Step: _ => _ { state }
        }

        type Q { g: G, x: i64 }

        fn main() {
            let q = Q { g: G::Idle, x: 0 };
            println(q.x);
        }
    ",
    );
    assert!(
        !has_unsupported_record_value_class(&pipeline, "Q"),
        "i64-payload machine record field must not be rejected; got: {:#?}",
        pipeline.diagnostics
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "i64-payload machine record must lower clean; got: {:#?}",
        pipeline.diagnostics
    );
}

/// A string-payload (non-`BitCopy`) machine field in a user record classifies
/// clean and routes through the `CowValue` clone/drop spine — the record earns
/// a `RecordInPlace` thunk exactly as a string-payload enum field does.
#[test]
fn string_payload_machine_record_field_admits() {
    let pipeline = lower_source(
        "
        machine H {
            events { Set; }
            state Empty;
            state Named { label: string; }
            on Set: Empty => Named { Named { label: \"x\" } }
            on Set: _ => _ { state }
        }

        type R { h: H, x: i64 }

        fn main() {
            let r = R { h: H::Empty, x: 0 };
            println(r.x);
        }
    ",
    );
    assert!(
        !has_unsupported_record_value_class(&pipeline, "R"),
        "string-payload machine record field must not be rejected; got: {:#?}",
        pipeline.diagnostics
    );
    assert!(
        pipeline.diagnostics.is_empty(),
        "string-payload machine record must lower clean; got: {:#?}",
        pipeline.diagnostics
    );
}
