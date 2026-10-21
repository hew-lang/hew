//! The generated Rust and ordinary native Hew walks share format machinery.
mod support;

#[path = "../../hew-runtime/src/node_protocol_generated.rs"]
mod protocol;

use hew_codec::Format;
use protocol::{
    CleanupFault, ClusterEnrollment, ClusterMember, DownReason, MonitorDown, NodeHandshake,
    RegistryGossip, RelayChunk, RelayClose, RelayOpen, ServiceOffer, SwimControl, SwimGossip,
    WireLocation, WireSession,
};

fn offer() -> ServiceOffer {
    ServiceOffer {
        grant_id: 7,
        target: WireLocation {
            node: b"0123456789abcdef".to_vec(),
            target_id: 4_294_967_297,
            incarnation: 4,
            target_kind: 1,
        },
        recipient: WireSession {
            node: b"fedcba9876543210".to_vec(),
            incarnation: 2,
        },
        handlers: vec!["tests.Service.read".into(), "tests.Service.write".into()],
        request_id: 3,
    }
}

fn down() -> MonitorDown {
    MonitorDown {
        ref_id: 19,
        target: offer().target,
        reason: DownReason { tag: 1, code: -1 },
        crash_kind: 2,
        end_reason: 2,
        close_fault: Some(CleanupFault {
            code: -7,
            message: "cleanup café".into(),
        }),
    }
}

fn registry() -> RegistryGossip {
    RegistryGossip {
        op: 5,
        name: "worker café".into(),
        location: offer().target,
    }
}

fn swim() -> SwimControl {
    SwimControl {
        msg_type: 3,
        from: offer().recipient,
        incarnation: 4_294_967_297,
        target: Some(WireSession {
            node: b"0123456789abcdef".to_vec(),
            incarnation: 4,
        }),
        gossip: vec![SwimGossip {
            member: offer().recipient,
            state: 2,
            incarnation: 19,
        }],
    }
}

fn member() -> ClusterMember {
    ClusterMember {
        transport: 0,
        credential: b"0123456789abcdef0123456789abcdef".to_vec(),
    }
}

fn enrollment() -> ClusterEnrollment {
    ClusterEnrollment {
        authority: b"authority".to_vec(),
        member: member().encode(Format::Cbor),
        signature: b"signature".to_vec(),
    }
}

fn handshake() -> NodeHandshake {
    NodeHandshake {
        protocol_version: u16::MAX,
        schema_hash: u32::MAX,
        feature_flags: 3,
        node: b"0123456789abcdef".to_vec(),
        incarnation: 4,
        noise_key: member().credential,
        enrollment: enrollment().encode(Format::Cbor),
    }
}

fn relay_open() -> RelayOpen {
    RelayOpen {
        circuit: 17,
        origin: offer().recipient,
        target: WireSession {
            node: b"0123456789abcdef".to_vec(),
            incarnation: 4,
        },
    }
}

fn relay_chunk() -> RelayChunk {
    RelayChunk {
        circuit: 17,
        offset: 4,
        body: b"relay bytes".to_vec(),
    }
}
fn relay_close() -> RelayClose {
    RelayClose { circuit: 17 }
}

fn write_admission_values(dir: &std::path::Path, format: Format, extension: &str) {
    for (name, bytes) in [
        ("member", member().encode(format)),
        ("enrollment", enrollment().encode(format)),
        ("handshake", handshake().encode(format)),
        ("relay-open", relay_open().encode(format)),
        ("relay-chunk", relay_chunk().encode(format)),
        ("relay-close", relay_close().encode(format)),
    ] {
        std::fs::write(dir.join(format!("rust-{name}.{extension}")), bytes).unwrap();
    }
}

fn check_admission_values(dir: &std::path::Path, format: Format, extension: &str) {
    let member_bytes = std::fs::read(dir.join(format!("hew-member.{extension}"))).unwrap();
    let enrollment_bytes = std::fs::read(dir.join(format!("hew-enrollment.{extension}"))).unwrap();
    let handshake_bytes = std::fs::read(dir.join(format!("hew-handshake.{extension}"))).unwrap();
    assert_eq!(
        ClusterMember::decode(format, &member_bytes).unwrap(),
        member()
    );
    assert_eq!(
        ClusterEnrollment::decode(format, &enrollment_bytes).unwrap(),
        enrollment()
    );
    assert_eq!(
        NodeHandshake::decode(format, &handshake_bytes).unwrap(),
        handshake()
    );
    assert_eq!(member_bytes, member().encode(format));
    assert_eq!(enrollment_bytes, enrollment().encode(format));
    assert_eq!(handshake_bytes, handshake().encode(format));
    let open = std::fs::read(dir.join(format!("hew-relay-open.{extension}"))).unwrap();
    let chunk = std::fs::read(dir.join(format!("hew-relay-chunk.{extension}"))).unwrap();
    let close = std::fs::read(dir.join(format!("hew-relay-close.{extension}"))).unwrap();
    assert_eq!(RelayOpen::decode(format, &open).unwrap(), relay_open());
    assert_eq!(RelayChunk::decode(format, &chunk).unwrap(), relay_chunk());
    assert_eq!(RelayClose::decode(format, &close).unwrap(), relay_close());
    assert_eq!(open, relay_open().encode(format));
    assert_eq!(chunk, relay_chunk().encode(format));
    assert_eq!(close, relay_close().encode(format));
}

#[test]
fn checked_node_values_cross_native_hew_and_rust_in_both_formats() {
    support::require_codegen();
    let dir = support::tempdir();
    let root = support::repo_root();
    for name in ["node_controls.hew", "codec_probe.hew"] {
        std::fs::copy(
            root.join("hew-runtime/protocol").join(name),
            dir.path().join(name),
        )
        .unwrap();
    }
    for (format, extension) in [(Format::Cbor, "cbor"), (Format::Msgpack, "msgpack")] {
        write_admission_values(dir.path(), format, extension);
        std::fs::write(
            dir.path().join(format!("rust-registry.{extension}")),
            registry().encode(format),
        )
        .unwrap();
        std::fs::write(
            dir.path().join(format!("rust-swim.{extension}")),
            swim().encode(format),
        )
        .unwrap();
        std::fs::write(
            dir.path().join(format!("rust-offer.{extension}")),
            offer().encode(format),
        )
        .unwrap();
        std::fs::write(
            dir.path().join(format!("rust-down.{extension}")),
            down().encode(format),
        )
        .unwrap();
    }
    let mut compile = std::process::Command::new(support::hew_binary());
    compile
        .args(["compile", "--emit-dir"])
        .arg(dir.path())
        .arg(dir.path().join("codec_probe.hew"))
        .current_dir(dir.path());
    let output = support::run_bounded_command(compile, "compile checked node codec probe");
    support::assert_success(&output, "compile checked node codec probe");
    let binary = dir.path().join(if cfg!(windows) {
        "codec_probe.exe"
    } else {
        "codec_probe"
    });
    let mut run = std::process::Command::new(binary);
    run.current_dir(dir.path());
    let output = support::run_bounded_command(run, "run checked node codec probe");
    support::assert_success(&output, "run checked node codec probe");
    for (format, extension) in [(Format::Cbor, "cbor"), (Format::Msgpack, "msgpack")] {
        check_admission_values(dir.path(), format, extension);
        let registry_bytes =
            std::fs::read(dir.path().join(format!("hew-registry.{extension}"))).unwrap();
        let swim_bytes = std::fs::read(dir.path().join(format!("hew-swim.{extension}"))).unwrap();
        assert_eq!(
            RegistryGossip::decode(format, &registry_bytes).unwrap(),
            registry()
        );
        assert_eq!(SwimControl::decode(format, &swim_bytes).unwrap(), swim());
        assert_eq!(
            registry_bytes,
            registry().encode(format),
            "same accepted registry bytes"
        );
        assert_eq!(
            swim_bytes,
            swim().encode(format),
            "same accepted SWIM bytes"
        );
        let offer_bytes = std::fs::read(dir.path().join(format!("hew-offer.{extension}"))).unwrap();
        let down_bytes = std::fs::read(dir.path().join(format!("hew-down.{extension}"))).unwrap();
        assert_eq!(ServiceOffer::decode(format, &offer_bytes).unwrap(), offer());
        assert_eq!(MonitorDown::decode(format, &down_bytes).unwrap(), down());
        assert_eq!(offer_bytes, offer().encode(format), "same accepted bytes");
        assert_eq!(down_bytes, down().encode(format), "same accepted bytes");
    }
}
