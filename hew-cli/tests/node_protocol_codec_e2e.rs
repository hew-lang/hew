//! The generated Rust and ordinary native Hew walks share format machinery.
mod support;

#[path = "../../hew-runtime/src/node_protocol_generated.rs"]
mod protocol;

use hew_codec::Format;
use protocol::{CleanupFault, DownReason, MonitorDown, ServiceOffer, WireLocation, WireSession};

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
        let offer_bytes = std::fs::read(dir.path().join(format!("hew-offer.{extension}"))).unwrap();
        let down_bytes = std::fs::read(dir.path().join(format!("hew-down.{extension}"))).unwrap();
        assert_eq!(ServiceOffer::decode(format, &offer_bytes).unwrap(), offer());
        assert_eq!(MonitorDown::decode(format, &down_bytes).unwrap(), down());
        assert_eq!(offer_bytes, offer().encode(format), "same accepted bytes");
        assert_eq!(down_bytes, down().encode(format), "same accepted bytes");
    }
}
