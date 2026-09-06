use std::path::Path;

use hew_compile::{run_file_frontend_to_typecheck, FrontendDiagnosticKind, FrontendOptions};
use hew_types::check::effects::SuspensionEffect;
use hew_types::runtime_call::AsyncIoOp;
use hew_types::{CallTarget, RuntimeCallFamily};

const SOURCE: &str = r#"
import std.fs;
import std.net;

fn files(path: string, bytes: bytes) {
    let _ = await fs.read(path);
    let _ = await fs.read_bytes(path);
    let _ = await fs.write(path, "text");
    let _ = await fs.write_bytes(path, bytes);
}
fn sockets(listener: net.Listener, conn: net.Connection) {
    let _ = await listener.accept();
    let _ = await conn.read();
    let _ = await conn.try_read();
    let _ = await conn.read_string();
    let _ = await conn.try_read_string();
}
fn main() {}
"#;

#[test]
fn canonical_io_wrappers_propagate_checked_native_operation_effects() {
    let directory = tempfile::tempdir().unwrap();
    let input = directory.path().join("main.hew");
    std::fs::write(&input, SOURCE).unwrap();
    let options = FrontendOptions {
        module_search_paths: Some(vec![Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .to_path_buf()]),
        project_dir: Some(directory.path().to_path_buf()),
        ..FrontendOptions::default()
    };
    let state = run_file_frontend_to_typecheck(input.to_str().unwrap(), &options)
        .unwrap_or_else(|error| panic!("{error:#?}"));
    let output = state.typecheck_result.tco.expect("checked source output");
    for op in [
        AsyncIoOp::FileReadBytes,
        AsyncIoOp::FileWriteString,
        AsyncIoOp::FileWriteBytes,
        AsyncIoOp::TcpRead,
        AsyncIoOp::TcpAccept,
    ] {
        assert!(
            output
                .direct_call_targets
                .values()
                .any(|target| *target == CallTarget::Runtime(RuntimeCallFamily::AsyncIo(op))),
            "missing {op:?}"
        );
    }
    for call in [
        "fs.read(path)",
        "fs.read_bytes(path)",
        "fs.write(path, \"text\")",
        "fs.write_bytes(path, bytes)",
        "listener.accept()",
        "conn.read()",
        "conn.try_read()",
        "conn.read_string()",
        "conn.try_read_string()",
    ] {
        let start = SOURCE.find(call).unwrap();
        assert!(
            output
                .suspension_effects
                .calls
                .iter()
                .any(|(key, effect)| key.module_idx == 0
                    && key.start == start
                    && *effect == SuspensionEffect::MaySuspend),
            "missing checked effect: {call}"
        );
    }
    for call in ["fs.read(path)", "conn.try_read_string()"] {
        let unawaited = SOURCE.replace(&format!("await {call}"), call);
        std::fs::write(&input, &unawaited).unwrap();
        let failure = run_file_frontend_to_typecheck(input.to_str().unwrap(), &options)
            .err()
            .expect("unawaited suspending wrapper must fail");
        let start = unawaited.find(call).unwrap();
        assert!(
            failure.diagnostics.iter().any(|diagnostic| {
                matches!(&diagnostic.kind, FrontendDiagnosticKind::Type(error)
                if error.message.contains("this call may suspend")
                    && error.span == (start..start + call.len())
                    && diagnostic.source.as_deref() == Some(unawaited.as_str()))
            }),
            "missing rejection at unawaited call: {failure:#?}"
        );
    }
}
