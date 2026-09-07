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
    // A plain call suspends on its own; `await` on it is only a warning.
    let start = SOURCE.find("fs.read(path)").unwrap();
    assert!(
        state.diagnostics.iter().any(|diagnostic| {
            matches!(&diagnostic.kind, FrontendDiagnosticKind::Type(error)
            if error.severity == hew_types::error::Severity::Warning
                && error.message.contains("`await` on a plain call adds nothing")
                && error.span == (start..start + "fs.read(path)".len()))
        }),
        "missing redundant await warning: {:#?}",
        state.diagnostics
    );
    let plain = SOURCE.replace("await ", "");
    std::fs::write(&input, &plain).unwrap();
    let state = run_file_frontend_to_typecheck(input.to_str().unwrap(), &options)
        .unwrap_or_else(|error| panic!("{error:#?}"));
    let output = state.typecheck_result.tco.expect("checked source output");
    let start = plain.find("fs.read(path)").unwrap();
    assert!(output
        .suspension_effects
        .calls
        .iter()
        .any(|(key, effect)| key.module_idx == 0
            && key.start == start
            && *effect == SuspensionEffect::MaySuspend));
}
