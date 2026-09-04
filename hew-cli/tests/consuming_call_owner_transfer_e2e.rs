#![cfg(unix)]

mod support;

use std::path::PathBuf;
use std::process::{Command, Output};

use support::{describe_output, hew_binary, repo_root, require_codegen, run_bounded_command};

const STREAM_PAIR_SOURCE: &str = r#"
import std.stream;

fn main() {
    let (text_sink, text_input) = stream.pipe(2).unwrap();
    text_sink.send("text-frame");
    text_sink.close();
    match text_input.try_recv() {
        .Some(frame) => {
            if frame != "text-frame" {
                panic("stream text payload changed");
            }
        },
        .None => panic("stream text frame missing"),
    }

    let (bytes_sink, bytes_input) = stream.bytes_pipe(2).unwrap();
    bytes_sink.send(b"ok");
    bytes_sink.close();
    match bytes_input.try_recv() {
        .Some(frame) => {
            if frame.len() != 2 {
                panic("stream bytes payload changed");
            }
        },
        .None => panic("stream bytes frame missing"),
    }
    println("stream-owner-transfer-ok");
}
"#;

const CHANNEL_PAIR_SOURCE: &str = r#"
import std.channel.channel;

fn main() {
    let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = channel.new(1).unwrap();
    tx.send("channel-frame");
    tx.close();
    match rx.try_recv() {
        .Some(frame) => {
            if frame != "channel-frame" {
                panic("channel payload changed");
            }
        },
        .None => panic("channel frame missing"),
    }
    println("channel-owner-transfer-ok");
}
"#;

const TCP_SPLIT_SOURCE: &str = r#"
import std.net;
import std.stream.{Sink, Stream};

fn main() {
    let listener = net.listen("127.0.0.1:0").unwrap();
    let port = listener.local_port();
    let peer = net.connect(f"127.0.0.1:{port as i64}").unwrap();
    let server = listener.accept();
    let (input, sink): (Stream<bytes>, Sink<bytes>) = server.into_stream_sink();
    sink.close();
    input.close();
    peer.close();
    listener.close();
    println("tcp-split-owner-transfer-ok");
}
"#;

fn selected_hew_binary() -> PathBuf {
    std::env::var_os("HEW_BIN").map_or_else(hew_binary, PathBuf::from)
}

fn run_case(name: &str, source: &str) -> Output {
    let dir = tempfile::Builder::new()
        .prefix(&format!("consuming-owner-transfer-{name}-"))
        .tempdir()
        .expect("create owner-transfer test directory");
    let path = dir.path().join(format!("{name}.hew"));
    std::fs::write(&path, source).expect("write owner-transfer Hew source");
    let mut command = Command::new(selected_hew_binary());
    command.arg("run").arg(&path).current_dir(repo_root());
    run_bounded_command(command, format!("hew run {}", path.display()))
}

#[test]
fn consumed_pair_carriers_run_to_clean_exit_with_sentinels() {
    require_codegen();

    let cases = [
        (
            "stream-pair",
            STREAM_PAIR_SOURCE,
            "stream-owner-transfer-ok",
        ),
        (
            "channel-pair",
            CHANNEL_PAIR_SOURCE,
            "channel-owner-transfer-ok",
        ),
        ("tcp-split", TCP_SPLIT_SOURCE, "tcp-split-owner-transfer-ok"),
    ];
    let outputs: Vec<_> = cases
        .iter()
        .map(|(name, source, sentinel)| (*name, *sentinel, run_case(name, source)))
        .collect();

    let mut failures = Vec::new();
    for (name, sentinel, output) in outputs {
        if !output.status.success() {
            failures.push(format!(
                "{name} must exit in the success class; status: {}:\n{}",
                output.status,
                describe_output(&output)
            ));
            continue;
        }
        let stdout = String::from_utf8(output.stdout).expect("Hew stdout must be UTF-8");
        if !stdout.lines().any(|line| line == sentinel) {
            failures.push(format!(
                "{name} must print sentinel `{sentinel}`; stdout:\n{stdout}"
            ));
        }
    }
    assert!(
        failures.is_empty(),
        "consuming-call owner-transfer runtime failures:\n\n{}",
        failures.join("\n\n")
    );
}
