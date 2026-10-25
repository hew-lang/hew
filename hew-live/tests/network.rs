#![cfg(feature = "component")]

//! The editor protocol reaches ordinary authenticated Hew node services.

use std::fmt::Write as _;
use std::io::{BufRead, BufReader, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::sync::mpsc;
use std::time::Duration;

use serde_json::{json, Value};
use sha2::{Digest, Sha256};

fn environment(name: &str) -> PathBuf {
    std::env::var_os(name).map_or_else(
        || panic!("{name} must be set by make test-live-network"),
        PathBuf::from,
    )
}

fn runtime() -> PathBuf {
    environment("HEW_LIVE_RUNTIME_DIR").join(format!(
        "{}hew{}",
        std::env::consts::DLL_PREFIX,
        std::env::consts::DLL_SUFFIX
    ))
}

fn run(command: &mut Command) -> String {
    let output = command.output().expect("run SDK command");
    assert!(
        output.status.success(),
        "{command:?}: {}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout).unwrap().trim().to_owned()
}

fn companion() -> Command {
    Command::new(env!("CARGO_BIN_EXE_hew-live-node"))
}

struct Process(Child);
impl Drop for Process {
    fn drop(&mut self) {
        let _ = self.0.kill();
        let _ = self.0.wait();
    }
}

impl Process {
    fn finish(mut self) {
        drop(self.0.stdin.take());
        let deadline = std::time::Instant::now() + Duration::from_secs(20);
        loop {
            if let Some(status) = self.0.try_wait().unwrap() {
                assert!(status.success(), "{status}");
                break;
            }
            assert!(
                std::time::Instant::now() < deadline,
                "node shutdown timed out"
            );
            std::thread::sleep(Duration::from_millis(10));
        }
    }
}

struct Editor {
    process: Process,
    replies: mpsc::Receiver<String>,
}
impl Editor {
    fn request(&mut self, mut request: Value, success: bool) -> Value {
        request["id"] = json!(42);
        writeln!(self.process.0.stdin.as_mut().unwrap(), "{request}").unwrap();
        self.process.0.stdin.as_mut().unwrap().flush().unwrap();
        let line = self
            .replies
            .recv_timeout(Duration::from_secs(90))
            .expect("editor reply");
        let reply: Value =
            serde_json::from_str(&line).expect("stdout must contain only JSON replies");
        assert_eq!(reply["id"], 42);
        assert_eq!(reply["ok"], success, "{reply}");
        if success {
            reply["result"].clone()
        } else {
            reply["error"].clone()
        }
    }
}

fn peer(compiler: &Path, directory: &Path, name: &str) -> String {
    let credential = run(Command::new(compiler)
        .args(["node", "identity", "--transport", "quic-mesh", "--key"])
        .arg(directory.join(format!("{name}.key"))));
    run(Command::new(compiler)
        .args(["node", "admit", "--transport", "quic-mesh", "--authority"])
        .arg(directory.join("authority.key"))
        .arg("--credential")
        .arg(&credential)
        .arg("--output")
        .arg(directory.join(format!("{name}.enrollment"))));
    let bytes: Vec<_> = credential
        .as_bytes()
        .chunks_exact(2)
        .map(|pair| u8::from_str_radix(std::str::from_utf8(pair).unwrap(), 16).unwrap())
        .collect();
    // The CLI exposes the credential; derive its documented authenticated ID.
    let mut hash = Sha256::new();
    hash.update(b"hew-node-id-v1\0");
    hash.update([2]);
    hash.update(u32::try_from(bytes.len()).unwrap().to_be_bytes());
    hash.update(bytes);
    let mut identity = String::with_capacity(32);
    for byte in &hash.finalize()[..16] {
        write!(identity, "{byte:02x}").unwrap();
    }
    identity
}

fn node(directory: &Path, module: &Path, name: &str, address: &str) -> Command {
    let mut command = companion();
    command
        .env("HEW_NODE_BIND", address)
        .env("HEW_NODE_TRANSPORT", "quic-mesh")
        .env("HEW_NODE_KEY", directory.join(format!("{name}.key")))
        .env(
            "HEW_NODE_CLUSTER",
            directory.join(format!("{name}.enrollment")),
        )
        .env("HEW_NODE_SEEDS", "[]")
        .env("HEW_LIVE_NODE_MODULE", module)
        .env("HEW_LIVE_RUNTIME", runtime())
        .stdin(Stdio::piped());
    command
}

fn server(directory: &Path, module: &Path, client_id: &str, address: &str) -> Process {
    let mut process = Process(
        node(directory, module, "server", address)
            .args(["serve", "--allow-peer", client_id, "--cache"])
            .arg(directory.join("cache"))
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .spawn()
            .unwrap(),
    );
    let stderr = process.0.stderr.take().unwrap();
    let (send, receive) = mpsc::channel();
    std::thread::spawn(move || {
        for line in BufReader::new(stderr).lines() {
            let line = line.unwrap();
            eprintln!("{line}");
            let _ = send.send(line);
        }
    });
    let line = receive
        .recv_timeout(Duration::from_secs(15))
        .expect("server startup");
    assert!(line.starts_with("node started;"), "{line}");
    process
}

fn editor(directory: &Path, module: &Path, server_id: &str, address: &str) -> Editor {
    let mut process = Process(
        node(directory, module, "client", "127.0.0.1:0")
            .args(["attach", "--endpoint", address, "--node", server_id])
            .stdout(Stdio::piped())
            .stderr(Stdio::inherit())
            .spawn()
            .unwrap(),
    );
    let stdout = process.0.stdout.take().unwrap();
    let (send, replies) = mpsc::channel();
    std::thread::spawn(move || {
        for line in BufReader::new(stdout).lines() {
            if send.send(line.unwrap()).is_err() {
                break;
            }
        }
    });
    Editor { process, replies }
}

#[test]
fn authenticated_source_replacement_retains_state_and_recovers_connection() {
    let directory = tempfile::tempdir().unwrap();
    let root = directory.path();
    let compiler = environment("HEW_LIVE_COMPILER");
    let module = root.join("transport");
    run(companion()
        .arg("prepare-transport")
        .arg("--compiler")
        .arg(&compiler)
        .arg("--runtime")
        .arg(runtime())
        .arg("--output")
        .arg(&module));
    run(Command::new(&compiler)
        .args(["node", "authority", "--key"])
        .arg(root.join("authority.key")));
    let server_id = peer(&compiler, root, "server");
    let client_id = peer(&compiler, root, "client");
    let socket = std::net::UdpSocket::bind("127.0.0.1:0").unwrap();
    let address = socket.local_addr().unwrap().to_string();
    drop(socket);
    let service = server(root, &module, &client_id, &address);
    let mut client = editor(root, &module, &server_id, &address);
    assert_eq!(client.request(json!({"op":"status"}), true)["epoch"], 0);
    let source = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/router.hew");
    let digest =
        client.request(json!({"op":"build_prepare", "source":source}), true)["digest"].clone();
    assert_eq!(client.request(json!({"op":"status"}), true)["epoch"], 0);
    assert_eq!(
        client.request(
            json!({"op":"replace", "digest":digest, "predecessor":0, "within_ms":10000}),
            true
        )["epoch"],
        1
    );
    assert_eq!(
        client.request(
            json!({"op":"route", "source":"inbox", "body":"before"}),
            true
        )["accepted"],
        true
    );
    client.request(
        json!({"op":"replace", "digest":digest, "predecessor":0, "within_ms":10000}),
        false,
    );
    assert_eq!(client.request(json!({"op":"status"}), true)["epoch"], 1);
    let replacement = client.request(
        json!({"op":"replace", "digest":digest, "predecessor":1, "within_ms":10000}),
        true,
    );
    assert_eq!(replacement["epoch"], 2);
    assert_eq!(replacement["accepted"], 1);
    client.process.finish();
    let mut client = editor(root, &module, &server_id, &address);
    assert_eq!(client.request(json!({"op":"status"}), true)["epoch"], 2);
    assert_eq!(
        client.request(
            json!({"op":"route", "source":"inbox", "body":"after reconnect"}),
            true
        )["accepted"],
        true
    );
    client.process.finish();
    service.finish();
    // The same immutable bytes remain usable after the host process restarts.
    let service = server(root, &module, &client_id, &address);
    let mut client = editor(root, &module, &server_id, &address);
    assert_eq!(
        client.request(
            json!({"op":"replace", "digest":digest, "predecessor":0, "within_ms":10000}),
            true
        )["epoch"],
        1
    );
    client.process.finish();
    service.finish();
    // A grant watcher with no connected peer must also stop cleanly.
    server(root, &module, &client_id, &address).finish();
}
