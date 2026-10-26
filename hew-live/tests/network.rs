#![cfg(feature = "component")]

//! The editor protocol reaches ordinary authenticated Hew node services.

use std::fmt::Write as _;
use std::io::{BufRead, BufReader, Read, Write};
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

fn transport(directory: &Path, compiler: &Path) -> PathBuf {
    let module = directory.join("transport");
    run(companion()
        .arg("prepare-transport")
        .arg("--compiler")
        .arg(compiler)
        .arg("--runtime")
        .arg(runtime())
        .arg("--output")
        .arg(&module));
    module
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
    observed_server(directory, module, client_id, address, None)
}

fn observed_server(
    directory: &Path,
    module: &Path,
    client_id: &str,
    address: &str,
    observe: Option<&str>,
) -> Process {
    let mut command = node(directory, module, "server", address);
    command.env_remove("HEW_PPROF");
    if let Some(observe) = observe {
        command.env("HEW_PPROF", observe);
    }
    command
        .args(["serve", "--allow-peer", client_id, "--cache"])
        .arg(directory.join("cache"));
    spawn_server(&mut command).0
}

fn spawn_server(command: &mut Command) -> (Process, mpsc::Receiver<String>) {
    let mut process = Process(
        command
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
    loop {
        let line = receive
            .recv_timeout(Duration::from_secs(15))
            .expect("server startup");
        if line.starts_with("node started;") {
            break;
        }
    }
    (process, receive)
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

fn observe_events(address: &str) -> Vec<Value> {
    let mut events = Vec::new();
    let mut target = "/api/traces?limit=256".to_owned();
    for _ in 0..64 {
        let mut stream = std::net::TcpStream::connect(address).unwrap();
        stream
            .set_read_timeout(Some(Duration::from_secs(5)))
            .unwrap();
        write!(
            stream,
            "GET {target} HTTP/1.1\r\nHost: {address}\r\nConnection: close\r\n\r\n"
        )
        .unwrap();
        let mut response = String::new();
        stream.read_to_string(&mut response).unwrap();
        assert!(response.starts_with("HTTP/1.1 200"), "{response}");
        let body: Value = serde_json::from_str(response.split_once("\r\n\r\n").unwrap().1).unwrap();
        let batch = &body["data"];
        events.extend(batch["events"].as_array().unwrap().iter().cloned());
        if !batch["has_more"].as_bool().unwrap() {
            return events;
        }
        target = format!(
            "/api/traces?limit=256&stream_id={}&after={}",
            batch["stream_id"].as_str().unwrap(),
            batch["next_cursor"].as_str().unwrap()
        );
    }
    panic!("Observe trace stream did not reach its tail");
}

fn verify_preview(client: &mut Editor, active_digest: &Value, observe: &str) {
    let source = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/preview_router.hew");
    let digest =
        client.request(json!({"op":"build_prepare", "source":source}), true)["digest"].clone();
    let mut preview = json!({"state":{"accepted":41}, "input":{"source":"private-preview-source", "body":"private-preview-body"}, "fixtures":[
        {"Destination":{"source":"private-preview-source", "result":{"Ok":"private-preview-destination"}}},
        {"Emit":{"message":{"destination":"private-preview-destination", "body":"private-preview-body"}, "result":{"Ok":null}}}
    ]});
    let result = client.request(
        json!({"op":"preview", "digest":digest, "preview":preview, "within_ms":10000}),
        true,
    );
    assert_eq!(result["accepted"], true);
    assert_eq!(result["cleanup_complete"], true);
    assert_eq!(result["used_fixtures"], 2);
    assert!(result["needs_input"].is_null());
    preview["fixtures"] = json!([]);
    let missing = client.request(
        json!({"op":"preview", "digest":digest, "preview":preview, "within_ms":10000}),
        true,
    );
    assert!(missing["accepted"].is_null());
    assert!(missing["needs_input"].is_string());
    assert_eq!(missing["cleanup_complete"], false);
    let status = client.request(json!({"op":"status"}), true);
    assert_eq!(status["epoch"], 2);
    assert_eq!(&status["digest"], active_digest);
    let events = observe_events(observe);
    let encoded = serde_json::to_string(&events).unwrap();
    assert!(!encoded.contains("private-preview-body"));
    assert!(!encoded.contains("private-preview-source"));
    let metadata: Vec<_> = events
        .iter()
        .filter_map(|event| event.get("live_execution"))
        .filter(|value| !value.is_null())
        .collect();
    assert!(
        metadata.iter().any(|event| event["mode"] == "preview"
            && event["phase"] == "completed"
            && event["code_id"] == digest
            && event["coverage"] == "explicit_services"
            && !event["invocation_id"].is_null()),
        "{metadata:?}"
    );
    assert!(
        metadata.iter().any(|event| event["mode"] == "production"
            && event["phase"] == "completed"
            && &event["code_id"] == active_digest
            && event["coverage"] == "metadata_only"),
        "{metadata:?}"
    );
}

#[test]
fn authenticated_source_replacement_retains_state_and_recovers_connection() {
    let directory = tempfile::tempdir().unwrap();
    let root = directory.path();
    let compiler = environment("HEW_LIVE_COMPILER");
    let module = transport(root, &compiler);
    run(Command::new(&compiler)
        .args(["node", "authority", "--key"])
        .arg(root.join("authority.key")));
    let server_id = peer(&compiler, root, "server");
    let client_id = peer(&compiler, root, "client");
    let socket = std::net::UdpSocket::bind("127.0.0.1:0").unwrap();
    let address = socket.local_addr().unwrap().to_string();
    drop(socket);
    let observe_listener = std::net::TcpListener::bind("127.0.0.1:0").unwrap();
    let observe = observe_listener.local_addr().unwrap().to_string();
    drop(observe_listener);
    let service = observed_server(root, &module, &client_id, &address, Some(&observe));
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
    verify_preview(&mut client, &digest, &observe);
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

fn expect_delivery(events: &mpsc::Receiver<String>, destination: &str, body: &str) {
    loop {
        let line = events
            .recv_timeout(Duration::from_secs(10))
            .expect("destination delivery");
        if let Ok(event) = serde_json::from_str::<Value>(&line) {
            if event["event"] == "delivery" {
                assert_eq!(event["destination"], destination);
                assert_eq!(event["body"], body);
                break;
            }
        }
    }
}

fn install(client: &mut Editor, component: &Path) -> Value {
    let digest =
        client.request(json!({"op":"prepare", "component":component}), true)["digest"].clone();
    assert_eq!(
        client.request(
            json!({"op":"replace", "digest":digest, "predecessor":0, "within_ms":10000}),
            true
        )["epoch"],
        1
    );
    digest
}

fn deliver(client: &mut Editor, events: &mpsc::Receiver<String>, destination: &str, body: &str) {
    assert_eq!(
        client.request(json!({"op":"route", "source":"inbox", "body":body}), true)["accepted"],
        true
    );
    expect_delivery(events, destination, body);
}

#[test]
fn one_portable_source_uses_two_nodes_destination_grants() {
    let directory = tempfile::tempdir().unwrap();
    let root = directory.path();
    let compiler = environment("HEW_LIVE_COMPILER");
    let module = transport(root, &compiler);
    run(Command::new(&compiler)
        .args(["node", "authority", "--key"])
        .arg(root.join("authority.key")));
    let west_id = peer(&compiler, root, "west");
    let east_id = peer(&compiler, root, "east");
    let client_id = peer(&compiler, root, "client");
    let west_socket = std::net::UdpSocket::bind("127.0.0.1:0").unwrap();
    let east_socket = std::net::UdpSocket::bind("127.0.0.1:0").unwrap();
    let west_address = west_socket.local_addr().unwrap().to_string();
    let east_address = east_socket.local_addr().unwrap().to_string();
    drop((west_socket, east_socket));
    let component = root.join("router.wasm");
    run(companion()
        .arg("build")
        .arg("--source")
        .arg(Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/preview_router.hew"))
        .arg("--output")
        .arg(&component));
    let serve = |name: &str, address: &str, prefix: &str| {
        spawn_server(
            node(root, &module, name, address)
                .env_remove("HEW_PPROF")
                .args([
                    "serve",
                    "--allow-peer",
                    &client_id,
                    "--destination-prefix",
                    prefix,
                    "--cache",
                ])
                .arg(root.join(format!("{name}-cache"))),
        )
    };
    let (west, west_events) = serve("west", &west_address, "west/");
    let (east, east_events) = serve("east", &east_address, "east/");
    let mut client = editor(root, &module, &west_id, &west_address);
    let digest = install(&mut client, &component);
    deliver(&mut client, &west_events, "west/inbox", "same source");
    client.process.finish();
    let mut client = editor(root, &module, &east_id, &east_address);
    assert_eq!(install(&mut client, &component), digest);
    deliver(&mut client, &east_events, "east/inbox", "same source");
    client.process.finish();
    // Installing the same candidate on the second node does not rebind the first.
    let mut client = editor(root, &module, &west_id, &west_address);
    assert_eq!(client.request(json!({"op":"status"}), true)["epoch"], 1);
    deliver(&mut client, &west_events, "west/inbox", "still west");
    client.process.finish();
    west.finish();
    east.finish();
}
