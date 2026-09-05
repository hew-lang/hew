//! Leak oracle for active-mode TCP `on_data(bytes)` delivery.
//!
//! The reactor constructs one refcount-1 `bytes` value for every socket read
//! and transfers its `BytesTriple` through the ordinary copy-mode actor mailbox.
//! The receive handler is the terminal owner unless it forwards the payload.
//! This test drives the generic TCP substrate directly (no MQTT framing):
//!
//! - zero deliveries are the allocation-noise control;
//! - fifty real connect/write/close deliveries must all reach `on_data`;
//! - both binaries must finish with exactly zero leaks under guard malloc.

#![cfg(unix)]

mod support;

use std::io::{Read, Write};
use std::net::{Shutdown, TcpListener, TcpStream};
use std::path::Path;
use std::process::{Child, Command, Stdio};
use std::thread;
use std::time::{Duration, Instant};

use support::leak_slope::{compile_to_native, parse_leaks_summary, require_leaks_tool};
use support::require_codegen;

const HIGH_DELIVERIES: usize = 50;
const PROCESS_TIMEOUT: Duration = Duration::from_secs(30);

struct ChildGuard(Child);

impl Drop for ChildGuard {
    fn drop(&mut self) {
        let _ = self.0.kill();
        let _ = self.0.wait();
    }
}

fn allocate_loopback_port() -> u16 {
    TcpListener::bind(("127.0.0.1", 0))
        .expect("bind loopback port")
        .local_addr()
        .expect("read loopback port")
        .port()
}

/// The handler does NOT hold the connection: `attach(consuming self, ..)`
/// transfers it to the reactor, which is then its sole owner and closes it.
/// Giving `ProbeSink` a `Connection` field as well would spawn-transfer the same
/// connection into actor state and close it a second time on `on_close` — the
/// exact double-close the sibling `active_transport_attach_ir` oracle pins as
/// refused. The measurement here is the per-delivery `bytes` payload, which is
/// unaffected by dropping the redundant field.
fn server_source(port: u16, deliveries: usize) -> String {
    format!(
        "import std.net.{{Connection, ConnectionHandler}};\n\
         \n\
         actor ProbeSink {{\n\
         \x20   receive fn on_data(data: bytes) {{\n\
         \x20       println(\"DATA\");\n\
         \x20   }}\n\
         \x20   receive fn on_close() {{}}\n\
         }}\n\
         \n\
         impl ConnectionHandler for ProbeSink {{\n\
         \x20   fn on_data(data: bytes) {{}}\n\
         \x20   fn on_close() {{}}\n\
         }}\n\
         \n\
         fn main() -> i64 {{\n\
         \x20   let listener = match net.listen(\"127.0.0.1:{port}\") {{ .Ok(value) => value, .Err(_) => panic(\"network setup failed\"), }};\n\
         \x20   var accepted: i64 = 0;\n\
         \x20   while accepted < {deliveries} {{\n\
         \x20       let conn = listener.accept();\n\
         \x20       let sink = spawn ProbeSink();\n\
         \x20       conn.attach(sink);\n\
         \x20       accepted = accepted + 1;\n\
         \x20   }}\n\
         \x20   sleep(1000ms);\n\
         \x20   println(\"DONE\");\n\
         \x20   0\n\
         }}\n"
    )
}

fn connect_and_deliver(port: u16, index: usize) {
    let deadline = Instant::now() + Duration::from_secs(5);
    let mut stream = loop {
        match TcpStream::connect(("127.0.0.1", port)) {
            Ok(stream) => break stream,
            Err(error) => {
                assert!(
                    Instant::now() < deadline,
                    "delivery {index}: active-mode server never listened on port {port}: {error}"
                );
                thread::sleep(Duration::from_millis(20));
            }
        }
    };
    stream
        .set_write_timeout(Some(Duration::from_secs(2)))
        .expect("set client write timeout");
    stream
        .write_all(b"active-mode-delivery")
        .unwrap_or_else(|error| panic!("delivery {index}: write payload: {error}"));
    stream
        .shutdown(Shutdown::Both)
        .unwrap_or_else(|error| panic!("delivery {index}: close client socket: {error}"));
}

fn run_under_leaks(bin: &Path, port: u16, deliveries: usize) -> (usize, usize) {
    let mut child = ChildGuard(
        Command::new("leaks")
            .args(["--atExit", "--"])
            .arg(bin)
            .env("MallocStackLogging", "1")
            .env("MallocScribble", "1")
            .env("MallocPreScribble", "1")
            .env("MallocGuardEdges", "1")
            .env("HEW_WORKERS", "2")
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .unwrap_or_else(|error| panic!("run {} under leaks(1): {error}", bin.display())),
    );

    for index in 0..deliveries {
        connect_and_deliver(port, index);
    }

    let deadline = Instant::now() + PROCESS_TIMEOUT;
    let status = loop {
        if let Some(status) = child.0.try_wait().expect("poll active-mode leak oracle") {
            break status;
        }
        assert!(
            Instant::now() < deadline,
            "active-mode leak oracle {} did not finish within {PROCESS_TIMEOUT:?}",
            bin.display()
        );
        thread::sleep(Duration::from_millis(20));
    };

    let mut stdout = String::new();
    child
        .0
        .stdout
        .take()
        .expect("active-mode leak stdout was captured")
        .read_to_string(&mut stdout)
        .expect("read active-mode leak stdout");
    let mut stderr = String::new();
    child
        .0
        .stderr
        .take()
        .expect("active-mode leak stderr was captured")
        .read_to_string(&mut stderr)
        .expect("read active-mode leak stderr");
    let report = format!("{stdout}\n{stderr}");

    assert_eq!(
        stdout.lines().filter(|line| *line == "DATA").count(),
        deliveries,
        "work witness: expected {deliveries} delivered on_data messages; report:\n{report}"
    );
    assert_eq!(
        stdout.lines().filter(|line| *line == "DONE").count(),
        1,
        "work witness: server did not reach its clean terminal sentinel:\n{report}"
    );
    let summary = parse_leaks_summary(&report)
        .unwrap_or_else(|| panic!("leaks(1) emitted no parseable summary:\n{report}"));
    assert!(
        status.success(),
        "active-mode server leaked or failed under guard malloc: status={status:?}\n{report}"
    );
    summary
}

const CONDITIONAL_HANDLER_BYTES_SOURCE: &str = r#"
actor ProbeSink {
    receive fn take(data: bytes) { println("FORWARDED"); }
}
actor Router {
    let sink: LocalPid<ProbeSink>,
    receive fn route(data: bytes, forward: bool) {
        if forward {
            sink.take(data);
        } else {
            let n = data.len();
            println(n as i64);
            println("LOCAL");
        }
    }
}
fn main() -> i64 {
    let sink = spawn ProbeSink();
    let router = spawn Router(sink: sink);
    router.route(b"forward-owner", true);
    router.route(b"local-owner", false);
    sleep(100ms);
    println("DONE");
    0
}
"#;

fn run_conditional_under_leaks(bin: &Path) -> (usize, usize) {
    let mut child = ChildGuard(
        Command::new("leaks")
            .args(["--atExit", "--"])
            .arg(bin)
            .env("MallocStackLogging", "1")
            .env("MallocScribble", "1")
            .env("MallocPreScribble", "1")
            .env("MallocGuardEdges", "1")
            .env("HEW_WORKERS", "2")
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .unwrap_or_else(|error| panic!("run {} under leaks(1): {error}", bin.display())),
    );
    let deadline = Instant::now() + PROCESS_TIMEOUT;
    let status = loop {
        if let Some(status) = child.0.try_wait().expect("poll conditional Bytes oracle") {
            break status;
        }
        assert!(
            Instant::now() < deadline,
            "conditional Bytes oracle {} did not finish within {PROCESS_TIMEOUT:?}",
            bin.display()
        );
        thread::sleep(Duration::from_millis(20));
    };
    let mut stdout = String::new();
    child
        .0
        .stdout
        .take()
        .expect("conditional Bytes stdout was captured")
        .read_to_string(&mut stdout)
        .expect("read conditional Bytes stdout");
    let mut stderr = String::new();
    child
        .0
        .stderr
        .take()
        .expect("conditional Bytes stderr was captured")
        .read_to_string(&mut stderr)
        .expect("read conditional Bytes stderr");
    let report = format!("{stdout}\n{stderr}");
    for witness in ["FORWARDED", "LOCAL", "DONE"] {
        assert_eq!(
            stdout.lines().filter(|line| *line == witness).count(),
            1,
            "conditional Bytes oracle did not execute `{witness}` exactly once:\n{report}"
        );
    }
    let summary = parse_leaks_summary(&report)
        .unwrap_or_else(|| panic!("leaks(1) emitted no parseable summary:\n{report}"));
    assert!(
        status.success(),
        "conditional Bytes handler leaked or failed under guard malloc: \
         status={status:?}\n{report}"
    );
    summary
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn active_mode_bytes_delivery_is_exactly_leak_free() {
    require_leaks_tool();
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("active-mode-bytes-delivery-")
        .tempdir()
        .expect("create active-mode leak tempdir");
    let low_port = allocate_loopback_port();
    let high_port = allocate_loopback_port();
    let low = compile_to_native(
        &server_source(low_port, 0),
        dir.path(),
        "active_mode_bytes_low",
    );
    let high = compile_to_native(
        &server_source(high_port, HIGH_DELIVERIES),
        dir.path(),
        "active_mode_bytes_high",
    );

    assert_eq!(
        run_under_leaks(&low, low_port, 0),
        (0, 0),
        "zero-delivery control must be leak-free"
    );
    assert_eq!(
        run_under_leaks(&high, high_port, HIGH_DELIVERIES),
        (0, 0),
        "every delivered active-mode bytes payload must be released"
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn conditional_handler_bytes_transfer_balances_both_paths() {
    require_leaks_tool();
    require_codegen();

    let dir = tempfile::Builder::new()
        .prefix("conditional-handler-bytes-")
        .tempdir()
        .expect("create conditional Bytes leak tempdir");
    let bin = compile_to_native(
        CONDITIONAL_HANDLER_BYTES_SOURCE,
        dir.path(),
        "conditional_handler_bytes",
    );
    assert_eq!(
        run_conditional_under_leaks(&bin),
        (0, 0),
        "the forwarded and locally-retained handler paths must each release exactly once"
    );
}
