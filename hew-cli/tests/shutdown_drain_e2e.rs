#![cfg(unix)]

mod support;

use std::io::{BufRead, BufReader, Read, Write};
use std::net::{Shutdown, TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, ExitStatus, Stdio};
use std::sync::mpsc;
use std::thread;
use std::time::{Duration, Instant};

use support::{describe_output, hew_binary, repo_root, require_codegen};

const PROCESS_TIMEOUT: Duration = Duration::from_secs(12);

struct ChildGuard(Child);

impl Drop for ChildGuard {
    fn drop(&mut self) {
        let _ = self.0.kill();
        let _ = self.0.wait();
    }
}

fn allocate_loopback_port() -> u16 {
    TcpListener::bind(("127.0.0.1", 0))
        .expect("bind ephemeral loopback listener")
        .local_addr()
        .expect("read loopback listener address")
        .port()
}

fn fixture_source(port: u16) -> String {
    format!(
        r#"
import std.net;
import std.encoding.utf8;

actor Handler {{
    let addr: string,

    receive fn run(unused: i64) {{
        let listener = match net.listen(addr) {{ .Ok(value) => value, .Err(error) => panic("network operation failed"), }};
        println("READY");
        let conn = listener.accept();
        listener.close();
        println("HANDLER_WAITING");
        let data = match conn.recv() {{ .Some(data) => data, .None => panic("connection closed"), }};
        let request = utf8.decode(data) handle error {{
            panic("invalid utf8");
        }};
        println("HANDLER_STARTED:" + request);
        let _ = conn.send(("response:" + request).to_bytes());
        println("HANDLER_DONE");
    }}
}}

fn main() {{
    let handler = spawn Handler(addr: "127.0.0.1:{port}");
    let _ = mailbox(handler, on_full: .Wait).run(0);
    sleep(750ms);
}}
"#
    )
}

fn compile_fixture(source: &str, dir: &Path, opt_level: u8) -> PathBuf {
    let source_path = dir.join("shutdown_drain_tcp.hew");
    std::fs::write(&source_path, source).expect("write shutdown drain fixture");
    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-dir",
            dir.to_str().expect("emit directory is UTF-8"),
            source_path.to_str().expect("fixture path is UTF-8"),
        ])
        .arg("--opt-level")
        .arg(opt_level.to_string())
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile");
    assert!(
        output.status.success(),
        "compiling shutdown drain fixture failed\n{}",
        describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    stdout
        .lines()
        .find_map(|line| line.strip_prefix("native: "))
        .map_or_else(
            || panic!("compiler reported no native artifact:\n{stdout}"),
            PathBuf::from,
        )
}

struct RunningFixture {
    child: ChildGuard,
    lines: mpsc::Receiver<String>,
    stdout_thread: thread::JoinHandle<Vec<String>>,
    stderr_thread: thread::JoinHandle<String>,
}

fn spawn_fixture(binary: &Path) -> RunningFixture {
    let mut child = Command::new(binary)
        .env("HEW_WORKERS", "2")
        .env("HEW_ACTOR_LEAK_CHECK", "1")
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn shutdown drain fixture");
    let stdout = child.stdout.take().expect("capture fixture stdout");
    let stderr = child.stderr.take().expect("capture fixture stderr");
    let (line_tx, line_rx) = mpsc::channel();
    let stdout_thread = thread::spawn(move || {
        let mut captured = Vec::new();
        for line in BufReader::new(stdout).lines() {
            let line = line.expect("read fixture stdout line");
            let _ = line_tx.send(line.clone());
            captured.push(line);
        }
        captured
    });
    let stderr_thread = thread::spawn(move || {
        let mut captured = String::new();
        BufReader::new(stderr)
            .read_to_string(&mut captured)
            .expect("read fixture stderr");
        captured
    });
    RunningFixture {
        child: ChildGuard(child),
        lines: line_rx,
        stdout_thread,
        stderr_thread,
    }
}

fn wait_for_line(lines: &mpsc::Receiver<String>, prefix: &str) -> String {
    let deadline = Instant::now() + PROCESS_TIMEOUT;
    loop {
        let remaining = deadline.saturating_duration_since(Instant::now());
        assert!(!remaining.is_zero(), "fixture never printed {prefix}");
        let line = lines
            .recv_timeout(remaining)
            .unwrap_or_else(|error| panic!("fixture never printed {prefix}: {error}"));
        if line.starts_with(prefix) {
            return line;
        }
    }
}

fn wait_for_exit(child: &mut ChildGuard) -> ExitStatus {
    let deadline = Instant::now() + PROCESS_TIMEOUT;
    loop {
        if let Some(status) = child.0.try_wait().expect("poll fixture process") {
            return status;
        }
        assert!(
            Instant::now() < deadline,
            "fixture did not exit within {PROCESS_TIMEOUT:?}"
        );
        thread::sleep(Duration::from_millis(10));
    }
}

fn finish_fixture(mut fixture: RunningFixture) -> (ExitStatus, Vec<String>, String) {
    let status = wait_for_exit(&mut fixture.child);
    let stdout = fixture
        .stdout_thread
        .join()
        .expect("join fixture stdout reader");
    let stderr = fixture
        .stderr_thread
        .join()
        .expect("join fixture stderr reader");
    (status, stdout, stderr)
}

#[test]
fn signal_shutdown_stops_root_admission_and_drains_accepted_peer_calls() {
    require_codegen();
    let source = r#"
#[resource]
type Ticket { id: i64 }
impl Ticket {
    fn close(consume self) { println(f"CLOSED:{self.id}"); }
}
actor Peer {
    receive fn answer() -> i64 { 42 }
}
actor Service {
    var held: Ticket,
    receive fn poll(peer: Peer) -> i64 {
        println("REQUEST_ACCEPTED");
        sleep(200ms);
        let value = peer.answer().expect("accepted handler must finish its peer call");
        println("PEER_REPLY");
        value
    }
}
fn main() {
    let root = Ticket { id: 1 };
    let peer = spawn Peer;
    let service = spawn Service(held: Ticket { id: 2 });
    loop {
        match service.poll(peer) {
            .Ok(value) => assert(value == 42),
            .Err(_) => break,
        }
    }
    println("ROOT_DONE");
}
"#;
    for opt_level in [0, 2] {
        let dir = tempfile::tempdir().expect("create signal shutdown fixture directory");
        let binary = compile_fixture(source, dir.path(), opt_level);
        let fixture = spawn_fixture(&binary);
        wait_for_line(&fixture.lines, "REQUEST_ACCEPTED");
        let pid = i32::try_from(fixture.child.0.id()).expect("child PID fits pid_t");
        // SAFETY: this live child belongs to the test; SIGTERM invokes its runtime
        // shutdown handler while the accepted service turn is suspended.
        assert_eq!(unsafe { libc::kill(pid, libc::SIGTERM) }, 0);
        let (status, stdout, stderr) = finish_fixture(fixture);
        assert!(status.success(), "shutdown exited {status}: {stderr}");
        assert!(stdout.iter().any(|line| line == "PEER_REPLY"), "{stdout:?}");
        for event in ["ROOT_DONE", "CLOSED:1", "CLOSED:2"] {
            assert_eq!(
                stdout.iter().filter(|line| *line == event).count(),
                1,
                "{stdout:?}"
            );
        }
        assert!(stderr.is_empty(), "clean shutdown diagnostics: {stderr}");
    }
}

#[test]
fn tcp_handler_finishing_inside_budget_delivers_response_and_exits_zero() {
    require_codegen();
    let port = allocate_loopback_port();
    let dir = tempfile::tempdir().expect("create shutdown drain fixture directory");
    let binary = compile_fixture(&fixture_source(port), dir.path(), 0);
    let fixture = spawn_fixture(&binary);

    wait_for_line(&fixture.lines, "READY");
    let mut client = TcpStream::connect(("127.0.0.1", port)).expect("connect fixture client");
    client
        .set_read_timeout(Some(PROCESS_TIMEOUT))
        .expect("set client read timeout");
    wait_for_line(&fixture.lines, "HANDLER_WAITING");

    thread::sleep(Duration::from_millis(1_250));
    client
        .write_all(b"request")
        .expect("finish request inside drain budget");
    client
        .shutdown(Shutdown::Write)
        .expect("close client write half");

    let mut response = String::new();
    client
        .read_to_string(&mut response)
        .expect("read drained handler response");
    let (status, stdout, stderr) = finish_fixture(fixture);

    assert!(
        status.success(),
        "drained fixture exited {status}: {stderr}"
    );
    assert_eq!(
        response, "response:request",
        "drained fixture stdout={stdout:?} stderr={stderr:?}"
    );
    assert_eq!(
        stdout.iter().filter(|line| *line == "HANDLER_DONE").count(),
        1,
        "drained handler must complete exactly once: {stdout:?}"
    );
    assert!(
        !stderr.contains("abandoning in-flight work"),
        "clean drain must not report abandonment: {stderr}"
    );
}

#[test]
fn tcp_handler_exceeding_budget_is_observable_as_nonzero_exit() {
    require_codegen();
    let port = allocate_loopback_port();
    let dir = tempfile::tempdir().expect("create shutdown drain fixture directory");
    let binary = compile_fixture(&fixture_source(port), dir.path(), 0);
    let fixture = spawn_fixture(&binary);

    wait_for_line(&fixture.lines, "READY");
    let mut client = TcpStream::connect(("127.0.0.1", port)).expect("connect fixture client");
    client
        .set_read_timeout(Some(PROCESS_TIMEOUT))
        .expect("set client read timeout");
    wait_for_line(&fixture.lines, "HANDLER_WAITING");
    let started = Instant::now();

    let (status, stdout, stderr) = finish_fixture(fixture);
    let elapsed = started.elapsed();
    let mut response = String::new();
    client
        .read_to_string(&mut response)
        .expect("process exit must close the abandoned connection");

    assert_eq!(
        status.code(),
        Some(1),
        "abandoned fixture stdout={stdout:?} stderr={stderr:?}"
    );
    assert!(
        elapsed >= Duration::from_secs(5) && elapsed < Duration::from_secs(8),
        "abandoned handler must consume one bounded drain window, took {elapsed:?}"
    );
    assert!(
        stderr.contains("shutdown drain timed out after 5s; abandoning in-flight work"),
        "abandonment must be reported explicitly:\n{stderr}"
    );
    assert!(
        !stdout.iter().any(|line| line == "HANDLER_DONE"),
        "handler must not fabricate completion after abandonment: {stdout:?}"
    );
    assert!(
        response.is_empty(),
        "abandoned handler must not fabricate a response: {response:?}"
    );
}

/// An unsupervised root actor blocked in `accept()` is not a fault when
/// SIGTERM cancels it: a task cancelled by a requested shutdown is expected
/// termination, and the process must exit success.
#[test]
fn unsupervised_root_actor_blocked_in_accept_exits_zero_on_sigterm() {
    require_codegen();
    let port = allocate_loopback_port();
    let source = format!(
        r#"
import std.net;

actor Acceptor {{
    let addr: string,
    receive fn start() {{
        let listener = match net.listen(addr) {{ .Ok(value) => value, .Err(_) => panic("listen failed") }};
        println("READY");
        let _conn = listener.accept();
        println("UNREACHABLE");
    }}
}}

fn main() {{
    let acceptor = spawn Acceptor(addr: "127.0.0.1:{port}");
    // The accept loop never returns on its own; this call keeps main blocked
    // (and the process alive) until shutdown cancels it.
    let _ = acceptor.start();
}}
"#
    );
    for opt_level in [0, 2] {
        let dir = tempfile::tempdir().expect("create accept-cancel fixture directory");
        let binary = compile_fixture(&source, dir.path(), opt_level);
        let mut fixture = spawn_fixture(&binary);
        wait_for_line(&fixture.lines, "READY");
        let pid = i32::try_from(fixture.child.0.id()).expect("child PID fits pid_t");
        // SAFETY: this live child belongs to the test; SIGTERM invokes its
        // runtime shutdown handler while `accept()` is parked with no peer.
        assert_eq!(unsafe { libc::kill(pid, libc::SIGTERM) }, 0);
        let status = wait_for_exit(&mut fixture.child);
        let stdout = fixture
            .stdout_thread
            .join()
            .expect("join fixture stdout reader");
        let stderr = fixture
            .stderr_thread
            .join()
            .expect("join fixture stderr reader");
        assert!(
            status.success(),
            "a shutdown-cancelled accept() must exit success: {status}\nstdout={stdout:?}\nstderr={stderr}"
        );
        assert!(
            !stdout.iter().any(|line| line == "UNREACHABLE"),
            "accept() must not fabricate a connection: {stdout:?}"
        );
        assert!(
            !stderr.contains("actor crash"),
            "a requested-shutdown cancellation is not a crash: {stderr}"
        );
    }
}

/// Negative control for the above: an actor panic unrelated to shutdown is
/// still a real fault and must still fail the process.
#[test]
fn unsupervised_root_actor_panic_is_nonzero_exit() {
    require_codegen();
    let source = r#"
actor Boom {
    receive fn detonate() -> i64 {
        panic("boom");
    }
}

fn main() {
    let b = spawn Boom;
    let _ = b.detonate();
}
"#;
    for opt_level in [0, 2] {
        let dir = tempfile::tempdir().expect("create panic fixture directory");
        let binary = compile_fixture(source, dir.path(), opt_level);
        let fixture = spawn_fixture(&binary);
        let (status, stdout, stderr) = finish_fixture(fixture);
        assert_eq!(
            status.code(),
            Some(1),
            "a real actor panic must still fail the process: stdout={stdout:?} stderr={stderr}"
        );
        assert!(
            stderr.contains("actor crash in Boom.detonate"),
            "a real fault must still be reported: {stderr}"
        );
    }
}
