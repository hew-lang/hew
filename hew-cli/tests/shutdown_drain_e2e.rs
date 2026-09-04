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

actor Handler {{
    let addr: string;

    receive fn run(unused: i64) {{
        let listener = net.listen(addr).unwrap();
        println("READY");
        let conn = listener.accept();
        listener.close();
        println("HANDLER_WAITING");
        let request = await conn.read_string();
        println("HANDLER_STARTED:" + request);
        let _ = conn.write_string("response:" + request);
        println("HANDLER_DONE");
    }}
}}

fn main() {{
    let handler = spawn Handler(addr: "127.0.0.1:{port}");
    handler.run(0);
    sleep(750ms);
}}
"#
    )
}

fn compile_fixture(source: &str, dir: &Path) -> PathBuf {
    let source_path = dir.join("shutdown_drain_tcp.hew");
    std::fs::write(&source_path, source).expect("write shutdown drain fixture");
    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-dir",
            dir.to_str().expect("emit directory is UTF-8"),
            source_path.to_str().expect("fixture path is UTF-8"),
        ])
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
fn tcp_handler_finishing_inside_budget_delivers_response_and_exits_zero() {
    require_codegen();
    let port = allocate_loopback_port();
    let dir = tempfile::tempdir().expect("create shutdown drain fixture directory");
    let binary = compile_fixture(&fixture_source(port), dir.path());
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
    let binary = compile_fixture(&fixture_source(port), dir.path());
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
