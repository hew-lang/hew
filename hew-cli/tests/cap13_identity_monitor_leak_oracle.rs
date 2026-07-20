//! CAP-13 cross-process monitor lifecycle leak and poisoned-allocator oracle.

#![cfg(unix)]

mod support;

use std::io::{BufRead, BufReader, Read};
use std::net::TcpListener;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Output, Stdio};
use std::thread;
use std::time::{Duration, Instant};

use support::leak_slope::leaks_supported;
use support::{describe_output, hew_binary, repo_root, require_codegen};

const PROCESS_TIMEOUT: Duration = Duration::from_secs(45);
const LOW_ITERATIONS: usize = 3;
const HIGH_ITERATIONS: usize = 50;

struct ChildGuard(Child);

impl Drop for ChildGuard {
    fn drop(&mut self) {
        let _ = self.0.kill();
        let _ = self.0.wait();
    }
}

fn compile_fixture(dir: &Path) -> PathBuf {
    let source = repo_root().join("hew-cli/tests/fixtures/distributed/dist_node.hew");
    let output = Command::new(hew_binary())
        .args([
            "compile",
            "--emit-dir",
            dir.to_str().expect("emit directory is UTF-8"),
            source.to_str().expect("fixture path is UTF-8"),
        ])
        .current_dir(repo_root())
        .output()
        .expect("invoke hew compile");
    assert!(
        output.status.success(),
        "compiling CAP-13 leak fixture failed\n{}",
        describe_output(&output)
    );
    let binary = dir.join("dist_node");
    assert!(
        binary.is_file(),
        "compiled fixture missing at {}",
        binary.display()
    );
    binary
}

fn allocate_loopback_port() -> u16 {
    TcpListener::bind(("127.0.0.1", 0))
        .expect("bind loopback port")
        .local_addr()
        .expect("read loopback port")
        .port()
}

fn spawn_server(binary: &Path, port: u16, scenario: &str, kx_dir: &Path) -> ChildGuard {
    ChildGuard(
        Command::new(binary)
            .env("HEW_TRANSPORT", "tcp")
            .env("HEW_DIST_ROLE", "server")
            .env("HEW_DIST_PORT", port.to_string())
            .env("HEW_DIST_SCENARIO", scenario)
            .env("HEW_DIST_KX_DIR", kx_dir)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .expect("spawn CAP-13 leak server"),
    )
}

fn wait_for_server_ready(server: &mut ChildGuard) -> BufReader<std::process::ChildStdout> {
    let stdout = server.0.stdout.take().expect("server stdout was captured");
    let mut reader = BufReader::new(stdout);
    let deadline = Instant::now() + PROCESS_TIMEOUT;
    loop {
        assert!(
            Instant::now() < deadline,
            "CAP-13 leak server did not become ready"
        );
        let mut line = String::new();
        match reader.read_line(&mut line) {
            Ok(0) => panic!("CAP-13 leak server exited before READY"),
            Ok(_) if line.starts_with("READY ") => return reader,
            Ok(_) => {}
            Err(error) => panic!("read CAP-13 leak server stdout: {error}"),
        }
    }
}

fn spawn_client_under_leaks(binary: &Path, port: u16, scenario: &str, kx_dir: &Path) -> ChildGuard {
    ChildGuard(
        Command::new("leaks")
            .arg("--atExit")
            .arg("--")
            .arg(binary)
            .env("MallocStackLogging", "1")
            .env("MallocScribble", "1")
            .env("MallocPreScribble", "1")
            .env("MallocGuardEdges", "1")
            .env("HEW_TRANSPORT", "tcp")
            .env("HEW_DIST_ROLE", "client")
            .env("HEW_DIST_PORT", port.to_string())
            .env("HEW_DIST_SCENARIO", scenario)
            .env("HEW_DIST_KX_DIR", kx_dir)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .expect("run CAP-13 leak client under leaks"),
    )
}

fn finish_client(mut client: ChildGuard) -> Output {
    let deadline = Instant::now() + PROCESS_TIMEOUT;
    let status = loop {
        if let Some(status) = client.0.try_wait().expect("poll CAP-13 leak client") {
            break status;
        }
        assert!(
            Instant::now() < deadline,
            "CAP-13 leak client did not finish within {PROCESS_TIMEOUT:?}"
        );
        thread::sleep(Duration::from_millis(20));
    };
    let mut stdout = Vec::new();
    client
        .0
        .stdout
        .take()
        .expect("client stdout was captured")
        .read_to_end(&mut stdout)
        .expect("drain CAP-13 leak client stdout");
    let mut stderr = Vec::new();
    client
        .0
        .stderr
        .take()
        .expect("client stderr was captured")
        .read_to_end(&mut stderr)
        .expect("drain CAP-13 leak client stderr");
    Output {
        status,
        stdout,
        stderr,
    }
}

fn parse_leak_nodes(report: &str) -> Option<usize> {
    report.lines().find_map(|line| {
        let rest = line.strip_prefix("Process ")?;
        if !rest.chars().next().is_some_and(|ch| ch.is_ascii_digit()) {
            return None;
        }
        let summary = rest.split_once(": ")?.1;
        let mut words = summary.split_whitespace();
        let nodes = words.next()?.parse().ok()?;
        matches!(words.next()?, "leak" | "leaks").then_some(nodes)
    })
}

fn finish_server(
    mut server: ChildGuard,
    mut reader: BufReader<std::process::ChildStdout>,
) -> String {
    let deadline = Instant::now() + PROCESS_TIMEOUT;
    loop {
        if let Some(status) = server.0.try_wait().expect("poll CAP-13 leak server") {
            let mut stdout = String::new();
            reader
                .read_to_string(&mut stdout)
                .expect("drain CAP-13 leak server stdout");
            let mut stderr = String::new();
            server
                .0
                .stderr
                .take()
                .expect("server stderr was captured")
                .read_to_string(&mut stderr)
                .expect("drain CAP-13 leak server stderr");
            assert!(
                status.success(),
                "CAP-13 leak server exited non-zero ({status:?})\nstdout:\n{stdout}\nstderr:\n{stderr}"
            );
            return stdout;
        }
        assert!(
            Instant::now() < deadline,
            "CAP-13 leak server did not finish within {PROCESS_TIMEOUT:?}"
        );
        thread::sleep(Duration::from_millis(20));
    }
}

fn run_probe(binary: &Path, scenario: &str, iterations: usize) -> usize {
    let kx_dir = tempfile::tempdir().expect("create CAP-13 key directory");
    let port = allocate_loopback_port();
    let mut server = spawn_server(binary, port, scenario, kx_dir.path());
    let client = spawn_client_under_leaks(binary, port, scenario, kx_dir.path());
    let server_reader = wait_for_server_ready(&mut server);
    let client = finish_client(client);
    let client_stdout = String::from_utf8_lossy(&client.stdout);
    let client_stderr = String::from_utf8_lossy(&client.stderr);
    let sentinel = format!(
        "PASS {scenario} iterations={iterations} closed={iterations} down=0 local=0 target=0 ids=distinct"
    );
    assert!(
        client.status.success() && client_stdout.contains(&sentinel),
        "CAP-13 leak client did not complete its exact lifecycle sentinel\n{}",
        describe_output(&client)
    );
    assert!(
        !client_stdout.contains("FAIL "),
        "CAP-13 leak client reported failure\n{}",
        describe_output(&client)
    );
    let server_stdout = finish_server(server, server_reader);
    assert!(
        server_stdout.contains(&format!(
            "PASS {scenario} iterations={iterations} target=0 max=1"
        )),
        "CAP-13 leak server missed exact target cleanup\nstdout:\n{server_stdout}"
    );
    let report = format!("{client_stdout}\n{client_stderr}");
    parse_leak_nodes(&report).unwrap_or_else(|| {
        panic!(
            "leaks did not emit a parseable node summary\n{}",
            describe_output(&client)
        )
    })
}

#[test]
fn cross_process_monitor_close_has_zero_leak_slope_and_no_poisoned_allocator_failure() {
    if !leaks_supported("cap13_identity_monitor") {
        return;
    }
    require_codegen();
    let emit_dir = tempfile::tempdir().expect("create CAP-13 compile directory");
    let binary = compile_fixture(emit_dir.path());

    let low = run_probe(&binary, "monitor_leak_low", LOW_ITERATIONS);
    let high = run_probe(&binary, "monitor_leak_high", HIGH_ITERATIONS);
    assert!(
        high <= low,
        "CAP-13 monitor lifecycle has a positive leak-node slope: low={low}, high={high}"
    );
}
