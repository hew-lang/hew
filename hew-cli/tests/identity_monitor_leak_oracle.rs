//! Identity-monitor cross-process close leak and poisoned-allocator oracle.

#![cfg(unix)]

#[path = "support/distributed.rs"]
mod distributed;
mod support;

use std::fs::File;
use std::io::{BufRead, BufReader, Read};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Output, Stdio};
use std::thread;
use std::time::{Duration, Instant};

use distributed::admitted_node_directory;
use support::leak_slope::{parse_leaks_summary, require_leaks_tool};
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

struct LeakClient {
    child: ChildGuard,
    stdout_path: PathBuf,
    stderr_path: PathBuf,
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
        "compiling identity monitor leak fixture failed\n{}",
        describe_output(&output)
    );
    let binary = hew_testutil::compiled_binary_path(dir, "dist_node");
    assert!(
        binary.is_file(),
        "compiled fixture missing at {}",
        binary.display()
    );
    binary
}

fn spawn_server(binary: &Path, scenario: &str, kx_dir: &Path) -> ChildGuard {
    ChildGuard(
        Command::new(binary)
            .env("HEW_DIST_TRANSPORT", "tcp")
            .env("HEW_DIST_ROLE", "server")
            .env("HEW_DIST_SCENARIO", scenario)
            .env("HEW_DIST_KX_DIR", kx_dir)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .expect("spawn identity monitor leak server"),
    )
}

fn wait_for_server_ready(server: &mut ChildGuard) -> BufReader<std::process::ChildStdout> {
    let stdout = server.0.stdout.take().expect("server stdout was captured");
    let mut reader = BufReader::new(stdout);
    let deadline = Instant::now() + PROCESS_TIMEOUT;
    let mut captured = String::new();
    loop {
        assert!(
            Instant::now() < deadline,
            "identity monitor leak server did not become ready\nstdout:\n{captured}"
        );
        let mut line = String::new();
        match reader.read_line(&mut line) {
            Ok(0) => panic!(
                "identity monitor leak server exited before READY; exit: {:?}\nstdout:\n{captured}",
                server.0.try_wait()
            ),
            Ok(_) if line.starts_with("READY ") => return reader,
            Ok(_) => captured.push_str(&line),
            Err(error) => panic!("read identity monitor leak server stdout: {error}"),
        }
    }
}

fn spawn_client_under_leaks(binary: &Path, scenario: &str, kx_dir: &Path) -> LeakClient {
    // `leaks --atExit` writes a full allocation report after the child exits.
    // A piped report can exceed the pipe buffer, deadlocking leaks before our
    // parent observes its exit. Capture to files so the child always drains.
    let stdout_path = kx_dir.join("client-leaks.stdout");
    let stderr_path = kx_dir.join("client-leaks.stderr");
    let stdout = File::create(&stdout_path).expect("create identity monitor client stdout capture");
    let stderr = File::create(&stderr_path).expect("create identity monitor client stderr capture");
    LeakClient {
        child: ChildGuard(
            Command::new("leaks")
                .arg("--atExit")
                .arg("--")
                .arg(binary)
                .env("MallocStackLogging", "1")
                .env("MallocScribble", "1")
                .env("MallocPreScribble", "1")
                .env("MallocGuardEdges", "1")
                .env("HEW_DIST_TRANSPORT", "tcp")
                .env("HEW_DIST_ROLE", "client")
                .env("HEW_DIST_SCENARIO", scenario)
                .env("HEW_DIST_KX_DIR", kx_dir)
                .stdout(Stdio::from(stdout))
                .stderr(Stdio::from(stderr))
                .spawn()
                .expect("run identity monitor leak client under leaks"),
        ),
        stdout_path,
        stderr_path,
    }
}

fn finish_client(mut client: LeakClient) -> Output {
    let deadline = Instant::now() + PROCESS_TIMEOUT;
    let status = loop {
        if let Some(status) = client
            .child
            .0
            .try_wait()
            .expect("poll identity monitor leak client")
        {
            break status;
        }
        assert!(
            Instant::now() < deadline,
            "identity monitor leak client did not finish within {PROCESS_TIMEOUT:?}"
        );
        thread::sleep(Duration::from_millis(20));
    };
    let stdout =
        std::fs::read(&client.stdout_path).expect("read identity monitor leak client stdout");
    let stderr =
        std::fs::read(&client.stderr_path).expect("read identity monitor leak client stderr");
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
        if let Some(status) = server
            .0
            .try_wait()
            .expect("poll identity monitor leak server")
        {
            let mut stdout = String::new();
            reader
                .read_to_string(&mut stdout)
                .expect("drain identity monitor leak server stdout");
            let mut stderr = String::new();
            server
                .0
                .stderr
                .take()
                .expect("server stderr was captured")
                .read_to_string(&mut stderr)
                .expect("drain identity monitor leak server stderr");
            assert!(
                status.success(),
                "identity monitor leak server exited non-zero ({status:?})\nstdout:\n{stdout}\nstderr:\n{stderr}"
            );
            return stdout;
        }
        assert!(
            Instant::now() < deadline,
            "identity monitor leak server did not finish within {PROCESS_TIMEOUT:?}"
        );
        thread::sleep(Duration::from_millis(20));
    }
}

fn run_probe(binary: &Path, scenario: &str, iterations: usize) -> usize {
    let kx_dir = admitted_node_directory(false, &["server", "client"]);
    let mut server = spawn_server(binary, scenario, kx_dir.path());
    let server_reader = wait_for_server_ready(&mut server);
    let client = spawn_client_under_leaks(binary, scenario, kx_dir.path());
    let client = finish_client(client);
    let client_stdout = String::from_utf8_lossy(&client.stdout);
    let client_stderr = String::from_utf8_lossy(&client.stderr);
    let sentinel = format!(
        "PASS {scenario} iterations={iterations} closed={iterations} down=0 local=0 target=0 ids=distinct"
    );
    assert!(
        !client_stdout.contains("FAIL "),
        "identity monitor leak client reported failure\n{}",
        describe_output(&client)
    );
    let server_stdout = finish_server(server, server_reader);
    assert!(
        server_stdout.contains(&format!(
            "PASS {scenario} iterations={iterations} target=0 max=1"
        )),
        "identity monitor leak server missed exact target cleanup\nstdout:\n{server_stdout}"
    );
    let report = format!("{client_stdout}\n{client_stderr}");
    let leak_nodes = parse_leak_nodes(&report).unwrap_or_else(|| {
        panic!(
            "leaks did not emit a parseable node summary\n{}",
            describe_output(&client)
        )
    });
    // `leaks(1)` reserves exit 1 for a completed inspection that found one
    // or more leaks. Any other nonzero exit is an inspector/tool failure.
    let completed_leak_inspection = client.status.code() == Some(1) && leak_nodes > 0;
    assert!(
        (client.status.success() || completed_leak_inspection) && client_stdout.contains(&sentinel),
        "identity monitor leak client did not complete cleanly or its exact lifecycle sentinel\n{}",
        describe_output(&client)
    );
    leak_nodes
}

fn identity_display_output_matches(stdout: &str, scenario: &str, iterations: usize) -> bool {
    let lines: Vec<_> = stdout
        .lines()
        .take_while(|line| !line.starts_with("Process:"))
        .filter(|line| !line.is_empty())
        .collect();
    let Some((sentinel, frames)) = lines.split_last() else {
        return false;
    };
    if *sentinel != format!("PASS {scenario} iterations={iterations}") {
        return false;
    }
    let Some(first_frame) = frames.first_chunk::<7>() else {
        return false;
    };
    let node_len = first_frame[0];
    let location_len = first_frame[1];
    if ![node_len, location_len]
        .into_iter()
        .all(|value| value.parse::<usize>().is_ok_and(|length| length > 0))
    {
        return false;
    }
    // NodeId and Location are the two discovery display surfaces. Each frame
    // exercises both through direct temporaries, named bindings, and mixed use.
    let expected_frame = [
        node_len,
        location_len,
        node_len,
        location_len,
        node_len,
        location_len,
        "frame",
    ];
    frames.len() == iterations * expected_frame.len()
        && frames
            .chunks_exact(expected_frame.len())
            .all(|frame| frame == expected_frame)
}

fn run_identity_display_probe(binary: &Path, scenario: &str, iterations: usize) -> (usize, usize) {
    let kx_dir = admitted_node_directory(false, &["server", "client"]);
    let mut server = spawn_server(binary, scenario, kx_dir.path());
    let server_reader = wait_for_server_ready(&mut server);
    let client = spawn_client_under_leaks(binary, scenario, kx_dir.path());
    let client = finish_client(client);
    let client_stdout = String::from_utf8_lossy(&client.stdout);
    let client_stderr = String::from_utf8_lossy(&client.stderr);
    let sentinel = format!("PASS {scenario} iterations={iterations}");
    let report = format!("{client_stdout}\n{client_stderr}");
    let summary = parse_leaks_summary(&report).unwrap_or_else(|| {
        panic!(
            "leaks did not emit a parseable identity display summary\n{}",
            describe_output(&client)
        )
    });
    // The shared authenticated-node fixture has a constant credential-file
    // allocation at shutdown. `leaks(1)` reports findings with exit 1; accept
    // that exit only when a parsed positive summary accounts for it. Every
    // other nonzero exit remains an allocator/tool failure.
    let completed_leak_inspection =
        client.status.code() == Some(1) && (summary.0 > 0 || summary.1 > 0);

    assert!(
        client.status.success() || completed_leak_inspection,
        "identity display client failed under leaks or the poisoned allocator\n{}",
        describe_output(&client)
    );
    assert!(
        !client_stdout.contains("FAIL ") && client_stdout.contains(&sentinel),
        "identity display client missed its exact completion sentinel\n{}",
        describe_output(&client)
    );
    assert!(
        identity_display_output_matches(&client_stdout, scenario, iterations),
        "identity display client did not execute every direct, named, and mixed path\n{}",
        describe_output(&client)
    );

    // The identity-display scenario is client-only. Once its clean shutdown is
    // observed, terminate the otherwise long-lived fixture server.
    drop(server_reader);
    drop(server);

    summary
}

#[test]
fn identity_display_output_requires_complete_frames_and_exact_sentinel() {
    let frame = "32\n42\n32\n42\n32\n42\nframe\n";
    for iterations in [1, 3] {
        let stdout = format!(
            "{}PASS identity_display_low iterations={iterations}\nProcess: dist_node\n",
            frame.repeat(iterations)
        );
        assert!(identity_display_output_matches(
            &stdout,
            "identity_display_low",
            iterations
        ));
        assert!(!identity_display_output_matches(
            &stdout,
            "identity_display_high",
            iterations
        ));
        assert!(!identity_display_output_matches(
            &stdout,
            "identity_display_low",
            iterations + 1
        ));
        assert!(!identity_display_output_matches(
            &stdout.replace("\nProcess:", " extra\nProcess:"),
            "identity_display_low",
            iterations
        ));
    }
}

#[test]
fn identity_display_output_rejects_missing_duplicate_or_inconsistent_paths() {
    let frame = ["32", "42", "32", "42", "32", "42", "frame"];
    for path in 0..frame.len() {
        let mut missing = frame.to_vec();
        missing.remove(path);
        let mut duplicated = frame.to_vec();
        duplicated.insert(path, frame[path]);
        let mut inconsistent = frame.to_vec();
        inconsistent[path] = "0";
        for malformed in [missing, duplicated, inconsistent] {
            let stdout = format!(
                "{}\n{}\nPASS identity_display_low iterations=2\n",
                frame.join("\n"),
                malformed.join("\n")
            );
            assert!(
                !identity_display_output_matches(&stdout, "identity_display_low", 2),
                "malformed frame was accepted:\n{stdout}"
            );
        }
    }
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn cross_process_monitor_close_has_zero_leak_slope_and_no_poisoned_allocator_failure() {
    require_leaks_tool();
    require_codegen();
    let emit_dir = tempfile::tempdir().expect("create identity monitor compile directory");
    let binary = compile_fixture(emit_dir.path());

    let low = run_probe(&binary, "monitor_leak_low", LOW_ITERATIONS);
    let high = run_probe(&binary, "monitor_leak_high", HIGH_ITERATIONS);
    assert!(
        high <= low,
        "identity monitor close lifecycle has a positive leak-node slope: low={low}, high={high}"
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn identity_display_temporaries_have_zero_leak_slope_and_no_poisoned_allocator_failure() {
    require_leaks_tool();
    require_codegen();
    let emit_dir = tempfile::tempdir().expect("create identity display compile directory");
    let binary = compile_fixture(emit_dir.path());

    let low = run_identity_display_probe(&binary, "identity_display_low", LOW_ITERATIONS);
    let high = run_identity_display_probe(&binary, "identity_display_high", HIGH_ITERATIONS);
    assert_eq!(
        high, low,
        "identity display nodes or bytes grew between low={low:?} and high={high:?}"
    );
}
