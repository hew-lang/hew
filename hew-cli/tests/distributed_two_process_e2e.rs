//! Two-process distributed end-to-end harness.
//!
//! This is the compiled-`.hew` proving gate for cross-node messaging: it spawns
//! a real server process and a real client process — each a native binary built
//! from `tests/fixtures/distributed/dist_node.hew` — that talk to each other
//! over a loopback TCP socket using the `Node::` distributed API. A Rust unit
//! test exercising the runtime crate can be green over a broken codegen path;
//! only a compiled `.hew` round-trip proves the whole stack (frontend → MIR →
//! codegen → runtime transport → cluster registry → remote ask) actually works.
//!
//! # First assertion
//!
//! `remote_ask`: the client puts two entries on the remote actor and then issues
//! two blocking remote asks, asserting the exact returned values (`"world"`,
//! `"42"`). The teeth are exact-value equality on the client's stdout, not a
//! bare exit-0 — a codegen path that returned garbage strings, dropped the
//! payload, or short-circuited the round-trip would fail these assertions.
//!
//! # Adding scenarios
//!
//! The harness is a shared scaffold, not a one-off. To add a scenario:
//!   1. Add a `scenario_<name>` function and a dispatch branch in
//!      `dist_node.hew` (keyed on `HEW_DIST_SCENARIO`), emitting
//!      `PASS <name> <detail>` / `FAIL <name> <detail>` lines.
//!   2. Add a `#[test]` here that calls [`run_two_process_scenario`] and asserts
//!      on the exact `PASS` lines.
//!
//! No new process plumbing is needed — codec conformance, link/monitor
//! propagation, and multi-node topology checks all reuse this same path.
//!
//! # Determinism
//!
//! * The harness pre-allocates a loopback port (`bind(:0)` → read → drop) and
//!   hands it to the server, because a compiled Hew node cannot yet report its
//!   own `:0`-bound port back to the harness.
//! * The server prints `READY <port>` only after its actor is registered; the
//!   harness blocks on that line before launching the client (readiness signal,
//!   not a fixed sleep).
//! * The client retries `Node::lookup` in a bounded poll loop until registry
//!   gossip resolves the remote actor, instead of sleeping once and hoping.

mod support;

use std::io::{BufRead, BufReader, Read};
use std::net::TcpListener;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::sync::OnceLock;
use std::time::{Duration, Instant};

use support::{hew_binary, repo_root, require_codegen};

/// Wall-clock ceiling for the server to print its `READY` line after launch.
/// The binary is pre-compiled, so startup is sub-second on a warm host; this
/// generous bound only guards a genuinely wedged node.
const SERVER_READY_TIMEOUT: Duration = Duration::from_secs(30);

/// Wall-clock ceiling for the client process to finish its scenario. The client
/// retries lookup for ~5 s and each remote ask has a 5 s timeout; 40 s leaves
/// headroom for loopback gossip under load without masking a true hang.
const CLIENT_RUN_TIMEOUT: Duration = Duration::from_secs(40);

/// Compile `dist_node.hew` exactly once per test binary and cache the resulting
/// native executable path. All scenario tests in this file share the artifact.
fn compiled_node_binary() -> &'static Path {
    static NODE_BINARY: OnceLock<PathBuf> = OnceLock::new();
    NODE_BINARY.get_or_init(|| {
        require_codegen();

        let source = repo_root()
            .join("hew-cli")
            .join("tests")
            .join("fixtures")
            .join("distributed")
            .join("dist_node.hew");
        assert!(
            source.is_file(),
            "dist_node fixture missing at {}",
            source.display()
        );

        // Emit into a stable, leaked temp dir: the binary must outlive this
        // function and be reused across every scenario test in the process.
        let emit_dir = tempfile::tempdir()
            .expect("create dist_node compile dir (leaked for process lifetime)");
        let emit_path = emit_dir.path().to_path_buf();
        // Intentionally leak the TempDir guard so the compiled binary survives
        // for the whole test-binary lifetime (the OS reclaims it at process
        // exit). Without this the dir would be removed when the guard drops.
        std::mem::forget(emit_dir);

        let mut command = Command::new(hew_binary());
        command
            .arg("compile")
            .arg("--emit-dir")
            .arg(&emit_path)
            .arg(&source)
            .current_dir(repo_root());
        let output =
            support::run_bounded_command(command, format!("hew compile {}", source.display()));
        assert!(
            output.status.success(),
            "compiling dist_node.hew failed\n{}",
            support::describe_output(&output)
        );

        let binary = emit_path.join("dist_node");
        assert!(
            binary.is_file(),
            "compiled dist_node binary missing at {}",
            binary.display()
        );
        binary
    })
}

/// Pre-allocate an ephemeral loopback port and release it so the server can
/// bind it. There is a small TOCTOU window between release and the server's
/// rebind; on serialized loopback test execution it is negligible, and a bind
/// failure surfaces as the server dying before `READY` (a loud, retryable
/// failure rather than a silent wrong answer).
fn allocate_loopback_port() -> u16 {
    TcpListener::bind(("127.0.0.1", 0))
        .expect("bind ephemeral loopback listener")
        .local_addr()
        .expect("read ephemeral loopback address")
        .port()
}

/// A child process whose stdout/stderr are captured and that is force-killed on
/// drop, so a wedged node never leaks past the test.
struct ManagedChild {
    child: Child,
    label: String,
}

impl ManagedChild {
    fn spawn(binary: &Path, role: &str, port: u16, scenario: &str) -> Self {
        let child = Command::new(binary)
            .env("HEW_TRANSPORT", "tcp")
            .env("HEW_DIST_ROLE", role)
            .env("HEW_DIST_PORT", port.to_string())
            .env("HEW_DIST_SCENARIO", scenario)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .unwrap_or_else(|error| panic!("failed to spawn dist_node {role}: {error}"));
        Self {
            child,
            label: role.to_string(),
        }
    }
}

impl Drop for ManagedChild {
    fn drop(&mut self) {
        // Best-effort force-kill; the server's happy path is to be killed here
        // after the client finishes (it otherwise sleeps to a safety ceiling).
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

/// Block until the server prints `READY <port>` on stdout, returning its stdout
/// reader so callers could read further lines. Fails loudly if the server dies
/// or the readiness window elapses.
fn wait_for_server_ready(server: &mut ManagedChild) -> BufReader<std::process::ChildStdout> {
    let stdout = server
        .child
        .stdout
        .take()
        .expect("server stdout was captured");
    let mut reader = BufReader::new(stdout);
    let deadline = Instant::now() + SERVER_READY_TIMEOUT;

    loop {
        assert!(
            Instant::now() < deadline,
            "server did not print READY within {:?}; exit status: {:?}",
            SERVER_READY_TIMEOUT,
            server.child.try_wait()
        );
        // The server stays alive after READY (it sleeps to a safety ceiling), so
        // a closed pipe before READY means the process exited early.
        let mut line = String::new();
        match reader.read_line(&mut line) {
            Ok(0) => panic!(
                "server stdout closed before READY (process exited early); exit: {:?}",
                server.child.try_wait()
            ),
            Ok(_) => {
                if line.trim_start().starts_with("READY ") {
                    return reader;
                }
                // Ignore non-READY chatter (cluster warnings, etc.).
            }
            Err(error) => panic!("error reading server stdout: {error}"),
        }
    }
}

/// Run the client to completion with a wall-clock cap, returning its captured
/// stdout. Force-kills on timeout so a wedged client never hangs the suite.
fn run_client_to_completion(mut client: ManagedChild) -> String {
    let deadline = Instant::now() + CLIENT_RUN_TIMEOUT;
    loop {
        match client.child.try_wait() {
            Ok(Some(status)) => {
                let mut stdout = String::new();
                if let Some(mut out) = client.child.stdout.take() {
                    let _ = out.read_to_string(&mut stdout);
                }
                let mut stderr = String::new();
                if let Some(mut err) = client.child.stderr.take() {
                    let _ = err.read_to_string(&mut stderr);
                }
                assert!(
                    status.success(),
                    "client exited non-zero ({status:?})\nstdout:\n{stdout}\nstderr:\n{stderr}"
                );
                return stdout;
            }
            Ok(None) => {
                assert!(
                    Instant::now() < deadline,
                    "client did not finish within {CLIENT_RUN_TIMEOUT:?}"
                );
                std::thread::sleep(Duration::from_millis(50));
            }
            Err(error) => panic!("error polling client {}: {error}", client.label),
        }
    }
}

/// Spawn the server + client pair for `scenario` and return the client's
/// captured stdout. The shared entry point every scenario test calls.
fn run_two_process_scenario(scenario: &str) -> String {
    let binary = compiled_node_binary();
    let port = allocate_loopback_port();

    let mut server = ManagedChild::spawn(binary, "server", port, scenario);
    let _server_stdout = wait_for_server_ready(&mut server);

    let client = ManagedChild::spawn(binary, "client", port, scenario);
    let stdout = run_client_to_completion(client);

    // `server` is dropped here, force-killing the still-sleeping server.
    drop(server);
    stdout
}

/// The linchpin assertion: a real two-process remote-ask round-trip over a
/// loopback socket returns the exact values stored on the remote actor.
#[test]
fn remote_ask_round_trip_returns_exact_values() {
    let stdout = run_two_process_scenario("remote_ask");

    // Teeth: exact PASS lines, not a bare exit-0. A broken codegen / transport
    // path that dropped the payload or returned a wrong string fails here.
    assert!(
        stdout.contains("PASS remote_ask get-hello=world"),
        "expected exact get('hello')=='world' from remote ask; client stdout:\n{stdout}"
    );
    assert!(
        stdout.contains("PASS remote_ask get-count=42"),
        "expected exact get('count')=='42' from remote ask; client stdout:\n{stdout}"
    );
    // Negative guard: no FAIL line may appear on a successful round-trip.
    assert!(
        !stdout.contains("FAIL "),
        "client reported a FAIL on the remote-ask round-trip; client stdout:\n{stdout}"
    );
}
