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
        Self::spawn_with_env(binary, role, port, scenario, &[])
    }

    fn spawn_with_env(
        binary: &Path,
        role: &str,
        port: u16,
        scenario: &str,
        extra_env: &[(&str, &str)],
    ) -> Self {
        let mut command = Command::new(binary);
        command
            .env("HEW_TRANSPORT", "tcp")
            .env("HEW_DIST_ROLE", role)
            .env("HEW_DIST_PORT", port.to_string())
            .env("HEW_DIST_SCENARIO", scenario)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());
        for (key, value) in extra_env {
            command.env(key, value);
        }
        let child = command
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

/// Run a cross-node link cascade scenario where the REMOTE actor dies on its own
/// (clean exit or crash) and the client polls its local linker's terminal state.
/// The client runs with `HEW_LINK_PROBE=1` so the runtime records actor terminal
/// reasons in the probe ledger the fixture reads. Returns the client's stdout.
fn run_link_cascade_scenario(scenario: &str) -> String {
    let binary = compiled_node_binary();
    let port = allocate_loopback_port();

    let mut server = ManagedChild::spawn(binary, "server", port, scenario);
    let _server_stdout = wait_for_server_ready(&mut server);

    let client =
        ManagedChild::spawn_with_env(binary, "client", port, scenario, &[("HEW_LINK_PROBE", "1")]);
    let stdout = run_client_to_completion(client);

    drop(server);
    stdout
}

/// Run the server + client pair for a connection-drop scenario: the client
/// prints `READY_DROP <name>` once its monitor is registered, the harness then
/// kills the server process (dropping the connection mid-scenario), and the
/// client runs to completion observing the connection-drop DOWN. Returns the
/// client's captured stdout.
fn run_conn_drop_scenario(scenario: &str) -> String {
    run_conn_drop_scenario_with_env(scenario, &[])
}

/// As [`run_conn_drop_scenario`], with extra client environment (e.g.
/// `HEW_LINK_PROBE=1` for the cross-node link partition cascade).
fn run_conn_drop_scenario_with_env(scenario: &str, client_env: &[(&str, &str)]) -> String {
    let binary = compiled_node_binary();
    let port = allocate_loopback_port();

    let mut server = ManagedChild::spawn(binary, "server", port, scenario);
    let _server_stdout = wait_for_server_ready(&mut server);

    let mut client = ManagedChild::spawn_with_env(binary, "client", port, scenario, client_env);

    // Block until the client signals its monitor is registered, then kill the
    // server to drop the connection. Reading the client's stdout incrementally
    // would consume the PASS lines we assert on later, so we take the stdout
    // handle, read up to READY_DROP, and stitch the remaining output back.
    let client_stdout = client
        .child
        .stdout
        .take()
        .expect("client stdout was captured");
    let mut reader = BufReader::new(client_stdout);
    let mut captured = String::new();
    let deadline = Instant::now() + SERVER_READY_TIMEOUT;
    loop {
        assert!(
            Instant::now() < deadline,
            "client did not print READY_DROP within {SERVER_READY_TIMEOUT:?}"
        );
        let mut line = String::new();
        match reader.read_line(&mut line) {
            Ok(0) => panic!(
                "client stdout closed before READY_DROP (exited early); captured:\n{captured}"
            ),
            Ok(_) => {
                captured.push_str(&line);
                if line.contains("READY_DROP ") {
                    break;
                }
            }
            Err(error) => panic!("error reading client stdout: {error}"),
        }
    }

    // Drop the connection by killing the server now that the monitor is live.
    let _ = server.child.kill();
    let _ = server.child.wait();

    // Drain the rest of the client's output to completion, with a wall-clock
    // cap so a wedged client never hangs the suite.
    let drain_deadline = Instant::now() + CLIENT_RUN_TIMEOUT;
    loop {
        let mut line = String::new();
        match reader.read_line(&mut line) {
            Ok(0) => break,
            Ok(_) => captured.push_str(&line),
            Err(error) => panic!("error draining client stdout: {error}"),
        }
        assert!(
            Instant::now() < drain_deadline,
            "client did not finish within {CLIENT_RUN_TIMEOUT:?} after connection drop"
        );
    }

    let status = client
        .child
        .wait()
        .expect("wait for client after connection-drop scenario");
    assert!(
        status.success(),
        "client exited non-zero ({status:?}) on connection-drop scenario\nstdout:\n{captured}"
    );
    captured
}

/// Run the server + client pair for a watcher-node-death scenario: the client
/// (the watcher) prints `READY_CLIENT_WATCHER <name>` once its monitor is
/// registered, the harness then kills the CLIENT process (simulating watcher-
/// node death), and the SERVER runs to completion observing the prune and
/// printing its assertion. Returns the server's captured stdout.
fn run_watcher_drop_scenario(scenario: &str) -> String {
    let binary = compiled_node_binary();
    let port = allocate_loopback_port();

    let mut server = ManagedChild::spawn(binary, "server", port, scenario);
    let server_stdout = server
        .child
        .stdout
        .take()
        .expect("server stdout was captured");
    let mut server_reader = BufReader::new(server_stdout);

    // Read until READY so we know actors are registered.
    let deadline = Instant::now() + SERVER_READY_TIMEOUT;
    loop {
        assert!(
            Instant::now() < deadline,
            "server did not print READY within {SERVER_READY_TIMEOUT:?}"
        );
        let mut line = String::new();
        match server_reader.read_line(&mut line) {
            Ok(0) => panic!("server stdout closed before READY"),
            Ok(_) => {
                if line.trim_start().starts_with("READY ") {
                    break;
                }
            }
            Err(error) => panic!("error reading server stdout: {error}"),
        }
    }

    // Launch the client (the watcher) and block until it signals its monitor
    // is live so the harness knows the watch-side entry is registered.
    let mut client = ManagedChild::spawn(binary, "client", port, scenario);
    let client_stdout = client
        .child
        .stdout
        .take()
        .expect("client stdout was captured");
    let mut client_reader = BufReader::new(client_stdout);
    let ready_deadline = Instant::now() + SERVER_READY_TIMEOUT;
    loop {
        assert!(
            Instant::now() < ready_deadline,
            "client did not print READY_CLIENT_WATCHER within {SERVER_READY_TIMEOUT:?}"
        );
        let mut line = String::new();
        match client_reader.read_line(&mut line) {
            Ok(0) => panic!("client stdout closed before READY_CLIENT_WATCHER"),
            Ok(_) => {
                if line.contains("READY_CLIENT_WATCHER ") {
                    break;
                }
            }
            Err(error) => panic!("error reading client stdout: {error}"),
        }
    }

    // Accumulate all server output (including diagnostic lines) in one buffer
    // so the final assertion can reference the full server trace on failure.
    let mut server_out = String::new();

    // Wait for the server to confirm it received and processed the
    // CTRL_MONITOR_REQ (target-side entry recorded).  This read must happen
    // BEFORE the client kill: killing the client races CTRL_MONITOR_REQ
    // processing — the OS may deliver the RST/FIN before the connection reader
    // finishes processing the frame, losing the registration.
    let reg_deadline = Instant::now() + SERVER_READY_TIMEOUT;
    loop {
        assert!(
            Instant::now() < reg_deadline,
            "server did not confirm watcher registration within {SERVER_READY_TIMEOUT:?}"
        );
        let mut line = String::new();
        match server_reader.read_line(&mut line) {
            Ok(0) => panic!("server stdout closed before READY_SERVER_WATCHER_REGISTERED"),
            Ok(_) => {
                server_out.push_str(&line);
                if line.contains("READY_SERVER_WATCHER_REGISTERED ") {
                    break;
                }
            }
            Err(error) => panic!("error reading server stdout: {error}"),
        }
    }

    // Kill the client to simulate watcher-node death. The server has already
    // confirmed registration so there is no race between kill and processing.
    let _ = client.child.kill();
    let _ = client.child.wait();

    // Drain the server's output until it prints its assertion (PASS or FAIL)
    // or the wall-clock cap elapses.
    let drain_deadline = Instant::now() + CLIENT_RUN_TIMEOUT;
    loop {
        let mut line = String::new();
        match server_reader.read_line(&mut line) {
            Ok(0) | Err(_) => break,
            Ok(_) => {
                server_out.push_str(&line);
                // Stop once the server prints its verdict.
                if line.contains("PASS monitor_watcher_node_death")
                    || line.contains("FAIL monitor_watcher_node_death")
                {
                    break;
                }
            }
        }
        assert!(
            Instant::now() < drain_deadline,
            "server did not print verdict within {CLIENT_RUN_TIMEOUT:?}"
        );
    }

    // Force-kill the server and discard its exit status.
    let _ = server.child.kill();
    let _ = server.child.wait();

    server_out
}

/// Run a server-verdict scenario where the SERVER prints the PASS/FAIL line and
/// the CLIENT runs to a clean exit on its own (it is not killed). Used by the
/// cross-node demonitor reclamation proof: the client registers a cross-node
/// monitor, closes it (sending `CTRL_DEMONITOR`), and exits; the server observes
/// its target-side watcher count rise then fall and prints the verdict. Returns
/// the server's captured stdout.
fn run_server_verdict_scenario(scenario: &str) -> String {
    let binary = compiled_node_binary();
    let port = allocate_loopback_port();

    let mut server = ManagedChild::spawn(binary, "server", port, scenario);
    let server_stdout = server
        .child
        .stdout
        .take()
        .expect("server stdout was captured");
    let mut server_reader = BufReader::new(server_stdout);

    // Read until READY so we know the actor is registered.
    let deadline = Instant::now() + SERVER_READY_TIMEOUT;
    loop {
        assert!(
            Instant::now() < deadline,
            "server did not print READY within {SERVER_READY_TIMEOUT:?}"
        );
        let mut line = String::new();
        match server_reader.read_line(&mut line) {
            Ok(0) => panic!("server stdout closed before READY"),
            Ok(_) => {
                if line.trim_start().starts_with("READY ") {
                    break;
                }
            }
            Err(error) => panic!("error reading server stdout: {error}"),
        }
    }

    // Launch the client; it registers, closes its monitor, and exits cleanly.
    let client = ManagedChild::spawn(binary, "client", port, scenario);

    // Drain the server's output until it prints its verdict or the cap elapses.
    let mut server_out = String::new();
    let drain_deadline = Instant::now() + CLIENT_RUN_TIMEOUT;
    loop {
        let mut line = String::new();
        match server_reader.read_line(&mut line) {
            Ok(0) | Err(_) => break,
            Ok(_) => {
                server_out.push_str(&line);
                if line.contains(&format!("PASS {scenario}"))
                    || line.contains(&format!("FAIL {scenario}"))
                {
                    break;
                }
            }
        }
        assert!(
            Instant::now() < drain_deadline,
            "server did not print verdict within {CLIENT_RUN_TIMEOUT:?}"
        );
    }

    // Best-effort wait for the client to exit, then force-kill the server.
    drop(client);
    let _ = server.child.kill();
    let _ = server.child.wait();

    server_out
}

/// Clean cross-node exit: a client monitoring a remote actor receives exactly
/// one DOWN carrying the clean-exit reason (`HewActorState::Stopped` == 6) when
/// the remote actor stops itself.
#[test]
fn remote_monitor_down_on_clean_exit() {
    let stdout = run_two_process_scenario("remote_monitor_clean_exit");

    // Teeth: the exact clean-exit reason code, distinct from crash (5) and the
    // MonitorLost drop sentinel (-1).
    assert!(
        stdout.contains("PASS remote_monitor_clean_exit reason=6"),
        "expected clean-exit DOWN reason 6; client stdout:\n{stdout}"
    );
    // Exactly-once: the second recv must time out (no duplicate DOWN).
    assert!(
        stdout.contains("PASS remote_monitor_clean_exit no-dup"),
        "expected no duplicate DOWN for the clean-exit monitor; client stdout:\n{stdout}"
    );
    assert!(
        !stdout.contains("FAIL "),
        "client reported a FAIL on the clean-exit monitor; client stdout:\n{stdout}"
    );
}

/// Cross-node crash: a client monitoring a remote actor receives exactly one
/// DOWN carrying the crash reason (`HewActorState::Crashed` == 5) when the remote
/// actor panics.
#[test]
fn remote_monitor_down_on_crash() {
    let stdout = run_two_process_scenario("remote_monitor_crash");

    // Teeth: the exact crash reason code, distinct from clean exit (6).
    assert!(
        stdout.contains("PASS remote_monitor_crash reason=5"),
        "expected crash DOWN reason 5; client stdout:\n{stdout}"
    );
    assert!(
        stdout.contains("PASS remote_monitor_crash no-dup"),
        "expected no duplicate DOWN for the crash monitor; client stdout:\n{stdout}"
    );
    assert!(
        !stdout.contains("FAIL "),
        "client reported a FAIL on the crash monitor; client stdout:\n{stdout}"
    );
}

/// Cross-node connection drop: a client monitoring a remote actor receives
/// exactly one DOWN carrying the `MonitorLost` sentinel (-1) when the server
/// process is killed mid-scenario — distinct from clean exit and crash.
#[test]
fn remote_monitor_down_on_connection_drop() {
    let stdout = run_conn_drop_scenario("remote_monitor_conn_drop");

    assert!(
        stdout.contains("PASS remote_monitor_conn_drop reason=-1"),
        "expected MonitorLost DOWN reason -1 on connection drop; client stdout:\n{stdout}"
    );
    assert!(
        stdout.contains("PASS remote_monitor_conn_drop no-dup"),
        "expected no duplicate DOWN for the connection-drop monitor; client stdout:\n{stdout}"
    );
    assert!(
        !stdout.contains("FAIL "),
        "client reported a FAIL on the connection-drop monitor; client stdout:\n{stdout}"
    );
}

/// Partition gate: a remote ask whose target peer is gone must resolve
/// fail-closed with a typed cause WITHOUT hanging, rather than blocking to the
/// client ceiling or fabricating a value. The harness kills the server after the
/// client signals `READY_DROP`; the client then asks the now-unreachable actor.
///
/// The EXACT fail-closed cause is timing-dependent — a two-process SIGKILL races
/// three correct fail paths: `RoutingFailed` (route torn down before the ask was
/// set up — dominates under CI load), `ConnectionDropped` (socket-drop failed the
/// pending ask — common on a quiet host), and `Partition` (the SWIM-DEAD verdict
/// fanned out to the pending ask). Pinning a single variant is the bug that flaked
/// this gate under load (the fixture's catch-all rejected `RoutingFailed`); the
/// invariant is no-hang + typed-fail-closed, not a specific variant. So the gate
/// asserts on any `PASS partition_ask` line and forbids any `FAIL`.
///
/// Teeth that keep this meaningful: (a) a `PASS partition_ask reason=<cause>` line
/// — the fixture only emits one when an `Err` came back BEFORE the ask's own
/// deadline (a proactive fail-closed verdict), never on a `Result::Ok` (which is
/// rejected as `never-partitioned`); (b) the run finishes well under the 40 s
/// client ceiling — `run_conn_drop_scenario`'s drain cap is the no-hang guard,
/// and a hang-to-deadline would blow it; (c) no `FAIL` line — in particular no
/// `reason=timeout`, which the fixture emits when the ask waited its WHOLE
/// deadline because no proactive fail-closed path fired (the hang this gate
/// exists to catch), and no `never-partitioned`, which would mean the ask
/// wrongly succeeded against a killed peer.
///
/// The deterministic SWIM-DEAD-while-socket-open → Partition path (the specific
/// fail-open that `swim_dead_wakes_pending_remote_ask_with_partition` closes) is
/// proven exactly in-process by that runtime unit test; this two-process fixture
/// pins the broader cross-wire no-hang invariant under real teardown races.
#[test]
fn remote_ask_under_partition_fails_closed_not_hang() {
    let stdout = run_conn_drop_scenario("partition_ask");

    // Any typed fail-closed cause is correct; the fixture emits this line only for
    // an `Err` that resolved BEFORE the ask's own deadline (a proactive verdict).
    assert!(
        stdout.contains("PASS partition_ask reason="),
        "expected a typed fail-closed PASS (the ask must resolve to a typed \
         AskError before its deadline, not hang); client stdout:\n{stdout}"
    );
    // A timeout is the hang-to-deadline failure this gate exists to catch: the ask
    // waited its whole deadline because no proactive fail-closed path fired.
    assert!(
        !stdout.contains("FAIL partition_ask reason=timeout"),
        "remote ask under partition timed out instead of failing closed \
         (no proactive fail-closed path fired); client stdout:\n{stdout}"
    );
    // A success against a killed peer would be a wrong answer, not a partition.
    assert!(
        !stdout.contains("FAIL partition_ask never-partitioned"),
        "remote ask wrongly succeeded against a killed peer; client stdout:\n{stdout}"
    );
    assert!(
        !stdout.contains("FAIL "),
        "client reported a FAIL on the partition-ask scenario; client stdout:\n{stdout}"
    );
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

/// DIST-10 `StaleRef`: a captured `RemotePid<T>` fails closed with `StaleRef` once
/// its registration is superseded by a re-registration of the same name to a
/// DIFFERENT actor — on both `tell` and `ask`, across two OS processes.
///
/// The client looks up "kv" (capturing the original actor's pid), the server
/// re-points "kv" to a fresh actor (unregister + re-register, which supersedes
/// the original serial and gossips the re-point), and the client's captured ref
/// must observe `SendError::StaleRef` / `AskError::StaleRef` — never a silent
/// delivery to the replacement, never a generic routing error.
///
/// Teeth: the scenario fails if the captured ref ever succeeds after the
/// re-point, and the exact `StaleRef` discriminant is required on BOTH verbs.
#[test]
fn captured_remote_ref_fails_closed_with_stale_ref_after_repoint() {
    let stdout = run_two_process_scenario("stale_ref");

    assert!(
        stdout.contains("PASS stale_ref tell=stale ask=stale"),
        "expected the captured ref to go StaleRef on both tell and ask after the \
         server re-pointed the name; client stdout:\n{stdout}"
    );
    // Negative guard: no FAIL line — in particular not `tell-never-stale` (the ref
    // kept delivering to the replacement) or a wrong-error discriminant.
    assert!(
        !stdout.contains("FAIL "),
        "client reported a FAIL on the StaleRef scenario; client stdout:\n{stdout}"
    );
}

/// A `#[wire]`-annotated payload with EXPLICIT `@N` field tags round-trips across
/// two OS processes. The `remote_ask` scenario above sends plain `record`/`enum`
/// types, which get `@N` tags by the codec's positional fallback; this scenario
/// sends `#[wire]` types (`WireCmd` enum request, `WireResult` struct reply) with
/// explicit `@N` field tags, exercising the tag-keyed CBOR body the positional
/// path does not — the struct `@N`-keyed map and the enum unit-tag / map-of-one
/// body, in both the request and the reply direction. The body shape conforms to
/// `hew-runtime/schemas/wire-body.cddl` (validated in
/// `hew-runtime/tests/wire_body_cddl_conformance.rs`).
///
/// Cross-platform note (`elf-init-array-ctors`, PR #2246): the codec registers
/// via a program-start constructor, which on ELF must land in `.init_array`. A
/// successful cross-process send IS the proof the constructor ran and registered
/// the codec; macOS masked the pre-#2246 ELF defect, so this scenario must also
/// be validated on a real Linux host (done out-of-band).
#[test]
fn wire_cbor_cross_process_round_trip() {
    let stdout = run_two_process_scenario("wire_cbor");

    // Teeth: exact field values on the #[wire] struct reply for both the payload
    // variant (Move → tag 1, sum 7) and the unit variant (Ping → tag 0, sum 0).
    assert!(
        stdout.contains("PASS wire_cbor move-tag=1"),
        "expected Move variant tag 1 from the #[wire] reply; client stdout:\n{stdout}"
    );
    assert!(
        stdout.contains("PASS wire_cbor move-sum=7"),
        "expected Move x+y == 7 from the #[wire] struct field round-trip; client stdout:\n{stdout}"
    );
    assert!(
        stdout.contains("PASS wire_cbor ping-tag=0"),
        "expected Ping unit-variant tag 0 from the #[wire] reply; client stdout:\n{stdout}"
    );
    assert!(
        stdout.contains("PASS wire_cbor ping-sum=0"),
        "expected Ping sum 0 from the #[wire] reply; client stdout:\n{stdout}"
    );
    // Negative guard: no FAIL line may appear on a successful round-trip.
    assert!(
        !stdout.contains("FAIL "),
        "client reported a FAIL on the #[wire] cross-process round-trip; client stdout:\n{stdout}"
    );
}

/// Cross-process proof: when the watcher node dies (client process killed),
/// the target-side `RemoteWatcher` entries it registered on the server are
/// pruned from the `targets` map, keeping the table bounded.
///
/// The server polls `hew_dist_monitor_remote_watcher_count()` until the count
/// drops to 0 (the prune fired) and prints `PASS monitor_watcher_node_death
/// pruned=<N>`. The harness asserts that PASS line with an exact count, that the
/// target actor itself is NOT reported as DOWN (the watcher dying must not affect
/// the watched actor's liveness), and that no FAIL line appears.
///
/// Teeth: the server-side assertion is an exact count (pruned > 0 would pass over
/// a table that never grew); the wait loop has a bounded timeout that fails if the
/// prune never fires; the no-FAIL guard catches any scenario-level error.
#[test]
fn monitor_watcher_node_death_prunes_target_table() {
    let server_out = run_watcher_drop_scenario("monitor_watcher_node_death");

    // The server must print PASS with an exact non-zero pruned count — the exact
    // integer after "pruned=" is the number of RemoteWatcher entries that were
    // present before the kill and then removed.
    assert!(
        server_out.contains("PASS monitor_watcher_node_death pruned="),
        "server must print PASS monitor_watcher_node_death pruned=<n>; \
         server out:\n{server_out}"
    );
    // The pruned count must be > 0 (the watcher DID register at least one entry).
    let pruned_count: i64 = server_out
        .lines()
        .find(|l| l.contains("PASS monitor_watcher_node_death pruned="))
        .and_then(|l| l.split("pruned=").nth(1))
        .and_then(|s| s.trim().parse().ok())
        .unwrap_or(0);
    assert!(
        pruned_count > 0,
        "pruned count must be > 0 (watcher registered at least one entry); \
         server out:\n{server_out}"
    );
    // No FAIL line — no scenario-level error.
    assert!(
        !server_out.contains("FAIL "),
        "server reported a FAIL on the watcher-node-death scenario; \
         server out:\n{server_out}"
    );
}

/// Cross-node link headline: a `CrashLinked` cross-node link crashes the LOCAL linked
/// actor when the remote actor cleanly exits. The client spawns a linker that
/// `link_remote(kv, CrashLinked)`s the remote actor, then tells the remote to
/// stop. The cross-node link-down lands in the linker's MAILBOX as a
/// `SYS_MSG_EXIT` and crashes it (terminal Crashed == 5) — NOT a monitor recv
/// slot. The fixture polls the linker's terminal state (probe ledger) and
/// asserts it reached Crashed.
#[test]
fn link_remote_crash_cascade_on_clean_exit() {
    let stdout = run_link_cascade_scenario("link_remote_clean_exit");
    assert!(
        stdout.contains("PASS link_remote_clean_exit linker-crashed reason=5"),
        "expected the local linked actor to crash (Crashed == 5) when its remote \
         peer cleanly exits under CrashLinked; client stdout:\n{stdout}"
    );
    assert!(
        !stdout.contains("FAIL "),
        "client reported a FAIL on the link clean-exit cascade; client stdout:\n{stdout}"
    );
}

/// A `CrashLinked` cross-node link crashes the LOCAL linked actor when the remote
/// actor CRASHES (panics). Same mailbox-EXIT cascade as the clean-exit case; the
/// terminal cause differs but the linked actor still crashes.
#[test]
fn link_remote_crash_cascade_on_crash() {
    let stdout = run_link_cascade_scenario("link_remote_crash");
    assert!(
        stdout.contains("PASS link_remote_crash linker-crashed reason=5"),
        "expected the local linked actor to crash when its remote peer crashes \
         under CrashLinked; client stdout:\n{stdout}"
    );
    assert!(
        !stdout.contains("FAIL "),
        "client reported a FAIL on the link crash cascade; client stdout:\n{stdout}"
    );
}

/// A `CrashLinked` cross-node link crashes the LOCAL linked actor on a PARTITION
/// (the server process is killed, dropping the connection). The death-signal
/// must fire on the partition terminal cause too — firing only on a clean exit
/// would fail-open (a linked actor surviving its dead peer).
#[test]
fn link_remote_crash_cascade_on_partition() {
    let stdout =
        run_conn_drop_scenario_with_env("link_remote_partition", &[("HEW_LINK_PROBE", "1")]);
    assert!(
        stdout.contains("PASS link_remote_partition linker-crashed reason=5"),
        "expected the local linked actor to crash on a partition under CrashLinked; \
         client stdout:\n{stdout}"
    );
    assert!(
        !stdout.contains("FAIL "),
        "client reported a FAIL on the link partition cascade; client stdout:\n{stdout}"
    );
}

/// A non-`CrashLinked` link policy (`MonitorLost`) must NOT crash the linked
/// actor when the remote dies — the policy discriminator has teeth. After the
/// remote cleanly exits, the linker must STILL answer a liveness ask. A "crash
/// on every death regardless of policy" implementation fails this test.
#[test]
fn link_remote_non_crashlinked_policy_no_crash() {
    let stdout = run_link_cascade_scenario("link_remote_non_crashlinked");
    assert!(
        stdout.contains("PASS link_remote_non_crashlinked linker-survived"),
        "expected the linked actor to SURVIVE a remote death under a non-CrashLinked \
         policy; client stdout:\n{stdout}"
    );
    assert!(
        !stdout.contains("FAIL "),
        "client reported a FAIL on the non-CrashLinked policy scenario; client stdout:\n{stdout}"
    );
}

/// Cross-node demonitor reclamation: when a client closes a cross-node
/// `MonitorRef`, the close routes to the cross-node teardown (not the local-table
/// no-op), sending `CTRL_DEMONITOR` to the peer. The server observes its
/// target-side watcher count rise then fall and prints the verdict with an exact
/// reclaimed count.
#[test]
fn cross_node_monitor_close_reclaims_watcher_entry() {
    let server_out = run_server_verdict_scenario("cross_node_monitor_close_reclaim");
    assert!(
        server_out.contains("PASS cross_node_monitor_close_reclaim reclaimed="),
        "server must print PASS cross_node_monitor_close_reclaim reclaimed=<n> after \
         the client closes its cross-node monitor; server out:\n{server_out}"
    );
    let reclaimed: i64 = server_out
        .lines()
        .find(|l| l.contains("PASS cross_node_monitor_close_reclaim reclaimed="))
        .and_then(|l| l.split("reclaimed=").nth(1))
        .and_then(|s| s.trim().parse().ok())
        .unwrap_or(0);
    assert!(
        reclaimed > 0,
        "reclaimed count must be > 0 (the monitor DID register an entry that was \
         then reclaimed on close); server out:\n{server_out}"
    );
    assert!(
        !server_out.contains("FAIL "),
        "server reported a FAIL on the cross-node demonitor reclamation; \
         server out:\n{server_out}"
    );
}

/// Cross-process rejoin-admission proof: a buried peer cannot be resurrected by a
/// stale (`<=`-incarnation) ALIVE gossip replay, but a strictly-higher-incarnation
/// rejoin IS admitted and evicts the quarantine entry. The client drives the
/// admission gate through the compiled runtime via injected SWIM gossip frames
/// (deterministic — no failure-detector race), proving the boundary across the
/// whole frontend → codegen → runtime stack.
///
/// The teeth: BOTH `PASS quarantine_rejoin stale-rejected` (the resurrection
/// guard — `resurrected-by-stale` must NEVER fire) AND
/// `PASS quarantine_rejoin readmitted-at=6` (the positive readmission) must
/// appear, and no `FAIL` line. A no-op admission gate that let the stale ALIVE
/// through would emit `FAIL quarantine_rejoin resurrected-by-stale`.
#[test]
fn quarantine_rejoin_stale_rejected_higher_readmitted() {
    let stdout = run_two_process_scenario("quarantine_rejoin");
    assert!(
        stdout.contains("PASS quarantine_rejoin stale-rejected"),
        "a stale (<=-incarnation) ALIVE gossip must NOT resurrect a buried peer; \
         client stdout:\n{stdout}"
    );
    assert!(
        stdout.contains("PASS quarantine_rejoin readmitted-at=6"),
        "a strictly-higher-incarnation rejoin must be admitted and evict the \
         quarantine entry; client stdout:\n{stdout}"
    );
    assert!(
        !stdout.contains("FAIL "),
        "client reported a FAIL on the rejoin-admission proof; client stdout:\n{stdout}"
    );
}
