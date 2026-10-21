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
//! * The server binds an ephemeral loopback port and publishes the port it got
//!   in the shared key-exchange directory, where the client reads it; no port
//!   is chosen ahead of the bind, so no other socket can take it in between.
//! * The server prints `READY <port>` only after its actor is registered; the
//!   harness blocks on that line before launching the client (readiness signal,
//!   not a fixed sleep).
//! * The client receives ordinary actor values from authenticated peer offers.
//!   Registry lookup supplies discovery metadata and never callable authority.

mod support;

use std::io::{BufRead, BufReader, Read};
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

/// The shared distributed fixture is substantially larger than ordinary E2E
/// programs. Allow a cold native compile to finish on slower hosted runners;
/// nextest's per-test ceiling remains the independent hang watchdog.
const DIST_NODE_COMPILE_TIMEOUT: Duration = Duration::from_mins(2);

#[derive(Clone, Copy)]
enum StderrExpectation {
    Ignore,
    ExactOneCrashIn(&'static str),
    NoCrashIn(&'static str),
    NoFaultDiagnostics,
}

fn assert_stderr_expectation(stderr: &str, expectation: StderrExpectation) {
    match expectation {
        StderrExpectation::Ignore => {}
        StderrExpectation::NoFaultDiagnostics => {
            assert!(
                !stderr.lines().any(|line| {
                    line.starts_with("hew: failure:")
                        || line.starts_with("hew: secondary failure:")
                        || line.starts_with("hew: actor crash")
                }),
                "remote observation must not report the origin fault again:\n{stderr}"
            );
        }
        StderrExpectation::ExactOneCrashIn(context) => {
            let prefix = format!("hew: actor crash in {context} (actor ");
            let diagnostics: Vec<_> = stderr
                .lines()
                .filter(|line| line.starts_with(&prefix))
                .collect();
            assert_eq!(
                diagnostics.len(),
                1,
                "expected exactly one current-format crash diagnostic in {context}; stderr:\n{stderr}"
            );
            let identity = diagnostics[0]
                .strip_prefix(&prefix)
                .and_then(|tail| tail.split_once("): "))
                .map(|(actor_id, reason)| {
                    assert!(
                        !reason.is_empty(),
                        "crash diagnostic must include a reason; stderr:\n{stderr}"
                    );
                    actor_id
                })
                .expect("crash diagnostic must contain `(actor <id>): <reason>`");
            assert!(
                identity.parse::<u64>().is_ok_and(|actor_id| actor_id != 0),
                "crash diagnostic must include a non-zero local actor identity; stderr:\n{stderr}"
            );
        }
        StderrExpectation::NoCrashIn(context) => {
            let prefix = format!("hew: actor crash in {context}");
            assert!(
                !stderr.lines().any(|line| line.starts_with(&prefix)),
                "did not expect a crash diagnostic in {context}; stderr:\n{stderr}"
            );
        }
    }
}

struct CompiledNodeBinary {
    _emit_dir: tempfile::TempDir,
    path: PathBuf,
}

/// Compile `dist_node.hew` exactly once per test binary and cache the resulting
/// native executable path. All scenario tests in this file share the artifact.
fn compiled_node_binary() -> &'static Path {
    static NODE_BINARY: OnceLock<CompiledNodeBinary> = OnceLock::new();
    &NODE_BINARY
        .get_or_init(|| {
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

            let emit_dir = tempfile::tempdir().expect("create dist_node compile dir");
            let emit_path = emit_dir.path().to_path_buf();

            let compiler =
                std::env::var_os("HEW_DIST_E2E_HEW_BIN").map_or_else(hew_binary, PathBuf::from);
            let mut command = Command::new(compiler);
            command.arg("compile");
            let sanitize = std::env::var("HEW_SANITIZE_ADDRESS").is_ok_and(|value| value == "1");
            if sanitize {
                command.arg("--emit-llvm");
            }
            // The same scenarios prove the optimized build when requested.
            if let Ok(level) = std::env::var("HEW_DIST_E2E_OPT_LEVEL") {
                command.arg("--opt-level").arg(level);
            }
            command
                .arg("--emit-dir")
                .arg(&emit_path)
                .arg(&source)
                .env("HEW_STD", repo_root().join("std"))
                .current_dir(repo_root());
            let output = support::try_run_bounded_command(
                command,
                format!("hew compile {}", source.display()),
                DIST_NODE_COMPILE_TIMEOUT,
            )
            .unwrap_or_else(|error| panic!("{error}"));
            assert!(
                output.status.success(),
                "compiling dist_node.hew failed\n{}",
                support::describe_output(&output)
            );

            let binary = hew_testutil::compiled_binary_path(&emit_path, "dist_node");
            assert!(
                binary.is_file(),
                "compiled dist_node binary missing at {}",
                binary.display()
            );
            if sanitize {
                let ir = std::fs::read_to_string(binary.with_extension("ll"))
                    .expect("read instrumented distributed LLVM IR");
                assert!(ir.contains("sanitize_address"));
                assert!(ir.contains("call void @__asan_init()"));
            }
            CompiledNodeBinary {
                _emit_dir: emit_dir,
                path: binary,
            }
        })
        .path
}

/// A child process whose stdout/stderr are captured and that is force-killed on
/// drop, so a wedged node never leaks past the test.
struct ManagedChild {
    child: Child,
    label: String,
}

impl Drop for ManagedChild {
    fn drop(&mut self) {
        // Best-effort force-kill; the server's happy path is to be killed here
        // after the client finishes (it otherwise sleeps to a safety ceiling).
        let _ = self.child.kill();
        let _ = self.child.wait();
        if std::thread::panicking() {
            if let Some(mut stderr) = self.child.stderr.take() {
                let mut diagnostics = String::new();
                let _ = stderr.read_to_string(&mut diagnostics);
                eprintln!("{} teardown stderr:\n{diagnostics}", self.label);
            }
        }
    }
}

struct PeerLink {
    port: u16,
    stopped: std::sync::Arc<std::sync::atomic::AtomicBool>,
    worker: Option<std::thread::JoinHandle<()>>,
}

impl PeerLink {
    fn new(directory: &Path) -> Self {
        use std::net::{Shutdown, TcpListener, TcpStream};
        use std::sync::{atomic::Ordering, Arc};
        let listener = TcpListener::bind("127.0.0.1:0").expect("bind peer gate");
        let port = listener.local_addr().unwrap().port();
        listener.set_nonblocking(true).unwrap();
        let directory = directory.to_owned();
        let stopped = Arc::new(std::sync::atomic::AtomicBool::new(false));
        let stop = Arc::clone(&stopped);
        let worker = std::thread::spawn(move || {
            let mut sockets: Vec<TcpStream> = Vec::new();
            let mut transfers = Vec::new();
            while !stop.load(Ordering::Acquire) {
                if directory.join("service.disconnect").exists()
                    && !directory.join("service.reconnect").exists()
                {
                    for socket in sockets.drain(..) {
                        let _ = socket.shutdown(Shutdown::Both);
                    }
                    std::fs::write(directory.join("service.disconnected"), "1").unwrap();
                    std::thread::sleep(Duration::from_millis(5));
                    continue;
                }
                match listener.accept() {
                    Ok((client, _)) => {
                        // Windows inherits the listener's nonblocking mode;
                        // the forwarding threads use blocking copy loops.
                        client
                            .set_nonblocking(false)
                            .expect("blocking peer gate stream");
                        let server_port = std::fs::read_to_string(directory.join("server.ready"))
                            .expect("published server port")
                            .parse::<u16>()
                            .unwrap();
                        let address = std::net::SocketAddr::from((
                            std::net::Ipv4Addr::LOCALHOST,
                            server_port,
                        ));
                        let Ok(server) =
                            TcpStream::connect_timeout(&address, Duration::from_millis(500))
                        else {
                            continue;
                        };
                        sockets.extend([client.try_clone().unwrap(), server.try_clone().unwrap()]);
                        for (mut input, mut output) in [
                            (client.try_clone().unwrap(), server.try_clone().unwrap()),
                            (server, client),
                        ] {
                            transfers.push(std::thread::spawn(move || {
                                let _ = std::io::copy(&mut input, &mut output);
                                let _ = output.shutdown(Shutdown::Write);
                            }));
                        }
                    }
                    Err(error) if error.kind() == std::io::ErrorKind::WouldBlock => {
                        std::thread::sleep(Duration::from_millis(5));
                    }
                    Err(error) => panic!("peer gate accept: {error}"),
                }
            }
            for socket in sockets {
                let _ = socket.shutdown(Shutdown::Both);
            }
            for transfer in transfers {
                transfer.join().expect("peer gate transfer");
            }
        });
        Self {
            port,
            stopped,
            worker: Some(worker),
        }
    }
}

impl Drop for PeerLink {
    fn drop(&mut self) {
        self.stopped
            .store(true, std::sync::atomic::Ordering::Release);
        self.worker
            .take()
            .unwrap()
            .join()
            .expect("peer gate worker");
    }
}

/// Nodes share one installed cluster authority, with separate stable transport
/// identities and enrolments. Session journals remain owned by each node.
struct SecureScenario {
    kx_dir: tempfile::TempDir,
    mesh: bool,
}

impl SecureScenario {
    fn new() -> Self {
        Self::with_transport(false)
    }

    fn with_transport(mesh: bool) -> Self {
        use hew_runtime::{cluster_admission, encryption, peer_binding::PeerCredential};
        let kx_dir = tempfile::tempdir().expect("create cluster directory");
        let authority = kx_dir.path().join("authority.key");
        let foreign = kx_dir.path().join("foreign.key");
        cluster_admission::authority_identity(&authority).expect("create authority");
        cluster_admission::authority_identity(&foreign).expect("create foreign authority");
        for role in ["server", "client"] {
            let key = kx_dir.path().join(format!("{role}.key"));
            let credential = if mesh {
                let identity = hew_runtime::quic_mesh::mesh_identity_load_or_create(&key)
                    .expect("create mesh identity");
                PeerCredential::Spki(identity.spki().to_vec())
            } else {
                let identity =
                    encryption::noise_identity_load_or_create(&key).expect("create Noise identity");
                PeerCredential::NoiseKey(identity.public())
            };
            let text = match &credential {
                PeerCredential::NoiseKey(key) => hew_runtime::peer_binding::hex_lower(key),
                PeerCredential::Spki(spki) => hew_runtime::peer_binding::hex_lower(spki),
            };
            std::fs::write(kx_dir.path().join(format!("{role}.pub")), text)
                .expect("publish public credential");
            for (issuer, suffix) in [(&authority, "enrollment"), (&foreign, "foreign.enrollment")] {
                let bundle =
                    cluster_admission::issue(issuer, &credential).expect("issue enrolment");
                cluster_admission::write_bundle(
                    &kx_dir.path().join(format!("{role}.{suffix}")),
                    &bundle,
                )
                .expect("install enrolment");
            }
        }
        Self { kx_dir, mesh }
    }

    /// Spawn one node process with the strict-binding environment, shared
    /// key-exchange directory, and fixture role and scenario.
    fn spawn(&self, role: &str, scenario: &str, extra_env: &[(&str, &str)]) -> ManagedChild {
        let binary = compiled_node_binary();
        let mut command = Command::new(binary);
        command
            .env(
                "HEW_DIST_TRANSPORT",
                if self.mesh { "quic-mesh" } else { "tcp" },
            )
            .env("HEW_DIST_ROLE", role)
            .env("HEW_DIST_SCENARIO", scenario)
            .env("HEW_DIST_KX_DIR", self.kx_dir.path())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());
        for (key, value) in extra_env {
            command.env(key, value);
        }
        let child = command
            .spawn()
            .unwrap_or_else(|error| panic!("failed to spawn dist_node {role}: {error}"));
        ManagedChild {
            child,
            label: role.to_string(),
        }
    }

    fn spawn_server(&self, scenario: &str) -> ManagedChild {
        self.spawn("server", scenario, &[])
    }

    fn spawn_server_with_env(&self, scenario: &str, extra_env: &[(&str, &str)]) -> ManagedChild {
        self.spawn("server", scenario, extra_env)
    }

    fn spawn_client(&self, scenario: &str, extra_env: &[(&str, &str)]) -> ManagedChild {
        self.spawn("client", scenario, extra_env)
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
fn run_client_to_completion(
    mut client: ManagedChild,
    expected_exit_code: i32,
    stderr_expectation: StderrExpectation,
) -> String {
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
                assert_eq!(
                    status.code(),
                    Some(expected_exit_code),
                    "client should exit {expected_exit_code} ({status:?})\nstdout:\n{stdout}\nstderr:\n{stderr}"
                );
                assert_stderr_expectation(&stderr, stderr_expectation);
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
    let scene = SecureScenario::new();

    // Both processes load their operator-provisioned identity and enrolment.
    let mut server = scene.spawn_server(scenario);
    let client = scene.spawn_client(scenario, &[]);

    let _server_stdout = wait_for_server_ready(&mut server);
    let stdout = run_client_to_completion(client, 0, StderrExpectation::Ignore);

    // `server` is dropped here, force-killing the still-sleeping server.
    drop(server);
    stdout
}

fn run_handshake_rejection_scenario() -> String {
    let scene = SecureScenario::new();
    let mut server =
        scene.spawn_server_with_env("handshake_reject", &[("HEW_DIST_FOREIGN_CLUSTER", "1")]);
    let client = scene.spawn_client("handshake_reject", &[]);

    let _server_stdout = wait_for_server_ready(&mut server);
    let stdout = run_client_to_completion(client, 0, StderrExpectation::Ignore);
    drop(server);
    stdout
}

/// Run a cross-node link cascade scenario where the REMOTE actor dies on its own
/// (clean exit or crash) and the client polls its local linker's terminal state.
/// The client runs with `HEW_LINK_PROBE=1` so the runtime records actor terminal
/// reasons in the probe ledger the fixture reads. Returns the client's stdout.
fn run_link_cascade_scenario(
    scenario: &str,
    expected_exit_code: i32,
    stderr_expectation: StderrExpectation,
) -> String {
    let scene = SecureScenario::new();

    // Concurrent launch for the mutual credential exchange (see
    // `run_two_process_scenario`).
    let mut server = scene.spawn_server(scenario);
    let client = scene.spawn_client(scenario, &[("HEW_LINK_PROBE", "1")]);

    let _server_stdout = wait_for_server_ready(&mut server);
    let stdout = run_client_to_completion(client, expected_exit_code, stderr_expectation);

    drop(server);
    stdout
}

fn run_setup_exit_race_scenario(stderr_expectation: StderrExpectation) -> String {
    let scene = SecureScenario::new();
    let mut server = scene.spawn_server_with_env(
        "remote_setup_exit_race",
        &[("HEW_DIST_SETUP_RACE_PROBE", "1")],
    );
    let client = scene.spawn_client("remote_setup_exit_race", &[("HEW_LINK_PROBE", "1")]);

    let _server_stdout = wait_for_server_ready(&mut server);
    let stdout = run_client_to_completion(client, 1, stderr_expectation);
    drop(server);
    stdout
}

/// Run the server + client pair for a connection-drop scenario: the client
/// prints `READY_DROP <name>` once its monitor is registered, the harness then
/// kills the server process (dropping the connection mid-scenario), and the
/// client runs to completion observing the connection-drop DOWN. Returns the
/// client's captured stdout.
fn run_conn_drop_scenario(scenario: &str) -> String {
    run_conn_drop_scenario_with_env(scenario, &[], 0, StderrExpectation::Ignore)
}

/// As [`run_conn_drop_scenario`], with extra client environment (e.g.
/// `HEW_LINK_PROBE=1` for the cross-node link partition cascade).
fn run_conn_drop_scenario_with_env(
    scenario: &str,
    client_env: &[(&str, &str)],
    expected_exit_code: i32,
    stderr_expectation: StderrExpectation,
) -> String {
    let scene = SecureScenario::new();

    // Concurrent launch for the mutual credential exchange (see
    // `run_two_process_scenario`); the client then waits on the server's
    // listener-liveness file before it connects.
    let mut server = scene.spawn_server(scenario);
    let mut client = scene.spawn_client(scenario, client_env);

    // Confirm the server reached READY (it has the client's credential and is
    // listening) before we read the client's READY_DROP handshake.
    let _server_stdout = wait_for_server_ready(&mut server);

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
    let mut stderr = String::new();
    if let Some(mut err) = client.child.stderr.take() {
        let _ = err.read_to_string(&mut stderr);
    }
    assert_eq!(
        status.code(),
        Some(expected_exit_code),
        "client should exit {expected_exit_code} ({status:?}) on connection-drop scenario\nstdout:\n{captured}\nstderr:\n{stderr}"
    );
    assert_stderr_expectation(&stderr, stderr_expectation);
    captured
}

/// Run the server + client pair for a watcher-node-death scenario: the client
/// (the watcher) prints `READY_CLIENT_WATCHER <name>` once its monitor is
/// registered, the harness then kills the CLIENT process (simulating watcher-
/// node death), and the SERVER runs to completion observing the prune and
/// printing its assertion. Returns the server's captured stdout.
fn run_watcher_drop_scenario(scenario: &str) -> String {
    let scene = SecureScenario::new();

    // Concurrent launch for the mutual credential exchange (see
    // `run_two_process_scenario`): the server needs the client's Noise
    // credential to reach READY, so both must be live before we read READY.
    let mut server = scene.spawn_server(scenario);
    let mut client = scene.spawn_client(scenario, &[]);

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

    // The client (the watcher) was launched concurrently; block until it signals
    // its monitor is live so the harness knows the watch-side entry is
    // registered.
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
    let scene = SecureScenario::new();

    // Concurrent launch for the mutual credential exchange (see
    // `run_two_process_scenario`).
    let mut server = scene.spawn_server(scenario);
    let client = scene.spawn_client(scenario, &[]);

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

    // The client was launched concurrently; it registers, closes its monitor,
    // and exits cleanly on its own (it is not killed).

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

fn run_stale_incarnation_restart_scenario() -> String {
    let scene = SecureScenario::new();
    let mut first_server = scene.spawn_server("stale_incarnation_restart");
    let mut client = scene.spawn_client("stale_incarnation_restart", &[]);

    let _first_server_stdout = wait_for_server_ready(&mut first_server);
    let client_stdout = client
        .child
        .stdout
        .take()
        .expect("client stdout was captured");
    let mut client_reader = BufReader::new(client_stdout);
    let mut captured = String::new();
    let capture_deadline = Instant::now() + CLIENT_RUN_TIMEOUT;
    loop {
        assert!(
            Instant::now() < capture_deadline,
            "client did not capture the first-session pid within {CLIENT_RUN_TIMEOUT:?}"
        );
        let mut line = String::new();
        match client_reader.read_line(&mut line) {
            Ok(0) => {
                panic!("client stdout closed before READY_STALE_CAPTURED; captured:\n{captured}")
            }
            Ok(_) => {
                captured.push_str(&line);
                if line.contains("READY_STALE_CAPTURED ") {
                    break;
                }
            }
            Err(error) => panic!("error reading stale-incarnation client stdout: {error}"),
        }
    }

    first_server
        .child
        .kill()
        .expect("kill first-session server");
    first_server
        .child
        .wait()
        .expect("wait for first-session server");

    let mut replacement =
        scene.spawn_server_with_env("stale_incarnation_restart", &[("HEW_DIST_RESTARTED", "1")]);
    let _replacement_stdout = wait_for_server_ready(&mut replacement);

    let drain_deadline = Instant::now() + CLIENT_RUN_TIMEOUT;
    loop {
        let mut line = String::new();
        match client_reader.read_line(&mut line) {
            Ok(0) => break,
            Ok(_) => captured.push_str(&line),
            Err(error) => panic!("error draining stale-incarnation client stdout: {error}"),
        }
        assert!(
            Instant::now() < drain_deadline,
            "client did not finish stale-incarnation proof within {CLIENT_RUN_TIMEOUT:?}"
        );
    }

    let status = client
        .child
        .wait()
        .expect("wait for stale-incarnation client");
    let mut stderr = String::new();
    if let Some(mut err) = client.child.stderr.take() {
        let _ = err.read_to_string(&mut stderr);
    }
    assert!(
        status.success(),
        "stale-incarnation client exited non-zero ({status:?})\nstdout:\n{captured}\nstderr:\n{stderr}"
    );

    drop(replacement);
    captured
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
        stdout.contains("PASS remote_monitor_clean_exit monitor-id-match="),
        "DOWN monitor id must match MonitorRef::id(); client stdout:\n{stdout}"
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
        stdout.contains("PASS remote_monitor_crash monitor-id-match="),
        "DOWN monitor id must match MonitorRef::id(); client stdout:\n{stdout}"
    );
    assert!(
        !stdout.contains("FAIL "),
        "client reported a FAIL on the crash monitor; client stdout:\n{stdout}"
    );
}

/// The target holds state cleanup until the origin sees the remote stopped
/// registration. A second call observes the completed actor through the same
/// authenticated grant, without replacing its original crash with cleanup failure.
fn assert_remote_stopped_crash(fail_close: bool) {
    let scenario = if fail_close {
        "remote_stopped_cleanup_fault"
    } else {
        "remote_stopped_crash"
    };
    let scene = SecureScenario::new();
    let mut server = scene.spawn_server(scenario);
    let client = scene.spawn_client(scenario, &[]);
    let _server_stdout = wait_for_server_ready(&mut server);
    let stdout = run_client_to_completion(client, 0, StderrExpectation::NoFaultDiagnostics);
    for pass in 0..2 {
        assert!(
            stdout.contains(&format!(
                "PASS remote_stopped pass={pass} cleanup={fail_close}"
            )),
            "live and late stopped must distinguish cleanup from original crash:\n{stdout}"
        );
    }
    // The client acknowledges both observations before the origin shuts down.
    // Waiting for process completion also makes the diagnostic count definitive.
    let deadline = Instant::now() + CLIENT_RUN_TIMEOUT;
    loop {
        if let Some(status) = server.child.try_wait().expect("poll origin") {
            assert_eq!(
                status.code(),
                Some(1),
                "original crash must remain unresolved"
            );
            break;
        }
        assert!(
            Instant::now() < deadline,
            "origin did not finish after observations"
        );
        std::thread::sleep(Duration::from_millis(10));
    }
    let mut stderr = String::new();
    server
        .child
        .stderr
        .take()
        .unwrap()
        .read_to_string(&mut stderr)
        .unwrap();
    assert_stderr_expectation(
        &stderr,
        StderrExpectation::ExactOneCrashIn("RemoteClosingActor.crash"),
    );
    assert_eq!(
        stderr.matches("remote original handler fault").count(),
        1,
        "{stderr}"
    );
    assert_eq!(
        stderr.matches("remote secondary cleanup fault").count(),
        usize::from(fail_close),
        "{stderr}"
    );
}

#[test]
fn remote_stopped_preserves_original_crash_without_cleanup_failure() {
    assert_remote_stopped_crash(false);
}

#[test]
fn remote_stopped_recovers_secondary_cleanup_fault_live_and_late() {
    assert_remote_stopped_crash(true);
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
        stdout.contains("PASS remote_monitor_conn_drop monitor-id-match="),
        "DOWN monitor id must match MonitorRef::id(); client stdout:\n{stdout}"
    );
    assert!(
        !stdout.contains("FAIL "),
        "client reported a FAIL on the connection-drop monitor; client stdout:\n{stdout}"
    );
}

/// Cross-node monitor setup after route teardown is not an error: it delivers
/// an immediate `MonitorLost` DOWN, never an inert `MonitorRef`.
#[test]
fn remote_monitor_setup_after_drop_delivers_lost_down() {
    let stdout = run_conn_drop_scenario("remote_monitor_setup_after_drop");
    assert!(
        stdout.contains("PASS remote_monitor_setup_after_drop down=MonitorLost"),
        "expected a MonitorLost DOWN for setup after route teardown; client stdout:\n{stdout}"
    );
    assert!(
        !stdout.contains("FAIL "),
        "client reported a FAIL on monitor setup after drop; client stdout:\n{stdout}"
    );
}

/// A target that exits after the request reaches its owner but before target-side
/// registration completes must resolve setup terminally. The monitor receives
/// one immediate DOWN and the linked actor's local monitor receives one DOWN
/// from the link cascade; neither setup may hang or duplicate delivery.
#[test]
fn remote_monitor_and_link_target_exit_during_setup_deliver_once() {
    let stdout = run_setup_exit_race_scenario(StderrExpectation::ExactOneCrashIn("Linker"));
    assert!(
        stdout.contains("PASS remote_setup_exit_race_monitor reason=6")
            && stdout.contains("PASS remote_setup_exit_race_monitor no-dup")
            && stdout.contains("PASS remote_setup_exit_race link-down=5")
            && stdout.contains("PASS remote_setup_exit_race link-no-dup"),
        "monitor/link setup races must terminate exactly once; client stdout:\n{stdout}"
    );
    assert!(
        !stdout.contains("FAIL "),
        "client reported a FAIL on the setup-exit race; client stdout:\n{stdout}"
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
/// Teeth that keep this meaningful: (a) a `PASS partition_ask probe-partition`
/// line — the fixture's phase A stalls a reply past the ask deadline while the
/// route is still LIVE, so the armed probe's fan-out is the only path that can
/// resolve the pending ask early; requiring `Partition` there proves the probe
/// FIRED (a sabotaged/no-op `hew_dist_partition_pending_remote_asks` times out
/// and fails this line — it cannot hide behind the kill's routing-failure /
/// socket-drop outcomes, which previously masked exactly that sabotage);
/// (b) a `PASS partition_ask reason=<cause>` line — phase B only emits one when
/// an `Err` came back BEFORE the ask's own deadline (a proactive fail-closed
/// verdict), never on a `Result::Ok` (which is rejected as `never-partitioned`);
/// (c) the run finishes well under the 40 s client ceiling —
/// `run_conn_drop_scenario`'s drain cap is the no-hang guard, and a
/// hang-to-deadline would blow it; (d) no `FAIL` line — in particular no
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
    // Arm the test-only partition probe on the client the harness spawns. The
    // probe (`hew_dist_partition_pending_remote_asks`) is inert by default — a
    // shipped libhew exports the symbol but it drains nothing unless
    // `HEW_DIST_TEST_PROBE=1` is set, so only this harness can drive the
    // deterministic fail-closed seam.
    let stdout = run_conn_drop_scenario_with_env(
        "partition_ask",
        &[("HEW_DIST_TEST_PROBE", "1")],
        0,
        StderrExpectation::Ignore,
    );

    // Phase A: the armed probe must PROVE it fired. The stalled ask is
    // resolvable only by the probe's Partition fan-out, so any other outcome
    // (timeout, reply, different cause) means the probe did not fire.
    assert!(
        stdout.contains("PASS partition_ask probe-partition"),
        "expected the armed partition probe to resolve the stalled ask with a \
         typed Partition (proof the fan-out fired); client stdout:\n{stdout}"
    );
    // Phase B: any typed fail-closed cause is correct; the fixture emits this
    // line only for an `Err` that resolved BEFORE the ask's own deadline (a
    // proactive verdict).
    assert!(
        stdout.contains("PASS partition_ask reason="),
        "expected a typed fail-closed PASS (the ask must resolve to a typed \
         ActorError before its deadline, not hang); client stdout:\n{stdout}"
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

#[test]
fn authenticated_service_grants_supply_and_revoke_exact_interface() {
    let scene = SecureScenario::new();
    let gate = PeerLink::new(scene.kx_dir.path());
    let mut server = scene.spawn_server("service_grants");
    let gate_port = gate.port.to_string();
    let client = scene.spawn_client("service_grants", &[("HEW_DIST_PROXY_PORT", &gate_port)]);
    let _server_stdout = wait_for_server_ready(&mut server);
    let stdout = run_client_to_completion(client, 0, StderrExpectation::Ignore);
    run_client_to_completion(server, 0, StderrExpectation::Ignore);
    assert!(
        stdout.contains(
            "PASS service_grants identity-equal independent-revocation rejected-request-retained"
        ),
        "{stdout}"
    );
    assert!(stdout.contains("PASS service_grants file=supplied-file sum=42 unit declared-failure incompatible generic-incompatible cancelled raw-metadata mailbox-acked-before-completion reconnect no-replay disconnected-revoke actor-restart node-restart"), "{stdout}");
}

/// The same authenticated grant follows one stable role through replacement,
/// while the admitted crash and its one-shot monitor retain the selected worker.
#[test]
fn stable_role_grant_survives_worker_restart_without_replaying_calls() {
    let stdout = run_two_process_scenario("stable_role");
    assert!(stdout.contains("PASS stable_role same-location same-grant one-shot-monitor no-replay replacement-call nested-owner-restart new-export owner-close"), "{stdout}");
    assert!(!stdout.contains("FAIL "), "{stdout}");
}

/// Frame-size refusals preserve unaccepted requests and healthy connections;
/// oversized replies report failure after exactly one handler execution.
#[test]
fn message_size_refusals_preserve_requests_and_connection() {
    let stdout = run_two_process_scenario("message_size");
    assert!(stdout.contains("PASS message_size local-large request-refused retained-request encoded-expansion reply-failed declared-error connection-survives no-replay"), "{stdout}");
    assert!(!stdout.contains("FAIL "), "{stdout}");
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
    assert!(
        stdout.contains("PASS remote_ask generic-identity"),
        "{stdout}"
    );
    assert!(
        stdout.contains("PASS remote_ask generic-point=3,4"),
        "{stdout}"
    );
    assert!(
        stdout.contains("PASS remote_ask specialized-echo"),
        "{stdout}"
    );
    // Negative guard: no FAIL line may appear on a successful round-trip.
    assert!(
        !stdout.contains("FAIL "),
        "client reported a FAIL on the remote-ask round-trip; client stdout:\n{stdout}"
    );
}

#[test]
fn remote_deadlines_withdraw_queued_work_and_preserve_claimed_handlers() {
    let stdout = run_two_process_scenario("deadline_budget");
    assert!(
        stdout.contains("PASS deadline_budget queued-not-started"),
        "{stdout}"
    );
    assert!(
        stdout.contains("PASS deadline_budget claimed-finishes-nested-times-out"),
        "{stdout}"
    );
    assert!(!stdout.contains("FAIL "), "{stdout}");
}

#[test]
fn pending_inbound_asks_preserve_other_services_and_retire_on_shutdown() {
    let stdout = run_two_process_scenario("inbound_isolation");
    for expected in [
        "PASS inbound_isolation healthy-with-64-pending",
        "PASS inbound_isolation 64-completed-1-refused",
        "PASS inbound_isolation trapped",
        "PASS inbound_isolation shutdown-retired-64",
    ] {
        assert!(stdout.contains(expected), "{stdout}");
    }
    assert!(!stdout.contains("FAIL "), "{stdout}");
}

/// Ordinary local actors operate beside a supplied actor from a live peer:
/// submission, completion, monitor DOWN and linked crash retain their semantics.
#[test]
fn local_actor_operations_compose_with_imported_peer() {
    // The linked crash leaves the client exiting nonzero.
    let stdout = run_link_cascade_scenario(
        "local_route",
        1,
        StderrExpectation::ExactOneCrashIn("Linker"),
    );
    for expected in [
        "PASS local_route local-ask=teal",
        "PASS local_route peer-ask=[]",
        "PASS local_route monitor-down=exited",
        "PASS local_route link-crash",
    ] {
        assert!(
            stdout.contains(expected),
            "expected `{expected}`; client stdout:\n{stdout}"
        );
    }
    assert!(
        !stdout.contains("FAIL "),
        "client reported a FAIL on the local-route scenario; client stdout:\n{stdout}"
    );
}

/// A valid v2 Noise handshake admits the key-derived peer identity and carries a
/// typed request/reply round trip; a credential enrolled under another root
/// remains unroutable.
#[test]
fn authenticated_handshake_rejects_foreign_cluster_authority() {
    let stdout = run_handshake_rejection_scenario();
    assert!(
        stdout.contains("PASS handshake_reject unroutable"),
        "foreign cluster authority must be rejected before routing; client stdout:\n{stdout}"
    );
    assert!(
        !stdout.contains("FAIL "),
        "client reported a FAIL on handshake rejection; client stdout:\n{stdout}"
    );
}

/// Both node transports enforce the same installed cluster root before calls.
#[test]
fn enrolled_mesh_peers_call_and_refuse_a_foreign_authority() {
    for scenario in ["remote_ask", "handshake_reject"] {
        let scene = SecureScenario::with_transport(true);
        let extra: &[(&str, &str)] = if scenario == "handshake_reject" {
            &[("HEW_DIST_FOREIGN_CLUSTER", "1")]
        } else {
            &[]
        };
        let mut server = scene.spawn_server_with_env(scenario, extra);
        let client = scene.spawn_client(scenario, &[]);
        let _server_stdout = wait_for_server_ready(&mut server);
        let stdout = run_client_to_completion(client, 0, StderrExpectation::Ignore);
        let expected = if scenario == "remote_ask" {
            "PASS remote_ask get-hello=world"
        } else {
            "PASS handshake_reject unroutable"
        };
        assert!(
            stdout.contains(expected) && !stdout.contains("FAIL "),
            "{stdout}"
        );
    }
}

/// Valid mesh frames larger than the initial reader buffer retain framing.
#[test]
fn mesh_large_requests_replies_and_declared_errors_preserve_following_frames() {
    let scene = SecureScenario::with_transport(true);
    let mut server = scene.spawn_server("mesh_large");
    let client = scene.spawn_client("mesh_large", &[]);
    let _server_stdout = wait_for_server_ready(&mut server);
    let stdout = run_client_to_completion(client, 0, StderrExpectation::Ignore);
    assert!(
        stdout.contains("PASS mesh_large request reply declared-error following-frame no-replay"),
        "{stdout}"
    );
    assert!(!stdout.contains("FAIL "), "{stdout}");
}

/// Node identity/location proof across two OS processes. The remote actor's
/// key-backed node id, complete location and session incarnation must match the registry
/// lookup exactly, and its node id must differ from the client's.
#[test]
fn node_identity_location_and_incarnation_cross_process() {
    let stdout = run_two_process_scenario("identity_location");
    let line = stdout
        .lines()
        .find(|line| line.starts_with("PASS identity_location "))
        .unwrap_or_else(|| panic!("missing identity PASS line; client stdout:\n{stdout}"));
    let field = |name: &str| -> &str {
        line.split_whitespace()
            .find_map(|part| part.strip_prefix(&format!("{name}=")))
            .unwrap_or_else(|| panic!("missing {name}= field in {line}"))
    };
    let local = field("local");
    let remote = field("remote");
    let parse_u64 = |name: &str| {
        field(name)
            .parse::<u64>()
            .unwrap_or_else(|error| panic!("invalid {name}= field in {line}: {error}"))
    };
    assert!(
        local.len() == 32 && local.bytes().all(|byte| byte.is_ascii_hexdigit()),
        "local node id must be 32 hexadecimal digits: {line}"
    );
    assert!(
        remote.len() == 32 && remote.bytes().all(|byte| byte.is_ascii_hexdigit()),
        "remote node id must be 32 hexadecimal digits: {line}"
    );
    assert_ne!(local, "00000000000000000000000000000000", "{line}");
    assert_ne!(remote, "00000000000000000000000000000000", "{line}");
    assert_ne!(
        local, remote,
        "two processes must have distinct node ids: {line}"
    );
    assert!(
        field("location").starts_with(remote),
        "location must carry its node identity: {line}"
    );
    assert_ne!(
        parse_u64("incarnation"),
        0,
        "session incarnation must be non-zero: {line}"
    );
    assert!(
        !stdout.contains("FAIL "),
        "client reported a FAIL on identity/location proof; client stdout:\n{stdout}"
    );
}

/// Re-pointing discovery and issuing another grant preserve the captured actor
/// identity and state while the newly supplied actor starts independently.
#[test]
fn captured_remote_ref_survives_name_repoint() {
    let stdout = run_two_process_scenario("repoint_preserves_ref");

    assert!(
        stdout.contains("PASS repoint_preserves_ref captured=live future=replacement"),
        "expected the captured ref to remain live while a future lookup selected \
         the replacement actor; client stdout:\n{stdout}"
    );
    assert!(
        !stdout.contains("FAIL "),
        "client reported a FAIL on the re-point scenario; client stdout:\n{stdout}"
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
/// Teeth: the server-side assertion requires exactly one registration; the wait
/// loop has a bounded timeout that fails if the
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
    assert_eq!(
        pruned_count, 1,
        "pruned count must be exactly 1; server out:\n{server_out}"
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
/// links the imported actor with `PartitionPolicy.CrashLinked`, then tells it to
/// stop. The cross-node link-down lands in the linker's MAILBOX as a
/// `HewSysMsg::Exit` and crashes it (terminal Crashed == 5) — NOT a monitor recv
/// slot. The fixture polls the linker's terminal state (probe ledger) and
/// asserts it reached Crashed.
#[test]
fn link_remote_crash_cascade_on_clean_exit() {
    let stdout = run_link_cascade_scenario(
        "link_remote_clean_exit",
        1,
        StderrExpectation::ExactOneCrashIn("Linker"),
    );
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

/// The typed hook retains the peer's complete Location and handles one crash.
#[test]
fn link_remote_typed_crash_preserves_origin_once() {
    let stdout = run_two_process_scenario("link_remote_typed_crash");
    assert!(
        stdout.contains("PASS link_remote_typed_crash remote-origin class=fault count=1"),
        "typed remote link notification failed: {stdout}"
    );
    assert!(!stdout.contains("FAIL "), "{stdout}");
}

/// A `CrashLinked` cross-node link crashes the LOCAL linked actor when the remote
/// actor CRASHES (panics). Same mailbox-EXIT cascade as the clean-exit case; the
/// terminal cause differs but the linked actor still crashes.
#[test]
fn link_remote_crash_cascade_on_crash() {
    let stdout = run_link_cascade_scenario(
        "link_remote_crash",
        1,
        StderrExpectation::ExactOneCrashIn("Linker"),
    );
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
    let stdout = run_conn_drop_scenario_with_env(
        "link_remote_partition",
        &[("HEW_LINK_PROBE", "1")],
        1,
        StderrExpectation::ExactOneCrashIn("Linker"),
    );
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
    let stdout = run_link_cascade_scenario(
        "link_remote_non_crashlinked",
        0,
        StderrExpectation::NoCrashIn("Linker"),
    );
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

/// A directly crashed local `Linker` uses the same native unwind diagnostic
/// funnel as a link EXIT, so the remote-link assertion cannot hide a path that
/// only reports cascaded faults.
#[test]
fn linker_local_crash_reports_once() {
    let stdout = run_link_cascade_scenario(
        "linker_local_crash",
        1,
        StderrExpectation::ExactOneCrashIn("Linker.crash_local"),
    );
    assert!(
        !stdout.contains("FAIL "),
        "client reported a FAIL on the local linker crash; client stdout:\n{stdout}"
    );
}

/// Cross-node demonitor reclamation: when a client closes a cross-node
/// `MonitorRef`, the close routes to the cross-node teardown (not the local-table
/// no-op), sending `CTRL_DEMONITOR` to the peer. The server derives the
/// registration from the monotonic `hew_dist_monitor_remote_watcher_registered_total()`
/// event counter (a sampled live count can miss the window under a coarse host
/// timer tick), waits for the live count to fall to zero, and prints the verdict
/// with an exact reclaimed count.
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
    assert_eq!(
        reclaimed, 1,
        "reclaimed count must be exactly 1; server out:\n{server_out}"
    );
    assert!(
        !server_out.contains("FAIL "),
        "server reported a FAIL on the cross-node demonitor reclamation; \
         server out:\n{server_out}"
    );
}

/// Repeated cross-node monitor setup/close keeps both authorities exact: every
/// handle id is distinct, close delivers no DOWN, and both watcher- and
/// target-side tables finish at zero. The server proves each cycle registered
/// via the monotonic `hew_dist_monitor_remote_watcher_registered_total()` event
/// counter (exactly one registration per cycle) — sampling the transient live
/// count phase-locks with the client's short holds under a coarse host timer
/// tick and can miss every window.
#[test]
fn repeated_cross_node_monitor_close_keeps_tables_empty() {
    let server_out = run_server_verdict_scenario("monitor_leak_low");
    assert!(
        server_out.contains("PASS monitor_leak_low iterations=3 target=0 max=1"),
        "server must observe one live registration at a time and exact zero cleanup; \
         server out:\n{server_out}"
    );
    assert!(
        !server_out.contains("FAIL "),
        "server reported a FAIL on repeated monitor cleanup; server out:\n{server_out}"
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

/// An imported actor is fenced across a same-key process restart. The
/// replacement has the same key-derived `NodeId` and a strictly higher durable
/// session incarnation; its fresh grant works while the captured grant fails. The
/// runtime's stale-reference refusal reaches the caller as `ActorError.Dead`:
/// the incarnation it named is gone and will not come back.
#[test]
fn imported_actor_rejected_after_same_key_restart() {
    let stdout = run_stale_incarnation_restart_scenario();
    assert!(
        stdout.contains("PASS stale_incarnation_restart old=")
            && stdout.contains(" new=")
            && stdout.contains(" error=Dead"),
        "same-key restart must admit only the higher incarnation and reject the \
         captured actor as Dead; client stdout:\n{stdout}"
    );
    assert!(
        !stdout.contains("FAIL "),
        "client reported a FAIL on stale-incarnation fencing; client stdout:\n{stdout}"
    );
}

/// A discovery location cannot be promoted to callable or lifecycle authority.
#[test]
fn discovery_locations_cannot_authorize_actor_operations() {
    let directory = tempfile::tempdir().expect("create location refusal fixtures");
    for (name, operation) in [
        ("completion", "target.ping()"),
        ("mailbox", "mailbox(target).ping()"),
        ("monitor", "monitor(target)"),
        ("link", "link(target)"),
        ("stopped", "stopped(target)"),
    ] {
        let source = directory.path().join(format!("{name}.hew"));
        std::fs::write(
            &source,
            format!(
                "actor Observer {{ receive fn inspect() {{ \
                 let target = Node.lookup(\"discovery-only\").expect(\"location\"); \
                 let _ = {operation}; }} }} fn main() {{}}"
            ),
        )
        .expect("write location refusal fixture");
        let mut command = Command::new(
            std::env::var_os("HEW_DIST_E2E_HEW_BIN").map_or_else(hew_binary, PathBuf::from),
        );
        command
            .arg("check")
            .arg(&source)
            .env("HEW_STD", repo_root().join("std"))
            .current_dir(repo_root());
        let output =
            support::run_bounded_command(command, format!("check location {name} refusal"));
        let diagnostics = support::describe_output(&output);
        assert!(
            !output.status.success()
                && diagnostics.contains("Location")
                && (diagnostics.contains("actor") || diagnostics.contains("method")),
            "discovery metadata must be refused as {name} authority:\n{diagnostics}"
        );
    }
}

#[test]
fn discovery_metadata_display_releases_temporary_and_named_strings() {
    for (scenario, iterations) in [("identity_display_low", 3), ("identity_display_high", 50)] {
        let stdout = run_two_process_scenario(scenario);
        assert!(
            stdout.contains(&format!("PASS {scenario} iterations={iterations}")),
            "{stdout}"
        );
        assert!(!stdout.contains("FAIL "), "{stdout}");
        assert_eq!(
            stdout.lines().filter(|line| *line == "frame").count(),
            iterations
        );
    }
}

/// Configured discovery survives an absent listener and a refused handshake,
/// then rejoins a same-key higher session without replaying the interrupted call.
#[test]
fn configured_seed_discovers_and_rejoins_without_explicit_connect() {
    let scene = SecureScenario::new();
    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("seed port");
    listener.set_nonblocking(true).unwrap();
    let port = listener.local_addr().unwrap().port().to_string();
    let env = [("HEW_DIST_SEED_PORT", port.as_str())];
    let mut server = scene.spawn_server_with_env("persistent_seed", &env);
    let mut client = scene.spawn_client("persistent_seed", &env);
    let stdout = client.child.stdout.take().expect("client stdout");
    let mut reader = BufReader::new(stdout);
    let mut captured = String::new();
    reader
        .read_line(&mut captured)
        .expect("seed startup marker");
    assert!(captured.contains("READY_SEED_STARTED"), "{captured}");
    // A live socket that refuses the protocol must not end seed discovery.
    let deadline = Instant::now() + SERVER_READY_TIMEOUT;
    let refused = loop {
        match listener.accept() {
            Ok((stream, _)) => break stream,
            Err(error) if error.kind() == std::io::ErrorKind::WouldBlock => {
                assert!(Instant::now() < deadline, "seed never dialled");
                std::thread::sleep(Duration::from_millis(10));
            }
            Err(error) => panic!("seed accept: {error}"),
        }
    };
    drop(refused);
    drop(listener);
    // The seed is now absent; only the configured supervisor can discover
    // the server once this gate permits its local start.
    std::fs::write(scene.kx_dir.path().join("seed.start-server"), "1").unwrap();
    let _first_stdout = wait_for_server_ready(&mut server);
    loop {
        let mut line = String::new();
        assert_ne!(reader.read_line(&mut line).unwrap(), 0, "{captured}");
        captured.push_str(&line);
        if line.contains("READY_SEED_RESTART") {
            break;
        }
    }
    server.child.kill().expect("kill admitted peer during call");
    server.child.wait().unwrap();
    let mut replacement = scene.spawn_server_with_env(
        "persistent_seed",
        &[
            ("HEW_DIST_SEED_PORT", port.as_str()),
            ("HEW_DIST_RESTARTED", "1"),
        ],
    );
    let _replacement_stdout = wait_for_server_ready(&mut replacement);
    reader.read_to_string(&mut captured).unwrap();
    let status = client.child.wait().unwrap();
    let mut stderr = String::new();
    client
        .child
        .stderr
        .take()
        .unwrap()
        .read_to_string(&mut stderr)
        .unwrap();
    assert!(status.success(), "{status:?}\n{captured}\n{stderr}");
    assert!(captured.contains("PASS persistent_seed absent-start authenticated-call higher-session no-replay fresh-call"), "{captured}");
}

#[test]
fn configured_seed_shutdown_joins_a_stalled_handshake() {
    let scene = SecureScenario::new();
    let listener = std::net::TcpListener::bind("127.0.0.1:0").expect("seed port");
    listener.set_nonblocking(true).unwrap();
    let port = listener.local_addr().unwrap().port().to_string();
    let env = [("HEW_DIST_SEED_PORT", port.as_str())];
    let _server = scene.spawn_server_with_env("persistent_seed_stop", &env);
    let mut client = scene.spawn_client("persistent_seed_stop", &env);
    let mut reader = BufReader::new(client.child.stdout.take().unwrap());
    let mut startup = String::new();
    reader.read_line(&mut startup).unwrap();
    assert!(startup.contains("READY_SEED_STARTED"), "{startup}");
    let deadline = Instant::now() + SERVER_READY_TIMEOUT;
    let _stalled = loop {
        match listener.accept() {
            Ok((stream, _)) => break stream,
            Err(error) if error.kind() == std::io::ErrorKind::WouldBlock => {
                assert!(Instant::now() < deadline, "seed never dialled");
                std::thread::sleep(Duration::from_millis(10));
            }
            Err(error) => panic!("seed accept: {error}"),
        }
    };
    // Keep the socket open without sending a handshake. Shutdown must await
    // the bounded admission owner, rather than detach it or close a reused id.
    client.child.stdout = Some(reader.into_inner());
    std::fs::write(scene.kx_dir.path().join("seed.stop-now"), "1").unwrap();
    let output = run_client_to_completion(client, 0, StderrExpectation::NoFaultDiagnostics);
    assert!(
        output.contains("PASS persistent_seed_stop active-attempt-joined"),
        "{output}"
    );
}
