mod support;

use std::io::{ErrorKind, Read};
use std::net::UdpSocket;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, ExitStatus, Output, Stdio};
use std::time::{Duration, Instant};

use support::{describe_output, hew_binary, repo_root, require_codegen};

const POLL_INTERVAL: Duration = Duration::from_millis(50);
const SERVER_READY_TIMEOUT: Duration = Duration::from_secs(10);
const EXAMPLE_TIMEOUT: Duration = Duration::from_secs(20);

fn hew_std() -> PathBuf {
    repo_root().join("std")
}

fn example_dir() -> PathBuf {
    repo_root().join("examples/quic_service")
}

fn build_example_binary(source: &Path, output_path: &Path) {
    let build_output = Command::new(hew_binary())
        .arg("build")
        .arg(source)
        .arg("-o")
        .arg(output_path)
        .env("HEW_STD", hew_std())
        .current_dir(repo_root())
        .output()
        .expect("run hew build");

    assert!(
        build_output.status.success(),
        "hew build {} failed\n{}",
        source.display(),
        describe_output(&build_output),
    );
}

/// Addresses the readiness probe tries, in order.
///
/// The example asks hew-std to listen on `":PORT"`, which resolves against the
/// default bind host, so the server owns `0.0.0.0:PORT` and never
/// `127.0.0.1:PORT`. Winsock rejects a second bind only on an exact address
/// match, so a loopback-only probe on Windows keeps succeeding while the server
/// holds the wildcard and the poll reports "not bound" until the deadline
/// expires. BSD and Linux treat the wildcard as overlapping every specific
/// address, which is why the same probe worked on macOS and hid the defect.
/// Probing both makes the observation independent of that difference.
const BIND_PROBE_HOSTS: [&str; 2] = ["0.0.0.0", "127.0.0.1"];

fn probed_addresses(port: u16) -> String {
    BIND_PROBE_HOSTS
        .iter()
        .map(|host| format!("{host}:{port}"))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Discover a port that is free on the wildcard, not merely on loopback: the
/// server binds the wildcard, so a port free only on `127.0.0.1` would collide.
fn pick_free_udp_port() -> u16 {
    UdpSocket::bind("0.0.0.0:0")
        .expect("bind udp socket for port discovery")
        .local_addr()
        .expect("discover local udp port")
        .port()
}

fn udp_port_is_bound(port: u16) -> Result<bool, String> {
    for host in BIND_PROBE_HOSTS {
        match UdpSocket::bind((host, port)) {
            Ok(socket) => drop(socket),
            Err(error) if error.kind() == ErrorKind::AddrInUse => return Ok(true),
            Err(error) => return Err(format!("cannot probe UDP {host}:{port}: {error}")),
        }
    }
    Ok(false)
}

fn read_pipe<T: Read>(mut stream: T, name: &str) -> Result<Vec<u8>, String> {
    let mut bytes = Vec::new();
    stream
        .read_to_end(&mut bytes)
        .map_err(|e| format!("cannot read child {name}: {e}"))?;
    Ok(bytes)
}

fn collect_child_output(child: &mut Child, status: ExitStatus) -> Result<Output, String> {
    let stdout = child
        .stdout
        .take()
        .map_or_else(|| Ok(Vec::new()), |stream| read_pipe(stream, "stdout"))?;
    let stderr = child
        .stderr
        .take()
        .map_or_else(|| Ok(Vec::new()), |stream| read_pipe(stream, "stderr"))?;

    Ok(Output {
        status,
        stdout,
        stderr,
    })
}

fn terminate_child(child: &mut Child) -> Result<Output, String> {
    match child.kill() {
        Ok(()) => {}
        Err(kill_error) => match child.try_wait() {
            Ok(Some(status)) => return collect_child_output(child, status),
            Ok(None) => return Err(format!("cannot kill child process: {kill_error}")),
            Err(wait_error) => {
                return Err(format!(
                    "cannot kill child process: {kill_error}; cannot poll child after kill failure: {wait_error}"
                ));
            }
        },
    }

    let status = child
        .wait()
        .map_err(|e| format!("cannot reap child process: {e}"))?;
    collect_child_output(child, status)
}

fn wait_for_child(child: &mut Child, timeout: Duration) -> Result<Output, String> {
    let start = Instant::now();
    loop {
        match child.try_wait() {
            Ok(Some(status)) => return collect_child_output(child, status),
            Ok(None) => {
                if start.elapsed() >= timeout {
                    let timed_out_output = terminate_child(child).map_or_else(
                        |error| format!("unable to collect timed-out child output: {error}"),
                        |output| describe_output(&output),
                    );
                    return Err(format!("timed out after {timeout:?}\n{timed_out_output}"));
                }
                std::thread::sleep(POLL_INTERVAL);
            }
            Err(e) => return Err(format!("cannot poll child process: {e}")),
        }
    }
}

struct RunningChild {
    child: Option<Child>,
}

impl RunningChild {
    fn spawn(mut command: Command) -> Self {
        let child = command
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .expect("spawn child process");
        Self { child: Some(child) }
    }

    fn assert_still_running(&mut self, context: &str) {
        let child = self.child.as_mut().expect("child process missing");
        match child.try_wait() {
            Ok(None) => {}
            Ok(Some(status)) => {
                let output =
                    collect_child_output(child, status).expect("collect exited child output");
                panic!("{context}\n{}", describe_output(&output));
            }
            Err(error) => panic!("cannot poll child process: {error}"),
        }
    }

    fn wait_for_udp_bind(&mut self, port: u16, timeout: Duration) {
        let start = Instant::now();
        loop {
            self.assert_still_running("server exited before binding the QUIC port");
            match udp_port_is_bound(port) {
                Ok(true) => return,
                Ok(false) => {
                    assert!(
                        start.elapsed() < timeout,
                        "server did not bind UDP port {port} within {timeout:?} \
                         (probed {}) — the server is still running, so either it \
                         has not bound yet or it bound an address this probe \
                         does not cover",
                        probed_addresses(port),
                    );
                    std::thread::sleep(POLL_INTERVAL);
                }
                Err(error) => panic!("{error}"),
            }
        }
    }

    fn wait_with_timeout(&mut self, timeout: Duration) -> Output {
        let mut child = self.child.take().expect("child process missing");
        wait_for_child(&mut child, timeout).unwrap_or_else(|error| panic!("{error}"))
    }
}

impl Drop for RunningChild {
    fn drop(&mut self) {
        if let Some(child) = self.child.as_mut() {
            if let Ok(None) = child.try_wait() {
                let _ = terminate_child(child);
            }
        }
    }
}

/// The readiness probe must be able to observe a wildcard bind.
///
/// `wait_for_udp_bind` is the only thing between spawning the server and driving
/// traffic at it, and its failure mode is a timeout — which reads as "the server
/// never came up" when the truth may be "the probe cannot see the address the
/// server bound". That is exactly how the loopback-only probe failed on Windows.
/// Bind the wildcard here and require the probe to report it, so a probe that
/// stops observing what it polls fails on its own terms instead of being
/// misattributed to the server.
#[test]
fn udp_bind_probe_observes_a_wildcard_bind() {
    let socket = UdpSocket::bind("0.0.0.0:0").expect("bind wildcard udp socket");
    let port = socket.local_addr().expect("discover local udp port").port();

    assert!(
        udp_port_is_bound(port).unwrap_or_else(|error| panic!("{error}")),
        "readiness probe reported UDP port {port} free while this test holds a \
         live wildcard bind on it (probed {})",
        probed_addresses(port),
    );

    drop(socket);

    // And the probe must be able to say "free" too, or the assertion above
    // would be satisfied by a probe that always answers "bound".
    assert!(
        !udp_port_is_bound(port).unwrap_or_else(|error| panic!("{error}")),
        "readiness probe reported UDP port {port} bound after the wildcard bind \
         was released (probed {})",
        probed_addresses(port),
    );
}

// The two blockers this test was parked behind are gone. `hew build` exists and
// the host build/link path is exercised by `build_host_e2e`; the QUIC
// stream/connection stdlib surface (`recv_string`/`send_string`/`finish`/
// `disconnect`/`kind`) now resolves in HIR, so both server.hew and client.hew
// pass `hew check`. The test builds both example binaries, spawns the server,
// waits for the UDP bind and drives a real round trip.
#[test]
fn quic_service_example_round_trip_succeeds() {
    require_codegen();

    let example_dir = example_dir();
    let workspace = tempfile::Builder::new()
        .prefix("quic-service-smoke-")
        .tempdir_in(repo_root())
        .expect("create smoke workspace in repo root");
    let server_binary = hew_testutil::compiled_binary_path(workspace.path(), "service_server");
    let client_binary = hew_testutil::compiled_binary_path(workspace.path(), "service_client");

    build_example_binary(&example_dir.join("server.hew"), &server_binary);
    build_example_binary(&example_dir.join("client.hew"), &client_binary);

    let port = pick_free_udp_port();
    let port_str = port.to_string();

    let mut server = RunningChild::spawn({
        let mut command = Command::new(&server_binary);
        command
            .env("HEW_QUIC_SERVICE_PORT", &port_str)
            .current_dir(&example_dir);
        command
    });

    server.wait_for_udp_bind(port, SERVER_READY_TIMEOUT);

    let mut client = Command::new(&client_binary)
        .env("HEW_QUIC_SERVICE_PORT", &port_str)
        .current_dir(&example_dir)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn client process");
    let client_output =
        wait_for_child(&mut client, EXAMPLE_TIMEOUT).unwrap_or_else(|error| panic!("{error}"));
    assert!(
        client_output.status.success(),
        "client example failed\n{}",
        describe_output(&client_output),
    );

    let server_output = server.wait_with_timeout(EXAMPLE_TIMEOUT);
    assert!(
        server_output.status.success(),
        "server example failed\n{}",
        describe_output(&server_output),
    );

    let server_stdout = String::from_utf8_lossy(&server_output.stdout);
    assert!(
        server_stdout.contains("[server] accepted connection")
            && server_stdout.contains("[server] accepted stream")
            && server_stdout.contains("[server] received: Hello from client")
            && server_stdout.contains("[server] sent response")
            && server_stdout.contains("[server] shutdown complete"),
        "unexpected server output\n{}",
        describe_output(&server_output),
    );

    let client_stdout = String::from_utf8_lossy(&client_output.stdout);
    assert!(
        client_stdout.contains("[client] connected to server")
            && client_stdout.contains("[client] opened stream")
            && client_stdout.contains("[client] sent message")
            && client_stdout.contains("[client] received: Echo from server")
            && client_stdout.contains("[client] shutdown complete"),
        "unexpected client output\n{}",
        describe_output(&client_output),
    );
}
