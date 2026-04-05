use std::io::Read;
use std::net::UdpSocket;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, ExitStatus, Output, Stdio};
use std::sync::OnceLock;
use std::time::{Duration, Instant};

fn repo_root() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-cli crate should live under the repo root")
}

fn require_codegen() -> bool {
    static BUILD_OK: OnceLock<bool> = OnceLock::new();
    *BUILD_OK.get_or_init(|| {
        Command::new("make")
            .args(["runtime", "stdlib"])
            .current_dir(repo_root())
            .status()
            .is_ok_and(|status| status.success())
    })
}

fn hew_binary() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_hew"))
}

fn hew_std() -> PathBuf {
    repo_root().join("std")
}

fn probe_dir() -> PathBuf {
    repo_root().join("docs/probes/quic-remote-service")
}

fn build_probe_binary(source: &Path, output_path: &Path) {
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

fn pick_free_udp_port() -> u16 {
    UdpSocket::bind("127.0.0.1:0")
        .expect("bind udp socket for port discovery")
        .local_addr()
        .expect("discover local udp port")
        .port()
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
                std::thread::sleep(Duration::from_millis(50));
            }
            Err(e) => return Err(format!("cannot poll child process: {e}")),
        }
    }
}

fn describe_output(output: &Output) -> String {
    format!(
        "stdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    )
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

#[test]
fn quic_remote_service_probe_round_trip_succeeds() {
    if !require_codegen() {
        return;
    }

    let probe_dir = probe_dir();
    let workspace = tempfile::Builder::new()
        .prefix("quic-service-smoke-")
        .tempdir_in(repo_root())
        .expect("create smoke workspace in repo root");
    let server_binary = workspace
        .path()
        .join(format!("service_server{}", std::env::consts::EXE_SUFFIX));
    let client_binary = workspace
        .path()
        .join(format!("service_client{}", std::env::consts::EXE_SUFFIX));

    build_probe_binary(&probe_dir.join("service_server.hew"), &server_binary);
    build_probe_binary(&probe_dir.join("service_client.hew"), &client_binary);

    let port = pick_free_udp_port().to_string();

    let mut server = RunningChild::spawn({
        let mut command = Command::new(&server_binary);
        command
            .env("HEW_QUIC_SERVICE_PORT", &port)
            .current_dir(&probe_dir);
        command
    });

    std::thread::sleep(Duration::from_secs(2));
    server.assert_still_running("server exited before the client connected");

    let mut client = Command::new(&client_binary)
        .env("HEW_QUIC_SERVICE_PORT", &port)
        .current_dir(&probe_dir)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn client process");
    let client_output = wait_for_child(&mut client, Duration::from_secs(15))
        .unwrap_or_else(|error| panic!("{error}"));
    assert!(
        client_output.status.success(),
        "client probe failed\n{}",
        describe_output(&client_output),
    );

    let server_output = server.wait_with_timeout(Duration::from_secs(15));
    assert!(
        server_output.status.success(),
        "server probe failed\n{}",
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
