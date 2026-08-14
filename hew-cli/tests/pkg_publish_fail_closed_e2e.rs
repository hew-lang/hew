//! Exit-code contract for `hew publish`'s fail-closed behaviour.
//!
//! `hew publish`'s whole purpose is a remote side effect. Before this test
//! existed, missing credentials, an unreachable registry, and a remote
//! publish failure all exited 0 after silently writing the package into the
//! local registry — "Published" printed regardless of whether anything
//! reached a registry. These tests assert the corrected contract: the
//! remote leg either completes (exit 0) or the command exits nonzero with no
//! local copy ever written implicitly. `hew publish --local` is the explicit,
//! opt-in local workflow (test 6); everything else here proves the remote
//! path never falls back to it.
//!
//! Every test relocates `$HOME` (and `$USERPROFILE`, for Windows parity) per
//! `Command`, never via `std::env::set_var` — see LESSONS
//! `global-test-isolation`. Every child is spawned through
//! `support::run_bounded_command` — see LESSONS `exec-test-must-bound-the-child`.
//!
//! Production-registry safety: no test here ever writes a `[registry]`
//! (default-registry) token into `credentials.toml`. Every test that
//! supplies a token uses a **named** registry pinned to `127.0.0.1`, so a bug
//! here can never address `https://registry.hewpkg.com`.
mod support;

use std::fs;
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::process::{Command, Output};
use std::time::Duration;

const PKG_NAME: &str = "repro_pkg";
const PKG_VERSION: &str = "0.1.0";

/// Write a minimal publishable manifest plus one source file into `project`.
///
/// Exactly the five fields `manifest::validate_for_publish` requires
/// (`name`, `version`, `description`, `license`, `authors`); `edition` is
/// included even though `manifest::default_edition` covers its absence.
/// Dependency-free, since path dependencies are also rejected by
/// `validate_for_publish`.
fn write_publishable_manifest(project: &Path) {
    fs::write(
        project.join("hew.toml"),
        format!(
            "[package]\n\
             name = \"{PKG_NAME}\"\n\
             version = \"{PKG_VERSION}\"\n\
             description = \"repro\"\n\
             license = \"MIT\"\n\
             authors = [\"a\"]\n\
             edition = \"2026\"\n"
        ),
    )
    .expect("write hew.toml");
    fs::write(project.join("main.hew"), "fn main() {}\n").expect("write main.hew");
}

/// Generate a signing keypair under `home/.hew/keys` via `hew key generate`.
fn generate_signing_key(home: &Path) {
    let mut cmd = support::hew_command();
    cmd.arg("key")
        .arg("generate")
        .env("HOME", home)
        .env("USERPROFILE", home);
    let output = support::run_bounded_command(cmd, "hew key generate");
    assert!(
        output.status.success(),
        "hew key generate failed\n{}",
        support::describe_output(&output)
    );
}

/// Write `home/.hew/config.toml` with a single named registry pointed at
/// `api_url`.
fn write_named_registry_config(home: &Path, registry_name: &str, api_url: &str) {
    let hew_dir = home.join(".hew");
    fs::create_dir_all(&hew_dir).expect("create .hew dir");
    fs::write(
        hew_dir.join("config.toml"),
        format!(
            "[registries.{registry_name}]\n\
             index = \"http://127.0.0.1:9/index\"\n\
             api = \"{api_url}\"\n"
        ),
    )
    .expect("write config.toml");
}

/// Write `home/.hew/credentials.toml` with a token for the **named**
/// registry only — never the default `[registry]` table, which would point
/// a bug at the real `registry.hewpkg.com`.
fn write_named_registry_token(home: &Path, registry_name: &str, token: &str) {
    let hew_dir = home.join(".hew");
    fs::create_dir_all(&hew_dir).expect("create .hew dir");
    fs::write(
        hew_dir.join("credentials.toml"),
        format!("[registries.{registry_name}]\ntoken = \"{token}\"\ngithub_user = \"tester\"\n"),
    )
    .expect("write credentials.toml");
}

/// Path the local registry would use if (and only if) a local publish wrote it.
fn local_package_dir(home: &Path) -> PathBuf {
    home.join(".hew")
        .join("packages")
        .join(PKG_NAME)
        .join(PKG_VERSION)
}

fn publish_command(project: &Path, home: &Path, args: &[&str]) -> Command {
    let mut cmd = support::hew_command();
    cmd.arg("publish")
        .args(args)
        .current_dir(project)
        .env("HOME", home)
        .env("USERPROFILE", home);
    cmd
}

fn run_publish(project: &Path, home: &Path, args: &[&str]) -> Output {
    support::run_bounded_command(publish_command(project, home, args), "hew publish")
}

fn stderr_of(output: &Output) -> String {
    String::from_utf8_lossy(&output.stderr).into_owned()
}

fn stdout_of(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout).into_owned()
}

/// Read an HTTP request's headers and drain its `Content-Length` body so the
/// client's write completes before the server responds — otherwise the
/// client can fail on the write side instead of on the response status.
fn drain_http_request(stream: &mut TcpStream) {
    stream
        .set_read_timeout(Some(Duration::from_secs(10)))
        .expect("set read timeout");
    let mut buf = Vec::new();
    let mut chunk = [0u8; 4096];
    while let Ok(n) = stream.read(&mut chunk) {
        if n == 0 {
            break;
        }
        buf.extend_from_slice(&chunk[..n]);
        let Some(header_end) = find_double_crlf(&buf) else {
            continue;
        };
        let header_text = String::from_utf8_lossy(&buf[..header_end]);
        let content_length: usize = header_text
            .lines()
            .find_map(|line| {
                let (key, value) = line.split_once(':')?;
                key.trim()
                    .eq_ignore_ascii_case("content-length")
                    .then(|| value.trim().parse().ok())
                    .flatten()
            })
            .unwrap_or(0);
        let body_so_far = buf.len() - (header_end + 4);
        let mut remaining = content_length.saturating_sub(body_so_far);
        while remaining > 0 {
            let Ok(n) = stream.read(&mut chunk) else {
                break;
            };
            if n == 0 {
                break;
            }
            remaining = remaining.saturating_sub(n);
        }
        break;
    }
}

fn find_double_crlf(buf: &[u8]) -> Option<usize> {
    buf.windows(4).position(|w| w == b"\r\n\r\n")
}

/// Bind an ephemeral port, accept exactly one connection, drain its request,
/// and reply with a canned `status_line`/`body` response, then stop.
///
/// Returns the bound port; the caller points a named registry's `api` at it.
fn spawn_canned_response_server(status_line: &'static str, body: &'static str) -> u16 {
    let listener = TcpListener::bind("127.0.0.1:0").expect("bind ephemeral port");
    let port = listener.local_addr().expect("local_addr").port();
    std::thread::spawn(move || {
        if let Ok((mut stream, _addr)) = listener.accept() {
            drain_http_request(&mut stream);
            let response = format!(
                "HTTP/1.1 {status_line}\r\n\
                 Content-Type: application/json\r\n\
                 Content-Length: {}\r\n\
                 Connection: close\r\n\
                 \r\n\
                 {body}",
                body.len()
            );
            let _ = stream.write_all(response.as_bytes());
        }
    });
    port
}

// ── 1. no credentials at all ────────────────────────────────────────────────

#[test]
fn publish_without_credentials_exits_one_and_writes_no_local_copy() {
    let home = support::tempdir();
    let project = support::tempdir();
    generate_signing_key(home.path());
    write_publishable_manifest(project.path());
    // No config.toml, no credentials.toml.

    let output = run_publish(project.path(), home.path(), &[]);

    assert_eq!(
        output.status.code(),
        Some(1),
        "expected exit 1\n{}",
        support::describe_output(&output)
    );
    let stderr = stderr_of(&output);
    assert!(
        stderr.contains("run `hew login`"),
        "stderr missing login hint:\n{stderr}"
    );
    assert!(
        stderr.contains("--local"),
        "stderr missing --local hint:\n{stderr}"
    );
    assert!(
        !local_package_dir(home.path()).exists(),
        "no local copy should be written when credentials are missing"
    );
}

// ── 2. named registry, no token for it (Arm C: has_token hardcoded true) ───

#[test]
fn publish_named_registry_without_token_exits_one() {
    let home = support::tempdir();
    let project = support::tempdir();
    generate_signing_key(home.path());
    write_publishable_manifest(project.path());
    write_named_registry_config(home.path(), "testreg", "http://127.0.0.1:9/api/v1");
    // No credentials.toml at all — not even for the default registry.

    let output = run_publish(project.path(), home.path(), &["--registry", "testreg"]);

    assert_eq!(
        output.status.code(),
        Some(1),
        "expected exit 1\n{}",
        support::describe_output(&output)
    );
    assert!(
        !local_package_dir(home.path()).exists(),
        "no local copy should be written when the named registry has no token"
    );
}

// ── 3. remote publish returns an error ──────────────────────────────────────

#[test]
fn publish_remote_error_exits_one_and_writes_no_local_copy() {
    let home = support::tempdir();
    let project = support::tempdir();
    generate_signing_key(home.path());
    write_publishable_manifest(project.path());

    let port =
        spawn_canned_response_server("500 Internal Server Error", r#"{"error":"registry down"}"#);
    write_named_registry_config(
        home.path(),
        "testreg",
        &format!("http://127.0.0.1:{port}/api/v1"),
    );
    write_named_registry_token(home.path(), "testreg", "tok_fake");

    let output = run_publish(project.path(), home.path(), &["--registry", "testreg"]);

    assert_eq!(
        output.status.code(),
        Some(1),
        "expected exit 1\n{}",
        support::describe_output(&output)
    );
    let stderr = stderr_of(&output);
    assert!(
        stderr.contains("hew publish: remote publish failed:"),
        "stderr missing remote-failure prefix:\n{stderr}"
    );
    assert!(
        !stderr.contains("Falling back"),
        "stderr must not mention a local fallback:\n{stderr}"
    );
    assert!(
        !local_package_dir(home.path()).exists(),
        "no local copy should be written when the remote publish fails"
    );
}

// ── 4. registry unreachable (connection refused) ────────────────────────────

#[test]
fn publish_unreachable_registry_exits_one() {
    let home = support::tempdir();
    let project = support::tempdir();
    generate_signing_key(home.path());
    write_publishable_manifest(project.path());

    // Bind, record the port, then drop the listener so the port is refusing
    // connections without a live server.
    let listener = TcpListener::bind("127.0.0.1:0").expect("bind ephemeral port");
    let port = listener.local_addr().expect("local_addr").port();
    drop(listener);

    write_named_registry_config(
        home.path(),
        "testreg",
        &format!("http://127.0.0.1:{port}/api/v1"),
    );
    write_named_registry_token(home.path(), "testreg", "tok_fake");

    let output = run_publish(project.path(), home.path(), &["--registry", "testreg"]);

    assert_eq!(
        output.status.code(),
        Some(1),
        "expected exit 1\n{}",
        support::describe_output(&output)
    );
    assert!(
        !local_package_dir(home.path()).exists(),
        "no local copy should be written when the registry is unreachable"
    );
}

// ── 5. remote publish succeeds (positive control) ───────────────────────────

#[test]
fn publish_remote_success_exits_zero_and_writes_no_local_copy() {
    let home = support::tempdir();
    let project = support::tempdir();
    generate_signing_key(home.path());
    write_publishable_manifest(project.path());

    let port = spawn_canned_response_server("201 Created", "{}");
    write_named_registry_config(
        home.path(),
        "testreg",
        &format!("http://127.0.0.1:{port}/api/v1"),
    );
    write_named_registry_token(home.path(), "testreg", "tok_fake");

    let output = run_publish(project.path(), home.path(), &["--registry", "testreg"]);

    assert_eq!(
        output.status.code(),
        Some(0),
        "expected exit 0\n{}",
        support::describe_output(&output)
    );
    let stdout = stdout_of(&output);
    assert!(
        stdout.contains("to registry"),
        "stdout missing remote-success message:\n{stdout}"
    );
    assert!(
        !local_package_dir(home.path()).exists(),
        "a successful remote publish must not also write a local copy"
    );
}

// ── 6. --local: explicit local workflow ─────────────────────────────────────

#[test]
fn publish_local_flag_exits_zero_and_writes_local_copy() {
    let home = support::tempdir();
    let project = support::tempdir();
    generate_signing_key(home.path());
    write_publishable_manifest(project.path());
    // No credentials.toml, no config.toml — --local must need neither.

    let output = run_publish(project.path(), home.path(), &["--local"]);

    assert_eq!(
        output.status.code(),
        Some(0),
        "expected exit 0\n{}",
        support::describe_output(&output)
    );
    let stdout = stdout_of(&output);
    assert!(
        stdout.contains("local registry only"),
        "stdout missing local-only annotation:\n{stdout}"
    );
    assert!(
        local_package_dir(home.path()).join("hew.toml").exists(),
        "--local must write the package into the local registry"
    );
}

// ── 7. --local --registry is a usage error ──────────────────────────────────

#[test]
fn publish_local_with_registry_flag_is_usage_error() {
    let home = support::tempdir();
    let project = support::tempdir();
    generate_signing_key(home.path());
    write_publishable_manifest(project.path());

    let output = run_publish(
        project.path(),
        home.path(),
        &["--local", "--registry", "testreg"],
    );

    assert_eq!(
        output.status.code(),
        Some(2),
        "expected clap usage error (exit 2)\n{}",
        support::describe_output(&output)
    );
    assert!(
        !local_package_dir(home.path()).exists(),
        "a usage error must not write a local copy"
    );
}
