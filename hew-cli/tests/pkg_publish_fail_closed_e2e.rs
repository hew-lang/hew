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
use std::net::TcpListener;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use support::http_canned::spawn_canned_response_server;

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

// ── 1b. corrupt credentials.toml — parse error, not "not logged in" ────────

#[test]
fn publish_corrupt_credentials_file_exits_one_with_parse_diagnostic() {
    let home = support::tempdir();
    let project = support::tempdir();
    generate_signing_key(home.path());
    write_publishable_manifest(project.path());
    let hew_dir = home.path().join(".hew");
    fs::create_dir_all(&hew_dir).expect("create .hew dir");
    fs::write(
        hew_dir.join("credentials.toml"),
        "this is not valid toml {{{",
    )
    .expect("write corrupt credentials.toml");

    let output = run_publish(project.path(), home.path(), &[]);

    assert_eq!(
        output.status.code(),
        Some(1),
        "expected exit 1\n{}",
        support::describe_output(&output)
    );
    let stderr = stderr_of(&output);
    assert!(
        stderr.contains("invalid credentials file"),
        "stderr missing parse-error diagnostic:\n{stderr}"
    );
    assert!(
        !stderr.contains("not logged in"),
        "a corrupt credentials file must not be misreported as not-logged-in:\n{stderr}"
    );
    assert!(
        !local_package_dir(home.path()).exists(),
        "no local copy should be written when credentials.toml is corrupt"
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

// ── 2b. --registry names a registry absent from config.toml ────────────────
//
// Credential resolution runs before `make_client` (test 1's comment above),
// so a typo'd `--registry` name must still surface `make_client`'s precise
// unknown-registry diagnostic rather than being misreported as "not logged
// in" — no stored token is ever found for a registry name that doesn't
// exist, so a naive `Err(_) => not-logged-in` classification would shadow
// this diagnostic entirely.

#[test]
fn publish_unknown_registry_name_exits_one_with_precise_diagnostic() {
    let home = support::tempdir();
    let project = support::tempdir();
    generate_signing_key(home.path());
    write_publishable_manifest(project.path());
    // No config.toml at all, and no credentials.toml — the unknown-registry
    // check must fire before any credential lookup.

    let output = run_publish(project.path(), home.path(), &["--registry", "typoreg"]);

    assert_eq!(
        output.status.code(),
        Some(1),
        "expected exit 1\n{}",
        support::describe_output(&output)
    );
    let stderr = stderr_of(&output);
    assert!(
        stderr.contains("hew: unknown registry 'typoreg'"),
        "stderr missing precise unknown-registry diagnostic:\n{stderr}"
    );
    assert!(
        stderr.contains("[registries.typoreg]"),
        "stderr missing config-hint for the unknown registry:\n{stderr}"
    );
    assert!(
        !stderr.contains("not logged in"),
        "unknown-registry diagnostic must not be shadowed by the not-logged-in message:\n{stderr}"
    );
    assert!(
        !local_package_dir(home.path()).exists(),
        "no local copy should be written when the named registry is unknown"
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
