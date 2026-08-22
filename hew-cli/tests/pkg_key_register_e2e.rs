//! Behaviour of `hew key register`.
//!
//! Publishing signs with `~/.hew/keys/id_ed25519`, and the registry rejects a
//! signature from a key it has never seen — its own diagnostic tells the user
//! to run `hew key register`. Before that subcommand existed the only way to
//! register an existing key was to delete it and generate a replacement,
//! which invalidates every signature the old key ever made.
//!
//! Every test relocates `$HOME` (and `$USERPROFILE`, for Windows parity) per
//! `Command`, never via `std::env::set_var` — see LESSONS
//! `global-test-isolation`. Registry-facing tests use a **named** registry
//! pinned to `127.0.0.1`, so no test here can address the real registry.
mod support;

use std::fs;
use std::path::Path;
use std::process::{Command, Output};
use std::time::Duration;

use support::http_canned::spawn_recording_canned_response_server;

const REGISTRY_NAME: &str = "testreg";

fn key_command(home: &Path, args: &[&str]) -> Output {
    let mut cmd = Command::new(support::hew_binary());
    cmd.arg("key")
        .args(args)
        .env("HOME", home)
        .env("USERPROFILE", home);
    support::run_bounded_command(cmd, "hew key")
}

fn generate_signing_key(home: &Path) {
    let output = key_command(home, &["generate"]);
    assert!(
        output.status.success(),
        "hew key generate failed\n{}",
        support::describe_output(&output)
    );
}

/// Point the named registry at `api_url` and store a token for it. The default
/// `[registry]` table is never written, so a bug cannot reach the real
/// registry with these credentials.
fn write_named_registry(home: &Path, api_url: &str) {
    let hew_dir = home.join(".hew");
    fs::create_dir_all(&hew_dir).expect("create .hew dir");
    fs::write(
        hew_dir.join("config.toml"),
        format!(
            "[registries.{REGISTRY_NAME}]\n\
             index = \"http://127.0.0.1:9/index\"\n\
             api = \"{api_url}\"\n"
        ),
    )
    .expect("write config.toml");
    fs::write(
        hew_dir.join("credentials.toml"),
        format!("[registries.{REGISTRY_NAME}]\ntoken = \"test-token\"\ngithub_user = \"tester\"\n"),
    )
    .expect("write credentials.toml");
}

#[test]
fn key_register_with_an_existing_key_registers_it_and_exits_zero() {
    let home = support::tempdir();
    generate_signing_key(home.path());
    let expected_public_key = fs::read_to_string(home.path().join(".hew/keys/id_ed25519.pub"))
        .expect("read generated public key")
        .trim()
        .to_owned();
    let (port, request_rx) = spawn_recording_canned_response_server(
        "200 OK",
        r#"{"fingerprint":"SHA256:0hOtRl0FvXqAr7cUf3jTSTfvhO2fYnTEXPz1yeF3aRM"}"#,
    );
    write_named_registry(home.path(), &format!("http://127.0.0.1:{port}"));

    let output = key_command(home.path(), &["register", "--registry", REGISTRY_NAME]);

    assert_eq!(
        output.status.code(),
        Some(0),
        "expected exit 0\n{}",
        support::describe_output(&output)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("Key registered with registry"),
        "stdout missing registration confirmation:\n{stdout}"
    );
    assert!(
        stdout.contains("SHA256:0hOtRl0FvXqAr7cUf3jTSTfvhO2fYnTEXPz1yeF3aRM"),
        "stdout missing the registry's fingerprint:\n{stdout}"
    );
    let request = request_rx
        .recv_timeout(Duration::from_secs(10))
        .expect("registration request");
    assert_eq!(request.method, "PUT");
    assert_eq!(request.path, "/keys");
    let body: serde_json::Value =
        serde_json::from_slice(&request.body).expect("registration JSON body");
    assert_eq!(body["key_type"], "ed25519");
    assert_eq!(body["public_key"], expected_public_key);
}

#[test]
fn key_register_without_a_key_exits_one_and_names_generate() {
    let home = support::tempdir();
    // Credentials and a registry are present: the key is the only thing
    // missing, so a success here could only come from registering nothing.
    write_named_registry(home.path(), "http://127.0.0.1:9");

    let output = key_command(home.path(), &["register", "--registry", REGISTRY_NAME]);

    assert_eq!(
        output.status.code(),
        Some(1),
        "expected exit 1\n{}",
        support::describe_output(&output)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("no signing key found"),
        "stderr missing missing-key diagnostic:\n{stderr}"
    );
    assert!(
        stderr.contains("hew key generate"),
        "stderr does not name the command that creates a key:\n{stderr}"
    );
}

#[test]
fn key_register_without_credentials_exits_one_and_names_login() {
    let home = support::tempdir();
    generate_signing_key(home.path());
    // A configured registry with no token for it.
    let hew_dir = home.path().join(".hew");
    fs::create_dir_all(&hew_dir).expect("create .hew dir");
    fs::write(
        hew_dir.join("config.toml"),
        format!(
            "[registries.{REGISTRY_NAME}]\n\
             index = \"http://127.0.0.1:9/index\"\n\
             api = \"http://127.0.0.1:9\"\n"
        ),
    )
    .expect("write config.toml");

    let output = key_command(home.path(), &["register", "--registry", REGISTRY_NAME]);

    assert_eq!(
        output.status.code(),
        Some(1),
        "expected exit 1\n{}",
        support::describe_output(&output)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("not logged in"),
        "stderr missing not-logged-in diagnostic:\n{stderr}"
    );
    assert!(
        stderr.contains("hew login"),
        "stderr does not name the command that authenticates:\n{stderr}"
    );
}

#[test]
fn key_register_with_corrupt_credentials_surfaces_the_parse_error() {
    let home = support::tempdir();
    generate_signing_key(home.path());
    let hew_dir = home.path().join(".hew");
    fs::write(hew_dir.join("credentials.toml"), "not valid toml {{{")
        .expect("write corrupt credentials");

    let output = key_command(home.path(), &["register"]);

    assert_eq!(
        output.status.code(),
        Some(1),
        "expected exit 1\n{}",
        support::describe_output(&output)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("invalid credentials file"),
        "stderr missing credential parse diagnostic:\n{stderr}"
    );
    assert!(
        !stderr.contains("not logged in"),
        "parse failure must not be reported as logged out:\n{stderr}"
    );
}

#[test]
fn key_register_with_unknown_named_registry_surfaces_the_config_error() {
    let home = support::tempdir();
    generate_signing_key(home.path());

    let output = key_command(home.path(), &["register", "--registry", "unknown"]);

    assert_eq!(
        output.status.code(),
        Some(1),
        "expected exit 1\n{}",
        support::describe_output(&output)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("unknown registry 'unknown'"),
        "stderr missing unknown-registry diagnostic:\n{stderr}"
    );
    assert!(
        !stderr.contains("not logged in"),
        "unknown registry must not be reported as logged out:\n{stderr}"
    );
}

#[test]
fn key_generate_over_an_existing_key_directs_the_user_to_register() {
    let home = support::tempdir();
    generate_signing_key(home.path());

    let output = key_command(home.path(), &["generate"]);

    assert_eq!(
        output.status.code(),
        Some(1),
        "expected exit 1\n{}",
        support::describe_output(&output)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("key already exists"),
        "stderr missing existing-key diagnostic:\n{stderr}"
    );
    assert!(
        stderr.contains("hew key register"),
        "stderr does not name the command that registers the existing key:\n{stderr}"
    );
}
