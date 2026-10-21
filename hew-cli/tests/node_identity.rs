//! Operator credentials share node identity storage without allocating sessions.

use std::process::Command;

#[test]
fn identity_is_stable_and_does_not_create_session_state() {
    for transport in ["tcp", "quic-mesh"] {
        let directory = tempfile::tempdir().expect("identity directory");
        let key = directory.path().join("node.key");
        let invoke = || {
            Command::new(env!("CARGO_BIN_EXE_hew"))
                .args(["node", "identity", "--transport", transport, "--key"])
                .arg(&key)
                .output()
                .expect("run identity command")
        };
        let first = invoke();
        assert!(first.status.success(), "{first:?}");
        assert!(first.stderr.is_empty(), "{first:?}");
        let bytes = std::fs::read(&key).expect("stored key");
        let second = invoke();
        assert!(second.status.success(), "{second:?}");
        assert_eq!(first.stdout, second.stdout);
        assert_eq!(bytes, std::fs::read(&key).expect("unchanged key"));
        let expected = if transport == "tcp" {
            let identity = hew_runtime::encryption::noise_identity_load_or_create(&key)
                .expect("node TCP identity");
            hew_runtime::peer_binding::hex_lower(&identity.public())
        } else {
            let identity = hew_runtime::quic_mesh::mesh_identity_load_or_create(&key)
                .expect("node QUIC identity");
            hew_runtime::peer_binding::hex_lower(identity.spki())
        };
        assert_eq!(first.stdout, format!("{expected}\n").as_bytes());
        assert_eq!(std::fs::read_dir(directory.path()).unwrap().count(), 1);
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            assert_eq!(
                std::fs::metadata(&key).unwrap().permissions().mode() & 0o777,
                0o600
            );
        }
    }
}

#[test]
fn invalid_existing_identity_is_reported_without_replacement() {
    let directory = tempfile::tempdir().expect("identity directory");
    let key = directory.path().join("invalid.key");
    std::fs::write(&key, b"invalid identity").unwrap();
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(&key, std::fs::Permissions::from_mode(0o600)).unwrap();
    }
    let output = Command::new(env!("CARGO_BIN_EXE_hew"))
        .args(["node", "identity", "--key"])
        .arg(&key)
        .output()
        .expect("run identity command");
    assert!(!output.status.success());
    assert!(output.stdout.is_empty());
    assert!(String::from_utf8_lossy(&output.stderr).contains("node identity:"));
    assert_eq!(std::fs::read(&key).unwrap(), b"invalid identity");
}

#[test]
fn authority_enrolls_both_transport_credentials_without_node_sessions() {
    let directory = tempfile::tempdir().expect("enrollment directory");
    let authority = directory.path().join("authority.key");
    let first = Command::new(env!("CARGO_BIN_EXE_hew"))
        .args(["node", "authority", "--key"])
        .arg(&authority)
        .output()
        .expect("authority");
    assert!(first.status.success(), "{first:?}");
    assert_eq!(first.stdout.len(), 65);
    for transport in ["tcp", "quic-mesh"] {
        let key = directory.path().join(format!("{transport}.key"));
        let identity = Command::new(env!("CARGO_BIN_EXE_hew"))
            .args(["node", "identity", "--transport", transport, "--key"])
            .arg(&key)
            .output()
            .expect("identity");
        assert!(identity.status.success(), "{identity:?}");
        let credential = String::from_utf8(identity.stdout).expect("public credential");
        let bundle = directory.path().join(format!("{transport}.enrollment"));
        let enrolled = Command::new(env!("CARGO_BIN_EXE_hew"))
            .args(["node", "admit", "--authority"])
            .arg(&authority)
            .args([
                "--credential",
                credential.trim(),
                "--transport",
                transport,
                "--output",
            ])
            .arg(&bundle)
            .output()
            .expect("enroll");
        assert!(enrolled.status.success(), "{enrolled:?}");
        assert!(enrolled.stdout.is_empty());
        let loaded = hew_runtime::cluster_admission::Admission::load(&bundle).expect("bundle");
        assert_eq!(
            hew_runtime::peer_binding::hex_lower(match loaded.credential() {
                hew_runtime::peer_binding::PeerCredential::NoiseKey(key) => key,
                hew_runtime::peer_binding::PeerCredential::Spki(key) => key,
            }),
            credential.trim()
        );
    }
    assert_eq!(
        std::fs::read_dir(directory.path())
            .expect("entries")
            .count(),
        5
    );
}

#[test]
fn admission_refuses_non_hex_credentials_without_creating_files() {
    let directory = tempfile::tempdir().expect("enrollment directory");
    let authority = directory.path().join("authority.key");
    let output = directory.path().join("enrollment");
    let result = Command::new(env!("CARGO_BIN_EXE_hew"))
        .args(["node", "admit", "--authority"])
        .arg(&authority)
        .args(["--credential", &"+1".repeat(32), "--output"])
        .arg(&output)
        .output()
        .expect("admit");
    assert!(!result.status.success());
    assert!(String::from_utf8_lossy(&result.stderr).contains("invalid transport credential"));
    assert_eq!(
        std::fs::read_dir(directory.path())
            .expect("directory")
            .count(),
        0
    );
}
