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
