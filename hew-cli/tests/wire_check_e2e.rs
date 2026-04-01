use std::path::{Path, PathBuf};
use std::process::{Command, Output};

fn repo_root() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-cli crate should live under the repo root")
}

fn hew_binary() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_hew"))
}

fn run_wire_check(current: &str, baseline: &str) -> Output {
    let dir = tempfile::tempdir().unwrap();
    let current_path = dir.path().join("current.hew");
    let baseline_path = dir.path().join("baseline.hew");
    std::fs::write(&current_path, current).unwrap();
    std::fs::write(&baseline_path, baseline).unwrap();

    Command::new(hew_binary())
        .args([
            "wire",
            "check",
            current_path.to_str().unwrap(),
            "--against",
            baseline_path.to_str().unwrap(),
        ])
        .current_dir(repo_root())
        .output()
        .unwrap()
}

#[test]
fn wire_check_rejects_reordered_wire_enum_variants() {
    let output = run_wire_check(
        "wire enum Command { Start; Pause; Stop; }\n",
        "wire enum Command { Start; Stop; Pause; }\n",
    );

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("changed variant order for `Command` at position 2"),
        "{stderr}",
    );
}

#[test]
fn wire_check_rejects_wire_enum_payload_changes() {
    let output = run_wire_check(
        "wire enum Command { Start; Data(String); }\n",
        "wire enum Command { Start; Data(String, u32); }\n",
    );

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("changed payload arity for `Command::Data`: 2 -> 1"),
        "{stderr}",
    );
}

#[test]
fn wire_check_rejects_wire_enum_struct_variant_field_renames() {
    let output = run_wire_check(
        "wire enum Command { Data { new_value: String }; }\n",
        "wire enum Command { Data { value: String }; }\n",
    );

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains(
            "changed payload field name for `Command::Data` item 1: `value` -> `new_value`"
        ),
        "{stderr}",
    );
}
