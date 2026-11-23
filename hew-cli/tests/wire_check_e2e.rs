mod support;

use std::process::{Command, Output};

use support::{hew_binary, repo_root};

fn run_wire_check(current: &str, baseline: &str) -> Output {
    let dir = support::tempdir();
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
fn wire_check_cli_reports_compatible_warning_and_incompatible_exit_status() {
    let clean = run_wire_check(
        "#[wire]\ntype Msg {\n    id: string @1;\n}\n",
        "#[wire]\ntype Msg {\n    id: string @1;\n}\n",
    );
    let clean_stderr = String::from_utf8_lossy(&clean.stderr);
    assert!(clean.status.success(), "{clean_stderr}");
    assert!(
        clean.stdout.is_empty(),
        "clean wire check should not print to stdout: {}",
        String::from_utf8_lossy(&clean.stdout)
    );
    assert!(
        clean_stderr.contains("wire check: 0 error(s), 0 warning(s)"),
        "{clean_stderr}"
    );

    let warn_ok = run_wire_check(
        "#[wire]\ntype Msg {\n    id: string @1;\n    extra: i32 @2;\n}\n",
        "#[wire]\ntype Msg {\n    id: string @1;\n}\n",
    );
    let warn_stderr = String::from_utf8_lossy(&warn_ok.stderr);
    assert!(warn_ok.status.success(), "{warn_stderr}");
    assert!(
        warn_stderr.contains("warning:") && warn_stderr.contains("Msg.extra @2"),
        "{warn_stderr}"
    );
    assert!(
        warn_stderr.contains("wire check: 0 error(s), 1 warning(s)"),
        "{warn_stderr}"
    );

    let breaking = run_wire_check(
        "#[wire]\ntype Msg {\n    count: i32 @1;\n}\n",
        "#[wire]\ntype Msg {\n    count: string @1;\n}\n",
    );
    let breaking_stderr = String::from_utf8_lossy(&breaking.stderr);
    assert_eq!(breaking.status.code(), Some(1), "{breaking_stderr}");
    assert!(
        breaking_stderr.contains("error:") && breaking_stderr.contains("Msg.count @1"),
        "{breaking_stderr}"
    );
    assert!(
        breaking_stderr.contains("wire check: 1 error(s), 0 warning(s)"),
        "{breaking_stderr}"
    );
}

#[test]
fn wire_check_cli_exits_nonzero_on_frontend_and_io_errors() {
    let parse_err = run_wire_check(
        "#[wire]\ntype Msg { id: string @1; also_id: i32 @1; }\n",
        "#[wire]\ntype Msg {\n    id: string @1;\n}\n",
    );
    let parse_stderr = String::from_utf8_lossy(&parse_err.stderr);
    assert_eq!(parse_err.status.code(), Some(1), "{parse_stderr}");
    assert!(parse_stderr.contains("@1"), "{parse_stderr}");

    let dir = support::tempdir();
    let missing_path = dir.path().join("missing.hew");
    let baseline_path = dir.path().join("baseline.hew");
    std::fs::write(&baseline_path, "#[wire]\ntype Msg { id: string @1; }\n").unwrap();
    let io_err = Command::new(hew_binary())
        .args([
            "wire",
            "check",
            missing_path.to_str().unwrap(),
            "--against",
            baseline_path.to_str().unwrap(),
        ])
        .current_dir(repo_root())
        .output()
        .unwrap();
    let io_stderr = String::from_utf8_lossy(&io_err.stderr);
    assert_eq!(io_err.status.code(), Some(1), "{io_stderr}");
    assert!(io_stderr.contains("cannot read"), "{io_stderr}");
}
