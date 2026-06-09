use std::path::PathBuf;
use std::process::Command;

#[test]
fn actor_e2e_counter_compile_and_exit_code() {
    compile_and_run_actor_fixture("actor_counter", 42);
}

#[test]
fn actor_init_on_start_compile_and_exit_code() {
    compile_and_run_actor_fixture("actor_counter_init", 42);
}

#[test]
fn actor_on_stop_compile_and_exit_code() {
    compile_and_run_actor_fixture("actor_on_stop", 42);
}

fn compile_and_run_actor_fixture(fixture_name: &str, expected_exit_code: i32) {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let repo = manifest_dir.parent().expect("workspace root");
    let binary = repo.join(format!(".tmp/compile-out/{fixture_name}"));
    let _ = std::fs::remove_file(&binary);
    let cargo = std::env::var("CARGO").unwrap_or_else(|_| "cargo".to_string());

    if !repo.join("target/debug/libhew.a").exists() {
        let stdlib = Command::new(&cargo)
            .current_dir(repo)
            .args(["build", "--quiet", "-p", "hew-lib"])
            .output()
            .expect("build libhew.a");
        assert!(
            stdlib.status.success(),
            "cargo build -p hew-lib failed\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&stdlib.stdout),
            String::from_utf8_lossy(&stdlib.stderr)
        );
    }

    let compile = Command::new(&cargo)
        .current_dir(repo)
        .args([
            "run",
            "--quiet",
            "-p",
            "hew-cli",
            "--bin",
            "hew",
            "--",
            "compile",
            &format!("examples/v05/{fixture_name}.hew"),
        ])
        .output()
        .expect("run hew compile");
    assert!(
        compile.status.success(),
        "hew compile failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let run = Command::new(&binary).output().expect("run actor fixture");
    assert_eq!(run.status.code(), Some(expected_exit_code));

    let _ = std::fs::remove_file(binary);
}
