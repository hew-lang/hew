use std::path::PathBuf;
use std::process::Command;

#[test]
fn actor_e2e_counter_compile_and_exit_code() {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let repo = manifest_dir.parent().expect("workspace root");
    let binary = repo.join(".tmp/compile-out/actor_counter");
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
            "examples/v05/actor_counter.hew",
        ])
        .output()
        .expect("run hew compile");
    assert!(
        compile.status.success(),
        "hew compile failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&compile.stdout),
        String::from_utf8_lossy(&compile.stderr)
    );

    let run = Command::new(&binary).output().expect("run actor_counter");
    assert_eq!(run.status.code(), Some(42));

    let _ = std::fs::remove_file(binary);
}
