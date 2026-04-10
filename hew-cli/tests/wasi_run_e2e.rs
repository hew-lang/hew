mod support;

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use serde::Deserialize;
use support::{hew_binary, repo_root, require_wasi_runner};

#[derive(Debug, Deserialize)]
struct PlaygroundEntry {
    id: String,
    source_path: PathBuf,
    expected_path: PathBuf,
}

fn playground_root() -> PathBuf {
    repo_root().join("examples").join("playground")
}

fn load_playground_manifest() -> Vec<PlaygroundEntry> {
    let manifest_path = playground_root().join("manifest.json");
    let manifest = fs::read_to_string(&manifest_path).expect("read playground manifest");
    serde_json::from_str(&manifest).expect("parse playground manifest")
}

fn run_wasi_example(source: &Path) -> Output {
    Command::new(hew_binary())
        .arg("run")
        .arg(source)
        .arg("--target")
        .arg("wasm32-wasi")
        .current_dir(repo_root())
        .output()
        .expect("run hew --target wasm32-wasi")
}

#[test]
fn curated_playground_examples_run_under_wasi() {
    require_wasi_runner();

    let manifest = load_playground_manifest();
    assert_eq!(
        manifest.len(),
        11,
        "expected the curated 11-snippet manifest"
    );

    let runnable: Vec<_> = manifest
        .iter()
        .filter(|entry| entry.id != "concurrency/supervisor")
        .collect();
    assert_eq!(
        runnable.len(),
        10,
        "expected exactly 10 WASI-runnable playground snippets"
    );

    for entry in runnable {
        let source = playground_root().join(&entry.source_path);
        let expected = fs::read_to_string(playground_root().join(&entry.expected_path))
            .expect("read expected output");
        let output = run_wasi_example(&source);
        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);

        assert!(
            output.status.success(),
            "hew run --target wasm32-wasi failed for {}\nstdout:\n{}\nstderr:\n{}",
            entry.id,
            stdout,
            stderr,
        );
        assert_eq!(
            stdout.as_ref(),
            expected.as_str(),
            "stdout mismatch for {}\nstderr:\n{}",
            entry.id,
            stderr,
        );
    }
}

#[test]
fn supervisor_stays_on_the_unsupported_diagnostic_path_under_wasi() {
    require_wasi_runner();

    let source = playground_root().join("concurrency").join("supervisor.hew");
    let output = run_wasi_example(&source);
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert_eq!(
        output.status.code(),
        Some(125),
        "supervisor should fail in compile phase\nstdout:\n{stdout}\nstderr:\n{stderr}",
    );
    assert!(
        stderr.contains("not supported on WASM32"),
        "expected unsupported WASM diagnostic\nstderr:\n{stderr}",
    );
    assert!(
        stderr.contains("hew.supervisor.new"),
        "expected explicit supervisor lowering failure\nstderr:\n{stderr}",
    );
}
