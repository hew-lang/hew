use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use std::process::Output;
use std::sync::OnceLock;

use assert_cmd::Command;
use hew_sandbox_wasm::{compile_to_sandbox_bytecode, Diagnostic};
use serde::Deserialize;

const RUNNABLE: &str = "runnable";
const UNSUPPORTED_NATIVE_ONLY: &str = "unsupported_native_only";
const SANDBOX_PROFILE: &str = "sandbox-vm-export";
const HEW_SEED: &str = "42";

const REQUIRED_SANDBOX_EXAMPLES: &[&str] = &[
    "basics/hello_world",
    "basics/fibonacci",
    "concurrency/counter_actor",
    "concurrency/actor_pipeline",
    "concurrency/supervisor",
    "machines/traffic_light",
];

#[derive(Debug, Deserialize)]
struct ManifestEntry {
    id: String,
    source_path: PathBuf,
    expected_path: PathBuf,
    capabilities: Capabilities,
}

#[derive(Debug, Deserialize)]
struct Capabilities {
    sandbox: String,
}

#[test]
fn playground_manifest_sources_run_at_native_sandbox_parity() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    let playground_dir = playground_dir();
    let entries = load_manifest(&playground_dir);
    let mut seen_ids = BTreeSet::new();

    for entry in &entries {
        assert!(
            seen_ids.insert(entry.id.as_str()),
            "duplicate playground manifest id {}",
            entry.id
        );
        assert_manifest_entry(entry, &playground_dir);
    }

    for required_id in REQUIRED_SANDBOX_EXAMPLES {
        assert!(
            seen_ids.contains(required_id),
            "playground manifest missing required sandbox parity entry {required_id}"
        );
    }
}

fn assert_manifest_entry(entry: &ManifestEntry, playground_dir: &Path) {
    let source_path = playground_dir.join(&entry.source_path);
    let source = std::fs::read_to_string(&source_path).unwrap_or_else(|err| {
        panic!(
            "failed to read playground source {} for {}: {err}",
            source_path.display(),
            entry.id
        )
    });
    let output = compile_to_sandbox_bytecode(&source, Some(SANDBOX_PROFILE))
        .unwrap_or_else(|err| panic!("compile_to_sandbox_bytecode threw for {}: {err}", entry.id));

    match entry.capabilities.sandbox.as_str() {
        RUNNABLE => {
            assert!(
                output
                    .diagnostics
                    .iter()
                    .all(|diagnostic| diagnostic.severity != "error"),
                "runnable playground entry {} produced error diagnostics:\n{}",
                entry.id,
                diagnostics_dump(&output.diagnostics)
            );
            let bytecode = output.bytecode.unwrap_or_else(|| {
                panic!(
                    "runnable playground entry {} did not emit bytecode; diagnostics:\n{}",
                    entry.id,
                    diagnostics_dump(&output.diagnostics)
                )
            });
            let native = run_native(&source_path);
            let sandbox = run_sandbox(&bytecode, &entry.id);
            let expected_path = playground_dir.join(&entry.expected_path);
            let expected = std::fs::read_to_string(&expected_path).unwrap_or_else(|err| {
                panic!(
                    "failed to read expected output {} for {}: {err}",
                    expected_path.display(),
                    entry.id
                )
            });

            assert_eq!(
                sandbox.status.code(),
                native.status.code(),
                "{} exit-code mismatch\nnative:\n{}\nsandbox:\n{}",
                entry.id,
                describe_output(&native),
                describe_output(&sandbox)
            );
            assert_eq!(
                String::from_utf8_lossy(&sandbox.stdout),
                String::from_utf8_lossy(&native.stdout),
                "{} stdout mismatch\nnative:\n{}\nsandbox:\n{}",
                entry.id,
                describe_output(&native),
                describe_output(&sandbox)
            );
            assert_eq!(
                String::from_utf8_lossy(&sandbox.stdout),
                expected,
                "{} sandbox stdout did not match {}",
                entry.id,
                expected_path.display()
            );
        }
        UNSUPPORTED_NATIVE_ONLY => {
            assert!(
                output.bytecode.is_none(),
                "unsupported_native_only playground entry {} unexpectedly emitted bytecode",
                entry.id
            );
            assert!(
                output.diagnostics.iter().any(is_typed_profile_rejection),
                "unsupported_native_only playground entry {} must fail closed with a typed profile diagnostic; got:\n{}",
                entry.id,
                diagnostics_dump(&output.diagnostics)
            );
        }
        other => panic!(
            "manifest entry {} has unsupported capabilities.sandbox {other:?}; expected {RUNNABLE:?} or {UNSUPPORTED_NATIVE_ONLY:?}",
            entry.id
        ),
    }
}

fn load_manifest(playground_dir: &Path) -> Vec<ManifestEntry> {
    let manifest_path = playground_dir.join("manifest.json");
    let manifest = std::fs::read_to_string(&manifest_path).unwrap_or_else(|err| {
        panic!(
            "failed to read playground manifest {}: {err}",
            manifest_path.display()
        )
    });
    serde_json::from_str(&manifest).unwrap_or_else(|err| {
        panic!(
            "failed to parse playground manifest {}: {err}",
            manifest_path.display()
        )
    })
}

fn playground_dir() -> PathBuf {
    repo_root().join("examples").join("playground")
}

fn set_test_hewpath() {
    std::env::set_var("HEWPATH", repo_root());
}

fn is_typed_profile_rejection(diagnostic: &Diagnostic) -> bool {
    diagnostic.phase == "profile"
        && !diagnostic.kind.is_empty()
        && (diagnostic.kind == "sandbox_profile_rejected"
            || diagnostic.kind.contains("NATIVE_ONLY")
            || diagnostic.kind.starts_with("reserved_")
            || diagnostic.kind.starts_with("unknown_")
            || diagnostic.kind.ends_with("_rejected"))
}

fn diagnostics_dump(diagnostics: &[Diagnostic]) -> String {
    serde_json::to_string_pretty(diagnostics).expect("diagnostics should serialize")
}

fn run_native(source_path: &Path) -> Output {
    Command::new(hew_binary())
        .arg("run")
        .arg(source_path)
        .current_dir(repo_root())
        .env("HEWPATH", repo_root())
        .env("HEW_SEED", HEW_SEED)
        .env("NO_COLOR", "1")
        .output()
        .unwrap_or_else(|err| panic!("failed to spawn native `hew run`: {err}"))
}

fn run_sandbox(bytecode: &hew_sandbox_wasm::SandboxBytecodePackage, id: &str) -> Output {
    let bytecode_json = serde_json::to_string_pretty(bytecode)
        .unwrap_or_else(|err| panic!("failed to serialize bytecode for {id}: {err}"));
    let tempdir = tempfile::tempdir()
        .unwrap_or_else(|err| panic!("failed to create tempdir for {id}: {err}"));
    let bytecode_path = tempdir.path().join("bytecode.json");
    std::fs::write(&bytecode_path, bytecode_json)
        .unwrap_or_else(|err| panic!("failed to write bytecode for {id}: {err}"));

    Command::new("npm")
        .arg("--prefix")
        .arg(repo_root().join("hew-sandbox-vm"))
        .arg("run")
        .arg("-s")
        .arg("parity:run")
        .arg("--")
        .arg(bytecode_path)
        .arg("--seed")
        .arg(HEW_SEED)
        .current_dir(repo_root())
        .env("NO_COLOR", "1")
        .output()
        .unwrap_or_else(|err| panic!("failed to spawn sandbox parity runner for {id}: {err}"))
}

fn ensure_native_toolchain() {
    static NATIVE_TOOLCHAIN: OnceLock<()> = OnceLock::new();
    NATIVE_TOOLCHAIN.get_or_init(|| {
        run_bootstrap_command(
            "cargo build -q -p hew-cli",
            std::process::Command::new("cargo")
                .args(["build", "-q", "-p", "hew-cli"])
                .current_dir(repo_root()),
        );
        run_bootstrap_command(
            "cargo build -q -p hew-lib",
            std::process::Command::new("cargo")
                .args(["build", "-q", "-p", "hew-lib"])
                .current_dir(repo_root()),
        );
    });
}

fn ensure_parity_runner_built() {
    static PARITY_RUNNER: OnceLock<()> = OnceLock::new();
    PARITY_RUNNER.get_or_init(|| {
        let vm_dir = repo_root().join("hew-sandbox-vm");
        assert!(
            vm_dir.join("node_modules").is_dir(),
            "hew-sandbox-vm dependencies are not installed; run `npm --prefix hew-sandbox-vm ci` or `make sandbox-parity`"
        );
        run_bootstrap_command(
            "npm --prefix hew-sandbox-vm run -s build",
            std::process::Command::new("npm")
                .arg("--prefix")
                .arg(&vm_dir)
                .arg("run")
                .arg("-s")
                .arg("build")
                .current_dir(repo_root()),
        );
    });
}

fn run_bootstrap_command(label: &str, command: &mut std::process::Command) {
    let output = command
        .output()
        .unwrap_or_else(|err| panic!("failed to invoke `{label}`: {err}"));
    assert!(
        output.status.success(),
        "`{label}` failed\n{}",
        describe_output(&output)
    );
}

fn hew_binary() -> PathBuf {
    if let Ok(path) = std::env::var("CARGO_BIN_EXE_hew") {
        return PathBuf::from(path);
    }
    target_debug_dir().join(format!("hew{}", std::env::consts::EXE_SUFFIX))
}

fn target_debug_dir() -> PathBuf {
    if let Ok(target_dir) = std::env::var("CARGO_TARGET_DIR") {
        return PathBuf::from(target_dir).join("debug");
    }
    repo_root().join("target").join("debug")
}

fn repo_root() -> &'static Path {
    static REPO_ROOT: OnceLock<PathBuf> = OnceLock::new();
    REPO_ROOT
        .get_or_init(|| {
            Path::new(env!("CARGO_MANIFEST_DIR"))
                .parent()
                .expect("hew-sandbox-wasm crate should have a workspace parent")
                .to_path_buf()
        })
        .as_path()
}

fn describe_output(output: &Output) -> String {
    format!(
        "status: {:?}\nstdout:\n{}\nstderr:\n{}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}
