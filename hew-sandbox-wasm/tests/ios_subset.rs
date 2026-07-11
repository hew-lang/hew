use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use std::process::Output;
use std::sync::OnceLock;

use assert_cmd::Command;
use hew_sandbox_wasm::{compile_to_sandbox_bytecode, Diagnostic, SandboxBytecodePackage};
use serde::Deserialize;

const SANDBOX_PROFILE: &str = "sandbox-vm-export";
const HEW_SEED: &str = "42";
const DEFAULT_TEMPLATE: &str = r#"// Hew: safe concurrency with actors
// Try the examples or tutorials to learn more!

actor Counter {
    let count: i64;

    receive fn increment(n: i64) -> i64 {
        count = count + n;
        count
    }
}

fn main() {
    let c = spawn Counter(count: 0);
    await c.increment(5);
    await c.increment(3);
    match await c.increment(12) {
        Ok(total) => println(f"Total: {total}"),
        Err(_) => println("send failed"),
    }
}
"#;

#[derive(Debug, Deserialize)]
struct ExampleFixture {
    id: String,
    source: String,
}

#[derive(Debug, Deserialize)]
struct TutorialFixture {
    id: String,
    #[serde(rename = "starterCode")]
    starter_code: String,
}

#[derive(Debug, Clone, Copy)]
enum Expectation {
    Parity,
    FailClosedProfile(&'static str),
    FailClosedTypecheck,
}

#[derive(Debug)]
struct IosCase {
    id: String,
    source: String,
    expectation: Expectation,
}

#[cfg_attr(windows, ignore)]
#[test]
fn ios_default_examples_and_tutorials_are_sandbox_safe() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();
    let cases = load_cases();
    let unique_ids: BTreeSet<_> = cases.iter().map(|case| case.id.as_str()).collect();

    assert_eq!(cases.len(), 27, "the pinned iOS core corpus changed");
    assert_eq!(
        unique_ids.len(),
        cases.len(),
        "the pinned iOS core corpus contains duplicate ids"
    );

    for case in &cases {
        assert_case(case);
    }
}

fn load_cases() -> Vec<IosCase> {
    let mut cases = vec![IosCase {
        id: "default-template".to_owned(),
        source: DEFAULT_TEMPLATE.to_owned(),
        expectation: Expectation::FailClosedTypecheck,
    }];
    let examples: Vec<ExampleFixture> =
        serde_json::from_str(include_str!("fixtures/ios/examples.json"))
            .expect("iOS example fixtures should parse");
    let tutorials: Vec<TutorialFixture> =
        serde_json::from_str(include_str!("fixtures/ios/tutorials.json"))
            .expect("iOS tutorial fixtures should parse");

    cases.extend(examples.into_iter().map(|fixture| {
        let expectation = match fixture.id.as_str() {
            "higher-order-functions" => Expectation::FailClosedProfile("unknown_symbol"),
            _ => Expectation::Parity,
        };
        IosCase {
            id: format!("example/{}", fixture.id),
            source: fixture.source,
            expectation,
        }
    }));
    cases.extend(tutorials.into_iter().map(|fixture| {
        let expectation = match fixture.id.as_str() {
            "closures" => Expectation::FailClosedProfile("unknown_symbol"),
            "closure-captures" => Expectation::FailClosedProfile("reserved_runtime_feature"),
            _ => Expectation::Parity,
        };
        IosCase {
            id: format!("tutorial/{}", fixture.id),
            source: fixture.starter_code,
            expectation,
        }
    }));
    cases
}

fn assert_case(case: &IosCase) {
    let native = run_native(&case.source, &case.id);
    let compiled = compile_to_sandbox_bytecode(&case.source, Some(SANDBOX_PROFILE))
        .unwrap_or_else(|err| panic!("sandbox compile threw for {}: {err}", case.id));

    match case.expectation {
        Expectation::Parity => {
            assert!(
                native.status.success(),
                "{} is shipped as runnable iOS Hew but failed natively:\n{}",
                case.id,
                describe_output(&native)
            );
            assert_no_error_diagnostics(case, &compiled.diagnostics);
            let bytecode = compiled.bytecode.unwrap_or_else(|| {
                panic!(
                    "sandbox compile emitted no bytecode for {}; diagnostics:\n{}",
                    case.id,
                    diagnostics_dump(&compiled.diagnostics)
                )
            });
            let sandbox = run_sandbox(&bytecode, &case.id);
            assert_eq!(
                sandbox.status.code(),
                native.status.code(),
                "{} exit-code mismatch\nnative:\n{}\nsandbox:\n{}",
                case.id,
                describe_output(&native),
                describe_output(&sandbox)
            );
            assert_eq!(
                String::from_utf8_lossy(&sandbox.stdout),
                String::from_utf8_lossy(&native.stdout),
                "{} stdout mismatch\nnative:\n{}\nsandbox:\n{}",
                case.id,
                describe_output(&native),
                describe_output(&sandbox)
            );
        }
        Expectation::FailClosedProfile(diagnostic_kind) => {
            assert!(
                native.status.success(),
                "{} should be valid native Hew before the sandbox profile rejects it:\n{}",
                case.id,
                describe_output(&native)
            );
            assert!(
                compiled.bytecode.is_none(),
                "{} must fail closed but emitted sandbox bytecode",
                case.id
            );
            assert!(
                compiled
                    .diagnostics
                    .iter()
                    .any(|diagnostic| diagnostic.severity == "error"
                        && diagnostic.phase == "profile"
                        && diagnostic.kind == diagnostic_kind),
                "{} must fail closed with profile diagnostic {diagnostic_kind:?}; got:\n{}",
                case.id,
                diagnostics_dump(&compiled.diagnostics)
            );
        }
        Expectation::FailClosedTypecheck => {
            assert!(
                !native.status.success(),
                "{} is expected to be invalid in current native Hew",
                case.id
            );
            assert!(
                compiled.bytecode.is_none(),
                "{} must not emit sandbox bytecode after typecheck failure",
                case.id
            );
            assert!(
                compiled
                    .diagnostics
                    .iter()
                    .any(|diagnostic| diagnostic.severity == "error"
                        && diagnostic.phase == "typecheck"),
                "{} must fail closed with a typecheck diagnostic; got:\n{}",
                case.id,
                diagnostics_dump(&compiled.diagnostics)
            );
        }
    }
}

fn assert_no_error_diagnostics(case: &IosCase, diagnostics: &[Diagnostic]) {
    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| diagnostic.severity != "error"),
        "{} produced sandbox compile errors:\n{}",
        case.id,
        diagnostics_dump(diagnostics)
    );
}

fn run_native(source: &str, id: &str) -> Output {
    let tempdir = tempfile::tempdir()
        .unwrap_or_else(|err| panic!("failed to create tempdir for {id}: {err}"));
    let source_path = tempdir.path().join("main.hew");
    std::fs::write(&source_path, source)
        .unwrap_or_else(|err| panic!("failed to write native source for {id}: {err}"));

    Command::new(hew_binary())
        .arg("run")
        .arg(source_path)
        .current_dir(repo_root())
        .env("HEWPATH", repo_root())
        .env("HEW_SEED", HEW_SEED)
        .env("NO_COLOR", "1")
        .output()
        .unwrap_or_else(|err| panic!("failed to spawn native `hew run` for {id}: {err}"))
}

fn run_sandbox(bytecode: &SandboxBytecodePackage, id: &str) -> Output {
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

fn set_test_hewpath() {
    std::env::set_var("HEWPATH", repo_root());
}

fn diagnostics_dump(diagnostics: &[Diagnostic]) -> String {
    serde_json::to_string_pretty(diagnostics).expect("diagnostics should serialize")
}

fn describe_output(output: &Output) -> String {
    format!(
        "status: {:?}\nstdout:\n{}\nstderr:\n{}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    )
}
