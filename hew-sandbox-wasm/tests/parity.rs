use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use std::process::Output;
use std::sync::OnceLock;

use assert_cmd::Command;
use hew_sandbox_wasm::{compile_to_sandbox_bytecode, Diagnostic};

const SANDBOX_PROFILE: &str = "sandbox-vm-export";
const HEW_SEED: &str = "42";

const REQUIRED_PARITY_TEST_NAMES: &[&str] = &[
    "hello_world",
    "fibonacci",
    "pattern_matching",
    "counter_actor",
    "actor_pipeline",
    "supervisor",
    "traffic_light",
];

const PARITY_CASES: &[ParityCase] = &[
    ParityCase {
        test_name: "hello_world",
        source_rel: "examples/playground/basics/hello_world.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "fibonacci",
        source_rel: "examples/playground/basics/fibonacci.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "pattern_matching",
        source_rel: "examples/playground/types/pattern_matching.hew",
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "counter_actor",
        source_rel: "examples/playground/concurrency/counter_actor.hew",
        // Actor/supervisor/machine support now emits bytecode; no profile divergence.
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "actor_pipeline",
        source_rel: "examples/playground/concurrency/actor_pipeline.hew",
        // Actor support now emits bytecode; no profile divergence.
        accepted_divergences: &[],
    },
    ParityCase {
        test_name: "supervisor",
        source_rel: "examples/playground/concurrency/supervisor.hew",
        // `supervisor_stop` is a native-only builtin not yet in the sandbox profile.
        accepted_divergences: &[AcceptedDivergence::UnknownProfileSymbol],
    },
    ParityCase {
        test_name: "traffic_light",
        source_rel: "examples/playground/machines/traffic_light.hew",
        // Machine support now emits bytecode; no profile divergence.
        accepted_divergences: &[],
    },
];

#[derive(Debug, Clone, Copy)]
struct ParityCase {
    test_name: &'static str,
    source_rel: &'static str,
    accepted_divergences: &'static [AcceptedDivergence],
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum AcceptedDivergence {
    #[doc = "Catalog: docs/sandbox-vm-divergences.md#unknown-profile-symbol"]
    UnknownProfileSymbol,
}

impl AcceptedDivergence {
    fn diagnostic_kind(self) -> &'static str {
        match self {
            Self::UnknownProfileSymbol => "unknown_symbol",
        }
    }

    fn reason(self) -> &'static str {
        match self {
            Self::UnknownProfileSymbol => {
                "playground source calls helpers or builtins that are not in the current sandbox profile allowlist"
            }
        }
    }
}

#[test]
fn minimum_parity_set_is_enforced_by_test_name() {
    let required: BTreeSet<_> = REQUIRED_PARITY_TEST_NAMES.iter().copied().collect();
    let actual: BTreeSet<_> = PARITY_CASES.iter().map(|case| case.test_name).collect();

    assert_eq!(
        actual, required,
        "native↔sandbox parity cases must exactly cover the required playground set"
    );
}

#[test]
fn playground_sources_match_native_or_catalogued_divergence() {
    set_test_hewpath();
    ensure_native_toolchain();
    ensure_parity_runner_built();

    for case in PARITY_CASES {
        assert_case(case);
    }
}

fn assert_case(case: &ParityCase) {
    let repo_root = repo_root();
    let source_path = repo_root.join(case.source_rel);
    let native = run_native(&source_path);
    let source = std::fs::read_to_string(&source_path).unwrap_or_else(|err| {
        panic!(
            "failed to read parity source {} for {}: {err}",
            source_path.display(),
            case.test_name
        )
    });
    let sandbox_compile = compile_to_sandbox_bytecode(&source, Some(SANDBOX_PROFILE))
        .unwrap_or_else(|err| panic!("sandbox compile threw for {}: {err}", case.test_name));

    if !case.accepted_divergences.is_empty() {
        assert_accepted_divergences(case, &sandbox_compile.diagnostics);
        assert!(
            sandbox_compile.bytecode.is_none(),
            "{} declares accepted profile divergences, but sandbox bytecode was emitted; remove the divergence catalog entry and enable parity comparison",
            case.test_name
        );
        return;
    }

    assert_no_error_diagnostics(case, &sandbox_compile.diagnostics);
    let bytecode = sandbox_compile.bytecode.unwrap_or_else(|| {
        panic!(
            "sandbox compile emitted no bytecode for {}; diagnostics:\n{}",
            case.test_name,
            diagnostics_dump(&sandbox_compile.diagnostics)
        )
    });
    let bytecode_json = serde_json::to_string_pretty(&bytecode)
        .unwrap_or_else(|err| panic!("failed to serialize bytecode for {}: {err}", case.test_name));
    let tempdir = tempfile::tempdir()
        .unwrap_or_else(|err| panic!("failed to create tempdir for {}: {err}", case.test_name));
    let bytecode_path = tempdir.path().join("bytecode.json");
    std::fs::write(&bytecode_path, bytecode_json)
        .unwrap_or_else(|err| panic!("failed to write bytecode for {}: {err}", case.test_name));

    let sandbox = run_sandbox(&bytecode_path);
    assert_exit_code_parity(case, &native, &sandbox);
    assert_stdout_parity(case, &native, &sandbox);
}

fn assert_accepted_divergences(case: &ParityCase, diagnostics: &[Diagnostic]) {
    let actual_error_kinds: BTreeSet<&str> = diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.severity == "error")
        .map(|diagnostic| diagnostic.kind.as_str())
        .collect();
    let accepted_kinds: BTreeSet<&str> = case
        .accepted_divergences
        .iter()
        .map(|divergence| divergence.diagnostic_kind())
        .collect();

    for divergence in case.accepted_divergences {
        let kind = divergence.diagnostic_kind();
        assert!(
            actual_error_kinds.contains(kind),
            "{} declared accepted divergence {:?} ({kind}: {}) but that diagnostic did not trigger; remove or update the catalog entry.\nActual diagnostics:\n{}",
            case.test_name,
            divergence,
            divergence.reason(),
            diagnostics_dump(diagnostics)
        );
    }

    for kind in &actual_error_kinds {
        assert!(
            accepted_kinds.contains(kind),
            "{} produced uncatalogued sandbox diagnostic kind {kind:?}; add an AcceptedDivergence with reason and diagnostic linkage or fix the parity gap.\nDiagnostics:\n{}",
            case.test_name,
            diagnostics_dump(diagnostics)
        );
    }
}

fn assert_no_error_diagnostics(case: &ParityCase, diagnostics: &[Diagnostic]) {
    assert!(
        diagnostics
            .iter()
            .all(|diagnostic| diagnostic.severity != "error"),
        "{} produced sandbox compile errors:\n{}",
        case.test_name,
        diagnostics_dump(diagnostics)
    );
}

fn assert_exit_code_parity(case: &ParityCase, native: &Output, sandbox: &Output) {
    assert_eq!(
        sandbox.status.code(),
        native.status.code(),
        "{} exit-code mismatch\nnative:\n{}\nsandbox:\n{}",
        case.test_name,
        describe_output(native),
        describe_output(sandbox)
    );
}

fn assert_stdout_parity(case: &ParityCase, native: &Output, sandbox: &Output) {
    assert_eq!(
        String::from_utf8_lossy(&sandbox.stdout),
        String::from_utf8_lossy(&native.stdout),
        "{} stdout mismatch\nnative:\n{}\nsandbox:\n{}",
        case.test_name,
        describe_output(native),
        describe_output(sandbox)
    );
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

fn run_sandbox(bytecode_path: &Path) -> Output {
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
        .unwrap_or_else(|err| panic!("failed to spawn sandbox parity runner: {err}"))
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
        assert!(
            vm_dir
                .join("dist")
                .join("interpreter")
                .join("parity-runner.js")
                .is_file(),
            "sandbox parity runner was not built at hew-sandbox-vm/dist/interpreter/parity-runner.js"
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
