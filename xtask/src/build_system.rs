use std::collections::BTreeSet;
use std::env;
use std::ffi::{OsStr, OsString};
use std::path::{Path, PathBuf};
use std::process::{Command, ExitStatus};

use serde_json::Value;

type Result<T> = std::result::Result<T, String>;

const NEXTEST: &str = "cargo-nextest@0.9.99";
const CARGO_DENY: &str = "cargo-deny@0.20.2";
const CARGO_ABOUT: &str = "cargo-about@0.9.1";
const LLVM_COV: &str = "cargo-llvm-cov@0.8.7";
const WASM_PACK: &str = "wasm-pack@0.15.0";
const WASMTIME: &str = "wasmtime@47.0.3";

#[derive(Debug, Clone, Copy)]
struct GateSpec {
    name: &'static str,
    dependencies: &'static [&'static str],
    tools: &'static [&'static str],
    targets: &'static [&'static str],
    ast_grep: bool,
}

const GATES: &[GateSpec] = &[
    gate_spec("native", &[], &[], &[], false),
    gate_spec("smoke", &[], &[NEXTEST], &[], false),
    gate_spec(
        "workspace",
        &[],
        &[NEXTEST, WASMTIME],
        &["wasm32-wasip1"],
        false,
    ),
    gate_spec("platform-full", &["workspace"], &[], &[], false),
    gate_spec(
        "platform-smoke",
        &[],
        &[NEXTEST, WASMTIME],
        &["wasm32-wasip1"],
        false,
    ),
    gate_spec("cabi", &[], &[NEXTEST], &[], false),
    gate_spec("vertical-slice", &[], &[], &[], false),
    gate_spec("hew-ratchet", &[], &[], &[], false),
    gate_spec("hew-inventory", &[], &[], &[], false),
    gate_spec("compiled-artifact-pack", &[], &[], &[], false),
    gate_spec("compiled-artifact-unpack", &[], &[], &[], false),
    gate_spec("compiled-shard", &[], &[], &[], false),
    gate_spec("stdlib-ratchet", &[], &[], &[], false),
    gate_spec(
        "playground",
        &["sandbox-fixtures-check"],
        &[WASM_PACK],
        &["wasm32-unknown-unknown"],
        false,
    ),
    gate_spec(
        "playground-wasi",
        &[],
        &[WASMTIME],
        &["wasm32-wasip1"],
        false,
    ),
    gate_spec(
        "freebsd",
        &["workspace", "cabi", "vertical-slice", "hew-ratchet"],
        &[NEXTEST, WASMTIME],
        &["wasm32-wasip1"],
        false,
    ),
    gate_spec("release-smoke", &[], &[], &[], false),
    gate_spec("release-link", &[], &[], &[], false),
    gate_spec("release-verify", &["release-link"], &[], &[], false),
    gate_spec("format", &[], &[], &[], false),
    gate_spec("clippy", &[], &[], &[], false),
    gate_spec("clippy-json", &[], &[], &[], false),
    gate_spec("structural-bootstrap", &[], &[], &[], false),
    gate_spec("structural-bootstrap-contract", &[], &[], &[], true),
    gate_spec("structural-lint", &[], &[], &[], true),
    gate_spec("freebsd-contract", &[], &[], &[], false),
    gate_spec("release-contract", &[], &[], &[], false),
    gate_spec("cutover-contract", &[], &[], &[], false),
    gate_spec("sanitizer-contract", &[], &[], &[], false),
    gate_spec("reachability", &[], &[], &[], false),
    gate_spec("hew-fmt", &[], &[], &[], false),
    gate_spec("ffi", &[], &[], &[], false),
    gate_spec("runtime-poison-lint", &[], &[], &[], false),
    gate_spec("wasm-todo-lint", &[], &[], &[], false),
    gate_spec("codegen-identity", &[], &[], &[], false),
    gate_spec("codegen-traps", &[], &[], &[], false),
    gate_spec("sys-closure", &[], &[], &[], false),
    gate_spec("leak-scan", &[], &[], &[], false),
    gate_spec("ll-identity", &[], &[], &[], false),
    gate_spec("sandbox-coverage", &[], &[], &[], false),
    gate_spec("example-contract", &[], &[], &[], false),
    gate_spec("docs-examples", &[], &[], &[], false),
    gate_spec("sandbox-fixtures", &[], &[], &[], false),
    gate_spec("wasm-capability", &[], &[], &[], false),
    gate_spec("playground-manifest", &[], &[], &[], false),
    gate_spec(
        "wasm-package",
        &[],
        &[WASM_PACK],
        &["wasm32-unknown-unknown"],
        false,
    ),
    gate_spec("fuzz-corpus", &[], &[], &[], false),
    gate_spec("fuzz-oracle", &[], &[], &[], false),
    gate_spec("fuzz-oracle-selftest", &[], &[], &[], false),
    gate_spec("checked-mir-verify", &[], &[], &[], false),
    gate_spec("checked-mir-golden", &[], &[], &[], false),
    gate_spec("checked-mir-run", &[], &[], &[], false),
    gate_spec("checked-mir-expect", &[], &[], &[], false),
    gate_spec("ll-diff", &[], &[], &[], false),
    gate_spec("ll-golden", &[], &[], &[], false),
    gate_spec("leak-oracle-selftest", &[], &[NEXTEST], &[], false),
    gate_spec(
        "compiler-pipeline",
        &[],
        &[NEXTEST, WASMTIME],
        &["wasm32-wasip1"],
        false,
    ),
    gate_spec("runtime-unit", &[], &[NEXTEST], &[], false),
    gate_spec("core-matrix", &[], &[], &[], false),
    gate_spec("hew-check", &[], &[], &[], false),
    gate_spec("hew-fmt-property", &[], &[], &[], false),
    gate_spec("pkg-import", &[], &[], &[], false),
    gate_spec("package-install", &[], &[], &[], false),
    gate_spec("surface-examples", &[], &[], &[], false),
    gate_spec("ux-examples", &[], &[], &[], false),
    gate_spec("doc-contract", &[], &[], &[], false),
    gate_spec("o2-contract", &[], &[], &[], false),
    gate_spec("sandbox-vm-deps", &[], &[], &[], false),
    gate_spec(
        "sandbox-parity",
        &["native", "sandbox-vm-deps"],
        &[],
        &[],
        false,
    ),
    gate_spec("observe-functional", &[], &[], &[], false),
    gate_spec("libhew-link-race", &[], &[], &[], false),
    gate_spec("macos-leak-oracle", &[], &[], &[], false),
    gate_spec("asan-fixture-selftest", &[], &[], &[], false),
    gate_spec("fuzz-smoke-bootstrap", &[], &[], &[], false),
    gate_spec(
        "lint",
        &[
            "format",
            "structural-bootstrap-contract",
            "freebsd-contract",
            "cutover-contract",
            "sanitizer-contract",
            "reachability",
            "structural-lint",
            "hew-fmt",
            "ffi",
            "runtime-poison-lint",
            "wasm-todo-lint",
            "codegen-identity",
            "codegen-traps",
            "sys-closure",
            "leak-scan",
            "ll-identity",
            "sandbox-coverage",
            "example-contract",
            "docs-examples",
            "clippy",
        ],
        &[],
        &[],
        false,
    ),
    gate_spec("licenses", &[], &[CARGO_ABOUT, CARGO_DENY], &[], false),
    gate_spec("licenses-generate", &[], &[CARGO_ABOUT], &[], false),
    gate_spec("sandbox-fixtures-check", &[], &[], &[], false),
    gate_spec("compiler-lifecycle", &[], &[], &["wasm32-wasip1"], false),
    gate_spec(
        "compiler-lifecycle-external",
        &[],
        &[],
        &["wasm32-wasip1"],
        false,
    ),
    gate_spec("stdlib-execution", &[], &[], &[], false),
    gate_spec("mqtt", &[], &[], &[], false),
    gate_spec("o2-differential", &[], &[], &[], false),
    gate_spec("coverage", &[], &[LLVM_COV, NEXTEST], &[], false),
    gate_spec("coverage-html", &[], &[LLVM_COV], &[], false),
    gate_spec("coverage-summary", &[], &[LLVM_COV], &[], false),
    gate_spec("coverage-runtime", &[], &[LLVM_COV], &[], false),
    gate_spec("asan", &[], &[], &[], false),
    gate_spec("asan-fixtures", &["asan-fixture-selftest"], &[], &[], false),
    gate_spec("tsan", &[], &[], &[], false),
    gate_spec("miri", &[], &[], &[], false),
    gate_spec("fuzz-smoke", &["fuzz-smoke-bootstrap"], &[], &[], false),
    gate_spec("stdlib-lint", &[], &[], &[], false),
    gate_spec("docs", &[], &[], &[], false),
    gate_spec("package-completions", &[], &[], &[], false),
    gate_spec("package-smoke", &[], &[], &[], false),
    gate_spec("package-verify", &[], &[], &[], false),
    gate_spec("windows-pdb", &[], &[], &[], false),
    gate_spec("build", &[], &[], &[], false),
    gate_spec(
        "ci",
        &[
            "lint",
            "licenses",
            "workspace",
            "cabi",
            "playground",
            "playground-wasi",
            "sandbox-parity",
            "vertical-slice",
            "hew-ratchet",
            "stdlib-ratchet",
            "stdlib-execution",
        ],
        &[],
        &[],
        false,
    ),
];

const fn gate_spec(
    name: &'static str,
    dependencies: &'static [&'static str],
    tools: &'static [&'static str],
    targets: &'static [&'static str],
    ast_grep: bool,
) -> GateSpec {
    GateSpec {
        name,
        dependencies,
        tools,
        targets,
        ast_grep,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Profile {
    Debug,
    Release,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Component {
    All,
    Native,
    Wasm,
    Release,
}

#[derive(Debug)]
struct BuildOptions {
    component: Component,
    profile: Profile,
    target: Option<String>,
}

pub(crate) fn run(args: &[String], root: &Path) -> Result<()> {
    let Some(command) = args.first().map(String::as_str) else {
        return Err(usage());
    };
    match command {
        "build" => build(root, &parse_build_options(&args[1..])?),
        "gate" => gate(root, &args[1..]),
        "tools" => tools(&args[1..]),
        "ci-local" => ci_local(root, &args[1..]),
        "preflight" => run_program_owned(root, "scripts/ci-preflight-dispatcher.sh", &args[1..]),
        "pre-release" => run_program_owned(root, "scripts/pre-release-validate.sh", &args[1..]),
        "assemble" => {
            let profile = if args.get(1).map(String::as_str) == Some("--release") {
                Profile::Release
            } else {
                Profile::Debug
            };
            assemble(root, profile)
        }
        "install" => install(root),
        "uninstall" => uninstall(),
        "install-hooks" => install_hooks(root),
        "clean" => clean(root),
        "help" | "--help" | "-h" => {
            println!("{}", usage());
            Ok(())
        }
        other => Err(format!("unknown xtask command {other:?}\n{}", usage())),
    }
}

fn usage() -> String {
    [
        "usage: cargo xtask <command>",
        "",
        "commands:",
        "  build [all|native|wasm|release] [--release] [--target TRIPLE]",
        "  gate <name> [--filter-expr NEXTEST_EXPRESSION]",
        "  tools --gates GATE[,GATE...] [--field tools|targets|ast-grep]",
        "  tools --verify GATE[,GATE...]",
        "  ci-local [--list]",
        "  preflight [DISPATCHER_OPTION...]",
        "  pre-release [PLATFORM...]",
        "  assemble [--release]",
        "  install",
        "  uninstall",
        "  install-hooks",
        "  clean",
        "  sandbox-fixtures [--check|--probe] [--fixtures-dir PATH]",
    ]
    .join("\n")
}

fn parse_build_options(args: &[String]) -> Result<BuildOptions> {
    let mut component = Component::All;
    let mut profile = Profile::Debug;
    let mut target = None;
    let mut index = 0;
    if let Some(value) = args.first().map(String::as_str) {
        component = match value {
            "all" => Component::All,
            "native" => Component::Native,
            "wasm" => Component::Wasm,
            "release" => {
                profile = Profile::Release;
                Component::Release
            }
            value if value.starts_with('-') => Component::All,
            other => return Err(format!("unknown build component {other:?}")),
        };
        if !value.starts_with('-') {
            index += 1;
        }
    }
    while index < args.len() {
        match args[index].as_str() {
            "--release" => {
                profile = Profile::Release;
                index += 1;
            }
            "--target" => {
                target = Some(
                    args.get(index + 1)
                        .ok_or_else(|| "--target requires a triple".to_string())?
                        .clone(),
                );
                index += 2;
            }
            other => return Err(format!("unknown build option {other:?}")),
        }
    }
    Ok(BuildOptions {
        component,
        profile,
        target,
    })
}

fn build(root: &Path, options: &BuildOptions) -> Result<()> {
    match options.component {
        Component::All => {
            build_native(root, options.profile, options.target.as_deref(), true)?;
            build_wasm(root, options.profile)?;
            if options.target.is_none() {
                assemble(root, options.profile)?;
            }
        }
        Component::Native => {
            build_native(root, options.profile, options.target.as_deref(), true)?;
        }
        Component::Wasm => build_wasm(root, options.profile)?,
        Component::Release => {
            build_native(root, Profile::Release, options.target.as_deref(), true)?;
            build_wasm(root, Profile::Release)?;
            if options.target.is_none() {
                assemble(root, Profile::Release)?;
            }
        }
    }
    Ok(())
}

fn build_native(root: &Path, profile: Profile, target: Option<&str>, all_bins: bool) -> Result<()> {
    build_libhew(root, profile, target)?;
    let mut args = vec!["build", "-p", "hew-cli"];
    if all_bins {
        args.extend(["-p", "hew-lsp", "-p", "hew-observe", "-p", "hew-runtime"]);
    }
    add_profile_and_target(&mut args, profile, target);
    cargo(root, &args)?;
    if profile == Profile::Debug {
        verify_libhew(root, target)?;
    }
    Ok(())
}

fn build_libhew(root: &Path, profile: Profile, target: Option<&str>) -> Result<()> {
    let mut cargo_args = vec!["cargo", "build", "-p", "hew-lib"];
    match profile {
        Profile::Debug => {}
        Profile::Release => cargo_args.extend(["--profile", "release-lib"]),
    }
    if let Some(target) = target {
        cargo_args.extend(["--target", target]);
    }

    if profile == Profile::Debug {
        let output = cargo_output_dir(root, "debug", target)?;
        let mut command = Command::new(python());
        command
            .current_dir(root)
            .arg("scripts/libhew-freshness.py")
            .arg("build")
            .arg("--debug-dir")
            .arg(output)
            .arg("--")
            .args(cargo_args);
        run_command(&mut command, "build and certify libhew")
    } else {
        let mut command = Command::new(cargo_args[0]);
        command.current_dir(root).args(&cargo_args[1..]);
        run_command(&mut command, "build release libhew")
    }
}

fn build_wasm(root: &Path, profile: Profile) -> Result<()> {
    let mut runtime = vec![
        "build",
        "-p",
        "hew-runtime",
        "--target",
        "wasm32-wasip1",
        "--no-default-features",
    ];
    let mut stdlib = vec!["build", "-p", "hew-std", "--target", "wasm32-wasip1"];
    if profile == Profile::Release {
        runtime.push("--release");
        stdlib.push("--release");
    }
    cargo(root, &runtime)?;
    cargo(root, &stdlib)
}

fn verify_libhew(root: &Path, target: Option<&str>) -> Result<()> {
    let output = cargo_output_dir(root, "debug", target)?;
    let mut command = Command::new(python());
    command
        .current_dir(root)
        .arg("scripts/libhew-freshness.py")
        .arg("verify")
        .arg("--debug-dir")
        .arg(output);
    run_command(&mut command, "verify libhew freshness")
}

fn gate(root: &Path, args: &[String]) -> Result<()> {
    let (name, filter_expr) = parse_gate_options(args)?;
    let mut completed = BTreeSet::new();
    run_gate(root, name, filter_expr, &mut completed)
}

fn run_gate(
    root: &Path,
    name: &str,
    filter_expr: Option<&str>,
    completed: &mut BTreeSet<String>,
) -> Result<()> {
    let spec = gate_spec_by_name(name)?;
    if !completed.insert(name.to_string()) {
        return Ok(());
    }
    for dependency in spec.dependencies {
        run_gate(root, dependency, None, completed)?;
    }
    eprintln!("==> xtask gate {name}");
    execute_gate(root, name, filter_expr)
}

fn gate_spec_by_name(name: &str) -> Result<&'static GateSpec> {
    GATES
        .iter()
        .find(|spec| spec.name == name)
        .ok_or_else(|| format!("unknown gate {name:?}"))
}

#[allow(
    clippy::too_many_lines,
    reason = "keeping every named gate in one exhaustive dispatcher makes missing runners visible"
)]
fn execute_gate(root: &Path, name: &str, filter_expr: Option<&str>) -> Result<()> {
    match name {
        "native" => build_native(root, Profile::Debug, None, true),
        "smoke" => {
            build_native(root, Profile::Debug, None, false)?;
            verify_libhew(root, None)?;
            cargo(root, &["fmt", "--all", "--", "--check"])?;
            nextest(root, &["--workspace", "--profile", "smoke"], None)
        }
        "workspace" => {
            build_native(root, Profile::Debug, None, true)?;
            build_wasm(root, Profile::Debug)?;
            verify_libhew(root, None)?;
            nextest(
                root,
                &[
                    "--workspace",
                    "--exclude",
                    "hew-wasm",
                    "--exclude",
                    "hew-cabi",
                    "--profile",
                    "ci",
                    "--no-fail-fast",
                ],
                filter_expr,
            )
        }
        "platform-smoke" => platform_smoke(root, filter_expr),
        "platform-full" | "lint" | "build" | "ci" => Ok(()),
        "cabi" => cargo(
            root,
            &["nextest", "run", "--profile", "ci-cabi", "-p", "hew-cabi"],
        ),
        "vertical-slice" => compiled_hew_script(root, "tests/vertical-slice/run.sh", &[]),
        "hew-ratchet" => hew_ratchet_gate(root),
        "hew-inventory" => hew_inventory_gate(root),
        "compiled-artifact-pack" => compiled_artifact_pack(root),
        "compiled-artifact-unpack" => compiled_artifact_unpack(root),
        "compiled-shard" => compiled_shard(root),
        "stdlib-ratchet" => compiled_hew_script(root, "scripts/stdlib-ratchet.sh", &[]),
        "playground" => playground_gate(root),
        "playground-wasi" => playground_wasi(root),
        "freebsd" => cargo(
            root,
            &[
                "nextest",
                "run",
                "-p",
                "hew-cli",
                "--test",
                "dwarf_debugger_locals_e2e",
                "--profile",
                "ci-focused",
            ],
        ),
        "release-smoke" => release_smoke(root),
        "release-link" => release_link(root),
        "release-verify" => release_verify(root),
        "format" => cargo(root, &["fmt", "--all", "--", "--check"]),
        "clippy" => cargo(
            root,
            &["clippy", "--workspace", "--tests", "--", "-D", "warnings"],
        ),
        "clippy-json" => cargo(
            root,
            &[
                "clippy",
                "--workspace",
                "--tests",
                "--message-format=json",
                "--",
                "-D",
                "warnings",
            ],
        ),
        "structural-bootstrap" => run_program(
            root,
            "scripts/ast-grep-lint.sh",
            &["--bootstrap", "--install-only"],
        ),
        "structural-bootstrap-contract" => run_many(
            root,
            &[
                (
                    python(),
                    &["scripts/tests/test_structural_authority_audit.py"],
                ),
                ("bash", &["scripts/tests/test_ast_grep_contract.sh"]),
                (
                    python(),
                    &["scripts/tests/test_structural_lint_bootstrap.py"],
                ),
            ],
        ),
        "structural-lint" => run_many(
            root,
            &[
                (
                    python(),
                    &["scripts/tests/test_structural_authority_audit.py"],
                ),
                ("scripts/ast-grep-lint.sh", &[]),
            ],
        ),
        "freebsd-contract" => run_program(
            root,
            python(),
            &["scripts/tests/test_freebsd_workflow_contract.py"],
        ),
        "release-contract" => run_many(
            root,
            &[
                (
                    python(),
                    &["scripts/tests/test_release_workflow_contract.py"],
                ),
                (
                    python(),
                    &["scripts/tests/test_pre_release_validate_contract.py"],
                ),
                (python(), &["scripts/tests/test_cargo_output_dir.py"]),
                (python(), &["scripts/tests/test_target_dir_gate_wiring.py"]),
            ],
        ),
        "cutover-contract" => run_program(root, python(), &["scripts/tests/test_xtask_cutover.py"]),
        "sanitizer-contract" => {
            run_program(root, "bash", &["scripts/tests/test_sanitizer_gate.sh"])
        }
        "reachability" => run_many(
            root,
            &[
                (python(), &["scripts/tests/test_check_gate_reachability.py"]),
                (python(), &["scripts/check-gate-reachability.py"]),
            ],
        ),
        "hew-fmt" => hew_fmt_gate(root),
        "ffi" => ffi_gate(root),
        "runtime-poison-lint" => run_many(
            root,
            &[
                (
                    "bash",
                    &["scripts/lint-runtime-poison-safe.sh", "--self-test"],
                ),
                ("bash", &["scripts/lint-runtime-poison-safe.sh"]),
            ],
        ),
        "wasm-todo-lint" => {
            cargo(root, &["run", "-p", "hew-capability-gen", "--", "--check"])?;
            run_many(
                root,
                &[
                    (python(), &["scripts/lint-wasm-todo.py", "--self-test"]),
                    (python(), &["scripts/lint-wasm-todo.py"]),
                ],
            )
        }
        "codegen-identity" => run_program(
            root,
            python(),
            &["scripts/check-codegen-carried-identity.py"],
        ),
        "codegen-traps" => {
            run_program(root, python(), &["scripts/check-codegen-trap-inventory.py"])
        }
        "sys-closure" => run_many(
            root,
            &[
                (python(), &["scripts/tests/test_sys_lane_closure.py"]),
                (python(), &["scripts/sys-lane-closure.py"]),
            ],
        ),
        "leak-scan" => run_many(
            root,
            &[
                ("bash", &["scripts/lint-orchestration-leak.sh"]),
                (
                    "bash",
                    &["scripts/lint-orchestration-leak.sh", "--scan-commits"],
                ),
            ],
        ),
        "ll-identity" => run_program(root, "bash", &["scripts/ll-identity-selftest.sh"]),
        "sandbox-coverage" => run_many(
            root,
            &[
                (
                    python(),
                    &["scripts/tests/test_check_sandbox_parity_coverage.py"],
                ),
                (python(), &["scripts/check-sandbox-parity-coverage.py"]),
            ],
        ),
        "example-contract" => run_program(
            root,
            python(),
            &["scripts/tests/test_example_expectations.py"],
        ),
        "docs-examples" => compiled_hew_script(root, "scripts/extract-doc-fences.sh", &[]),
        "sandbox-fixtures" => cargo(root, &["xtask", "sandbox-fixtures"]),
        "wasm-capability" => cargo(root, &["run", "-p", "hew-capability-gen"]),
        "playground-manifest" => {
            cargo(root, &["run", "-p", "hew-capability-gen"])?;
            run_program(root, python(), &["scripts/gen-playground-manifest.py"])
        }
        "wasm-package" => run_program(
            root,
            "wasm-pack",
            &["build", "hew-wasm", "--target", "web", "--release"],
        ),
        "fuzz-corpus" => run_program(root, "bash", &["scripts/fuzz/hydrate-corpus.sh"]),
        "fuzz-oracle" => fuzz_oracle(root),
        "fuzz-oracle-selftest" => compiled_hew_script(root, "scripts/fuzz/oracle-selftest.sh", &[]),
        "checked-mir-verify" => {
            compiled_hew_script(root, "scripts/checked-mir-corpus.sh", &["verify"])
        }
        "checked-mir-golden" => {
            compiled_hew_script(root, "scripts/checked-mir-corpus.sh", &["golden"])
        }
        "checked-mir-run" => compiled_hew_script(root, "scripts/checked-mir-corpus.sh", &["run"]),
        "checked-mir-expect" => {
            compiled_hew_script(root, "scripts/checked-mir-corpus.sh", &["expect"])
        }
        "ll-diff" => compiled_hew_script(root, "scripts/ll-corpus.sh", &["verify"]),
        "ll-golden" => compiled_hew_script(root, "scripts/ll-corpus.sh", &["golden"]),
        "leak-oracle-selftest" => run_many(
            root,
            &[
                (
                    "cargo",
                    &[
                        "nextest",
                        "run",
                        "--profile",
                        "ci",
                        "-p",
                        "hew-cli",
                        "--test",
                        "leak_harness_fail_closed",
                    ],
                ),
                ("bash", &["scripts/tests/test_macos_leak_oracle_runner.sh"]),
            ],
        ),
        "compiler-pipeline" => compiler_pipeline(root),
        "runtime-unit" => cargo(
            root,
            &[
                "nextest",
                "run",
                "--profile",
                "ci",
                "-p",
                "hew-runtime",
                "--no-default-features",
            ],
        ),
        "core-matrix" => core_matrix(root),
        "hew-check" => compiled_hew_script(root, "scripts/hew-corpus-check.sh", &[]),
        "hew-fmt-property" => compiled_hew_script(root, "scripts/hew-fmt-property.sh", &[]),
        "pkg-import" => compiled_hew_script(root, "tests/pkg-import/run.sh", &[]),
        "package-install" => compiled_hew_script(root, "tests/package-install/run.sh", &[]),
        "surface-examples" => example_gate(root, true),
        "ux-examples" => example_gate(root, false),
        "doc-contract" => run_many(
            root,
            &[
                ("scripts/tests/test_ratchet_membership_wiring.sh", &[]),
                ("scripts/tests/test_doc_ratchet_membership.sh", &[]),
            ],
        ),
        "o2-contract" => run_program(root, "bash", &["scripts/o2-differential-selftest.sh"]),
        "sandbox-vm-deps" => sandbox_dependencies(root),
        "sandbox-parity" => sandbox_parity(root),
        "observe-functional" => observe_functional(root),
        "libhew-link-race" => libhew_link_race(root),
        "macos-leak-oracle" => compiled_hew_script(root, "scripts/macos-leak-oracle.sh", &[]),
        "licenses" => licenses_gate(root),
        "licenses-generate" => cargo(
            root,
            &[
                "about",
                "generate",
                "--workspace",
                "about.hbs",
                "-o",
                "THIRD-PARTY-LICENSES",
            ],
        ),
        "sandbox-fixtures-check" => cargo(root, &["xtask", "sandbox-fixtures", "--check"]),
        "compiler-lifecycle" => compiler_lifecycle(root, false),
        "compiler-lifecycle-external" => compiler_lifecycle(root, true),
        "stdlib-execution" => {
            compiled_hew_script(root, "scripts/stdlib-execution-proof.sh", &["--check"])
        }
        "mqtt" => compiled_hew_script(root, "scripts/mqtt-broker-e2e.sh", &[]),
        "o2-differential" => o2_differential_gate(root),
        "coverage" => coverage_gate(root),
        "coverage-html" => cargo(
            root,
            &[
                "llvm-cov",
                "report",
                "--html",
                "--output-dir",
                "coverage-html",
            ],
        ),
        "coverage-summary" => cargo(root, &["llvm-cov", "report"]),
        "coverage-runtime" => run_program(root, "bash", &["scripts/coverage-runtime-e2e.sh"]),
        "asan" => sanitizer_gate(root, "address"),
        "asan-fixtures" => asan_fixtures(root),
        "asan-fixture-selftest" => {
            run_program(root, "scripts/asan-fixture-check.sh", &["--selftest"])
        }
        "tsan" => sanitizer_gate(root, "thread"),
        "miri" => miri_gate(root),
        "fuzz-smoke" => run_program(root, "bash", &["scripts/fuzz/run-smoke.sh"]),
        "fuzz-smoke-bootstrap" => run_program(root, "bash", &["scripts/fuzz/smoke-bootstrap.sh"]),
        "stdlib-lint" => stdlib_lint(root),
        "docs" => docs_gate(root),
        "package-completions" => package_completions(root),
        "package-smoke" => package_smoke(root),
        "package-verify" => package_verify(root),
        "windows-pdb" => windows_pdb_gate(root),
        other => Err(format!("gate {other:?} is declared but has no runner")),
    }
}

fn parse_gate_options(args: &[String]) -> Result<(&str, Option<&str>)> {
    let name = args
        .first()
        .ok_or_else(|| "gate requires a gate name".to_string())?;
    let mut filter_expr = None;
    let mut index = 1;
    while index < args.len() {
        match args[index].as_str() {
            "--filter-expr" => {
                filter_expr = Some(
                    args.get(index + 1)
                        .ok_or_else(|| "--filter-expr requires an expression".to_string())?
                        .as_str(),
                );
                index += 2;
            }
            other => return Err(format!("unknown gate option {other:?}")),
        }
    }
    if filter_expr.is_some()
        && !matches!(
            name.as_str(),
            "workspace" | "platform-full" | "platform-smoke"
        )
    {
        return Err(format!("gate {name:?} does not accept --filter-expr"));
    }
    Ok((name, filter_expr))
}

fn platform_smoke(root: &Path, filter_expr: Option<&str>) -> Result<()> {
    build_native(root, Profile::Debug, None, true)?;
    build_wasm(root, Profile::Debug)?;
    verify_libhew(root, None)?;
    nextest(
        root,
        &[
            "-p",
            "hew-runtime",
            "-p",
            "hew-codegen-rs",
            "-p",
            "hew-compile",
            "-p",
            "hew-cli",
            "-p",
            "hew-lib",
            "--profile",
            "ci",
        ],
        filter_expr,
    )
}

fn playground_gate(root: &Path) -> Result<()> {
    cargo(root, &["run", "-p", "hew-capability-gen", "--", "--check"])?;
    run_program(
        root,
        python(),
        &["scripts/gen-playground-manifest.py", "--check"],
    )?;
    cargo(root, &["test", "-p", "hew-wasm"])?;
    run_program(
        root,
        "wasm-pack",
        &["build", "hew-wasm", "--target", "web", "--release"],
    )
}

fn playground_wasi(root: &Path) -> Result<()> {
    build_wasm(root, Profile::Debug)?;
    build_native(root, Profile::Debug, None, false)?;
    for test in [
        "curated_playground_examples_run_under_wasi",
        "supervisor_stays_on_the_unsupported_diagnostic_path_under_wasi",
    ] {
        cargo(
            root,
            &[
                "test",
                "-p",
                "hew-cli",
                "--test",
                "wasi_run_e2e",
                test,
                "--",
                "--exact",
            ],
        )?;
    }
    Ok(())
}

fn release_smoke(root: &Path) -> Result<()> {
    build_native(root, Profile::Release, None, false)?;
    run_program(root, "scripts/test-release-binary.sh", &["--no-build"])
}

fn release_link(root: &Path) -> Result<()> {
    let target = env::var("HEW_RELEASE_TARGET").ok();
    build_native(root, Profile::Release, target.as_deref(), false)?;
    let hew = cargo_output_dir(root, "release", target.as_deref())?.join(executable("hew"));
    let archive_name = if cfg!(windows) { "hew.lib" } else { "libhew.a" };
    let archive = cargo_output_dir(root, "release-lib", target.as_deref())?.join(archive_name);
    let mut command = if cfg!(windows) {
        let mut command = Command::new("powershell");
        command
            .args(["-NoProfile", "-ExecutionPolicy", "Bypass", "-File"])
            .arg("scripts/test-release-lib-link.ps1")
            .arg("-Hew")
            .arg(hew)
            .arg("-Archive")
            .arg(archive);
        command
    } else {
        let mut command = Command::new("scripts/test-release-lib-link.sh");
        command.arg("--hew").arg(hew).arg("--archive").arg(archive);
        command
    };
    command.current_dir(root);
    run_command(&mut command, "prove release library consumer linking")
}

fn release_verify(root: &Path) -> Result<()> {
    let target = env::var("HEW_RELEASE_TARGET").ok();
    build_native(root, Profile::Release, target.as_deref(), true)?;
    let directory = cargo_output_dir(root, "release", target.as_deref())?;
    for binary in ["hew", "hew-lsp", "hew-observe"] {
        let mut command = Command::new(directory.join(executable(binary)));
        command.current_dir(root).arg("--version");
        run_command(&mut command, "verify release binary")?;
    }
    Ok(())
}

fn run_many(root: &Path, commands: &[(&str, &[&str])]) -> Result<()> {
    for (program, args) in commands {
        run_program(root, program, args)?;
    }
    Ok(())
}

fn ffi_gate(root: &Path) -> Result<()> {
    let mut verify = Command::new(python());
    verify
        .current_dir(root)
        .args([
            "scripts/verify-ffi-symbols.py",
            "--classify",
            "stable",
            "--validate",
        ])
        .stdout(std::process::Stdio::null());
    run_command(&mut verify, "verify FFI symbols")?;
    run_program(
        root,
        python(),
        &["scripts/tests/test_verify_ffi_symbols.py"],
    )?;
    let mut fallback = Command::new(python());
    fallback
        .current_dir(root)
        .env("HEW_FORCE_TOML_FALLBACK", "1")
        .arg("scripts/tests/test_toml_compat.py");
    run_command(&mut fallback, "test Python 3.10 TOML fallback")
}

fn hew_fmt_gate(root: &Path) -> Result<()> {
    build_native(root, Profile::Debug, None, false)?;
    let hew = cargo_output_dir(root, "debug", None)?.join(executable("hew"));
    let mut sources = Vec::new();
    for directory in [root.join("std"), root.join("examples")] {
        collect_extension(&directory, "hew", &mut sources)?;
    }
    if sources.is_empty() {
        return Err("hew format gate selected no .hew sources".to_string());
    }
    sources.sort();
    let mut command = Command::new(hew);
    command
        .current_dir(root)
        .args(["fmt", "--check"])
        .args(&sources);
    run_command(&mut command, "check Hew formatting")
}

fn collect_extension(directory: &Path, extension: &str, files: &mut Vec<PathBuf>) -> Result<()> {
    for entry in std::fs::read_dir(directory)
        .map_err(|err| format!("read {}: {err}", directory.display()))?
    {
        let entry = entry.map_err(|err| format!("read {} entry: {err}", directory.display()))?;
        let path = entry.path();
        if path.is_dir() {
            collect_extension(&path, extension, files)?;
        } else if path.extension() == Some(OsStr::new(extension)) {
            files.push(path);
        }
    }
    Ok(())
}

fn compiler_lifecycle(root: &Path, external: bool) -> Result<()> {
    build_native(root, Profile::Debug, None, false)?;
    build_wasm(root, Profile::Debug)?;
    verify_libhew(root, None)?;
    let hew = cargo_output_dir(root, "debug", None)?.join(executable("hew"));
    for script in [
        "scripts/tests/test_opaque_resource_lifecycle_facts.py",
        "scripts/tests/test_opaque_resource_lifecycle_matrix.py",
    ] {
        let mut command = Command::new(python());
        command.current_dir(root).env("HEW_BIN", &hew).arg(script);
        if external && script.ends_with("matrix.py") {
            command.args(["--runtime-profile", "external-network"]);
        }
        run_command(&mut command, "run compiler lifecycle gate")?;
    }
    Ok(())
}

fn licenses_gate(root: &Path) -> Result<()> {
    cargo(root, &["deny", "check", "licenses"])?;
    cargo(root, &["deny", "check", "bans", "sources"])?;
    cargo(root, &["deny", "check", "advisories"])?;
    run_program(root, "scripts/check-licenses-fresh.sh", &[])
}

fn coverage_gate(root: &Path) -> Result<()> {
    cargo(
        root,
        &[
            "llvm-cov",
            "nextest",
            "--workspace",
            "--exclude",
            "hew-wasm",
            "--profile",
            "ci",
            "-E",
            "not test(eval_wasm) and not binary(wasi_run_e2e)",
            "--status-level",
            "pass",
            "--final-status-level",
            "slow",
            "--lcov",
            "--output-path",
            "lcov.info",
        ],
    )
}

fn sanitizer_gate(root: &Path, kind: &str) -> Result<()> {
    let target = env::var("SANITIZER_RUST_TARGET").unwrap_or(host_triple(root)?);
    let mut command = Command::new(cargo_executable());
    command.current_dir(root).arg("+nightly").arg("test");
    if kind == "address" {
        command
            .env("CARGO_TARGET_DIR", env::var_os("CARGO_TARGET_DIR").unwrap_or_else(|| "target/sanitizer-runtime-asan".into()))
            .env("RUSTFLAGS", env::var_os("RUSTFLAGS").unwrap_or_else(|| "-Zsanitizer=address -Cforce-frame-pointers=yes -Cunsafe-allow-abi-mismatch=sanitizer".into()))
            .env("ASAN_OPTIONS", env::var_os("ASAN_OPTIONS").unwrap_or_else(|| "detect_leaks=1".into()))
            .env("LSAN_OPTIONS", env::var_os("LSAN_OPTIONS").unwrap_or_else(|| "suppressions=lsan.supp".into()))
            .args(["--target", &target, "-p", "hew-runtime", "--lib"]);
    } else {
        command
            .env("CARGO_TARGET_DIR", env::var_os("CARGO_TARGET_DIR").unwrap_or_else(|| "target/sanitizer-runtime-tsan".into()))
            .env("RUSTFLAGS", env::var_os("RUSTFLAGS").unwrap_or_else(|| "-Zsanitizer=thread -Cforce-frame-pointers=yes -Cunsafe-allow-abi-mismatch=sanitizer".into()))
            .env("TSAN_OPTIONS", env::var_os("TSAN_OPTIONS").unwrap_or_else(|| "halt_on_error=0 suppressions=tsan.supp".into()))
            .args(["--target", &target, "-p", "hew-runtime", "--no-default-features", "--lib", "--", "--test-threads=1"]);
    }
    run_command(&mut command, "run sanitizer gate")
}

fn host_triple(root: &Path) -> Result<String> {
    let output = Command::new("rustc")
        .current_dir(root)
        .arg("-vV")
        .output()
        .map_err(|err| format!("execute rustc -vV: {err}"))?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    stdout
        .lines()
        .find_map(|line| line.strip_prefix("host: "))
        .map(str::to_string)
        .ok_or_else(|| "rustc -vV omitted host triple".to_string())
}

fn miri_gate(root: &Path) -> Result<()> {
    let toolchain = env::var("MIRI_TOOLCHAIN").unwrap_or_else(|_| "nightly".to_string());
    let mut command = Command::new(cargo_executable());
    command
        .current_dir(root)
        .env(
            "CARGO_TARGET_DIR",
            env::var_os("CARGO_TARGET_DIR").unwrap_or_else(|| "target/miri-runtime".into()),
        )
        .env(
            "MIRIFLAGS",
            env::var_os("MIRIFLAGS")
                .unwrap_or_else(|| "-Zmiri-disable-isolation -Zmiri-permissive-provenance".into()),
        )
        .arg(format!("+{toolchain}"))
        .args([
            "miri",
            "test",
            "-p",
            "hew-runtime",
            "--no-default-features",
            "--lib",
            "--",
            "send_ptr::",
            "rc::",
            "arc::",
            "tagged_union::",
            "arena::",
            "bytes::",
            "vecdeque::",
            "vec::",
        ]);
    run_command(&mut command, "run Miri gate")
}

fn stdlib_lint(root: &Path) -> Result<()> {
    for pattern in [
        "os error",
        "contains\\(\\\"Connection refused",
        "contains\\(\\\"Permission denied",
        "contains\\(\\\"timed out",
    ] {
        let status = Command::new("rg")
            .current_dir(root)
            .args(["--glob", "*.hew", pattern, "std/"])
            .status()
            .map_err(|err| format!("scan stdlib errno patterns: {err}"))?;
        if status.success() {
            return Err(format!(
                "stdlib contains banned OS-message pattern {pattern:?}"
            ));
        }
        if status.code() != Some(1) {
            return Err(format!("stdlib errno scan failed with {status}"));
        }
    }
    run_program(root, "bash", &["scripts/lint-stdlib-int-surface.sh"])
}

fn docs_gate(root: &Path) -> Result<()> {
    build_native(root, Profile::Release, None, false)?;
    let hew = cargo_output_dir(root, "release", None)?.join(executable("hew"));
    let output = cargo_output_dir(root, "doc", None)?;
    let mut command = Command::new(hew);
    command
        .current_dir(root)
        .arg("doc")
        .arg("std/")
        .arg("--output-dir")
        .arg(output);
    run_command(&mut command, "build standard library documentation")
}

fn hew_ratchet_gate(root: &Path) -> Result<()> {
    if env::var_os("HEW_SHARD_REPORT_DIR").is_some() {
        let reports = required_env("HEW_SHARD_REPORT_DIR")?;
        let inventory = required_env("HEW_FULL_INVENTORY")?;
        let shards = required_env("HEW_SHARD_COUNT")?;
        return run_program(
            root,
            python(),
            &[
                "scripts/compiled-hew-shards.py",
                "aggregate",
                "--mode",
                "ratchet",
                "--reports-dir",
                &reports,
                "--full-inventory",
                &inventory,
                "--shard-count",
                &shards,
            ],
        );
    }
    if let Ok(path) = env::var("HEW_O0_OUTCOMES_FILE") {
        compiled_hew_script(
            root,
            "scripts/hew-suite-ratchet.sh",
            &["--emit-o0-outcomes", &path],
        )
    } else {
        compiled_hew_script(root, "scripts/hew-suite-ratchet.sh", &[])
    }
}

fn o2_differential_gate(root: &Path) -> Result<()> {
    if env::var_os("HEW_SHARD_REPORT_DIR").is_some() {
        let reports = required_env("HEW_SHARD_REPORT_DIR")?;
        let inventory = required_env("HEW_FULL_INVENTORY")?;
        let shards = required_env("HEW_SHARD_COUNT")?;
        return run_program(
            root,
            python(),
            &[
                "scripts/compiled-hew-shards.py",
                "aggregate",
                "--mode",
                "differential",
                "--reports-dir",
                &reports,
                "--full-inventory",
                &inventory,
                "--shard-count",
                &shards,
            ],
        );
    }
    compiled_hew_script(root, "scripts/o2-differential.sh", &[])
}

fn hew_inventory_gate(root: &Path) -> Result<()> {
    let hew = if let Some(path) = env::var_os("HEW_BIN") {
        PathBuf::from(path)
    } else {
        build_native(root, Profile::Debug, None, false)?;
        cargo_output_dir(root, "debug", None)?.join(executable("hew"))
    };
    let mut fixtures = Vec::new();
    for entry in std::fs::read_dir(root.join("tests/hew"))
        .map_err(|err| format!("read tests/hew: {err}"))?
    {
        let entry = entry.map_err(|err| format!("read tests/hew entry: {err}"))?;
        let path = entry.path();
        if path.is_file() && path.extension() == Some(OsStr::new("hew")) {
            fixtures.push(path);
        }
    }
    fixtures.sort();
    if fixtures.is_empty() {
        return Err("Hew suite inventory selected no tests/hew/*.hew fixtures".to_string());
    }

    let mut inventory = Vec::new();
    for fixture in fixtures {
        let output = Command::new(&hew)
            .current_dir(root)
            .arg("test")
            .arg(&fixture)
            .args(["--list", "--allow-empty"])
            .output()
            .map_err(|err| format!("list Hew suite inventory for {}: {err}", fixture.display()))?;
        ensure_success(
            output.status,
            &format!("list Hew suite inventory for {}", fixture.display()),
        )?;
        inventory.extend(
            String::from_utf8(output.stdout)
                .map_err(|err| format!("decode inventory for {}: {err}", fixture.display()))?
                .lines()
                .map(str::to_owned),
        );
    }
    inventory.sort();
    let rendered = format!("{}\n", inventory.join("\n"));
    let destination = PathBuf::from(required_env("HEW_FULL_INVENTORY")?);
    std::fs::write(&destination, rendered)
        .map_err(|err| format!("write {}: {err}", destination.display()))
}

fn package_root() -> Result<PathBuf> {
    env::var_os("HEW_PACKAGE_ROOT")
        .map(PathBuf::from)
        .ok_or_else(|| "HEW_PACKAGE_ROOT must name the staged package root".to_string())
}

fn package_completions(root: &Path) -> Result<()> {
    let package = package_root()?;
    let hew = package.join("bin").join(executable("hew"));
    let completions = package.join("completions");
    std::fs::create_dir_all(&completions)
        .map_err(|err| format!("create {}: {err}", completions.display()))?;
    for shell in ["bash", "zsh", "fish"] {
        let output = Command::new(&hew)
            .current_dir(root)
            .args(["completions", shell])
            .output()
            .map_err(|err| format!("generate {shell} completions: {err}"))?;
        ensure_success(output.status, "generate shell completions")?;
        let destination = completions.join(format!("hew.{shell}"));
        std::fs::write(&destination, output.stdout)
            .map_err(|err| format!("write {}: {err}", destination.display()))?;
    }
    Ok(())
}

fn package_verify(root: &Path) -> Result<()> {
    let package = package_root()?;
    for binary in ["hew", "hew-lsp", "hew-observe"] {
        let mut command = Command::new(package.join("bin").join(executable(binary)));
        command.current_dir(root).arg("--version");
        run_command(&mut command, "verify packaged binary")?;
    }
    Ok(())
}

fn package_smoke(root: &Path) -> Result<()> {
    let package = package_root()?;
    let scratch = env::temp_dir().join(format!("hew-package-smoke-{}", std::process::id()));
    std::fs::create_dir_all(&scratch)
        .map_err(|err| format!("create {}: {err}", scratch.display()))?;
    let source = scratch.join("package-smoke.hew");
    std::fs::write(
        &source,
        "fn main() {\n    println(\"package-smoke-ok\")\n}\n",
    )
    .map_err(|err| format!("write {}: {err}", source.display()))?;
    let output = Command::new(package.join("bin").join(executable("hew")))
        .current_dir(root)
        .env("HEW_STD", package.join("std"))
        .arg("run")
        .arg(&source)
        .output()
        .map_err(|err| format!("run package smoke test: {err}"))?;
    ensure_success(output.status, "run package smoke test")?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    if !stdout.contains("package-smoke-ok") {
        return Err(format!("package smoke output was {stdout:?}"));
    }
    Ok(())
}

fn windows_pdb_gate(root: &Path) -> Result<()> {
    if !cfg!(windows) {
        return Err("windows-pdb gate requires Windows".to_string());
    }
    build_native(root, Profile::Release, None, false)?;
    let directory = env::temp_dir().join(format!("hew-pdb-e2e-{}", std::process::id()));
    std::fs::create_dir_all(&directory)
        .map_err(|err| format!("create {}: {err}", directory.display()))?;
    let source = directory.join("pdb-e2e.hew");
    let binary = directory.join("pdb-e2e.exe");
    std::fs::write(&source, "fn main() {\n    println(2235)\n}\n")
        .map_err(|err| format!("write {}: {err}", source.display()))?;
    let hew = cargo_output_dir(root, "release", None)?.join(executable("hew"));
    let mut build = Command::new(hew);
    build
        .current_dir(root)
        .arg("build")
        .arg("-g")
        .arg(&source)
        .arg("-o")
        .arg(&binary);
    run_command(&mut build, "build Windows PDB fixture")?;
    let pdb = directory.join("pdb-e2e.pdb");
    if !pdb.is_file() {
        return Err(format!("{} was not produced", pdb.display()));
    }
    let output = Command::new("llvm-pdbutil")
        .current_dir(root)
        .args(["dump", "-summary", "-modules", "-symbols", "-l"])
        .arg(&pdb)
        .output()
        .map_err(|err| format!("inspect {}: {err}", pdb.display()))?;
    ensure_success(output.status, "inspect Windows PDB")?;
    let dump = String::from_utf8_lossy(&output.stdout);
    if !dump.contains("pdb-e2e.hew") {
        return Err("PDB dump has no source module record for pdb-e2e.hew".to_string());
    }
    let lower = dump.to_ascii_lowercase();
    if !lower.contains("main") || !lower.contains("line") {
        return Err("PDB dump has no main symbol or line information".to_string());
    }
    Ok(())
}

fn required_env(name: &str) -> Result<String> {
    env::var(name).map_err(|_| format!("{name} must be set for this gate"))
}

fn compiler_pipeline(root: &Path) -> Result<()> {
    build_wasm(root, Profile::Debug)?;
    build_native(root, Profile::Debug, None, false)?;
    verify_libhew(root, None)?;
    nextest(
        root,
        &[
            "--profile",
            "ci",
            "-p",
            "hew-lexer",
            "-p",
            "hew-parser",
            "-p",
            "hew-types",
            "-p",
            "hew-hir",
            "-p",
            "hew-mir",
            "-p",
            "hew-codegen-rs",
            "-p",
            "hew-cli",
            "-p",
            "hew-pkg",
        ],
        None,
    )?;
    compiler_lifecycle(root, false)
}

fn core_matrix(root: &Path) -> Result<()> {
    build_native(root, Profile::Debug, None, false)?;
    verify_libhew(root, None)?;
    let generated = env::temp_dir().join(format!("hew-core-matrix-{}", std::process::id()));
    let generated_text = generated
        .to_str()
        .ok_or_else(|| "core matrix temporary path is not UTF-8".to_string())?;
    run_program(
        root,
        python(),
        &["scripts/core-matrix-gen.py", "--out", generated_text],
    )?;
    let mut compare = Command::new("diff");
    compare
        .current_dir(root)
        .arg("-r")
        .arg("tests/core-matrix/cells")
        .arg(&generated);
    run_command(&mut compare, "compare generated core matrix")?;
    let hew = cargo_output_dir(root, "debug", None)?.join(executable("hew"));
    let mut run = Command::new(python());
    run.current_dir(root)
        .env("HEW_BIN", hew)
        .arg("scripts/core-matrix.py");
    run_command(&mut run, "run core matrix")
}

fn example_gate(root: &Path, surface: bool) -> Result<()> {
    build_native(root, Profile::Debug, None, false)?;
    verify_libhew(root, None)?;
    run_program(
        root,
        python(),
        &["scripts/tests/test_example_expectations.py"],
    )?;
    let hew = cargo_output_dir(root, "debug", None)?.join(executable("hew"));
    let mut command = Command::new(python());
    command
        .current_dir(root)
        .arg("scripts/example-expectations.py")
        .arg("--hew-bin")
        .arg(hew);
    if surface {
        command.args([
            "--label",
            "surface",
            "--source-root",
            "examples/v05/surfaces",
            "--source",
            "examples/net/http_await_service.hew",
        ]);
    } else {
        command.args([
            "--label",
            "ux + progressive tutorial",
            "--source-root",
            "examples/ux",
            "--source-root",
            "examples/progressive",
        ]);
    }
    run_command(&mut command, "run example expectation gate")
}

fn sandbox_parity(root: &Path) -> Result<()> {
    run_program(
        root,
        "npm",
        &["--prefix", "hew-sandbox-vm", "run", "conformance"],
    )?;
    cargo(
        root,
        &[
            "test",
            "-p",
            "hew-sandbox-wasm",
            "--test",
            "parity",
            "--test",
            "parity_ratchet",
            "--test",
            "playground",
            "--test",
            "ios_subset",
        ],
    )
}

fn sandbox_dependencies(root: &Path) -> Result<()> {
    run_program(root, "npm", &["--prefix", "hew-sandbox-vm", "ci"])
}

fn asan_fixtures(root: &Path) -> Result<()> {
    if cfg!(target_os = "macos") {
        println!("asan-fixtures is not supported on macOS; use the leaks oracle");
        return Ok(());
    }
    run_program(root, "scripts/asan-fixture-check.sh", &[])
}

fn observe_functional(root: &Path) -> Result<()> {
    build_native(root, Profile::Debug, None, true)?;
    verify_libhew(root, None)?;
    cargo(
        root,
        &[
            "test",
            "-p",
            "hew-observe",
            "--test",
            "functional",
            "--",
            "--ignored",
            "--nocapture",
        ],
    )
}

fn libhew_link_race(root: &Path) -> Result<()> {
    build_native(root, Profile::Debug, None, false)?;
    verify_libhew(root, None)?;
    cargo(
        root,
        &[
            "test",
            "-p",
            "hew-testutil",
            "--test",
            "libhew_link_race",
            "--",
            "--ignored",
            "--nocapture",
            "--test-threads=1",
        ],
    )
}

fn compiled_artifact_pack(root: &Path) -> Result<()> {
    build_native(root, Profile::Debug, None, false)?;
    verify_libhew(root, None)?;
    let source = cargo_output_dir(root, "debug", None)?;
    let revision = required_env("HEW_SOURCE_REVISION")?;
    let output = required_env("HEW_ARTIFACT_PATH")?;
    run_program(
        root,
        python(),
        &[
            "scripts/compiled-hew-artifact.py",
            "pack",
            "--source-debug-dir",
            source
                .to_str()
                .ok_or_else(|| "Cargo output path is not UTF-8".to_string())?,
            "--source-revision",
            &revision,
            "--output",
            &output,
        ],
    )
}

fn compiled_artifact_unpack(root: &Path) -> Result<()> {
    let input = required_env("HEW_ARTIFACT_PATH")?;
    let destination = required_env("HEW_ARTIFACT_DESTINATION")?;
    let revision = required_env("HEW_SOURCE_REVISION")?;
    run_program(
        root,
        python(),
        &[
            "scripts/compiled-hew-artifact.py",
            "unpack",
            "--input",
            &input,
            "--destination",
            &destination,
            "--expected-revision",
            &revision,
        ],
    )
}

fn compiled_shard(root: &Path) -> Result<()> {
    let compiler = required_env("HEW_BIN")?;
    let partition = required_env("HEW_SHARD_PARTITION")?;
    let output = required_env("HEW_SHARD_OUTPUT_DIR")?;
    run_program(
        root,
        python(),
        &[
            "scripts/compiled-hew-shards.py",
            "run",
            "--compiler",
            &compiler,
            "--partition",
            &partition,
            "--output-dir",
            &output,
        ],
    )
}

fn compiled_hew_script(root: &Path, script: &str, args: &[&str]) -> Result<()> {
    build_native(root, Profile::Debug, None, false)?;
    verify_libhew(root, None)?;
    let hew = cargo_output_dir(root, "debug", None)?.join(executable("hew"));
    let mut command = Command::new(script);
    command.current_dir(root).env("HEW_BIN", hew).args(args);
    run_command(&mut command, script)
}

fn fuzz_oracle(root: &Path) -> Result<()> {
    build_native(root, Profile::Debug, None, false)?;
    verify_libhew(root, None)?;
    let hew = cargo_output_dir(root, "debug", None)?.join(executable("hew"));
    let mut command = Command::new(python());
    command
        .current_dir(root)
        .arg("scripts/fuzz/run-oracle.py")
        .arg("--hew")
        .arg(hew)
        .args(["--timeout", "30"]);
    if env::var_os("FUZZ_ORACLE_FULL").is_some() {
        command.arg("--full");
    }
    run_command(&mut command, "run deterministic fuzz oracle")
}

fn tools(args: &[String]) -> Result<()> {
    let mut gates = None;
    let mut field = "tools";
    let mut verify = false;
    let mut index = 0;
    while index < args.len() {
        match args[index].as_str() {
            "--gates" | "--verify" => {
                verify = args[index] == "--verify";
                gates = Some(
                    args.get(index + 1)
                        .ok_or_else(|| format!("{} requires a gate list", args[index]))?
                        .clone(),
                );
                index += 2;
            }
            "--field" => {
                field = args
                    .get(index + 1)
                    .ok_or_else(|| "--field requires a value".to_string())?;
                index += 2;
            }
            other => return Err(format!("unknown tools option {other:?}")),
        }
    }
    let plan = ToolPlan::for_gates(gates.as_deref().unwrap_or(""))?;
    if verify {
        return plan.verify();
    }
    match field {
        "tools" => println!("{}", plan.tools.into_iter().collect::<Vec<_>>().join(",")),
        "targets" => println!("{}", plan.targets.into_iter().collect::<Vec<_>>().join(" ")),
        "ast-grep" => println!("{}", plan.ast_grep),
        other => return Err(format!("unknown tools field {other:?}")),
    }
    Ok(())
}

#[derive(Debug, Default)]
struct ToolPlan {
    tools: BTreeSet<&'static str>,
    targets: BTreeSet<&'static str>,
    ast_grep: bool,
}

impl ToolPlan {
    fn for_gates(input: &str) -> Result<Self> {
        let mut plan = Self::default();
        let mut visited = BTreeSet::new();
        for gate in input
            .split(',')
            .map(str::trim)
            .filter(|gate| !gate.is_empty())
        {
            plan.add_gate(gate, &mut visited)?;
        }
        Ok(plan)
    }

    fn add_gate(&mut self, name: &str, visited: &mut BTreeSet<String>) -> Result<()> {
        if !visited.insert(name.to_string()) {
            return Ok(());
        }
        let spec = gate_spec_by_name(name)?;
        self.tools.extend(spec.tools.iter().copied());
        self.targets.extend(spec.targets.iter().copied());
        self.ast_grep |= spec.ast_grep;
        for dependency in spec.dependencies {
            self.add_gate(dependency, visited)?;
        }
        Ok(())
    }

    fn verify(&self) -> Result<()> {
        for tool in &self.tools {
            let (binary, expected) = match *tool {
                NEXTEST => ("cargo-nextest", "0.9.99"),
                CARGO_DENY => ("cargo-deny", "0.20.2"),
                CARGO_ABOUT => ("cargo-about", "0.9.1"),
                LLVM_COV => ("cargo-llvm-cov", "0.8.7"),
                WASM_PACK => ("wasm-pack", "0.15.0"),
                WASMTIME => ("wasmtime", "47.0.3"),
                _ => return Err(format!("no verifier for {tool}")),
            };
            let output = Command::new(binary)
                .arg("--version")
                .output()
                .map_err(|err| format!("execute {binary} --version: {err}"))?;
            if !output.status.success() {
                return Err(format!("{binary} --version failed with {}", output.status));
            }
            let version = String::from_utf8_lossy(&output.stdout);
            if !version.contains(expected) {
                return Err(format!(
                    "{binary} version mismatch: expected {expected}, got {}",
                    version.trim()
                ));
            }
            println!("verified {binary} {expected}");
        }
        if self.ast_grep {
            let binary = Path::new(".ast-grep/tool/bin").join(executable("ast-grep"));
            let status = Command::new(&binary)
                .arg("--version")
                .status()
                .map_err(|err| format!("execute {}: {err}", binary.display()))?;
            ensure_success(status, "verify ast-grep")?;
            if !Path::new(".ast-grep/hew-lang.so").is_file() {
                return Err(".ast-grep/hew-lang.so is missing".to_string());
            }
        }
        Ok(())
    }
}

fn ci_local(root: &Path, args: &[String]) -> Result<()> {
    let mut command = Command::new("act");
    command
        .current_dir(root)
        .args(["workflow_dispatch", "-W", ".github/workflows/ci-local.yml"]);
    if args == ["--list"] {
        command.arg("--list");
    } else if args.is_empty() {
        command.args(["-j", "provisioning-smoke"]);
    } else {
        return Err("ci-local accepts only --list".to_string());
    }
    run_command(&mut command, "run local Linux CI")
}

fn install(root: &Path) -> Result<()> {
    let destination = install_root()?;
    let release = cargo_output_dir(root, "release", None)?;
    let release_lib = cargo_output_dir(root, "release-lib", None)?;
    let wasm_release = cargo_output_dir(root, "release", Some("wasm32-wasip1"))?;
    let binaries = destination.join("bin");
    let libraries = destination.join("lib");
    let stdlib = destination.join("std");
    let completions = destination.join("completions");
    for directory in [&binaries, &libraries, &stdlib, &completions] {
        std::fs::create_dir_all(directory)
            .map_err(|err| format!("create {}: {err}", directory.display()))?;
    }
    for name in ["hew", "hew-lsp", "hew-observe"] {
        copy_file(
            &release.join(executable(name)),
            &binaries.join(executable(name)),
            true,
        )?;
    }
    let archive_name = if cfg!(windows) { "hew.lib" } else { "libhew.a" };
    copy_file(
        &release_lib.join(archive_name),
        &libraries.join(archive_name),
        false,
    )?;
    for name in ["libhew_runtime.a", "libhew_std.a"] {
        let source = wasm_release.join(name);
        if source.is_file() {
            let destination = libraries.join("wasm32-wasip1").join(name);
            if let Some(parent) = destination.parent() {
                std::fs::create_dir_all(parent)
                    .map_err(|err| format!("create {}: {err}", parent.display()))?;
            }
            copy_file(&source, &destination, false)?;
        }
    }
    copy_tree(&root.join("std"), &stdlib)?;
    generate_completions(root, &binaries.join(executable("hew")), &completions)?;
    println!("installed Hew to {}", destination.display());
    Ok(())
}

fn assemble(root: &Path, profile: Profile) -> Result<()> {
    let build = root.join("build");
    if build.exists() {
        std::fs::remove_dir_all(&build)
            .map_err(|err| format!("remove {}: {err}", build.display()))?;
    }
    let binaries = build.join("bin");
    let libraries = build.join("lib");
    let stdlib = build.join("std");
    for directory in [&binaries, &libraries, &stdlib] {
        std::fs::create_dir_all(directory)
            .map_err(|err| format!("create {}: {err}", directory.display()))?;
    }
    let cargo_profile = if profile == Profile::Release {
        "release"
    } else {
        "debug"
    };
    let output = cargo_output_dir(root, cargo_profile, None)?;
    let names: &[&str] = if profile == Profile::Release {
        &["hew", "hew-lsp", "hew-observe"]
    } else {
        &["hew", "hew-observe"]
    };
    for name in names {
        copy_file(
            &output.join(executable(name)),
            &binaries.join(executable(name)),
            true,
        )?;
    }
    let library_profile = if profile == Profile::Release {
        "release-lib"
    } else {
        "debug"
    };
    let archive_name = if cfg!(windows) { "hew.lib" } else { "libhew.a" };
    copy_file(
        &cargo_output_dir(root, library_profile, None)?.join(archive_name),
        &libraries.join(archive_name),
        false,
    )?;
    let wasm = cargo_output_dir(root, cargo_profile, Some("wasm32-wasip1"))?;
    for name in ["libhew_runtime.a", "libhew_std.a"] {
        let source = wasm.join(name);
        if source.is_file() {
            let destination = libraries.join("wasm32-wasip1").join(name);
            std::fs::create_dir_all(destination.parent().expect("library has a parent"))
                .map_err(|err| format!("create wasm library directory: {err}"))?;
            copy_file(&source, &destination, false)?;
        }
    }
    copy_tree(&root.join("std"), &stdlib)?;
    println!("assembled {}", build.display());
    Ok(())
}

fn uninstall() -> Result<()> {
    let destination = install_root()?;
    if destination.exists() {
        std::fs::remove_dir_all(&destination)
            .map_err(|err| format!("remove {}: {err}", destination.display()))?;
    }
    println!("removed {}", destination.display());
    Ok(())
}

fn install_root() -> Result<PathBuf> {
    let prefix = env::var("PREFIX").unwrap_or_else(|_| "/usr/local/hew".to_string());
    if !Path::new(&prefix).is_absolute() || matches!(prefix.as_str(), "/" | "/." | "/.." | "//") {
        return Err(format!("refusing unsafe installation prefix {prefix:?}"));
    }
    let destination = env::var("DESTDIR").unwrap_or_default();
    if !destination.is_empty() && !Path::new(&destination).is_absolute() {
        return Err(format!(
            "refusing non-absolute staging directory {destination:?}"
        ));
    }
    let combined = format!("{destination}{prefix}");
    let path = PathBuf::from(&combined);
    if combined.is_empty() || path == Path::new("/") {
        return Err(format!("refusing unsafe installation root {combined:?}"));
    }
    Ok(path)
}

fn copy_file(source: &Path, destination: &Path, executable_file: bool) -> Result<()> {
    if !source.is_file() {
        return Err(format!(
            "required release artifact is missing: {}",
            source.display()
        ));
    }
    std::fs::copy(source, destination).map_err(|err| {
        format!(
            "copy {} to {}: {err}",
            source.display(),
            destination.display()
        )
    })?;
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mode = if executable_file { 0o755 } else { 0o644 };
        std::fs::set_permissions(destination, std::fs::Permissions::from_mode(mode))
            .map_err(|err| format!("chmod {}: {err}", destination.display()))?;
    }
    Ok(())
}

fn copy_tree(source: &Path, destination: &Path) -> Result<()> {
    for entry in
        std::fs::read_dir(source).map_err(|err| format!("read {}: {err}", source.display()))?
    {
        let entry = entry.map_err(|err| format!("read {} entry: {err}", source.display()))?;
        let from = entry.path();
        let to = destination.join(entry.file_name());
        if from.is_dir() {
            std::fs::create_dir_all(&to)
                .map_err(|err| format!("create {}: {err}", to.display()))?;
            copy_tree(&from, &to)?;
        } else {
            copy_file(&from, &to, false)?;
        }
    }
    Ok(())
}

fn generate_completions(root: &Path, hew: &Path, destination: &Path) -> Result<()> {
    for shell in ["bash", "zsh", "fish"] {
        let output = Command::new(hew)
            .current_dir(root)
            .args(["completions", shell])
            .output()
            .map_err(|err| format!("generate {shell} completions: {err}"))?;
        ensure_success(output.status, "generate installed completions")?;
        if output.stdout.is_empty() {
            return Err(format!("{shell} completion generator produced no output"));
        }
        let path = destination.join(format!("hew.{shell}"));
        std::fs::write(&path, output.stdout)
            .map_err(|err| format!("write {}: {err}", path.display()))?;
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o644))
                .map_err(|err| format!("chmod {}: {err}", path.display()))?;
        }
    }
    Ok(())
}

fn cargo(root: &Path, args: &[&str]) -> Result<()> {
    run_program(root, cargo_executable(), args)
}

fn nextest(root: &Path, args: &[&str], filter_expr: Option<&str>) -> Result<()> {
    let mut command = Command::new(cargo_executable());
    command
        .current_dir(root)
        .args(["nextest", "run"])
        .args(args);
    if let Some(expression) = filter_expr {
        command.args(["-E", expression]);
    }
    run_command(&mut command, "run nextest gate")
}

fn run_program(root: &Path, program: impl AsRef<OsStr>, args: &[&str]) -> Result<()> {
    let mut command = Command::new(program);
    command.current_dir(root).args(args);
    run_command(&mut command, "command")
}

fn run_program_owned(root: &Path, program: impl AsRef<OsStr>, args: &[String]) -> Result<()> {
    let mut command = Command::new(program);
    command.current_dir(root).args(args);
    run_command(&mut command, "command")
}

fn clean(root: &Path) -> Result<()> {
    for directory in [root.join("build"), root.join("coverage-html")] {
        if directory.exists() {
            std::fs::remove_dir_all(&directory)
                .map_err(|err| format!("remove {}: {err}", directory.display()))?;
        }
    }
    cargo(root, &["clean"])
}

fn install_hooks(root: &Path) -> Result<()> {
    #[cfg(not(unix))]
    {
        let _ = root;
        return Err("hook installation is supported on Unix hosts".to_string());
    }
    #[cfg(unix)]
    {
        use std::os::unix::fs::{symlink, PermissionsExt};

        let output = Command::new("git")
            .current_dir(root)
            .args(["rev-parse", "--git-common-dir"])
            .output()
            .map_err(|err| format!("resolve git common directory: {err}"))?;
        ensure_success(output.status, "resolve git common directory")?;
        let value = String::from_utf8(output.stdout)
            .map_err(|err| format!("decode git common directory: {err}"))?;
        let value = value.trim();
        let common = if Path::new(value).is_absolute() {
            PathBuf::from(value)
        } else {
            root.join(value)
        };
        let hooks = common.join("hooks");
        let entries = [
            ("pre-commit.d/format", "../../../scripts/pre-commit-fmt.sh"),
            (
                "pre-push.d/ci-preflight",
                "../../../scripts/pre-push-ci-preflight.sh",
            ),
            (
                "pre-push.d/leak-scan",
                "../../../scripts/pre-push-leak-scan.sh",
            ),
        ];
        for (name, target) in entries {
            let path = hooks.join(name);
            std::fs::create_dir_all(
                path.parent()
                    .ok_or_else(|| format!("{} has no parent", path.display()))?,
            )
            .map_err(|err| format!("create hook directory: {err}"))?;
            match std::fs::symlink_metadata(&path) {
                Ok(metadata) if metadata.file_type().is_symlink() => {
                    std::fs::remove_file(&path)
                        .map_err(|err| format!("replace {}: {err}", path.display()))?;
                }
                Ok(_) => continue,
                Err(err) if err.kind() == std::io::ErrorKind::NotFound => {}
                Err(err) => return Err(format!("inspect {}: {err}", path.display())),
            }
            symlink(target, &path).map_err(|err| format!("link {}: {err}", path.display()))?;
        }
        let dispatcher = b"#!/usr/bin/env bash\nset -Eeuo pipefail\nhook_name=\"$(basename \"$0\")\"\nhook_dir=\"$(dirname \"$0\")/${hook_name}.d\"\n[ -d \"$hook_dir\" ] || exit 0\nfor hook in \"$hook_dir\"/*; do\n    [ -x \"$hook\" ] || continue\n    \"$hook\" \"$@\"\ndone\n";
        for name in ["pre-commit", "pre-push"] {
            let path = hooks.join(name);
            if !path.exists() {
                std::fs::write(&path, dispatcher)
                    .map_err(|err| format!("write {}: {err}", path.display()))?;
                std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o755))
                    .map_err(|err| format!("chmod {}: {err}", path.display()))?;
            }
        }
        Ok(())
    }
}

fn run_command(command: &mut Command, description: &str) -> Result<()> {
    eprintln!("+ {command:?}");
    let status = command
        .status()
        .map_err(|err| format!("{description}: {err}"))?;
    ensure_success(status, description)
}

fn ensure_success(status: ExitStatus, description: &str) -> Result<()> {
    if status.success() {
        Ok(())
    } else {
        Err(format!("{description} failed with {status}"))
    }
}

fn cargo_output_dir(root: &Path, profile: &str, target: Option<&str>) -> Result<PathBuf> {
    let output = Command::new(cargo_executable())
        .current_dir(root)
        .args(["metadata", "--no-deps", "--format-version", "1"])
        .output()
        .map_err(|err| format!("execute cargo metadata: {err}"))?;
    if !output.status.success() {
        return Err(format!(
            "cargo metadata failed: {}",
            String::from_utf8_lossy(&output.stderr).trim()
        ));
    }
    let metadata: Value = serde_json::from_slice(&output.stdout)
        .map_err(|err| format!("parse cargo metadata: {err}"))?;
    let mut directory = PathBuf::from(
        metadata["target_directory"]
            .as_str()
            .ok_or_else(|| "cargo metadata omitted target_directory".to_string())?,
    );
    let configured_target = env::var("CARGO_BUILD_TARGET").ok();
    if let Some(target) = target.or(configured_target.as_deref()) {
        directory.push(target);
    }
    directory.push(profile);
    Ok(directory)
}

fn add_profile_and_target<'a>(args: &mut Vec<&'a str>, profile: Profile, target: Option<&'a str>) {
    if profile == Profile::Release {
        args.push("--release");
    }
    if let Some(target) = target {
        args.extend(["--target", target]);
    }
}

fn cargo_executable() -> OsString {
    env::var_os("CARGO").unwrap_or_else(|| OsString::from("cargo"))
}

fn python() -> &'static str {
    if cfg!(windows) {
        "python"
    } else {
        "python3"
    }
}

fn executable(name: &str) -> OsString {
    if cfg!(windows) {
        OsString::from(format!("{name}.exe"))
    } else {
        OsString::from(name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tool_plan_unions_gate_requirements() {
        let plan = ToolPlan::for_gates("workspace,playground,lint").unwrap();
        assert!(plan.tools.contains(NEXTEST));
        assert!(plan.tools.contains(WASMTIME));
        assert!(plan.tools.contains(WASM_PACK));
        assert_eq!(plan.targets.len(), 2);
        assert!(plan.ast_grep);
    }

    #[test]
    fn tool_plan_rejects_unknown_gates() {
        assert!(ToolPlan::for_gates("workspace,typo").is_err());
    }

    #[test]
    fn gate_options_limit_filters_to_platform_test_gates() {
        let workspace = [
            "workspace".to_string(),
            "--filter-expr".to_string(),
            "not binary(~oracle)".to_string(),
        ];
        assert_eq!(
            parse_gate_options(&workspace).unwrap(),
            ("workspace", Some("not binary(~oracle)"))
        );

        let cabi = [
            "cabi".to_string(),
            "--filter-expr".to_string(),
            "all()".to_string(),
        ];
        assert!(parse_gate_options(&cabi).is_err());
    }

    #[test]
    fn gate_graph_is_closed_and_unique() {
        let mut names = BTreeSet::new();
        for gate in GATES {
            assert!(names.insert(gate.name), "duplicate gate {}", gate.name);
        }
        for gate in GATES {
            for dependency in gate.dependencies {
                assert!(
                    names.contains(dependency),
                    "gate {} depends on unknown gate {}",
                    gate.name,
                    dependency
                );
            }
        }
    }

    #[test]
    fn tool_plan_follows_transitive_gate_edges() {
        let plan = ToolPlan::for_gates("ci").unwrap();
        for tool in [NEXTEST, WASMTIME, WASM_PACK, CARGO_ABOUT, CARGO_DENY] {
            assert!(plan.tools.contains(tool), "ci plan omitted {tool}");
        }
        assert!(plan.targets.contains("wasm32-wasip1"));
        assert!(plan.targets.contains("wasm32-unknown-unknown"));
        assert!(plan.ast_grep);
    }
}
