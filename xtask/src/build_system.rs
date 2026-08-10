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
        "  gate <smoke|workspace|platform-smoke|platform-full|cabi|vertical-slice|hew-ratchet|stdlib-ratchet|playground|release-smoke|freebsd>",
        "  tools --gates GATE[,GATE...] [--field tools|targets|ast-grep]",
        "  tools --verify GATE[,GATE...]",
        "  ci-local [--list]",
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
        }
        Component::Native => {
            build_native(root, options.profile, options.target.as_deref(), true)?;
        }
        Component::Wasm => build_wasm(root, options.profile)?,
        Component::Release => {
            build_native(root, Profile::Release, options.target.as_deref(), true)?;
            build_wasm(root, Profile::Release)?;
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
    let name = args
        .first()
        .ok_or_else(|| "gate requires a gate name".to_string())?;
    if args.len() != 1 {
        return Err("gate accepts exactly one gate name".to_string());
    }
    match name.as_str() {
        "smoke" => {
            build_native(root, Profile::Debug, None, false)?;
            verify_libhew(root, None)?;
            cargo(root, &["fmt", "--all", "--", "--check"])?;
            cargo(
                root,
                &["nextest", "run", "--workspace", "--profile", "smoke"],
            )
        }
        "workspace" | "platform-full" => {
            build_native(root, Profile::Debug, None, true)?;
            build_wasm(root, Profile::Debug)?;
            verify_libhew(root, None)?;
            cargo(
                root,
                &[
                    "nextest",
                    "run",
                    "--workspace",
                    "--exclude",
                    "hew-wasm",
                    "--exclude",
                    "hew-cabi",
                    "--profile",
                    "ci",
                    "--no-fail-fast",
                ],
            )
        }
        "platform-smoke" => {
            build_native(root, Profile::Debug, None, true)?;
            build_wasm(root, Profile::Debug)?;
            verify_libhew(root, None)?;
            cargo(
                root,
                &[
                    "nextest",
                    "run",
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
            )
        }
        "cabi" => cargo(
            root,
            &["nextest", "run", "--profile", "ci-cabi", "-p", "hew-cabi"],
        ),
        "vertical-slice" => compiled_hew_script(root, "tests/vertical-slice/run.sh", &[]),
        "hew-ratchet" => compiled_hew_script(root, "scripts/hew-suite-ratchet.sh", &[]),
        "stdlib-ratchet" => compiled_hew_script(root, "scripts/stdlib-ratchet.sh", &[]),
        "playground" => {
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
        "freebsd" => freebsd_gate(root),
        "release-smoke" => release_smoke(root),
        other => Err(format!("unknown gate {other:?}")),
    }
}

fn freebsd_gate(root: &Path) -> Result<()> {
    gate(root, &["workspace".to_string()])?;
    gate(root, &["cabi".to_string()])?;
    gate(root, &["vertical-slice".to_string()])?;
    gate(root, &["hew-ratchet".to_string()])?;
    cargo(
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
    )
}

fn release_smoke(root: &Path) -> Result<()> {
    build_native(root, Profile::Release, None, false)?;
    run_program(root, "scripts/test-release-binary.sh", &["--no-build"])
}

fn compiled_hew_script(root: &Path, script: &str, args: &[&str]) -> Result<()> {
    build_native(root, Profile::Debug, None, false)?;
    verify_libhew(root, None)?;
    let hew = cargo_output_dir(root, "debug", None)?.join(executable("hew"));
    let mut command = Command::new(script);
    command.current_dir(root).env("HEW_BIN", hew).args(args);
    run_command(&mut command, script)
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
        for gate in input
            .split(',')
            .map(str::trim)
            .filter(|gate| !gate.is_empty())
        {
            match gate {
                "workspace" | "platform-smoke" | "platform-full" | "freebsd" => {
                    plan.tools.extend([NEXTEST, WASMTIME]);
                    plan.targets.insert("wasm32-wasip1");
                }
                "smoke" | "cabi" => {
                    plan.tools.insert(NEXTEST);
                }
                "playground" => {
                    plan.tools.insert(WASM_PACK);
                    plan.targets.insert("wasm32-unknown-unknown");
                }
                "lint" => plan.ast_grep = true,
                "licenses" => plan.tools.extend([CARGO_ABOUT, CARGO_DENY]),
                "coverage" => plan.tools.extend([LLVM_COV, NEXTEST]),
                "vertical-slice" | "hew-ratchet" | "stdlib-ratchet" | "release-smoke" | "build" => {
                }
                other => return Err(format!("unknown CI gate {other:?}")),
            }
        }
        Ok(plan)
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

fn cargo(root: &Path, args: &[&str]) -> Result<()> {
    run_program(root, cargo_executable(), args)
}

fn run_program(root: &Path, program: impl AsRef<OsStr>, args: &[&str]) -> Result<()> {
    let mut command = Command::new(program);
    command.current_dir(root).args(args);
    run_command(&mut command, "command")
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
}
