#![cfg(feature = "component")]

use std::path::{Path, PathBuf};

use hew_live::component::build::{build, Toolchain};

fn environment(name: &str) -> PathBuf {
    std::env::var_os(name).map_or_else(
        || panic!("{name} must be set by make test-live-source"),
        PathBuf::from,
    )
}

#[test]
fn tracked_router_sources_build_with_their_generated_service_context() {
    let toolchain = Toolchain {
        compiler: environment("HEW_LIVE_COMPILER"),
        wasi_sdk: environment("HEW_WASI_SDK"),
        wasm_lib_dir: environment("HEW_LIVE_WASM_LIB_DIR"),
    };
    for relative in [
        "examples/live_router.hew",
        "tests/fixtures/router.hew",
        "tests/fixtures/preview_router.hew",
    ] {
        let source = Path::new(env!("CARGO_MANIFEST_DIR")).join(relative);
        build(&source, &toolchain).unwrap_or_else(|error| panic!("{relative}: {error}"));
    }
}

#[test]
fn source_build_preserves_project_imports_and_helper_diagnostics() {
    let workspace = tempfile::tempdir().unwrap();
    let project = workspace.path().join("app");
    let dependency = workspace.path().join("support");
    let shared = workspace.path().join("shared");
    for directory in [&project, &dependency, &shared] {
        std::fs::create_dir(directory).unwrap();
    }
    std::fs::write(project.join("hew.toml"), "[package]\nname = \"app\"\nversion = \"0.1.0\"\nedition = \"2026\"\n[dependencies]\nsupport = { path = \"../support\" }\n").unwrap();
    std::fs::write(
        dependency.join("hew.toml"),
        "[package]\nname = \"support\"\nversion = \"0.1.0\"\nedition = \"2026\"\n",
    )
    .unwrap();
    std::fs::write(
        dependency.join("support.hew"),
        "pub fn answer() -> u64 { 10 }\n",
    )
    .unwrap();
    let helper = project.join("helper.hew");
    std::fs::write(&helper, "pub fn answer() -> u64 { 20 }\n").unwrap();
    std::fs::write(
        shared.join("relative.hew"),
        "pub fn relative_answer() -> u64 { 30 }\n",
    )
    .unwrap();
    let fixture = std::fs::read_to_string(
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/router.hew"),
    )
    .unwrap();
    let source = project.join("router.hew");
    std::fs::write(
        &source,
        format!(
            "import helper;\nimport support;\nimport \"../shared/relative.hew\";\n{}",
            fixture.replace(
                "accepted: accepted",
                "accepted: accepted + helper.answer() + support.answer() + relative_answer()"
            )
        ),
    )
    .unwrap();
    let toolchain = Toolchain {
        compiler: environment("HEW_LIVE_COMPILER"),
        wasi_sdk: environment("HEW_WASI_SDK"),
        wasm_lib_dir: environment("HEW_LIVE_WASM_LIB_DIR"),
    };
    let installed = std::process::Command::new(&toolchain.compiler)
        .arg("install")
        .current_dir(&project)
        .env("HEW_HOME", workspace.path().join("package-cache"))
        .output()
        .unwrap();
    assert!(
        installed.status.success(),
        "{}",
        String::from_utf8_lossy(&installed.stderr)
    );
    let bytes = build(&source, &toolchain).expect("original project and all import forms compile");
    assert!(!bytes.is_empty());
    assert!(
        !project.join("router_services.hew").exists(),
        "generated source must stay outside the authored tree"
    );
    std::fs::write(&helper, "pub fn answer() -> u64 { missing_value }\n").unwrap();
    let error = build(&source, &toolchain).unwrap_err().to_string();
    assert!(
        error.contains(&helper.display().to_string()),
        "helper diagnostic lost its original path: {error}"
    );
    assert!(!project.join("router_services.hew").exists());
}
