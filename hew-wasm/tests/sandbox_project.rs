use hew_wasm::sandbox::{
    compile_project_to_sandbox_bytecode, compile_to_sandbox_bytecode, ProjectCompileOutput,
};
use serde_json::json;

fn compile(entry: &str, files: &[(&str, &str)]) -> ProjectCompileOutput {
    compile_project_to_sandbox_bytecode(
        &json!({"entry": entry, "files": files.iter().map(|(path, source)| {
            json!({"path": path, "source": source})
        }).collect::<Vec<_>>()})
        .to_string(),
        None,
    )
    .expect("project compilation")
}

fn admitted(output: &ProjectCompileOutput) {
    assert!(
        output.output.bytecode.is_some(),
        "{:?}",
        output.output.diagnostics
    );
}

fn run_in_vm(project: &ProjectCompileOutput) -> serde_json::Value {
    let file = tempfile::NamedTempFile::new().expect("package file");
    serde_json::to_writer(file.as_file(), project.output.bytecode.as_ref().unwrap())
        .expect("serialize bytecode");
    let output = std::process::Command::new("node")
        .current_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/../hew-sandbox-vm"))
        .args([
            "--input-type=module",
            "-e",
            "import fs from 'node:fs'; import { runBytecode } from './dist/interpreter/index.js'; \
             const pkg = JSON.parse(fs.readFileSync(process.argv[1], 'utf8')); \
             process.stdout.write(JSON.stringify(await runBytecode(pkg, {})));",
        ])
        .arg(file.path())
        .output()
        .expect("run sandbox VM");
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    serde_json::from_slice(&output.stdout).expect("VM trace")
}

#[test]
fn single_buffer_and_project_share_the_package_lowering() {
    let source = "fn main() { println(42); }";
    let project = compile("playground.hew", &[("playground.hew", source)]);
    let single = compile_to_sandbox_bytecode(source, None).unwrap();
    admitted(&project);
    assert_eq!(
        serde_json::to_value(&project.output).unwrap(),
        serde_json::to_value(single).unwrap()
    );
    assert_eq!(project.resolved_files, ["playground.hew"]);
}

#[test]
fn imports_use_the_transitive_source_closure_without_unused_files_or_std() {
    for (import, name) in [
        ("import user.nav;", "nav"),
        ("import user . nav;", "nav"),
        ("import user.nav as route;", "route"),
    ] {
        let main = format!("import std.io; {import} fn main() {{ println({name}.go()); }}");
        let project = compile(
            "main.hew",
            &[
                ("main.hew", &main),
                ("user/hew.toml", "[package]\nname = \"user\"\n"),
                (
                    "user/nav.hew",
                    "import user.steps; pub fn go() -> string { steps.go() }",
                ),
                ("user/steps.hew", "pub fn go() -> string { \"MOVE\" }"),
                ("user/unused.hew", "this supplied file must not be parsed"),
            ],
        );
        admitted(&project);
        assert_eq!(
            project.resolved_files,
            ["main.hew", "user/nav.hew", "user/steps.hew"]
        );
    }
}

#[test]
fn directory_modules_include_the_native_frontends_peer_sources() {
    let project = compile(
        "main.hew",
        &[
            (
                "main.hew",
                "import user.nav; fn main() { println(nav.go()); }",
            ),
            ("user/hew.toml", "[package]\nname = \"user\"\n"),
            ("user/nav/nav.hew", "pub fn go() -> string { command() }"),
            ("user/nav/steps.hew", "fn command() -> string { \"MOVE\" }"),
        ],
    );
    admitted(&project);
    assert_eq!(
        project.resolved_files,
        ["main.hew", "user/nav/nav.hew", "user/nav/steps.hew"]
    );
}

#[test]
fn local_response_binding_shadows_imported_response_module_in_vm() {
    let project = compile(
        "main.hew",
        &[
            (
                "main.hew",
                include_str!("../../tests/pkg-import/local_module_shadow_response.hew"),
            ),
            (
                "user/hew.toml",
                include_str!("../../tests/pkg-import/pkgs/user/hew.toml"),
            ),
            (
                "user/response.hew",
                include_str!("../../tests/pkg-import/pkgs/user/response.hew"),
            ),
        ],
    );
    admitted(&project);
    assert_eq!(project.resolved_files, ["main.hew", "user/response.hew"]);

    let trace = run_in_vm(&project);
    assert_eq!(trace["result"], "ok", "{trace:#}");
    assert_eq!(trace["final_state"]["stdout"], json!(["ok\n"]));
}

#[test]
fn vec_fold_project_runs_with_seed_first_arguments_in_vm() {
    let project = compile(
        "main.hew",
        &[(
            "main.hew",
            include_str!("../../tests/core-acceptance/cases/vec-fold-method.hew"),
        )],
    );
    admitted(&project);
    let trace = run_in_vm(&project);
    assert_eq!(trace["result"], "ok", "{trace:#}");
    assert_eq!(trace["final_state"]["stdout"], json!(["6\n", "7\n", "6\n"]));
}

#[test]
fn imported_diagnostics_keep_file_local_utf8_spans() {
    let library = "// 🐦\npub fn go() -> string { 42 }";
    let project = compile(
        "mission.hew",
        &[
            (
                "mission.hew",
                "import user.nav; fn main() { println(nav.go()); }",
            ),
            ("user/nav.hew", library),
        ],
    );
    assert!(project.output.bytecode.is_none());
    let diagnostic = project
        .output
        .diagnostics
        .iter()
        .find(|d| d.severity == "error")
        .unwrap();
    assert_eq!(diagnostic.source_module.as_deref(), Some("user/nav.hew"));
    assert_eq!(
        library[diagnostic.start_offset..diagnostic.end_offset].trim(),
        "42"
    );
}

#[test]
fn independent_projects_do_not_retain_previous_documents_or_read_host_sources() {
    let main = "import user.nav; fn main() { println(nav.go()); }";
    admitted(&compile(
        "main.hew",
        &[
            ("main.hew", main),
            ("user/nav.hew", "pub fn go() -> string { \"MOVE\" }"),
        ],
    ));
    let missing = compile("main.hew", &[("main.hew", main)]);
    assert!(missing.output.bytecode.is_none());
    assert!(missing
        .output
        .diagnostics
        .iter()
        .any(|d| d.kind == "E_MODULE_NOT_FOUND"));
    assert_eq!(missing.resolved_files, ["main.hew"]);
    // This file exists in the host checkout, but was not supplied to the project.
    assert!(std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/stdin_lines.hew")
        .exists());
    let host = compile(
        "main.hew",
        &[(
            "main.hew",
            "import tests.fixtures.stdin_lines; fn main() {}",
        )],
    );
    assert!(host.output.bytecode.is_none());
    assert!(host
        .output
        .diagnostics
        .iter()
        .any(|d| d.kind == "E_MODULE_NOT_FOUND"));
}

#[test]
fn malformed_projects_and_source_authority_replacements_are_refused() {
    let requests = [
        "{bad".to_owned(),
        json!({"entry":"main.hew","files":[],"unknown":true}).to_string(),
        json!({"entry":"main.hew","files":[]}).to_string(),
        json!({"entry":"../main.hew","files":[]}).to_string(),
        json!({"entry":"C:\\main.hew","files":[]}).to_string(),
        json!({"entry":"main.hew","files":[{"path":"main.hew","source":"fn main() {}"},{"path":"main.hew","source":"fn main() {}"}]}).to_string(),
        json!({"entry":"main.hew","files":[{"path":"main.hew","source":"fn main() {}"},{"path":"std/io.hew","source":"pub fn read_line() -> string { \"forged\" }"}]}).to_string(),
    ];
    for request in requests {
        let output = compile_project_to_sandbox_bytecode(&request, None).unwrap();
        assert!(output.output.bytecode.is_none(), "{request}");
        assert!(output.resolved_files.is_empty(), "{request}");
        assert_eq!(
            output.output.diagnostics[0].kind, "invalid_project",
            "{request}"
        );
    }
}
