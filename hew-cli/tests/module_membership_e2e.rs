//! Module membership is anchored to the package manifest or the std root,
//! never to the name of the directory a project was checked out into
//! (HEW-SPEC-2026 §3.5.1, §3.5.3). Each test runs the real CLI in temporary
//! directories and carries a negative control.

mod support;

use std::fs;
use std::path::Path;
use std::process::{Command, Output};

use support::{describe_output, hew_binary, repo_root, require_codegen, run_bounded_command};

fn write(root: &Path, path: &str, source: &str) {
    let path = root.join(path);
    fs::create_dir_all(path.parent().expect("a fixture file has a parent"))
        .expect("create fixture directory");
    fs::write(path, source).expect("write fixture file");
}

fn manifest(name: &str) -> String {
    format!("[package]\nname = \"{name}\"\nversion = \"0.1.0\"\n")
}

fn hew_in(dir: &Path, args: &[&str]) -> Output {
    let mut command = Command::new(hew_binary());
    command.args(args).current_dir(dir);
    run_bounded_command(command, format!("hew {}", args.join(" ")))
}

fn text(output: &Output) -> String {
    support::strip_ansi(&format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    ))
}

/// A flat program directory: `app.hew` plus two programs' helpers that each
/// declare `pub fn run`, and a `main.hew` that imports `app`.
fn write_flat_program(root: &Path) {
    write(
        root,
        "app.hew",
        "pub fn hello() -> string { \"from app.hew\" }\n",
    );
    write(root, "one.hew", "pub fn run() -> i64 { 1 }\n");
    write(root, "two.hew", "pub fn run() -> i64 { 2 }\n");
    write(
        root,
        "main.hew",
        "import app;\n\nfn main() {\n    println(app.hello());\n}\n",
    );
}

#[test]
fn a_file_means_the_same_module_whatever_its_directory_is_called() {
    require_codegen();
    let workspace = support::tempdir();
    // `app/app.hew` used to make every file in `app/` one module, so the two
    // `pub fn run` declarations collided only in a directory named `app`.
    for name in ["app", "zz"] {
        let root = workspace.path().join(name);
        write_flat_program(&root);
        let run = hew_in(&root, &["run", "main.hew"]);
        assert!(run.status.success(), "{name}: {}", describe_output(&run));
        assert_eq!(
            String::from_utf8_lossy(&run.stdout),
            "from app.hew\n",
            "{name}"
        );
        let check = hew_in(&root, &["check", "one.hew"]);
        assert!(
            check.status.success(),
            "{name}: {}",
            describe_output(&check)
        );
        assert!(
            !text(&check).contains("belongs to directory module"),
            "{name}: a loose file is a module of its own\n{}",
            text(&check)
        );
    }
}

#[test]
fn a_package_test_reaches_its_package_by_name_from_any_working_directory() {
    require_codegen();
    let workspace = support::tempdir();
    let root = workspace.path().join("app");
    write_flat_program(&root);
    write(
        &root,
        "tests/t.hew",
        "import app;\n\nfn main() {\n    println(app.hello());\n}\n",
    );
    let test_file = root.join("tests/t.hew");
    let test_file = test_file.to_str().expect("UTF-8 path");
    let dirs = [Path::new("/"), root.as_path(), &root.join("tests")];

    // Negative control: without a package, `app` is in a parent directory of
    // the test and is not found, wherever the command runs.
    for dir in dirs {
        let run = hew_in(dir, &["run", test_file]);
        assert!(!run.status.success(), "{}", describe_output(&run));
        let output = text(&run);
        assert!(
            output.contains("module `app` not found")
                && output.contains("is in a parent directory of the importing file"),
            "from {}: {output}",
            dir.display()
        );
    }

    fs::write(root.join("hew.toml"), manifest("app")).expect("write manifest");
    for dir in dirs {
        let run = hew_in(dir, &["run", test_file]);
        assert!(
            run.status.success(),
            "from {}: {}",
            dir.display(),
            describe_output(&run)
        );
        assert_eq!(String::from_utf8_lossy(&run.stdout), "from app.hew\n");
    }
}

fn write_greeting(root: &Path, helpers: &str) {
    write(
        root,
        "main.hew",
        "import greeting;\n\nfn main() {\n    println(greeting.hello() + \" \" + greeting.target());\n}\n",
    );
    write(
        root,
        "greeting/greeting.hew",
        "pub fn hello() -> string { \"Hello\" }\n",
    );
    write(root, "greeting/greeting_helpers.hew", helpers);
}

const HELPERS: &str = "pub fn target() -> string { \"from a merged directory module!\" }\n";

#[test]
fn a_directory_module_belongs_to_its_package() {
    require_codegen();
    let workspace = support::tempdir();
    let root = workspace.path();
    write_greeting(root, HELPERS);

    // Negative control: outside a package `greeting/` is only a namespace.
    let loose = hew_in(root, &["run", "main.hew"]);
    assert!(!loose.status.success(), "{}", describe_output(&loose));
    let output = text(&loose);
    assert!(
        output.contains("module `greeting` not found")
            && output
                .contains("forms a directory module, and directory modules belong to a package")
            && output.contains("hew init"),
        "{output}"
    );

    fs::write(root.join("hew.toml"), manifest("myapp")).expect("write manifest");
    let run = hew_in(root, &["run", "main.hew"]);
    assert!(run.status.success(), "{}", describe_output(&run));
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "Hello from a merged directory module!\n"
    );
    let check = hew_in(root, &["check", "greeting/greeting_helpers.hew"]);
    assert!(check.status.success(), "{}", describe_output(&check));
    assert!(
        text(&check).contains("belongs to directory module `myapp.greeting`"),
        "the note names the dotted module\n{}",
        text(&check)
    );
}

#[test]
fn colliding_peers_name_both_files_and_why_they_share_a_module() {
    let workspace = support::tempdir();
    let root = workspace.path();
    fs::write(root.join("hew.toml"), manifest("myapp")).expect("write manifest");
    write_greeting(root, HELPERS);
    write(
        root,
        "greeting/more_helpers.hew",
        "pub fn target() -> string { \"again\" }\n",
    );

    for args in [
        &["check", "main.hew"][..],
        &["check", "greeting/more_helpers.hew"][..],
    ] {
        let check = hew_in(root, args);
        assert!(!check.status.success(), "{}", describe_output(&check));
        let output = text(&check);
        assert!(
            output.contains("duplicate pub name `target` in module `myapp.greeting`")
                && output.contains("`greeting/greeting_helpers.hew`")
                && output.contains("`greeting/more_helpers.hew`")
                && output.contains(
                    "`greeting/` is a directory module because `greeting/greeting.hew` exists"
                ),
            "{output}"
        );
    }
}

/// Package `acme.http`: the root module `http.hew` and `client.hew` both
/// declare `pub fn location`, which only collide if `client.hew` is merged
/// into the root module.
fn write_http_package(root: &Path) {
    write(root, "hew.toml", &manifest("acme.http"));
    write(
        root,
        "http.hew",
        "pub fn location() -> string { \"root\" }\n",
    );
    write(
        root,
        "client.hew",
        "pub fn location() -> string { \"client\" }\n",
    );
    write(
        root,
        "tests/public_api.hew",
        "import acme.http;\nimport acme.http.client;\n\nfn main() {\n    println(http.location() + \"/\" + client.location());\n}\n",
    );
}

const CONSUMER: &str = "import acme.http;\nimport acme.http.client;\n\nfn main() {\n    println(http.location() + \"/\" + client.location());\n}\n";

#[test]
fn a_package_module_is_the_same_checked_out_installed_or_linked() {
    require_codegen();
    let workspace = support::tempdir();
    for name in ["http", "my-http"] {
        let root = workspace.path().join(name);
        write_http_package(&root);
        let run = hew_in(&root, &["run", "tests/public_api.hew"]);
        assert!(run.status.success(), "{name}: {}", describe_output(&run));
        assert_eq!(
            String::from_utf8_lossy(&run.stdout),
            "root/client\n",
            "{name}"
        );
    }

    // A path dependency: a link (a copy on Windows) to `my-http/`.
    let home = workspace.path().join("home");
    let linked = workspace.path().join("linked");
    fs::create_dir_all(&home).expect("create home");
    fs::create_dir_all(&linked).expect("create consumer");
    fs::write(linked.join("hew.toml"), manifest("consumer")).expect("write manifest");
    write(&linked, "main.hew", CONSUMER);
    for args in [
        &["add", "acme.http", "--path", "../my-http"][..],
        &["install"][..],
    ] {
        let output = Command::new(hew_binary())
            .args(args)
            .current_dir(&linked)
            .env("HOME", &home)
            .env_remove("USERPROFILE")
            .output()
            .expect("run hew package command");
        assert!(output.status.success(), "{}", describe_output(&output));
    }
    let run = hew_in(&linked, &["run", "main.hew"]);
    assert!(run.status.success(), "linked: {}", describe_output(&run));
    assert_eq!(String::from_utf8_lossy(&run.stdout), "root/client\n");

    // The registry layout: the package unpacked at `.hew/packages/acme/http/`.
    let installed = workspace.path().join("installed");
    fs::create_dir_all(&installed).expect("create consumer");
    fs::write(
        installed.join("hew.toml"),
        format!(
            "{}\n[dependencies]\n\"acme.http\" = \"0.1.0\"\n",
            manifest("consumer")
        ),
    )
    .expect("write manifest");
    write(&installed, "main.hew", CONSUMER);
    write_http_package(&installed.join(".hew/packages/acme/http"));
    let run = hew_in(&installed, &["run", "main.hew"]);
    assert!(run.status.success(), "installed: {}", describe_output(&run));
    assert_eq!(String::from_utf8_lossy(&run.stdout), "root/client\n");
}

#[test]
fn two_spellings_of_one_directory_module_are_one_module_in_either_order() {
    require_codegen();
    let workspace = support::tempdir();
    let root = workspace.path();
    fs::write(root.join("hew.toml"), manifest("p")).expect("write manifest");
    write(
        root,
        "roles/roles.hew",
        "pub fn role() -> string { \"broker\" }\n",
    );
    write(
        root,
        "roles/service.hew",
        "pub fn service() -> string { role() + \"-service\" }\n",
    );
    write(
        root,
        "other.hew",
        "import roles;\n\npub fn describe() -> string { roles.service() }\n",
    );
    let mut outputs = Vec::new();
    for imports in [
        "import p.roles;\nimport other;\n",
        "import other;\nimport p.roles;\n",
    ] {
        write(
            root,
            "main.hew",
            &format!("{imports}\nfn main() {{\n    println(roles.role() + \" \" + other.describe());\n}}\n"),
        );
        let run = hew_in(root, &["run", "main.hew"]);
        assert!(run.status.success(), "{imports}{}", describe_output(&run));
        outputs.push(String::from_utf8_lossy(&run.stdout).into_owned());
    }
    assert_eq!(outputs[0], "broker broker-service\n");
    assert_eq!(outputs[0], outputs[1]);
}

#[test]
fn a_user_module_beside_the_std_root_is_not_importable() {
    require_codegen();
    let workspace = support::tempdir();
    let std_root = workspace.path().join("toolchain");
    let mut pending = vec![repo_root().join("std")];
    while let Some(dir) = pending.pop() {
        for entry in fs::read_dir(&dir).expect("read std").flatten() {
            let path = entry.path();
            let relative = path
                .strip_prefix(repo_root())
                .expect("below the repository");
            if path.is_dir() {
                pending.push(path.clone());
            } else if path.extension().is_some_and(|extension| extension == "hew") {
                write(
                    &std_root,
                    relative.to_str().expect("UTF-8 path"),
                    &fs::read_to_string(&path).expect("read std source"),
                );
            }
        }
    }
    write(&std_root, "mid_leak.hew", "pub fn leaked() -> i64 { 1 }\n");
    let program = workspace.path().join("program");
    write(
        &program,
        "leak.hew",
        "import mid_leak;\n\nfn main() {\n    println(mid_leak.leaked());\n}\n",
    );
    write(
        &program,
        "hex.hew",
        "import std.encoding.hex;\n\nfn main() {\n    println(hex.encode(b\"hi\"));\n}\n",
    );
    let std_dir = std_root.join("std");
    let run = |file: &str| {
        let mut command = Command::new(hew_binary());
        command
            .args(["run", file])
            .current_dir(&program)
            .env("HEW_STD", &std_dir);
        run_bounded_command(command, format!("hew run {file}"))
    };

    let leak = run("leak.hew");
    assert!(!leak.status.success(), "{}", describe_output(&leak));
    assert!(
        text(&leak).contains("module `mid_leak` not found"),
        "{}",
        text(&leak)
    );
    // Negative control: the same std root still serves `std.*`.
    let hex = run("hex.hew");
    assert!(hex.status.success(), "{}", describe_output(&hex));
    assert_eq!(String::from_utf8_lossy(&hex.stdout), "6869\n");
}

#[test]
fn a_dotted_package_imports_itself_by_its_whole_name() {
    require_codegen();
    let workspace = support::tempdir();
    let root = workspace.path();
    fs::write(root.join("hew.toml"), manifest("meshcore.broker")).expect("write manifest");
    write(root, "wire.hew", "pub fn frame() -> i64 { 7 }\n");
    write(
        root,
        "main.hew",
        "import meshcore.broker.wire;\n\nfn main() {\n    println(wire.frame());\n}\n",
    );
    let run = hew_in(root, &["run", "main.hew"]);
    assert!(run.status.success(), "{}", describe_output(&run));
    assert_eq!(String::from_utf8_lossy(&run.stdout), "7\n");

    // Negative control: a shorter prefix of the name is not the package.
    write(
        root,
        "main.hew",
        "import meshcore.wire;\n\nfn main() {\n    println(wire.frame());\n}\n",
    );
    let check = hew_in(root, &["check", "main.hew"]);
    assert!(!check.status.success(), "{}", describe_output(&check));
    assert!(
        text(&check).contains("`meshcore.wire` is not declared in hew.toml"),
        "{}",
        text(&check)
    );
}

#[test]
fn running_a_file_without_main_names_the_file() {
    require_codegen();
    let workspace = support::tempdir();
    let root = workspace.path();
    write(root, "library.hew", "pub fn answer() -> i64 { 42 }\n");

    let run = hew_in(root, &["run", "library.hew"]);
    assert!(!run.status.success(), "{}", describe_output(&run));
    let output = text(&run);
    assert!(
        output.contains("E_NO_MAIN") && output.contains("library.hew has no `fn main`"),
        "{output}"
    );
    assert!(
        !output.contains("linker"),
        "the linker must not run: {output}"
    );
    // Negative control: the same file is a valid module to check.
    let check = hew_in(root, &["check", "library.hew"]);
    assert!(check.status.success(), "{}", describe_output(&check));
}

#[test]
fn checking_a_package_root_module_checks_it_under_the_package_name() {
    let workspace = support::tempdir();
    let root = workspace.path().join("checkout");
    fs::create_dir_all(&root).expect("create package");
    fs::write(root.join("hew.toml"), manifest("acme.meter")).expect("write manifest");
    write(
        &root,
        "meter.hew",
        "pub type Meter {\n    v: i64;\n}\n\npub fn read(m: meter.Meter) -> i64 {\n    m.v\n}\n",
    );
    let check = hew_in(&root, &["check", "meter.hew"]);
    assert!(check.status.success(), "{}", describe_output(&check));
    // Negative control: another top-level file is a module of its own, so
    // the root module's qualifier is not its name.
    write(
        &root,
        "other.hew",
        "pub type Meter {\n    v: i64;\n}\n\npub fn read(m: meter.Meter) -> i64 {\n    m.v\n}\n",
    );
    let other = hew_in(&root, &["check", "other.hew"]);
    assert!(!other.status.success(), "{}", describe_output(&other));
}

#[test]
fn a_nested_file_reaches_package_modules_by_their_path_from_the_root() {
    require_codegen();
    let workspace = support::tempdir();
    let root = workspace.path().join("roles");
    write(&root, "wire.hew", "pub fn tag() -> i64 { 1 }\n");
    write(&root, "hostlib/binary.hew", "pub fn width() -> i64 { 8 }\n");
    write(
        &root,
        "hostlib/events.hew",
        "import hostlib.binary;\n\npub fn size() -> i64 { binary.width() * 2 }\n",
    );
    write(
        &root,
        "meshproto/radio.hew",
        "import wire;\nimport hostlib.events;\n\npub fn frame() -> i64 { wire.tag() + events.size() }\n",
    );
    write(
        &root,
        "main.hew",
        "import meshproto.radio;\n\nfn main() {\n    println(radio.frame());\n}\n",
    );

    let loose = hew_in(&root, &["run", "main.hew"]);
    assert!(!loose.status.success(), "{}", describe_output(&loose));

    fs::write(root.join("hew.toml"), manifest("meshcore.roles")).expect("write manifest");
    let run = hew_in(&root, &["run", "main.hew"]);
    assert!(run.status.success(), "{}", describe_output(&run));
    assert_eq!(String::from_utf8_lossy(&run.stdout), "17\n");

    write(&root, "meshproto/wire.hew", "pub fn tag() -> i64 { 2 }\n");
    let ambiguous = hew_in(&root, &["check", "main.hew"]);
    assert!(
        !ambiguous.status.success(),
        "{}",
        describe_output(&ambiguous)
    );
    let diagnostic = text(&ambiguous);
    assert!(diagnostic.contains("E_IMPORT_AMBIGUOUS"), "{diagnostic}");
}
