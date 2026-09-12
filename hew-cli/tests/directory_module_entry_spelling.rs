//! A directory module has one import spelling: its directory.
//!
//! `pkg/dir/dir.hew` is the entry file of the directory module `pkg.dir`, not a
//! module of its own. Accepting `pkg.dir.dir` as well would let one compilation
//! reach one source under two names, and the second identity made the entry
//! file's own private calls unresolvable at HIR lowering.
mod support;

use std::fs;
use std::path::Path;

/// Write the two-file package used by both tests. `mid.hew` reaches the
/// directory module through `spelling`.
fn write_package(root: &Path, spelling: &str) {
    fs::create_dir_all(root.join("src/lib")).expect("create package directories");
    fs::write(
        root.join("hew.toml"),
        "[package]\nname = \"probe\"\nedition = \"2026\"\nversion = \"0.1.0\"\n\n[dependencies]\n",
    )
    .expect("write manifest");
    fs::write(
        root.join("main.hew"),
        "import probe.lib;\nimport probe.mid;\n\nfn main() { println(lib.describe() + mid.through()); }\n",
    )
    .expect("write root module");
    fs::write(
        root.join("src/mid.hew"),
        format!("import {spelling};\n\npub fn through() -> string {{ \"/mid\" }}\n"),
    )
    .expect("write second importer");
    fs::write(
        root.join("src/lib/lib.hew"),
        "fn tag() -> string { \"lib\" }\n\npub fn describe() -> string { tag() + peer_note() }\n",
    )
    .expect("write entry file");
    fs::write(
        root.join("src/lib/lib_notes.hew"),
        "pub fn peer_note() -> string { \"+peer\" }\n",
    )
    .expect("write peer file");
}

#[test]
fn entry_file_spelling_is_refused_and_names_the_directory_module() {
    let workspace = support::tempdir();
    write_package(workspace.path(), "probe.lib.lib");

    let output = support::run_hew_in(workspace.path(), &["check", "--format", "json", "main.hew"]);

    assert!(
        !output.status.success(),
        "`import probe.lib.lib` must be refused\n{}",
        support::describe_output(&output),
    );
    let diagnostics = support::strip_ansi(&String::from_utf8_lossy(&output.stdout));
    assert!(
        diagnostics.contains("\"code\": \"E_ENTRY_FILE_IMPORT\""),
        "refusal must carry its diagnostic code\n{diagnostics}",
    );
    assert!(
        diagnostics.contains("import `probe.lib` instead"),
        "refusal must name the directory module to import\n{diagnostics}",
    );
}

/// Negative control: the directory spelling of the same package checks, so the
/// refusal above is about the spelling and not about the package's shape.
#[test]
fn directory_spelling_of_the_same_package_checks() {
    let workspace = support::tempdir();
    write_package(workspace.path(), "probe.lib");

    let output = support::run_hew_in(workspace.path(), &["check", "main.hew"]);

    assert!(
        output.status.success(),
        "`import probe.lib` from two importers must check\n{}",
        support::describe_output(&output),
    );
}

/// Negative control: a repeated last segment is not by itself the entry-file
/// spelling. `probe.probe` names the directory module `src/probe/`, whose name
/// happens to match its package; the import resolves through that directory,
/// not through a shorter path's entry file, so it stays accepted.
#[test]
fn namesake_directory_module_is_the_directory_spelling() {
    let workspace = support::tempdir();
    let root = workspace.path();
    fs::create_dir_all(root.join("src/probe")).expect("create package directories");
    fs::write(
        root.join("hew.toml"),
        "[package]\nname = \"probe\"\nedition = \"2026\"\nversion = \"0.1.0\"\n\n[dependencies]\n",
    )
    .expect("write manifest");
    fs::write(
        root.join("src/probe/probe.hew"),
        "fn tag() -> string { \"namesake\" }\n\npub fn describe() -> string { tag() }\n",
    )
    .expect("write entry file");
    fs::write(
        root.join("main.hew"),
        "import probe.probe;\n\nfn main() { println(probe.describe()); }\n",
    )
    .expect("write root module");

    let output = support::run_hew_in(root, &["check", "main.hew"]);

    assert!(
        output.status.success(),
        "`import probe.probe` names a directory module and must check\n{}",
        support::describe_output(&output),
    );
}
