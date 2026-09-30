//! `hew fmt --migrate` rewrites retired spellings syntactically: every file
//! migrates on its own, nothing is type-checked, and any refusal leaves every
//! file as it was.
mod support;

use std::path::Path;
use std::process::{Command, Output};

use support::{hew_binary, strip_ansi};

fn migrate(args: &[&str], root: &Path) -> Output {
    Command::new(hew_binary())
        .args(["fmt", "--migrate"])
        .args(args)
        .arg("--root")
        .arg(root)
        .output()
        .unwrap()
}

fn stderr(output: &Output) -> String {
    strip_ansi(&String::from_utf8_lossy(&output.stderr))
}

/// A directory module whose entry and peer both carry retired punctuation and
/// `::` paths, plus a nested importer.
fn legacy_tree(dir: &Path) {
    let module = dir.join("greeting");
    std::fs::create_dir(&module).unwrap();
    std::fs::write(
        module.join("greeting.hew"),
        "pub type Label { text: string, }\npub fn empty_labels() -> Vec<Label> { Vec::new() }\n",
    )
    .unwrap();
    std::fs::write(
        module.join("dog.hew"),
        "pub fn dogs() -> Vec<Label> { Vec::<Label>::new() }\n",
    )
    .unwrap();
    std::fs::write(
        dir.join("main.hew"),
        "import greeting::{empty_labels};\nfn main() { let values = empty_labels(); }\n",
    )
    .unwrap();
}

#[test]
fn migrate_root_rewrites_every_file_and_a_second_pass_changes_nothing() {
    let dir = support::tempdir();
    legacy_tree(dir.path());

    let first = migrate(&[], dir.path());
    assert!(first.status.success(), "{}", stderr(&first));
    let entry = std::fs::read_to_string(dir.path().join("greeting/greeting.hew")).unwrap();
    let peer = std::fs::read_to_string(dir.path().join("greeting/dog.hew")).unwrap();
    let main = std::fs::read_to_string(dir.path().join("main.hew")).unwrap();
    assert!(
        entry.contains("text: string;") && entry.contains("Vec.new()"),
        "{entry}"
    );
    assert!(peer.contains("Vec<Label>.new()"), "{peer}");
    assert!(main.contains("import greeting.{empty_labels};"), "{main}");

    let second = migrate(&["--check"], dir.path());
    assert!(second.status.success(), "{}", stderr(&second));
    let check = Command::new(hew_binary())
        .arg("check")
        .arg(dir.path().join("main.hew"))
        .output()
        .unwrap();
    assert!(check.status.success(), "{}", stderr(&check));
}

/// A deliberately ill-typed program is still a program: its syntax migrates
/// and its type error stays for the author.
#[test]
fn migrate_does_not_type_check_what_it_rewrites() {
    let dir = support::tempdir();
    let path = dir.path().join("negative.hew");
    std::fs::write(
        &path,
        "type Pair { left: i64, }\nfn main() { let x: i64 = \"text\"; }\n",
    )
    .unwrap();

    let output = migrate(&[], dir.path());
    assert!(output.status.success(), "{}", stderr(&output));
    let migrated = std::fs::read_to_string(&path).unwrap();
    assert!(migrated.contains("left: i64;"), "{migrated}");
    assert!(migrated.contains("\"text\""), "{migrated}");
}

#[test]
fn a_refused_file_leaves_every_file_unwritten() {
    let dir = support::tempdir();
    legacy_tree(dir.path());
    let bad = dir.path().join("greeting/bad.hew");
    std::fs::write(&bad, "import std::*;\npub fn broken() {}\n").unwrap();
    let before = [
        "greeting/greeting.hew",
        "greeting/dog.hew",
        "main.hew",
        "greeting/bad.hew",
    ]
    .map(|file| std::fs::read(dir.path().join(file)).unwrap());

    let output = migrate(&[], dir.path());
    assert!(
        !output.status.success(),
        "a removed glob import must refuse"
    );
    assert!(
        stderr(&output).contains("migration refused"),
        "{}",
        stderr(&output)
    );
    let after = [
        "greeting/greeting.hew",
        "greeting/dog.hew",
        "main.hew",
        "greeting/bad.hew",
    ]
    .map(|file| std::fs::read(dir.path().join(file)).unwrap());
    assert_eq!(before, after);
}
