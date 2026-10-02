//! `hew fmt --migrate` rewrites retired spellings: punctuation and paths
//! syntactically, bare variants by the type the checker says their context
//! expects. Type errors never block it, and any refusal leaves every file as
//! it was.
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
        stderr(&output).contains("migration refused")
            && stderr(&output).contains(&format!("{}:1:", bad.display())),
        "a refusal names the file, line and column: {}",
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

fn snapshot(dir: &Path, files: &[&str]) -> Vec<Vec<u8>> {
    files
        .iter()
        .map(|file| std::fs::read(dir.join(file)).unwrap())
        .collect()
}

const TREE: [&str; 3] = ["greeting/greeting.hew", "greeting/dog.hew", "main.hew"];

#[test]
fn preview_reports_every_change_and_writes_nothing() {
    let dir = support::tempdir();
    legacy_tree(dir.path());
    let before = snapshot(dir.path(), &TREE);

    let preview = migrate(&["--check"], dir.path());
    assert_eq!(preview.status.code(), Some(1), "{}", stderr(&preview));
    assert_eq!(snapshot(dir.path(), &TREE), before);
    let report = stderr(&preview);
    for file in TREE {
        assert!(report.contains(file), "{report}");
    }
    assert!(
        report.contains("migration: 3 to migrate, 0 unchanged, 0 excluded"),
        "{report}"
    );
}

#[test]
fn excluded_paths_are_left_as_they_are() {
    let dir = support::tempdir();
    legacy_tree(dir.path());
    let before = snapshot(dir.path(), &TREE);

    let output = migrate(
        &[
            "--exclude",
            &dir.path().join("greeting").display().to_string(),
        ],
        dir.path(),
    );
    assert!(output.status.success(), "{}", stderr(&output));
    let after = snapshot(dir.path(), &TREE);
    assert_eq!(
        after[..2],
        before[..2],
        "the excluded module must not change"
    );
    assert_ne!(after[2], before[2], "main.hew must migrate");
    assert!(
        stderr(&output).contains("migration: 1 migrated, 0 unchanged, 2 excluded"),
        "{}",
        stderr(&output)
    );
}

/// A write that fails part-way names what was migrated and what was not, and
/// the rerun completes the rest.
#[cfg(unix)]
#[test]
fn an_io_failure_names_written_and_unwritten_files() {
    use std::os::unix::fs::PermissionsExt;

    let dir = support::tempdir();
    let first = dir.path().join("a");
    let locked = dir.path().join("b");
    std::fs::create_dir(&first).unwrap();
    std::fs::create_dir(&locked).unwrap();
    std::fs::write(first.join("one.hew"), "type One { value: i64, }\n").unwrap();
    std::fs::write(locked.join("two.hew"), "type Two { value: i64, }\n").unwrap();
    std::fs::set_permissions(&locked, std::fs::Permissions::from_mode(0o555)).unwrap();
    if std::fs::write(locked.join("probe"), "").is_ok() {
        // Running with privileges that ignore directory permissions.
        std::fs::set_permissions(&locked, std::fs::Permissions::from_mode(0o755)).unwrap();
        return;
    }

    let failed = migrate(&[], dir.path());
    std::fs::set_permissions(&locked, std::fs::Permissions::from_mode(0o755)).unwrap();
    assert!(!failed.status.success());
    let report = stderr(&failed);
    assert!(
        report.contains("migrated before the failure:") && report.contains("one.hew"),
        "{report}"
    );
    assert!(
        report.contains("not migrated:") && report.contains("two.hew"),
        "{report}"
    );
    assert!(std::fs::read_to_string(first.join("one.hew"))
        .unwrap()
        .contains("value: i64;"));
    assert!(std::fs::read_to_string(locked.join("two.hew"))
        .unwrap()
        .contains("value: i64,"));

    let rerun = migrate(&[], dir.path());
    assert!(rerun.status.success(), "{}", stderr(&rerun));
    assert!(std::fs::read_to_string(locked.join("two.hew"))
        .unwrap()
        .contains("value: i64;"));
}

#[cfg(unix)]
#[test]
fn a_migrated_file_keeps_its_permissions() {
    use std::os::unix::fs::PermissionsExt;

    let dir = support::tempdir();
    let path = dir.path().join("script.hew");
    std::fs::write(&path, "type One { value: i64, }\n").unwrap();
    std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o640)).unwrap();

    let output = migrate(&[], dir.path());
    assert!(output.status.success(), "{}", stderr(&output));
    let mode = std::fs::metadata(&path).unwrap().permissions().mode() & 0o777;
    assert_eq!(mode, 0o640);
}

/// Bare variants respell by the type their context expects: `.Ok` where the
/// return type selects `Result`, `Option.None` and `Colour.Red` where nothing
/// does. An unrelated type error neither blocks the pass nor changes, and a
/// directory-module peer respells against its module.
#[test]
fn migrate_respells_bare_variants_by_their_context() {
    let dir = support::tempdir();
    let path = dir.path().join("variants.hew");
    std::fs::write(
        &path,
        concat!(
            "enum Colour { Red; Green; }\n",
            "fn pick(n: i64) -> Result<i64, string> {\n",
            "    if n < 0 { return Err(\"neg\"); }\n",
            "    let none = None;\n",
            "    let colour = Red;\n",
            "    let typed: Colour = Green;\n",
            "    let wrong: i64 = \"text\";\n",
            "    Ok(n)\n",
            "}\n",
            "fn main() {}\n",
        ),
    )
    .unwrap();
    let module = dir.path().join("shapes");
    std::fs::create_dir(&module).unwrap();
    std::fs::write(
        module.join("shapes.hew"),
        "pub fn first(values: Vec<i64>) -> Option<i64> { if values.is_empty() { None } else { Some(values[0]) } }\n",
    )
    .unwrap();
    std::fs::write(
        module.join("extra.hew"),
        "pub fn wrap(value: i64) -> Option<i64> { Some(value) }\n",
    )
    .unwrap();

    let output = migrate(&[], dir.path());
    assert!(output.status.success(), "{}", stderr(&output));
    let migrated = std::fs::read_to_string(&path).unwrap();
    for spelling in [
        "return .Err(\"neg\");",
        "let none = Option.None;",
        "let colour = Colour.Red;",
        "let typed: Colour = .Green;",
        "let wrong: i64 = \"text\";",
        "    .Ok(n)\n",
    ] {
        assert!(
            migrated.contains(spelling),
            "missing `{spelling}`:\n{migrated}"
        );
    }
    let entry = std::fs::read_to_string(module.join("shapes.hew")).unwrap();
    let peer = std::fs::read_to_string(module.join("extra.hew")).unwrap();
    assert!(
        entry.contains(".None") && entry.contains(".Some(values[0])"),
        "{entry}"
    );
    assert!(peer.contains(".Some(value)"), "{peer}");

    let again = migrate(&["--check"], dir.path());
    assert!(again.status.success(), "{}", stderr(&again));
}
