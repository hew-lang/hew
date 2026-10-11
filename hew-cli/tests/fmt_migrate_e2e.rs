//! `hew fmt --migrate` rewrites retired spellings: punctuation and paths
//! syntactically, bare variants by the type the checker says their context
//! expects. Type errors never block it, a file that does not parse is
//! skipped, and any other refusal leaves every file as it was.
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
    // A directory module belongs to a package (HEW-SPEC-2026 §3.5.1).
    std::fs::write(dir.join("hew.toml"), "[package]\nname = \"app\"\n").unwrap();
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

/// A file the migrator cannot read is named and left as it is; the rest of
/// the tree still migrates, and the run reports the failure.
#[test]
fn a_file_that_does_not_parse_is_skipped_and_the_rest_migrate() {
    let dir = support::tempdir();
    legacy_tree(dir.path());
    let bad = dir.path().join("greeting/bad.hew");
    std::fs::write(&bad, "import std::*;\npub fn broken() {}\n").unwrap();
    let before = std::fs::read(&bad).unwrap();

    let output = migrate(&[], dir.path());
    assert!(
        !output.status.success(),
        "a skipped file fails the run: {}",
        stderr(&output)
    );
    assert!(
        stderr(&output).contains("migration skipped")
            && slashes(&stderr(&output))
                .contains(&format!("{}:1:", slashes(&bad.display().to_string())))
            && stderr(&output).contains("1 skipped"),
        "a skip names the file, line and column: {}",
        stderr(&output)
    );
    assert_eq!(std::fs::read(&bad).unwrap(), before);
    let main = std::fs::read_to_string(dir.path().join("main.hew")).unwrap();
    assert!(main.contains("import greeting.{empty_labels};"), "{main}");
}

/// Compare paths the way the report prints them on every host.
fn slashes(text: &str) -> String {
    text.replace('\\', "/")
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
    let report = slashes(&stderr(&preview));
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

/// A bare variant whose respelling cannot check refuses the migration: the
/// inference failure at `first(None)` is a consequence of the bare site, so
/// it must not survive as `first(.None)`.
#[test]
fn a_respelling_that_still_fails_at_its_site_refuses() {
    let dir = support::tempdir();
    let path = dir.path().join("generic.hew");
    let source = concat!(
        "fn first<T>(o: Option<T>) -> bool {\n",
        "    match o { .Some(_) => true, .None => false }\n",
        "}\n",
        "fn main() { println(first(None)); }\n",
    );
    std::fs::write(&path, source).unwrap();

    let output = migrate(&[], dir.path());
    assert!(!output.status.success(), "{}", stderr(&output));
    let reported = stderr(&output);
    assert!(
        reported.contains("migration refused")
            && reported.contains("generic.hew:")
            && reported.contains("InferenceFailed"),
        "{reported}"
    );
    assert_eq!(std::fs::read_to_string(&path).unwrap(), source);
}

/// An error inside a bare variant's payload is not the site's own: it
/// survives the respelling unchanged and does not block the migration.
#[test]
fn an_error_inside_a_payload_still_migrates() {
    let dir = support::tempdir();
    let path = dir.path().join("payload.hew");
    std::fs::write(
        &path,
        "fn wrap() -> Option<i64> { Some(missing) }\nfn main() {}\n",
    )
    .unwrap();

    let output = migrate(&[], dir.path());
    assert!(output.status.success(), "{}", stderr(&output));
    let migrated = std::fs::read_to_string(&path).unwrap();
    assert!(migrated.contains(".Some(missing)"), "{migrated}");
}

/// A callable that fails through `?` or `return error`, or whose tail relied
/// on the retired success wrapping, moves to its failure edge with its exits;
/// a `-> Result` value return is left alone, and a second pass changes
/// nothing (D578).
#[test]
fn migrate_moves_failing_callables_to_their_edge() {
    let dir = support::tempdir();
    let path = dir.path().join("edges.hew");
    std::fs::write(
        &path,
        concat!(
            "fn parse(text: string) -> Result<i64, string> {\n",
            "    if text == \"\" {\n",
            "        return .Err(\"empty\");\n",
            "    }\n",
            "    .Ok(1)\n",
            "}\n",
            "\n",
            "fn twice(text: string) -> Result<i64, string> {\n",
            "    let value = parse(text)?;\n",
            "    if value > 9 {\n",
            "        return .Err(\"too big\");\n",
            "    }\n",
            "    .Ok(value * 2)\n",
            "}\n",
            "\n",
            "fn check(text: string) -> Result<(), string> {\n",
            "    let _ = parse(text)?;\n",
            "    if text == \"skip\" {\n",
            "        return .Ok(());\n",
            "    }\n",
            "    .Ok(())\n",
            "}\n",
            "\n",
            "fn forward(text: string) -> Result<i64, string> {\n",
            "    let value = twice(text)?;\n",
            "    match value {\n",
            "        0 => parse(text),\n",
            "        _ => .Ok(value),\n",
            "    }\n",
            "}\n",
            "\n",
            "fn total(items: Vec<string>) -> Result<i64, string> {\n",
            "    let parsed = items.map(|text| {\n",
            "        let value = parse(text)?;\n",
            "        .Ok(value + 1)\n",
            "    });\n",
            "    var sum = 0;\n",
            "    for item in parsed {\n",
            "        sum = sum + item?;\n",
            "    }\n",
            "    .Ok(sum)\n",
            "}\n",
            "\n",
            "actor Store {\n",
            "    receive fn load(text: string) -> Result<i64, string> {\n",
            "        let value = parse(text)?;\n",
            "        .Ok(value)\n",
            "    }\n",
            "}\n",
            "\n",
            "fn main() {}\n",
        ),
    )
    .unwrap();

    let output = migrate(&[], dir.path());
    assert!(output.status.success(), "{}", stderr(&output));
    let migrated = std::fs::read_to_string(&path).unwrap();
    for spelling in [
        // A value return without a failure exit keeps its form.
        "fn parse(text: string) -> Result<i64, string> {",
        "        return .Err(\"empty\");\n",
        "fn twice(text: string) -> i64 fails string {",
        "        return error \"too big\";\n",
        "    value * 2\n",
        "fn check(text: string) fails string {",
        "        return;\n",
        "fn forward(text: string) -> i64 fails string {",
        "        0 => parse(text)?,\n",
        "        _ => value,\n",
        "fn total(items: Vec<string>) -> i64 fails string {",
        "        value + 1\n",
        "    sum\n",
        "receive fn load(text: string) -> Result<i64, string> {",
    ] {
        assert!(
            migrated.contains(spelling),
            "missing `{spelling}`:\n{migrated}"
        );
    }
    assert!(!migrated.contains(".Ok(())"), "{migrated}");

    let again = migrate(&["--check"], dir.path());
    assert!(again.status.success(), "{}", stderr(&again));
}

fn write_and_migrate(dir: &Path, name: &str, source: &str) -> (Output, String) {
    let path = dir.join(name);
    std::fs::write(&path, source).unwrap();
    let output = migrate(&[], dir);
    let migrated = std::fs::read_to_string(&path).unwrap();
    (output, migrated)
}

fn assert_runs(dir: &Path, name: &str, stdout: &str) {
    let run = Command::new(hew_binary())
        .arg("run")
        .arg(dir.join(name))
        .output()
        .unwrap();
    assert!(run.status.success(), "{}", stderr(&run));
    assert_eq!(String::from_utf8_lossy(&run.stdout), stdout);
}

/// An exit nested in another rewritten exit is rewritten inside it, and a
/// qualified `Result.Ok`/`Result.Err` moves like the contextual spelling.
#[test]
fn nested_and_qualified_exits_move_to_the_edge() {
    let dir = support::tempdir();
    let (output, migrated) = write_and_migrate(
        dir.path(),
        "nested.hew",
        concat!(
            "fn parse(t: string) -> i64 fails string {\n",
            "    if t == \"\" { return error \"empty\"; }\n",
            "    7\n",
            "}\n",
            "fn pick(t: string, k: i64) -> Result<i64, string> {\n",
            "    let base = parse(t)?;\n",
            "    .Ok(match k {\n",
            "        0 => return .Err(\"zero\"),\n",
            "        n => n + base,\n",
            "    })\n",
            "}\n",
            "fn qualified(t: string) -> Result<i64, string> {\n",
            "    let n = parse(t)?;\n",
            "    if n > 5 { return Result.Err(\"big\"); }\n",
            "    Result.Ok(n)\n",
            "}\n",
            "fn main() {\n",
            "    println(f\"{pick(\"a\", 0):?} {pick(\"a\", 2):?} {qualified(\"a\"):?}\");\n",
            "}\n",
        ),
    );
    assert!(output.status.success(), "{}", stderr(&output));
    for spelling in [
        "fn pick(t: string, k: i64) -> i64 fails string {",
        "        0 => return error \"zero\",\n",
        "fn qualified(t: string) -> i64 fails string {",
        "        return error \"big\";\n",
        "    n\n",
    ] {
        assert!(
            migrated.contains(spelling),
            "missing `{spelling}`:\n{migrated}"
        );
    }
    assert_runs(dir.path(), "nested.hew", "Err(zero) Ok(9) Err(big)\n");
}

/// A unit `Ok` that was an `else` branch's whole value leaves with its
/// `else`, an `Err` tail becomes a `return error` statement, a `?` on an
/// `Option` is no failure exit, and a function-typed success keeps its
/// parentheses.
#[test]
fn edge_rewrites_keep_the_program_well_formed() {
    let dir = support::tempdir();
    let (output, migrated) = write_and_migrate(
        dir.path(),
        "shapes.hew",
        concat!(
            "fn parse(t: string) -> i64 fails string {\n",
            "    if t == \"\" { return error \"empty\"; }\n",
            "    3\n",
            "}\n",
            "fn send(t: string, status: i64) -> Result<(), string> {\n",
            "    let d = parse(t)?;\n",
            "    if status == d {\n",
            "        .Err(\"absent\")\n",
            "    } else {\n",
            "        .Ok(())\n",
            "    }\n",
            "}\n",
            "fn positive(v: i64) -> Option<i64> { if v > 0 { .Some(v) } else { .None } }\n",
            "fn bump(xs: Vec<Option<i64>>) -> Vec<Option<i64>> {\n",
            "    xs.map(|o| positive(o? + 1))\n",
            "}\n",
            "fn scale(t: string) -> Result<fn(i64) -> i64, string> {\n",
            "    let k = parse(t)?;\n",
            "    .Ok(|n: i64| n * k)\n",
            "}\n",
            "fn main() {\n",
            "    println(f\"{send(\"a\", 3):?} {send(\"a\", 1):?}\");\n",
            "    println(f\"{bump([.Some(1), .None]):?}\");\n",
            "    match scale(\"a\") {\n",
            "        .Ok(f) => println(f\"{f(2)}\"),\n",
            "        .Err(e) => println(e),\n",
            "    }\n",
            "}\n",
        ),
    );
    assert!(output.status.success(), "{}", stderr(&output));
    for spelling in [
        "fn send(t: string, status: i64) fails string {",
        "        return error \"absent\";\n    }\n}",
        "    xs.map(|o| positive(o? + 1))\n",
        "fn scale(t: string) -> (fn(i64) -> i64) fails string {",
    ] {
        assert!(
            migrated.contains(spelling),
            "missing `{spelling}`:\n{migrated}"
        );
    }
    assert_runs(
        dir.path(),
        "shapes.hew",
        "Err(absent) Ok(())\n[Some(2), None]\n6\n",
    );
}

#[test]
fn a_migrated_handler_preserves_its_reply_envelope() {
    let dir = support::tempdir();
    std::fs::write(
        dir.path().join("store.hew"),
        concat!(
            "fn parse(t: string) -> i64 fails string {\n",
            "    if t == \"\" { return error \"empty\"; }\n",
            "    7\n",
            "}\n",
            "pub actor Store {\n",
            "    receive fn load(text: string) -> Result<i64, string> {\n",
            "        let value = parse(text)?;\n",
            "        .Ok(value)\n",
            "    }\n",
            "}\n",
        ),
    )
    .unwrap();
    std::fs::write(
        dir.path().join("main.hew"),
        concat!(
            "import store;\n",
            "fn main() {\n",
            "    let s = spawn store.Store();\n",
            "    match s.load(\"x\") {\n",
            "        .Ok(.Ok(v)) => println(f\"ok {v}\"),\n",
            "        .Ok(.Err(e)) => println(f\"err {e}\"),\n",
            "        .Err(_) => println(\"ask failed\"),\n",
            "    }\n",
            "}\n",
        ),
    )
    .unwrap();
    let output = migrate(&[], dir.path());
    assert!(output.status.success(), "{}", stderr(&output));
    assert_runs(dir.path(), "main.hew", "ok 7\n");
}
