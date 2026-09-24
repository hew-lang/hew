//! End-to-end behaviour of the bare-variant rule in both positions.
//!
//! `E_BARE_VARIANT_EXPR` and `E_BARE_VARIANT_PATTERN` are hard errors from
//! v0.6.0 (issues #3084, A363), including the builtin `Option` and `Result`
//! variants (#3240). The dotted spellings stay legal, and
//! `hew fmt --migrate` must still rewrite a legacy source that `hew check`
//! refuses — otherwise the graduation would leave users with no mechanical
//! way forward.

mod support;

use std::path::Path;

use support::{hew_command, repo_root, run_hew_in, strip_ansi, tempdir};

const BARE: &str = r"enum Choice {
    Present(i64),
    Absent,
}

fn make() -> Choice {
    Absent
}

fn main() {
    match make() {
        .Present(number) => println(number),
        .Absent => println(0),
    }
}
";

const CONTEXTUAL: &str = r"enum Choice {
    Present(i64),
    Absent,
}

fn make() -> Choice {
    .Absent
}

fn main() {
    match make() {
        .Present(number) => println(number),
        .Absent => println(0),
    }
}
";

const QUALIFIED: &str = r"enum Choice {
    Present(i64),
    Absent,
}

fn make() -> Choice {
    Choice.Absent
}

fn main() {
    match make() {
        .Present(number) => println(number),
        .Absent => println(0),
    }
}
";

/// A bare variant in pattern position only — the expression side is written
/// qualified, so a run that reports `E_BARE_VARIANT_EXPR` here would mean the
/// two rules had been wired to the same site.
const BARE_PATTERN: &str = r"enum Choice {
    Present(i64),
    Absent,
}

fn main() {
    match Choice.Absent {
        Present(number) => println(number),
        Absent => println(0),
    }
}
";

/// Both spellings in one source, across every shape the migrator has to
/// rewrite: a contextual expression, a qualified-owner expression, a tuple
/// pattern, a struct-variant pattern, a unit pattern, and a `let`-position tag
/// test.
const MIXED: &str = r"enum Choice {
    Present(i64),
    Absent,
    Named { count: i64 }
}

fn make() -> Choice {
    Absent
}

fn read(value: Choice) -> i64 {
    match value {
        Present(number) => number,
        Named { count } => count,
        Absent => 0,
    }
}

fn tag_test(value: Choice) -> i64 {
    let Absent = value else { return 1 };
    0
}

fn main() {
    println(read(make()));
    println(read(Present(7)));
    println(tag_test(Absent));
}
";

/// `hew_lexer::lex` emits an entire f-string interpolation as one
/// `InterpolatedString` token, so the migrator's flat token scan for the
/// identifier behind a `BareVariantExpr` warning cannot find one whose span
/// falls inside `{...}` here. Before #3243 that token-lookup miss produced a
/// hard refusal that aborted the whole migration.
const FSTRING_INTERPOLATION: &str = r#"enum Choice {
    Present(i64),
    Absent,
}

fn accept(c: Choice) -> i64 {
    match c {
        .Present(n) => n,
        .Absent => 0,
    }
}

fn main() {
    println(f"{accept(Absent)}");
}
"#;

/// The builtin `Option`/`Result` variants and a user unit variant, bare in
/// every expression position: an early `return`, a function tail, an
/// unannotated and an annotated `let`, a call argument, a closure body and a
/// value with no expected type. The match arm values stay dotted so each
/// refusal below has one owner.
const BARE_BUILTINS: &str = r#"enum Colour {
    Red,
    Green,
}

fn half(n: i64) -> Result<i64, string> {
    if n % 2 == 1 {
        return Err("odd");
    }
    Ok(n / 2)
}

fn show(value: Option<i64>) -> i64 {
    value ?? 0
}

fn main() {
    let inferred = Some(1);
    let annotated: Option<i64> = None;
    let passed = show(Some(2));
    let arm = match inferred {
        .Some(n) => n,
        .None => 0,
    };
    let wrap = |n: i64| -> Option<i64> { Some(n) };
    let colour = Red;
    println(f"{show(inferred)} {show(annotated)} {passed} {arm} {show(wrap(3))} {half(4).is_ok()}");
    match colour {
        .Red => println("red"),
        .Green => println("green"),
    }
}
"#;

/// The accepted spellings in the same positions, plus the sites where a
/// dotted tail follows a block (`defer`, a bare block), an actor state
/// assignment, a spawn argument and a machine state selected by its type.
const DOTTED_BUILTINS: &str = r#"machine Door {
    events {
        Open,
        Close,
    }

    state Shut,
    state Ajar,

    on Open: Shut => Ajar,
    on Close: Ajar => Shut,

    default { state }
}

fn half(n: i64) -> Result<i64, string> {
    if n % 2 == 1 {
        return .Err("odd");
    }
    defer {
        println("halved");
    }
    .Ok(n / 2)
}

fn first(values: Vec<i64>) -> Option<i64> {
    {
        println("looking");
    }
    if values.is_empty() {
        return Option.None;
    }
    .Some(values[0])
}

fn show(value: Option<i64>) -> i64 {
    value ?? 0
}

actor Keeper {
    var held: Option<i64>,

    receive fn keep(n: i64) {
        held = .Some(n);
        println(f"kept {show(held)}");
    }
}

fn main() {
    let inferred = Option.Some(1);
    let annotated: Option<i64> = .None;
    let passed = show(.Some(2));
    let arm: Option<i64> = match inferred {
        .Some(n) => .Some(n + 1),
        .None => .None,
    };
    let wrap = |n: i64| -> Option<i64> { .Some(n) };
    let result: Result<i64, string> = Result.Ok(5);
    var door: Door = .Shut;
    let _ = door.step(.Open);
    println(f"{show(inferred)} {show(annotated)} {passed} {show(arm)} {show(wrap(3))}");
    println(f"{half(4).is_ok()} {half(3).is_err()} {result.is_ok()} {show(first([7]))}");
    println(door.state_name());
    let keeper = spawn Keeper(held: .None);
    let _ = keeper.keep(9);
}
"#;

/// A statement block followed by a bare `Ok` tail, as legacy sources wrote it.
const SCOPE_TAIL: &str = r#"fn work() -> Result<i64, string> {
    scope {
        println("in scope");
    }
    Ok(6)
}

fn work2() -> Result<i64, string> {
    unsafe {
        println("in unsafe");
    }
    Ok(7)
}

fn main() {
    match work() {
        .Ok(v) => println(v),
        .Err(m) => println(m),
    }
    match work2() {
        .Ok(v) => println(v),
        .Err(m) => println(m),
    }
}
"#;

fn write_source(dir: &Path, name: &str, source: &str) -> std::path::PathBuf {
    let path = dir.join(name);
    std::fs::write(&path, source).expect("fixture must be writable");
    path
}

fn check(dir: &Path, name: &str, source: &str) -> (bool, String) {
    let path = write_source(dir, name, source);
    let output = run_hew_in(dir, &["check", path.to_str().expect("UTF-8 path")]);
    let rendered = strip_ansi(&format!(
        "{}{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    ));
    (output.status.success(), rendered)
}

#[test]
fn bare_variant_expression_is_rejected() {
    let dir = tempdir();
    let (ok, rendered) = check(dir.path(), "bare.hew", BARE);
    assert!(
        !ok,
        "a bare variant expression must not compile:\n{rendered}"
    );
    assert!(
        rendered.contains("error: E_BARE_VARIANT_EXPR: bare variant `Absent`"),
        "the refusal must keep the E_BARE_VARIANT_EXPR code:\n{rendered}"
    );
    assert!(
        rendered.contains(
            "use `.Absent` when the surrounding type selects the enum, or qualify the variant with its type"
        ),
        "the refusal must keep the migration help text:\n{rendered}"
    );
}

#[test]
fn contextual_variant_expression_is_accepted() {
    let dir = tempdir();
    let (ok, rendered) = check(dir.path(), "contextual.hew", CONTEXTUAL);
    assert!(ok, "`.Variant` must still compile:\n{rendered}");
    assert!(
        !rendered.contains("E_BARE_VARIANT_EXPR"),
        "`.Variant` must not report the bare-variant rule:\n{rendered}"
    );
}

#[test]
fn qualified_variant_expression_is_accepted() {
    let dir = tempdir();
    let (ok, rendered) = check(dir.path(), "qualified.hew", QUALIFIED);
    assert!(ok, "`Type.Variant` must still compile:\n{rendered}");
    assert!(
        !rendered.contains("E_BARE_VARIANT_EXPR"),
        "`Type.Variant` must not report the bare-variant rule:\n{rendered}"
    );
}

/// The pattern form refuses on the same footing as the expression form. The
/// negative control is the severity: a run of `hew check` may not report this
/// rule as a warning any more, which is what the pre-0.6.0 deprecation did.
#[test]
fn bare_variant_pattern_is_rejected() {
    let dir = tempdir();
    let (ok, rendered) = check(dir.path(), "pattern.hew", BARE_PATTERN);
    assert!(!ok, "a bare variant pattern must not compile:\n{rendered}");
    assert!(
        rendered.contains("error: E_BARE_VARIANT_PATTERN: bare variant pattern `Present`"),
        "the refusal must keep the E_BARE_VARIANT_PATTERN code:\n{rendered}"
    );
    assert!(
        rendered.contains("replace `Present` with `.Present`"),
        "the refusal must keep its machine-applicable fix-it:\n{rendered}"
    );
    assert!(
        !rendered.contains("warning: E_BARE_VARIANT_PATTERN"),
        "the deprecation path must be gone, not merely outranked:\n{rendered}"
    );
    assert!(
        !rendered.contains("E_BARE_VARIANT_EXPR"),
        "the expression rule must not fire on a pattern:\n{rendered}"
    );
}

/// The migrator is the sanctioned way past the new error, so it must still
/// resolve and rewrite bare variants in a source `hew check` refuses.
#[test]
fn migrate_rewrites_a_source_check_now_rejects() {
    let dir = tempdir();
    let path = write_source(dir.path(), "legacy.hew", BARE);
    let source = path.to_str().expect("UTF-8 path").to_string();

    let refusal = run_hew_in(dir.path(), &["check", &source]);
    assert!(
        !refusal.status.success(),
        "the legacy source must be refused before migration"
    );

    let migrated = run_hew_in(dir.path(), &["fmt", "--migrate", &source]);
    assert!(
        migrated.status.success(),
        "migration must succeed on a bare-variant source:\n{}",
        strip_ansi(&String::from_utf8_lossy(&migrated.stderr))
    );

    let rewritten = std::fs::read_to_string(&path).expect("migrated source must be readable");
    assert!(
        rewritten.contains(".Absent\n"),
        "migration must rewrite the bare variant to its contextual form:\n{rewritten}"
    );

    let recheck = run_hew_in(dir.path(), &["check", &source]);
    assert!(
        recheck.status.success(),
        "the migrated source must type-check:\n{}",
        strip_ansi(&String::from_utf8_lossy(&recheck.stderr))
    );
}

/// One migration pass must clear both spellings in every shape, and a second
/// pass over its own output must change nothing — otherwise `hew fmt
/// --migrate` could not be run over a tree twice, or in CI as a check.
#[test]
fn migrate_rewrites_both_spellings_and_is_idempotent() {
    let dir = tempdir();
    let path = write_source(dir.path(), "mixed.hew", MIXED);
    let source = path.to_str().expect("UTF-8 path").to_string();

    let refusal = run_hew_in(dir.path(), &["check", &source]);
    let refusal_text = strip_ansi(&String::from_utf8_lossy(&refusal.stderr));
    assert!(
        !refusal.status.success(),
        "the mixed source must be refused before migration"
    );
    assert!(
        refusal_text.contains("E_BARE_VARIANT_EXPR")
            && refusal_text.contains("E_BARE_VARIANT_PATTERN"),
        "the fixture must exercise both rules before migration:\n{refusal_text}"
    );

    let first = run_hew_in(dir.path(), &["fmt", "--migrate", &source]);
    assert!(
        first.status.success(),
        "migration must succeed on a mixed source:\n{}",
        strip_ansi(&String::from_utf8_lossy(&first.stderr))
    );
    let once = std::fs::read_to_string(&path).expect("migrated source must be readable");

    for expected in [
        "    .Absent\n",
        ".Present(number) => number,",
        ".Named { count } => count,",
        ".Absent => 0,",
        "let .Absent = value else",
        "read(.Present(7))",
        "tag_test(.Absent)",
    ] {
        assert!(
            once.contains(expected),
            "migration must rewrite `{expected}`:\n{once}"
        );
    }

    let recheck = run_hew_in(dir.path(), &["check", &source]);
    assert!(
        recheck.status.success(),
        "the migrated source must type-check:\n{}",
        strip_ansi(&String::from_utf8_lossy(&recheck.stderr))
    );

    let second = run_hew_in(dir.path(), &["fmt", "--migrate", &source]);
    assert!(
        second.status.success(),
        "a second migration pass must succeed:\n{}",
        strip_ansi(&String::from_utf8_lossy(&second.stderr))
    );
    let twice = std::fs::read_to_string(&path).expect("migrated source must be readable");
    assert_eq!(
        once, twice,
        "migration must be idempotent; the second pass changed the source"
    );

    let migrate_check = run_hew_in(dir.path(), &["fmt", "--migrate", "--check", &source]);
    assert!(
        migrate_check.status.success(),
        "`fmt --migrate --check` must be clean on an already-migrated source:\n{}",
        strip_ansi(&String::from_utf8_lossy(&migrate_check.stderr))
    );
}

/// A bare variant inside an f-string interpolation is not rewritten yet
/// (#3243). The migrator names the skip, and because the output would still
/// be refused it reports failure and leaves the file untouched instead of
/// claiming success.
#[test]
fn migrate_refuses_when_an_fstring_interpolation_keeps_a_bare_variant() {
    let dir = tempdir();
    let path = write_source(dir.path(), "fstring.hew", FSTRING_INTERPOLATION);
    let source = path.to_str().expect("UTF-8 path").to_string();

    let migrated = run_hew_in(dir.path(), &["fmt", "--migrate", &source]);
    let stderr = strip_ansi(&String::from_utf8_lossy(&migrated.stderr));
    assert!(
        !migrated.status.success(),
        "migration must not report success on output that does not check:\n{stderr}"
    );
    assert!(
        stderr.contains("skipping bare-variant migration inside an f-string interpolation"),
        "migration must name the f-string skip:\n{stderr}"
    );
    assert!(
        stderr.contains("the migrated source does not type-check"),
        "migration must name the failed re-check:\n{stderr}"
    );
    assert_eq!(
        std::fs::read_to_string(&path).expect("source must be readable"),
        FSTRING_INTERPOLATION,
        "a refused migration must not write"
    );
}

/// A legacy `scope { }` statement followed by a bare `Ok(x)` tail migrates to
/// a `.Ok(x)` tail that stays its own expression, and the result runs.
#[test]
fn migrate_keeps_a_dotted_tail_after_a_statement_block() {
    let dir = tempdir();
    let path = write_source(dir.path(), "scope_tail.hew", SCOPE_TAIL);
    let source = path.to_str().expect("UTF-8 path").to_string();

    let migrated = run_hew_in(dir.path(), &["fmt", "--migrate", &source]);
    assert!(
        migrated.status.success(),
        "migration must succeed:\n{}",
        strip_ansi(&String::from_utf8_lossy(&migrated.stderr))
    );
    let rewritten = std::fs::read_to_string(&path).expect("migrated source must be readable");
    assert!(
        rewritten.contains("    };\n    .Ok(6)\n") && rewritten.contains("    };\n    .Ok(7)\n"),
        "the tails must stay separate expressions:\n{rewritten}"
    );
    let run = run_hew_in(dir.path(), &["run", &source]);
    assert_eq!(
        String::from_utf8_lossy(&run.stdout),
        "in scope\n6\nin unsafe\n7\n",
        "{}",
        strip_ansi(&String::from_utf8_lossy(&run.stderr))
    );
}

fn copy_tree(from: &Path, to: &Path) {
    std::fs::create_dir_all(to).expect("create std copy directory");
    for entry in std::fs::read_dir(from).expect("read std directory") {
        let entry = entry.expect("read std entry");
        let target = to.join(entry.file_name());
        if entry.file_type().expect("std entry type").is_dir() {
            copy_tree(&entry.path(), &target);
        } else {
            std::fs::copy(entry.path(), &target).expect("copy std file");
        }
    }
}

/// The prelude's `Option` impls resolve from the std root the rest of std
/// uses, so `HEW_STD` selects them too, and a defect in that copy is reported
/// against the std file that holds it. The control is the user file: nothing
/// may be reported there at an offset from a foreign source.
#[test]
fn prelude_impl_diagnostics_name_the_selected_std_file() {
    let dir = tempdir();
    let std_copy = dir.path().join("std");
    copy_tree(&repo_root().join("std"), &std_copy);
    let option = std_copy.join("option.hew");
    let source = std::fs::read_to_string(&option).expect("read std option");
    assert!(source.contains("self = .None;"), "fixture anchor moved");
    std::fs::write(&option, source.replace("self = .None;", "self = None;"))
        .expect("break std option copy");
    let user = write_source(dir.path(), "user.hew", "fn main() {\n    println(1);\n}\n");

    let output = hew_command()
        .args(["check", user.to_str().expect("UTF-8 path")])
        .current_dir(dir.path())
        .env("HEW_STD", &std_copy)
        .output()
        .expect("hew check must run");
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        !output.status.success(),
        "the broken std copy must refuse:\n{stderr}"
    );
    assert!(
        stderr.contains(&format!(
            "{}:213:16: error: E_BARE_VARIANT_EXPR",
            std::fs::canonicalize(&option)
                .expect("canonical std option path")
                .display()
        )),
        "the refusal must name the std file it came from:\n{stderr}"
    );
    assert!(
        !stderr.contains("user.hew:"),
        "nothing belongs to the user's file:\n{stderr}"
    );
}

/// A machine names its states bare inside its own declaration; everywhere
/// else a state follows the variant rule (D550), with the same fix-its.
#[test]
fn bare_machine_state_outside_its_machine_is_rejected() {
    const MACHINE: &str = r"machine Door {
    events {
        Open,
    }

    state Shut,
    state Ajar,

    on Open: Shut => Ajar,

    default { state }
}

fn main() {
    var first = Shut;
    let _ = first.step(.Open);
    let second: Door = Ajar;
    println(first.state_name());
    println(second.state_name());
}
";
    let dir = tempdir();
    let (ok, rendered) = check(dir.path(), "door.hew", MACHINE);
    assert!(
        !ok,
        "bare states outside the machine must not compile:\n{rendered}"
    );
    for (site, fix) in [
        ("door.hew:15:17", "replace `Shut` with `Door.Shut`"),
        ("door.hew:17:24", "replace `Ajar` with `.Ajar`"),
    ] {
        let at = rendered
            .find(&format!("{site}: error: E_BARE_VARIANT_EXPR"))
            .unwrap_or_else(|| panic!("missing refusal at {site}:\n{rendered}"));
        assert!(
            rendered[at..]
                .lines()
                .find(|line| line.trim_start().starts_with("= help:"))
                .is_some_and(|help| help.contains(fix)),
            "the refusal at {site} must offer `{fix}`:\n{rendered}"
        );
    }
    assert_eq!(
        rendered.matches("error: E_BARE_VARIANT_EXPR").count(),
        2,
        "the bare `Shut` and `Ajar` inside the machine are not refused:\n{rendered}"
    );
}
