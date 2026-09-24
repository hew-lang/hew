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

use support::{run_hew_in, strip_ansi, tempdir};

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
/// assignment, a spawn argument and a machine state named bare, which is not
/// a variant at the surface.
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
    var door = Shut;
    let _ = door.step(.Open);
    println(f"{show(inferred)} {show(annotated)} {passed} {show(arm)} {show(wrap(3))}");
    println(f"{half(4).is_ok()} {half(3).is_err()} {result.is_ok()} {show(first([7]))}");
    println(door.state_name());
    let keeper = spawn Keeper(held: .None);
    let _ = keeper.keep(9);
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

/// A bare-variant warning whose span falls inside an f-string interpolation
/// must be skipped, not treated as a hard refusal that aborts the whole
/// file's migration (#3243).
#[test]
fn migrate_skips_bare_variant_inside_fstring_interpolation_without_aborting() {
    let dir = tempdir();
    let path = write_source(dir.path(), "fstring.hew", FSTRING_INTERPOLATION);
    let source = path.to_str().expect("UTF-8 path").to_string();

    let migrated = run_hew_in(dir.path(), &["fmt", "--migrate", &source]);
    let stderr = strip_ansi(&String::from_utf8_lossy(&migrated.stderr));
    assert!(
        migrated.status.success(),
        "migration must not abort on a bare variant inside an f-string interpolation:\n{stderr}"
    );
    assert!(
        stderr.contains("skipping bare-variant migration inside an f-string interpolation"),
        "migration must report the f-string skip, not a hard token-lookup refusal:\n{stderr}"
    );
    assert!(
        !stderr.contains("checker-selected variant has no identifier token"),
        "the f-string case must not fall through to the generic hard refusal:\n{stderr}"
    );

    let rewritten = std::fs::read_to_string(&path).expect("migrated source must be readable");
    assert!(
        rewritten.contains("f\"{accept(Absent)}\""),
        "the bare variant inside the f-string interpolation must be left \
         unrewritten (skip-not-abort, not a silent rewrite):\n{rewritten}"
    );
}

/// Every bare builtin variant is refused on the same footing as a user
/// variant, and the fix-it names the contextual form where the expected type
/// selects the enum and the qualified form where nothing does.
#[test]
fn bare_builtin_variants_are_rejected_in_every_expression_position() {
    let dir = tempdir();
    let (ok, rendered) = check(dir.path(), "bare_builtins.hew", BARE_BUILTINS);
    assert!(!ok, "bare builtin variants must not compile:\n{rendered}");
    for (site, fix) in [
        ("bare_builtins.hew:8:16", "replace `Err` with `.Err`"),
        ("bare_builtins.hew:10:5", "replace `Ok` with `.Ok`"),
        (
            "bare_builtins.hew:18:20",
            "replace `Some` with `Option.Some`",
        ),
        ("bare_builtins.hew:19:34", "replace `None` with `.None`"),
        ("bare_builtins.hew:20:23", "replace `Some` with `.Some`"),
        ("bare_builtins.hew:25:42", "replace `Some` with `.Some`"),
        ("bare_builtins.hew:26:18", "replace `Red` with `Colour.Red`"),
    ] {
        let at = rendered
            .find(&format!("{site}: error: E_BARE_VARIANT_EXPR"))
            .unwrap_or_else(|| panic!("missing refusal at {site}:\n{rendered}"));
        let help = rendered[at..]
            .lines()
            .find(|line| line.trim_start().starts_with("= help:"))
            .unwrap_or_default();
        assert!(
            help.contains(fix),
            "the refusal at {site} must offer `{fix}`, got `{help}`"
        );
    }
    assert_eq!(
        rendered.matches("error: E_BARE_VARIANT_EXPR").count(),
        7,
        "only the seven bare sites may be refused:\n{rendered}"
    );
}

/// The dotted and qualified spellings compile and run in the same positions.
/// A machine state named bare is the negative control: states are not
/// variants at the surface, so the rule must not reach them.
#[test]
fn dotted_builtin_variants_run_in_every_expression_position() {
    let dir = tempdir();
    let path = write_source(dir.path(), "dotted_builtins.hew", DOTTED_BUILTINS);
    let output = run_hew_in(dir.path(), &["run", path.to_str().expect("UTF-8 path")]);
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        output.status.success(),
        "dotted builtin variants must compile and run:\n{stderr}"
    );
    assert!(
        !stderr.contains("E_BARE_VARIANT_EXPR"),
        "no dotted spelling or machine state may report the rule:\n{stderr}"
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "1 0 2 2 3\nhalved\nlooking\ntrue true true 7\nAjar\nkept 9\n"
    );
}

/// The migrator rewrites every bare builtin to the checker's fix-it, the
/// result type-checks, and a second pass changes nothing.
#[test]
fn migrate_rewrites_bare_builtin_variants_and_is_idempotent() {
    let dir = tempdir();
    let path = write_source(dir.path(), "bare_builtins.hew", BARE_BUILTINS);
    let source = path.to_str().expect("UTF-8 path").to_string();

    let first = run_hew_in(dir.path(), &["fmt", "--migrate", &source]);
    assert!(
        first.status.success(),
        "migration must succeed on bare builtin variants:\n{}",
        strip_ansi(&String::from_utf8_lossy(&first.stderr))
    );
    let once = std::fs::read_to_string(&path).expect("migrated source must be readable");
    for expected in [
        "return .Err(\"odd\");",
        "    .Ok(n / 2)\n",
        "let inferred = Option.Some(1);",
        "let annotated: Option<i64> = .None;",
        "show(.Some(2))",
        "-> Option<i64> {\n        .Some(n)\n    };",
        "let colour = Colour.Red;",
    ] {
        assert!(
            once.contains(expected),
            "migration must produce `{expected}`:\n{once}"
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
        "a second migration pass must succeed"
    );
    let twice = std::fs::read_to_string(&path).expect("migrated source must be readable");
    assert_eq!(once, twice, "migration must be idempotent");
}
