//! End-to-end behaviour of the bare-variant rule in both positions.
//!
//! `E_BARE_VARIANT_EXPR` and `E_BARE_VARIANT_PATTERN` are hard errors from
//! v0.6.0 (issues #3084, A363). The dotted spellings stay legal, and
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
