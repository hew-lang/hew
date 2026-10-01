//! End-to-end behaviour of the bare-variant rule in both positions.
//!
//! `E_BARE_VARIANT_EXPR` and `E_BARE_VARIANT_PATTERN` are hard errors from
//! v0.6.0 (issues #3084, A363), including the builtin `Option` and `Result`
//! variants (#3240). The dotted spellings stay legal.

mod support;

use std::path::Path;

use support::{hew_command, repo_root, run_hew_in, strip_ansi, tempdir};

const BARE: &str = r"enum Choice {
    Present(i64);
    Absent;
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
    Present(i64);
    Absent;
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
    Present(i64);
    Absent;
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
    Present(i64);
    Absent;
}

fn main() {
    match Choice.Absent {
        Present(number) => println(number),
        Absent => println(0),
    }
}
";

/// The builtin `Option`/`Result` variants and a user unit variant, bare in
/// every expression position: an early `return`, a function tail, an
/// unannotated and an annotated `let`, a call argument, a closure body and a
/// value with no expected type. The match arm values stay dotted so each
/// refusal below has one owner.
const BARE_BUILTINS: &str = r#"enum Colour {
    Red;
    Green;
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
    let wrap = |n: i64| -> Option<i64> {
        Some(n)
    };
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
        Open;
        Close;
    }

    state Shut;
    state Ajar;

    on Open: Shut => Ajar;
    on Close: Ajar => Shut;

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
    };
    if values.is_empty() {
        return Option.None;
    }
    .Some(values[0])
}

fn show(value: Option<i64>) -> i64 {
    value ?? 0
}

actor Keeper {
    var held: Option<i64>;

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
    let wrap = |n: i64| -> Option<i64> {
        .Some(n)
    };
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
    let std_copy = dir.path().join("selected").join("std");
    copy_tree(&repo_root().join("std"), &std_copy);
    let option = std_copy.join("option.hew");
    let source = std::fs::read_to_string(&option).expect("read std option");
    let anchor = source.find("self = .None;").expect("fixture anchor moved");
    let line = source[..anchor].matches('\n').count() + 1;
    let column = anchor
        - source[..anchor]
            .rfind('\n')
            .map_or(0, |newline| newline + 1)
        + 8;
    std::fs::write(&option, source.replace("self = .None;", "self = None;"))
        .expect("break std option copy");
    let project = dir.path().join("project");
    std::fs::create_dir(&project).expect("create project directory");
    let user = write_source(&project, "user.hew", "fn main() {\n    println(1);\n}\n");

    let output = hew_command()
        .args(["check", user.to_str().expect("UTF-8 path")])
        .current_dir(&project)
        .env("HEW_STD", &std_copy)
        .output()
        .expect("hew check must run");
    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));
    assert!(
        !output.status.success(),
        "the broken std copy must refuse:\n{stderr}"
    );
    // Diagnostics render the canonical path without Windows' `\\?\` prefix
    // (`hew-compile::display_path`).
    let shown = std::fs::canonicalize(&option)
        .expect("canonical std option path")
        .display()
        .to_string();
    let shown = shown
        .strip_prefix(r"\\?\UNC\")
        .map(|rest| format!(r"\\{rest}"))
        .or_else(|| shown.strip_prefix(r"\\?\").map(str::to_string))
        .unwrap_or(shown);
    assert!(
        stderr.contains(&format!(
            "{shown}:{line}:{column}: error: E_BARE_VARIANT_EXPR"
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
        Open;
    }

    state Shut;
    state Ajar;

    on Open: Shut => Ajar;

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
    for fix in [
        "replace `Shut` with `Door.Shut`",
        "replace `Ajar` with `.Ajar`",
    ] {
        assert!(rendered.contains(fix), "missing `{fix}`:\n{rendered}");
    }
    assert_eq!(
        rendered.matches("error: E_BARE_VARIANT_EXPR").count(),
        2,
        "the bare `Shut` and `Ajar` inside the machine are not refused:\n{rendered}"
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
    for fix in [
        "replace `Err` with `.Err`",
        "replace `Ok` with `.Ok`",
        "replace `Some` with `Option.Some`",
        "replace `None` with `.None`",
        "replace `Some` with `.Some`",
        "replace `Red` with `Colour.Red`",
    ] {
        assert!(rendered.contains(fix), "missing `{fix}`:\n{rendered}");
    }
    assert_eq!(
        rendered.matches("error: E_BARE_VARIANT_EXPR").count(),
        7,
        "only the seven bare sites may be refused:\n{rendered}"
    );
}

/// The dotted and qualified spellings compile and run in the same positions.
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
        "no dotted spelling may report the rule:\n{stderr}"
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "1 0 2 2 3\nhalved\nlooking\ntrue true true 7\nAjar\nkept 9\n"
    );
}
