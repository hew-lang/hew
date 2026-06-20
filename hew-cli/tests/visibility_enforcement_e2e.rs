/// Acceptance tests for visibility enforcement.
///
/// Covers the §6 fixture matrix from the plan:
///   - private cross-module → `E_VISIBILITY_PRIVATE`
///   - package cross-package → `E_VISIBILITY_PACKAGE`
///   - pub cross-module / same-module private / same-package package → OK
///   - private type referenced cross-module → `E_VISIBILITY_PRIVATE`
///   - package fn referenced cross-package → `E_VISIBILITY_PACKAGE`
///   - no raw debug payload in the error output
mod support;

use std::fs;
use std::process::Command;
use support::{describe_output, hew_binary, strip_ansi};

/// Set up a two-file module layout and run `hew check` on the main file.
///
/// `module_file` is the name of the depended-on module (e.g. `"mymod"`), which is
/// written to `<tmpdir>/mymod.hew`.  `main_content` references the module via
/// `import mymod;`.  Returns the raw `Output` so callers can inspect stderr.
fn check_with_module(
    module_name: &str,
    module_content: &str,
    main_content: &str,
) -> std::process::Output {
    let dir = support::tempdir();
    fs::write(
        dir.path().join(format!("{module_name}.hew")),
        module_content,
    )
    .expect("write module file");
    fs::write(dir.path().join("main.hew"), main_content).expect("write main file");

    Command::new(hew_binary())
        .arg("check")
        .arg(dir.path().join("main.hew"))
        .output()
        .expect("hew binary must run")
}

// ── negative (reject) cases ──────────────────────────────────────────────────

/// A private function in another module must be rejected with `E_VISIBILITY`.
#[test]
fn private_fn_cross_module_emits_visibility_error() {
    let output = check_with_module(
        "mymod",
        // module: private function
        "fn secret() -> i64 { 42 }\n",
        // main: cross-module call to private fn
        "import mymod;\nfn main() -> i64 { mymod.secret() }\n",
    );

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(
        !output.status.success(),
        "cross-module call to private fn must be rejected\n{}",
        describe_output(&output),
    );
    assert!(
        stderr.contains("is private to module") || stderr.contains("E_VISIBILITY"),
        "error must mention visibility restriction; got:\n{stderr}",
    );
    assert!(
        stderr.contains("secret"),
        "error must name the symbol; got:\n{stderr}",
    );
}

/// A private type referenced cross-module must be rejected with `E_VISIBILITY`.
#[test]
fn private_type_cross_module_emits_visibility_error() {
    let output = check_with_module(
        "shapes",
        // module: private record type
        "type Point { x: i64, y: i64 }\n\
         pub fn origin() -> i64 { 0 }\n",
        // main: cross-module type reference via qualified name
        "import shapes;\nfn locate(p: shapes.Point) -> i64 { 0 }\nfn main() {}\n",
    );

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(
        !output.status.success(),
        "cross-module reference to private type must be rejected\n{}",
        describe_output(&output),
    );
    assert!(
        stderr.contains("is private to module") || stderr.contains("E_VISIBILITY"),
        "error must mention visibility restriction; got:\n{stderr}",
    );
    assert!(
        stderr.contains("Point"),
        "error must name the symbol; got:\n{stderr}",
    );
}

/// Visibility error must emit a human-readable message without raw debug payloads.
#[test]
fn visibility_error_has_no_debug_payload() {
    let output = check_with_module(
        "mymod",
        "fn secret() -> i64 { 42 }\n",
        "import mymod;\nfn main() -> i64 { mymod.secret() }\n",
    );

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    // The error must appear (so the fixture is exercising the right path).
    assert!(
        !output.status.success(),
        "private fn must be rejected; got:\n{}",
        describe_output(&output),
    );
    // The diagnostic must use the human-readable message, never raw Rust Debug.
    assert!(
        !stderr.contains("VisibilityViolationPrivate")
            && !stderr.contains("VisibilityViolationPackage"),
        "raw variant names must not appear in stderr; got:\n{stderr}",
    );
    assert!(
        !stderr.contains("TypeErrorKind"),
        "TypeErrorKind debug must not appear in stderr; got:\n{stderr}",
    );
    // The human-readable message must be present.
    assert!(
        stderr.contains("is private to module") || stderr.contains("is package-visible"),
        "rendered diagnostic must use human-readable text; got:\n{stderr}",
    );
}

// ── positive (accept) cases ──────────────────────────────────────────────────

/// A pub function must be accessible cross-module without any error.
#[test]
fn pub_fn_cross_module_accepted() {
    let output = check_with_module(
        "math",
        "pub fn add(a: i64, b: i64) -> i64 { a + b }\n",
        "import math;\nfn main() -> i64 { math.add(1, 2) }\n",
    );

    assert!(
        output.status.success(),
        "pub fn must be accessible cross-module\n{}",
        describe_output(&output),
    );
}

/// A private function called within the same module file must be accepted.
#[test]
fn private_fn_same_module_accepted() {
    // Everything in one file (root module context — no cross-module boundary).
    let dir = support::tempdir();
    fs::write(
        dir.path().join("main.hew"),
        "fn helper() -> i64 { 42 }\nfn main() -> i64 { helper() }\n",
    )
    .expect("write main");

    let output = Command::new(hew_binary())
        .arg("check")
        .arg(dir.path().join("main.hew"))
        .output()
        .expect("hew binary must run");

    assert!(
        output.status.success(),
        "private fn within same module must be accepted\n{}",
        describe_output(&output),
    );
}

/// A pub type referenced cross-module must be accepted.
#[test]
fn pub_type_cross_module_accepted() {
    let output = check_with_module(
        "shapes",
        "pub type Point { x: i64, y: i64 }\n",
        "import shapes;\nfn locate(p: shapes.Point) -> i64 { 0 }\nfn main() {}\n",
    );

    assert!(
        output.status.success(),
        "pub type must be accessible cross-module\n{}",
        describe_output(&output),
    );
}

// ── package visibility ────────────────────────────────────────────────────────

/// A `package fn` accessed from within the same package must be accepted.
#[test]
fn package_fn_same_package_accepted() {
    // Set up a flat two-module package: both siblings in the same directory,
    // imported by a main file in the same directory.
    // Package boundary = parent directory of declaring module.
    // mod_a and main are in the same directory → same package.
    let dir = support::tempdir();
    fs::write(
        dir.path().join("mod_a.hew"),
        "package fn internal() -> i64 { 7 }\n",
    )
    .expect("write mod_a");
    fs::write(
        dir.path().join("main.hew"),
        "import mod_a;\nfn main() -> i64 { mod_a.internal() }\n",
    )
    .expect("write main");

    let output = Command::new(hew_binary())
        .arg("check")
        .arg(dir.path().join("main.hew"))
        .output()
        .expect("hew binary must run");

    assert!(
        output.status.success(),
        "package fn must be accessible from within the same package\n{}",
        describe_output(&output),
    );
}

/// A `package fn` accessed from a different package must be rejected.
///
/// Layout: main.hew at tmp root (package root);
/// subpkg/helper.hew at a subdirectory (package subpkg).
/// Cross-package access must emit `E_VISIBILITY_PACKAGE`.
#[test]
fn package_fn_cross_package_rejected() {
    let dir = support::tempdir();
    let subpkg = dir.path().join("subpkg");
    fs::create_dir(&subpkg).expect("create subpkg dir");
    fs::write(
        subpkg.join("helper.hew"),
        "package fn secret() -> i64 { 99 }\n",
    )
    .expect("write helper");
    fs::write(
        dir.path().join("main.hew"),
        "import subpkg::helper;\nfn main() -> i64 { helper.secret() }\n",
    )
    .expect("write main");

    let output = Command::new(hew_binary())
        .arg("check")
        .arg(dir.path().join("main.hew"))
        .output()
        .expect("hew binary must run");

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(
        !output.status.success(),
        "package fn accessed from a different package must be rejected\n{}",
        describe_output(&output),
    );
    assert!(
        stderr.contains("is package-visible") || stderr.contains("E_VISIBILITY"),
        "error must mention package visibility restriction; got:\n{stderr}",
    );
    assert!(
        stderr.contains("secret"),
        "error must name the symbol; got:\n{stderr}",
    );
}

/// A `package type` accessed from the same package must be accepted.
///
/// This verifies that the same-package predicate does not produce
/// false positives for types declared in a sibling module file.
#[test]
fn package_type_same_package_accepted() {
    let dir = support::tempdir();
    fs::write(
        dir.path().join("mod_a.hew"),
        "package type Hidden { value: i64 }\n\
         pub fn make_hidden(v: i64) -> Hidden { Hidden { value: v } }\n",
    )
    .expect("write mod_a");
    fs::write(
        dir.path().join("main.hew"),
        "import mod_a;\nfn main() -> i64 { let h: mod_a.Hidden = mod_a.make_hidden(1); 0 }\n",
    )
    .expect("write main");

    let output = Command::new(hew_binary())
        .arg("check")
        .arg(dir.path().join("main.hew"))
        .output()
        .expect("hew binary must run");

    assert!(
        output.status.success(),
        "package type must be accessible from within the same package\n{}",
        describe_output(&output),
    );
}

/// A `package type` referenced from a different package must be rejected.
///
/// Layout: main.hew at tmp root (package root);
/// subpkg/shapes.hew at a subdirectory (package subpkg).
/// A qualified type reference across the package boundary must emit
/// `E_VISIBILITY_PACKAGE` — the type path's `Package` branch in resolution.rs.
#[test]
fn package_type_cross_package_rejected() {
    let dir = support::tempdir();
    let subpkg = dir.path().join("subpkg");
    fs::create_dir(&subpkg).expect("create subpkg dir");
    fs::write(
        subpkg.join("shapes.hew"),
        "package type Point { x: i64, y: i64 }\n\
         pub fn origin() -> i64 { 0 }\n",
    )
    .expect("write shapes");
    fs::write(
        dir.path().join("main.hew"),
        "import subpkg::shapes;\n\
         fn locate(p: shapes.Point) -> i64 { 0 }\n\
         fn main() {}\n",
    )
    .expect("write main");

    let output = Command::new(hew_binary())
        .arg("check")
        .arg(dir.path().join("main.hew"))
        .output()
        .expect("hew binary must run");

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(
        !output.status.success(),
        "package type referenced from a different package must be rejected\n{}",
        describe_output(&output),
    );
    assert!(
        stderr.contains("is package-visible") || stderr.contains("E_VISIBILITY"),
        "error must mention package visibility restriction; got:\n{stderr}",
    );
    assert!(
        stderr.contains("Point"),
        "error must name the symbol; got:\n{stderr}",
    );
}
