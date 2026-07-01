//! End-to-end diagnostic tests for the codegen fail-closed caret invariant.
//!
//! Invariant (#2091): a codegen fail-closed error must render a `^^^` caret at
//! the user's source ONLY when the offending function's body provably indexes
//! the source being rendered (the root compilation unit), and must NEVER render
//! a caret against the wrong source. Attribution is carried, not inferred: each
//! MIR function carries a `SourceOrigin`, resolved positively from
//! `HirModule::root_item_ids`. `build_module_for_target` keeps the error's span
//! only for a `SourceOrigin::RootUnit` function and strips it for every other
//! origin, so the CLI renderer either points a caret at the correct root line
//! or degrades to a bare plain-line — never a false caret.
//!
//! Cases:
//! (a) root-module fail-closed → caret at the offending root line.
//! (b) module-path import (`import dep;`) fail-closed → bare, no root caret.
//! (c) file-path import (`import "dep.hew"`), long root so the dep span is
//!     in-bounds → bare, no root caret. A bounds check alone cannot prevent the
//!     false caret here; only the carried origin can.
//! (d) black-hole regression: a root fail-closed carries a source location,
//!     never a bare location-free message.
//! (e) generated impl DEFAULT-METHOD from a file-imported trait: the default
//!     body is synthesised at the root impl site but its span indexes the
//!     imported trait's source → bare, no false root caret. This is the exact
//!     producer an absence-from-a-foreign-set proxy misclassified.
//!
//! Each false-caret case (b, c, e) is a revert-repro: it fails (a false root
//! caret appears) if origin-carrying is disabled by making
//! `SourceOrigin::renders_root_caret` always return `true`.

mod support;

use std::process::Command;

use support::{hew_binary, strip_ansi};

/// Write a set of `(filename, content)` pairs into a temporary directory and
/// return the directory handle (kept alive for the duration of the test).
fn write_fixture(files: &[(&str, &str)]) -> tempfile::TempDir {
    let dir = support::tempdir();
    for (name, content) in files {
        std::fs::write(dir.path().join(name), content).expect("cannot write fixture file");
    }
    dir
}

/// True when stderr contains a line that is BOTH attributed to `main.hew` AND
/// an `error:` header — i.e. a caret rendered against the root source. Warning
/// lines (`main.hew:..: warning:`) are deliberately excluded: an unused-import
/// warning is correct and is not a false codegen attribution.
fn has_root_error_caret(stderr: &str) -> bool {
    stderr
        .lines()
        .any(|line| line.contains("main.hew:") && line.contains("error:"))
}

// The fixture below uses a function with an array return type. The Hew type
// checker accepts array-typed function signatures; codegen rejects them at the
// declare/lowering boundary (`CodegenError::Unsupported`, "Array type — composite
// lowering is Cluster 2"). This makes it a reliable, stable codegen-boundary
// failure that type-checks first.
const ARRAY_RETURN_SOURCE: &str =
    "pub fn unsupported_array_return(x: [i64; 2]) -> [i64; 2] {\n    return x;\n}\nfn main() {}\n";

// ---------------------------------------------------------------------------
// (a) root-module fail-closed shows a caret
// ---------------------------------------------------------------------------

/// A codegen fail-closed in the ROOT module renders a `^^^` caret pointing at
/// the offending construct — not a bare, location-free message.
#[test]
fn root_module_failclosed_renders_caret() {
    let fixture = write_fixture(&[("main.hew", ARRAY_RETURN_SOURCE)]);
    let main_path = fixture.path().join("main.hew");

    let output = Command::new(hew_binary())
        .args(["check", main_path.to_str().unwrap()])
        .current_dir(fixture.path())
        .output()
        .expect("hew binary must run");

    assert!(
        !output.status.success(),
        "hew check must fail on an unsupported array return type"
    );

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    // A `main.hew:line:col: error:` location header must appear — the carried
    // root origin makes the caret safe to render.
    assert!(
        has_root_error_caret(&stderr),
        "root-module fail-closed must render a main.hew:line:col: error: header; got:\n{stderr}"
    );
    // A `^^^` caret underline must appear.
    assert!(
        stderr.contains('^'),
        "root-module fail-closed must render a ^^^ caret underline; got:\n{stderr}"
    );
    // The message must still name the failing construct.
    assert!(
        stderr.contains("unsupported construct") || stderr.contains("fail-closed"),
        "error message must name the failing construct; got:\n{stderr}"
    );
    // The rendered source context must show the offending declaration.
    assert!(
        stderr.contains("[i64; 2]") || stderr.contains("unsupported_array_return"),
        "caret must show the offending function declaration; got:\n{stderr}"
    );
}

// ---------------------------------------------------------------------------
// (b) module-path import fail-closed must NOT produce a wrong root-file caret
// ---------------------------------------------------------------------------

/// A codegen fail-closed originating in a MODULE-PATH import (`import dep;`)
/// must not render a caret against the root file's source.
#[test]
fn module_path_import_failclosed_no_false_root_caret() {
    let root_source = "import dep;\nfn main() {}\n";
    let dep_source = "pub fn bad_array_fn(x: [i64; 2]) -> [i64; 2] {\n    return x;\n}\n";

    let fixture = write_fixture(&[("main.hew", root_source), ("dep.hew", dep_source)]);
    let main_path = fixture.path().join("main.hew");

    let output = Command::new(hew_binary())
        .args(["check", main_path.to_str().unwrap()])
        .current_dir(fixture.path())
        .output()
        .expect("hew binary must run");

    assert!(
        !output.status.success(),
        "hew check must fail when dep.hew has an unsupported array return type"
    );

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    // The fail-closed error must still appear (not silently swallowed).
    assert!(
        stderr.contains("unsupported construct") || stderr.contains("fail-closed"),
        "dep-originating error must still appear; got:\n{stderr}"
    );
    // CRITICAL: no caret attributed to the root file for a dep-origin error.
    assert!(
        !has_root_error_caret(&stderr),
        "module-path import fail-closed must NOT produce a main.hew: error: caret; got:\n{stderr}"
    );
}

// ---------------------------------------------------------------------------
// (c) file-path import fail-closed, in-bounds dep span, must NOT false-caret
// ---------------------------------------------------------------------------

/// A dep function introduced via FILE-PATH import (`import "dep.hew"`) must not
/// render a caret against the root file even when the dep function's byte-offset
/// span falls WITHIN the root source's byte length.
///
/// The root source is deliberately long and the dep function sits near byte 0 of
/// its file, so a bounds check (`span.start < root_source.len()`) would pass and,
/// before the fix, render a `^^^` caret at unrelated root code. Only the carried
/// origin (the dep function is not `RootUnit`, so its span is stripped) prevents
/// the false caret. Revert-repro: fails if origin-carrying is disabled.
#[test]
fn file_path_import_inbounds_dep_span_no_false_root_caret() {
    let root_source = concat!(
        "// This root file is intentionally long so a dep function's small\n",
        "// byte-offset span (near byte 0 in dep.hew) falls within root len.\n",
        "// File-path imports must be treated as non-root for span attribution.\n",
        "import \"dep.hew\";\n",
        "fn main() {}\n",
    );
    let dep_source = "pub fn bad_array_fn(x: [i64; 2]) -> [i64; 2] {\n    return x;\n}\n";

    // Precondition: root is meaningfully longer than the dep function's span
    // start, so the dep span is in-bounds against the root source.
    assert!(
        root_source.len() > dep_source.len(),
        "fixture: root source must be longer than dep source for the in-bounds case"
    );

    let fixture = write_fixture(&[("main.hew", root_source), ("dep.hew", dep_source)]);
    let main_path = fixture.path().join("main.hew");

    let output = Command::new(hew_binary())
        .args(["check", main_path.to_str().unwrap()])
        .current_dir(fixture.path())
        .output()
        .expect("hew binary must run");

    assert!(
        !output.status.success(),
        "hew check must fail when dep.hew has an unsupported array return type"
    );

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(
        stderr.contains("unsupported construct") || stderr.contains("fail-closed"),
        "dep-originating error must still appear; got:\n{stderr}"
    );
    // CRITICAL: an in-bounds dep span must NOT produce a root-file caret.
    assert!(
        !has_root_error_caret(&stderr),
        "in-bounds file-path import span must NOT produce a main.hew: error: caret; got:\n{stderr}"
    );
}

// ---------------------------------------------------------------------------
// (d) black-hole regression: a root fail-closed carries a source location
// ---------------------------------------------------------------------------

/// Regression guard: the codegen fail-closed message for a root-module failure
/// must carry a source location (not the previous bare, location-free
/// black-hole line). Pins the fix so it cannot regress.
#[test]
fn regression_failclosed_carries_location_not_blackhole() {
    let fixture = write_fixture(&[("main.hew", ARRAY_RETURN_SOURCE)]);
    let main_path = fixture.path().join("main.hew");

    let output = Command::new(hew_binary())
        .args(["check", main_path.to_str().unwrap()])
        .current_dir(fixture.path())
        .output()
        .expect("hew binary must run");

    assert!(
        !output.status.success(),
        "codegen-front failure must exit non-zero"
    );

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    assert!(
        stderr.contains("main.hew"),
        "codegen fail-closed must name the source file; got:\n{stderr}"
    );
    let has_detail = stderr.contains("main.hew:") // location header
        || stderr.contains("[i64; 2]")            // source excerpt
        || stderr.contains('^'); // caret underline
    assert!(
        has_detail,
        "codegen fail-closed must carry source location detail, not a bare black-hole; got:\n{stderr}"
    );
}

// ---------------------------------------------------------------------------
// (e) generated default-method from a file-imported trait must NOT false-caret
// ---------------------------------------------------------------------------

/// A trait DEFAULT method inherited from a FILE-IMPORTED trait is synthesised as
/// a `HirItem::Function` at the root impl site (`impl ImportedTrait for RootType`
/// with no override), but its body span indexes the imported trait's source. A
/// codegen fail-closed in that generated body must NOT render a caret against the
/// root file — even though the body offset falls within the (longer) root source.
///
/// This is the exact producer that an absence-from-a-foreign-set proxy
/// misclassified: the generated method is at root module index 0 and is absent
/// from the foreign source-module table, so a proxy treats it as root and
/// renders a false caret. The carried origin excludes generated default-method
/// ids from `root_item_ids`, so it resolves away from `RootUnit` and its span is
/// stripped. Revert-repro: fails if origin-carrying is disabled.
#[test]
fn file_imported_trait_default_method_no_false_root_caret() {
    // Imported trait: `make` is a DEFAULT method whose signature/body use an
    // array type the codegen boundary rejects. `tag` is required (no default).
    let trait_lib = concat!(
        "trait Emit {\n",
        "    fn tag(self) -> i64;\n",
        "    fn make(self, a: [i64; 2]) -> [i64; 2] {\n",
        "        return a;\n",
        "    }\n",
        "}\n",
    );
    // Root file: defines a root-local type and impls the imported trait WITHOUT
    // overriding `make`, so the default body is generated at the root impl site.
    // Long enough that the trait body offset falls within root source bounds.
    let root_source = concat!(
        "// This root file is intentionally long so that the imported trait's\n",
        "// default method body byte-offset span falls within the root source's\n",
        "// byte length. The generated default-method must be treated as non-root.\n",
        "import \"trait_lib.hew\";\n",
        "\n",
        "type RootThing { x: i64 }\n",
        "\n",
        "impl Emit for RootThing {\n",
        "    fn tag(self) -> i64 {\n",
        "        return self.x;\n",
        "    }\n",
        "}\n",
        "\n",
        "fn main() {\n",
        "    let v = RootThing { x: 1 };\n",
        "    println(v.tag());\n",
        "}\n",
    );

    // Precondition: the in-bounds scenario — root longer than the trait body span.
    assert!(
        root_source.len() > trait_lib.len(),
        "fixture: root source must be longer than the trait lib for the in-bounds case"
    );

    let fixture = write_fixture(&[("main.hew", root_source), ("trait_lib.hew", trait_lib)]);
    let main_path = fixture.path().join("main.hew");

    let output = Command::new(hew_binary())
        .args(["check", main_path.to_str().unwrap()])
        .current_dir(fixture.path())
        .output()
        .expect("hew binary must run");

    assert!(
        !output.status.success(),
        "hew check must fail on the generated default-method's unsupported array type"
    );

    let stderr = strip_ansi(&String::from_utf8_lossy(&output.stderr));

    // The fail-closed error must appear (the generated default-method reaches
    // codegen and fails closed).
    assert!(
        stderr.contains("unsupported construct") || stderr.contains("fail-closed"),
        "generated default-method error must still appear; got:\n{stderr}"
    );
    // CRITICAL: the generated default-method's trait-indexed span must NOT be
    // rendered as a caret against the root file.
    assert!(
        !has_root_error_caret(&stderr),
        "file-imported trait default-method must NOT produce a main.hew: error: caret; got:\n{stderr}"
    );
}
