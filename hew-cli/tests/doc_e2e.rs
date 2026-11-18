mod support;

use support::{assert_success, run_hew};

/// A file with parse errors must exit nonzero and emit an error message.
#[test]
fn parse_error_file_exits_nonzero() {
    let dir = support::tempdir();
    let src = dir.path().join("bad.hew");
    // Intentionally broken syntax: unclosed brace.
    std::fs::write(&src, "fn broken( {\n").unwrap();
    let out_dir = dir.path().join("docs");

    let output = run_hew(&[
        "doc",
        src.to_str().unwrap(),
        "--output-dir",
        out_dir.to_str().unwrap(),
    ]);

    assert!(
        !output.status.success(),
        "expected nonzero exit for parse-error file\nstderr: {}",
        String::from_utf8_lossy(&output.stderr),
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr
            .lines()
            .any(|line| line.starts_with("Error:") && line.contains("bad.hew")),
        "expected a parse error for bad.hew, got: {stderr}",
    );
    assert!(!out_dir.exists(), "parse error generated documentation");
}

/// Passing a nonexistent file must exit nonzero.
#[test]
fn nonexistent_file_exits_nonzero() {
    let dir = support::tempdir();
    let out_dir = dir.path().join("docs");

    let output = run_hew(&[
        "doc",
        dir.path().join("missing.hew").to_str().unwrap(),
        "--output-dir",
        out_dir.to_str().unwrap(),
    ]);

    assert!(
        !output.status.success(),
        "expected nonzero exit for nonexistent file\nstderr: {}",
        String::from_utf8_lossy(&output.stderr),
    );
}

/// File input should publish a module and its documented API.
#[test]
fn file_input_publishes_module_api() {
    let dir = support::tempdir();
    let src = dir.path().join("math.hew");
    std::fs::write(
        &src,
        "//! Math utilities.\n/// Adds numbers.\npub fn add(a: i32, b: i32) -> i32 { a + b }\n",
    )
    .unwrap();
    let out_dir = dir.path().join("docs");

    let output = run_hew(&[
        "doc",
        src.to_str().unwrap(),
        "--output-dir",
        out_dir.to_str().unwrap(),
    ]);

    assert_success(&output, "expected exit 0 for valid file");
    assert!(out_dir.join("404.html").is_file(), "missing 404.html");

    let index = std::fs::read_to_string(out_dir.join("index.html")).unwrap();
    assert!(index.contains("href=\"math.html\""), "index: {index}");
    assert!(index.contains("Math utilities."), "index: {index}");

    let module = std::fs::read_to_string(out_dir.join("math.html")).unwrap();
    assert!(module.contains("<!DOCTYPE html>"), "module: {module}");
    assert!(module.contains("id=\"fn.add\""), "module: {module}");
    assert!(module.contains("Adds numbers."), "module: {module}");
}

/// Directory input should recurse through `.hew` files and use directory-qualified module names.
#[test]
fn directory_input_recurses_and_uses_directory_module_names() {
    let dir = support::tempdir();
    let pkg_dir = dir.path().join("pkg");
    std::fs::create_dir_all(pkg_dir.join("nested")).unwrap();
    std::fs::write(
        pkg_dir.join("math.hew"),
        "//! Math utilities.\nfn add(a: i32, b: i32) -> i32 { a + b }\n",
    )
    .unwrap();
    std::fs::write(
        pkg_dir.join("nested").join("util.hew"),
        "//! Nested utilities.\nfn id(x: i32) -> i32 { x }\n",
    )
    .unwrap();
    let out_dir = dir.path().join("docs");

    let output = run_hew(&[
        "doc",
        pkg_dir.to_str().unwrap(),
        "--output-dir",
        out_dir.to_str().unwrap(),
    ]);

    assert_success(&output, "expected exit 0 for directory input");

    assert!(out_dir.join("index.html").is_file(), "missing index.html");
    assert!(
        out_dir.join("pkg.math.html").is_file(),
        "missing pkg.math.html"
    );
    assert!(
        out_dir.join("pkg.nested.util.html").is_file(),
        "missing pkg.nested.util.html"
    );

    let index = std::fs::read_to_string(out_dir.join("index.html")).unwrap();
    assert!(index.contains("pkg.math"), "index: {index}");
    assert!(index.contains("pkg.nested.util"), "index: {index}");
}

/// A maintenance build can label a site for its documented release even when
/// a newer generator renders it.
#[test]
fn explicit_site_version_labels_generated_pages() {
    let dir = support::tempdir();
    let src = dir.path().join("versioned.hew");
    std::fs::write(
        &src,
        "//! Versioned documentation.\npub fn value() -> i64 { 1 }\n",
    )
    .unwrap();
    let config = dir.path().join("site.toml");
    std::fs::write(
        &config,
        "brand_tag = 'Hew {version}'\nfooter_text = 'Reference for Hew {version}'\n",
    )
    .unwrap();
    let out_dir = dir.path().join("docs");

    let output = run_hew(&[
        "doc",
        src.to_str().unwrap(),
        "--site-config",
        config.to_str().unwrap(),
        "--site-version",
        "0.9.2",
        "--output-dir",
        out_dir.to_str().unwrap(),
    ]);

    assert_success(&output, "expected explicit site version to be accepted");
    for page in ["index.html", "versioned.html"] {
        let html = std::fs::read_to_string(out_dir.join(page)).unwrap();
        assert!(html.contains("Hew 0.9.2"), "{page}: {html}");
        assert!(!html.contains(env!("CARGO_PKG_VERSION")), "{page}: {html}");
    }
}

/// Parse warnings should be reported but still produce documentation and exit 0.
#[test]
fn parse_warnings_report_but_do_not_fail_generation() {
    let dir = support::tempdir();
    let src = dir.path().join("warn.hew");
    std::fs::write(&src, "//! Warning test.\nfn main() {\n    let x = 1;;\n}\n").unwrap();
    let out_dir = dir.path().join("docs");

    let output = run_hew(&[
        "doc",
        src.to_str().unwrap(),
        "--output-dir",
        out_dir.to_str().unwrap(),
    ]);

    assert_success(&output, "expected exit 0 for parse warnings");

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr
            .lines()
            .any(|line| line.starts_with("Warning:") && line.contains("warn.hew")),
        "stderr: {stderr}"
    );
    assert!(out_dir.join("index.html").is_file(), "missing index.html");
    assert!(out_dir.join("warn.html").is_file(), "missing warn.html");
}
