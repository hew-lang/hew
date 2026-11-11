//! `hew symbolize` on real `hew build -g` binaries at O0 and O2: a Hew
//! function's address resolves to its source file and line, an address no
//! symbols cover prints `??`, and a binary from another build is refused.
//!
//! Windows images carry no COFF symbol table to find the probe address in;
//! the PDB path is covered by `hew-symbolize`'s own tests.
#![cfg(not(windows))]

mod support;

use std::path::Path;
use std::process::Command;

use object::{Object, ObjectSymbol};
use support::{hew_binary, require_codegen, tempdir};

const SOURCE: &str = "\
fn level2(x: i64) -> i64 {
    let y = x * 3;
    y + 1
}

fn main() -> i64 {
    println(level2(4));
    0
}
";

/// The file address of `level2`, as `nm` prints it.
fn level2_address(binary: &Path) -> u64 {
    let data = std::fs::read(binary).expect("read binary");
    let file = object::File::parse(&*data).expect("parse binary");
    file.symbols()
        .find(|symbol| {
            symbol
                .name()
                .is_ok_and(|name| name.ends_with("__hew_fn_level2"))
        })
        .expect("level2 has a symbol")
        .address()
}

fn build(dir: &Path, release: bool) -> std::path::PathBuf {
    let src = dir.join("p.hew");
    std::fs::write(&src, SOURCE).expect("write source");
    let binary = hew_testutil::compiled_binary_path(dir, "p");
    let mut command = Command::new(hew_binary());
    command.arg("build").arg("-g");
    if release {
        command.arg("--release");
    }
    let output = command
        .arg(&src)
        .arg("-o")
        .arg(&binary)
        .output()
        .expect("run hew build");
    assert!(
        output.status.success(),
        "hew build failed:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    binary
}

fn symbolize(binary: &Path, args: &[String]) -> std::process::Output {
    Command::new(hew_binary())
        .arg("symbolize")
        .arg(binary)
        .args(args)
        .output()
        .expect("run hew symbolize")
}

fn names_level2_in_source(release: bool) {
    require_codegen();
    let dir = tempdir();
    let binary = build(dir.path(), release);
    let start = format!("{:#x}", level2_address(&binary));
    let output = symbolize(&binary, std::slice::from_ref(&start));
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    assert!(stdout.contains("level2 at "), "{stdout}");
    assert!(stdout.contains("p.hew:1"), "{stdout}");

    // Far outside the image: no symbols cover it.
    let output = symbolize(&binary, &["0x7fff00000000".into()]);
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "0x7fff00000000 ??\n"
    );

    // A frame recorded by another build is refused, not misattributed. The
    // binary may have no build id of its own (a FreeBSD link without
    // --build-id), so only the refusal and the expected id are fixed.
    let output = symbolize(&binary, &[start, "--build-id=00112233".into()]);
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert_eq!(output.status.code(), Some(1), "{stderr}");
    assert!(stderr.contains("00112233"), "{stderr}");
}

#[test]
fn debug_build_resolves_a_hew_function_to_its_source() {
    names_level2_in_source(false);
}

#[test]
fn optimized_debug_build_resolves_a_hew_function_to_its_source() {
    names_level2_in_source(true);
}

const PANICS: &str = "\
fn level2(x: i64) -> i64 {
    if x > 3 {
        panic(\"too big\");
    }
    x
}

fn main() -> i64 {
    println(level2(4));
    0
}
";

/// A release build without `-g` keeps no symbol table, only the Hew tables:
/// the runtime names the failing frame and site from them, and `hew
/// symbolize` resolves the recorded frame from them too.
#[test]
fn stripped_release_build_resolves_frames_from_the_hew_tables() {
    require_codegen();
    let dir = tempdir();
    let src = dir.path().join("p.hew");
    std::fs::write(&src, PANICS).expect("write source");
    let binary = hew_testutil::compiled_binary_path(dir.path(), "p");
    let output = Command::new(hew_binary())
        .args(["build", "--release"])
        .arg(&src)
        .arg("-o")
        .arg(&binary)
        .output()
        .expect("run hew build");
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );

    let log = dir.path().join("failures.ndjson");
    let run = Command::new(&binary)
        .env("HEW_BACKTRACE", "1")
        .env("HEW_FAILURE_LOG", &log)
        .output()
        .expect("run program");
    let stderr = String::from_utf8_lossy(&run.stderr);
    assert!(
        stderr.contains("hew: failure: UserPanic (212) at "),
        "{stderr}"
    );
    assert!(stderr.contains("p.hew:3:15: too big"), "{stderr}");
    // O2 may inline level2 into main. The site still identifies the panic,
    // while the frame table describes the surviving machine-code function.
    assert!(stderr.contains("hew:   at main ("), "{stderr}");
    assert!(stderr.contains("p.hew:8)"), "{stderr}");

    let record: serde_json::Value = serde_json::from_str(
        std::fs::read_to_string(&log)
            .expect("failure log")
            .lines()
            .next()
            .expect("one record"),
    )
    .expect("record parses");
    let frame = record["frames"]
        .as_array()
        .expect("frames")
        .iter()
        .find(|frame| frame["hew_fn"] == "main")
        .unwrap_or_else(|| panic!("a main frame: {record}"));
    let output = symbolize(
        &binary,
        &[
            frame["pc"].as_str().expect("pc").to_owned(),
            format!("--base={}", record["build"]["image_base"].as_str().unwrap()),
            "--return-addresses".to_owned(),
        ],
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("main at "), "{stdout}");
    assert!(stdout.contains("p.hew:8"), "{stdout}");
}

/// The compact table only describes physical frames; DWARF additionally
/// expands an optimized physical frame into the source calls it inlined.
#[test]
fn optimized_debug_build_resolves_inlined_failure_frames() {
    require_codegen();
    let dir = tempdir();
    let src = dir.path().join("inline.hew");
    std::fs::write(&src, PANICS).expect("write source");
    let binary = hew_testutil::compiled_binary_path(dir.path(), "inline");
    let build = Command::new(hew_binary())
        .args(["build", "--release", "-g"])
        .arg(&src)
        .arg("-o")
        .arg(&binary)
        .output()
        .expect("build optimized debug program");
    assert!(
        build.status.success(),
        "{}",
        String::from_utf8_lossy(&build.stderr)
    );
    let log = dir.path().join("failure.ndjson");
    let run = Command::new(&binary)
        .env("HEW_BACKTRACE", "1")
        .env("HEW_FAILURE_LOG", &log)
        .output()
        .expect("run optimized debug program");
    assert!(!run.status.success());
    let record: serde_json::Value = serde_json::from_str(
        std::fs::read_to_string(log)
            .expect("failure log")
            .lines()
            .next()
            .expect("record"),
    )
    .expect("parse failure record");
    let frame = record["frames"]
        .as_array()
        .expect("frames")
        .iter()
        .find(|frame| frame["hew_fn"] == "main")
        .unwrap_or_else(|| panic!("main frame missing: {record}"));
    let output = symbolize(
        &binary,
        &[
            frame["pc"].as_str().expect("pc").to_owned(),
            format!("--base={}", record["build"]["image_base"].as_str().unwrap()),
            "--return-addresses".to_owned(),
        ],
    );
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("level2 at "), "{stdout}");
    assert!(stdout.contains("(inlined)"), "{stdout}");
    assert!(stdout.contains("main at "), "{stdout}");
}
