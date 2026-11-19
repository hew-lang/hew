//! `hew build --link=dynamic`: programs that load the shared Hew runtime.
//!
//! A dynamically linked program must behave exactly like its statically
//! linked twin: same output, same exit status, same fault reports (including
//! the Hew backtrace a `-g` build prints, whose tables reach the runtime only
//! through the entry's registration call). It records the exact release in
//! the runtime name it loads and finds that runtime through its run path.
#![cfg(any(
    all(target_os = "linux", target_env = "gnu"),
    target_os = "freebsd",
    target_os = "macos"
))]

mod support;

use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use support::{describe_output, hew_binary, repo_root, require_codegen};

const HELLO: &str = "fn main() {\n    println(\"hello from hew\");\n}\n";

const FAULT: &str = "fn boom(v: Vec<i64>) -> i64 {\n    v[5]\n}\n\n\
                     fn main() {\n    let v: Vec<i64> = Vec.new();\n    println(boom(v));\n}\n";

fn runtime_name() -> String {
    let version = env!("CARGO_PKG_VERSION");
    if cfg!(target_os = "macos") {
        format!("libhew.{version}.dylib")
    } else {
        format!("libhew.so.{version}")
    }
}

/// Build `source` as `<dir>/<name>/prog.hew` into `<dir>/<name>/<name>`.
/// Every build reads a file of the same name, so fault reports, which name
/// the source file, compare across builds.
fn build(dir: &Path, name: &str, source: &str, args: &[&str]) -> PathBuf {
    let dir = dir.join(name);
    std::fs::create_dir_all(&dir).expect("build directory");
    let input = dir.join("prog.hew");
    std::fs::write(&input, source).expect("write source");
    let output = dir.join(name);
    let built = Command::new(hew_binary())
        .arg("build")
        .args(args)
        .arg(&input)
        .arg("-o")
        .arg(&output)
        .current_dir(repo_root())
        .output()
        .expect("invoke hew build");
    assert!(built.status.success(), "{}", describe_output(&built));
    output
}

/// Run a built program with the loader's own search variables cleared, so
/// it finds its runtime only through the run path it recorded. (Cargo puts
/// the target directory, which holds the in-tree runtime, on them for tests.)
fn run(program: &Path) -> Output {
    Command::new(program)
        .env_remove("LD_LIBRARY_PATH")
        .env_remove("DYLD_LIBRARY_PATH")
        .env_remove("DYLD_FALLBACK_LIBRARY_PATH")
        .output()
        .expect("run program")
}

/// The fault report without its `build <id>` line, which names each binary.
fn report(output: &Output) -> Vec<String> {
    String::from_utf8_lossy(&output.stderr)
        .lines()
        .filter(|line| !line.starts_with("hew:   build "))
        .map(str::to_string)
        .collect()
}

#[test]
fn dynamic_program_matches_its_static_twin() {
    require_codegen();
    let dir = tempfile::tempdir().expect("temp dir");
    let fixed = build(dir.path(), "hello-static", HELLO, &["--link=static"]);
    let shared = build(dir.path(), "hello-dynamic", HELLO, &["--link=dynamic"]);

    let (fixed_run, shared_run) = (run(&fixed), run(&shared));
    assert!(
        shared_run.status.success(),
        "{}",
        describe_output(&shared_run)
    );
    assert_eq!(shared_run.stdout, fixed_run.stdout);
    assert_eq!(shared_run.stdout, b"hello from hew\n");

    let size = |path: &Path| std::fs::metadata(path).expect("binary metadata").len();
    assert!(
        size(&shared) * 10 < size(&fixed),
        "a dynamic program carries no runtime: {} vs {} bytes",
        size(&shared),
        size(&fixed)
    );

    // The loader looks the runtime up by its versioned name.
    let image = std::fs::read(&shared).expect("read binary");
    let name = runtime_name();
    assert!(
        image
            .windows(name.len())
            .any(|window| window == name.as_bytes()),
        "the program must record {name}"
    );
    assert!(
        !std::fs::read(&fixed)
            .expect("read binary")
            .windows(name.len())
            .any(|window| window == name.as_bytes()),
        "a static program must not depend on {name}"
    );
}

#[test]
fn dynamic_fault_report_matches_static_including_debug_backtrace() {
    require_codegen();
    let dir = tempfile::tempdir().expect("temp dir");
    for debug in [false, true] {
        let mut args = vec![];
        if debug {
            args.push("-g");
        }
        let fixed = build(
            dir.path(),
            &format!("fault-static-{debug}"),
            FAULT,
            &[args.as_slice(), &["--link=static"]].concat(),
        );
        let shared = build(
            dir.path(),
            &format!("fault-dynamic-{debug}"),
            FAULT,
            &[args.as_slice(), &["--link=dynamic"]].concat(),
        );
        let (fixed_run, shared_run) = (run(&fixed), run(&shared));
        assert_eq!(shared_run.status.code(), fixed_run.status.code());
        assert_eq!(report(&shared_run), report(&fixed_run), "-g: {debug}");
        if debug {
            let text = report(&shared_run).join("\n");
            assert!(text.contains("at boom ("), "{text}");
        }
    }
}

#[test]
fn rpath_replaces_the_default_search_path() {
    require_codegen();
    let dir = tempfile::tempdir().expect("temp dir");
    let origin = if cfg!(target_os = "macos") {
        "@executable_path"
    } else {
        "$ORIGIN"
    };
    let program = build(
        dir.path(),
        "hello",
        HELLO,
        &["--link=dynamic", "--rpath", origin],
    );

    // Only the program's own directory is searched, and it holds no runtime:
    // the loader refuses to start the program and names what it looked for.
    let missing = run(&program);
    assert!(!missing.status.success(), "{}", describe_output(&missing));
    let name = runtime_name();
    assert!(
        String::from_utf8_lossy(&missing.stderr).contains(&name),
        "{}",
        describe_output(&missing)
    );

    // Shipping the runtime beside the program is enough.
    let runtime = hew_binary()
        .parent()
        .expect("compiler directory")
        .join(&name)
        .canonicalize()
        .expect("in-tree shared runtime");
    std::fs::copy(&runtime, program.with_file_name(&name)).expect("copy runtime");
    let found = run(&program);
    assert!(found.status.success(), "{}", describe_output(&found));
    assert_eq!(found.stdout, b"hello from hew\n");
}

/// A build that trims paths (the `--release` default) records no
/// build-machine directory, so it links the same on every machine; it runs
/// once its runtime ships beside it. `--no-trim-paths` keeps the in-place
/// search directory.
#[test]
fn trimmed_builds_record_no_runtime_install_directory() {
    require_codegen();
    let dir = tempfile::tempdir().expect("temp dir");
    let runtime = hew_binary()
        .parent()
        .expect("compiler directory")
        .join(runtime_name())
        .canonicalize()
        .expect("in-tree shared runtime");
    let install_dir = runtime
        .parent()
        .expect("runtime directory")
        .display()
        .to_string();
    let records_install_dir = |program: &Path| {
        let image = std::fs::read(program).expect("read binary");
        image
            .windows(install_dir.len())
            .any(|window| window == install_dir.as_bytes())
    };

    let trimmed = build(
        dir.path(),
        "release",
        HELLO,
        &["--release", "--link=dynamic"],
    );
    assert!(
        !records_install_dir(&trimmed),
        "a trimmed build must not name {install_dir}"
    );
    assert!(
        !run(&trimmed).status.success(),
        "no runtime beside the program yet"
    );
    std::fs::copy(&runtime, trimmed.with_file_name(runtime_name())).expect("copy runtime");
    let shipped = run(&trimmed);
    assert!(shipped.status.success(), "{}", describe_output(&shipped));
    assert_eq!(shipped.stdout, b"hello from hew\n");

    let untrimmed = build(
        dir.path(),
        "release-untrimmed",
        HELLO,
        &["--release", "--no-trim-paths", "--link=dynamic"],
    );
    assert!(
        records_install_dir(&untrimmed),
        "--no-trim-paths keeps {install_dir}"
    );
    let in_place = run(&untrimmed);
    assert!(in_place.status.success(), "{}", describe_output(&in_place));
}

#[test]
fn rpath_without_dynamic_linking_is_a_usage_error() {
    require_codegen();
    let dir = tempfile::tempdir().expect("temp dir");
    let input = dir.path().join("hello.hew");
    std::fs::write(&input, HELLO).expect("write source");
    let built = Command::new(hew_binary())
        .args(["build", "--rpath", "/opt/app/lib"])
        .arg(&input)
        .arg("-o")
        .arg(dir.path().join("hello"))
        .output()
        .expect("invoke hew build");
    assert_eq!(built.status.code(), Some(2), "{}", describe_output(&built));
    assert!(
        String::from_utf8_lossy(&built.stderr).contains("--rpath applies only with --link=dynamic"),
        "{}",
        describe_output(&built)
    );
}
