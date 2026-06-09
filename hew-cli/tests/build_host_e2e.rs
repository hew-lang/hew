mod support;

use std::path::Path;
use std::process::Command;

use object::BinaryFormat;
use support::{describe_output, hew_binary, repo_root, require_codegen, run_bounded_command};

fn workspace() -> tempfile::TempDir {
    tempfile::Builder::new()
        .prefix("build-host-hew-")
        .tempdir_in(repo_root())
        .expect("temp dir")
}

fn write_hello(dir: &Path) -> std::path::PathBuf {
    let source = dir.join("hello.hew");
    std::fs::write(&source, "fn main() {\n    println(\"build-host-ok\")\n}\n")
        .expect("write source");
    source
}

/// `hew build <input>` with no `-o` produces `./<stem>` in the cwd, the binary
/// is a runnable native executable, and running it prints the program's output.
#[test]
fn build_default_output_produces_runnable_binary() {
    require_codegen();

    let dir = workspace();
    let source = write_hello(dir.path());
    let output = Command::new(hew_binary())
        .args(["build", source.to_str().expect("source path")])
        .current_dir(dir.path())
        .output()
        .expect("run hew build");

    assert!(
        output.status.success(),
        "hew build failed\n{}",
        describe_output(&output),
    );

    // Default output is `./<stem>` in the cwd (no extension on Unix).
    let binary = dir.path().join("hello");
    assert!(binary.exists(), "default ./hello binary not created");

    let run = run_bounded_command(Command::new(&binary), format!("run {}", binary.display()));
    assert!(
        run.status.success(),
        "linked binary exited non-zero: {:?}\nstderr: {}",
        run.status,
        String::from_utf8_lossy(&run.stderr),
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout).trim(),
        "build-host-ok",
        "program stdout mismatch",
    );
}

/// `hew build <input> -o <path>` writes the binary to the requested path
/// verbatim and the binary runs.
#[test]
fn build_explicit_output_path_is_used_verbatim() {
    require_codegen();

    let dir = workspace();
    let source = write_hello(dir.path());
    let output_path = dir.path().join("nested").join("custom-name");
    std::fs::create_dir_all(output_path.parent().unwrap()).expect("create nested dir");

    let output = Command::new(hew_binary())
        .args([
            "build",
            source.to_str().expect("source path"),
            "-o",
            output_path.to_str().expect("output path"),
        ])
        .current_dir(dir.path())
        .output()
        .expect("run hew build");

    assert!(
        output.status.success(),
        "hew build -o failed\n{}",
        describe_output(&output),
    );
    assert!(output_path.exists(), "explicit -o binary not created");

    let run = run_bounded_command(
        Command::new(&output_path),
        format!("run {}", output_path.display()),
    );
    assert!(run.status.success(), "explicit-output binary failed to run");
}

/// `hew build <input> --emit-obj` writes a relocatable object (no link) named
/// `<stem><.o|.obj>` in the cwd and exits 0.
#[test]
fn build_emit_obj_writes_object_without_linking() {
    require_codegen();

    let dir = workspace();
    let source = write_hello(dir.path());

    let output = Command::new(hew_binary())
        .args(["build", source.to_str().expect("source path"), "--emit-obj"])
        .current_dir(dir.path())
        .output()
        .expect("run hew build --emit-obj");

    assert!(
        output.status.success(),
        "hew build --emit-obj failed\n{}",
        describe_output(&output),
    );

    // Host object extension is `.o` on every supported non-Windows host.
    let object_path = dir.path().join("hello.o");
    assert!(object_path.exists(), "emit-obj object not created");
    // No linked binary should be produced in --emit-obj mode.
    assert!(
        !dir.path().join("hello").exists(),
        "--emit-obj must not produce a linked binary",
    );

    let bytes = std::fs::read(&object_path).expect("read object");
    let parsed = object::File::parse(&*bytes).expect("parse object");
    // The host object must be a relocatable object, not an executable.
    assert!(
        matches!(
            parsed.format(),
            BinaryFormat::MachO | BinaryFormat::Elf | BinaryFormat::Coff
        ),
        "unexpected object format {:?}",
        parsed.format(),
    );
}
