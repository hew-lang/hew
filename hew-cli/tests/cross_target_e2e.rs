use std::path::{Path, PathBuf};
use std::process::Command;

use object::{Architecture, BinaryFormat, Object};

fn repo_root() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-cli crate should live under the repo root")
}

fn hew_binary() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_hew"))
}

fn workspace() -> tempfile::TempDir {
    tempfile::Builder::new()
        .prefix("cross-target-hew-")
        .tempdir_in(repo_root())
        .expect("temp dir")
}

fn write_main(dir: &Path) -> PathBuf {
    let source = dir.join("main.hew");
    std::fs::write(&source, "fn main() {}\n").expect("write source");
    source
}

fn foreign_native_target() -> &'static str {
    if cfg!(all(target_os = "macos", target_arch = "aarch64"))
        || cfg!(all(target_os = "linux", target_arch = "aarch64"))
    {
        "x86_64-unknown-linux-gnu"
    } else if cfg!(all(target_os = "macos", target_arch = "x86_64"))
        || cfg!(all(target_os = "linux", target_arch = "x86_64"))
    {
        "aarch64-unknown-linux-gnu"
    } else if cfg!(target_os = "windows") {
        "x86_64-unknown-linux-gnu"
    } else {
        "x86_64-pc-windows-gnu"
    }
}

#[test]
fn emit_obj_reports_expected_metadata_for_cross_target_matrix() {
    struct Case {
        triple: &'static str,
        output_name: &'static str,
        format: BinaryFormat,
        arch: Architecture,
    }

    let cases = [
        Case {
            triple: "arm64-apple-darwin",
            output_name: "darwin-arm64.o",
            format: BinaryFormat::MachO,
            arch: Architecture::Aarch64,
        },
        Case {
            triple: "x86_64-unknown-linux-gnu",
            output_name: "linux-x86_64.o",
            format: BinaryFormat::Elf,
            arch: Architecture::X86_64,
        },
        Case {
            triple: "aarch64-unknown-linux-gnu",
            output_name: "linux-aarch64.o",
            format: BinaryFormat::Elf,
            arch: Architecture::Aarch64,
        },
        Case {
            // The first Windows prototype lane uses the GNU ABI rather than
            // widening this coverage to both GNU and MSVC at once.
            triple: "x86_64-pc-windows-gnu",
            output_name: "windows-x64.obj",
            format: BinaryFormat::Coff,
            arch: Architecture::X86_64,
        },
    ];

    for case in cases {
        let dir = workspace();
        let source = write_main(dir.path());
        let object_path = dir.path().join(case.output_name);

        let output = Command::new(hew_binary())
            .args([
                "build",
                source.to_str().expect("source path"),
                "--target",
                case.triple,
                "--emit-obj",
                "-o",
                object_path.to_str().expect("object path"),
            ])
            .current_dir(dir.path())
            .output()
            .expect("run hew build");

        assert!(
            output.status.success(),
            "hew build --target {} --emit-obj failed\nstdout:\n{}\nstderr:\n{}",
            case.triple,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr),
        );

        let bytes = std::fs::read(&object_path).expect("read object");
        let file = object::File::parse(&*bytes).expect("parse object");
        assert_eq!(
            file.format(),
            case.format,
            "wrong object format for {}",
            case.triple
        );
        assert_eq!(
            file.architecture(),
            case.arch,
            "wrong object architecture for {}",
            case.triple
        );
    }
}

#[test]
fn build_rejects_foreign_native_link_targets_with_clear_error() {
    let dir = workspace();
    let source = write_main(dir.path());
    let target = foreign_native_target();
    let output_path = dir.path().join("foreign-link-output");

    let output = Command::new(hew_binary())
        .args([
            "build",
            source.to_str().expect("source path"),
            "--target",
            target,
            "-o",
            output_path.to_str().expect("output path"),
        ])
        .current_dir(dir.path())
        .output()
        .expect("run hew build");

    assert!(!output.status.success());
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.contains(target));
    assert!(stderr.contains("can emit objects"));
    assert!(stderr.contains("--emit-obj"));
}

#[test]
fn run_and_debug_reject_foreign_targets_before_execution() {
    let target = foreign_native_target();

    for command in ["run", "debug"] {
        let dir = workspace();
        let source = write_main(dir.path());
        let output = Command::new(hew_binary())
            .args([
                command,
                source.to_str().expect("source path"),
                "--target",
                target,
            ])
            .current_dir(dir.path())
            .output()
            .expect("run hew command");

        assert!(!output.status.success(), "{command} unexpectedly succeeded");
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(stderr.contains(target), "{command} stderr: {stderr}");
        assert!(
            stderr.contains("Cross-target executable"),
            "{command} stderr: {stderr}"
        );
        assert!(stderr.contains("--emit-obj"), "{command} stderr: {stderr}");
    }
}
