mod support;

use std::path::{Path, PathBuf};
use std::process::Command;
#[cfg(target_os = "macos")]
use std::sync::OnceLock;

use object::{Architecture, BinaryFormat, Object};
#[cfg(target_os = "macos")]
use std::os::unix::fs as unix_fs;
use support::require_codegen;

fn repo_root() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-cli crate should live under the repo root")
}

fn hew_binary() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_hew"))
}

#[cfg(target_os = "macos")]
static DARWIN_CROSS_LIB_STATUS: OnceLock<Result<(), String>> = OnceLock::new();

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

#[cfg(target_os = "macos")]
fn darwin_cross_target() -> (&'static str, Architecture) {
    if cfg!(target_arch = "aarch64") {
        ("x86_64-apple-darwin", Architecture::X86_64)
    } else {
        ("aarch64-apple-darwin", Architecture::Aarch64)
    }
}

#[cfg(target_os = "macos")]
fn require_darwin_cross_target_library() {
    if let Err(error) = DARWIN_CROSS_LIB_STATUS.get_or_init(bootstrap_darwin_cross_target_library) {
        panic!("{error}");
    }
}

#[cfg(target_os = "macos")]
fn bootstrap_darwin_cross_target_library() -> Result<(), String> {
    let (target, _) = darwin_cross_target();
    let target_dir = hew_binary()
        .parent()
        .and_then(Path::parent)
        .ok_or_else(|| {
            format!(
                "hew binary path {} has no Cargo target dir",
                hew_binary().display()
            )
        })?
        .to_path_buf();
    let profile = match hew_binary()
        .parent()
        .and_then(|dir| dir.file_name())
        .and_then(|name| name.to_str())
    {
        Some("release") => "release",
        _ => "debug",
    };
    let built_archive = target_dir.join(target).join(profile).join("libhew.a");

    if !built_archive.is_file() {
        let output = Command::new("cargo")
            .args(["build", "-q", "-p", "hew-lib", "--target", target])
            .env("CARGO_TARGET_DIR", &target_dir)
            .current_dir(repo_root())
            .output()
            .map_err(|error| format!("failed to invoke cargo build for {target}: {error}"))?;
        if !output.status.success() {
            return Err(format!(
                "failed to build hew-lib for {target}\nstdout:\n{}\nstderr:\n{}",
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr),
            ));
        }
    }

    if !built_archive.is_file() {
        return Err(format!(
            "cross-target hew-lib build completed but {} was not created",
            built_archive.display()
        ));
    }

    let staged_dir = target_dir.join("lib").join(target);
    std::fs::create_dir_all(&staged_dir)
        .map_err(|error| format!("failed to create {}: {error}", staged_dir.display()))?;
    let staged_archive = staged_dir.join("libhew.a");
    if staged_archive.exists() {
        std::fs::remove_file(&staged_archive)
            .map_err(|error| format!("failed to replace {}: {error}", staged_archive.display()))?;
    }
    unix_fs::symlink(&built_archive, &staged_archive).map_err(|error| {
        format!(
            "failed to stage Darwin cross-target archive {} -> {}: {error}",
            staged_archive.display(),
            built_archive.display(),
        )
    })?;

    Ok(())
}

#[test]
fn emit_obj_reports_expected_metadata_for_cross_target_matrix() {
    struct Case {
        triple: &'static str,
        output_suffix: &'static str,
        format: BinaryFormat,
        arch: Architecture,
    }

    let cases = [
        Case {
            triple: "arm64-apple-darwin",
            output_suffix: ".o",
            format: BinaryFormat::MachO,
            arch: Architecture::Aarch64,
        },
        Case {
            triple: "x86_64-unknown-linux-gnu",
            output_suffix: ".o",
            format: BinaryFormat::Elf,
            arch: Architecture::X86_64,
        },
        Case {
            // The first Windows prototype lane uses the GNU ABI rather than
            // widening this coverage to both GNU and MSVC at once.
            triple: "x86_64-pc-windows-gnu",
            output_suffix: ".obj",
            format: BinaryFormat::Coff,
            arch: Architecture::X86_64,
        },
    ];

    for case in cases {
        let dir = workspace();
        let source = write_main(dir.path());
        let object_path = dir.path().join(format!("main{}", case.output_suffix));

        let output = Command::new(hew_binary())
            .args([
                "build",
                source.to_str().expect("source path"),
                "--target",
                case.triple,
                "--emit-obj",
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

/// On macOS arm64, `hew build --target arm64-apple-darwin` must produce a
/// runnable native Mach-O arm64 executable — not just an object file.
///
/// This is the same-arch integration smoke test for the target-aware native-link
/// slice (issue #254).  It verifies:
///   1. The build exits 0.
///   2. The output is a Mach-O arm64 binary.
///   3. The binary executes without error on this host.
///   4. No deployment-target mismatch linker warning is emitted (completing
///      the fix started in PR #771 for the debug/`hew build` code path).
#[cfg(all(target_os = "macos", target_arch = "aarch64"))]
#[test]
fn native_link_same_arch_explicit_target_produces_runnable_binary() {
    require_codegen();

    let dir = workspace();
    let source = write_main(dir.path());
    let output_path = dir.path().join("hello-arm64");

    let result = Command::new(hew_binary())
        .args([
            "build",
            source.to_str().expect("source path"),
            "--target",
            "arm64-apple-darwin",
            "-o",
            output_path.to_str().expect("output path"),
        ])
        .current_dir(dir.path())
        .output()
        .expect("run hew build");

    assert!(
        result.status.success(),
        "hew build --target arm64-apple-darwin failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&result.stdout),
        String::from_utf8_lossy(&result.stderr),
    );
    assert!(output_path.exists(), "output binary not created");

    // Verify binary format and architecture.
    let data = std::fs::read(&output_path).expect("read binary");
    let obj = object::File::parse(data.as_slice()).expect("parse binary");
    assert_eq!(obj.format(), BinaryFormat::MachO, "expected Mach-O binary");
    assert_eq!(obj.architecture(), Architecture::Aarch64, "expected arm64");

    // The binary must execute without error on this host.
    let run = Command::new(&output_path).output().expect("run binary");
    assert!(
        run.status.success(),
        "linked binary exited non-zero: {:?}\nstderr: {}",
        run.status,
        String::from_utf8_lossy(&run.stderr),
    );

    // No deployment-target mismatch warnings in the linker output.
    let stderr = String::from_utf8_lossy(&result.stderr);
    assert!(
        !stderr.contains("newer macOS version"),
        "deployment-target mismatch warning in linker output:\n{stderr}",
    );
}

#[cfg(target_os = "macos")]
#[test]
fn native_link_darwin_cross_arch_produces_foreign_macho_binary() {
    require_codegen();
    require_darwin_cross_target_library();

    let dir = workspace();
    let source = write_main(dir.path());
    let output_path = dir.path().join("hello-cross-arch");
    let (target, expected_arch) = darwin_cross_target();

    let result = Command::new(hew_binary())
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

    assert!(
        result.status.success(),
        "hew build --target {target} failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&result.stdout),
        String::from_utf8_lossy(&result.stderr),
    );
    assert!(output_path.exists(), "output binary not created");

    let data = std::fs::read(&output_path).expect("read binary");
    let obj = object::File::parse(data.as_slice()).expect("parse binary");
    assert_eq!(obj.format(), BinaryFormat::MachO, "expected Mach-O binary");
    assert_eq!(obj.architecture(), expected_arch, "wrong cross-arch output");

    let stderr = String::from_utf8_lossy(&result.stderr);
    assert!(
        !stderr.contains("--emit-obj"),
        "cross-arch Darwin native link regressed to fail-closed path:\n{stderr}",
    );
}
