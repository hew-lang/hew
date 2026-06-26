mod support;

use std::path::{Path, PathBuf};
use std::process::Command;
#[cfg(any(target_os = "macos", target_os = "linux"))]
use std::sync::OnceLock;

use object::{Architecture, BinaryFormat, Object, ObjectSection};
#[cfg(target_os = "macos")]
use support::require_codegen;
use support::{hew_binary, repo_root};

#[cfg(target_os = "macos")]
static DARWIN_CROSS_LIB_STATUS: OnceLock<Result<(), String>> = OnceLock::new();

#[cfg(target_os = "linux")]
static LINUX_CROSS_LIB_STATUS: OnceLock<Result<(), String>> = OnceLock::new();

#[cfg(any(target_os = "macos", target_os = "linux"))]
const REQUIRED_CROSS_TARGET_RUNTIME_SYMBOL: &str = "hew_lambda_drain_all";

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
    // Must be a target on a different OS from the host — one that is never
    // linkable with native host tools from any supported OS.
    //
    // Important: Linux same-OS cross-arch (aarch64 ↔ x86_64) is now
    // supported by hew (issue #254 Phase 3), so Linux targets are no longer
    // foreign on a Linux host.  A Windows target is foreign on all Unix hosts,
    // and a Linux target is foreign on Windows hosts.
    if cfg!(target_os = "windows") {
        "x86_64-unknown-linux-gnu"
    } else {
        "x86_64-pc-windows-gnu"
    }
}

#[cfg(target_os = "linux")]
fn linux_cross_target() -> (&'static str, Architecture) {
    // Mirror the host ABI env (gnu vs musl) so that the same-OS cross-arch
    // link gate accepts the target.  A mismatched env would trigger the env
    // mismatch rejection path rather than the cross-arch acceptance path.
    if cfg!(target_arch = "aarch64") {
        if cfg!(target_env = "musl") {
            ("x86_64-unknown-linux-musl", Architecture::X86_64)
        } else {
            ("x86_64-unknown-linux-gnu", Architecture::X86_64)
        }
    } else if cfg!(target_env = "musl") {
        ("aarch64-unknown-linux-musl", Architecture::Aarch64)
    } else {
        ("aarch64-unknown-linux-gnu", Architecture::Aarch64)
    }
}

/// Returns the multiarch sysroot path for the cross target, or `None` if the
/// directory does not exist on this host.
///
/// Handles both GNU (`/usr/<arch>-linux-gnu`) and musl
/// (`/usr/<arch>-linux-musl`) layouts so that musl hosts look for the correct
/// sysroot rather than silently checking a non-existent GNU path.
#[cfg(target_os = "linux")]
fn linux_cross_sysroot_path(cross_triple: &str) -> Option<std::path::PathBuf> {
    let env_suffix = if cross_triple.ends_with("-musl") {
        "musl"
    } else {
        "gnu"
    };
    let arch_tuple = if cross_triple.starts_with("aarch64-") {
        format!("aarch64-linux-{env_suffix}")
    } else if cross_triple.starts_with("x86_64-") {
        format!("x86_64-linux-{env_suffix}")
    } else {
        return None;
    };
    let path = std::path::PathBuf::from(format!("/usr/{arch_tuple}"));
    if path.is_dir() {
        Some(path)
    } else {
        None
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
    bootstrap_cross_target_library(target)
}

#[cfg(any(target_os = "macos", target_os = "linux"))]
fn cargo_target_dir_and_profile() -> Result<(PathBuf, &'static str), String> {
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

    Ok((target_dir, profile))
}

#[cfg(any(target_os = "macos", target_os = "linux"))]
fn bootstrap_cross_target_library(target: &str) -> Result<(), String> {
    let (target_dir, profile) = cargo_target_dir_and_profile()?;
    let built_archive = target_dir.join(target).join(profile).join("libhew.a");
    let stamp_path = target_dir.join(format!("hew-cli-cross-target-{target}-{profile}.stamp"));
    let run_id =
        std::env::var("NEXTEST_RUN_ID").unwrap_or_else(|_| format!("pid:{}", std::process::id()));

    let archive_current = built_archive.is_file()
        && std::fs::read_to_string(&stamp_path).is_ok_and(|stamp| stamp == run_id)
        && archive_defines_symbol(&built_archive, REQUIRED_CROSS_TARGET_RUNTIME_SYMBOL)?;

    if !archive_current {
        let output = build_cross_target_hew_lib(target, &target_dir, profile)?;
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

    if !archive_defines_symbol(&built_archive, REQUIRED_CROSS_TARGET_RUNTIME_SYMBOL)? {
        return Err(format!(
            "cross-target hew-lib archive {} does not define `{}` after rebuild",
            built_archive.display(),
            REQUIRED_CROSS_TARGET_RUNTIME_SYMBOL
        ));
    }

    std::fs::write(&stamp_path, run_id).map_err(|error| {
        format!(
            "failed to write cross-target bootstrap stamp {}: {error}",
            stamp_path.display()
        )
    })?;

    Ok(())
}

#[cfg(any(target_os = "macos", target_os = "linux"))]
fn build_cross_target_hew_lib(
    target: &str,
    target_dir: &Path,
    profile: &str,
) -> Result<std::process::Output, String> {
    let mut command = Command::new("cargo");
    command
        .args(["build", "-q", "-p", "hew-lib", "--target", target])
        .env("CARGO_TARGET_DIR", target_dir)
        .current_dir(repo_root());
    if profile == "release" {
        command.arg("--release");
    }

    command
        .output()
        .map_err(|error| format!("failed to invoke cargo build for {target}: {error}"))
}

#[cfg(any(target_os = "macos", target_os = "linux"))]
fn archive_defines_symbol(archive: &Path, symbol: &str) -> Result<bool, String> {
    if !archive.is_file() {
        return Ok(false);
    }

    let output = Command::new("nm")
        .arg("-g")
        .arg(archive)
        .output()
        .map_err(|error| {
            format!(
                "failed to inspect cross-target archive {} with nm: {error}",
                archive.display()
            )
        })?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let defines_symbol = stdout
        .lines()
        .any(|line| nm_line_defines_symbol(line, symbol));
    if defines_symbol {
        return Ok(true);
    }
    if !output.status.success() {
        return Err(format!(
            "failed to inspect cross-target archive {} with nm; symbol `{}` not found in nm output\nstderr:\n{}",
            archive.display(),
            symbol,
            String::from_utf8_lossy(&output.stderr)
        ));
    }

    Ok(false)
}

#[cfg(any(target_os = "macos", target_os = "linux"))]
fn nm_line_defines_symbol(line: &str, symbol: &str) -> bool {
    let fields: Vec<&str> = line.split_whitespace().collect();
    let Some(name) = fields.last() else {
        return false;
    };
    let darwin_name = format!("_{symbol}");
    if *name != symbol && *name != darwin_name {
        return false;
    }

    let Some(kind) = fields.get(fields.len().saturating_sub(2)) else {
        return false;
    };
    !matches!(*kind, "U" | "u")
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
            // Windows GNU ABI coverage is scoped to x86_64-pc-windows-gnu rather
            // than widening to both GNU and MSVC at once.
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

/// A Hew program with a Serializable actor message registers its cross-node
/// codec from a program-start constructor (`hew_module_init_actor_codecs`). That
/// constructor only runs if it lands in the section the target's startup
/// mechanism actually walks: ELF `.init_array`, Mach-O `__mod_init_func`.
///
/// The bug this guards: hew's `TargetMachine` is built through the LLVM-C API,
/// whose `UseInitArray` option defaults to `false` with no C setter (clang/llc
/// set it `true`). With `UseInitArray == false` the `AsmPrinter` lowers
/// `@llvm.global_ctors` into the LEGACY ELF `.ctors` section, which modern
/// glibc/musl startup does not walk and `--gc-sections` strips — so on Linux the
/// codec registration never ran and every cross-node send failed closed with
/// "no serialization codec registered". macOS was unaffected (Mach-O has only
/// `__mod_init_func`). The fix emits the ctor pointers directly into
/// `.init_array` + `@llvm.used` on ELF.
///
/// Teeth: assert the ELF object carries an EXACTLY-named `.init_array` section
/// (not the legacy `.ctors`, and not the `,.init_array` an inkwell host-cfg
/// quirk produces when cross-building from macOS), and the Mach-O object carries
/// `__mod_init_func`. Section presence, not `count > 0`.
#[test]
fn serializable_actor_emits_target_walked_ctor_section() {
    struct Case {
        triple: &'static str,
        output_suffix: &'static str,
        // The section name the target's startup mechanism walks for ctors.
        expected_ctor_section: &'static str,
    }

    let cases = [
        Case {
            triple: "arm64-apple-darwin",
            output_suffix: ".o",
            expected_ctor_section: "__mod_init_func",
        },
        Case {
            triple: "x86_64-unknown-linux-gnu",
            output_suffix: ".o",
            expected_ctor_section: ".init_array",
        },
    ];

    // Minimal program that forces a cross-node codec ctor: a Serializable record
    // message + an actor with a receive handler taking it. The codec module-init
    // pass seeds a codec for the handler's message type, emitting the ctor.
    let src = "\
record Ping { seq: i64 }
actor Echo {
    receive fn handle(msg: Ping) -> i64 { msg.seq }
}
fn main() {}
";

    for case in cases {
        let dir = workspace();
        let source = dir.path().join("ctor_section.hew");
        std::fs::write(&source, src).expect("write source");
        let object_path = dir
            .path()
            .join(format!("ctor_section{}", case.output_suffix));

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
        let section_names: Vec<String> = file
            .sections()
            .map(|s| String::from_utf8_lossy(s.name_bytes().unwrap_or_default()).into_owned())
            .collect();

        assert!(
            section_names
                .iter()
                .any(|n| n == case.expected_ctor_section),
            "{}: expected a ctor section named exactly `{}`; got sections: {:?}",
            case.triple,
            case.expected_ctor_section,
            section_names,
        );

        if case.triple.contains("linux") {
            // Negative guards: the legacy/mangled section names that mean the
            // constructor would NOT run on Linux must not appear.
            assert!(
                !section_names.iter().any(|n| n == ".ctors"),
                "linux object must not use the legacy `.ctors` section (it is not \
                 walked by modern startup and is GC-stripped); sections: {section_names:?}"
            );
            assert!(
                !section_names.iter().any(|n| n.contains(',')),
                "linux object must not carry a comma-mangled section name (the \
                 inkwell macOS-host `set_section` quirk would turn `.init_array` \
                 into `,.init_array`); sections: {section_names:?}"
            );
        }
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
    let run = support::run_bounded_command(
        Command::new(&output_path),
        format!("run {}", output_path.display()),
    );
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

// ── Linux cross-arch proof ─────────────────────────────────────────────────

#[cfg(target_os = "linux")]
fn require_linux_cross_target_library(cross_triple: &str) {
    if let Err(error) =
        LINUX_CROSS_LIB_STATUS.get_or_init(|| bootstrap_linux_cross_target_library(cross_triple))
    {
        panic!("{error}");
    }
}

/// Build `hew-lib` for the cross-arch Linux target in its native Cargo target
/// directory so `find_hew_lib` can probe `target/<triple>/<profile>/libhew.a`
/// directly without extra staging. The shared bootstrap validates that the
/// archive exports current codegen-emitted runtime ABI symbols before reuse.
#[cfg(target_os = "linux")]
fn bootstrap_linux_cross_target_library(target: &str) -> Result<(), String> {
    bootstrap_cross_target_library(target)
}

/// Linux same-OS cross-arch native link proof (issue #254 Phase 3).
///
/// On a Linux host, `hew build --target <cross-arch-linux>` must produce a
/// native ELF binary for the cross arch rather than falling through to the
/// "can emit objects" rejection path.
///
/// The test skips when the cross-arch sysroot (`/usr/<arch-tuple>`) is absent
/// so that CI hosts without `gcc-<arch>-linux-gnu` installed do not fail
/// mysteriously.  When the sysroot is present, the test fully proves the lane:
///   1. `hew build` exits 0 with a cross-arch ELF output.
///   2. The output is parseable as ELF with the correct architecture.
///   3. The build did not fall back to the `--emit-obj` rejection path.
#[cfg(target_os = "linux")]
#[test]
fn native_link_linux_cross_arch_produces_foreign_elf_binary() {
    let (cross_target, expected_arch) = linux_cross_target();

    // Skip when the cross sysroot is absent — install gcc-<arch>-linux-gnu to
    // enable this test in a local or CI environment.
    if linux_cross_sysroot_path(cross_target).is_none() {
        eprintln!(
            "SKIP native_link_linux_cross_arch_produces_foreign_elf_binary: \
             cross sysroot for {cross_target} not found; \
             install gcc-aarch64-linux-gnu or gcc-x86-64-linux-gnu to run"
        );
        return;
    }

    require_linux_cross_target_library(cross_target);

    let dir = workspace();
    let source = write_main(dir.path());
    let output_path = dir.path().join("hello-cross-linux");

    let result = Command::new(hew_binary())
        .args([
            "build",
            source.to_str().expect("source path"),
            "--target",
            cross_target,
            "-o",
            output_path.to_str().expect("output path"),
        ])
        .current_dir(dir.path())
        .output()
        .expect("run hew build");

    assert!(
        result.status.success(),
        "hew build --target {cross_target} failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&result.stdout),
        String::from_utf8_lossy(&result.stderr),
    );
    assert!(output_path.exists(), "output binary not created");

    let data = std::fs::read(&output_path).expect("read binary");
    let obj = object::File::parse(data.as_slice()).expect("parse binary");
    assert_eq!(obj.format(), BinaryFormat::Elf, "expected ELF binary");
    assert_eq!(obj.architecture(), expected_arch, "wrong cross-arch output");

    let stderr = String::from_utf8_lossy(&result.stderr);
    assert!(
        !stderr.contains("--emit-obj"),
        "cross-arch Linux native link regressed to fail-closed path:\n{stderr}",
    );
}

/// `hew build --target <cross-arch-linux>` without a `-o` path must emit an
/// ELF object with the cross-arch header, not a reject from the link gate.
///
/// This verifies that the fail-closed rejection error is no longer triggered
/// for Linux same-OS cross-arch builds after issue #254 Phase 3.
#[cfg(target_os = "linux")]
#[test]
fn linux_cross_arch_passes_link_gate_not_rejected() {
    let (cross_target, _) = linux_cross_target();

    // Confirm the target is accepted by the link gate (can_link_with_host_tools).
    // We don't need the cross sysroot for this check — the error under test
    // happens before any sysroot is consulted.  We verify the *error message*
    // from a failed link attempt does NOT contain "can emit objects" (the
    // gate rejection text).
    //
    // If the cross sysroot or hew-lib is absent the link will fail later
    // (e.g. "cannot find libhew.a") — but that is a different error than the
    // pre-gate rejection we're proving against here.
    let dir = workspace();
    let source = write_main(dir.path());
    let output_path = dir.path().join("cross-link-gate-check");

    let result = Command::new(hew_binary())
        .args([
            "build",
            source.to_str().expect("source path"),
            "--target",
            cross_target,
            "-o",
            output_path.to_str().expect("output path"),
        ])
        .current_dir(dir.path())
        .output()
        .expect("run hew build");

    let stderr = String::from_utf8_lossy(&result.stderr);

    // The pre-gate rejection contains "can emit objects" and "--emit-obj".
    // After Phase 3, this MUST NOT appear for same-OS cross-arch Linux targets.
    assert!(
        !stderr.contains("can emit objects"),
        "Linux cross-arch target {cross_target} still being rejected at the link gate:\n{stderr}",
    );
}
