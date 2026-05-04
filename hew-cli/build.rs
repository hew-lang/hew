use std::env;
use std::ffi::OsString;
use std::fmt::Write as _;
use std::path::{Path, PathBuf};
use std::process::Command;

const UNKNOWN_GIT_METADATA: &str = "unknown";

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=../hew-codegen/CMakeLists.txt");
    println!("cargo:rerun-if-changed=../hew-codegen/include");
    println!("cargo:rerun-if-changed=../hew-codegen/src");
    println!("cargo:rustc-check-cfg=cfg(hew_embedded_codegen)");
    println!("cargo:rerun-if-env-changed=LLVM_DIR");
    println!("cargo:rerun-if-env-changed=MLIR_DIR");
    println!("cargo:rerun-if-env-changed=LLVM_PREFIX");
    println!("cargo:rerun-if-env-changed=CC");
    println!("cargo:rerun-if-env-changed=CXX");
    println!("cargo:rerun-if-env-changed=HEW_EMBED_STATIC");

    let repo_dir = Path::new("..");
    emit_git_metadata(repo_dir);

    let out_dir = PathBuf::from(
        env::var_os("OUT_DIR").expect("invariant: cargo sets OUT_DIR for build scripts"),
    );
    let build_dir = out_dir.join("hew-codegen-cmake");
    let source_dir = repo_dir.join("hew-codegen");
    let target = env::var("TARGET").expect("invariant: cargo sets TARGET for build scripts");
    let target_env = target.replace('-', "_");

    println!("cargo:rerun-if-env-changed=CC_{target_env}");
    println!("cargo:rerun-if-env-changed=CXX_{target_env}");

    let embed_static = env_flag("HEW_EMBED_STATIC");
    let llvm_prefix_env = env::var_os("LLVM_PREFIX");
    let llvm_dir_env = env::var_os("LLVM_DIR");
    let mlir_dir_env = env::var_os("MLIR_DIR");
    let embedded_codegen_requested = embed_static
        || llvm_prefix_env.is_some()
        || llvm_dir_env.is_some()
        || mlir_dir_env.is_some();
    let llvm_prefix = llvm_prefix_env
        .map(PathBuf::from)
        .or_else(detect_llvm_prefix);
    let cc = target_tool_env("CC", &target_env)
        .or_else(|| env::var_os("CC"))
        .or_else(|| llvm_prefix_tool(llvm_prefix.as_ref(), "clang"))
        .or_else(|| find_tool("clang"))
        .or_else(|| find_tool("cc"))
        .or_else(|| find_tool("cl"));
    let cxx = target_tool_env("CXX", &target_env)
        .or_else(|| env::var_os("CXX"))
        .or_else(|| llvm_prefix_tool(llvm_prefix.as_ref(), "clang++"))
        .or_else(|| find_tool("clang++"))
        .or_else(|| find_tool("c++"))
        .or_else(|| find_tool("cl"));

    if !configure_and_build(
        &source_dir,
        &build_dir,
        &target,
        embed_static,
        llvm_prefix.as_ref(),
        cc,
        cxx,
    ) {
        assert!(
            !embedded_codegen_requested,
            "embedded codegen configure failed even though LLVM/MLIR was explicitly requested; \
             see the cmake output above for the real error"
        );
        println!(
            "cargo:warning=embedded codegen unavailable — building without the embedded MLIR/LLVM backend"
        );
        return;
    }
    println!("cargo:rustc-cfg=hew_embedded_codegen");

    let cargo_link_file = build_dir.join("hew_embedded_codegen.cargo");
    let cargo_link_lines = std::fs::read_to_string(&cargo_link_file)
        .unwrap_or_else(|error| panic!("failed to read {}: {error}", cargo_link_file.display()));
    for line in cargo_link_lines
        .lines()
        .filter(|line| !line.trim().is_empty())
    {
        println!("{line}");
    }

    // Emit link args so that all T-class symbols (including the runtime C-ABI
    // symbols pulled in via `extern crate hew_runtime as _` in main.rs) appear
    // in the dynamic export table.  This populates the table that
    // `DynamicLibrarySearchGenerator::GetForCurrentProcess` reads via dlsym at
    // JIT session startup.
    //
    // Note: `-export_dynamic` exports all T symbols, not just the
    // kStableJitHostSymbols subset.  This is intentional for the current
    // milestone; the jit_symbol_map's filter already restricts resolution to
    // the stable set.  A narrower export surface (absoluteSymbols map) is a
    // separate future improvement.
    let target_os = env::var("CARGO_CFG_TARGET_OS").unwrap_or_default();
    if target_os == "macos" {
        println!("cargo:rustc-link-arg=-Wl,-export_dynamic");
    } else if target_os == "linux" {
        println!("cargo:rustc-link-arg=-Wl,--export-dynamic");
    }

    emit_runtime_symbol_anchor(&build_dir, &out_dir);
}

/// Parse `generated_jit_stable_symbols.h` and write `runtime_export.rs` into
/// `OUT_DIR`.  The generated file contains a `#[used]` static array that holds
/// a pointer to every symbol in `kStableJitHostSymbols`, which prevents the
/// linker (and LTO) from discarding archive members that define those symbols.
///
/// Without this anchor, normal rlib linkage uses lazy symbol resolution: only
/// archive members that satisfy an already-referenced symbol are pulled in, so
/// the 653 runtime symbols — which the Rust side never calls directly — would
/// all be DCE'd before the linker emits the binary.
fn emit_runtime_symbol_anchor(build_dir: &Path, out_dir: &Path) {
    let header = build_dir
        .join("include")
        .join("hew")
        .join("generated_jit_stable_symbols.h");

    println!("cargo:rerun-if-changed={}", header.display());

    let content = std::fs::read_to_string(&header)
        .unwrap_or_else(|error| panic!("failed to read {}: {error}", header.display()));

    // Extract symbol names from lines like: `    "hew_actor_ask",`
    let symbols: Vec<&str> = content
        .lines()
        .filter_map(|line| {
            let trimmed = line.trim();
            if trimmed.starts_with('"') && trimmed.contains("hew_") {
                // Strip leading `"` and trailing `",` or `"`
                Some(
                    trimmed
                        .trim_start_matches('"')
                        .trim_end_matches(',')
                        .trim_end_matches('"'),
                )
            } else {
                None
            }
        })
        .collect();

    assert!(
        !symbols.is_empty(),
        "generated_jit_stable_symbols.h contained no symbol entries; \
         the header format may have changed"
    );

    // Build the generated Rust source.
    let mut src = String::from(
        "// @generated — do not edit.  Regenerated by hew-cli/build.rs from\n\
         // hew-codegen/build/include/hew/generated_jit_stable_symbols.h\n\
         //\n\
         // Purpose: keep every kStableJitHostSymbols entry alive through the\n\
         // linker and LTO by referencing each symbol's address in a #[used]\n\
         // static.  Without this, lazy rlib linkage silently drops all runtime\n\
         // symbols because the Rust side never calls them directly.\n\n\
         extern \"C\" {\n",
    );

    for sym in &symbols {
        let _ = writeln!(src, "    fn {sym}();");
    }

    src.push_str(
        "}\n\n\
         // SAFETY: These are valid C-ABI function pointers exported by hew-runtime.\n\
         // We only take their addresses; we never call them through this array.\n\
         #[allow(dead_code, reason = \"anchor is only read by the linker, not by Rust code\")]\n\
         #[used]\n\
         static _RUNTIME_SYMBOL_ANCHORS: [unsafe extern \"C\" fn(); ",
    );
    src.push_str(&symbols.len().to_string());
    src.push_str("] = [\n");

    for sym in &symbols {
        let _ = writeln!(src, "    {sym},");
    }

    src.push_str("];\n");

    let out_file = out_dir.join("runtime_export.rs");
    std::fs::write(&out_file, &src)
        .unwrap_or_else(|error| panic!("failed to write {}: {error}", out_file.display()));
}

/// Configure and build the embedded codegen library.
/// Returns `true` on success, `false` if cmake configure failed.
fn configure_and_build(
    source_dir: &Path,
    build_dir: &Path,
    target: &str,
    embed_static: bool,
    llvm_prefix: Option<&PathBuf>,
    cc: Option<OsString>,
    cxx: Option<OsString>,
) -> bool {
    // Reject unsupported cross-compilation: the embedded C++ codegen backend is
    // built for the host toolchain by default, so a host≠target mismatch would
    // silently embed the wrong backend artifact.  Native builds are fine.
    let host = required_target_env("HOST");
    if let Some(message) = embedded_codegen_guard_error(&host, target) {
        panic!("{message}");
    }

    let build_type = if env::var("PROFILE").is_ok_and(|profile| profile == "release") {
        "Release"
    } else {
        "Debug"
    };

    let mut configure = Command::new("cmake");
    configure
        .arg("-S")
        .arg(source_dir)
        .arg("-B")
        .arg(build_dir)
        .arg("-G")
        .arg("Ninja")
        .arg(format!("-DCMAKE_BUILD_TYPE={build_type}"))
        .arg(format!(
            "-DHEW_STATIC_LINK={}",
            if embed_static { "ON" } else { "OFF" }
        ));

    let llvm_dir = env::var("LLVM_DIR").ok();
    let mlir_dir = env::var("MLIR_DIR").ok();

    // Canonicalize llvm_prefix for the signature so that Homebrew patch upgrades
    // (e.g. 22.1.1 → 22.1.2) bust the cached build dir even though the stable
    // symlink path (e.g. /opt/homebrew/opt/llvm@22) stays the same.
    let signature = format!(
        "target={target}\nbuild_type={build_type}\nembed_static={embed_static}\nllvm_prefix={}\nllvm_dir={}\nmlir_dir={}\ncc={}\ncxx={}\n",
        llvm_prefix.map_or_else(String::new, |path| {
            path.canonicalize().unwrap_or_else(|_| path.clone()).display().to_string()
        }),
        llvm_dir.clone().unwrap_or_default(),
        mlir_dir.clone().unwrap_or_default(),
        cc.as_ref().map_or_else(String::new, |tool| path_display(tool.clone())),
        cxx.as_ref().map_or_else(String::new, |tool| path_display(tool.clone())),
    );
    reset_build_dir_if_needed(build_dir, &signature);

    if let Some(value) = llvm_dir {
        configure.arg(format!("-DLLVM_DIR={value}"));
    } else if let Some(prefix) = llvm_prefix {
        configure.arg(format!(
            "-DLLVM_DIR={}",
            prefix.join("lib/cmake/llvm").display()
        ));
    }
    if let Some(value) = mlir_dir {
        configure.arg(format!("-DMLIR_DIR={value}"));
    } else if let Some(prefix) = llvm_prefix {
        configure.arg(format!(
            "-DMLIR_DIR={}",
            prefix.join("lib/cmake/mlir").display()
        ));
    }
    if let Some(cc) = cc {
        configure.arg(format!("-DCMAKE_C_COMPILER={}", path_display(cc)));
    }
    if let Some(cxx) = cxx {
        configure.arg(format!("-DCMAKE_CXX_COMPILER={}", path_display(cxx)));
    }

    // macOS cross-compile or native: propagate Xcode SDK sysroot
    let target_is_macos = target.contains("apple") || target.contains("darwin");
    if target_is_macos {
        if let Ok(output) = Command::new("xcrun").arg("--show-sdk-path").output() {
            if output.status.success() {
                let sysroot = String::from_utf8_lossy(&output.stdout).trim().to_string();
                if !sysroot.is_empty() {
                    configure.arg(format!("-DCMAKE_OSX_SYSROOT={sysroot}"));
                }
            }
        }
        // Static builds link libc++.a directly via cmake — no rpath needed.
        // Development builds use the shared libc++ from Homebrew LLVM.
        if !embed_static {
            if let Some(prefix) = llvm_prefix {
                let libcxx_dir = prefix.join("lib/c++");
                if libcxx_dir.exists() {
                    configure.arg(format!(
                        "-DCMAKE_EXE_LINKER_FLAGS=-L{0} -Wl,-rpath,{0}",
                        libcxx_dir.display()
                    ));
                }
            }
        }
    }

    if !try_run(&mut configure, "configure embedded codegen") {
        return false;
    }

    let mut build = Command::new("cmake");
    build
        .arg("--build")
        .arg(build_dir)
        .arg("--target")
        .arg("HewCodegenCAPI");
    run(&mut build, "build embedded codegen");
    true
}

fn run(command: &mut Command, description: &str) {
    let status = command
        .status()
        .unwrap_or_else(|error| panic!("failed to {description}: {error}"));
    assert!(status.success(), "failed to {description}");
}

/// Like `run` but returns `false` on failure instead of panicking.
fn try_run(command: &mut Command, description: &str) -> bool {
    match command.status() {
        Ok(status) if status.success() => true,
        Ok(_) => {
            println!("cargo:warning={description} failed");
            false
        }
        Err(error) => {
            println!("cargo:warning={description}: {error}");
            false
        }
    }
}

fn path_display(value: OsString) -> String {
    PathBuf::from(value).display().to_string()
}

fn emit_git_metadata(repo_dir: &Path) {
    match git_stdout(repo_dir, &["rev-parse", "--git-dir"]) {
        Ok(git_dir) => {
            let git_dir = repo_dir.join(git_dir);
            for path in [git_dir.join("HEAD"), git_dir.join("index")] {
                println!("cargo:rerun-if-changed={}", path.display());
            }
        }
        Err(error) => {
            cargo_warning(&format!(
                "failed to locate git metadata for hew-cli build: {error}"
            ));
        }
    }

    let git_hash = git_output_or_unknown(
        repo_dir,
        &["rev-parse", "--short", "HEAD"],
        "git commit hash",
    );
    println!("cargo:rustc-env=HEW_GIT_HASH={git_hash}");

    let git_dirty = match git_stdout(repo_dir, &["status", "--porcelain"]) {
        Ok(status) => {
            if status.is_empty() {
                "false".to_string()
            } else {
                "true".to_string()
            }
        }
        Err(error) => {
            cargo_warning(&format!(
                "failed to determine git dirty state for hew-cli build: {error}"
            ));
            UNKNOWN_GIT_METADATA.to_string()
        }
    };
    println!("cargo:rustc-env=HEW_GIT_DIRTY={git_dirty}");
}

fn git_output_or_unknown(repo_dir: &Path, args: &[&str], description: &str) -> String {
    match git_stdout(repo_dir, args) {
        Ok(output) if !output.is_empty() => output,
        Ok(_) => {
            cargo_warning(&format!(
                "{description} was empty during hew-cli build; using {UNKNOWN_GIT_METADATA}"
            ));
            UNKNOWN_GIT_METADATA.to_string()
        }
        Err(error) => {
            cargo_warning(&format!(
                "failed to read {description} during hew-cli build: {error}; using {UNKNOWN_GIT_METADATA}"
            ));
            UNKNOWN_GIT_METADATA.to_string()
        }
    }
}

fn git_stdout(repo_dir: &Path, args: &[&str]) -> Result<String, String> {
    let output = Command::new("git")
        .current_dir(repo_dir)
        .args(args)
        .output()
        .map_err(|error| error.to_string())?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr).trim().to_string();
        let stdout = String::from_utf8_lossy(&output.stdout).trim().to_string();
        let detail = if !stderr.is_empty() {
            stderr
        } else if !stdout.is_empty() {
            stdout
        } else {
            format!("git {:?} exited with {}", args, output.status)
        };
        return Err(detail);
    }
    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

fn cargo_warning(message: &str) {
    println!("cargo::warning={}", message.replace('\n', " "));
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
struct TargetTuple<'a> {
    arch: &'a str,
    os: &'a str,
}

fn embedded_codegen_guard_error(host: &str, target: &str) -> Option<String> {
    if host == target {
        return None;
    }

    let host_tuple = parse_target_tuple("HOST", host).unwrap_or_else(|message| panic!("{message}"));
    let target_tuple =
        parse_target_tuple("TARGET", target).unwrap_or_else(|message| panic!("{message}"));
    if host_tuple == target_tuple {
        return None;
    }

    Some(format!(
        "Cross-compiling hew-cli from {host} to {target} is not supported: \
         the embedded C++ codegen backend artifact is only available for the \
         host target tuple ({}/{}), but the requested target tuple is ({}/{}). \
         Build natively on the requested target OS/architecture or use a CI \
         runner that matches it.",
        host_tuple.arch, host_tuple.os, target_tuple.arch, target_tuple.os
    ))
}

fn parse_target_tuple<'a>(label: &str, triple: &'a str) -> Result<TargetTuple<'a>, String> {
    if triple.trim().is_empty() {
        return Err(format!(
            "environment variable {label} was empty; expected a Rust target triple such as x86_64-unknown-linux-gnu"
        ));
    }

    let parts: Vec<_> = triple.split('-').collect();
    let arch = parts.first().copied().filter(|part| !part.is_empty());
    let os = parts
        .iter()
        .skip(1)
        .copied()
        .find(|part| is_known_os_component(part))
        .or(match parts.as_slice() {
            [_, os] | [_, _, os, ..] => Some(*os),
            _ => None,
        });
    match (arch, os.filter(|part| !part.is_empty())) {
        (Some(arch), Some(os)) => Ok(TargetTuple { arch, os }),
        _ => Err(format!(
            "environment variable {label} had invalid target triple '{triple}'; expected a Rust target triple such as x86_64-unknown-linux-gnu"
        )),
    }
}

fn required_target_env(name: &str) -> String {
    match env::var(name) {
        Ok(value) if !value.trim().is_empty() => value,
        Ok(_) => panic!(
            "environment variable {name} was empty; expected a Rust target triple such as x86_64-unknown-linux-gnu"
        ),
        Err(error) => panic!(
            "environment variable {name} is required for the hew-cli build script: {error}"
        ),
    }
}

fn is_known_os_component(component: &str) -> bool {
    matches!(
        component,
        "aix"
            | "android"
            | "darwin"
            | "dragonfly"
            | "emscripten"
            | "espidf"
            | "freebsd"
            | "fuchsia"
            | "haiku"
            | "hermit"
            | "horizon"
            | "hurd"
            | "illumos"
            | "ios"
            | "linux"
            | "macos"
            | "netbsd"
            | "none"
            | "nto"
            | "nuttx"
            | "openbsd"
            | "psp"
            | "redox"
            | "solaris"
            | "teeos"
            | "trusty"
            | "uefi"
            | "visionos"
            | "vita"
            | "wasi"
            | "wasip1"
            | "wasip2"
            | "windows"
            | "xros"
    )
}

fn llvm_prefix_tool(llvm_prefix: Option<&PathBuf>, name: &str) -> Option<OsString> {
    llvm_prefix.and_then(|prefix| {
        let candidate = prefix.join("bin").join(name);
        candidate.exists().then(|| candidate.into_os_string())
    })
}

fn find_tool(name: &str) -> Option<OsString> {
    env::var_os("PATH").and_then(|path| {
        env::split_paths(&path)
            .map(|dir| dir.join(name))
            .find(|candidate| candidate.exists())
            .map(PathBuf::into_os_string)
    })
}

fn target_tool_env(name: &str, target_env: &str) -> Option<OsString> {
    env::var_os(format!("{name}_{target_env}"))
}

fn env_flag(name: &str) -> bool {
    env::var(name).ok().is_some_and(|value| {
        matches!(
            value.as_str(),
            "1" | "true" | "TRUE" | "yes" | "YES" | "on" | "ON"
        )
    })
}

fn detect_llvm_prefix() -> Option<PathBuf> {
    // Linux (apt.llvm.org) and FreeBSD well-known paths
    let static_candidates = [
        "/usr/lib/llvm-22",
        "/usr/lib/llvm-21",
        "/usr/lib/llvm-20",
        "/usr/lib/llvm-19",
        "/usr/local/llvm22-src",
        "/usr/local/llvm22",
        "/usr/local/llvm21",
        "/usr/local/llvm20",
        "/usr/local/llvm19",
    ];
    if let Some(path) = static_candidates
        .iter()
        .map(PathBuf::from)
        .find(|path| path.exists())
    {
        return Some(path);
    }

    // macOS: ask Homebrew
    if cfg!(target_os = "macos") {
        if let Ok(output) = Command::new("brew").args(["--prefix", "llvm"]).output() {
            if output.status.success() {
                let prefix = String::from_utf8_lossy(&output.stdout).trim().to_string();
                let path = PathBuf::from(&prefix);
                if path.exists() {
                    return Some(path);
                }
            }
        }
    }

    None
}

fn reset_build_dir_if_needed(build_dir: &Path, signature: &str) {
    let signature_path = build_dir.join(".hew-build-signature");
    let current = std::fs::read_to_string(&signature_path).ok();
    if current.as_deref() != Some(signature) && build_dir.exists() {
        std::fs::remove_dir_all(build_dir)
            .unwrap_or_else(|error| panic!("failed to reset embedded codegen build dir: {error}"));
    }
    std::fs::create_dir_all(build_dir)
        .unwrap_or_else(|error| panic!("failed to create embedded codegen build dir: {error}"));
    std::fs::write(signature_path, signature).unwrap_or_else(|error| {
        panic!("failed to record embedded codegen build signature: {error}")
    });
}

#[cfg(test)]
mod tests {
    use super::{embedded_codegen_guard_error, parse_target_tuple, UNKNOWN_GIT_METADATA};

    #[test]
    fn cross_compile_guard_rejects_same_arch_different_os() {
        let error = embedded_codegen_guard_error("x86_64-apple-darwin", "x86_64-unknown-linux-gnu")
            .expect("guard should reject mismatched OS targets");
        assert!(error.contains("requested target tuple is (x86_64/linux)"));
    }

    #[test]
    fn cross_compile_guard_allows_matching_arch_and_os_tuple() {
        assert_eq!(
            embedded_codegen_guard_error("x86_64-unknown-linux-gnu", "x86_64-unknown-linux-musl"),
            None
        );
    }

    #[test]
    fn target_tuple_parser_finds_wasi_os_component() {
        assert_eq!(
            parse_target_tuple("TARGET", "wasm32-unknown-unknown-wasi")
                .expect("wasi triple should parse"),
            super::TargetTuple {
                arch: "wasm32",
                os: "wasi",
            }
        );
    }

    #[test]
    fn target_tuple_parser_rejects_empty_triples() {
        let error = parse_target_tuple("HOST", "").expect_err("empty HOST should fail");
        assert!(error.contains("environment variable HOST was empty"));
    }

    #[test]
    fn git_metadata_uses_unknown_sentinel() {
        assert_eq!(UNKNOWN_GIT_METADATA, "unknown");
    }
}
