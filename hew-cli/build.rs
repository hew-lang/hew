use std::env;
use std::ffi::OsString;
use std::path::{Path, PathBuf};
use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed=../hew-codegen/CMakeLists.txt");
    println!("cargo:rerun-if-changed=../hew-codegen/include");
    println!("cargo:rerun-if-changed=../hew-codegen/src");
    println!("cargo:rerun-if-env-changed=LLVM_DIR");
    println!("cargo:rerun-if-env-changed=MLIR_DIR");
    println!("cargo:rerun-if-env-changed=LLVM_PREFIX");
    println!("cargo:rerun-if-env-changed=CC");
    println!("cargo:rerun-if-env-changed=CXX");
    println!("cargo:rerun-if-env-changed=HEW_EMBED_STATIC");

    let repo_dir = Path::new("..");
    emit_git_metadata(repo_dir);

    let out_dir = PathBuf::from(env::var_os("OUT_DIR").expect("OUT_DIR should exist"));
    let build_dir = out_dir.join("hew-codegen-cmake");
    let source_dir = repo_dir.join("hew-codegen");
    let target = env::var("TARGET").expect("TARGET should exist");
    let target_env = target.replace('-', "_");

    println!("cargo:rerun-if-env-changed=CC_{target_env}");
    println!("cargo:rerun-if-env-changed=CXX_{target_env}");

    let embed_static = env_flag("HEW_EMBED_STATIC");
    let llvm_prefix = env::var("LLVM_PREFIX")
        .ok()
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
        // CMake configure failed — LLVM/MLIR not installed.  This is fine for
        // `cargo clippy` / `cargo check` which only type-check; they don't
        // link, so missing native symbols won't matter.  A real `cargo build`
        // will fail at link time with clear missing-symbol errors.
        println!("cargo:warning=LLVM/MLIR not found — skipping embedded codegen build");
        return;
    }

    let cargo_link_file = build_dir.join("hew_embedded_codegen.cargo");
    let cargo_link_lines = std::fs::read_to_string(&cargo_link_file)
        .unwrap_or_else(|error| panic!("failed to read {}: {error}", cargo_link_file.display()));
    for line in cargo_link_lines
        .lines()
        .filter(|line| !line.trim().is_empty())
    {
        println!("{line}");
    }
}

/// Configure and build the embedded codegen library.
/// Returns `true` on success, `false` if cmake configure failed (LLVM not found).
fn configure_and_build(
    source_dir: &Path,
    build_dir: &Path,
    target: &str,
    embed_static: bool,
    llvm_prefix: Option<&PathBuf>,
    cc: Option<OsString>,
    cxx: Option<OsString>,
) -> bool {
    // Reject unsupported cross-compilation: the embedded C++ codegen must be
    // built for the same architecture as the Rust target.  CMake builds for the
    // host by default, so a host≠target mismatch would silently embed a
    // wrong-arch static library.  Native builds and macOS universal builds
    // (detected via CARGO_CFG_TARGET_OS) are fine.
    let host = env::var("HOST").unwrap_or_default();
    if host != target {
        let host_arch = host.split('-').next().unwrap_or("");
        let target_arch = target.split('-').next().unwrap_or("");
        assert!(
            host_arch == target_arch,
            "Cross-compiling hew-cli from {host} to {target} is not supported: \
             the embedded C++ codegen would be built for the host architecture ({host_arch}) \
             instead of the target ({target_arch}).  Build natively on the target platform \
             or use CI runners that match the target architecture."
        );
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
            println!("cargo:warning={description} failed (LLVM/MLIR may not be installed)");
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
    if let Some(git_dir) = git_stdout(repo_dir, &["rev-parse", "--git-dir"]) {
        let git_dir = repo_dir.join(git_dir);
        for path in [git_dir.join("HEAD"), git_dir.join("index")] {
            println!("cargo:rerun-if-changed={}", path.display());
        }
    }

    let git_hash = git_stdout(repo_dir, &["rev-parse", "--short", "HEAD"]).unwrap_or_default();
    println!("cargo:rustc-env=HEW_GIT_HASH={git_hash}");

    let is_dirty =
        git_stdout(repo_dir, &["status", "--porcelain"]).is_some_and(|status| !status.is_empty());
    println!("cargo:rustc-env=HEW_GIT_DIRTY={is_dirty}");
}

fn git_stdout(repo_dir: &Path, args: &[&str]) -> Option<String> {
    let output = Command::new("git")
        .current_dir(repo_dir)
        .args(args)
        .output()
        .ok()?;
    if !output.status.success() {
        return None;
    }
    Some(String::from_utf8_lossy(&output.stdout).trim().to_string())
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
