//! Linker invocation: drives `cc` (or `wasm-ld` for WASM targets) to produce
//! the final binary from the object file emitted by `hew-codegen` and the Hew
//! runtime library.

fn runtime_lib_name() -> &'static str {
    if cfg!(target_os = "windows") {
        "hew_runtime.lib"
    } else {
        "libhew_runtime.a"
    }
}

/// Link an object file with the Hew runtime into a native executable (or `.wasm`
/// binary when the target triple indicates a WASM platform).
///
/// When `debug` is `true`, debug symbols are preserved (no stripping).
///
/// # Errors
///
/// Returns a human-readable message when the linker cannot be invoked or exits
/// with a non-zero status.
pub fn link_executable(
    object_path: &str,
    output_path: &str,
    target: Option<&str>,
    debug: bool,
    extra_libs: &[String],
) -> Result<(), String> {
    if target.is_some_and(|t| t.starts_with("wasm32")) {
        return link_wasm(object_path, output_path, target.unwrap());
    }

    let runtime_lib = find_runtime_lib(runtime_lib_name())?;

    // Prevent output paths starting with '-' from being interpreted as cc flags
    let safe_output = if output_path.starts_with('-') {
        format!("./{output_path}")
    } else {
        output_path.to_string()
    };

    // Prefer clang (consistent with the LLVM/MLIR toolchain), fall back to cc.
    #[cfg(not(target_os = "windows"))]
    let compiler = if has_tool("clang") { "clang" } else { "cc" };
    #[cfg(target_os = "windows")]
    let compiler = if has_tool("clang") {
        "clang"
    } else {
        return Err("Error: clang not found. Install LLVM to link Hew programs.".into());
    };

    let mut cmd = std::process::Command::new(compiler);

    // Use lld when available — ~20x faster than GNU ld for large static libs
    #[cfg(not(target_os = "windows"))]
    if has_tool("ld.lld") {
        cmd.arg("-fuse-ld=lld");
    }
    #[cfg(target_os = "windows")]
    if has_tool("lld-link") {
        cmd.arg("-fuse-ld=lld-link");
    }

    cmd.arg(object_path).arg(&runtime_lib);

    // Link per-package staticlibs (e.g., libhew_std_encoding_hex.a)
    for lib in extra_libs {
        cmd.arg(lib);
    }

    cmd.arg("-o").arg(&safe_output);

    if debug {
        cmd.arg("-g");
    }

    // Dead-code elimination: discard unreferenced sections from the archive.
    // Skip stripping when building for debug so symbols are preserved.
    #[cfg(all(unix, not(target_os = "macos")))]
    {
        cmd.arg("-Wl,--gc-sections");
        if !debug {
            cmd.arg("-Wl,--strip-all");
        }
        // When linking stdlib package staticlibs alongside the runtime, both
        // archives contain embedded copies of shared dependencies (e.g.
        // hew_cabi) because Cargo bakes all transitive deps into each
        // staticlib. Allow the linker to pick one copy and discard the rest.
        if !extra_libs.is_empty() {
            cmd.arg("-Wl,--allow-multiple-definition");
        }
    }

    #[cfg(target_os = "macos")]
    {
        cmd.arg("-Wl,-dead_strip");
        if !debug {
            cmd.arg("-Wl,-x");
        }
        // Same rationale as the ELF `--allow-multiple-definition` above:
        // Cargo staticlibs embed shared dependencies, producing duplicate
        // symbols when linked together. `-Wl,-multiply_defined,suppress`
        // tells ld64 to silently pick one definition.
        if !extra_libs.is_empty() {
            cmd.arg("-Wl,-multiply_defined,suppress");
        }
    }

    #[cfg(target_os = "windows")]
    {
        // Clang defaults to the static CRT (libcmt) but the Rust-compiled
        // runtime uses the DLL CRT (msvcrt). Override the default so the
        // CRT linkage matches.
        cmd.args(["-Wl,/NODEFAULTLIB:libcmt", "-Wl,/DEFAULTLIB:msvcrt"]);

        // MSVC-style dead-code elimination via clang → lld-link/link.exe
        if !extra_libs.is_empty() {
            cmd.arg("-Wl,/FORCE:MULTIPLE");
        }
    }

    // Platform-specific libraries
    #[cfg(target_os = "linux")]
    cmd.args(["-lpthread", "-lm", "-ldl", "-lrt"]);

    // FreeBSD: dlopen/clock_gettime are in libc — no -ldl or -lrt needed.
    #[cfg(target_os = "freebsd")]
    cmd.args(["-lpthread", "-lm"]);

    #[cfg(target_os = "macos")]
    cmd.args([
        "-lpthread",
        "-lm",
        "-framework",
        "CoreFoundation",
        "-framework",
        "Security",
    ]);

    #[cfg(target_os = "windows")]
    cmd.args([
        // The Rust runtime references `printf` via __declspec(dllimport), but
        // the UCRT inlines printf through __stdio_common_vfprintf. The legacy
        // definitions library provides the classic __imp_printf symbol.
        "-llegacy_stdio_definitions",
        // Windows system libraries required by the runtime
        "-lws2_32",
        "-luserenv",
        "-lbcrypt",
        "-lntdll",
        "-ladvapi32",
    ]);

    let output = cmd
        .output()
        .map_err(|e| format!("Error: cannot invoke linker: {e}"))?;

    // Forward raw linker output so the user sees the full error.
    let stderr_text = String::from_utf8_lossy(&output.stderr);
    if !stderr_text.is_empty() {
        eprint!("{stderr_text}");
    }

    if !output.status.success() {
        for hint in diagnose_linker_errors(&stderr_text) {
            eprintln!("{hint}");
        }
        return Err("linking failed".into());
    }

    Ok(())
}

/// Link a WASM object file using `wasm-ld`.
fn link_wasm(object_path: &str, output_path: &str, target: &str) -> Result<(), String> {
    let wasm_ld = find_wasm_ld()?;

    let mut cmd = std::process::Command::new(&wasm_ld);

    // The program object file comes before libraries so its symbols (including
    // `main`) are visible and libraries can satisfy its undefined references.
    cmd.arg(object_path);

    // Link the WASM runtime (hew-runtime compiled for wasm32-wasip1).
    for rt_lib in find_wasm_runtime_libs(target) {
        cmd.arg(&rt_lib);
    }

    // Link WASI libc from Rust's sysroot to provide malloc, free, etc.
    if let Some(wasi_libc) = find_wasi_libc(target) {
        cmd.arg(&wasi_libc);
    }

    cmd.arg("-o")
        .arg(output_path)
        // Allow unresolved symbols for runtime functions not yet in the WASM
        // runtime (they become WASM imports from the `env` module).
        .arg("--allow-undefined")
        // We provide `_start` in the runtime, not via WASI CRT1.
        .arg("--no-entry")
        .arg("--export=_start");

    let status = cmd
        .status()
        .map_err(|e| format!("Error: cannot invoke wasm-ld: {e}"))?;

    if !status.success() {
        return Err("WASM linking failed".into());
    }

    Ok(())
}

fn find_wasm_runtime_libs(target: &str) -> Vec<String> {
    let Ok(exe) = std::env::current_exe() else {
        return Vec::new();
    };
    let exe_dir = exe.parent().expect("exe should have a parent directory");

    // Map wasm32-wasi to wasm32-wasip1 (modern Rust toolchain name)
    let rust_target = if target == "wasm32-wasi" {
        "wasm32-wasip1"
    } else {
        target
    };

    // libhew_runtime.a compiled for wasm32-wasip1
    let candidates = [
        exe_dir.join(format!(
            "../../target/{rust_target}/release/libhew_runtime.a"
        )),
        exe_dir.join(format!("../../target/{rust_target}/debug/libhew_runtime.a")),
        exe_dir.join(format!("../lib/{rust_target}/libhew_runtime.a")),
    ];

    for c in &candidates {
        if c.exists() {
            return vec![c
                .canonicalize()
                .unwrap_or_else(|_| c.clone())
                .display()
                .to_string()];
        }
    }

    Vec::new()
}

/// Locate `libc.a` from Rust's WASI sysroot so `malloc`/`free`/etc. resolve.
fn find_wasi_libc(target: &str) -> Option<String> {
    let rust_target = if target == "wasm32-wasi" {
        "wasm32-wasip1"
    } else {
        target
    };

    // `rustc --print sysroot` gives the toolchain root.
    // WASI libc lives at <sysroot>/lib/rustlib/<target>/lib/self-contained/libc.a
    let output = std::process::Command::new("rustc")
        .args(["--print", "sysroot"])
        .output()
        .ok()?;

    let sysroot = String::from_utf8(output.stdout).ok()?.trim().to_owned();
    let libc_path = std::path::PathBuf::from(&sysroot)
        .join("lib/rustlib")
        .join(rust_target)
        .join("lib/self-contained/libc.a");

    if libc_path.exists() {
        Some(libc_path.display().to_string())
    } else {
        None
    }
}

fn find_runtime_lib(name: &str) -> Result<String, String> {
    let exe = std::env::current_exe().map_err(|e| format!("cannot find self: {e}"))?;
    let exe_dir = exe.parent().expect("exe should have a parent directory");

    let candidates = [
        exe_dir.join("../lib").join(name),
        exe_dir.join("../lib/hew").join(name), // /usr/lib/hew/ (Debian/Ubuntu)
        exe_dir.join("../lib64/hew").join(name), // /usr/lib64/hew/ (Fedora/RHEL)
        exe_dir.join(name), // same dir as the binary (target/debug/ or target/release/)
        exe_dir.join("../../target/release").join(name),
        exe_dir.join("../../target/debug").join(name),
        exe_dir
            .join("../../target/wasm32-wasip1/release")
            .join(name),
        exe_dir.join("../../target/wasm32-wasip1/debug").join(name),
        exe_dir.join("../../hew-runtime/target/release").join(name),
    ];

    for c in &candidates {
        if c.exists() {
            return Ok(c
                .canonicalize()
                .unwrap_or_else(|_| c.clone())
                .display()
                .to_string());
        }
    }

    Err(format!(
        "Error: cannot find {name}. Build with: cargo build -p hew-runtime"
    ))
}

/// Map a set of imported module paths to their package staticlib paths.
///
/// For each module that has a standalone package crate (e.g.,
/// `std::encoding::hex` → `libhew_std_encoding_hex.a`), searches
/// the standard candidate directories and returns the found paths.
pub fn find_package_libs(modules: &[String], pkg_path: Option<&std::path::Path>) -> Vec<String> {
    let Ok(exe) = std::env::current_exe() else {
        return vec![];
    };
    let exe_dir = exe.parent().expect("exe should have a parent directory");

    let mut libs = Vec::new();
    for module_path in modules {
        let lib_name = module_to_staticlib_name(module_path);
        let mut candidates: Vec<std::path::PathBuf> = vec![
            exe_dir.join("../lib").join(&lib_name),
            exe_dir.join("../lib/hew").join(&lib_name),
            exe_dir.join("../lib64/hew").join(&lib_name),
            exe_dir.join(&lib_name),
            exe_dir.join("../../target/release").join(&lib_name),
            exe_dir.join("../../target/debug").join(&lib_name),
        ];
        // When --pkg-path points to a local ecosystem checkout, also search
        // <pkg_path>/target/{debug,release}/ for the compiled staticlibs.
        if let Some(pkg) = pkg_path {
            candidates.push(pkg.join("target/debug").join(&lib_name));
            candidates.push(pkg.join("target/release").join(&lib_name));
        }
        for c in &candidates {
            if c.exists() {
                if let Ok(p) = c.canonicalize() {
                    libs.push(p.display().to_string());
                    break;
                }
            }
        }
    }
    libs
}

/// Map a module path to its expected staticlib filename.
///
/// `"std::encoding::hex"` → `"libhew_std_encoding_hex.a"`
fn module_to_staticlib_name(module_path: &str) -> String {
    let crate_name = format!("hew_{}", module_path.replace("::", "_"));
    if cfg!(target_os = "windows") {
        format!("{crate_name}.lib")
    } else {
        format!("lib{crate_name}.a")
    }
}

/// Parse linker stderr for unresolved `hew_*` symbols and return human-readable
/// hints pointing to the runtime feature that must be enabled.
pub(crate) fn diagnose_linker_errors(stderr: &str) -> Vec<String> {
    let mut seen: std::collections::BTreeSet<(&'static str, &'static str)> =
        std::collections::BTreeSet::new();

    for line in stderr.lines() {
        if let Some(sym) = extract_undefined_hew_symbol(line) {
            if let Some(hint) = symbol_to_feature_hint(sym) {
                seen.insert(hint);
            }
        }
    }

    seen.into_iter()
        .map(|(module, feature)| {
            format!(
                "hint: The `{module}` module requires the `{feature}` runtime feature.\n      \
                 Rebuild the runtime with: cargo build -p hew-runtime --features {feature}"
            )
        })
        .collect()
}

/// Extract a `hew_`-prefixed symbol name from a linker error line, or `None`
/// if the line does not contain an undefined `hew_*` reference.
fn extract_undefined_hew_symbol(line: &str) -> Option<&str> {
    // GNU ld / lld: undefined reference to `hew_foo_bar'
    if let Some(pos) = line.find("undefined reference to `hew_") {
        let start = pos + "undefined reference to `".len();
        let rest = &line[start..];
        let end = rest.find('\'').unwrap_or(rest.len());
        return Some(&rest[..end]);
    }
    // macOS ld64: "_hew_foo_bar", referenced from:
    if let Some(pos) = line.find("\"_hew_") {
        let start = pos + "\"_".len();
        let rest = &line[start..];
        let end = rest.find('"').unwrap_or(rest.len());
        return Some(&rest[..end]);
    }
    // MSVC lld-link: unresolved external symbol hew_foo_bar
    if let Some(pos) = line.find("unresolved external symbol hew_") {
        let start = pos + "unresolved external symbol ".len();
        let rest = &line[start..];
        let end = rest
            .find(|c: char| !c.is_alphanumeric() && c != '_')
            .unwrap_or(rest.len());
        return Some(&rest[..end]);
    }
    None
}

/// Map a `hew_`-prefixed symbol name to a `(module_name, feature_name)` pair
/// describing which runtime feature is needed to resolve it.
fn symbol_to_feature_hint(symbol: &str) -> Option<(&'static str, &'static str)> {
    if symbol.starts_with("hew_json_") {
        return Some(("json", "serialization"));
    }
    if symbol.starts_with("hew_yaml_") {
        return Some(("yaml", "serialization"));
    }
    if symbol.starts_with("hew_postgres_") {
        return Some(("postgres", "db"));
    }
    if symbol.starts_with("hew_mysql_") {
        return Some(("mysql", "db"));
    }
    if symbol.starts_with("hew_redis_") {
        return Some(("redis", "db"));
    }
    if symbol.starts_with("hew_http_") {
        return Some(("http", "http"));
    }
    if symbol.starts_with("hew_grpc_") {
        return Some(("grpc", "http"));
    }
    if symbol.starts_with("hew_tcp_") {
        return Some(("tcp", "net"));
    }
    None
}

fn find_wasm_ld() -> Result<String, String> {
    // Try bare `wasm-ld` first, then versioned variants
    for name in &["wasm-ld", "wasm-ld-21", "wasm-ld-19"] {
        if has_tool(name) {
            return Ok((*name).to_string());
        }
    }

    // Try LLVM installation directories (Linux apt.llvm.org)
    for version in &["22", "21", "19", "18", "17"] {
        let path = format!("/usr/lib/llvm-{version}/bin/wasm-ld");
        if std::path::Path::new(&path).exists() {
            return Ok(path);
        }
    }

    // Try FreeBSD pkg paths
    for version in &["22", "21", "20", "19"] {
        let path = format!("/usr/local/llvm{version}/bin/wasm-ld");
        if std::path::Path::new(&path).exists() {
            return Ok(path);
        }
    }

    // Try Windows LLVM installation
    #[cfg(target_os = "windows")]
    {
        let path = "C:/Program Files/LLVM/bin/wasm-ld.exe";
        if std::path::Path::new(path).exists() {
            return Ok(path.to_string());
        }
    }

    Err("Error: cannot find wasm-ld. Install LLVM or add wasm-ld to PATH".into())
}

fn has_tool(name: &str) -> bool {
    std::process::Command::new(name)
        .arg("--version")
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .is_ok_and(|s| s.success())
}
