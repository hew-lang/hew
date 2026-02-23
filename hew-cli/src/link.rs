//! Linker invocation: drives `cc` (or `wasm-ld` for WASM targets) to produce
//! the final binary from the object file emitted by `hew-codegen` and the Hew
//! runtime library.

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

    let runtime_lib = find_runtime_lib("libhew_runtime.a")?;

    // Prevent output paths starting with '-' from being interpreted as cc flags
    let safe_output = if output_path.starts_with('-') {
        format!("./{output_path}")
    } else {
        output_path.to_string()
    };

    // Prefer clang (consistent with the LLVM/MLIR toolchain), fall back to cc.
    let compiler = if has_tool("clang") { "clang" } else { "cc" };
    let mut cmd = std::process::Command::new(compiler);

    // Use lld when available — ~20x faster than GNU ld for large static libs
    if has_tool("ld.lld") {
        cmd.arg("-fuse-ld=lld");
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
    #[cfg(target_os = "linux")]
    {
        cmd.arg("-Wl,--gc-sections");
        if !debug {
            cmd.arg("-Wl,--strip-all");
        }
    }

    #[cfg(target_os = "macos")]
    {
        cmd.arg("-Wl,-dead_strip");
        if !debug {
            cmd.arg("-Wl,-x");
        }
    }

    // Platform-specific libraries
    #[cfg(target_os = "linux")]
    cmd.args(["-lpthread", "-lm", "-ldl", "-lrt"]);

    #[cfg(target_os = "macos")]
    cmd.args([
        "-lpthread",
        "-lm",
        "-framework",
        "CoreFoundation",
        "-framework",
        "Security",
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
pub fn find_package_libs(modules: &[String]) -> Vec<String> {
    let exe = match std::env::current_exe() {
        Ok(e) => e,
        Err(_) => return vec![],
    };
    let exe_dir = exe.parent().expect("exe should have a parent directory");

    let mut libs = Vec::new();
    for module_path in modules {
        if let Some(lib_name) = module_to_staticlib_name(module_path) {
            let candidates = [
                exe_dir.join("../lib").join(&lib_name),
                exe_dir.join(&lib_name),
                exe_dir.join("../../target/release").join(&lib_name),
                exe_dir.join("../../target/debug").join(&lib_name),
            ];
            for c in &candidates {
                if c.exists() {
                    if let Ok(p) = c.canonicalize() {
                        libs.push(p.display().to_string());
                        break;
                    }
                }
            }
        }
    }
    libs
}

/// Map a module path to its expected staticlib filename.
///
/// `"std::encoding::hex"` → `"libhew_std_encoding_hex.a"`
fn module_to_staticlib_name(module_path: &str) -> Option<String> {
    let crate_name = match module_path {
        "std::encoding::base64" => "hew_std_encoding_base64",
        "std::encoding::csv" => "hew_std_encoding_csv",
        "std::encoding::json" => "hew_std_encoding_json",
        "std::encoding::markdown" => "hew_std_encoding_markdown",
        "std::encoding::msgpack" => "hew_std_encoding_msgpack",
        "std::encoding::protobuf" => "hew_std_encoding_protobuf",
        "std::encoding::toml" => "hew_std_encoding_toml",
        "std::encoding::yaml" => "hew_std_encoding_yaml",
        "std::crypto::crypto" => "hew_std_crypto_crypto",
        "std::crypto::jwt" => "hew_std_crypto_jwt",
        "std::crypto::password" => "hew_std_crypto_password",
        "std::net::http" => "hew_std_net_http",
        "std::net::ipnet" => "hew_std_net_ipnet",
        "std::net::smtp" => "hew_std_net_smtp",
        "std::net::url" => "hew_std_net_url",
        "std::net::websocket" => "hew_std_net_websocket",
        "std::time::cron" => "hew_std_time_cron",
        "std::time::datetime" => "hew_std_time_datetime",
        "std::text::regex" => "hew_std_text_regex",
        "std::text::semver" => "hew_std_text_semver",
        "std::encoding::compress" => "hew_std_encoding_compress",
        "std::misc::uuid" => "hew_std_misc_uuid",
        "std::misc::log" => "hew_std_misc_log",
        "ecosystem::db::postgres" => "hew_ecosystem_db_postgres",
        "ecosystem::db::redis" => "hew_ecosystem_db_redis",
        "ecosystem::db::sqlite" => "hew_ecosystem_db_sqlite",
        "ecosystem::misc::glob" => "hew_ecosystem_misc_glob",
        _ => return None,
    };
    Some(format!("lib{crate_name}.a"))
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

    // Try LLVM installation directories
    for version in &["21", "19", "18", "17"] {
        let path = format!("/usr/lib/llvm-{version}/bin/wasm-ld");
        if std::path::Path::new(&path).exists() {
            return Ok(path);
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
