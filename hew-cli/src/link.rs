//! Linker invocation: drives `cc` (or `wasm-ld` for WASM targets) to produce
//! the final binary from the object file emitted by Hew's embedded codegen
//! backend and the combined Hew library (`libhew.a`).

use crate::target::TargetSpec;

/// Link an object file with the combined Hew library into a native executable
/// (or `.wasm` binary when the target triple indicates a WASM platform).
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
    target: &TargetSpec,
    extra_libs: &[String],
    debug: bool,
) -> Result<(), String> {
    if target.is_wasm() {
        return link_wasm(object_path, output_path, target.normalized_triple());
    }

    if !target.can_link_with_host_tools() {
        return Err(target.unsupported_native_link_error());
    }

    // Collect the target-driven link plan before touching the command builder.
    // Every platform-specific decision below (gc flags, platform libs, SDK,
    // dsymutil) derives from the *target* TargetOs via NativeLinkPlan, not
    // from the compile-time host `#[cfg]` world.
    let plan = target.native_link_plan();

    let hew_lib = find_hew_lib(target.hew_lib_name(), target.normalized_triple())?;

    // Prevent output paths starting with '-' from being interpreted as cc flags
    let safe_output = if output_path.starts_with('-') {
        format!("./{output_path}")
    } else {
        output_path.to_string()
    };

    // ── Compiler selection (host-driven) ───────────────────────────────
    // We run the linker on the host, so tool availability is a host concern.
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

    // ── lld selection (host-driven) ────────────────────────────────────
    // Use lld when available — ~20x faster than GNU ld for large static libs.
    // Which lld variant exists depends on the host toolchain installation.
    #[cfg(not(target_os = "windows"))]
    if has_tool("ld.lld") {
        cmd.arg("-fuse-ld=lld");
    }
    #[cfg(target_os = "windows")]
    if has_tool("lld-link") {
        cmd.arg("-fuse-ld=lld-link");
    }

    // Wire the explicit target triple so clang uses the right ABI, calling
    // convention, and SDK rather than inferring from the host environment.
    // On Darwin this becomes e.g. `aarch64-apple-macosx13.0`; on Linux the
    // normalized triple is passed unchanged.
    cmd.arg("-target").arg(target.linker_triple());

    cmd.arg(object_path).arg(&hew_lib);

    cmd.arg("-o").arg(&safe_output);

    if debug {
        cmd.arg("-g");
    }

    // ── Dead-code elimination (target-driven via NativeLinkPlan) ──────
    cmd.args(plan.gc_flags);
    if !debug {
        cmd.args(plan.strip_flags);
    }

    // ── Darwin SDK path (target-driven intent, host tool resolves path) ─
    // Anchor the SDK explicitly so the linker finds system frameworks even
    // when invoked from a non-standard PATH (e.g. CI without Xcode on PATH).
    // Failure is non-fatal: clang falls back to its own SDK search heuristics.
    if plan.needs_darwin_sdk {
        if let Some(sdk) = find_darwin_sdk() {
            cmd.arg("-isysroot").arg(sdk);
        }
    }

    // ── Windows CRT linkage fixup (target-driven) ─────────────────────
    // Clang defaults to the static CRT (libcmt) but the Rust-compiled runtime
    // uses the DLL CRT (msvcrt).  Override so the CRT linkage matches.
    if plan.needs_windows_crt_fixup {
        cmd.args(["-Wl,/NODEFAULTLIB:libcmt", "-Wl,/DEFAULTLIB:msvcrt"]);
    }

    // ── Platform system libraries (target-driven via NativeLinkPlan) ──
    cmd.args(plan.platform_libs);

    // FreeBSD: compiled natively on a FreeBSD host (TargetOs has no FreeBsd
    // variant yet, so the plan above doesn't cover it).
    // FOLLOW-UP (#254 Phase 3): add TargetOs::FreeBsd and integrate into
    // NativeLinkPlan; dlopen/clock_gettime are in libc — no -ldl or -lrt.
    #[cfg(target_os = "freebsd")]
    cmd.args(["-lpthread", "-lm"]);

    for lib in extra_libs {
        cmd.arg(lib);
    }

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

    // ── dsymutil for Darwin debug builds (target-driven intent) ───────
    // Darwin debug info requires a separate dSYM bundle: the linker writes a
    // "debug map" referencing the object file, and dsymutil pulls the DWARF
    // into a .dSYM bundle next to the binary.  Without this the debugger sees
    // no Hew debug info in the linked binary.
    if debug && plan.needs_dsymutil {
        run_dsymutil_for_darwin(&safe_output);
    }

    Ok(())
}

/// Run `dsymutil` after a Darwin debug link, if the tool is present on the host.
///
/// `plan.needs_dsymutil` is set for any Darwin target.  The actual invocation
/// is guarded to macOS hosts at compile time because dsymutil is a macOS tool;
/// on other hosts this is a no-op (unreachable in practice, since
/// `can_link_with_host_tools` only permits Darwin targets on macOS hosts).
fn run_dsymutil_for_darwin(binary_path: &str) {
    #[cfg(target_os = "macos")]
    run_dsymutil(binary_path);
    #[cfg(not(target_os = "macos"))]
    let _ = binary_path;
}

/// Run `dsymutil` on a linked binary to produce a `.dSYM` bundle.
///
/// On macOS the linker embeds a "debug map" that references the original object
/// file's DWARF sections.  `dsymutil` reads that map and copies the DWARF into
/// a standalone `.dSYM` bundle next to the binary.  If the object file has
/// already been deleted (e.g. a temp file), the DWARF is lost — so we call
/// this immediately after a successful link while the object is still alive.
///
/// Failure is non-fatal: the binary is still usable, just without rich debug
/// info in the dSYM bundle.
#[cfg(target_os = "macos")]
fn run_dsymutil(binary_path: &str) {
    let tool = if has_tool("dsymutil") {
        "dsymutil"
    } else if has_tool("llvm-dsymutil") {
        "llvm-dsymutil"
    } else {
        eprintln!("warning: dsymutil not found — .dSYM bundle not generated");
        return;
    };

    let status = std::process::Command::new(tool).arg(binary_path).status();

    match status {
        Ok(s) if s.success() => {}
        Ok(s) => {
            eprintln!("warning: dsymutil exited with status {s} — .dSYM may be incomplete");
        }
        Err(e) => {
            eprintln!("warning: failed to run dsymutil: {e}");
        }
    }
}

/// Find the Darwin SDK path using `xcrun --show-sdk-path`.
///
/// Returns `None` on non-macOS hosts (where `xcrun` is not available) and when
/// `xcrun` fails.  Failure is non-fatal; clang falls back to its own SDK search
/// heuristics.  The `#[cfg]` guard ensures the function compiles on all hosts
/// while making availability explicit.
fn find_darwin_sdk() -> Option<String> {
    #[cfg(target_os = "macos")]
    {
        find_macos_sdk()
    }
    #[cfg(not(target_os = "macos"))]
    {
        None
    }
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

fn find_hew_lib(name: &str, triple: &str) -> Result<String, String> {
    let exe = std::env::current_exe().map_err(|e| format!("cannot find self: {e}"))?;
    let exe_dir = exe.parent().expect("exe should have a parent directory");

    let candidates = [
        // Per-triple path first: supports Phase 2 pre-built libs per target
        // and lets `make assemble` wire the host triple without disturbing the
        // generic fallback path.  Mirrors the `lib/wasm32-wasip1/` pattern.
        exe_dir.join(format!("../lib/{triple}")).join(name),
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
        "Error: cannot find {name}. Build with: make stdlib"
    ))
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

/// Query `xcrun` for the path to the current Xcode SDK.
///
/// Returns `None` if `xcrun` is not available or the command fails; the
/// linker will still attempt to locate the SDK through its own search
/// heuristics in that case.
#[cfg(target_os = "macos")]
fn find_macos_sdk() -> Option<String> {
    let output = std::process::Command::new("xcrun")
        .arg("--show-sdk-path")
        .output()
        .ok()?;
    if !output.status.success() {
        return None;
    }
    let path = String::from_utf8(output.stdout).ok()?.trim().to_owned();
    if path.is_empty() {
        None
    } else {
        Some(path)
    }
}

fn has_tool(name: &str) -> bool {
    std::process::Command::new(name)
        .arg("--version")
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .is_ok_and(|s| s.success())
}

#[cfg(test)]
mod tests {
    use super::*;

    // ── extract_undefined_hew_symbol ──────────────────────────────────

    #[test]
    fn extract_symbol_gnu_ld_format() {
        let line = "foo.o: undefined reference to `hew_json_parse'";
        assert_eq!(extract_undefined_hew_symbol(line), Some("hew_json_parse"));
    }

    #[test]
    fn extract_symbol_gnu_ld_with_section_info() {
        let line = "foo.o:(.text+0x1a): undefined reference to `hew_http_get'";
        assert_eq!(extract_undefined_hew_symbol(line), Some("hew_http_get"));
    }

    #[test]
    fn extract_symbol_macos_ld64_format() {
        let line = "\"_hew_http_get\", referenced from:";
        assert_eq!(extract_undefined_hew_symbol(line), Some("hew_http_get"));
    }

    #[test]
    fn extract_symbol_msvc_lld_link_format() {
        let line = "error: unresolved external symbol hew_redis_connect";
        assert_eq!(
            extract_undefined_hew_symbol(line),
            Some("hew_redis_connect")
        );
    }

    #[test]
    fn extract_symbol_msvc_with_trailing_punct() {
        // lld-link may add extra context after the symbol
        let line = "error: unresolved external symbol hew_tcp_bind, referenced in foo.obj";
        assert_eq!(extract_undefined_hew_symbol(line), Some("hew_tcp_bind"));
    }

    #[test]
    fn extract_symbol_no_match_non_hew_undefined() {
        assert_eq!(
            extract_undefined_hew_symbol("undefined reference to `foo_bar'"),
            None
        );
    }

    #[test]
    fn extract_symbol_no_match_empty_string() {
        assert_eq!(extract_undefined_hew_symbol(""), None);
    }

    // ── symbol_to_feature_hint ────────────────────────────────────────

    #[test]
    fn feature_hint_json() {
        assert_eq!(
            symbol_to_feature_hint("hew_json_parse"),
            Some(("json", "serialization"))
        );
    }

    #[test]
    fn feature_hint_yaml() {
        assert_eq!(
            symbol_to_feature_hint("hew_yaml_load"),
            Some(("yaml", "serialization"))
        );
    }

    #[test]
    fn feature_hint_postgres() {
        assert_eq!(
            symbol_to_feature_hint("hew_postgres_connect"),
            Some(("postgres", "db"))
        );
    }

    #[test]
    fn feature_hint_mysql() {
        assert_eq!(
            symbol_to_feature_hint("hew_mysql_query"),
            Some(("mysql", "db"))
        );
    }

    #[test]
    fn feature_hint_redis() {
        assert_eq!(
            symbol_to_feature_hint("hew_redis_get"),
            Some(("redis", "db"))
        );
    }

    #[test]
    fn feature_hint_http() {
        assert_eq!(
            symbol_to_feature_hint("hew_http_get"),
            Some(("http", "http"))
        );
    }

    #[test]
    fn feature_hint_grpc() {
        assert_eq!(
            symbol_to_feature_hint("hew_grpc_call"),
            Some(("grpc", "http"))
        );
    }

    #[test]
    fn feature_hint_tcp() {
        assert_eq!(symbol_to_feature_hint("hew_tcp_bind"), Some(("tcp", "net")));
    }

    #[test]
    fn feature_hint_unknown_prefix() {
        assert_eq!(symbol_to_feature_hint("hew_unknown_func"), None);
    }

    #[test]
    fn feature_hint_non_hew_symbol() {
        assert_eq!(symbol_to_feature_hint("some_other_symbol"), None);
    }

    // ── diagnose_linker_errors ────────────────────────────────────────

    #[test]
    fn diagnose_single_feature_hint() {
        let stderr = "foo.o: undefined reference to `hew_json_parse'\n";
        let hints = diagnose_linker_errors(stderr);
        assert_eq!(hints.len(), 1);
        assert!(hints[0].contains("json"));
        assert!(hints[0].contains("serialization"));
    }

    #[test]
    fn diagnose_multiple_distinct_features() {
        let stderr = "\
            foo.o: undefined reference to `hew_json_parse'\n\
            foo.o: undefined reference to `hew_http_get'\n\
            foo.o: undefined reference to `hew_tcp_bind'\n";
        let hints = diagnose_linker_errors(stderr);
        assert_eq!(hints.len(), 3);
    }

    #[test]
    fn diagnose_deduplicates_same_feature() {
        // Both symbols map to ("json", "serialization") — only one hint expected
        let stderr = "\
            foo.o: undefined reference to `hew_json_parse'\n\
            foo.o: undefined reference to `hew_json_serialize'\n";
        let hints = diagnose_linker_errors(stderr);
        assert_eq!(hints.len(), 1);
    }

    #[test]
    fn diagnose_empty_stderr() {
        let hints = diagnose_linker_errors("");
        assert!(hints.is_empty());
    }

    #[test]
    fn diagnose_no_hew_symbols() {
        let hints = diagnose_linker_errors("undefined reference to `some_lib_func'\n");
        assert!(hints.is_empty());
    }

    #[test]
    fn diagnose_mixed_linker_formats() {
        // GNU ld + macOS ld64 + MSVC in the same stderr (unlikely but exercises all paths)
        let stderr = "\
            foo.o: undefined reference to `hew_json_parse'\n\
            \"_hew_http_get\", referenced from:\n\
            error: unresolved external symbol hew_tcp_bind\n";
        let hints = diagnose_linker_errors(stderr);
        assert_eq!(hints.len(), 3);
    }

    #[test]
    fn diagnose_hint_includes_rebuild_command() {
        let stderr = "foo.o: undefined reference to `hew_postgres_connect'\n";
        let hints = diagnose_linker_errors(stderr);
        assert_eq!(hints.len(), 1);
        assert!(hints[0].contains("cargo build -p hew-runtime --features db"));
    }

    #[test]
    fn diagnose_unknown_hew_symbol_ignored() {
        // hew_foobar_ doesn't map to any known feature
        let stderr = "foo.o: undefined reference to `hew_foobar_something'\n";
        let hints = diagnose_linker_errors(stderr);
        assert!(hints.is_empty());
    }

    // ── has_tool ──────────────────────────────────────────────────────

    #[test]
    fn has_tool_finds_true_command() {
        // `true` is a standard Unix utility that always succeeds
        assert!(has_tool("true"));
    }

    #[test]
    fn has_tool_rejects_nonexistent_tool() {
        assert!(!has_tool("definitely_not_a_real_tool_abc123xyz"));
    }

    // ── find_hew_lib ──────────────────────────────────────────────────

    #[test]
    fn find_hew_lib_nonexistent_returns_error() {
        let result = find_hew_lib("nonexistent_lib_xyz.a", "aarch64-apple-darwin");
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.contains("cannot find nonexistent_lib_xyz.a"));
        assert!(err.contains("make stdlib"));
    }

    // ── output-path sanitisation (extracted from link_executable) ─────

    #[test]
    fn output_path_dash_prefix_is_sanitised() {
        let output_path = "-evil";
        let safe = if output_path.starts_with('-') {
            format!("./{output_path}")
        } else {
            output_path.to_string()
        };
        assert_eq!(safe, "./-evil");
    }
}
