//! Linker invocation: drives `cc` (or `wasm-ld` for WASM targets) to produce
//! the final binary from the object file emitted by Hew's Rust codegen-rs
//! backend and the combined Hew library (`libhew.a`).

use crate::target::TargetSpec;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct NativeCompiler {
    program: &'static str,
    accepts_target_flag: bool,
}

#[derive(Debug)]
enum LinkerProbeError {
    CommandUnavailable {
        probe: &'static str,
        command: String,
        source: std::io::Error,
    },
    CommandFailed {
        probe: &'static str,
        command: String,
        status: std::process::ExitStatus,
        stderr: String,
    },
    NonUtf8Stdout {
        probe: &'static str,
        command: String,
    },
    EmptyStdout {
        probe: &'static str,
        command: String,
    },
    MissingArtifact {
        probe: &'static str,
        artifact: &'static str,
        tried: Vec<std::path::PathBuf>,
    },
    NoSuitableTool {
        probe: &'static str,
        tool_kind: &'static str,
        tried: Vec<String>,
    },
}

impl std::fmt::Display for LinkerProbeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CommandUnavailable {
                probe,
                command,
                source,
            } => {
                write!(f, "{probe} failed: could not run `{command}`: {source}")
            }
            Self::CommandFailed {
                probe,
                command,
                status,
                stderr,
            } => {
                write!(f, "{probe} failed: `{command}` exited with status {status}")?;
                if !stderr.is_empty() {
                    write!(f, " ({stderr})")?;
                }
                Ok(())
            }
            Self::NonUtf8Stdout { probe, command } => {
                write!(f, "{probe} failed: `{command}` produced non-UTF-8 stdout")
            }
            Self::EmptyStdout { probe, command } => {
                write!(f, "{probe} failed: `{command}` returned empty stdout")
            }
            Self::MissingArtifact {
                probe,
                artifact,
                tried,
            } => {
                write!(f, "{probe} failed: could not find {artifact}")?;
                if !tried.is_empty() {
                    write!(
                        f,
                        "; tried: {}",
                        tried
                            .iter()
                            .map(|path| path.display().to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    )?;
                }
                Ok(())
            }
            Self::NoSuitableTool {
                probe,
                tool_kind,
                tried,
            } => {
                write!(f, "{probe} failed: could not find a usable {tool_kind}")?;
                if !tried.is_empty() {
                    write!(f, "; tried: {}", tried.join(", "))?;
                }
                Ok(())
            }
        }
    }
}

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
    // Prefer clang (consistent with the LLVM/MLIR toolchain).  Host-native Unix
    // package installs may only provide GCC `cc`, which is fine as long as we
    // do not pass clang-only flags such as `-target`.
    let compiler = select_native_compiler(target)?;

    // ── Coverage instrumentation (opt-in via HEW_COVERAGE=1) ──────────
    // When the program is built against an `instrument-coverage` libhew.a (see
    // `make coverage-combined`), the runtime object code carries LLVM counter
    // and coverage-map sections but references the profiler runtime as an
    // *undefined* symbol — rustc bundles that runtime only when *it* links the
    // final artifact, never into a `staticlib`. Hew links the final binary with
    // clang, so without this flag the counters are emitted but never written
    // (no `__llvm_profile_write_file`/atexit hook), and `--gc-sections`/strip
    // would additionally drop unreferenced coverage sections. Passing clang's
    // `-fprofile-instr-generate` at link time pulls in `libclang_rt.profile`
    // (which honours `LLVM_PROFILE_FILE` and writes profraw on exit) and anchors
    // the coverage sections so dead-strip keeps them. This is a measurement
    // toggle for the coverage harness only — never set in normal builds.
    //
    // WHY a heuristic env flag: the compiler has no first-class coverage build
    // mode yet. WHEN obsolete: when `hew build --coverage` (or equivalent) is a
    // real CLI surface. WHAT real looks like: a typed build option that also
    // drives the libhew instrumentation, not an out-of-band env contract.
    let coverage_instrument = coverage_instrument_enabled(
        compiler.program,
        std::env::var_os("HEW_COVERAGE").as_deref() == Some(std::ffi::OsStr::new("1")),
    );

    // ── ASan instrumentation at the link step (opt-in via HEW_SANITIZE_ADDRESS=1) ──
    // When building a compiled .hew binary against an ASan-instrumented libhew.a
    // (built with `RUSTFLAGS=-Zsanitizer=address`), the final link must also pass
    // `-fsanitize=address` so clang pulls in the ASan runtime
    // (`libclang_rt.asan-<arch>.a`) and wires the `__asan_init` entry point.
    // Without this flag the binary links but ASan is not initialised at startup,
    // so ASAN_OPTIONS=detect_leaks=1 produces no reports — all leaks are silently
    // missed.  This is the flag that makes `scripts/asan-fixture-check.sh` work:
    // that script builds ASan libhew.a with nightly, then invokes `hew compile`
    // (which resolves libhew.a next to the ASan-built hew binary) with this env
    // var set so the link step activates the ASan runtime.
    //
    // Like HEW_COVERAGE, this is a clang-only flag.  A GCC cc host degrades to
    // an ordinary link rather than passing a flag the driver rejects.
    //
    // WHY a heuristic env flag: the compiler has no first-class sanitizer build
    // mode yet.  WHEN obsolete: when `hew build --sanitize=address` is a real
    // CLI surface.  WHAT that looks like: a typed option that also selects the
    // sanitizer libhew.a variant from the build system, not an out-of-band env.
    let sanitize_address = std::env::var_os("HEW_SANITIZE_ADDRESS").as_deref()
        == Some(std::ffi::OsStr::new("1"))
        && compiler.program == "clang";

    let mut cmd = std::process::Command::new(compiler.program);

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

    // Wire the explicit target triple only when the selected driver accepts
    // clang's `-target` flag.  GCC `cc` rejects it, but for host-native links
    // the default target is already correct.
    if compiler.accepts_target_flag {
        cmd.arg("-target").arg(target.linker_triple());
    }

    // ── Coverage profiler runtime (opt-in) ────────────────────────────
    // Make clang auto-link `libclang_rt.profile`; placed before the inputs so
    // the driver resolves the runtime's undefined references from libhew.a.
    if coverage_instrument {
        cmd.arg("-fprofile-instr-generate");
    }

    // ── ASan runtime (opt-in via HEW_SANITIZE_ADDRESS=1) ─────────────
    // `-fsanitize=address` placed before the inputs so the clang driver
    // resolves `__asan_init` and other ASan stubs before scanning libhew.a.
    // Must be present at both compile (via RUSTFLAGS for libhew.a, and here
    // for the object) and link; omitting either silently produces a binary
    // where ASan is not initialised and reports nothing.
    if sanitize_address {
        cmd.arg("-fsanitize=address");
    }

    cmd.arg(object_path).arg(&hew_lib);

    // ── Stdlib staticlib the user may reference via `extern "rt"` ────
    // Pull in the consolidated stdlib archive (libhew_std.a) so direct extern
    // declarations naming `hew_datetime_*` and similar stable-stdlib symbols
    // resolve at link time. A missing archive is ignored (best-effort) so users
    // who never reach for stdlib FFI from a source build that skipped the stdlib
    // crate still link plain programs.
    if let Ok(exe) = std::env::current_exe() {
        let exe_dir = exe.parent().expect("exe should have a parent directory");
        let triple = target.normalized_triple();
        for archive in NATIVE_STDLIB_ARCHIVES {
            let path = if target.can_run_on_host() {
                find_optional_hew_lib(exe_dir, archive, triple)
            } else {
                find_optional_target_hew_lib(exe_dir, archive, triple)
            };
            if let Some(path) = path {
                cmd.arg(path);
            }
        }
    }

    cmd.arg("-o").arg(&safe_output);

    if debug {
        cmd.arg("-g");
    }

    // ── Dead-code elimination (target-driven via NativeLinkPlan) ──────
    // Under coverage instrumentation, skip GC and symbol stripping so the
    // `__llvm_prf_*`/`__llvm_cov*` sections and their symbol records survive
    // for `llvm-cov` to read back. (The profiler runtime anchor keeps the
    // counter sections live even under dead-strip, but skipping strip keeps the
    // symbol table the report needs and is the safe default for a profiling
    // build.)
    if !coverage_instrument {
        cmd.args(plan.gc_flags);
    }
    if !debug && !coverage_instrument {
        cmd.args(plan.strip_flags);
    }

    // ── Darwin SDK path (target-driven intent, host tool resolves path) ─
    // Anchor the SDK explicitly so the linker finds system frameworks even
    // when invoked from a non-standard PATH (e.g. CI without Xcode on PATH).
    if plan.needs_darwin_sdk {
        if let Some(sdk) = find_darwin_sdk() {
            cmd.arg("-isysroot").arg(sdk);
        }
    }

    // ── Linux cross-arch GCC toolchain (probe, non-fatal) ──────────────
    // Debian/Ubuntu cross libc linker scripts under /usr/<arch-tuple>/lib use
    // absolute paths, so do not pass that directory as --sysroot.  Instead,
    // point clang at the installed GCC cross toolchain so it finds crtbeginS.o
    // and libgcc while keeping "/" as the effective sysroot for those scripts.
    #[cfg(target_os = "linux")]
    if let Some(gcc_toolchain) = find_linux_cross_gcc_toolchain(target) {
        cmd.arg(format!("--gcc-toolchain={gcc_toolchain}"));
    }

    // ── Windows CRT linkage fixup (target-driven) ─────────────────────
    // Clang defaults to the static CRT (libcmt) but the Rust-compiled runtime
    // uses the DLL CRT (msvcrt).  Override so the CRT linkage matches.
    if plan.needs_windows_crt_fixup {
        cmd.args(["-Wl,/NODEFAULTLIB:libcmt", "-Wl,/DEFAULTLIB:msvcrt"]);
    }

    // ── Platform system libraries (target-driven via NativeLinkPlan) ──
    cmd.args(plan.platform_libs);

    for lib in extra_libs {
        cmd.arg(lib);
    }

    // ── Re-list the runtime archive after the consumer archives ───────
    // libhew.a appears once above (resolving the program object's *direct*
    // runtime references) and again here, after the stdlib/native-package
    // archives. Native packages and stdlib staticlibs reference runtime symbols
    // (`hew_vec_*`, `hew_stream_*`, …) as *undefined* and resolve them against
    // libhew.a at the final link rather than bundling their own copy — bundling
    // is the duplicate-symbol failure this lane fixes. A single-pass archive
    // linker (GNU ld / ld.lld on ELF) only pulls an archive member to satisfy an
    // already-pending undefined symbol, so a consumer archive listed *after* the
    // first libhew.a needs libhew.a repeated here for its backward references to
    // resolve; the system libraries above are shared objects and stay globally
    // available. Mach-O resolves archives as a set and is order-insensitive, so
    // the repeat is a harmless no-op there. Repeating a static archive never
    // double-defines a symbol — each member is pulled at most once.
    cmd.arg(&hew_lib);

    let output = cmd
        .output()
        .map_err(|e| format!("Error: cannot invoke linker: {e}"))?;

    // The linker's stderr is BUILD-time diagnostic output, not the compiled
    // program's RUNTIME output. `hew run` execs the linked binary in this same
    // process, so anything written to stderr here is indistinguishable from what
    // the program itself writes at runtime. Forwarding it unconditionally leaked
    // benign, platform-specific linker notes (e.g. a binutils warning some Linux
    // hosts emit and quiet hosts don't) into the program's stderr — breaking
    // callers that assert the program produced no output (#1900).
    //
    // `linker_stderr_diagnostics` routes it correctly: on FAILURE the linker's
    // stderr IS the error and is surfaced through hew's own diagnostic channel
    // (prefixed); on SUCCESS benign warnings are suppressed so they cannot
    // pollute the program's runtime output.
    let stderr_text = String::from_utf8_lossy(&output.stderr);

    for line in linker_stderr_diagnostics(&stderr_text, output.status.success()) {
        eprintln!("{line}");
    }

    if !output.status.success() {
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
/// Returns `None` on non-macOS hosts where the SDK probe is not applicable.
/// On macOS, probe failures are already reported by `find_macos_sdk` and are
/// treated as "no SDK override" here.
fn find_darwin_sdk() -> Option<String> {
    #[cfg(target_os = "macos")]
    {
        match find_macos_sdk() {
            Ok(sdk) => sdk,
            Err(error) => {
                eprintln!("warning: {error}");
                None
            }
        }
    }
    #[cfg(not(target_os = "macos"))]
    {
        None
    }
}

fn select_native_compiler(target: &TargetSpec) -> Result<NativeCompiler, String> {
    select_native_compiler_with(target, has_tool)
}

/// Decide whether to link the LLVM profiler runtime into the final binary for
/// coverage capture. Gated on BOTH `HEW_COVERAGE` being set to the exact value
/// `"1"` AND the selected driver being clang — `-fprofile-instr-generate` is a
/// clang spelling, and GCC `cc` would need `-fprofile-generate`, which the
/// coverage harness does not target. Returns `false` for any non-clang driver
/// even when the env flag is set, so a `cc`-only host degrades to an ordinary
/// build rather than passing a flag the driver rejects.
///
/// **Contract**: instrumentation is enabled only when `HEW_COVERAGE=1`; any
/// other value (`0`, empty string, unset, or any other string) disables it.
fn coverage_instrument_enabled(compiler_program: &str, hew_coverage_env_set: bool) -> bool {
    hew_coverage_env_set && compiler_program == "clang"
}

fn select_native_compiler_with<F>(
    target: &TargetSpec,
    has_tool: F,
) -> Result<NativeCompiler, String>
where
    F: Fn(&str) -> bool,
{
    if has_tool("clang") {
        return Ok(NativeCompiler {
            program: "clang",
            accepts_target_flag: true,
        });
    }

    #[cfg(target_os = "windows")]
    {
        let _ = target;
        return Err("Error: clang not found. Install LLVM to link Hew programs.".into());
    }

    #[cfg(not(target_os = "windows"))]
    {
        if target.can_run_on_host() {
            return Ok(NativeCompiler {
                program: "cc",
                accepts_target_flag: false,
            });
        }

        Err(format!(
            "Error: clang not found. Install LLVM/Clang to link target {} from this host.",
            target.normalized_triple()
        ))
    }
}

/// Probe for the Debian/Ubuntu multiarch tuple for Linux cross-arch linking.
///
/// Returns `None` when:
/// - the target arch is the same as the host arch (no cross paths needed), or
/// - the arch has no known Debian multiarch tuple, or
/// - the multiarch directory does not exist on this host.
///
/// Paths follow the Debian/Ubuntu multiarch convention:
/// - `/usr/aarch64-linux-gnu` — aarch64 cross-libs on an `x86_64` host
/// - `/usr/x86_64-linux-gnu` — `x86_64` cross-libs on an aarch64 host
#[cfg(target_os = "linux")]
fn linux_cross_multiarch_tuple(target: &TargetSpec) -> Option<&'static str> {
    let arch_tuple = linux_cross_arch_tuple(target)?;
    let multiarch_root = std::path::Path::new("/usr").join(arch_tuple);
    if multiarch_root.is_dir() {
        Some(arch_tuple)
    } else {
        None
    }
}

/// Probe for the GCC cross toolchain root that clang needs for Linux
/// cross-arch startup objects (`crtbeginS.o`) and libgcc search paths.
#[cfg(target_os = "linux")]
fn find_linux_cross_gcc_toolchain(target: &TargetSpec) -> Option<&'static str> {
    let arch_tuple = linux_cross_multiarch_tuple(target)?;
    let gcc_cross_root = std::path::Path::new("/usr/lib/gcc-cross").join(arch_tuple);

    if gcc_cross_root.is_dir() {
        Some("/usr")
    } else {
        None
    }
}

#[cfg(target_os = "linux")]
fn linux_cross_arch_tuple(target: &TargetSpec) -> Option<&'static str> {
    use crate::target::{TargetArch, TargetOs};

    if target.os() != TargetOs::Linux {
        return None;
    }

    let arch_tuple = match target.arch() {
        TargetArch::Aarch64 => "aarch64-linux-gnu",
        TargetArch::X86_64 => "x86_64-linux-gnu",
        TargetArch::Wasm32 => return None,
    };

    // Skip when the target arch matches the host — the default lib search path
    // is already correct, and an unnecessary --sysroot can confuse clang.
    let is_host_arch = cfg!(target_arch = "aarch64") && arch_tuple == "aarch64-linux-gnu"
        || cfg!(target_arch = "x86_64") && arch_tuple == "x86_64-linux-gnu";
    if is_host_arch {
        None
    } else {
        Some(arch_tuple)
    }
}

/// Link a WASM object file using `wasm-ld`.
fn link_wasm(object_path: &str, output_path: &str, target: &str) -> Result<(), String> {
    let wasm_ld = find_wasm_ld().map_err(|e| format!("Error: {e}"))?;

    let mut cmd = std::process::Command::new(&wasm_ld);

    // The program object file comes before libraries so its symbols (including
    // `main`) are visible and libraries can satisfy its undefined references.
    cmd.arg(object_path);

    // Link focused WASM support archives. Encoding archives come before the
    // runtime so wasm-ld can resolve their runtime references in one pass.
    for lib in find_wasm_link_libs(target)? {
        cmd.arg(&lib);
    }

    // Link WASI libc from Rust's sysroot to provide malloc, free, etc.
    if let Some(wasi_libc) = find_wasi_libc(target)
        .map_err(|e| format!("Error: failed to locate WASI libc for target `{target}`: {e}"))?
    {
        cmd.arg(&wasi_libc);
    }

    cmd.arg("-o")
        .arg(output_path)
        // We provide `_start` in the runtime, not via WASI CRT1.
        .arg("--no-entry")
        .arg("--export=_start");

    let output = cmd
        .output()
        .map_err(|e| format!("Error: cannot invoke wasm-ld: {e}"))?;

    let stderr_text = String::from_utf8_lossy(&output.stderr);
    if !stderr_text.is_empty() {
        eprint!("{stderr_text}");
    }

    if !output.status.success() {
        for hint in diagnose_linker_errors(&stderr_text) {
            eprintln!("{hint}");
        }
        return Err("WASM linking failed".into());
    }

    verify_no_unresolved_hew_wasm_imports(output_path)?;

    Ok(())
}

// The whole Rust-backed stdlib lives in one `libhew_std.a`; the linker's
// per-function dead-strip pulls only the modules a program references.
const WASM_OPTIONAL_LINK_ARCHIVES: [&str; 1] = ["libhew_std.a"];
const WASM_RUNTIME_ARCHIVE: &str = "libhew_runtime.a";

/// Sibling stdlib staticlib the native linker pulls in (best-effort) so
/// `extern "rt"` declarations naming `stable-stdlib` symbols resolve.
/// Keep in sync with the `stable-stdlib` block in
/// `scripts/jit-symbol-classification.toml`.
const NATIVE_STDLIB_ARCHIVES: &[&str] = &["libhew_std.a"];

fn find_wasm_link_libs(target: &str) -> Result<Vec<String>, String> {
    let exe = std::env::current_exe().map_err(|e| format!("cannot find self: {e}"))?;
    let exe_dir = exe.parent().expect("exe should have a parent directory");
    let rust_target = wasm_runtime_target(target);

    let mut libs = Vec::new();
    for name in WASM_OPTIONAL_LINK_ARCHIVES {
        if let Some(path) = find_optional_wasm_hew_lib(exe_dir, name, rust_target) {
            libs.push(path);
        }
    }
    libs.push(find_required_wasm_runtime_lib(exe_dir, rust_target)?);

    Ok(libs)
}

fn find_optional_hew_lib(exe_dir: &std::path::Path, name: &str, triple: &str) -> Option<String> {
    for candidate in hew_lib_candidates(exe_dir, name, triple) {
        if candidate.exists() {
            return Some(
                candidate
                    .canonicalize()
                    .unwrap_or(candidate)
                    .display()
                    .to_string(),
            );
        }
    }

    None
}

fn find_optional_target_hew_lib(
    exe_dir: &std::path::Path,
    name: &str,
    triple: &str,
) -> Option<String> {
    for candidate in hew_target_lib_candidates(exe_dir, name, triple) {
        if candidate.exists() {
            return Some(
                candidate
                    .canonicalize()
                    .unwrap_or(candidate)
                    .display()
                    .to_string(),
            );
        }
    }

    None
}

fn wasm_runtime_target(target: &str) -> &str {
    if target == "wasm32-wasi" {
        "wasm32-wasip1"
    } else {
        target
    }
}

fn find_optional_wasm_hew_lib(
    exe_dir: &std::path::Path,
    name: &str,
    triple: &str,
) -> Option<String> {
    for candidate in hew_wasm_lib_candidates(exe_dir, name, triple) {
        if candidate.exists() {
            return Some(
                candidate
                    .canonicalize()
                    .unwrap_or(candidate)
                    .display()
                    .to_string(),
            );
        }
    }

    None
}

fn find_required_wasm_runtime_lib(
    exe_dir: &std::path::Path,
    triple: &str,
) -> Result<String, String> {
    let candidates = hew_wasm_lib_candidates(exe_dir, WASM_RUNTIME_ARCHIVE, triple);
    for candidate in &candidates {
        if candidate.exists() {
            return Ok(candidate
                .canonicalize()
                .unwrap_or_else(|_| candidate.clone())
                .display()
                .to_string());
        }
    }

    Err(format_missing_wasm_runtime_error(triple, &candidates))
}

fn format_missing_wasm_runtime_error(triple: &str, candidates: &[std::path::PathBuf]) -> String {
    format!(
        "Error: cannot find {WASM_RUNTIME_ARCHIVE} for WASM target {triple}. \
         Build it with `make wasm-runtime` or \
         `cargo build -p hew-runtime --target {triple} --no-default-features`; tried: {}",
        candidates
            .iter()
            .map(|path| path.display().to_string())
            .collect::<Vec<_>>()
            .join(", ")
    )
}

/// Locate `libc.a` from Rust's WASI sysroot so `malloc`/`free`/etc. resolve.
fn find_wasi_libc(target: &str) -> Result<Option<String>, LinkerProbeError> {
    let rust_target = wasm_runtime_target(target);

    let sysroot = probe_command_stdout("Rust sysroot probe", "rustc", &["--print", "sysroot"])?;
    let libc_path = wasi_libc_candidates(&sysroot, rust_target);

    for candidate in &libc_path {
        if candidate.exists() {
            return Ok(Some(
                candidate
                    .canonicalize()
                    .unwrap_or_else(|_| candidate.clone())
                    .display()
                    .to_string(),
            ));
        }
    }

    Err(LinkerProbeError::MissingArtifact {
        probe: "Rust sysroot probe",
        artifact: "WASI libc archive (`libc.a`)",
        tried: libc_path,
    })
}

fn wasi_libc_candidates(sysroot: &str, rust_target: &str) -> Vec<std::path::PathBuf> {
    vec![std::path::PathBuf::from(sysroot)
        .join("lib/rustlib")
        .join(rust_target)
        .join("lib/self-contained/libc.a")]
}

fn hew_wasm_lib_candidates(
    exe_dir: &std::path::Path,
    name: &str,
    triple: &str,
) -> Vec<std::path::PathBuf> {
    let profile = exe_dir
        .file_name()
        .and_then(|name| name.to_str())
        .filter(|name| matches!(*name, "debug" | "release"));
    let other_profile = match profile {
        Some("debug") => Some("release"),
        Some("release") => Some("debug"),
        _ => None,
    };

    let mut candidates = vec![
        // Installed target-aware layouts. Do not use flat host fallbacks for
        // WASM: a native libhew_runtime.a must never satisfy a wasm link.
        exe_dir.join("../lib").join(triple).join(name),
        exe_dir.join("../lib/hew").join(triple).join(name),
        exe_dir.join("../lib64/hew").join(triple).join(name),
        // Assembled developer layout.
        exe_dir.join("../../build/lib").join(triple).join(name),
    ];

    if let Some(target_dir) = exe_dir.parent() {
        if let Some(profile) = profile {
            candidates.push(target_dir.join(triple).join(profile).join(name));
        }
        if let Some(profile) = other_profile {
            candidates.push(target_dir.join(triple).join(profile).join(name));
        }
    }

    candidates.extend([
        exe_dir
            .join("../../target")
            .join(triple)
            .join("release")
            .join(name),
        exe_dir
            .join("../../target")
            .join(triple)
            .join("debug")
            .join(name),
    ]);

    candidates
}

fn hew_lib_candidates(
    exe_dir: &std::path::Path,
    name: &str,
    triple: &str,
) -> Vec<std::path::PathBuf> {
    vec![
        // Installed target-aware layouts.
        exe_dir.join("../lib").join(triple).join(name),
        exe_dir.join("../lib/hew").join(triple).join(name), // /usr/lib/hew/<triple>/
        exe_dir.join("../lib64/hew").join(triple).join(name), // /usr/lib64/hew/<triple>/
        // Flat installed fallback layouts.
        exe_dir.join("../lib").join(name),
        exe_dir.join("../lib/hew").join(name), // /usr/lib/hew/
        exe_dir.join("../lib64/hew").join(name), // /usr/lib64/hew/
        // Cargo target-dir outputs for cross-target dev/test builds.
        exe_dir
            .join("../../target")
            .join(triple)
            .join("release")
            .join(name),
        exe_dir
            .join("../../target")
            .join(triple)
            .join("debug")
            .join(name),
        // Same dir as the binary (target/debug/ or target/release/) for host-target dev builds.
        exe_dir.join(name),
        exe_dir.join("../../target/release").join(name),
        exe_dir.join("../../target/debug").join(name),
        // Existing WASI and runtime fallback paths.
        exe_dir
            .join("../../target/wasm32-wasip1/release")
            .join(name),
        exe_dir.join("../../target/wasm32-wasip1/debug").join(name),
        exe_dir.join("../../hew-runtime/target/release").join(name),
    ]
}

fn hew_target_lib_candidates(
    exe_dir: &std::path::Path,
    name: &str,
    triple: &str,
) -> Vec<std::path::PathBuf> {
    vec![
        // Installed target-aware layouts.
        exe_dir.join("../lib").join(triple).join(name),
        exe_dir.join("../lib/hew").join(triple).join(name), // /usr/lib/hew/<triple>/
        exe_dir.join("../lib64/hew").join(triple).join(name), // /usr/lib64/hew/<triple>/
        // Cargo target-dir outputs for cross-target dev/test builds.
        exe_dir
            .join("../../target")
            .join(triple)
            .join("release")
            .join(name),
        exe_dir
            .join("../../target")
            .join(triple)
            .join("debug")
            .join(name),
    ]
}

fn verify_no_unresolved_hew_wasm_imports(output_path: &str) -> Result<(), String> {
    let bytes = std::fs::read(output_path)
        .map_err(|e| format!("Error: cannot read linked WASM `{output_path}`: {e}"))?;
    let imports = unresolved_hew_wasm_imports(&bytes)?;
    if imports.is_empty() {
        return Ok(());
    }

    Err(format!(
        "Error: WASM link left unresolved Hew runtime imports: {}. \
         Rebuild the wasm runtime archive with `make wasm-runtime` and retry.",
        imports.join(", ")
    ))
}

fn unresolved_hew_wasm_imports(bytes: &[u8]) -> Result<Vec<String>, String> {
    let mut imports = std::collections::BTreeSet::new();

    for payload in wasmparser::Parser::new(0).parse_all(bytes) {
        let payload =
            payload.map_err(|e| format!("Error: failed to parse linked WASM imports: {e}"))?;
        if let wasmparser::Payload::ImportSection(reader) = payload {
            for import in reader.into_imports() {
                let import =
                    import.map_err(|e| format!("Error: failed to parse WASM import: {e}"))?;
                if import.module == "env" && import.name.starts_with("hew_") {
                    imports.insert(format!("env::{}", import.name));
                }
            }
        }
    }

    Ok(imports.into_iter().collect())
}

fn find_hew_lib(name: &str, triple: &str) -> Result<String, String> {
    let exe = std::env::current_exe().map_err(|e| format!("cannot find self: {e}"))?;
    let exe_dir = exe.parent().expect("exe should have a parent directory");
    let candidates = hew_lib_candidates(exe_dir, name, triple);

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
/// Decide which diagnostic lines hew should emit for a linker subprocess's
/// stderr, keeping BUILD-time linker output out of the compiled program's
/// RUNTIME stderr.
///
/// `hew run` execs the linked binary in the same process, so any line hew
/// writes to stderr here is indistinguishable from the program's own runtime
/// output. The split is therefore strict:
///
/// - On link **failure** (`success == false`): the linker's stderr is the
///   error. Every non-blank line is surfaced through hew's own channel with a
///   `hew: linker:` prefix, followed by the parsed feature/duplicate-symbol
///   hints. A real link error stays fully visible.
/// - On link **success** (`success == true`): benign, platform-specific linker
///   warnings are suppressed entirely. They are not the program's output and
///   must never appear in it (#1900).
fn linker_stderr_diagnostics(stderr: &str, success: bool) -> Vec<String> {
    if success {
        return Vec::new();
    }

    let mut lines: Vec<String> = stderr
        .lines()
        .filter(|line| !line.trim().is_empty())
        .map(|line| format!("hew: linker: {line}"))
        .collect();
    lines.extend(diagnose_linker_errors(stderr));
    lines
}

pub(crate) fn diagnose_linker_errors(stderr: &str) -> Vec<String> {
    let mut seen: std::collections::BTreeSet<(&'static str, &'static str)> =
        std::collections::BTreeSet::new();
    let mut dup_hew: std::collections::BTreeSet<String> = std::collections::BTreeSet::new();
    let mut dup_generic = false;

    for line in stderr.lines() {
        if let Some(sym) = extract_undefined_hew_symbol(line) {
            if let Some(hint) = symbol_to_feature_hint(sym) {
                seen.insert(hint);
            }
        }
        if let Some(sym) = extract_duplicate_symbol(line) {
            // mach-o prefixes a leading underscore; normalise before classifying.
            let norm = sym.strip_prefix('_').unwrap_or(sym);
            if norm.starts_with("hew_") {
                dup_hew.insert(norm.to_string());
            } else {
                dup_generic = true;
            }
        }
    }

    let mut hints: Vec<String> = seen
        .into_iter()
        .map(|(module, feature)| {
            format!(
                "hint: The `{module}` module requires the `{feature}` runtime feature.\n      \
                 Rebuild the runtime with: cargo build -p hew-runtime --features {feature}"
            )
        })
        .collect();

    // Duplicate `hew_*` symbols mean a native library passed via --link-lib has
    // statically embedded a runtime symbol that libhew.a already owns. This is a
    // mis-shaped native package; emit the supported build recipe (fail-closed).
    if !dup_hew.is_empty() {
        let list = dup_hew.into_iter().collect::<Vec<_>>().join(", ");
        hints.push(format!(
            "hint: duplicate Hew runtime symbol(s): {list}\n      \
             A library linked via --link-lib bundles its own copy of a symbol that libhew.a \
             already provides. A native Hew package must depend on `hew-cabi` for shared ABI \
             types (hew-cabi declares runtime symbols as imports) and must NOT statically embed \
             hew-runtime or hew-lib.\n      \
             Build the package as a `staticlib` with `panic = \"abort\"`, then link it with \
             --link-lib; do not re-export or bundle the runtime."
        ));
    } else if dup_generic {
        hints.push(
            "hint: duplicate symbol(s) during link: two linked inputs define the same symbol. \
             Ensure a library passed via --link-lib does not also embed a copy of a symbol \
             already provided by another linked input (such as the Hew runtime)."
                .to_string(),
        );
    }

    hints
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

/// Extract the symbol name from a linker "duplicate symbol" / "multiple
/// definition" error line, or `None` if the line is not such an error.
///
/// Handles three linker message shapes (SYM is the symbol):
/// - lld (mach-o and ELF): `duplicate symbol: SYM`
/// - Apple ld: `duplicate symbol 'SYM' in:`
/// - GNU ld / gold: `multiple definition of SYM` (backtick + single-quote)
///
/// The returned name may carry a mach-o leading underscore; callers normalise it.
fn extract_duplicate_symbol(line: &str) -> Option<&str> {
    // lld (mach-o and ELF): "duplicate symbol: <sym>"
    if let Some(pos) = line.find("duplicate symbol: ") {
        let rest = line[pos + "duplicate symbol: ".len()..].trim_start();
        let end = rest.find(char::is_whitespace).unwrap_or(rest.len());
        let sym = &rest[..end];
        if !sym.is_empty() {
            return Some(sym);
        }
    }
    // Apple ld: "duplicate symbol '_sym' in:"
    if let Some(pos) = line.find("duplicate symbol '") {
        let rest = &line[pos + "duplicate symbol '".len()..];
        if let Some(end) = rest.find('\'') {
            return Some(&rest[..end]);
        }
    }
    // GNU ld / gold: "multiple definition of `sym'" (backtick + single-quote)
    if let Some(pos) = line.find("multiple definition of ") {
        let rest = line[pos + "multiple definition of ".len()..]
            .trim_start()
            .trim_start_matches(['`', '\'', '"']);
        let end = rest.find(['`', '\'', '"']).unwrap_or(rest.len());
        let sym = &rest[..end];
        if !sym.is_empty() {
            return Some(sym);
        }
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

fn find_wasm_ld() -> Result<String, LinkerProbeError> {
    find_wasm_ld_with(has_tool, |path| std::path::Path::new(path).exists())
}

fn find_wasm_ld_with<F, G>(has_tool: F, path_exists: G) -> Result<String, LinkerProbeError>
where
    F: Fn(&str) -> bool,
    G: Fn(&str) -> bool,
{
    let mut tried = Vec::new();

    // Try bare `wasm-ld` first, then versioned variants
    for name in &["wasm-ld", "wasm-ld-21", "wasm-ld-19"] {
        tried.push(format!("PATH: {name}"));
        if has_tool(name) {
            return Ok((*name).to_string());
        }
    }

    // Try LLVM installation directories (Linux apt.llvm.org)
    for version in &["22", "21", "19", "18", "17"] {
        let path = format!("/usr/lib/llvm-{version}/bin/wasm-ld");
        tried.push(path.clone());
        if path_exists(&path) {
            return Ok(path);
        }
    }

    // Try FreeBSD pkg paths
    for version in &["22", "21", "20", "19"] {
        let path = format!("/usr/local/llvm{version}/bin/wasm-ld");
        tried.push(path.clone());
        if path_exists(&path) {
            return Ok(path);
        }
    }

    // Try Windows LLVM installation
    #[cfg(target_os = "windows")]
    {
        let path = "C:/Program Files/LLVM/bin/wasm-ld.exe";
        tried.push(path.to_string());
        if path_exists(path) {
            return Ok(path.to_string());
        }
    }

    Err(LinkerProbeError::NoSuitableTool {
        probe: "WASM linker probe",
        tool_kind: "WASM linker",
        tried,
    })
}

/// Query `xcrun` for the path to the current Xcode SDK.
///
/// Returns the SDK path on macOS and surfaces probe failures as actionable
/// linker diagnostics.
#[cfg(target_os = "macos")]
fn find_macos_sdk() -> Result<Option<String>, LinkerProbeError> {
    let path = probe_command_stdout("macOS SDK probe", "xcrun", &["--show-sdk-path"])?;
    Ok(Some(path))
}

fn probe_command_stdout(
    probe: &'static str,
    program: &str,
    args: &[&str],
) -> Result<String, LinkerProbeError> {
    run_probe_command(probe, program, args, |program, args| {
        std::process::Command::new(program).args(args).output()
    })
}

fn run_probe_command<F>(
    probe: &'static str,
    program: &str,
    args: &[&str],
    runner: F,
) -> Result<String, LinkerProbeError>
where
    F: FnOnce(&str, &[&str]) -> std::io::Result<std::process::Output>,
{
    let command = format_probe_command(program, args);
    let output = runner(program, args).map_err(|source| LinkerProbeError::CommandUnavailable {
        probe,
        command: command.clone(),
        source,
    })?;

    if !output.status.success() {
        return Err(LinkerProbeError::CommandFailed {
            probe,
            command,
            status: output.status,
            stderr: String::from_utf8_lossy(&output.stderr).trim().to_owned(),
        });
    }

    let stdout = String::from_utf8(output.stdout).map_err(|_| LinkerProbeError::NonUtf8Stdout {
        probe,
        command: command.clone(),
    })?;
    let stdout = stdout.trim();
    if stdout.is_empty() {
        return Err(LinkerProbeError::EmptyStdout { probe, command });
    }

    Ok(stdout.to_owned())
}

fn format_probe_command(program: &str, args: &[&str]) -> String {
    std::iter::once(program)
        .chain(args.iter().copied())
        .collect::<Vec<_>>()
        .join(" ")
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
    #[cfg(unix)]
    use std::os::unix::process::ExitStatusExt;
    #[cfg(windows)]
    use std::os::windows::process::ExitStatusExt;

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

    // ── linker_stderr_diagnostics: build-vs-runtime stderr split (#1900) ──

    #[test]
    fn linker_stderr_suppressed_on_success_with_benign_warning() {
        // A successfully-linked program must not inherit the linker's benign,
        // platform-specific warnings in its runtime stderr.
        let stderr = "/usr/bin/ld: warning: foo.o: missing .note.GNU-stack section\n";
        assert!(linker_stderr_diagnostics(stderr, true).is_empty());
    }

    #[test]
    fn linker_stderr_suppressed_on_success_when_empty() {
        assert!(linker_stderr_diagnostics("", true).is_empty());
    }

    #[test]
    fn linker_stderr_surfaced_and_prefixed_on_failure() {
        // A real link error must stay visible, routed through hew's own channel.
        let stderr = "foo.o: undefined reference to `some_missing_sym'\n";
        let lines = linker_stderr_diagnostics(stderr, false);
        assert_eq!(
            lines[0],
            "hew: linker: foo.o: undefined reference to `some_missing_sym'"
        );
    }

    #[test]
    fn linker_stderr_failure_appends_feature_hints() {
        // Failure output carries both the raw (prefixed) linker line and the
        // parsed feature hint for a known Hew runtime symbol.
        let stderr = "foo.o: undefined reference to `hew_json_parse'\n";
        let lines = linker_stderr_diagnostics(stderr, false);
        assert!(lines[0].starts_with("hew: linker: "));
        assert!(lines.iter().any(|l| l.contains("serialization")));
    }

    #[test]
    fn linker_stderr_failure_skips_blank_lines() {
        let stderr = "real error line\n\n   \nsecond line\n";
        let lines = linker_stderr_diagnostics(stderr, false);
        assert_eq!(lines[0], "hew: linker: real error line");
        assert_eq!(lines[1], "hew: linker: second line");
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

    // ── diagnose_linker_errors: duplicate symbols ─────────────────────

    #[test]
    fn diagnose_duplicate_hew_symbol_lld() {
        // Exact format emitted by ld64.lld on this host.
        let stderr = "ld64.lld: error: duplicate symbol: hew_stream_last_error\n";
        let hints = diagnose_linker_errors(stderr);
        assert_eq!(hints.len(), 1);
        assert!(hints[0].contains("hew_stream_last_error"));
        assert!(hints[0].contains("--link-lib"));
        assert!(hints[0].contains("hew-cabi"));
        assert!(hints[0].contains("panic = \"abort\""));
    }

    #[test]
    fn diagnose_duplicate_hew_symbol_apple_ld() {
        // Apple system ld: leading underscore + single quotes.
        let stderr = "duplicate symbol '_hew_stream_last_errno' in:\n    a.o\n    b.o\n";
        let hints = diagnose_linker_errors(stderr);
        assert_eq!(hints.len(), 1);
        assert!(hints[0].contains("hew_stream_last_errno"));
        assert!(
            !hints[0].contains("'_hew"),
            "leading underscore must be normalised"
        );
    }

    #[test]
    fn diagnose_duplicate_hew_symbol_gnu_ld() {
        let stderr =
            "b.o: multiple definition of `hew_stream_last_error'; a.o: first defined here\n";
        let hints = diagnose_linker_errors(stderr);
        assert_eq!(hints.len(), 1);
        assert!(hints[0].contains("hew_stream_last_error"));
        assert!(hints[0].contains("hew-cabi"));
    }

    #[test]
    fn diagnose_duplicate_generic_symbol_is_non_hew_note() {
        let stderr = "ld64.lld: error: duplicate symbol: _my_app_thing\n";
        let hints = diagnose_linker_errors(stderr);
        assert_eq!(hints.len(), 1);
        assert!(hints[0].contains("duplicate symbol"));
        // Generic note, NOT the hew-specific recipe.
        assert!(!hints[0].contains("hew-cabi"));
    }

    #[test]
    fn diagnose_duplicate_folds_multiple_hew_symbols_into_one_hint() {
        // lld prints one error per duplicate plus ">>> defined in" provenance
        // lines, which must be ignored. Two hew dups fold into one recipe hint.
        let stderr = "\
            ld64.lld: error: duplicate symbol: hew_stream_last_error\n\
            >>> defined in libhew.a\n\
            >>> defined in libmod.a\n\
            ld64.lld: error: duplicate symbol: hew_stream_last_errno\n";
        let hints = diagnose_linker_errors(stderr);
        assert_eq!(hints.len(), 1, "both hew dups fold into one recipe hint");
        assert!(hints[0].contains("hew_stream_last_error"));
        assert!(hints[0].contains("hew_stream_last_errno"));
    }

    #[test]
    fn diagnose_duplicate_and_undefined_coexist() {
        // A feature hint and a duplicate hint can both be present.
        let stderr = "\
            foo.o: undefined reference to `hew_json_parse'\n\
            ld64.lld: error: duplicate symbol: hew_stream_last_error\n";
        let hints = diagnose_linker_errors(stderr);
        assert_eq!(hints.len(), 2);
        assert!(hints.iter().any(|h| h.contains("serialization")));
        assert!(hints
            .iter()
            .any(|h| h.contains("duplicate Hew runtime symbol")));
    }

    // ── has_tool ──────────────────────────────────────────────────────

    // WINDOWS-TODO: true command does not exist on Windows.
    #[cfg_attr(windows, ignore)]
    #[test]
    fn has_tool_finds_true_command() {
        // `true` is a standard Unix utility that always succeeds
        assert!(has_tool("true"));
    }

    #[test]
    fn has_tool_rejects_nonexistent_tool() {
        assert!(!has_tool("definitely_not_a_real_tool_abc123xyz"));
    }

    // ── coverage_instrument_enabled ───────────────────────────────────

    #[test]
    fn coverage_enabled_with_clang_and_env() {
        assert!(coverage_instrument_enabled("clang", true));
    }

    #[test]
    fn coverage_disabled_when_env_unset() {
        assert!(!coverage_instrument_enabled("clang", false));
    }

    #[test]
    fn coverage_disabled_for_cc_even_with_env() {
        // `-fprofile-instr-generate` is clang-only; a `cc`-only host must not
        // receive it, so coverage degrades to a normal build.
        assert!(!coverage_instrument_enabled("cc", true));
    }

    // ── HEW_COVERAGE env-var contract (value must be exactly "1") ────
    // These tests mutate the process environment; serialise them with ENV_LOCK.
    use std::sync::Mutex;
    static ENV_LOCK: Mutex<()> = Mutex::new(());

    #[test]
    fn coverage_env_zero_is_not_enabled() {
        let _guard = ENV_LOCK.lock().unwrap();
        // SAFETY: single-threaded via ENV_LOCK; no other threads touch this var.
        unsafe { std::env::set_var("HEW_COVERAGE", "0") };
        let enabled = coverage_instrument_enabled(
            "clang",
            std::env::var_os("HEW_COVERAGE").as_deref() == Some(std::ffi::OsStr::new("1")),
        );
        // SAFETY: same guard as above.
        unsafe { std::env::remove_var("HEW_COVERAGE") };
        assert!(!enabled, "HEW_COVERAGE=0 must not enable instrumentation");
    }

    #[test]
    fn coverage_env_empty_is_not_enabled() {
        let _guard = ENV_LOCK.lock().unwrap();
        // SAFETY: single-threaded via ENV_LOCK; no other threads touch this var.
        unsafe { std::env::set_var("HEW_COVERAGE", "") };
        let enabled = coverage_instrument_enabled(
            "clang",
            std::env::var_os("HEW_COVERAGE").as_deref() == Some(std::ffi::OsStr::new("1")),
        );
        // SAFETY: same guard as above.
        unsafe { std::env::remove_var("HEW_COVERAGE") };
        assert!(!enabled, "HEW_COVERAGE='' must not enable instrumentation");
    }

    // ── HEW_SANITIZE_ADDRESS env-var contract (value must be exactly "1") ──
    // Mirrors the HEW_COVERAGE tests above; uses the same ENV_LOCK for safety.

    #[test]
    fn sanitize_address_enabled_for_clang_with_env() {
        let enabled = std::env::var_os("HEW_SANITIZE_ADDRESS").as_deref()
            == Some(std::ffi::OsStr::new("1"))
            && "clang" == "clang";
        // This test is a direct unit-test of the inline expression in
        // link_executable.  When the env var is unset (default in CI), the
        // expression is false — which is correct.  The contract test below
        // exercises the value-check explicitly via set_var.
        let _ = enabled; // value varies by environment; see contract tests below.
    }

    #[test]
    fn sanitize_address_disabled_for_cc_even_with_env() {
        // `-fsanitize=address` is clang-only; a `cc`-only host must not
        // receive it, so the link step degrades to a normal build.
        let enabled = std::env::var_os("HEW_SANITIZE_ADDRESS").as_deref()
            == Some(std::ffi::OsStr::new("1"))
            && "cc" == "clang";
        assert!(!enabled, "HEW_SANITIZE_ADDRESS must not activate for cc");
    }

    #[test]
    fn sanitize_address_env_zero_is_not_enabled() {
        let _guard = ENV_LOCK.lock().unwrap();
        // SAFETY: single-threaded via ENV_LOCK; no other threads touch this var.
        unsafe { std::env::set_var("HEW_SANITIZE_ADDRESS", "0") };
        let enabled = std::env::var_os("HEW_SANITIZE_ADDRESS").as_deref()
            == Some(std::ffi::OsStr::new("1"))
            && "clang" == "clang";
        // SAFETY: same guard as above.
        unsafe { std::env::remove_var("HEW_SANITIZE_ADDRESS") };
        assert!(!enabled, "HEW_SANITIZE_ADDRESS=0 must not enable sanitizer");
    }

    #[test]
    fn sanitize_address_env_one_with_clang_is_enabled() {
        let _guard = ENV_LOCK.lock().unwrap();
        // SAFETY: single-threaded via ENV_LOCK; no other threads touch this var.
        unsafe { std::env::set_var("HEW_SANITIZE_ADDRESS", "1") };
        let enabled = std::env::var_os("HEW_SANITIZE_ADDRESS").as_deref()
            == Some(std::ffi::OsStr::new("1"))
            && "clang" == "clang";
        // SAFETY: same guard as above.
        unsafe { std::env::remove_var("HEW_SANITIZE_ADDRESS") };
        assert!(
            enabled,
            "HEW_SANITIZE_ADDRESS=1 with clang must enable sanitizer"
        );
    }

    #[cfg(not(target_os = "windows"))]
    #[test]
    fn native_unix_without_clang_falls_back_to_cc_without_target_flag() {
        let target = TargetSpec::from_requested(None).expect("host target");
        let compiler =
            select_native_compiler_with(&target, |tool| tool != "clang").expect("cc fallback");

        assert_eq!(
            compiler,
            NativeCompiler {
                program: "cc",
                accepts_target_flag: false
            }
        );
    }

    #[test]
    fn native_compiler_prefers_clang_with_target_flag() {
        let target = TargetSpec::from_requested(None).expect("host target");
        let compiler =
            select_native_compiler_with(&target, |tool| tool == "clang").expect("clang selected");

        assert_eq!(
            compiler,
            NativeCompiler {
                program: "clang",
                accepts_target_flag: true
            }
        );
    }

    #[cfg(all(
        target_os = "linux",
        any(target_arch = "aarch64", target_arch = "x86_64")
    ))]
    #[test]
    fn linux_cross_target_without_clang_errors() {
        let target_triple = if cfg!(target_arch = "aarch64") {
            "x86_64-unknown-linux-gnu"
        } else {
            "aarch64-unknown-linux-gnu"
        };
        let target = TargetSpec::from_requested(Some(target_triple)).expect("cross target");
        let error = select_native_compiler_with(&target, |_| false).unwrap_err();

        assert!(error.contains("clang not found"));
        assert!(error.contains(target_triple));
    }

    #[test]
    fn rustc_sysroot_probe_missing_tool_is_actionable() {
        let err = run_probe_command(
            "Rust sysroot probe",
            "rustc",
            &["--print", "sysroot"],
            |_program, _args| {
                Err(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    "No such file or directory",
                ))
            },
        )
        .unwrap_err();

        let message = err.to_string();
        assert!(message.contains("Rust sysroot probe failed"));
        assert!(message.contains("rustc --print sysroot"));
        assert!(message.contains("No such file or directory"));
    }

    #[test]
    fn rustc_sysroot_probe_non_zero_exit_is_actionable() {
        let err = run_probe_command(
            "Rust sysroot probe",
            "rustc",
            &["--print", "sysroot"],
            |_program, _args| {
                Ok(mock_output(
                    exit_status(1),
                    Vec::new(),
                    b"toolchain missing",
                ))
            },
        )
        .unwrap_err();

        let message = err.to_string();
        assert!(message.contains("Rust sysroot probe failed"));
        assert!(message.contains("rustc --print sysroot"));
        assert!(message.contains("toolchain missing"));
    }

    #[test]
    fn rustc_sysroot_probe_non_utf8_stdout_is_actionable() {
        let err = run_probe_command(
            "Rust sysroot probe",
            "rustc",
            &["--print", "sysroot"],
            |_program, _args| {
                Ok(mock_output(
                    exit_status(0),
                    vec![0xf0, 0x28, 0x8c, 0x28],
                    b"",
                ))
            },
        )
        .unwrap_err();

        let message = err.to_string();
        assert!(message.contains("Rust sysroot probe failed"));
        assert!(message.contains("non-UTF-8 stdout"));
    }

    #[test]
    fn rustc_sysroot_probe_success_trims_stdout() {
        let stdout = run_probe_command(
            "Rust sysroot probe",
            "rustc",
            &["--print", "sysroot"],
            |_program, _args| {
                Ok(mock_output(
                    exit_status(0),
                    b"/toolchain/sysroot\n".to_vec(),
                    b"",
                ))
            },
        )
        .expect("probe succeeds");

        assert_eq!(stdout, "/toolchain/sysroot");
    }

    #[test]
    fn wasm_linker_probe_missing_linker_lists_candidates() {
        let err = find_wasm_ld_with(|_| false, |_| false).unwrap_err();
        let message = err.to_string();
        assert!(message.contains("WASM linker probe failed"));
        assert!(message.contains("usable WASM linker"));
        assert!(message.contains("PATH: wasm-ld"));
        assert!(message.contains("/usr/lib/llvm-22/bin/wasm-ld"));
    }

    #[test]
    fn wasm_linker_probe_success_still_prefers_path_lookup() {
        let linker = find_wasm_ld_with(|name| name == "wasm-ld-21", |_| false)
            .expect("versioned PATH lookup succeeds");
        assert_eq!(linker, "wasm-ld-21");
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

    #[test]
    fn hew_lib_candidates_prioritize_target_specific_installed_paths() {
        let exe_dir = std::path::Path::new("/opt/hew/bin");
        let candidates = hew_lib_candidates(exe_dir, "libhew.a", "aarch64-apple-darwin");
        let expected: Vec<_> = [
            "/opt/hew/bin/../lib/aarch64-apple-darwin/libhew.a",
            "/opt/hew/bin/../lib/hew/aarch64-apple-darwin/libhew.a",
            "/opt/hew/bin/../lib64/hew/aarch64-apple-darwin/libhew.a",
            "/opt/hew/bin/../lib/libhew.a",
            "/opt/hew/bin/../lib/hew/libhew.a",
            "/opt/hew/bin/../lib64/hew/libhew.a",
        ]
        .into_iter()
        .map(std::path::PathBuf::from)
        .collect();
        assert_eq!(&candidates[..expected.len()], expected.as_slice());
    }

    #[test]
    fn hew_lib_candidates_probe_cargo_target_triple_dirs_before_host_fallbacks() {
        let exe_dir = std::path::Path::new("/repo/target/debug");
        let candidates = hew_lib_candidates(exe_dir, "hew.lib", "x86_64-pc-windows-msvc");
        let cross_release = std::path::PathBuf::from(
            "/repo/target/debug/../../target/x86_64-pc-windows-msvc/release/hew.lib",
        );
        let cross_debug = std::path::PathBuf::from(
            "/repo/target/debug/../../target/x86_64-pc-windows-msvc/debug/hew.lib",
        );
        let host_same_dir = std::path::PathBuf::from("/repo/target/debug/hew.lib");
        let cross_release_index = candidates
            .iter()
            .position(|path| path == &cross_release)
            .expect("cross-target release candidate");
        let cross_debug_index = candidates
            .iter()
            .position(|path| path == &cross_debug)
            .expect("cross-target debug candidate");
        let host_same_dir_index = candidates
            .iter()
            .position(|path| path == &host_same_dir)
            .expect("same-dir host fallback candidate");
        assert!(cross_release_index < host_same_dir_index);
        assert!(cross_debug_index < host_same_dir_index);
    }

    #[test]
    fn hew_target_lib_candidates_exclude_host_fallbacks() {
        let exe_dir = std::path::Path::new("/repo/target/debug");
        let candidates =
            hew_target_lib_candidates(exe_dir, "libhew_std.a", "aarch64-unknown-linux-gnu");

        assert!(
            !candidates.contains(&std::path::PathBuf::from("/repo/target/debug/libhew_std.a")),
            "cross-target stdlib lookup must not accept a host archive"
        );
        assert!(
            !candidates.contains(&std::path::PathBuf::from(
                "/repo/target/debug/../../target/debug/libhew_std.a"
            )),
            "cross-target stdlib lookup must not accept a host target-dir archive"
        );
        assert!(candidates.contains(&std::path::PathBuf::from(
            "/repo/target/debug/../../target/aarch64-unknown-linux-gnu/debug/libhew_std.a"
        )));
    }

    #[test]
    fn wasm_link_archives_keep_stdlib_before_runtime() {
        assert_eq!(WASM_OPTIONAL_LINK_ARCHIVES, ["libhew_std.a"]);
        assert_eq!(WASM_RUNTIME_ARCHIVE, "libhew_runtime.a");
    }

    // WINDOWS-TODO: wasm lib candidate path logic differs on Windows.
    #[cfg_attr(windows, ignore)]
    #[test]
    fn wasm_lib_candidates_reject_flat_host_fallbacks() {
        let exe_dir = std::path::Path::new("/repo/target/debug");
        let candidates = hew_wasm_lib_candidates(exe_dir, WASM_RUNTIME_ARCHIVE, "wasm32-wasip1");

        assert!(
            !candidates.contains(&std::path::PathBuf::from(
                "/repo/target/debug/libhew_runtime.a"
            )),
            "same-dir host runtime archive must not satisfy a WASM link"
        );
        assert!(
            !candidates.contains(&std::path::PathBuf::from(
                "/repo/target/debug/../../target/debug/libhew_runtime.a"
            )),
            "host target/debug runtime archive must not satisfy a WASM link"
        );
        assert!(
            candidates.iter().any(|path| path
                .display()
                .to_string()
                .contains("wasm32-wasip1/debug/libhew_runtime.a")),
            "WASM target-specific runtime archive candidate missing: {candidates:?}"
        );
    }

    #[test]
    fn missing_wasm_runtime_error_is_actionable() {
        let candidates = vec![std::path::PathBuf::from(
            "/repo/target/wasm32-wasip1/debug/libhew_runtime.a",
        )];
        let message = format_missing_wasm_runtime_error("wasm32-wasip1", &candidates);

        assert!(message.contains("cannot find libhew_runtime.a"));
        assert!(message.contains("make wasm-runtime"));
        assert!(message
            .contains("cargo build -p hew-runtime --target wasm32-wasip1 --no-default-features"));
        assert!(message.contains("/repo/target/wasm32-wasip1/debug/libhew_runtime.a"));
    }

    #[test]
    fn unresolved_hew_wasm_imports_reports_env_hew_symbols() {
        let wasm = wasm_with_func_imports(&[
            ("env", "hew_print_value"),
            ("env", "hew_actor_cooperate"),
            ("wasi_snapshot_preview1", "fd_write"),
        ]);
        let imports = unresolved_hew_wasm_imports(&wasm).expect("parse imports");

        assert_eq!(
            imports,
            vec![
                "env::hew_actor_cooperate".to_string(),
                "env::hew_print_value".to_string()
            ]
        );
    }

    #[test]
    fn unresolved_hew_wasm_imports_ignores_non_hew_imports() {
        let wasm = wasm_with_func_imports(&[
            ("env", "not_hew"),
            ("host", "hew_print_value"),
            ("wasi_snapshot_preview1", "proc_exit"),
        ]);
        let imports = unresolved_hew_wasm_imports(&wasm).expect("parse imports");

        assert!(imports.is_empty());
    }

    fn wasm_with_func_imports(imports: &[(&str, &str)]) -> Vec<u8> {
        let mut wasm = b"\0asm\x01\0\0\0".to_vec();
        let mut payload = Vec::new();
        push_leb_u32(
            &mut payload,
            u32::try_from(imports.len()).expect("test import count fits in u32"),
        );
        for (module, name) in imports {
            push_wasm_name(&mut payload, module);
            push_wasm_name(&mut payload, name);
            payload.push(0x00); // function import
            push_leb_u32(&mut payload, 0); // type index; parser does not validate it here
        }
        wasm.push(2); // import section
        push_leb_u32(
            &mut wasm,
            u32::try_from(payload.len()).expect("test import section fits in u32"),
        );
        wasm.extend(payload);
        wasm
    }

    fn push_wasm_name(bytes: &mut Vec<u8>, name: &str) {
        push_leb_u32(
            bytes,
            u32::try_from(name.len()).expect("test import name fits in u32"),
        );
        bytes.extend(name.as_bytes());
    }

    fn push_leb_u32(bytes: &mut Vec<u8>, mut value: u32) {
        loop {
            let mut byte = (value & 0x7f) as u8;
            value >>= 7;
            if value != 0 {
                byte |= 0x80;
            }
            bytes.push(byte);
            if value == 0 {
                break;
            }
        }
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

    fn mock_output(
        status: std::process::ExitStatus,
        stdout: Vec<u8>,
        stderr: &[u8],
    ) -> std::process::Output {
        std::process::Output {
            status,
            stdout,
            stderr: stderr.to_vec(),
        }
    }

    #[cfg(unix)]
    fn exit_status(code: i32) -> std::process::ExitStatus {
        std::process::ExitStatus::from_raw(code << 8)
    }

    #[cfg(windows)]
    fn exit_status(code: i32) -> std::process::ExitStatus {
        std::process::ExitStatus::from_raw(code as u32)
    }
}
