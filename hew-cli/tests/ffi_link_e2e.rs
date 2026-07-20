//! End-to-end tests for linking native Rust FFI staticlibs into Hew binaries
//! via `hew build --link-lib <archive>`.
//!
//! These cover the supported native-package link path (the fix for the
//! "cannot link a Rust FFI native staticlib" blocker):
//!
//!   * a minimal `extern "C"` symbol resolves and runs when its staticlib is
//!     supplied via `--link-lib`;
//!   * an unresolved `extern` (no `--link-lib`) still fails the link
//!     (fail-closed — no silent success);
//!   * a native module that imports the runtime stream-error channel directly
//!     (a hand-declared `extern "C" fn hew_stream_set_last_error`) links cleanly
//!     with NO duplicate-symbol error;
//!   * the REAL ecosystem-module shape — a `staticlib` that depends on the
//!     `hew-cabi` crate and calls `hew_cabi::sink::set_last_error` — links
//!     cleanly with NO duplicate symbol. This is the load-bearing regression
//!     guard for the substrate fix: before it, hew-cabi's `#[no_mangle]`
//!     `hew_stream_*` exports were bundled into every package archive and
//!     collided with libhew.a's copy. After it, hew-cabi declares those symbols
//!     as imports, so the package references them as undefined and resolves them
//!     at final link; and
//!   * a mis-shaped module that re-defines a runtime-owned `#[no_mangle]` symbol
//!     fails the link with an actionable fail-closed diagnostic.
//!
//! Most fixtures are built with `rustc` directly (no cargo / no workspace) as a
//! `staticlib` with `panic = "abort"` — the documented supported recipe. The
//! hew-cabi-dependent fixture is built with `cargo` (a path dependency on the
//! in-tree `hew-cabi`), since it must compile and link against that crate. The
//! tests are unix-only: they parse/assert against unix linker behaviour and the
//! mach-o / ELF duplicate-symbol message shapes. Windows uses a different
//! linker and is out of scope here (the task targets macOS/Linux).
#![cfg(not(target_os = "windows"))]

mod support;

use std::path::{Path, PathBuf};
use std::process::Command;

use support::leak_slope::{
    leaks_supported, measure_leaks, HIGH_FRAMES, LOW_FRAMES, SLOPE_TOLERANCE,
};
use support::{describe_output, hew_binary, require_codegen, run_bounded_command, tempdir};

const BORROW_BOUNDARY_RUST: &str = r#"static VALUES: [i64; 4] = [i64::MIN, -1, 0, i64::MAX];

#[no_mangle]
pub extern "C" fn borrow_boundary_get(index: i64) -> *const i64 {
    match index {
        0..=3 => &VALUES[index as usize],
        _ => core::ptr::null(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn borrow_boundary_read(value: *const i64) -> i64 {
    assert!(!value.is_null());
    unsafe { *value }
}
"#;

const BORROW_BOUNDARY_HEW: &str = r#"extern "C" {
    fn borrow_boundary_get(index: i64) -> &i64;
    fn borrow_boundary_read(value: &i64) -> i64;
}

fn read_at(index: i64) -> i64 {
    let view = unsafe { borrow_boundary_get(index) };
    unsafe { borrow_boundary_read(view) }
}

fn main() {
    let min = read_at(0);
    let neg = read_at(1);
    let zero = read_at(2);
    let max = read_at(3);
    println(f"{min},{neg},{zero},{max}");
}
"#;

fn borrow_boundary_loop_source(iterations: usize) -> String {
    format!(
        r#"extern "C" {{
    fn borrow_boundary_get(index: i64) -> &i64;
    fn borrow_boundary_read(value: &i64) -> i64;
}}

fn read_at(index: i64) -> i64 {{
    let view = unsafe {{ borrow_boundary_get(index) }};
    unsafe {{ borrow_boundary_read(view) }}
}}

fn main() {{
    var i: i64 = 0;
    while i < {iterations} {{
        let low = read_at(0);
        let high = read_at(3);
        if low == high {{
            panic("foreign boundary values unexpectedly match");
        }}
        i = i + 1;
    }}
}}
"#,
    )
}

/// Compile `source` into a `staticlib` archive `lib<name>.a` under `dir`,
/// following the supported native-package recipe: `panic = "abort"` (matches
/// libhew.a's panic strategy, avoiding a `rust_eh_personality` mismatch) and
/// `codegen-units = 1` (co-locates all `#[no_mangle]` fns in one object so that
/// a duplicate-symbol shape is pulled deterministically by the linker).
///
/// Returns the archive path to pass to `--link-lib`. Skips the calling test (by
/// returning `None`) if `rustc` is not invokable.
fn build_staticlib(dir: &Path, name: &str, source: &str) -> Option<PathBuf> {
    let src_path = dir.join(format!("{name}.rs"));
    std::fs::write(&src_path, source).expect("write fixture .rs");
    let archive = dir.join(format!("lib{name}.a"));

    let mut cmd = Command::new("rustc");
    cmd.args([
        "--crate-type",
        "staticlib",
        "--crate-name",
        name,
        "--edition",
        "2021",
        "-C",
        "panic=abort",
        "-C",
        "codegen-units=1",
        "-o",
    ])
    .arg(&archive)
    .arg(&src_path)
    .current_dir(dir);

    let out = match cmd.output() {
        Ok(out) => out,
        Err(error) => {
            eprintln!("SKIP: cannot invoke rustc to build fixture staticlib: {error}");
            return None;
        }
    };
    assert!(
        out.status.success(),
        "rustc failed to build fixture `{name}`:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    );
    assert!(
        archive.exists(),
        "fixture archive not produced: {}",
        archive.display()
    );
    Some(archive)
}

/// Write a `.hew` program named `<name>.hew` under `dir`; return its path.
fn write_program(dir: &Path, name: &str, source: &str) -> PathBuf {
    let path = dir.join(format!("{name}.hew"));
    std::fs::write(&path, source).expect("write fixture .hew");
    path
}

/// Absolute path to the in-tree `hew-cabi` crate (sibling of this `hew-cli`
/// crate in the same workspace), for use as a `cargo` path dependency.
fn hew_cabi_crate_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-cli has a parent dir (the workspace root)")
        .join("hew-cabi")
}

/// Build a `staticlib` that depends on the in-tree `hew-cabi` crate and exposes
/// `<name>_lib` symbols, via `cargo`. This is the REAL ecosystem-module shape:
/// a native package links `hew-cabi` for the shared C-ABI helpers. The crate is
/// built with `panic = "abort"` (matching libhew.a) and an empty `[workspace]`
/// table so it is self-contained and not absorbed into a parent workspace.
///
/// `lib_rs` is the crate's `src/lib.rs` body (it may `use hew_cabi::...`).
/// Returns the archive path, or `None` (skipping the test) if `cargo` cannot be
/// invoked. Uses `--offline`: `require_codegen()` has already built the runtime
/// (and thus `hew-cabi` + `libc`) so the dependencies are cached.
fn build_cabi_wrapper_staticlib(dir: &Path, name: &str, lib_rs: &str) -> Option<PathBuf> {
    let crate_dir = dir.join(format!("{name}-crate"));
    std::fs::create_dir_all(crate_dir.join("src")).expect("create fixture crate dirs");

    let cabi_dir = hew_cabi_crate_dir();
    let cabi_path = cabi_dir.display();
    let manifest = format!(
        "[package]\n\
         name = \"{name}\"\n\
         version = \"0.0.0\"\n\
         edition = \"2021\"\n\
         publish = false\n\n\
         [lib]\n\
         crate-type = [\"staticlib\"]\n\n\
         [dependencies]\n\
         hew-cabi = {{ path = '{cabi_path}' }}\n\n\
         [profile.dev]\n\
         panic = \"abort\"\n\n\
         [workspace]\n",
    );
    std::fs::write(crate_dir.join("Cargo.toml"), manifest).expect("write fixture Cargo.toml");
    std::fs::write(crate_dir.join("src").join("lib.rs"), lib_rs).expect("write fixture lib.rs");

    let target_dir = crate_dir.join("target");
    let mut cmd = Command::new("cargo");
    cmd.arg("build")
        .arg("--offline")
        .arg("--manifest-path")
        .arg(crate_dir.join("Cargo.toml"))
        .env("CARGO_TARGET_DIR", &target_dir)
        .current_dir(&crate_dir);

    let out = match cmd.output() {
        Ok(out) => out,
        Err(error) => {
            eprintln!("SKIP: cannot invoke cargo to build hew-cabi wrapper fixture: {error}");
            return None;
        }
    };
    assert!(
        out.status.success(),
        "cargo failed to build hew-cabi wrapper fixture `{name}`:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&out.stdout),
        String::from_utf8_lossy(&out.stderr),
    );
    let archive = target_dir.join("debug").join(format!("lib{name}.a"));
    assert!(
        archive.exists(),
        "hew-cabi wrapper archive not produced: {}",
        archive.display()
    );
    Some(archive)
}

/// Assert that `archive` *references* `symbol` as an undefined import (nm type
/// `U`) and never *defines* it (`T`/`D`/`B`/`R`/…). This is the single-owner
/// invariant the substrate fix enforces: a native package resolves runtime
/// symbols against `libhew.a` at the final link instead of bundling its own
/// `#[no_mangle]` copy (the bundled copy is exactly the duplicate that collided
/// with `libhew.a` — the std/net/http acceptance criterion). Skips (does not
/// fail) if `nm` is unavailable on the host.
fn assert_archive_symbol_is_undefined(archive: &Path, symbol: &str) {
    let Ok(out) = Command::new("nm").arg(archive).output() else {
        eprintln!("skip nm audit of `{symbol}`: nm not available");
        return;
    };
    if !out.status.success() {
        eprintln!(
            "skip nm audit of `{symbol}`: nm failed on {}",
            archive.display()
        );
        return;
    }
    let text = String::from_utf8_lossy(&out.stdout);
    let mut saw_undefined = false;
    for line in text.lines() {
        // nm rows: defined `<addr> <type> <name>`, undefined `U <name>` (no addr).
        let (ty, name) = match line.split_whitespace().collect::<Vec<_>>().as_slice() {
            [ty, name] => (*ty, *name),
            [_addr, ty, name] => (*ty, *name),
            _ => continue,
        };
        // Normalise the Mach-O leading underscore; mangled Rust wrappers
        // (`_ZN8hew_cabi…`) never equal the bare C symbol, so they are ignored.
        if name.strip_prefix('_').unwrap_or(name) != symbol {
            continue;
        }
        assert!(
            ty == "U",
            "package archive must reference `{symbol}` as an undefined import, but \
             nm reports it DEFINED (`{ty}`) — a bundled copy collides with libhew.a \
             at the final link:\n{line}",
        );
        saw_undefined = true;
    }
    assert!(
        saw_undefined,
        "expected `{symbol}` as an undefined import in {}, but nm listed no such symbol",
        archive.display(),
    );
}

/// Invoke `hew build [--link-lib <lib>] <prog> -o <out_bin>`.
fn hew_build(
    lib: Option<&Path>,
    prog: &Path,
    out_bin: &Path,
    work_dir: &Path,
) -> std::process::Output {
    let mut cmd = Command::new(hew_binary());
    cmd.arg("build");
    if let Some(lib) = lib {
        cmd.arg("--link-lib").arg(lib);
    }
    cmd.arg(prog).arg("-o").arg(out_bin).current_dir(work_dir);
    run_bounded_command(cmd, "hew build --link-lib")
}

/// Resolve a tool on the CURRENT PATH (before any shadowing), mirroring the
/// `which` probe `select_native_compiler` performs. Returns the absolute path.
fn resolve_tool(name: &str) -> Option<PathBuf> {
    let out = Command::new("which").arg(name).output().ok()?;
    if !out.status.success() {
        return None;
    }
    let path = String::from_utf8_lossy(&out.stdout).trim().to_string();
    if path.is_empty() {
        None
    } else {
        Some(PathBuf::from(path))
    }
}

/// The native link line must carry `libhew.a` as the ONLY runtime/stdlib
/// archive: the `--link-lib` fixture appears exactly once and NO
/// `libhew_std.a` token appears — even when a stray dev-build
/// `target/{debug,release}/libhew_std.a` exists. Before the fix,
/// `link_executable` probed for that archive best-effort and dev layouts
/// carried a second full runtime copy on the line; an FFI consumer archive
/// then died on duplicate `hew_*` symbols against the fat-LTO release
/// `libhew.a`. The compiler argv is captured with a PATH shim that logs one
/// argument per line, then execs the real driver.
#[test]
fn link_line_carries_single_runtime_archive() {
    require_codegen();
    let dir = tempdir();

    // Shadow whichever driver the host resolves (`select_native_compiler`
    // prefers clang, falls back to cc). Resolve the REAL path before
    // prepending the shim dir to PATH.
    let Some((tool_name, real_tool)) = ["clang", "cc"]
        .iter()
        .find_map(|name| resolve_tool(name).map(|path| (*name, path)))
    else {
        eprintln!("SKIP: neither clang nor cc is resolvable on this host");
        return;
    };

    let shim_dir = dir.path().join("shim-bin");
    std::fs::create_dir_all(&shim_dir).expect("create shim dir");
    let argv_log = dir.path().join("argv.log");
    let shim_path = shim_dir.join(tool_name);
    std::fs::write(
        &shim_path,
        format!(
            "#!/bin/sh\nprintf '%s\\n' \"$@\" >> {}\nexec {} \"$@\"\n",
            argv_log.display(),
            real_tool.display(),
        ),
    )
    .expect("write shim script");
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(&shim_path, std::fs::Permissions::from_mode(0o755))
            .expect("chmod shim script");
    }

    let Some(lib) = build_staticlib(
        dir.path(),
        "argv_ffi",
        r#"#[no_mangle]
pub extern "C" fn argv_fixture_value() -> i64 { 7 }
"#,
    ) else {
        return;
    };

    let prog = write_program(
        dir.path(),
        "argv_prog",
        r#"extern "C" { fn argv_fixture_value() -> i64; }
fn main() {
    let v: i64 = unsafe { argv_fixture_value() };
    println(f"{v}");
}
"#,
    );
    let out_bin = dir.path().join("argv_out");

    let orig_path = std::env::var("PATH").unwrap_or_default();
    let mut cmd = Command::new(hew_binary());
    cmd.arg("build")
        .arg("--link-lib")
        .arg(&lib)
        .arg(&prog)
        .arg("-o")
        .arg(&out_bin)
        .env("PATH", format!("{}:{orig_path}", shim_dir.display()))
        .current_dir(dir.path());
    let out = run_bounded_command(cmd, "hew build --link-lib (argv shim)");
    assert!(
        out.status.success(),
        "shimmed link must succeed:\n{}",
        describe_output(&out),
    );

    let log = std::fs::read_to_string(&argv_log).expect("shim must have captured a linker argv");
    let tokens: Vec<&str> = log.lines().collect();
    let fixture_count = tokens
        .iter()
        .filter(|token| token.ends_with("libargv_ffi.a"))
        .count();
    assert_eq!(
        fixture_count, 1,
        "the --link-lib archive must appear exactly once on the link line:\n{log}",
    );
    assert!(
        !tokens.iter().any(|token| token.contains("libhew_std.a")),
        "libhew.a is the single runtime/stdlib archive — libhew_std.a must \
         never be on the native link line:\n{log}",
    );
    assert!(
        tokens.iter().any(|token| token.ends_with("libhew.a")),
        "the runtime archive libhew.a must be on the link line:\n{log}",
    );
}

/// In-repo miniature of the mqtt/nats ecosystem shape: a hew-cabi-dependent
/// package archive plus a program that BOTH spawns an actor (pulling the
/// actor-runtime members) and calls the package's extern (pulling the package
/// member whose `hew_*` refs are undefined imports). Pre-fix, dev layouts
/// carried TWO runtime-defining archives on the line, and this exact pull
/// pattern is what turned the redundancy into a duplicate-symbol hard error.
#[test]
fn actor_spawn_with_cabi_staticlib_links_and_runs() {
    require_codegen();
    let dir = tempdir();

    let Some(lib) = build_cabi_wrapper_staticlib(
        dir.path(),
        "actorcabi",
        r#"//! Fixture package: hew-cabi-dependent, exercised alongside an actor spawn.
use hew_cabi::sink::set_last_error;

#[no_mangle]
pub extern "C" fn actorcabi_probe() -> i64 {
    set_last_error("actorcabi: probe".to_string());
    11
}
"#,
    ) else {
        return;
    };
    assert_archive_symbol_is_undefined(&lib, "hew_stream_set_last_error");

    let prog = write_program(
        dir.path(),
        "actorcabi_prog",
        r#"extern "C" { fn actorcabi_probe() -> i64; }

actor Adder {
    var total: i64 = 0;
    receive fn add(n: i64) -> i64 {
        total = total + n;
        total
    }
}

fn main() {
    let ffi: i64 = unsafe { actorcabi_probe() };
    let adder = spawn Adder();
    match await adder.add(ffi) {
        Ok(v) => println(f"total={v}"),
        Err(_) => println("err"),
    }
}
"#,
    );
    let out_bin = dir.path().join("actorcabi_out");

    let link = hew_build(Some(&lib), &prog, &out_bin, dir.path());
    assert!(
        link.status.success(),
        "an actor-spawning program with a hew-cabi package must link cleanly:\n{}",
        describe_output(&link),
    );
    let link_stderr = String::from_utf8_lossy(&link.stderr);
    assert!(
        !link_stderr.contains("duplicate"),
        "unexpected duplicate-symbol error linking actor + hew-cabi package:\n{link_stderr}",
    );

    let mut run = Command::new(&out_bin);
    run.current_dir(dir.path());
    let run = run_bounded_command(run, "run actorcabi_out");
    assert!(
        run.status.success(),
        "linked binary must run:\n{}",
        describe_output(&run)
    );
    assert_eq!(
        String::from_utf8_lossy(&run.stdout).trim(),
        "total=11",
        "expected the actor to echo the extern's exact value:\n{}",
        describe_output(&run),
    );
}

/// `hew run --link-lib <archive>` must thread the archive into the native link
/// step exactly like `hew build --link-lib` does. Before the fix,
/// `compile_temp_artifact` passed a literal `&[]` to `compile_build_binary`,
/// silently dropping the flag — the link then failed with an undefined
/// `hew_fixture_value` reference.
#[test]
fn run_link_lib_links_and_executes() {
    require_codegen();
    let dir = tempdir();

    let Some(lib) = build_staticlib(
        dir.path(),
        "run_ffi",
        r#"#[no_mangle]
pub extern "C" fn hew_fixture_value() -> i32 { 42 }
"#,
    ) else {
        return;
    };

    let prog = write_program(
        dir.path(),
        "run_prog",
        r#"extern "C" { fn hew_fixture_value() -> i32; }
fn main() {
    let v: i32 = unsafe { hew_fixture_value() };
    println(f"{v}");
}
"#,
    );

    let mut cmd = Command::new(hew_binary());
    cmd.arg("run")
        .arg("--link-lib")
        .arg(&lib)
        .arg(&prog)
        .current_dir(dir.path());
    let out = run_bounded_command(cmd, "hew run --link-lib");
    assert!(
        out.status.success(),
        "`hew run --link-lib` must link the archive and execute:\n{}",
        describe_output(&out),
    );
    // Exact value: `len() > 0`-style assertions pass on garbage.
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert_eq!(
        stdout.trim(),
        "42",
        "expected the extern's exact value on stdout:\n{}",
        describe_output(&out),
    );
}

/// `hew run --target wasm32-wasi --link-lib …` must be rejected loudly (exit 2)
/// BEFORE any compilation — a native archive cannot be linked into a wasm
/// module, and silently dropping the flag is fail-open. The archive path
/// deliberately does not exist: the guard must fire before anything touches it.
#[test]
fn run_wasi_link_lib_rejected() {
    let dir = tempdir();
    let prog = write_program(
        dir.path(),
        "wasi_prog",
        "fn main() {\n    println(\"unreachable\");\n}\n",
    );

    let mut cmd = Command::new(hew_binary());
    cmd.arg("run")
        .arg("--target")
        .arg("wasm32-wasi")
        .arg("--link-lib")
        .arg("libdoes_not_exist.a")
        .arg(&prog)
        .current_dir(dir.path());
    let out = run_bounded_command(cmd, "hew run --target wasm32-wasi --link-lib");
    assert_eq!(
        out.status.code(),
        Some(2),
        "wasi run with --link-lib must exit 2 (usage error):\n{}",
        describe_output(&out),
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("--link-lib is not supported for WASI targets"),
        "expected the loud WASI rejection message:\n{}",
        describe_output(&out),
    );
    assert!(
        out.stdout.is_empty(),
        "the program must not run:\n{}",
        describe_output(&out),
    );
}

/// `hew build --target wasm32-unknown-unknown --link-lib …` must be rejected
/// loudly (exit 2) — the wasm branch of the build path used to silently ignore
/// the archives.
#[test]
fn build_wasm_link_lib_rejected() {
    let dir = tempdir();
    let prog = write_program(
        dir.path(),
        "wasm_prog",
        "fn main() {\n    println(\"unreachable\");\n}\n",
    );

    let mut cmd = Command::new(hew_binary());
    cmd.arg("build")
        .arg("--target")
        .arg("wasm32-unknown-unknown")
        .arg("--link-lib")
        .arg("libdoes_not_exist.a")
        .arg(&prog)
        .arg("-o")
        .arg(dir.path().join("wasm_out"))
        .current_dir(dir.path());
    let out = run_bounded_command(cmd, "hew build --target wasm32-unknown-unknown --link-lib");
    assert_eq!(
        out.status.code(),
        Some(2),
        "wasm build with --link-lib must exit 2 (usage error):\n{}",
        describe_output(&out),
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("--link-lib is not supported for wasm targets"),
        "expected the loud wasm rejection message:\n{}",
        describe_output(&out),
    );
}

/// A minimal `extern "C"` function resolves and runs when its staticlib is
/// supplied via `--link-lib`. (`mvp_add(2, 3) == 5`.)
#[test]
fn ffi_link_mvp_add_extern_links_and_runs() {
    require_codegen();
    let dir = tempdir();

    let Some(lib) = build_staticlib(
        dir.path(),
        "mvp_ffi",
        r#"#[no_mangle]
pub extern "C" fn mvp_add(a: i64, b: i64) -> i64 { a + b }
"#,
    ) else {
        return;
    };

    let prog = write_program(
        dir.path(),
        "mvp_prog",
        r#"extern "C" { fn mvp_add(a: i64, b: i64) -> i64; }
fn main() {
    let r: i64 = unsafe { mvp_add(2, 3) };
    println(f"mvp_add={r}");
}

"#,
    );
    let out_bin = dir.path().join("mvp_out");

    let link = hew_build(Some(&lib), &prog, &out_bin, dir.path());
    assert!(
        link.status.success(),
        "linking a minimal extern via --link-lib must succeed:\n{}",
        describe_output(&link),
    );

    let mut run = Command::new(&out_bin);
    run.current_dir(dir.path());
    let run = run_bounded_command(run, "run mvp_out");
    assert!(
        run.status.success(),
        "linked binary must run:\n{}",
        describe_output(&run),
    );
    let stdout = String::from_utf8_lossy(&run.stdout);
    assert!(
        stdout.contains("mvp_add=5"),
        "expected `mvp_add=5`, got:\n{stdout}"
    );
}

/// A foreign `*const i64` return crosses Hew as `&i64`, remains non-owning in
/// an inferred ordinary local, and can be passed back to a foreign reader.
#[test]
fn ffi_borrow_boundary_links_and_prints_exact_values() {
    require_codegen();
    let dir = tempdir();
    let Some(lib) = build_staticlib(dir.path(), "borrow_boundary", BORROW_BOUNDARY_RUST) else {
        return;
    };
    let program = write_program(dir.path(), "borrow_boundary_values", BORROW_BOUNDARY_HEW);
    let binary = dir.path().join("borrow_boundary_values");

    let link = hew_build(Some(&lib), &program, &binary, dir.path());
    assert!(
        link.status.success(),
        "foreign borrow boundary must link:\n{}",
        describe_output(&link),
    );

    let run = run_bounded_command(Command::new(&binary), "run foreign borrow boundary");
    assert!(
        run.status.success(),
        "foreign borrow boundary must run:\n{}",
        describe_output(&run),
    );
    assert_eq!(
        run.stdout,
        b"-9223372036854775808,-1,0,9223372036854775807\n",
        "foreign boundary values must remain byte-exact:\n{}",
        describe_output(&run),
    );
}

/// Repeated foreign-returned views neither acquire ownership nor trigger a
/// release under the poisoned allocator and leak-slope oracles.
#[test]
fn ffi_borrow_boundary_has_no_drop_or_leak_slope() {
    require_codegen();
    let dir = tempdir();
    let Some(lib) = build_staticlib(dir.path(), "borrow_boundary", BORROW_BOUNDARY_RUST) else {
        return;
    };

    let build_loop = |iterations, name: &str| {
        let program = write_program(dir.path(), name, &borrow_boundary_loop_source(iterations));
        let binary = dir.path().join(name);
        let link = hew_build(Some(&lib), &program, &binary, dir.path());
        assert!(
            link.status.success(),
            "foreign borrow loop must link:\n{}",
            describe_output(&link),
        );
        binary
    };

    let high_binary = build_loop(HIGH_FRAMES, "borrow_boundary_high");
    let mut scribble = Command::new(&high_binary);
    scribble
        .env("MallocScribble", "1")
        .env("MallocPreScribble", "1")
        .env("MallocGuardEdges", "1");
    let scribble = run_bounded_command(scribble, "run foreign borrow boundary under scribble");
    assert!(
        scribble.status.success(),
        "foreign borrow boundary must not release static pointers:\n{}",
        describe_output(&scribble),
    );

    if !leaks_supported("ffi_borrow_boundary") {
        return;
    }
    let low_binary = build_loop(LOW_FRAMES, "borrow_boundary_low");
    let Some(low_leaks) = measure_leaks(&low_binary) else {
        return;
    };
    let Some(high_leaks) = measure_leaks(&high_binary) else {
        return;
    };
    assert!(
        high_leaks <= low_leaks + SLOPE_TOLERANCE,
        "foreign borrow leak slope exceeded tolerance: low={low_leaks}, \
         high={high_leaks}, tolerance={SLOPE_TOLERANCE}",
    );
}

/// An unresolved `extern` (no `--link-lib`) must FAIL the link — fail-closed,
/// never a silent success.
#[test]
fn ffi_link_unresolved_extern_fails_closed() {
    require_codegen();
    let dir = tempdir();

    let prog = write_program(
        dir.path(),
        "noload_prog",
        r#"extern "C" { fn mvp_add(a: i64, b: i64) -> i64; }
fn main() {
    let r: i64 = unsafe { mvp_add(2, 3) };
    println(f"mvp_add={r}");
}
"#,
    );
    let out_bin = dir.path().join("noload_out");

    // No --link-lib: `mvp_add` is unresolved.
    let out = hew_build(None, &prog, &out_bin, dir.path());
    assert!(
        !out.status.success(),
        "an unresolved extern must fail the link (fail-closed), but it succeeded:\n{}",
        describe_output(&out),
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("mvp_add") || stderr.to_lowercase().contains("undefined"),
        "expected an undefined-symbol error mentioning `mvp_add`:\n{stderr}",
    );
}

/// A native module that imports the runtime stream-error channel directly
/// (a hand-declared `extern "C" fn hew_stream_set_last_error`) must link cleanly
/// with NO duplicate-symbol error. This is a fast, dependency-free check that
/// the `hew_stream_*` C ABI is owned by libhew.a alone, so a consumer resolves
/// it at final link. The stronger, realistic guard — a package that pulls in the
/// `hew-cabi` crate — is `ffi_link_hew_cabi_wrapper_links_clean` below.
#[test]
fn ffi_link_runtime_channel_importer_links_clean() {
    require_codegen();
    let dir = tempdir();

    let Some(lib) = build_staticlib(
        dir.path(),
        "imp_ffi",
        r#"extern "C" {
    fn hew_stream_set_last_error(ptr: *const u8, len: usize);
}

#[no_mangle]
pub extern "C" fn ffi_report_error() -> i64 {
    let msg = b"ffi: simulated failure";
    // SAFETY: ptr/len describe a valid byte range; the runtime copies it.
    unsafe { hew_stream_set_last_error(msg.as_ptr(), msg.len()) };
    -1
}
"#,
    ) else {
        return;
    };

    let prog = write_program(
        dir.path(),
        "imp_prog",
        r#"extern "C" { fn ffi_report_error() -> i64; }
fn main() {
    let r: i64 = unsafe { ffi_report_error() };
    println(f"report={r}");
}
"#,
    );
    let out_bin = dir.path().join("imp_out");

    let link = hew_build(Some(&lib), &prog, &out_bin, dir.path());
    assert!(
        link.status.success(),
        "a module importing the runtime error channel must link cleanly \
         (no duplicate symbol):\n{}",
        describe_output(&link),
    );

    let mut run = Command::new(&out_bin);
    run.current_dir(dir.path());
    let run = run_bounded_command(run, "run imp_out");
    assert!(
        run.status.success(),
        "linked binary must run:\n{}",
        describe_output(&run)
    );
    assert!(
        String::from_utf8_lossy(&run.stdout).contains("report=-1"),
        "expected `report=-1`, got:\n{}",
        String::from_utf8_lossy(&run.stdout),
    );
}

/// The REAL ecosystem-module shape: a `staticlib` that depends on the `hew-cabi`
/// crate and calls `hew_cabi::sink::set_last_error`. It must link cleanly with
/// NO duplicate symbol.
///
/// This is the load-bearing regression guard. Before the fix, hew-cabi defined
/// `hew_stream_last_error` / `hew_stream_last_errno` as `#[no_mangle]` exports,
/// so every package that linked hew-cabi (i.e. every native ecosystem module)
/// bundled its own copy, which collided with libhew.a's copy at final link.
/// After the fix, hew-cabi declares those symbols as `extern "C"` imports and
/// forwards through mangled (per-crate-hash, non-colliding) Rust wrappers, so
/// the package references the C ABI as undefined and resolves it against
/// libhew.a — proving a real `hew-cabi`-dependent package now links.
#[test]
fn ffi_link_hew_cabi_wrapper_links_clean() {
    require_codegen();
    let dir = tempdir();

    // The wrapper calls hew-cabi's safe forwarder, which (post-fix) references
    // the runtime-owned `hew_stream_set_last_error` as an undefined import.
    let Some(lib) = build_cabi_wrapper_staticlib(
        dir.path(),
        "cabiwrap",
        r#"//! Fixture package: depends on hew-cabi, exercises its error-channel wrapper.
use hew_cabi::sink::set_last_error;

#[no_mangle]
pub extern "C" fn cabiwrap_report() -> i64 {
    set_last_error("cabiwrap: simulated failure".to_string());
    -7
}
"#,
    ) else {
        return;
    };

    // The substrate invariant, asserted structurally (the std/net/http
    // acceptance criterion): the package archive references the runtime-owned
    // setter as an undefined import — it must NOT bundle a `#[no_mangle]`
    // definition that would collide with libhew.a.
    assert_archive_symbol_is_undefined(&lib, "hew_stream_set_last_error");

    let prog = write_program(
        dir.path(),
        "cabiwrap_prog",
        r#"extern "C" { fn cabiwrap_report() -> i64; }
fn main() {
    let r: i64 = unsafe { cabiwrap_report() };
    println(f"cabiwrap={r}");
}
"#,
    );
    let out_bin = dir.path().join("cabiwrap_out");

    let link = hew_build(Some(&lib), &prog, &out_bin, dir.path());
    assert!(
        link.status.success(),
        "a staticlib that depends on hew-cabi must link cleanly (no duplicate \
         `hew_stream_*` symbol):\n{}",
        describe_output(&link),
    );
    // Guard against a false pass: if the substrate fix regressed, the link error
    // would be a duplicate symbol. Make that explicit in the failure surface.
    let link_stderr = String::from_utf8_lossy(&link.stderr);
    assert!(
        !link_stderr.contains("duplicate"),
        "unexpected duplicate-symbol error linking a hew-cabi package:\n{link_stderr}",
    );

    let mut run = Command::new(&out_bin);
    run.current_dir(dir.path());
    let run = run_bounded_command(run, "run cabiwrap_out");
    assert!(
        run.status.success(),
        "linked binary must run:\n{}",
        describe_output(&run)
    );
    assert!(
        String::from_utf8_lossy(&run.stdout).contains("cabiwrap=-7"),
        "expected `cabiwrap=-7`, got:\n{}",
        String::from_utf8_lossy(&run.stdout),
    );
}

/// A mis-shaped module that re-defines a runtime-owned `#[no_mangle]` symbol
/// (`hew_stream_last_error`) must FAIL the link with the actionable fail-closed
/// diagnostic — not silently produce a broken binary.
#[test]
fn ffi_link_duplicate_runtime_symbol_fails_closed_with_hint() {
    require_codegen();
    let dir = tempdir();

    let Some(lib) = build_staticlib(
        dir.path(),
        "dup_ffi",
        r#"#[no_mangle]
pub extern "C" fn dup_add(a: i64, b: i64) -> i64 { a + b }

// Mis-shaped: re-defines a symbol that libhew.a already owns.
#[no_mangle]
pub extern "C" fn hew_stream_last_error() -> *const core::ffi::c_char {
    core::ptr::null()
}
"#,
    ) else {
        return;
    };

    let prog = write_program(
        dir.path(),
        "dup_prog",
        r#"extern "C" { fn dup_add(a: i64, b: i64) -> i64; }
fn main() {
    let r: i64 = unsafe { dup_add(2, 3) };
    println(f"dup={r}");
}
"#,
    );
    let out_bin = dir.path().join("dup_out");

    let out = hew_build(Some(&lib), &prog, &out_bin, dir.path());
    assert!(
        !out.status.success(),
        "a module re-defining a runtime symbol must fail the link (fail-closed):\n{}",
        describe_output(&out),
    );
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        stderr.contains("duplicate Hew runtime symbol") && stderr.contains("hew_stream_last_error"),
        "expected the fail-closed duplicate-symbol diagnostic naming the symbol:\n{stderr}",
    );
}
