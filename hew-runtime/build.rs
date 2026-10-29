fn main() {
    println!("cargo:rustc-check-cfg=cfg(hew_fault_loom)");
    println!("cargo:rustc-check-cfg=cfg(kani)");
    println!("cargo:rustc-check-cfg=cfg(hew_protocol_host)");
    println!("cargo:rerun-if-env-changed=LLVM_SYS_221_PREFIX");
    println!("cargo:rerun-if-changed=src/coro_llvm.cpp");
    // The coroutine builtins lower the indirect calls with LLVM's calling
    // convention. Prefer the compiler's configured LLVM installation.
    let msvc = std::env::var("CARGO_CFG_TARGET_ENV").is_ok_and(|target| target == "msvc");
    let compiler = if msvc { "clang-cl" } else { "clang" };
    let clang = std::env::var_os("LLVM_SYS_221_PREFIX").map_or_else(
        || std::path::PathBuf::from(compiler),
        |prefix| std::path::PathBuf::from(prefix).join("bin").join(compiler),
    );
    #[cfg(windows)]
    let clang = clang.with_extension("exe");
    cc::Build::new()
        .compiler(clang)
        .cpp(true)
        .cpp_link_stdlib(None)
        .std("c++20")
        .file("src/coro_llvm.cpp")
        .flag(if msvc { "/EHsc" } else { "-fexceptions" })
        .compile("hew_coro_llvm");

    // Build scripts run on the host, so use Cargo's target cfg env vars rather
    // than `#[cfg(unix)]`/`#[cfg(windows)]` to avoid leaking native link flags
    // into cross-target WASM test binaries.
    if std::env::var_os("CARGO_CFG_UNIX").is_some() {
        println!("cargo:rustc-link-lib=pthread");
        println!("cargo:rustc-link-lib=m");
    } else if std::env::var_os("CARGO_CFG_WINDOWS").is_some() {
        println!("cargo:rustc-link-lib=ws2_32");
        println!("cargo:rustc-link-lib=userenv");
    }
}
