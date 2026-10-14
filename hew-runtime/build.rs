fn main() {
    println!("cargo:rerun-if-env-changed=LLVM_SYS_221_PREFIX");
    println!("cargo:rerun-if-changed=src/coro_llvm.cpp");
    // The coroutine builtins lower the indirect calls with LLVM's calling
    // convention. Prefer the compiler's configured LLVM installation.
    let clang = std::env::var_os("LLVM_SYS_221_PREFIX").map_or_else(
        || std::path::PathBuf::from("clang"),
        |prefix| {
            std::path::PathBuf::from(prefix)
                .join("bin")
                .join(if cfg!(windows) { "clang.exe" } else { "clang" })
        },
    );
    cc::Build::new()
        .compiler(clang)
        .cpp(true)
        .cpp_link_stdlib(None)
        .flag("-std=c++20")
        .file("src/coro_llvm.cpp")
        .flag("-fexceptions")
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
