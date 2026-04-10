fn main() {
    // Pure Rust runtime — no C compilation needed.
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
