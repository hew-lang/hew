fn main() {
    // Pure Rust runtime — no C compilation needed.
    // Link pthread and math on Unix targets.
    #[cfg(unix)]
    {
        println!("cargo:rustc-link-lib=pthread");
        println!("cargo:rustc-link-lib=m");
    }
    #[cfg(windows)]
    {
        println!("cargo:rustc-link-lib=ws2_32");
        println!("cargo:rustc-link-lib=userenv");
    }
}
