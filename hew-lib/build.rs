fn main() {
    let target_os = std::env::var("CARGO_CFG_TARGET_OS").expect("Cargo supplies the target OS");
    match target_os.as_str() {
        "linux" | "freebsd" => {
            println!("cargo:rustc-link-arg-cdylib=-Wl,-soname,libhew.so");
        }
        "macos" => {
            println!("cargo:rustc-link-arg-cdylib=-Wl,-install_name,@rpath/libhew.dylib");
        }
        _ => {}
    }
}
