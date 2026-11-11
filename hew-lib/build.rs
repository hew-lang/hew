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
    if std::env::var("TARGET").unwrap().ends_with("-windows-msvc") {
        let out = std::path::PathBuf::from(std::env::var_os("OUT_DIR").unwrap());
        // Cargo's OUT_DIR is <profile>/build/<package>/out. Keep the runtime
        // DLL's PDB beside that image with its own basename; the driver owns
        // the ordinary hew.pdb. Two concurrent links must not share a PDB.
        let profile = out.ancestors().nth(3).expect("Cargo profile directory");
        println!(
            "cargo:rustc-link-arg-cdylib=/PDB:{}",
            profile.join("hew-runtime.pdb").display()
        );
    }
}
