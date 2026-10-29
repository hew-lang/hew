fn main() {
    println!("cargo:rustc-check-cfg=cfg(hew_fault_loom)");
    if std::env::var_os("CARGO_FEATURE_FAULT_LOOM").is_some() {
        println!("cargo:rustc-cfg=hew_fault_loom");
    }
    println!("cargo:rustc-check-cfg=cfg(hew_protocol_host)");
    println!("cargo:rustc-cfg=hew_protocol_host");
}
