#[path = "llvm_flags.rs"]
mod llvm_flags;

use std::env;
use std::path::Path;
use std::process::Command;

fn main() {
    println!("cargo:rerun-if-env-changed=LLVM_SYS_221_PREFIX");
    println!("cargo:rerun-if-changed=src/physical_debug_shim.cpp");
    println!("cargo:rerun-if-changed=llvm_flags.rs");
    let llvm_config = env::var_os("DEP_LLVM_22_CONFIG_PATH")
        .expect("llvm-sys must provide its selected LLVM configuration path");
    let llvm_config = Path::new(&llvm_config);
    let version = llvm_config_output(llvm_config, "--version");
    let major = version
        .split('.')
        .next()
        .and_then(|part| part.parse::<u32>().ok());
    assert_eq!(major, Some(22), "hew-codegen-rs requires LLVM major 22");
    // ASan execution links with the same toolchain used by llvm-sys.
    println!(
        "cargo:rustc-env=HEW_LLVM_BINDIR={}",
        llvm_config_output(llvm_config, "--bindir")
    );

    // The debug shim calls LLVM's C++ IR API directly, so it must
    // see the headers and the ABI flags of the very install llvm-sys links —
    // `DEP_LLVM_22_CONFIG_PATH` is that install, and `--cxxflags` carries the
    // `LLVM_ENABLE_ABI_BREAKING_CHECKS` and RTTI settings its libraries were
    // built with. A mismatch is a silent struct-layout corruption inside
    // LLVM, not a link error. `cc` compiles this into a static archive
    // the crate links, so the `hew` binary gains no dynamic LLVM dependency.
    let include_dir = llvm_config_output(llvm_config, "--includedir");
    let cxx_flags = llvm_config_output(llvm_config, "--cxxflags");
    let flags = llvm_flags::cxx_flags(&cxx_flags, &include_dir)
        .expect("cannot parse llvm-config C++ flags");
    cc::Build::new()
        .cpp(true)
        .file("src/physical_debug_shim.cpp")
        .include(include_dir)
        .flags(flags)
        .flag_if_supported("-std=c++17")
        .flag_if_supported("/std:c++17")
        .warnings(false)
        .compile("hew_physical_debug_shim");
}

fn llvm_config_output(llvm_config: &Path, arg: &str) -> String {
    let output = Command::new(llvm_config)
        .arg(arg)
        .output()
        .unwrap_or_else(|error| panic!("failed to run {}: {error}", llvm_config.display()));
    assert!(
        output.status.success(),
        "{} {arg} failed with status {}",
        llvm_config.display(),
        output.status
    );
    String::from_utf8(output.stdout)
        .expect("llvm-config output must be UTF-8")
        .trim()
        .to_owned()
}
