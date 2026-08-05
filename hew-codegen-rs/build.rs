use std::env;
use std::path::{Path, PathBuf};
use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=src/llvm_debug_info_shim.cpp");
    // llvm-sys re-resolves its LLVM install when this changes; the shim must
    // re-compile against the SAME install, or a cached shim object built from
    // the old headers links into a compiler using the new libLLVM — a C++
    // struct-layout mismatch inside DIBuilder.
    println!("cargo:rerun-if-env-changed=LLVM_SYS_221_PREFIX");

    let llvm_config = llvm_config_path();
    assert_llvm_22(&llvm_config);
    let include_dir = llvm_config_arg(&llvm_config, "--includedir");
    let cxxflags = llvm_config_output(&llvm_config, "--cxxflags");

    let mut build = cc::Build::new();
    build
        .cpp(true)
        .file("src/llvm_debug_info_shim.cpp")
        .include(include_dir)
        .flags(cxxflags.split_whitespace())
        .flag_if_supported("-std=c++17")
        .flag_if_supported("/std:c++17")
        .warnings(false)
        .compile("hew_llvm_debug_info_shim");
}

/// The `llvm-config` llvm-sys itself used, threaded through Cargo's
/// links-metadata (`links = "llvm-22"` + `cargo:config_path=` ⇒
/// `DEP_LLVM_22_CONFIG_PATH`). One source of truth: an independent probe here
/// could land on a DIFFERENT LLVM 22 install than the one llvm-sys links —
/// same major, different `LLVM_ENABLE_ABI_BREAKING_CHECKS` layout — and the
/// major-version gate below would not catch it.
fn llvm_config_path() -> PathBuf {
    let path = env::var_os("DEP_LLVM_22_CONFIG_PATH").unwrap_or_else(|| {
        panic!(
            "DEP_LLVM_22_CONFIG_PATH is not set; hew-codegen-rs must declare a \
             direct `llvm-sys` dependency so the debug-info shim compiles \
             against the exact LLVM install llvm-sys links"
        )
    });
    PathBuf::from(path)
}

fn llvm_config_arg(llvm_config: &Path, arg: &str) -> PathBuf {
    PathBuf::from(llvm_config_output(llvm_config, arg))
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

fn assert_llvm_22(llvm_config: &Path) {
    let version = llvm_config_output(llvm_config, "--version");
    let major = version
        .split('.')
        .next()
        .and_then(|component| component.parse::<u32>().ok());
    assert_eq!(
        major,
        Some(22),
        "{} reports LLVM {version}; hew-codegen-rs requires LLVM major 22",
        llvm_config.display()
    );
}
