use std::env;
use std::path::{Path, PathBuf};
use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=src/llvm_debug_info_shim.cpp");

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

fn llvm_config_path() -> PathBuf {
    if let Some(prefix) = env::var_os("LLVM_SYS_221_PREFIX") {
        return PathBuf::from(prefix)
            .join("bin")
            .join(executable("llvm-config"));
    }

    [
        PathBuf::from("llvm-config-22"),
        PathBuf::from("/opt/homebrew/opt/llvm/bin/llvm-config"),
        PathBuf::from("/usr/local/opt/llvm/bin/llvm-config"),
        PathBuf::from("/usr/lib/llvm-22/bin/llvm-config"),
        PathBuf::from("llvm-config"),
    ]
    .into_iter()
    .find(|candidate| command_works(candidate))
    .unwrap_or_else(|| panic!("LLVM 22 llvm-config is not available; set LLVM_SYS_221_PREFIX"))
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

fn command_works(command: &Path) -> bool {
    Command::new(command)
        .arg("--version")
        .output()
        .is_ok_and(|output| output.status.success())
}

fn executable(name: &str) -> String {
    format!("{name}{}", env::consts::EXE_SUFFIX)
}
