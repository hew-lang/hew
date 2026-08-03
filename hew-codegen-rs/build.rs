use std::env;
use std::path::{Path, PathBuf};
use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=src/llvm_debug_info_shim.cpp");

    let llvm_config = llvm_config_path();
    let include_dir = llvm_config_arg(&llvm_config, "--includedir");

    let mut build = cc::Build::new();
    build
        .cpp(true)
        .file("src/llvm_debug_info_shim.cpp")
        .include(include_dir)
        .flag_if_supported("-std=c++17")
        .flag_if_supported("/std:c++17")
        .warnings(false)
        .compile("hew_llvm_debug_info_shim");
}

fn llvm_config_path() -> PathBuf {
    if let Some(path) = env::var_os("DEP_LLVM_22_CONFIG_PATH") {
        return PathBuf::from(path);
    }
    if let Some(prefix) = env::var_os("LLVM_SYS_221_PREFIX") {
        return PathBuf::from(prefix)
            .join("bin")
            .join(executable("llvm-config"));
    }

    [
        PathBuf::from("llvm-config-22"),
        PathBuf::from("llvm-config"),
        PathBuf::from("/opt/homebrew/opt/llvm/bin/llvm-config"),
        PathBuf::from("/usr/local/opt/llvm/bin/llvm-config"),
        PathBuf::from("/usr/lib/llvm-22/bin/llvm-config"),
    ]
    .into_iter()
    .find(|candidate| command_works(candidate))
    .unwrap_or_else(|| {
        panic!(
            "LLVM 22 llvm-config was not provided by llvm-sys and is not on PATH; \
                 set LLVM_SYS_221_PREFIX"
        )
    })
}

fn llvm_config_arg(llvm_config: &Path, arg: &str) -> PathBuf {
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
    PathBuf::from(
        String::from_utf8(output.stdout)
            .expect("llvm-config output must be UTF-8")
            .trim(),
    )
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
