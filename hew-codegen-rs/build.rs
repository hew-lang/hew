use std::env;
use std::path::Path;
use std::process::Command;

fn main() {
    println!("cargo:rerun-if-env-changed=LLVM_SYS_221_PREFIX");
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
