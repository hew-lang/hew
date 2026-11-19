#[path = "llvm_flags.rs"]
mod llvm_flags;

use std::env;
use std::path::Path;
use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=src/physical_debug_shim.cpp");
    println!("cargo:rerun-if-changed=llvm_flags.rs");
    // llvm-sys is the single authority on the LLVM release this crate binds:
    // its `links = "llvm-<major>"` key names the configuration path it hands
    // dependents, and its build script owns `LLVM_SYS_<ver>_PREFIX`. Deriving
    // the major from that key keeps this script free of a second version pin.
    let (llvm_sys_major, llvm_config) = llvm_sys_config();
    let llvm_config = Path::new(&llvm_config);
    let version = llvm_config_output(llvm_config, "--version");
    let major = version
        .split('.')
        .next()
        .and_then(|part| part.parse::<u32>().ok());
    assert_eq!(
        major,
        Some(llvm_sys_major),
        "llvm-sys binds LLVM {llvm_sys_major} but {} reports {version}",
        llvm_config.display()
    );
    // ASan execution links with the same toolchain used by llvm-sys.
    println!(
        "cargo:rustc-env=HEW_LLVM_BINDIR={}",
        llvm_config_output(llvm_config, "--bindir")
    );

    // The debug shim calls LLVM's C++ IR API directly, so it must
    // see the headers and the ABI flags of the very install llvm-sys links —
    // `DEP_LLVM_<major>_CONFIG_PATH` is that install, and `--cxxflags` carries the
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

/// The LLVM major llvm-sys selected and its `llvm-config` path, read from
/// the `DEP_LLVM_<major>_CONFIG_PATH` metadata its build script exports.
fn llvm_sys_config() -> (u32, std::ffi::OsString) {
    let mut found = env::vars_os().filter_map(|(key, value)| {
        let name = key.to_str()?;
        let major = name
            .strip_prefix("DEP_LLVM_")?
            .strip_suffix("_CONFIG_PATH")?;
        // A llvm-sys metadata key always names its LLVM major; anything else
        // is a toolchain fault, not a variable to skip.
        let major = major
            .parse::<u32>()
            .unwrap_or_else(|_| panic!("llvm-sys exported {name}, which names no LLVM major"));
        Some((major, value))
    });
    let selected = found
        .next()
        .expect("llvm-sys must provide its selected LLVM configuration path");
    assert!(
        found.next().is_none(),
        "more than one llvm-sys release is linked into hew-codegen-rs"
    );
    selected
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
