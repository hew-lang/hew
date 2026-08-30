use std::env;
use std::path::{Path, PathBuf};
use std::process::Command;

fn main() {
    export_runtime_symbols_to_jit_hosts();
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

/// Make this crate's test binaries export their dynamic symbol table.
///
/// The `exec` tests JIT-compile an emitted module and call it in-process, so
/// MCJIT has to resolve the module's `hew_*` runtime references back to the
/// runtime linked into the test binary. Mach-O puts every global symbol in the
/// dynamic symbol table, so the JIT's process-symbol resolver finds them and
/// the same test passes on macOS. ELF exports nothing from an executable
/// unless the link asks, so on Linux that resolver found nothing, MCJIT left
/// the relocation at address 0, and the JIT-compiled call dereferenced null -
/// a bare SIGSEGV with no diagnostic.
///
/// `--export-dynamic` gives ELF the same property, which makes the test
/// binary's own dynamic symbol table the single authority for which runtime
/// symbols a JIT-executed test can see, on both platforms.
fn export_runtime_symbols_to_jit_hosts() {
    // Windows resolves JIT symbols from the PE export table and has no
    // equivalent flag; the JIT exec tests are `#[cfg(unix)]` anyway.
    if env::var("CARGO_CFG_TARGET_OS").as_deref() == Ok("linux") {
        println!("cargo:rustc-link-arg-tests=-Wl,--export-dynamic");
    }
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
