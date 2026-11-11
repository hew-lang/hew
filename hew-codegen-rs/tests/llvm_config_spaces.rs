//! Exercise LLVM's unquoted include path when its installation contains spaces.

#[path = "../llvm_flags.rs"]
mod llvm_flags;

#[test]
fn quoted_flags_remain_single_arguments() {
    let flags = llvm_flags::cxx_flags(
        "-I/opt/LLVM with spaces/include -DNAME='two words' -fno-exceptions",
        "/opt/LLVM with spaces/include",
    )
    .unwrap();
    assert_eq!(flags, ["-DNAME=two words", "-fno-exceptions"]);
    assert!(llvm_flags::cxx_flags("-DNAME='unterminated", "/include").is_err());
}

#[cfg(target_os = "linux")]
#[test]
fn relocated_llvm_config_compiles_the_debug_shim() {
    use std::os::unix::fs::symlink;
    use std::process::Command;

    let root = tempfile::Builder::new()
        .prefix("hew llvm with spaces ")
        .tempdir()
        .unwrap();
    let bin = root.path().join("bin");
    std::fs::create_dir(&bin).unwrap();
    let original_bin = std::path::Path::new(env!("HEW_LLVM_BINDIR"));
    let original = original_bin.join("llvm-config");
    let original_prefix = original_bin.parent().unwrap();
    let config = bin.join("llvm-config");
    std::fs::copy(original, &config).unwrap();
    symlink(original_prefix.join("include"), root.path().join("include")).unwrap();
    symlink(original_prefix.join("lib"), root.path().join("lib")).unwrap();
    let query = |arg| {
        let output = Command::new(&config).arg(arg).output().unwrap();
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
        String::from_utf8(output.stdout).unwrap().trim().to_owned()
    };
    let include = query("--includedir");
    assert!(include.contains("hew llvm with spaces"), "{include}");
    let flags = llvm_flags::cxx_flags(&query("--cxxflags"), &include).unwrap();
    let output = Command::new(original_bin.join("clang++"))
        .args(&flags)
        .arg("-I")
        .arg(&include)
        .arg("-std=c++17")
        .arg("-c")
        .arg(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/src/physical_debug_shim.cpp"
        ))
        .arg("-o")
        .arg(root.path().join("debug-shim.o"))
        .output()
        .expect("compile the LLVM debug shim");
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
}
