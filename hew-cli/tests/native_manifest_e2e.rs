//! `[native]` C and C++ sources, system libraries and `--emit-deps`, through
//! real `hew build` and `hew run` invocations.
//!
//! A program links the `[native]` code of every package it compiles: the
//! package holding its entry file and the package owning each imported module,
//! at any depth. Each test writes a small package tree and checks what the
//! linked program prints.

mod support;

use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use support::{describe_output, hew_binary, require_codegen, run_bounded_command};

fn workspace() -> tempfile::TempDir {
    tempfile::Builder::new()
        .prefix("native-manifest-hew-")
        .tempdir()
        .expect("temp dir")
}

fn write(path: &Path, text: &str) {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent).expect("create parent directory");
    }
    std::fs::write(path, text).expect("write fixture file");
}

fn manifest(name: &str, native: &str) -> String {
    format!(
        "[package]\nname = \"{name}\"\nversion = \"0.1.0\"\nedition = \"2026\"\n\n[native]\n{native}"
    )
}

fn hew(dir: &Path, args: &[&str]) -> Output {
    let mut command = Command::new(hew_binary());
    command.args(args).current_dir(dir);
    run_bounded_command(command, format!("hew {}", args.join(" ")))
}

fn run_binary(binary: &Path) -> Output {
    run_bounded_command(Command::new(binary), format!("run {}", binary.display()))
}

fn binary(dir: &Path, name: &str) -> PathBuf {
    dir.join(format!("{name}{}", std::env::consts::EXE_SUFFIX))
}

fn stdout(output: &Output) -> String {
    String::from_utf8_lossy(&output.stdout).into_owned()
}

fn stderr(output: &Output) -> String {
    String::from_utf8_lossy(&output.stderr).into_owned()
}

fn assert_ok(output: &Output, context: &str) {
    assert!(
        output.status.success(),
        "{context} failed\n{}",
        describe_output(output)
    );
}

/// A package whose externs are a C source with a header and a define.
fn write_c_package(root: &Path) {
    write(
        &root.join("hew.toml"),
        &manifest(
            "calc",
            "sources = [\"native/calc.c\"]\ninclude-dirs = [\"native/include\"]\n\
             defines = [\"CALC_BASE=40\"]\ncflags = [\"-std=c11\", \"-Wall\"]\n",
        ),
    );
    write(
        &root.join("native/include/calc.h"),
        "#include <stdint.h>\nint32_t calc_answer(int32_t extra);\n",
    );
    write(
        &root.join("native/calc.c"),
        "#include \"calc.h\"\nint32_t calc_answer(int32_t extra) { return CALC_BASE + extra; }\n",
    );
    write(
        &root.join("main.hew"),
        "extern \"C\" {\n    fn calc_answer(extra: i32) -> i32;\n}\n\n\
         fn main() {\n    println(unsafe { calc_answer(2) });\n}\n",
    );
}

#[test]
fn trimmed_package_paths_are_stable_across_roots() {
    require_codegen();
    let dir = workspace();
    let source_root = dir.path().join("sources");
    std::fs::create_dir(&source_root).unwrap();
    let roots = [
        source_root.join("first"),
        source_root.join("a longer second root"),
    ];
    for root in &roots {
        std::fs::create_dir_all(root).unwrap();
        write_c_package(root);
        write(&root.join("native/calc.c"), "#include <stdio.h>\n#include \"calc.h\"\nint32_t calc_answer(int32_t extra) { fprintf(stderr, \"%s\\n\", __FILE__); return CALC_BASE + extra; }\n");
    }
    assert_untrimmed_debug_source(&roots);
    for flags in [&["--trim-paths"][..], &["--release"][..]] {
        let objects: Vec<_> = roots
            .iter()
            .map(|root| emit_package_paths(root, flags))
            .collect();
        assert!(objects[0].0 == objects[1].0, "trimmed object bytes");
        assert_eq!(objects[0].1, objects[1].1, "trimmed LLVM");
        assert_eq!(
            llvm_debug_directory(&objects[0].1, "main.hew"),
            Path::new("."),
            "trimmed source directory"
        );
        for root in &roots {
            assert!(!objects[0]
                .1
                .contains(&llvm_string_contents(root.to_str().unwrap())));
            let linked = binary(root, "linked");
            let mut args = vec!["build", ".", "-g", "-o", linked.to_str().unwrap()];
            args.extend_from_slice(flags);
            assert_ok(&hew(root, &args), "link native package");
            let output = run_binary(&linked);
            assert_ok(&output, "run native package");
            assert_eq!(stdout(&output), "42\n");
            let native_sources: Vec<_> = stderr(&output).lines().map(PathBuf::from).collect();
            assert_eq!(native_sources, [Path::new("native").join("calc.c")]);
        }
    }
    assert_native_objects_match(&roots);
    let native_errors: Vec<_> = roots
        .iter()
        .map(|root| {
            write(&root.join("native/calc.c"), "#error source-path-check\n");
            let output = hew(root, &["build", ".", "--release"]);
            assert!(!output.status.success());
            assert!(!stderr(&output).contains(root.to_str().unwrap()));
            stderr(&output)
        })
        .collect();
    assert_eq!(
        native_errors[0], native_errors[1],
        "native compiler diagnostics"
    );
    for root in &roots {
        write(
            &root.join("broken.hew"),
            "fn main() { let value: i64 = true; println(value); }\n",
        );
    }
    for root in &roots {
        assert_trimmed_rust_package(root);
    }
    for format in [&[][..], &["--format", "json"][..]] {
        let diagnostics: Vec<_> = roots
            .iter()
            .map(|root| {
                let source = root.join("broken.hew");
                let mut args = vec!["build", source.to_str().unwrap(), "--trim-paths"];
                args.extend_from_slice(format);
                let output = hew(root, &args);
                assert!(!output.status.success());
                (output.stdout, output.stderr)
            })
            .collect();
        assert_eq!(diagnostics[0], diagnostics[1], "trimmed diagnostics");
    }
    for root in &roots {
        let missing = root.join("missing.hew");
        let output = hew(
            root,
            &[
                "build",
                missing.to_str().unwrap(),
                "--trim-paths",
                "--format",
                "json",
            ],
        );
        assert_trimmed_missing_input(&output, root);
    }
    #[cfg(target_os = "linux")]
    assert_remapped_debug_source(dir.path(), &roots);
}

fn assert_trimmed_missing_input(output: &Output, root: &Path) {
    let details = describe_output(output);
    assert!(!output.status.success(), "{details}");
    let diagnostics: serde_json::Value = serde_json::from_slice(&output.stdout)
        .unwrap_or_else(|error| panic!("invalid JSON: {error}\n{details}"));
    assert_eq!(diagnostics, serde_json::json!([]), "{details}");
    assert!(
        !stdout(output).contains(root.to_str().unwrap()),
        "{details}"
    );
    assert!(
        !stderr(output).contains(root.to_str().unwrap()),
        "{details}"
    );
    assert!(stderr(output).contains("missing.hew"), "{details}");
}

#[cfg(unix)]
#[test]
fn trimmed_missing_input_through_a_symlink_preserves_the_read_error() {
    let dir = workspace();
    let real = dir.path().join("real");
    let alias = dir.path().join("alias");
    std::fs::create_dir(&real).unwrap();
    std::os::unix::fs::symlink(&real, &alias).unwrap();
    let missing = alias.join("missing.hew");
    let output = hew(
        &alias,
        &[
            "build",
            missing.to_str().unwrap(),
            "--trim-paths",
            "--format",
            "json",
        ],
    );
    assert_trimmed_missing_input(&output, &alias);
    assert_trimmed_missing_input(&output, &real.canonicalize().unwrap());
    assert!(
        stderr(&output).contains("cannot read"),
        "{}",
        describe_output(&output)
    );
}

#[cfg(unix)]
#[test]
fn debug_source_directory_preserves_llvm_string_escapes() {
    require_codegen();
    let dir = workspace();
    let root = dir.path().join("literal\\separator\"quote");
    write(
        &root.join("hew.toml"),
        "[package]\nname = \"debug-escape\"\nversion = \"0.1.0\"\n",
    );
    write(&root.join("main.hew"), "fn main() { println(1); }\n");
    let (_, llvm) = emit_package_paths(&root, &["--no-trim-paths"]);
    assert_eq!(
        llvm_debug_directory(&llvm, "main.hew")
            .canonicalize()
            .unwrap(),
        root.canonicalize().unwrap(),
        "{llvm}"
    );
}

fn assert_untrimmed_debug_source(roots: &[PathBuf; 2]) {
    // Exercise debug source identity independently of invocation spelling.
    #[cfg(unix)]
    let original_roots: Vec<_> = roots
        .iter()
        .enumerate()
        .map(|(index, root)| {
            let alias = root.parent().unwrap().join(format!("source alias {index}"));
            std::os::unix::fs::symlink(root, &alias).unwrap();
            alias
        })
        .collect();
    #[cfg(not(unix))]
    let original_roots = roots.to_vec();
    let original: Vec<_> = original_roots
        .iter()
        .map(|root| emit_package_paths(root, &["--release", "--no-trim-paths"]))
        .collect();
    assert_ne!(original[0].0, original[1].0);
    for (root, (_, llvm)) in original_roots.iter().zip(&original) {
        let directory = llvm_debug_directory(llvm, "main.hew");
        assert_eq!(
            directory.canonicalize().expect("untrimmed debug directory"),
            root.canonicalize().unwrap(),
            "untrimmed debug source directory: {}\n{llvm}",
            directory.display()
        );
    }
}

// LLVM can render Windows separators differently from the fixture spelling.
// Verify the actual DIFile directory identifies the original source tree.
fn llvm_debug_directory(llvm: &str, filename: &str) -> PathBuf {
    let prefix = format!(
        "!DIFile(filename: \"{}\", directory: \"",
        llvm_string_contents(filename)
    );
    let encoded = llvm
        .lines()
        .find_map(|line| {
            let (_, rest) = line.split_once(&prefix)?;
            rest.split_once('"').map(|(directory, _)| directory)
        })
        .unwrap_or_else(|| panic!("missing debug file {filename}:\n{llvm}"));
    let mut bytes = Vec::new();
    let mut remaining = encoded.as_bytes();
    while let Some((&byte, rest)) = remaining.split_first() {
        if byte == b'\\' {
            assert!(!rest.is_empty(), "truncated LLVM escape: {encoded:?}");
            if rest[0] == b'\\' {
                bytes.push(b'\\');
                remaining = &rest[1..];
            } else {
                assert!(rest.len() >= 2, "truncated LLVM escape: {encoded:?}");
                let hex = std::str::from_utf8(&rest[..2]).unwrap();
                bytes.push(
                    u8::from_str_radix(hex, 16).unwrap_or_else(|error| {
                        panic!("invalid LLVM escape in {encoded:?}: {error}")
                    }),
                );
                remaining = &rest[2..];
            }
        } else {
            bytes.push(byte);
            remaining = rest;
        }
    }
    PathBuf::from(String::from_utf8(bytes).expect("UTF-8 debug directory"))
}

fn llvm_string_contents(value: &str) -> String {
    use std::fmt::Write as _;

    let mut escaped = String::new();
    for byte in value.bytes() {
        match byte {
            b' '..=b'~' if !matches!(byte, b'"' | b'\\') => escaped.push(char::from(byte)),
            _ => write!(escaped, "\\{byte:02X}").unwrap(),
        }
    }
    escaped
}

fn emit_package_paths(root: &Path, flags: &[&str]) -> (Vec<u8>, String) {
    let mut args = vec![
        "build",
        ".",
        "-g",
        "--emit-llvm",
        "--emit-obj",
        "-o",
        "probe.o",
    ];
    args.extend_from_slice(flags);
    assert_ok(&hew(root, &args), "emit package object");
    (
        std::fs::read(root.join("probe.o")).unwrap(),
        std::fs::read_to_string(root.join("probe.ll")).unwrap(),
    )
}

fn assert_native_objects_match(roots: &[PathBuf; 2]) {
    let objects: Vec<_> = roots
        .iter()
        .map(|root| {
            let mut objects: Vec<_> = std::fs::read_dir(root.join("target/native"))
                .unwrap()
                .flat_map(|entry| {
                    let dir = entry.unwrap().path();
                    [dir.join("native/calc.c.o"), dir.join("native/calc.c.obj")]
                })
                .filter(|path| path.is_file())
                .map(|path| std::fs::read(path).unwrap())
                .collect();
            assert_eq!(objects.len(), 2, "O0 and O2 native objects");
            objects.sort();
            objects
        })
        .collect();
    assert!(
        objects[0] == objects[1],
        "native C object bytes across roots"
    );
}

#[cfg(target_os = "linux")]
fn assert_remapped_debug_source(dir: &Path, roots: &[PathBuf; 2]) {
    let moved = dir.join("moved-build");
    std::fs::rename(&roots[0], &moved).unwrap();
    write(
        &roots[1].join("main.hew"),
        "// relocated-source-marker\nfn main() {}\n",
    );
    let source_root = roots[1]
        .to_string_lossy()
        .replace('\\', "\\\\")
        .replace('"', "\\\"");
    let mut command = Command::new("gdb");
    command
        .current_dir(dir)
        .args(["--batch", "--nx", "-ex"])
        .arg(format!("set substitute-path . \"{source_root}\""))
        .args(["-ex", "list main.hew:1"])
        .arg(binary(&moved, "linked"));
    let output = run_bounded_command(command, "read remapped debugger source");
    assert_ok(&output, "debugger source remapping");
    assert!(
        stdout(&output).contains("relocated-source-marker"),
        "{}",
        describe_output(&output)
    );
}

fn assert_trimmed_rust_package(root: &Path) {
    write(
        &root.join("hew.toml"),
        "[package]\nname = \"acme.paths\"\nversion = \"0.1.0\"\nedition = \"2026\"\ninclude = [\"hew.toml\", \"main.hew\", \"native/Cargo.toml\", \"native/Cargo.lock\", \"native/src/**\", \"native/.cargo/**\", \"dependency/**\"]\n[native]\ncrate = \"native\"\n",
    );
    write(&root.join("native/Cargo.toml"), "[workspace]\n[package]\nname = \"rustbridge\"\nversion = \"0.1.0\"\nedition = \"2021\"\n[lib]\ncrate-type = [\"staticlib\"]\n[dependencies]\npath_dep = { path = \"../dependency\" }\n[profile.release]\ndebug = 1\n");
    write(&root.join("native/.cargo/config.toml"), "[build]\nrustflags = [\"--cfg\", \"hew_native_trim_flags\", \"--check-cfg=cfg(hew_native_trim_flags)\"]\n");
    write(&root.join("native/src/lib.rs"), "#![allow(unexpected_cfgs)]\n#[cfg(not(hew_native_trim_flags))]\ncompile_error!(\"native build lost Cargo flags\");\n#[no_mangle]\npub extern \"C\" fn path_probe() -> *const u8 { path_dep::source().as_ptr() }\n#[no_mangle]\npub extern \"C\" fn calc_answer(extra: i32) -> i32 { 40 + extra }\n");
    write(
        &root.join("dependency/Cargo.toml"),
        "[workspace]\n[package]\nname = \"path_dep\"\nversion = \"0.1.0\"\nedition = \"2021\"\n",
    );
    write(
        &root.join("dependency/src/lib.rs"),
        "#![allow(unexpected_cfgs)]\n#[cfg(not(hew_native_trim_flags))]\ncompile_error!(\"dependency lost Cargo flags\");\n#[cfg(hew_native_wrong_flags)]\ncompile_error!(\"Cargo flag precedence changed\");\npub fn source() -> &'static str { file!() }\n",
    );
    let identity = hew_pkg::native::embedded_rustc_identity();
    let target_flags = format!(
        "CARGO_TARGET_{}_RUSTFLAGS",
        identity.host.to_uppercase().replace('-', "_")
    );
    let build = |args: &[&str], env: &[(&str, &str)]| {
        let mut command = Command::new(hew_binary());
        command
            .current_dir(root)
            .args(args)
            .env_remove("RUSTFLAGS")
            .env_remove("CARGO_ENCODED_RUSTFLAGS")
            .env_remove("CARGO_BUILD_RUSTFLAGS")
            .env_remove(&target_flags);
        for (name, value) in env {
            command.env(name, value);
        }
        run_bounded_command(command, format!("hew {}", args.join(" ")))
    };
    let build_args = ["build", ".", "--release", "-o", "rust-linked"];
    let output = build(&build_args, &[]);
    assert_ok(&output, "build trimmed Rust package");
    assert!(
        !stderr(&output).contains(root.to_str().unwrap()),
        "trimmed Cargo output"
    );
    let target = std::env::var_os("CARGO_TARGET_DIR")
        .map_or_else(|| root.join("native/target"), PathBuf::from);
    let target = if target.is_absolute() {
        target
    } else {
        root.join("native").join(target)
    };
    let artifact = target
        .join(&identity.host)
        .join("release-lib")
        .join(if cfg!(windows) {
            "rustbridge.lib"
        } else {
            "librustbridge.a"
        });
    let assert_trimmed = || {
        let archive = std::fs::read(&artifact).unwrap();
        let prefix = root.to_str().unwrap().as_bytes();
        assert!(
            !archive.windows(prefix.len()).any(|bytes| bytes == prefix),
            "native Rust dependency retains checkout path"
        );
    };
    assert_trimmed();
    assert_ok(
        &build(
            &["native", "build", "--output", "target/paths.tar.zst"],
            &[],
        ),
        "build trimmed native archive from staged source",
    );
    assert_trimmed();

    // Target flags suppress build flags, and each environment form overrides
    // the lower-priority source. Dependencies enforce the same contract.
    let config = root.join("native/.cargo/config.toml");
    write(
        &config,
        "[build]\nrustc = \"missing-configured-rustc\"\nrustflags = [\"--cfg=hew_native_wrong_flags\"]\n[target.'cfg(all())']\nrustflags = [\"--cfg=hew_native_trim_flags\"]\n",
    );
    assert_ok(&build(&build_args, &[]), "preserve target cfg flags");
    assert_trimmed();
    write(
        &config,
        &format!(
            "[target.{}]\nrustflags = [\"--cfg=hew_native_wrong_flags\"]\n",
            identity.host
        ),
    );
    for env in [
        vec![("RUSTFLAGS", "--cfg=hew_native_trim_flags")],
        vec![
            ("RUSTFLAGS", "--cfg=hew_native_wrong_flags"),
            (
                "CARGO_ENCODED_RUSTFLAGS",
                "--cfg\u{1f}hew_native_trim_flags",
            ),
        ],
    ] {
        assert_ok(&build(&build_args, &env), "preserve environment flags");
        assert_trimmed();
    }
}

#[test]
fn c_package_builds_and_runs_at_o0_and_o2() {
    require_codegen();
    let dir = workspace();
    write_c_package(dir.path());

    for (profile, extra) in [("o0", &[][..]), ("o2", &["--release"][..])] {
        let out = binary(dir.path(), profile);
        let mut args = vec!["build", "main.hew", "-o", out.to_str().unwrap()];
        args.extend_from_slice(extra);
        assert_ok(&hew(dir.path(), &args), "hew build");
        let run = run_binary(&out);
        assert_ok(&run, "the linked program");
        assert_eq!(stdout(&run), "42\n", "{profile}");
    }

    let run = hew(dir.path(), &["run", "main.hew"]);
    assert_ok(&run, "hew run");
    assert_eq!(stdout(&run), "42\n");
}

#[test]
fn imported_packages_link_their_native_code_at_any_depth() {
    require_codegen();
    let dir = workspace();
    // app.hew (no manifest) -> outer (C) -> inner (C), a package inside
    // outer: imports resolve beside the importing file (HEW-SPEC-2026 §3.5.3).
    write(
        &dir.path().join("outer/inner/hew.toml"),
        &manifest("inner", "sources = [\"inner.c\"]\n"),
    );
    write(
        &dir.path().join("outer/inner/inner.c"),
        "#include <stdint.h>\nint32_t inner_value(void) { return 30; }\n",
    );
    write(
        &dir.path().join("outer/inner/inner.hew"),
        "extern \"C\" {\n    fn inner_value() -> i32;\n}\n\n\
         pub fn value() -> i32 {\n    unsafe { inner_value() }\n}\n",
    );
    write(
        &dir.path().join("outer/hew.toml"),
        &manifest("outer", "sources = [\"outer.c\"]\n"),
    );
    write(
        &dir.path().join("outer/outer.c"),
        "#include <stdint.h>\nint32_t outer_value(void) { return 12; }\n",
    );
    write(
        &dir.path().join("outer/outer.hew"),
        "import inner;\n\nextern \"C\" {\n    fn outer_value() -> i32;\n}\n\n\
         pub fn value() -> i32 {\n    let own = unsafe { outer_value() };\n    own + inner.value()\n}\n",
    );
    write(
        &dir.path().join("app.hew"),
        "import outer;\n\nfn main() {\n    println(outer.value());\n}\n",
    );

    let run = hew(dir.path(), &["run", "app.hew"]);
    assert_ok(&run, "hew run with two native packages");
    assert_eq!(stdout(&run), "42\n");
}

#[test]
fn a_package_reached_twice_links_its_objects_once() {
    require_codegen();
    let dir = workspace();
    let root = dir.path().join("twice");
    // The entry and the module it imports belong to one package. Linking its
    // object twice would fail on a duplicate `twice_value`.
    write(
        &root.join("hew.toml"),
        &manifest("twice", "sources = [\"twice.c\"]\n"),
    );
    write(
        &root.join("twice.c"),
        "#include <stdint.h>\nint32_t twice_value(void) { return 42; }\n",
    );
    write(
        &root.join("helper.hew"),
        "extern \"C\" {\n    fn twice_value() -> i32;\n}\n\n\
         pub fn value() -> i32 {\n    unsafe { twice_value() }\n}\n",
    );
    write(
        &root.join("main.hew"),
        "import twice.helper;\n\nfn main() {\n    println(helper.value());\n}\n",
    );

    let run = hew(&root, &["run"]);
    assert_ok(&run, "hew run of a package importing its own module");
    assert_eq!(stdout(&run), "42\n");
}

#[test]
fn cxx_sources_link_the_cxx_runtime() {
    require_codegen();
    let dir = workspace();
    write(
        &dir.path().join("hew.toml"),
        &manifest(
            "words",
            "sources = [\"native/words.cpp\"]\ncxxflags = [\"-std=c++17\"]\n",
        ),
    );
    write(
        &dir.path().join("native/words.cpp"),
        "#include <cstdint>\n#include <string>\n#include <vector>\n\
         extern \"C\" int32_t words_total(void) {\n\
         std::vector<std::string> words{\"maple\", \"spruce\", \"birch\"};\n\
         std::size_t total = 0;\nfor (const auto &word : words) total += word.size();\n\
         return static_cast<int32_t>(total);\n}\n",
    );
    write(
        &dir.path().join("main.hew"),
        "extern \"C\" {\n    fn words_total() -> i32;\n}\n\n\
         fn main() {\n    println(unsafe { words_total() });\n}\n",
    );

    let run = hew(dir.path(), &["run", "main.hew"]);
    assert_ok(&run, "hew run with a C++ source");
    assert_eq!(stdout(&run), "16\n");
}

/// `PKG_CONFIG` selects the program; a script stands in for pkg-config so the
/// test needs no particular library installed.
#[cfg(unix)]
#[test]
fn pkg_config_flags_reach_the_compile_and_the_link() {
    use std::os::unix::fs::PermissionsExt as _;

    require_codegen();
    let dir = workspace();
    // The libraries repeat a two-token flag: each token must reach the link
    // in place, or the second half of the pair is read as an input file.
    let pair = if cfg!(target_os = "macos") {
        "-framework CoreFoundation -lm -framework Security"
    } else {
        "-Wl,-z -Wl,now -lm -Wl,-z -Wl,relro"
    };
    let script = dir.path().join("fake-pkg-config");
    write(
        &script,
        &format!(
            "#!/bin/sh\ncase \"$1\" in\n  --cflags) echo '-DFROM_PKG_CONFIG=41' ;;\n  \
             --libs) echo '{pair}' ;;\nesac\n"
        ),
    );
    std::fs::set_permissions(&script, std::fs::Permissions::from_mode(0o755))
        .expect("make the script executable");
    write(
        &dir.path().join("hew.toml"),
        &manifest(
            "probe",
            "sources = [\"probe.c\"]\npkg-config = [\"probe-lib\"]\n",
        ),
    );
    write(
        &dir.path().join("probe.c"),
        "#include <math.h>\n#include <stdint.h>\n\
         int32_t probe_value(double x) { return FROM_PKG_CONFIG + (int32_t)cbrt(x); }\n",
    );
    write(
        &dir.path().join("main.hew"),
        "extern \"C\" {\n    fn probe_value(x: f64) -> i32;\n}\n\n\
         fn main() {\n    println(unsafe { probe_value(1.0) });\n}\n",
    );

    let mut command = Command::new(hew_binary());
    command
        .args(["run", "main.hew"])
        .current_dir(dir.path())
        .env("PKG_CONFIG", &script);
    let run = run_bounded_command(command, "hew run with pkg-config");
    assert_ok(&run, "hew run with pkg-config");
    assert_eq!(stdout(&run), "42\n");

    let mut command = Command::new(hew_binary());
    command
        .args(["run", "main.hew"])
        .current_dir(dir.path())
        .env("PKG_CONFIG", dir.path().join("no-such-pkg-config"));
    let missing = run_bounded_command(command, "hew run without pkg-config");
    assert!(!missing.status.success(), "{}", describe_output(&missing));
    let text = stderr(&missing);
    assert!(text.contains("E_NATIVE_PKG_CONFIG"), "{text}");
    assert!(text.contains("link-libs"), "{text}");
}

#[test]
fn a_native_source_defining_a_runtime_symbol_is_refused_with_a_rename() {
    require_codegen();
    let dir = workspace();
    write(
        &dir.path().join("hew.toml"),
        &manifest("meshcore.roles", "sources = [\"bridge.c\"]\n"),
    );
    write(
        &dir.path().join("bridge.c"),
        "#include <stdint.h>\nint64_t hew_listener_now(void) { return 1; }\n\
         static int64_t hew_private_helper(void) { return 2; }\n\
         int64_t roles_listener_wall(void) { return hew_private_helper(); }\n",
    );
    write(
        &dir.path().join("main.hew"),
        "extern \"C\" {\n    fn hew_listener_now() -> i64;\n}\n\n\
         fn main() {\n    println(unsafe { hew_listener_now() });\n}\n",
    );

    let out = binary(dir.path(), "refused");
    let build = hew(
        dir.path(),
        &["build", "main.hew", "-o", out.to_str().unwrap()],
    );
    assert!(!build.status.success(), "{}", describe_output(&build));
    let text = stderr(&build);
    assert!(text.contains("E_RESERVED_NATIVE_SYMBOL"), "{text}");
    assert!(text.contains("`hew_listener_now`"), "{text}");
    assert!(text.contains("`meshcore_roles_listener_now`"), "{text}");
    // A file-local helper is not a symbol the runtime can collide with.
    assert!(!text.contains("hew_private_helper"), "{text}");
    assert!(!out.exists(), "no binary may be produced");
}

#[test]
fn a_malformed_native_section_names_the_field() {
    require_codegen();
    let dir = workspace();
    write_c_package(dir.path());
    write(
        &dir.path().join("hew.toml"),
        &manifest("calc", "sources = [\"../outside.c\"]\n"),
    );
    let build = hew(dir.path(), &["build", "main.hew", "-o", "never"]);
    assert!(!build.status.success(), "{}", describe_output(&build));
    let text = stderr(&build);
    assert!(text.contains("E_INVALID_NATIVE"), "{text}");
    assert!(
        text.contains("[native] sources entry \"../outside.c\""),
        "{text}"
    );
}

#[test]
fn a_wasm_build_of_a_native_package_is_refused() {
    require_codegen();
    let dir = workspace();
    write_c_package(dir.path());
    let build = hew(
        dir.path(),
        &[
            "build",
            "main.hew",
            "--target",
            "wasm32-wasip1",
            "-o",
            "out.wasm",
        ],
    );
    assert!(!build.status.success(), "{}", describe_output(&build));
    let text = stderr(&build);
    assert!(
        text.contains("package `calc` declares [native] code"),
        "{text}"
    );
}

#[test]
fn emit_deps_names_modules_manifests_sources_and_headers() {
    require_codegen();
    let dir = workspace();
    write_c_package(dir.path());
    write(
        &dir.path().join("util.hew"),
        "pub fn two() -> i32 {\n    2\n}\n",
    );
    write(
        &dir.path().join("main.hew"),
        "import calc.util;\n\nextern \"C\" {\n    fn calc_answer(extra: i32) -> i32;\n}\n\n\
         fn main() {\n    println(unsafe { calc_answer(util.two()) });\n}\n",
    );

    let build = hew(
        dir.path(),
        &[
            "build",
            "main.hew",
            "-o",
            "out/app",
            "--emit-deps",
            "out/app.d",
        ],
    );
    assert_ok(&build, "hew build --emit-deps");
    let deps = std::fs::read_to_string(dir.path().join("out/app.d")).expect("dependency file");
    let (rule, phony) = deps.split_once("\n\n").expect("rule, then empty rules");
    let rule = rule.replace("\\\n", " ");
    let mut words = rule.split_whitespace();
    assert_eq!(words.next(), Some("out/app:"), "{deps}");
    let prerequisites: Vec<&str> = words.collect();
    // Hew sources and manifests come first; the C driver's own dependency
    // list follows, which on macOS also names the SDK's settings file.
    assert_eq!(
        prerequisites[..3],
        ["main.hew", "util.hew", "hew.toml"],
        "{deps}"
    );
    for native in ["native/calc.c", "native/include/calc.h"] {
        assert!(prerequisites.contains(&native), "{deps}");
    }
    for prerequisite in &prerequisites {
        assert!(phony.contains(&format!("{prerequisite}:\n")), "{deps}");
    }

    // `hew run` has no output file; its dependency file is its own target.
    let run = hew(dir.path(), &["run", "main.hew", "--emit-deps", "run.d"]);
    assert_ok(&run, "hew run --emit-deps");
    assert_eq!(stdout(&run), "42\n");
    let deps = std::fs::read_to_string(dir.path().join("run.d")).expect("run dependency file");
    assert!(deps.starts_with("run.d: \\\n  main.hew"), "{deps}");
}

#[test]
fn objects_rebuild_when_a_header_changes() {
    require_codegen();
    let dir = workspace();
    write_c_package(dir.path());
    let run = hew(dir.path(), &["run", "main.hew"]);
    assert_ok(&run, "first hew run");
    assert_eq!(stdout(&run), "42\n");

    // The header now defines the base; the define in hew.toml stays, so only
    // a rebuild that saw the header change can print the new value. The
    // header's time is set past the object's so the check does not depend on
    // the file system's timestamp resolution.
    let header = dir.path().join("native/include/calc.h");
    write(
        &header,
        "#include <stdint.h>\n#undef CALC_BASE\n#define CALC_BASE 100\n\
         int32_t calc_answer(int32_t extra);\n",
    );
    let later = std::time::SystemTime::now() + std::time::Duration::from_mins(1);
    filetime::set_file_mtime(&header, filetime::FileTime::from_system_time(later))
        .expect("set header time");
    let run = hew(dir.path(), &["run", "main.hew"]);
    assert_ok(&run, "hew run after a header change");
    assert_eq!(stdout(&run), "102\n");
}

/// Valid manifests without `[native]` do not add a native build prerequisite.
const PLAIN_MANIFESTS: &[(&str, &str, &str)] = &[
    (
        "workspace only",
        "[workspace]\nmembers = []\n",
        "import lib.lib as lib;",
    ),
    (
        "dependencies only",
        "[dependencies]\n",
        "import lib.lib as lib;",
    ),
    ("empty", "", "import lib.lib as lib;"),
    (
        "package",
        "[package]\nname = \"lib\"\nversion = \"0.1.0\"\n",
        "import lib;",
    ),
];

#[test]
fn valid_manifests_without_native_allow_a_single_file_build() {
    require_codegen();
    for (shape, text, module_import) in PLAIN_MANIFESTS {
        let dir = workspace();
        write(&dir.path().join("hew.toml"), text);
        write(
            &dir.path().join("main.hew"),
            "fn main() {\n    println(7);\n}\n",
        );
        let run = hew(dir.path(), &["run", "main.hew"]);
        assert_ok(&run, &format!("hew run beside a manifest ({shape})"));
        assert_eq!(stdout(&run), "7\n", "{shape}");

        // The same manifest beside an imported file.
        let dir = workspace();
        write(&dir.path().join("lib/hew.toml"), text);
        write(
            &dir.path().join("lib/lib.hew"),
            "pub fn value() -> i32 {\n    5\n}\n",
        );
        write(
            &dir.path().join("app.hew"),
            &format!("{module_import}\n\nfn main() {{\n    println(lib.value());\n}}\n"),
        );
        let run = hew(dir.path(), &["run", "app.hew"]);
        assert_ok(&run, &format!("hew run importing a file ({shape})"));
        assert_eq!(stdout(&run), "5\n", "{shape}");
    }
}

fn copy_tree(from: &Path, to: &Path) {
    std::fs::create_dir_all(to).expect("create directory");
    for entry in std::fs::read_dir(from).expect("read directory") {
        let entry = entry.expect("directory entry");
        let target = to.join(entry.file_name());
        if entry.file_type().expect("file type").is_dir() {
            copy_tree(&entry.path(), &target);
        } else {
            std::fs::copy(entry.path(), &target).expect("copy file");
        }
    }
}

#[test]
fn a_copied_package_tree_rebuilds_its_objects() {
    require_codegen();
    let original = workspace();
    write_c_package(original.path());
    let run = hew(original.path(), &["run", "main.hew"]);
    assert_ok(&run, "hew run in the original");
    assert_eq!(stdout(&run), "42\n");

    // Copy the tree with its `target/`, then change a source in the copy. The
    // cached dependency file still names the original's sources, which have
    // not changed.
    let copy = workspace();
    copy_tree(original.path(), copy.path());
    write(
        &copy.path().join("native/calc.c"),
        "#include \"calc.h\"\nint32_t calc_answer(int32_t extra) { return CALC_BASE + extra + 1000; }\n",
    );
    let run = hew(copy.path(), &["run", "main.hew"]);
    assert_ok(&run, "hew run in the copy");
    assert_eq!(stdout(&run), "1042\n");
}

#[test]
fn thread_local_and_common_symbols_in_the_runtime_prefix_are_refused() {
    require_codegen();
    for (kind, source, cflags, symbol) in [
        (
            "thread-local",
            "__thread int hew_tls_slot = 3;\nint tls_read(void) { return hew_tls_slot; }\n",
            "",
            "hew_tls_slot",
        ),
        (
            "common",
            "int hew_common_slot;\nint common_read(void) { return hew_common_slot; }\n",
            "cflags = [\"-fcommon\"]\n",
            "hew_common_slot",
        ),
    ] {
        let dir = workspace();
        write(
            &dir.path().join("hew.toml"),
            &manifest(
                "meshcore.roles",
                &format!("sources = [\"bridge.c\"]\n{cflags}"),
            ),
        );
        write(&dir.path().join("bridge.c"), source);
        write(
            &dir.path().join("main.hew"),
            "fn main() {\n    println(1);\n}\n",
        );
        let build = hew(dir.path(), &["build", "main.hew", "-o", "refused"]);
        assert!(
            !build.status.success(),
            "{kind}\n{}",
            describe_output(&build)
        );
        let text = stderr(&build);
        assert!(text.contains("E_RESERVED_NATIVE_SYMBOL"), "{kind}: {text}");
        assert!(text.contains(&format!("`{symbol}`")), "{kind}: {text}");
    }
}

#[test]
fn every_malformed_native_manifest_carries_the_native_code() {
    require_codegen();
    for (native, expected) in [
        ("sources = \"calc.c\"\n", "sources"),
        ("sourcse = [\"calc.c\"]\n", "sourcse"),
        (
            "sources = [\"calc.c\"]\n[native.win32]\nlink-libs = [\"x\"]\n",
            "win32",
        ),
        (
            "sources = [\"calc.c\"]\n[native.linux]\nlib = \"x\"\n",
            "lib",
        ),
        ("sources = [\"calc.c\", \"./calc.c\"]\n", "more than once"),
    ] {
        let dir = workspace();
        write(&dir.path().join("hew.toml"), &manifest("calc", native));
        write(
            &dir.path().join("calc.c"),
            "int calc_value(void) { return 1; }\n",
        );
        write(
            &dir.path().join("main.hew"),
            "fn main() {\n    println(1);\n}\n",
        );
        let build = hew(dir.path(), &["build", "main.hew", "-o", "never"]);
        assert!(
            !build.status.success(),
            "{native}\n{}",
            describe_output(&build)
        );
        let text = stderr(&build);
        assert!(text.contains("E_INVALID_NATIVE"), "{native}: {text}");
        assert!(text.contains(expected), "{native}: {text}");
    }

    let dir = workspace();
    write(
        &dir.path().join("hew.toml"),
        "[package]\nname = \"calc\"\nversion = \"0.1.0\"\nedition = \"2026\"\n\n[native]\nsources = 3\n",
    );
    write(
        &dir.path().join("main.hew"),
        "fn main() {\n    println(1);\n}\n",
    );
    let build = hew(dir.path(), &["build", "main.hew", "-o", "never"]);
    assert!(
        stderr(&build).contains("E_INVALID_NATIVE"),
        "{}",
        stderr(&build)
    );
}

#[cfg(unix)]
#[test]
fn a_read_only_package_directory_still_builds_its_native_code() {
    use std::os::unix::fs::PermissionsExt as _;

    require_codegen();
    let dir = workspace();
    write_c_package(dir.path());
    let locked = std::fs::Permissions::from_mode(0o555);
    let open = std::fs::Permissions::from_mode(0o755);
    std::fs::set_permissions(dir.path(), locked).expect("lock the package");
    if std::fs::create_dir(dir.path().join("probe")).is_ok() {
        // Running as a user the mode does not bind (root): nothing to test.
        std::fs::set_permissions(dir.path(), open).expect("unlock the package");
        return;
    }
    let run = hew(dir.path(), &["run", "main.hew"]);
    std::fs::set_permissions(dir.path(), open).expect("unlock the package");
    assert_ok(&run, "hew run in a read-only package");
    assert_eq!(stdout(&run), "42\n");
    assert!(!dir.path().join("target").exists());
}

/// A staticlib crate exporting `hew_rust_value`.
fn write_rust_package(root: &Path, name: &str) {
    write(
        &root.join("hew.toml"),
        &manifest(name, "crate = \"native\"\n"),
    );
    write(
        &root.join("native/Cargo.toml"),
        "[workspace]\n\n[package]\nname = \"rustbridge\"\nversion = \"0.1.0\"\nedition = \"2021\"\n\n\
         [lib]\ncrate-type = [\"staticlib\"]\n",
    );
    write(
        &root.join("native/src/lib.rs"),
        "#[no_mangle]\npub extern \"C\" fn hew_rust_value() -> i32 {\n    11\n}\n",
    );
    write(
        &root.join("main.hew"),
        "extern \"C\" {\n    fn hew_rust_value() -> i32;\n}\n\n\
         fn main() {\n    println(unsafe { hew_rust_value() });\n}\n",
    );
}

#[test]
fn a_rust_crate_exporting_a_runtime_symbol_is_refused_outside_the_hew_namespace() {
    require_codegen();
    let dir = workspace();
    write_rust_package(dir.path(), "acme.rustbridge");
    let build = hew(dir.path(), &["build", "main.hew", "-o", "refused"]);
    assert!(!build.status.success(), "{}", describe_output(&build));
    let text = stderr(&build);
    assert!(text.contains("E_RESERVED_NATIVE_SYMBOL"), "{text}");
    assert!(text.contains("Rust crate `rustbridge`"), "{text}");
    assert!(text.contains("`hew_rust_value`"), "{text}");
    assert!(text.contains("`acme_rustbridge_rust_value`"), "{text}");
}

#[test]
fn a_hew_namespace_package_may_export_runtime_prefixed_symbols() {
    require_codegen();
    let dir = workspace();
    write_rust_package(dir.path(), "hew.rustbridge");
    let run = hew(dir.path(), &["run", "main.hew"]);
    assert_ok(&run, "hew run of a hew.* package");
    assert_eq!(stdout(&run), "11\n");
}

#[test]
fn a_path_dependency_installs_imports_and_links_its_native_code() {
    require_codegen();
    let dir = workspace();
    write(
        &dir.path().join("lib/hew.toml"),
        &manifest("acme.local", "sources = [\"native/l.c\"]\n"),
    );
    write(
        &dir.path().join("lib/native/l.c"),
        "int acme_local_v(void) { return 9; }\n",
    );
    write(
        &dir.path().join("lib/local.hew"),
        "extern \"C\" {\n    fn acme_local_v() -> i32;\n}\n\n\
         pub fn v() -> i32 {\n    unsafe { acme_local_v() }\n}\n",
    );
    write(
        &dir.path().join("app/hew.toml"),
        "[package]\nname = \"app\"\nversion = \"0.1.0\"\nedition = \"2026\"\n\n\
         [dependencies]\n\"acme.local\" = { path = \"../lib\" }\n",
    );
    write(
        &dir.path().join("app/main.hew"),
        "import acme.local;\n\nfn main() {\n    println(local.v());\n}\n",
    );
    let app = dir.path().join("app");
    assert_ok(&hew(&app, &["install"]), "hew install");
    let run = hew(&app, &["run", "main.hew"]);
    assert_ok(&run, "hew run with a path dependency");
    assert_eq!(stdout(&run), "9\n");
}

#[cfg(all(target_arch = "x86_64", target_os = "linux"))]
fn write_assembly_package(lib: &Path) {
    write(&lib.join("hew.toml"), &manifest("acme.assembly",
        "sources = [\"raw.s\", \"preprocessed.S\", \"bridge.c\"]\ninclude-dirs = [\"include\"]\ndefines = [\"EXTRA=2\"]\ncflags = [\"-std=c11\", \"-Werror\"]\nasflags = [\"-Wa,--noexecstack\"]\n"));
    write(&lib.join("assembly.hew"),
        "extern \"C\" { fn raw_value() -> i32; fn cpp_value() -> i32; fn c_value() -> i32; }\npub fn value() -> i32 { unsafe { raw_value() + cpp_value() + c_value() } }\n");
    write(
        &lib.join("bridge.c"),
        "int c_value(void) { return EXTRA; }\n",
    );
    write(&lib.join("raw.s"),
        ".include \"raw.inc\"\n.text\n.globl raw_value\n.type raw_value,@function\nraw_value: mov $RAW_VALUE,%eax\nret\n.section .note.GNU-stack,\"\",@progbits\n");
    write(&lib.join("include/raw.inc"), ".equ RAW_VALUE,19\n");
    write(&lib.join("preprocessed.S"),
        "#include \"value.h\"\n.text\n.globl cpp_value\n.type cpp_value,@function\ncpp_value: mov $(CPP_VALUE+EXTRA),%eax\nret\n.section .note.GNU-stack,\"\",@progbits\n");
    write(&lib.join("include/value.h"), "#define CPP_VALUE 19\n");
}

/// GNU assembler fixtures target the host used by this test, not every CPU.
#[cfg(all(target_arch = "x86_64", target_os = "linux"))]
#[test]
fn assembly_path_package_runs_and_rebuilds_included_inputs() {
    require_codegen();
    let dir = workspace();
    let lib = dir.path().join("lib");
    let app = dir.path().join("app");
    write_assembly_package(&lib);
    write(&app.join("hew.toml"),
        "[package]\nname=\"app\"\nversion=\"0.1.0\"\nedition=\"2026\"\n[dependencies]\n\"acme.assembly\"={path=\"../lib\"}\n");
    write(
        &app.join("main.hew"),
        "import acme.assembly;\nfn main() { println(assembly.value()); }\n",
    );
    let mut install = Command::new(hew_binary());
    install
        .args(["install", "--offline"])
        .current_dir(&app)
        .env("HEW_HOME", dir.path().join("cache"));
    assert_ok(
        &run_bounded_command(install, "install assembly path dependency"),
        "hew install",
    );
    for (profile, extra) in [("o0", &[][..]), ("o2", &["--release"][..])] {
        let out = binary(&app, profile);
        let mut args = vec![
            "build",
            "main.hew",
            "-o",
            out.to_str().unwrap(),
            "--emit-deps",
            "app.d",
        ];
        args.extend_from_slice(extra);
        assert_ok(&hew(&app, &args), "assembly package build");
        let run = run_binary(&out);
        assert_ok(&run, "assembly package executable");
        assert_eq!(stdout(&run), "42\n", "{profile}");
    }
    let deps = std::fs::read_to_string(app.join("app.d")).unwrap();
    for input in ["raw.s", "preprocessed.S", "include/value.h"] {
        assert!(deps.contains(input), "missing {input}: {deps}");
    }
    // Both assembler `.include` and preprocessor `#include` changes must be
    // observed even though integrated assemblers omit raw include depfiles.
    write(&lib.join("include/raw.inc"), ".equ RAW_VALUE,20\n");
    write(&lib.join("include/value.h"), "#define CPP_VALUE 20\n");
    let run = hew(&app, &["run", "main.hew"]);
    assert_ok(&run, "assembly rebuild after included inputs change");
    assert_eq!(stdout(&run), "44\n");

    // Upstream projects sometimes ship CPP assembly under lowercase `.s`.
    // The package can select preprocessing explicitly without renaming it.
    std::fs::rename(lib.join("preprocessed.S"), lib.join("preprocessed.s")).unwrap();
    let manifest_path = lib.join("hew.toml");
    let text = std::fs::read_to_string(&manifest_path)
        .unwrap()
        .replace("preprocessed.S", "preprocessed.s")
        .replace(
            "asflags = [\"-Wa,--noexecstack\"]",
            "asflags = [\"-x\", \"assembler-with-cpp\"]",
        );
    write(&manifest_path, &text);
    let run = hew(&app, &["run", "main.hew"]);
    assert_ok(&run, "explicit preprocessing of lowercase assembly");
    assert_eq!(stdout(&run), "44\n");
    assert_ok(
        &hew(
            &app,
            &[
                "build",
                "main.hew",
                "--emit-deps",
                "forced.d",
                "-o",
                "forced",
            ],
        ),
        "forced preprocessing deps",
    );
    assert!(std::fs::read_to_string(app.join("forced.d"))
        .unwrap()
        .contains("include/value.h"));

    // Joined -x overrides also permit an uppercase file to be raw assembly.
    std::fs::rename(lib.join("raw.s"), lib.join("raw.S")).unwrap();
    write(&lib.join("preprocessed.s"),
        ".text\n.globl cpp_value\n.type cpp_value,@function\ncpp_value: mov $22,%eax\nret\n.section .note.GNU-stack,\"\",@progbits\n");
    let text = std::fs::read_to_string(&manifest_path)
        .unwrap()
        .replace("raw.s", "raw.S")
        .replace(
            "asflags = [\"-x\", \"assembler-with-cpp\"]",
            "asflags = [\"-xassembler\"]",
        );
    write(&manifest_path, &text);
    let run = hew(&app, &["run", "main.hew"]);
    assert_ok(&run, "explicit raw mode for uppercase assembly");
    assert_eq!(stdout(&run), "44\n");
}

/// A package whose Rust crate's Cargo package, `[lib]` name and Hew package
/// name all differ, with `crate_types` as its `[lib] crate-type`.
fn write_named_rust_crate(root: &Path, crate_types: &str) {
    write(
        &root.join("hew.toml"),
        &manifest("acme.bridge", "crate = \"native\"\n"),
    );
    write(
        &root.join("native/Cargo.toml"),
        &format!(
            "[workspace]\n\n[package]\nname = \"bridge-crate\"\nversion = \"0.1.0\"\n\
             edition = \"2021\"\n\n[lib]\nname = \"foo_native\"\ncrate-type = [{crate_types}]\n\n\
             [profile.release]\npanic = \"abort\"\n"
        ),
    );
    write(
        &root.join("native/src/lib.rs"),
        "#[no_mangle]\npub extern \"C\" fn acme_bridge_value() -> i32 {\n    23\n}\n",
    );
    write(
        &root.join("main.hew"),
        "extern \"C\" {\n    fn acme_bridge_value() -> i32;\n}\n\n\
         fn main() {\n    println(unsafe { acme_bridge_value() });\n}\n",
    );
}

#[test]
fn the_library_identity_comes_from_cargo() {
    require_codegen();
    let dir = workspace();
    write_named_rust_crate(dir.path(), "\"staticlib\"");
    let check = hew(dir.path(), &["check"]);
    assert_ok(&check, "hew check of a staticlib crate");
    let run = hew(dir.path(), &["run", "main.hew"]);
    assert_ok(&run, "hew run linking foo_native");
    assert_eq!(stdout(&run), "23\n");
}

#[test]
fn a_crate_without_a_linkable_library_is_refused_before_building() {
    require_codegen();
    let dir = workspace();
    write_named_rust_crate(dir.path(), "\"rlib\"");
    let check = hew(dir.path(), &["check"]);
    assert!(!check.status.success(), "{}", describe_output(&check));
    let text = stderr(&check);
    assert!(text.contains("E_INVALID_NATIVE"), "{text}");
    assert!(text.contains("builds no linkable library"), "{text}");
    assert!(
        !dir.path().join("native/target").exists(),
        "the check must not build the crate"
    );
}

#[test]
fn a_manifest_naming_lib_or_kind_is_refused_with_the_line() {
    require_codegen();
    let dir = workspace();
    write_named_rust_crate(dir.path(), "\"staticlib\"");
    write(
        &dir.path().join("hew.toml"),
        &manifest("acme.bridge", "crate = \"native\"\nlib = \"foo_native\"\n"),
    );
    let build = hew(dir.path(), &["build", "main.hew", "-o", "never"]);
    assert!(!build.status.success(), "{}", describe_output(&build));
    let text = stderr(&build);
    assert!(
        text.contains("E_INVALID_NATIVE: line 8: [native] `lib` is no longer read"),
        "{text}"
    );
    assert!(text.contains("remove them"), "{text}");
}
