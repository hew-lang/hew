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

use support::{describe_output, hew_binary, repo_root, require_codegen, run_bounded_command};

/// Builds resolve `libhew.a` by walking up from the working directory, so
/// fixtures live under the repo root like every other linking test.
fn workspace() -> tempfile::TempDir {
    tempfile::Builder::new()
        .prefix("native-manifest-hew-")
        .tempdir_in(repo_root())
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

/// A `hew.toml` is a package manifest only when it declares `[native]`; every
/// other shape just marks a directory and must not stop a program from
/// building.
const PLAIN_MANIFESTS: &[(&str, &str)] = &[
    ("workspace only", "[workspace]\nmembers = []\n"),
    ("dependencies only", "[dependencies]\n"),
    ("empty", ""),
    ("package without a version", "[package]\nname = \"p\"\n"),
    (
        "package with a lib table",
        "[package]\nname = \"p\"\nversion = \"0.1.0\"\n[lib]\nname = \"p\"\n",
    ),
];

#[test]
fn a_hew_toml_without_native_never_blocks_a_single_file_build() {
    require_codegen();
    for (shape, text) in PLAIN_MANIFESTS {
        let dir = workspace();
        write(&dir.path().join("hew.toml"), text);
        write(
            &dir.path().join("main.hew"),
            "fn main() {\n    println(7);\n}\n",
        );
        let run = hew(dir.path(), &["run", "main.hew"]);
        assert_ok(&run, &format!("hew run beside a manifest ({shape})"));
        assert_eq!(stdout(&run), "7\n", "{shape}");

        // The same manifest owning an imported module.
        let dir = workspace();
        write(&dir.path().join("lib/hew.toml"), text);
        write(
            &dir.path().join("lib/lib.hew"),
            "pub fn value() -> i32 {\n    5\n}\n",
        );
        write(
            &dir.path().join("app.hew"),
            "import lib;\n\nfn main() {\n    println(lib.value());\n}\n",
        );
        let run = hew(dir.path(), &["run", "app.hew"]);
        assert_ok(&run, &format!("hew run importing a module ({shape})"));
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
        &manifest(name, "lib = \"rustbridge\"\ncrate = \"native\"\n"),
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
