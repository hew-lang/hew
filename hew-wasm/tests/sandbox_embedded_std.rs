//! The browser package carries the whole standard library.
//!
//! Module resolution reads `.hew` sources through filesystem paths and the
//! browser has no filesystem, so the sources travel inside the package. These
//! pins keep that set complete: a standard-library module added on disk but
//! missing from the package would compile natively and fail in the browser,
//! which is the split this whole change exists to close.

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("the crate has a workspace parent")
        .to_path_buf()
}

fn shipped_sources() -> BTreeSet<String> {
    fn walk(root: &Path, dir: &Path, found: &mut BTreeSet<String>) {
        for entry in std::fs::read_dir(dir)
            .expect("read the standard library")
            .flatten()
        {
            let path = entry.path();
            if path.is_dir() {
                walk(root, &path, found);
            } else if path.extension().is_some_and(|extension| extension == "hew") {
                let relative = path
                    .strip_prefix(root)
                    .expect("a walked path is under the repository root")
                    .to_string_lossy()
                    .replace('\\', "/");
                found.insert(format!("./{relative}"));
            }
        }
    }
    let root = repo_root();
    let mut found = BTreeSet::new();
    walk(&root, &root.join("std"), &mut found);
    found
}

#[test]
fn every_shipped_standard_library_module_travels_in_the_package() {
    let embedded: BTreeSet<String> = hew_wasm::sandbox::embedded_standard_library_paths()
        .iter()
        .map(|path| (*path).to_string())
        .collect();
    let shipped = shipped_sources();

    assert!(
        !shipped.is_empty(),
        "the standard library must have sources"
    );
    let missing: Vec<_> = shipped.difference(&embedded).collect();
    assert!(
        missing.is_empty(),
        "these standard-library modules are on disk but not in the browser package: {missing:#?}"
    );
    let extra: Vec<_> = embedded.difference(&shipped).collect();
    assert!(
        extra.is_empty(),
        "these modules are in the browser package but not on disk: {extra:#?}"
    );
}

#[test]
fn an_embedded_module_is_its_source_on_disk() {
    let root = repo_root();
    for (path, source) in hew_wasm::sandbox::embedded_standard_library() {
        let on_disk = root.join(path.trim_start_matches("./"));
        let expected = std::fs::read_to_string(&on_disk)
            .unwrap_or_else(|error| panic!("read {}: {error}", on_disk.display()));
        assert_eq!(
            *source, expected,
            "the package's copy of {path} has drifted from the shipped source"
        );
    }
}

#[test]
fn a_standard_library_import_compiles_without_reading_the_filesystem() {
    // The browser reaches the same standard library the native compiler does.
    // Running here proves the sources are wired into resolution; the browser's
    // own build is proved by compiling this import through the wasm bridge.
    let output = hew_wasm::sandbox::compile_to_sandbox_bytecode(
        "import std.random;\n\nfn main() {\n    random.seed(1);\n    println(random.randint(0, 99));\n}\n",
        Some("sandbox-vm-export"),
    )
    .expect("compilation should not hard-error");
    let errors: Vec<_> = output
        .diagnostics
        .iter()
        .filter(|diagnostic| diagnostic.severity == "error")
        .collect();
    assert!(errors.is_empty(), "unexpected diagnostics: {errors:#?}");
    assert!(
        output.bytecode.is_some(),
        "an admitted import emits a package"
    );
}
