use std::fs;
use std::process::Command;

#[test]
fn reused_binary_generates_in_the_invoked_checkout() {
    for explicit_root in [false, true] {
        let root = tempfile::tempdir().unwrap();
        let elsewhere = tempfile::tempdir().unwrap();
        let invoke = |args: &[&str]| {
            let mut command = Command::new(env!("CARGO_BIN_EXE_hew-capability-gen"));
            command.args(args).env_remove("HEW_BUILD_SOURCE_ROOT");
            command.current_dir(if explicit_root {
                elsewhere.path()
            } else {
                root.path()
            });
            if explicit_root {
                command.env("HEW_BUILD_SOURCE_ROOT", root.path());
            }
            command.output().unwrap()
        };

        // Prove the selected checkout before allowing a write: a stale binary
        // must never modify the real checkout it was compiled from.
        let missing = invoke(&["--check"]);
        assert!(!missing.status.success());
        let diagnostic = String::from_utf8_lossy(&missing.stderr);
        assert!(
            diagnostic.contains(root.path().to_string_lossy().as_ref()),
            "{diagnostic}"
        );

        for (path, contents) in [
            (
                "wasm-capability-manifest.toml",
                include_str!("../../wasm-capability-manifest.toml"),
            ),
            (
                "docs/wasm-capability-matrix.md",
                include_str!("../../docs/wasm-capability-matrix.md"),
            ),
            (
                "examples/playground/manifest.json",
                include_str!("../../examples/playground/manifest.json"),
            ),
        ] {
            let path = root.path().join(path);
            fs::create_dir_all(path.parent().unwrap()).unwrap();
            fs::write(path, contents).unwrap();
        }
        fs::create_dir_all(root.path().join("hew-types/src")).unwrap();
        let generated = invoke(&[]);
        assert!(
            generated.status.success(),
            "{}",
            String::from_utf8_lossy(&generated.stderr)
        );
        assert!(root.path().join(hew_capability_gen::RUST_OUTPUT).is_file());
        let checked = invoke(&["--check"]);
        assert!(
            checked.status.success(),
            "{}",
            String::from_utf8_lossy(&checked.stderr)
        );
    }
}
