use std::fs;
use std::process::Command;

#[test]
fn reused_binary_generates_in_the_invoked_checkout() {
    for explicit_root in [false, true] {
        let root = tempfile::tempdir().unwrap();
        let elsewhere = tempfile::tempdir().unwrap();
        let invoke = |mode| {
            let mut command = Command::new(env!("CARGO_BIN_EXE_xtask"));
            command
                .args(["runtime-declarations", mode])
                .env_remove("HEW_BUILD_SOURCE_ROOT");
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

        // Check the target before writing so an old binary cannot regenerate
        // metadata in the real checkout it was compiled from.
        let missing = invoke("--check");
        assert!(!missing.status.success());
        let diagnostic = String::from_utf8_lossy(&missing.stderr);
        assert!(
            diagnostic.contains(root.path().to_string_lossy().as_ref()),
            "{diagnostic}"
        );

        fs::create_dir(root.path().join("scripts")).unwrap();
        let generated = invoke("--write");
        assert!(
            generated.status.success(),
            "{}",
            String::from_utf8_lossy(&generated.stderr)
        );
        assert!(root
            .path()
            .join("scripts/generated-runtime-declarations.toml")
            .is_file());
        let checked = invoke("--check");
        assert!(
            checked.status.success(),
            "{}",
            String::from_utf8_lossy(&checked.stderr)
        );
    }
}
