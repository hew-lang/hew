use std::fs;
use std::process::Command;

#[test]
fn reused_binary_generates_in_the_invoked_checkout() {
    for explicit_root in [false, true] {
        let root = tempfile::tempdir().unwrap();
        let elsewhere = tempfile::tempdir().unwrap();
        let invoke = |args: &[&str]| {
            let mut command = Command::new(env!("CARGO_BIN_EXE_xtask"));
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

        // Check the target before writing so an old binary cannot regenerate
        // metadata in the real checkout it was compiled from.
        let missing = invoke(&["runtime-declarations", "--check"]);
        assert!(!missing.status.success());
        let diagnostic = String::from_utf8_lossy(&missing.stderr);
        assert!(
            diagnostic.contains(root.path().to_string_lossy().as_ref()),
            "{diagnostic}"
        );

        // The native oracle must select fixtures from the same invocation
        // root, and this missing case must fail before invoking a compiler.
        let acceptance = invoke(&["core-acceptance", "--case", "invocation-root-probe"]);
        assert!(!acceptance.status.success());
        let diagnostic = String::from_utf8_lossy(&acceptance.stderr);
        assert!(
            diagnostic.contains(root.path().to_string_lossy().as_ref()),
            "{diagnostic}"
        );

        fs::create_dir(root.path().join("scripts")).unwrap();
        let generated = invoke(&["runtime-declarations", "--write"]);
        assert!(
            generated.status.success(),
            "{}",
            String::from_utf8_lossy(&generated.stderr)
        );
        assert!(root
            .path()
            .join("scripts/generated-runtime-declarations.toml")
            .is_file());
        let checked = invoke(&["runtime-declarations", "--check"]);
        assert!(
            checked.status.success(),
            "{}",
            String::from_utf8_lossy(&checked.stderr)
        );
    }
}
