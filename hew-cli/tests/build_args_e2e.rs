use std::path::{Path, PathBuf};
use std::process::Command;

fn repo_root() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-cli crate should live under the repo root")
}

fn hew_binary() -> PathBuf {
    PathBuf::from(env!("CARGO_BIN_EXE_hew"))
}

#[test]
fn removed_werror_flag_is_rejected_by_build_style_commands() {
    for command in ["build", "check", "run", "debug"] {
        let output = Command::new(hew_binary())
            .args([command, "--Werror", "placeholder.hew"])
            .current_dir(repo_root())
            .output()
            .unwrap();

        assert!(
            !output.status.success(),
            "{command} unexpectedly accepted removed --Werror flag",
        );

        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("Unknown option: --Werror"),
            "{command} stderr did not explain the rejected flag: {stderr}",
        );
    }
}
