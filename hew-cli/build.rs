use std::path::Path;
use std::process::Command;

const UNKNOWN_GIT_METADATA: &str = "unknown";

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    // `hew_embedded_codegen` was set when the C++/MLIR codegen subtree was
    // present and built via cmake from this build script. The subtree has
    // been retired; the cfg is permanently unset, and the gated code paths
    // in `compile.rs`, `jit/mod.rs`, `target.rs`, and `main.rs` fall back
    // to the existing "embedded MLIR/LLVM codegen is unavailable" stubs.
    // The check-cfg declaration is kept so future re-introductions or
    // accidental `cfg(hew_embedded_codegen)` usage do not trip an unknown
    // cfg warning under `-D warnings`.
    println!("cargo:rustc-check-cfg=cfg(hew_embedded_codegen)");

    let repo_dir = Path::new("..");
    emit_git_metadata(repo_dir);
}

fn emit_git_metadata(repo_dir: &Path) {
    match git_stdout(repo_dir, &["rev-parse", "--git-dir"]) {
        Ok(git_dir) => {
            let git_dir = repo_dir.join(git_dir);
            for path in [git_dir.join("HEAD"), git_dir.join("index")] {
                println!("cargo:rerun-if-changed={}", path.display());
            }
        }
        Err(error) => {
            cargo_warning(&format!(
                "failed to locate git metadata for hew-cli build: {error}"
            ));
        }
    }

    let git_hash = git_output_or_unknown(
        repo_dir,
        &["rev-parse", "--short", "HEAD"],
        "git commit hash",
    );
    println!("cargo:rustc-env=HEW_GIT_HASH={git_hash}");

    let git_dirty = match git_stdout(repo_dir, &["status", "--porcelain"]) {
        Ok(status) => {
            if status.is_empty() {
                "false".to_string()
            } else {
                "true".to_string()
            }
        }
        Err(error) => {
            cargo_warning(&format!(
                "failed to determine git dirty state for hew-cli build: {error}"
            ));
            UNKNOWN_GIT_METADATA.to_string()
        }
    };
    println!("cargo:rustc-env=HEW_GIT_DIRTY={git_dirty}");
}

fn git_output_or_unknown(repo_dir: &Path, args: &[&str], description: &str) -> String {
    match git_stdout(repo_dir, args) {
        Ok(output) if !output.is_empty() => output,
        Ok(_) => {
            cargo_warning(&format!(
                "{description} was empty during hew-cli build; using {UNKNOWN_GIT_METADATA}"
            ));
            UNKNOWN_GIT_METADATA.to_string()
        }
        Err(error) => {
            cargo_warning(&format!(
                "failed to read {description} during hew-cli build: {error}; using {UNKNOWN_GIT_METADATA}"
            ));
            UNKNOWN_GIT_METADATA.to_string()
        }
    }
}

fn git_stdout(repo_dir: &Path, args: &[&str]) -> Result<String, String> {
    let output = Command::new("git")
        .current_dir(repo_dir)
        .args(args)
        .output()
        .map_err(|error| error.to_string())?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr).trim().to_string();
        let stdout = String::from_utf8_lossy(&output.stdout).trim().to_string();
        let detail = if !stderr.is_empty() {
            stderr
        } else if !stdout.is_empty() {
            stdout
        } else {
            format!("git {:?} exited with {}", args, output.status)
        };
        return Err(detail);
    }
    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

fn cargo_warning(message: &str) {
    println!("cargo::warning={}", message.replace('\n', " "));
}

#[cfg(test)]
mod tests {
    use super::UNKNOWN_GIT_METADATA;

    #[test]
    fn git_metadata_uses_unknown_sentinel() {
        assert_eq!(UNKNOWN_GIT_METADATA, "unknown");
    }
}
