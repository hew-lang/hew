use std::path::Path;
use std::process::Command;

const UNKNOWN_GIT_METADATA: &str = "unknown";

#[derive(Debug, Eq, PartialEq)]
struct GitMetadata {
    hash: String,
    dirty: String,
}

fn main() {
    println!("cargo:rerun-if-changed=build.rs");
    // Keep this build script focused on version metadata; the active
    // compiler path is the Rust MIR/codegen-rs pipeline.

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

    let metadata = read_git_metadata(repo_dir);
    println!("cargo:rustc-env=HEW_GIT_HASH={}", metadata.hash);
    println!("cargo:rustc-env=HEW_GIT_DIRTY={}", metadata.dirty);
}

fn read_git_metadata(repo_dir: &Path) -> GitMetadata {
    let hash = git_output_or_unknown(
        repo_dir,
        &["rev-parse", "--short", "HEAD"],
        "git commit hash",
    );

    let dirty = match git_stdout(repo_dir, &["status", "--porcelain"]) {
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

    GitMetadata { hash, dirty }
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
    use std::fs;
    use std::path::Path;
    use std::process::Command;

    use super::{read_git_metadata, GitMetadata, UNKNOWN_GIT_METADATA};

    /// Runs git against `repo` with a hermetic configuration: `config` replaces
    /// the global and system config files via the environment, so the temp repo
    /// gets a deterministic identity, no commit signing, and no hooks without
    /// passing `-c` overrides on the command line. Host git policy wrappers may
    /// refuse `-c` overrides of keys like `commit.gpgsign`; file-based config
    /// carries the same settings without per-invocation overrides.
    fn git(repo: &Path, config: &Path, args: &[&str]) {
        let output = Command::new("git")
            .current_dir(repo)
            .env("GIT_CONFIG_GLOBAL", config)
            .env("GIT_CONFIG_SYSTEM", config)
            .args(args)
            .output()
            .expect("git should run");
        assert!(
            output.status.success(),
            "git {args:?} failed:\nstdout: {}\nstderr: {}",
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    /// Writes the hermetic git config used by [`git`] into `dir` and returns
    /// its path. Kept outside the repository worktree so the config file never
    /// shows up as an untracked (dirty) entry in `git status`.
    fn write_git_test_config(dir: &Path) -> std::path::PathBuf {
        let config = dir.join("gitconfig");
        fs::write(
            &config,
            "[user]\n\
             \tname = Hew Tests\n\
             \temail = tests@hew.dev\n\
             [commit]\n\
             \tgpgsign = false\n\
             [core]\n\
             \thooksPath = .git/disabled-hooks\n",
        )
        .expect("write git test config");
        config
    }

    #[test]
    fn git_metadata_uses_unknown_sentinel() {
        assert_eq!(UNKNOWN_GIT_METADATA, "unknown");
    }

    #[test]
    fn git_metadata_reports_unavailable_repository_coherently() {
        let dir = tempfile::tempdir().expect("temporary directory");
        assert_eq!(
            read_git_metadata(dir.path()),
            GitMetadata {
                hash: UNKNOWN_GIT_METADATA.to_string(),
                dirty: UNKNOWN_GIT_METADATA.to_string(),
            }
        );
    }

    #[test]
    fn git_metadata_tracks_clean_dirty_and_detached_states() {
        let dir = tempfile::tempdir().expect("temporary directory");
        let config_dir = tempfile::tempdir().expect("config directory");
        let config = write_git_test_config(config_dir.path());
        git(dir.path(), &config, &["init", "--quiet"]);
        git(
            dir.path(),
            &config,
            &[
                "commit",
                "--quiet",
                "--allow-empty",
                "-m",
                "test: seed metadata repository",
            ],
        );

        let clean = read_git_metadata(dir.path());
        assert!(!clean.hash.is_empty());
        assert!(clean
            .hash
            .chars()
            .all(|ch| ch.is_ascii_digit() || matches!(ch, 'a'..='f')));
        assert_eq!(clean.dirty, "false");

        fs::write(dir.path().join("mutation"), "dirty\n").expect("write mutation");
        assert_eq!(read_git_metadata(dir.path()).dirty, "true");
        fs::remove_file(dir.path().join("mutation")).expect("remove mutation");

        git(dir.path(), &config, &["checkout", "--quiet", "--detach"]);
        assert_eq!(read_git_metadata(dir.path()), clean);
    }
}
