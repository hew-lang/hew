use std::path::{Path, PathBuf};
use std::process::Command;

fn main() {
    println!("cargo:rerun-if-changed=build.rs");

    let repo_dir = Path::new("..");
    let version = env!("CARGO_PKG_VERSION");
    println!(
        "cargo:rustc-env=HEW_VERSION={}",
        git_version(repo_dir, version)
    );
    emit_git_watch_paths(repo_dir);
}

pub(crate) fn git_version(repo_dir: &Path, cargo_version: &str) -> String {
    if git_stdout(repo_dir, &["rev-parse", "--git-dir"]).is_err() {
        return cargo_version.to_string();
    }

    let Ok(describe) = git_stdout(repo_dir, &["describe", "--tags", "--long", "--dirty"]) else {
        return shallow_clone_version(repo_dir, cargo_version);
    };

    let dirty = describe.ends_with("-dirty");
    let describe = describe.strip_suffix("-dirty").unwrap_or(&describe);
    let Some((tag_and_count, sha)) = describe.rsplit_once("-g") else {
        return cargo_version.to_string();
    };
    let Some((tag, count)) = tag_and_count.rsplit_once('-') else {
        return cargo_version.to_string();
    };
    let Ok(commits_since_tag) = count.parse::<u64>() else {
        return cargo_version.to_string();
    };

    if commits_since_tag == 0 && normalize_tag(tag) == cargo_version {
        return if dirty {
            format!("{cargo_version}+dirty")
        } else {
            cargo_version.to_string()
        };
    }

    dev_version(cargo_version, commits_since_tag, sha, dirty)
}

fn shallow_clone_version(repo_dir: &Path, cargo_version: &str) -> String {
    let Ok(commits_since_root) = git_stdout(repo_dir, &["rev-list", "--count", "HEAD"])
        .and_then(|count| count.parse::<u64>().map_err(|error| error.to_string()))
    else {
        return cargo_version.to_string();
    };
    let Ok(sha) = git_stdout(repo_dir, &["rev-parse", "--short=7", "HEAD"]) else {
        return cargo_version.to_string();
    };
    let dirty =
        git_stdout(repo_dir, &["status", "--porcelain"]).is_ok_and(|status| !status.is_empty());

    dev_version(cargo_version, commits_since_root, &sha, dirty)
}

fn dev_version(cargo_version: &str, commits_since_tag: u64, sha: &str, dirty: bool) -> String {
    let mut version = format!("{cargo_version}-dev.{commits_since_tag}+{sha}");
    if dirty {
        version.push_str(".dirty");
    }
    version
}

fn normalize_tag(tag: &str) -> &str {
    tag.strip_prefix('v').unwrap_or(tag)
}

fn emit_git_watch_paths(repo_dir: &Path) {
    for path in git_watch_paths(repo_dir) {
        println!("cargo:rerun-if-changed={}", path.display());
    }
}

fn git_watch_paths(repo_dir: &Path) -> Vec<PathBuf> {
    let git_path = |name| {
        git_stdout(repo_dir, &["rev-parse", "--git-path", name])
            .ok()
            .map(|path| repo_dir.join(path))
    };
    let mut paths = Vec::new();
    if let Some(head) = git_path("HEAD").filter(|path| path.exists()) {
        paths.push(head);
    }
    if let Ok(ref_name) = git_stdout(repo_dir, &["symbolic-ref", "--quiet", "HEAD"]) {
        if let Some(reference) = git_path(&ref_name) {
            if reference.exists() {
                paths.push(reference);
            } else {
                // A packed branch becomes loose on its next update. Watch the
                // nearest existing ref directory to observe that creation;
                // nonexistent Cargo watch paths would force every build dirty.
                if let Some(parent) = git_path("refs").and_then(|refs| {
                    reference
                        .ancestors()
                        .skip(1)
                        .take_while(|path| path.starts_with(&refs))
                        .find(|path| path.exists())
                }) {
                    paths.push(parent.to_path_buf());
                }
                if let Some(packed) = git_path("packed-refs").filter(|path| path.exists()) {
                    paths.push(packed);
                }
            }
        }
    }
    paths
}

fn git_stdout(repo_dir: &Path, args: &[&str]) -> Result<String, String> {
    let output = Command::new("git")
        .current_dir(repo_dir)
        .args(args)
        .output()
        .map_err(|error| error.to_string())?;
    if !output.status.success() {
        return Err(String::from_utf8_lossy(&output.stderr).trim().to_string());
    }
    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

#[cfg(test)]
mod tests {
    use super::{dev_version, git_stdout, git_watch_paths, normalize_tag};

    #[test]
    fn linked_worktree_watches_loose_and_packed_branch_updates() {
        let dir = tempfile::tempdir().expect("temporary repository");
        let repo = dir.path();
        let git = |args: &[&str]| git_stdout(repo, args).expect("git fixture command");
        git(&["init", "--initial-branch=main"]);
        git(&[
            "-c",
            "user.name=Test",
            "-c",
            "user.email=test@example.invalid",
            "commit",
            "--no-gpg-sign",
            "--allow-empty",
            "-m",
            "initial",
        ]);
        let worktree = repo.join("linked");
        git(&[
            "worktree",
            "add",
            "-b",
            "nested/topic",
            worktree.to_str().unwrap(),
        ]);
        let reference = repo.join(".git/refs/heads/nested/topic");
        let watches = git_watch_paths(&worktree);
        assert!(watches.iter().all(|path| path.exists()));
        assert!(watches.contains(&reference));
        assert!(watches.contains(&repo.join(".git/worktrees/linked/HEAD")));

        git(&["pack-refs", "--all", "--prune"]);
        assert!(!reference.exists());
        let watches = git_watch_paths(&worktree);
        assert!(watches.iter().all(|path| path.exists()));
        assert!(watches.contains(&repo.join(".git/packed-refs")));
        assert!(watches
            .iter()
            .any(|path| path.is_dir() && reference.starts_with(path)));
        git_stdout(
            &worktree,
            &[
                "-c",
                "user.name=Test",
                "-c",
                "user.email=test@example.invalid",
                "commit",
                "--no-gpg-sign",
                "--allow-empty",
                "-m",
                "advance",
            ],
        )
        .unwrap();
        assert!(reference.exists());
        assert!(git_watch_paths(&worktree).contains(&reference));

        git_stdout(&worktree, &["checkout", "--detach"]).unwrap();
        let watches = git_watch_paths(&worktree);
        assert_eq!(watches.len(), 1);
        assert!(watches[0].ends_with("HEAD"));
    }

    #[test]
    fn normalizes_release_tag_prefix() {
        assert_eq!(normalize_tag("v0.6.0-rc3"), "0.6.0-rc3");
        assert_eq!(normalize_tag("0.6.0-rc3"), "0.6.0-rc3");
    }

    #[test]
    fn renders_shallow_clone_identity() {
        assert_eq!(
            dev_version("0.6.0-rc3", 1, "abcdef0", false),
            "0.6.0-rc3-dev.1+abcdef0"
        );
    }

    #[test]
    fn renders_dirty_dev_identity() {
        assert_eq!(
            dev_version("0.6.0-rc3", 1, "abcdef0", true),
            "0.6.0-rc3-dev.1+abcdef0.dirty"
        );
    }
}
