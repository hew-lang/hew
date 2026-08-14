use std::path::Path;
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

fn git_version(repo_dir: &Path, cargo_version: &str) -> String {
    if git_stdout(repo_dir, &["rev-parse", "--git-dir"]).is_err() {
        return cargo_version.to_string();
    }

    let Ok(describe) = git_stdout(repo_dir, &["describe", "--tags", "--long", "--dirty"]) else {
        return cargo_version.to_string();
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
    let Ok(git_dir) = git_stdout(repo_dir, &["rev-parse", "--git-dir"]) else {
        return;
    };
    let git_dir = repo_dir.join(git_dir);
    println!("cargo:rerun-if-changed={}", git_dir.join("HEAD").display());

    if let Ok(ref_name) = git_stdout(repo_dir, &["symbolic-ref", "--quiet", "HEAD"]) {
        println!(
            "cargo:rerun-if-changed={}",
            git_dir.join(ref_name).display()
        );
    }
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
    use super::normalize_tag;

    #[test]
    fn normalizes_release_tag_prefix() {
        assert_eq!(normalize_tag("v0.6.0-rc2"), "0.6.0-rc2");
        assert_eq!(normalize_tag("0.6.0-rc2"), "0.6.0-rc2");
    }
}
