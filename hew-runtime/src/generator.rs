//! Build-time generator filesystem policy and deterministic execution mode.
//!
//! The driver supplies this record to the child process. Admission removes
//! other host access. This is not an OS sandbox: use OS isolation if hostile
//! generators, including concurrent filesystem replacement, become a requirement.
//! Generators must use the declared output path's spelling, including case:
//! missing directories cannot be canonicalized to discover filesystem aliases.

use std::io;
use std::path::{Component, Path, PathBuf};
use std::sync::OnceLock;

/// Filesystem authority required by an operation.
#[derive(Clone, Copy)]
pub(crate) enum Access {
    Read,
    Write,
}

#[derive(serde::Deserialize)]
#[serde(deny_unknown_fields)]
struct Record {
    read_files: Vec<PathBuf>,
    read_roots: Vec<PathBuf>,
    output: PathBuf,
    staging: PathBuf,
}

struct Policy {
    read_files: Vec<PathBuf>,
    read_roots: Vec<PathBuf>,
    output: PathBuf,
    resolved_output: PathBuf,
    staging: PathBuf,
}

fn denied(message: impl Into<String>) -> io::Error {
    io::Error::new(io::ErrorKind::PermissionDenied, message.into())
}

// Resolve existing components, including symlinks, before interpreting `..`.
// Missing leaves are retained so creation receives the same containment check.
fn resolve(path: &Path, depth: usize) -> io::Result<PathBuf> {
    if depth > 40 {
        return Err(denied("generator path contains too many symbolic links"));
    }
    let absolute = if path.is_absolute() {
        path.to_path_buf()
    } else {
        std::env::current_dir()?.join(path)
    };
    let mut resolved = PathBuf::new();
    for component in absolute.components() {
        match component {
            Component::ParentDir => {
                resolved.pop();
            }
            Component::CurDir => {}
            Component::Prefix(_) | Component::RootDir => resolved.push(component),
            Component::Normal(name) => {
                resolved.push(name);
                match std::fs::symlink_metadata(&resolved) {
                    Ok(metadata) if metadata.is_symlink() => {
                        let target = std::fs::read_link(&resolved)?;
                        resolved.pop();
                        resolved = resolve(&resolved.join(target), depth + 1)?;
                    }
                    Ok(_) => {
                        // Canonicalization also handles platform path aliases.
                        resolved = std::fs::canonicalize(&resolved)?;
                    }
                    Err(error) if error.kind() == io::ErrorKind::NotFound => {}
                    Err(error) => return Err(error),
                }
            }
        }
    }
    Ok(resolved)
}

impl Policy {
    fn new(record: Record) -> io::Result<Self> {
        if !record.output.is_absolute() || !record.staging.is_absolute() {
            return Err(denied("generator output paths must be absolute"));
        }
        if record
            .output
            .components()
            .any(|part| matches!(part, Component::ParentDir))
        {
            return Err(denied(
                "generator output path must not contain parent traversal",
            ));
        }
        if std::fs::symlink_metadata(&record.output).is_ok_and(|meta| meta.is_symlink()) {
            return Err(denied(
                "generator output directory must not be a symbolic link",
            ));
        }
        let staging = std::fs::canonicalize(&record.staging)?;
        let resolved_output = resolve(&record.output, 0)?;
        if staging.starts_with(&resolved_output) || resolved_output.starts_with(&staging) {
            return Err(denied(
                "generator staging and output directories must be separate",
            ));
        }
        Ok(Self {
            read_files: record
                .read_files
                .iter()
                .map(|path| resolve(path, 0))
                .collect::<io::Result<_>>()?,
            read_roots: record
                .read_roots
                .iter()
                .map(|path| resolve(path, 0))
                .collect::<io::Result<_>>()?,
            output: record.output,
            resolved_output,
            staging,
        })
    }

    fn check(&self, path: &Path, access: Access) -> io::Result<PathBuf> {
        let absolute = if path.is_absolute() {
            path.to_path_buf()
        } else {
            std::env::current_dir()?.join(path)
        };
        // Remap before looking at old output children: --check observes only
        // the fresh staging tree, with the generator's ordinary arguments.
        let target = if let Ok(relative) = absolute.strip_prefix(&self.output) {
            self.staging.join(relative)
        } else {
            let resolved = resolve(&absolute, 0)?;
            if let Ok(relative) = resolved.strip_prefix(&self.resolved_output) {
                self.staging.join(relative)
            } else {
                absolute
            }
        };
        let resolved = resolve(&target, 0)?;
        let allowed = resolved.starts_with(&self.staging)
            || matches!(access, Access::Read)
                && (self.read_files.contains(&resolved)
                    || self
                        .read_roots
                        .iter()
                        .any(|root| resolved.starts_with(root)));
        if allowed {
            Ok(target)
        } else {
            Err(denied(format!(
                "generator path is outside its declared access: {}; use the declared output path exactly as spelled, including case",
                path.display()
            )))
        }
    }
    fn directory_names(&self, directory: &Path, names: &mut Vec<String>) -> io::Result<()> {
        names.retain(|name| self.check(&directory.join(name), Access::Read).is_ok());
        if self.resolved_output.parent() == Some(resolve(directory, 0)?.as_path()) {
            if let Some(name) = self.output.file_name().and_then(|name| name.to_str()) {
                names.push(name.to_owned());
            }
        }
        names.sort();
        names.dedup();
        Ok(())
    }
}

static POLICY: OnceLock<Result<Option<Policy>, String>> = OnceLock::new();

fn policy() -> &'static Result<Option<Policy>, String> {
    POLICY.get_or_init(|| {
        let Some(value) = std::env::var_os("HEW_GENERATE_POLICY") else {
            return Ok(None);
        };
        let value = value
            .into_string()
            .map_err(|_| "generator policy is not UTF-8".to_owned())?;
        let record = serde_json::from_str(&value)
            .map_err(|error| format!("invalid generator policy: {error}"))?;
        Policy::new(record)
            .map(Some)
            .map_err(|error| error.to_string())
    })
}

/// Whether this process was started as a build-time generator.
/// Invalid records retain deterministic mode and deny every filesystem request.
#[must_use]
pub fn enabled() -> bool {
    !matches!(policy(), Ok(None))
}

pub(crate) fn checked_path(path: &Path, access: Access) -> io::Result<PathBuf> {
    match policy() {
        Ok(Some(policy)) => policy.check(path, access),
        Ok(None) => Ok(path.to_path_buf()),
        Err(error) => Err(denied(error.clone())),
    }
}

pub(crate) fn directory_names(path: &Path, names: &mut Vec<String>) -> io::Result<()> {
    match policy() {
        Ok(Some(policy)) => policy.directory_names(path, names),
        Ok(None) => Ok(()),
        Err(error) => Err(denied(error.clone())),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn policy_applies_before_runtime_start() {
        use crate::test_string::ManagedString;

        if let Some(root) = std::env::var_os("HEW_GENERATE_TEST_ROOT") {
            let root = PathBuf::from(root);
            let output = ManagedString::new(root.join("output/result.hew").to_str().unwrap());
            let denied = ManagedString::new(root.join("input.json").to_str().unwrap());
            let content = ManagedString::new("generated");
            // SAFETY: managed strings remain live; clock/random have no preconditions.
            unsafe {
                assert_eq!(
                    crate::file_io::hew_file_write(output.as_ptr(), content.as_ptr()),
                    0
                );
                assert_eq!(
                    crate::file_io::hew_file_write(denied.as_ptr(), content.as_ptr()),
                    -1
                );
                assert_eq!(
                    crate::stream_error::take_last_error_kind(),
                    crate::stream_error::IO_ERROR_KIND_PERMISSION_DENIED
                );
                assert_eq!(
                    crate::file_secure::hew_fs_mkdir_mode(denied.as_ptr(), 0o700),
                    -1
                );
                assert_eq!(
                    crate::stream_error::take_last_error_kind(),
                    crate::stream_error::IO_ERROR_KIND_PERMISSION_DENIED
                );
                assert_eq!(crate::clock::hew_now_ms(), 0);
                assert_eq!(crate::clock::hew_instant_now(), 0);
                for (index, expected) in [(0, "hew generate"), (1, "--exact")] {
                    let argument = crate::env::hew_args_get(index);
                    assert_eq!(hew_cabi::string::string_as_str(argument), expected);
                    hew_cabi::string::string_release(argument);
                }
                assert_eq!(
                    crate::random::hew_random_random().to_bits(),
                    0.844_421_851_525_048_1_f64.to_bits()
                );
                assert_eq!(
                    crate::random::hew_random_random().to_bits(),
                    0.757_954_402_940_302_5_f64.to_bits()
                );
            }
            return;
        }

        let root =
            std::env::temp_dir().join(format!("hew-generator-startup-{}", std::process::id()));
        std::fs::create_dir_all(root.join("staging")).unwrap();
        std::fs::write(root.join("input.json"), "retained").unwrap();
        let record = serde_json::json!({
            "read_files": [root.join("input.json")],
            "read_roots": [],
            "output": root.join("output"),
            "staging": root.join("staging"),
        });
        let child = std::process::Command::new(std::env::current_exe().unwrap())
            .args([
                "--exact",
                "generator::tests::policy_applies_before_runtime_start",
                "--nocapture",
            ])
            .env("HEW_GENERATE_TEST_ROOT", &root)
            .env("HEW_GENERATE_POLICY", record.to_string())
            .output()
            .unwrap();
        assert!(
            child.status.success(),
            "{}\n{}",
            String::from_utf8_lossy(&child.stdout),
            String::from_utf8_lossy(&child.stderr)
        );
        assert_eq!(
            std::fs::read_to_string(root.join("staging/result.hew")).unwrap(),
            "generated"
        );
        assert_eq!(
            std::fs::read_to_string(root.join("input.json")).unwrap(),
            "retained"
        );
        assert!(!root.join("output").exists());
        std::fs::remove_dir_all(root).unwrap();
    }

    #[test]
    fn remaps_output_and_denies_traversal_and_undeclared_paths() {
        let root =
            std::env::temp_dir().join(format!("hew-generator-policy-{}", std::process::id()));
        std::fs::create_dir_all(root.join("staging")).unwrap();
        std::fs::create_dir_all(root.join("package")).unwrap();
        std::fs::write(root.join("input.json"), "{}").unwrap();
        let policy = Policy::new(Record {
            read_files: vec![root.join("input.json")],
            read_roots: vec![root.join("package")],
            output: root.join("output"),
            staging: root.join("staging"),
        })
        .unwrap();
        let target = policy
            .check(&root.join("output/new.hew"), Access::Write)
            .unwrap();
        std::fs::write(&target, "generated").unwrap();
        assert!(!root.join("output/new.hew").exists());
        assert_eq!(
            std::fs::read_to_string(
                policy
                    .check(&root.join("output/new.hew"), Access::Read)
                    .unwrap()
            )
            .unwrap(),
            "generated"
        );
        let mut names = vec![
            "input.json".to_owned(),
            "package".to_owned(),
            "secret".to_owned(),
        ];
        policy.directory_names(&root, &mut names).unwrap();
        assert_eq!(names, ["input.json", "output", "package"]);
        std::fs::create_dir(root.join("output")).unwrap();
        let mut names_with_output = names.clone();
        names_with_output.push("output".to_owned());
        policy
            .directory_names(&root, &mut names_with_output)
            .unwrap();
        assert_eq!(names_with_output, names);
        assert!(policy.check(&root.join("input.json"), Access::Read).is_ok());
        assert!(policy
            .check(&root.join("package/source.hew"), Access::Read)
            .is_ok());
        for path in ["input.json", "package/source.hew", "output/../escape.hew"] {
            assert_eq!(
                policy
                    .check(&root.join(path), Access::Write)
                    .unwrap_err()
                    .kind(),
                io::ErrorKind::PermissionDenied
            );
        }
        assert_eq!(
            policy
                .check(&root.join("secret"), Access::Read)
                .unwrap_err()
                .kind(),
            io::ErrorKind::PermissionDenied
        );
        #[cfg(unix)]
        {
            std::os::unix::fs::symlink(&root, root.join("staging/escape")).unwrap();
            assert_eq!(
                policy
                    .check(&root.join("output/escape/new.hew"), Access::Write)
                    .unwrap_err()
                    .kind(),
                io::ErrorKind::PermissionDenied
            );
            std::os::unix::fs::symlink(root.join("missing"), root.join("staging/dangling"))
                .unwrap();
            assert_eq!(
                policy
                    .check(&root.join("output/dangling"), Access::Write)
                    .unwrap_err()
                    .kind(),
                io::ErrorKind::PermissionDenied
            );
            std::os::unix::fs::symlink(&root, root.join("package/escape")).unwrap();
            assert_eq!(
                policy
                    .check(&root.join("package/escape/secret"), Access::Read)
                    .unwrap_err()
                    .kind(),
                io::ErrorKind::PermissionDenied
            );
            std::fs::remove_dir(root.join("output")).unwrap();
            std::os::unix::fs::symlink(&root, root.join("output")).unwrap();
            assert!(Policy::new(Record {
                read_files: vec![],
                read_roots: vec![],
                output: root.join("output"),
                staging: root.join("staging")
            })
            .is_err());
        }
        std::fs::remove_dir_all(root).unwrap();
    }
}
