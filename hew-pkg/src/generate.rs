//! Manifest-owned source generation and content stamps.

use std::collections::{BTreeMap, BTreeSet};
use std::path::{Component, Path, PathBuf};

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

/// One package-owned generator. Registry dependencies never execute these steps.
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct GenerateStep {
    pub name: String,
    pub run: String,
    #[serde(default)]
    pub args: Vec<String>,
    #[serde(default)]
    pub inputs: Vec<String>,
    pub outputs: String,
}

/// Validate paths once, then order steps by their declared input dependencies.
///
/// # Errors
/// Returns a diagnostic for invalid paths, overlapping outputs or cycles.
pub fn step_order(steps: &[GenerateStep]) -> Result<Vec<usize>, String> {
    let mut names = BTreeSet::new();
    for step in steps {
        if step.name.is_empty()
            || !step
                .name
                .bytes()
                .all(|byte| byte.is_ascii_alphanumeric() || b"_-".contains(&byte))
            || !names.insert(&step.name)
        {
            return Err(format!(
                "generation step name `{}` must be unique and contain only letters, digits, `_` or `-`",
                step.name
            ));
        }
        relative_path(&step.outputs)?;
        if Path::new(&step.run)
            .extension()
            .is_some_and(|extension| extension == "hew")
        {
            relative_path(&step.run)?;
            if Path::new(&step.run).starts_with(&step.outputs) {
                return Err(format!(
                    "generator `{}` is inside its own output directory",
                    step.name
                ));
            }
        } else if step.run.is_empty()
            || !step.run.split('.').all(|part| {
                !part.is_empty()
                    && part
                        .bytes()
                        .all(|byte| byte.is_ascii_alphanumeric() || byte == b'_')
            })
        {
            return Err(format!(
                "generator `{}` must name a `.hew` source or a dev-dependency",
                step.run
            ));
        }
        for input in &step.inputs {
            relative_path(input)?;
            glob::Pattern::new(input)
                .map_err(|error| format!("invalid generator input `{input}`: {error}"))?;
        }
    }
    for (index, step) in steps.iter().enumerate() {
        for other in &steps[..index] {
            if Path::new(&step.outputs).starts_with(&other.outputs)
                || Path::new(&other.outputs).starts_with(&step.outputs)
            {
                return Err(format!(
                    "generation steps `{}` and `{}` have overlapping output directories",
                    step.name, other.name
                ));
            }
        }
    }
    let dependencies = steps
        .iter()
        .map(|step| {
            steps
                .iter()
                .enumerate()
                .filter_map(|(index, other)| {
                    ((Path::new(&step.run)
                        .extension()
                        .is_some_and(|extension| extension == "hew")
                        && Path::new(&step.run).starts_with(&other.outputs))
                        || step
                            .inputs
                            .iter()
                            .any(|input| input_may_read_output(input, &other.outputs)))
                    .then_some(index)
                })
                .collect::<BTreeSet<_>>()
        })
        .collect::<Vec<_>>();
    let mut ordered = Vec::new();
    let mut finished = BTreeSet::new();
    while ordered.len() < steps.len() {
        let next = (0..steps.len())
            .find(|index| !finished.contains(index) && dependencies[*index].is_subset(&finished));
        let Some(next) = next else {
            let remaining = steps
                .iter()
                .enumerate()
                .filter(|(index, _)| !finished.contains(index))
                .map(|(_, step)| step.name.as_str())
                .collect::<Vec<_>>();
            return Err(format!(
                "generation input cycle: {}",
                remaining.join(" -> ")
            ));
        };
        finished.insert(next);
        ordered.push(next);
    }
    Ok(ordered)
}

fn relative_path(value: &str) -> Result<(), String> {
    if value.is_empty()
        || value.contains('\\')
        || value.contains(':')
        || Path::new(value)
            .components()
            .any(|component| !matches!(component, Component::Normal(_)))
        || value
            .split('/')
            .any(|part| matches!(part, "" | "." | ".." | ".git" | ".hew"))
    {
        return Err(format!(
            "generation path `{value}` must be a package-relative path without traversal or reserved directories"
        ));
    }
    Ok(())
}

#[expect(
    clippy::case_sensitive_file_extension_comparisons,
    reason = "matches the literal suffix of a glob against the required lowercase output extension"
)]
fn input_may_read_output(input: &str, output: &str) -> bool {
    fn below(pattern: &[&str], directory: &[&str]) -> bool {
        let Some(segment) = pattern.first() else {
            return false;
        };
        if directory.is_empty() {
            return true;
        }
        if *segment == "**" {
            return below(&pattern[1..], directory) || below(pattern, &directory[1..]);
        }
        glob::Pattern::new(segment).is_ok_and(|pattern| pattern.matches(directory[0]))
            && below(&pattern[1..], &directory[1..])
    }
    if !below(
        &input.split('/').collect::<Vec<_>>(),
        &output.split('/').collect::<Vec<_>>(),
    ) {
        return false;
    }
    let filename = input.rsplit('/').next().unwrap_or(input);
    let suffix = filename.rsplit(['*', '?', ']']).next().unwrap_or(filename);
    // Every generator output is Hew source. A literal incompatible suffix
    // cannot consume another step's output, even under a recursive glob.
    suffix.is_empty() || ".hew".ends_with(suffix) || suffix.ends_with(".hew")
}

/// Expand declared input globs into sorted paths whose targets stay inside the package.
///
/// # Errors
/// Returns an error for unreadable paths or a symlink escaping the package.
pub fn input_files(root: &Path, step: &GenerateStep) -> Result<Vec<PathBuf>, String> {
    fn visit(
        root: &Path,
        path: &Path,
        patterns: &[glob::Pattern],
        parents: &mut Vec<PathBuf>,
        files: &mut BTreeSet<PathBuf>,
    ) -> Result<(), String> {
        let target = match path.canonicalize() {
            Ok(target) => target,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(()),
            Err(error) => return Err(error.to_string()),
        };
        if !target.starts_with(root) {
            return Err(format!(
                "generator input {} escapes its package",
                path.display()
            ));
        }
        let Some(pattern) = patterns.first() else {
            if target.is_file() {
                files.insert(path.to_path_buf());
            }
            return Ok(());
        };
        let recursive = pattern.as_str() == "**";
        if recursive {
            if parents.contains(&target) {
                return Ok(());
            }
            visit(root, path, &patterns[1..], parents, files)?;
        }
        if !target.is_dir() {
            return Ok(());
        }
        parents.push(target);
        for entry in std::fs::read_dir(path).map_err(|error| error.to_string())? {
            let entry = entry.map_err(|error| error.to_string())?;
            if recursive || pattern.matches_path(Path::new(&entry.file_name())) {
                visit(
                    root,
                    &entry.path(),
                    if recursive { patterns } else { &patterns[1..] },
                    parents,
                    files,
                )?;
            }
        }
        parents.pop();
        Ok(())
    }

    let root = root.canonicalize().map_err(|error| error.to_string())?;
    let mut files = BTreeSet::new();
    for input in &step.inputs {
        relative_path(input)?;
        // Keep filesystem roots (including Windows verbatim/UNC prefixes)
        // separate from glob syntax and preserve each matched path's spelling.
        let patterns = input
            .split('/')
            .map(glob::Pattern::new)
            .collect::<Result<Vec<_>, _>>()
            .map_err(|error| error.to_string())?;
        visit(&root, &root, &patterns, &mut Vec::new(), &mut files)?;
    }
    Ok(files.into_iter().collect())
}

/// Source marker written by a generator before the driver formats its output.
#[must_use]
pub fn header(step: &GenerateStep) -> String {
    format!(
        "// @generated by hew generate ({}) from {}; do not edit",
        step.name,
        step.inputs.join(", ")
    )
}

/// Resolve a generator through its explicit source or installed dev-dependency.
///
/// # Errors
/// Returns an error for a missing source, dependency or inconsistent lock entry.
pub fn generator_entry(
    root: &Path,
    manifest: &crate::manifest::HewManifest,
    step: &GenerateStep,
) -> Result<(PathBuf, PathBuf), String> {
    if Path::new(&step.run)
        .extension()
        .is_some_and(|extension| extension == "hew")
    {
        let source = root
            .join(&step.run)
            .canonicalize()
            .map_err(|error| format!("cannot read generator {}: {error}", step.run))?;
        if !source.starts_with(root) {
            return Err(format!("generator {} escapes its package", step.run));
        }
        return Ok((source, root.to_path_buf()));
    }
    let spec = manifest
        .dev_dependencies
        .get(&step.run)
        .ok_or_else(|| format!("generator `{}` is not a declared dev-dependency", step.run))?;
    let (package, version) = if let crate::manifest::DepSpec::Table(table) = spec {
        table.path.as_ref().map(|path| (root.join(path), None))
    } else {
        None
    }
    .map_or_else(
        || {
            let lock = crate::lockfile::read_lockfile(&root.join("hew.lock"))
                .map_err(|error| format!("{error}; run `hew install` before generation"))?;
            let locked = lock
                .packages
                .iter()
                .find(|package| package.name == step.run)
                .ok_or_else(|| {
                    format!(
                        "generator `{}` is absent from hew.lock; run `hew install`",
                        step.run
                    )
                })?;
            let package = crate::paths::package_link(&root.join(".hew/packages"), &step.run)?;
            Ok::<_, String>((package, Some(locked.version.clone())))
        },
        Ok,
    )?;
    let package = package.canonicalize().map_err(|error| {
        format!(
            "generator package {} is unavailable: {error}; run `hew install`",
            package.display()
        )
    })?;
    let selected = crate::manifest::parse_manifest(&package.join("hew.toml"))
        .map_err(|error| error.to_string())?;
    if selected.package.name != step.run
        || version.is_some_and(|version| selected.package.version != version)
    {
        return Err(format!(
            "generator package `{}` does not match hew.lock",
            step.run
        ));
    }
    let entry = package.join(selected.package.main_source());
    if !entry.is_file() {
        return Err(format!("generator entry {} is missing", entry.display()));
    }
    Ok((entry, package))
}

/// Identify generated source without depending on its manifest being present.
#[must_use]
pub fn is_generated(source: &str) -> bool {
    source
        .lines()
        .next()
        .is_some_and(|line| line.starts_with("// @generated by hew generate ("))
}

/// The source list comes from the shared frontend's actual input reads.
#[derive(Debug, Serialize, Deserialize)]
pub struct Stamp {
    pub digest: String,
    pub sources: BTreeMap<PathBuf, Option<String>>,
    pub output_digest: String,
    pub package_root: PathBuf,
    pub output_directories: Vec<PathBuf>,
    pub compiler: String,
}

/// The package inventory is both readable by the generator and hashed by its stamp.
///
/// # Errors
/// Returns the package snapshot error, including unsafe symlinks.
pub fn package_files(root: &Path, outputs: &[PathBuf]) -> Result<Vec<PathBuf>, String> {
    crate::package_fs::collect_package_snapshot(root)
        .map(|files| {
            files
                .into_iter()
                .map(|file| root.join(file.path))
                .filter(|path| !outputs.iter().any(|output| path.starts_with(output)))
                .collect()
        })
        .map_err(|error| format!("cannot read generator package {}: {error}", root.display()))
}

/// Whether a changed path belongs to package content, excluding build/cache directories.
#[must_use]
pub fn is_package_path(root: &Path, path: &Path) -> bool {
    path.strip_prefix(root).is_ok_and(|relative| {
        !relative.components().any(|component| {
            component
                .as_os_str()
                .to_str()
                .is_some_and(crate::package_fs::is_skipped_package_dir)
        })
    })
}

/// Hash all tracked generator sources, package lockfiles and declared input data.
///
/// # Errors
/// Returns an error when a tracked file cannot be read.
pub fn stamp(
    root: &Path,
    package_root: &Path,
    output_directories: &[PathBuf],
    step: &GenerateStep,
    sources: impl IntoIterator<Item = PathBuf>,
    compiler: &str,
) -> Result<Stamp, String> {
    let mut files = sources.into_iter().collect::<BTreeSet<_>>();
    files.extend(package_files(package_root, output_directories)?);
    files.extend(input_files(root, step)?);
    files.insert(root.join("hew.toml"));
    if root.join("hew.lock").is_file() {
        files.insert(root.join("hew.lock"));
    }
    let mut hash = Sha256::new();
    hash.update(serde_json::to_vec(step).map_err(|error| error.to_string())?);
    hash.update(compiler.as_bytes());
    let mut sources = BTreeMap::new();
    for file in files {
        let digest = match std::fs::read(&file) {
            Ok(data) => Some(crate::package_fs::sha256_prefixed(&data)),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => None,
            Err(error) => return Err(format!("cannot read {}: {error}", file.display())),
        };
        hash.update(file.to_string_lossy().as_bytes());
        hash.update([0]);
        hash.update(digest.as_deref().unwrap_or("missing").as_bytes());
        sources.insert(file, digest);
    }
    Ok(Stamp {
        digest: crate::package_fs::format_sha256(&hash.finalize()),
        sources,
        output_digest: String::new(),
        package_root: package_root.to_path_buf(),
        output_directories: output_directories.to_vec(),
        compiler: compiler.to_string(),
    })
}

/// Read an output tree without following symlinks, in stable path order.
///
/// # Errors
/// Returns an error for symlinks, non-source files or unreadable output.
pub fn output_files(root: &Path) -> Result<BTreeMap<PathBuf, Vec<u8>>, String> {
    fn visit(
        root: &Path,
        path: &Path,
        files: &mut BTreeMap<PathBuf, Vec<u8>>,
    ) -> Result<(), String> {
        for entry in std::fs::read_dir(path).map_err(|error| error.to_string())? {
            let entry = entry.map_err(|error| error.to_string())?;
            let kind = entry.file_type().map_err(|error| error.to_string())?;
            let path = entry.path();
            if kind.is_dir() {
                visit(root, &path, files)?;
            } else if kind.is_file() && path.extension().is_some_and(|extension| extension == "hew")
            {
                files.insert(
                    path.strip_prefix(root)
                        .map_err(|error| error.to_string())?
                        .to_path_buf(),
                    std::fs::read(&path).map_err(|error| error.to_string())?,
                );
            } else {
                return Err(format!(
                    "generator output {} must be an ordinary `.hew` file or directory",
                    path.display()
                ));
            }
        }
        Ok(())
    }
    let mut files = BTreeMap::new();
    if root.exists() {
        if root
            .symlink_metadata()
            .map_err(|error| error.to_string())?
            .file_type()
            .is_symlink()
        {
            return Err(format!(
                "generator output {} must not be a symlink",
                root.display()
            ));
        }
        visit(root, root, &mut files)?;
    }
    Ok(files)
}

#[must_use]
pub fn output_digest(files: &BTreeMap<PathBuf, Vec<u8>>) -> String {
    let mut hash = Sha256::new();
    for (path, bytes) in files {
        hash.update(path.to_string_lossy().as_bytes());
        hash.update([0]);
        hash.update(bytes.len().to_le_bytes());
        hash.update(bytes);
    }
    crate::package_fs::format_sha256(&hash.finalize())
}

/// Inspect cached inputs and outputs without compiling or running a generator.
///
/// # Errors
/// Returns an error for unreadable package inputs or output.
pub fn is_stale(root: &Path, step: &GenerateStep, compiler: &str) -> Result<bool, String> {
    let path = root
        .join(".hew/generate")
        .join(format!("{}.stamp", step.name));
    let Some(stamp) = std::fs::read(path)
        .ok()
        .and_then(|bytes| serde_json::from_slice::<Stamp>(&bytes).ok())
    else {
        return Ok(true);
    };
    if stamp.compiler != compiler {
        return Ok(true);
    }
    if package_files(&stamp.package_root, &stamp.output_directories)?
        .iter()
        .any(|path| !stamp.sources.contains_key(path))
    {
        return Ok(true);
    }
    for (path, expected) in &stamp.sources {
        let actual = match std::fs::read(path) {
            Ok(bytes) => Some(crate::package_fs::sha256_prefixed(&bytes)),
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => None,
            Err(error) => return Err(error.to_string()),
        };
        if &actual != expected {
            return Ok(true);
        }
    }
    if input_files(root, step)?
        .iter()
        .any(|path| !stamp.sources.contains_key(path))
    {
        return Ok(true);
    }
    Ok(output_digest(&output_files(&root.join(&step.outputs))?) != stamp.output_digest)
}

#[cfg(test)]
mod tests {
    use super::{input_files, step_order, GenerateStep};

    #[test]
    fn input_globs_keep_the_root_separate_and_preserve_matched_paths() {
        let temporary = tempfile::tempdir().unwrap();
        let root = temporary.path().join("package[1]");
        std::fs::create_dir_all(root.join("data/nested/deeper")).unwrap();
        for name in [
            "data/a.json",
            "data/nested/deeper/b.json",
            "data/ignore.txt",
        ] {
            std::fs::write(root.join(name), "input").unwrap();
        }
        let root = root.canonicalize().unwrap();
        let mut step = GenerateStep {
            name: "model".into(),
            run: "generator.hew".into(),
            args: vec![],
            inputs: vec!["data/**/*.json".into(), "data/a.json".into()],
            outputs: "generated".into(),
        };
        assert_eq!(
            input_files(&root, &step).unwrap(),
            vec![
                root.join("data/a.json"),
                root.join("data/nested/deeper/b.json")
            ]
        );
        step.inputs = vec!["missing/*.json".into()];
        assert!(input_files(&root, &step).unwrap().is_empty());

        #[cfg(unix)]
        {
            use std::os::unix::fs::symlink;
            symlink("data", root.join("alias")).unwrap();
            symlink("../..", root.join("data/nested/loop")).unwrap();
            step.inputs = vec!["alias/**/*.json".into()];
            assert_eq!(
                input_files(&root, &step).unwrap(),
                vec![
                    root.join("alias/a.json"),
                    root.join("alias/nested/deeper/b.json")
                ]
            );
            std::fs::write(temporary.path().join("outside.json"), "outside").unwrap();
            symlink(
                temporary.path().join("outside.json"),
                root.join("escape.json"),
            )
            .unwrap();
            step.inputs = vec!["*.json".into()];
            assert!(input_files(&root, &step).unwrap_err().contains("escapes"));
        }
    }

    #[test]
    fn generated_entry_and_input_dependencies_share_ordering_and_cycle_checks() {
        let mut steps = vec![
            GenerateStep {
                name: "model".into(),
                run: "tools/schema.hew".into(),
                args: vec![],
                inputs: vec!["data/*.json".into()],
                outputs: "models".into(),
            },
            GenerateStep {
                name: "tool".into(),
                run: "bootstrap.hew".into(),
                args: vec![],
                inputs: vec![],
                outputs: "tools".into(),
            },
        ];
        assert_eq!(step_order(&steps).unwrap(), vec![1, 0]);
        steps[1].inputs.push("models/**/*.hew".into());
        assert!(step_order(&steps).unwrap_err().contains("cycle"));
        steps[1].inputs = vec!["models/*.json".into()];
        assert_eq!(step_order(&steps).unwrap(), vec![1, 0]);
    }
}
