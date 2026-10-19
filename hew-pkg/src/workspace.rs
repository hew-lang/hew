//! Virtual workspaces: a root `hew.toml` with `[workspace]` and no
//! `[package]`, listing its member packages explicitly.
//!
//! ```toml
//! [workspace]
//! members = ["dag", "db/*", "net/http"]
//! ```
//!
//! A `*` segment matches exactly one directory. Every match and every literal
//! entry must hold a package manifest. Nothing else is discovered: no
//! directory walk happens, so `.hew/packages`, `target/` and nested
//! checkouts never become members.

use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::path::{Component, Path, PathBuf};

use serde::Deserialize;

use crate::manifest::{self, HewManifest};
use crate::project::MANIFEST_FILE;

/// The `[workspace]` table of a root manifest.
#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct WorkspaceFile {
    workspace: WorkspaceTable,
}

#[derive(Debug, Deserialize)]
#[serde(deny_unknown_fields)]
struct WorkspaceTable {
    members: Vec<String>,
}

/// A member package of a workspace.
#[derive(Debug)]
pub struct Member {
    /// Canonical directory holding the member's `hew.toml`.
    pub dir: PathBuf,
    /// The member's manifest.
    pub manifest: HewManifest,
}

impl Member {
    /// The member's package name.
    #[must_use]
    pub fn name(&self) -> &str {
        &self.manifest.package.name
    }

    /// The member's version.
    #[must_use]
    pub fn version(&self) -> &str {
        &self.manifest.package.version
    }
}

/// A loaded workspace: its root and members, sorted by name.
#[derive(Debug)]
pub struct Workspace {
    /// Canonical directory holding the workspace `hew.toml`.
    pub root: PathBuf,
    /// Members, sorted by package name.
    pub members: Vec<Member>,
}

/// Why a workspace cannot be loaded or ordered.
#[derive(Debug)]
pub enum WorkspaceError {
    /// A manifest could not be read or parsed.
    Manifest {
        /// The manifest that failed.
        path: PathBuf,
        /// The underlying error.
        source: manifest::ManifestError,
    },
    /// The root manifest's `[workspace]` table is malformed.
    Invalid {
        /// The root manifest.
        path: PathBuf,
        /// What is wrong.
        detail: String,
    },
    /// A member entry is not a package, or two members share a name.
    Member(String),
    /// A member's requirement on another member does not admit its version.
    Requirement {
        /// The depending member.
        member: String,
        /// The member depended on.
        dependency: String,
        /// The requirement `member` declares.
        requirement: String,
        /// The version `dependency` declares.
        version: String,
    },
    /// Members depend on each other in a cycle.
    Cycle(Vec<String>),
}

impl fmt::Display for WorkspaceError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Manifest { path, source } => {
                write!(f, "cannot load {}: {source}", path.display())
            }
            Self::Invalid { path, detail } => {
                write!(f, "invalid workspace {}: {detail}", path.display())
            }
            Self::Member(detail) => write!(f, "E_WORKSPACE_MEMBER: {detail}"),
            Self::Requirement {
                member,
                dependency,
                requirement,
                version,
            } => write!(
                f,
                "E_WORKSPACE_REQUIREMENT: {member} requires {dependency} `{requirement}`, but the \
                 workspace member {dependency} is {version}"
            ),
            Self::Cycle(cycle) => write!(
                f,
                "E_WORKSPACE_CYCLE: workspace members depend on each other in a cycle: {}",
                cycle.join(" -> ")
            ),
        }
    }
}

impl std::error::Error for WorkspaceError {}

/// Whether the TOML table `table` is a workspace root.
#[must_use]
pub(crate) fn declares_workspace(table: &toml::Table) -> bool {
    table.contains_key("workspace")
}

/// Load the workspace whose root manifest is `root/hew.toml`.
///
/// # Errors
///
/// Returns [`WorkspaceError`] when the root manifest has no `[workspace]`,
/// a member entry is malformed or names no package, a member manifest fails
/// to parse, or two members share a name.
pub fn load(root: &Path) -> Result<Workspace, WorkspaceError> {
    let (root, dirs) = member_dirs(root)?;
    let mut members = Vec::new();
    let mut seen: BTreeMap<String, PathBuf> = BTreeMap::new();
    for dir in dirs {
        let manifest_path = dir.join(MANIFEST_FILE);
        let manifest = manifest::parse_manifest(&manifest_path).map_err(|source| {
            WorkspaceError::Manifest {
                path: manifest_path.clone(),
                source,
            }
        })?;
        if let Some(first) = seen.insert(manifest.package.name.clone(), dir.clone()) {
            return Err(WorkspaceError::Member(format!(
                "members {} and {} are both named `{}`",
                first.display(),
                dir.display(),
                manifest.package.name
            )));
        }
        members.push(Member { dir, manifest });
    }
    members.sort_by(|left, right| left.name().cmp(right.name()));
    Ok(Workspace { root, members })
}

/// The canonical workspace root and its members' canonical directories,
/// expanded from the root manifest without parsing any member.
fn member_dirs(root: &Path) -> Result<(PathBuf, BTreeSet<PathBuf>), WorkspaceError> {
    let root = root
        .canonicalize()
        .map_err(|error| WorkspaceError::Manifest {
            path: root.join(MANIFEST_FILE),
            source: error.into(),
        })?;
    let path = root.join(MANIFEST_FILE);
    let text = std::fs::read_to_string(&path).map_err(|error| WorkspaceError::Manifest {
        path: path.clone(),
        source: error.into(),
    })?;
    let file: WorkspaceFile = toml::from_str(&text).map_err(|error| WorkspaceError::Invalid {
        path: path.clone(),
        detail: error.to_string(),
    })?;
    let mut dirs = BTreeSet::new();
    for entry in &file.workspace.members {
        dirs.extend(expand_member(&root, entry)?);
    }
    Ok((root, dirs))
}

/// Expand one `members` entry to canonical package directories.
fn expand_member(root: &Path, entry: &str) -> Result<Vec<PathBuf>, WorkspaceError> {
    let refuse = |detail: &str| Err(WorkspaceError::Member(format!("`{entry}`: {detail}")));
    if entry.is_empty() || entry.contains('\\') {
        return refuse("write members as `/`-separated paths relative to the workspace");
    }
    let relative = Path::new(entry);
    if relative
        .components()
        .any(|component| !matches!(component, Component::Normal(_) | Component::CurDir))
    {
        return refuse("members must be below the workspace root");
    }

    let mut candidates = vec![root.to_path_buf()];
    for component in relative.components() {
        let Component::Normal(segment) = component else {
            continue;
        };
        let segment = segment.to_string_lossy();
        if segment == "*" {
            let mut next = Vec::new();
            for dir in &candidates {
                let mut matches = std::fs::read_dir(dir)
                    .map_err(|error| {
                        WorkspaceError::Member(format!(
                            "`{entry}`: cannot read {}: {error}",
                            dir.display()
                        ))
                    })?
                    .filter_map(Result::ok)
                    .filter(|child| {
                        child.path().is_dir()
                            && !child.file_name().to_string_lossy().starts_with('.')
                    })
                    .map(|child| child.path())
                    .collect::<Vec<_>>();
                matches.sort();
                next.extend(matches);
            }
            candidates = next;
        } else if segment.contains(['*', '?', '[']) {
            return refuse("only a whole `*` segment is a pattern");
        } else {
            for dir in &mut candidates {
                dir.push(segment.as_ref());
            }
        }
    }

    if candidates.is_empty() {
        return refuse("matches no directory");
    }
    candidates
        .into_iter()
        .map(|dir| {
            let canonical = dir.canonicalize().map_err(|error| {
                WorkspaceError::Member(format!("`{entry}`: {}: {error}", dir.display()))
            })?;
            if !canonical.starts_with(root) {
                return Err(WorkspaceError::Member(format!(
                    "`{entry}`: {} resolves outside the workspace root",
                    dir.display()
                )));
            }
            if !canonical.join(MANIFEST_FILE).is_file() {
                return Err(WorkspaceError::Member(format!(
                    "`{entry}`: {} has no {MANIFEST_FILE}",
                    dir.display()
                )));
            }
            Ok(canonical)
        })
        .collect()
}

/// The nearest directory at or above `start` whose `hew.toml` declares
/// `[workspace]`.
#[must_use]
pub fn find_root(start: &Path) -> Option<PathBuf> {
    start
        .ancestors()
        .find(|dir| {
            std::fs::read_to_string(dir.join(MANIFEST_FILE))
                .ok()
                .and_then(|text| toml::from_str::<toml::Table>(&text).ok())
                .is_some_and(|table| declares_workspace(&table))
        })
        .map(Path::to_path_buf)
}

/// Find the workspace that `package_root` is a member of: the first ancestor
/// whose `hew.toml` declares `[workspace]`, when its members include the
/// package.
///
/// # Errors
///
/// Returns [`WorkspaceError`] when that ancestor's workspace cannot be loaded.
pub fn find_enclosing(package_root: &Path) -> Result<Option<Workspace>, WorkspaceError> {
    let Ok(package_root) = package_root.canonicalize() else {
        return Ok(None);
    };
    let Some(root) = package_root.parent().and_then(find_root) else {
        return Ok(None);
    };
    // Membership needs only the member list; a non-member never depends on
    // whether the members' manifests parse.
    if !member_dirs(&root)?.1.contains(&package_root) {
        return Ok(None);
    }
    load(&root).map(Some)
}

impl Workspace {
    /// The member named `name`, if any.
    #[must_use]
    pub fn member(&self, name: &str) -> Option<&Member> {
        self.members.iter().find(|member| member.name() == name)
    }

    /// Check every requirement `member` declares on another member.
    ///
    /// # Errors
    ///
    /// Returns [`WorkspaceError::Requirement`] for the first requirement that
    /// does not admit the member's version.
    pub fn check_member_requirements(&self, member: &Member) -> Result<(), WorkspaceError> {
        for (dependency, spec) in &member.manifest.dependencies {
            let Some(target) = self.member(dependency) else {
                continue;
            };
            if !is_explicit_source(spec) {
                member_satisfies(
                    member.name(),
                    dependency,
                    spec.version_req(),
                    target.version(),
                )?;
            }
        }
        Ok(())
    }

    /// A workspace of the one package at `dir`, for publishing a package that
    /// belongs to no workspace.
    ///
    /// # Errors
    ///
    /// Returns [`WorkspaceError::Manifest`] when its manifest does not parse.
    pub fn single(dir: &Path) -> Result<Self, WorkspaceError> {
        let dir = dir
            .canonicalize()
            .map_err(|error| WorkspaceError::Manifest {
                path: dir.join(MANIFEST_FILE),
                source: error.into(),
            })?;
        let path = dir.join(MANIFEST_FILE);
        let manifest = manifest::parse_manifest(&path)
            .map_err(|source| WorkspaceError::Manifest { path, source })?;
        Ok(Self {
            root: dir.clone(),
            members: vec![Member { dir, manifest }],
        })
    }

    /// Members in publish order: each after every member it depends on,
    /// choosing the lowest name among the ready members at each step.
    ///
    /// # Errors
    ///
    /// Returns [`WorkspaceError::Cycle`] naming a cycle among the members.
    pub fn publish_order(&self) -> Result<Vec<&Member>, WorkspaceError> {
        let by_name: BTreeMap<&str, &Member> = self
            .members
            .iter()
            .map(|member| (member.name(), member))
            .collect();
        let mut waiting: BTreeMap<&str, BTreeSet<&str>> = self
            .members
            .iter()
            .map(|member| {
                let deps = member
                    .manifest
                    .dependencies
                    .iter()
                    .filter(|(name, spec)| {
                        by_name.contains_key(name.as_str()) && !is_explicit_source(spec)
                    })
                    .map(|(name, _)| name.as_str())
                    .collect();
                (member.name(), deps)
            })
            .collect();
        let mut order = Vec::new();
        while let Some(next) = waiting
            .iter()
            .find(|(_, deps)| deps.is_empty())
            .map(|(name, _)| *name)
        {
            waiting.remove(next);
            for deps in waiting.values_mut() {
                deps.remove(next);
            }
            order.push(by_name[next]);
        }
        if waiting.is_empty() {
            Ok(order)
        } else {
            Err(WorkspaceError::Cycle(find_cycle(&waiting)))
        }
    }
}

/// A dependency with an explicit `path` or `registry` keeps its declared
/// source; only a plain requirement resolves to the workspace member.
pub(crate) fn is_explicit_source(spec: &manifest::DepSpec) -> bool {
    matches!(spec, manifest::DepSpec::Table(table) if table.path.is_some() || table.registry.is_some())
}

/// Check that `requirement`, declared by `member` on the workspace member
/// `dependency`, admits that member's `version`.
///
/// # Errors
///
/// Returns [`WorkspaceError::Requirement`] when it does not, or when either
/// side does not parse.
pub(crate) fn member_satisfies(
    member: &str,
    dependency: &str,
    requirement: &str,
    version: &str,
) -> Result<(), WorkspaceError> {
    let admits = crate::resolver::VersionReq::parse(requirement)
        .ok()
        .zip(semver::Version::parse(version).ok())
        .is_some_and(|(requirement, version)| requirement.matches(&version));
    if admits {
        Ok(())
    } else {
        Err(WorkspaceError::Requirement {
            member: member.to_string(),
            dependency: dependency.to_string(),
            requirement: requirement.to_string(),
            version: version.to_string(),
        })
    }
}

/// Walk the remaining (all blocked) members to one cycle, start repeated at
/// the end.
fn find_cycle(waiting: &BTreeMap<&str, BTreeSet<&str>>) -> Vec<String> {
    let start = *waiting.keys().next().expect("a blocked member remains");
    let mut path = vec![start];
    let mut current = start;
    loop {
        let next = *waiting[current]
            .iter()
            .next()
            .expect("a blocked member waits on another blocked member");
        if let Some(index) = path.iter().position(|name| *name == next) {
            let mut cycle: Vec<String> = path[index..].iter().map(ToString::to_string).collect();
            cycle.push(next.to_string());
            return cycle;
        }
        path.push(next);
        current = next;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn package(root: &Path, dir: &str, name: &str, deps: &str) {
        let dir = root.join(dir);
        std::fs::create_dir_all(&dir).unwrap();
        std::fs::write(
            dir.join(MANIFEST_FILE),
            format!("[package]\nname = \"{name}\"\nversion = \"1.0.0\"\n\n[dependencies]\n{deps}"),
        )
        .unwrap();
    }

    fn workspace(root: &Path, members: &str) {
        std::fs::write(
            root.join(MANIFEST_FILE),
            format!("[workspace]\nmembers = [{members}]\n"),
        )
        .unwrap();
    }

    fn names(order: &[&Member]) -> Vec<String> {
        order
            .iter()
            .map(|member| member.name().to_string())
            .collect()
    }

    #[test]
    fn publish_order_follows_dependencies_then_names() {
        let root = tempfile::tempdir().unwrap();
        package(root.path(), "a", "a", "");
        package(root.path(), "b", "b", "\"a\" = \"1.0.0\"\n");
        package(root.path(), "c", "c", "");
        workspace(root.path(), "\"a\", \"b\", \"c\"");
        let ws = load(root.path()).unwrap();
        assert_eq!(names(&ws.publish_order().unwrap()), ["a", "b", "c"]);

        package(root.path(), "b", "b", "\"c\" = \"1.0.0\"\n");
        let ws = load(root.path()).unwrap();
        assert_eq!(names(&ws.publish_order().unwrap()), ["a", "c", "b"]);
    }

    #[test]
    fn cycle_names_its_members() {
        let root = tempfile::tempdir().unwrap();
        package(root.path(), "a", "a", "\"b\" = \"1.0.0\"\n");
        package(root.path(), "b", "b", "\"a\" = \"1.0.0\"\n");
        workspace(root.path(), "\"a\", \"b\"");
        let error = load(root.path()).unwrap().publish_order().unwrap_err();
        assert_eq!(
            error.to_string(),
            "E_WORKSPACE_CYCLE: workspace members depend on each other in a cycle: a -> b -> a"
        );
    }

    #[test]
    fn star_matches_one_directory_level_and_skips_hidden_directories() {
        let root = tempfile::tempdir().unwrap();
        package(root.path(), "db/sql", "db.sql", "");
        package(root.path(), "db/sqlite", "db.sqlite", "");
        package(root.path(), "db/.hew/packages", "db.cached", "");
        package(root.path(), "db/sql/nested", "db.nested", "");
        package(root.path(), ".hew/packages/x", "x", "");
        workspace(root.path(), "\"db/*\"");
        let ws = load(root.path()).unwrap();
        let names: Vec<_> = ws.members.iter().map(Member::name).collect();
        assert_eq!(names, ["db.sql", "db.sqlite"]);
    }

    #[test]
    fn member_without_manifest_is_named() {
        let root = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(root.path().join("empty")).unwrap();
        workspace(root.path(), "\"empty\"");
        let error = load(root.path()).unwrap_err().to_string();
        assert!(error.starts_with("E_WORKSPACE_MEMBER: `empty`:"), "{error}");
        assert!(error.contains("has no hew.toml"), "{error}");

        workspace(root.path(), "\"missing\"");
        let error = load(root.path()).unwrap_err().to_string();
        assert!(
            error.starts_with("E_WORKSPACE_MEMBER: `missing`:"),
            "{error}"
        );

        package(root.path(), "db/sql", "db.sql", "");
        std::fs::create_dir_all(root.path().join("db/common")).unwrap();
        workspace(root.path(), "\"db/*\"");
        let error = load(root.path()).unwrap_err().to_string();
        assert!(error.contains("common"), "{error}");
    }

    #[test]
    fn malformed_member_entries_are_refused() {
        let root = tempfile::tempdir().unwrap();
        for entry in ["../outside", "/abs", "db/s*", ""] {
            workspace(root.path(), &format!("{entry:?}"));
            let error = load(root.path()).unwrap_err().to_string();
            assert!(error.starts_with("E_WORKSPACE_MEMBER"), "{entry}: {error}");
        }
    }

    #[test]
    #[cfg(unix)]
    fn member_links_stay_inside_the_workspace() {
        use std::os::unix::fs::symlink;

        let root = tempfile::tempdir().unwrap();
        let outside = tempfile::tempdir().unwrap();
        package(root.path(), "inside", "inside", "");
        package(outside.path(), "package", "outside", "");
        symlink(root.path().join("inside"), root.path().join("alias")).unwrap();
        workspace(root.path(), "\"inside\", \"alias\"");
        let ws = load(root.path()).unwrap();
        assert_eq!(ws.members.len(), 1);
        assert_eq!(ws.members[0].name(), "inside");

        std::fs::create_dir(root.path().join("links")).unwrap();
        symlink(
            outside.path().join("package"),
            root.path().join("links/outside"),
        )
        .unwrap();
        for entry in ["links/outside", "links/*"] {
            workspace(root.path(), &format!("{entry:?}"));
            let error = load(root.path()).unwrap_err().to_string();
            assert!(
                error.contains("resolves outside the workspace root"),
                "{error}"
            );
        }
    }

    #[test]
    fn duplicate_names_name_both_paths() {
        let root = tempfile::tempdir().unwrap();
        package(root.path(), "one", "same", "");
        package(root.path(), "two", "same", "");
        workspace(root.path(), "\"one\", \"two\"");
        let error = load(root.path()).unwrap_err().to_string();
        assert!(error.starts_with("E_WORKSPACE_MEMBER: members"), "{error}");
        assert!(error.contains("one") && error.contains("two"), "{error}");
    }

    #[test]
    fn listing_a_member_twice_loads_it_once() {
        let root = tempfile::tempdir().unwrap();
        package(root.path(), "db/sql", "db.sql", "");
        workspace(root.path(), "\"db/*\", \"db/sql\", \"./db/sql\"");
        assert_eq!(load(root.path()).unwrap().members.len(), 1);
    }

    #[test]
    fn sibling_requirement_must_admit_the_member_version() {
        let root = tempfile::tempdir().unwrap();
        package(root.path(), "a", "a", "");
        package(root.path(), "b", "b", "\"a\" = \"0.3.0\"\n");
        workspace(root.path(), "\"a\", \"b\"");
        let ws = load(root.path()).unwrap();
        let error = ws
            .check_member_requirements(ws.member("b").unwrap())
            .unwrap_err();
        assert_eq!(
            error.to_string(),
            "E_WORKSPACE_REQUIREMENT: b requires a `0.3.0`, but the workspace member a is 1.0.0"
        );

        package(root.path(), "b", "b", "\"a\" = \"1.0.0\"\n");
        let ws = load(root.path()).unwrap();
        ws.check_member_requirements(ws.member("b").unwrap())
            .unwrap();
    }

    #[test]
    fn a_non_member_ignores_a_broken_member_manifest() {
        let root = tempfile::tempdir().unwrap();
        package(root.path(), "a", "a", "");
        package(root.path(), "fixture", "fixture", "");
        std::fs::write(root.path().join("a/hew.toml"), "[package\n").unwrap();
        workspace(root.path(), "\"a\"");
        assert!(find_enclosing(&root.path().join("fixture"))
            .unwrap()
            .is_none());
        // A member still sees the broken workspace.
        std::fs::write(
            root.path().join("fixture/hew.toml"),
            "[package]\nname = \"fixture\"\nversion = \"1.0.0\"\n",
        )
        .unwrap();
        workspace(root.path(), "\"a\", \"fixture\"");
        assert!(find_enclosing(&root.path().join("fixture")).is_err());
    }

    #[test]
    fn enclosing_workspace_applies_only_to_members() {
        let root = tempfile::tempdir().unwrap();
        package(root.path(), "a", "a", "");
        package(root.path(), "fixture", "fixture", "");
        workspace(root.path(), "\"a\"");
        let ws = find_enclosing(&root.path().join("a")).unwrap().unwrap();
        assert_eq!(ws.members.len(), 1);
        assert!(find_enclosing(&root.path().join("fixture"))
            .unwrap()
            .is_none());
    }
}
