use std::path::{Path, PathBuf};

pub(crate) fn build_policy(
    input: Option<&Path>,
    project_dir: Option<&Path>,
    enabled: bool,
) -> hew_compile::SourcePathPolicy {
    let mut policy = hew_compile::SourcePathPolicy::default();
    if enabled {
        let input = input.unwrap_or_else(|| Path::new("."));
        let root = if let Some(root) = project_dir {
            root.to_path_buf()
        } else if input.is_dir() {
            let dir = input.canonicalize().unwrap_or_else(|_| input.to_path_buf());
            crate::native_link::PackageOwners::default()
                .owner(&dir)
                .unwrap_or(dir)
        } else {
            input
                .parent()
                .unwrap_or_else(|| Path::new("."))
                .to_path_buf()
        };
        policy.insert(&root, Path::new("."));
        for root in hew_types::module_registry::stdlib_search_paths() {
            policy.insert(&root.join("std"), Path::new("std"));
        }
    }
    policy
}

pub(crate) fn register_packages<'a>(paths: impl IntoIterator<Item = &'a Path>) {
    let mut policy = hew_compile::source_path_policy();
    if policy.prefixes().next().is_none() {
        return;
    }
    let mut owners = crate::native_link::PackageOwners::default();
    for path in paths {
        if !path.is_absolute() || policy.display(path) != path {
            continue;
        }
        let dir = if path.is_dir() {
            path
        } else {
            path.parent().unwrap_or(path)
        };
        let Some(root) = owners.owner(dir) else {
            continue;
        };
        let Ok(manifest) = hew_pkg::manifest::parse_manifest(&root.join("hew.toml")) else {
            continue;
        };
        let display = PathBuf::from("packages")
            .join(manifest.package.name)
            .join(manifest.package.version);
        policy.insert(&root, &display);
        hew_compile::register_source_path(&root, &display);
    }
}

pub(crate) fn register_diagnostics(diagnostics: &[hew_compile::FrontendDiagnostic]) {
    let mut paths = Vec::new();
    for diagnostic in diagnostics {
        paths.extend(diagnostic.filename.iter().map(Path::new));
        paths.extend(
            diagnostic
                .note_sources
                .iter()
                .flatten()
                .map(|(_, filename)| Path::new(filename)),
        );
        if let hew_compile::FrontendDiagnosticKind::Message(message) = &diagnostic.kind {
            paths.extend(message.notes.iter().map(|note| Path::new(&note.filename)));
        }
    }
    register_packages(paths);
}
