use std::path::PathBuf;

use hew_types::module_registry::{build_module_search_paths, ModuleRegistry};
use hew_types::Checker;

pub fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(|p| p.parent())
        .expect("fuzz crate lives at hew-parser/fuzz")
        .to_path_buf()
}

pub fn module_search_paths() -> Vec<PathBuf> {
    let mut paths = build_module_search_paths();
    let root = repo_root();
    if root.join("std").exists() && !paths.contains(&root) {
        paths.push(root);
    }
    paths
}

pub fn checker() -> Checker {
    Checker::new(ModuleRegistry::new(module_search_paths()))
}
