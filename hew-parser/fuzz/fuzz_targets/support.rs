use std::path::PathBuf;

use hew_hir::{lower_program, verify_hir, ResolutionCtx};
use hew_mir::{lower_hir_module, IrPipeline};
use hew_parser::Severity;
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

#[allow(dead_code)]
pub fn parse_check_lower(source: &str) -> Option<IrPipeline> {
    let parsed = hew_parser::parse(source);
    if parsed.errors.iter().any(|e| e.severity == Severity::Error) {
        return None;
    }

    let mut checker = checker();
    let type_check = checker.check_program(&parsed.program);
    if !type_check.errors.is_empty() {
        return None;
    }

    let hir = lower_program(&parsed.program, &type_check, &ResolutionCtx);
    if !hir.diagnostics.is_empty() || !verify_hir(&hir.module).is_empty() {
        return None;
    }

    Some(lower_hir_module(&hir.module))
}

#[allow(dead_code)]
pub fn exercise_parse_check_lower(source: &str) {
    let _ = parse_check_lower(source);
}
