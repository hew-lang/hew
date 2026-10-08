use std::path::PathBuf;

use hew_parser::Severity;
use hew_types::module_registry::{stdlib_search_paths, ModuleRegistry};
use hew_types::Checker;

/// The compiler's own std root. Without it every `std.*` import fails to
/// resolve and the checker targets reject every input before reaching the
/// code under test, so an empty root stops the run instead.
pub fn module_search_paths() -> Vec<PathBuf> {
    let paths = stdlib_search_paths();
    assert!(
        !paths.is_empty(),
        "fuzz harness found no std root; set HEW_STD to a std/ directory"
    );
    paths
}

pub fn checker() -> Checker {
    Checker::new(ModuleRegistry::new(module_search_paths()))
}

#[allow(dead_code, reason = "shared by every fuzz target; each target uses a subset")]
pub fn parse_check_lower(source: &str) -> Option<hew_compile::SessionOutput> {
    let parsed = hew_parser::parse(source);
    if parsed.errors.iter().any(|e| e.severity == Severity::Error) {
        return None;
    }

    let mut checker = checker();
    let type_check = checker.check_program(&parsed.program);
    if !type_check.errors.is_empty() {
        return None;
    }

    hew_compile::Session::new(
        hew_compile::SessionTarget::native(),
        hew_compile::DiagnosticPolicy::default(),
    )
    .lower_program(&parsed.program, &type_check)
    .ok()
}

#[allow(dead_code, reason = "shared by every fuzz target; each target uses a subset")]
pub fn exercise_parse_check_lower(source: &str) {
    let _ = parse_check_lower(source);
}
