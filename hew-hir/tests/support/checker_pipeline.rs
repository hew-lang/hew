use hew_hir::{lower_program_host_target, LowerOutput, ResolutionCtx};
use hew_parser::ParseResult;
use hew_types::{module_registry::ModuleRegistry, Checker, TypeCheckOutput};

pub fn parse_source(source: &str) -> ParseResult {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    parsed
}

pub fn typecheck_source(source: &str) -> (ParseResult, TypeCheckOutput) {
    let parsed = parse_source(source);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tco = checker.check_program(&parsed.program);
    (parsed, tco)
}

pub fn lower_through_checker(source: &str) -> LowerOutput {
    let (parsed, tco) = typecheck_source(source);
    lower_program_host_target(&parsed.program, &tco, &ResolutionCtx)
}

#[allow(
    dead_code,
    reason = "module-backed gate tests use this helper in follow-up coverage lanes"
)]
pub fn lower_through_checker_with_modules(source: &str) -> LowerOutput {
    let parsed = parse_source(source);
    let repo_root = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-hir crate must live below repo root")
        .to_path_buf();
    let mut checker = Checker::new(ModuleRegistry::new(vec![repo_root]));
    let tco = checker.check_program(&parsed.program);
    lower_program_host_target(&parsed.program, &tco, &ResolutionCtx)
}
