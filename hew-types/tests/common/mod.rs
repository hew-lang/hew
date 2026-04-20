#![allow(
    dead_code,
    reason = "shared integration-test helpers are not used by every test target"
)]

use std::path::{Path, PathBuf};

use hew_types::error::{TypeError, TypeErrorKind};
use hew_types::module_registry::ModuleRegistry;
use hew_types::{Checker, TypeCheckOutput};

pub fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("hew-types crate should live under the repo root")
        .to_path_buf()
}

pub fn checker() -> Checker {
    Checker::new(ModuleRegistry::new(vec![repo_root()]))
}

pub fn isolated_checker() -> Checker {
    Checker::new(ModuleRegistry::new(vec![]))
}

pub fn parse_program(source: &str) -> hew_parser::ast::Program {
    let parse_result = hew_parser::parse(source);
    assert!(
        parse_result.errors.is_empty(),
        "should parse cleanly, got: {:#?}",
        parse_result.errors
    );
    parse_result.program
}

pub fn typecheck(source: &str) -> TypeCheckOutput {
    let program = parse_program(source);
    let mut checker = checker();
    checker.check_program(&program)
}

pub fn typecheck_isolated(source: &str) -> TypeCheckOutput {
    let program = parse_program(source);
    let mut checker = isolated_checker();
    checker.check_program(&program)
}

pub fn parse_and_typecheck_inline(source: &str) -> (hew_parser::ast::Program, TypeCheckOutput) {
    let program = parse_program(source);
    let mut checker = checker();
    let output = checker.check_program(&program);
    (program, output)
}

pub fn parse_and_typecheck_isolated(source: &str) -> (hew_parser::ast::Program, TypeCheckOutput) {
    let program = parse_program(source);
    let mut checker = isolated_checker();
    let output = checker.check_program(&program);
    (program, output)
}

pub fn typecheck_wasm(source: &str) -> TypeCheckOutput {
    let program = parse_program(source);
    let mut checker = checker();
    checker.enable_wasm_target();
    checker.check_program(&program)
}

pub fn warnings_of_kind<'a>(
    output: &'a TypeCheckOutput,
    kind: &TypeErrorKind,
) -> Vec<&'a TypeError> {
    output
        .warnings
        .iter()
        .filter(|warning| &warning.kind == kind)
        .collect()
}
