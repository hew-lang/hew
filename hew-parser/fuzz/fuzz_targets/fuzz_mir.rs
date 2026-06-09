#![no_main]

mod support;

use hew_hir::{lower_program_host_target, verify_hir, ResolutionCtx};
use hew_mir::lower_hir_module;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(source) = std::str::from_utf8(data) {
        let parsed = hew_parser::parse(source);
        if parsed
            .errors
            .iter()
            .any(|e| e.severity == hew_parser::Severity::Error)
        {
            return;
        }

        let mut checker = support::checker();
        let type_check = checker.check_program(&parsed.program);
        if !type_check.errors.is_empty() {
            return;
        }

        let hir = lower_program_host_target(&parsed.program, &type_check, &ResolutionCtx);
        if !hir.diagnostics.is_empty() || !verify_hir(&hir.module).is_empty() {
            return;
        }

        let _ = lower_hir_module(&hir.module);
    }
});
