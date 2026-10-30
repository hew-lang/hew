#![no_main]

//! Grows and minimises the parity oracle's corpus. Each input renders through
//! the parity generator, which must always produce a program the frontend
//! accepts; compile and execution stay in `scripts/fuzz/run-parity.py`.

mod support;

use arbitrary::Unstructured;
use hew_parser::Severity;
use hew_parser_fuzz::parity;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let program =
        parity::generate(&mut Unstructured::new(data), None).expect("generation never fails");
    let parsed = hew_parser::parse(&program.source);
    let errors: Vec<_> = parsed
        .errors
        .iter()
        .filter(|error| error.severity == Severity::Error)
        .collect();
    assert!(
        errors.is_empty(),
        "generated program does not parse: {errors:?}\n{}",
        program.source
    );
    let mut checker = support::checker();
    let checked = checker.check_program(&parsed.program);
    assert!(
        checked.errors.is_empty(),
        "generated program does not check: {:?}\n{}",
        checked.errors,
        program.source
    );
});
