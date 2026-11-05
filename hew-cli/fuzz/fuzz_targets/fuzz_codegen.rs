//! Arbitrary Hew source through verified physical MIR and LLVM lowering.
#![no_main]

use libfuzzer_sys::fuzz_target;

mod compiler;

fuzz_target!(|data: &[u8]| {
    let Ok(source) = std::str::from_utf8(data) else {
        return;
    };
    compiler::exercise(source, false);
});
