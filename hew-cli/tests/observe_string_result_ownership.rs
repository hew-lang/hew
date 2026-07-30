//! Compiled ownership regression for transferred observe snapshots.
//!
//! The four positions below deliberately exercise the bespoke runtime-ABI
//! observe lowering: a discarded result, a temporary borrowed by `.len()`, a
//! named scope owner, and a tail return. The MIR canary pins the exact release
//! placement; this test proves the emitted native program actually runs those
//! paths and the Darwin oracle guards their per-iteration allocation slope.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance_exact_lines, compile_to_native, run_probe_witness,
};
use support::require_codegen;

fn observe_result_source(frames: usize) -> String {
    format!(
        "import std::observe;\n\
         fn discarded() {{\n\
         \x20   observe.scrape();\n\
         \x20   observe.series();\n\
         }}\n\
         fn nested() -> i64 {{\n\
         \x20   observe.scrape().len() + observe.series().len()\n\
         }}\n\
         fn bound() -> i64 {{\n\
         \x20   let scrape = observe.scrape();\n\
         \x20   let series = observe.series();\n\
         \x20   scrape.len() + series.len()\n\
         }}\n\
         fn tail_return() -> string {{\n\
         \x20   observe.scrape()\n\
         }}\n\
         fn main() {{\n\
         \x20   for _ in 0..{frames} {{\n\
         \x20       discarded();\n\
         \x20       let total = nested() + bound() + tail_return().len();\n\
         \x20       println(total);\n\
         \x20   }}\n\
         }}\n"
    )
}

#[test]
fn compiled_observe_string_results_run_in_every_ownership_position() {
    require_codegen();
    let dir = support::tempdir();
    let bin = compile_to_native(
        &observe_result_source(2),
        dir.path(),
        "observe_string_result_ownership",
    );
    assert_eq!(
        run_probe_witness(&bin, &[]),
        2,
        "the compiled probe must run every ownership position twice"
    );
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn compiled_observe_string_results_have_no_per_iteration_leak() {
    assert_frame_slope_below_tolerance_exact_lines(
        "observe_string_results",
        observe_result_source,
        std::convert::identity,
    );
}
