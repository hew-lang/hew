//! Stage 2 (gdb `-g`) — the per-instruction source-span side-table
//! ([`RawMirFunction::instr_spans`]) must attribute a plain function's
//! backend `Instr` / terminator stream to its *originating statement* line,
//! not to a single coarse function line. This is the MIR-side fact codegen
//! threads into the DWARF line table so gdb steps line-by-line; a regression
//! that collapsed every instruction onto one span would silently break
//! stepping while still emitting a (wrong) `-g` binary, so pin it here at the
//! MIR boundary where it is cheap to assert.

use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, IrPipeline, RawMirFunction};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

/// Parse → typecheck → HIR-lower → raw/elaborated MIR.
fn pipeline_with_tc(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "parse errors: {:#?}", parsed.errors);
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let output = lower_program(
        &parsed.program,
        &tc_output,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    lower_hir_module(&output.module)
}

fn raw_fn<'a>(p: &'a IrPipeline, name: &str) -> &'a RawMirFunction {
    p.raw_mir
        .iter()
        .find(|f| f.name == name)
        .expect("function must be present in raw_mir")
}

/// 1-based line number containing byte `offset` of `source`.
fn line_of(source: &str, offset: usize) -> usize {
    source[..offset.min(source.len())]
        .bytes()
        .filter(|&b| b == b'\n')
        .count()
        + 1
}

#[test]
fn instr_spans_attribute_each_statement_to_its_own_line() {
    // Each statement on its own line — the side-table must spread the function's
    // instructions/terminator across distinct source lines.
    let source = "\
fn add(a: i64, b: i64) -> i64 {
    let sum = a + b;
    let doubled = sum + sum;
    doubled
}
";
    let pipeline = pipeline_with_tc(source);
    let f = raw_fn(&pipeline, "add");

    assert!(
        !f.instr_spans.is_empty(),
        "the side-table must be populated for a plain fn lowered from source"
    );

    // Every recorded span is a well-formed in-file byte range.
    for &(start, end) in f.instr_spans.values() {
        assert!(
            usize::try_from(start).unwrap() <= source.len(),
            "span start {start} must lie within the source ({} bytes)",
            source.len()
        );
        assert!(start <= end, "span start {start} must be <= end {end}");
    }

    let lines: std::collections::BTreeSet<usize> = f
        .instr_spans
        .values()
        .map(|&(start, _)| line_of(source, usize::try_from(start).unwrap()))
        .collect();

    // `let sum` (line 2), `let doubled` (line 3) and the `doubled` tail (line 4)
    // must each contribute their own line — per-statement granularity, the whole
    // point of Stage 2. A coarse single-line table would fail this.
    assert!(
        lines.contains(&2) && lines.contains(&3) && lines.contains(&4),
        "side-table must attribute instructions to lines 2, 3 and 4; got {lines:?}"
    );
}

#[test]
fn instr_spans_locate_a_call_statement_terminator() {
    // A statement that lowers to a `Terminator::Call` (`println(r)`) must still
    // be located: its span is recorded on the block terminator, keyed one slot
    // past the last instruction. Without that, the call line is missing from the
    // table and gdb cannot stop on it.
    let source = "\
fn add(a: i64, b: i64) -> i64 {
    a + b
}
fn main() {
    let x = 10;
    let y = 32;
    let r = add(x, y);
    println(r);
}
";
    let pipeline = pipeline_with_tc(source);
    let f = raw_fn(&pipeline, "main");

    let lines: std::collections::BTreeSet<usize> = f
        .instr_spans
        .values()
        .map(|&(start, _)| line_of(source, usize::try_from(start).unwrap()))
        .collect();

    // `let x` (5), `let y` (6), `let r = add(...)` (7, a call) and `println(r)`
    // (8, also a call) — the two call statements prove terminator spans land.
    assert!(
        lines.contains(&7) && lines.contains(&8),
        "call-statement lines 7 (add) and 8 (println) must appear; got {lines:?}"
    );
}
