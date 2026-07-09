//! IR-pinned regression for #2437: a TaskEntry adapter's cancel-exit edge for
//! a composite (`record`/array) return type must synthesize a well-defined
//! zero value, never load the unstored `return_slot` alloca.
//!
//! `emit_cancel_trap_or_return` (hew-codegen-rs/src/llvm.rs) has four arms
//! over `fn_ctx.return_ty`. Three (`IntType`/`PointerType`/`FloatType`)
//! synthesize a constant directly; the `StructType`/`ArrayType` arm used to
//! diverge into `build_load(return_slot)` — a read of genuinely uninitialized
//! stack memory on the FunctionEntry/LoopBackEdge cancel path, since nothing
//! stores into `return_slot` before that check runs. The fix makes the
//! composite arm match its three siblings.
//!
//! Compiles REAL `.hew` source through the full frontend pipeline (parser →
//! checker → HIR → MIR), not hand-built MIR: the TaskEntry adapter and its
//! cooperate sites are synthesized by `hew-mir`'s
//! `synthesize_task_entry_adapter` + `compute_cooperate_sites`, which a
//! hand-built `IrPipeline` would have to reproduce by hand and risk not
//! exercising faithfully.

use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::lower_hir_module;
use hew_types::{module_registry::ModuleRegistry, Checker};

fn pipeline_from_source(source: &str) -> hew_mir::IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
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

fn emit_ll_text(pipeline: &hew_mir::IrPipeline, module_name: &str) -> String {
    let tmp = std::env::temp_dir().join(format!("hew-task-entry-cancel-composite-{module_name}"));
    std::fs::create_dir_all(&tmp).expect("create scratch dir");
    let options = EmitOptions {
        module_name,
        out_dir: &tmp,
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    let artefacts = emit_module(pipeline, &options).expect("emit_module must succeed");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

/// `fork t = <fn>()` over a composite-returning function, awaited inside a
/// `scope { }` in a suspend-context-carrying actor receive handler — the
/// shape that synthesizes a `__hew_task_entry_<fn>` TaskEntry adapter with a
/// FunctionEntry cooperate site (`hew-mir/src/dataflow.rs`'s
/// `compute_cooperate_sites`: any non-leaf function's bb0 gets one
/// unconditionally).
const COMPOSITE_FORK_SOURCE: &str = r#"
record Point {
    x: i64,
    y: i64,
}

fn compute() -> Point {
    return Point { x: 11, y: 22 };
}

actor Driver {
    receive fn run_value() -> Point {
        var out: Point = Point { x: 0, y: 0 };
        scope {
            fork t = compute();
            let v = await t;
            out = v;
        }
        return out;
    }
}

fn main() {
    let d = spawn Driver;
    let r = await d.run_value();
    match r {
        Ok(p) => println(f"x={p.x} y={p.y}"),
        Err(_) => println("failed"),
    }
}
"#;

/// Slice the IR text spanning the named block's label through the next `ret`
/// (inclusive) — the cancel-exit block is a single-purpose "synthesize +
/// return" block with no other terminator shape.
fn block_through_next_ret<'a>(ll: &'a str, label: &str) -> &'a str {
    let start = ll
        .find(&format!("{label}:"))
        .unwrap_or_else(|| panic!("no `{label}:` block; IR:\n{ll}"));
    let rest = &ll[start..];
    let ret_end = rest
        .find("\n  ret ")
        .map(|offset| offset + "\n  ret ".len())
        .unwrap_or_else(|| panic!("no `ret` in `{label}:` block; IR:\n{rest}"));
    let line_end = rest[ret_end..]
        .find('\n')
        .map(|offset| ret_end + offset)
        .unwrap_or(rest.len());
    &rest[..line_end]
}

#[test]
fn task_entry_composite_cancel_exit_never_loads_return_slot() {
    let ll = emit_ll_text(
        &pipeline_from_source(COMPOSITE_FORK_SOURCE),
        "composite_fork",
    );

    let adapter_define = ll
        .lines()
        .find(|l| {
            l.trim_start().starts_with("define")
                && l.contains("__hew_task_entry_")
                && l.contains("compute")
        })
        .unwrap_or_else(|| panic!("no `__hew_task_entry_*compute*` adapter definition; IR:\n{ll}"));
    let adapter_start = ll.find(adapter_define).expect("adapter define offset");
    let adapter_end = ll[adapter_start..]
        .find("\n}\n")
        .map(|offset| adapter_start + offset)
        .unwrap_or(ll.len());
    let adapter_ir = &ll[adapter_start..adapter_end];

    assert!(
        adapter_ir.contains("cancel_exit"),
        "TaskEntry adapter must have a cancel_exit block (FunctionEntry cooperate site on bb0);\n--- adapter IR ---\n{adapter_ir}"
    );

    let cancel_exit_block = block_through_next_ret(adapter_ir, "cancel_exit");

    assert!(
        !cancel_exit_block.contains("return_slot"),
        "the cancel_exit block must never reference return_slot — the composite \
         arm must synthesize a zero value directly, not load the unstored \
         alloca (#2437);\n--- cancel_exit block ---\n{cancel_exit_block}"
    );
    assert!(
        !cancel_exit_block.to_lowercase().contains("load"),
        "the cancel_exit block must not contain a load at all on the fixed \
         path;\n--- cancel_exit block ---\n{cancel_exit_block}"
    );
    assert!(
        cancel_exit_block.contains("ret {")
            || cancel_exit_block.contains("ret %")
            || cancel_exit_block.contains("zeroinitializer"),
        "the cancel_exit block must return a well-defined composite value \
         (LLVM's zeroinitializer for a const_zero() struct);\n--- cancel_exit block ---\n{cancel_exit_block}"
    );
}
