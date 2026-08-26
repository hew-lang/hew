//! Fresh `VecIter` temporary ownership across a trapping closure call.
//!
//! The caller owns the snapshot produced by `values.iter()`. A normal return
//! releases it immediately after the call; a panic must instead reach the
//! call-site-specific LLVM unwind edge and run the same guarded release once.

#![cfg(unix)]

mod support;

use std::collections::{HashMap, HashSet, VecDeque};
use support::leak_slope::{
    compile_to_native_with_ir, measure_leaks_exact, require_leaks_tool, run_under_malloc_scribble,
};
use support::{describe_output, require_codegen};

const TRAPPING_VEC_ITER_CLOSURE: &str = r#"
actor VecIterCrasher {
    receive fn boom() {
        let root = Rc.new(41);
        let values: Vec<Rc<i64>> = Vec.new();
        values.push(root);

        let crash = |incoming: VecIter<Rc<i64>>| {
            let _ = incoming;
            panic("vec-iter-closure-trap");
            let _ = 0;
        };
        crash(values.iter());
        // The call-site unwind state owns `values`; the later suspend state
        // owns `moved`. Selecting the block's Suspend plan for the invoke
        // would release an uninitialized destination after the closure traps.
        let moved = values;
        sleep(1ms);
        let _ = moved.len();
    }
}

actor VecIterProbe {
    receive fn ping() -> i64 { 0 }
}

fn main() -> i64 {
    let crasher = spawn VecIterCrasher;
    let probe = spawn VecIterProbe;
    crasher.boom();
    sleep(300ms);
    match await probe.ping() {
        Ok(value) => value,
        Err(_) => 1,
    }
}
"#;

fn llvm_block_label(line: &str) -> Option<&str> {
    if line.starts_with(char::is_whitespace) {
        return None;
    }
    let (label, _) = line.split_once(':')?;
    (!label.contains(char::is_whitespace)).then_some(label.trim_matches('"'))
}

fn llvm_label_targets(block: &str) -> Vec<String> {
    block
        .split("label %")
        .skip(1)
        .filter_map(|tail| {
            tail.split(|ch: char| ch.is_whitespace() || matches!(ch, ',' | ']' | ')'))
                .next()
                .map(|label| label.trim_matches('"').to_string())
        })
        .collect()
}

fn assert_closure_unwind_reaches_vec_iter_release(ir: &str) {
    let invoke_offset = ir
        .find("closure_call_result = invoke ")
        .unwrap_or_else(|| panic!("closure call is not an LLVM invoke:\n{ir}"));
    let function_start = ir[..invoke_offset].rfind("\ndefine ").unwrap_or(0);
    let function_tail = &ir[function_start..];
    let function_end = function_tail
        .find("\n}")
        .unwrap_or_else(|| panic!("closure caller has no closing brace:\n{function_tail}"));
    let function = &function_tail[..function_end];
    let unwind_label = function[invoke_offset - function_start..]
        .split("unwind label %")
        .nth(1)
        .and_then(|tail| tail.split_whitespace().next())
        .map_or_else(
            || panic!("closure invoke has no unwind label:\n{function}"),
            |label| label.trim_matches('"'),
        );

    let mut blocks: HashMap<String, String> = HashMap::new();
    let mut current = None;
    for line in function.lines() {
        if let Some(label) = llvm_block_label(line) {
            current = Some(label.to_string());
            blocks.entry(label.to_string()).or_default();
        } else if let Some(label) = current.as_ref() {
            let block = blocks.get_mut(label).expect("current LLVM block exists");
            block.push_str(line);
            block.push('\n');
        }
    }

    let mut pending = VecDeque::from([unwind_label.to_string()]);
    let mut visited = HashSet::new();
    let mut saw_landingpad = false;
    let mut saw_vec_iter_release = false;
    let mut saw_resume = false;
    while let Some(label) = pending.pop_front() {
        if !visited.insert(label.clone()) {
            continue;
        }
        let block = blocks
            .get(&label)
            .unwrap_or_else(|| panic!("cleanup references absent LLVM block %{label}"));
        saw_landingpad |= label == unwind_label && block.contains("landingpad");
        saw_vec_iter_release |= block.contains("@hew_vec_free_owned");
        saw_resume |= block
            .lines()
            .any(|line| line.trim_start().starts_with("resume "));
        pending.extend(llvm_label_targets(block));
    }
    assert!(
        saw_landingpad && saw_vec_iter_release && saw_resume,
        "closure unwind region %{unwind_label} lacks landingpad, typed VecIter release, or resume; reachable blocks: {visited:?}"
    );
}

#[test]
#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leaks(1) and malloc poisoning are macOS-only"
)]
fn temporary_vec_iter_closure_trap_invokes_guarded_cleanup_without_leak() {
    require_codegen();
    require_leaks_tool();

    let temp = tempfile::tempdir().expect("create VecIter closure unwind oracle dir");
    let (bin, ir_path) = compile_to_native_with_ir(
        TRAPPING_VEC_ITER_CLOSURE,
        temp.path(),
        "vec_iter_closure_unwind",
    );
    let ir = std::fs::read_to_string(&ir_path).expect("read generated LLVM IR");
    assert_closure_unwind_reaches_vec_iter_release(&ir);

    let witness = run_under_malloc_scribble(&bin);
    assert_eq!(
        witness.status.code(),
        Some(1),
        "actor-isolated trapping closure must reach the runtime's ordinary crash exit, not a memory-safety signal:\n{}",
        describe_output(&witness)
    );
    assert!(
        String::from_utf8_lossy(&witness.stderr).contains("vec-iter-closure-trap"),
        "trap witness did not execute the closure body:\n{}",
        describe_output(&witness)
    );

    assert_eq!(
        measure_leaks_exact(&bin),
        (0, 0),
        "fresh VecIter snapshot leaked or retained stale cleanup authority on closure unwind"
    );
}
