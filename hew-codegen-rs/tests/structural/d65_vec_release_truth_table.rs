use hew_codegen_rs::{emit_module, EmitOptions};
use hew_hir::{lower_program, ResolutionCtx};
use hew_mir::{lower_hir_module, IrPipeline};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

fn pipeline(source: &str) -> IrPipeline {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:#?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let checked = checker.check_program(&parsed.program);
    assert!(
        checked.errors.is_empty(),
        "type errors: {:#?}",
        checked.errors
    );
    let hir = lower_program(
        &parsed.program,
        &checked,
        &ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    let pipeline = lower_hir_module(&hir.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "MIR diagnostics: {:#?}",
        pipeline.diagnostics
    );
    pipeline
}

fn emit_ir(source: &str, name: &str) -> String {
    let dir = tempfile::Builder::new()
        .prefix(&format!("d65-{name}-"))
        .tempdir()
        .expect("tempdir");
    let options = EmitOptions {
        module_name: name,
        out_dir: dir.path(),
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    let artifacts = emit_module(&pipeline(source), &options).expect("emit D65 shape");
    std::fs::read_to_string(artifacts.ll_path.expect("LLVM IR path")).expect("read LLVM IR")
}

fn function_body<'a>(ir: &'a str, symbol: &str) -> &'a str {
    let needle = format!("@{symbol}(");
    let mut offset = 0;
    let start = ir
        .split_inclusive('\n')
        .find_map(|line| {
            let line_start = offset;
            offset += line.len();
            (line.starts_with("define ") && line.contains(&needle)).then_some(line_start)
        })
        .unwrap_or_else(|| panic!("missing definition for `{symbol}` in IR:\n{ir}"));

    let mut end = start;
    for line in ir[start..].split_inclusive('\n') {
        end += line.len();
        if line.trim() == "}" {
            return &ir[start..end];
        }
    }
    panic!("missing closing brace for `{symbol}` in IR:\n{ir}");
}

fn vec_release_calls(body: &str) -> usize {
    body.matches("call void @hew_vec_free(").count()
        + body.matches("call void @hew_vec_free_owned(").count()
}

/// Collect the i64 constants that can reach `slot`, following one level of
/// `load`/`store` copy chain. `None` means the pattern was not recognised.
fn i64_slot_constants(body: &str, slot: &str) -> Option<Vec<i64>> {
    let store_prefix = format!(", ptr {slot},");
    let mut constants = Vec::new();
    let mut recognised = false;
    for line in body.lines() {
        let line = line.trim();
        let Some(rest) = line.strip_prefix("store i64 ") else {
            continue;
        };
        if !rest.contains(&store_prefix) {
            continue;
        }
        let value = rest.split(',').next()?.trim();
        recognised = true;
        if let Ok(literal) = value.parse::<i64>() {
            constants.push(literal);
        } else {
            // `store i64 %v, ptr slot` — resolve `%v` back to its source slot.
            let source = ssa_def(body, value)?
                .strip_prefix("load i64, ptr ")?
                .split(',')
                .next()?
                .trim()
                .to_string();
            constants.extend(i64_slot_constants(body, &source)?);
        }
    }
    recognised.then_some(constants)
}

/// Right-hand side of the SSA definition of `name`, e.g. `%c = icmp ne i8 %d, 0`.
fn ssa_def<'a>(body: &'a str, name: &str) -> Option<&'a str> {
    let prefix = format!("{name} = ");
    body.lines()
        .map(str::trim)
        .find_map(|line| line.strip_prefix(prefix.as_str()))
}

/// The constants that can reach the drop flag guarding the cursor release, or
/// `None` when the guard shape is not the expected conditional-drop-flag form.
///
/// Deliberately fail-closed: an unrecognised shape returns `None` and the
/// caller treats that as a failure, because a release whose guard cannot be
/// resolved has not been shown to be dead.
fn cursor_release_guard_flag_constants(body: &str) -> Option<Vec<i64>> {
    // The block that performs the release.
    let mut release_label = None;
    let mut current: Option<String> = None;
    for line in body.lines() {
        if !line.starts_with(char::is_whitespace) {
            if let Some(label) = line.split(':').next() {
                if !label.is_empty() && !line.starts_with("define") && !line.starts_with('}') {
                    current = Some(label.to_string());
                }
            }
        }
        if line.contains("call void @hew_vec_free") {
            release_label.clone_from(&current);
        }
    }
    let release_label = release_label?;

    // The conditional branch that selects it.
    let cond = body.lines().map(str::trim).find_map(|line| {
        let rest = line.strip_prefix("br i1 ")?;
        let (cond, targets) = rest.split_once(',')?;
        targets
            .contains(&format!("label %{release_label}"))
            .then(|| cond.trim().to_string())
    })?;

    // `%cond = icmp ne i8 %flag_byte, 0` ← `load i8, ptr %byte_slot`
    let byte = ssa_def(body, &cond)?
        .strip_prefix("icmp ne i8 ")?
        .split(',')
        .next()?
        .trim()
        .to_string();
    let byte_slot = ssa_def(body, &byte)?
        .strip_prefix("load i8, ptr ")?
        .split(',')
        .next()?
        .trim()
        .to_string();

    // `store i8 %zext, ptr %byte_slot` ← `zext i1 %cmp` ← `icmp eq i64 %flag, %zero`
    let zext = body.lines().map(str::trim).find_map(|line| {
        let rest = line.strip_prefix("store i8 ")?;
        rest.contains(&format!(", ptr {byte_slot},"))
            .then(|| rest.split(',').next().map(|v| v.trim().to_string()))?
    })?;
    let cmp = ssa_def(body, &zext)?
        .strip_prefix("zext i1 ")?
        .split_whitespace()
        .next()?
        .to_string();
    let operands = ssa_def(body, &cmp)?.strip_prefix("icmp eq i64 ")?;
    let (lhs, _) = operands.split_once(',')?;
    let flag_slot = ssa_def(body, lhs.trim())?
        .strip_prefix("load i64, ptr ")?
        .split(',')
        .next()?
        .trim()
        .to_string();

    i64_slot_constants(body, &flag_slot)
}

/// The iteration cursor over an actor-state Vec must never release that Vec:
/// `__hew_state_drop_Holder` is its sole owner, so a handler-side release would
/// free the state on every message and double-free at teardown.
///
/// Zero emitted releases is the strongest form. Under the conditional
/// drop-flag lowering the release site may still be EMITTED while being gated
/// by a flag proven constant, so a surviving site is accepted only when its
/// guard flag can never take the release value (`0`). An unrecognised guard is
/// a failure, not a pass — a release that cannot be shown dead is live.
fn assert_iteration_cursor_borrows_actor_state(name: &str, handler: &str) {
    if vec_release_calls(handler) == 0 {
        return;
    }
    let constants = cursor_release_guard_flag_constants(handler).unwrap_or_else(|| {
        panic!(
            "{name}: the handler releases a Vec and the release is not a resolvable \
             drop-flag-gated cursor release, so it is not proven dead\n{handler}"
        )
    });
    assert!(
        !constants.is_empty() && !constants.contains(&0),
        "{name}: the iteration cursor must borrow actor state — its release guard flag can \
         reach {constants:?}, and `0` takes the release branch that frees the actor's own \
         state Vec\n{handler}"
    );
}

fn vec_release_owner_slots(body: &str) -> std::collections::BTreeSet<&str> {
    body.lines()
        .filter(|line| line.contains("\"hew_vec_free") && line.contains("= load ptr, ptr "))
        .filter_map(|line| {
            let (_, rest) = line.split_once("= load ptr, ptr ")?;
            rest.split_once(',').map(|(slot, _)| slot.trim())
        })
        .collect()
}

fn assert_exact_owner_slot_count(
    name: &str,
    owner_slots: &std::collections::BTreeSet<&str>,
    expected: usize,
) {
    assert_eq!(
        owner_slots.len(),
        expected,
        "{name}: expected exactly {expected} Vec owner release slot(s), got \
         {owner_slots:?}"
    );
}

/// Pin the invariant that makes a released yield safe: a loop that RELEASES its
/// yielded element must have obtained that element from the `hew_vec_get_clone`
/// clone-out, never as an alias into the buffer the source Vec still owns.
///
/// `source_vec_bindings` is the number of Vec bindings the shape declares, so
/// any owner slot beyond them is the per-iteration element release. When the
/// element type owns heap the yield is deep-cloned, the release balances that
/// clone, and the count legitimately exceeds the binding count — that is why
/// `local_nested_full` carries three slots (`rows`, the push-neutralised `row`,
/// and the cloned `current`) while `local_nested_partial`, whose `rows[0]`
/// elements are `i64`, carries two.
///
/// Releasing an ALIASED yield instead would free the element twice: once per
/// iteration and again when the source Vec's recursive teardown runs. The
/// `vec_iter_yield_is_fresh_owner` admission gate used to prevent that pairing
/// and was retired in favour of universal clone-out, so this is the structural
/// guard that the premise still holds.
fn assert_yield_release_paired_with_clone_out(
    name: &str,
    main_body: &str,
    source_vec_bindings: usize,
) {
    let owner_slots = vec_release_owner_slots(main_body).len();
    if owner_slots <= source_vec_bindings {
        return;
    }
    assert!(
        main_body.contains("@hew_vec_get_clone("),
        "{name}: {owner_slots} owner release slots exceed the {source_vec_bindings} source Vec \
         binding(s), so the loop releases its yielded element — but no `hew_vec_get_clone` \
         clone-out produced it. Releasing an aliased yield double-frees the element the source \
         Vec still owns.\n{main_body}"
    );
}

fn exact_count_rejects_mutations(observed: usize, expected: usize) -> bool {
    observed + 1 != expected && observed.saturating_sub(1) != expected
}

fn d65_shape_exact_count(name: &str) -> (usize, usize) {
    let (source, symbol, expected, owner_slots) = match name {
        "local_flat_full" => (LOCAL_FLAT_FULL.to_string(), "main", 1, true),
        "local_flat_partial" => (LOCAL_FLAT_PARTIAL.to_string(), "main", 1, true),
        "local_nested_full" => (LOCAL_NESTED_FULL.to_string(), "main", 3, true),
        "local_nested_partial" => (LOCAL_NESTED_PARTIAL.to_string(), "main", 2, true),
        "state_flat_full" => (
            state_source(
                "Vec<i64>",
                "var total: i64 = 0; for value in values { total = total + value; } total",
                "let values: Vec<i64> = Vec::new(); values.push(1); values.push(2);",
            ),
            "__hew_state_drop_Holder",
            1,
            false,
        ),
        "state_flat_partial" => (
            state_source(
                "Vec<i64>",
                "for value in values { if value == 1 { break; } } 0",
                "let values: Vec<i64> = Vec::new(); values.push(1); values.push(2);",
            ),
            "__hew_state_drop_Holder",
            1,
            false,
        ),
        "state_nested_full" => (
            state_source(
                "Vec<Vec<i64>>",
                "var total: i64 = 0; for row in values { total = total + row[0]; } total",
                "let values: Vec<Vec<i64>> = Vec::new(); let row: Vec<i64> = Vec::new(); \
                 row.push(1); values.push(row);",
            ),
            "__hew_state_drop_Holder",
            1,
            false,
        ),
        "state_nested_partial" => (
            state_source(
                "Vec<Vec<i64>>",
                "for value in values[0] { if value == 1 { break; } } 0",
                "let values: Vec<Vec<i64>> = Vec::new(); let row: Vec<i64> = Vec::new(); \
                 row.push(1); row.push(2); values.push(row);",
            ),
            "__hew_state_drop_Holder",
            1,
            false,
        ),
        other => panic!("unknown D65 shape: {other}"),
    };
    let ir = emit_ir(&source, name);
    let body = function_body(&ir, symbol);
    let observed = if owner_slots {
        vec_release_owner_slots(body).len()
    } else {
        vec_release_calls(body)
    };
    (observed, expected)
}

fn enforce_exact_release_count(name: &str, observed: usize, expected: usize) {
    if observed > expected {
        eprintln!("{name}: injected extra release: observed {observed}, expected {expected}");
        std::process::abort();
    }
    assert_eq!(
        observed, expected,
        "{name}: suppressed release: observed {observed}, expected {expected}"
    );
}

const LOCAL_FLAT_FULL: &str = r#"
fn main() -> i64 {
    let values: Vec<i64> = Vec::new();
    values.push(1);
    values.push(2);
    var total: i64 = 0;
    for value in values {
        total = total + value;
    }
    total
}
"#;

const LOCAL_FLAT_PARTIAL: &str = r#"
fn main() -> i64 {
    let values: Vec<i64> = Vec::new();
    values.push(1);
    values.push(2);
    for value in values {
        if value == 1 {
            break;
        }
    }
    0
}
"#;

const LOCAL_NESTED_FULL: &str = r#"
fn main() -> i64 {
    let rows: Vec<Vec<i64>> = Vec::new();
    let row: Vec<i64> = Vec::new();
    row.push(1);
    rows.push(row);
    var total: i64 = 0;
    for current in rows {
        total = total + current[0];
    }
    total
}
"#;

const LOCAL_NESTED_PARTIAL: &str = r#"
fn main() -> i64 {
    let rows: Vec<Vec<i64>> = Vec::new();
    let row: Vec<i64> = Vec::new();
    row.push(1);
    row.push(2);
    rows.push(row);
    for value in rows[0] {
        if value == 1 {
            break;
        }
    }
    0
}
"#;

fn state_source(field_ty: &str, body: &str, init: &str) -> String {
    format!(
        r#"
actor Holder {{
    var values: {field_ty};

    receive fn scan() -> i64 {{
        {body}
    }}
}}

fn main() -> i64 {{
    {init}
    let holder = spawn Holder(values: values);
    0
}}
"#
    )
}

#[test]
fn d65_function_body_handles_windows_line_endings() {
    let valid = concat!(
        "define internal i64 @Holder__recv__scan(ptr %0, i32 %1) {\r\n",
        "entry:\r\n",
        "  ret i64 0\r\n",
        "}\r\n",
        "\r\n",
        "define void @__hew_state_drop_Holder(ptr %0) {\r\n",
        "entry:\r\n",
        "  call void @hew_vec_free_owned(ptr %0)\r\n",
        "  ret void\r\n",
        "}\r\n",
    );
    let handler = function_body(valid, "Holder__recv__scan");
    let state_drop = function_body(valid, "__hew_state_drop_Holder");
    assert_iteration_cursor_borrows_actor_state("windows_crlf", handler);
    assert_eq!(vec_release_calls(state_drop), 1);
    assert!(!handler.contains("__hew_state_drop_Holder"));

    let broken = valid.replacen(
        "entry:\r\n  ret i64 0",
        "entry:\r\n  call void @hew_vec_free_owned(ptr %0)\r\n  ret i64 0",
        1,
    );
    assert!(
        std::panic::catch_unwind(|| {
            let handler = function_body(&broken, "Holder__recv__scan");
            assert_iteration_cursor_borrows_actor_state("injected_double_release", handler);
        })
        .is_err(),
        "the actor-state borrow assertion accepted a real Vec release call"
    );
}

#[test]
fn d65_cursor_recursion_truth_table_has_one_owner_release_per_shape() {
    for (name, source, expected_owner_slots) in [
        ("local_flat_full", LOCAL_FLAT_FULL, 1),
        ("local_flat_partial", LOCAL_FLAT_PARTIAL, 1),
        ("local_nested_full", LOCAL_NESTED_FULL, 3),
        ("local_nested_partial", LOCAL_NESTED_PARTIAL, 2),
    ] {
        let ir = emit_ir(source, name);
        let main = function_body(&ir, "main");
        let owner_slots = vec_release_owner_slots(main);
        assert_exact_owner_slot_count(name, &owner_slots, expected_owner_slots);
    }

    let state_shapes = [
        (
            "state_flat_full",
            state_source(
                "Vec<i64>",
                "var total: i64 = 0; for value in values { total = total + value; } total",
                "let values: Vec<i64> = Vec::new(); values.push(1); values.push(2);",
            ),
        ),
        (
            "state_flat_partial",
            state_source(
                "Vec<i64>",
                "for value in values { if value == 1 { break; } } 0",
                "let values: Vec<i64> = Vec::new(); values.push(1); values.push(2);",
            ),
        ),
        (
            "state_nested_full",
            state_source(
                "Vec<Vec<i64>>",
                "var total: i64 = 0; for row in values { total = total + row[0]; } total",
                "let values: Vec<Vec<i64>> = Vec::new(); let row: Vec<i64> = Vec::new(); \
                 row.push(1); values.push(row);",
            ),
        ),
        (
            "state_nested_partial",
            state_source(
                "Vec<Vec<i64>>",
                "for value in values[0] { if value == 1 { break; } } 0",
                "let values: Vec<Vec<i64>> = Vec::new(); let row: Vec<i64> = Vec::new(); \
                 row.push(1); row.push(2); values.push(row);",
            ),
        ),
    ];
    for (name, source) in state_shapes {
        let ir = emit_ir(&source, name);
        let handler = function_body(&ir, "Holder__recv__scan");
        let state_drop = function_body(&ir, "__hew_state_drop_Holder");
        assert_iteration_cursor_borrows_actor_state(name, handler);
        assert_eq!(
            vec_release_calls(state_drop),
            1,
            "{name}: actor-state teardown must be the sole Vec release\n{state_drop}"
        );
    }
}

#[test]
fn d65_exact_count_oracle_rejects_extra_and_suppressed_release_per_shape() {
    for name in [
        "local_flat_full",
        "local_flat_partial",
        "local_nested_full",
        "local_nested_partial",
        "state_flat_full",
        "state_flat_partial",
        "state_nested_full",
        "state_nested_partial",
    ] {
        let (observed, expected) = d65_shape_exact_count(name);
        enforce_exact_release_count(name, observed, expected);
        assert!(
            exact_count_rejects_mutations(observed, expected),
            "{name}: exact count accepted an injected extra or suppressed release"
        );
        assert!(
            std::panic::catch_unwind(|| {
                enforce_exact_release_count(name, observed.saturating_sub(1), expected);
            })
            .is_err(),
            "{name}: suppressed release did not fail the exact-count oracle"
        );
    }
}

#[test]
#[cfg(unix)]
fn d65_injected_extra_release_aborts_for_every_shape() {
    use std::os::unix::process::ExitStatusExt;

    let current_exe = std::env::current_exe().expect("current structural test executable");
    for name in [
        "local_flat_full",
        "local_flat_partial",
        "local_nested_full",
        "local_nested_partial",
        "state_flat_full",
        "state_flat_partial",
        "state_nested_full",
        "state_nested_partial",
    ] {
        let output = std::process::Command::new(&current_exe)
            .args([
                "--exact",
                "d65_vec_release_truth_table::d65_injected_extra_release_helper",
                "--nocapture",
            ])
            .env("HEW_D65_EXTRA_RELEASE_SHAPE", name)
            .output()
            .unwrap_or_else(|error| panic!("run D65 extra-release helper for {name}: {error}"));
        assert_eq!(
            output.status.signal(),
            Some(libc::SIGABRT),
            "{name}: injected extra release must abort loudly; status={:?}\nstdout:\n{}\nstderr:\n{}",
            output.status,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }
}

#[test]
fn d65_injected_extra_release_helper() {
    let Ok(name) = std::env::var("HEW_D65_EXTRA_RELEASE_SHAPE") else {
        return;
    };
    let (observed, expected) = d65_shape_exact_count(&name);
    enforce_exact_release_count(&name, observed + 1, expected);
    panic!("{name}: injected extra release did not abort");
}

/// A released yield must be a cloned yield, for every local shape — and the
/// check must actually reject the unpaired case rather than pass vacuously.
#[test]
fn d65_released_yield_is_always_a_cloned_yield() {
    for (name, source, source_vec_bindings) in [
        ("local_flat_full", LOCAL_FLAT_FULL, 1),
        ("local_flat_partial", LOCAL_FLAT_PARTIAL, 1),
        ("local_nested_full", LOCAL_NESTED_FULL, 2),
        ("local_nested_partial", LOCAL_NESTED_PARTIAL, 2),
    ] {
        let ir = emit_ir(source, name);
        let main = function_body(&ir, "main");
        assert_yield_release_paired_with_clone_out(name, main, source_vec_bindings);
    }

    // Counterfactual: `local_nested_full` is the shape that actually releases a
    // yielded owner. Strip the clone-out that produced it and the pairing check
    // must fail — otherwise it would accept the aliased-yield double-free the
    // retired `vec_iter_yield_is_fresh_owner` gate used to reject.
    let ir = emit_ir(LOCAL_NESTED_FULL, "local_nested_full_counterfactual");
    let main = function_body(&ir, "main");
    assert!(
        main.contains("@hew_vec_get_clone("),
        "local_nested_full must yield through the clone-out for the counterfactual to be \
         meaningful\n{main}"
    );
    let unpaired: String = main
        .lines()
        .filter(|line| !line.contains("@hew_vec_get_clone("))
        .collect::<Vec<_>>()
        .join("\n");
    assert!(
        std::panic::catch_unwind(|| {
            assert_yield_release_paired_with_clone_out("unpaired_yield", &unpaired, 2);
        })
        .is_err(),
        "the clone-out pairing check accepted a released yield with no clone-out"
    );
}

/// The actor-state borrow proof must be a proof, not a formality: flipping the
/// cursor drop flag to the release value has to be rejected.
///
/// Without this, `assert_iteration_cursor_borrows_actor_state` could accept any
/// emitted release as "gated" and the truth table would stop noticing a handler
/// that frees the state Vec it only borrows.
#[test]
fn d65_actor_state_borrow_proof_rejects_a_live_release_flag() {
    let source = state_source(
        "Vec<i64>",
        "var total: i64 = 0; for value in values { total = total + value; } total",
        "let values: Vec<i64> = Vec::new(); values.push(1); values.push(2);",
    );
    let ir = emit_ir(&source, "state_flat_full_flag_counterfactual");
    let handler = function_body(&ir, "Holder__recv__scan");

    // Real IR: the release site exists but its guard flag is constant, so the
    // release is dead and the proof succeeds.
    assert_iteration_cursor_borrows_actor_state("state_flat_full", handler);
    assert!(
        vec_release_calls(handler) > 0,
        "counterfactual is vacuous unless the handler actually emits a gated release\n{handler}"
    );
    let constants = cursor_release_guard_flag_constants(handler)
        .expect("the gated cursor release guard must resolve");
    assert!(
        !constants.is_empty() && !constants.contains(&0),
        "expected a borrow-only guard flag, got {constants:?}"
    );

    // Counterfactual: make the guard flag reach the release value.
    let live = handler.replace("store i64 1,", "store i64 0,");
    assert!(
        std::panic::catch_unwind(|| {
            assert_iteration_cursor_borrows_actor_state("live_release_flag", &live);
        })
        .is_err(),
        "the actor-state borrow proof accepted a cursor release whose flag takes the release \
         branch, which would free the actor's own state Vec on every message"
    );
}
