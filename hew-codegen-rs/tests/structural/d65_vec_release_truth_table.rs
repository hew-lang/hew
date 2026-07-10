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
    let symbol_pos = ir
        .find(&format!("@{symbol}("))
        .unwrap_or_else(|| panic!("missing function `{symbol}` in IR:\n{ir}"));
    let start = ir[..symbol_pos]
        .rfind("define ")
        .unwrap_or_else(|| panic!("missing definition for `{symbol}`"));
    let end = ir[start..]
        .find("\n}\n")
        .map(|offset| start + offset + 3)
        .unwrap_or(ir.len());
    &ir[start..end]
}

fn vec_release_calls(body: &str) -> usize {
    body.matches("call void @hew_vec_free(").count()
        + body.matches("call void @hew_vec_free_owned(").count()
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

fn exact_count_rejects_mutations(observed: usize, expected: usize) -> bool {
    observed + 1 != expected && observed.saturating_sub(1) != expected
}

fn d65_shape_exact_count(name: &str) -> (usize, usize) {
    let (source, symbol, expected, owner_slots) = match name {
        "local_flat_full" => (LOCAL_FLAT_FULL.to_string(), "main", 1, true),
        "local_flat_partial" => (LOCAL_FLAT_PARTIAL.to_string(), "main", 1, true),
        "local_nested_full" => (LOCAL_NESTED_FULL.to_string(), "main", 2, true),
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
fn d65_cursor_recursion_truth_table_has_one_owner_release_per_shape() {
    for (name, source, expected_owner_slots) in [
        ("local_flat_full", LOCAL_FLAT_FULL, 1),
        ("local_flat_partial", LOCAL_FLAT_PARTIAL, 1),
        ("local_nested_full", LOCAL_NESTED_FULL, 2),
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
        assert_eq!(
            vec_release_calls(handler),
            0,
            "{name}: the iteration cursor must borrow actor state\n{handler}"
        );
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
