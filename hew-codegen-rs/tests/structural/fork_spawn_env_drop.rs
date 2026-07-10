//! Fork-call task environments own moved arguments, unlike scope-owned closure
//! environments whose captures are borrowed. The RC callback must drop only the
//! owned payload fields; `hew_rc_new` reclaims its allocation itself.

use std::path::Path;

use hew_codegen_rs::{emit_module, EmitOptions};
use hew_types::module_registry::ModuleRegistry;
use hew_types::Checker;

fn emit_ll(source: &str) -> String {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(ModuleRegistry::new(vec![]));
    let tc_output = checker.check_program(&parsed.program);
    let output = hew_hir::lower_program(
        &parsed.program,
        &tc_output,
        &hew_hir::ResolutionCtx,
        hew_hir::TargetArch::host(),
    );
    assert!(
        output.diagnostics.is_empty(),
        "hir diagnostics: {:?}",
        output.diagnostics
    );
    let pipeline = hew_mir::lower_hir_module(&output.module);
    assert!(
        pipeline.diagnostics.is_empty(),
        "mir diagnostics: {:?}",
        pipeline.diagnostics
    );
    let tmp = tempfile::Builder::new()
        .prefix("hew-fork-spawn-env-drop-")
        .tempdir()
        .expect("tempdir");
    let options = EmitOptions {
        module_name: "fork_spawn_env_drop",
        out_dir: tmp.path(),
        native: false,
        wasm: false,
        target_triple: None,
        debug: false,
        opt_level: hew_codegen_rs::OptLevel::O0,
        source_path: None,
    };
    let artefacts = emit_module(&pipeline, &options).expect("fork pipeline must emit");
    let ll_path: &Path = artefacts
        .ll_path
        .as_deref()
        .expect("emit_module must populate ll_path");
    std::fs::read_to_string(ll_path).expect("read emitted .ll")
}

fn function_body(ll: &str, fn_name: &str) -> String {
    let needle = format!("@{fn_name}(");
    let mut body = String::new();
    let mut in_fn = false;
    for line in ll.lines() {
        if !in_fn && line.starts_with("define") && line.contains(&needle) {
            in_fn = true;
        }
        if in_fn {
            body.push_str(line);
            body.push('\n');
            if line.trim() == "}" {
                break;
            }
        }
    }
    body
}

#[test]
fn fork_env_rc_callback_drops_only_moved_string_argument() {
    let ll = emit_ll(
        r#"
        fn shout(n: i64, message: string) {}

        actor Driver {
            receive fn drive() {
                let greeting = "hello".to_upper();
                scope {
                    fork { shout(7, greeting); }
                };
            }
        }

        fn main() -> i64 {
            let d = spawn Driver;
            d.drive();
            0
        }
        "#,
    );
    let prefix = "__hew_spawn_env_rc_drop_";
    let thunk_name = ll
        .lines()
        .find_map(|line| {
            if !line.starts_with("define") {
                return None;
            }
            let start = line.find(&format!("@{prefix}"))? + 1;
            line[start..].split('(').next().map(str::to_owned)
        })
        .expect("moved string fork argument must emit an Rc payload callback");
    let thunk = function_body(&ll, &thunk_name);
    assert_eq!(
        thunk.matches("hew_string_drop").count(),
        1,
        "the moved string must be released exactly once by its task env callback: {thunk}"
    );
    assert!(
        !thunk.contains("hew_dyn_box_free"),
        "Rc payload callback must not free Rc-managed storage: {thunk}"
    );
    assert!(
        ll.contains(&format!("ptr @{thunk_name}")),
        "hew_rc_new must receive the generated payload callback: {ll}"
    );
}
