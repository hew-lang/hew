use hew_wasm::sandbox::compile_tests_to_sandbox_bytecode_js;

#[test]
fn browser_export_compiles_selected_test_packages_once_per_source() {
    let source =
        "#[test]\nfn first() { assert(true); }\n#[test]\n#[should_panic]\nfn second() { assert(1 == 2); }\n";
    let output = compile_tests_to_sandbox_bytecode_js(source, "playground.hew");
    let output: serde_json::Value = serde_json::from_str(&output).expect("browser JSON");
    assert_eq!(output["diagnostics"], serde_json::json!([]));
    let tests = output["tests"].as_array().expect("test packages");
    assert_eq!(tests.len(), 2);
    assert_eq!(tests[0]["identity"], "playground.hew::first");
    assert_eq!(tests[1]["identity"], "playground.hew::second");
    assert_eq!(tests[0]["should_panic"], false);
    assert_eq!(tests[1]["should_panic"], true);
    assert!(tests
        .iter()
        .all(|test| test["bytecode"]["entry"].is_object()));
    assert_ne!(tests[0]["bytecode"]["entry"], tests[1]["bytecode"]["entry"]);
}

#[test]
fn browser_export_keeps_real_time_tests_visible_without_vm_admission() {
    let source =
        "#[test]\n#[real_time]\nfn host_only() {}\n#[test]\nfn deterministic() { assert(true); }\n";
    let output = compile_tests_to_sandbox_bytecode_js(source, "playground.hew");
    let output: serde_json::Value = serde_json::from_str(&output).expect("browser JSON");
    assert_eq!(output["diagnostics"], serde_json::json!([]));
    let tests = output["tests"].as_array().expect("test packages");
    assert_eq!(tests.len(), 2);
    assert_eq!(tests[0]["real_time"], true);
    assert!(tests[0]["bytecode"].is_null());
    assert_eq!(tests[1]["real_time"], false);
    assert!(tests[1]["bytecode"]["entry"].is_object());
}
