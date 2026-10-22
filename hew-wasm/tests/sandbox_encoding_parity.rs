//! Checked source contracts for encoding values. Runtime execution is covered
//! separately: JSON runs in the VM; unsupported formats refuse at admission.

fn sandbox_outcome(source: &str) -> (bool, Vec<String>) {
    let output: serde_json::Value = serde_json::from_str(
        &hew_wasm::sandbox::compile_to_sandbox_bytecode_json(source, "sandbox-vm-export"),
    )
    .expect("sandbox output is JSON");
    let accepted = output
        .get("bytecode")
        .is_some_and(|bytecode| !bytecode.is_null());
    let mut codes: Vec<String> = output
        .get("diagnostics")
        .and_then(serde_json::Value::as_array)
        .into_iter()
        .flatten()
        .map(|diagnostic| format!("{}:{}", diagnostic["severity"], diagnostic["kind"]))
        .collect();
    codes.sort();
    (accepted, codes)
}

fn program(format: &str, object: &str) -> String {
    format!(
        "import std.encoding.{format};\n\
         fn main() {{\n\
         \x20   var value = {format}.{object}();\n\
         \x20   value.set(\"n\", {format}.from_int(1)).expect(\"container\");\n\
         \x20   var copy = value;\n\
         \x20   copy.set(\"n\", {format}.from_int(2)).expect(\"container\");\n\
         \x20   println(value.get_field(\"n\").expect(\"container\").expect(\"present\").get_int().expect(\"integer\"));\n\
         }}\n"
    )
}

#[test]
fn encoding_values_share_checked_source_contracts() {
    let json = sandbox_outcome(&program("json", "object"));
    assert!(json.0, "{json:?}");
    let yaml = sandbox_outcome(&program("yaml", "object"));
    let toml = sandbox_outcome(&program("toml", "table"));
    assert_eq!(toml, json);
    assert_eq!(toml, yaml);
    // Negative control: the oracle distinguishes a refused program.
    let refused = sandbox_outcome(&program("toml", "table").replace("get_int()", "get_u64()"));
    assert!(!refused.0, "{refused:?}");
    assert_eq!(refused.1, ["\"error\":\"UndefinedMethod\""]);
}
