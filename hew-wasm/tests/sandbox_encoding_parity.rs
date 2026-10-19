//! The sandbox VM implements no encoding runtime, so TOML values must meet
//! the sandbox boundary exactly as JSON and YAML values do.

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
        .map(|diagnostic| {
            diagnostic
                .get("severity")
                .map_or_else(String::new, ToString::to_string)
        })
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
fn toml_values_meet_the_sandbox_boundary_like_json_and_yaml() {
    let json = sandbox_outcome(&program("json", "object"));
    let yaml = sandbox_outcome(&program("yaml", "object"));
    let toml = sandbox_outcome(&program("toml", "table"));
    assert_eq!(toml, json);
    assert_eq!(toml, yaml);
    // Negative control: the oracle distinguishes a refused program.
    let refused = sandbox_outcome(&program("toml", "table").replace("get_int()", "get_u64()"));
    assert!(!refused.0, "{refused:?}");
}
