//! Public v1 packages remain readable independently of current emitter output.

use hew_wasm::sandbox::bytecode::{OpV1, TermV1};
use serde_json::Value;

#[test]
fn recorded_v1_instructions_round_trip_without_wire_changes() {
    let fixtures =
        std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("../hew-sandbox-vm/fixtures");
    let mut packages = 0;
    for directory in std::fs::read_dir(fixtures).expect("fixture directory") {
        let path = directory
            .expect("fixture entry")
            .path()
            .join("bytecode.json");
        if !path.is_file() {
            continue;
        }
        let package: Value =
            serde_json::from_slice(&std::fs::read(&path).expect("fixture package"))
                .expect("JSON fixture");
        for function in package["functions"].as_array().expect("fixture functions") {
            for block in function["blocks"].as_array().expect("fixture blocks") {
                for operation in block["ops"].as_array().expect("fixture operations") {
                    let typed: OpV1 = serde_json::from_value(operation.clone())
                        .unwrap_or_else(|error| panic!("{}: {operation}: {error}", path.display()));
                    assert_eq!(
                        serde_json::to_value(typed).expect("typed operation"),
                        *operation,
                        "{}",
                        path.display()
                    );
                }
                let typed: TermV1 =
                    serde_json::from_value(block["term"].clone()).unwrap_or_else(|error| {
                        panic!("{}: {}: {error}", path.display(), block["term"])
                    });
                assert_eq!(
                    serde_json::to_value(typed).expect("typed terminator"),
                    block["term"],
                    "{}",
                    path.display()
                );
            }
        }
        packages += 1;
    }
    assert!(
        packages > 0,
        "the compatibility oracle must read external packages"
    );
}

#[test]
fn aggregate_result_descriptors_preserve_wire_presence() {
    for (operation, identity) in [("runtime.call", "family"), ("extern.call", "extern")] {
        let mut call = serde_json::json!({
            "op": operation,
            "args": [],
            "result": null,
            "normal": null,
            "unwind": null,
            "result_shape": null,
        });
        call[identity] = 0.into();
        for descriptor in [None, Some(Value::Null), Some(7.into())] {
            let mut input = call.clone();
            if let Some(descriptor) = descriptor {
                input["aggregate_result_shape"] = descriptor;
            }
            let typed: TermV1 = serde_json::from_value(input.clone()).expect("external call");
            assert_eq!(serde_json::to_value(typed).expect("encoded call"), input);
        }
    }
}
