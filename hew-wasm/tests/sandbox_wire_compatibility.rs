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
        for descriptor in [None, Some(Value::Null), Some(0.into()), Some(7.into())] {
            let mut input = call.clone();
            if let Some(descriptor) = descriptor {
                input["aggregate_result_shape"] = descriptor;
            }
            let typed: TermV1 = serde_json::from_value(input.clone()).expect("external call");
            assert_eq!(serde_json::to_value(typed).expect("encoded call"), input);
        }
    }
}

#[test]
fn runtime_identities_round_trip_through_the_v1_family_detail_shape() {
    use hew_wasm::sandbox::bytecode::RuntimeFamilyEntry;

    for identity in hew_types::runtime_call::all_runtime_call_families() {
        let entry = RuntimeFamilyEntry {
            id: 7,
            identity,
            null_detail: false,
        };
        let encoded = serde_json::to_value(&entry).expect("runtime entry");
        let expected = match serde_json::to_value(identity).expect("compiler identity") {
            Value::String(family) => serde_json::json!({"id": 7, "family": family}),
            Value::Object(fields) => {
                let (family, detail) = fields.into_iter().next().expect("payload variant");
                serde_json::json!({"id": 7, "family": family, "detail": detail})
            }
            other => panic!("compiler runtime identity is not an enum: {other}"),
        };
        assert_eq!(encoded, expected, "{identity:?}");
        assert_eq!(
            serde_json::from_value::<RuntimeFamilyEntry>(encoded).expect("runtime identity"),
            entry,
            "{identity:?}"
        );
    }
}

#[test]
fn runtime_unit_detail_preserves_omission_and_null() {
    use hew_wasm::sandbox::bytecode::RuntimeFamilyEntry;

    for detail in [None, Some(Value::Null)] {
        let mut input = serde_json::json!({"id": 0, "family": "BytesLen"});
        if let Some(detail) = detail {
            input["detail"] = detail;
        }
        let typed: RuntimeFamilyEntry = serde_json::from_value(input.clone()).expect("unit entry");
        assert_eq!(serde_json::to_value(typed).expect("unit entry"), input);
    }
}

#[test]
fn runtime_identity_rejects_mismatched_or_unknown_payloads() {
    use hew_wasm::sandbox::bytecode::RuntimeFamilyEntry;

    for input in [
        serde_json::json!({"id": 0, "family": "BytesLen", "detail": "Len"}),
        serde_json::json!({"id": 0, "family": "Vector"}),
        serde_json::json!({"id": 0, "family": "Vector", "detail": null}),
        serde_json::json!({"id": 0, "family": "Vector", "detail": "NotAnOperation"}),
        serde_json::json!({"id": 0, "family": "FutureFamily"}),
        serde_json::json!({"id": 0, "family": "Print", "detail": {"kind": "I64", "newline": "yes"}}),
    ] {
        assert!(
            serde_json::from_value::<RuntimeFamilyEntry>(input.clone()).is_err(),
            "accepted invalid runtime identity: {input}"
        );
    }
}

#[test]
fn suspension_detail_preserves_omission_null_and_typed_values() {
    for (kind, details) in [
        ("Sleep", vec![None, Some(Value::Null)]),
        (
            "Select",
            vec![Some(
                serde_json::json!({"has_timeout": true, "order": "completion"}),
            )],
        ),
        (
            "NativeIo",
            vec![Some(serde_json::json!({"operation": "StdinReadLine"}))],
        ),
    ] {
        for detail in details {
            let mut input = serde_json::json!({
                "op": "suspend", "kind": kind, "inputs": [], "result": null,
                "resumes": [], "cancel": null, "unwind": null,
            });
            if let Some(detail) = detail {
                input["detail"] = detail;
            }
            let typed: TermV1 = serde_json::from_value(input.clone()).expect("suspension");
            assert_eq!(serde_json::to_value(typed).expect("suspension"), input);
        }
    }
}

#[test]
fn recorded_v1_packages_retain_the_complete_wire_shape() {
    use hew_wasm::sandbox::package::Package;
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
        let input: Value = serde_json::from_slice(&std::fs::read(&path).expect("fixture package"))
            .expect("fixture JSON");
        let typed: Package = serde_json::from_value(input.clone())
            .unwrap_or_else(|error| panic!("{}: {error}", path.display()));
        assert_eq!(
            serde_json::to_value(typed).expect("typed package"),
            input,
            "{}",
            path.display()
        );
        packages += 1;
    }
    assert!(
        packages > 0,
        "the wire oracle requires independently recorded packages"
    );
}
