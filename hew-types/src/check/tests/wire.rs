#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

#[test]
fn wire_encode_decode_record_binary_codec_rewrite() {
    let output = check_source(
        r"
        #[wire]
        type Point { x: i64 @1, y: i64 @2 }

        fn main() -> i64 {
            let p = Point { x: 1, y: 2 };
            let b = p.encode();
            let p2 = Point.decode(b);
            return p2.x + p2.y;
        }
        ",
    );

    assert!(
        output.errors.is_empty(),
        "wire `.encode()` / `.decode()` should type-check: {:#?}",
        output.errors
    );
    let encode_rewrite = output.method_call_rewrites.values().any(|rewrite| {
        matches!(
            rewrite,
            MethodCallRewrite::WireCodec {
                direction: WireCodecDirection::Encode,
                ..
            }
        )
    });
    assert!(
        encode_rewrite,
        "`.encode()` must record a WireCodec::Encode rewrite (not MethodCallNoRewrite)"
    );
    let decode_rewrite = output.method_call_rewrites.values().any(|rewrite| {
        matches!(
            rewrite,
            MethodCallRewrite::WireCodec {
                direction: WireCodecDirection::Decode,
                ..
            }
        )
    });
    assert!(
        decode_rewrite,
        "`Type.decode(bytes)` must record a WireCodec::Decode rewrite (not MethodCallNoRewrite)"
    );
}

#[test]
fn wire_text_format_methods_record_codec_rewrite() {
    // The text-format wire methods (`to_json`/`from_json`/`to_yaml`/`from_yaml`)
    // lower through the CBOR↔text bridge: each records a WireCodec rewrite with
    // its text direction so HIR/codegen drive the bridge thunks. (Replaces the
    // historical pin that asserted NO rewrite, from before the text codec landed.)
    let output = check_source(
        r#"
        #[wire]
        type Point { x: i64 @1, y: i64 @2 }

        fn main() {
            let p = Point { x: 1, y: 2 };
            let _j: string = p.to_json();
            let _y: string = p.to_yaml();
            let _fj: Result<Point, string> = Point.from_json("{}");
            let _fy: Result<Point, string> = Point.from_yaml("");
        }
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "wire text-format methods must type-check: {:?}",
        output.errors
    );

    let has_dir = |want: WireCodecDirection| {
        output.method_call_rewrites.values().any(|rewrite| {
            matches!(
                rewrite,
                MethodCallRewrite::WireCodec { direction, .. } if *direction == want
            )
        })
    };
    assert!(
        has_dir(WireCodecDirection::ToJson),
        "to_json must record ToJson"
    );
    assert!(
        has_dir(WireCodecDirection::ToYaml),
        "to_yaml must record ToYaml"
    );
    assert!(
        has_dir(WireCodecDirection::FromJson),
        "from_json must record FromJson"
    );
    assert!(
        has_dir(WireCodecDirection::FromYaml),
        "from_yaml must record FromYaml"
    );
}

#[test]
fn generic_wire_facade_records_all_typed_codec_rewrites() {
    let parsed = hew_parser::parse(
        r"
        import std.encoding.wire;

        fn main() {
            let values: HashMap<string, i64> = HashMap.new();
            let binary: bytes = wire.encode(values);
            let _binary_back: HashMap<string, i64> =
                wire.decode<HashMap<string, i64>>(binary);
            let json: string = wire.to_json(values);
            let _json_back: Result<HashMap<string, i64>, string> =
                wire.from_json<HashMap<string, i64>>(json);
            let yaml: string = wire.to_yaml(values);
            let _yaml_back: Result<HashMap<string, i64>, string> =
                wire.from_yaml<HashMap<string, i64>>(yaml);
        }
        ",
    );
    assert!(
        parsed.errors.is_empty(),
        "generic wire facade source must parse: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    assert!(
        output.errors.is_empty(),
        "generic wire facade must type-check: {:?}",
        output.errors
    );

    for direction in [
        WireCodecDirection::Encode,
        WireCodecDirection::Decode,
        WireCodecDirection::ToJson,
        WireCodecDirection::FromJson,
        WireCodecDirection::ToYaml,
        WireCodecDirection::FromYaml,
    ] {
        assert!(
            output.method_call_rewrites.values().any(|rewrite| {
                matches!(
                    rewrite,
                    MethodCallRewrite::GenericWireCodec { direction: actual, .. }
                        if *actual == direction
                )
            }),
            "generic wire facade must record {direction:?} through the typed codec rewrite"
        );
    }
}

#[test]
fn generic_wire_facade_admits_owned_key_and_element_shapes() {
    let parsed = hew_parser::parse(
        r#"
        import std.encoding.wire;

        #[wire]
        type Key { id: i64 @1 }

        fn main() {
            let _record_keyed =
                wire.from_json<HashMap<Key, string>>("[]");
            let _vec_bytes = wire.from_json<Vec<bytes>>("[]");
        }
        "#,
    );
    assert!(
        parsed.errors.is_empty(),
        "source must parse: {:?}",
        parsed.errors
    );
    let mut checker = Checker::new(test_registry());
    let output = checker.check_program(&parsed.program);
    assert!(
        output.errors.is_empty(),
        "record-keyed maps and Vec<bytes> share ordinary native value ownership: {:?}",
        output.errors
    );
}

fn check_wire_program(source: &str) -> TypeCheckOutput {
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "source must parse: {:?}",
        parsed.errors
    );
    Checker::new(test_registry()).check_program(&parsed.program)
}

fn generic_codec_value_types(output: &TypeCheckOutput) -> Vec<String> {
    let mut types: Vec<String> = output
        .method_call_rewrites
        .values()
        .filter_map(|rewrite| match rewrite {
            MethodCallRewrite::GenericWireCodec { value_ty, .. } => {
                Some(value_ty.user_facing().to_string())
            }
            _ => None,
        })
        .collect();
    types.sort();
    types
}

#[test]
fn generic_wire_facade_admits_bare_wire_type() {
    let output = check_wire_program(
        r#"
        import std.encoding.wire;

        #[wire]
        type Config { name: string @1, port: i64 @2 }

        fn main() {
            let c = Config { name: "svc", port: 80 };
            let _text = wire.to_json(c);
            let _back = wire.from_json<Config>("{}");
        }
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "a #[wire] type is Serializable: {:#?}",
        output.errors
    );
    assert_eq!(generic_codec_value_types(&output), ["Config", "Config"]);
}

#[test]
fn generic_wire_facade_records_literal_value_type_after_defaulting() {
    let output = check_wire_program(
        r"
        import std.encoding.wire;

        fn main() {
            let _text = wire.to_json([1, 2, 3]);
        }
        ",
    );
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    assert_eq!(generic_codec_value_types(&output), ["Vec<i64>"]);
}

#[test]
fn generic_wire_facade_named_import_records_codec_rewrite() {
    let output = check_wire_program(
        r"
        import std.encoding.wire.{to_json};

        fn main() {
            let _text = to_json([true]);
        }
        ",
    );
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    assert_eq!(generic_codec_value_types(&output), ["Vec<bool>"]);
}

#[test]
fn generic_wire_facade_refuses_values_without_a_codec() {
    for (value_ty, value) in [
        ("Plain", "Plain { a: 1 }"),
        ("Vec<Plain>", "[Plain { a: 1 }]"),
        ("(i64, string)", r#"(1, "a")"#),
        ("Option<Option<i64>>", "Some(Some(1))"),
        ("Outer", "Outer { p: Plain { a: 1 } }"),
        ("Tree", "Tree { v: 1, kids: [] }"),
        ("Handle", "Handle { fd: 3 }"),
    ] {
        let output = check_wire_program(&format!(
            r"
            import std.encoding.wire;

            type Plain {{ a: i64 }}

            #[wire]
            type Outer {{ p: Plain @1 }}

            #[wire]
            type Tree {{ v: i64 @1, kids: Vec<Tree> @2 }}

            #[resource]
            type Handle {{ fd: i64 }}

            impl Handle {{
                fn close(consume self) {{}}
            }}

            fn main() {{
                let v: {value_ty} = {value};
                let _text = wire.to_json(v);
            }}
            "
        ));
        assert!(
            output.errors.iter().any(|error| {
                error.kind == TypeErrorKind::BoundsNotSatisfied
                    && error.message.contains(&format!(
                        "type `{value_ty}` does not implement trait `Serializable`"
                    ))
            }),
            "`{value_ty}` has no codec and must be refused at check time: {:#?}",
            output.errors
        );
    }
}

#[test]
fn generic_wire_facade_bounded_type_param_is_serializable() {
    let output = check_wire_program(
        r"
        import std.encoding.wire;

        fn show<T: Serializable>(values: Vec<T>) -> string {
            wire.to_json(values)
        }

        fn show_any<T>(values: Vec<T>) -> string {
            wire.to_json(values)
        }

        fn main() {}
        ",
    );
    assert_eq!(output.errors.len(), 1, "{:#?}", output.errors);
    assert!(
        output.errors[0]
            .message
            .contains("type `Vec<T>` does not implement trait `Serializable`"),
        "only the unbounded parameter is refused: {:#?}",
        output.errors
    );
}

#[test]
fn generic_wire_decode_without_a_value_type_is_refused() {
    let output = check_wire_program(
        r#"
        import std.encoding.wire;

        fn main() {
            let _r = wire.from_json("[1]");
        }
        "#,
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::InferenceFailed
                && error.message.contains("wire codec call")),
        "an unsettled codec value type is a check-time error: {:#?}",
        output.errors
    );
    assert!(generic_codec_value_types(&output).is_empty());
}

#[test]
fn generic_wire_codec_is_not_a_function_value() {
    let output = check_wire_program(
        r"
        import std.encoding.wire;

        fn main() {
            let _f = wire.to_json;
        }
        ",
    );
    assert!(
        output.errors.iter().any(|error| error
            .message
            .contains("is a compiler codec and cannot be used as a value")),
        "{:#?}",
        output.errors
    );
}

#[test]
fn plain_type_has_no_codec_methods() {
    let output = check_wire_program(
        r#"
        type Plain { a: i64, b: string }

        fn main() {
            let p = Plain { a: 1, b: "x" };
            let _text = p.to_json();
            let _back = Plain.from_json("{}");
        }
        "#,
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.message.contains("no method `to_json` on `Plain`")),
        "{:#?}",
        output.errors
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.span.start > 0 && error.message.contains("from_json")),
        "a plain type has no static codec either: {:#?}",
        output.errors
    );
}

#[test]
fn wire_from_json_returns_result_self_string() {
    // A `#[wire]` type's `from_json`/`from_yaml` static parsers are fallible
    // (arbitrary user input — config files, HTTP bodies), so they return
    // `Result<Self, string>`, matching the non-wire `Encode` path. This pins
    // the ratified return shape on the wire registration path.
    let output = check_source(
        r#"
        #[wire]
        type Point { x: i64 @1, y: i64 @2 }

        fn main() {
            let _r: Result<Point, string> = Point.from_json("{\"x\":1,\"y\":2}");
            let _y: Result<Point, string> = Point.from_yaml("x: 1");
        }
        "#,
    );
    assert!(
        output.errors.is_empty(),
        "wire from_json/from_yaml must type-check as Result<Self, string>; got: {:?}",
        output.errors
    );
}

#[test]
fn wire_from_json_bare_self_is_type_error() {
    // Assigning a wire `from_json` result directly to `Self` (not
    // `Result<Self, …>`) must be a type error — the bare-`Self` registration
    // is gone.
    let output = check_source(
        r#"
        #[wire]
        type Point { x: i64 @1, y: i64 @2 }

        fn main() {
            let _p: Point = Point.from_json("{\"x\":1,\"y\":2}");
        }
        "#,
    );
    assert!(
        !output.errors.is_empty(),
        "assigning Result<Point, string> to Point must be a type error"
    );
}

#[test]
fn wire_layout_table_populated_from_wire_struct() {
    let output = check_source(
        r"
        #[wire]
        type Point { x: i64 @1, y: i64 @2 }
        ",
    );

    assert!(output.errors.is_empty(), "type errors: {:?}", output.errors);
    assert!(output.wire_layouts.contains_key("Point"));
    let entry = &output.wire_layouts["Point"];
    assert_eq!(entry.fields.len(), 2);
    assert_eq!(entry.fields[0].tag, 1);
    assert_eq!(entry.fields[0].name, "x");
    assert_eq!(entry.fields[1].tag, 2);
    assert_eq!(entry.fields[1].name, "y");
}

#[test]
fn wire_layout_table_populated_from_wire_enum() {
    let output = check_source(
        r"
        #[wire]
        enum Status { Active, Inactive, }
        ",
    );

    assert!(output.errors.is_empty(), "type errors: {:?}", output.errors);
    assert!(output.wire_layouts.contains_key("Status"));
    let entry = &output.wire_layouts["Status"];
    assert_eq!(entry.variants.len(), 2);
}

#[test]
fn wire_layout_json_name_override_preserved() {
    let output = check_source(
        r#"
        #[wire]
        type Cfg { host: string @1 json_name="hostname" }
        "#,
    );

    assert!(output.errors.is_empty(), "type errors: {:?}", output.errors);
    let entry = output
        .wire_layouts
        .get("Cfg")
        .expect("Cfg should have a wire layout entry");
    assert_eq!(entry.fields[0].json_name, Some("hostname".to_string()));
}

#[test]
fn wire_optional_field_requires_semantic_option_type() {
    let source = r"
        #[wire]
        type Invalid { value: string @1 optional }

        #[wire]
        type RequiredValue { value: string @1 }

        #[wire]
        type RequiredOption { value: Option<string> @1 }

        #[wire]
        type OptionalOption { value: Option<string> @1 optional }
    ";
    let output = check_source(source);
    let error = output
        .errors
        .iter()
        .find(|error| error.kind == TypeErrorKind::WireOptionalFieldRequiresOption)
        .expect("bare optional field must be rejected");

    assert_eq!(
        error.message,
        "E_WIRE_OPTIONAL_REQUIRES_OPTION: wire field `value` is marked `optional` but must have type `Option<T>`"
    );
    assert_eq!(error.span, 47..54);
    assert_eq!(
        output
            .errors
            .iter()
            .filter(|error| error.kind == TypeErrorKind::WireOptionalFieldRequiresOption)
            .count(),
        1,
        "required T, required Option<T>, and optional Option<T> are counterfactual controls: {:#?}",
        output.errors
    );
}

#[test]
fn wire_optional_field_accepts_option_alias_after_resolution() {
    let output = check_source(
        r"
        type MaybeText = Option<string>;

        #[wire]
        type Message { body: MaybeText @1 optional }
        ",
    );

    assert!(
        !output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::WireOptionalFieldRequiresOption),
        "an alias resolving to Option<T> must be admitted: {:#?}",
        output.errors
    );
}
