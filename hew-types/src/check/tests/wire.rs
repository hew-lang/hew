#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

fn codec_rewrites(output: &TypeCheckOutput) -> Vec<(Codec, String)> {
    let mut found: Vec<(Codec, String)> = output
        .method_call_rewrites
        .values()
        .filter_map(|rewrite| match rewrite {
            MethodCallRewrite::Codec { codec, value_ty } => {
                Some((*codec, value_ty.user_facing().to_string()))
            }
            _ => None,
        })
        .collect();
    found.sort();
    found
}

#[test]
fn every_format_records_encode_and_decode_codecs() {
    let output = check_wire_program(
        r"
        import std.encoding.cbor;
        import std.encoding.json;
        import std.encoding.yaml;
        import std.encoding.toml;
        import std.encoding.msgpack;
        import std.encoding.wire;

        #[wire]
        type Point {
            x: i64 @1;
            y: i64 @2;
        }

        fn main() {
            let p = Point { x: 1, y: 2 };
            let _c: bytes = cbor.encode(p);
            let _cb: Result<Point, wire.DecodeError> = cbor.decode(_c);
            let _j: string = json.encode(p);
            let _jb: Result<Point, wire.DecodeError> = json.decode(_j);
            let _y: string = yaml.encode(p);
            let _yb: Result<Point, wire.DecodeError> = yaml.decode(_y);
            let _t: string = toml.encode(p);
            let _tb: Result<Point, wire.DecodeError> = toml.decode(_t);
            let _m: bytes = msgpack.encode(p);
            let _mb: Result<Point, wire.DecodeError> = msgpack.decode(_m);
        }
        ",
    );
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    let mut want = Vec::new();
    for format in [
        CodecFormat::Cbor,
        CodecFormat::Json,
        CodecFormat::Yaml,
        CodecFormat::Toml,
        CodecFormat::Msgpack,
    ] {
        for direction in [CodecDirection::Encode, CodecDirection::Decode] {
            want.push((Codec { format, direction }, "Point".to_string()));
        }
    }
    want.sort();
    assert_eq!(codec_rewrites(&output), want);
}

#[test]
fn per_type_wire_codec_methods_are_gone() {
    let output = check_wire_program(
        r#"
        #[wire]
        type Point {
            x: i64 @1;
        }

        fn main() {
            let p = Point { x: 1 };
            let _b = p.encode();
            let _j = p.to_json();
            let _r = Point.from_json("{}");
        }
        "#,
    );
    for method in ["encode", "to_json", "from_json"] {
        assert!(
            output
                .errors
                .iter()
                .any(|error| error.message.contains(method)),
            "`{method}` must no longer resolve on a #[wire] type: {:#?}",
            output.errors
        );
    }
    assert!(codec_rewrites(&output).is_empty());
}

#[test]
fn format_codec_admits_owned_key_and_element_shapes() {
    let output = check_wire_program(
        r#"import std.encoding.json;
import std.encoding.wire;

#[wire]
type Key {
    id: i64 @1;
}

fn main() {
    let _record_keyed: Result<HashMap<Key, string>, wire.DecodeError> = json.decode("[]");
    let _vec_bytes: Result<Vec<bytes>, wire.DecodeError> = json.decode("[]");
}
"#,
    );
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

fn codec_value_types(output: &TypeCheckOutput) -> Vec<String> {
    codec_rewrites(output)
        .into_iter()
        .map(|(_, ty)| ty)
        .collect()
}

#[test]
fn format_codec_admits_bare_wire_type() {
    let output = check_wire_program(
        r#"import std.encoding.json;

#[wire]
type Config {
    name: string @1;
    port: i64 @2;
}

fn main() {
    let c = Config { name: "svc", port: 80 };
    let _text = json.encode(c);
    let _back = json.decode<Config>("{}");
}
"#,
    );
    assert!(
        output.errors.is_empty(),
        "a #[wire] type is Serializable: {:#?}",
        output.errors
    );
    assert_eq!(codec_value_types(&output), ["Config", "Config"]);
}

#[test]
fn format_codec_records_literal_value_type_after_defaulting() {
    let output = check_wire_program(
        r"
        import std.encoding.json;

        fn main() {
            let _text = json.encode([1, 2, 3]);
        }
        ",
    );
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    assert_eq!(codec_value_types(&output), ["Vec<i64>"]);
}

#[test]
fn format_codec_named_import_records_codec_rewrite() {
    let output = check_wire_program(
        r"
        import std.encoding.json.{encode};

        fn main() {
            let _text = encode([true]);
        }
        ",
    );
    assert!(output.errors.is_empty(), "{:#?}", output.errors);
    assert_eq!(codec_value_types(&output), ["Vec<bool>"]);
}

#[test]
fn format_codec_admits_data_and_refuses_the_rest_with_a_path() {
    for (value_ty, value, refused) in [
        ("Plain", "Plain { a: 1 }", false),
        ("Vec<Plain>", "[Plain { a: 1 }]", false),
        ("(i64, string)", r#"(1, "a")"#, false),
        ("Outer", "Outer { p: Plain { a: 1 } }", false),
        ("Tree", "Tree { v: 1, kids: [] }", false),
        ("Option<Option<i64>>", "Some(Some(1))", true),
        ("Handle", "Handle { fd: 3 }", true),
        ("Vec<Holder>", "[Holder { h: Handle { fd: 3 } }]", true),
    ] {
        let output = check_wire_program(&format!(
            r"
            import std.encoding.json;

            type Plain {{ a: i64; }}

            #[wire]
            type Outer {{ p: Plain @1; }}

            #[wire]
            type Tree {{ v: i64 @1; kids: Vec<Tree> @2; }}

            #[resource]
            type Handle {{ fd: i64; }}

            impl Handle {{
                fn close(consume self) {{}}
            }}

            type Holder {{ h: Handle; }}

            fn main() {{
                let v: {value_ty} = {value};
                let _text = json.encode(v);
            }}
            "
        ));
        if !refused {
            assert!(
                output.errors.is_empty(),
                "`{value_ty}` is data: {:#?}",
                output.errors
            );
            continue;
        }
        assert!(
            output.errors.iter().any(|error| {
                error.kind == TypeErrorKind::BoundsNotSatisfied
                    && error.message.contains(&format!(
                        "type `{value_ty}` does not implement trait `Serializable`"
                    ))
                    && error
                        .suggestions
                        .iter()
                        .any(|help| help.starts_with("E_NOT_SERIALIZABLE"))
            }),
            "`{value_ty}` is not data and must be refused at check time: {:#?}",
            output.errors
        );
    }
}

#[test]
fn format_codec_bounded_type_param_is_serializable() {
    let output = check_wire_program(
        r"
        import std.encoding.json;

        fn show<T: Serializable>(values: Vec<T>) -> string {
            json.encode(values)
        }

        fn show_any<T>(values: Vec<T>) -> string {
            json.encode(values)
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
fn format_decode_without_a_value_type_is_refused() {
    let output = check_wire_program(
        r#"
        import std.encoding.json;

        fn main() {
            let _r = json.decode("[1]");
        }
        "#,
    );
    assert!(
        output
            .errors
            .iter()
            .any(|error| error.kind == TypeErrorKind::InferenceFailed
                && error.message.starts_with("E_TYPE_ANNOTATION_NEEDED")),
        "an unsettled codec value type is a check-time error: {:#?}",
        output.errors
    );
    assert!(codec_value_types(&output).is_empty());
}

#[test]
fn format_codec_is_not_a_function_value() {
    let output = check_wire_program(
        r"
        import std.encoding.json;

        fn main() {
            let _f = json.encode;
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

const TOML_PRELUDE: &str = r"
import std.encoding.toml;
import std.encoding.wire;

type Plain { a: i64; b: string; }

#[wire]
type Settings {
    host: string @1;
    port: i64 @2;
    tag: Option<string> @3 optional;
}

#[wire]
type Required {
    host: string @1;
    tag: Option<string> @2;
}
";

fn toml_errors(body: &str) -> Vec<String> {
    check_wire_program(&format!("{TOML_PRELUDE}\nfn main() {{\n{body}\n}}\n"))
        .errors
        .iter()
        .map(|error| error.message.clone())
        .collect()
}

#[test]
fn toml_refuses_values_it_cannot_represent() {
    let scalar = toml_errors("let _t = toml.encode(42);");
    assert!(
        scalar
            .iter()
            .any(|m| m.starts_with("E_FORMAT_CANNOT_REPRESENT")),
        "{scalar:?}"
    );
    let required = toml_errors(r#"let _t = toml.encode(Required { host: "h", tag: .None });"#);
    assert!(
        required
            .iter()
            .any(|m| m.starts_with("E_FORMAT_CANNOT_REPRESENT") && m.contains("tag")),
        "{required:?}"
    );
    let decode = toml_errors("let _r: Result<Required, wire.DecodeError> = toml.decode(\"\");");
    assert!(
        decode
            .iter()
            .any(|m| m.starts_with("E_FORMAT_CANNOT_REPRESENT")),
        "{decode:?}"
    );
}

#[test]
fn toml_admits_tables_and_optional_fields() {
    // Negative controls for `toml_refuses_values_it_cannot_represent`.
    for body in [
        r#"let _t = toml.encode(Settings { host: "h", port: 1, tag: .None });"#,
        r#"let _t = toml.encode(Plain { a: 1, b: "x" });"#,
        "let _r: Result<Settings, wire.DecodeError> = toml.decode(\"\");",
    ] {
        let errors = toml_errors(body);
        assert!(errors.is_empty(), "{body}: {errors:?}");
    }
}

#[test]
fn wire_text_name_collisions_fail_at_declaration() {
    let explicit = check_source(
        r#"#[wire]
type Cfg {
    #[serial(key = "x")]
    a: string @1;
    #[serial(key = "x")]
    b: string @2;
}
"#,
    );
    assert!(
        explicit.errors.iter().any(|error| {
            error.message == "E_SERIAL_KEY_COLLISION: two fields of `Cfg` share the text key `x`"
        }),
        "{:?}",
        explicit.errors
    );

    let cased = check_source(
        r#"#[wire]
#[serial(case = "camelCase")]
type Cfg {
    foo_bar: string @1;
    fooBar: string @2;
}
"#,
    );
    assert!(
        cased.errors.iter().any(|error| {
            error.message
                == "E_SERIAL_KEY_COLLISION: two fields of `Cfg` share the text key `fooBar`"
        }),
        "{:?}",
        cased.errors
    );
}

#[test]
fn wire_optional_field_requires_semantic_option_type() {
    let source = r"#[wire]
type Invalid {
    value: string @1 optional;
}

#[wire]
type RequiredValue {
    value: string @1;
}

#[wire]
type RequiredOption {
    value: Option<string> @1;
}

#[wire]
type OptionalOption {
    value: Option<string> @1 optional;
}
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
    assert_eq!(&source[error.span.clone()], "string ");
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
        r"type MaybeText = Option<string>;

#[wire]
type Message {
    body: MaybeText @1 optional;
}
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
