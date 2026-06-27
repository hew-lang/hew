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
        struct Point { x: i64 @1, y: i64 @2 }

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
        struct Point { x: i64 @1, y: i64 @2 }

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
fn wire_from_json_returns_result_self_string() {
    // A `#[wire]` type's `from_json`/`from_yaml` static parsers are fallible
    // (arbitrary user input — config files, HTTP bodies), so they return
    // `Result<Self, string>`, matching the non-wire `Encode` path. This pins
    // the ratified return shape on the wire registration path.
    let output = check_source(
        r#"
        #[wire]
        struct Point { x: i64 @1, y: i64 @2 }

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
        struct Point { x: i64 @1, y: i64 @2 }

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
        struct Point { x: i64 @1, y: i64 @2 }
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
        enum Status { Active; Inactive; }
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
        struct Cfg { host: string @1 json_name="hostname" }
        "#,
    );

    assert!(output.errors.is_empty(), "type errors: {:?}", output.errors);
    let entry = output
        .wire_layouts
        .get("Cfg")
        .expect("Cfg should have a wire layout entry");
    assert_eq!(entry.fields[0].json_name, Some("hostname".to_string()));
}
