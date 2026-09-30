//! Round trips and refusals through the event ABI, driven the way a compiled
//! walk drives it: a shape per type, one encode walk and one decode walk.

use hew_codec::{DecodeError, Format, Member, Sink, Source, Table, Value};

const ALL: [Format; 5] = [
    Format::Json,
    Format::Yaml,
    Format::Toml,
    Format::Msgpack,
    Format::Cbor,
];

/// A decoded Hew value, as a walk would build it.
#[derive(Debug, Clone, PartialEq)]
enum D {
    I(i64),
    U(u64),
    F(f64),
    B(bool),
    S(String),
    C(char),
    Bytes(Vec<u8>),
    Opt(Option<Box<D>>),
    Seq(Vec<D>),
    Map(Vec<(D, D)>),
    Rec(Vec<D>),
    Var(usize, Option<Box<D>>),
    Any(Value),
}

/// A type's data shape, as the checker would publish it.
#[derive(Debug, Clone, Copy)]
enum Shape {
    I64,
    U16,
    U64,
    F64,
    Bool,
    Str,
    Char,
    Bytes,
    Any,
    Opt(&'static Shape),
    Vec(&'static Shape),
    Set(&'static Shape),
    Map(&'static Shape, &'static Shape),
    Tuple(&'static [Shape]),
    Rec(&'static Table<'static>, &'static [Shape]),
    Enum(&'static Table<'static>, &'static [Option<Shape>]),
    /// Resolved lazily so recursive types terminate.
    Lazy(fn() -> Shape),
}

fn encode(shape: Shape, value: &D, sink: &mut Sink<'static>) {
    match (shape, value) {
        (Shape::Lazy(resolve), _) => encode(resolve(), value, sink),
        (Shape::I64, D::I(v)) => sink.i64(*v),
        (Shape::U16 | Shape::U64, D::U(v)) => sink.u64(*v),
        (Shape::F64, D::F(v)) => sink.f64(*v),
        (Shape::Bool, D::B(v)) => sink.bool(*v),
        (Shape::Str, D::S(v)) => sink.str(v),
        (Shape::Char, D::C(v)) => sink.str(&v.to_string()),
        (Shape::Bytes, D::Bytes(v)) => sink.bytes(v),
        (Shape::Any, D::Any(v)) => sink.value(v),
        (Shape::Opt(_), D::Opt(None)) => sink.null(),
        (Shape::Opt(inner), D::Opt(Some(v))) => encode(*inner, v, sink),
        (Shape::Vec(elem), D::Seq(items)) => {
            sink.seq_begin(items.len());
            for item in items {
                encode(*elem, item, sink);
            }
            sink.seq_end();
        }
        (Shape::Set(elem), D::Seq(items)) => {
            sink.set_begin(items.len());
            for item in items {
                encode(*elem, item, sink);
            }
            sink.seq_end();
        }
        (Shape::Tuple(elems), D::Seq(items)) => {
            sink.seq_begin(items.len());
            for (elem, item) in elems.iter().zip(items) {
                encode(*elem, item, sink);
            }
            sink.seq_end();
        }
        (Shape::Map(k, v), D::Map(entries)) => {
            sink.map_begin(entries.len(), matches!(k, Shape::Str));
            for (key, value) in entries {
                encode(*k, key, sink);
                encode(*v, value, sink);
            }
            sink.map_end();
        }
        (Shape::Rec(table, fields), D::Rec(values)) => {
            sink.record_begin(*table);
            for (index, (field, value)) in fields.iter().zip(values).enumerate() {
                if table.members[index].has(Member::SKIP) {
                    continue;
                }
                sink.field(index);
                encode(*field, value, sink);
            }
            sink.record_end();
        }
        (Shape::Enum(table, payloads), D::Var(index, payload)) => {
            sink.variant(*table, *index);
            if let (Some(shape), Some(payload)) = (payloads[*index], payload) {
                encode(shape, payload, sink);
            }
            sink.variant_end();
        }
        (shape, value) => panic!("test walk: {value:?} is not a {shape:?}"),
    }
}

fn decode(shape: Shape, source: &mut Source<'static>) -> Result<D, DecodeError> {
    Ok(match shape {
        Shape::Lazy(resolve) => decode(resolve(), source)?,
        Shape::I64 => D::I(source.read_i64(i64::MIN, i64::MAX)?),
        Shape::U16 => D::U(source.read_u64(u16::MAX.into())?),
        Shape::U64 => D::U(source.read_u64(u64::MAX)?),
        Shape::F64 => D::F(source.read_f64()?),
        Shape::Bool => D::B(source.read_bool()?),
        Shape::Str => D::S(source.read_str()?),
        Shape::Char => D::C(source.read_char()?),
        Shape::Bytes => D::Bytes(source.read_bytes()?),
        Shape::Any => D::Any(source.read_any()?),
        Shape::Opt(inner) => {
            if source.is_null()? {
                D::Opt(None)
            } else {
                D::Opt(Some(Box::new(decode(*inner, source)?)))
            }
        }
        Shape::Vec(elem) => {
            source.seq_begin()?;
            let mut items = Vec::new();
            while source.seq_next()? {
                items.push(decode(*elem, source)?);
            }
            D::Seq(items)
        }
        Shape::Set(elem) => {
            source.set_begin()?;
            let mut items = Vec::new();
            while source.seq_next()? {
                items.push(decode(*elem, source)?);
            }
            D::Seq(items)
        }
        Shape::Tuple(elems) => {
            source.tuple_begin(elems.len())?;
            let mut items = Vec::new();
            for elem in elems {
                assert!(source.seq_next()?);
                items.push(decode(*elem, source)?);
            }
            assert!(!source.seq_next()?);
            D::Seq(items)
        }
        Shape::Map(k, v) => {
            source.map_begin(matches!(k, Shape::Str))?;
            let mut entries = Vec::new();
            while source.map_next()? {
                let key = decode(*k, source)?;
                entries.push((key, decode(*v, source)?));
            }
            D::Map(entries)
        }
        Shape::Rec(table, fields) => {
            source.record_begin(*table)?;
            let mut values = vec![None; fields.len()];
            while let Some(index) = source.record_next()? {
                values[index] = Some(decode(fields[index], source)?);
            }
            D::Rec(
                values
                    .into_iter()
                    .map(|v| v.expect("every member handed out"))
                    .collect(),
            )
        }
        Shape::Enum(table, payloads) => {
            let index = source.variant(*table)?;
            let payload = match payloads[index] {
                Some(shape) => Some(Box::new(decode(shape, source)?)),
                None => None,
            };
            source.variant_end()?;
            D::Var(index, payload)
        }
    })
}

fn to_bytes(format: Format, shape: Shape, value: &D) -> Vec<u8> {
    let mut sink = Sink::new(format);
    encode(shape, value, &mut sink);
    sink.finish()
}

fn from_bytes(format: Format, shape: Shape, input: &[u8]) -> Result<D, DecodeError> {
    decode(shape, &mut Source::new(format, input)?)
}

fn json(shape: Shape, value: &D) -> String {
    String::from_utf8(to_bytes(Format::Json, shape, value)).unwrap()
}

fn from_json(shape: Shape, text: &str) -> Result<D, DecodeError> {
    from_bytes(Format::Json, shape, text.as_bytes())
}

/// Maps and sets come back in canonical order; compare them as collections.
fn normalize(value: &D) -> D {
    match value {
        D::Seq(items) => D::Seq(items.iter().map(normalize).collect()),
        D::Map(entries) => {
            let mut entries: Vec<(D, D)> = entries
                .iter()
                .map(|(k, v)| (normalize(k), normalize(v)))
                .collect();
            entries.sort_by_key(|(k, _)| format!("{k:?}"));
            D::Map(entries)
        }
        D::Rec(values) => D::Rec(values.iter().map(normalize).collect()),
        D::Opt(Some(v)) => D::Opt(Some(Box::new(normalize(v)))),
        D::Var(i, Some(v)) => D::Var(*i, Some(Box::new(normalize(v)))),
        other => other.clone(),
    }
}

fn sorted_set(items: &[D]) -> D {
    let mut items = items.to_vec();
    items.sort_by_key(|item| format!("{item:?}"));
    D::Seq(items)
}

// ── The shapes of a realistic configuration ────────────────────────────────

const fn m(key: &'static str, tag: u64, flags: u32) -> Member<'static> {
    Member { key, tag, flags }
}

static PAIR: Table<'static> = Table {
    name: "Pair",
    members: &[m("left", 1, 0), m("right", 2, 0)],
    tagged: false,
};
// A generic `Pair<i64>`: the instantiation's fields are the shape.
const PAIR_I64: Shape = Shape::Rec(&PAIR, &[Shape::I64, Shape::I64]);

static TREE: Table<'static> = Table {
    name: "Tree",
    members: &[m("label", 1, 0), m("children", 2, 0)],
    tagged: false,
};
fn tree() -> Shape {
    Shape::Rec(&TREE, &[Shape::Str, Shape::Vec(&Shape::Lazy(tree))])
}

static RESULT: Table<'static> = Table {
    name: "Result",
    members: &[m("Ok", 0, Member::PAYLOAD), m("Err", 1, Member::PAYLOAD)],
    tagged: false,
};
const RESULT_I64_STR: Shape = Shape::Enum(&RESULT, &[Some(Shape::I64), Some(Shape::Str)]);

static RECT: Table<'static> = Table {
    name: "Rect",
    members: &[m("w", 1, 0), m("h", 2, 0)],
    tagged: false,
};
static FIGURE: Table<'static> = Table {
    name: "Figure",
    members: &[
        m("Circle", 1, Member::PAYLOAD),
        m("Rect", 2, Member::PAYLOAD),
        m("Line", 3, Member::PAYLOAD),
        m("Empty", 4, 0),
    ],
    tagged: false,
};
const FIGURE_SHAPE: Shape = Shape::Enum(
    &FIGURE,
    &[
        Some(Shape::F64),
        Some(Shape::Rec(&RECT, &[Shape::F64, Shape::F64])),
        Some(Shape::Tuple(&[Shape::I64, Shape::I64])),
        None,
    ],
);

static CONFIG: Table<'static> = Table {
    name: "Config",
    members: &[
        m("serviceName", 1, 0),
        m("port", 2, 0),
        m("tags", 3, 0),
        m("limits", 4, 0),
        m("ids", 5, 0),
        m("codes", 6, 0),
        m("pair", 7, 0),
        m("tree", 8, 0),
        m("outcome", 9, 0),
        m("origin", 10, 0),
        m("figures", 11, 0),
        m("blob", 12, 0),
        m("grade", 13, 0),
        m("timeout", 14, Member::ACCEPT_ABSENT),
        m("cached", 15, Member::SKIP | Member::ACCEPT_ABSENT),
        m("extra", 16, 0),
    ],
    tagged: false,
};
fn config_shape() -> Shape {
    Shape::Rec(
        &CONFIG,
        &[
            Shape::Str,
            Shape::U16,
            Shape::Vec(&Shape::Str),
            Shape::Map(&Shape::Str, &Shape::I64),
            Shape::Set(&Shape::I64),
            Shape::Map(&Shape::I64, &Shape::Str),
            PAIR_I64,
            Shape::Lazy(tree),
            RESULT_I64_STR,
            Shape::Tuple(&[Shape::I64, Shape::F64, Shape::Bool]),
            Shape::Vec(&FIGURE_SHAPE),
            Shape::Bytes,
            Shape::Char,
            Shape::Opt(&Shape::I64),
            Shape::Opt(&Shape::F64),
            Shape::Any,
        ],
    )
}

fn s(text: &str) -> D {
    D::S(text.to_owned())
}

fn leaf(label: &str) -> D {
    D::Rec(vec![s(label), D::Seq(vec![])])
}

fn config() -> D {
    D::Rec(vec![
        s("api"),
        D::U(8080),
        D::Seq(vec![s("edge"), s("blue")]),
        D::Map(vec![(s("rps"), D::I(500)), (s("burst"), D::I(-20))]),
        D::Seq(vec![D::I(30), D::I(4), D::I(-1)]),
        D::Map(vec![(D::I(404), s("missing")), (D::I(200), s("ok"))]),
        D::Rec(vec![D::I(1), D::I(-2)]),
        D::Rec(vec![
            s("root"),
            D::Seq(vec![
                leaf("a"),
                D::Rec(vec![s("b"), D::Seq(vec![leaf("c")])]),
            ]),
        ]),
        D::Var(1, Some(Box::new(s("refused")))),
        D::Seq(vec![D::I(3), D::F(0.5), D::B(true)]),
        D::Seq(vec![
            D::Var(0, Some(Box::new(D::F(1.5)))),
            D::Var(1, Some(Box::new(D::Rec(vec![D::F(2.0), D::F(3.25)])))),
            D::Var(2, Some(Box::new(D::Seq(vec![D::I(0), D::I(9)])))),
            D::Var(3, None),
        ]),
        D::Bytes(vec![0, 1, 2, 250, 255]),
        D::C('é'),
        D::Opt(None),
        D::Opt(None),
        D::Any(Value::Map(vec![(
            Value::Str("hook".into()),
            Value::Seq(vec![Value::Bool(true), Value::Int(7)]),
        )])),
    ])
}

#[test]
fn config_round_trips_through_every_format() {
    let shape = config_shape();
    let value = config();
    for format in ALL {
        let bytes = to_bytes(format, shape, &value);
        let back = from_bytes(format, shape, &bytes)
            .unwrap_or_else(|e| panic!("{format:?}: {e}\n{}", String::from_utf8_lossy(&bytes)));
        let D::Rec(fields) = &back else { panic!() };
        let D::Rec(expected) = &value else { panic!() };
        for (index, (got, want)) in fields.iter().zip(expected).enumerate() {
            let (got, want) = if index == 4 {
                let (D::Seq(g), D::Seq(w)) = (got, want) else {
                    panic!()
                };
                (sorted_set(g), sorted_set(w))
            } else {
                (normalize(got), normalize(want))
            };
            assert_eq!(got, want, "{format:?} field {}", CONFIG.members[index].key);
        }
        // Output is deterministic: encoding the decoded value is byte-identical.
        assert_eq!(
            to_bytes(format, shape, &back),
            bytes,
            "{format:?} is not canonical"
        );
    }
}

#[test]
fn json_spells_each_shape_as_designed() {
    let text = json(config_shape(), &config());
    assert_eq!(
        text,
        concat!(
            r#"{"serviceName":"api","port":8080,"tags":["edge","blue"],"#,
            r#""limits":{"burst":-20,"rps":500},"ids":[-1,30,4],"#,
            r#""codes":[[200,"ok"],[404,"missing"]],"pair":{"left":1,"right":-2},"#,
            r#""tree":{"label":"root","children":[{"label":"a","children":[]},{"label":"b","children":[{"label":"c","children":[]}]}]},"#,
            r#""outcome":{"Err":"refused"},"origin":[3,0.5,true],"#,
            r#""figures":[{"Circle":1.5},{"Rect":{"w":2.0,"h":3.25}},{"Line":[0,9]},"Empty"],"#,
            r#""blob":"AAEC+v8=","grade":"é","timeout":null,"extra":{"hook":[true,7]}}"#
        )
    );
}

#[test]
fn json_numbers_are_exact_and_non_finite_floats_are_strings() {
    let values = D::Seq(vec![
        D::F(f64::NAN),
        D::F(f64::INFINITY),
        D::F(f64::NEG_INFINITY),
        D::F(0.1),
    ]);
    let text = json(Shape::Vec(&Shape::F64), &values);
    assert_eq!(text, r#"["NaN","Infinity","-Infinity",0.1]"#);
    let D::Seq(back) = from_json(Shape::Vec(&Shape::F64), &text).unwrap() else {
        panic!()
    };
    assert!(matches!(back[0], D::F(v) if v.is_nan()));
    assert_eq!(
        back[1..],
        [D::F(f64::INFINITY), D::F(f64::NEG_INFINITY), D::F(0.1)]
    );
    // A non-finite spelling is a float only where a float is expected.
    assert!(matches!(from_json(Shape::Str, r#""NaN""#), Ok(D::S(_))));
    assert!(matches!(
        from_json(Shape::I64, r#""NaN""#),
        Err(DecodeError::Type { .. })
    ));

    assert_eq!(json(Shape::U64, &D::U(u64::MAX)), "18446744073709551615");
    assert_eq!(
        from_json(Shape::U64, "18446744073709551615").unwrap(),
        D::U(u64::MAX)
    );
    assert_eq!(
        from_json(Shape::U64, "18446744073709551616"),
        Err(DecodeError::Range {
            path: String::new(),
            value: "18446744073709551616".into(),
            target: "u64".into(),
        })
    );
    assert_eq!(
        from_json(Shape::I64, "1.5"),
        Err(DecodeError::Type {
            path: String::new(),
            expected: "integer".into(),
            found: "float".into(),
        })
    );
    assert!(matches!(
        from_json(config_shape(), &json(config_shape(), &config()).replace("8080", "70000")),
        Err(DecodeError::Range { ref path, ref target, .. }) if path == ".port" && target == "u16"
    ));
}

#[test]
fn errors_name_the_failing_path() {
    let shape = config_shape();
    let good = json(shape, &config());

    let wrong = good.replace(r#"["edge","blue"]"#, r#"["edge",7]"#);
    let err = from_json(shape, &wrong).unwrap_err();
    assert_eq!(
        err.to_string(),
        "Type: .tags[1]: expected string, found integer"
    );

    let wrong = good.replace(r#"{"Rect":{"w":2.0,"h":3.25}}"#, r#"{"Rect":{"w":2.0}}"#);
    assert_eq!(
        from_json(shape, &wrong).unwrap_err(),
        DecodeError::Missing {
            path: ".figures[1].Rect.h".into()
        }
    );

    let wrong = good.replace(r#""Empty""#, r#""Hexagon""#);
    let err = from_json(shape, &wrong).unwrap_err();
    assert_eq!(
        err.to_string(),
        "UnknownVariant: .figures[3]: no variant named Hexagon"
    );

    let wrong = good.replace(r#""rps":500"#, r#""rps":"many""#);
    assert_eq!(
        from_json(shape, &wrong).unwrap_err().to_string(),
        r#"Type: .limits["rps"]: expected integer, found string"#
    );

    let wrong = good.replace(r#"[[200,"ok"],"#, "[[200],");
    assert_eq!(
        from_json(shape, &wrong).unwrap_err().to_string(),
        "Type: .codes[0]: expected [key, value] pair, found sequence"
    );

    let wrong = good.replace(r#""blob":"AAEC+v8=""#, r#""blob":"not base64!""#);
    assert_eq!(
        from_json(shape, &wrong).unwrap_err().to_string(),
        "Type: .blob: expected base64 string, found string"
    );

    let wrong = good.replace(r#""origin":[3,0.5,true]"#, r#""origin":[3]"#);
    assert_eq!(
        from_json(shape, &wrong).unwrap_err().to_string(),
        "Type: .origin: expected sequence of 3, found sequence of 1"
    );

    // A missing required field; the absent `timeout` and skipped `cached` are fine.
    let wrong = good.replace(r#""port":8080,"#, "");
    assert_eq!(
        from_json(shape, &wrong).unwrap_err(),
        DecodeError::Missing {
            path: ".port".into()
        }
    );
}

#[test]
fn unknown_and_skipped_keys_are_ignored_and_absent_options_read_none() {
    let shape = config_shape();
    let good = json(shape, &config());
    let lenient = good.replace(r#""timeout":null,"#, "").replacen(
        '{',
        r#"{"future":{"x":[1]},"cached":2.5,"#,
        1,
    );
    assert_ne!(lenient, good);
    let back = from_json(shape, &lenient).expect("unknown keys must be tolerated");
    assert_eq!(
        normalize(&back),
        normalize(&from_json(shape, &good).unwrap())
    );
    let D::Rec(fields) = back else { panic!() };
    assert_eq!(fields[14], D::Opt(None), "a skipped field decodes as None");
}

#[test]
fn syntax_errors_carry_positions() {
    assert_eq!(
        from_json(Shape::I64, "{\n  \"a\" 1}").unwrap_err(),
        DecodeError::Syntax {
            offset: 8,
            line: Some(2),
            column: Some(7),
            reason: "expected `:`".into(),
        }
    );
    assert_eq!(
        from_json(Shape::I64, "1 2").unwrap_err().to_string(),
        "Syntax: line 1, column 3: trailing characters after the value"
    );
    let err = from_bytes(Format::Yaml, Shape::I64, b"a: [1, 2\nb: 3\n").unwrap_err();
    assert!(
        matches!(
            err,
            DecodeError::Syntax {
                line: Some(_),
                column: Some(_),
                ..
            }
        ),
        "{err:?}"
    );
    let err = from_bytes(Format::Toml, Shape::I64, b"a = \n").unwrap_err();
    assert!(
        matches!(err, DecodeError::Syntax { line: Some(1), .. }),
        "{err:?}"
    );
    let err = from_bytes(Format::Cbor, Shape::I64, &[0x82, 0x01]).unwrap_err();
    assert!(
        matches!(
            err,
            DecodeError::Syntax {
                line: None,
                column: None,
                ..
            }
        ),
        "{err:?}"
    );
    let err = from_bytes(Format::Cbor, Shape::I64, &[0x01, 0x02]).unwrap_err();
    assert_eq!(
        err.to_string(),
        "Syntax: offset 1: trailing bytes after the value"
    );
}

#[test]
fn every_format_refuses_duplicate_keys() {
    let map = Shape::Map(&Shape::Str, &Shape::I64);
    let cases: [(Format, &[u8]); 5] = [
        (Format::Json, br#"{"a":1,"a":2}"#),
        (Format::Yaml, b"a: 1\na: 2\n"),
        (Format::Toml, b"a = 1\na = 2\n"),
        // {"a": 1, "a": 2}
        (Format::Cbor, &[0xa2, 0x61, b'a', 0x01, 0x61, b'a', 0x02]),
        (Format::Msgpack, &[0x82, 0xa1, b'a', 0x01, 0xa1, b'a', 0x02]),
    ];
    for (format, input) in cases {
        let err = from_bytes(format, map, input).unwrap_err();
        assert!(
            matches!(err, DecodeError::Syntax { .. }),
            "{format:?}: {err:?}"
        );
    }
    // The negative control: distinct keys decode in each format.
    let control: [(Format, &[u8]); 5] = [
        (Format::Json, br#"{"a":1,"b":2}"#),
        (Format::Yaml, b"a: 1\nb: 2\n"),
        (Format::Toml, b"a = 1\nb = 2\n"),
        (Format::Cbor, &[0xa2, 0x61, b'a', 0x01, 0x61, b'b', 0x02]),
        (Format::Msgpack, &[0x82, 0xa1, b'a', 0x01, 0xa1, b'b', 0x02]),
    ];
    for (format, input) in control {
        assert!(from_bytes(format, map, input).is_ok(), "{format:?}");
    }
    // The JSON offset is the second occurrence.
    assert!(matches!(
        from_json(map, r#"{"a":1,"a":2}"#),
        Err(DecodeError::Syntax { offset: 7, .. })
    ));
    // Pair-form maps and sets repeat nothing either.
    let pairs = Shape::Map(&Shape::I64, &Shape::Str);
    assert_eq!(
        from_json(pairs, r#"[[1,"a"],[1,"b"]]"#)
            .unwrap_err()
            .to_string(),
        "Duplicate: 1 repeats"
    );
    assert_eq!(
        from_json(Shape::Set(&Shape::I64), "[3,1,3]")
            .unwrap_err()
            .to_string(),
        "Duplicate: 3 repeats"
    );
}

#[test]
fn every_format_bounds_nesting_at_the_same_depth() {
    fn nested(depth: usize) -> Value {
        (0..depth).fold(Value::Int(1), |inner, _| Value::Seq(vec![inner]))
    }
    for format in [Format::Json, Format::Yaml, Format::Msgpack, Format::Cbor] {
        for (depth, accepted) in [
            (hew_codec::MAX_DEPTH, true),
            (hew_codec::MAX_DEPTH + 1, false),
        ] {
            let mut sink = Sink::new(format);
            sink.value(&nested(depth));
            let bytes = sink.finish();
            let result = Source::new(format, &bytes);
            assert_eq!(
                result.is_ok(),
                accepted,
                "{format:?} at depth {depth}: {result:?}"
            );
        }
    }
}

// ── Tagged (`#[wire]`) types and presence ───────────────────────────────────

static PACKET: Table<'static> = Table {
    name: "Packet",
    members: &[
        m("id", 1, 0),
        m("note", 3, Member::ACCEPT_ABSENT | Member::OMIT_NULL),
        m("reply_to", 2, 0),
    ],
    tagged: true,
};
const PACKET_SHAPE: Shape = Shape::Rec(
    &PACKET,
    &[Shape::I64, Shape::Opt(&Shape::Str), Shape::Opt(&Shape::I64)],
);

static EVENT: Table<'static> = Table {
    name: "Event",
    members: &[m("Joined", 1, Member::PAYLOAD), m("Idle", 7, 0)],
    tagged: true,
};
const EVENT_SHAPE: Shape = Shape::Enum(&EVENT, &[Some(Shape::Str), None]);

#[test]
fn tagged_types_use_tags_in_binary_formats_and_keys_in_text() {
    let packet = D::Rec(vec![D::I(5), D::Opt(None), D::Opt(None)]);
    // Ascending tag order, the optional None omitted, the required None present.
    assert_eq!(
        to_bytes(Format::Cbor, PACKET_SHAPE, &packet),
        [0xa2, 0x01, 0x05, 0x02, 0xf6]
    );
    assert_eq!(json(PACKET_SHAPE, &packet), r#"{"id":5,"reply_to":null}"#);
    let full = D::Rec(vec![
        D::I(5),
        D::Opt(Some(Box::new(s("hi")))),
        D::Opt(Some(Box::new(D::I(9)))),
    ]);
    assert_eq!(
        to_bytes(Format::Cbor, PACKET_SHAPE, &full),
        [0xa3, 0x01, 0x05, 0x02, 0x09, 0x03, 0x62, b'h', b'i']
    );
    for format in ALL {
        let bytes = to_bytes(format, PACKET_SHAPE, &full);
        assert_eq!(
            from_bytes(format, PACKET_SHAPE, &bytes).unwrap(),
            full,
            "{format:?}"
        );
    }
    // A required Option must be present, even as null.
    assert_eq!(
        from_bytes(Format::Cbor, PACKET_SHAPE, &[0xa1, 0x01, 0x05]).unwrap_err(),
        DecodeError::Missing {
            path: ".reply_to".into()
        }
    );
    assert_eq!(
        from_json(PACKET_SHAPE, r#"{"id":5,"reply_to":null}"#).unwrap(),
        packet
    );

    assert_eq!(
        to_bytes(Format::Cbor, EVENT_SHAPE, &D::Var(1, None)),
        [0x07]
    );
    assert_eq!(
        to_bytes(
            Format::Msgpack,
            EVENT_SHAPE,
            &D::Var(0, Some(Box::new(s("x"))))
        ),
        [0x81, 0x01, 0xa1, b'x']
    );
    assert_eq!(json(EVENT_SHAPE, &D::Var(1, None)), r#""Idle""#);
    assert_eq!(
        from_bytes(Format::Cbor, EVENT_SHAPE, &[0x08])
            .unwrap_err()
            .to_string(),
        "UnknownVariant: no variant named 8"
    );
    assert_eq!(
        from_json(EVENT_SHAPE, r#""Joined""#)
            .unwrap_err()
            .to_string(),
        "Type: expected Joined with a payload, found string"
    );
    assert_eq!(
        from_json(EVENT_SHAPE, r#"{"Idle":1}"#)
            .unwrap_err()
            .to_string(),
        "Type: expected unit variant Idle, found map"
    );
}

#[test]
fn canonical_order_is_per_format() {
    let map = Shape::Map(&Shape::Str, &Shape::I64);
    let value = D::Map(vec![(s("b"), D::I(1)), (s("aa"), D::I(2))]);
    // Text sorts by the JSON spelling; MessagePack by its encoding, whose
    // length prefix puts shorter strings first; CBOR sorts shorter keys first.
    assert_eq!(json(map, &value), r#"{"aa":2,"b":1}"#);
    assert_eq!(
        to_bytes(Format::Cbor, map, &value),
        [0xa2, 0x61, b'b', 0x01, 0x62, b'a', b'a', 0x02]
    );
    assert_eq!(
        to_bytes(Format::Msgpack, map, &value),
        [0x82, 0xa1, b'b', 0x01, 0xa2, b'a', b'a', 0x02]
    );
}

static MAYBE_PAIR: Table<'static> = Table {
    name: "MaybePair",
    members: &[m("left", 1, 0), m("right", 2, Member::ACCEPT_ABSENT)],
    tagged: false,
};

#[test]
fn text_formats_spell_the_same_document() {
    let shape = Shape::Rec(&MAYBE_PAIR, &[Shape::I64, Shape::Opt(&Shape::I64)]);
    let value = D::Rec(vec![D::I(1), D::Opt(None)]);
    assert_eq!(
        String::from_utf8(to_bytes(Format::Yaml, shape, &value)).unwrap(),
        "left: 1\nright: null\n"
    );
    // TOML has no null: the key is omitted and reads back as None.
    let toml = to_bytes(Format::Toml, shape, &value);
    assert_eq!(String::from_utf8(toml.clone()).unwrap(), "left = 1\n");
    assert_eq!(from_bytes(Format::Toml, shape, &toml).unwrap(), value);
    let floats = Shape::Rec(&PAIR, &[Shape::F64, Shape::F64]);
    let value = D::Rec(vec![D::F(f64::INFINITY), D::F(-0.5)]);
    assert_eq!(
        String::from_utf8(to_bytes(Format::Toml, floats, &value)).unwrap(),
        "left = inf\nright = -0.5\n"
    );
    assert_eq!(
        String::from_utf8(to_bytes(Format::Yaml, floats, &value)).unwrap(),
        "left: .inf\nright: -0.5\n"
    );
}

#[test]
fn decode_error_display_covers_every_variant() {
    let cases = [
        (
            DecodeError::Syntax {
                offset: 1,
                line: Some(1),
                column: Some(2),
                reason: "expected a value".into(),
            },
            "Syntax: line 1, column 2: expected a value",
        ),
        (
            DecodeError::Syntax {
                offset: 9,
                line: None,
                column: None,
                reason: "malformed CBOR".into(),
            },
            "Syntax: offset 9: malformed CBOR",
        ),
        (
            DecodeError::Type {
                path: ".tags[1]".into(),
                expected: "string".into(),
                found: "integer".into(),
            },
            "Type: .tags[1]: expected string, found integer",
        ),
        (
            DecodeError::Missing {
                path: ".port".into(),
            },
            "Missing: .port",
        ),
        (
            DecodeError::Range {
                path: ".port".into(),
                value: "70000".into(),
                target: "u16".into(),
            },
            "Range: .port: 70000 does not fit u16",
        ),
        (
            DecodeError::UnknownVariant {
                path: ".shape".into(),
                name: "Hex".into(),
            },
            "UnknownVariant: .shape: no variant named Hex",
        ),
        (
            DecodeError::Duplicate {
                path: ".ids".into(),
                key: "3".into(),
            },
            "Duplicate: .ids: 3 repeats",
        ),
        (
            DecodeError::Invalid {
                path: ".total".into(),
                reason: "NotDecimal: twelve".into(),
            },
            "Invalid: .total: NotDecimal: twelve",
        ),
    ];
    for (error, text) in cases {
        assert_eq!(error.to_string(), text);
    }
}
