# std.encoding.json

JSON values own their data and clean up automatically. Assignment and field or
array extraction produce independent logical values. Mutate a `var` receiver;
`set` and `push` preserve the child supplied by the caller.

```hew
import std.encoding.json;

fn main() {
    var original = json.object();
    original.set("name", json.from_string("Hew")).expect("set succeeds");
    var edited = original;
    edited.set("name", json.from_string("Hew next")).expect("set succeeds");
    let name = original.get_field("name").expect("get_field succeeds").expect("get_field returns a value");
    println(name.get_string().expect("get_string succeeds")); // Hew
    println(edited.stringify().expect("stringify succeeds"));
}
```

`parse` returns `Result<Value, ParseError>` and `stringify` returns
`Result<string, EncodeError>`. Both errors retain the native format diagnostic.

`type_of` returns `Kind`. Scalar getters return `Result<scalar, AccessError>`;
a wrong kind never invents zero, false or an empty string. `get_int` reads exact
`i64` values and `get_u64` reads exact `u64` values; an integer outside the requested
range returns `IntegerRange`. `get_float` accepts only the float kind. Constructors
include `from_int`, `from_u64`, `from_float`, `from_string`, `from_bool` and `null`.

`get_field` and `array_get` return `Result<Option<Value>, AccessError>`:

- A missing key or element is `Ok(None)`.
- An explicit null is `Ok(Some(value))` whose kind is `Null`.
- A receiver of the wrong kind returns `WrongKind(expected, actual)`.
- An index below zero or above 2147483647 returns `InvalidIndex` before narrowing.

`array_len` is also checked. `set` requires an object and `push` requires an array.
Both return `Result<(), AccessError>` and leave the receiver and
child unchanged on an error. Successful insertion stores an independent value.

Equality compares format values, including their numeric representation, rather
than pointer identity or serialized text. Integer and float forms remain distinct;
floating signed zero compares equal. `Hash` is unavailable.

JSON objects have string keys in sorted order; duplicate parsed keys keep the
last value. `keys()` returns a checked, independent array of those keys. JSON
numbers retain the parser's i64/u64/f64 range; they are not arbitrary-precision
numbers. `from_float` returns `Result<Value, EncodeError>` and rejects infinity
and NaN with `NonFiniteFloat`.

Migration: remove data `free()` and `close()` calls. Replace consuming `with_*`
builders with `var` plus `set`, and `push_*` builders with `push` of a constructed
value. Handle checked getters and the two lookup layers explicitly. Use
`from_string` for string construction. The shared `CanonicalValueMethods` trait
has been removed; TOML currently retains its own independent resource API.

## Typed encode and decode

`json.encode(value)` writes any data value as JSON text, and
`json.decode<T>(document)` reads it back as `Result<T, wire.DecodeError>`.
`json.decode(document)` without a known `T` is `E_TYPE_ANNOTATION_NEEDED`.

- A record encodes as an object keyed by field name, in declaration order.
  `#[serial(case = "camelCase")]` on the type renames every key (also
  `PascalCase`, `snake_case`, `SCREAMING_SNAKE` and `kebab-case`), and
  `#[serial(key = "..")]` on a field sets that one key; the field key wins.
- An `Option<T>` field encodes `None` as `null`. Decoding accepts `null` or
  an absent key as `None`; every other field is required.
- A failed decode is one `wire.DecodeError`. Its text starts with the variant
  name, then the path of the value that did not fit:
  `Type: .tags[1]: expected string, found integer`,
  `Missing: .temp`, `Range: .port: 70000 does not fit u16`,
  `Syntax: line 1, column 1: expected a value`. Match the variants to act on
  the path rather than reading the text.

```hew
import std.encoding.json;
import std.encoding.wire;

#[serial(case = "camelCase")]
type Reading {
    battery_mv: i64;
    noise_floor_dbm: Option<i64>;
    #[serial(key = "temp")]
    temperature_c: f64;
    tags: Vec<string>;
}

fn describe(error: wire.DecodeError) -> string {
    match error {
        wire.DecodeError.Missing { path } => f"add {path}",
        wire.DecodeError.Type { path, expected, .. } => f"{path} must be {expected}",
        other => f"{other}",
    }
}

fn main() {
    let reading = Reading { battery_mv: 4100, noise_floor_dbm: .None, temperature_c: 21.5, tags: ["roof"] };
    let text = json.encode(reading);
    println(text); // {"batteryMv":4100,"noiseFloorDbm":null,"temp":21.5,"tags":["roof"]}
    match json.decode<Reading>(text) {
        .Ok(back) => println(back.tags[0]), // roof
        .Err(e) => println(describe(e)),
    }
    match json.decode<Reading>("{\"batteryMv\":1}") {
        .Ok(_) => println("unexpected"),
        .Err(e) => println(f"{e} / {describe(e)}"), // Missing: .temp / add .temp
    }
}
```

See the [stdlib overview](../../README.md) for other modules.
