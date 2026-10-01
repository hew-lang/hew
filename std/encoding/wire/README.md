# std.encoding.wire

Shared types of the data codecs, and the `#[wire]` tagged-schema layer. The
format modules encode and decode; this module holds what they share. See
[`docs/specs/HEW-WIRE-FORMAT-DOCTRINE.md`](../../../docs/specs/HEW-WIRE-FORMAT-DOCTRINE.md)
§2 for which module to reach for.

## Encoding and decoding

`std.encoding.cbor`, `json`, `yaml`, `toml` and `msgpack` each offer
`encode<T: Serializable>(value: T)` and
`decode<T: Serializable>(document) -> Result<T, wire.DecodeError>`. Text
formats read and write `string`; `cbor` and `msgpack` read and write `bytes`.
The earlier `wire.encode`, `wire.to_json`, `wire.from_json` facade and the
per-type `.encode()` / `.to_json()` / `Type.decode` methods are gone.

```hew
import std.encoding.json;

type Config {
    name: string;
    retries: i64;
}

fn main() {
    println(json.encode(Config { name: "api", retries: 3 })); // {"name":"api","retries":3}
    match json.decode<Config>("{\"name\":\"api\",\"retries\":\"x\"}") {
        .Ok(config) => println(config.name),
        .Err(e) => println(e), // Type: .retries: expected integer, found string
    }
}
```

## DecodeError

A failed decode returns `wire.DecodeError`, a variant per failure kind. Paths
start at the document root: `.field`, `[index]`, `["key"]`.

| Variant                                   | Meaning                                                                           |
| ----------------------------------------- | --------------------------------------------------------------------------------- |
| `Syntax { offset, line, column, reason }` | The input is not well-formed; binary formats leave `line` and `column` as `None`. |
| `Type { path, expected, found }`          | A value has the wrong kind for its target.                                        |
| `Missing { path }`                        | A required key is absent.                                                         |
| `Range { path, value, target }`           | A number does not fit its target type.                                            |
| `UnknownVariant { path, name }`           | A variant name or tag matches no variant.                                         |
| `Duplicate { path, key }`                 | A map key or set element repeats.                                                 |
| `Invalid { path, reason }`                | A representation override refused its representation.                             |

`DecodeError` implements `Display`.

## `#[wire]` schemas

`#[wire]` on a record or enum gives it a stable schema for remote actors: every
field and variant carries a `@N` tag, assigned once and never reused. CBOR
encodes by tag; text formats encode by field name, or by `#[serial]`
overrides.

## Field presence

Presence is independent of `Option<T>`'s null/value encoding. Required `T` and
required `Option<T>` fields always emit their map key and reject absence;
required `Option<T>::None` uses a present null. A field declared
`Option<T> @N optional` omits its key for `None`, emits it for `Some`, and
reconstructs `None` from either an absent key or an explicit null. Unknown
fields remain tolerated. `Option<Option<T>>` is outside the wire-body floor
because null cannot preserve all of its states. TOML has no null, so
`toml.encode` and `toml.decode` refuse a `#[wire]` record with a required
`Option` field (`E_FORMAT_CANNOT_REPRESENT`); mark it `optional`.
