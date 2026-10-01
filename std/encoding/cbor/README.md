# std.encoding.cbor

CBOR (RFC 8949) encoding of any data value.

```hew
import std.encoding.cbor;

type Point {
    x: i64;
    y: i64;
}

fn main() {
    let data = cbor.encode(Point { x: 1, y: 2 });
    match cbor.decode<Point>(data) {
        .Ok(p) => println(p.x + p.y), // 3
        .Err(e) => println(e),
    }
}
```

`encode<T: Serializable>(value: T) -> bytes` accepts scalars, records, enums,
tuples, `Vec`, `HashMap`, `HashSet` and `Option` of serializable values.
`decode<T: Serializable>(data: bytes) -> Result<T, wire.DecodeError>` returns an
`Err` that names the path of the value that did not fit, for example
`Type: .tags[1]: expected string, found integer`; malformed input never traps.
The target type is part of the call: `cbor.decode(data)` without a known `T` is
`E_TYPE_ANNOTATION_NEEDED`.

A `#[wire]` record or enum encodes by its `@N` tags, the same body actors send
to remote peers. Other records encode as maps keyed by field name. See
[`std.encoding.wire`](../wire/README.md) for `DecodeError` and the schema rules.

See the [stdlib overview](../../README.md) for other modules.
