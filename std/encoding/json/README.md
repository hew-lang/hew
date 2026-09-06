# std.encoding.json

JSON values own their data and clean up automatically. Assignment and field or
array extraction produce independent logical values. Mutate a `var` receiver;
`set` and `push` preserve the child supplied by the caller.

```hew
import std.encoding.json;

fn main() {
    var original = json.object();
    original.set("name", json.from_string("Hew")).unwrap();
    var edited = original;
    edited.set("name", json.from_string("Hew next")).unwrap();
    let name = original.get_field("name").unwrap().unwrap();
    println(name.get_string().unwrap()); // Hew
    println(edited.stringify().unwrap());
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

See the [stdlib overview](../../README.md) for other modules.
