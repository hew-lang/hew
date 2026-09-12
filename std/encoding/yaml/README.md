# std.encoding.yaml

YAML values own their data and clean up automatically. Assignment and field or
array extraction produce independent logical values. Mutate a `var` receiver;
`set` and `push` preserve the child supplied by the caller.

```hew
import std.encoding.yaml;

fn main() {
    var original = yaml.object();
    original.set("name", yaml.from_string("Hew")).expect("set succeeds");
    var edited = original;
    edited.set("name", yaml.from_string("Hew next")).expect("set succeeds");
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

`array_len` is also checked. `set` requires a mapping and `push` requires a YAML sequence.
Both return `Result<(), AccessError>` and leave the receiver and
child unchanged on an error. Successful insertion stores an independent value.

`mapping_keys()` returns `Result<Vec<string>, AccessError>` with every string key
in insertion order. An empty mapping produces an empty vector. A non-mapping
returns `WrongKind(Mapping, actual)`; any non-string key returns
`UnsupportedMappingKey(actual)` for the first such key, without dropping or
coercing keys. Returned strings remain independent of the mapping, which stays
usable for field access, including explicit null and nested values.

Equality compares format values, including their numeric representation, rather
than pointer identity or serialized text. Integer and float forms remain distinct;
floating signed zero compares equal. `Hash` is unavailable.

YAML preserves mapping insertion order, arbitrary value keys and tags through
parse, copy, equality and encoding. Duplicate mapping keys are rejected. The
string-keyed `get_field` and `set` surface does not provide arbitrary-key editing.
Tagged values have `Kind.Tagged`; scalar/container access and mutation return a
wrong-kind error instead of implicitly peeling the tag. Tag editing is not part
of this API. YAML supports non-finite floats, so `from_float` returns `Value`.
YAML NaNs compare equal. Numeric range remains i64/u64/f64; encoding need not
preserve lexical spelling, comments or whitespace.

Migration: remove data `free()` and `close()` calls. Replace consuming `with_*`
builders with `var` plus `set`, and `push_*` builders with `push` of a constructed
value. Handle checked getters and the two lookup layers explicitly. Use
`from_string` for string construction. The shared `CanonicalValueMethods` trait
has been removed; TOML currently retains its own independent resource API.

See the [stdlib overview](../../README.md) for other modules.
