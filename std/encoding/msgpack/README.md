# std.encoding.msgpack

std.encoding.msgpack — MessagePack encoding and decoding

Part of the [Hew](https://hew.sh) standard library. See the [std overview](../../README.md) for all modules.

For the project-wide decision on which encoding module to use at which layer, see
[`docs/specs/HEW-WIRE-FORMAT-DOCTRINE.md`](../../../docs/specs/HEW-WIRE-FORMAT-DOCTRINE.md)
§2 ("User-facing wire formats — stdlib encoding modules").

## Typed encode and decode

`msgpack.encode(value)` writes any data value as MessagePack bytes, and
`msgpack.decode<T>(document)` reads it back as `Result<T, wire.DecodeError>`. A
failed decode names the path of the value that did not fit. `msgpack.decode(document)`
without a known `T` is `E_TYPE_ANNOTATION_NEEDED`.

```hew
import std.encoding.msgpack;

type Config {
    name: string;
    retries: i64;
}

fn main() {
    let document = msgpack.encode(Config { name: "api", retries: 3 });
    match msgpack.decode<Config>(document) {
        .Ok(config) => println(config.name), // api
        .Err(e) => println(e),
    }
}
```
