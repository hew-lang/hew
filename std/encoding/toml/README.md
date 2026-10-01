# std.encoding.toml

std.encoding.toml — TOML parsing and generation

Part of the [Hew](https://hew.sh) standard library. See the [std overview](../../README.md) for all modules.

## Typed encode and decode

`toml.encode(value)` writes any data value as TOML text, and
`toml.decode<T>(document)` reads it back as `Result<T, wire.DecodeError>`. A
failed decode names the path of the value that did not fit. `toml.decode(document)`
without a known `T` is `E_TYPE_ANNOTATION_NEEDED`.

```hew
import std.encoding.toml;

type Config {
    name: string;
    retries: i64;
}

fn main() {
    let document = toml.encode(Config { name: "api", retries: 3 });
    match toml.decode<Config>(document) {
        .Ok(config) => println(config.name), // api
        .Err(e) => println(e),
    }
}
```
