# std.encoding.toml

std.encoding.toml — TOML parsing and generation

Part of the [Hew](https://hew.sh) standard library. See the [std overview](../../README.md) for all modules.

For typed data, prefer `toml.decode<Service>(text)` (below). `Value` is for
documents whose shape is not known until run time.

## Values

`toml.Value` is an owning TOML value with automatic cleanup. Assignment and
extraction produce independent copies. Lookups and getters are checked:

- `type_of()` returns a `Kind`: `Bool`, `Integer`, `Float`, `String`,
  `Datetime`, `Array` or `Table`. TOML has no null.
- `get_field(key)` returns `Result<Option<Value>, AccessError>`: a missing key
  is `Ok(.None)`, and a receiver that is not a table is
  `Err(AccessError.WrongKind(Kind.Table, actual))`.
- `get_bool`, `get_int`, `get_float`, `get_string` and `get_datetime` return
  `Result<_, AccessError>` and never substitute `false`, `0` or `""` for a
  value of another kind. An integer does not read as a float.
- `get_datetime()` returns the TOML lexical form of an offset or local
  date-time, a local date, or a local time.
- `keys()` returns an independent Array of the table's keys, in sorted order.
- `array_len()` and `array_get(index)`; a missing element is `Ok(.None)` and a
  negative or oversized index is `AccessError.InvalidIndex`.
- `set(key, child)` and `push(child)` mutate a `var` receiver and insert an
  independent copy of the child. A failed kind check leaves both unchanged.
- `stringify()` returns `Result<string, EncodeError>`. Only a table can be a
  document root.

```hew
import std.encoding.toml;

type Service {
    name: string;
    port: i64;
    debug: bool;
}

fn read_service(text: string) -> Service {
    let doc = toml.parse(text).expect("service.toml must parse");
    let server = match doc.get_field("server").expect("document is a table") {
        .Some(table) => table,
        .None => panic("missing [server]"),
    };
    let name = server.get_field("name").expect("server is a table")
        .expect("server.name is required").get_string().expect("server.name is a string");
    let port = match server.get_field("port").expect("server is a table") {
        .Some(value) => value.get_int().expect("server.port is an integer"),
        .None => 8080,
    };
    let debug = match server.get_field("debug").expect("server is a table") {
        .Some(value) => value.get_bool().expect("server.debug is a boolean"),
        .None => false,
    };
    Service { name: name, port: port, debug: debug }
}

fn main() {
    let service = read_service("[server]\nname = \"api\"\nport = 9000\n");
    println(f"{service.name}:{service.port} debug={service.debug}"); // api:9000 debug=false

    var doc = toml.table();
    var server = toml.table();
    server.set("name", toml.from_string("api")).expect("table");
    doc.set("server", server).expect("table");
    println(doc.stringify().expect("table root")); // [server]\nname = "api"\n
}
```

Migration from the earlier resource API:

| Old                  | New                            |
| -------------------- | ------------------------------ |
| `free()` / `close()` | delete                         |
| `get_bool() == 1`    | `get_bool()?`                  |
| `type_of() == 6`     | `type_of() == Kind.Table`      |
| `with_string(k, v)`  | `set(k, toml.from_string(v))?` |
| `push_int(n)`        | `push(toml.from_int(n))?`      |
| `toml.stringify(v)`  | `v.stringify()`                |

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
