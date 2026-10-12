# Compile-time inputs

`include_str!` and `include_bytes!` copy a file into a compiled program as a
constant string or byte value. They use paths relative to the source file
containing the invocation.

```hew
const SCHEMA: string = include_str!("sql/schema.sql");
const LOGO: bytes = include_bytes!("assets/logo.png");

fn main() {
    println(SCHEMA);
    println(LOGO.len());
}
```

An include takes one string literal. It can appear in an expression or a
`const` initializer. The file must stay inside the package root, even through
`..` or a symbolic link. The package root is the nearest directory containing
`hew.toml`, or the source file's directory for a standalone program. Absolute
paths are refused. A dependency can include its own files.

`include_str!` requires UTF-8 and reports the first invalid byte during
compilation. `include_bytes!` accepts arbitrary bytes. Changing or removing
the original file after compilation does not change the executable.

`stringify!` renders an expression as canonical Hew source without resolving
or evaluating it:

```hew
fn main() {
    println(stringify!(price  +  tax)); // price + tax
}
```

The compiler records each input's identity and SHA-256 fingerprint. Missing
inputs are retained as dependencies so editors can recheck when they appear.
The LSP watches input changes; hovering over an included path shows its name
and size, and go-to-definition opens the file. Publishing checks the package
through the same frontend and rejects an archive that excludes a consumed
input.

`location!()` returns a `SourceLocation` record with `file: string`,
`line: i64` and `column: i64`. Its file is relative to the package root;
line and column start at one, and columns count Unicode characters. A
constant retains its definition's location when used elsewhere.
`SourceLocation` keeps source positions distinct from distributed actor
`Location` values.

```hew
fn main() {
    let here = location!();
    println(here.file);
    println(here.line);
}
```

`embed!` decodes a JSON, YAML or TOML file during compilation. The expected
type supplies its schema, including `#[serial]` field names and skipped
fields, using the same decoder as `json.decode`, `yaml.decode` and
`toml.decode`.

```hew
type Config {
    host: string;
    port: u16;
    labels: Vec<string>;
}

const DEFAULTS: Config = embed!("config/defaults.toml");

fn main() {
    let local: Config = embed!("config/local.json");
    println(f"{local.host}:{local.port}");
    println(DEFAULTS.labels[0]);
}
```

The filename must end in `.json`, `.yaml`, `.yml` or `.toml`. An invocation
needs a concrete expected type, such as a binding annotation, a function
parameter type or a return type. Data errors name the consumed file and
failing field or element; syntax errors also retain their source position.
Map keys and set elements with user-defined equality require runtime
decoding because compilation cannot execute Hew methods. Structurally
derived keys are supported, and duplicate keys or elements are rejected.
The result contains ordinary Hew values and needs no decoder at runtime.
Embedded files follow the same path, tracking and publication rules as
included files.

An aggregate constant produces an independent value at each use. Passing
`DEFAULTS` to a consuming parameter or changing a local collection taken
from it leaves subsequent uses of `DEFAULTS` intact. A closure can refer to
the constant without capturing a local owner.

Set `[package] hew` in `hew.toml` to the compiler version requirement your
package needs, for example `hew = ">=0.6.1"`. The shared frontend checks this
requirement before parsing source, including manifests supplied to browser
analysis and compilation.

Browser analysis and sandbox compilation accept an optional
`Map<string, string | Uint8Array>` of package-relative files. They use the
same frontend and include rules as native compilation. Browser inputs never
fall back to the host filesystem or a network request.

```javascript
const files = new Map([["assets/greeting.txt", "Hello from Hew\n"]]);
const result = JSON.parse(
  compileToSandboxBytecode(
    'fn main() { print(include_str!("assets/greeting.txt")); }',
    "sandbox-vm-export",
    files,
  ),
);
```
