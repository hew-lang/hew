# Hew Standard Library

The Hew standard library provides core types, data structures, networking, encoding, and utilities.

This file is the canonical index of shipped stdlib modules in this repository. Module links below go to the public `.hew` surface that Hew programs import.

For the curated language-only playground snippets, start with [`../examples/playground/manifest.json`](../examples/playground/manifest.json) and [`../examples/README.md`](../examples/README.md). Use this README when you need the canonical index of shipped `std::*` modules.

## Builtins — auto-imported, plain function calls

`println`, `print`, `sleep_ms`, `exit`, and `panic` are **ordinary function calls** auto-imported into every Hew file — no `!` suffix, no special syntax.

```hew
fn main() {
    print("count: ");
    println(42);
}
```

All other standard library modules require an explicit import at the top of the file:

```hew
import std::fs;
import std::encoding::json;

fn main() {
    let raw = fs.read("data.json");
    println(json.parse(raw));
}
```

## Quick wayfinding

- **CLI, files, and OS** — [`std::io`](io.hew), [`std::fs`](fs.hew), [`std::path`](path.hew), [`std::os`](os.hew), [`std::process`](process.hew)
- **Collections and scans** — [`std::vec`](vec.hew), [`std::deque`](deque.hew), [`std::collections::hashset`](collections/hashset/hashset.hew), [`std::iter`](iter.hew), [`std::sort`](sort/sort.hew)
- **Streams and coordination** — [`std::stream`](stream.hew), [`std::channel::channel`](channel/channel.hew), [`std::semaphore`](semaphore.hew)
- **Data formats and wire protocols** — [`std::encoding::json`](encoding/json/json.hew), [`std::encoding::yaml`](encoding/yaml/yaml.hew), [`std::encoding::toml`](encoding/toml/toml.hew), [`std::encoding::csv`](encoding/csv/csv.hew), [`std::encoding::xml`](encoding/xml/xml.hew), [`std::encoding::wire`](encoding/wire/wire.hew)
- **Networking** — [`std::net`](net/net.hew), [`std::net::http`](net/http/http.hew), [`std::net::dns`](net/dns/dns.hew), [`std::net::tls`](net/tls/tls.hew), [`std::net::quic`](net/quic/quic.hew), [`std::net::url`](net/url/url.hew)
- **Testing and perf** — [`std::testing`](testing/testing.hew), [`std::bench`](bench/bench.hew)

## Shipped module index

Every shipped module under `std/` should appear here.

### Core and formatting

| Module | Import | Use for |
| --- | --- | --- |
| [`builtins`](builtins.hew) | _(auto-imported)_ | `println`, `print`, `sleep_ms`, `exit`, and `panic` |
| [`string`](string.hew) | `std::string` | String conversion and manipulation utilities |
| [`fmt`](fmt/fmt.hew) | `std::fmt` | Number formatting, padding, and repetition helpers |
| [`option`](option.hew) | `std::option` | Helper functions for common `Option<T>` patterns |
| [`result`](result.hew) | `std::result` | Helper functions for common `Result<T, E>` patterns |
| [`math`](math/math.hew) | `std::math` | Integer helpers, float ops, and common constants |

### Files, OS, and processes

| Module | Import | Use for |
| --- | --- | --- |
| [`io`](io.hew) | `std::io` | stdin, stdout, and stderr helpers |
| [`fs`](fs.hew) | `std::fs` | File system operations |
| [`path`](path.hew) | `std::path` | File path and glob utilities |
| [`os`](os.hew) | `std::os` | Operating system interfaces |
| [`process`](process.hew) | `std::process` | Process execution |

### Collections, iteration, and concurrency

| Module | Import | Use for |
| --- | --- | --- |
| [`vec`](vec.hew) | `std::vec` | Utility helpers for `Vec<T>` |
| [`deque`](deque.hew) | `std::deque` | Double-ended queue operations |
| [`hashset`](collections/hashset/hashset.hew) | `std::collections::hashset` | Hash set collection |
| [`iter`](iter.hew) | `std::iter` | Map/filter/fold-style helpers for `Vec<T>` |
| [`sort`](sort/sort.hew) | `std::sort` | Sorting and reversing vector helpers |
| [`stream`](stream.hew) | `std::stream` | Typed `Stream<T>`/`Sink<T>` pipes and file streams |
| [`channel`](channel/channel.hew) | `std::channel::channel` | Bounded MPSC channels |
| [`semaphore`](semaphore.hew) | `std::semaphore` | Counting semaphore for concurrency control |

### Encoding and wire formats

| Module | Import | Use for |
| --- | --- | --- |
| [`base64`](encoding/base64/base64.hew) | `std::encoding::base64` | Base64 encoding and decoding |
| [`compress`](encoding/compress/compress.hew) | `std::encoding::compress` | Compression and decompression |
| [`csv`](encoding/csv/csv.hew) | `std::encoding::csv` | CSV parsing |
| [`hex`](encoding/hex/hex.hew) | `std::encoding::hex` | Hexadecimal encoding and decoding |
| [`json`](encoding/json/json.hew) | `std::encoding::json` | JSON parsing and manipulation |
| [`markdown`](encoding/markdown/markdown.hew) | `std::encoding::markdown` | Markdown to HTML conversion |
| [`msgpack`](encoding/msgpack/msgpack.hew) | `std::encoding::msgpack` | MessagePack serialization |
| [`protobuf`](encoding/protobuf/protobuf.hew) | `std::encoding::protobuf` | Protocol Buffers message construction |
| [`toml`](encoding/toml/toml.hew) | `std::encoding::toml` | TOML parsing and generation |
| [`wire`](encoding/wire/wire.hew) | `std::encoding::wire` | Hew wire format encoding and decoding |
| [`xml`](encoding/xml/xml.hew) | `std::encoding::xml` | XML parsing and manipulation |
| [`yaml`](encoding/yaml/yaml.hew) | `std::encoding::yaml` | YAML parsing and generation |

### Crypto

| Module | Import | Use for |
| --- | --- | --- |
| [`crypto`](crypto/crypto/crypto.hew) | `std::crypto::crypto` | Cryptographic hashing and utilities |
| [`encrypt`](crypto/encrypt/encrypt.hew) | `std::crypto::encrypt` | Symmetric encryption and decryption |
| [`jwt`](crypto/jwt/jwt.hew) | `std::crypto::jwt` | JSON Web Token encoding and validation |
| [`password`](crypto/password/password.hew) | `std::crypto::password` | Password hashing and verification |

### Networking

| Module | Import | Use for |
| --- | --- | --- |
| [`net`](net/net.hew) | `std::net` | TCP listeners and connections |
| [`dns`](net/dns/dns.hew) | `std::net::dns` | DNS hostname resolution |
| [`http`](net/http/http.hew) | `std::net::http` | HTTP server and request/response handling |
| [`http_client`](net/http/http_client.hew) | `import std::net::http` + `http_client.*` | Outbound HTTP GET/POST helpers |
| [`ipnet`](net/ipnet/ipnet.hew) | `std::net::ipnet` | IP address and CIDR utilities |
| [`mime`](net/mime/mime.hew) | `std::net::mime` | MIME type detection |
| [`quic`](net/quic/quic.hew) | `std::net::quic` | QUIC transport for internode messaging |
| [`smtp`](net/smtp/smtp.hew) | `std::net::smtp` | SMTP client for sending email |
| [`tls`](net/tls/tls.hew) | `std::net::tls` | TLS client connections |
| [`url`](net/url/url.hew) | `std::net::url` | URL parsing |
| [`websocket`](net/websocket/websocket.hew) | `std::net::websocket` | WebSocket client and server support |

### Text, time, and utilities

| Module | Import | Use for |
| --- | --- | --- |
| [`regex`](text/regex/regex.hew) | `std::text::regex` | Regular expression matching |
| [`semver`](text/semver/semver.hew) | `std::text::semver` | Semantic version parsing and comparison |
| [`datetime`](time/datetime/datetime.hew) | `std::time::datetime` | Date and time operations |
| [`cron`](time/cron/cron.hew) | `std::time::cron` | Cron expression parsing and scheduling |
| [`log`](misc/log/log.hew) | `std::misc::log` | Structured logging |
| [`uuid`](misc/uuid/uuid.hew) | `std::misc::uuid` | UUID generation and validation |

### Testing and benchmarking

| Module | Import | Use for |
| --- | --- | --- |
| [`testing`](testing/testing.hew) | `std::testing` | Assertion helpers for Hew tests |
| [`bench`](bench/bench.hew) | `std::bench` | Benchmark harness for measuring function performance |

## Architecture

Each stdlib module includes:

- **`.hew` surface** -- Declares the public API (types, traits, functions) that Hew programs import, including Hew-native wrappers that can lift raw native status codes/messages into `Result` and enum surfaces like `IoError`
- **`hew.toml`** -- Package metadata for the module
- **Rust crate** (`src/lib.rs`, when present) -- Either exports native `#[no_mangle] pub extern "C" fn` symbols or remains as a placeholder static library to keep the workspace and `hew-lib` build graph intact

Implementation strategy varies by module:

- Some modules are implemented entirely in Hew, with the Rust crate retained only as a build placeholder
- Some modules, such as `std::math`, expose a Hew surface that codegen lowers directly to compiler intrinsics/MLIR ops, again leaving the Rust crate as a placeholder
- Modules that need native capabilities still use their Rust crates for the final linked implementation

The compiler resolves `import std::*` paths against the `HEW_STD` directory and links any corresponding native static libraries required by the build graph at compile time, whether they provide runtime FFI symbols or act only as placeholders.
