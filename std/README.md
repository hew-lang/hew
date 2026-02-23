# Hew Standard Library

The Hew standard library provides core types, data structures, networking, encoding, and utilities. Modules are imported with `import std::module::path`.

## Module Overview

### Core (top-level)

| Module    | Import            | Description                                     |
| --------- | ----------------- | ----------------------------------------------- |
| builtins  | _(auto-imported)_ | `println`, `print`, `sleep_ms`, `exit`, `panic` |
| string    | `std::string`     | String operations and formatting                |
| vec       | `std::vec`        | Growable vector type                            |
| option    | `std::option`     | `Option<T>` helpers (`is_some`, `unwrap_or`)    |
| result    | `std::result`     | `Result<T,E>` helpers (`is_ok`, `unwrap_or`)    |
| stream    | `std::stream`     | Byte streams, channels, `Sink`/`Stream` types   |
| fs        | `std::fs`         | File system read/write operations               |
| path      | `std::path`       | Path manipulation                               |
| os        | `std::os`         | Environment variables, platform info            |
| process   | `std::process`    | Spawn and manage child processes                |
| semaphore | `std::semaphore`  | Counting semaphore for concurrency control      |

### Encoding (`std::encoding::*`)

| Module   | Description                        |
| -------- | ---------------------------------- |
| json     | JSON parsing and serialization     |
| yaml     | YAML parsing and serialization     |
| toml     | TOML parsing and serialization     |
| csv      | CSV reading and writing            |
| msgpack  | MessagePack binary serialization   |
| protobuf | Protocol Buffers encoding/decoding |
| base64   | Base64 encode/decode               |
| hex      | Hexadecimal encode/decode          |
| compress | Zstd and gzip compression          |
| markdown | Markdown to HTML rendering         |
| wire     | Hew wire format serialization      |

### Crypto (`std::crypto::*`)

| Module   | Description                                     |
| -------- | ----------------------------------------------- |
| crypto   | SHA-256/384/512, HMAC, constant-time comparison |
| jwt      | JSON Web Token creation and verification        |
| password | Argon2 password hashing and verification        |
| encrypt  | AES-256-GCM encryption (planned)                |

### Networking (`std::net::*`)

| Module    | Description                              |
| --------- | ---------------------------------------- |
| net       | TCP listener and connection types        |
| http      | HTTP server and client                   |
| websocket | WebSocket client connections             |
| smtp      | Email sending via SMTP                   |
| url       | URL parsing and construction             |
| ipnet     | IP address and CIDR utilities            |
| mime      | MIME type detection from file extensions |

### Text (`std::text::*`)

| Module | Description                             |
| ------ | --------------------------------------- |
| regex  | Regular expression matching             |
| semver | Semantic version parsing and comparison |

### Time (`std::time::*`)

| Module   | Description                            |
| -------- | -------------------------------------- |
| datetime | Date/time formatting and arithmetic    |
| cron     | Cron expression parsing and scheduling |

### Misc (`std::misc::*`)

| Module | Description                    |
| ------ | ------------------------------ |
| log    | Structured logging with levels |
| uuid   | UUID v4 generation             |

### Bench (`std::bench`)

| Module | Description                                       |
| ------ | ------------------------------------------------- |
| bench  | Microbenchmark harness with warmup and statistics |

## Architecture

Each stdlib module consists of:

- **`.hew` stub** -- Declares the public API (types, traits, functions) that Hew programs import
- **Rust native crate** (`src/lib.rs`) -- Implements the module as `#[no_mangle] pub extern "C" fn` symbols linked into the final binary
- **`hew.toml`** -- Package metadata for the module

The compiler resolves `import std::*` paths against the `HEW_STD` directory and links the corresponding native static libraries at compile time.
