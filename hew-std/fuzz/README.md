# Standard-library fuzz testing

cargo-fuzz targets for the Rust half of the standard library: every parser
and codec in `hew-std` that reads untrusted bytes or text without a network
or filesystem. The typed wire codecs and YAML values live in
`hew-codec/fuzz`, which builds without the runtime and is much lighter.

| Target               | Input                                    | Properties beyond "never panics"                                           |
| -------------------- | ---------------------------------------- | -------------------------------------------------------------------------- |
| `fuzz_value_formats` | JSON, YAML or TOML text                  | stringify then parse gives an equal value                                  |
| `fuzz_msgpack`       | MessagePack bytes                        | agrees with `rmp_serde`; only the depth limit refuses more; round trip     |
| `fuzz_protobuf`      | protobuf wire bytes                      | getters safe on every field; encoding is a fixed point                     |
| `fuzz_xml`           | XML text                                 | navigation safe; printed text parses back and prints identically           |
| `fuzz_compress`      | gzip, deflate or zlib                    | output never exceeds the caller's limit; agrees with `flate2`; round trip  |
| `fuzz_markdown`      | Markdown                                 | safe mode emits only generator tags and attributes and allowed URL schemes |
| `fuzz_websocket`     | upgrade request or response, then frames | Hew's caps hold; only offered subprotocols are accepted                    |
| `fuzz_jwt`           | token and secret                         | decode, validate and inspect agree; encode keeps the claims                |
| `fuzz_regex`         | pattern, subject, replacement            | queries agree with each other                                              |
| `fuzz_calendar`      | instant, text, strftime pattern          | components rebuild the instant; lossless pattern round trip                |
| `fuzz_net_text`      | URL, IP, CIDR, UUID text                 | URL printing is a fixed point; IP, CIDR and UUID agree with their crates   |

HTTP request, response and chunked parsing belong to `tiny_http` and `ureq`,
which offer no in-memory entry point; they are not fuzzed here.

## Running

```bash
cd hew-std/fuzz
cargo +nightly fuzz list
cargo +nightly fuzz build -s none
cargo +nightly fuzz run -s none fuzz_xml corpus/fuzz_xml seeds/fuzz_xml -- \
  -dict=dicts/fuzz_xml.dict -rss_limit_mb=1024 -timeout=10 -max_total_time=600
```

The seed corpus is checked in under `seeds/<target>/` and dictionaries
under `dicts/<target>.dict`. `seeds/make-seeds.py` is the source of the
seeds: change it and rerun it rather than editing seeds by hand, and
`python3 seeds/make-seeds.py --check` fails when the checked-in seeds no
longer match. `corpus/` and `artifacts/` are generated and ignored.
The `fuzzing` feature of `hew-std` exposes the WebSocket caps the harness
must share with production; no shipped build enables it.

hew-runtime installs the global allocator, so memory is bounded through the
RSS limit. Every FFI result is released through its documented contract,
so steady RSS growth during a run is a leak worth reporting.
