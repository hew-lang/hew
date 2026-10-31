# Codec fuzz testing

cargo-fuzz targets for `hew-codec`: the document parsers and typed walks
behind `std.encoding.wire`, and the ordinary YAML values `std.encoding.yaml`
and the sandbox share. This crate builds without the runtime, so it is the
light half of the standard-library fuzzing; the rest is in `hew-std/fuzz`.

| Target               | Input                                                              | Properties beyond "never panics"                                          |
| -------------------- | ------------------------------------------------------------------ | ------------------------------------------------------------------------- |
| `fuzz_codec_formats` | format byte, then a JSON, YAML, TOML, CBOR or MessagePack document | heap bounded; the writer's output parses; writing is a fixed point        |
| `fuzz_codec_json`    | JSON text                                                          | agrees with `serde_json` except for documented refusals                   |
| `fuzz_codec_typed`   | format byte, then a document                                       | typed decode over a shape with every node kind; encoding is a fixed point |
| `fuzz_yaml_value`    | YAML text                                                          | alias expansion stays within the heap bound; agrees with `serde_yaml`     |

A metering global allocator in `fuzz_targets/support.rs` records the peak
live heap of each parse, so a declared length or alias fan-out that
preallocates fails the run even when its pages are never touched.
Ordinary YAML permits expansion up to its absolute budget, independently of
input size. Its heap allowance includes container overhead for that budget;
the typed wire targets retain their stricter input-relative bound. Seeds
exercise both an admitted expanded tree and a refused alias bomb.

## Running

```bash
cd hew-codec/fuzz
cargo +nightly fuzz list
cargo +nightly fuzz run -s none fuzz_yaml_value corpus/fuzz_yaml_value seeds/fuzz_yaml_value -- \
  -dict=dicts/fuzz_yaml_value.dict -rss_limit_mb=1024 -timeout=10 -max_total_time=600
```

The seed corpus is checked in under `seeds/<target>/` and dictionaries
under `dicts/<target>.dict`. `seeds/make-seeds.py` is the source of the
seeds: change it and rerun it rather than editing seeds by hand, and
`python3 seeds/make-seeds.py --check` fails when the checked-in seeds no
longer match. `corpus/` and `artifacts/` are generated and ignored.
