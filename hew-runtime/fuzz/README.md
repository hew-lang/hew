# Runtime Fuzz Testing

Fuzz targets for the bytes a Hew node reads from its peers, using cargo-fuzz /
libFuzzer. The targets reach crate-private decoders and the control-frame
handler through the `fuzzing` feature (`hew_runtime::fuzzing`), which no
release build enables. Builds use `default-features = false`, so tokio, quinn,
rustls and hyper stay out of the fuzz binaries.

## Targets

| Target                 | Input                                                                            | Property                                                                                                       |
| ---------------------- | -------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------- |
| `fuzz_wire_frames`     | first byte picks a decoder from `fuzzing::DECODERS`; the rest is its input       | no panic; live heap at most 1 KiB per input byte plus 1 MiB; an accepted value is a fixed point of its encoder |
| `fuzz_control_handler` | sequence of `[peer u8][len u16 BE][wire frame]` deliveries to an in-process node | no panic; member table never grows; no incarnation moves backwards; at most `2 * peers + 2` sends per frame    |

The decoders cover `decode_wire_frame`, every control payload (SWIM, registry
gossip, monitor, link, setup result, service offer), the node handshake, and
the checked-protocol records from `protocol/node_controls.hew` in all five
codec formats. The round trip asserts `decode(encode(decode(x))) == decode(x)`
and stable re-encoding; non-canonical accepted input is allowed.

The fuzz binaries install a counting global allocator (the runtime's own
allocator is off under `fuzzing`). Without a sanitizer libFuzzer's
`-malloc_limit_mb` is inert and an untouched reservation does not raise RSS, so
the live-heap bound is what catches a decoder that trusts a declared length.

## Running

```bash
cd hew-runtime
cargo fuzz list
cargo fuzz build -s none
cargo fuzz run -s none fuzz_wire_frames fuzz/corpus/fuzz_wire_frames fuzz/seeds/fuzz_wire_frames \
  -- -max_total_time=900 -rss_limit_mb=1024 -timeout=10 -close_fd_mask=2 \
  -dict=fuzz/dictionaries/wire.dict
```

libFuzzer writes new inputs to the first corpus directory, which is ignored by
git. `-close_fd_mask=2` silences the cluster's stderr diagnostics; crash
reports still print. `fuzz/dictionaries/wire.dict` carries the CBOR control
headers, length headers and checked-protocol field names for both targets.

## Seeds

`fuzz/seeds/<target>/` holds seeds built from the runtime's own encoders,
addressed with the control harness's authenticated peer identities. The `seeds`
test fails when a committed seed no longer matches its encoder; regenerate
them with:

```bash
cd hew-runtime/fuzz
HEW_FUZZ_WRITE_SEEDS=1 cargo test --test seeds
```
