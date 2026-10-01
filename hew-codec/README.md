# hew-codec

The one serialization engine behind Hew's `json`, `yaml`, `toml`, `msgpack`
and `cbor` modules. A compiled per-type walk drives a `Sink` with structural
events and pulls from a `Source`; each format only spells those events.
Map and set output is in canonical order, duplicate keys are refused on every
input, and every `DecodeError` carries the path of the failing value.
