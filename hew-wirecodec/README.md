# hew-wirecodec

Unified codec plan for Hew `wire` types.

This crate owns `WireCodecPlan`, the single choke point that lowers a
`WireDecl` into a structural plan that every wire codec (msgpack, JSON,
YAML) consumes. The plan is the single source of truth for wire-type
encoding across the Rust IR ladder.
