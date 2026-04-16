# hew-wirecodec

Unified codec plan for Hew `wire` types.

This crate owns `WireCodecPlan`, the single choke point that lowers a
`WireDecl` into a structural plan that every wire codec (msgpack, JSON,
YAML) consumes. It replaces the three parallel hand-written dispatch
chains in `hew-serialize/src/msgpack.rs` and
`hew-codegen/src/mlir/MLIRGenWire.cpp` with one descriptor-driven path.

See `.tmp/plans/2026-04-16-foundations/lane-7-wire-codec-unification.md`
for the migration plan.
