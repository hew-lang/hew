# hew-std-encoding-wire

`std::encoding::wire` holds low-level wire helpers plus the canonical opaque
`Value` contract shared by the stdlib encoding modules.

## Why `Value` stays opaque

Issue #1247 settled the stdlib `Value` surface on opaque handles. JSON, TOML,
and YAML can align on methods and tag numbering without exposing the backing
runtime representation, so callers do not couple themselves to per-encoding
layout details.

## Why `view()` is deferred

A future structural `view()` seam needs explicit codegen support. Per #1247's
opaque-handle decision, this package cannot introduce an ad-hoc exposed view
type without also teaching the compiler/runtime boundary how to materialize it
without leaking internals.

## Canonical tag numbering

The shared prefix is:

- `0 = null`
- `1 = bool`
- `2 = int/i64`
- `3 = float/f64`
- `4 = string`
- `5 = array`
- `6 = object`

Encoding-specific variants start at `7+`. TOML uses `7` for datetime-like
values. MessagePack currently bridges through JSON (`from_json` / `to_json`),
so it follows the same canonical ordering when it crosses the opaque `Value`
surface indirectly.
