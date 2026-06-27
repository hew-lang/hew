# std::encoding::capnproto — stub (not implemented)

**Status: NOT IMPLEMENTED.** This directory is a deliberate placeholder.
No `Cargo.toml`, no `hew.toml`, no source files. The stub exists to make
the non-choice legible in the workspace.

## What the doctrine says

The wire-format doctrine evaluated Cap'n Proto during the HBF deletion.
The zero-copy win is real, but the toolchain commitment and code-generation
surface exceeded what v0.5 could absorb. CBOR + CDDL was chosen as the
smaller-scope alternative.

Full rationale in
[`docs/specs/HEW-WIRE-FORMAT-DOCTRINE.md`](../../../docs/specs/HEW-WIRE-FORMAT-DOCTRINE.md)
§1 ("Why CBOR, not JSON / msgpack / Protobuf / HBF") and §5 §S2
("Cap'n Proto stub crate").

## What to use instead

- **Inter-process actor messaging:** you do not choose the format. The
  runtime's CBOR envelope handles it automatically.
- **User-facing wire serialisation:** see [`std::encoding::json`](../json/),
  [`std::encoding::msgpack`](../msgpack/), or
  [`std::encoding::protobuf`](../protobuf/) depending on what the other
  side speaks.

## Obsolescence markers

> **WHEN-obsolete:** when a future effort
> decides to either (a) implement Cap'n Proto codegen in earnest, or
> (b) formally close the option with a project-level ADR that buries it
> for good. Either path must update or delete this README.
>
> **WHAT-real-solution-looks-like:** a `hew-codegen-capnproto` stub
> crate (or real crate) that compiles — either to a build-time error
> message stating "Cap'n Proto codegen is not implemented; use the CBOR
> envelope substrate for inter-process messaging", or to a real
> implementation. The stub crate carries a
> `// JUSTIFIED: documented non-choice per HEW-WIRE-FORMAT-DOCTRINE §5`
> marker and a CI gate that fails if anything in the workspace builds
> against it unintentionally.

Until that effort lands, this README is the signal: **the module does not
exist, the choice was deliberate, and new code must not assume it will.**
