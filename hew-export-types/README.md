# hew-export-types

Shared type definitions for the `hew-export-macro` proc macro system.

Defines the `ExportMeta` struct and related types that describe a Hew stdlib function's signature. These types are used at compile time by the proc macro and at build time by `hew-stdlib-gen` to generate `.hew` stubs.

## Part of the Hew compiler

This crate is an internal component of the [Hew](https://github.com/hew-lang/hew) compiler toolchain.
