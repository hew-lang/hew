# hew-export-macro

Proc macro for annotating Hew runtime and stdlib exports with metadata.

The `#[hew_export]` attribute macro annotates `pub extern "C"` functions with structured metadata describing their Hew-visible signature (parameter types, return type, module path). This metadata is consumed by `hew-stdlib-gen` to auto-generate `.hew` stub files and type checker entries.

## Example

```rust
#[hew_export(module = "std::misc::uuid", name = "new_v4", returns = "String")]
pub extern "C" fn hew_uuid_new_v4() -> *const u8 { /* ... */ }
```

## Part of the Hew compiler

This crate is an internal component of the [Hew](https://github.com/hew-lang/hew) compiler toolchain.
