# hew-types

Type checker for the Hew programming language.

Performs full type inference and checking on the parsed AST, including:

- Hindley-Milner-style type inference with unification
- Generics and trait bounds
- Actor message types and lifecycle checking
- Stream type propagation
- Result/Option type inference for match expressions
- Module-level type resolution across imports

## Usage

```rust
use hew_parser::parse;
use hew_types::check;

let ast = parse(source)?;
let checked = check(&ast)?;
```

## Part of the Hew compiler

This crate is an internal component of the [Hew](https://github.com/hew-lang/hew) compiler toolchain. The type-checked AST is serialized to MessagePack and passed to the MLIR code generator.
