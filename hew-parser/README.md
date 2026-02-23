# hew-parser

Recursive-descent parser for the Hew programming language.

Consumes a token stream from `hew-lexer` and produces a typed AST. Supports the full Hew grammar including actors, streams, pattern matching, generics, traits, and module imports.

## Architecture

The parser is a hand-written recursive descent parser (no parser generators). This gives precise control over error recovery and diagnostic messages. The AST types are defined in `src/ast.rs` and are used downstream by the type checker, serializer, and code generator.

## Usage

```rust
use hew_parser::parse;

let source = r#"fn add(a: i32, b: i32) -> i32 { a + b }"#;
let ast = parse(source).expect("parse error");
```

## Part of the Hew compiler

This crate is an internal component of the [Hew](https://github.com/hew-lang/hew) compiler toolchain.
