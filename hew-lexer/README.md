# hew-lexer

Tokenizer for the Hew programming language, built on [Logos](https://github.com/maciejhirsz/logos).

Converts Hew source text into a stream of tokens (keywords, operators, literals, identifiers) for consumption by the parser. The lexer defines `ALL_KEYWORDS` which serves as the single source of truth for keyword lists across the toolchain (editor plugins, syntax highlighting, completions).

## Usage

```rust
use hew_lexer::{Lexer, Token};

let source = r#"pub fn main() { print("hello"); }"#;
for (token, span) in Lexer::new(source) {
    println!("{token:?} @ {span:?}");
}
```

## Part of the Hew compiler

This crate is an internal component of the [Hew](https://github.com/hew-lang/hew) compiler toolchain. It is published separately to support IDE tooling and the WebAssembly playground.
