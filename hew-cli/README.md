# hew-cli

The Hew programming language compiler driver.

## Usage

```sh
hew build file.hew [-o output]    # Compile to executable
hew run file.hew [-- args...]     # Compile and run
hew check file.hew                # Parse + typecheck only
hew doc file.hew                  # Generate documentation
hew eval "expr"                   # Evaluate an expression
hew test file.hew                 # Run tests
hew wire check file.hew --against baseline.hew
                                  # Validate wire compatibility
hew fmt file.hew                  # Format source code
hew init [name]                   # Initialize a new project
hew completions <shell>           # Generate shell completions
hew version                       # Print version info
```

`hew file.hew` is shorthand for `hew build file.hew`.
