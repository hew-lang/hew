# std.net.url

std.net.url — URL parsing and manipulation

`encode(text)` percent-encodes UTF-8 bytes. `decode(component)` returns
`Result<string, DecodeError>` and decodes exactly once: `+` remains literal,
`%252F` becomes `%2F`, and valid decoded NUL is preserved. Malformed escapes
return `MalformedEscape(offset)`, where the offset counts bytes in the encoded
input. `InvalidUtf8(error)` reports the first bad byte in the decoded sequence.
Invalid text never silently becomes an empty string or passes through as an
undecoded escape.

`decode_query(query)` returns `Result<Vec<(string, string)>, DecodeError>` using
the same decoder, with `+` converted to space only for query/form components.
Entries without `=` are skipped; encoding errors fail the whole operation.
Escape offsets are relative to the encoded key or value. Split a URI's raw path
into segments before calling `decode`, so encoded separators stay component data.

Callers of the earlier string-returning decoder should propagate with `?`,
recover with `handle error`, or use `expect` for a known-valid literal.

Part of the [Hew](https://hew.sh) standard library. See the [std overview](../../README.md) for all modules.
