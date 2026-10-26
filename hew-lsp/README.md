# hew-lsp

Language Server Protocol implementation for Hew.

Shipped IDE/LSP features for Hew source files include:

- Real-time and pull diagnostics (parse errors, type errors)
- Completions
- Hover information
- Go-to-definition, including record field accesses at use sites
- Go-to-type-definition for checked nominal values and named types
- Go-to-implementation for traits, trait methods and nominal receivers
- Document highlights that respect binding, member and field identity
- Nested selection ranges for tokens, expressions, statements and bodies
- Find references
- Rename (with prepare support)
- Document symbols
- Workspace symbols
- Signature help
- Inlay hints
- Semantic tokens for syntax highlighting
- Code actions / quick-fixes
- Call hierarchy
- Type hierarchy (supertypes / subtypes)
- Document links for imports
- Folding ranges
- Code lenses for reference counts and test execution
- Execute command support for running Hew tests (`hew.runTest`)

Type-definition and implementation navigation use the checker’s declaration
identities and prefer unsaved buffers when locating declarations. Primitive
types without authored declarations return no location. Binding and parameter
declarations work even when unused, including destructured bindings. Implementation results
identify authored methods in the current compilation’s module graph.

Document diagnostic requests return a full report or an unchanged report when
the previous result ID still names the same diagnostics. Pulls wait for pending
edits and their dependent documents to finish analysis. Clients that advertise
diagnostic refresh support receive refresh requests after source changes.
Workspace-wide diagnostic pulls are not advertised.

## Usage

The language server is linked into the `hew` executable. Editors launch `hew`
with the `lsp` argument; no sibling executable is required.

From a source checkout, run:

```sh
make hew-native
target/debug/hew lsp
```

For editor integrations, use a matching installed Hew SDK and pass `lsp`
before language-server options such as `--pkg-path`.

See [`docs/dev/lsp-editor-setup.md`](../docs/dev/lsp-editor-setup.md) for the
build/launch/VS Code walkthrough, and [`../editors/README.md`](../editors/README.md)
for editor-specific setup notes.

## Generated SDK analysis context

Clients can register SDK-generated source with `hew/analysisContext` when
`capabilities.experimental.hewAnalysisContext` is `1`. Parameters contain
`textDocument: {uri}` for the entry and `sources: [{uri, text}]` for its exact
virtual file overlays. Replacing the list refreshes ordinary diagnostics and
navigation; an empty list removes that entry's context. Nothing is written to
the source tree. Open and pending dirty editor buffers take precedence, and
closing such a buffer restores its generated context.

Editors should use the compiler selected by the SDK and display generated
definition targets as read-only source from that same context. This keeps WIT
binding generation in the SDK and type checking in the ordinary compiler.

## Part of the Hew compiler

This crate is an internal component of the [Hew](https://github.com/hew-lang/hew) compiler toolchain.
