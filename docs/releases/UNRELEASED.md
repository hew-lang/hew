# Unreleased

Native applications can supply rooted `std.fs.ReadDir` and `WriteDir` resources
once when constructing an actor or component. Ordinary `read` and `write`
methods operate on relative child paths, `subdir` narrows the root, and
`open_read` or `open_write` returns an independently owned directional file.
Closing a parent does not close its narrowed roots or opened files.

Contained relative symlinks work; absolute paths, parent escapes and escaping
symlinks refuse without acquiring outside authority. Rooted handles retain the
opened object if its pathname changes. A host may refuse renaming an open root.
Directory acquisition remains ambient and is refused transitively inside a
`#[confined]` boundary; supplied directory operations are permitted.

The browser VM can use an in-memory implementation of the same ordinary service
interface. Native directory resources and hosted filesystem grants are not
available in the browser VM.
