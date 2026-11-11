# Setext heading parser backport

Hew enables heading attributes and definition lists together. In pulldown-cmark
0.13.5, an attribute-only line inside a setext heading can make the parser
truncate beyond its last text node and panic. This copy backports the merged
[upstream fix #1183](https://github.com/pulldown-cmark/pulldown-cmark/pull/1183)
(commit `862d8eb83c5fa7f37003cd42112924767e449c4f`) to the published 0.13.5
crate. The fix is absent from the 0.13.6 maintenance release.

The changes are in `src/firstpass.rs`, `tests/suite/heading_attrs.rs`, and
`tests/suite/regression.rs`. The source patch uses an equivalent byte-slice
match for upstream's `LineEnding::at_end` helper, which is absent from 0.13.5.
All other source files and the MIT license are from the published crate.

The root workspace, standard-library fuzz workspace, and CLI fuzz workspace
patch crates.io to this directory. When a compatible maintenance release
includes #1183, update all three dependency resolutions, remove their patches
and this directory, and retain Hew's Markdown regressions.
