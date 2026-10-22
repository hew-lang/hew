# Versioning

Hew is pre-1.0. The compiler version and language edition are separate:
`hew --version` identifies the compiler, while `edition = "2026"` in
`hew.toml` selects the language edition.

Development builds use the next planned release version followed by
`-dev.X+<commit>`. `X` is the number of commits since the nearest reachable Hew
release tag, such as `v0.6.5`; unrelated toolchain or backup tags do not reset it.
For example, development after v0.6.5 identifies itself as `0.6.6-dev.X`.
The workspace stores only the base version (`0.6.6`); the build derives the
development count and commit. A clean build at the matching release tag reports
the plain release version. Accurate counts require the release tags and their
history to be available in the checkout.

The 0.6 series develops edition 2026 through feature and maintenance releases.
A patch release can add language or standard-library features and can include
intentional surface changes with exact migration guidance. A package using
an addition declares its minimum compiler version, for example
`hew = ">=0.6.3"`. Edition 2026 is still under development; a shared edition
does not make every newer feature available in an older compiler.

The [language specification](specs/HEW-SPEC-2026.md#editions-and-release-alignment)
owns the version and edition contract. Contributor release guidance lives in
the [versioning policy](internal/versioning.md) and
[release runbook](release-runbook.md).

Use the [v0.6.6 release notes](releases/v0.6.6.md) when upgrading a released
installation. Changes after that tag belong in
[Unreleased](releases/UNRELEASED.md), rather than being attributed to v0.6.6.
