# Unreleased

Changes after [v0.6.6](v0.6.6.md) belong here.

- JSON, TOML and YAML values execute in the sandbox through one format-aware
  path. Parsing and formatting use the native engines; copies, checked access,
  mutation and cleanup preserve each format's ordinary value semantics.
- QUIC hostname resolution observes connection and listener deadlines, with a
  bounded worker budget shared across node lifetimes. Numeric addresses retain
  their direct connection path.
