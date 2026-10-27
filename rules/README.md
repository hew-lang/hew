# Structural lint rules

The repository uses pinned ast-grep rules for Rust and Hew, configured in
`sgconfig.yml`. Run `make structural-lint-bootstrap` once to acquire the pinned
tool and grammar, then run `make structural-lint` for subsequent scans.
Error-severity findings fail the gate; other severities are advisory.

- `rust/authority/` protects compiler identity and type-fact authorities.
- `rust/fail-closed/` guards compiler boundaries against silent failure.
- `rust/concurrency-drop/` checks concurrency and resource lifecycle patterns.
- `rust/panics-nyi/` and `rust/hygiene/` check panic, unsafe and debug patterns.
- `hew/` reports source-level idioms using the pinned Hew grammar.

Rules are structural, not type-aware. Check advisory findings in context before
applying a suggestion. Rules with an automatic fix restrict the matched shape;
review the resulting change as usual. Some error rules support a preceding
`// JUSTIFIED:` annotation with a rationale. ast-grep also supports
`// ast-grep-ignore` or `// ast-grep-ignore: <rule-id>` on the preceding line.
