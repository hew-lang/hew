# Legacy syntax migration corpus

This corpus proves the migrator's representative rewrite boundaries.

- `accept/typed_variants.hew` covers checker-selected contextual variants in
  expected-type expression and pattern sites.
- `accept/lexical_forms.hew` covers legacy paths and selected imports.
- `accept/turbofish_forms.hew` covers each supported legacy turbofish shape.
- `reject/unresolvable.hew` remains an unresolved call so migration must
  report the file and leave it unchanged.

Each `.expected` file is the exact formatted result after migration.
