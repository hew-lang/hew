# Syntax migration corpus

This corpus proves the migrator's representative rewrite boundaries. Every
rewrite is syntactic; nothing is type-checked during migration.

- `accept/lexical_forms.input` covers legacy paths and selected imports.
- `accept/turbofish_forms.input` covers each supported legacy turbofish shape.
- `reject/removed_glob.hew` has a removed glob import, which has no current
  spelling, so migration must report the file and leave it unchanged.

Each `.expected` file is the exact formatted result after migration.

Accepted inputs use `.input` so routine Hew formatting cannot erase the legacy
syntax under test. The Make target copies them to temporary `.hew` files before
migration, checks the exact result and successful type checking, then requires
a byte-identical second pass and successful check mode.
