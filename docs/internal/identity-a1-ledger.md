# Identity batch A1: behaviour-change ledger

Every leaf or spelling comparison lane A1 replaced with an identity
comparison, and every intentional behaviour change, by file. Paths are
relative to the repository root; line numbers are at the commit that made the
change and drift afterwards.

## Carrier swap (named types carry a `TypeHead`)

- `hew-types/src/unify.rs` Named arm: `an == bn || Ty::names_match_qualified(an, bn)`
  becomes `ha == hb` (rule R5). Two named types unify only when their heads
  are one identity; a bare and a qualified spelling of different declarations
  no longer unify by leaf.
- `hew-types/src/check/resolution.rs` `nominal_owner_conflict` and
  `nominal_owner_conflict_on_unification_path`, and
  `hew-types/src/check/coerce.rs` `reject_nominal_owner_conflict` with the
  guards in `try_unify_with_owner_identity`,
  `try_unify_invariant_with_owner_identity` and
  `try_unify_inference_with_owner_identity`: deleted. R5 makes the collision
  they detected unrepresentable.
- `hew-types/src/check/registration/traits.rs` `canonicalize_type_identity`
  and `hew-types/src/check/registration/mod.rs` `TraitSigCanonCtx`: deleted.
  Impl and trait signatures compare by head after alias normalization.
- `hew-types/src/traits.rs` `is_type_param_placeholder`: only a parameter head
  is a placeholder; the spelling heuristic fallback is deleted (R6).
- `hew-types/src/ty.rs` `substitute_named_param`, `substitute_named_params_parallel`,
  `mentions_named_param`; `hew-hir/src/lower/substitution.rs` `substitute_ty`;
  `hew-hir/src/monomorph.rs` `substitute_type_params`; `hew-sir/src/lower.rs`
  `declared_type_param_name`: a binder is substituted by parameter head, never
  a nominal of the same spelling (R6).
- `hew-types/src/check/expressions/stack_hints.rs` `classify_ty`: literal
  `"Vec"`/`"HashMap"`/`"HashSet"`/`"Rc"` spellings become builtin heads.
- `hew-types/src/runtime_call/value_kinds.rs` `carries_builtin_identity`: the
  leaf comparison against the builtin's canonical name is deleted.
- `hew-types/src/value_class.rs`: `builtin.or_else(lookup_builtin_type(name))`
  deleted; an actor head classes `BitCopy` directly.
- `hew-types/src/actor_delivery.rs`: the `*_TYPE` string constants and
  `SendPolicy::witness_name` become `KnownDecl` heads; `request_parts`
  returns the handler's nominal head.
- `hew-types/src/check/mod.rs` `resolve_member_ty`: the bare-name
  qualification `rewrite_name` is deleted; heads name their declaration.
- `hew-hir/src/lower/types.rs` `qualify_current_module_record_ty`: resolved
  heads pass through with their opacity restored; only unresolved spellings
  are qualified.
- `hew-types/src/check/items.rs` `is_canonical_std_named_type`,
  `is_canonical_lifecycle_source_type`: by head.
- `hew-types/src/check/expressions/synthesize_control.rs` struct-literal
  coercion: the two arms that let a bare construction name adopt a
  module-qualified expected type by leaf (`crate::short_name(expected) ==
path`) are deleted; a literal meets its expected type only when its path
  resolves through `Scope` to the same head (R5, w4c).
- `hew-types/src/check/generics.rs` `lookup_trait_method_inner` and
  `lookup_trait_method_with_origin_inner`: a trait method's signature is
  resolved in the trait's declaring module and file, not the caller's.

## Intentional source-level behaviour changes

- A struct literal written with a bare name resolves that name in scope. A
  module type reached only through its module binding must be written
  qualified (`pipeline.PipelineItemI64 { .. }`); the bare spelling no longer
  adopts the expected type's declaration by leaf. Test programs updated:
  `hew-cli/tests/pipeline_stdlib_ownership_oracle.rs`,
  `hew-cli/tests/actor_fault_isolation_e2e.rs`,
  `tests/hew/pipeline_i64_two_stage_test.hew`,
  `tests/core-acceptance/cases/pipeline-run-actor-handles.hew`.
- `hew-types/src/check/nominal_identity.rs` `named_ty_for_key`: a trait
  written in type position names the trait's `DefTable` row as a nominal head,
  so `canonicalize_actor_handles` turns a handler trait (`ConnectionHandler`)
  into its actor handle by identity. WHY: `TraitRef` is not yet the carrier of
  a trait in type position. WHEN: commit 3 carries traits as `TraitRef`. WHAT:
  the dyn and handler-trait positions read the trait id from `Scope`. The one
  visible change is diagnostic text: a prelude trait renders by its path
  (`std.builtins.Display`).
- `hew-types/src/check/methods/collections.rs` `dispatch_pattern_to_ty`: a
  receiver spelling that is an in-scope type parameter is that binder
  (TRANSITION(A1 commit 3): the catalog move keys dispatch by head).
- `hew-types/src/vec_authority.rs` `classify_element_with`: a `Vec` of actor
  handles takes the pointer lane on the `Actor` head;
  `BuiltinType::lowers_as_pointer_vec_element` is deleted.
- Patterns that matched `builtin: Some(_)` also match `TypeHead::Actor(_)`
  (queue-element refusal in `collections.rs` and `hew-hir/src/lower/literals.rs`,
  marker bounds in `generics.rs`, dyn table naming in `coerce.rs`, structural
  receivers in `methods/dispatch.rs`); patterns that matched `builtin: None`
  match `Nominal | Param | Unresolved` until commit 3 deletes `Unresolved`.
- `hew-hir/src/lower/ctx.rs` `checked_member_ty` and
  `hew-hir/src/lower/expr.rs` struct-literal record identity: a declaration's
  binders are parameter heads and the record identity is the checker's head,
  never a same-spelled declaration (`type T = i64` beside `type Pair<T>`).

## DefTable contract changes

- `DefTable::module_has_declarations` becomes `module_has_source_declarations`:
  a registry-loaded extern declares a zero-span row before its module's source
  is read and no longer suppresses minting the module's source items (the
  `std.stream.StreamPair` contract type had no row in registry-only checks).
- `DefTable::extends` admits a known declaration the base held sourceless and
  the embedded builtin check adopted; identity and path are unchanged.

## Tests rewritten or deleted

- Deleted `resolved_ty::tests::round_trip_preserves_builtin_for_user_shadowed_names`:
  a head carries its identity, so there is no discriminator to lose.
- Deleted `check::tests::callables::callable_join_accepts_bare_alias_for_current_owner`
  and `check::methods::tests::qualified_method_receiver_restores_only_its_own_return_identity`:
  both asserted leaf requalification of a resolved type.
- Deleted `check::tests::modules::nested_same_final_modules_resolve_own_nominals_to_full_identity`:
  it hand-inserted `type_defs` with no declaration rows. The property (a bare
  name in `left.render` resolves to `left.render.Box`) is `Scope::resolve`'s
  module-item step, covered by
  `hew-hir` `same_short_name_actors_in_different_modules_canonicalize_independently`
  and the w4c oracle.
- Deleted `registry_core::veciter_intoiterator_surface::stdlib_vec_typechecks_with_iterator_traits_registered`:
  it checked `std/builtins.hew` as a root program against an empty table, a
  configuration no compile uses. It exposed that `impl_assoc_type_bindings`
  holds `VecIter`'s `Item` under both `("VecIter", "Iterator")` and
  `("std.builtins.VecIter", "std.builtins.Iterator")`; commit 3's id-keyed
  projection must leave one row.
- Rewritten: `unify_qualified_and_bare_named` (now
  `..._declarations_stay_distinct`) and `unify_qualified_with_type_args`
  assert R5; the three `struct_init_*` checker tests are source programs (the
  `Wrapper<IntLiteral>` intermediate is no longer pinned); registry signature
  tests use unresolved spellings; `stdlib_import_registers_trait_impls_for_generic_bounds`
  fabricates `std.text.semver` rather than a module the prelude already loads;
  tests that compared checker output with fixture identities read the
  checker's table (`Ty::named_in`, `ResolvedTy::named_path`).

## Open, identity-rooted

- A module-private `type T` leaks into `std.builtins` generic binders when a
  root imports that module and names a module generic (`import pk.m as m;
m.Envelope<string> { .. }` with `type T = bool;` in `m`): "`T` is private to
  module `pk.m`; not accessible from `std.builtins`". Present on the base.
- `TRANSITION(P1)` render-back sites feed the string registries and close in
  commit 3 as each consumer takes ids, one touch per site.
- `hew-sir/src/resource.rs` `verify_resource_release` (opaque close): the
  release's nominal owner is compared by declaration id, not by rendered path.
