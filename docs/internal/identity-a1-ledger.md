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

## Type definitions keyed by declaration

- `type_defs` is `HashMap<NominalId, TypeDef>` in the checker and in
  `TypeCheckOutput`; `TypeDefView` reads it by head. A builtin's std
  declaration is bound when the row is minted (`DefTable::bind_builtin_declaration`),
  so `Vec`/`VecIter`/`Stream` heads reach their definitions by id.
- `hew-types/src/check/registration/type_publication.rs`
  `register_qualified_type_alias`: the copy of a bare-keyed definition onto a
  qualified key is deleted. It copied a root `type Connection { .. }` onto
  `std.net.Connection` once both spellings were one table; the qualified
  spelling reaches the declaration's own row.
- The same file's post-registration refresh passes read the module's own
  declaration by its full path instead of the importer's bare spelling.
- `hew-types/src/check/registration/functions.rs` `publish_impl_method_sig`:
  the second write onto the qualified entry is deleted; bare and qualified
  spellings are one row.
- `hew-types/src/cycle.rs`: the actor-reference and recursive-value walks run
  over `NominalId`s; paths are rendered only for diagnostics.
- `hew-types/src/method_resolution.rs` and `hew-types/src/type_facts.rs` read
  definitions through `TypeDefView`; a registry key that spells a builtin
  reaches its std declaration only in method lookup (TRANSITION until the
  catalog move), never in type facts.

## Function signatures keyed by declaration

- `fn_sigs` is `HashMap<DefId, FnSig>` in the checker and in
  `TypeCheckOutput`; every spelling a caller still uses (`Type::method`,
  `{module}.{name}`, a mangled specialisation, an import binding) is a key of
  `fn_sig_keys` naming one declaration (TRANSITION until callers resolve
  through `Scope` and the dispatch table). Compiler builtin signatures live in
  `builtin_fn_sigs` by name until the catalog move. Several spellings of one
  declaration now share one signature row instead of holding copies that could
  drift apart.
- An enum variant is a `DeclarationKind::Variant` row owned by its enum, and a
  desugared machine's states are `MachineState` rows owned by the machine
  (`DefTable::member_of_kind`); variant and state constructor signatures are
  filed under those rows. A wire type's compiler codec entry points
  (`decode`, `from_json`) are sourceless member rows of the type.
- A module function a registry publishes before the module's source is read
  gets a sourceless row that the source declaration adopts.
- `hew-types/src/check/registration/type_publication.rs`: an actor imported
  through a module surface registers its signatures in its declaring module,
  so a receive handler's parameter types resolve there instead of in the
  importer (`pipeline.StageI64::push` resolved `PipelineItemI64` as an
  unresolved spelling).
- Downstream (mechanical): hew-hir reads `fn_sigs_by_path()` into a field
  renamed `fn_sigs_by_path`; hew-analysis reads signatures through
  `TypeCheckOutput::sigs()`.

## Traits keyed by declaration

- `trait_defs` and `trait_super` are keyed by the trait's `DefId`; the bare,
  `builtins.`, module-short, qualified and import-binding spellings are keys
  of `trait_def_keys` naming one declaration (TRANSITION until bounds carry
  `TraitRef`s). A super-trait list is recorded once per trait.
- `hew-types/src/check/registration/imports.rs` file-import traits: the
  importer binds the trait's spelling and never replaces the definition the
  declaring file registered (whose default bodies check in that file).
- A module trait registered by a route that reads it before the module's
  declarations are minted gets a sourceless row its declaration adopts.

## Method dispatch by declaration (R1)

- `hew-types/src/check/dispatch_table.rs`: source methods are filed by the
  receiver's declaration (a primitive or builtin receiver by its anchor row),
  the method's owner (inherent or a trait `DefId`) and the method name. A
  concrete impl (`impl Show for Box<i64>`) serves only its instance and wins
  over the generic impl of the same owner.
- `hew-types/src/check/methods/dispatch.rs` and
  `methods/named_method_resolution.rs`: a dot call selects through the table
  (R1). The inherent method wins; one trait method is selected; two or more
  trait methods with no inherent one is `AmbiguousTraitMethod` naming the
  traits. Before, the `Type::method` spelling held whichever impl registered
  last (w1b_dot now refused).
- `hew-types/src/check/items.rs` `check_impl`: an impl method's body is
  checked against its own declaration's signature, so two traits' `size`
  methods on one type no longer conflate (w1d type-checks).
- `hew-types/src/check/registration/functions.rs` `impl_method_declaration_id`:
  the receiver and trait in an impl method's path are their declarations'
  paths, so a file-import route and the declaring file reach one row.
- Not yet end to end: HIR still keys impl bodies by `Type::method`, so w1a,
  w1c, w1d and the f-string Display of a specialised impl need HIR to lower by
  declaration (lane B1).

## Predicates, patterns and type parameters by identity (R2, R5, R6)

- `hew-types/src/check/generics.rs` `bound_marker`: a bound spelled like a
  compiler predicate is that predicate only when no declared trait answers
  to the spelling (`Display` only as the prelude's declaration). `Ord`
  implies `PartialOrd` only between the predicates (w2b_ord refused).
- `TraitObjectBound.trait_id` / `ResolvedTraitBound.trait_id`: a `dyn` bound
  carries the declared trait it names; `traits.rs` never treats a declared
  trait as a marker, and the actor boundary asks the `Send` predicate itself
  (`type_is_send`), so `dyn Send` of a user trait is not Send (w2a refused).
  Equality still ignores `trait_id` until HIR lowers `dyn` types from the
  checker (TRANSITION).
- `hew-types/src/check/patterns.rs`: a record pattern's name must resolve to
  the scrutinee's declaration (w4b refused). The let-only spelling check in
  `statements.rs` is deleted; the binder owns the rule in every position.
- `hew-types/src/check/expressions/synthesize_spawn_forms.rs`: a field access
  on a type parameter is refused (w4a); the fixture moved to `reject/` with a
  control without the same-spelled nominal.

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

- Rewritten: `hew-hir` `imported_impl_lower` roots now `import shapes;` and
  write `shapes.Foo { .. }`; a root without the import no longer sees the
  module's `Foo` through the bare table (the R5/w4c change above).
- Rewritten: `hew-analysis` `hover::tests::hover_finds_type_def` builds its
  output with the checker; `signature_help` tests and `cycle::tests::*` use
  fixture identities; `q297_stdlib_iterator_next_and_vec_iter_carry_mut_receiver_flag`
  reads `std.builtins.VecIter`.
- Deleted `validate_handle_types_no_field_overlap_prunes_bare_alias_twin` and
  `validate_handle_types_no_field_overlap_qualified_alias_span_is_propagated`:
  a bare alias twin of a definition no longer exists.

- Rewritten: `machine_typecheck::imported_machine_unit_state_constructor_resolves`
  compares with the checker's own identities (`Ty::named_in`) rather than
  fixture identities that matched the checker's only by row order.
- Tests that fabricated string-keyed signature tables build them through
  `FnSigFixture` or `Checker::test_fn_sig`.

- Rewritten: `overlapping_user_record_impls_rejected` became
  `concrete_and_generic_user_record_impls_project_per_instance`; it passed
  only because the two impls' bodies were checked against one conflated
  signature. Whether a user type may carry a generic and a concrete impl of
  one trait is an open question (the builtin-constructor overlap stays
  refused).
- Rewritten: hew-compile `mixed_file_and_package_impls_keep_declaration_owned_dispatch_in_both_import_orders`
  pins the file-import impl method's declaration path.

## Open, identity-rooted

- A module-private `type T` leaks into `std.builtins` generic binders when a
  root imports that module and names a module generic (`import pk.m as m;
m.Envelope<string> { .. }` with `type T = bool;` in `m`): "`T` is private to
  module `pk.m`; not accessible from `std.builtins`". Present on the base.
- `TRANSITION(P1)` render-back sites feed the string registries and close in
  commit 3 as each consumer takes ids, one touch per site.
- `hew-sir/src/resource.rs` `verify_resource_release` (opaque close): the
  release's nominal owner is compared by declaration id, not by rendered path.
