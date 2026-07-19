#!/usr/bin/env bash
# Cross-module package-import oracle: real .hew fixtures importing the
# in-tree `hew::testffi` package (tests/pkg-import/pkgs/testffi) through
# `hew run --pkg-path`. Exercises, end-to-end:
#   - imported-actor value asks (i32 / i64 / string / record replies)
#   - imported-type trait methods (rows/get/total/free on the handle)
#   - the prelude-shadowing record name (`type Result`)
#   - [native] auto-link: the package's Rust staticlib builds on demand
#   - local-actor asks coexisting with imported asks (regression guard)
#   - mixed file-import + package-import impls on distinct same-bare-named
#     types (dedup must skip by module origin, not by `<type>:<trait>` name)
# On macOS the trait-method demo re-runs under MallocScribble/GuardEdges to
# hold the handle's single-release contract.
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
HEW="${ROOT}/target/debug/hew"
DIR="${ROOT}/tests/pkg-import"
PKGS="${DIR}/pkgs"

cargo build -q -p hew-lib
cargo build -q -p hew-cli

mkdir -p "${ROOT}/.tmp"
actual_output="${ROOT}/.tmp/pkg-import-actual.txt"
trap 'rm -f "${actual_output}"' EXIT

fixtures=(
  imported_actor_ask_i32
  imported_actor_ask_i64
  imported_actor_ask_string
  imported_actor_ask_record
  imported_trait_method
  local_actor_ask_guard
  mixed_import_impl_collision
  # Two packages export a divergent-layout `Widget` (i8 vs i64); each is
  # constructed and read through its module-qualified identity. The MIR
  # `RecordLayout` registry keys each layout by qualified name, not the bare
  # `Widget`, so both field loads read the correct slot width.
  samename_type_layout
  # A generic enum (`Result<Vec<Box>, _>`) whose payload nests a qualified user
  # type (`nestbox.Box`) reached via two import paths (directly and through
  # `hew::nestrelay`, which re-imports `nestbox`). The heap-owning `Box`es are
  # dropped at scope exit, exercising the generic-enum / owned-Vec-element drop
  # and codec seed paths. The layout key for every such site must shorten the
  # WHOLE type-arg spine to bare names, identical to the registration side, or
  # the keys diverge and the drop falls through the codegen fail-closed.
  nested_qualified_payload
  # A LOCAL generic record (`Holder<T>`) and enum (`Slot<T>`) constructed only
  # inside LOCAL generic factories, instantiated with the QUALIFIED type-arg
  # `lmonobox.Box`. The concrete `Holder<Box>` / `Slot<Box>` layouts are
  # discovered only post-function-mono (the `layout_mono` pass). That pass must
  # shorten the whole type-arg spine to bare names for both the registration key
  # and the mangled name, identical to every codegen / MIR layout lookup, or the
  # qualified `Holder$$lmonobox.Box` registration diverges from the bare
  # `Holder$$Box` lookup and the heap-owning Box drop / field read falls through
  # the codegen fail-closed.
  postmono_qualified_layout
  # An IMPORTED generic pure-value record (`keyed.Key<T>`, two i64, no owned or
  # Option field) monomorphised to `Key$$string` across the module boundary
  # (#2744). Its BitCopy value-class marker registers under the bare-origin
  # mangling (`Key$$string`) the declaration produces, but the importer's
  # use-site carries the qualified name `keyed.Key<string>`. The MIR value-class
  # probe must shorten the qualified origin before mangling, or the qualified
  # `keyed.Key$$string` misses the bare marker, the instance falls to `Unknown`,
  # and the fail-closed value-class gate rejects a valid BitCopy record. Both
  # axes are necessary: imported NON-generic and single-file generic both pass —
  # only imported-origin + generic-mono hits the qualified key. Output "7"
  # proves the mono instance classified BitCopy and the field read lowered.
  imported_generic_valueclass
  # A module-PRIVATE generic record (`Slot<T>`) used only as the element of a
  # pub `Store<T>`'s `Vec<Slot<T>>`, monomorphised across the module boundary to
  # a `string` payload (#2755). The private generic record never becomes an
  # emitted HIR item, so before the fix the consumer had no `RecordLayout` for
  # it: the `Slot { .. }` construction dropped its type args and `Vec::push` on
  # the type-parameter element failed closed. The layout is now discovered from
  # the substituted `Store::add$$string` body's type structure, keyed bare, and
  # `generation_at` reads a private-slot field back cross-module. Output "0\n1".
  private_generic_record_vec_element
  # The Arena acceptance shape (#2755): a module-PRIVATE generic `Slot<T>` whose
  # field is a nested `Option<T>`, behind a pub `Arena<T>`'s `Vec<Slot<T>>`. The
  # nested `Option$$string` must register from the private record's TYPE
  # STRUCTURE (Bug A #2746 discipline), alongside `Slot$$string`, from the
  # substituted `Arena::insert$$string` body. The heap-owning `string` payload
  # rides inside the private slot's `Option` and round-trips: `value_at` returns
  # it and the call site matches it. Output "world".
  private_generic_record_option_element
  # A monomorphic record (`Point`) and a generic record (`Holder<Box>`) both
  # CONSTRUCTED through their module-qualified OUTER name (`qualshapes.Point`,
  # `qualshapes.Holder<qualshapes.Box>`) under qualified-by-default. Layout
  # registration keys on the bare outer name (`Point`, `Holder$$Box`), so the
  # MIR `StructInit` field-order lookup, the MIR codegen-readiness check, and
  # the codegen `record_struct_for` monomorphic arm must each shorten the
  # qualified outer name before the lookup, or the construction falls through
  # the field-order / record-layout fail-closed.
  qualified_construct_layout
  # A selectively imported `Point` resolves to the checker-qualified
  # `qualshapes.Point`, while HIR registers its declaration as bare `Point`.
  # Record-let desugaring must strip that qualifier when it looks up fields.
  record_destructure_selective_import
  # An `impl Closable` whose `close` returns the error type through its
  # module-qualified spelling (`closableerr.CloseError`) while the trait
  # declares it bare (`CloseError`). `check_impl_method_against_trait` must
  # shorten the known-module qualifier before comparing signatures, or it
  # rejects the impl with a false `TraitImplSignatureMismatch`. Called via
  # receiver dispatch so the proof turns only on trait conformance.
  qualified_trait_sig
  # A bare `Gadget` opted in from `widgetpub8::{ Gadget }` while `widgetpub64`
  # (which also exports a divergent-layout `Gadget`) is plain-imported. The bare
  # reference must resolve to the PUBLISHED owner `widgetpub8.Gadget` (i8) and
  # read 7 — the plain import must not poison the opt-in into a false ambiguity,
  # and must not bind the wrong (i64) layout. Ambiguity is decided over
  # PUBLISHED bare bindings, not bare exports.
  published_bare_no_poison
  # An ALIASED opt-in (`import hew::aliassrc::{ Payload as Tag }`) constructs the
  # record through its alias binding (`Tag { code: … }`). The checker resolves
  # `Tag` to the SOURCE identity `aliassrc.Payload`; HIR lowering must stamp that
  # source identity onto the StructInit result type, not the bare binding `Tag`,
  # or the MIR field-order lookup misses the registered bare `Payload` layout and
  # the construction falls through the field-order fail-closed. Reading the field
  # back proves the source layout was constructed end-to-end.
  alias_import_resolves_bare_binding_to_source_identity
  # The same owner module ALSO exports a DISTINCT `Other` record. Aliasing
  # `Payload as Other` must bind the SOURCE `aliassrc.Payload`, never the
  # same-named export `aliassrc.Other`: the construction `Other { code: … }`
  # initialises `Payload`'s sole field, and HIR keys the StructInit off the
  # resolved source identity rather than the bare alias binding so the field-order
  # lookup hits `Payload`, not the unregistered `Other` alias key.
  alias_import_does_not_conflate_with_same_named_export
  # An ALIASED import alias resolves in function-parameter type-annotation
  # position. `import hew::aliassrc::{ Payload as Tag }` then
  # `fn get_code(item: Tag) -> i64` must lower `Tag` in the parameter type
  # annotation to the source identity `aliassrc.Payload`. Before this fix,
  # HIR `lower_type` / `resolve_named_type_ref` had no access to the alias
  # table and produced an unresolvable `ResolvedTy::Named { name: "Tag" }`.
  aliased_import_type_annotation
  # An ALIASED enum import alias resolves in type-annotation position, tuple
  # variant construction, and match-arm pattern dispatch. `import
  # hew::aliassrc::{ Color as Hue }` then `let h: Hue = Hue::Blue(42)` and
  # `match h { Hue::Red => 1, Hue::Blue(n) => n }` must all resolve through
  # the alias. Before this fix the annotation `Hue` was not canonicalised in
  # `lower_type`, the constructor `Hue::Blue(42)` was not found in
  # `lookup_variant_constructor`, and the HIR `lookup_variant_ctor` missed the
  # aliased path `Hue::Blue` → `aliassrc.Color::Blue`.
  aliased_import_enum_variant
  # The SAME aliased import (`Color as Hue`, `Payload as Tag`) but declared and
  # used INSIDE a package module (`hew::deepalias`) whose dotted path has depth
  # ≥ 2.  The checker keys the per-module import-alias map by the importer's
  # FULL dotted path (`hew.deepalias`); HIR lowering must use the SAME key
  # rather than the short last segment (`deepalias`), or the depth-2 importer's
  # aliases are missed and `Hue`/`Tag` fail to resolve (UnresolvedSymbol).
  # Root-importer fixtures (depth 1) cannot catch this — short and full keys
  # coincide there.  Output "50" proves the aliases resolved end-to-end.
  package_module_type_alias
  # A LOCAL type declaration shadows an import alias of the SAME bare name
  # (local-shadows-imported rule, B3 fix).  `import hew::aliassrc::{ Payload
  # as U }` brings in the alias `U` → `aliassrc.Payload`, but the program
  # ALSO declares `type U { local: i64 }`.  HIR `resolve_named_type_ref` must
  # apply the alias ONLY as a fallback (after the record-registry check for `U`
  # succeeds), so the construction `U { local: 7 }` and field read `.local`
  # resolve against the LOCAL type.  Output "7" proves the local type was used.
  local_type_shadows_import_alias
  # --- #2202: import alias used in a type-declaration MEMBER position ---
  # An import alias resolves in a type-decl RECORD-FIELD member position (the
  # canonical #2202 repro). `import hew::aliassrc::{ Payload as Tag }` then
  # `type Boxed { item: Tag }`. The checker resolves member types in Pass 1
  # (`collect_types`) BEFORE imports are processed in Pass 2, so the bare alias
  # `Tag` used to freeze as unresolved `Named("Tag")` while its construction
  # (Pass 3) resolved to `aliassrc.Payload` -- a spurious `expected \`Tag\`,
  # found \`aliassrc.Payload\``. The member-type re-resolution pass (run AFTER
  # imports) upgrades the frozen member; HIR member-lowering stamps the same
  # source identity so the field read lowers. Output "50".
  aliased_import_record_field_member
  # The same alias in an ENUM-VARIANT-PAYLOAD member position: `enum Wrap {
  # Has(Tag) }`. The variant constructor `fn_sig` is re-keyed by the
  # re-resolution pass so the payload binds `aliassrc.Payload`; the match arm
  # `Wrap::Has(t) => t.code` reads it back. Output "42".
  aliased_import_enum_payload_member
  # The same alias in a MACHINE-STATE-FIELD member position: `state Full { item:
  # Tag }`. The state-variant table is re-resolved after imports so a transition
  # body reading the aliased field (`self.item.code + 2`) type-checks (only
  # `aliassrc.Payload` has `.code`) and an external match on the machine value
  # (`Holder::Full { item } => item.code`) reads it back. Output "42".
  aliased_import_machine_state_member
  # The member positions used INSIDE a depth-2 package module
  # (`hew::aliasmember`, dotted `hew.aliasmember`): a record field
  # (`DeepBoxed { item: Tag }`) and an enum payload (`DeepWrap::Has(Tag)`). The
  # checker keys the per-module alias map by the FULL dotted path; HIR
  # member-lowering must use the SAME key, or the depth-2 importer's members
  # freeze as bare `Tag` and fail closed at the MIR boundary
  # (`unknown type Tag`). Root importers (depth 1) cannot catch this -- short
  # and full keys coincide there. `member_score()` = 20 + 13 = "33".
  aliased_import_member_depth2
  # Control: a QUALIFIED member type (`type Boxed { item: aliassrc.Payload }`)
  # already resolved correctly in Pass 1; the re-resolution pass must treat it as
  # unchanged (no new diagnostics) and the field read must still lower. The fix
  # must not regress the already-working qualified path. Output "50".
  aliased_import_qualified_field_control
  # Control: a LOCAL `type Tag { local: i64 }` shadows the import alias `Tag` in
  # a member position (`type Boxed { item: Tag }`). The re-resolution pass seeds
  # each scope's OWN type names before consulting the import alias map, so the
  # unqualified member binds the LOCAL `Tag` (field `local`), not the imported
  # `aliassrc.Payload` (field `code`). Preserves the local-shadows-imported
  # rule. Output "9".
  local_type_shadows_import_alias_member
  # An ALIASED trait opt-in (`import hew::closableerr::{ Closable as C }`) impls
  # the trait under its alias (`impl C for MonitorRef`) returning the error type
  # through its CORRECT module-qualified spelling (`closableerr.CloseError`). The
  # trait-conformance check must resolve the aliased trait `C` to its SOURCE
  # identity (`closableerr.Closable`) to find the registered method signatures and
  # to qualify the trait declaration's bare `CloseError` against the trait owner,
  # then ACCEPT the matching impl. Run end-to-end so the aliased impl is also
  # exercised through codegen + receiver dispatch.
  aliased_trait_sig
  # Two imported packages (`hew::srccollidea`, `hew::srccollideb`) each export a
  # trait literally named `Source` with DIVERGENT signatures (`Result<Payload,
  # ErrA>` vs `i64`). The importer aliases module A's trait (`Source as A`) and
  # plain-imports module B, then impls A with A's CORRECT `Result<Payload, ErrA>`
  # signature. The bare trait-method key `Source::close` collides across the two
  # modules (first-write-wins, registration-order-dependent), so conformance must
  # resolve the aliased trait to its source owner and compare against the
  # collision-free owner-qualified key (`srccollidea.Source::close`). The correct
  # impl must ACCEPT even while module B's same-named `Source::close` (the `i64`
  # shape) is also registered. Run end-to-end so the aliased impl is exercised
  # through codegen + receiver dispatch.
  aliased_trait_source_name_collision_accept
  # A LOCAL trait `Source` shadows an imported same-name trait. The program
  # declares its own `Source` (`close` → `i64`) while `hew::srccollidea` (aliased
  # `Source as A`, whose `close` → `Result<Payload, ErrA>`) and `hew::srccollideb`
  # are also in scope, polluting the bare `Source::close` key (first-write-wins).
  # Trait conformance must resolve `Source` to the LOCAL trait — not the imported
  # signature leaked into the bare key — so the local `i64` impl is valid and runs.
  local_trait_shadows_imported_collision_accept
  # A correct ALIASED multi-method trait impl (`Proc as P` from `hew::multitrait`,
  # `tick` + `close` → `Result<Tally, ProcErr>`) under a same-name collision with
  # `hew::multitraitb`'s one-method `Proc`. Conformance resolves `P` to its
  # owner-qualified identity and ACCEPTs — exercising BOTH method-set membership
  # and per-method signature comparison against the collision-free owner key.
  aliased_multimethod_trait_collision_accept
  # An `impl Sub for W` (where `trait Sub: Super`) provides a SUPER-trait method
  # (`base`) inline alongside the sub-trait's own method. The impl-site method-set
  # check must fold the whole super-trait chain into the KNOWN set, so the inline
  # super-method is not flagged as extraneous. ACCEPT + run e2e.
  subtrait_inline_supermethod_accept
  # IMPORTED supertrait, inline supermethod. An importer brings ONLY `Sub` into
  # scope (`trait Sub: Base` in `hew::supertraits`) and provides the inherited
  # `base` inline. The supertrait edge is OWNER-QUALIFIED (`supertraits.Base`), so
  # `base`'s methods are folded into the known set through `Base`'s owner — not
  # the importer namespace, where `Base` is absent — and the inline `base`'s
  # signature is checked against `supertraits.Base` (`-> i64`). ACCEPT + run e2e
  # so the inherited `base` and the sub's own `tag` both dispatch. (Guards the
  # over-strict "declares method(s) not on the trait: base" rejection of an
  # import-only sub whose super edge resolved in the wrong namespace.)
  imported_subtrait_inline_supermethod_accept
  # IMPORTED multi-level chain (`Deep: Mid`, `Mid: Base`). An import-only `Deep`
  # provides every inherited method inline; each owner-qualified super edge
  # resolves the method set + signatures two levels up. ACCEPT + run e2e.
  imported_deep_chain_inline_supermethods_accept
  # IMPORTED diamond (`Diamond: Left + Right`, both `Left`/`Right: Base`). An
  # import-only `Diamond` provides the doubly-inherited `base` ONCE plus both
  # branch methods and the apex; the owner-qualified super walk dedups the two
  # branch paths so the single inline `base` is KNOWN. ACCEPT + run e2e.
  imported_diamond_inline_supermethods_accept
  # ALIASED imported sub-trait (`Sub as S`) provides the inherited `base` inline.
  # The alias resolves to its SOURCE identity (`supertraits.Sub`), whose
  # owner-qualified super edge anchors `base` at `supertraits.Base`; the inline
  # `base`'s signature is checked through the alias. ACCEPT + run e2e.
  aliased_subtrait_inline_supermethod_accept
  # RE-EXPORTED supertrait, inline supermethod. The importer brings ONLY `Sub`
  # into scope from `hew::reexsub`, where `trait Sub: Base` and `Base` is itself
  # re-imported by `reexsub` from `hew::reexbase`. The super edge must follow the
  # re-export chain to `reexbase.Base` (reached through `reexsub`'s own import) —
  # there is no `reexsub.Base` def, so a `{declaring}.Base`-only check misses and
  # the bare fallback would bind the final importer's namespace. The inline
  # `base`'s methods are folded into the known set through `reexbase.Base`'s owner
  # and its signature is checked against `reexbase.Base` (`-> i64`). ACCEPT + e2e.
  reexport_super_inline_supermethod_accept
  # ALIASED re-exported supertrait: `SubA` from `hew::reexsubalias`, where
  # `SubA: B` and `B` is `reexbase.Base` re-imported under an alias. The super
  # edge `B` follows the module's ALIASED import binding to `reexbase.Base`, never
  # a reconstructed `reexsubalias.B`. ACCEPT + run e2e.
  reexport_aliased_super_inline_supermethod_accept
  # TWO-HOP re-export chain: `Top` from `hew::reexchaina` (`Top: Mid`, `Mid: Base`,
  # both re-exported). The importer provides `base`/`mid`/`top` inline; each super
  # edge follows its declaring module's import binding to the original owner across
  # two re-export hops, so the whole transitive method set is KNOWN and each inline
  # method's signature is checked against its origin. ACCEPT + run e2e.
  reexport_chain_inline_supermethods_accept
  # REDECLARED supertrait satisfied by a SEPARATE impl (the stdlib
  # `ValueMethods: CanonicalValueMethods` shape). `trait Redecl: Base` REDECLARES
  # the inherited `base`; the type provides `base` through a SEPARATE `impl Base
  # for W` and supplies only `Redecl`'s own `tag` in `impl Redecl for W`. The
  # redeclared `base` is bodyless-required on `Redecl` but INHERITED from `Base`,
  # so the method-set check must drop it from `Redecl`'s required set rather than
  # demand it on the sub-impl block. ACCEPT + run e2e so the inherited `base`
  # (via `impl Base`) and the sub's own `tag` both dispatch. Guards the
  # over-strict "is missing required method(s): base" rejection of the
  # separate-supertrait-impl pattern.
  imported_redeclared_super_separate_impl_accept
  # CAP-02 slice 2: a DIRECT call to an imported generic free fn with inferred
  # type args (`genhelpers.first([1, 2, 3])`). This parses as a method-call and
  # lowers through the module-qualified rewrite arm, which previously registered
  # no monomorphisation — so MIR found neither the mangled name in
  # `module_fn_names` nor a `call_site_type_args` entry and fell through to the
  # "function call" E_NOT_YET_IMPLEMENTED. Two instantiations of the SAME fn at
  # distinct concrete types (i64, string) prove the per-instantiation mangling
  # distinguishes the two emitted symbols (no one-symbol collapse). Prints only
  # concrete-typed values, so the gate keys on the call-lowering fix alone.
  generic_freefn_direct_call
  # CAP-02 slice 2: a lambda whose body wraps the same imported-generic direct
  # call, then invoked. Reproduced the IDENTICAL "function call" NYI at the same
  # MIR site; the monomorphisation recording walks the lambda body's call sites
  # the same way, so the single fix closes both.
  generic_freefn_lambda_wrap
  # CAP-02 slice 2: a `Vec<owned-record>` arg crossing the imported-generic
  # `first` call. The element `Boxed` owns a heap array, so the Vec arg and the
  # returned element exercise the owned-element drop ABI at the cross-module
  # mono boundary — caller and emitted specialised body must agree on
  # owned-vs-BitCopy or the drop falls through the codegen fail-closed
  # (`container-abi-ctor-op-agreement`). Reading `b.payload[0]` back proves the
  # element survived the call end-to-end.
  generic_freefn_owned_element
  # Extern record-return ABI oracle (hew::recordret): user extern "C" fns
  # returning #[repr(C)] records by value, one fixture per aggregate-return
  # class, every field a distinct non-zero sentinel. Pins that the classified
  # declaration + call edge reconstruct each record exactly:
  #   one   —  8 B {i64}          Direct single-field (natural, the control)
  #   small —  8 B {i32,i32}      Direct multi-field  (i64 coerced-int carrier)
  #   mid   — 16 B {i32,i64}      RegisterPair        ([2 x i64] carrier)
  #   packed— 16 B {i64,i32,i32}  RegisterPair packed ([2 x i64] carrier)
  #   big   — 24 B {i64,i64,i64}  Indirect            (sret; the #2399 headline)
  #   big_mixed — 24 B from (i32,i64) params: pins the hidden-sret-pointer
  #     argument-index shift (hi > 2^32 must not truncate, lo/hi must not swap).
  recordret_one
  recordret_small
  recordret_mid
  recordret_packed
  recordret_big
  recordret_big_mixed
  # #2419: closures capturing IMPORTED records (hew::capturestats). The env
  # free thunk dispatches a record capture through
  # `__hew_record_drop_inplace_<key>`; the closure-capture seed pass must
  # resolve the imported record to the same key or the helper is
  # declared-but-undefined and LLVM verify rejects the module. One Vec-bearing
  # shape (real drop glue) and one all-scalar shape (no glue, still keyed).
  closure_capture_imported_record
  closure_capture_imported_flat_record
  # Issue #2651 accept side: legal bare/qualified alias of the SAME imported
  # definition. `import hew::widgeti8::{ Widget }` opts the bare name in, so
  # `Widget` and `widgeti8.Widget` denote ONE definition; a value of the
  # qualified spelling must satisfy a bare-typed parameter and vice versa. The
  # owner-qualified identity gate resolves the single-publisher bare name to its
  # owner and finds the two spellings identical, so it does NOT fire — the legal
  # alias is preserved. Prints bare=7 then qual=7.
  bare_import_alias_accept
  # Issue #2651 accept side: a module that references its OWN type both bare and
  # through its own module qualifier. `selfqualtype.roundtrip()` passes a bare
  # `Meter` into a `selfqualtype.Meter` parameter, forcing the checker to compare
  # bare `Meter` against `selfqualtype.Meter` with the current module set to
  # `selfqualtype`. The owner-identity gate strips the redundant current-module
  # self-qualifier so the two spellings of one definition compare equal; without
  # that carve-out the pairing false-rejects. Prints 3. (This is the type-level
  # equivalent of the generic-machine self-qualified transition — e.g. the stdlib
  # `Lifecycle<T>` machine — reached deterministically without machine plumbing.)
  self_qualified_type_identity
  # Issue #2651 accept side: a legal callee-frame bare alias reaching the unique
  # registered-owner scan. `framealias.make()` returns its result spelled bare
  # (`Widget`) in the callee frame; the local `read` parameter is typed with the
  # module-qualified `framealias.Widget`. Under a plain (non-opt-in) import the
  # bare name is not a published bare binding, so the gate qualifies it through
  # the UNIQUE registered-owner scan over `type_defs`/`known_types`
  # (→ `framealias.Widget`) and ACCEPTS the owner-identical pairing. Prints 11.
  callee_frame_bare_alias_accept
  # Issue #2208 finding 1: a ROOT-declared `Payload` collides with an imported
  # `rootreply.Payload` returned by an actor ask. MIR's `collided_type_names`
  # counts distinct qualified identities across EVERY item INCLUDING root, so it
  # keys the reply layout `rootreply.Payload`. The checker/HIR collision set
  # must include the root declarant too, or the ask-reply value flow stays bare,
  # resolves to the root `Payload` (field `tag`, not `code`), and the reply read
  # fails. Prints `1` then `42`.
  root_shadow_actor_ask
  # Issue #2208 finding 2: an imported actor reply `Result<Unique, Colliding>`
  # where `Colliding` collides with a root-declared `Colliding` but `Unique` is
  # unique. Only `Colliding` may be owner-qualified in the checker's reply
  # identity — a `current_module` scope over the whole signature would qualify
  # `Unique` too (`Result<mixreply.Unique, mixreply.Colliding>`), diverging from
  # HIR's collision-gated transform (`Result<Unique, mixreply.Colliding>`) and
  # tripping MIR's actor-reply equality. Prints `1` then `9`.
  mixed_collision_reply
)

for fixture in "${fixtures[@]}"; do
  if ! "${HEW}" run --pkg-path "${PKGS}" "${DIR}/${fixture}.hew" >"${actual_output}" 2>/dev/null; then
    echo "FAIL ${fixture}: hew run exited non-zero" >&2
    "${HEW}" run --pkg-path "${PKGS}" "${DIR}/${fixture}.hew" >&2 || true
    exit 1
  fi
  if ! diff -u "${DIR}/${fixture}.expected" "${actual_output}"; then
    echo "FAIL ${fixture}: output diverged from ${fixture}.expected" >&2
    exit 1
  fi
  echo "PASS ${fixture}"
done

# Reject fixture: two imported packages (`hew::replysend`, `hew::replynonsend`)
# both export a type named `Reply` — one Send (`i64`), one non-Send (`Rc<i64>`).
# The ask-reply Send gate derives Send from a named type's member set; keying it
# on the bare name `Reply` collides across the two packages (last-write-wins), so
# a non-Send reply could read the Send package's fields and slip the gate,
# reaching codegen where it trips the D10 named-`Rc` fail-closed. The gate must
# derive Send through each reply's module-qualified identity: the non-Send ask is
# rejected with E_DUPLEX_NON_SEND, the Send ask is accepted, regardless of import
# order. Asserts the gate fires at type-check time, NOT a D10 codegen fall-through.
reject_fixture="samename_reply_reject"
reject_out="$("${HEW}" check --pkg-path "${PKGS}" "${DIR}/${reject_fixture}.hew" 2>&1)" && {
  echo "FAIL ${reject_fixture}: hew check unexpectedly succeeded (non-Send reply slipped the gate)" >&2
  echo "${reject_out}" >&2
  exit 1
}
if ! grep -q "E_DUPLEX_NON_SEND" <<<"${reject_out}"; then
  echo "FAIL ${reject_fixture}: expected E_DUPLEX_NON_SEND on the non-Send ask" >&2
  echo "${reject_out}" >&2
  exit 1
fi
if grep -q "D10 violation" <<<"${reject_out}"; then
  echo "FAIL ${reject_fixture}: non-Send reply fell through to the D10 codegen gate instead of E_DUPLEX_NON_SEND" >&2
  echo "${reject_out}" >&2
  exit 1
fi
echo "PASS ${reject_fixture}"

# Reject fixtures (issue #2651): a root-local type conflated with an imported
# same-bare-name type. A root-local `type Widget` keeps its own bare identity and
# is DISTINCT from any imported `owner.Widget`; passing the local value where the
# import is required must be rejected at the type boundary with a nominal
# type-mismatch, NOT accepted by the context-free suffix compare and left to trip
# the downstream `E_CODEGEN_FRONT` LLVM-verify fail-closed. Two variants:
#   - divergent layout (`widgeti8.Widget` i8 vs local i64): the headline case
#     that previously reached codegen.
#   - same layout (`widgeti64.Widget` i64 vs local i64): proves the gate rejects
#     on DEFINITION IDENTITY, not on a layout heuristic that would accept it.
for nominal_reject in \
  local_shadow_import_divergent_reject \
  local_shadow_import_same_layout_reject; do
  nominal_out="$("${HEW}" check --pkg-path "${PKGS}" "${DIR}/${nominal_reject}.hew" 2>&1)" && {
    echo "FAIL ${nominal_reject}: hew check unexpectedly succeeded (local/import nominal collision slipped the gate)" >&2
    echo "${nominal_out}" >&2
    exit 1
  }
  if ! grep -q "type mismatch: expected" <<<"${nominal_out}"; then
    echo "FAIL ${nominal_reject}: expected a nominal type-mismatch diagnostic" >&2
    echo "${nominal_out}" >&2
    exit 1
  fi
  if grep -qE "E_CODEGEN_FRONT|D10 violation" <<<"${nominal_out}"; then
    echo "FAIL ${nominal_reject}: local/import collision fell through to codegen-front/D10 instead of a type-mismatch" >&2
    echo "${nominal_out}" >&2
    exit 1
  fi
  echo "PASS ${nominal_reject}"
done


# trait `Closable` (from `hew::closableerr`) requires `close` to return
# `closableerr.CloseError`; this impl returns the DISTINCT
# `closableerr2.CloseError` (a different type from a different module that
# shares the bare name). The signature comparison must reject this at the type
# boundary with `TraitImplSignatureMismatch` — NOT accept it via an over-strip
# that collapses both qualifiers to bare `CloseError`, and NOT defer to a
# downstream `E_CODEGEN_FRONT` / D10 codegen fall-through. The positive
# `qualified_trait_sig` oracle above (same-module spelling) must still pass.
trait_sig_reject="cross_module_trait_sig_reject"
trait_sig_out="$("${HEW}" check --pkg-path "${PKGS}" "${DIR}/${trait_sig_reject}.hew" 2>&1)" && {
  echo "FAIL ${trait_sig_reject}: hew check unexpectedly succeeded (wrong-module return type slipped the trait-sig gate)" >&2
  echo "${trait_sig_out}" >&2
  exit 1
}
if ! grep -q "but trait \`Closable\` requires" <<<"${trait_sig_out}"; then
  echo "FAIL ${trait_sig_reject}: expected a TraitImplSignatureMismatch return-type diagnostic" >&2
  echo "${trait_sig_out}" >&2
  exit 1
fi
if grep -qE "E_CODEGEN_FRONT|D10 violation" <<<"${trait_sig_out}"; then
  echo "FAIL ${trait_sig_reject}: wrong-module return type fell through to codegen-front/D10 instead of TraitImplSignatureMismatch" >&2
  echo "${trait_sig_out}" >&2
  exit 1
fi
echo "PASS ${trait_sig_reject}"

# Reject fixture: local-shadow + cross-module trait impl. The importer opts only
# the trait `Closable` into scope and defines its OWN local `CloseError`, then
# impls `close` returning that LOCAL bare `CloseError`. The trait requires
# `closableerr.CloseError`; the local type is distinct, so the impl must be
# REJECTED with `TraitImplSignatureMismatch`. The local-shadow carve-out is
# side-specific: it preserves the local identity only on the IMPL/actual side,
# while the trait side's bare `CloseError` always qualifies to the trait owner.
# A shared carve-out would leave the trait side bare too and falsely ACCEPT the
# wrong-module impl (fail-open).
local_shadow_reject="local_shadow_trait_sig_reject"
local_shadow_out="$("${HEW}" check --pkg-path "${PKGS}" "${DIR}/${local_shadow_reject}.hew" 2>&1)" && {
  echo "FAIL ${local_shadow_reject}: hew check unexpectedly succeeded (local-shadow impl slipped the trait-sig gate)" >&2
  echo "${local_shadow_out}" >&2
  exit 1
}
if ! grep -q "but trait \`Closable\` requires" <<<"${local_shadow_out}"; then
  echo "FAIL ${local_shadow_reject}: expected a TraitImplSignatureMismatch return-type diagnostic" >&2
  echo "${local_shadow_out}" >&2
  exit 1
fi
if grep -qE "E_CODEGEN_FRONT|D10 violation" <<<"${local_shadow_out}"; then
  echo "FAIL ${local_shadow_reject}: local-shadow impl fell through to codegen-front/D10 instead of TraitImplSignatureMismatch" >&2
  echo "${local_shadow_out}" >&2
  exit 1
fi
echo "PASS ${local_shadow_reject}"

# Reject fixture: ALIASED trait + cross-module wrong return type. The importer
# brings `Closable` into scope under an alias (`Closable as C`) and impls `close`
# returning the DISTINCT `closableerr2.CloseError`. The alias must not let the
# impl bypass the signature comparison: the checker must resolve `C` to its
# SOURCE identity (`closableerr.Closable`) to find the registered signatures
# (under `closableerr.Closable::close`, not the alias `C::close`) and to qualify
# the trait's bare `CloseError` to the trait owner. Without that resolution the
# alias-keyed lookup misses and the wrong-module impl is silently accepted
# (fail-open). Must REJECT with `TraitImplSignatureMismatch` at the type
# boundary, NOT a codegen-front / D10 fall-through.
aliased_trait_reject="aliased_trait_cross_module_sig_reject"
aliased_trait_out="$("${HEW}" check --pkg-path "${PKGS}" "${DIR}/${aliased_trait_reject}.hew" 2>&1)" && {
  echo "FAIL ${aliased_trait_reject}: hew check unexpectedly succeeded (aliased-trait wrong-module return type slipped the trait-sig gate)" >&2
  echo "${aliased_trait_out}" >&2
  exit 1
}
if ! grep -q "TraitImplSignatureMismatch\|but trait .* requires" <<<"${aliased_trait_out}"; then
  echo "FAIL ${aliased_trait_reject}: expected a TraitImplSignatureMismatch return-type diagnostic" >&2
  echo "${aliased_trait_out}" >&2
  exit 1
fi
if grep -qE "E_CODEGEN_FRONT|D10 violation" <<<"${aliased_trait_out}"; then
  echo "FAIL ${aliased_trait_reject}: aliased-trait wrong-module return type fell through to codegen-front/D10 instead of TraitImplSignatureMismatch" >&2
  echo "${aliased_trait_out}" >&2
  exit 1
fi
echo "PASS ${aliased_trait_reject}"

# Reject fixture: ALIASED trait + local-shadow. The importer aliases the trait
# (`Closable as C`), defines its OWN local `CloseError`, and impls `close`
# returning that LOCAL bare type. Resolving `C` to its SOURCE owner
# (`closableerr`) qualifies the trait side's bare `CloseError` to
# `closableerr.CloseError`, while the impl side's local shadow keeps its local
# identity; the two differ, so the impl must be REJECTED. Proves the
# side-specific local-shadow carve-out is reached through an alias.
aliased_local_shadow_reject="aliased_trait_local_shadow_sig_reject"
aliased_local_shadow_out="$("${HEW}" check --pkg-path "${PKGS}" "${DIR}/${aliased_local_shadow_reject}.hew" 2>&1)" && {
  echo "FAIL ${aliased_local_shadow_reject}: hew check unexpectedly succeeded (aliased-trait local-shadow impl slipped the trait-sig gate)" >&2
  echo "${aliased_local_shadow_out}" >&2
  exit 1
}
if ! grep -q "TraitImplSignatureMismatch\|but trait .* requires" <<<"${aliased_local_shadow_out}"; then
  echo "FAIL ${aliased_local_shadow_reject}: expected a TraitImplSignatureMismatch return-type diagnostic" >&2
  echo "${aliased_local_shadow_out}" >&2
  exit 1
fi
if grep -qE "E_CODEGEN_FRONT|D10 violation" <<<"${aliased_local_shadow_out}"; then
  echo "FAIL ${aliased_local_shadow_reject}: aliased-trait local-shadow impl fell through to codegen-front/D10 instead of TraitImplSignatureMismatch" >&2
  echo "${aliased_local_shadow_out}" >&2
  exit 1
fi
echo "PASS ${aliased_local_shadow_reject}"

# Reject fixture: ALIASED trait + same-name trait collision across modules. Two
# imported packages (`hew::srccollidea`, `hew::srccollideb`) each export a trait
# literally named `Source` with DIVERGENT signatures (`Result<Payload, ErrA>` vs
# `i64`). The importer aliases module A's trait (`Source as A`) and plain-imports
# module B, then impls A with module B's `i64` signature. The bare trait-method
# key `Source::close` is first-write-wins and collides across the two modules, so
# a bare-key comparison is registration-order-dependent and could silently accept
# B's `i64` impl against A's trait (a NON-DETERMINISTIC fail-open on the soundness
# boundary). Conformance must resolve the aliased trait `A` to its SOURCE owner
# (`srccollidea.Source`) and compare against the collision-free owner-qualified
# key (`srccollidea.Source::close`). Must REJECT with `TraitImplSignatureMismatch`
# at the type boundary, NOT a codegen-front / D10 fall-through.
samename_trait_collision_reject="aliased_trait_source_name_collision_reject"
samename_trait_collision_out="$("${HEW}" check --pkg-path "${PKGS}" "${DIR}/${samename_trait_collision_reject}.hew" 2>&1)" && {
  echo "FAIL ${samename_trait_collision_reject}: hew check unexpectedly succeeded (same-name trait collision slipped the trait-sig gate)" >&2
  echo "${samename_trait_collision_out}" >&2
  exit 1
}
if ! grep -q "TraitImplSignatureMismatch\|but trait .* requires" <<<"${samename_trait_collision_out}"; then
  echo "FAIL ${samename_trait_collision_reject}: expected a TraitImplSignatureMismatch return-type diagnostic" >&2
  echo "${samename_trait_collision_out}" >&2
  exit 1
fi
if grep -qE "E_CODEGEN_FRONT|D10 violation" <<<"${samename_trait_collision_out}"; then
  echo "FAIL ${samename_trait_collision_reject}: same-name trait collision fell through to codegen-front/D10 instead of TraitImplSignatureMismatch" >&2
  echo "${samename_trait_collision_out}" >&2
  exit 1
fi
echo "PASS ${samename_trait_collision_reject}"

# --- Method-SET membership reject fixtures ----------------------------------
# An `impl <Trait> for <Type>` must provide EXACTLY the trait's method set:
# every required (bodyless) method present, and no method that is not on the
# trait. The required/known method set is resolved through the trait's
# owner-qualified identity, so a same-name trait collision can never leak a
# neighbour's method set into the comparison (the previous fail-open: the
# conformance pass iterated only the impl's methods, so a missing or extra
# method slipped through and `record_trait_impl` ran unconditionally).
# Shared assertion: each fixture must REJECT with the method-set diagnostic and
# NOT fall through to a codegen-front / D10 gate.
assert_method_set_reject() { # <fixture> <needle>
  local fixture="$1" needle="$2" out
  out="$("${HEW}" check --pkg-path "${PKGS}" "${DIR}/${fixture}.hew" 2>&1)" && {
    echo "FAIL ${fixture}: hew check unexpectedly succeeded (method-set defect slipped the gate)" >&2
    echo "${out}" >&2
    exit 1
  }
  if ! grep -q "${needle}" <<<"${out}"; then
    echo "FAIL ${fixture}: expected a method-set diagnostic matching '${needle}'" >&2
    echo "${out}" >&2
    exit 1
  fi
  if grep -qE "E_CODEGEN_FRONT|D10 violation" <<<"${out}"; then
    echo "FAIL ${fixture}: method-set defect fell through to codegen-front/D10" >&2
    echo "${out}" >&2
    exit 1
  fi
  echo "PASS ${fixture}"
}

# Missing required method, resolved through each trait kind's owner-qualified
# identity under a same-name collision (aliased / imported-bare / local-shadow).
assert_method_set_reject trait_impl_missing_method_aliased_reject "is missing required method(s): close"
assert_method_set_reject trait_impl_missing_method_bare_reject "is missing required method(s): close"
assert_method_set_reject trait_impl_missing_method_local_reject "is missing required method(s): close"
# Extra method not declared on the trait, under a same-name collision.
assert_method_set_reject trait_impl_extra_method_reject "declares method(s) not on the trait: extra"

# --- Imported-supertrait inline-supermethod reject fixtures ------------------
# An `impl <Sub> for T` may provide an inherited supertrait method inline. The
# method's SIGNATURE must be checked against the supertrait that DECLARES it,
# resolved through the OWNER-QUALIFIED super chain (`trait Sub: Base` names the
# sub's owner's `Base`, never the importer namespace's). The rev7 method-set
# check permitted the inline super method but never checked its signature — a
# fail-open this closes. Each fixture must REJECT with the signature-mismatch
# diagnostic and NOT fall through to a codegen-front / D10 gate.
assert_super_sig_reject() { # <fixture>
  local fixture="$1" out
  out="$("${HEW}" check --pkg-path "${PKGS}" "${DIR}/${fixture}.hew" 2>&1)" && {
    echo "FAIL ${fixture}: hew check unexpectedly succeeded (inline super wrong-sig slipped the gate)" >&2
    echo "${out}" >&2
    exit 1
  }
  if ! grep -qE "TraitImplSignatureMismatch|but trait .* requires" <<<"${out}"; then
    echo "FAIL ${fixture}: expected a TraitImplSignatureMismatch diagnostic on the inline super method" >&2
    echo "${out}" >&2
    exit 1
  fi
  if grep -qE "E_CODEGEN_FRONT|D10 violation" <<<"${out}"; then
    echo "FAIL ${fixture}: inline super wrong-sig fell through to codegen-front/D10 instead of TraitImplSignatureMismatch" >&2
    echo "${out}" >&2
    exit 1
  fi
  echo "PASS ${fixture}"
}

# LOCAL super (`Sub: Super`), inline `base -> bool` vs `Super::base -> i64`. The
# same fail-open applies to local traits — the declaring-trait fallback keys on
# the supertrait's source name, not the sub-trait written in the impl.
assert_super_sig_reject subtrait_inline_super_wrong_sig_reject
# Direct imported super (`Sub: Base`), inline `base -> bool` vs `Base::base -> i64`.
assert_super_sig_reject imported_subtrait_inline_super_wrong_sig_reject
# Same-name supertrait collision: `Sub` from `hew::supertraits` (i64 `Base`), a
# different bare `Base` from `hew::supertraitsb` (bool); inline `base -> bool`
# must check against the OWNER-qualified `supertraits.Base` (i64), not the
# importer-namespace `supertraitsb.Base`. Run the collision case 10x below for
# determinism; this single run asserts the diagnostic shape.
assert_super_sig_reject supertrait_collision_inline_wrong_sig_reject
# Wrong-sig at an INTERMEDIATE level of a multi-level chain (`Deep: Mid: Base`):
# inline `mid -> bool` vs `Mid::mid -> i64`.
assert_super_sig_reject imported_deep_chain_inline_super_wrong_sig_reject
# Wrong-sig on a DIAMOND-inherited method (`base` reachable via two branches).
assert_super_sig_reject imported_diamond_inline_super_wrong_sig_reject
# Wrong-sig inline super reached through an ALIASED sub-trait (`Sub as S`).
assert_super_sig_reject aliased_subtrait_inline_super_wrong_sig_reject

# --- RE-EXPORTED supertrait inline-supermethod reject fixtures ----------------
# A supertrait reached via the DECLARING module's own import (a re-export) has no
# same-module `{declaring}.Base` def, so the old edge resolver fell back to the
# bare name and bound whatever same-named `Base` the FINAL importer had in scope
# (a collision-unsafe fail-open). The unified resolver follows the declaring
# module's import binding (the re-export chain) to the original owner, so the
# inline supermethod's signature is checked against that owner. Each fixture must
# REJECT with the signature-mismatch diagnostic and NOT fall through to codegen.
#
# Direct re-export with a same-name collision: `Sub` from `hew::reexsub` (super
# `reexbase.Base`, `base -> i64`) plus a different bare `Base` from
# `hew::wrongbase` (`base -> bool`); inline `base -> bool` must check against
# `reexbase.Base`, not the importer-namespace `wrongbase.Base`. Run 10x below for
# determinism; this single run asserts the diagnostic shape.
assert_super_sig_reject reexport_super_wrong_collision_reject
# ALIASED re-export: `SubA` from `hew::reexsubalias` (super `B = reexbase.Base`),
# inline `base -> bool` vs `reexbase.Base::base -> i64`. The aliased import binding
# follows `B` to `reexbase.Base` — there is no `reexsubalias.B` def to fall back on.
assert_super_sig_reject reexport_aliased_super_wrong_sig_reject
# INTERMEDIATE level of a two-hop chain: `Top` from `hew::reexchaina`, inline
# `mid -> bool` vs `reexmid.Mid::mid -> i64`. The intermediate super edge follows
# `reexchaina`'s import binding to `reexmid.Mid`.
assert_super_sig_reject reexport_chain_intermediate_wrong_sig_reject
# ORIGIN of a two-hop chain: `Top` from `hew::reexchaina`, inline `base -> bool`
# vs `reexbase.Base::base -> i64` reached by walking Top -> reexmid.Mid ->
# reexbase.Base, following an import binding at each hop.
assert_super_sig_reject reexport_deep_chain_base_wrong_sig_reject

# Extra method on NEITHER the imported sub nor its owner-qualified super chain.
assert_method_set_reject imported_subtrait_extra_method_reject "declares method(s) not on the trait: nope"
# Missing the imported sub's OWN required method (`tag`) while providing the
# inherited `base` — the sub's required set is unchanged by the super-edge fix.
assert_method_set_reject imported_subtrait_missing_own_method_reject "is missing required method(s): tag"
# REDECLARED supertrait method (`base`) satisfied by a separate `impl Base`, but
# the sub's OWN required `tag` omitted. The inherited `base` is dropped from
# `Redecl`'s required set, but `Redecl`'s own `tag` is not — so this rejects on
# `tag`, proving the inherited-method relaxation does not leak onto own methods.
assert_method_set_reject imported_redeclared_super_missing_own_reject "is missing required method(s): tag"

# Determinism: the same-name supertrait collision must REJECT on every run
# regardless of module-registration order. A bare super edge would bind whichever
# `Base` the importer registered, making the verdict order-dependent; the
# owner-qualified edge is stable. Run the collision reject 10x.
for i in $(seq 1 10); do
  det_out="$("${HEW}" check --pkg-path "${PKGS}" "${DIR}/supertrait_collision_inline_wrong_sig_reject.hew" 2>&1)" && {
    echo "FAIL supertrait_collision_inline_wrong_sig_reject (run ${i}): unexpectedly succeeded" >&2
    echo "${det_out}" >&2
    exit 1
  }
  if ! grep -qE "but trait .* requires" <<<"${det_out}"; then
    echo "FAIL supertrait_collision_inline_wrong_sig_reject (run ${i}): missing the signature-mismatch diagnostic" >&2
    echo "${det_out}" >&2
    exit 1
  fi
done
echo "PASS supertrait_collision_inline_wrong_sig_reject (10x determinism)"

# Determinism: the RE-EXPORTED supertrait collision must REJECT on every run
# regardless of module-registration order. The super edge for `reexsub.Sub`
# resolves through `reexsub`'s import binding to `reexbase.Base` (i64); a bare
# fallback would bind whichever `Base` the final importer registered first
# (`reexbase.Base` i64 or `wrongbase.Base` bool), making the verdict
# order-dependent. The import-binding resolution is stable. Run the reject 10x.
for i in $(seq 1 10); do
  reex_det_out="$("${HEW}" check --pkg-path "${PKGS}" "${DIR}/reexport_super_wrong_collision_reject.hew" 2>&1)" && {
    echo "FAIL reexport_super_wrong_collision_reject (run ${i}): unexpectedly succeeded" >&2
    echo "${reex_det_out}" >&2
    exit 1
  }
  if ! grep -qE "but trait .* requires" <<<"${reex_det_out}"; then
    echo "FAIL reexport_super_wrong_collision_reject (run ${i}): missing the signature-mismatch diagnostic" >&2
    echo "${reex_det_out}" >&2
    exit 1
  fi
done
echo "PASS reexport_super_wrong_collision_reject (10x determinism)"

# --- REQUIRED-ASSOCIATED-TYPE same-name collision reject/accept oracle ---------
# An `impl <Trait> for <Type>` that omits a REQUIRED associated type must be
# rejected. `hew::assocreqimplbad` impls `assocreq.Source` (which requires
# `type Item`) for `W` without defining `Item`. Pulling in `hew::assocemptyuse`
# (which imports a same-named empty `Source` from `hew::assocempty`) collides on
# the bare `Source` name in `trait_defs`. A bare `trait_defs["Source"]` read is
# last-write-wins between the two same-named traits, so the missing-`Item` check
# only fired when the required `Source` happened to win the write — an
# order-dependent fail-open. The required-associated-type enforcement keys off
# the impl's OWNER-qualified trait identity, so the verdict is stable. Run both
# import orders 10x; each must reject with the missing-associated-type
# diagnostic, NOT fall through to a codegen-front / D10 gate.
for fixture in \
  missing_assoc_type_collision_reject \
  missing_assoc_type_collision_reject_reversed; do
  for i in $(seq 1 10); do
    assoc_det_out="$("${HEW}" check --pkg-path "${PKGS}" "${DIR}/${fixture}.hew" 2>&1)" && {
      echo "FAIL ${fixture} (run ${i}): unexpectedly succeeded (impl omitting required type Item slipped the gate)" >&2
      echo "${assoc_det_out}" >&2
      exit 1
    }
    if ! grep -q "must define associated type \`Item\`" <<<"${assoc_det_out}"; then
      echo "FAIL ${fixture} (run ${i}): missing the required-associated-type diagnostic" >&2
      echo "${assoc_det_out}" >&2
      exit 1
    fi
    if grep -qE "E_CODEGEN_FRONT|D10 violation" <<<"${assoc_det_out}"; then
      echo "FAIL ${fixture} (run ${i}): fell through to codegen-front/D10 instead of the type-boundary diagnostic" >&2
      echo "${assoc_det_out}" >&2
      exit 1
    fi
  done
  echo "PASS ${fixture} (10x determinism)"
done

# Accept control: a COMPLETE impl (`hew::assocreqimplok` defines `type Item`)
# under the SAME same-name `Source` collision must still be accepted on every
# run — the owner-qualified check must not over-reject a correct impl just
# because an empty same-named `Source` is in the graph.
for i in $(seq 1 10); do
  assoc_ok_out="$("${HEW}" check --pkg-path "${PKGS}" "${DIR}/missing_assoc_type_collision_accept.hew" 2>&1)" || {
    echo "FAIL missing_assoc_type_collision_accept (run ${i}): a complete impl was rejected under the same-name collision" >&2
    echo "${assoc_ok_out}" >&2
    exit 1
  }
done
echo "PASS missing_assoc_type_collision_accept (10x determinism)"

# Reject fixture: two opt-ins of the same bare name from divergent-layout
# modules is a genuine ambiguity. The checker must fail closed with `ambiguous
# type` naming both published candidates, at the type boundary (NOT a bare
# last-write-wins binding that trips a downstream MIR field-order failure).
ambig_fixture="published_bare_two_optin_ambiguous"
ambig_out="$("${HEW}" check --pkg-path "${PKGS}" "${DIR}/${ambig_fixture}.hew" 2>&1)" && {
  echo "FAIL ${ambig_fixture}: hew check unexpectedly succeeded (ambiguous bare name slipped the gate)" >&2
  echo "${ambig_out}" >&2
  exit 1
}
if ! grep -q "ambiguous type \`Gadget\`" <<<"${ambig_out}"; then
  echo "FAIL ${ambig_fixture}: expected an ambiguous-type diagnostic over the published candidates" >&2
  echo "${ambig_out}" >&2
  exit 1
fi
if grep -qE "E_NOT_YET_IMPLEMENTED|field-order table" <<<"${ambig_out}"; then
  echo "FAIL ${ambig_fixture}: ambiguous bare name fell through to a MIR field-order failure instead of the type-boundary diagnostic" >&2
  echo "${ambig_out}" >&2
  exit 1
fi
echo "PASS ${ambig_fixture}"

# Reject fixture: two plain imports both export `Gadget` but neither publishes
# it bare, so a bare `Gadget` is not in scope. The checker must fail closed at
# the type boundary naming both exporting modules.
unpub_fixture="published_bare_two_plain_rejected"
unpub_out="$("${HEW}" check --pkg-path "${PKGS}" "${DIR}/${unpub_fixture}.hew" 2>&1)" && {
  echo "FAIL ${unpub_fixture}: hew check unexpectedly succeeded (unpublished bare name slipped the gate)" >&2
  echo "${unpub_out}" >&2
  exit 1
}
if ! grep -q "is not in scope" <<<"${unpub_out}"; then
  echo "FAIL ${unpub_fixture}: expected a not-in-scope diagnostic for the unpublished bare name" >&2
  echo "${unpub_out}" >&2
  exit 1
fi
if grep -qE "E_NOT_YET_IMPLEMENTED|field-order table" <<<"${unpub_out}"; then
  echo "FAIL ${unpub_fixture}: unpublished bare name fell through to a MIR field-order failure instead of the type-boundary diagnostic" >&2
  echo "${unpub_out}" >&2
  exit 1
fi
echo "PASS ${unpub_fixture}"

# Reject fixture: a PACKAGE-imported trait whose bodyless `put` takes a sibling
# `#[resource]` by value WITHOUT `consume`. A bodyless trait method signature is
# an invisible-body boundary; the importer lowers the imported module's items and
# must reject the unannotated resource param with `ResourceBoundaryParamMustConsume`
# (RAII-2 #1295). The imported-module item loop previously had no trait arm, so
# root/file-flattened traits were checked but package-imported ones slipped the
# gate — a caller could borrow across an invisible body that actually consumes.
imported_trait_reject="imported_trait_resource_param_missing_consume_reject"
imported_trait_reject_out="$("${HEW}" check --pkg-path "${PKGS}" "${DIR}/${imported_trait_reject}.hew" 2>&1)" && {
  echo "FAIL ${imported_trait_reject}: hew check unexpectedly succeeded (package-imported trait resource param slipped the boundary gate)" >&2
  echo "${imported_trait_reject_out}" >&2
  exit 1
}
if ! grep -q "ResourceBoundaryParamMustConsume" <<<"${imported_trait_reject_out}"; then
  echo "FAIL ${imported_trait_reject}: expected a ResourceBoundaryParamMustConsume diagnostic on the imported trait signature" >&2
  echo "${imported_trait_reject_out}" >&2
  exit 1
fi
echo "PASS ${imported_trait_reject}"

# Positive control: the same package-imported trait shape but with the resource
# param pinned `consume`. The disposition is spelled at the invisible-body
# boundary, so the importer must ACCEPT it — proving the package-import boundary
# check is load-bearing without over-rejecting valid `consume` signatures.
imported_trait_ok="imported_trait_resource_param_consume_ok"
imported_trait_ok_out="$("${HEW}" check --pkg-path "${PKGS}" "${DIR}/${imported_trait_ok}.hew" 2>&1)" || {
  echo "FAIL ${imported_trait_ok}: hew check rejected a consume-pinned imported trait boundary (false positive)" >&2
  echo "${imported_trait_ok_out}" >&2
  exit 1
}
if grep -q "ResourceBoundaryParamMustConsume" <<<"${imported_trait_ok_out}"; then
  echo "FAIL ${imported_trait_ok}: consume-pinned imported trait param wrongly flagged ResourceBoundaryParamMustConsume" >&2
  echo "${imported_trait_ok_out}" >&2
  exit 1
fi
echo "PASS ${imported_trait_ok}"

# KNOWN-GAP REJECT PIN (#2653) — the boundary of the #2744 fix, NOT a
# desired-behaviour test.
#
# The #2744 fix (imported single-module generic value record → resolves its
# BitCopy value class; see `imported_generic_valueclass` above) closes the
# common case. Its boundary is the two-module collision: TWO imported modules
# each defining a generic value record named `Key<T>` with DIFFERENT payload
# types (`keyleft.Key<T>.v: i64` vs `keyright.Key<T>.v: string`) both
# monomorphise to the bare mangled symbol `Key$$i32`, so the two distinct
# layouts collide on identity. That is the qualified-mono-identity gap tracked
# by #2653 (layout mangling keys on the bare origin name, not the
# module-qualified definition identity), a cross-layer reshape out of scope for
# #2744.
#
# This pin asserts the collision fails CLOSED: `hew check` REJECTS the program
# at the codegen-front slot-type gate (`E_CODEGEN_FRONT_FAIL_CLOSED`), proving
# the collision is memory-safe (no silent miscompile / type confusion), never
# that the rejection is desirable. When #2653 lands, this program becomes VALID
# (each `Key<T>` resolves to its own module's instantiation) and this pin flips
# to a compile+run oracle — update it then, do not delete it.
two_module_generic_reject="two_module_same_generic_valueclass_reject"
two_module_generic_reject_out="$("${HEW}" check --pkg-path "${PKGS}" "${DIR}/${two_module_generic_reject}.hew" 2>&1)" && {
  echo "FAIL ${two_module_generic_reject}: hew check unexpectedly succeeded — the #2653 two-module same-name generic collision must fail closed until #2653 lands" >&2
  echo "${two_module_generic_reject_out}" >&2
  exit 1
}
if ! grep -q "E_CODEGEN_FRONT_FAIL_CLOSED" <<<"${two_module_generic_reject_out}"; then
  echo "FAIL ${two_module_generic_reject}: expected the codegen-front slot-type fail-closed diagnostic (#2653 known gap)" >&2
  echo "${two_module_generic_reject_out}" >&2
  exit 1
fi
echo "PASS ${two_module_generic_reject}"

# Memory-safety pass on the ask-reply + explicit-release path (macOS only:
# MallocScribble/MallocGuardEdges are libmalloc features).
if [[ "$(uname -s)" == "Darwin" ]]; then
  if ! MallocScribble=1 MallocGuardEdges=1 \
    "${HEW}" run --pkg-path "${PKGS}" "${DIR}/imported_trait_method.hew" \
    >"${actual_output}" 2>/dev/null; then
    echo "FAIL imported_trait_method under MallocScribble" >&2
    exit 1
  fi
  if ! diff -u "${DIR}/imported_trait_method.expected" "${actual_output}"; then
    echo "FAIL imported_trait_method: MallocScribble output diverged" >&2
    exit 1
  fi
  echo "PASS imported_trait_method (MallocScribble)"
fi

echo "pkg-import oracle: all fixtures pass"
