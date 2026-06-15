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
  # `RecordLayout` registry must key each layout by qualified name, not the
  # bare `Widget`, or one field load reads the wrong slot width and trips the
  # codegen fail-closed. Kept last: until qualified type identity lands this
  # fixture is RED (hew run aborts), and a RED entry exits the loop before the
  # reject fixture below.
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
  # A monomorphic record (`Point`) and a generic record (`Holder<Box>`) both
  # CONSTRUCTED through their module-qualified OUTER name (`qualshapes.Point`,
  # `qualshapes.Holder<qualshapes.Box>`) under qualified-by-default. Layout
  # registration keys on the bare outer name (`Point`, `Holder$$Box`), so the
  # MIR `StructInit` field-order lookup, the MIR codegen-readiness check, and
  # the codegen `record_struct_for` monomorphic arm must each shorten the
  # qualified outer name before the lookup, or the construction falls through
  # the field-order / record-layout fail-closed.
  qualified_construct_layout
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

# Reject fixture: cross-module same-bare-name trait-signature mismatch. The
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
