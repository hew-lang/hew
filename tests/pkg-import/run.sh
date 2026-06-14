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
