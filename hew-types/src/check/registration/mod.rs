//! Checker registration/method/expression logic, split into submodules.
#![allow(
    unused_imports,
    redundant_imports,
    reason = "header retained verbatim from the pre-split file"
)]
use super::types::ImportBindingKey;
#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
use super::*;
use crate::BuiltinType;
use hew_parser::ast::WireMetadata;

mod part1;
mod part2;
mod part3;
mod part4;
mod part5;
mod part6;
#[cfg(test)]
mod tests;

/// Whether a stdlib Hew-source registration publishes its types' bare names
/// into the importer's scope, passed to `register_stdlib_hew_items`.
///
/// `Prelude` is the compiled-in bootstrap path (`builtins`, `closable`,
/// `link_monitor`, the receiver-impl surfaces, …): these are genuine
/// always-in-scope prelude surfaces with no user `import` statement, so their
/// bare names are published unconditionally.
///
/// `Import(spec)` is a real `import std::…` of a C-backed stdlib module that
/// also ships Hew source. It obeys the same qualified-by-default gate as a
/// user-package import: a plain `import` (spec `None`) publishes only the
/// qualified name, and a named/glob/aliased import publishes the bare (or
/// aliased) binding. This closes the asymmetry where a plain stdlib import
/// exposed `Server` bare while the equivalent user-package import rejected it.
#[derive(Clone, Copy)]
pub(in crate::check) enum StdlibBarePublication<'a> {
    Prelude,
    Import(&'a Option<ImportSpec>),
}

/// Guard token for a primary item-signature registration block opened by
/// [`Checker::enter_primary_sig_scope`]. Records the previous
/// `scope_local_type_params_only` value and whether an enclosing type-param
/// frame was pushed, so [`Checker::exit_primary_sig_scope`] restores both
/// exactly. `#[must_use]` so a forgotten `exit` (which would leave the resolver
/// pinned in scope-local mode and leak a frame) is a compile error.
#[must_use = "every enter_primary_sig_scope must be paired with exit_primary_sig_scope"]
pub(in crate::check) struct PrimarySigScope {
    prev_scope_local: bool,
    pushed_frame: bool,
}

/// Every key one impl method's declaration identity is published under.
///
/// Ownership is exclusive: a generic declaration holds `shared` + `canonical`
/// and no mangled key; a concrete specialisation holds only `mangled`. The two
/// sets never overlap, so `impl Render for Box<i64>` cannot claim the identity
/// `impl<T> Render for Box<T>` published.
struct ImplMethodDeclarationKeys {
    /// `Type::method` — the cross-module compatibility dispatch key.
    shared: Option<String>,
    /// `module.Type::method` — the declaring module's own dispatch key.
    canonical: Option<String>,
    /// `Type$$i64::method` and its module-owned form.
    mangled: Vec<String>,
}

#[derive(Default)]
struct ExplicitReturnFinder {
    saw_return: bool,
}

fn extern_signature_description(
    signature_params: &[Ty],
    return_type: &Ty,
    consuming_params: &[bool],
    is_variadic: bool,
) -> String {
    let mut params = signature_params
        .iter()
        .enumerate()
        .map(|(index, ty)| {
            let ownership = if consuming_params.get(index).copied().unwrap_or(false) {
                "consume "
            } else {
                ""
            };
            format!("{ownership}{}", ty.user_facing())
        })
        .collect::<Vec<_>>();
    if is_variadic {
        params.push("...".to_string());
    }
    format!("fn({}) -> {}", params.join(", "), return_type.user_facing())
}

/// Canonical nominal identity of an extern signature type, resolved AT
/// REGISTRATION in the declaring item's own lexical context and STORED on
/// the symbol's contract, so the single-owner comparison is plain identity
/// equality of already-resolved signatures (rc1-F1 stage C).
///
/// Resolution authority, in order:
/// 1. The declaring FILE: a bare name declared in the item's own source
///    file resolves to that file's minted module identity, so the same
///    declaration reached through directory peer assembly and through a
///    direct submodule import mints one owner — and two same-named
///    declarations in different peer files mint two (a false merge would
///    equate distinct layouts on one C symbol; a false split diagnoses
///    loudly and is correctable at the declaration).
/// 2. Exactly one sibling file of the declaring module: lexically visible,
///    single declaration — its file identity.
/// 3. The checker's canonical resolution for imported/prelude spellings
///    (`canonical_nominal_name`), which refuses ambiguity.
///
/// The `builtin` marker is checker metadata and can legitimately differ
/// when one module spells a nominal through an import qualifier, so it is
/// erased rather than compared.
fn extern_contract_nominal_identity(checker: &Checker, ty: &Ty) -> Ty {
    match ty {
        Ty::Named { name, args, .. } => Ty::Named {
            name: checker
                .extern_signature_nominal_owner(name)
                .unwrap_or_else(|| name.clone()),
            args: args
                .iter()
                .map(|arg| extern_contract_nominal_identity(checker, arg))
                .collect(),
            builtin: None,
        },
        _ => ty.map_children_pub(&|child| extern_contract_nominal_identity(checker, child)),
    }
}

impl super::lints::NodeVisitor for ExplicitReturnFinder {
    fn visit_stmt(&mut self, stmt: &Stmt, _span: &Span) {
        self.saw_return |= matches!(stmt, Stmt::Return(_));
    }

    fn visit_expr(&mut self, expr: &Expr, _span: &Span) {
        self.saw_return |= matches!(expr, Expr::Return(_) | Expr::ReturnError(_));
    }
}

fn block_has_explicit_return(block: &hew_parser::ast::Block) -> bool {
    let mut finder = ExplicitReturnFinder::default();
    super::lints::walk_block(block, &mut finder);
    finder.saw_return
}

impl StdlibBarePublication<'_> {
    /// The bare binding name to publish for `name`, or `None` if this
    /// registration does not publish it unqualified. `Prelude` always
    /// publishes the bare name as-is; `Import(spec)` publishes only when the
    /// spec opts the name in (named/glob/aliased), applying any alias — the
    /// same gate `register_user_module` uses via `should_import_name` /
    /// `resolve_import_name`.
    fn bare_binding(self, name: &str) -> Option<String> {
        match self {
            Self::Prelude => Some(name.to_string()),
            Self::Import(spec) => Checker::should_import_name(name, spec).then(|| {
                Checker::resolve_import_name(spec, name).unwrap_or_else(|| name.to_string())
            }),
        }
    }

    /// Whether this publication came from a real source `import`.
    ///
    /// Only real imports need an HIR source-identity binding. Prelude types are
    /// always in scope and remain governed by the builtin catalog; publishing a
    /// Prelude alias here would let it outrank a root-authored same-name type.
    fn records_import_identity(self) -> bool {
        matches!(self, Self::Import(_))
    }
}

/// A trait reference (`impl <Trait> for ...`) resolved to its OWNER-QUALIFIED
/// identity, so trait conformance never keys off the bare `Trait::method` name
/// (which is first-write-wins and polluted under same-name collisions). Produced
/// by `resolve_trait_conformance_identity`.
pub(in crate::check) struct ResolvedTraitIdentity {
    /// The trait's defining module (`Some("srccollidea")` for an
    /// aliased/imported-bare/single-owner-import trait), or `None` for a
    /// local/root trait or a reference that did not resolve to a single owner.
    owner: Option<String>,
    /// The trait's source name (the name as declared in its defining module,
    /// recovering through an alias: `import m::{ Trait as C }` resolves `C` to
    /// source name `Trait`).
    source_trait_name: String,
    /// `true` when the reference binds to a LOCAL trait declaration, which
    /// shadows any imported same-name trait. Callers source the required-method
    /// set and signatures from the local `TraitInfo`, never the polluted bare
    /// `fn_sigs` key.
    is_local: bool,
}

/// The scope a trait name is resolved in. A primary trait / bound is spelled in
/// the importing program (`Current`); a SUPERTRAIT edge is spelled inside the
/// declaring module (`Declaring`) and must follow that module's import bindings
/// (the re-export chain), never the importer's same-name trait. See
/// `resolve_trait_ref`.
#[derive(Clone, Copy)]
pub(in crate::check) enum TraitRefScope<'a> {
    Current,
    Declaring { module: &'a str },
}

/// Context for canonicalizing trait-vs-impl signature types to a single
/// defining-module-qualified identity before comparison
/// (`check_impl_method_against_trait`). Carries only borrowed predicates so the
/// recursion holds no `&self` borrow across the later mutable error reporting.
struct TraitSigCanonCtx<'a> {
    /// In-scope module short names; a `module.Name` whose `module` is here is an
    /// explicit, unambiguous identity that keeps its qualifier.
    modules: &'a std::collections::HashSet<String>,
    /// The trait's defining module, used to qualify a bare type name written in
    /// the trait declaration. `None` for a root/local trait.
    trait_owner: Option<&'a str>,
    /// Whether a `{owner}.{bare}` spelling is a registered type def.
    defines_qualified: &'a dyn Fn(&str) -> bool,
    /// Whether a bare name shadows one of the impl scope's own types (then it
    /// keeps the bare identity rather than being qualified to the trait owner).
    is_local: &'a dyn Fn(&str) -> bool,
}

/// Import-free projection generated from the owning declarations in
/// `std/builtins.hew` and `std/link_monitor.hew`.
const MONITOR_REF_HEW: &str = include_str!(concat!(env!("OUT_DIR"), "/monitor_ref.hew"));

/// Embedded source for `std/failure.hew`.
///
/// Parsed at import-registration time for `std::failure` so the
/// `CrashInfo` struct and `CrashAction` enum used in `#[on(crash)]` hook
/// signatures are visible in the checker even in programs that were not
/// loaded through the module-graph path (e.g. inline programs in tests).
///
const FAILURE_HEW: &str = include_str!("../../../../std/failure.hew");

/// Stdlib-floor modules permitted to DECLARE `#[intrinsic("…")]` functions.
///
/// A605 (ratified): the `#[intrinsic]` surface is compiler-internal-only — no
/// user-reachable module may declare an intrinsic. Each entry is a `.`-joined
/// module path matched against the checker's `current_module`
/// (e.g. `ModuleId { path: ["std", "math"] }` → `"std.math"`).
///
/// The list is an explicit, enumerated allowlist (not a prefix match): a module
/// is a floor module only if its full dotted path is present here. This is the
/// security boundary's authority — keep it as small as the floor requires.
///
/// Current members:
/// - `std.math` — the math intrinsics (`exp`/`log`/`sqrt`/…) are declared as
///   typed `#[intrinsic("math.*")]` stubs in `std/math/math.hew`; the catalog
///   supplies the lowering. Registration carries the exact catalog identity
///   across module aliases; direct calls publish a typed
///   `RuntimeCallFamily::MathIntrinsic`, while `abs`/`min`/`max` select their
///   i64/f64 family from the resolved operand type.
/// - `std.mem` — the memory-intrinsic floor (`mem.alloc`/`mem.realloc`/
///   `mem.dealloc` + byte-level `mem.ptr_offset`/`mem.ptr_copy`) declared as
///   typed `#[intrinsic("mem.*")]` stubs in `std/mem/mem.hew` (W5.005 / F1b).
///   The pointer ops are byte-level monomorphic (A612) — no `<T>`. These are
///   unsafe heap primitives; A605 keeps them compiler-internal-only — no user
///   surface may reach them. Codegen synthesizes their trampoline bodies from
///   the catalog id threaded on `RawMirFunction::intrinsic_id` (Decision 4
///   Option A); an unrecognised id is fail-closed (D343), never a silent
///   empty-body no-op.
/// - `std.encoding.utf8` — validating and explicitly lossy byte decoding.
///   These are ordinary typed Hew declarations whose exact canonical source
///   identity selects a closed runtime operation; the raw status/out ABI is
///   not exposed to source programs.
/// - `std.encoding.wire` — the generic codec facade. Every call carries a
///   checker-recorded `GenericWireCodec` rewrite; the declarations have no
///   body to fall back to.
const INTRINSIC_FLOOR_MODULES: &[&str] = &[
    "std.math",
    "std.mem",
    "std.encoding.utf8",
    "std.encoding.wire",
    "std.stream",
];

#[must_use]
pub fn intrinsic_floor_modules() -> &'static [&'static str] {
    INTRINSIC_FLOOR_MODULES
}

/// Returns `true` iff `module` is a stdlib-floor module permitted to declare
/// `#[intrinsic]` functions (see [`INTRINSIC_FLOOR_MODULES`]).
///
/// Fail-closed: the root/user module (`None`) is never a floor module, and any
/// module path not in the explicit allowlist is rejected.
fn is_intrinsic_floor_module(module: Option<&str>) -> bool {
    module.is_some_and(|m| INTRINSIC_FLOOR_MODULES.contains(&m))
}

fn exact_source_nominal_matches(ty: &Ty, qualified: &str, declaring_module: Option<&str>) -> bool {
    matches!(
        ty,
        Ty::Named { name, args, .. }
            if args.is_empty()
                && crate::ffi_contracts::source_nominal_matches_qualified(
                    qualified,
                    declaring_module,
                    name,
                )
    )
}

fn imported_result_surface_matches(
    declaration: &SourceExternDeclaration,
    surface: &str,
    qualified: &str,
) -> bool {
    if surface == qualified {
        return true;
    }
    let Some((owner_module, nominal)) = qualified.rsplit_once('.') else {
        return false;
    };
    let Some((surface_owner, surface_nominal)) = surface.rsplit_once('.') else {
        return false;
    };
    if surface_nominal != nominal {
        return false;
    }

    // Ordinary named imports retain the source module's short identity in
    // resolved `Ty::Named` values. Tie that spelling back to exactly one
    // resolved direct graph edge, so two same-short modules cannot authorize
    // each other by last-write-wins registration.
    let matching_targets: Vec<_> = declaration
        .direct_import_modules
        .iter()
        .filter(|target| crate::short_name(target) == surface_owner)
        .collect();
    matches!(matching_targets.as_slice(), [target] if target.as_str() == owner_module)
}

fn imported_source_result_matches(
    ty: &Ty,
    qualified: &str,
    declaration: &SourceExternDeclaration,
    module_import_bindings: &HashMap<ImportBindingKey, String>,
    import_type_name_aliases: &HashMap<ImportBindingKey, String>,
) -> bool {
    let Ty::Named { name, args, .. } = ty else {
        return false;
    };
    if !args.is_empty() {
        return false;
    }
    if exact_source_nominal_matches(ty, qualified, declaration.declaring_module.as_deref()) {
        return true;
    }

    let Some(producer_module) = declaration.declaring_module.as_ref() else {
        return false;
    };
    let Some((owner_module, nominal)) = qualified.rsplit_once('.') else {
        return false;
    };
    if !declaration.direct_import_modules.contains(owner_module) {
        return false;
    }

    // Whole-module bindings preserve their exact resolved target, including
    // aliases (`import example::io as device` → `device.Socket`).
    if let Some((binding, tail)) = name.split_once('.') {
        if tail == nominal
            && module_import_bindings
                .get(&(
                    Some(producer_module.clone()),
                    declaration.declaring_file,
                    binding.to_string(),
                ))
                .is_some_and(|target| target == owner_module)
        {
            return true;
        }
    }

    // Named imports and declaration aliases can resolve directly to the
    // source identity. The alias table supplies the original declaration
    // spelling; the direct graph target supplies the full owner provenance.
    if let Some(source_identity) = import_type_name_aliases.get(&(
        Some(producer_module.clone()),
        declaration.declaring_file,
        name.clone(),
    )) {
        if imported_result_surface_matches(declaration, source_identity, qualified) {
            return true;
        }
    }

    imported_result_surface_matches(declaration, name, qualified)
}

fn lifecycle_description(candidate: &OpaqueResourceLifecycleCandidate) -> String {
    format!(
        "resource={}, close={}, release={}({})@{}, depth={:?}, result={:?}, retention={:?}",
        candidate.resource_declaration.full_path(),
        candidate.close_declaration.full_path(),
        candidate.release_symbol,
        candidate.release_declaration.full_path(),
        candidate.release_param_index,
        candidate.discharge_depth,
        candidate.result_ownership,
        candidate.result_retention
    )
}

fn lifecycle_matches(
    established: &OpaqueResourceLifecycleCandidate,
    candidate: &OpaqueResourceLifecycleCandidate,
) -> bool {
    established.resource_declaration == candidate.resource_declaration
        && established.close_declaration == candidate.close_declaration
        && established.release_declaration == candidate.release_declaration
        && established.release_symbol == candidate.release_symbol
        && established.release_param_index == candidate.release_param_index
        && established.discharge_depth == candidate.discharge_depth
        && established.result_ownership == candidate.result_ownership
        && established.result_retention == candidate.result_retention
}

fn release_signature_mismatch(
    declaration: &SourceExternDeclaration,
    signature: &FnSig,
    release_contract: &crate::ffi_contracts::ExternOwnershipContract,
    owner_module: &str,
    resource_type: &str,
) -> Option<String> {
    use crate::ffi_contracts::{ExternParamOwnership, ExternResultOwnership};

    if release_contract.result != ExternResultOwnership::None {
        return Some("release contract produces an owned result".to_string());
    }
    if release_contract.params.len() != release_contract.resource_param_types.len() {
        return Some("release contract resource parameter arity is incomplete".to_string());
    }
    if signature.params.len() != release_contract.params.len()
        || declaration.consuming_params.len() != release_contract.params.len()
    {
        return Some(format!(
            "source release arity {} does not match contract arity {}",
            signature.params.len(),
            release_contract.params.len()
        ));
    }

    let matching_positions: Vec<_> = release_contract
        .params
        .iter()
        .zip(release_contract.resource_param_types)
        .enumerate()
        .filter(|(_, (mode, nominal))| {
            **mode == ExternParamOwnership::Consume && **nominal == resource_type
        })
        .map(|(index, _)| index)
        .collect();
    if matching_positions.len() != 1 {
        return Some(format!(
            "release contract must consume exactly one {resource_type}, found {}",
            matching_positions.len()
        ));
    }

    for (index, ((mode, nominal), source_ty)) in release_contract
        .params
        .iter()
        .zip(release_contract.resource_param_types)
        .zip(&signature.params)
        .enumerate()
    {
        let source_consumes = declaration.consuming_params[index];
        if source_consumes != (*mode == ExternParamOwnership::Consume) {
            return Some(format!(
                "source consume disposition differs from contract at parameter {index}"
            ));
        }
        if !nominal.is_empty()
            && !exact_source_nominal_matches(source_ty, nominal, Some(owner_module))
        {
            return Some(format!(
                "source resource nominal differs from contract at parameter {index}"
            ));
        }
    }
    None
}

enum SourceCandidateOutcome {
    Irrelevant,
    Candidate(OpaqueResourceLifecycleCandidate),
    Conflict {
        resource_type: String,
        release_symbol: String,
        kind: OpaqueResourceLifecycleConflictKind,
    },
}

/// The reason one type cannot cross a C-ABI boundary, if it cannot.
///
/// Only shapes with no possible C representation are refused here. Records,
/// opaque handles and the runtime's pointer-carrier builtins pass through:
/// the physical target resolver is the authority on their layout.
fn unmarshallable_extern_ty(ty: &Ty) -> Option<&'static str> {
    match ty {
        Ty::Tuple(_) => Some("is a tuple"),
        Ty::Array(..) | Ty::Slice(_) => Some("is an array or slice"),
        Ty::Function { .. } | Ty::Closure { .. } => Some("is a callable value"),
        Ty::TraitObject { .. } => Some("is a trait object"),
        _ => None,
    }
}

fn validated_resource_candidate(
    resource_declaration: crate::DefId,
    typed_result: crate::ffi_contracts::ExternOwnedResourceResult,
    release_contract: &crate::ffi_contracts::ExternOwnershipContract,
    producer_declaration: &SourceExternDeclaration,
    release_declaration: &SourceExternDeclaration,
    close_declaration: crate::DefId,
    producer_symbol: &str,
) -> OpaqueResourceLifecycleCandidate {
    OpaqueResourceLifecycleCandidate {
        resource_declaration,
        resource_type: typed_result.resource_type.to_string(),
        owner_module: typed_result.owner_module.to_string(),
        close_declaration,
        release_declaration: release_declaration.declaration.clone(),
        release_symbol: typed_result.release_symbol.to_string(),
        release_param_index: release_contract
            .params
            .iter()
            .zip(release_contract.resource_param_types)
            .position(|(mode, nominal)| {
                *mode == crate::ffi_contracts::ExternParamOwnership::Consume
                    && *nominal == typed_result.resource_type
            })
            .expect("validated release contract has one consuming resource position"),
        discharge_depth: typed_result.discharge_depth,
        result_ownership: typed_result.result,
        result_retention: typed_result.result_retention,
        producer_symbols: [producer_symbol.to_string()].into_iter().collect(),
        producer_declarations: [producer_declaration.declaration.clone()]
            .into_iter()
            .collect(),
        producer_modules: producer_declaration
            .declaring_module
            .iter()
            .cloned()
            .collect(),
    }
}

#[expect(
    clippy::too_many_arguments,
    clippy::too_many_lines,
    reason = "resource candidates join declaration, contract, import, and signature authorities"
)]
fn derive_source_resource_candidate(
    producer_declaration: &SourceExternDeclaration,
    producer_symbol: &str,
    producer_contract: &crate::ffi_contracts::ExternOwnershipContract,
    source_declarations: &[SourceExternDeclaration],
    fn_sigs: &HashMap<String, FnSig>,
    module_import_bindings: &HashMap<ImportBindingKey, String>,
    import_type_name_aliases: &HashMap<ImportBindingKey, String>,
    impl_method_declaration_ids: &HashMap<String, crate::DefId>,
    contracts_by_symbol: &std::collections::BTreeMap<
        &str,
        &crate::ffi_contracts::ExternOwnershipContract,
    >,
    identity: &crate::identity::IdentityTable,
) -> SourceCandidateOutcome {
    let Some(typed_result) =
        crate::ffi_contracts::owned_resource_result_for_contract(producer_contract)
    else {
        return SourceCandidateOutcome::Irrelevant;
    };
    let producer_has_owner_provenance = producer_declaration.declaring_module.as_deref()
        == Some(typed_result.owner_module)
        || (producer_declaration.declaring_module.is_some()
            && producer_declaration
                .direct_import_modules
                .contains(typed_result.owner_module));
    if !producer_has_owner_provenance {
        // A raw symbol collision outside the nominal owner has no lifecycle
        // relevance unless its module graph carries a direct edge to that
        // owner. Root, transitive, and unrelated-module lookalikes stay silent.
        return SourceCandidateOutcome::Irrelevant;
    }
    let failure = |kind| SourceCandidateOutcome::Conflict {
        resource_type: typed_result.resource_type.to_string(),
        release_symbol: typed_result.release_symbol.to_string(),
        kind,
    };
    let Some(resource_declaration) = identity
        .declaration_by_path(typed_result.resource_type)
        .cloned()
    else {
        return failure(
            OpaqueResourceLifecycleConflictKind::ProducerResultMismatch {
                actual: "<missing source declaration identity>".to_string(),
            },
        );
    };

    let Some(producer_signature) = fn_sigs.get(&producer_declaration.signature_key) else {
        if producer_declaration.declaring_module.as_deref() != Some(typed_result.owner_module) {
            return SourceCandidateOutcome::Irrelevant;
        }
        return failure(
            OpaqueResourceLifecycleConflictKind::ProducerResultMismatch {
                actual: "<missing source signature>".to_string(),
            },
        );
    };
    if !imported_source_result_matches(
        &producer_signature.return_type,
        typed_result.resource_type,
        producer_declaration,
        module_import_bindings,
        import_type_name_aliases,
    ) {
        if producer_declaration.declaring_module.as_deref() != Some(typed_result.owner_module) {
            // A direct importer may reuse the C endpoint spelling for an
            // unrelated declaration. Until its result resolves to the exact
            // owner nominal it has no standing to conflict-kill that owner's
            // lifecycle.
            return SourceCandidateOutcome::Irrelevant;
        }
        return failure(
            OpaqueResourceLifecycleConflictKind::ProducerResultMismatch {
                actual: format!("{:?}", producer_signature.return_type),
            },
        );
    }

    let Some(release_contract) = contracts_by_symbol.get(typed_result.release_symbol) else {
        return failure(
            OpaqueResourceLifecycleConflictKind::ReleaseSignatureMismatch {
                detail: "release symbol has no ownership contract".to_string(),
            },
        );
    };
    let matching_release_declarations: Vec<_> = source_declarations
        .iter()
        .filter(|declaration| {
            declaration.declaring_module.as_deref() == Some(typed_result.owner_module)
                && declaration.symbol == typed_result.release_symbol
        })
        .collect();
    let Some(release_declaration) = matching_release_declarations.first() else {
        return failure(OpaqueResourceLifecycleConflictKind::ReleaseDeclarationMissing);
    };
    if matching_release_declarations.len() != 1 {
        return failure(
            OpaqueResourceLifecycleConflictKind::ReleaseSignatureMismatch {
                detail: format!(
                    "expected one source release declaration, found {}",
                    matching_release_declarations.len()
                ),
            },
        );
    }
    let Some(release_signature) = fn_sigs.get(&release_declaration.signature_key) else {
        return failure(
            OpaqueResourceLifecycleConflictKind::ReleaseSignatureMismatch {
                detail: "source release signature is missing".to_string(),
            },
        );
    };
    if let Some(detail) = release_signature_mismatch(
        release_declaration,
        release_signature,
        release_contract,
        typed_result.owner_module,
        typed_result.resource_type,
    ) {
        return failure(OpaqueResourceLifecycleConflictKind::ReleaseSignatureMismatch { detail });
    }

    let close_dispatch_key = format!("{}::close", typed_result.resource_type);
    let Some(close_declaration) = impl_method_declaration_ids
        .get(&close_dispatch_key)
        .cloned()
    else {
        return failure(OpaqueResourceLifecycleConflictKind::CloseDeclarationMissing);
    };

    SourceCandidateOutcome::Candidate(validated_resource_candidate(
        resource_declaration,
        typed_result,
        release_contract,
        producer_declaration,
        release_declaration,
        close_declaration,
        producer_symbol,
    ))
}

fn source_declaration_matches_endpoint(
    declaration: &SourceExternDeclaration,
    endpoint: &str,
) -> bool {
    declaration.symbol == endpoint
        || declaration
            .symbol_template
            .as_ref()
            .is_some_and(|template| template.matches_canonical_expansion(endpoint))
}

fn derive_opaque_resource_candidate_graph(
    source_declarations: &[SourceExternDeclaration],
    fn_sigs: &HashMap<String, FnSig>,
    module_import_bindings: &HashMap<ImportBindingKey, String>,
    import_type_name_aliases: &HashMap<ImportBindingKey, String>,
    impl_method_declaration_ids: &HashMap<String, crate::DefId>,
    contracts: &[(&str, crate::ffi_contracts::ExternOwnershipContract)],
    identity: &crate::identity::IdentityTable,
) -> OpaqueResourceCandidateGraph {
    let contracts_by_symbol: std::collections::BTreeMap<
        &str,
        &crate::ffi_contracts::ExternOwnershipContract,
    > = contracts
        .iter()
        .map(|(symbol, contract)| (*symbol, contract))
        .collect();
    let mut graph = OpaqueResourceCandidateGraph::default();
    let mut conflicted_types = std::collections::BTreeSet::new();

    for producer_declaration in source_declarations {
        for (producer_symbol, producer_contract) in &contracts_by_symbol {
            if !source_declaration_matches_endpoint(producer_declaration, producer_symbol) {
                continue;
            }
            let candidate = match derive_source_resource_candidate(
                producer_declaration,
                producer_symbol,
                producer_contract,
                source_declarations,
                fn_sigs,
                module_import_bindings,
                import_type_name_aliases,
                impl_method_declaration_ids,
                &contracts_by_symbol,
                identity,
            ) {
                SourceCandidateOutcome::Irrelevant => continue,
                SourceCandidateOutcome::Conflict {
                    resource_type,
                    release_symbol,
                    kind,
                } => {
                    if let Some(declaration) = identity.declaration_by_path(&resource_type) {
                        conflicted_types.insert(declaration.clone());
                    }
                    graph.conflicts.push(OpaqueResourceLifecycleConflict {
                        resource_type,
                        producer_symbol: (*producer_symbol).to_string(),
                        release_symbol,
                        kind,
                    });
                    continue;
                }
                SourceCandidateOutcome::Candidate(candidate) => candidate,
            };
            let resource_declaration = candidate.resource_declaration.clone();
            match graph.candidates.entry(resource_declaration.clone()) {
                std::collections::btree_map::Entry::Vacant(entry) => {
                    entry.insert(candidate);
                }
                std::collections::btree_map::Entry::Occupied(mut entry)
                    if lifecycle_matches(entry.get(), &candidate) =>
                {
                    entry
                        .get_mut()
                        .producer_symbols
                        .insert((*producer_symbol).to_string());
                    entry
                        .get_mut()
                        .producer_declarations
                        .insert(producer_declaration.declaration.clone());
                    entry
                        .get_mut()
                        .producer_modules
                        .extend(candidate.producer_modules);
                }
                std::collections::btree_map::Entry::Occupied(entry) => {
                    let established = lifecycle_description(entry.get());
                    let conflicting = lifecycle_description(&candidate);
                    graph.conflicts.push(OpaqueResourceLifecycleConflict {
                        resource_type: candidate.resource_type.clone(),
                        producer_symbol: (*producer_symbol).to_string(),
                        release_symbol: candidate.release_symbol.clone(),
                        kind: OpaqueResourceLifecycleConflictKind::MultipleProducerLifecycle {
                            established,
                            conflicting,
                        },
                    });
                    conflicted_types.insert(resource_declaration);
                }
            }
        }
    }

    for resource_declaration in conflicted_types {
        graph.candidates.remove(&resource_declaration);
    }
    graph.conflicts.sort_by(|left, right| {
        (
            &left.resource_type,
            &left.producer_symbol,
            &left.release_symbol,
        )
            .cmp(&(
                &right.resource_type,
                &right.producer_symbol,
                &right.release_symbol,
            ))
    });
    graph
}

/// Module-graph nodes reached by a root file-path import.
///
/// The checker visits these nodes under their source module so diagnostics and
/// span keys retain file provenance. The frontend later flattens their items
/// into `program.items`, however, and HIR emits those declarations in the root
/// namespace. Declaration IDs must follow that emitted ownership, while package
/// imports remain module-qualified.
pub(in crate::check) fn flat_file_import_module_ids(
    program: &Program,
) -> HashSet<hew_parser::module::ModuleId> {
    let Some(module_graph) = program.module_graph.as_ref() else {
        return HashSet::new();
    };
    let file_paths = hew_parser::module::file_import_chain_sources(&program.items);
    if file_paths.is_empty() {
        return HashSet::new();
    }
    module_graph
        .modules
        .iter()
        .filter(|(module_id, module)| {
            **module_id != module_graph.root
                && module
                    .source_paths
                    .iter()
                    .any(|source| file_paths.contains(source))
        })
        .map(|(module_id, _)| module_id.clone())
        .collect()
}

/// Whether `item` is a record/type-decl declaring the bare `type_name`.
/// Mirrors the HIR lowering helper of the same shape (#2208).
fn collision_item_declares_type_name(item: &Item, type_name: &str) -> bool {
    match item {
        Item::TypeDecl(decl) => decl.name == type_name,
        Item::Record(decl) => decl.name == type_name,
        _ => false,
    }
}

/// Package modules that are re-export "doubles" subsumed by a superset module,
/// so a type they re-surface is not double-counted as a cross-module collision.
/// Faithful port of the HIR lowering's `preferred_package_module_ids` re-export
/// subsumption: module M is preferred (kept as the canonical declarant) iff some
/// N ≠ M has `files(M) ⊆ files(N)` and either the subset is strict, or the sets
/// are equal and N precedes M in topo order (a deterministic tiebreak keeping
/// exactly one). Used to dedup a stdlib module surfaced through two import paths
/// (e.g. `std::net::http` + `std::net::http::http_client`) so `http.Response`
/// stays unique and is never owner-qualified (#2208).
fn collision_preferred_package_module_ids(
    program: &Program,
    file_import_modules: &HashSet<hew_parser::module::ModuleId>,
) -> HashSet<hew_parser::module::ModuleId> {
    use std::path::{Path, PathBuf};

    let mut preferred = HashSet::new();
    let Some(mg) = program.module_graph.as_ref() else {
        return preferred;
    };

    let mut candidates: Vec<(&hew_parser::module::ModuleId, HashSet<&Path>, usize)> = Vec::new();
    for (pos, id) in mg.topo_order.iter().enumerate() {
        if *id == mg.root || file_import_modules.contains(id) {
            continue;
        }
        let Some(module) = mg.modules.get(id) else {
            continue;
        };
        let files: HashSet<&Path> = module.source_paths.iter().map(PathBuf::as_path).collect();
        if files.is_empty() {
            continue;
        }
        candidates.push((id, files, pos));
    }

    for i in 0..candidates.len() {
        let (m_files, m_pos) = (&candidates[i].1, candidates[i].2);
        for j in 0..candidates.len() {
            if i == j {
                continue;
            }
            let (n_files, n_pos) = (&candidates[j].1, candidates[j].2);
            if !m_files.is_subset(n_files) {
                continue;
            }
            let equal = m_files.len() == n_files.len();
            if !equal || n_pos < m_pos {
                preferred.insert(candidates[i].0.clone());
                break;
            }
        }
    }
    preferred
}

/// Whether an aggregate directory module is replaying a member-bearing item
/// owned by a distinct child source module.
///
/// Item equality alone is insufficient because two modules can author the
/// same declaration shape independently. Match the source span as well, and
/// let a same-leaf preferred module retain ownership of the directory's root
/// file before considering distinct children.
fn member_item_is_absorbed_from_distinct_child(
    program: &Program,
    preferred_modules: &HashSet<hew_parser::module::ModuleId>,
    current_module: &hew_parser::module::ModuleId,
    item: &Item,
    item_span: &Span,
) -> bool {
    if preferred_modules.contains(current_module) {
        return false;
    }
    let Some(module_graph) = program.module_graph.as_ref() else {
        return false;
    };
    let current_leaf = current_module.path.last();
    let appears_in = |module_id: &hew_parser::module::ModuleId| {
        module_graph.modules.get(module_id).is_some_and(|module| {
            module
                .items
                .iter()
                .any(|(candidate, candidate_span)| candidate == item && candidate_span == item_span)
        })
    };
    if preferred_modules
        .iter()
        .any(|module_id| module_id.path.last() == current_leaf && appears_in(module_id))
    {
        return false;
    }
    preferred_modules
        .iter()
        .any(|module_id| module_id.path.last() != current_leaf && appears_in(module_id))
}

/// Whether `type_name` is declared by 2+ distinct non-root modules (package or,
/// with an empty `file_import_modules` exclusion, file-import), counting a
/// re-export double only once via `preferred_modules` subsumption. Faithful port
/// of the HIR lowering's `imported_type_name_collides` — the authoritative
/// collision notion the checker owner-qualification must agree with (#2208).
fn collision_imported_type_name_collides(
    program: &Program,
    file_import_modules: &HashSet<hew_parser::module::ModuleId>,
    preferred_modules: &HashSet<hew_parser::module::ModuleId>,
    type_name: &str,
) -> bool {
    let Some(module_graph) = program.module_graph.as_ref() else {
        return false;
    };
    module_graph
        .modules
        .iter()
        .filter(|(module_id, module)| {
            // Include the root module as a declarant: MIR's authoritative
            // `collided_type_names` counts distinct qualified identities over
            // EVERY item (root included), so a root `Result` colliding with an
            // imported `pkg.Result` is a real collision MIR keys `pkg.Result`.
            // Excluding root left the checker/HIR value flow bare against that
            // qualified layout (#2208). Re-export subsumption below still dedups
            // a package that re-exports a file module's item.
            !file_import_modules.contains(*module_id)
                && module.items.iter().any(|(item, _)| {
                    if !collision_item_declares_type_name(item, type_name) {
                        return false;
                    }
                    if preferred_modules.contains(*module_id) {
                        return true;
                    }
                    !preferred_modules.iter().any(|preferred_id| {
                        module_graph
                            .modules
                            .get(preferred_id)
                            .is_some_and(|preferred| {
                                preferred
                                    .items
                                    .iter()
                                    .any(|(candidate, _)| candidate == item)
                            })
                    })
                })
        })
        .take(2)
        .count()
        > 1
}
