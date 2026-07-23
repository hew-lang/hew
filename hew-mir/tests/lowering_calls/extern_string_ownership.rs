//! Extern C-ABI string-return ownership classification at MIR lowering.
//!
//! `ExternDecl::malloc_string_return` is the memory-safety hinge for foreign
//! string returns: `true` tells codegen to copy the raw malloc-owned C string
//! into the Hew string domain and `free` the foreign allocation; `false` tells
//! codegen the return is ALREADY a header-aware refcounted Hew string it must
//! not adopt. Getting it wrong corrupts memory in BOTH directions — adopting a
//! header-aware string frees `base+16`; treating a foreign C string as a Hew
//! string reads a phantom 16-byte header.
//!
//! The classification is derived from the extern's carried
//! [`hew_hir::ExternProvenance`] (a proven defining-module fact), NEVER by
//! absence from `diagnostic_source_modules`. These tests pin the matrix:
//!
//! - standard-library provenance → header-aware (`false`);
//! - root / user-package provenance → foreign adopt (`true`);
//! - a user-package symbol whose NAME collides with a std producer that is
//!   absent from the JIT catalog → still `true` (provenance is primary; the
//!   catalog is not);
//! - a classified Hew runtime symbol → the monotonic secondary guard suppresses
//!   adoption back to `false`;
//! - an unrepresentable provenance (`Module("")`) → fails closed with an
//!   `ExternStringOwnershipUnresolved` diagnostic instead of guessing.

use std::collections::HashMap;

use hew_hir::{ids::IdGen, ExternProvenance, HirExternFn, HirItem, HirModule};
use hew_mir::{
    classify_extern_string_ownership, lower_hir_module, ExternStringOwnership, MirDiagnosticKind,
};
use hew_types::ResolvedTy;

fn empty_module(items: Vec<HirItem>) -> HirModule {
    HirModule {
        items,
        diagnostic_source_modules: HashMap::default(),
        root_item_ids: std::collections::HashSet::new(),
        wire_layouts: std::sync::Arc::new(HashMap::default()),
        type_classes: HashMap::default(),
        monomorphisations: vec![],
        call_site_type_args: HashMap::default(),
        vec_generic_element_abi: HashMap::default(),
        record_layouts: vec![],
        enum_layouts: vec![],
        machine_instantiations: vec![],
        supervisor_child_slots: HashMap::default(),
        pool_accessor_sites: HashMap::default(),
        regex_literals: vec![],
    }
}

/// Build a single C-ABI `extern fn <name>() -> <return_ty>` item carrying the
/// given defining-module provenance.
fn extern_item(
    ids: &mut IdGen,
    name: &str,
    return_ty: ResolvedTy,
    abi: &str,
    provenance: ExternProvenance,
) -> HirItem {
    HirItem::ExternFn(HirExternFn {
        id: ids.item(),
        node: ids.node(),
        name: name.to_string(),
        abi: abi.to_string(),
        param_tys: vec![],
        param_consume: vec![],
        return_ty,
        provenance,
        runtime_capability: None,
        span: 0..0,
    })
}

/// Lower a one-extern module and return that extern's `malloc_string_return`.
fn malloc_string_return_for(
    name: &str,
    return_ty: ResolvedTy,
    abi: &str,
    provenance: ExternProvenance,
) -> bool {
    let mut ids = IdGen::default();
    let item = extern_item(&mut ids, name, return_ty, abi, provenance);
    let pipeline = lower_hir_module(&empty_module(vec![item]));
    assert_eq!(
        pipeline.extern_decls.len(),
        1,
        "expected exactly one lowered extern decl"
    );
    pipeline.extern_decls[0].malloc_string_return
}

// ---------------------------------------------------------------------------
// Direct classification matrix — `classify_extern_string_ownership`.
// ---------------------------------------------------------------------------

#[test]
fn stdlib_provenance_is_header_aware() {
    // A std string producer that is ABSENT from the JIT catalog is still
    // header-aware purely on provenance — the catalog cannot gate this.
    assert_eq!(
        classify_extern_string_ownership(
            &ExternProvenance::Module("std.crypto".to_string()),
            "hew_uuid_v4",
        ),
        ExternStringOwnership::HeaderAware,
    );
    // The bare `std` module and `::`-scoped spellings resolve the same way.
    assert_eq!(
        classify_extern_string_ownership(&ExternProvenance::Module("std".to_string()), "hew_x"),
        ExternStringOwnership::HeaderAware,
    );
    assert_eq!(
        classify_extern_string_ownership(&ExternProvenance::Module("std::io".to_string()), "hew_y",),
        ExternStringOwnership::HeaderAware,
    );
}

#[test]
fn root_provenance_foreign_string_is_adopted() {
    // Root compilation unit extern — a genuinely foreign C string producer.
    assert_eq!(
        classify_extern_string_ownership(&ExternProvenance::Root, "my_c_strdup"),
        ExternStringOwnership::ForeignAdopt,
    );
}

#[test]
fn user_package_provenance_foreign_string_is_adopted() {
    // A non-std named module (package/subpkg) is user/foreign provenance.
    assert_eq!(
        classify_extern_string_ownership(
            &ExternProvenance::Module("hew.testffi".to_string()),
            "hew_testffi_name",
        ),
        ExternStringOwnership::ForeignAdopt,
    );
}

#[test]
fn user_package_symbol_collision_with_absent_std_producer_still_adopts() {
    // A user-package extern whose NAME wears the `hew_` prefix but is absent
    // from the JIT catalog must classify from its user/package provenance —
    // adopt — not from the name spelling. This is the exact failure the old
    // "absence from diagnostic_source_modules" heuristic could not
    // distinguish; provenance can. (`hew_uuid_v4` served as the fixture until
    // the std-used extern surface was classified into the catalog, which
    // correctly flipped it to the suppressed case below.)
    assert!(!hew_types::jit_symbols::is_classified_hew_ffi_symbol(
        "hew_uuid_v99"
    ));
    assert_eq!(
        classify_extern_string_ownership(
            &ExternProvenance::Module("mypkg.strings".to_string()),
            "hew_uuid_v99",
        ),
        ExternStringOwnership::ForeignAdopt,
    );
    // The classified std producer itself is monotonically suppressed to
    // header-aware even from user provenance: adopting a header-aware
    // `str_to_malloc` string as foreign malloc is the memory-unsafe direction.
    assert!(hew_types::jit_symbols::is_classified_hew_ffi_symbol(
        "hew_uuid_v4"
    ));
    assert_eq!(
        classify_extern_string_ownership(
            &ExternProvenance::Module("mypkg.strings".to_string()),
            "hew_uuid_v4",
        ),
        ExternStringOwnership::HeaderAware,
    );
}

#[test]
fn classified_runtime_symbol_from_root_is_guarded_to_header_aware() {
    // The JIT catalog is a MONOTONIC secondary guard: a classified Hew runtime
    // producer (`hew_io_read_line` is in the catalog) is suppressed back to
    // header-aware even from root/user provenance — the guard only ever turns
    // adoption OFF, never on.
    assert!(hew_types::jit_symbols::is_classified_hew_ffi_symbol(
        "hew_io_read_line"
    ));
    assert_eq!(
        classify_extern_string_ownership(&ExternProvenance::Root, "hew_io_read_line"),
        ExternStringOwnership::HeaderAware,
    );
}

#[test]
fn empty_module_provenance_is_unresolved() {
    // A named module carrying no identity is unrepresentable provenance: refuse
    // to guess a memory-unsafe direction.
    assert_eq!(
        classify_extern_string_ownership(&ExternProvenance::Module(String::new()), "whatever"),
        ExternStringOwnership::Unresolved,
    );
}

// ---------------------------------------------------------------------------
// End-to-end through `lower_hir_module` → `ExternDecl::malloc_string_return`.
// ---------------------------------------------------------------------------

#[test]
fn lowering_stdlib_string_extern_does_not_adopt() {
    assert!(!malloc_string_return_for(
        "hew_uuid_v4",
        ResolvedTy::String,
        "C",
        ExternProvenance::Module("std.crypto".to_string()),
    ));
}

#[test]
fn lowering_root_foreign_string_extern_adopts() {
    assert!(malloc_string_return_for(
        "my_c_strdup",
        ResolvedTy::String,
        "C",
        ExternProvenance::Root,
    ));
}

#[test]
fn lowering_user_package_collision_string_extern_adopts() {
    // Package provenance beats a `hew_`-prefixed name absent from the catalog
    // — the decl adopts. A CLASSIFIED std producer (`hew_uuid_v4`) is instead
    // monotonically suppressed even from package provenance.
    assert!(malloc_string_return_for(
        "hew_uuid_v99",
        ResolvedTy::String,
        "C",
        ExternProvenance::Module("mypkg.strings".to_string()),
    ));
    assert!(!malloc_string_return_for(
        "hew_uuid_v4",
        ResolvedTy::String,
        "C",
        ExternProvenance::Module("mypkg.strings".to_string()),
    ));
}

#[test]
fn lowering_non_c_abi_string_extern_never_adopts() {
    // Ownership classification is gated on `abi == "C"`; an `rt`-ABI string
    // return is a runtime symbol and never adopts, regardless of provenance.
    assert!(!malloc_string_return_for(
        "hew_uuid_v4",
        ResolvedTy::String,
        "rt",
        ExternProvenance::Root,
    ));
}

#[test]
fn lowering_c_abi_non_string_extern_never_adopts() {
    // A C-ABI extern that does not return a string is out of scope for adoption.
    assert!(!malloc_string_return_for(
        "my_c_count",
        ResolvedTy::I64,
        "C",
        ExternProvenance::Root,
    ));
}

#[test]
fn lowering_unresolved_provenance_fails_closed_with_diagnostic() {
    let mut ids = IdGen::default();
    let item = extern_item(
        &mut ids,
        "orphan_strdup",
        ResolvedTy::String,
        "C",
        ExternProvenance::Module(String::new()),
    );
    let pipeline = lower_hir_module(&empty_module(vec![item]));

    // Fail closed: the inert decl carries `false` (never a guessed `true`)...
    assert_eq!(pipeline.extern_decls.len(), 1);
    assert!(!pipeline.extern_decls[0].malloc_string_return);

    // ...and a diagnostic naming the symbol gates codegen at the CLI.
    let found = pipeline.diagnostics.iter().any(|d| {
        matches!(
            &d.kind,
            MirDiagnosticKind::ExternStringOwnershipUnresolved { symbol }
                if symbol == "orphan_strdup"
        )
    });
    assert!(
        found,
        "expected ExternStringOwnershipUnresolved diagnostic for orphan_strdup, got {:?}",
        pipeline.diagnostics
    );
}
