//! Compile the presentation component with the same compiler libraries and ABI.
//! The build graph is acyclic: these libraries do not depend on the CLI.

use std::path::PathBuf;

use hew_codegen_rs::physical::{emit_host_object, HostExport, PhysicalEmitOptions};

fn main() {
    let package = PathBuf::from(std::env::var_os("CARGO_MANIFEST_DIR").unwrap());
    let source = package.join("src/diagnostic_renderer.hew");
    let root = package.parent().unwrap();
    let out = PathBuf::from(std::env::var_os("OUT_DIR").unwrap());
    let triple = std::env::var("TARGET").unwrap();
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed={}", source.display());
    // Resolve against this source tree, never an installed SDK. Cargo rebuilds
    // the compiler dependencies; the frontend records exact source inputs.
    let frontend = hew_compile::FrontendOptions {
        module_search_paths: Some(vec![root.to_path_buf()]),
        ..Default::default()
    };
    let state = hew_compile::run_file_frontend_to_typecheck(source.to_str().unwrap(), &frontend)
        .unwrap_or_else(|error| panic!("diagnostic renderer frontend: {}", error.message));
    for input in frontend.documents.inputs() {
        // Absent optional package files are not renderer dependencies. Cargo
        // would rebuild on every invocation if asked to watch a missing file.
        if input.sha256.is_some() {
            println!("cargo:rerun-if-changed={}", input.path.display());
        }
    }
    let checked = state.typecheck_result.tco.as_ref().unwrap();
    let target = hew_compile::SessionTarget {
        hir_arch: if triple.starts_with("aarch64") {
            hew_hir::TargetArch::Aarch64
        } else if triple.starts_with("x86_64") {
            hew_hir::TargetArch::X86_64
        } else {
            hew_hir::TargetArch::Other
        },
        codegen_triple: Some(triple.clone()),
        ..hew_compile::SessionTarget::native()
    };
    let session = hew_compile::Session::from_frontend_options(target, &frontend);
    let (semantic, declarations) = session
        .lower_host_program(&state.program, checked, &["render"])
        .expect("lower diagnostic renderer");
    let inventory = hew_mir::physical::physical_type_inventory(&semantic.semantics().module);
    let target = hew_codegen_rs::physical::physical_target_for_inventory(&triple, &inventory)
        .expect("diagnostic renderer target layout");
    let physical = semantic.lower_physical(target).expect("physical renderer");
    let callable = physical
        .module()
        .callables
        .iter()
        .find(|callable| callable.declaration == declarations[0])
        .expect("selected renderer declaration");
    let selections = [(callable.id, "diagnostic_render_text")];
    let export = HostExport::new(&physical, &selections).expect("typed renderer export");
    let header = export.header();
    // The Rust adapter borrows one opaque text owner and receives one text
    // owner. Refuse a source signature change before linking its fixed ABI.
    assert!(
        header.lines().any(|line| line == "int32_t diagnostic_render_text(const HewText *arg0, HewText **out, HewError **error);"),
        "diagnostic renderer must export a borrowed string -> string function"
    );
    let artefacts = emit_host_object(
        &export,
        &PhysicalEmitOptions {
            module_name: "diagnostic_renderer",
            out_dir: &out,
            target_triple: Some(&triple),
            opt_level: hew_codegen_rs::OptLevel::O2,
            emit_llvm: false,
            address_sanitizer: false,
            debug_source: None,
            debug_display_source: None,
            link_freestanding_wasm: false,
            build_info: None,
        },
    )
    .expect("emit diagnostic renderer");
    std::fs::write(out.join("diagnostic_renderer.h"), header).unwrap();
    cc::Build::new()
        .object(artefacts.native_obj_path.expect("native renderer object"))
        .compile("diagnostic_renderer");
}
