//! Compile the presentation component with the same compiler libraries and ABI.
//! The build graph is acyclic: these libraries do not depend on the CLI.

use std::path::PathBuf;

use hew_codegen_rs::physical::{emit_host_object, HostExport, PhysicalEmitOptions};
use hew_mir::physical::{ParamCarrier, SemParamPassing};
use hew_types::ResolvedTy;

fn main() {
    // The renderers are part of this toolchain, so they compile against the
    // standard library this checkout's compiler crates were built from.
    // `HEW_STD` selects the std a user program runs against; left in place
    // it would resolve std sources outside this checkout while this
    // checkout's builtins stay compiled in, and the checker then refuses the
    // checkout's own prelude declarations as user redeclarations.
    std::env::remove_var("HEW_STD");
    let build = std::thread::Builder::new()
        .name("hew-presentation-renderers".into())
        .stack_size(hew_compile::COMPILER_STACK_SIZE)
        .spawn(|| {
            build_renderer(
                "diagnostic_renderer",
                "src/diagnostic_renderer.hew",
                &[
                    ("render", "diagnostic_render_text"),
                    ("render_ownership", "ownership_render_text"),
                ],
            );
            build_renderer(
                "machine_renderer",
                "src/machine/diagram.hew",
                &[("render", "machine_render_text")],
            );
        })
        .expect("start presentation renderer compiler");
    if let Err(error) = build.join() {
        std::panic::resume_unwind(error);
    }
}

fn build_renderer(module_name: &str, source_path: &str, exports: &[(&str, &str)]) {
    let package = PathBuf::from(std::env::var_os("CARGO_MANIFEST_DIR").unwrap());
    let source = package.join(source_path);
    let root = package.parent().unwrap();
    let out = PathBuf::from(std::env::var_os("OUT_DIR").unwrap());
    let triple = std::env::var("TARGET").unwrap();
    println!("cargo:rerun-if-changed=build.rs");
    println!("cargo:rerun-if-changed={}", source.display());
    // Resolve against this source tree, never an installed SDK. The checker
    // decides which sources are the canonical std from the compiler's own std
    // root, so the frontend resolves from that same root, and it must be this
    // checkout. Cargo rebuilds the compiler dependencies; the frontend records
    // exact source inputs.
    let std_roots = hew_types::module_registry::stdlib_search_paths();
    assert_eq!(
        std_roots,
        [root.canonicalize().unwrap()],
        "the compiler's standard-library root must be this checkout"
    );
    let frontend = hew_compile::FrontendOptions {
        module_search_paths: Some(std_roots),
        ..Default::default()
    };
    let state = hew_compile::run_file_frontend_to_typecheck(source.to_str().unwrap(), &frontend)
        .unwrap_or_else(|error| panic!("presentation renderer frontend: {}", describe(&error)));
    for input in state.typecheck_result.sources.inputs() {
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
    let names: Vec<_> = exports.iter().map(|(name, _)| *name).collect();
    let (semantic, declarations) = session
        .lower_host_program(&state.program, checked, &names)
        .expect("lower presentation renderer");
    let plan = semantic.physical_plan();
    let target = hew_codegen_rs::physical::physical_target_for_inventory(&triple, plan.inventory())
        .expect("presentation renderer target layout");
    let physical = plan.lower(target).expect("physical renderer");
    let selections: Vec<_> = declarations
        .iter()
        .zip(exports)
        .map(|(declaration, (_, export_name))| {
            let callable = physical
                .module()
                .callables
                .iter()
                .find(|callable| callable.declaration == *declaration)
                .expect("selected renderer declaration");
            // The Rust adapter borrows one opaque text owner and receives one
            // text owner. Check the selected contract before linking its ABI.
            assert!(
                matches!(callable.params.as_slice(), [param]
                    if param.ty == ResolvedTy::String
                        && param.passing == SemParamPassing::Borrow
                        && param.carrier == ParamCarrier::Direct)
                    && callable.return_ty == ResolvedTy::String,
                "presentation renderer must export a borrowed string -> string function"
            );
            (callable.id, *export_name)
        })
        .collect();
    let export = HostExport::new(&physical, &selections).expect("typed renderer export");
    let artefacts = emit_host_object(
        &export,
        &PhysicalEmitOptions {
            module_name,
            out_dir: &out,
            target_triple: Some(&triple),
            opt_level: hew_codegen_rs::OptLevel::O2,
            emit_llvm: false,
            address_sanitizer: false,
            debug_source: None,
            link_freestanding_wasm: false,
            build_info: None,
        },
    )
    .expect("emit presentation renderer");
    std::fs::write(out.join(format!("{module_name}.h")), export.header()).unwrap();
    cc::Build::new()
        .object(artefacts.native_obj_path.expect("native renderer object"))
        .compile(module_name);
}

/// The failure message with each diagnostic's own message.
fn describe(error: &hew_compile::FrontendFailure) -> String {
    let mut text = error.message.clone();
    for diagnostic in &error.diagnostics {
        text.push('\n');
        text += &match &diagnostic.kind {
            hew_compile::FrontendDiagnosticKind::Message(message) => message.message.clone(),
            hew_compile::FrontendDiagnosticKind::Parse(error) => error.message.clone(),
            hew_compile::FrontendDiagnosticKind::Type(error) => error.to_string(),
            hew_compile::FrontendDiagnosticKind::Hir(error) => error.note.clone(),
        };
    }
    text
}
