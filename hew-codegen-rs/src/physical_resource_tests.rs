//! Exercise checked opaque lifecycles against independent C-ABI owners.

use super::*;
use hew_hir::{lower_program_host_target, ResolutionCtx};
use hew_parser::{
    ast::Item,
    module::{Module as SourceModule, ModuleGraph, ModuleId},
};
use hew_types::{module_registry::ModuleRegistry, Checker};

const PROCESS_RESOURCE: &str = r#"
    #[resource] #[opaque] pub type ProcessResultHandle {}
    impl ProcessResultHandle {
        fn close(consume self) { unsafe { hew_process_result_free(self) }; }
    }
    extern "C" {
        fn hew_process_run(command: string) -> ProcessResultHandle;
        fn hew_process_result_free(consume result: ProcessResultHandle);
    }
    pub fn make() -> ProcessResultHandle { unsafe { hew_process_run("owned") } }
"#;

fn resource_module(module_name: &str, source: &str, caller: &str) -> hew_sir::SemModule {
    let parsed = hew_parser::parse(source);
    assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
    let mut imported = hew_parser::parse(&format!("import {module_name}; {caller}"));
    assert!(imported.errors.is_empty(), "{:?}", imported.errors);
    let Item::Import(import) = &mut imported.program.items[0].0 else {
        panic!("resource import fixture")
    };
    import.resolved_items = Some(parsed.program.items.clone());
    let source_path = match module_name {
        "std.process" => "std/process.hew",
        "std.net" => "std/net/net.hew",
        _ => panic!("unknown fixture module"),
    };
    import.resolved_source_paths = vec![std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join(source_path)];
    let root = ModuleId::root();
    let resource = ModuleId::new(module_name.split('.').map(str::to_string).collect());
    let mut graph = ModuleGraph::new(root.clone());
    graph
        .add_module(SourceModule {
            id: resource.clone(),
            items: parsed.program.items,
            imports: vec![],
            source_paths: import.resolved_source_paths.clone(),
            doc: None,
        })
        .unwrap();
    graph
        .add_module(SourceModule {
            id: root.clone(),
            items: imported.program.items.clone(),
            imports: vec![],
            source_paths: vec![],
            doc: None,
        })
        .unwrap();
    graph.topo_order = vec![resource, root];
    let mut program = imported.program;
    program.module_graph = Some(graph);
    let checked = Checker::new(ModuleRegistry::new(vec![])).check_program(&program);
    assert!(checked.errors.is_empty(), "{:?}", checked.errors);
    let hir = lower_program_host_target(&program, &checked, &ResolutionCtx);
    assert!(hir.diagnostics.is_empty(), "{:?}", hir.diagnostics);
    let lowered = hew_sir::lower_module(&hir.module, &checked);
    assert!(
        lowered
            .callable_statuses
            .iter()
            .all(|(_, status)| !matches!(status, hew_sir::SirLoweringStatus::Unsupported { .. })),
        "{:?}",
        lowered.callable_statuses
    );
    hew_sir::check_module(&lowered.module).unwrap();
    lowered.module
}

thread_local! {
    static CASE: std::cell::Cell<i64> = const { std::cell::Cell::new(0) };
    static CREATED: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    static RELEASED: std::cell::RefCell<Vec<usize>> = const { std::cell::RefCell::new(Vec::new()) };
}

extern "C" fn selected_case() -> i64 {
    CASE.get()
}

unsafe extern "C" fn create_owner(command: *const std::ffi::c_void) -> *mut usize {
    // SAFETY: the declared extern borrows a live managed source string.
    assert_eq!(
        unsafe { hew_runtime::string::hew_string_byte_length(command.cast()) },
        5
    );
    let id = CREATED.get() + 1;
    CREATED.set(id);
    Box::into_raw(Box::new(id))
}

unsafe extern "C" fn release_owner(owner: *mut usize) {
    // SAFETY: each generated close must transfer one independently allocated owner.
    let owner = unsafe { Box::from_raw(owner) };
    RELEASED.with_borrow_mut(|released| released.push(*owner));
}

#[test]
fn opaque_resource_c_abi_releases_once_on_return_close_and_fault_at_o0_o2() {
    let semantic = resource_module(
        "std.process",
        PROCESS_RESOURCE,
        r#"
        extern "C" { fn selected_case() -> i64; }
        fn main() -> i64 {
            let choice = unsafe { selected_case() };
            let first = process.make();
            if choice == 0 { first.close(); return 10; }
            if choice == 1 { return 11; }
            let second = process.make();
            if choice == 2 { second.close(); return 12; }
            4 / (choice - 3)
        }
        "#,
    );
    let triple = crate::native_emission_triple();
    let target = physical_target_for_inventory(
        &triple,
        &hew_mir::physical::physical_type_inventory(&semantic),
    )
    .unwrap();
    let verified = hew_mir::lower_physical_module(&semantic, target).unwrap();
    for opt in [OptLevel::O0, OptLevel::O2] {
        let ctx = Context::create();
        let machine = crate::llvm::target_machine_for_triple_with_opt_level(&triple, opt).unwrap();
        let llvm = build_module(&ctx, verified.module(), "opaque_resources", &machine).unwrap();
        crate::llvm::run_module_pipeline(&llvm, &machine, opt).unwrap();
        let engine = llvm
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .unwrap();
        for (symbol, address) in [
            ("selected_case", selected_case as *const () as usize),
            ("hew_process_run", create_owner as *const () as usize),
            (
                "hew_process_result_free",
                release_owner as *const () as usize,
            ),
            (
                "hew_string_literal_new",
                hew_runtime::string::hew_string_literal_new as *const () as usize,
            ),
            (
                "hew_string_drop",
                hew_runtime::string::hew_string_drop as *const () as usize,
            ),
            (
                "hew_fault_new",
                hew_runtime::fault::hew_fault_new as *const () as usize,
            ),
            (
                "hew_fault_drop",
                hew_runtime::fault::hew_fault_drop as *const () as usize,
            ),
            (
                "hew_fault_report",
                hew_runtime::fault::hew_fault_report as *const () as usize,
            ),
        ] {
            engine.add_global_mapping(&llvm.get_function(symbol).unwrap(), address);
        }
        type MainBody =
            unsafe extern "C" fn(*mut i64, *mut *mut hew_runtime::fault::HewFault) -> i32;
        // SAFETY: the native main body exposes its checked result/fault ABI.
        let main = unsafe { engine.get_function::<MainBody>("__hew_main_body").unwrap() };
        for (case, expected, releases) in [
            (0, 10, vec![1]),
            (1, 11, vec![1]),
            (2, 12, vec![2, 1]),
            (3, -99, vec![2, 1]),
            (4, 4, vec![2, 1]),
        ] {
            CASE.set(case);
            CREATED.set(0);
            RELEASED.with_borrow_mut(Vec::clear);
            let mut result = -99;
            let mut fault = std::ptr::null_mut();
            // SAFETY: both outputs stay live for the complete synchronous call.
            let status = unsafe { main.call(&raw mut result, &raw mut fault) };
            assert_eq!(
                status,
                if case == 3 {
                    HEW_TRAP_DIVIDE_BY_ZERO
                } else {
                    0
                }
            );
            assert_eq!(result, expected);
            assert_eq!(CREATED.get(), releases.len());
            RELEASED.with_borrow(|actual| assert_eq!(actual, &releases));
            assert_eq!(fault.is_null(), case != 3);
            if !fault.is_null() {
                // SAFETY: failure transfers exactly one fault owner to this caller.
                unsafe { hew_runtime::fault::hew_fault_drop(fault) };
            }
        }
    }
}

#[test]
fn opaque_resource_layouts_preserve_pointer_and_native_io_token_abis() {
    let pointer = resource_module(
        "std.process",
        PROCESS_RESOURCE,
        "fn main() { let _value = process.make(); }",
    );
    let token = resource_module(
        "std.net",
        r#"
        #[resource] #[opaque] pub type Connection {}
        impl Connection { fn close(consume self) { unsafe { hew_tcp_close(self) }; } }
        extern "C" {
            fn hew_tcp_connect(address: string) -> Connection;
            fn hew_tcp_close(consume connection: Connection) -> i32;
        }
        pub fn make() -> Connection { unsafe { hew_tcp_connect("unused") } }
    "#,
        "fn main() { let _value = net.make(); }",
    );
    for triple in [
        "x86_64-unknown-linux-gnu",
        "x86_64-pc-windows-msvc",
        "aarch64-apple-darwin",
        "wasm32-unknown-unknown",
    ] {
        for (semantic, owner, repr) in [
            (
                &pointer,
                "std.process.ProcessResultHandle",
                PhysicalRepr::Pointer,
            ),
            (
                &token,
                "std.net.Connection",
                PhysicalRepr::Integer { bits: 32 },
            ),
        ] {
            let inventory = hew_mir::physical::physical_type_inventory(semantic);
            let target = physical_target_for_inventory(triple, &inventory).unwrap();
            let ty = semantic
                .resources
                .keys()
                .find(|ty| matches!(ty, ResolvedTy::Named { name, .. } if name == owner))
                .unwrap();
            assert_eq!(target.layout(ty).unwrap().repr, repr, "{triple}: {owner}");
            let physical = hew_mir::lower_physical_module(semantic, target).unwrap();
            let ctx = Context::create();
            let machine =
                crate::llvm::target_machine_for_triple_with_opt_level(triple, OptLevel::O0)
                    .unwrap();
            build_module(&ctx, physical.module(), "resource_layout", &machine)
                .unwrap()
                .verify()
                .unwrap();
        }
    }
}
