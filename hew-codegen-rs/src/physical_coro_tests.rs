//! Post-split cleanup must discard dead artifacts without losing live releases.

use std::cell::RefCell;
use std::collections::BTreeSet;
use std::ffi::{c_void, CString};

use inkwell::memory_buffer::MemoryBuffer;
use inkwell::OptimizationLevel;

use super::*;

fn machine() -> TargetMachine {
    crate::llvm::target_machine_for_triple_with_opt_level(&native_emission_triple(), OptLevel::O0)
        .unwrap()
}

fn suspended_owner<'ctx>(ctx: &'ctx Context, machine: &TargetMachine) -> Module<'ctx> {
    let module = ctx.create_module("suspended_owner");
    module.set_triple(&machine.get_triple());
    module.set_data_layout(&machine.get_target_data().get_data_layout());
    let pointer = ctx.ptr_type(AddressSpace::default());
    let function = module.add_function(
        "owned_task",
        pointer.fn_type(&[pointer.into()], false),
        None,
    );
    let release = module.add_function(
        "release_payload",
        ctx.void_type().fn_type(&[pointer.into()], false),
        None,
    );
    let builder = ctx.create_builder();
    let entry = ctx.append_basic_block(function, "entry");
    builder.position_at_end(entry);
    let owner = function.get_first_param().unwrap().into_pointer_value();
    let frame = begin(ctx, &module, &builder, function, owner).unwrap();
    let resumed = ctx.append_basic_block(function, "resumed");
    let cancelled = ctx.append_basic_block(function, "cancelled");
    frame
        .suspend(ctx, &module, &builder, resumed, cancelled, false)
        .unwrap();
    for (block, destroying) in [(resumed, false), (cancelled, true)] {
        builder.position_at_end(block);
        if destroying {
            builder
                .build_store(frame.destroying, ctx.bool_type().const_int(1, false))
                .unwrap();
        }
        builder
            .build_call(release, &[frame.state.into()], "")
            .unwrap();
        builder.build_unconditional_branch(frame.finish).unwrap();
    }
    module.verify().unwrap();
    module
}

#[test]
fn post_split_cleanup_removes_dead_heap_coroutine_artifacts_at_o0() {
    let ctx = Context::create();
    let machine = machine();
    let module = suspended_owner(&ctx, &machine);
    lower(&module, &machine).unwrap();
    assert!(module.get_function("owned_task.resume").is_some());
    assert!(module.get_function("owned_task.destroy").is_some());
    assert!(module.get_function("owned_task.cleanup").is_none());
    assert!(module.get_global("owned_task.resumers").is_none());
    let ir = module.print_to_string().to_string();
    assert!(!ir.contains("select i1 true"), "{ir}");
    assert!(module.get_function("release_payload").is_some());
}

#[test]
fn post_split_cleanup_preserves_selected_dynamic_and_retained_functions() {
    for (condition, root, destroy_live, cleanup_live) in [
        ("true", "", true, false),
        ("false", "", false, true),
        ("%condition", "", true, true),
        (
            "true",
            "@exported = constant [2 x ptr] [ptr @task.destroy, ptr @task.cleanup]",
            true,
            true,
        ),
        (
            "true",
            r#"@llvm.used = appending global [1 x ptr] [ptr @task.cleanup], section "llvm.metadata""#,
            true,
            true,
        ),
    ] {
        let ctx = Context::create();
        let ir = format!(
            r"
            {root}
            @unused = private constant [2 x ptr] [ptr @task.destroy, ptr @task.cleanup]
            declare void @observe(i32)
            define internal void @task.destroy(ptr %frame) {{
                call void @observe(i32 1)
                ret void
            }}
            define internal void @task.cleanup(ptr %frame) {{
                call void @observe(i32 2)
                ret void
            }}
            define ptr @choose(i1 %condition) {{
                %selected = select i1 {condition}, ptr @task.destroy, ptr @task.cleanup
                ret ptr %selected
            }}
            "
        );
        let ir = CString::new(ir).unwrap();
        let module = ctx
            .create_module_from_ir(MemoryBuffer::create_from_memory_range_copy(
                ir.as_bytes_with_nul(),
                "selector",
            ))
            .unwrap();
        let machine = machine();
        module.set_triple(&machine.get_triple());
        module.set_data_layout(&machine.get_target_data().get_data_layout());
        lower(&module, &machine).unwrap();
        assert_eq!(
            module.get_function("task.destroy").is_some(),
            destroy_live,
            "{condition}, {root}"
        );
        assert_eq!(
            module.get_function("task.cleanup").is_some(),
            cleanup_live,
            "{condition}, {root}"
        );
        assert!(module.get_global("unused").is_none());
    }
}

#[derive(Default)]
struct Frames {
    live: BTreeSet<usize>,
    allocations: usize,
    frees: usize,
}

thread_local! {
    static FRAMES: RefCell<Frames> = RefCell::default();
}

unsafe extern "C" fn allocate_frame(size: u64) -> *mut c_void {
    // SAFETY: forward the generated coroutine's allocation request unchanged.
    let frame = unsafe { hew_runtime::cont::hew_cont_frame_alloc(size) };
    FRAMES.with_borrow_mut(|frames| {
        frames.allocations += 1;
        frames.live.insert(frame as usize);
    });
    frame
}

unsafe extern "C" fn release_frame(frame: *mut c_void) {
    let live = FRAMES.with_borrow_mut(|frames| {
        frames.frees += 1;
        frames.live.remove(&(frame as usize))
    });
    if live {
        // SAFETY: only the first release of a registered frame reaches the runtime.
        unsafe { hew_runtime::cont::hew_cont_frame_free(frame) };
    }
}

#[repr(C)]
struct Owner {
    text: *mut c_void,
    releases: usize,
}

unsafe extern "C" fn release_payload(owner: *mut Owner) {
    // SAFETY: the fixture retains this unique owner until its coroutine is destroyed.
    let owner = unsafe { &mut *owner };
    owner.releases += 1;
    if !owner.text.is_null() {
        // SAFETY: this string is the fixture's live runtime allocation.
        unsafe { hew_runtime::string::hew_string_drop(owner.text.cast()) };
        owner.text = std::ptr::null_mut();
    }
}

#[test]
fn post_split_cleanup_releases_owner_and_frame_once_on_resume_or_cancel_at_o0_o2() {
    for level in [OptLevel::O0, OptLevel::O2] {
        let ctx = Context::create();
        let machine = machine();
        let module = suspended_owner(&ctx, &machine);
        lower(&module, &machine).unwrap();
        crate::llvm::run_module_pipeline(&module, &machine, level).unwrap();
        let engine = module
            .create_jit_execution_engine(OptimizationLevel::None)
            .unwrap();
        for (symbol, address) in [
            ("hew_cont_frame_alloc", allocate_frame as *const () as usize),
            ("hew_cont_frame_free", release_frame as *const () as usize),
            ("release_payload", release_payload as *const () as usize),
        ] {
            engine.add_global_mapping(&module.get_function(symbol).unwrap(), address);
        }
        for resume in [false, true] {
            FRAMES.with_borrow_mut(|frames| *frames = Frames::default());
            let mut text = std::ptr::null_mut();
            // SAFETY: the literal and output slot are valid for the runtime constructor.
            unsafe {
                hew_runtime::string::hew_string_literal_new(b"owned".as_ptr(), 5, &raw mut text);
            }
            let mut owner = Owner {
                text: text.cast(),
                releases: 0,
            };
            type Start = unsafe extern "C" fn(*mut Owner) -> *mut c_void;
            // SAFETY: the fixture's LLVM signature takes this owner and returns its frame.
            let frame = unsafe {
                engine
                    .get_function::<Start>("owned_task")
                    .unwrap()
                    .call(&raw mut owner)
            };
            assert!(!frame.is_null());
            assert_eq!(owner.releases, 0);
            if resume {
                // SAFETY: this frame is at its first, non-final suspension.
                unsafe { hew_runtime::cont::hew_cont_resume(frame) };
                assert_eq!(owner.releases, 1);
                FRAMES.with_borrow(|frames| assert_eq!(frames.frees, 0));
            }
            // SAFETY: the frame is live, either at its first or final suspension.
            unsafe { hew_runtime::cont::hew_cont_destroy(frame) };
            let released = owner.text.is_null();
            if !released {
                // SAFETY: a missing cleanup is a test failure, but must not leak the string.
                unsafe { hew_runtime::string::hew_string_drop(owner.text.cast()) };
            }
            assert!(released, "{level:?}, resume={resume}");
            assert_eq!(owner.releases, 1, "{level:?}, resume={resume}");
            FRAMES.with_borrow(|frames| {
                assert_eq!(frames.allocations, 1);
                assert_eq!(frames.frees, 1);
                assert!(frames.live.is_empty());
            });
        }
    }
}
