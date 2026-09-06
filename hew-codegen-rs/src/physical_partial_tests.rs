//! Execute verified aggregate aliases and their initialization transfers.

use std::cell::RefCell;
use std::collections::BTreeSet;

use super::*;

#[path = "../../hew-mir/src/physical_partial_fixture.rs"]
mod fixture;

#[repr(C)]
struct Inner {
    left: *mut c_void,
    right: *mut c_void,
}

#[repr(C)]
struct Root {
    outer: *mut c_void,
    inner: Inner,
    unit: (),
    code: i64,
}

#[derive(Default)]
struct Releases {
    live: BTreeSet<usize>,
    order: Vec<usize>,
}

thread_local! {
    static RELEASES: RefCell<Releases> = RefCell::default();
}

unsafe extern "C" fn release(value: *mut c_void) {
    let live = RELEASES.with_borrow_mut(|releases| {
        releases.order.push(value as usize);
        releases.live.remove(&(value as usize))
    });
    if live {
        // SAFETY: only the first release of a registered real runtime allocation
        // reaches the runtime. Duplicate or invalid releases remain test failures.
        unsafe { hew_runtime::string::hew_string_drop(value.cast()) };
    }
}

fn roots(code: i64) -> (Root, Root, Vec<usize>) {
    let pointers = (0..6)
        .map(|index| {
            let text = format!("owner {index}");
            let mut pointer = std::ptr::null_mut();
            // SAFETY: the text is valid UTF-8 and the output slot is uniquely writable.
            unsafe {
                hew_runtime::string::hew_string_literal_new(
                    text.as_ptr(),
                    u32::try_from(text.len()).unwrap(),
                    &raw mut pointer,
                )
            };
            pointer.cast::<c_void>()
        })
        .collect::<Vec<_>>();
    let owners = pointers
        .iter()
        .map(|pointer| *pointer as usize)
        .collect::<Vec<_>>();
    RELEASES.with_borrow_mut(|releases| {
        assert!(releases.live.is_empty());
        releases.live.extend(owners.iter().copied());
        releases.order.clear();
    });
    let root = |offset: usize| Root {
        outer: pointers[offset],
        inner: Inner {
            left: pointers[offset + 1],
            right: pointers[offset + 2],
        },
        unit: (),
        code,
    };
    (root(0), root(3), owners)
}

fn physical_fixture(case: fixture::Case) -> PhysicalModule {
    let semantic = fixture::module(case);
    let diagnostics = hew_sir::verify_module(&semantic);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    let target = physical_target_for_inventory(
        &native_emission_triple(),
        &hew_mir::physical::physical_type_inventory(&semantic),
    )
    .unwrap();
    let root_layout = target.layout(&fixture::root_ty()).unwrap();
    assert_eq!(
        root_layout.size,
        u64::try_from(std::mem::size_of::<Root>()).unwrap()
    );
    assert_eq!(
        root_layout.align,
        u32::try_from(std::mem::align_of::<Root>()).unwrap()
    );
    hew_mir::lower_physical_module(&semantic, target)
        .unwrap()
        .into_unverified()
}

fn execute(case: fixture::Case, flag: bool, code: i64, order: &[usize]) {
    let physical = physical_fixture(case);
    let callee = &physical.callables[0];
    assert_eq!(callee.params[0].carrier, ParamCarrier::Indirect);
    assert_eq!(callee.params[1].carrier, ParamCarrier::Indirect);
    assert_eq!(
        callee.params[2].layout.repr,
        PhysicalRepr::Integer { bits: 8 }
    );
    let symbol = emitted_symbol(&physical, callee);
    for optimized in [false, true] {
        let ctx = Context::create();
        let llvm = llvm(&ctx, &physical);
        llvm.get_function(&symbol)
            .unwrap()
            .set_linkage(Linkage::External);
        let engine = engine(&llvm, optimized);
        engine.add_global_mapping(
            &llvm.get_function("hew_string_drop").unwrap(),
            release as *const () as usize,
        );
        type Run =
            unsafe extern "C" fn(*mut Root, *mut Root, u8, *mut i64, *mut *mut c_void) -> i32;
        let (mut first, mut second, owners) = roots(code);
        let mut result = -1;
        let mut fault = std::ptr::null_mut();
        // SAFETY: checked target layouts and physical carriers match the C ABI;
        // all six strings are independent runtime allocations consumed by the call.
        let status = unsafe {
            engine.get_function::<Run>(&symbol).unwrap().call(
                &raw mut first,
                &raw mut second,
                u8::from(flag),
                &raw mut result,
                &raw mut fault,
            )
        };
        let (actual, remaining) = RELEASES.with_borrow_mut(|releases| {
            (
                std::mem::take(&mut releases.order),
                std::mem::take(&mut releases.live),
            )
        });
        for pointer in &remaining {
            // SAFETY: these registered allocations were not released by the call.
            unsafe { hew_runtime::string::hew_string_drop((*pointer as *mut c_void).cast()) };
        }
        if !fault.is_null() {
            // SAFETY: the failed call returned one owned runtime fault.
            unsafe { hew_runtime::fault::hew_fault_drop(fault.cast()) };
        }
        assert!(
            remaining.is_empty(),
            "{case:?} O2={optimized}: leaked owners {remaining:?}"
        );
        assert_eq!(
            actual,
            order.iter().map(|index| owners[*index]).collect::<Vec<_>>(),
            "{case:?} O2={optimized}"
        );
        if matches!(case, fixture::Case::Fault) && code == 0 {
            assert_ne!(status, 0);
            assert!(!fault.is_null());
            assert_eq!(result, -1);
        } else {
            assert_eq!(status, 0, "{case:?} O2={optimized}");
            assert!(fault.is_null());
            assert_eq!(result, 42);
        }
    }
}

#[test]
fn partial_subtree_assignment_releases_live_dead_and_mixed_old_fields_at_o0_o2() {
    execute(
        fixture::Case::LiveReplacement,
        false,
        1,
        &[2, 1, 5, 4, 0, 3],
    );
    execute(
        fixture::Case::DeadReplacement,
        false,
        1,
        &[2, 1, 5, 4, 0, 3],
    );
    execute(
        fixture::Case::MixedReplacement,
        false,
        1,
        &[1, 2, 5, 4, 0, 3],
    );
    execute(
        fixture::Case::BranchReplacement,
        false,
        1,
        &[2, 1, 5, 4, 0, 3],
    );
    execute(
        fixture::Case::BranchReplacement,
        true,
        1,
        &[1, 2, 5, 4, 0, 3],
    );
}

#[test]
fn loop_permutation_transfers_payloads_and_different_leaf_states_at_o0_o2() {
    execute(fixture::Case::Permutation, false, 1, &[1, 3, 2, 0, 5, 4]);
    execute(fixture::Case::Permutation, true, 1, &[1, 3, 5, 4, 2, 0]);
}

#[test]
fn fault_cleanup_and_zero_sized_reinitialization_release_each_owner_at_o0_o2() {
    execute(fixture::Case::Fault, false, 0, &[1, 2, 0, 5, 4, 3]);
    execute(fixture::Case::Fault, false, 1, &[1, 2, 0, 5, 4, 3]);
    execute(fixture::Case::ZeroSized, false, 1, &[2, 1, 0, 5, 4, 3]);
}

#[test]
fn projected_storage_uses_root_addresses_and_separate_initialization_bits() {
    let physical = physical_fixture(fixture::Case::Permutation);
    let ctx = Context::create();
    let llvm = llvm(&ctx, &physical);
    let function = &physical.functions[0];
    let symbol = emitted_symbol(&physical, &physical.callables[0]);
    let emitted = llvm.get_function(&symbol).unwrap();
    let allocations = emitted
        .get_basic_blocks()
        .iter()
        .flat_map(|block| block.get_instructions())
        .filter(|instruction| {
            instruction.get_opcode() == inkwell::values::InstructionOpcode::Alloca
        })
        .map(|instruction| {
            instruction
                .get_name()
                .unwrap()
                .to_string_lossy()
                .into_owned()
        })
        .collect::<BTreeSet<_>>();
    for (&id, projection) in &function.aggregate_storage {
        assert_eq!(
            allocations.contains(&format!("s{}", id.0)),
            projection.path.is_empty()
        );
        for leaf in &projection.leaves {
            assert!(allocations.contains(&format!("s{}.initialized", leaf.storage.0)));
        }
    }
}
